	.text
.no_dead_strip _Divide__uDivide_var_u16_u16
_Divide__uDivide_var_u16_u16:
LFB0:
	pushq	%rbp
LCFI0:
	movq	%rsp, %rbp
LCFI1:
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movzwl	104+_MM_Divide(%rip), %edx
	movzwl	%dx, %edx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI2:
	ret
LFE0:
.no_dead_strip _Divide__Divide_var_u16_u16
_Divide__Divide_var_u16_u16:
LFB1:
	pushq	%rbp
LCFI3:
	movq	%rsp, %rbp
LCFI4:
	pushq	%rbx
LCFI5:
	subq	$8, %rsp
LCFI6:
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ebx
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L3
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L3
	subq	$1, %rax
	addq	%rcx, %rdx
L3:
	cmpq	$65535, %rax
	jle	L4
	movl	$1761, %edi
	call	__m3_fault
L4:
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ebx
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L5
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L5
	subq	$1, %rax
	addq	%rcx, %rdx
L5:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI7:
	ret
LFE1:
.no_dead_strip _Divide__uDivide_param_u16_u16
_Divide__uDivide_param_u16_u16:
LFB2:
	pushq	%rbp
LCFI8:
	movq	%rsp, %rbp
LCFI9:
	movw	%di, -2(%rbp)
	movw	%si, -4(%rbp)
	movzwl	-2(%rbp), %eax
	movzwl	-4(%rbp), %edx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI10:
	ret
LFE2:
.no_dead_strip _Divide__Divide_param_u16_u16
_Divide__Divide_param_u16_u16:
LFB3:
	pushq	%rbp
LCFI11:
	movq	%rsp, %rbp
LCFI12:
	pushq	%rbx
LCFI13:
	subq	$24, %rsp
LCFI14:
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %ebx
	movzwl	-20(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L8
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L8
	subq	$1, %rax
	addq	%rcx, %rdx
L8:
	cmpq	$65535, %rax
	jle	L9
	movl	$1825, %edi
	call	__m3_fault
L9:
	movzwl	-18(%rbp), %ebx
	movzwl	-20(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L10
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L10
	subq	$1, %rax
	addq	%rcx, %rdx
L10:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI15:
	ret
LFE3:
.no_dead_strip _Divide__uDivide_var_u16_u32
_Divide__uDivide_var_u16_u32:
LFB4:
	pushq	%rbp
LCFI16:
	movq	%rsp, %rbp
LCFI17:
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movl	108+_MM_Divide(%rip), %edx
	mov	%edx, %edx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI18:
	ret
LFE4:
.no_dead_strip _Divide__Divide_var_u16_u32
_Divide__Divide_var_u16_u32:
LFB5:
	pushq	%rbp
LCFI19:
	movq	%rsp, %rbp
LCFI20:
	pushq	%rbx
LCFI21:
	subq	$8, %rsp
LCFI22:
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ebx
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L13
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L13
	subq	$1, %rax
	addq	%rcx, %rdx
L13:
	cmpq	$65535, %rax
	jle	L14
	movl	$1889, %edi
	call	__m3_fault
L14:
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ebx
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L15
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L15
	subq	$1, %rax
	addq	%rcx, %rdx
L15:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI23:
	ret
LFE5:
.no_dead_strip _Divide__uDivide_param_u16_u32
_Divide__uDivide_param_u16_u32:
LFB6:
	pushq	%rbp
LCFI24:
	movq	%rsp, %rbp
LCFI25:
	movw	%di, -2(%rbp)
	movl	%esi, -8(%rbp)
	movzwl	-2(%rbp), %eax
	mov	-8(%rbp), %edx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI26:
	ret
LFE6:
.no_dead_strip _Divide__Divide_param_u16_u32
_Divide__Divide_param_u16_u32:
LFB7:
	pushq	%rbp
LCFI27:
	movq	%rsp, %rbp
LCFI28:
	pushq	%rbx
LCFI29:
	subq	$24, %rsp
LCFI30:
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %ebx
	mov	-24(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L18
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L18
	subq	$1, %rax
	addq	%rcx, %rdx
L18:
	cmpq	$65535, %rax
	jle	L19
	movl	$1953, %edi
	call	__m3_fault
L19:
	movzwl	-18(%rbp), %ebx
	mov	-24(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L20
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L20
	subq	$1, %rax
	addq	%rcx, %rdx
L20:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI31:
	ret
LFE7:
.no_dead_strip _Divide__uDivide_var_u16_LC
_Divide__uDivide_var_u16_LC:
LFB8:
	pushq	%rbp
LCFI32:
	movq	%rsp, %rbp
LCFI33:
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	112+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI34:
	ret
LFE8:
.no_dead_strip _Divide__Divide_var_u16_LC
_Divide__Divide_var_u16_LC:
LFB9:
	pushq	%rbp
LCFI35:
	movq	%rsp, %rbp
LCFI36:
	pushq	%rbx
LCFI37:
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ebx
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L23
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L23
	subq	$1, %rax
	addq	%rcx, %rdx
L23:
	popq	%rbx
	leave
LCFI38:
	ret
LFE9:
.no_dead_strip _Divide__uDivide_param_u16_LC
_Divide__uDivide_param_u16_LC:
LFB10:
	pushq	%rbp
LCFI39:
	movq	%rsp, %rbp
LCFI40:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movzwl	-2(%rbp), %eax
	movl	$0, %edx
	divq	-16(%rbp)
	leave
LCFI41:
	ret
LFE10:
.no_dead_strip _Divide__Divide_param_u16_LC
_Divide__Divide_param_u16_LC:
LFB11:
	pushq	%rbp
LCFI42:
	movq	%rsp, %rbp
LCFI43:
	pushq	%rbx
LCFI44:
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %ebx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L26
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L26
	subq	$1, %rax
	addq	%rcx, %rdx
L26:
	popq	%rbx
	leave
LCFI45:
	ret
LFE11:
.no_dead_strip _Divide__uDivide_var_u16_I
_Divide__uDivide_var_u16_I:
LFB12:
	pushq	%rbp
LCFI46:
	movq	%rsp, %rbp
LCFI47:
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	120+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI48:
	ret
LFE12:
.no_dead_strip _Divide__Divide_var_u16_I
_Divide__Divide_var_u16_I:
LFB13:
	pushq	%rbp
LCFI49:
	movq	%rsp, %rbp
LCFI50:
	pushq	%rbx
LCFI51:
	subq	$8, %rsp
LCFI52:
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ebx
	movq	120+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L29
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L29
	subq	$1, %rax
	addq	%rcx, %rdx
L29:
	testq	%rax, %rax
	js	L30
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ebx
	movq	120+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L31
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L31
	subq	$1, %rax
	addq	%rcx, %rdx
L31:
	cmpq	$65535, %rax
	jle	L32
L30:
	movl	$2145, %edi
	call	__m3_fault
L32:
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ebx
	movq	120+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L33
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L33
	subq	$1, %rax
	addq	%rcx, %rdx
L33:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI53:
	ret
LFE13:
.no_dead_strip _Divide__uDivide_param_u16_I
_Divide__uDivide_param_u16_I:
LFB14:
	pushq	%rbp
LCFI54:
	movq	%rsp, %rbp
LCFI55:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movzwl	-2(%rbp), %eax
	movq	-16(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI56:
	ret
LFE14:
.no_dead_strip _Divide__Divide_param_u16_I
_Divide__Divide_param_u16_I:
LFB15:
	pushq	%rbp
LCFI57:
	movq	%rsp, %rbp
LCFI58:
	subq	$16, %rsp
LCFI59:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movzwl	-2(%rbp), %ecx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L36
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L36
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L36:
	testq	%rax, %rax
	js	L37
	movzwl	-2(%rbp), %ecx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L38
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L38
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L38:
	cmpq	$65535, %rax
	jle	L39
L37:
	movl	$2209, %edi
	call	__m3_fault
L39:
	movzwl	-2(%rbp), %ecx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L40
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L40
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L40:
	leave
LCFI60:
	ret
LFE15:
.no_dead_strip _Divide__uDivide_var_u16_i16
_Divide__uDivide_var_u16_i16:
LFB16:
	pushq	%rbp
LCFI61:
	movq	%rsp, %rbp
LCFI62:
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movzwl	128+_MM_Divide(%rip), %edx
	movswq	%dx, %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI63:
	ret
LFE16:
.no_dead_strip _Divide__Divide_var_u16_i16
_Divide__Divide_var_u16_i16:
LFB17:
	pushq	%rbp
LCFI64:
	movq	%rsp, %rbp
LCFI65:
	pushq	%rbx
LCFI66:
	subq	$8, %rsp
LCFI67:
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ebx
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L43
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L43
	subq	$1, %rax
	addq	%rcx, %rdx
L43:
	testq	%rax, %rax
	js	L44
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ebx
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L45
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L45
	subq	$1, %rax
	addq	%rcx, %rdx
L45:
	cmpq	$65535, %rax
	jle	L46
L44:
	movl	$2273, %edi
	call	__m3_fault
L46:
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ebx
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L47
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L47
	subq	$1, %rax
	addq	%rcx, %rdx
L47:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI68:
	ret
LFE17:
.no_dead_strip _Divide__uDivide_param_u16_i16
_Divide__uDivide_param_u16_i16:
LFB18:
	pushq	%rbp
LCFI69:
	movq	%rsp, %rbp
LCFI70:
	movw	%di, -2(%rbp)
	movw	%si, -4(%rbp)
	movzwl	-2(%rbp), %eax
	movswq	-4(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI71:
	ret
LFE18:
.no_dead_strip _Divide__Divide_param_u16_i16
_Divide__Divide_param_u16_i16:
LFB19:
	pushq	%rbp
LCFI72:
	movq	%rsp, %rbp
LCFI73:
	pushq	%rbx
LCFI74:
	subq	$24, %rsp
LCFI75:
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %ebx
	movswq	-20(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L50
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L50
	subq	$1, %rax
	addq	%rcx, %rdx
L50:
	testq	%rax, %rax
	js	L51
	movzwl	-18(%rbp), %ebx
	movswq	-20(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L52
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L52
	subq	$1, %rax
	addq	%rcx, %rdx
L52:
	cmpq	$65535, %rax
	jle	L53
L51:
	movl	$2337, %edi
	call	__m3_fault
L53:
	movzwl	-18(%rbp), %ebx
	movswq	-20(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L54
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L54
	subq	$1, %rax
	addq	%rcx, %rdx
L54:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI76:
	ret
LFE19:
.no_dead_strip _Divide__uDivide_var_u16_u64
_Divide__uDivide_var_u16_u64:
LFB20:
	pushq	%rbp
LCFI77:
	movq	%rsp, %rbp
LCFI78:
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	136+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI79:
	ret
LFE20:
.no_dead_strip _Divide__Divide_var_u16_u64
_Divide__Divide_var_u16_u64:
LFB21:
	pushq	%rbp
LCFI80:
	movq	%rsp, %rbp
LCFI81:
	pushq	%rbx
LCFI82:
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ebx
	movq	136+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L57
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L57
	subq	$1, %rax
	addq	%rcx, %rdx
L57:
	popq	%rbx
	leave
LCFI83:
	ret
LFE21:
.no_dead_strip _Divide__uDivide_param_u16_u64
_Divide__uDivide_param_u16_u64:
LFB22:
	pushq	%rbp
LCFI84:
	movq	%rsp, %rbp
LCFI85:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movzwl	-2(%rbp), %eax
	movq	-16(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI86:
	ret
LFE22:
.no_dead_strip _Divide__Divide_param_u16_u64
_Divide__Divide_param_u16_u64:
LFB23:
	pushq	%rbp
LCFI87:
	movq	%rsp, %rbp
LCFI88:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movzwl	-2(%rbp), %ecx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L60
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L60
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L60:
	leave
LCFI89:
	ret
LFE23:
.no_dead_strip _Divide__uDivide_var_u16_i32
_Divide__uDivide_var_u16_i32:
LFB24:
	pushq	%rbp
LCFI90:
	movq	%rsp, %rbp
LCFI91:
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movl	152+_MM_Divide(%rip), %edx
	movslq	%edx, %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI92:
	ret
LFE24:
.no_dead_strip _Divide__Divide_var_u16_i32
_Divide__Divide_var_u16_i32:
LFB25:
	pushq	%rbp
LCFI93:
	movq	%rsp, %rbp
LCFI94:
	pushq	%rbx
LCFI95:
	subq	$8, %rsp
LCFI96:
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ebx
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L63
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L63
	subq	$1, %rax
	addq	%rcx, %rdx
L63:
	testq	%rax, %rax
	js	L64
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ebx
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L65
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L65
	subq	$1, %rax
	addq	%rcx, %rdx
L65:
	cmpq	$65535, %rax
	jle	L66
L64:
	movl	$2529, %edi
	call	__m3_fault
L66:
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ebx
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L67
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L67
	subq	$1, %rax
	addq	%rcx, %rdx
L67:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI97:
	ret
LFE25:
.no_dead_strip _Divide__uDivide_param_u16_i32
_Divide__uDivide_param_u16_i32:
LFB26:
	pushq	%rbp
LCFI98:
	movq	%rsp, %rbp
LCFI99:
	movw	%di, -2(%rbp)
	movl	%esi, -8(%rbp)
	movzwl	-2(%rbp), %eax
	movl	-8(%rbp), %edx
	movslq	%edx, %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI100:
	ret
LFE26:
.no_dead_strip _Divide__Divide_param_u16_i32
_Divide__Divide_param_u16_i32:
LFB27:
	pushq	%rbp
LCFI101:
	movq	%rsp, %rbp
LCFI102:
	pushq	%rbx
LCFI103:
	subq	$24, %rsp
LCFI104:
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %ebx
	movl	-24(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L70
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L70
	subq	$1, %rax
	addq	%rcx, %rdx
L70:
	testq	%rax, %rax
	js	L71
	movzwl	-18(%rbp), %ebx
	movl	-24(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L72
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L72
	subq	$1, %rax
	addq	%rcx, %rdx
L72:
	cmpq	$65535, %rax
	jle	L73
L71:
	movl	$2593, %edi
	call	__m3_fault
L73:
	movzwl	-18(%rbp), %ebx
	movl	-24(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L74
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L74
	subq	$1, %rax
	addq	%rcx, %rdx
L74:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI105:
	ret
LFE27:
.no_dead_strip _Divide__uDivide_var_u16_C
_Divide__uDivide_var_u16_C:
LFB28:
	pushq	%rbp
LCFI106:
	movq	%rsp, %rbp
LCFI107:
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	160+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI108:
	ret
LFE28:
.no_dead_strip _Divide__Divide_var_u16_C
_Divide__Divide_var_u16_C:
LFB29:
	pushq	%rbp
LCFI109:
	movq	%rsp, %rbp
LCFI110:
	pushq	%rbx
LCFI111:
	subq	$8, %rsp
LCFI112:
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ebx
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L77
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L77
	subq	$1, %rax
	addq	%rcx, %rdx
L77:
	testq	%rax, %rax
	js	L78
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ebx
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L79
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L79
	subq	$1, %rax
	addq	%rcx, %rdx
L79:
	cmpq	$65535, %rax
	jle	L80
L78:
	movl	$2657, %edi
	call	__m3_fault
L80:
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ebx
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L81
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L81
	subq	$1, %rax
	addq	%rcx, %rdx
L81:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI113:
	ret
LFE29:
.no_dead_strip _Divide__uDivide_param_u16_C
_Divide__uDivide_param_u16_C:
LFB30:
	pushq	%rbp
LCFI114:
	movq	%rsp, %rbp
LCFI115:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movzwl	-2(%rbp), %eax
	movl	$0, %edx
	divq	-16(%rbp)
	leave
LCFI116:
	ret
LFE30:
.no_dead_strip _Divide__Divide_param_u16_C
_Divide__Divide_param_u16_C:
LFB31:
	pushq	%rbp
LCFI117:
	movq	%rsp, %rbp
LCFI118:
	pushq	%rbx
LCFI119:
	subq	$24, %rsp
LCFI120:
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %ebx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L84
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L84
	subq	$1, %rax
	addq	%rcx, %rdx
L84:
	testq	%rax, %rax
	js	L85
	movzwl	-18(%rbp), %ebx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L86
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L86
	subq	$1, %rax
	addq	%rcx, %rdx
L86:
	cmpq	$65535, %rax
	jle	L87
L85:
	movl	$2721, %edi
	call	__m3_fault
L87:
	movzwl	-18(%rbp), %ebx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L88
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L88
	subq	$1, %rax
	addq	%rcx, %rdx
L88:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI121:
	ret
LFE31:
.no_dead_strip _Divide__uDivide_var_u16_u8
_Divide__uDivide_var_u16_u8:
LFB32:
	pushq	%rbp
LCFI122:
	movq	%rsp, %rbp
LCFI123:
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movzbl	168+_MM_Divide(%rip), %edx
	movzbl	%dl, %edx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI124:
	ret
LFE32:
.no_dead_strip _Divide__Divide_var_u16_u8
_Divide__Divide_var_u16_u8:
LFB33:
	pushq	%rbp
LCFI125:
	movq	%rsp, %rbp
LCFI126:
	pushq	%rbx
LCFI127:
	subq	$8, %rsp
LCFI128:
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ebx
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L91
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L91
	subq	$1, %rax
	addq	%rcx, %rdx
L91:
	cmpq	$65535, %rax
	jle	L92
	movl	$2785, %edi
	call	__m3_fault
L92:
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ebx
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L93
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L93
	subq	$1, %rax
	addq	%rcx, %rdx
L93:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI129:
	ret
LFE33:
.no_dead_strip _Divide__uDivide_param_u16_u8
_Divide__uDivide_param_u16_u8:
LFB34:
	pushq	%rbp
LCFI130:
	movq	%rsp, %rbp
LCFI131:
	movw	%di, -2(%rbp)
	movb	%sil, -3(%rbp)
	movzwl	-2(%rbp), %eax
	movzbl	-3(%rbp), %edx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI132:
	ret
LFE34:
.no_dead_strip _Divide__Divide_param_u16_u8
_Divide__Divide_param_u16_u8:
LFB35:
	pushq	%rbp
LCFI133:
	movq	%rsp, %rbp
LCFI134:
	pushq	%rbx
LCFI135:
	subq	$24, %rsp
LCFI136:
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %ebx
	movzbl	-19(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L96
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L96
	subq	$1, %rax
	addq	%rcx, %rdx
L96:
	cmpq	$65535, %rax
	jle	L97
	movl	$2849, %edi
	call	__m3_fault
L97:
	movzwl	-18(%rbp), %ebx
	movzbl	-19(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L98
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L98
	subq	$1, %rax
	addq	%rcx, %rdx
L98:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI137:
	ret
LFE35:
.no_dead_strip _Divide__uDivide_var_u16_i64
_Divide__uDivide_var_u16_i64:
LFB36:
	pushq	%rbp
LCFI138:
	movq	%rsp, %rbp
LCFI139:
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	176+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI140:
	ret
LFE36:
.no_dead_strip _Divide__Divide_var_u16_i64
_Divide__Divide_var_u16_i64:
LFB37:
	pushq	%rbp
LCFI141:
	movq	%rsp, %rbp
LCFI142:
	pushq	%rbx
LCFI143:
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ebx
	movq	176+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L101
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L101
	subq	$1, %rax
	addq	%rcx, %rdx
L101:
	popq	%rbx
	leave
LCFI144:
	ret
LFE37:
.no_dead_strip _Divide__uDivide_param_u16_i64
_Divide__uDivide_param_u16_i64:
LFB38:
	pushq	%rbp
LCFI145:
	movq	%rsp, %rbp
LCFI146:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movzwl	-2(%rbp), %eax
	movq	-16(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI147:
	ret
LFE38:
.no_dead_strip _Divide__Divide_param_u16_i64
_Divide__Divide_param_u16_i64:
LFB39:
	pushq	%rbp
LCFI148:
	movq	%rsp, %rbp
LCFI149:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movzwl	-2(%rbp), %ecx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L104
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L104
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L104:
	leave
LCFI150:
	ret
LFE39:
.no_dead_strip _Divide__uDivide_var_u16_L
_Divide__uDivide_var_u16_L:
LFB40:
	pushq	%rbp
LCFI151:
	movq	%rsp, %rbp
LCFI152:
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movq	192+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI153:
	ret
LFE40:
.no_dead_strip _Divide__Divide_var_u16_L
_Divide__Divide_var_u16_L:
LFB41:
	pushq	%rbp
LCFI154:
	movq	%rsp, %rbp
LCFI155:
	pushq	%rbx
LCFI156:
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ebx
	movq	192+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L107
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L107
	subq	$1, %rax
	addq	%rcx, %rdx
L107:
	popq	%rbx
	leave
LCFI157:
	ret
LFE41:
.no_dead_strip _Divide__uDivide_param_u16_L
_Divide__uDivide_param_u16_L:
LFB42:
	pushq	%rbp
LCFI158:
	movq	%rsp, %rbp
LCFI159:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movzwl	-2(%rbp), %eax
	movq	-16(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI160:
	ret
LFE42:
.no_dead_strip _Divide__Divide_param_u16_L
_Divide__Divide_param_u16_L:
LFB43:
	pushq	%rbp
LCFI161:
	movq	%rsp, %rbp
LCFI162:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movzwl	-2(%rbp), %ecx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L110
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L110
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L110:
	leave
LCFI163:
	ret
LFE43:
.no_dead_strip _Divide__uDivide_var_u16_i8
_Divide__uDivide_var_u16_i8:
LFB44:
	pushq	%rbp
LCFI164:
	movq	%rsp, %rbp
LCFI165:
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %eax
	movzbl	200+_MM_Divide(%rip), %edx
	movsbq	%dl, %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI166:
	ret
LFE44:
.no_dead_strip _Divide__Divide_var_u16_i8
_Divide__Divide_var_u16_i8:
LFB45:
	pushq	%rbp
LCFI167:
	movq	%rsp, %rbp
LCFI168:
	pushq	%rbx
LCFI169:
	subq	$8, %rsp
LCFI170:
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ebx
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L113
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L113
	subq	$1, %rax
	addq	%rcx, %rdx
L113:
	testq	%rax, %rax
	js	L114
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ebx
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L115
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L115
	subq	$1, %rax
	addq	%rcx, %rdx
L115:
	cmpq	$65535, %rax
	jle	L116
L114:
	movl	$3169, %edi
	call	__m3_fault
L116:
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ebx
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L117
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L117
	subq	$1, %rax
	addq	%rcx, %rdx
L117:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI171:
	ret
LFE45:
.no_dead_strip _Divide__uDivide_param_u16_i8
_Divide__uDivide_param_u16_i8:
LFB46:
	pushq	%rbp
LCFI172:
	movq	%rsp, %rbp
LCFI173:
	movw	%di, -2(%rbp)
	movb	%sil, -3(%rbp)
	movzwl	-2(%rbp), %eax
	movsbq	-3(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI174:
	ret
LFE46:
.no_dead_strip _Divide__Divide_param_u16_i8
_Divide__Divide_param_u16_i8:
LFB47:
	pushq	%rbp
LCFI175:
	movq	%rsp, %rbp
LCFI176:
	pushq	%rbx
LCFI177:
	subq	$24, %rsp
LCFI178:
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %ebx
	movsbq	-19(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L120
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L120
	subq	$1, %rax
	addq	%rcx, %rdx
L120:
	testq	%rax, %rax
	js	L121
	movzwl	-18(%rbp), %ebx
	movsbq	-19(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L122
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L122
	subq	$1, %rax
	addq	%rcx, %rdx
L122:
	cmpq	$65535, %rax
	jle	L123
L121:
	movl	$3233, %edi
	call	__m3_fault
L123:
	movzwl	-18(%rbp), %ebx
	movsbq	-19(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L124
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L124
	subq	$1, %rax
	addq	%rcx, %rdx
L124:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI179:
	ret
LFE47:
.no_dead_strip _Divide__uDivide_var_u32_u16
_Divide__uDivide_var_u32_u16:
LFB48:
	pushq	%rbp
LCFI180:
	movq	%rsp, %rbp
LCFI181:
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movzwl	104+_MM_Divide(%rip), %edx
	movzwl	%dx, %edx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI182:
	ret
LFE48:
.no_dead_strip _Divide__Divide_var_u32_u16
_Divide__Divide_var_u32_u16:
LFB49:
	pushq	%rbp
LCFI183:
	movq	%rsp, %rbp
LCFI184:
	pushq	%rbx
LCFI185:
	subq	$8, %rsp
LCFI186:
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ebx
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L127
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L127
	subq	$1, %rax
	addq	%rcx, %rdx
L127:
	movl	$4294967295, %edx
	cmpq	%rdx, %rax
	jle	L128
	movl	$3297, %edi
	call	__m3_fault
L128:
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ebx
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L129
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L129
	subq	$1, %rax
	addq	%rcx, %rdx
L129:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI187:
	ret
LFE49:
.no_dead_strip _Divide__uDivide_param_u32_u16
_Divide__uDivide_param_u32_u16:
LFB50:
	pushq	%rbp
LCFI188:
	movq	%rsp, %rbp
LCFI189:
	movl	%edi, -4(%rbp)
	movw	%si, -6(%rbp)
	mov	-4(%rbp), %eax
	movzwl	-6(%rbp), %edx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI190:
	ret
LFE50:
.no_dead_strip _Divide__Divide_param_u32_u16
_Divide__Divide_param_u32_u16:
LFB51:
	pushq	%rbp
LCFI191:
	movq	%rsp, %rbp
LCFI192:
	pushq	%rbx
LCFI193:
	subq	$24, %rsp
LCFI194:
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %ebx
	movzwl	-22(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L132
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L132
	subq	$1, %rax
	addq	%rcx, %rdx
L132:
	movl	$4294967295, %edx
	cmpq	%rdx, %rax
	jle	L133
	movl	$3361, %edi
	call	__m3_fault
L133:
	mov	-20(%rbp), %ebx
	movzwl	-22(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L134
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L134
	subq	$1, %rax
	addq	%rcx, %rdx
L134:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI195:
	ret
LFE51:
.no_dead_strip _Divide__uDivide_var_u32_u32
_Divide__uDivide_var_u32_u32:
LFB52:
	pushq	%rbp
LCFI196:
	movq	%rsp, %rbp
LCFI197:
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movl	108+_MM_Divide(%rip), %edx
	mov	%edx, %edx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI198:
	ret
LFE52:
.no_dead_strip _Divide__Divide_var_u32_u32
_Divide__Divide_var_u32_u32:
LFB53:
	pushq	%rbp
LCFI199:
	movq	%rsp, %rbp
LCFI200:
	pushq	%rbx
LCFI201:
	subq	$8, %rsp
LCFI202:
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ebx
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L137
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L137
	subq	$1, %rax
	addq	%rcx, %rdx
L137:
	movl	$4294967295, %edx
	cmpq	%rdx, %rax
	jle	L138
	movl	$3425, %edi
	call	__m3_fault
L138:
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ebx
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L139
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L139
	subq	$1, %rax
	addq	%rcx, %rdx
L139:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI203:
	ret
LFE53:
.no_dead_strip _Divide__uDivide_param_u32_u32
_Divide__uDivide_param_u32_u32:
LFB54:
	pushq	%rbp
LCFI204:
	movq	%rsp, %rbp
LCFI205:
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	mov	-4(%rbp), %eax
	mov	-8(%rbp), %edx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI206:
	ret
LFE54:
.no_dead_strip _Divide__Divide_param_u32_u32
_Divide__Divide_param_u32_u32:
LFB55:
	pushq	%rbp
LCFI207:
	movq	%rsp, %rbp
LCFI208:
	pushq	%rbx
LCFI209:
	subq	$24, %rsp
LCFI210:
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %ebx
	mov	-24(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L142
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L142
	subq	$1, %rax
	addq	%rcx, %rdx
L142:
	movl	$4294967295, %edx
	cmpq	%rdx, %rax
	jle	L143
	movl	$3489, %edi
	call	__m3_fault
L143:
	mov	-20(%rbp), %ebx
	mov	-24(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L144
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L144
	subq	$1, %rax
	addq	%rcx, %rdx
L144:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI211:
	ret
LFE55:
.no_dead_strip _Divide__uDivide_var_u32_LC
_Divide__uDivide_var_u32_LC:
LFB56:
	pushq	%rbp
LCFI212:
	movq	%rsp, %rbp
LCFI213:
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	112+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI214:
	ret
LFE56:
.no_dead_strip _Divide__Divide_var_u32_LC
_Divide__Divide_var_u32_LC:
LFB57:
	pushq	%rbp
LCFI215:
	movq	%rsp, %rbp
LCFI216:
	pushq	%rbx
LCFI217:
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ebx
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L147
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L147
	subq	$1, %rax
	addq	%rcx, %rdx
L147:
	popq	%rbx
	leave
LCFI218:
	ret
LFE57:
.no_dead_strip _Divide__uDivide_param_u32_LC
_Divide__uDivide_param_u32_LC:
LFB58:
	pushq	%rbp
LCFI219:
	movq	%rsp, %rbp
LCFI220:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	mov	-4(%rbp), %eax
	movl	$0, %edx
	divq	-16(%rbp)
	leave
LCFI221:
	ret
LFE58:
.no_dead_strip _Divide__Divide_param_u32_LC
_Divide__Divide_param_u32_LC:
LFB59:
	pushq	%rbp
LCFI222:
	movq	%rsp, %rbp
LCFI223:
	pushq	%rbx
LCFI224:
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %ebx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L150
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L150
	subq	$1, %rax
	addq	%rcx, %rdx
L150:
	popq	%rbx
	leave
LCFI225:
	ret
LFE59:
.no_dead_strip _Divide__uDivide_var_u32_I
_Divide__uDivide_var_u32_I:
LFB60:
	pushq	%rbp
LCFI226:
	movq	%rsp, %rbp
LCFI227:
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	120+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI228:
	ret
LFE60:
.no_dead_strip _Divide__Divide_var_u32_I
_Divide__Divide_var_u32_I:
LFB61:
	pushq	%rbp
LCFI229:
	movq	%rsp, %rbp
LCFI230:
	pushq	%rbx
LCFI231:
	subq	$8, %rsp
LCFI232:
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ebx
	movq	120+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L153
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L153
	subq	$1, %rax
	addq	%rcx, %rdx
L153:
	testq	%rax, %rax
	js	L154
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ebx
	movq	120+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L155
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L155
	subq	$1, %rax
	addq	%rcx, %rdx
L155:
	movl	$4294967295, %edx
	cmpq	%rdx, %rax
	jle	L156
L154:
	movl	$3681, %edi
	call	__m3_fault
L156:
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ebx
	movq	120+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L157
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L157
	subq	$1, %rax
	addq	%rcx, %rdx
L157:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI233:
	ret
LFE61:
.no_dead_strip _Divide__uDivide_param_u32_I
_Divide__uDivide_param_u32_I:
LFB62:
	pushq	%rbp
LCFI234:
	movq	%rsp, %rbp
LCFI235:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	mov	-4(%rbp), %eax
	movq	-16(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI236:
	ret
LFE62:
.no_dead_strip _Divide__Divide_param_u32_I
_Divide__Divide_param_u32_I:
LFB63:
	pushq	%rbp
LCFI237:
	movq	%rsp, %rbp
LCFI238:
	subq	$16, %rsp
LCFI239:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	mov	-4(%rbp), %ecx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L160
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L160
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L160:
	testq	%rax, %rax
	js	L161
	mov	-4(%rbp), %ecx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L162
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L162
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L162:
	movl	$4294967295, %edx
	cmpq	%rdx, %rax
	jle	L163
L161:
	movl	$3745, %edi
	call	__m3_fault
L163:
	mov	-4(%rbp), %ecx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L164
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L164
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L164:
	leave
LCFI240:
	ret
LFE63:
.no_dead_strip _Divide__uDivide_var_u32_i16
_Divide__uDivide_var_u32_i16:
LFB64:
	pushq	%rbp
LCFI241:
	movq	%rsp, %rbp
LCFI242:
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movzwl	128+_MM_Divide(%rip), %edx
	movswq	%dx, %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI243:
	ret
LFE64:
.no_dead_strip _Divide__Divide_var_u32_i16
_Divide__Divide_var_u32_i16:
LFB65:
	pushq	%rbp
LCFI244:
	movq	%rsp, %rbp
LCFI245:
	pushq	%rbx
LCFI246:
	subq	$8, %rsp
LCFI247:
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ebx
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L167
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L167
	subq	$1, %rax
	addq	%rcx, %rdx
L167:
	testq	%rax, %rax
	js	L168
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ebx
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L169
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L169
	subq	$1, %rax
	addq	%rcx, %rdx
L169:
	movl	$4294967295, %edx
	cmpq	%rdx, %rax
	jle	L170
L168:
	movl	$3809, %edi
	call	__m3_fault
L170:
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ebx
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L171
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L171
	subq	$1, %rax
	addq	%rcx, %rdx
L171:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI248:
	ret
LFE65:
.no_dead_strip _Divide__uDivide_param_u32_i16
_Divide__uDivide_param_u32_i16:
LFB66:
	pushq	%rbp
LCFI249:
	movq	%rsp, %rbp
LCFI250:
	movl	%edi, -4(%rbp)
	movw	%si, -6(%rbp)
	mov	-4(%rbp), %eax
	movswq	-6(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI251:
	ret
LFE66:
.no_dead_strip _Divide__Divide_param_u32_i16
_Divide__Divide_param_u32_i16:
LFB67:
	pushq	%rbp
LCFI252:
	movq	%rsp, %rbp
LCFI253:
	pushq	%rbx
LCFI254:
	subq	$24, %rsp
LCFI255:
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %ebx
	movswq	-22(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L174
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L174
	subq	$1, %rax
	addq	%rcx, %rdx
L174:
	testq	%rax, %rax
	js	L175
	mov	-20(%rbp), %ebx
	movswq	-22(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L176
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L176
	subq	$1, %rax
	addq	%rcx, %rdx
L176:
	movl	$4294967295, %edx
	cmpq	%rdx, %rax
	jle	L177
L175:
	movl	$3873, %edi
	call	__m3_fault
L177:
	mov	-20(%rbp), %ebx
	movswq	-22(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L178
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L178
	subq	$1, %rax
	addq	%rcx, %rdx
L178:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI256:
	ret
LFE67:
.no_dead_strip _Divide__uDivide_var_u32_u64
_Divide__uDivide_var_u32_u64:
LFB68:
	pushq	%rbp
LCFI257:
	movq	%rsp, %rbp
LCFI258:
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	136+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI259:
	ret
LFE68:
.no_dead_strip _Divide__Divide_var_u32_u64
_Divide__Divide_var_u32_u64:
LFB69:
	pushq	%rbp
LCFI260:
	movq	%rsp, %rbp
LCFI261:
	pushq	%rbx
LCFI262:
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ebx
	movq	136+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L181
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L181
	subq	$1, %rax
	addq	%rcx, %rdx
L181:
	popq	%rbx
	leave
LCFI263:
	ret
LFE69:
.no_dead_strip _Divide__uDivide_param_u32_u64
_Divide__uDivide_param_u32_u64:
LFB70:
	pushq	%rbp
LCFI264:
	movq	%rsp, %rbp
LCFI265:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	mov	-4(%rbp), %eax
	movq	-16(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI266:
	ret
LFE70:
.no_dead_strip _Divide__Divide_param_u32_u64
_Divide__Divide_param_u32_u64:
LFB71:
	pushq	%rbp
LCFI267:
	movq	%rsp, %rbp
LCFI268:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	mov	-4(%rbp), %ecx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L184
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L184
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L184:
	leave
LCFI269:
	ret
LFE71:
.no_dead_strip _Divide__uDivide_var_u32_i32
_Divide__uDivide_var_u32_i32:
LFB72:
	pushq	%rbp
LCFI270:
	movq	%rsp, %rbp
LCFI271:
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movl	152+_MM_Divide(%rip), %edx
	movslq	%edx, %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI272:
	ret
LFE72:
.no_dead_strip _Divide__Divide_var_u32_i32
_Divide__Divide_var_u32_i32:
LFB73:
	pushq	%rbp
LCFI273:
	movq	%rsp, %rbp
LCFI274:
	pushq	%rbx
LCFI275:
	subq	$8, %rsp
LCFI276:
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ebx
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L187
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L187
	subq	$1, %rax
	addq	%rcx, %rdx
L187:
	testq	%rax, %rax
	js	L188
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ebx
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L189
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L189
	subq	$1, %rax
	addq	%rcx, %rdx
L189:
	movl	$4294967295, %edx
	cmpq	%rdx, %rax
	jle	L190
L188:
	movl	$4065, %edi
	call	__m3_fault
L190:
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ebx
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L191
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L191
	subq	$1, %rax
	addq	%rcx, %rdx
L191:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI277:
	ret
LFE73:
.no_dead_strip _Divide__uDivide_param_u32_i32
_Divide__uDivide_param_u32_i32:
LFB74:
	pushq	%rbp
LCFI278:
	movq	%rsp, %rbp
LCFI279:
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	mov	-4(%rbp), %eax
	movl	-8(%rbp), %edx
	movslq	%edx, %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI280:
	ret
LFE74:
.no_dead_strip _Divide__Divide_param_u32_i32
_Divide__Divide_param_u32_i32:
LFB75:
	pushq	%rbp
LCFI281:
	movq	%rsp, %rbp
LCFI282:
	pushq	%rbx
LCFI283:
	subq	$24, %rsp
LCFI284:
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %ebx
	movl	-24(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L194
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L194
	subq	$1, %rax
	addq	%rcx, %rdx
L194:
	testq	%rax, %rax
	js	L195
	mov	-20(%rbp), %ebx
	movl	-24(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L196
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L196
	subq	$1, %rax
	addq	%rcx, %rdx
L196:
	movl	$4294967295, %edx
	cmpq	%rdx, %rax
	jle	L197
L195:
	movl	$4129, %edi
	call	__m3_fault
L197:
	mov	-20(%rbp), %ebx
	movl	-24(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L198
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L198
	subq	$1, %rax
	addq	%rcx, %rdx
L198:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI285:
	ret
LFE75:
.no_dead_strip _Divide__uDivide_var_u32_C
_Divide__uDivide_var_u32_C:
LFB76:
	pushq	%rbp
LCFI286:
	movq	%rsp, %rbp
LCFI287:
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	160+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI288:
	ret
LFE76:
.no_dead_strip _Divide__Divide_var_u32_C
_Divide__Divide_var_u32_C:
LFB77:
	pushq	%rbp
LCFI289:
	movq	%rsp, %rbp
LCFI290:
	pushq	%rbx
LCFI291:
	subq	$8, %rsp
LCFI292:
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ebx
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L201
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L201
	subq	$1, %rax
	addq	%rcx, %rdx
L201:
	testq	%rax, %rax
	js	L202
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ebx
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L203
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L203
	subq	$1, %rax
	addq	%rcx, %rdx
L203:
	movl	$4294967295, %edx
	cmpq	%rdx, %rax
	jle	L204
L202:
	movl	$4193, %edi
	call	__m3_fault
L204:
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ebx
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L205
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L205
	subq	$1, %rax
	addq	%rcx, %rdx
L205:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI293:
	ret
LFE77:
.no_dead_strip _Divide__uDivide_param_u32_C
_Divide__uDivide_param_u32_C:
LFB78:
	pushq	%rbp
LCFI294:
	movq	%rsp, %rbp
LCFI295:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	mov	-4(%rbp), %eax
	movl	$0, %edx
	divq	-16(%rbp)
	leave
LCFI296:
	ret
LFE78:
.no_dead_strip _Divide__Divide_param_u32_C
_Divide__Divide_param_u32_C:
LFB79:
	pushq	%rbp
LCFI297:
	movq	%rsp, %rbp
LCFI298:
	pushq	%rbx
LCFI299:
	subq	$24, %rsp
LCFI300:
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %ebx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L208
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L208
	subq	$1, %rax
	addq	%rcx, %rdx
L208:
	testq	%rax, %rax
	js	L209
	mov	-20(%rbp), %ebx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L210
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L210
	subq	$1, %rax
	addq	%rcx, %rdx
L210:
	movl	$4294967295, %edx
	cmpq	%rdx, %rax
	jle	L211
L209:
	movl	$4257, %edi
	call	__m3_fault
L211:
	mov	-20(%rbp), %ebx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L212
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L212
	subq	$1, %rax
	addq	%rcx, %rdx
L212:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI301:
	ret
LFE79:
.no_dead_strip _Divide__uDivide_var_u32_u8
_Divide__uDivide_var_u32_u8:
LFB80:
	pushq	%rbp
LCFI302:
	movq	%rsp, %rbp
LCFI303:
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movzbl	168+_MM_Divide(%rip), %edx
	movzbl	%dl, %edx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI304:
	ret
LFE80:
.no_dead_strip _Divide__Divide_var_u32_u8
_Divide__Divide_var_u32_u8:
LFB81:
	pushq	%rbp
LCFI305:
	movq	%rsp, %rbp
LCFI306:
	pushq	%rbx
LCFI307:
	subq	$8, %rsp
LCFI308:
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ebx
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L215
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L215
	subq	$1, %rax
	addq	%rcx, %rdx
L215:
	movl	$4294967295, %edx
	cmpq	%rdx, %rax
	jle	L216
	movl	$4321, %edi
	call	__m3_fault
L216:
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ebx
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L217
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L217
	subq	$1, %rax
	addq	%rcx, %rdx
L217:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI309:
	ret
LFE81:
.no_dead_strip _Divide__uDivide_param_u32_u8
_Divide__uDivide_param_u32_u8:
LFB82:
	pushq	%rbp
LCFI310:
	movq	%rsp, %rbp
LCFI311:
	movl	%edi, -4(%rbp)
	movb	%sil, -5(%rbp)
	mov	-4(%rbp), %eax
	movzbl	-5(%rbp), %edx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI312:
	ret
LFE82:
.no_dead_strip _Divide__Divide_param_u32_u8
_Divide__Divide_param_u32_u8:
LFB83:
	pushq	%rbp
LCFI313:
	movq	%rsp, %rbp
LCFI314:
	pushq	%rbx
LCFI315:
	subq	$24, %rsp
LCFI316:
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %ebx
	movzbl	-21(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L220
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L220
	subq	$1, %rax
	addq	%rcx, %rdx
L220:
	movl	$4294967295, %edx
	cmpq	%rdx, %rax
	jle	L221
	movl	$4385, %edi
	call	__m3_fault
L221:
	mov	-20(%rbp), %ebx
	movzbl	-21(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L222
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L222
	subq	$1, %rax
	addq	%rcx, %rdx
L222:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI317:
	ret
LFE83:
.no_dead_strip _Divide__uDivide_var_u32_i64
_Divide__uDivide_var_u32_i64:
LFB84:
	pushq	%rbp
LCFI318:
	movq	%rsp, %rbp
LCFI319:
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	176+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI320:
	ret
LFE84:
.no_dead_strip _Divide__Divide_var_u32_i64
_Divide__Divide_var_u32_i64:
LFB85:
	pushq	%rbp
LCFI321:
	movq	%rsp, %rbp
LCFI322:
	pushq	%rbx
LCFI323:
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ebx
	movq	176+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L225
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L225
	subq	$1, %rax
	addq	%rcx, %rdx
L225:
	popq	%rbx
	leave
LCFI324:
	ret
LFE85:
.no_dead_strip _Divide__uDivide_param_u32_i64
_Divide__uDivide_param_u32_i64:
LFB86:
	pushq	%rbp
LCFI325:
	movq	%rsp, %rbp
LCFI326:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	mov	-4(%rbp), %eax
	movq	-16(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI327:
	ret
LFE86:
.no_dead_strip _Divide__Divide_param_u32_i64
_Divide__Divide_param_u32_i64:
LFB87:
	pushq	%rbp
LCFI328:
	movq	%rsp, %rbp
LCFI329:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	mov	-4(%rbp), %ecx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L228
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L228
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L228:
	leave
LCFI330:
	ret
LFE87:
.no_dead_strip _Divide__uDivide_var_u32_L
_Divide__uDivide_var_u32_L:
LFB88:
	pushq	%rbp
LCFI331:
	movq	%rsp, %rbp
LCFI332:
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movq	192+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI333:
	ret
LFE88:
.no_dead_strip _Divide__Divide_var_u32_L
_Divide__Divide_var_u32_L:
LFB89:
	pushq	%rbp
LCFI334:
	movq	%rsp, %rbp
LCFI335:
	pushq	%rbx
LCFI336:
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ebx
	movq	192+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L231
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L231
	subq	$1, %rax
	addq	%rcx, %rdx
L231:
	popq	%rbx
	leave
LCFI337:
	ret
LFE89:
.no_dead_strip _Divide__uDivide_param_u32_L
_Divide__uDivide_param_u32_L:
LFB90:
	pushq	%rbp
LCFI338:
	movq	%rsp, %rbp
LCFI339:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	mov	-4(%rbp), %eax
	movq	-16(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI340:
	ret
LFE90:
.no_dead_strip _Divide__Divide_param_u32_L
_Divide__Divide_param_u32_L:
LFB91:
	pushq	%rbp
LCFI341:
	movq	%rsp, %rbp
LCFI342:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	mov	-4(%rbp), %ecx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L234
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L234
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L234:
	leave
LCFI343:
	ret
LFE91:
.no_dead_strip _Divide__uDivide_var_u32_i8
_Divide__uDivide_var_u32_i8:
LFB92:
	pushq	%rbp
LCFI344:
	movq	%rsp, %rbp
LCFI345:
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %eax
	movzbl	200+_MM_Divide(%rip), %edx
	movsbq	%dl, %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI346:
	ret
LFE92:
.no_dead_strip _Divide__Divide_var_u32_i8
_Divide__Divide_var_u32_i8:
LFB93:
	pushq	%rbp
LCFI347:
	movq	%rsp, %rbp
LCFI348:
	pushq	%rbx
LCFI349:
	subq	$8, %rsp
LCFI350:
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ebx
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L237
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L237
	subq	$1, %rax
	addq	%rcx, %rdx
L237:
	testq	%rax, %rax
	js	L238
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ebx
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L239
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L239
	subq	$1, %rax
	addq	%rcx, %rdx
L239:
	movl	$4294967295, %edx
	cmpq	%rdx, %rax
	jle	L240
L238:
	movl	$4705, %edi
	call	__m3_fault
L240:
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ebx
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L241
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L241
	subq	$1, %rax
	addq	%rcx, %rdx
L241:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI351:
	ret
LFE93:
.no_dead_strip _Divide__uDivide_param_u32_i8
_Divide__uDivide_param_u32_i8:
LFB94:
	pushq	%rbp
LCFI352:
	movq	%rsp, %rbp
LCFI353:
	movl	%edi, -4(%rbp)
	movb	%sil, -5(%rbp)
	mov	-4(%rbp), %eax
	movsbq	-5(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI354:
	ret
LFE94:
.no_dead_strip _Divide__Divide_param_u32_i8
_Divide__Divide_param_u32_i8:
LFB95:
	pushq	%rbp
LCFI355:
	movq	%rsp, %rbp
LCFI356:
	pushq	%rbx
LCFI357:
	subq	$24, %rsp
LCFI358:
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %ebx
	movsbq	-21(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L244
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L244
	subq	$1, %rax
	addq	%rcx, %rdx
L244:
	testq	%rax, %rax
	js	L245
	mov	-20(%rbp), %ebx
	movsbq	-21(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L246
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L246
	subq	$1, %rax
	addq	%rcx, %rdx
L246:
	movl	$4294967295, %edx
	cmpq	%rdx, %rax
	jle	L247
L245:
	movl	$4769, %edi
	call	__m3_fault
L247:
	mov	-20(%rbp), %ebx
	movsbq	-21(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L248
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L248
	subq	$1, %rax
	addq	%rcx, %rdx
L248:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI359:
	ret
LFE95:
.no_dead_strip _Divide__uDivide_var_LC_u16
_Divide__uDivide_var_LC_u16:
LFB96:
	pushq	%rbp
LCFI360:
	movq	%rsp, %rbp
LCFI361:
	movq	112+_MM_Divide(%rip), %rax
	movzwl	104+_MM_Divide(%rip), %edx
	movzwl	%dx, %edx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI362:
	ret
LFE96:
.no_dead_strip _Divide__Divide_var_LC_u16
_Divide__Divide_var_LC_u16:
LFB97:
	pushq	%rbp
LCFI363:
	movq	%rsp, %rbp
LCFI364:
	pushq	%rbx
LCFI365:
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, %rbx
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L251
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L251
	subq	$1, %rax
	addq	%rcx, %rdx
L251:
	popq	%rbx
	leave
LCFI366:
	ret
LFE97:
.no_dead_strip _Divide__uDivide_param_LC_u16
_Divide__uDivide_param_LC_u16:
LFB98:
	pushq	%rbp
LCFI367:
	movq	%rsp, %rbp
LCFI368:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movzwl	-10(%rbp), %eax
	movq	%rax, -24(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI369:
	ret
LFE98:
.no_dead_strip _Divide__Divide_param_LC_u16
_Divide__Divide_param_LC_u16:
LFB99:
	pushq	%rbp
LCFI370:
	movq	%rsp, %rbp
LCFI371:
	pushq	%rbx
LCFI372:
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rbx
	movzwl	-26(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L254
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L254
	subq	$1, %rax
	addq	%rcx, %rdx
L254:
	popq	%rbx
	leave
LCFI373:
	ret
LFE99:
.no_dead_strip _Divide__uDivide_var_LC_u32
_Divide__uDivide_var_LC_u32:
LFB100:
	pushq	%rbp
LCFI374:
	movq	%rsp, %rbp
LCFI375:
	movq	112+_MM_Divide(%rip), %rax
	movl	108+_MM_Divide(%rip), %edx
	mov	%edx, %edx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI376:
	ret
LFE100:
.no_dead_strip _Divide__Divide_var_LC_u32
_Divide__Divide_var_LC_u32:
LFB101:
	pushq	%rbp
LCFI377:
	movq	%rsp, %rbp
LCFI378:
	pushq	%rbx
LCFI379:
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, %rbx
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L257
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L257
	subq	$1, %rax
	addq	%rcx, %rdx
L257:
	popq	%rbx
	leave
LCFI380:
	ret
LFE101:
.no_dead_strip _Divide__uDivide_param_LC_u32
_Divide__uDivide_param_LC_u32:
LFB102:
	pushq	%rbp
LCFI381:
	movq	%rsp, %rbp
LCFI382:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	mov	-12(%rbp), %eax
	movq	%rax, -24(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI383:
	ret
LFE102:
.no_dead_strip _Divide__Divide_param_LC_u32
_Divide__Divide_param_LC_u32:
LFB103:
	pushq	%rbp
LCFI384:
	movq	%rsp, %rbp
LCFI385:
	pushq	%rbx
LCFI386:
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rbx
	mov	-28(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L260
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L260
	subq	$1, %rax
	addq	%rcx, %rdx
L260:
	popq	%rbx
	leave
LCFI387:
	ret
LFE103:
.no_dead_strip _Divide__uDivide_var_LC_LC
_Divide__uDivide_var_LC_LC:
LFB104:
	pushq	%rbp
LCFI388:
	movq	%rsp, %rbp
LCFI389:
	movq	112+_MM_Divide(%rip), %rax
	movq	112+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI390:
	ret
LFE104:
.no_dead_strip _Divide__Divide_var_LC_LC
_Divide__Divide_var_LC_LC:
LFB105:
	pushq	%rbp
LCFI391:
	movq	%rsp, %rbp
LCFI392:
	pushq	%rbx
LCFI393:
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, %rbx
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L263
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L263
	subq	$1, %rax
	addq	%rcx, %rdx
L263:
	popq	%rbx
	leave
LCFI394:
	ret
LFE105:
.no_dead_strip _Divide__uDivide_param_LC_LC
_Divide__uDivide_param_LC_LC:
LFB106:
	pushq	%rbp
LCFI395:
	movq	%rsp, %rbp
LCFI396:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-16(%rbp)
	leave
LCFI397:
	ret
LFE106:
.no_dead_strip _Divide__Divide_param_LC_LC
_Divide__Divide_param_LC_LC:
LFB107:
	pushq	%rbp
LCFI398:
	movq	%rsp, %rbp
LCFI399:
	pushq	%rbx
LCFI400:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rbx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L266
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L266
	subq	$1, %rax
	addq	%rcx, %rdx
L266:
	popq	%rbx
	leave
LCFI401:
	ret
LFE107:
.no_dead_strip _Divide__uDivide_var_LC_I
_Divide__uDivide_var_LC_I:
LFB108:
	pushq	%rbp
LCFI402:
	movq	%rsp, %rbp
LCFI403:
	movq	112+_MM_Divide(%rip), %rax
	movq	120+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI404:
	ret
LFE108:
.no_dead_strip _Divide__Divide_var_LC_I
_Divide__Divide_var_LC_I:
LFB109:
	pushq	%rbp
LCFI405:
	movq	%rsp, %rbp
LCFI406:
	pushq	%rbx
LCFI407:
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, %rbx
	movq	120+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L269
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L269
	subq	$1, %rax
	addq	%rcx, %rdx
L269:
	popq	%rbx
	leave
LCFI408:
	ret
LFE109:
.no_dead_strip _Divide__uDivide_param_LC_I
_Divide__uDivide_param_LC_I:
LFB110:
	pushq	%rbp
LCFI409:
	movq	%rsp, %rbp
LCFI410:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	%rax, -24(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI411:
	ret
LFE110:
.no_dead_strip _Divide__Divide_param_LC_I
_Divide__Divide_param_LC_I:
LFB111:
	pushq	%rbp
LCFI412:
	movq	%rsp, %rbp
LCFI413:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L272
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L272
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L272:
	leave
LCFI414:
	ret
LFE111:
.no_dead_strip _Divide__uDivide_var_LC_i16
_Divide__uDivide_var_LC_i16:
LFB112:
	pushq	%rbp
LCFI415:
	movq	%rsp, %rbp
LCFI416:
	movq	112+_MM_Divide(%rip), %rax
	movzwl	128+_MM_Divide(%rip), %edx
	movswq	%dx, %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI417:
	ret
LFE112:
.no_dead_strip _Divide__Divide_var_LC_i16
_Divide__Divide_var_LC_i16:
LFB113:
	pushq	%rbp
LCFI418:
	movq	%rsp, %rbp
LCFI419:
	pushq	%rbx
LCFI420:
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, %rbx
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L275
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L275
	subq	$1, %rax
	addq	%rcx, %rdx
L275:
	popq	%rbx
	leave
LCFI421:
	ret
LFE113:
.no_dead_strip _Divide__uDivide_param_LC_i16
_Divide__uDivide_param_LC_i16:
LFB114:
	pushq	%rbp
LCFI422:
	movq	%rsp, %rbp
LCFI423:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movswq	-10(%rbp), %rax
	movq	%rax, -24(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI424:
	ret
LFE114:
.no_dead_strip _Divide__Divide_param_LC_i16
_Divide__Divide_param_LC_i16:
LFB115:
	pushq	%rbp
LCFI425:
	movq	%rsp, %rbp
LCFI426:
	pushq	%rbx
LCFI427:
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rbx
	movswq	-26(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L278
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L278
	subq	$1, %rax
	addq	%rcx, %rdx
L278:
	popq	%rbx
	leave
LCFI428:
	ret
LFE115:
.no_dead_strip _Divide__uDivide_var_LC_u64
_Divide__uDivide_var_LC_u64:
LFB116:
	pushq	%rbp
LCFI429:
	movq	%rsp, %rbp
LCFI430:
	movq	112+_MM_Divide(%rip), %rax
	movq	136+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI431:
	ret
LFE116:
.no_dead_strip _Divide__Divide_var_LC_u64
_Divide__Divide_var_LC_u64:
LFB117:
	pushq	%rbp
LCFI432:
	movq	%rsp, %rbp
LCFI433:
	pushq	%rbx
LCFI434:
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, %rbx
	movq	136+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L281
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L281
	subq	$1, %rax
	addq	%rcx, %rdx
L281:
	popq	%rbx
	leave
LCFI435:
	ret
LFE117:
.no_dead_strip _Divide__uDivide_param_LC_u64
_Divide__uDivide_param_LC_u64:
LFB118:
	pushq	%rbp
LCFI436:
	movq	%rsp, %rbp
LCFI437:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	%rax, -24(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI438:
	ret
LFE118:
.no_dead_strip _Divide__Divide_param_LC_u64
_Divide__Divide_param_LC_u64:
LFB119:
	pushq	%rbp
LCFI439:
	movq	%rsp, %rbp
LCFI440:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L284
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L284
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L284:
	leave
LCFI441:
	ret
LFE119:
.no_dead_strip _Divide__uDivide_var_LC_i32
_Divide__uDivide_var_LC_i32:
LFB120:
	pushq	%rbp
LCFI442:
	movq	%rsp, %rbp
LCFI443:
	movq	112+_MM_Divide(%rip), %rax
	movl	152+_MM_Divide(%rip), %edx
	movslq	%edx, %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI444:
	ret
LFE120:
.no_dead_strip _Divide__Divide_var_LC_i32
_Divide__Divide_var_LC_i32:
LFB121:
	pushq	%rbp
LCFI445:
	movq	%rsp, %rbp
LCFI446:
	pushq	%rbx
LCFI447:
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, %rbx
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L287
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L287
	subq	$1, %rax
	addq	%rcx, %rdx
L287:
	popq	%rbx
	leave
LCFI448:
	ret
LFE121:
.no_dead_strip _Divide__uDivide_param_LC_i32
_Divide__uDivide_param_LC_i32:
LFB122:
	pushq	%rbp
LCFI449:
	movq	%rsp, %rbp
LCFI450:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movl	-12(%rbp), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI451:
	ret
LFE122:
.no_dead_strip _Divide__Divide_param_LC_i32
_Divide__Divide_param_LC_i32:
LFB123:
	pushq	%rbp
LCFI452:
	movq	%rsp, %rbp
LCFI453:
	pushq	%rbx
LCFI454:
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rbx
	movl	-28(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L290
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L290
	subq	$1, %rax
	addq	%rcx, %rdx
L290:
	popq	%rbx
	leave
LCFI455:
	ret
LFE123:
.no_dead_strip _Divide__uDivide_var_LC_C
_Divide__uDivide_var_LC_C:
LFB124:
	pushq	%rbp
LCFI456:
	movq	%rsp, %rbp
LCFI457:
	movq	112+_MM_Divide(%rip), %rax
	movq	160+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI458:
	ret
LFE124:
.no_dead_strip _Divide__Divide_var_LC_C
_Divide__Divide_var_LC_C:
LFB125:
	pushq	%rbp
LCFI459:
	movq	%rsp, %rbp
LCFI460:
	pushq	%rbx
LCFI461:
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, %rbx
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L293
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L293
	subq	$1, %rax
	addq	%rcx, %rdx
L293:
	popq	%rbx
	leave
LCFI462:
	ret
LFE125:
.no_dead_strip _Divide__uDivide_param_LC_C
_Divide__uDivide_param_LC_C:
LFB126:
	pushq	%rbp
LCFI463:
	movq	%rsp, %rbp
LCFI464:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-16(%rbp)
	leave
LCFI465:
	ret
LFE126:
.no_dead_strip _Divide__Divide_param_LC_C
_Divide__Divide_param_LC_C:
LFB127:
	pushq	%rbp
LCFI466:
	movq	%rsp, %rbp
LCFI467:
	pushq	%rbx
LCFI468:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rbx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L296
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L296
	subq	$1, %rax
	addq	%rcx, %rdx
L296:
	popq	%rbx
	leave
LCFI469:
	ret
LFE127:
.no_dead_strip _Divide__uDivide_var_LC_u8
_Divide__uDivide_var_LC_u8:
LFB128:
	pushq	%rbp
LCFI470:
	movq	%rsp, %rbp
LCFI471:
	movq	112+_MM_Divide(%rip), %rax
	movzbl	168+_MM_Divide(%rip), %edx
	movzbl	%dl, %edx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI472:
	ret
LFE128:
.no_dead_strip _Divide__Divide_var_LC_u8
_Divide__Divide_var_LC_u8:
LFB129:
	pushq	%rbp
LCFI473:
	movq	%rsp, %rbp
LCFI474:
	pushq	%rbx
LCFI475:
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, %rbx
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L299
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L299
	subq	$1, %rax
	addq	%rcx, %rdx
L299:
	popq	%rbx
	leave
LCFI476:
	ret
LFE129:
.no_dead_strip _Divide__uDivide_param_LC_u8
_Divide__uDivide_param_LC_u8:
LFB130:
	pushq	%rbp
LCFI477:
	movq	%rsp, %rbp
LCFI478:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movzbl	-9(%rbp), %eax
	movq	%rax, -24(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI479:
	ret
LFE130:
.no_dead_strip _Divide__Divide_param_LC_u8
_Divide__Divide_param_LC_u8:
LFB131:
	pushq	%rbp
LCFI480:
	movq	%rsp, %rbp
LCFI481:
	pushq	%rbx
LCFI482:
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rbx
	movzbl	-25(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L302
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L302
	subq	$1, %rax
	addq	%rcx, %rdx
L302:
	popq	%rbx
	leave
LCFI483:
	ret
LFE131:
.no_dead_strip _Divide__uDivide_var_LC_i64
_Divide__uDivide_var_LC_i64:
LFB132:
	pushq	%rbp
LCFI484:
	movq	%rsp, %rbp
LCFI485:
	movq	112+_MM_Divide(%rip), %rax
	movq	176+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI486:
	ret
LFE132:
.no_dead_strip _Divide__Divide_var_LC_i64
_Divide__Divide_var_LC_i64:
LFB133:
	pushq	%rbp
LCFI487:
	movq	%rsp, %rbp
LCFI488:
	pushq	%rbx
LCFI489:
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, %rbx
	movq	176+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L305
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L305
	subq	$1, %rax
	addq	%rcx, %rdx
L305:
	popq	%rbx
	leave
LCFI490:
	ret
LFE133:
.no_dead_strip _Divide__uDivide_param_LC_i64
_Divide__uDivide_param_LC_i64:
LFB134:
	pushq	%rbp
LCFI491:
	movq	%rsp, %rbp
LCFI492:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	%rax, -24(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI493:
	ret
LFE134:
.no_dead_strip _Divide__Divide_param_LC_i64
_Divide__Divide_param_LC_i64:
LFB135:
	pushq	%rbp
LCFI494:
	movq	%rsp, %rbp
LCFI495:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L308
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L308
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L308:
	leave
LCFI496:
	ret
LFE135:
.no_dead_strip _Divide__uDivide_var_LC_L
_Divide__uDivide_var_LC_L:
LFB136:
	pushq	%rbp
LCFI497:
	movq	%rsp, %rbp
LCFI498:
	movq	112+_MM_Divide(%rip), %rax
	movq	192+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI499:
	ret
LFE136:
.no_dead_strip _Divide__Divide_var_LC_L
_Divide__Divide_var_LC_L:
LFB137:
	pushq	%rbp
LCFI500:
	movq	%rsp, %rbp
LCFI501:
	pushq	%rbx
LCFI502:
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, %rbx
	movq	192+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L311
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L311
	subq	$1, %rax
	addq	%rcx, %rdx
L311:
	popq	%rbx
	leave
LCFI503:
	ret
LFE137:
.no_dead_strip _Divide__uDivide_param_LC_L
_Divide__uDivide_param_LC_L:
LFB138:
	pushq	%rbp
LCFI504:
	movq	%rsp, %rbp
LCFI505:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	%rax, -24(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI506:
	ret
LFE138:
.no_dead_strip _Divide__Divide_param_LC_L
_Divide__Divide_param_LC_L:
LFB139:
	pushq	%rbp
LCFI507:
	movq	%rsp, %rbp
LCFI508:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L314
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L314
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L314:
	leave
LCFI509:
	ret
LFE139:
.no_dead_strip _Divide__uDivide_var_LC_i8
_Divide__uDivide_var_LC_i8:
LFB140:
	pushq	%rbp
LCFI510:
	movq	%rsp, %rbp
LCFI511:
	movq	112+_MM_Divide(%rip), %rax
	movzbl	200+_MM_Divide(%rip), %edx
	movsbq	%dl, %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI512:
	ret
LFE140:
.no_dead_strip _Divide__Divide_var_LC_i8
_Divide__Divide_var_LC_i8:
LFB141:
	pushq	%rbp
LCFI513:
	movq	%rsp, %rbp
LCFI514:
	pushq	%rbx
LCFI515:
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, %rbx
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L317
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L317
	subq	$1, %rax
	addq	%rcx, %rdx
L317:
	popq	%rbx
	leave
LCFI516:
	ret
LFE141:
.no_dead_strip _Divide__uDivide_param_LC_i8
_Divide__uDivide_param_LC_i8:
LFB142:
	pushq	%rbp
LCFI517:
	movq	%rsp, %rbp
LCFI518:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movsbq	-9(%rbp), %rax
	movq	%rax, -24(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI519:
	ret
LFE142:
.no_dead_strip _Divide__Divide_param_LC_i8
_Divide__Divide_param_LC_i8:
LFB143:
	pushq	%rbp
LCFI520:
	movq	%rsp, %rbp
LCFI521:
	pushq	%rbx
LCFI522:
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rbx
	movsbq	-25(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L320
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L320
	subq	$1, %rax
	addq	%rcx, %rdx
L320:
	popq	%rbx
	leave
LCFI523:
	ret
LFE143:
.no_dead_strip _Divide__uDivide_var_I_u16
_Divide__uDivide_var_I_u16:
LFB144:
	pushq	%rbp
LCFI524:
	movq	%rsp, %rbp
LCFI525:
	movq	120+_MM_Divide(%rip), %rax
	movzwl	104+_MM_Divide(%rip), %edx
	movzwl	%dx, %edx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI526:
	ret
LFE144:
.no_dead_strip _Divide__Divide_var_I_u16
_Divide__Divide_var_I_u16:
LFB145:
	pushq	%rbp
LCFI527:
	movq	%rsp, %rbp
LCFI528:
	pushq	%rbx
LCFI529:
	movq	120+_MM_Divide(%rip), %rbx
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L323
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L323
	subq	$1, %rax
	addq	%rcx, %rdx
L323:
	popq	%rbx
	leave
LCFI530:
	ret
LFE145:
.no_dead_strip _Divide__uDivide_param_I_u16
_Divide__uDivide_param_I_u16:
LFB146:
	pushq	%rbp
LCFI531:
	movq	%rsp, %rbp
LCFI532:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movq	-8(%rbp), %rax
	movzwl	-10(%rbp), %edx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI533:
	ret
LFE146:
.no_dead_strip _Divide__Divide_param_I_u16
_Divide__Divide_param_I_u16:
LFB147:
	pushq	%rbp
LCFI534:
	movq	%rsp, %rbp
LCFI535:
	pushq	%rbx
LCFI536:
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %ecx
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L326
	movq	%rcx, %rbx
	xorq	-24(%rbp), %rbx
	testq	%rbx, %rbx
	jns	L326
	subq	$1, %rax
	addq	%rcx, %rdx
L326:
	popq	%rbx
	leave
LCFI537:
	ret
LFE147:
.no_dead_strip _Divide__uDivide_var_I_u32
_Divide__uDivide_var_I_u32:
LFB148:
	pushq	%rbp
LCFI538:
	movq	%rsp, %rbp
LCFI539:
	movq	120+_MM_Divide(%rip), %rax
	movl	108+_MM_Divide(%rip), %edx
	mov	%edx, %edx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI540:
	ret
LFE148:
.no_dead_strip _Divide__Divide_var_I_u32
_Divide__Divide_var_I_u32:
LFB149:
	pushq	%rbp
LCFI541:
	movq	%rsp, %rbp
LCFI542:
	pushq	%rbx
LCFI543:
	movq	120+_MM_Divide(%rip), %rbx
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L329
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L329
	subq	$1, %rax
	addq	%rcx, %rdx
L329:
	popq	%rbx
	leave
LCFI544:
	ret
LFE149:
.no_dead_strip _Divide__uDivide_param_I_u32
_Divide__uDivide_param_I_u32:
LFB150:
	pushq	%rbp
LCFI545:
	movq	%rsp, %rbp
LCFI546:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movq	-8(%rbp), %rax
	mov	-12(%rbp), %edx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI547:
	ret
LFE150:
.no_dead_strip _Divide__Divide_param_I_u32
_Divide__Divide_param_I_u32:
LFB151:
	pushq	%rbp
LCFI548:
	movq	%rsp, %rbp
LCFI549:
	pushq	%rbx
LCFI550:
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %ecx
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L332
	movq	%rcx, %rbx
	xorq	-24(%rbp), %rbx
	testq	%rbx, %rbx
	jns	L332
	subq	$1, %rax
	addq	%rcx, %rdx
L332:
	popq	%rbx
	leave
LCFI551:
	ret
LFE151:
.no_dead_strip _Divide__uDivide_var_I_LC
_Divide__uDivide_var_I_LC:
LFB152:
	pushq	%rbp
LCFI552:
	movq	%rsp, %rbp
LCFI553:
	movq	120+_MM_Divide(%rip), %rax
	movq	112+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI554:
	ret
LFE152:
.no_dead_strip _Divide__Divide_var_I_LC
_Divide__Divide_var_I_LC:
LFB153:
	pushq	%rbp
LCFI555:
	movq	%rsp, %rbp
LCFI556:
	pushq	%rbx
LCFI557:
	movq	120+_MM_Divide(%rip), %rbx
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L335
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L335
	subq	$1, %rax
	addq	%rcx, %rdx
L335:
	popq	%rbx
	leave
LCFI558:
	ret
LFE153:
.no_dead_strip _Divide__uDivide_param_I_LC
_Divide__uDivide_param_I_LC:
LFB154:
	pushq	%rbp
LCFI559:
	movq	%rsp, %rbp
LCFI560:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-16(%rbp)
	leave
LCFI561:
	ret
LFE154:
.no_dead_strip _Divide__Divide_param_I_LC
_Divide__Divide_param_I_LC:
LFB155:
	pushq	%rbp
LCFI562:
	movq	%rsp, %rbp
LCFI563:
	pushq	%rbx
LCFI564:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rcx
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L338
	movq	%rcx, %rbx
	xorq	-24(%rbp), %rbx
	testq	%rbx, %rbx
	jns	L338
	subq	$1, %rax
	addq	%rcx, %rdx
L338:
	popq	%rbx
	leave
LCFI565:
	ret
LFE155:
.no_dead_strip _Divide__uDivide_var_I_I
_Divide__uDivide_var_I_I:
LFB156:
	pushq	%rbp
LCFI566:
	movq	%rsp, %rbp
LCFI567:
	movq	120+_MM_Divide(%rip), %rax
	movq	120+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI568:
	ret
LFE156:
.no_dead_strip _Divide__Divide_var_I_I
_Divide__Divide_var_I_I:
LFB157:
	pushq	%rbp
LCFI569:
	movq	%rsp, %rbp
LCFI570:
	pushq	%rbx
LCFI571:
	movq	120+_MM_Divide(%rip), %rbx
	movq	120+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L341
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L341
	subq	$1, %rax
	addq	%rcx, %rdx
L341:
	popq	%rbx
	leave
LCFI572:
	ret
LFE157:
.no_dead_strip _Divide__uDivide_param_I_I
_Divide__uDivide_param_I_I:
LFB158:
	pushq	%rbp
LCFI573:
	movq	%rsp, %rbp
LCFI574:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI575:
	ret
LFE158:
.no_dead_strip _Divide__Divide_param_I_I
_Divide__Divide_param_I_I:
LFB159:
	pushq	%rbp
LCFI576:
	movq	%rsp, %rbp
LCFI577:
	pushq	%rbx
LCFI578:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-32(%rbp)
	testq	%rdx, %rdx
	je	L344
	movq	-32(%rbp), %rcx
	movq	-24(%rbp), %rbx
	xorq	%rbx, %rcx
	testq	%rcx, %rcx
	jns	L344
	subq	$1, %rax
	addq	-32(%rbp), %rdx
L344:
	popq	%rbx
	leave
LCFI579:
	ret
LFE159:
.no_dead_strip _Divide__uDivide_var_I_i16
_Divide__uDivide_var_I_i16:
LFB160:
	pushq	%rbp
LCFI580:
	movq	%rsp, %rbp
LCFI581:
	movq	120+_MM_Divide(%rip), %rax
	movzwl	128+_MM_Divide(%rip), %edx
	movswq	%dx, %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI582:
	ret
LFE160:
.no_dead_strip _Divide__Divide_var_I_i16
_Divide__Divide_var_I_i16:
LFB161:
	pushq	%rbp
LCFI583:
	movq	%rsp, %rbp
LCFI584:
	pushq	%rbx
LCFI585:
	movq	120+_MM_Divide(%rip), %rbx
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L347
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L347
	subq	$1, %rax
	addq	%rcx, %rdx
L347:
	popq	%rbx
	leave
LCFI586:
	ret
LFE161:
.no_dead_strip _Divide__uDivide_param_I_i16
_Divide__uDivide_param_I_i16:
LFB162:
	pushq	%rbp
LCFI587:
	movq	%rsp, %rbp
LCFI588:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movq	-8(%rbp), %rax
	movswq	-10(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI589:
	ret
LFE162:
.no_dead_strip _Divide__Divide_param_I_i16
_Divide__Divide_param_I_i16:
LFB163:
	pushq	%rbp
LCFI590:
	movq	%rsp, %rbp
LCFI591:
	pushq	%rbx
LCFI592:
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp), %rcx
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L350
	movq	%rcx, %rbx
	xorq	-24(%rbp), %rbx
	testq	%rbx, %rbx
	jns	L350
	subq	$1, %rax
	addq	%rcx, %rdx
L350:
	popq	%rbx
	leave
LCFI593:
	ret
LFE163:
.no_dead_strip _Divide__uDivide_var_I_u64
_Divide__uDivide_var_I_u64:
LFB164:
	pushq	%rbp
LCFI594:
	movq	%rsp, %rbp
LCFI595:
	movq	120+_MM_Divide(%rip), %rax
	movq	136+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI596:
	ret
LFE164:
.no_dead_strip _Divide__Divide_var_I_u64
_Divide__Divide_var_I_u64:
LFB165:
	pushq	%rbp
LCFI597:
	movq	%rsp, %rbp
LCFI598:
	pushq	%rbx
LCFI599:
	movq	120+_MM_Divide(%rip), %rbx
	movq	136+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L353
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L353
	subq	$1, %rax
	addq	%rcx, %rdx
L353:
	popq	%rbx
	leave
LCFI600:
	ret
LFE165:
.no_dead_strip _Divide__uDivide_param_I_u64
_Divide__uDivide_param_I_u64:
LFB166:
	pushq	%rbp
LCFI601:
	movq	%rsp, %rbp
LCFI602:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI603:
	ret
LFE166:
.no_dead_strip _Divide__Divide_param_I_u64
_Divide__Divide_param_I_u64:
LFB167:
	pushq	%rbp
LCFI604:
	movq	%rsp, %rbp
LCFI605:
	pushq	%rbx
LCFI606:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-32(%rbp)
	testq	%rdx, %rdx
	je	L356
	movq	-32(%rbp), %rcx
	movq	-24(%rbp), %rbx
	xorq	%rbx, %rcx
	testq	%rcx, %rcx
	jns	L356
	subq	$1, %rax
	addq	-32(%rbp), %rdx
L356:
	popq	%rbx
	leave
LCFI607:
	ret
LFE167:
.no_dead_strip _Divide__uDivide_var_I_i32
_Divide__uDivide_var_I_i32:
LFB168:
	pushq	%rbp
LCFI608:
	movq	%rsp, %rbp
LCFI609:
	movq	120+_MM_Divide(%rip), %rax
	movl	152+_MM_Divide(%rip), %edx
	movslq	%edx, %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI610:
	ret
LFE168:
.no_dead_strip _Divide__Divide_var_I_i32
_Divide__Divide_var_I_i32:
LFB169:
	pushq	%rbp
LCFI611:
	movq	%rsp, %rbp
LCFI612:
	pushq	%rbx
LCFI613:
	movq	120+_MM_Divide(%rip), %rbx
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L359
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L359
	subq	$1, %rax
	addq	%rcx, %rdx
L359:
	popq	%rbx
	leave
LCFI614:
	ret
LFE169:
.no_dead_strip _Divide__uDivide_param_I_i32
_Divide__uDivide_param_I_i32:
LFB170:
	pushq	%rbp
LCFI615:
	movq	%rsp, %rbp
LCFI616:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movq	-8(%rbp), %rax
	movl	-12(%rbp), %edx
	movslq	%edx, %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI617:
	ret
LFE170:
.no_dead_strip _Divide__Divide_param_I_i32
_Divide__Divide_param_I_i32:
LFB171:
	pushq	%rbp
LCFI618:
	movq	%rsp, %rbp
LCFI619:
	pushq	%rbx
LCFI620:
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax, %rcx
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L362
	movq	%rcx, %rbx
	xorq	-24(%rbp), %rbx
	testq	%rbx, %rbx
	jns	L362
	subq	$1, %rax
	addq	%rcx, %rdx
L362:
	popq	%rbx
	leave
LCFI621:
	ret
LFE171:
.no_dead_strip _Divide__uDivide_var_I_C
_Divide__uDivide_var_I_C:
LFB172:
	pushq	%rbp
LCFI622:
	movq	%rsp, %rbp
LCFI623:
	movq	120+_MM_Divide(%rip), %rax
	movq	160+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI624:
	ret
LFE172:
.no_dead_strip _Divide__Divide_var_I_C
_Divide__Divide_var_I_C:
LFB173:
	pushq	%rbp
LCFI625:
	movq	%rsp, %rbp
LCFI626:
	pushq	%rbx
LCFI627:
	movq	120+_MM_Divide(%rip), %rbx
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L365
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L365
	subq	$1, %rax
	addq	%rcx, %rdx
L365:
	popq	%rbx
	leave
LCFI628:
	ret
LFE173:
.no_dead_strip _Divide__uDivide_param_I_C
_Divide__uDivide_param_I_C:
LFB174:
	pushq	%rbp
LCFI629:
	movq	%rsp, %rbp
LCFI630:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-16(%rbp)
	leave
LCFI631:
	ret
LFE174:
.no_dead_strip _Divide__Divide_param_I_C
_Divide__Divide_param_I_C:
LFB175:
	pushq	%rbp
LCFI632:
	movq	%rsp, %rbp
LCFI633:
	pushq	%rbx
LCFI634:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rcx
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L368
	movq	%rcx, %rbx
	xorq	-24(%rbp), %rbx
	testq	%rbx, %rbx
	jns	L368
	subq	$1, %rax
	addq	%rcx, %rdx
L368:
	popq	%rbx
	leave
LCFI635:
	ret
LFE175:
.no_dead_strip _Divide__uDivide_var_I_u8
_Divide__uDivide_var_I_u8:
LFB176:
	pushq	%rbp
LCFI636:
	movq	%rsp, %rbp
LCFI637:
	movq	120+_MM_Divide(%rip), %rax
	movzbl	168+_MM_Divide(%rip), %edx
	movzbl	%dl, %edx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI638:
	ret
LFE176:
.no_dead_strip _Divide__Divide_var_I_u8
_Divide__Divide_var_I_u8:
LFB177:
	pushq	%rbp
LCFI639:
	movq	%rsp, %rbp
LCFI640:
	pushq	%rbx
LCFI641:
	movq	120+_MM_Divide(%rip), %rbx
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L371
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L371
	subq	$1, %rax
	addq	%rcx, %rdx
L371:
	popq	%rbx
	leave
LCFI642:
	ret
LFE177:
.no_dead_strip _Divide__uDivide_param_I_u8
_Divide__uDivide_param_I_u8:
LFB178:
	pushq	%rbp
LCFI643:
	movq	%rsp, %rbp
LCFI644:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movq	-8(%rbp), %rax
	movzbl	-9(%rbp), %edx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI645:
	ret
LFE178:
.no_dead_strip _Divide__Divide_param_I_u8
_Divide__Divide_param_I_u8:
LFB179:
	pushq	%rbp
LCFI646:
	movq	%rsp, %rbp
LCFI647:
	pushq	%rbx
LCFI648:
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %ecx
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L374
	movq	%rcx, %rbx
	xorq	-24(%rbp), %rbx
	testq	%rbx, %rbx
	jns	L374
	subq	$1, %rax
	addq	%rcx, %rdx
L374:
	popq	%rbx
	leave
LCFI649:
	ret
LFE179:
.no_dead_strip _Divide__uDivide_var_I_i64
_Divide__uDivide_var_I_i64:
LFB180:
	pushq	%rbp
LCFI650:
	movq	%rsp, %rbp
LCFI651:
	movq	120+_MM_Divide(%rip), %rax
	movq	176+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI652:
	ret
LFE180:
.no_dead_strip _Divide__Divide_var_I_i64
_Divide__Divide_var_I_i64:
LFB181:
	pushq	%rbp
LCFI653:
	movq	%rsp, %rbp
LCFI654:
	pushq	%rbx
LCFI655:
	movq	120+_MM_Divide(%rip), %rbx
	movq	176+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L377
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L377
	subq	$1, %rax
	addq	%rcx, %rdx
L377:
	popq	%rbx
	leave
LCFI656:
	ret
LFE181:
.no_dead_strip _Divide__uDivide_param_I_i64
_Divide__uDivide_param_I_i64:
LFB182:
	pushq	%rbp
LCFI657:
	movq	%rsp, %rbp
LCFI658:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI659:
	ret
LFE182:
.no_dead_strip _Divide__Divide_param_I_i64
_Divide__Divide_param_I_i64:
LFB183:
	pushq	%rbp
LCFI660:
	movq	%rsp, %rbp
LCFI661:
	pushq	%rbx
LCFI662:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-32(%rbp)
	testq	%rdx, %rdx
	je	L380
	movq	-32(%rbp), %rcx
	movq	-24(%rbp), %rbx
	xorq	%rbx, %rcx
	testq	%rcx, %rcx
	jns	L380
	subq	$1, %rax
	addq	-32(%rbp), %rdx
L380:
	popq	%rbx
	leave
LCFI663:
	ret
LFE183:
.no_dead_strip _Divide__uDivide_var_I_L
_Divide__uDivide_var_I_L:
LFB184:
	pushq	%rbp
LCFI664:
	movq	%rsp, %rbp
LCFI665:
	movq	120+_MM_Divide(%rip), %rax
	movq	192+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI666:
	ret
LFE184:
.no_dead_strip _Divide__Divide_var_I_L
_Divide__Divide_var_I_L:
LFB185:
	pushq	%rbp
LCFI667:
	movq	%rsp, %rbp
LCFI668:
	pushq	%rbx
LCFI669:
	movq	120+_MM_Divide(%rip), %rbx
	movq	192+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L383
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L383
	subq	$1, %rax
	addq	%rcx, %rdx
L383:
	popq	%rbx
	leave
LCFI670:
	ret
LFE185:
.no_dead_strip _Divide__uDivide_param_I_L
_Divide__uDivide_param_I_L:
LFB186:
	pushq	%rbp
LCFI671:
	movq	%rsp, %rbp
LCFI672:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI673:
	ret
LFE186:
.no_dead_strip _Divide__Divide_param_I_L
_Divide__Divide_param_I_L:
LFB187:
	pushq	%rbp
LCFI674:
	movq	%rsp, %rbp
LCFI675:
	pushq	%rbx
LCFI676:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-32(%rbp)
	testq	%rdx, %rdx
	je	L386
	movq	-32(%rbp), %rcx
	movq	-24(%rbp), %rbx
	xorq	%rbx, %rcx
	testq	%rcx, %rcx
	jns	L386
	subq	$1, %rax
	addq	-32(%rbp), %rdx
L386:
	popq	%rbx
	leave
LCFI677:
	ret
LFE187:
.no_dead_strip _Divide__uDivide_var_I_i8
_Divide__uDivide_var_I_i8:
LFB188:
	pushq	%rbp
LCFI678:
	movq	%rsp, %rbp
LCFI679:
	movq	120+_MM_Divide(%rip), %rax
	movzbl	200+_MM_Divide(%rip), %edx
	movsbq	%dl, %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI680:
	ret
LFE188:
.no_dead_strip _Divide__Divide_var_I_i8
_Divide__Divide_var_I_i8:
LFB189:
	pushq	%rbp
LCFI681:
	movq	%rsp, %rbp
LCFI682:
	pushq	%rbx
LCFI683:
	movq	120+_MM_Divide(%rip), %rbx
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L389
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L389
	subq	$1, %rax
	addq	%rcx, %rdx
L389:
	popq	%rbx
	leave
LCFI684:
	ret
LFE189:
.no_dead_strip _Divide__uDivide_param_I_i8
_Divide__uDivide_param_I_i8:
LFB190:
	pushq	%rbp
LCFI685:
	movq	%rsp, %rbp
LCFI686:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movq	-8(%rbp), %rax
	movsbq	-9(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI687:
	ret
LFE190:
.no_dead_strip _Divide__Divide_param_I_i8
_Divide__Divide_param_I_i8:
LFB191:
	pushq	%rbp
LCFI688:
	movq	%rsp, %rbp
LCFI689:
	pushq	%rbx
LCFI690:
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp), %rcx
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L392
	movq	%rcx, %rbx
	xorq	-24(%rbp), %rbx
	testq	%rbx, %rbx
	jns	L392
	subq	$1, %rax
	addq	%rcx, %rdx
L392:
	popq	%rbx
	leave
LCFI691:
	ret
LFE191:
.no_dead_strip _Divide__uDivide_var_i16_u16
_Divide__uDivide_var_i16_u16:
LFB192:
	pushq	%rbp
LCFI692:
	movq	%rsp, %rbp
LCFI693:
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rax
	movzwl	104+_MM_Divide(%rip), %edx
	movzwl	%dx, %edx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI694:
	ret
LFE192:
.no_dead_strip _Divide__Divide_var_i16_u16
_Divide__Divide_var_i16_u16:
LFB193:
	pushq	%rbp
LCFI695:
	movq	%rsp, %rbp
LCFI696:
	pushq	%rbx
LCFI697:
	subq	$8, %rsp
LCFI698:
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rbx
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L395
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L395
	subq	$1, %rax
	addq	%rcx, %rdx
L395:
	cmpq	$-32768, %rax
	jl	L396
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rbx
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L397
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L397
	subq	$1, %rax
	addq	%rcx, %rdx
L397:
	cmpq	$32767, %rax
	jle	L398
L396:
	movl	$7905, %edi
	call	__m3_fault
L398:
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rbx
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L399
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L399
	subq	$1, %rax
	addq	%rcx, %rdx
L399:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI699:
	ret
LFE193:
.no_dead_strip _Divide__uDivide_param_i16_u16
_Divide__uDivide_param_i16_u16:
LFB194:
	pushq	%rbp
LCFI700:
	movq	%rsp, %rbp
LCFI701:
	movw	%di, -2(%rbp)
	movw	%si, -4(%rbp)
	movswq	-2(%rbp), %rax
	movzwl	-4(%rbp), %edx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI702:
	ret
LFE194:
.no_dead_strip _Divide__Divide_param_i16_u16
_Divide__Divide_param_i16_u16:
LFB195:
	pushq	%rbp
LCFI703:
	movq	%rsp, %rbp
LCFI704:
	pushq	%rbx
LCFI705:
	subq	$24, %rsp
LCFI706:
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp), %rbx
	movzwl	-20(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L402
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L402
	subq	$1, %rax
	addq	%rcx, %rdx
L402:
	cmpq	$-32768, %rax
	jl	L403
	movswq	-18(%rbp), %rbx
	movzwl	-20(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L404
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L404
	subq	$1, %rax
	addq	%rcx, %rdx
L404:
	cmpq	$32767, %rax
	jle	L405
L403:
	movl	$7969, %edi
	call	__m3_fault
L405:
	movswq	-18(%rbp), %rbx
	movzwl	-20(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L406
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L406
	subq	$1, %rax
	addq	%rcx, %rdx
L406:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI707:
	ret
LFE195:
.no_dead_strip _Divide__uDivide_var_i16_u32
_Divide__uDivide_var_i16_u32:
LFB196:
	pushq	%rbp
LCFI708:
	movq	%rsp, %rbp
LCFI709:
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rax
	movl	108+_MM_Divide(%rip), %edx
	mov	%edx, %edx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI710:
	ret
LFE196:
.no_dead_strip _Divide__Divide_var_i16_u32
_Divide__Divide_var_i16_u32:
LFB197:
	pushq	%rbp
LCFI711:
	movq	%rsp, %rbp
LCFI712:
	pushq	%rbx
LCFI713:
	subq	$8, %rsp
LCFI714:
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rbx
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L409
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L409
	subq	$1, %rax
	addq	%rcx, %rdx
L409:
	cmpq	$-32768, %rax
	jl	L410
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rbx
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L411
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L411
	subq	$1, %rax
	addq	%rcx, %rdx
L411:
	cmpq	$32767, %rax
	jle	L412
L410:
	movl	$8033, %edi
	call	__m3_fault
L412:
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rbx
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L413
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L413
	subq	$1, %rax
	addq	%rcx, %rdx
L413:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI715:
	ret
LFE197:
.no_dead_strip _Divide__uDivide_param_i16_u32
_Divide__uDivide_param_i16_u32:
LFB198:
	pushq	%rbp
LCFI716:
	movq	%rsp, %rbp
LCFI717:
	movw	%di, -2(%rbp)
	movl	%esi, -8(%rbp)
	movswq	-2(%rbp), %rax
	mov	-8(%rbp), %edx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI718:
	ret
LFE198:
.no_dead_strip _Divide__Divide_param_i16_u32
_Divide__Divide_param_i16_u32:
LFB199:
	pushq	%rbp
LCFI719:
	movq	%rsp, %rbp
LCFI720:
	pushq	%rbx
LCFI721:
	subq	$24, %rsp
LCFI722:
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp), %rbx
	mov	-24(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L416
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L416
	subq	$1, %rax
	addq	%rcx, %rdx
L416:
	cmpq	$-32768, %rax
	jl	L417
	movswq	-18(%rbp), %rbx
	mov	-24(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L418
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L418
	subq	$1, %rax
	addq	%rcx, %rdx
L418:
	cmpq	$32767, %rax
	jle	L419
L417:
	movl	$8097, %edi
	call	__m3_fault
L419:
	movswq	-18(%rbp), %rbx
	mov	-24(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L420
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L420
	subq	$1, %rax
	addq	%rcx, %rdx
L420:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI723:
	ret
LFE199:
.no_dead_strip _Divide__uDivide_var_i16_LC
_Divide__uDivide_var_i16_LC:
LFB200:
	pushq	%rbp
LCFI724:
	movq	%rsp, %rbp
LCFI725:
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rax
	movq	112+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI726:
	ret
LFE200:
.no_dead_strip _Divide__Divide_var_i16_LC
_Divide__Divide_var_i16_LC:
LFB201:
	pushq	%rbp
LCFI727:
	movq	%rsp, %rbp
LCFI728:
	pushq	%rbx
LCFI729:
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rbx
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L423
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L423
	subq	$1, %rax
	addq	%rcx, %rdx
L423:
	popq	%rbx
	leave
LCFI730:
	ret
LFE201:
.no_dead_strip _Divide__uDivide_param_i16_LC
_Divide__uDivide_param_i16_LC:
LFB202:
	pushq	%rbp
LCFI731:
	movq	%rsp, %rbp
LCFI732:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movswq	-2(%rbp), %rax
	movl	$0, %edx
	divq	-16(%rbp)
	leave
LCFI733:
	ret
LFE202:
.no_dead_strip _Divide__Divide_param_i16_LC
_Divide__Divide_param_i16_LC:
LFB203:
	pushq	%rbp
LCFI734:
	movq	%rsp, %rbp
LCFI735:
	pushq	%rbx
LCFI736:
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp), %rbx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L426
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L426
	subq	$1, %rax
	addq	%rcx, %rdx
L426:
	popq	%rbx
	leave
LCFI737:
	ret
LFE203:
.no_dead_strip _Divide__uDivide_var_i16_I
_Divide__uDivide_var_i16_I:
LFB204:
	pushq	%rbp
LCFI738:
	movq	%rsp, %rbp
LCFI739:
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rax
	movq	120+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI740:
	ret
LFE204:
.no_dead_strip _Divide__Divide_var_i16_I
_Divide__Divide_var_i16_I:
LFB205:
	pushq	%rbp
LCFI741:
	movq	%rsp, %rbp
LCFI742:
	pushq	%rbx
LCFI743:
	subq	$8, %rsp
LCFI744:
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rbx
	movq	120+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L429
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L429
	subq	$1, %rax
	addq	%rcx, %rdx
L429:
	cmpq	$-32768, %rax
	jl	L430
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rbx
	movq	120+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L431
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L431
	subq	$1, %rax
	addq	%rcx, %rdx
L431:
	cmpq	$32767, %rax
	jle	L432
L430:
	movl	$8289, %edi
	call	__m3_fault
L432:
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rbx
	movq	120+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L433
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L433
	subq	$1, %rax
	addq	%rcx, %rdx
L433:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI745:
	ret
LFE205:
.no_dead_strip _Divide__uDivide_param_i16_I
_Divide__uDivide_param_i16_I:
LFB206:
	pushq	%rbp
LCFI746:
	movq	%rsp, %rbp
LCFI747:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movswq	-2(%rbp), %rax
	movq	-16(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI748:
	ret
LFE206:
.no_dead_strip _Divide__Divide_param_i16_I
_Divide__Divide_param_i16_I:
LFB207:
	pushq	%rbp
LCFI749:
	movq	%rsp, %rbp
LCFI750:
	subq	$16, %rsp
LCFI751:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movswq	-2(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L436
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L436
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L436:
	cmpq	$-32768, %rax
	jl	L437
	movswq	-2(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L438
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L438
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L438:
	cmpq	$32767, %rax
	jle	L439
L437:
	movl	$8353, %edi
	call	__m3_fault
L439:
	movswq	-2(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L440
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L440
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L440:
	leave
LCFI752:
	ret
LFE207:
.no_dead_strip _Divide__uDivide_var_i16_i16
_Divide__uDivide_var_i16_i16:
LFB208:
	pushq	%rbp
LCFI753:
	movq	%rsp, %rbp
LCFI754:
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rax
	movzwl	128+_MM_Divide(%rip), %edx
	movswq	%dx, %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI755:
	ret
LFE208:
.no_dead_strip _Divide__Divide_var_i16_i16
_Divide__Divide_var_i16_i16:
LFB209:
	pushq	%rbp
LCFI756:
	movq	%rsp, %rbp
LCFI757:
	pushq	%rbx
LCFI758:
	subq	$8, %rsp
LCFI759:
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rbx
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L443
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L443
	subq	$1, %rax
	addq	%rcx, %rdx
L443:
	cmpq	$-32768, %rax
	jl	L444
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rbx
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L445
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L445
	subq	$1, %rax
	addq	%rcx, %rdx
L445:
	cmpq	$32767, %rax
	jle	L446
L444:
	movl	$8417, %edi
	call	__m3_fault
L446:
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rbx
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L447
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L447
	subq	$1, %rax
	addq	%rcx, %rdx
L447:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI760:
	ret
LFE209:
.no_dead_strip _Divide__uDivide_param_i16_i16
_Divide__uDivide_param_i16_i16:
LFB210:
	pushq	%rbp
LCFI761:
	movq	%rsp, %rbp
LCFI762:
	movw	%di, -2(%rbp)
	movw	%si, -4(%rbp)
	movswq	-2(%rbp), %rax
	movswq	-4(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI763:
	ret
LFE210:
.no_dead_strip _Divide__Divide_param_i16_i16
_Divide__Divide_param_i16_i16:
LFB211:
	pushq	%rbp
LCFI764:
	movq	%rsp, %rbp
LCFI765:
	pushq	%rbx
LCFI766:
	subq	$24, %rsp
LCFI767:
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp), %rbx
	movswq	-20(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L450
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L450
	subq	$1, %rax
	addq	%rcx, %rdx
L450:
	cmpq	$-32768, %rax
	jl	L451
	movswq	-18(%rbp), %rbx
	movswq	-20(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L452
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L452
	subq	$1, %rax
	addq	%rcx, %rdx
L452:
	cmpq	$32767, %rax
	jle	L453
L451:
	movl	$8481, %edi
	call	__m3_fault
L453:
	movswq	-18(%rbp), %rbx
	movswq	-20(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L454
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L454
	subq	$1, %rax
	addq	%rcx, %rdx
L454:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI768:
	ret
LFE211:
.no_dead_strip _Divide__uDivide_var_i16_u64
_Divide__uDivide_var_i16_u64:
LFB212:
	pushq	%rbp
LCFI769:
	movq	%rsp, %rbp
LCFI770:
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rax
	movq	136+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI771:
	ret
LFE212:
.no_dead_strip _Divide__Divide_var_i16_u64
_Divide__Divide_var_i16_u64:
LFB213:
	pushq	%rbp
LCFI772:
	movq	%rsp, %rbp
LCFI773:
	pushq	%rbx
LCFI774:
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rbx
	movq	136+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L457
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L457
	subq	$1, %rax
	addq	%rcx, %rdx
L457:
	popq	%rbx
	leave
LCFI775:
	ret
LFE213:
.no_dead_strip _Divide__uDivide_param_i16_u64
_Divide__uDivide_param_i16_u64:
LFB214:
	pushq	%rbp
LCFI776:
	movq	%rsp, %rbp
LCFI777:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movswq	-2(%rbp), %rax
	movq	-16(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI778:
	ret
LFE214:
.no_dead_strip _Divide__Divide_param_i16_u64
_Divide__Divide_param_i16_u64:
LFB215:
	pushq	%rbp
LCFI779:
	movq	%rsp, %rbp
LCFI780:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movswq	-2(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L460
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L460
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L460:
	leave
LCFI781:
	ret
LFE215:
.no_dead_strip _Divide__uDivide_var_i16_i32
_Divide__uDivide_var_i16_i32:
LFB216:
	pushq	%rbp
LCFI782:
	movq	%rsp, %rbp
LCFI783:
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rax
	movl	152+_MM_Divide(%rip), %edx
	movslq	%edx, %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI784:
	ret
LFE216:
.no_dead_strip _Divide__Divide_var_i16_i32
_Divide__Divide_var_i16_i32:
LFB217:
	pushq	%rbp
LCFI785:
	movq	%rsp, %rbp
LCFI786:
	pushq	%rbx
LCFI787:
	subq	$8, %rsp
LCFI788:
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rbx
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L463
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L463
	subq	$1, %rax
	addq	%rcx, %rdx
L463:
	cmpq	$-32768, %rax
	jl	L464
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rbx
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L465
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L465
	subq	$1, %rax
	addq	%rcx, %rdx
L465:
	cmpq	$32767, %rax
	jle	L466
L464:
	movl	$8673, %edi
	call	__m3_fault
L466:
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rbx
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L467
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L467
	subq	$1, %rax
	addq	%rcx, %rdx
L467:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI789:
	ret
LFE217:
.no_dead_strip _Divide__uDivide_param_i16_i32
_Divide__uDivide_param_i16_i32:
LFB218:
	pushq	%rbp
LCFI790:
	movq	%rsp, %rbp
LCFI791:
	movw	%di, -2(%rbp)
	movl	%esi, -8(%rbp)
	movswq	-2(%rbp), %rax
	movl	-8(%rbp), %edx
	movslq	%edx, %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI792:
	ret
LFE218:
.no_dead_strip _Divide__Divide_param_i16_i32
_Divide__Divide_param_i16_i32:
LFB219:
	pushq	%rbp
LCFI793:
	movq	%rsp, %rbp
LCFI794:
	pushq	%rbx
LCFI795:
	subq	$24, %rsp
LCFI796:
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp), %rbx
	movl	-24(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L470
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L470
	subq	$1, %rax
	addq	%rcx, %rdx
L470:
	cmpq	$-32768, %rax
	jl	L471
	movswq	-18(%rbp), %rbx
	movl	-24(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L472
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L472
	subq	$1, %rax
	addq	%rcx, %rdx
L472:
	cmpq	$32767, %rax
	jle	L473
L471:
	movl	$8737, %edi
	call	__m3_fault
L473:
	movswq	-18(%rbp), %rbx
	movl	-24(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L474
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L474
	subq	$1, %rax
	addq	%rcx, %rdx
L474:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI797:
	ret
LFE219:
.no_dead_strip _Divide__uDivide_var_i16_C
_Divide__uDivide_var_i16_C:
LFB220:
	pushq	%rbp
LCFI798:
	movq	%rsp, %rbp
LCFI799:
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rax
	movq	160+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI800:
	ret
LFE220:
.no_dead_strip _Divide__Divide_var_i16_C
_Divide__Divide_var_i16_C:
LFB221:
	pushq	%rbp
LCFI801:
	movq	%rsp, %rbp
LCFI802:
	pushq	%rbx
LCFI803:
	subq	$8, %rsp
LCFI804:
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rbx
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L477
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L477
	subq	$1, %rax
	addq	%rcx, %rdx
L477:
	cmpq	$-32768, %rax
	jl	L478
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rbx
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L479
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L479
	subq	$1, %rax
	addq	%rcx, %rdx
L479:
	cmpq	$32767, %rax
	jle	L480
L478:
	movl	$8801, %edi
	call	__m3_fault
L480:
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rbx
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L481
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L481
	subq	$1, %rax
	addq	%rcx, %rdx
L481:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI805:
	ret
LFE221:
.no_dead_strip _Divide__uDivide_param_i16_C
_Divide__uDivide_param_i16_C:
LFB222:
	pushq	%rbp
LCFI806:
	movq	%rsp, %rbp
LCFI807:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movswq	-2(%rbp), %rax
	movl	$0, %edx
	divq	-16(%rbp)
	leave
LCFI808:
	ret
LFE222:
.no_dead_strip _Divide__Divide_param_i16_C
_Divide__Divide_param_i16_C:
LFB223:
	pushq	%rbp
LCFI809:
	movq	%rsp, %rbp
LCFI810:
	pushq	%rbx
LCFI811:
	subq	$24, %rsp
LCFI812:
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp), %rbx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L484
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L484
	subq	$1, %rax
	addq	%rcx, %rdx
L484:
	cmpq	$-32768, %rax
	jl	L485
	movswq	-18(%rbp), %rbx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L486
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L486
	subq	$1, %rax
	addq	%rcx, %rdx
L486:
	cmpq	$32767, %rax
	jle	L487
L485:
	movl	$8865, %edi
	call	__m3_fault
L487:
	movswq	-18(%rbp), %rbx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L488
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L488
	subq	$1, %rax
	addq	%rcx, %rdx
L488:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI813:
	ret
LFE223:
.no_dead_strip _Divide__uDivide_var_i16_u8
_Divide__uDivide_var_i16_u8:
LFB224:
	pushq	%rbp
LCFI814:
	movq	%rsp, %rbp
LCFI815:
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rax
	movzbl	168+_MM_Divide(%rip), %edx
	movzbl	%dl, %edx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI816:
	ret
LFE224:
.no_dead_strip _Divide__Divide_var_i16_u8
_Divide__Divide_var_i16_u8:
LFB225:
	pushq	%rbp
LCFI817:
	movq	%rsp, %rbp
LCFI818:
	pushq	%rbx
LCFI819:
	subq	$8, %rsp
LCFI820:
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rbx
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L491
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L491
	subq	$1, %rax
	addq	%rcx, %rdx
L491:
	cmpq	$-32768, %rax
	jl	L492
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rbx
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L493
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L493
	subq	$1, %rax
	addq	%rcx, %rdx
L493:
	cmpq	$32767, %rax
	jle	L494
L492:
	movl	$8929, %edi
	call	__m3_fault
L494:
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rbx
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L495
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L495
	subq	$1, %rax
	addq	%rcx, %rdx
L495:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI821:
	ret
LFE225:
.no_dead_strip _Divide__uDivide_param_i16_u8
_Divide__uDivide_param_i16_u8:
LFB226:
	pushq	%rbp
LCFI822:
	movq	%rsp, %rbp
LCFI823:
	movw	%di, -2(%rbp)
	movb	%sil, -3(%rbp)
	movswq	-2(%rbp), %rax
	movzbl	-3(%rbp), %edx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI824:
	ret
LFE226:
.no_dead_strip _Divide__Divide_param_i16_u8
_Divide__Divide_param_i16_u8:
LFB227:
	pushq	%rbp
LCFI825:
	movq	%rsp, %rbp
LCFI826:
	pushq	%rbx
LCFI827:
	subq	$24, %rsp
LCFI828:
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp), %rbx
	movzbl	-19(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L498
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L498
	subq	$1, %rax
	addq	%rcx, %rdx
L498:
	cmpq	$-32768, %rax
	jl	L499
	movswq	-18(%rbp), %rbx
	movzbl	-19(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L500
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L500
	subq	$1, %rax
	addq	%rcx, %rdx
L500:
	cmpq	$32767, %rax
	jle	L501
L499:
	movl	$8993, %edi
	call	__m3_fault
L501:
	movswq	-18(%rbp), %rbx
	movzbl	-19(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L502
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L502
	subq	$1, %rax
	addq	%rcx, %rdx
L502:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI829:
	ret
LFE227:
.no_dead_strip _Divide__uDivide_var_i16_i64
_Divide__uDivide_var_i16_i64:
LFB228:
	pushq	%rbp
LCFI830:
	movq	%rsp, %rbp
LCFI831:
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rax
	movq	176+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI832:
	ret
LFE228:
.no_dead_strip _Divide__Divide_var_i16_i64
_Divide__Divide_var_i16_i64:
LFB229:
	pushq	%rbp
LCFI833:
	movq	%rsp, %rbp
LCFI834:
	pushq	%rbx
LCFI835:
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rbx
	movq	176+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L505
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L505
	subq	$1, %rax
	addq	%rcx, %rdx
L505:
	popq	%rbx
	leave
LCFI836:
	ret
LFE229:
.no_dead_strip _Divide__uDivide_param_i16_i64
_Divide__uDivide_param_i16_i64:
LFB230:
	pushq	%rbp
LCFI837:
	movq	%rsp, %rbp
LCFI838:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movswq	-2(%rbp), %rax
	movq	-16(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI839:
	ret
LFE230:
.no_dead_strip _Divide__Divide_param_i16_i64
_Divide__Divide_param_i16_i64:
LFB231:
	pushq	%rbp
LCFI840:
	movq	%rsp, %rbp
LCFI841:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movswq	-2(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L508
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L508
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L508:
	leave
LCFI842:
	ret
LFE231:
.no_dead_strip _Divide__uDivide_var_i16_L
_Divide__uDivide_var_i16_L:
LFB232:
	pushq	%rbp
LCFI843:
	movq	%rsp, %rbp
LCFI844:
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rax
	movq	192+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI845:
	ret
LFE232:
.no_dead_strip _Divide__Divide_var_i16_L
_Divide__Divide_var_i16_L:
LFB233:
	pushq	%rbp
LCFI846:
	movq	%rsp, %rbp
LCFI847:
	pushq	%rbx
LCFI848:
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rbx
	movq	192+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L511
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L511
	subq	$1, %rax
	addq	%rcx, %rdx
L511:
	popq	%rbx
	leave
LCFI849:
	ret
LFE233:
.no_dead_strip _Divide__uDivide_param_i16_L
_Divide__uDivide_param_i16_L:
LFB234:
	pushq	%rbp
LCFI850:
	movq	%rsp, %rbp
LCFI851:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movswq	-2(%rbp), %rax
	movq	-16(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI852:
	ret
LFE234:
.no_dead_strip _Divide__Divide_param_i16_L
_Divide__Divide_param_i16_L:
LFB235:
	pushq	%rbp
LCFI853:
	movq	%rsp, %rbp
LCFI854:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movswq	-2(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L514
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L514
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L514:
	leave
LCFI855:
	ret
LFE235:
.no_dead_strip _Divide__uDivide_var_i16_i8
_Divide__uDivide_var_i16_i8:
LFB236:
	pushq	%rbp
LCFI856:
	movq	%rsp, %rbp
LCFI857:
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rax
	movzbl	200+_MM_Divide(%rip), %edx
	movsbq	%dl, %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI858:
	ret
LFE236:
.no_dead_strip _Divide__Divide_var_i16_i8
_Divide__Divide_var_i16_i8:
LFB237:
	pushq	%rbp
LCFI859:
	movq	%rsp, %rbp
LCFI860:
	pushq	%rbx
LCFI861:
	subq	$8, %rsp
LCFI862:
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rbx
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L517
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L517
	subq	$1, %rax
	addq	%rcx, %rdx
L517:
	cmpq	$-32768, %rax
	jl	L518
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rbx
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L519
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L519
	subq	$1, %rax
	addq	%rcx, %rdx
L519:
	cmpq	$32767, %rax
	jle	L520
L518:
	movl	$9313, %edi
	call	__m3_fault
L520:
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rbx
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L521
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L521
	subq	$1, %rax
	addq	%rcx, %rdx
L521:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI863:
	ret
LFE237:
.no_dead_strip _Divide__uDivide_param_i16_i8
_Divide__uDivide_param_i16_i8:
LFB238:
	pushq	%rbp
LCFI864:
	movq	%rsp, %rbp
LCFI865:
	movw	%di, -2(%rbp)
	movb	%sil, -3(%rbp)
	movswq	-2(%rbp), %rax
	movsbq	-3(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI866:
	ret
LFE238:
.no_dead_strip _Divide__Divide_param_i16_i8
_Divide__Divide_param_i16_i8:
LFB239:
	pushq	%rbp
LCFI867:
	movq	%rsp, %rbp
LCFI868:
	pushq	%rbx
LCFI869:
	subq	$24, %rsp
LCFI870:
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp), %rbx
	movsbq	-19(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L524
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L524
	subq	$1, %rax
	addq	%rcx, %rdx
L524:
	cmpq	$-32768, %rax
	jl	L525
	movswq	-18(%rbp), %rbx
	movsbq	-19(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L526
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L526
	subq	$1, %rax
	addq	%rcx, %rdx
L526:
	cmpq	$32767, %rax
	jle	L527
L525:
	movl	$9377, %edi
	call	__m3_fault
L527:
	movswq	-18(%rbp), %rbx
	movsbq	-19(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L528
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L528
	subq	$1, %rax
	addq	%rcx, %rdx
L528:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI871:
	ret
LFE239:
.no_dead_strip _Divide__uDivide_var_u64_u16
_Divide__uDivide_var_u64_u16:
LFB240:
	pushq	%rbp
LCFI872:
	movq	%rsp, %rbp
LCFI873:
	movq	136+_MM_Divide(%rip), %rax
	movzwl	104+_MM_Divide(%rip), %edx
	movzwl	%dx, %edx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI874:
	ret
LFE240:
.no_dead_strip _Divide__Divide_var_u64_u16
_Divide__Divide_var_u64_u16:
LFB241:
	pushq	%rbp
LCFI875:
	movq	%rsp, %rbp
LCFI876:
	pushq	%rbx
LCFI877:
	movq	136+_MM_Divide(%rip), %rbx
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L531
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L531
	subq	$1, %rax
	addq	%rcx, %rdx
L531:
	popq	%rbx
	leave
LCFI878:
	ret
LFE241:
.no_dead_strip _Divide__uDivide_param_u64_u16
_Divide__uDivide_param_u64_u16:
LFB242:
	pushq	%rbp
LCFI879:
	movq	%rsp, %rbp
LCFI880:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movq	-8(%rbp), %rax
	movzwl	-10(%rbp), %edx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI881:
	ret
LFE242:
.no_dead_strip _Divide__Divide_param_u64_u16
_Divide__Divide_param_u64_u16:
LFB243:
	pushq	%rbp
LCFI882:
	movq	%rsp, %rbp
LCFI883:
	pushq	%rbx
LCFI884:
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %ecx
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L534
	movq	%rcx, %rbx
	xorq	-24(%rbp), %rbx
	testq	%rbx, %rbx
	jns	L534
	subq	$1, %rax
	addq	%rcx, %rdx
L534:
	popq	%rbx
	leave
LCFI885:
	ret
LFE243:
.no_dead_strip _Divide__uDivide_var_u64_u32
_Divide__uDivide_var_u64_u32:
LFB244:
	pushq	%rbp
LCFI886:
	movq	%rsp, %rbp
LCFI887:
	movq	136+_MM_Divide(%rip), %rax
	movl	108+_MM_Divide(%rip), %edx
	mov	%edx, %edx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI888:
	ret
LFE244:
.no_dead_strip _Divide__Divide_var_u64_u32
_Divide__Divide_var_u64_u32:
LFB245:
	pushq	%rbp
LCFI889:
	movq	%rsp, %rbp
LCFI890:
	pushq	%rbx
LCFI891:
	movq	136+_MM_Divide(%rip), %rbx
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L537
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L537
	subq	$1, %rax
	addq	%rcx, %rdx
L537:
	popq	%rbx
	leave
LCFI892:
	ret
LFE245:
.no_dead_strip _Divide__uDivide_param_u64_u32
_Divide__uDivide_param_u64_u32:
LFB246:
	pushq	%rbp
LCFI893:
	movq	%rsp, %rbp
LCFI894:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movq	-8(%rbp), %rax
	mov	-12(%rbp), %edx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI895:
	ret
LFE246:
.no_dead_strip _Divide__Divide_param_u64_u32
_Divide__Divide_param_u64_u32:
LFB247:
	pushq	%rbp
LCFI896:
	movq	%rsp, %rbp
LCFI897:
	pushq	%rbx
LCFI898:
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %ecx
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L540
	movq	%rcx, %rbx
	xorq	-24(%rbp), %rbx
	testq	%rbx, %rbx
	jns	L540
	subq	$1, %rax
	addq	%rcx, %rdx
L540:
	popq	%rbx
	leave
LCFI899:
	ret
LFE247:
.no_dead_strip _Divide__uDivide_var_u64_LC
_Divide__uDivide_var_u64_LC:
LFB248:
	pushq	%rbp
LCFI900:
	movq	%rsp, %rbp
LCFI901:
	movq	136+_MM_Divide(%rip), %rax
	movq	112+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI902:
	ret
LFE248:
.no_dead_strip _Divide__Divide_var_u64_LC
_Divide__Divide_var_u64_LC:
LFB249:
	pushq	%rbp
LCFI903:
	movq	%rsp, %rbp
LCFI904:
	pushq	%rbx
LCFI905:
	movq	136+_MM_Divide(%rip), %rbx
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L543
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L543
	subq	$1, %rax
	addq	%rcx, %rdx
L543:
	popq	%rbx
	leave
LCFI906:
	ret
LFE249:
.no_dead_strip _Divide__uDivide_param_u64_LC
_Divide__uDivide_param_u64_LC:
LFB250:
	pushq	%rbp
LCFI907:
	movq	%rsp, %rbp
LCFI908:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-16(%rbp)
	leave
LCFI909:
	ret
LFE250:
.no_dead_strip _Divide__Divide_param_u64_LC
_Divide__Divide_param_u64_LC:
LFB251:
	pushq	%rbp
LCFI910:
	movq	%rsp, %rbp
LCFI911:
	pushq	%rbx
LCFI912:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rcx
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L546
	movq	%rcx, %rbx
	xorq	-24(%rbp), %rbx
	testq	%rbx, %rbx
	jns	L546
	subq	$1, %rax
	addq	%rcx, %rdx
L546:
	popq	%rbx
	leave
LCFI913:
	ret
LFE251:
.no_dead_strip _Divide__uDivide_var_u64_I
_Divide__uDivide_var_u64_I:
LFB252:
	pushq	%rbp
LCFI914:
	movq	%rsp, %rbp
LCFI915:
	movq	136+_MM_Divide(%rip), %rax
	movq	120+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI916:
	ret
LFE252:
.no_dead_strip _Divide__Divide_var_u64_I
_Divide__Divide_var_u64_I:
LFB253:
	pushq	%rbp
LCFI917:
	movq	%rsp, %rbp
LCFI918:
	pushq	%rbx
LCFI919:
	movq	136+_MM_Divide(%rip), %rbx
	movq	120+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L549
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L549
	subq	$1, %rax
	addq	%rcx, %rdx
L549:
	popq	%rbx
	leave
LCFI920:
	ret
LFE253:
.no_dead_strip _Divide__uDivide_param_u64_I
_Divide__uDivide_param_u64_I:
LFB254:
	pushq	%rbp
LCFI921:
	movq	%rsp, %rbp
LCFI922:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI923:
	ret
LFE254:
.no_dead_strip _Divide__Divide_param_u64_I
_Divide__Divide_param_u64_I:
LFB255:
	pushq	%rbp
LCFI924:
	movq	%rsp, %rbp
LCFI925:
	pushq	%rbx
LCFI926:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-32(%rbp)
	testq	%rdx, %rdx
	je	L552
	movq	-32(%rbp), %rcx
	movq	-24(%rbp), %rbx
	xorq	%rbx, %rcx
	testq	%rcx, %rcx
	jns	L552
	subq	$1, %rax
	addq	-32(%rbp), %rdx
L552:
	popq	%rbx
	leave
LCFI927:
	ret
LFE255:
.no_dead_strip _Divide__uDivide_var_u64_i16
_Divide__uDivide_var_u64_i16:
LFB256:
	pushq	%rbp
LCFI928:
	movq	%rsp, %rbp
LCFI929:
	movq	136+_MM_Divide(%rip), %rax
	movzwl	128+_MM_Divide(%rip), %edx
	movswq	%dx, %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI930:
	ret
LFE256:
.no_dead_strip _Divide__Divide_var_u64_i16
_Divide__Divide_var_u64_i16:
LFB257:
	pushq	%rbp
LCFI931:
	movq	%rsp, %rbp
LCFI932:
	pushq	%rbx
LCFI933:
	movq	136+_MM_Divide(%rip), %rbx
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L555
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L555
	subq	$1, %rax
	addq	%rcx, %rdx
L555:
	popq	%rbx
	leave
LCFI934:
	ret
LFE257:
.no_dead_strip _Divide__uDivide_param_u64_i16
_Divide__uDivide_param_u64_i16:
LFB258:
	pushq	%rbp
LCFI935:
	movq	%rsp, %rbp
LCFI936:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movq	-8(%rbp), %rax
	movswq	-10(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI937:
	ret
LFE258:
.no_dead_strip _Divide__Divide_param_u64_i16
_Divide__Divide_param_u64_i16:
LFB259:
	pushq	%rbp
LCFI938:
	movq	%rsp, %rbp
LCFI939:
	pushq	%rbx
LCFI940:
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp), %rcx
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L558
	movq	%rcx, %rbx
	xorq	-24(%rbp), %rbx
	testq	%rbx, %rbx
	jns	L558
	subq	$1, %rax
	addq	%rcx, %rdx
L558:
	popq	%rbx
	leave
LCFI941:
	ret
LFE259:
.no_dead_strip _Divide__uDivide_var_u64_u64
_Divide__uDivide_var_u64_u64:
LFB260:
	pushq	%rbp
LCFI942:
	movq	%rsp, %rbp
LCFI943:
	movq	136+_MM_Divide(%rip), %rax
	movq	136+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI944:
	ret
LFE260:
.no_dead_strip _Divide__Divide_var_u64_u64
_Divide__Divide_var_u64_u64:
LFB261:
	pushq	%rbp
LCFI945:
	movq	%rsp, %rbp
LCFI946:
	pushq	%rbx
LCFI947:
	movq	136+_MM_Divide(%rip), %rbx
	movq	136+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L561
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L561
	subq	$1, %rax
	addq	%rcx, %rdx
L561:
	popq	%rbx
	leave
LCFI948:
	ret
LFE261:
.no_dead_strip _Divide__uDivide_param_u64_u64
_Divide__uDivide_param_u64_u64:
LFB262:
	pushq	%rbp
LCFI949:
	movq	%rsp, %rbp
LCFI950:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI951:
	ret
LFE262:
.no_dead_strip _Divide__Divide_param_u64_u64
_Divide__Divide_param_u64_u64:
LFB263:
	pushq	%rbp
LCFI952:
	movq	%rsp, %rbp
LCFI953:
	pushq	%rbx
LCFI954:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-32(%rbp)
	testq	%rdx, %rdx
	je	L564
	movq	-32(%rbp), %rcx
	movq	-24(%rbp), %rbx
	xorq	%rbx, %rcx
	testq	%rcx, %rcx
	jns	L564
	subq	$1, %rax
	addq	-32(%rbp), %rdx
L564:
	popq	%rbx
	leave
LCFI955:
	ret
LFE263:
.no_dead_strip _Divide__uDivide_var_u64_i32
_Divide__uDivide_var_u64_i32:
LFB264:
	pushq	%rbp
LCFI956:
	movq	%rsp, %rbp
LCFI957:
	movq	136+_MM_Divide(%rip), %rax
	movl	152+_MM_Divide(%rip), %edx
	movslq	%edx, %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI958:
	ret
LFE264:
.no_dead_strip _Divide__Divide_var_u64_i32
_Divide__Divide_var_u64_i32:
LFB265:
	pushq	%rbp
LCFI959:
	movq	%rsp, %rbp
LCFI960:
	pushq	%rbx
LCFI961:
	movq	136+_MM_Divide(%rip), %rbx
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L567
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L567
	subq	$1, %rax
	addq	%rcx, %rdx
L567:
	popq	%rbx
	leave
LCFI962:
	ret
LFE265:
.no_dead_strip _Divide__uDivide_param_u64_i32
_Divide__uDivide_param_u64_i32:
LFB266:
	pushq	%rbp
LCFI963:
	movq	%rsp, %rbp
LCFI964:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movq	-8(%rbp), %rax
	movl	-12(%rbp), %edx
	movslq	%edx, %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI965:
	ret
LFE266:
.no_dead_strip _Divide__Divide_param_u64_i32
_Divide__Divide_param_u64_i32:
LFB267:
	pushq	%rbp
LCFI966:
	movq	%rsp, %rbp
LCFI967:
	pushq	%rbx
LCFI968:
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax, %rcx
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L570
	movq	%rcx, %rbx
	xorq	-24(%rbp), %rbx
	testq	%rbx, %rbx
	jns	L570
	subq	$1, %rax
	addq	%rcx, %rdx
L570:
	popq	%rbx
	leave
LCFI969:
	ret
LFE267:
.no_dead_strip _Divide__uDivide_var_u64_C
_Divide__uDivide_var_u64_C:
LFB268:
	pushq	%rbp
LCFI970:
	movq	%rsp, %rbp
LCFI971:
	movq	136+_MM_Divide(%rip), %rax
	movq	160+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI972:
	ret
LFE268:
.no_dead_strip _Divide__Divide_var_u64_C
_Divide__Divide_var_u64_C:
LFB269:
	pushq	%rbp
LCFI973:
	movq	%rsp, %rbp
LCFI974:
	pushq	%rbx
LCFI975:
	movq	136+_MM_Divide(%rip), %rbx
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L573
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L573
	subq	$1, %rax
	addq	%rcx, %rdx
L573:
	popq	%rbx
	leave
LCFI976:
	ret
LFE269:
.no_dead_strip _Divide__uDivide_param_u64_C
_Divide__uDivide_param_u64_C:
LFB270:
	pushq	%rbp
LCFI977:
	movq	%rsp, %rbp
LCFI978:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-16(%rbp)
	leave
LCFI979:
	ret
LFE270:
.no_dead_strip _Divide__Divide_param_u64_C
_Divide__Divide_param_u64_C:
LFB271:
	pushq	%rbp
LCFI980:
	movq	%rsp, %rbp
LCFI981:
	pushq	%rbx
LCFI982:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rcx
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L576
	movq	%rcx, %rbx
	xorq	-24(%rbp), %rbx
	testq	%rbx, %rbx
	jns	L576
	subq	$1, %rax
	addq	%rcx, %rdx
L576:
	popq	%rbx
	leave
LCFI983:
	ret
LFE271:
.no_dead_strip _Divide__uDivide_var_u64_u8
_Divide__uDivide_var_u64_u8:
LFB272:
	pushq	%rbp
LCFI984:
	movq	%rsp, %rbp
LCFI985:
	movq	136+_MM_Divide(%rip), %rax
	movzbl	168+_MM_Divide(%rip), %edx
	movzbl	%dl, %edx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI986:
	ret
LFE272:
.no_dead_strip _Divide__Divide_var_u64_u8
_Divide__Divide_var_u64_u8:
LFB273:
	pushq	%rbp
LCFI987:
	movq	%rsp, %rbp
LCFI988:
	pushq	%rbx
LCFI989:
	movq	136+_MM_Divide(%rip), %rbx
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L579
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L579
	subq	$1, %rax
	addq	%rcx, %rdx
L579:
	popq	%rbx
	leave
LCFI990:
	ret
LFE273:
.no_dead_strip _Divide__uDivide_param_u64_u8
_Divide__uDivide_param_u64_u8:
LFB274:
	pushq	%rbp
LCFI991:
	movq	%rsp, %rbp
LCFI992:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movq	-8(%rbp), %rax
	movzbl	-9(%rbp), %edx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI993:
	ret
LFE274:
.no_dead_strip _Divide__Divide_param_u64_u8
_Divide__Divide_param_u64_u8:
LFB275:
	pushq	%rbp
LCFI994:
	movq	%rsp, %rbp
LCFI995:
	pushq	%rbx
LCFI996:
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %ecx
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L582
	movq	%rcx, %rbx
	xorq	-24(%rbp), %rbx
	testq	%rbx, %rbx
	jns	L582
	subq	$1, %rax
	addq	%rcx, %rdx
L582:
	popq	%rbx
	leave
LCFI997:
	ret
LFE275:
.no_dead_strip _Divide__uDivide_var_u64_i64
_Divide__uDivide_var_u64_i64:
LFB276:
	pushq	%rbp
LCFI998:
	movq	%rsp, %rbp
LCFI999:
	movq	136+_MM_Divide(%rip), %rax
	movq	176+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1000:
	ret
LFE276:
.no_dead_strip _Divide__Divide_var_u64_i64
_Divide__Divide_var_u64_i64:
LFB277:
	pushq	%rbp
LCFI1001:
	movq	%rsp, %rbp
LCFI1002:
	pushq	%rbx
LCFI1003:
	movq	136+_MM_Divide(%rip), %rbx
	movq	176+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L585
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L585
	subq	$1, %rax
	addq	%rcx, %rdx
L585:
	popq	%rbx
	leave
LCFI1004:
	ret
LFE277:
.no_dead_strip _Divide__uDivide_param_u64_i64
_Divide__uDivide_param_u64_i64:
LFB278:
	pushq	%rbp
LCFI1005:
	movq	%rsp, %rbp
LCFI1006:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1007:
	ret
LFE278:
.no_dead_strip _Divide__Divide_param_u64_i64
_Divide__Divide_param_u64_i64:
LFB279:
	pushq	%rbp
LCFI1008:
	movq	%rsp, %rbp
LCFI1009:
	pushq	%rbx
LCFI1010:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-32(%rbp)
	testq	%rdx, %rdx
	je	L588
	movq	-32(%rbp), %rcx
	movq	-24(%rbp), %rbx
	xorq	%rbx, %rcx
	testq	%rcx, %rcx
	jns	L588
	subq	$1, %rax
	addq	-32(%rbp), %rdx
L588:
	popq	%rbx
	leave
LCFI1011:
	ret
LFE279:
.no_dead_strip _Divide__uDivide_var_u64_L
_Divide__uDivide_var_u64_L:
LFB280:
	pushq	%rbp
LCFI1012:
	movq	%rsp, %rbp
LCFI1013:
	movq	136+_MM_Divide(%rip), %rax
	movq	192+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1014:
	ret
LFE280:
.no_dead_strip _Divide__Divide_var_u64_L
_Divide__Divide_var_u64_L:
LFB281:
	pushq	%rbp
LCFI1015:
	movq	%rsp, %rbp
LCFI1016:
	pushq	%rbx
LCFI1017:
	movq	136+_MM_Divide(%rip), %rbx
	movq	192+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L591
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L591
	subq	$1, %rax
	addq	%rcx, %rdx
L591:
	popq	%rbx
	leave
LCFI1018:
	ret
LFE281:
.no_dead_strip _Divide__uDivide_param_u64_L
_Divide__uDivide_param_u64_L:
LFB282:
	pushq	%rbp
LCFI1019:
	movq	%rsp, %rbp
LCFI1020:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1021:
	ret
LFE282:
.no_dead_strip _Divide__Divide_param_u64_L
_Divide__Divide_param_u64_L:
LFB283:
	pushq	%rbp
LCFI1022:
	movq	%rsp, %rbp
LCFI1023:
	pushq	%rbx
LCFI1024:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-32(%rbp)
	testq	%rdx, %rdx
	je	L594
	movq	-32(%rbp), %rcx
	movq	-24(%rbp), %rbx
	xorq	%rbx, %rcx
	testq	%rcx, %rcx
	jns	L594
	subq	$1, %rax
	addq	-32(%rbp), %rdx
L594:
	popq	%rbx
	leave
LCFI1025:
	ret
LFE283:
.no_dead_strip _Divide__uDivide_var_u64_i8
_Divide__uDivide_var_u64_i8:
LFB284:
	pushq	%rbp
LCFI1026:
	movq	%rsp, %rbp
LCFI1027:
	movq	136+_MM_Divide(%rip), %rax
	movzbl	200+_MM_Divide(%rip), %edx
	movsbq	%dl, %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1028:
	ret
LFE284:
.no_dead_strip _Divide__Divide_var_u64_i8
_Divide__Divide_var_u64_i8:
LFB285:
	pushq	%rbp
LCFI1029:
	movq	%rsp, %rbp
LCFI1030:
	pushq	%rbx
LCFI1031:
	movq	136+_MM_Divide(%rip), %rbx
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L597
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L597
	subq	$1, %rax
	addq	%rcx, %rdx
L597:
	popq	%rbx
	leave
LCFI1032:
	ret
LFE285:
.no_dead_strip _Divide__uDivide_param_u64_i8
_Divide__uDivide_param_u64_i8:
LFB286:
	pushq	%rbp
LCFI1033:
	movq	%rsp, %rbp
LCFI1034:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movq	-8(%rbp), %rax
	movsbq	-9(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1035:
	ret
LFE286:
.no_dead_strip _Divide__Divide_param_u64_i8
_Divide__Divide_param_u64_i8:
LFB287:
	pushq	%rbp
LCFI1036:
	movq	%rsp, %rbp
LCFI1037:
	pushq	%rbx
LCFI1038:
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp), %rcx
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L600
	movq	%rcx, %rbx
	xorq	-24(%rbp), %rbx
	testq	%rbx, %rbx
	jns	L600
	subq	$1, %rax
	addq	%rcx, %rdx
L600:
	popq	%rbx
	leave
LCFI1039:
	ret
LFE287:
.no_dead_strip _Divide__Divide_var_f64_f64
_Divide__Divide_var_f64_f64:
LFB288:
	pushq	%rbp
LCFI1040:
	movq	%rsp, %rbp
LCFI1041:
	leaq	_MM_Divide(%rip), %rax
	addq	$144, %rax
	movsd	(%rax), %xmm0
	leaq	_MM_Divide(%rip), %rax
	addq	$144, %rax
	movsd	(%rax), %xmm1
	divsd	%xmm1, %xmm0
	leave
LCFI1042:
	ret
LFE288:
.no_dead_strip _Divide__Divide_param_f64_f64
_Divide__Divide_param_f64_f64:
LFB289:
	pushq	%rbp
LCFI1043:
	movq	%rsp, %rbp
LCFI1044:
	movsd	%xmm0, -8(%rbp)
	movsd	%xmm1, -16(%rbp)
	movsd	-8(%rbp), %xmm0
	divsd	-16(%rbp), %xmm0
	leave
LCFI1045:
	ret
LFE289:
.no_dead_strip _Divide__uDivide_var_i32_u16
_Divide__uDivide_var_i32_u16:
LFB290:
	pushq	%rbp
LCFI1046:
	movq	%rsp, %rbp
LCFI1047:
	movl	152+_MM_Divide(%rip), %eax
	cltq
	movzwl	104+_MM_Divide(%rip), %edx
	movzwl	%dx, %edx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1048:
	ret
LFE290:
.no_dead_strip _Divide__Divide_var_i32_u16
_Divide__Divide_var_i32_u16:
LFB291:
	pushq	%rbp
LCFI1049:
	movq	%rsp, %rbp
LCFI1050:
	pushq	%rbx
LCFI1051:
	subq	$8, %rsp
LCFI1052:
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rbx
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L605
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L605
	subq	$1, %rax
	addq	%rcx, %rdx
L605:
	cmpq	$-2147483648, %rax
	jl	L606
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rbx
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L607
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L607
	subq	$1, %rax
	addq	%rcx, %rdx
L607:
	cmpq	$2147483647, %rax
	jle	L608
L606:
	movl	$11041, %edi
	call	__m3_fault
L608:
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rbx
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L609
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L609
	subq	$1, %rax
	addq	%rcx, %rdx
L609:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1053:
	ret
LFE291:
.no_dead_strip _Divide__uDivide_param_i32_u16
_Divide__uDivide_param_i32_u16:
LFB292:
	pushq	%rbp
LCFI1054:
	movq	%rsp, %rbp
LCFI1055:
	movl	%edi, -4(%rbp)
	movw	%si, -6(%rbp)
	movl	-4(%rbp), %eax
	cltq
	movzwl	-6(%rbp), %edx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1056:
	ret
LFE292:
.no_dead_strip _Divide__Divide_param_i32_u16
_Divide__Divide_param_i32_u16:
LFB293:
	pushq	%rbp
LCFI1057:
	movq	%rsp, %rbp
LCFI1058:
	pushq	%rbx
LCFI1059:
	subq	$24, %rsp
LCFI1060:
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax, %rbx
	movzwl	-22(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L612
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L612
	subq	$1, %rax
	addq	%rcx, %rdx
L612:
	cmpq	$-2147483648, %rax
	jl	L613
	movl	-20(%rbp), %eax
	movslq	%eax, %rbx
	movzwl	-22(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L614
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L614
	subq	$1, %rax
	addq	%rcx, %rdx
L614:
	cmpq	$2147483647, %rax
	jle	L615
L613:
	movl	$11105, %edi
	call	__m3_fault
L615:
	movl	-20(%rbp), %eax
	movslq	%eax, %rbx
	movzwl	-22(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L616
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L616
	subq	$1, %rax
	addq	%rcx, %rdx
L616:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI1061:
	ret
LFE293:
.no_dead_strip _Divide__uDivide_var_i32_u32
_Divide__uDivide_var_i32_u32:
LFB294:
	pushq	%rbp
LCFI1062:
	movq	%rsp, %rbp
LCFI1063:
	movl	152+_MM_Divide(%rip), %eax
	cltq
	movl	108+_MM_Divide(%rip), %edx
	mov	%edx, %edx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1064:
	ret
LFE294:
.no_dead_strip _Divide__Divide_var_i32_u32
_Divide__Divide_var_i32_u32:
LFB295:
	pushq	%rbp
LCFI1065:
	movq	%rsp, %rbp
LCFI1066:
	pushq	%rbx
LCFI1067:
	subq	$8, %rsp
LCFI1068:
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rbx
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L619
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L619
	subq	$1, %rax
	addq	%rcx, %rdx
L619:
	cmpq	$-2147483648, %rax
	jl	L620
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rbx
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L621
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L621
	subq	$1, %rax
	addq	%rcx, %rdx
L621:
	cmpq	$2147483647, %rax
	jle	L622
L620:
	movl	$11169, %edi
	call	__m3_fault
L622:
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rbx
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L623
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L623
	subq	$1, %rax
	addq	%rcx, %rdx
L623:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1069:
	ret
LFE295:
.no_dead_strip _Divide__uDivide_param_i32_u32
_Divide__uDivide_param_i32_u32:
LFB296:
	pushq	%rbp
LCFI1070:
	movq	%rsp, %rbp
LCFI1071:
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	movl	-4(%rbp), %eax
	cltq
	mov	-8(%rbp), %edx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1072:
	ret
LFE296:
.no_dead_strip _Divide__Divide_param_i32_u32
_Divide__Divide_param_i32_u32:
LFB297:
	pushq	%rbp
LCFI1073:
	movq	%rsp, %rbp
LCFI1074:
	pushq	%rbx
LCFI1075:
	subq	$24, %rsp
LCFI1076:
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax, %rbx
	mov	-24(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L626
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L626
	subq	$1, %rax
	addq	%rcx, %rdx
L626:
	cmpq	$-2147483648, %rax
	jl	L627
	movl	-20(%rbp), %eax
	movslq	%eax, %rbx
	mov	-24(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L628
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L628
	subq	$1, %rax
	addq	%rcx, %rdx
L628:
	cmpq	$2147483647, %rax
	jle	L629
L627:
	movl	$11233, %edi
	call	__m3_fault
L629:
	movl	-20(%rbp), %eax
	movslq	%eax, %rbx
	mov	-24(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L630
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L630
	subq	$1, %rax
	addq	%rcx, %rdx
L630:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI1077:
	ret
LFE297:
.no_dead_strip _Divide__uDivide_var_i32_LC
_Divide__uDivide_var_i32_LC:
LFB298:
	pushq	%rbp
LCFI1078:
	movq	%rsp, %rbp
LCFI1079:
	movl	152+_MM_Divide(%rip), %eax
	cltq
	movq	112+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1080:
	ret
LFE298:
.no_dead_strip _Divide__Divide_var_i32_LC
_Divide__Divide_var_i32_LC:
LFB299:
	pushq	%rbp
LCFI1081:
	movq	%rsp, %rbp
LCFI1082:
	pushq	%rbx
LCFI1083:
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rbx
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L633
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L633
	subq	$1, %rax
	addq	%rcx, %rdx
L633:
	popq	%rbx
	leave
LCFI1084:
	ret
LFE299:
.no_dead_strip _Divide__uDivide_param_i32_LC
_Divide__uDivide_param_i32_LC:
LFB300:
	pushq	%rbp
LCFI1085:
	movq	%rsp, %rbp
LCFI1086:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movl	-4(%rbp), %eax
	cltq
	movl	$0, %edx
	divq	-16(%rbp)
	leave
LCFI1087:
	ret
LFE300:
.no_dead_strip _Divide__Divide_param_i32_LC
_Divide__Divide_param_i32_LC:
LFB301:
	pushq	%rbp
LCFI1088:
	movq	%rsp, %rbp
LCFI1089:
	pushq	%rbx
LCFI1090:
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax, %rbx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L636
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L636
	subq	$1, %rax
	addq	%rcx, %rdx
L636:
	popq	%rbx
	leave
LCFI1091:
	ret
LFE301:
.no_dead_strip _Divide__uDivide_var_i32_I
_Divide__uDivide_var_i32_I:
LFB302:
	pushq	%rbp
LCFI1092:
	movq	%rsp, %rbp
LCFI1093:
	movl	152+_MM_Divide(%rip), %eax
	cltq
	movq	120+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1094:
	ret
LFE302:
.no_dead_strip _Divide__Divide_var_i32_I
_Divide__Divide_var_i32_I:
LFB303:
	pushq	%rbp
LCFI1095:
	movq	%rsp, %rbp
LCFI1096:
	pushq	%rbx
LCFI1097:
	subq	$8, %rsp
LCFI1098:
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rbx
	movq	120+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L639
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L639
	subq	$1, %rax
	addq	%rcx, %rdx
L639:
	cmpq	$-2147483648, %rax
	jl	L640
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rbx
	movq	120+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L641
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L641
	subq	$1, %rax
	addq	%rcx, %rdx
L641:
	cmpq	$2147483647, %rax
	jle	L642
L640:
	movl	$11425, %edi
	call	__m3_fault
L642:
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rbx
	movq	120+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L643
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L643
	subq	$1, %rax
	addq	%rcx, %rdx
L643:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1099:
	ret
LFE303:
.no_dead_strip _Divide__uDivide_param_i32_I
_Divide__uDivide_param_i32_I:
LFB304:
	pushq	%rbp
LCFI1100:
	movq	%rsp, %rbp
LCFI1101:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movl	-4(%rbp), %eax
	cltq
	movq	-16(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1102:
	ret
LFE304:
.no_dead_strip _Divide__Divide_param_i32_I
_Divide__Divide_param_i32_I:
LFB305:
	pushq	%rbp
LCFI1103:
	movq	%rsp, %rbp
LCFI1104:
	subq	$16, %rsp
LCFI1105:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movl	-4(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L646
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L646
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L646:
	cmpq	$-2147483648, %rax
	jl	L647
	movl	-4(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L648
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L648
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L648:
	cmpq	$2147483647, %rax
	jle	L649
L647:
	movl	$11489, %edi
	call	__m3_fault
L649:
	movl	-4(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L650
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L650
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L650:
	leave
LCFI1106:
	ret
LFE305:
.no_dead_strip _Divide__uDivide_var_i32_i16
_Divide__uDivide_var_i32_i16:
LFB306:
	pushq	%rbp
LCFI1107:
	movq	%rsp, %rbp
LCFI1108:
	movl	152+_MM_Divide(%rip), %eax
	cltq
	movzwl	128+_MM_Divide(%rip), %edx
	movswq	%dx, %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1109:
	ret
LFE306:
.no_dead_strip _Divide__Divide_var_i32_i16
_Divide__Divide_var_i32_i16:
LFB307:
	pushq	%rbp
LCFI1110:
	movq	%rsp, %rbp
LCFI1111:
	pushq	%rbx
LCFI1112:
	subq	$8, %rsp
LCFI1113:
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rbx
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L653
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L653
	subq	$1, %rax
	addq	%rcx, %rdx
L653:
	cmpq	$-2147483648, %rax
	jl	L654
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rbx
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L655
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L655
	subq	$1, %rax
	addq	%rcx, %rdx
L655:
	cmpq	$2147483647, %rax
	jle	L656
L654:
	movl	$11553, %edi
	call	__m3_fault
L656:
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rbx
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L657
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L657
	subq	$1, %rax
	addq	%rcx, %rdx
L657:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1114:
	ret
LFE307:
.no_dead_strip _Divide__uDivide_param_i32_i16
_Divide__uDivide_param_i32_i16:
LFB308:
	pushq	%rbp
LCFI1115:
	movq	%rsp, %rbp
LCFI1116:
	movl	%edi, -4(%rbp)
	movw	%si, -6(%rbp)
	movl	-4(%rbp), %eax
	cltq
	movswq	-6(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1117:
	ret
LFE308:
.no_dead_strip _Divide__Divide_param_i32_i16
_Divide__Divide_param_i32_i16:
LFB309:
	pushq	%rbp
LCFI1118:
	movq	%rsp, %rbp
LCFI1119:
	pushq	%rbx
LCFI1120:
	subq	$24, %rsp
LCFI1121:
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax, %rbx
	movswq	-22(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L660
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L660
	subq	$1, %rax
	addq	%rcx, %rdx
L660:
	cmpq	$-2147483648, %rax
	jl	L661
	movl	-20(%rbp), %eax
	movslq	%eax, %rbx
	movswq	-22(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L662
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L662
	subq	$1, %rax
	addq	%rcx, %rdx
L662:
	cmpq	$2147483647, %rax
	jle	L663
L661:
	movl	$11617, %edi
	call	__m3_fault
L663:
	movl	-20(%rbp), %eax
	movslq	%eax, %rbx
	movswq	-22(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L664
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L664
	subq	$1, %rax
	addq	%rcx, %rdx
L664:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI1122:
	ret
LFE309:
.no_dead_strip _Divide__uDivide_var_i32_u64
_Divide__uDivide_var_i32_u64:
LFB310:
	pushq	%rbp
LCFI1123:
	movq	%rsp, %rbp
LCFI1124:
	movl	152+_MM_Divide(%rip), %eax
	cltq
	movq	136+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1125:
	ret
LFE310:
.no_dead_strip _Divide__Divide_var_i32_u64
_Divide__Divide_var_i32_u64:
LFB311:
	pushq	%rbp
LCFI1126:
	movq	%rsp, %rbp
LCFI1127:
	pushq	%rbx
LCFI1128:
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rbx
	movq	136+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L667
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L667
	subq	$1, %rax
	addq	%rcx, %rdx
L667:
	popq	%rbx
	leave
LCFI1129:
	ret
LFE311:
.no_dead_strip _Divide__uDivide_param_i32_u64
_Divide__uDivide_param_i32_u64:
LFB312:
	pushq	%rbp
LCFI1130:
	movq	%rsp, %rbp
LCFI1131:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movl	-4(%rbp), %eax
	cltq
	movq	-16(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1132:
	ret
LFE312:
.no_dead_strip _Divide__Divide_param_i32_u64
_Divide__Divide_param_i32_u64:
LFB313:
	pushq	%rbp
LCFI1133:
	movq	%rsp, %rbp
LCFI1134:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movl	-4(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L670
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L670
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L670:
	leave
LCFI1135:
	ret
LFE313:
.no_dead_strip _Divide__uDivide_var_i32_i32
_Divide__uDivide_var_i32_i32:
LFB314:
	pushq	%rbp
LCFI1136:
	movq	%rsp, %rbp
LCFI1137:
	movl	152+_MM_Divide(%rip), %eax
	cltq
	movl	152+_MM_Divide(%rip), %edx
	movslq	%edx, %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1138:
	ret
LFE314:
.no_dead_strip _Divide__Divide_var_i32_i32
_Divide__Divide_var_i32_i32:
LFB315:
	pushq	%rbp
LCFI1139:
	movq	%rsp, %rbp
LCFI1140:
	pushq	%rbx
LCFI1141:
	subq	$8, %rsp
LCFI1142:
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rbx
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L673
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L673
	subq	$1, %rax
	addq	%rcx, %rdx
L673:
	cmpq	$-2147483648, %rax
	jl	L674
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rbx
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L675
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L675
	subq	$1, %rax
	addq	%rcx, %rdx
L675:
	cmpq	$2147483647, %rax
	jle	L676
L674:
	movl	$11809, %edi
	call	__m3_fault
L676:
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rbx
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L677
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L677
	subq	$1, %rax
	addq	%rcx, %rdx
L677:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1143:
	ret
LFE315:
.no_dead_strip _Divide__uDivide_param_i32_i32
_Divide__uDivide_param_i32_i32:
LFB316:
	pushq	%rbp
LCFI1144:
	movq	%rsp, %rbp
LCFI1145:
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	movl	-4(%rbp), %eax
	cltq
	movl	-8(%rbp), %edx
	movslq	%edx, %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1146:
	ret
LFE316:
.no_dead_strip _Divide__Divide_param_i32_i32
_Divide__Divide_param_i32_i32:
LFB317:
	pushq	%rbp
LCFI1147:
	movq	%rsp, %rbp
LCFI1148:
	pushq	%rbx
LCFI1149:
	subq	$24, %rsp
LCFI1150:
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax, %rbx
	movl	-24(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L680
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L680
	subq	$1, %rax
	addq	%rcx, %rdx
L680:
	cmpq	$-2147483648, %rax
	jl	L681
	movl	-20(%rbp), %eax
	movslq	%eax, %rbx
	movl	-24(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L682
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L682
	subq	$1, %rax
	addq	%rcx, %rdx
L682:
	cmpq	$2147483647, %rax
	jle	L683
L681:
	movl	$11873, %edi
	call	__m3_fault
L683:
	movl	-20(%rbp), %eax
	movslq	%eax, %rbx
	movl	-24(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L684
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L684
	subq	$1, %rax
	addq	%rcx, %rdx
L684:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI1151:
	ret
LFE317:
.no_dead_strip _Divide__uDivide_var_i32_C
_Divide__uDivide_var_i32_C:
LFB318:
	pushq	%rbp
LCFI1152:
	movq	%rsp, %rbp
LCFI1153:
	movl	152+_MM_Divide(%rip), %eax
	cltq
	movq	160+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1154:
	ret
LFE318:
.no_dead_strip _Divide__Divide_var_i32_C
_Divide__Divide_var_i32_C:
LFB319:
	pushq	%rbp
LCFI1155:
	movq	%rsp, %rbp
LCFI1156:
	pushq	%rbx
LCFI1157:
	subq	$8, %rsp
LCFI1158:
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rbx
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L687
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L687
	subq	$1, %rax
	addq	%rcx, %rdx
L687:
	cmpq	$-2147483648, %rax
	jl	L688
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rbx
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L689
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L689
	subq	$1, %rax
	addq	%rcx, %rdx
L689:
	cmpq	$2147483647, %rax
	jle	L690
L688:
	movl	$11937, %edi
	call	__m3_fault
L690:
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rbx
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L691
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L691
	subq	$1, %rax
	addq	%rcx, %rdx
L691:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1159:
	ret
LFE319:
.no_dead_strip _Divide__uDivide_param_i32_C
_Divide__uDivide_param_i32_C:
LFB320:
	pushq	%rbp
LCFI1160:
	movq	%rsp, %rbp
LCFI1161:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movl	-4(%rbp), %eax
	cltq
	movl	$0, %edx
	divq	-16(%rbp)
	leave
LCFI1162:
	ret
LFE320:
.no_dead_strip _Divide__Divide_param_i32_C
_Divide__Divide_param_i32_C:
LFB321:
	pushq	%rbp
LCFI1163:
	movq	%rsp, %rbp
LCFI1164:
	pushq	%rbx
LCFI1165:
	subq	$24, %rsp
LCFI1166:
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax, %rbx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L694
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L694
	subq	$1, %rax
	addq	%rcx, %rdx
L694:
	cmpq	$-2147483648, %rax
	jl	L695
	movl	-20(%rbp), %eax
	movslq	%eax, %rbx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L696
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L696
	subq	$1, %rax
	addq	%rcx, %rdx
L696:
	cmpq	$2147483647, %rax
	jle	L697
L695:
	movl	$12001, %edi
	call	__m3_fault
L697:
	movl	-20(%rbp), %eax
	movslq	%eax, %rbx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L698
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L698
	subq	$1, %rax
	addq	%rcx, %rdx
L698:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI1167:
	ret
LFE321:
.no_dead_strip _Divide__uDivide_var_i32_u8
_Divide__uDivide_var_i32_u8:
LFB322:
	pushq	%rbp
LCFI1168:
	movq	%rsp, %rbp
LCFI1169:
	movl	152+_MM_Divide(%rip), %eax
	cltq
	movzbl	168+_MM_Divide(%rip), %edx
	movzbl	%dl, %edx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1170:
	ret
LFE322:
.no_dead_strip _Divide__Divide_var_i32_u8
_Divide__Divide_var_i32_u8:
LFB323:
	pushq	%rbp
LCFI1171:
	movq	%rsp, %rbp
LCFI1172:
	pushq	%rbx
LCFI1173:
	subq	$8, %rsp
LCFI1174:
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rbx
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L701
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L701
	subq	$1, %rax
	addq	%rcx, %rdx
L701:
	cmpq	$-2147483648, %rax
	jl	L702
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rbx
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L703
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L703
	subq	$1, %rax
	addq	%rcx, %rdx
L703:
	cmpq	$2147483647, %rax
	jle	L704
L702:
	movl	$12065, %edi
	call	__m3_fault
L704:
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rbx
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L705
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L705
	subq	$1, %rax
	addq	%rcx, %rdx
L705:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1175:
	ret
LFE323:
.no_dead_strip _Divide__uDivide_param_i32_u8
_Divide__uDivide_param_i32_u8:
LFB324:
	pushq	%rbp
LCFI1176:
	movq	%rsp, %rbp
LCFI1177:
	movl	%edi, -4(%rbp)
	movb	%sil, -5(%rbp)
	movl	-4(%rbp), %eax
	cltq
	movzbl	-5(%rbp), %edx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1178:
	ret
LFE324:
.no_dead_strip _Divide__Divide_param_i32_u8
_Divide__Divide_param_i32_u8:
LFB325:
	pushq	%rbp
LCFI1179:
	movq	%rsp, %rbp
LCFI1180:
	pushq	%rbx
LCFI1181:
	subq	$24, %rsp
LCFI1182:
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax, %rbx
	movzbl	-21(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L708
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L708
	subq	$1, %rax
	addq	%rcx, %rdx
L708:
	cmpq	$-2147483648, %rax
	jl	L709
	movl	-20(%rbp), %eax
	movslq	%eax, %rbx
	movzbl	-21(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L710
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L710
	subq	$1, %rax
	addq	%rcx, %rdx
L710:
	cmpq	$2147483647, %rax
	jle	L711
L709:
	movl	$12129, %edi
	call	__m3_fault
L711:
	movl	-20(%rbp), %eax
	movslq	%eax, %rbx
	movzbl	-21(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L712
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L712
	subq	$1, %rax
	addq	%rcx, %rdx
L712:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI1183:
	ret
LFE325:
.no_dead_strip _Divide__uDivide_var_i32_i64
_Divide__uDivide_var_i32_i64:
LFB326:
	pushq	%rbp
LCFI1184:
	movq	%rsp, %rbp
LCFI1185:
	movl	152+_MM_Divide(%rip), %eax
	cltq
	movq	176+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1186:
	ret
LFE326:
.no_dead_strip _Divide__Divide_var_i32_i64
_Divide__Divide_var_i32_i64:
LFB327:
	pushq	%rbp
LCFI1187:
	movq	%rsp, %rbp
LCFI1188:
	pushq	%rbx
LCFI1189:
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rbx
	movq	176+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L715
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L715
	subq	$1, %rax
	addq	%rcx, %rdx
L715:
	popq	%rbx
	leave
LCFI1190:
	ret
LFE327:
.no_dead_strip _Divide__uDivide_param_i32_i64
_Divide__uDivide_param_i32_i64:
LFB328:
	pushq	%rbp
LCFI1191:
	movq	%rsp, %rbp
LCFI1192:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movl	-4(%rbp), %eax
	cltq
	movq	-16(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1193:
	ret
LFE328:
.no_dead_strip _Divide__Divide_param_i32_i64
_Divide__Divide_param_i32_i64:
LFB329:
	pushq	%rbp
LCFI1194:
	movq	%rsp, %rbp
LCFI1195:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movl	-4(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L718
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L718
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L718:
	leave
LCFI1196:
	ret
LFE329:
.no_dead_strip _Divide__uDivide_var_i32_L
_Divide__uDivide_var_i32_L:
LFB330:
	pushq	%rbp
LCFI1197:
	movq	%rsp, %rbp
LCFI1198:
	movl	152+_MM_Divide(%rip), %eax
	cltq
	movq	192+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1199:
	ret
LFE330:
.no_dead_strip _Divide__Divide_var_i32_L
_Divide__Divide_var_i32_L:
LFB331:
	pushq	%rbp
LCFI1200:
	movq	%rsp, %rbp
LCFI1201:
	pushq	%rbx
LCFI1202:
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rbx
	movq	192+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L721
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L721
	subq	$1, %rax
	addq	%rcx, %rdx
L721:
	popq	%rbx
	leave
LCFI1203:
	ret
LFE331:
.no_dead_strip _Divide__uDivide_param_i32_L
_Divide__uDivide_param_i32_L:
LFB332:
	pushq	%rbp
LCFI1204:
	movq	%rsp, %rbp
LCFI1205:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movl	-4(%rbp), %eax
	cltq
	movq	-16(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1206:
	ret
LFE332:
.no_dead_strip _Divide__Divide_param_i32_L
_Divide__Divide_param_i32_L:
LFB333:
	pushq	%rbp
LCFI1207:
	movq	%rsp, %rbp
LCFI1208:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movl	-4(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L724
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L724
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L724:
	leave
LCFI1209:
	ret
LFE333:
.no_dead_strip _Divide__uDivide_var_i32_i8
_Divide__uDivide_var_i32_i8:
LFB334:
	pushq	%rbp
LCFI1210:
	movq	%rsp, %rbp
LCFI1211:
	movl	152+_MM_Divide(%rip), %eax
	cltq
	movzbl	200+_MM_Divide(%rip), %edx
	movsbq	%dl, %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1212:
	ret
LFE334:
.no_dead_strip _Divide__Divide_var_i32_i8
_Divide__Divide_var_i32_i8:
LFB335:
	pushq	%rbp
LCFI1213:
	movq	%rsp, %rbp
LCFI1214:
	pushq	%rbx
LCFI1215:
	subq	$8, %rsp
LCFI1216:
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rbx
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L727
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L727
	subq	$1, %rax
	addq	%rcx, %rdx
L727:
	cmpq	$-2147483648, %rax
	jl	L728
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rbx
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L729
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L729
	subq	$1, %rax
	addq	%rcx, %rdx
L729:
	cmpq	$2147483647, %rax
	jle	L730
L728:
	movl	$12449, %edi
	call	__m3_fault
L730:
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rbx
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L731
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L731
	subq	$1, %rax
	addq	%rcx, %rdx
L731:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1217:
	ret
LFE335:
.no_dead_strip _Divide__uDivide_param_i32_i8
_Divide__uDivide_param_i32_i8:
LFB336:
	pushq	%rbp
LCFI1218:
	movq	%rsp, %rbp
LCFI1219:
	movl	%edi, -4(%rbp)
	movb	%sil, -5(%rbp)
	movl	-4(%rbp), %eax
	cltq
	movsbq	-5(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1220:
	ret
LFE336:
.no_dead_strip _Divide__Divide_param_i32_i8
_Divide__Divide_param_i32_i8:
LFB337:
	pushq	%rbp
LCFI1221:
	movq	%rsp, %rbp
LCFI1222:
	pushq	%rbx
LCFI1223:
	subq	$24, %rsp
LCFI1224:
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax, %rbx
	movsbq	-21(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L734
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L734
	subq	$1, %rax
	addq	%rcx, %rdx
L734:
	cmpq	$-2147483648, %rax
	jl	L735
	movl	-20(%rbp), %eax
	movslq	%eax, %rbx
	movsbq	-21(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L736
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L736
	subq	$1, %rax
	addq	%rcx, %rdx
L736:
	cmpq	$2147483647, %rax
	jle	L737
L735:
	movl	$12513, %edi
	call	__m3_fault
L737:
	movl	-20(%rbp), %eax
	movslq	%eax, %rbx
	movsbq	-21(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L738
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L738
	subq	$1, %rax
	addq	%rcx, %rdx
L738:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI1225:
	ret
LFE337:
.no_dead_strip _Divide__uDivide_var_C_u16
_Divide__uDivide_var_C_u16:
LFB338:
	pushq	%rbp
LCFI1226:
	movq	%rsp, %rbp
LCFI1227:
	movq	160+_MM_Divide(%rip), %rax
	movzwl	104+_MM_Divide(%rip), %edx
	movzwl	%dx, %edx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1228:
	ret
LFE338:
.no_dead_strip _Divide__Divide_var_C_u16
_Divide__Divide_var_C_u16:
LFB339:
	pushq	%rbp
LCFI1229:
	movq	%rsp, %rbp
LCFI1230:
	pushq	%rbx
LCFI1231:
	subq	$8, %rsp
LCFI1232:
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rbx
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L741
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L741
	subq	$1, %rax
	addq	%rcx, %rdx
L741:
	testq	%rax, %rax
	jns	L742
	movl	$12577, %edi
	call	__m3_fault
L742:
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rbx
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L743
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L743
	subq	$1, %rax
	addq	%rcx, %rdx
L743:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1233:
	ret
LFE339:
.no_dead_strip _Divide__uDivide_param_C_u16
_Divide__uDivide_param_C_u16:
LFB340:
	pushq	%rbp
LCFI1234:
	movq	%rsp, %rbp
LCFI1235:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movzwl	-10(%rbp), %eax
	movq	%rax, -24(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1236:
	ret
LFE340:
.no_dead_strip _Divide__Divide_param_C_u16
_Divide__Divide_param_C_u16:
LFB341:
	pushq	%rbp
LCFI1237:
	movq	%rsp, %rbp
LCFI1238:
	pushq	%rbx
LCFI1239:
	subq	$24, %rsp
LCFI1240:
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rbx
	movzwl	-26(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L746
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L746
	subq	$1, %rax
	addq	%rcx, %rdx
L746:
	testq	%rax, %rax
	jns	L747
	movl	$12641, %edi
	call	__m3_fault
L747:
	movq	-24(%rbp), %rbx
	movzwl	-26(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L748
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L748
	subq	$1, %rax
	addq	%rcx, %rdx
L748:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI1241:
	ret
LFE341:
.no_dead_strip _Divide__uDivide_var_C_u32
_Divide__uDivide_var_C_u32:
LFB342:
	pushq	%rbp
LCFI1242:
	movq	%rsp, %rbp
LCFI1243:
	movq	160+_MM_Divide(%rip), %rax
	movl	108+_MM_Divide(%rip), %edx
	mov	%edx, %edx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1244:
	ret
LFE342:
.no_dead_strip _Divide__Divide_var_C_u32
_Divide__Divide_var_C_u32:
LFB343:
	pushq	%rbp
LCFI1245:
	movq	%rsp, %rbp
LCFI1246:
	pushq	%rbx
LCFI1247:
	subq	$8, %rsp
LCFI1248:
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rbx
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L751
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L751
	subq	$1, %rax
	addq	%rcx, %rdx
L751:
	testq	%rax, %rax
	jns	L752
	movl	$12705, %edi
	call	__m3_fault
L752:
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rbx
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L753
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L753
	subq	$1, %rax
	addq	%rcx, %rdx
L753:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1249:
	ret
LFE343:
.no_dead_strip _Divide__uDivide_param_C_u32
_Divide__uDivide_param_C_u32:
LFB344:
	pushq	%rbp
LCFI1250:
	movq	%rsp, %rbp
LCFI1251:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	mov	-12(%rbp), %eax
	movq	%rax, -24(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1252:
	ret
LFE344:
.no_dead_strip _Divide__Divide_param_C_u32
_Divide__Divide_param_C_u32:
LFB345:
	pushq	%rbp
LCFI1253:
	movq	%rsp, %rbp
LCFI1254:
	pushq	%rbx
LCFI1255:
	subq	$24, %rsp
LCFI1256:
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rbx
	mov	-28(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L756
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L756
	subq	$1, %rax
	addq	%rcx, %rdx
L756:
	testq	%rax, %rax
	jns	L757
	movl	$12769, %edi
	call	__m3_fault
L757:
	movq	-24(%rbp), %rbx
	mov	-28(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L758
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L758
	subq	$1, %rax
	addq	%rcx, %rdx
L758:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI1257:
	ret
LFE345:
.no_dead_strip _Divide__uDivide_var_C_LC
_Divide__uDivide_var_C_LC:
LFB346:
	pushq	%rbp
LCFI1258:
	movq	%rsp, %rbp
LCFI1259:
	movq	160+_MM_Divide(%rip), %rax
	movq	112+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1260:
	ret
LFE346:
.no_dead_strip _Divide__Divide_var_C_LC
_Divide__Divide_var_C_LC:
LFB347:
	pushq	%rbp
LCFI1261:
	movq	%rsp, %rbp
LCFI1262:
	pushq	%rbx
LCFI1263:
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rbx
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L761
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L761
	subq	$1, %rax
	addq	%rcx, %rdx
L761:
	popq	%rbx
	leave
LCFI1264:
	ret
LFE347:
.no_dead_strip _Divide__uDivide_param_C_LC
_Divide__uDivide_param_C_LC:
LFB348:
	pushq	%rbp
LCFI1265:
	movq	%rsp, %rbp
LCFI1266:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-16(%rbp)
	leave
LCFI1267:
	ret
LFE348:
.no_dead_strip _Divide__Divide_param_C_LC
_Divide__Divide_param_C_LC:
LFB349:
	pushq	%rbp
LCFI1268:
	movq	%rsp, %rbp
LCFI1269:
	pushq	%rbx
LCFI1270:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rbx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L764
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L764
	subq	$1, %rax
	addq	%rcx, %rdx
L764:
	popq	%rbx
	leave
LCFI1271:
	ret
LFE349:
.no_dead_strip _Divide__uDivide_var_C_I
_Divide__uDivide_var_C_I:
LFB350:
	pushq	%rbp
LCFI1272:
	movq	%rsp, %rbp
LCFI1273:
	movq	160+_MM_Divide(%rip), %rax
	movq	120+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1274:
	ret
LFE350:
.no_dead_strip _Divide__Divide_var_C_I
_Divide__Divide_var_C_I:
LFB351:
	pushq	%rbp
LCFI1275:
	movq	%rsp, %rbp
LCFI1276:
	pushq	%rbx
LCFI1277:
	subq	$8, %rsp
LCFI1278:
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rbx
	movq	120+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L767
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L767
	subq	$1, %rax
	addq	%rcx, %rdx
L767:
	testq	%rax, %rax
	jns	L768
	movl	$12961, %edi
	call	__m3_fault
L768:
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rbx
	movq	120+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L769
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L769
	subq	$1, %rax
	addq	%rcx, %rdx
L769:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1279:
	ret
LFE351:
.no_dead_strip _Divide__uDivide_param_C_I
_Divide__uDivide_param_C_I:
LFB352:
	pushq	%rbp
LCFI1280:
	movq	%rsp, %rbp
LCFI1281:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	%rax, -24(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1282:
	ret
LFE352:
.no_dead_strip _Divide__Divide_param_C_I
_Divide__Divide_param_C_I:
LFB353:
	pushq	%rbp
LCFI1283:
	movq	%rsp, %rbp
LCFI1284:
	subq	$16, %rsp
LCFI1285:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L772
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L772
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L772:
	testq	%rax, %rax
	jns	L773
	movl	$13025, %edi
	call	__m3_fault
L773:
	movq	-8(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L774
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L774
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L774:
	leave
LCFI1286:
	ret
LFE353:
.no_dead_strip _Divide__uDivide_var_C_i16
_Divide__uDivide_var_C_i16:
LFB354:
	pushq	%rbp
LCFI1287:
	movq	%rsp, %rbp
LCFI1288:
	movq	160+_MM_Divide(%rip), %rax
	movzwl	128+_MM_Divide(%rip), %edx
	movswq	%dx, %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1289:
	ret
LFE354:
.no_dead_strip _Divide__Divide_var_C_i16
_Divide__Divide_var_C_i16:
LFB355:
	pushq	%rbp
LCFI1290:
	movq	%rsp, %rbp
LCFI1291:
	pushq	%rbx
LCFI1292:
	subq	$8, %rsp
LCFI1293:
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rbx
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L777
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L777
	subq	$1, %rax
	addq	%rcx, %rdx
L777:
	testq	%rax, %rax
	jns	L778
	movl	$13089, %edi
	call	__m3_fault
L778:
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rbx
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L779
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L779
	subq	$1, %rax
	addq	%rcx, %rdx
L779:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1294:
	ret
LFE355:
.no_dead_strip _Divide__uDivide_param_C_i16
_Divide__uDivide_param_C_i16:
LFB356:
	pushq	%rbp
LCFI1295:
	movq	%rsp, %rbp
LCFI1296:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movswq	-10(%rbp), %rax
	movq	%rax, -24(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1297:
	ret
LFE356:
.no_dead_strip _Divide__Divide_param_C_i16
_Divide__Divide_param_C_i16:
LFB357:
	pushq	%rbp
LCFI1298:
	movq	%rsp, %rbp
LCFI1299:
	pushq	%rbx
LCFI1300:
	subq	$24, %rsp
LCFI1301:
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rbx
	movswq	-26(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L782
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L782
	subq	$1, %rax
	addq	%rcx, %rdx
L782:
	testq	%rax, %rax
	jns	L783
	movl	$13153, %edi
	call	__m3_fault
L783:
	movq	-24(%rbp), %rbx
	movswq	-26(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L784
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L784
	subq	$1, %rax
	addq	%rcx, %rdx
L784:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI1302:
	ret
LFE357:
.no_dead_strip _Divide__uDivide_var_C_u64
_Divide__uDivide_var_C_u64:
LFB358:
	pushq	%rbp
LCFI1303:
	movq	%rsp, %rbp
LCFI1304:
	movq	160+_MM_Divide(%rip), %rax
	movq	136+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1305:
	ret
LFE358:
.no_dead_strip _Divide__Divide_var_C_u64
_Divide__Divide_var_C_u64:
LFB359:
	pushq	%rbp
LCFI1306:
	movq	%rsp, %rbp
LCFI1307:
	pushq	%rbx
LCFI1308:
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rbx
	movq	136+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L787
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L787
	subq	$1, %rax
	addq	%rcx, %rdx
L787:
	popq	%rbx
	leave
LCFI1309:
	ret
LFE359:
.no_dead_strip _Divide__uDivide_param_C_u64
_Divide__uDivide_param_C_u64:
LFB360:
	pushq	%rbp
LCFI1310:
	movq	%rsp, %rbp
LCFI1311:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	%rax, -24(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1312:
	ret
LFE360:
.no_dead_strip _Divide__Divide_param_C_u64
_Divide__Divide_param_C_u64:
LFB361:
	pushq	%rbp
LCFI1313:
	movq	%rsp, %rbp
LCFI1314:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L790
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L790
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L790:
	leave
LCFI1315:
	ret
LFE361:
.no_dead_strip _Divide__uDivide_var_C_i32
_Divide__uDivide_var_C_i32:
LFB362:
	pushq	%rbp
LCFI1316:
	movq	%rsp, %rbp
LCFI1317:
	movq	160+_MM_Divide(%rip), %rax
	movl	152+_MM_Divide(%rip), %edx
	movslq	%edx, %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1318:
	ret
LFE362:
.no_dead_strip _Divide__Divide_var_C_i32
_Divide__Divide_var_C_i32:
LFB363:
	pushq	%rbp
LCFI1319:
	movq	%rsp, %rbp
LCFI1320:
	pushq	%rbx
LCFI1321:
	subq	$8, %rsp
LCFI1322:
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rbx
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L793
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L793
	subq	$1, %rax
	addq	%rcx, %rdx
L793:
	testq	%rax, %rax
	jns	L794
	movl	$13345, %edi
	call	__m3_fault
L794:
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rbx
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L795
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L795
	subq	$1, %rax
	addq	%rcx, %rdx
L795:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1323:
	ret
LFE363:
.no_dead_strip _Divide__uDivide_param_C_i32
_Divide__uDivide_param_C_i32:
LFB364:
	pushq	%rbp
LCFI1324:
	movq	%rsp, %rbp
LCFI1325:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movl	-12(%rbp), %eax
	cltq
	movq	%rax, -24(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1326:
	ret
LFE364:
.no_dead_strip _Divide__Divide_param_C_i32
_Divide__Divide_param_C_i32:
LFB365:
	pushq	%rbp
LCFI1327:
	movq	%rsp, %rbp
LCFI1328:
	pushq	%rbx
LCFI1329:
	subq	$24, %rsp
LCFI1330:
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rbx
	movl	-28(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L798
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L798
	subq	$1, %rax
	addq	%rcx, %rdx
L798:
	testq	%rax, %rax
	jns	L799
	movl	$13409, %edi
	call	__m3_fault
L799:
	movq	-24(%rbp), %rbx
	movl	-28(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L800
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L800
	subq	$1, %rax
	addq	%rcx, %rdx
L800:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI1331:
	ret
LFE365:
.no_dead_strip _Divide__uDivide_var_C_C
_Divide__uDivide_var_C_C:
LFB366:
	pushq	%rbp
LCFI1332:
	movq	%rsp, %rbp
LCFI1333:
	movq	160+_MM_Divide(%rip), %rax
	movq	160+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1334:
	ret
LFE366:
.no_dead_strip _Divide__Divide_var_C_C
_Divide__Divide_var_C_C:
LFB367:
	pushq	%rbp
LCFI1335:
	movq	%rsp, %rbp
LCFI1336:
	pushq	%rbx
LCFI1337:
	subq	$8, %rsp
LCFI1338:
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rbx
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L803
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L803
	subq	$1, %rax
	addq	%rcx, %rdx
L803:
	testq	%rax, %rax
	jns	L804
	movl	$13473, %edi
	call	__m3_fault
L804:
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rbx
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L805
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L805
	subq	$1, %rax
	addq	%rcx, %rdx
L805:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1339:
	ret
LFE367:
.no_dead_strip _Divide__uDivide_param_C_C
_Divide__uDivide_param_C_C:
LFB368:
	pushq	%rbp
LCFI1340:
	movq	%rsp, %rbp
LCFI1341:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-16(%rbp)
	leave
LCFI1342:
	ret
LFE368:
.no_dead_strip _Divide__Divide_param_C_C
_Divide__Divide_param_C_C:
LFB369:
	pushq	%rbp
LCFI1343:
	movq	%rsp, %rbp
LCFI1344:
	pushq	%rbx
LCFI1345:
	subq	$24, %rsp
LCFI1346:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rbx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L808
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L808
	subq	$1, %rax
	addq	%rcx, %rdx
L808:
	testq	%rax, %rax
	jns	L809
	movl	$13537, %edi
	call	__m3_fault
L809:
	movq	-24(%rbp), %rbx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L810
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L810
	subq	$1, %rax
	addq	%rcx, %rdx
L810:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI1347:
	ret
LFE369:
.no_dead_strip _Divide__uDivide_var_C_u8
_Divide__uDivide_var_C_u8:
LFB370:
	pushq	%rbp
LCFI1348:
	movq	%rsp, %rbp
LCFI1349:
	movq	160+_MM_Divide(%rip), %rax
	movzbl	168+_MM_Divide(%rip), %edx
	movzbl	%dl, %edx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1350:
	ret
LFE370:
.no_dead_strip _Divide__Divide_var_C_u8
_Divide__Divide_var_C_u8:
LFB371:
	pushq	%rbp
LCFI1351:
	movq	%rsp, %rbp
LCFI1352:
	pushq	%rbx
LCFI1353:
	subq	$8, %rsp
LCFI1354:
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rbx
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L813
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L813
	subq	$1, %rax
	addq	%rcx, %rdx
L813:
	testq	%rax, %rax
	jns	L814
	movl	$13601, %edi
	call	__m3_fault
L814:
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rbx
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L815
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L815
	subq	$1, %rax
	addq	%rcx, %rdx
L815:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1355:
	ret
LFE371:
.no_dead_strip _Divide__uDivide_param_C_u8
_Divide__uDivide_param_C_u8:
LFB372:
	pushq	%rbp
LCFI1356:
	movq	%rsp, %rbp
LCFI1357:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movzbl	-9(%rbp), %eax
	movq	%rax, -24(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1358:
	ret
LFE372:
.no_dead_strip _Divide__Divide_param_C_u8
_Divide__Divide_param_C_u8:
LFB373:
	pushq	%rbp
LCFI1359:
	movq	%rsp, %rbp
LCFI1360:
	pushq	%rbx
LCFI1361:
	subq	$24, %rsp
LCFI1362:
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rbx
	movzbl	-25(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L818
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L818
	subq	$1, %rax
	addq	%rcx, %rdx
L818:
	testq	%rax, %rax
	jns	L819
	movl	$13665, %edi
	call	__m3_fault
L819:
	movq	-24(%rbp), %rbx
	movzbl	-25(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L820
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L820
	subq	$1, %rax
	addq	%rcx, %rdx
L820:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI1363:
	ret
LFE373:
.no_dead_strip _Divide__uDivide_var_C_i64
_Divide__uDivide_var_C_i64:
LFB374:
	pushq	%rbp
LCFI1364:
	movq	%rsp, %rbp
LCFI1365:
	movq	160+_MM_Divide(%rip), %rax
	movq	176+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1366:
	ret
LFE374:
.no_dead_strip _Divide__Divide_var_C_i64
_Divide__Divide_var_C_i64:
LFB375:
	pushq	%rbp
LCFI1367:
	movq	%rsp, %rbp
LCFI1368:
	pushq	%rbx
LCFI1369:
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rbx
	movq	176+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L823
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L823
	subq	$1, %rax
	addq	%rcx, %rdx
L823:
	popq	%rbx
	leave
LCFI1370:
	ret
LFE375:
.no_dead_strip _Divide__uDivide_param_C_i64
_Divide__uDivide_param_C_i64:
LFB376:
	pushq	%rbp
LCFI1371:
	movq	%rsp, %rbp
LCFI1372:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	%rax, -24(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1373:
	ret
LFE376:
.no_dead_strip _Divide__Divide_param_C_i64
_Divide__Divide_param_C_i64:
LFB377:
	pushq	%rbp
LCFI1374:
	movq	%rsp, %rbp
LCFI1375:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L826
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L826
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L826:
	leave
LCFI1376:
	ret
LFE377:
.no_dead_strip _Divide__uDivide_var_C_L
_Divide__uDivide_var_C_L:
LFB378:
	pushq	%rbp
LCFI1377:
	movq	%rsp, %rbp
LCFI1378:
	movq	160+_MM_Divide(%rip), %rax
	movq	192+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1379:
	ret
LFE378:
.no_dead_strip _Divide__Divide_var_C_L
_Divide__Divide_var_C_L:
LFB379:
	pushq	%rbp
LCFI1380:
	movq	%rsp, %rbp
LCFI1381:
	pushq	%rbx
LCFI1382:
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rbx
	movq	192+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L829
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L829
	subq	$1, %rax
	addq	%rcx, %rdx
L829:
	popq	%rbx
	leave
LCFI1383:
	ret
LFE379:
.no_dead_strip _Divide__uDivide_param_C_L
_Divide__uDivide_param_C_L:
LFB380:
	pushq	%rbp
LCFI1384:
	movq	%rsp, %rbp
LCFI1385:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	%rax, -24(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1386:
	ret
LFE380:
.no_dead_strip _Divide__Divide_param_C_L
_Divide__Divide_param_C_L:
LFB381:
	pushq	%rbp
LCFI1387:
	movq	%rsp, %rbp
LCFI1388:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L832
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L832
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L832:
	leave
LCFI1389:
	ret
LFE381:
.no_dead_strip _Divide__uDivide_var_C_i8
_Divide__uDivide_var_C_i8:
LFB382:
	pushq	%rbp
LCFI1390:
	movq	%rsp, %rbp
LCFI1391:
	movq	160+_MM_Divide(%rip), %rax
	movzbl	200+_MM_Divide(%rip), %edx
	movsbq	%dl, %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1392:
	ret
LFE382:
.no_dead_strip _Divide__Divide_var_C_i8
_Divide__Divide_var_C_i8:
LFB383:
	pushq	%rbp
LCFI1393:
	movq	%rsp, %rbp
LCFI1394:
	pushq	%rbx
LCFI1395:
	subq	$8, %rsp
LCFI1396:
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rbx
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L835
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L835
	subq	$1, %rax
	addq	%rcx, %rdx
L835:
	testq	%rax, %rax
	jns	L836
	movl	$13985, %edi
	call	__m3_fault
L836:
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rbx
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L837
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L837
	subq	$1, %rax
	addq	%rcx, %rdx
L837:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1397:
	ret
LFE383:
.no_dead_strip _Divide__uDivide_param_C_i8
_Divide__uDivide_param_C_i8:
LFB384:
	pushq	%rbp
LCFI1398:
	movq	%rsp, %rbp
LCFI1399:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movsbq	-9(%rbp), %rax
	movq	%rax, -24(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1400:
	ret
LFE384:
.no_dead_strip _Divide__Divide_param_C_i8
_Divide__Divide_param_C_i8:
LFB385:
	pushq	%rbp
LCFI1401:
	movq	%rsp, %rbp
LCFI1402:
	pushq	%rbx
LCFI1403:
	subq	$24, %rsp
LCFI1404:
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rbx
	movsbq	-25(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L840
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L840
	subq	$1, %rax
	addq	%rcx, %rdx
L840:
	testq	%rax, %rax
	jns	L841
	movl	$14049, %edi
	call	__m3_fault
L841:
	movq	-24(%rbp), %rbx
	movsbq	-25(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L842
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L842
	subq	$1, %rax
	addq	%rcx, %rdx
L842:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI1405:
	ret
LFE385:
.no_dead_strip _Divide__uDivide_var_u8_u16
_Divide__uDivide_var_u8_u16:
LFB386:
	pushq	%rbp
LCFI1406:
	movq	%rsp, %rbp
LCFI1407:
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movzwl	104+_MM_Divide(%rip), %edx
	movzwl	%dx, %edx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1408:
	ret
LFE386:
.no_dead_strip _Divide__Divide_var_u8_u16
_Divide__Divide_var_u8_u16:
LFB387:
	pushq	%rbp
LCFI1409:
	movq	%rsp, %rbp
LCFI1410:
	pushq	%rbx
LCFI1411:
	subq	$8, %rsp
LCFI1412:
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ebx
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L845
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L845
	subq	$1, %rax
	addq	%rcx, %rdx
L845:
	cmpq	$255, %rax
	jle	L846
	movl	$14113, %edi
	call	__m3_fault
L846:
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ebx
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L847
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L847
	subq	$1, %rax
	addq	%rcx, %rdx
L847:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1413:
	ret
LFE387:
.no_dead_strip _Divide__uDivide_param_u8_u16
_Divide__uDivide_param_u8_u16:
LFB388:
	pushq	%rbp
LCFI1414:
	movq	%rsp, %rbp
LCFI1415:
	movb	%dil, -1(%rbp)
	movw	%si, -4(%rbp)
	movzbl	-1(%rbp), %eax
	movzwl	-4(%rbp), %edx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1416:
	ret
LFE388:
.no_dead_strip _Divide__Divide_param_u8_u16
_Divide__Divide_param_u8_u16:
LFB389:
	pushq	%rbp
LCFI1417:
	movq	%rsp, %rbp
LCFI1418:
	pushq	%rbx
LCFI1419:
	subq	$24, %rsp
LCFI1420:
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %ebx
	movzwl	-20(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L850
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L850
	subq	$1, %rax
	addq	%rcx, %rdx
L850:
	cmpq	$255, %rax
	jle	L851
	movl	$14177, %edi
	call	__m3_fault
L851:
	movzbl	-17(%rbp), %ebx
	movzwl	-20(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L852
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L852
	subq	$1, %rax
	addq	%rcx, %rdx
L852:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI1421:
	ret
LFE389:
.no_dead_strip _Divide__uDivide_var_u8_u32
_Divide__uDivide_var_u8_u32:
LFB390:
	pushq	%rbp
LCFI1422:
	movq	%rsp, %rbp
LCFI1423:
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movl	108+_MM_Divide(%rip), %edx
	mov	%edx, %edx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1424:
	ret
LFE390:
.no_dead_strip _Divide__Divide_var_u8_u32
_Divide__Divide_var_u8_u32:
LFB391:
	pushq	%rbp
LCFI1425:
	movq	%rsp, %rbp
LCFI1426:
	pushq	%rbx
LCFI1427:
	subq	$8, %rsp
LCFI1428:
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ebx
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L855
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L855
	subq	$1, %rax
	addq	%rcx, %rdx
L855:
	cmpq	$255, %rax
	jle	L856
	movl	$14241, %edi
	call	__m3_fault
L856:
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ebx
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L857
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L857
	subq	$1, %rax
	addq	%rcx, %rdx
L857:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1429:
	ret
LFE391:
.no_dead_strip _Divide__uDivide_param_u8_u32
_Divide__uDivide_param_u8_u32:
LFB392:
	pushq	%rbp
LCFI1430:
	movq	%rsp, %rbp
LCFI1431:
	movb	%dil, -1(%rbp)
	movl	%esi, -8(%rbp)
	movzbl	-1(%rbp), %eax
	mov	-8(%rbp), %edx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1432:
	ret
LFE392:
.no_dead_strip _Divide__Divide_param_u8_u32
_Divide__Divide_param_u8_u32:
LFB393:
	pushq	%rbp
LCFI1433:
	movq	%rsp, %rbp
LCFI1434:
	pushq	%rbx
LCFI1435:
	subq	$24, %rsp
LCFI1436:
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %ebx
	mov	-24(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L860
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L860
	subq	$1, %rax
	addq	%rcx, %rdx
L860:
	cmpq	$255, %rax
	jle	L861
	movl	$14305, %edi
	call	__m3_fault
L861:
	movzbl	-17(%rbp), %ebx
	mov	-24(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L862
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L862
	subq	$1, %rax
	addq	%rcx, %rdx
L862:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI1437:
	ret
LFE393:
.no_dead_strip _Divide__uDivide_var_u8_LC
_Divide__uDivide_var_u8_LC:
LFB394:
	pushq	%rbp
LCFI1438:
	movq	%rsp, %rbp
LCFI1439:
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	112+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1440:
	ret
LFE394:
.no_dead_strip _Divide__Divide_var_u8_LC
_Divide__Divide_var_u8_LC:
LFB395:
	pushq	%rbp
LCFI1441:
	movq	%rsp, %rbp
LCFI1442:
	pushq	%rbx
LCFI1443:
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ebx
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L865
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L865
	subq	$1, %rax
	addq	%rcx, %rdx
L865:
	popq	%rbx
	leave
LCFI1444:
	ret
LFE395:
.no_dead_strip _Divide__uDivide_param_u8_LC
_Divide__uDivide_param_u8_LC:
LFB396:
	pushq	%rbp
LCFI1445:
	movq	%rsp, %rbp
LCFI1446:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movzbl	-1(%rbp), %eax
	movl	$0, %edx
	divq	-16(%rbp)
	leave
LCFI1447:
	ret
LFE396:
.no_dead_strip _Divide__Divide_param_u8_LC
_Divide__Divide_param_u8_LC:
LFB397:
	pushq	%rbp
LCFI1448:
	movq	%rsp, %rbp
LCFI1449:
	pushq	%rbx
LCFI1450:
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %ebx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L868
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L868
	subq	$1, %rax
	addq	%rcx, %rdx
L868:
	popq	%rbx
	leave
LCFI1451:
	ret
LFE397:
.no_dead_strip _Divide__uDivide_var_u8_I
_Divide__uDivide_var_u8_I:
LFB398:
	pushq	%rbp
LCFI1452:
	movq	%rsp, %rbp
LCFI1453:
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	120+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1454:
	ret
LFE398:
.no_dead_strip _Divide__Divide_var_u8_I
_Divide__Divide_var_u8_I:
LFB399:
	pushq	%rbp
LCFI1455:
	movq	%rsp, %rbp
LCFI1456:
	pushq	%rbx
LCFI1457:
	subq	$8, %rsp
LCFI1458:
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ebx
	movq	120+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L871
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L871
	subq	$1, %rax
	addq	%rcx, %rdx
L871:
	testq	%rax, %rax
	js	L872
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ebx
	movq	120+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L873
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L873
	subq	$1, %rax
	addq	%rcx, %rdx
L873:
	cmpq	$255, %rax
	jle	L874
L872:
	movl	$14497, %edi
	call	__m3_fault
L874:
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ebx
	movq	120+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L875
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L875
	subq	$1, %rax
	addq	%rcx, %rdx
L875:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1459:
	ret
LFE399:
.no_dead_strip _Divide__uDivide_param_u8_I
_Divide__uDivide_param_u8_I:
LFB400:
	pushq	%rbp
LCFI1460:
	movq	%rsp, %rbp
LCFI1461:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movzbl	-1(%rbp), %eax
	movq	-16(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1462:
	ret
LFE400:
.no_dead_strip _Divide__Divide_param_u8_I
_Divide__Divide_param_u8_I:
LFB401:
	pushq	%rbp
LCFI1463:
	movq	%rsp, %rbp
LCFI1464:
	subq	$16, %rsp
LCFI1465:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movzbl	-1(%rbp), %ecx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L878
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L878
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L878:
	testq	%rax, %rax
	js	L879
	movzbl	-1(%rbp), %ecx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L880
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L880
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L880:
	cmpq	$255, %rax
	jle	L881
L879:
	movl	$14561, %edi
	call	__m3_fault
L881:
	movzbl	-1(%rbp), %ecx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L882
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L882
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L882:
	leave
LCFI1466:
	ret
LFE401:
.no_dead_strip _Divide__uDivide_var_u8_i16
_Divide__uDivide_var_u8_i16:
LFB402:
	pushq	%rbp
LCFI1467:
	movq	%rsp, %rbp
LCFI1468:
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movzwl	128+_MM_Divide(%rip), %edx
	movswq	%dx, %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1469:
	ret
LFE402:
.no_dead_strip _Divide__Divide_var_u8_i16
_Divide__Divide_var_u8_i16:
LFB403:
	pushq	%rbp
LCFI1470:
	movq	%rsp, %rbp
LCFI1471:
	pushq	%rbx
LCFI1472:
	subq	$8, %rsp
LCFI1473:
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ebx
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L885
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L885
	subq	$1, %rax
	addq	%rcx, %rdx
L885:
	testq	%rax, %rax
	js	L886
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ebx
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L887
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L887
	subq	$1, %rax
	addq	%rcx, %rdx
L887:
	cmpq	$255, %rax
	jle	L888
L886:
	movl	$14625, %edi
	call	__m3_fault
L888:
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ebx
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L889
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L889
	subq	$1, %rax
	addq	%rcx, %rdx
L889:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1474:
	ret
LFE403:
.no_dead_strip _Divide__uDivide_param_u8_i16
_Divide__uDivide_param_u8_i16:
LFB404:
	pushq	%rbp
LCFI1475:
	movq	%rsp, %rbp
LCFI1476:
	movb	%dil, -1(%rbp)
	movw	%si, -4(%rbp)
	movzbl	-1(%rbp), %eax
	movswq	-4(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1477:
	ret
LFE404:
.no_dead_strip _Divide__Divide_param_u8_i16
_Divide__Divide_param_u8_i16:
LFB405:
	pushq	%rbp
LCFI1478:
	movq	%rsp, %rbp
LCFI1479:
	pushq	%rbx
LCFI1480:
	subq	$24, %rsp
LCFI1481:
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %ebx
	movswq	-20(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L892
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L892
	subq	$1, %rax
	addq	%rcx, %rdx
L892:
	testq	%rax, %rax
	js	L893
	movzbl	-17(%rbp), %ebx
	movswq	-20(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L894
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L894
	subq	$1, %rax
	addq	%rcx, %rdx
L894:
	cmpq	$255, %rax
	jle	L895
L893:
	movl	$14689, %edi
	call	__m3_fault
L895:
	movzbl	-17(%rbp), %ebx
	movswq	-20(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L896
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L896
	subq	$1, %rax
	addq	%rcx, %rdx
L896:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI1482:
	ret
LFE405:
.no_dead_strip _Divide__uDivide_var_u8_u64
_Divide__uDivide_var_u8_u64:
LFB406:
	pushq	%rbp
LCFI1483:
	movq	%rsp, %rbp
LCFI1484:
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	136+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1485:
	ret
LFE406:
.no_dead_strip _Divide__Divide_var_u8_u64
_Divide__Divide_var_u8_u64:
LFB407:
	pushq	%rbp
LCFI1486:
	movq	%rsp, %rbp
LCFI1487:
	pushq	%rbx
LCFI1488:
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ebx
	movq	136+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L899
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L899
	subq	$1, %rax
	addq	%rcx, %rdx
L899:
	popq	%rbx
	leave
LCFI1489:
	ret
LFE407:
.no_dead_strip _Divide__uDivide_param_u8_u64
_Divide__uDivide_param_u8_u64:
LFB408:
	pushq	%rbp
LCFI1490:
	movq	%rsp, %rbp
LCFI1491:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movzbl	-1(%rbp), %eax
	movq	-16(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1492:
	ret
LFE408:
.no_dead_strip _Divide__Divide_param_u8_u64
_Divide__Divide_param_u8_u64:
LFB409:
	pushq	%rbp
LCFI1493:
	movq	%rsp, %rbp
LCFI1494:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movzbl	-1(%rbp), %ecx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L902
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L902
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L902:
	leave
LCFI1495:
	ret
LFE409:
.no_dead_strip _Divide__uDivide_var_u8_i32
_Divide__uDivide_var_u8_i32:
LFB410:
	pushq	%rbp
LCFI1496:
	movq	%rsp, %rbp
LCFI1497:
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movl	152+_MM_Divide(%rip), %edx
	movslq	%edx, %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1498:
	ret
LFE410:
.no_dead_strip _Divide__Divide_var_u8_i32
_Divide__Divide_var_u8_i32:
LFB411:
	pushq	%rbp
LCFI1499:
	movq	%rsp, %rbp
LCFI1500:
	pushq	%rbx
LCFI1501:
	subq	$8, %rsp
LCFI1502:
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ebx
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L905
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L905
	subq	$1, %rax
	addq	%rcx, %rdx
L905:
	testq	%rax, %rax
	js	L906
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ebx
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L907
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L907
	subq	$1, %rax
	addq	%rcx, %rdx
L907:
	cmpq	$255, %rax
	jle	L908
L906:
	movl	$14881, %edi
	call	__m3_fault
L908:
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ebx
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L909
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L909
	subq	$1, %rax
	addq	%rcx, %rdx
L909:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1503:
	ret
LFE411:
.no_dead_strip _Divide__uDivide_param_u8_i32
_Divide__uDivide_param_u8_i32:
LFB412:
	pushq	%rbp
LCFI1504:
	movq	%rsp, %rbp
LCFI1505:
	movb	%dil, -1(%rbp)
	movl	%esi, -8(%rbp)
	movzbl	-1(%rbp), %eax
	movl	-8(%rbp), %edx
	movslq	%edx, %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1506:
	ret
LFE412:
.no_dead_strip _Divide__Divide_param_u8_i32
_Divide__Divide_param_u8_i32:
LFB413:
	pushq	%rbp
LCFI1507:
	movq	%rsp, %rbp
LCFI1508:
	pushq	%rbx
LCFI1509:
	subq	$24, %rsp
LCFI1510:
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %ebx
	movl	-24(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L912
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L912
	subq	$1, %rax
	addq	%rcx, %rdx
L912:
	testq	%rax, %rax
	js	L913
	movzbl	-17(%rbp), %ebx
	movl	-24(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L914
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L914
	subq	$1, %rax
	addq	%rcx, %rdx
L914:
	cmpq	$255, %rax
	jle	L915
L913:
	movl	$14945, %edi
	call	__m3_fault
L915:
	movzbl	-17(%rbp), %ebx
	movl	-24(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L916
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L916
	subq	$1, %rax
	addq	%rcx, %rdx
L916:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI1511:
	ret
LFE413:
.no_dead_strip _Divide__uDivide_var_u8_C
_Divide__uDivide_var_u8_C:
LFB414:
	pushq	%rbp
LCFI1512:
	movq	%rsp, %rbp
LCFI1513:
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	160+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1514:
	ret
LFE414:
.no_dead_strip _Divide__Divide_var_u8_C
_Divide__Divide_var_u8_C:
LFB415:
	pushq	%rbp
LCFI1515:
	movq	%rsp, %rbp
LCFI1516:
	pushq	%rbx
LCFI1517:
	subq	$8, %rsp
LCFI1518:
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ebx
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L919
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L919
	subq	$1, %rax
	addq	%rcx, %rdx
L919:
	testq	%rax, %rax
	js	L920
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ebx
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L921
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L921
	subq	$1, %rax
	addq	%rcx, %rdx
L921:
	cmpq	$255, %rax
	jle	L922
L920:
	movl	$15009, %edi
	call	__m3_fault
L922:
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ebx
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L923
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L923
	subq	$1, %rax
	addq	%rcx, %rdx
L923:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1519:
	ret
LFE415:
.no_dead_strip _Divide__uDivide_param_u8_C
_Divide__uDivide_param_u8_C:
LFB416:
	pushq	%rbp
LCFI1520:
	movq	%rsp, %rbp
LCFI1521:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movzbl	-1(%rbp), %eax
	movl	$0, %edx
	divq	-16(%rbp)
	leave
LCFI1522:
	ret
LFE416:
.no_dead_strip _Divide__Divide_param_u8_C
_Divide__Divide_param_u8_C:
LFB417:
	pushq	%rbp
LCFI1523:
	movq	%rsp, %rbp
LCFI1524:
	pushq	%rbx
LCFI1525:
	subq	$24, %rsp
LCFI1526:
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %ebx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L926
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L926
	subq	$1, %rax
	addq	%rcx, %rdx
L926:
	testq	%rax, %rax
	js	L927
	movzbl	-17(%rbp), %ebx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L928
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L928
	subq	$1, %rax
	addq	%rcx, %rdx
L928:
	cmpq	$255, %rax
	jle	L929
L927:
	movl	$15073, %edi
	call	__m3_fault
L929:
	movzbl	-17(%rbp), %ebx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L930
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L930
	subq	$1, %rax
	addq	%rcx, %rdx
L930:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI1527:
	ret
LFE417:
.no_dead_strip _Divide__uDivide_var_u8_u8
_Divide__uDivide_var_u8_u8:
LFB418:
	pushq	%rbp
LCFI1528:
	movq	%rsp, %rbp
LCFI1529:
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movzbl	168+_MM_Divide(%rip), %edx
	movzbl	%dl, %edx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1530:
	ret
LFE418:
.no_dead_strip _Divide__Divide_var_u8_u8
_Divide__Divide_var_u8_u8:
LFB419:
	pushq	%rbp
LCFI1531:
	movq	%rsp, %rbp
LCFI1532:
	pushq	%rbx
LCFI1533:
	subq	$8, %rsp
LCFI1534:
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ebx
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L933
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L933
	subq	$1, %rax
	addq	%rcx, %rdx
L933:
	cmpq	$255, %rax
	jle	L934
	movl	$15137, %edi
	call	__m3_fault
L934:
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ebx
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L935
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L935
	subq	$1, %rax
	addq	%rcx, %rdx
L935:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1535:
	ret
LFE419:
.no_dead_strip _Divide__uDivide_param_u8_u8
_Divide__uDivide_param_u8_u8:
LFB420:
	pushq	%rbp
LCFI1536:
	movq	%rsp, %rbp
LCFI1537:
	movb	%dil, -1(%rbp)
	movb	%sil, -2(%rbp)
	movzbl	-1(%rbp), %eax
	movzbl	-2(%rbp), %edx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1538:
	ret
LFE420:
.no_dead_strip _Divide__Divide_param_u8_u8
_Divide__Divide_param_u8_u8:
LFB421:
	pushq	%rbp
LCFI1539:
	movq	%rsp, %rbp
LCFI1540:
	pushq	%rbx
LCFI1541:
	subq	$24, %rsp
LCFI1542:
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %ebx
	movzbl	-18(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L938
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L938
	subq	$1, %rax
	addq	%rcx, %rdx
L938:
	cmpq	$255, %rax
	jle	L939
	movl	$15201, %edi
	call	__m3_fault
L939:
	movzbl	-17(%rbp), %ebx
	movzbl	-18(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L940
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L940
	subq	$1, %rax
	addq	%rcx, %rdx
L940:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI1543:
	ret
LFE421:
.no_dead_strip _Divide__uDivide_var_u8_i64
_Divide__uDivide_var_u8_i64:
LFB422:
	pushq	%rbp
LCFI1544:
	movq	%rsp, %rbp
LCFI1545:
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	176+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1546:
	ret
LFE422:
.no_dead_strip _Divide__Divide_var_u8_i64
_Divide__Divide_var_u8_i64:
LFB423:
	pushq	%rbp
LCFI1547:
	movq	%rsp, %rbp
LCFI1548:
	pushq	%rbx
LCFI1549:
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ebx
	movq	176+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L943
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L943
	subq	$1, %rax
	addq	%rcx, %rdx
L943:
	popq	%rbx
	leave
LCFI1550:
	ret
LFE423:
.no_dead_strip _Divide__uDivide_param_u8_i64
_Divide__uDivide_param_u8_i64:
LFB424:
	pushq	%rbp
LCFI1551:
	movq	%rsp, %rbp
LCFI1552:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movzbl	-1(%rbp), %eax
	movq	-16(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1553:
	ret
LFE424:
.no_dead_strip _Divide__Divide_param_u8_i64
_Divide__Divide_param_u8_i64:
LFB425:
	pushq	%rbp
LCFI1554:
	movq	%rsp, %rbp
LCFI1555:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movzbl	-1(%rbp), %ecx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L946
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L946
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L946:
	leave
LCFI1556:
	ret
LFE425:
.no_dead_strip _Divide__uDivide_var_u8_L
_Divide__uDivide_var_u8_L:
LFB426:
	pushq	%rbp
LCFI1557:
	movq	%rsp, %rbp
LCFI1558:
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movq	192+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1559:
	ret
LFE426:
.no_dead_strip _Divide__Divide_var_u8_L
_Divide__Divide_var_u8_L:
LFB427:
	pushq	%rbp
LCFI1560:
	movq	%rsp, %rbp
LCFI1561:
	pushq	%rbx
LCFI1562:
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ebx
	movq	192+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L949
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L949
	subq	$1, %rax
	addq	%rcx, %rdx
L949:
	popq	%rbx
	leave
LCFI1563:
	ret
LFE427:
.no_dead_strip _Divide__uDivide_param_u8_L
_Divide__uDivide_param_u8_L:
LFB428:
	pushq	%rbp
LCFI1564:
	movq	%rsp, %rbp
LCFI1565:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movzbl	-1(%rbp), %eax
	movq	-16(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1566:
	ret
LFE428:
.no_dead_strip _Divide__Divide_param_u8_L
_Divide__Divide_param_u8_L:
LFB429:
	pushq	%rbp
LCFI1567:
	movq	%rsp, %rbp
LCFI1568:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movzbl	-1(%rbp), %ecx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L952
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L952
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L952:
	leave
LCFI1569:
	ret
LFE429:
.no_dead_strip _Divide__uDivide_var_u8_i8
_Divide__uDivide_var_u8_i8:
LFB430:
	pushq	%rbp
LCFI1570:
	movq	%rsp, %rbp
LCFI1571:
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %eax
	movzbl	200+_MM_Divide(%rip), %edx
	movsbq	%dl, %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1572:
	ret
LFE430:
.no_dead_strip _Divide__Divide_var_u8_i8
_Divide__Divide_var_u8_i8:
LFB431:
	pushq	%rbp
LCFI1573:
	movq	%rsp, %rbp
LCFI1574:
	pushq	%rbx
LCFI1575:
	subq	$8, %rsp
LCFI1576:
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ebx
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L955
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L955
	subq	$1, %rax
	addq	%rcx, %rdx
L955:
	testq	%rax, %rax
	js	L956
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ebx
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L957
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L957
	subq	$1, %rax
	addq	%rcx, %rdx
L957:
	cmpq	$255, %rax
	jle	L958
L956:
	movl	$15521, %edi
	call	__m3_fault
L958:
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ebx
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L959
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L959
	subq	$1, %rax
	addq	%rcx, %rdx
L959:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1577:
	ret
LFE431:
.no_dead_strip _Divide__uDivide_param_u8_i8
_Divide__uDivide_param_u8_i8:
LFB432:
	pushq	%rbp
LCFI1578:
	movq	%rsp, %rbp
LCFI1579:
	movb	%dil, -1(%rbp)
	movb	%sil, -2(%rbp)
	movzbl	-1(%rbp), %eax
	movsbq	-2(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1580:
	ret
LFE432:
.no_dead_strip _Divide__Divide_param_u8_i8
_Divide__Divide_param_u8_i8:
LFB433:
	pushq	%rbp
LCFI1581:
	movq	%rsp, %rbp
LCFI1582:
	pushq	%rbx
LCFI1583:
	subq	$24, %rsp
LCFI1584:
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %ebx
	movsbq	-18(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L962
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L962
	subq	$1, %rax
	addq	%rcx, %rdx
L962:
	testq	%rax, %rax
	js	L963
	movzbl	-17(%rbp), %ebx
	movsbq	-18(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L964
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L964
	subq	$1, %rax
	addq	%rcx, %rdx
L964:
	cmpq	$255, %rax
	jle	L965
L963:
	movl	$15585, %edi
	call	__m3_fault
L965:
	movzbl	-17(%rbp), %ebx
	movsbq	-18(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L966
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L966
	subq	$1, %rax
	addq	%rcx, %rdx
L966:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI1585:
	ret
LFE433:
.no_dead_strip _Divide__uDivide_var_i64_u16
_Divide__uDivide_var_i64_u16:
LFB434:
	pushq	%rbp
LCFI1586:
	movq	%rsp, %rbp
LCFI1587:
	movq	176+_MM_Divide(%rip), %rax
	movzwl	104+_MM_Divide(%rip), %edx
	movzwl	%dx, %edx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1588:
	ret
LFE434:
.no_dead_strip _Divide__Divide_var_i64_u16
_Divide__Divide_var_i64_u16:
LFB435:
	pushq	%rbp
LCFI1589:
	movq	%rsp, %rbp
LCFI1590:
	pushq	%rbx
LCFI1591:
	movq	176+_MM_Divide(%rip), %rbx
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L969
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L969
	subq	$1, %rax
	addq	%rcx, %rdx
L969:
	popq	%rbx
	leave
LCFI1592:
	ret
LFE435:
.no_dead_strip _Divide__uDivide_param_i64_u16
_Divide__uDivide_param_i64_u16:
LFB436:
	pushq	%rbp
LCFI1593:
	movq	%rsp, %rbp
LCFI1594:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movq	-8(%rbp), %rax
	movzwl	-10(%rbp), %edx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1595:
	ret
LFE436:
.no_dead_strip _Divide__Divide_param_i64_u16
_Divide__Divide_param_i64_u16:
LFB437:
	pushq	%rbp
LCFI1596:
	movq	%rsp, %rbp
LCFI1597:
	pushq	%rbx
LCFI1598:
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %ecx
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L972
	movq	%rcx, %rbx
	xorq	-24(%rbp), %rbx
	testq	%rbx, %rbx
	jns	L972
	subq	$1, %rax
	addq	%rcx, %rdx
L972:
	popq	%rbx
	leave
LCFI1599:
	ret
LFE437:
.no_dead_strip _Divide__uDivide_var_i64_u32
_Divide__uDivide_var_i64_u32:
LFB438:
	pushq	%rbp
LCFI1600:
	movq	%rsp, %rbp
LCFI1601:
	movq	176+_MM_Divide(%rip), %rax
	movl	108+_MM_Divide(%rip), %edx
	mov	%edx, %edx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1602:
	ret
LFE438:
.no_dead_strip _Divide__Divide_var_i64_u32
_Divide__Divide_var_i64_u32:
LFB439:
	pushq	%rbp
LCFI1603:
	movq	%rsp, %rbp
LCFI1604:
	pushq	%rbx
LCFI1605:
	movq	176+_MM_Divide(%rip), %rbx
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L975
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L975
	subq	$1, %rax
	addq	%rcx, %rdx
L975:
	popq	%rbx
	leave
LCFI1606:
	ret
LFE439:
.no_dead_strip _Divide__uDivide_param_i64_u32
_Divide__uDivide_param_i64_u32:
LFB440:
	pushq	%rbp
LCFI1607:
	movq	%rsp, %rbp
LCFI1608:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movq	-8(%rbp), %rax
	mov	-12(%rbp), %edx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1609:
	ret
LFE440:
.no_dead_strip _Divide__Divide_param_i64_u32
_Divide__Divide_param_i64_u32:
LFB441:
	pushq	%rbp
LCFI1610:
	movq	%rsp, %rbp
LCFI1611:
	pushq	%rbx
LCFI1612:
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %ecx
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L978
	movq	%rcx, %rbx
	xorq	-24(%rbp), %rbx
	testq	%rbx, %rbx
	jns	L978
	subq	$1, %rax
	addq	%rcx, %rdx
L978:
	popq	%rbx
	leave
LCFI1613:
	ret
LFE441:
.no_dead_strip _Divide__uDivide_var_i64_LC
_Divide__uDivide_var_i64_LC:
LFB442:
	pushq	%rbp
LCFI1614:
	movq	%rsp, %rbp
LCFI1615:
	movq	176+_MM_Divide(%rip), %rax
	movq	112+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1616:
	ret
LFE442:
.no_dead_strip _Divide__Divide_var_i64_LC
_Divide__Divide_var_i64_LC:
LFB443:
	pushq	%rbp
LCFI1617:
	movq	%rsp, %rbp
LCFI1618:
	pushq	%rbx
LCFI1619:
	movq	176+_MM_Divide(%rip), %rbx
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L981
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L981
	subq	$1, %rax
	addq	%rcx, %rdx
L981:
	popq	%rbx
	leave
LCFI1620:
	ret
LFE443:
.no_dead_strip _Divide__uDivide_param_i64_LC
_Divide__uDivide_param_i64_LC:
LFB444:
	pushq	%rbp
LCFI1621:
	movq	%rsp, %rbp
LCFI1622:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-16(%rbp)
	leave
LCFI1623:
	ret
LFE444:
.no_dead_strip _Divide__Divide_param_i64_LC
_Divide__Divide_param_i64_LC:
LFB445:
	pushq	%rbp
LCFI1624:
	movq	%rsp, %rbp
LCFI1625:
	pushq	%rbx
LCFI1626:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rcx
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L984
	movq	%rcx, %rbx
	xorq	-24(%rbp), %rbx
	testq	%rbx, %rbx
	jns	L984
	subq	$1, %rax
	addq	%rcx, %rdx
L984:
	popq	%rbx
	leave
LCFI1627:
	ret
LFE445:
.no_dead_strip _Divide__uDivide_var_i64_I
_Divide__uDivide_var_i64_I:
LFB446:
	pushq	%rbp
LCFI1628:
	movq	%rsp, %rbp
LCFI1629:
	movq	176+_MM_Divide(%rip), %rax
	movq	120+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1630:
	ret
LFE446:
.no_dead_strip _Divide__Divide_var_i64_I
_Divide__Divide_var_i64_I:
LFB447:
	pushq	%rbp
LCFI1631:
	movq	%rsp, %rbp
LCFI1632:
	pushq	%rbx
LCFI1633:
	movq	176+_MM_Divide(%rip), %rbx
	movq	120+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L987
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L987
	subq	$1, %rax
	addq	%rcx, %rdx
L987:
	popq	%rbx
	leave
LCFI1634:
	ret
LFE447:
.no_dead_strip _Divide__uDivide_param_i64_I
_Divide__uDivide_param_i64_I:
LFB448:
	pushq	%rbp
LCFI1635:
	movq	%rsp, %rbp
LCFI1636:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1637:
	ret
LFE448:
.no_dead_strip _Divide__Divide_param_i64_I
_Divide__Divide_param_i64_I:
LFB449:
	pushq	%rbp
LCFI1638:
	movq	%rsp, %rbp
LCFI1639:
	pushq	%rbx
LCFI1640:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-32(%rbp)
	testq	%rdx, %rdx
	je	L990
	movq	-32(%rbp), %rcx
	movq	-24(%rbp), %rbx
	xorq	%rbx, %rcx
	testq	%rcx, %rcx
	jns	L990
	subq	$1, %rax
	addq	-32(%rbp), %rdx
L990:
	popq	%rbx
	leave
LCFI1641:
	ret
LFE449:
.no_dead_strip _Divide__uDivide_var_i64_i16
_Divide__uDivide_var_i64_i16:
LFB450:
	pushq	%rbp
LCFI1642:
	movq	%rsp, %rbp
LCFI1643:
	movq	176+_MM_Divide(%rip), %rax
	movzwl	128+_MM_Divide(%rip), %edx
	movswq	%dx, %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1644:
	ret
LFE450:
.no_dead_strip _Divide__Divide_var_i64_i16
_Divide__Divide_var_i64_i16:
LFB451:
	pushq	%rbp
LCFI1645:
	movq	%rsp, %rbp
LCFI1646:
	pushq	%rbx
LCFI1647:
	movq	176+_MM_Divide(%rip), %rbx
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L993
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L993
	subq	$1, %rax
	addq	%rcx, %rdx
L993:
	popq	%rbx
	leave
LCFI1648:
	ret
LFE451:
.no_dead_strip _Divide__uDivide_param_i64_i16
_Divide__uDivide_param_i64_i16:
LFB452:
	pushq	%rbp
LCFI1649:
	movq	%rsp, %rbp
LCFI1650:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movq	-8(%rbp), %rax
	movswq	-10(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1651:
	ret
LFE452:
.no_dead_strip _Divide__Divide_param_i64_i16
_Divide__Divide_param_i64_i16:
LFB453:
	pushq	%rbp
LCFI1652:
	movq	%rsp, %rbp
LCFI1653:
	pushq	%rbx
LCFI1654:
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp), %rcx
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L996
	movq	%rcx, %rbx
	xorq	-24(%rbp), %rbx
	testq	%rbx, %rbx
	jns	L996
	subq	$1, %rax
	addq	%rcx, %rdx
L996:
	popq	%rbx
	leave
LCFI1655:
	ret
LFE453:
.no_dead_strip _Divide__uDivide_var_i64_u64
_Divide__uDivide_var_i64_u64:
LFB454:
	pushq	%rbp
LCFI1656:
	movq	%rsp, %rbp
LCFI1657:
	movq	176+_MM_Divide(%rip), %rax
	movq	136+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1658:
	ret
LFE454:
.no_dead_strip _Divide__Divide_var_i64_u64
_Divide__Divide_var_i64_u64:
LFB455:
	pushq	%rbp
LCFI1659:
	movq	%rsp, %rbp
LCFI1660:
	pushq	%rbx
LCFI1661:
	movq	176+_MM_Divide(%rip), %rbx
	movq	136+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L999
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L999
	subq	$1, %rax
	addq	%rcx, %rdx
L999:
	popq	%rbx
	leave
LCFI1662:
	ret
LFE455:
.no_dead_strip _Divide__uDivide_param_i64_u64
_Divide__uDivide_param_i64_u64:
LFB456:
	pushq	%rbp
LCFI1663:
	movq	%rsp, %rbp
LCFI1664:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1665:
	ret
LFE456:
.no_dead_strip _Divide__Divide_param_i64_u64
_Divide__Divide_param_i64_u64:
LFB457:
	pushq	%rbp
LCFI1666:
	movq	%rsp, %rbp
LCFI1667:
	pushq	%rbx
LCFI1668:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-32(%rbp)
	testq	%rdx, %rdx
	je	L1002
	movq	-32(%rbp), %rcx
	movq	-24(%rbp), %rbx
	xorq	%rbx, %rcx
	testq	%rcx, %rcx
	jns	L1002
	subq	$1, %rax
	addq	-32(%rbp), %rdx
L1002:
	popq	%rbx
	leave
LCFI1669:
	ret
LFE457:
.no_dead_strip _Divide__uDivide_var_i64_i32
_Divide__uDivide_var_i64_i32:
LFB458:
	pushq	%rbp
LCFI1670:
	movq	%rsp, %rbp
LCFI1671:
	movq	176+_MM_Divide(%rip), %rax
	movl	152+_MM_Divide(%rip), %edx
	movslq	%edx, %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1672:
	ret
LFE458:
.no_dead_strip _Divide__Divide_var_i64_i32
_Divide__Divide_var_i64_i32:
LFB459:
	pushq	%rbp
LCFI1673:
	movq	%rsp, %rbp
LCFI1674:
	pushq	%rbx
LCFI1675:
	movq	176+_MM_Divide(%rip), %rbx
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1005
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1005
	subq	$1, %rax
	addq	%rcx, %rdx
L1005:
	popq	%rbx
	leave
LCFI1676:
	ret
LFE459:
.no_dead_strip _Divide__uDivide_param_i64_i32
_Divide__uDivide_param_i64_i32:
LFB460:
	pushq	%rbp
LCFI1677:
	movq	%rsp, %rbp
LCFI1678:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movq	-8(%rbp), %rax
	movl	-12(%rbp), %edx
	movslq	%edx, %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1679:
	ret
LFE460:
.no_dead_strip _Divide__Divide_param_i64_i32
_Divide__Divide_param_i64_i32:
LFB461:
	pushq	%rbp
LCFI1680:
	movq	%rsp, %rbp
LCFI1681:
	pushq	%rbx
LCFI1682:
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax, %rcx
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1008
	movq	%rcx, %rbx
	xorq	-24(%rbp), %rbx
	testq	%rbx, %rbx
	jns	L1008
	subq	$1, %rax
	addq	%rcx, %rdx
L1008:
	popq	%rbx
	leave
LCFI1683:
	ret
LFE461:
.no_dead_strip _Divide__uDivide_var_i64_C
_Divide__uDivide_var_i64_C:
LFB462:
	pushq	%rbp
LCFI1684:
	movq	%rsp, %rbp
LCFI1685:
	movq	176+_MM_Divide(%rip), %rax
	movq	160+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1686:
	ret
LFE462:
.no_dead_strip _Divide__Divide_var_i64_C
_Divide__Divide_var_i64_C:
LFB463:
	pushq	%rbp
LCFI1687:
	movq	%rsp, %rbp
LCFI1688:
	pushq	%rbx
LCFI1689:
	movq	176+_MM_Divide(%rip), %rbx
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1011
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1011
	subq	$1, %rax
	addq	%rcx, %rdx
L1011:
	popq	%rbx
	leave
LCFI1690:
	ret
LFE463:
.no_dead_strip _Divide__uDivide_param_i64_C
_Divide__uDivide_param_i64_C:
LFB464:
	pushq	%rbp
LCFI1691:
	movq	%rsp, %rbp
LCFI1692:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-16(%rbp)
	leave
LCFI1693:
	ret
LFE464:
.no_dead_strip _Divide__Divide_param_i64_C
_Divide__Divide_param_i64_C:
LFB465:
	pushq	%rbp
LCFI1694:
	movq	%rsp, %rbp
LCFI1695:
	pushq	%rbx
LCFI1696:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rcx
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1014
	movq	%rcx, %rbx
	xorq	-24(%rbp), %rbx
	testq	%rbx, %rbx
	jns	L1014
	subq	$1, %rax
	addq	%rcx, %rdx
L1014:
	popq	%rbx
	leave
LCFI1697:
	ret
LFE465:
.no_dead_strip _Divide__uDivide_var_i64_u8
_Divide__uDivide_var_i64_u8:
LFB466:
	pushq	%rbp
LCFI1698:
	movq	%rsp, %rbp
LCFI1699:
	movq	176+_MM_Divide(%rip), %rax
	movzbl	168+_MM_Divide(%rip), %edx
	movzbl	%dl, %edx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1700:
	ret
LFE466:
.no_dead_strip _Divide__Divide_var_i64_u8
_Divide__Divide_var_i64_u8:
LFB467:
	pushq	%rbp
LCFI1701:
	movq	%rsp, %rbp
LCFI1702:
	pushq	%rbx
LCFI1703:
	movq	176+_MM_Divide(%rip), %rbx
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1017
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1017
	subq	$1, %rax
	addq	%rcx, %rdx
L1017:
	popq	%rbx
	leave
LCFI1704:
	ret
LFE467:
.no_dead_strip _Divide__uDivide_param_i64_u8
_Divide__uDivide_param_i64_u8:
LFB468:
	pushq	%rbp
LCFI1705:
	movq	%rsp, %rbp
LCFI1706:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movq	-8(%rbp), %rax
	movzbl	-9(%rbp), %edx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1707:
	ret
LFE468:
.no_dead_strip _Divide__Divide_param_i64_u8
_Divide__Divide_param_i64_u8:
LFB469:
	pushq	%rbp
LCFI1708:
	movq	%rsp, %rbp
LCFI1709:
	pushq	%rbx
LCFI1710:
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %ecx
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1020
	movq	%rcx, %rbx
	xorq	-24(%rbp), %rbx
	testq	%rbx, %rbx
	jns	L1020
	subq	$1, %rax
	addq	%rcx, %rdx
L1020:
	popq	%rbx
	leave
LCFI1711:
	ret
LFE469:
.no_dead_strip _Divide__uDivide_var_i64_i64
_Divide__uDivide_var_i64_i64:
LFB470:
	pushq	%rbp
LCFI1712:
	movq	%rsp, %rbp
LCFI1713:
	movq	176+_MM_Divide(%rip), %rax
	movq	176+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1714:
	ret
LFE470:
.no_dead_strip _Divide__Divide_var_i64_i64
_Divide__Divide_var_i64_i64:
LFB471:
	pushq	%rbp
LCFI1715:
	movq	%rsp, %rbp
LCFI1716:
	pushq	%rbx
LCFI1717:
	movq	176+_MM_Divide(%rip), %rbx
	movq	176+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1023
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1023
	subq	$1, %rax
	addq	%rcx, %rdx
L1023:
	popq	%rbx
	leave
LCFI1718:
	ret
LFE471:
.no_dead_strip _Divide__uDivide_param_i64_i64
_Divide__uDivide_param_i64_i64:
LFB472:
	pushq	%rbp
LCFI1719:
	movq	%rsp, %rbp
LCFI1720:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1721:
	ret
LFE472:
.no_dead_strip _Divide__Divide_param_i64_i64
_Divide__Divide_param_i64_i64:
LFB473:
	pushq	%rbp
LCFI1722:
	movq	%rsp, %rbp
LCFI1723:
	pushq	%rbx
LCFI1724:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-32(%rbp)
	testq	%rdx, %rdx
	je	L1026
	movq	-32(%rbp), %rcx
	movq	-24(%rbp), %rbx
	xorq	%rbx, %rcx
	testq	%rcx, %rcx
	jns	L1026
	subq	$1, %rax
	addq	-32(%rbp), %rdx
L1026:
	popq	%rbx
	leave
LCFI1725:
	ret
LFE473:
.no_dead_strip _Divide__uDivide_var_i64_L
_Divide__uDivide_var_i64_L:
LFB474:
	pushq	%rbp
LCFI1726:
	movq	%rsp, %rbp
LCFI1727:
	movq	176+_MM_Divide(%rip), %rax
	movq	192+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1728:
	ret
LFE474:
.no_dead_strip _Divide__Divide_var_i64_L
_Divide__Divide_var_i64_L:
LFB475:
	pushq	%rbp
LCFI1729:
	movq	%rsp, %rbp
LCFI1730:
	pushq	%rbx
LCFI1731:
	movq	176+_MM_Divide(%rip), %rbx
	movq	192+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1029
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1029
	subq	$1, %rax
	addq	%rcx, %rdx
L1029:
	popq	%rbx
	leave
LCFI1732:
	ret
LFE475:
.no_dead_strip _Divide__uDivide_param_i64_L
_Divide__uDivide_param_i64_L:
LFB476:
	pushq	%rbp
LCFI1733:
	movq	%rsp, %rbp
LCFI1734:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1735:
	ret
LFE476:
.no_dead_strip _Divide__Divide_param_i64_L
_Divide__Divide_param_i64_L:
LFB477:
	pushq	%rbp
LCFI1736:
	movq	%rsp, %rbp
LCFI1737:
	pushq	%rbx
LCFI1738:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-32(%rbp)
	testq	%rdx, %rdx
	je	L1032
	movq	-32(%rbp), %rcx
	movq	-24(%rbp), %rbx
	xorq	%rbx, %rcx
	testq	%rcx, %rcx
	jns	L1032
	subq	$1, %rax
	addq	-32(%rbp), %rdx
L1032:
	popq	%rbx
	leave
LCFI1739:
	ret
LFE477:
.no_dead_strip _Divide__uDivide_var_i64_i8
_Divide__uDivide_var_i64_i8:
LFB478:
	pushq	%rbp
LCFI1740:
	movq	%rsp, %rbp
LCFI1741:
	movq	176+_MM_Divide(%rip), %rax
	movzbl	200+_MM_Divide(%rip), %edx
	movsbq	%dl, %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1742:
	ret
LFE478:
.no_dead_strip _Divide__Divide_var_i64_i8
_Divide__Divide_var_i64_i8:
LFB479:
	pushq	%rbp
LCFI1743:
	movq	%rsp, %rbp
LCFI1744:
	pushq	%rbx
LCFI1745:
	movq	176+_MM_Divide(%rip), %rbx
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1035
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1035
	subq	$1, %rax
	addq	%rcx, %rdx
L1035:
	popq	%rbx
	leave
LCFI1746:
	ret
LFE479:
.no_dead_strip _Divide__uDivide_param_i64_i8
_Divide__uDivide_param_i64_i8:
LFB480:
	pushq	%rbp
LCFI1747:
	movq	%rsp, %rbp
LCFI1748:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movq	-8(%rbp), %rax
	movsbq	-9(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1749:
	ret
LFE480:
.no_dead_strip _Divide__Divide_param_i64_i8
_Divide__Divide_param_i64_i8:
LFB481:
	pushq	%rbp
LCFI1750:
	movq	%rsp, %rbp
LCFI1751:
	pushq	%rbx
LCFI1752:
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp), %rcx
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1038
	movq	%rcx, %rbx
	xorq	-24(%rbp), %rbx
	testq	%rbx, %rbx
	jns	L1038
	subq	$1, %rax
	addq	%rcx, %rdx
L1038:
	popq	%rbx
	leave
LCFI1753:
	ret
LFE481:
.no_dead_strip _Divide__Divide_var_f32_f32
_Divide__Divide_var_f32_f32:
LFB482:
	pushq	%rbp
LCFI1754:
	movq	%rsp, %rbp
LCFI1755:
	leaq	_MM_Divide(%rip), %rax
	addq	$184, %rax
	movss	(%rax), %xmm0
	leaq	_MM_Divide(%rip), %rax
	addq	$184, %rax
	movss	(%rax), %xmm1
	divss	%xmm1, %xmm0
	leave
LCFI1756:
	ret
LFE482:
.no_dead_strip _Divide__Divide_param_f32_f32
_Divide__Divide_param_f32_f32:
LFB483:
	pushq	%rbp
LCFI1757:
	movq	%rsp, %rbp
LCFI1758:
	movss	%xmm0, -4(%rbp)
	movss	%xmm1, -8(%rbp)
	movss	-4(%rbp), %xmm0
	divss	-8(%rbp), %xmm0
	leave
LCFI1759:
	ret
LFE483:
.no_dead_strip _Divide__uDivide_var_L_u16
_Divide__uDivide_var_L_u16:
LFB484:
	pushq	%rbp
LCFI1760:
	movq	%rsp, %rbp
LCFI1761:
	movq	192+_MM_Divide(%rip), %rax
	movzwl	104+_MM_Divide(%rip), %edx
	movzwl	%dx, %edx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1762:
	ret
LFE484:
.no_dead_strip _Divide__Divide_var_L_u16
_Divide__Divide_var_L_u16:
LFB485:
	pushq	%rbp
LCFI1763:
	movq	%rsp, %rbp
LCFI1764:
	pushq	%rbx
LCFI1765:
	movq	192+_MM_Divide(%rip), %rbx
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1043
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1043
	subq	$1, %rax
	addq	%rcx, %rdx
L1043:
	popq	%rbx
	leave
LCFI1766:
	ret
LFE485:
.no_dead_strip _Divide__uDivide_param_L_u16
_Divide__uDivide_param_L_u16:
LFB486:
	pushq	%rbp
LCFI1767:
	movq	%rsp, %rbp
LCFI1768:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movq	-8(%rbp), %rax
	movzwl	-10(%rbp), %edx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1769:
	ret
LFE486:
.no_dead_strip _Divide__Divide_param_L_u16
_Divide__Divide_param_L_u16:
LFB487:
	pushq	%rbp
LCFI1770:
	movq	%rsp, %rbp
LCFI1771:
	pushq	%rbx
LCFI1772:
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %ecx
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1046
	movq	%rcx, %rbx
	xorq	-24(%rbp), %rbx
	testq	%rbx, %rbx
	jns	L1046
	subq	$1, %rax
	addq	%rcx, %rdx
L1046:
	popq	%rbx
	leave
LCFI1773:
	ret
LFE487:
.no_dead_strip _Divide__uDivide_var_L_u32
_Divide__uDivide_var_L_u32:
LFB488:
	pushq	%rbp
LCFI1774:
	movq	%rsp, %rbp
LCFI1775:
	movq	192+_MM_Divide(%rip), %rax
	movl	108+_MM_Divide(%rip), %edx
	mov	%edx, %edx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1776:
	ret
LFE488:
.no_dead_strip _Divide__Divide_var_L_u32
_Divide__Divide_var_L_u32:
LFB489:
	pushq	%rbp
LCFI1777:
	movq	%rsp, %rbp
LCFI1778:
	pushq	%rbx
LCFI1779:
	movq	192+_MM_Divide(%rip), %rbx
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1049
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1049
	subq	$1, %rax
	addq	%rcx, %rdx
L1049:
	popq	%rbx
	leave
LCFI1780:
	ret
LFE489:
.no_dead_strip _Divide__uDivide_param_L_u32
_Divide__uDivide_param_L_u32:
LFB490:
	pushq	%rbp
LCFI1781:
	movq	%rsp, %rbp
LCFI1782:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movq	-8(%rbp), %rax
	mov	-12(%rbp), %edx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1783:
	ret
LFE490:
.no_dead_strip _Divide__Divide_param_L_u32
_Divide__Divide_param_L_u32:
LFB491:
	pushq	%rbp
LCFI1784:
	movq	%rsp, %rbp
LCFI1785:
	pushq	%rbx
LCFI1786:
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %ecx
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1052
	movq	%rcx, %rbx
	xorq	-24(%rbp), %rbx
	testq	%rbx, %rbx
	jns	L1052
	subq	$1, %rax
	addq	%rcx, %rdx
L1052:
	popq	%rbx
	leave
LCFI1787:
	ret
LFE491:
.no_dead_strip _Divide__uDivide_var_L_LC
_Divide__uDivide_var_L_LC:
LFB492:
	pushq	%rbp
LCFI1788:
	movq	%rsp, %rbp
LCFI1789:
	movq	192+_MM_Divide(%rip), %rax
	movq	112+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1790:
	ret
LFE492:
.no_dead_strip _Divide__Divide_var_L_LC
_Divide__Divide_var_L_LC:
LFB493:
	pushq	%rbp
LCFI1791:
	movq	%rsp, %rbp
LCFI1792:
	pushq	%rbx
LCFI1793:
	movq	192+_MM_Divide(%rip), %rbx
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1055
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1055
	subq	$1, %rax
	addq	%rcx, %rdx
L1055:
	popq	%rbx
	leave
LCFI1794:
	ret
LFE493:
.no_dead_strip _Divide__uDivide_param_L_LC
_Divide__uDivide_param_L_LC:
LFB494:
	pushq	%rbp
LCFI1795:
	movq	%rsp, %rbp
LCFI1796:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-16(%rbp)
	leave
LCFI1797:
	ret
LFE494:
.no_dead_strip _Divide__Divide_param_L_LC
_Divide__Divide_param_L_LC:
LFB495:
	pushq	%rbp
LCFI1798:
	movq	%rsp, %rbp
LCFI1799:
	pushq	%rbx
LCFI1800:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rcx
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1058
	movq	%rcx, %rbx
	xorq	-24(%rbp), %rbx
	testq	%rbx, %rbx
	jns	L1058
	subq	$1, %rax
	addq	%rcx, %rdx
L1058:
	popq	%rbx
	leave
LCFI1801:
	ret
LFE495:
.no_dead_strip _Divide__uDivide_var_L_I
_Divide__uDivide_var_L_I:
LFB496:
	pushq	%rbp
LCFI1802:
	movq	%rsp, %rbp
LCFI1803:
	movq	192+_MM_Divide(%rip), %rax
	movq	120+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1804:
	ret
LFE496:
.no_dead_strip _Divide__Divide_var_L_I
_Divide__Divide_var_L_I:
LFB497:
	pushq	%rbp
LCFI1805:
	movq	%rsp, %rbp
LCFI1806:
	pushq	%rbx
LCFI1807:
	movq	192+_MM_Divide(%rip), %rbx
	movq	120+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1061
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1061
	subq	$1, %rax
	addq	%rcx, %rdx
L1061:
	popq	%rbx
	leave
LCFI1808:
	ret
LFE497:
.no_dead_strip _Divide__uDivide_param_L_I
_Divide__uDivide_param_L_I:
LFB498:
	pushq	%rbp
LCFI1809:
	movq	%rsp, %rbp
LCFI1810:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1811:
	ret
LFE498:
.no_dead_strip _Divide__Divide_param_L_I
_Divide__Divide_param_L_I:
LFB499:
	pushq	%rbp
LCFI1812:
	movq	%rsp, %rbp
LCFI1813:
	pushq	%rbx
LCFI1814:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-32(%rbp)
	testq	%rdx, %rdx
	je	L1064
	movq	-32(%rbp), %rcx
	movq	-24(%rbp), %rbx
	xorq	%rbx, %rcx
	testq	%rcx, %rcx
	jns	L1064
	subq	$1, %rax
	addq	-32(%rbp), %rdx
L1064:
	popq	%rbx
	leave
LCFI1815:
	ret
LFE499:
.no_dead_strip _Divide__uDivide_var_L_i16
_Divide__uDivide_var_L_i16:
LFB500:
	pushq	%rbp
LCFI1816:
	movq	%rsp, %rbp
LCFI1817:
	movq	192+_MM_Divide(%rip), %rax
	movzwl	128+_MM_Divide(%rip), %edx
	movswq	%dx, %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1818:
	ret
LFE500:
.no_dead_strip _Divide__Divide_var_L_i16
_Divide__Divide_var_L_i16:
LFB501:
	pushq	%rbp
LCFI1819:
	movq	%rsp, %rbp
LCFI1820:
	pushq	%rbx
LCFI1821:
	movq	192+_MM_Divide(%rip), %rbx
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1067
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1067
	subq	$1, %rax
	addq	%rcx, %rdx
L1067:
	popq	%rbx
	leave
LCFI1822:
	ret
LFE501:
.no_dead_strip _Divide__uDivide_param_L_i16
_Divide__uDivide_param_L_i16:
LFB502:
	pushq	%rbp
LCFI1823:
	movq	%rsp, %rbp
LCFI1824:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movq	-8(%rbp), %rax
	movswq	-10(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1825:
	ret
LFE502:
.no_dead_strip _Divide__Divide_param_L_i16
_Divide__Divide_param_L_i16:
LFB503:
	pushq	%rbp
LCFI1826:
	movq	%rsp, %rbp
LCFI1827:
	pushq	%rbx
LCFI1828:
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp), %rcx
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1070
	movq	%rcx, %rbx
	xorq	-24(%rbp), %rbx
	testq	%rbx, %rbx
	jns	L1070
	subq	$1, %rax
	addq	%rcx, %rdx
L1070:
	popq	%rbx
	leave
LCFI1829:
	ret
LFE503:
.no_dead_strip _Divide__uDivide_var_L_u64
_Divide__uDivide_var_L_u64:
LFB504:
	pushq	%rbp
LCFI1830:
	movq	%rsp, %rbp
LCFI1831:
	movq	192+_MM_Divide(%rip), %rax
	movq	136+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1832:
	ret
LFE504:
.no_dead_strip _Divide__Divide_var_L_u64
_Divide__Divide_var_L_u64:
LFB505:
	pushq	%rbp
LCFI1833:
	movq	%rsp, %rbp
LCFI1834:
	pushq	%rbx
LCFI1835:
	movq	192+_MM_Divide(%rip), %rbx
	movq	136+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1073
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1073
	subq	$1, %rax
	addq	%rcx, %rdx
L1073:
	popq	%rbx
	leave
LCFI1836:
	ret
LFE505:
.no_dead_strip _Divide__uDivide_param_L_u64
_Divide__uDivide_param_L_u64:
LFB506:
	pushq	%rbp
LCFI1837:
	movq	%rsp, %rbp
LCFI1838:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1839:
	ret
LFE506:
.no_dead_strip _Divide__Divide_param_L_u64
_Divide__Divide_param_L_u64:
LFB507:
	pushq	%rbp
LCFI1840:
	movq	%rsp, %rbp
LCFI1841:
	pushq	%rbx
LCFI1842:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-32(%rbp)
	testq	%rdx, %rdx
	je	L1076
	movq	-32(%rbp), %rcx
	movq	-24(%rbp), %rbx
	xorq	%rbx, %rcx
	testq	%rcx, %rcx
	jns	L1076
	subq	$1, %rax
	addq	-32(%rbp), %rdx
L1076:
	popq	%rbx
	leave
LCFI1843:
	ret
LFE507:
.no_dead_strip _Divide__uDivide_var_L_i32
_Divide__uDivide_var_L_i32:
LFB508:
	pushq	%rbp
LCFI1844:
	movq	%rsp, %rbp
LCFI1845:
	movq	192+_MM_Divide(%rip), %rax
	movl	152+_MM_Divide(%rip), %edx
	movslq	%edx, %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1846:
	ret
LFE508:
.no_dead_strip _Divide__Divide_var_L_i32
_Divide__Divide_var_L_i32:
LFB509:
	pushq	%rbp
LCFI1847:
	movq	%rsp, %rbp
LCFI1848:
	pushq	%rbx
LCFI1849:
	movq	192+_MM_Divide(%rip), %rbx
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1079
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1079
	subq	$1, %rax
	addq	%rcx, %rdx
L1079:
	popq	%rbx
	leave
LCFI1850:
	ret
LFE509:
.no_dead_strip _Divide__uDivide_param_L_i32
_Divide__uDivide_param_L_i32:
LFB510:
	pushq	%rbp
LCFI1851:
	movq	%rsp, %rbp
LCFI1852:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movq	-8(%rbp), %rax
	movl	-12(%rbp), %edx
	movslq	%edx, %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1853:
	ret
LFE510:
.no_dead_strip _Divide__Divide_param_L_i32
_Divide__Divide_param_L_i32:
LFB511:
	pushq	%rbp
LCFI1854:
	movq	%rsp, %rbp
LCFI1855:
	pushq	%rbx
LCFI1856:
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax, %rcx
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1082
	movq	%rcx, %rbx
	xorq	-24(%rbp), %rbx
	testq	%rbx, %rbx
	jns	L1082
	subq	$1, %rax
	addq	%rcx, %rdx
L1082:
	popq	%rbx
	leave
LCFI1857:
	ret
LFE511:
.no_dead_strip _Divide__uDivide_var_L_C
_Divide__uDivide_var_L_C:
LFB512:
	pushq	%rbp
LCFI1858:
	movq	%rsp, %rbp
LCFI1859:
	movq	192+_MM_Divide(%rip), %rax
	movq	160+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1860:
	ret
LFE512:
.no_dead_strip _Divide__Divide_var_L_C
_Divide__Divide_var_L_C:
LFB513:
	pushq	%rbp
LCFI1861:
	movq	%rsp, %rbp
LCFI1862:
	pushq	%rbx
LCFI1863:
	movq	192+_MM_Divide(%rip), %rbx
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1085
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1085
	subq	$1, %rax
	addq	%rcx, %rdx
L1085:
	popq	%rbx
	leave
LCFI1864:
	ret
LFE513:
.no_dead_strip _Divide__uDivide_param_L_C
_Divide__uDivide_param_L_C:
LFB514:
	pushq	%rbp
LCFI1865:
	movq	%rsp, %rbp
LCFI1866:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-16(%rbp)
	leave
LCFI1867:
	ret
LFE514:
.no_dead_strip _Divide__Divide_param_L_C
_Divide__Divide_param_L_C:
LFB515:
	pushq	%rbp
LCFI1868:
	movq	%rsp, %rbp
LCFI1869:
	pushq	%rbx
LCFI1870:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rcx
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1088
	movq	%rcx, %rbx
	xorq	-24(%rbp), %rbx
	testq	%rbx, %rbx
	jns	L1088
	subq	$1, %rax
	addq	%rcx, %rdx
L1088:
	popq	%rbx
	leave
LCFI1871:
	ret
LFE515:
.no_dead_strip _Divide__uDivide_var_L_u8
_Divide__uDivide_var_L_u8:
LFB516:
	pushq	%rbp
LCFI1872:
	movq	%rsp, %rbp
LCFI1873:
	movq	192+_MM_Divide(%rip), %rax
	movzbl	168+_MM_Divide(%rip), %edx
	movzbl	%dl, %edx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1874:
	ret
LFE516:
.no_dead_strip _Divide__Divide_var_L_u8
_Divide__Divide_var_L_u8:
LFB517:
	pushq	%rbp
LCFI1875:
	movq	%rsp, %rbp
LCFI1876:
	pushq	%rbx
LCFI1877:
	movq	192+_MM_Divide(%rip), %rbx
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1091
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1091
	subq	$1, %rax
	addq	%rcx, %rdx
L1091:
	popq	%rbx
	leave
LCFI1878:
	ret
LFE517:
.no_dead_strip _Divide__uDivide_param_L_u8
_Divide__uDivide_param_L_u8:
LFB518:
	pushq	%rbp
LCFI1879:
	movq	%rsp, %rbp
LCFI1880:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movq	-8(%rbp), %rax
	movzbl	-9(%rbp), %edx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1881:
	ret
LFE518:
.no_dead_strip _Divide__Divide_param_L_u8
_Divide__Divide_param_L_u8:
LFB519:
	pushq	%rbp
LCFI1882:
	movq	%rsp, %rbp
LCFI1883:
	pushq	%rbx
LCFI1884:
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %ecx
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1094
	movq	%rcx, %rbx
	xorq	-24(%rbp), %rbx
	testq	%rbx, %rbx
	jns	L1094
	subq	$1, %rax
	addq	%rcx, %rdx
L1094:
	popq	%rbx
	leave
LCFI1885:
	ret
LFE519:
.no_dead_strip _Divide__uDivide_var_L_i64
_Divide__uDivide_var_L_i64:
LFB520:
	pushq	%rbp
LCFI1886:
	movq	%rsp, %rbp
LCFI1887:
	movq	192+_MM_Divide(%rip), %rax
	movq	176+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1888:
	ret
LFE520:
.no_dead_strip _Divide__Divide_var_L_i64
_Divide__Divide_var_L_i64:
LFB521:
	pushq	%rbp
LCFI1889:
	movq	%rsp, %rbp
LCFI1890:
	pushq	%rbx
LCFI1891:
	movq	192+_MM_Divide(%rip), %rbx
	movq	176+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1097
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1097
	subq	$1, %rax
	addq	%rcx, %rdx
L1097:
	popq	%rbx
	leave
LCFI1892:
	ret
LFE521:
.no_dead_strip _Divide__uDivide_param_L_i64
_Divide__uDivide_param_L_i64:
LFB522:
	pushq	%rbp
LCFI1893:
	movq	%rsp, %rbp
LCFI1894:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1895:
	ret
LFE522:
.no_dead_strip _Divide__Divide_param_L_i64
_Divide__Divide_param_L_i64:
LFB523:
	pushq	%rbp
LCFI1896:
	movq	%rsp, %rbp
LCFI1897:
	pushq	%rbx
LCFI1898:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-32(%rbp)
	testq	%rdx, %rdx
	je	L1100
	movq	-32(%rbp), %rcx
	movq	-24(%rbp), %rbx
	xorq	%rbx, %rcx
	testq	%rcx, %rcx
	jns	L1100
	subq	$1, %rax
	addq	-32(%rbp), %rdx
L1100:
	popq	%rbx
	leave
LCFI1899:
	ret
LFE523:
.no_dead_strip _Divide__uDivide_var_L_L
_Divide__uDivide_var_L_L:
LFB524:
	pushq	%rbp
LCFI1900:
	movq	%rsp, %rbp
LCFI1901:
	movq	192+_MM_Divide(%rip), %rax
	movq	192+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1902:
	ret
LFE524:
.no_dead_strip _Divide__Divide_var_L_L
_Divide__Divide_var_L_L:
LFB525:
	pushq	%rbp
LCFI1903:
	movq	%rsp, %rbp
LCFI1904:
	pushq	%rbx
LCFI1905:
	movq	192+_MM_Divide(%rip), %rbx
	movq	192+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1103
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1103
	subq	$1, %rax
	addq	%rcx, %rdx
L1103:
	popq	%rbx
	leave
LCFI1906:
	ret
LFE525:
.no_dead_strip _Divide__uDivide_param_L_L
_Divide__uDivide_param_L_L:
LFB526:
	pushq	%rbp
LCFI1907:
	movq	%rsp, %rbp
LCFI1908:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1909:
	ret
LFE526:
.no_dead_strip _Divide__Divide_param_L_L
_Divide__Divide_param_L_L:
LFB527:
	pushq	%rbp
LCFI1910:
	movq	%rsp, %rbp
LCFI1911:
	pushq	%rbx
LCFI1912:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-32(%rbp)
	testq	%rdx, %rdx
	je	L1106
	movq	-32(%rbp), %rcx
	movq	-24(%rbp), %rbx
	xorq	%rbx, %rcx
	testq	%rcx, %rcx
	jns	L1106
	subq	$1, %rax
	addq	-32(%rbp), %rdx
L1106:
	popq	%rbx
	leave
LCFI1913:
	ret
LFE527:
.no_dead_strip _Divide__uDivide_var_L_i8
_Divide__uDivide_var_L_i8:
LFB528:
	pushq	%rbp
LCFI1914:
	movq	%rsp, %rbp
LCFI1915:
	movq	192+_MM_Divide(%rip), %rax
	movzbl	200+_MM_Divide(%rip), %edx
	movsbq	%dl, %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1916:
	ret
LFE528:
.no_dead_strip _Divide__Divide_var_L_i8
_Divide__Divide_var_L_i8:
LFB529:
	pushq	%rbp
LCFI1917:
	movq	%rsp, %rbp
LCFI1918:
	pushq	%rbx
LCFI1919:
	movq	192+_MM_Divide(%rip), %rbx
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1109
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1109
	subq	$1, %rax
	addq	%rcx, %rdx
L1109:
	popq	%rbx
	leave
LCFI1920:
	ret
LFE529:
.no_dead_strip _Divide__uDivide_param_L_i8
_Divide__uDivide_param_L_i8:
LFB530:
	pushq	%rbp
LCFI1921:
	movq	%rsp, %rbp
LCFI1922:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movq	-8(%rbp), %rax
	movsbq	-9(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1923:
	ret
LFE530:
.no_dead_strip _Divide__Divide_param_L_i8
_Divide__Divide_param_L_i8:
LFB531:
	pushq	%rbp
LCFI1924:
	movq	%rsp, %rbp
LCFI1925:
	pushq	%rbx
LCFI1926:
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp), %rcx
	movq	-24(%rbp), %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1112
	movq	%rcx, %rbx
	xorq	-24(%rbp), %rbx
	testq	%rbx, %rbx
	jns	L1112
	subq	$1, %rax
	addq	%rcx, %rdx
L1112:
	popq	%rbx
	leave
LCFI1927:
	ret
LFE531:
.no_dead_strip _Divide__uDivide_var_i8_u16
_Divide__uDivide_var_i8_u16:
LFB532:
	pushq	%rbp
LCFI1928:
	movq	%rsp, %rbp
LCFI1929:
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rax
	movzwl	104+_MM_Divide(%rip), %edx
	movzwl	%dx, %edx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1930:
	ret
LFE532:
.no_dead_strip _Divide__Divide_var_i8_u16
_Divide__Divide_var_i8_u16:
LFB533:
	pushq	%rbp
LCFI1931:
	movq	%rsp, %rbp
LCFI1932:
	pushq	%rbx
LCFI1933:
	subq	$8, %rsp
LCFI1934:
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rbx
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1115
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1115
	subq	$1, %rax
	addq	%rcx, %rdx
L1115:
	cmpq	$-128, %rax
	jl	L1116
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rbx
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1117
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1117
	subq	$1, %rax
	addq	%rcx, %rdx
L1117:
	cmpq	$127, %rax
	jle	L1118
L1116:
	movl	$18785, %edi
	call	__m3_fault
L1118:
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rbx
	movzwl	104+_MM_Divide(%rip), %eax
	movzwl	%ax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1119
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1119
	subq	$1, %rax
	addq	%rcx, %rdx
L1119:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1935:
	ret
LFE533:
.no_dead_strip _Divide__uDivide_param_i8_u16
_Divide__uDivide_param_i8_u16:
LFB534:
	pushq	%rbp
LCFI1936:
	movq	%rsp, %rbp
LCFI1937:
	movb	%dil, -1(%rbp)
	movw	%si, -4(%rbp)
	movsbq	-1(%rbp), %rax
	movzwl	-4(%rbp), %edx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1938:
	ret
LFE534:
.no_dead_strip _Divide__Divide_param_i8_u16
_Divide__Divide_param_i8_u16:
LFB535:
	pushq	%rbp
LCFI1939:
	movq	%rsp, %rbp
LCFI1940:
	pushq	%rbx
LCFI1941:
	subq	$24, %rsp
LCFI1942:
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp), %rbx
	movzwl	-20(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1122
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1122
	subq	$1, %rax
	addq	%rcx, %rdx
L1122:
	cmpq	$-128, %rax
	jl	L1123
	movsbq	-17(%rbp), %rbx
	movzwl	-20(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1124
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1124
	subq	$1, %rax
	addq	%rcx, %rdx
L1124:
	cmpq	$127, %rax
	jle	L1125
L1123:
	movl	$18849, %edi
	call	__m3_fault
L1125:
	movsbq	-17(%rbp), %rbx
	movzwl	-20(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1126
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1126
	subq	$1, %rax
	addq	%rcx, %rdx
L1126:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI1943:
	ret
LFE535:
.no_dead_strip _Divide__uDivide_var_i8_u32
_Divide__uDivide_var_i8_u32:
LFB536:
	pushq	%rbp
LCFI1944:
	movq	%rsp, %rbp
LCFI1945:
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rax
	movl	108+_MM_Divide(%rip), %edx
	mov	%edx, %edx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1946:
	ret
LFE536:
.no_dead_strip _Divide__Divide_var_i8_u32
_Divide__Divide_var_i8_u32:
LFB537:
	pushq	%rbp
LCFI1947:
	movq	%rsp, %rbp
LCFI1948:
	pushq	%rbx
LCFI1949:
	subq	$8, %rsp
LCFI1950:
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rbx
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1129
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1129
	subq	$1, %rax
	addq	%rcx, %rdx
L1129:
	cmpq	$-128, %rax
	jl	L1130
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rbx
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1131
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1131
	subq	$1, %rax
	addq	%rcx, %rdx
L1131:
	cmpq	$127, %rax
	jle	L1132
L1130:
	movl	$18913, %edi
	call	__m3_fault
L1132:
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rbx
	movl	108+_MM_Divide(%rip), %eax
	mov	%eax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1133
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1133
	subq	$1, %rax
	addq	%rcx, %rdx
L1133:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1951:
	ret
LFE537:
.no_dead_strip _Divide__uDivide_param_i8_u32
_Divide__uDivide_param_i8_u32:
LFB538:
	pushq	%rbp
LCFI1952:
	movq	%rsp, %rbp
LCFI1953:
	movb	%dil, -1(%rbp)
	movl	%esi, -8(%rbp)
	movsbq	-1(%rbp), %rax
	mov	-8(%rbp), %edx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1954:
	ret
LFE538:
.no_dead_strip _Divide__Divide_param_i8_u32
_Divide__Divide_param_i8_u32:
LFB539:
	pushq	%rbp
LCFI1955:
	movq	%rsp, %rbp
LCFI1956:
	pushq	%rbx
LCFI1957:
	subq	$24, %rsp
LCFI1958:
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp), %rbx
	mov	-24(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1136
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1136
	subq	$1, %rax
	addq	%rcx, %rdx
L1136:
	cmpq	$-128, %rax
	jl	L1137
	movsbq	-17(%rbp), %rbx
	mov	-24(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1138
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1138
	subq	$1, %rax
	addq	%rcx, %rdx
L1138:
	cmpq	$127, %rax
	jle	L1139
L1137:
	movl	$18977, %edi
	call	__m3_fault
L1139:
	movsbq	-17(%rbp), %rbx
	mov	-24(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1140
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1140
	subq	$1, %rax
	addq	%rcx, %rdx
L1140:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI1959:
	ret
LFE539:
.no_dead_strip _Divide__uDivide_var_i8_LC
_Divide__uDivide_var_i8_LC:
LFB540:
	pushq	%rbp
LCFI1960:
	movq	%rsp, %rbp
LCFI1961:
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rax
	movq	112+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1962:
	ret
LFE540:
.no_dead_strip _Divide__Divide_var_i8_LC
_Divide__Divide_var_i8_LC:
LFB541:
	pushq	%rbp
LCFI1963:
	movq	%rsp, %rbp
LCFI1964:
	pushq	%rbx
LCFI1965:
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rbx
	movq	112+_MM_Divide(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1143
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1143
	subq	$1, %rax
	addq	%rcx, %rdx
L1143:
	popq	%rbx
	leave
LCFI1966:
	ret
LFE541:
.no_dead_strip _Divide__uDivide_param_i8_LC
_Divide__uDivide_param_i8_LC:
LFB542:
	pushq	%rbp
LCFI1967:
	movq	%rsp, %rbp
LCFI1968:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movsbq	-1(%rbp), %rax
	movl	$0, %edx
	divq	-16(%rbp)
	leave
LCFI1969:
	ret
LFE542:
.no_dead_strip _Divide__Divide_param_i8_LC
_Divide__Divide_param_i8_LC:
LFB543:
	pushq	%rbp
LCFI1970:
	movq	%rsp, %rbp
LCFI1971:
	pushq	%rbx
LCFI1972:
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp), %rbx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1146
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1146
	subq	$1, %rax
	addq	%rcx, %rdx
L1146:
	popq	%rbx
	leave
LCFI1973:
	ret
LFE543:
.no_dead_strip _Divide__uDivide_var_i8_I
_Divide__uDivide_var_i8_I:
LFB544:
	pushq	%rbp
LCFI1974:
	movq	%rsp, %rbp
LCFI1975:
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rax
	movq	120+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1976:
	ret
LFE544:
.no_dead_strip _Divide__Divide_var_i8_I
_Divide__Divide_var_i8_I:
LFB545:
	pushq	%rbp
LCFI1977:
	movq	%rsp, %rbp
LCFI1978:
	pushq	%rbx
LCFI1979:
	subq	$8, %rsp
LCFI1980:
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rbx
	movq	120+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1149
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1149
	subq	$1, %rax
	addq	%rcx, %rdx
L1149:
	cmpq	$-128, %rax
	jl	L1150
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rbx
	movq	120+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1151
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1151
	subq	$1, %rax
	addq	%rcx, %rdx
L1151:
	cmpq	$127, %rax
	jle	L1152
L1150:
	movl	$19169, %edi
	call	__m3_fault
L1152:
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rbx
	movq	120+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1153
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1153
	subq	$1, %rax
	addq	%rcx, %rdx
L1153:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1981:
	ret
LFE545:
.no_dead_strip _Divide__uDivide_param_i8_I
_Divide__uDivide_param_i8_I:
LFB546:
	pushq	%rbp
LCFI1982:
	movq	%rsp, %rbp
LCFI1983:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movsbq	-1(%rbp), %rax
	movq	-16(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1984:
	ret
LFE546:
.no_dead_strip _Divide__Divide_param_i8_I
_Divide__Divide_param_i8_I:
LFB547:
	pushq	%rbp
LCFI1985:
	movq	%rsp, %rbp
LCFI1986:
	subq	$16, %rsp
LCFI1987:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movsbq	-1(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L1156
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L1156
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L1156:
	cmpq	$-128, %rax
	jl	L1157
	movsbq	-1(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L1158
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L1158
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L1158:
	cmpq	$127, %rax
	jle	L1159
L1157:
	movl	$19233, %edi
	call	__m3_fault
L1159:
	movsbq	-1(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L1160
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L1160
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L1160:
	leave
LCFI1988:
	ret
LFE547:
.no_dead_strip _Divide__uDivide_var_i8_i16
_Divide__uDivide_var_i8_i16:
LFB548:
	pushq	%rbp
LCFI1989:
	movq	%rsp, %rbp
LCFI1990:
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rax
	movzwl	128+_MM_Divide(%rip), %edx
	movswq	%dx, %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI1991:
	ret
LFE548:
.no_dead_strip _Divide__Divide_var_i8_i16
_Divide__Divide_var_i8_i16:
LFB549:
	pushq	%rbp
LCFI1992:
	movq	%rsp, %rbp
LCFI1993:
	pushq	%rbx
LCFI1994:
	subq	$8, %rsp
LCFI1995:
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rbx
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1163
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1163
	subq	$1, %rax
	addq	%rcx, %rdx
L1163:
	cmpq	$-128, %rax
	jl	L1164
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rbx
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1165
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1165
	subq	$1, %rax
	addq	%rcx, %rdx
L1165:
	cmpq	$127, %rax
	jle	L1166
L1164:
	movl	$19297, %edi
	call	__m3_fault
L1166:
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rbx
	movzwl	128+_MM_Divide(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1167
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1167
	subq	$1, %rax
	addq	%rcx, %rdx
L1167:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1996:
	ret
LFE549:
.no_dead_strip _Divide__uDivide_param_i8_i16
_Divide__uDivide_param_i8_i16:
LFB550:
	pushq	%rbp
LCFI1997:
	movq	%rsp, %rbp
LCFI1998:
	movb	%dil, -1(%rbp)
	movw	%si, -4(%rbp)
	movsbq	-1(%rbp), %rax
	movswq	-4(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI1999:
	ret
LFE550:
.no_dead_strip _Divide__Divide_param_i8_i16
_Divide__Divide_param_i8_i16:
LFB551:
	pushq	%rbp
LCFI2000:
	movq	%rsp, %rbp
LCFI2001:
	pushq	%rbx
LCFI2002:
	subq	$24, %rsp
LCFI2003:
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp), %rbx
	movswq	-20(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1170
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1170
	subq	$1, %rax
	addq	%rcx, %rdx
L1170:
	cmpq	$-128, %rax
	jl	L1171
	movsbq	-17(%rbp), %rbx
	movswq	-20(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1172
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1172
	subq	$1, %rax
	addq	%rcx, %rdx
L1172:
	cmpq	$127, %rax
	jle	L1173
L1171:
	movl	$19361, %edi
	call	__m3_fault
L1173:
	movsbq	-17(%rbp), %rbx
	movswq	-20(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1174
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1174
	subq	$1, %rax
	addq	%rcx, %rdx
L1174:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI2004:
	ret
LFE551:
.no_dead_strip _Divide__uDivide_var_i8_u64
_Divide__uDivide_var_i8_u64:
LFB552:
	pushq	%rbp
LCFI2005:
	movq	%rsp, %rbp
LCFI2006:
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rax
	movq	136+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI2007:
	ret
LFE552:
.no_dead_strip _Divide__Divide_var_i8_u64
_Divide__Divide_var_i8_u64:
LFB553:
	pushq	%rbp
LCFI2008:
	movq	%rsp, %rbp
LCFI2009:
	pushq	%rbx
LCFI2010:
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rbx
	movq	136+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1177
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1177
	subq	$1, %rax
	addq	%rcx, %rdx
L1177:
	popq	%rbx
	leave
LCFI2011:
	ret
LFE553:
.no_dead_strip _Divide__uDivide_param_i8_u64
_Divide__uDivide_param_i8_u64:
LFB554:
	pushq	%rbp
LCFI2012:
	movq	%rsp, %rbp
LCFI2013:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movsbq	-1(%rbp), %rax
	movq	-16(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI2014:
	ret
LFE554:
.no_dead_strip _Divide__Divide_param_i8_u64
_Divide__Divide_param_i8_u64:
LFB555:
	pushq	%rbp
LCFI2015:
	movq	%rsp, %rbp
LCFI2016:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movsbq	-1(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L1180
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L1180
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L1180:
	leave
LCFI2017:
	ret
LFE555:
.no_dead_strip _Divide__uDivide_var_i8_i32
_Divide__uDivide_var_i8_i32:
LFB556:
	pushq	%rbp
LCFI2018:
	movq	%rsp, %rbp
LCFI2019:
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rax
	movl	152+_MM_Divide(%rip), %edx
	movslq	%edx, %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI2020:
	ret
LFE556:
.no_dead_strip _Divide__Divide_var_i8_i32
_Divide__Divide_var_i8_i32:
LFB557:
	pushq	%rbp
LCFI2021:
	movq	%rsp, %rbp
LCFI2022:
	pushq	%rbx
LCFI2023:
	subq	$8, %rsp
LCFI2024:
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rbx
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1183
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1183
	subq	$1, %rax
	addq	%rcx, %rdx
L1183:
	cmpq	$-128, %rax
	jl	L1184
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rbx
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1185
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1185
	subq	$1, %rax
	addq	%rcx, %rdx
L1185:
	cmpq	$127, %rax
	jle	L1186
L1184:
	movl	$19553, %edi
	call	__m3_fault
L1186:
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rbx
	movl	152+_MM_Divide(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1187
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1187
	subq	$1, %rax
	addq	%rcx, %rdx
L1187:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI2025:
	ret
LFE557:
.no_dead_strip _Divide__uDivide_param_i8_i32
_Divide__uDivide_param_i8_i32:
LFB558:
	pushq	%rbp
LCFI2026:
	movq	%rsp, %rbp
LCFI2027:
	movb	%dil, -1(%rbp)
	movl	%esi, -8(%rbp)
	movsbq	-1(%rbp), %rax
	movl	-8(%rbp), %edx
	movslq	%edx, %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI2028:
	ret
LFE558:
.no_dead_strip _Divide__Divide_param_i8_i32
_Divide__Divide_param_i8_i32:
LFB559:
	pushq	%rbp
LCFI2029:
	movq	%rsp, %rbp
LCFI2030:
	pushq	%rbx
LCFI2031:
	subq	$24, %rsp
LCFI2032:
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp), %rbx
	movl	-24(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1190
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1190
	subq	$1, %rax
	addq	%rcx, %rdx
L1190:
	cmpq	$-128, %rax
	jl	L1191
	movsbq	-17(%rbp), %rbx
	movl	-24(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1192
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1192
	subq	$1, %rax
	addq	%rcx, %rdx
L1192:
	cmpq	$127, %rax
	jle	L1193
L1191:
	movl	$19617, %edi
	call	__m3_fault
L1193:
	movsbq	-17(%rbp), %rbx
	movl	-24(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1194
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1194
	subq	$1, %rax
	addq	%rcx, %rdx
L1194:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI2033:
	ret
LFE559:
.no_dead_strip _Divide__uDivide_var_i8_C
_Divide__uDivide_var_i8_C:
LFB560:
	pushq	%rbp
LCFI2034:
	movq	%rsp, %rbp
LCFI2035:
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rax
	movq	160+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI2036:
	ret
LFE560:
.no_dead_strip _Divide__Divide_var_i8_C
_Divide__Divide_var_i8_C:
LFB561:
	pushq	%rbp
LCFI2037:
	movq	%rsp, %rbp
LCFI2038:
	pushq	%rbx
LCFI2039:
	subq	$8, %rsp
LCFI2040:
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rbx
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1197
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1197
	subq	$1, %rax
	addq	%rcx, %rdx
L1197:
	cmpq	$-128, %rax
	jl	L1198
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rbx
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1199
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1199
	subq	$1, %rax
	addq	%rcx, %rdx
L1199:
	cmpq	$127, %rax
	jle	L1200
L1198:
	movl	$19681, %edi
	call	__m3_fault
L1200:
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rbx
	movq	160+_MM_Divide(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1201
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1201
	subq	$1, %rax
	addq	%rcx, %rdx
L1201:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI2041:
	ret
LFE561:
.no_dead_strip _Divide__uDivide_param_i8_C
_Divide__uDivide_param_i8_C:
LFB562:
	pushq	%rbp
LCFI2042:
	movq	%rsp, %rbp
LCFI2043:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movsbq	-1(%rbp), %rax
	movl	$0, %edx
	divq	-16(%rbp)
	leave
LCFI2044:
	ret
LFE562:
.no_dead_strip _Divide__Divide_param_i8_C
_Divide__Divide_param_i8_C:
LFB563:
	pushq	%rbp
LCFI2045:
	movq	%rsp, %rbp
LCFI2046:
	pushq	%rbx
LCFI2047:
	subq	$24, %rsp
LCFI2048:
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp), %rbx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1204
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1204
	subq	$1, %rax
	addq	%rcx, %rdx
L1204:
	cmpq	$-128, %rax
	jl	L1205
	movsbq	-17(%rbp), %rbx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1206
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1206
	subq	$1, %rax
	addq	%rcx, %rdx
L1206:
	cmpq	$127, %rax
	jle	L1207
L1205:
	movl	$19745, %edi
	call	__m3_fault
L1207:
	movsbq	-17(%rbp), %rbx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1208
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1208
	subq	$1, %rax
	addq	%rcx, %rdx
L1208:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI2049:
	ret
LFE563:
.no_dead_strip _Divide__uDivide_var_i8_u8
_Divide__uDivide_var_i8_u8:
LFB564:
	pushq	%rbp
LCFI2050:
	movq	%rsp, %rbp
LCFI2051:
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rax
	movzbl	168+_MM_Divide(%rip), %edx
	movzbl	%dl, %edx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI2052:
	ret
LFE564:
.no_dead_strip _Divide__Divide_var_i8_u8
_Divide__Divide_var_i8_u8:
LFB565:
	pushq	%rbp
LCFI2053:
	movq	%rsp, %rbp
LCFI2054:
	pushq	%rbx
LCFI2055:
	subq	$8, %rsp
LCFI2056:
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rbx
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1211
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1211
	subq	$1, %rax
	addq	%rcx, %rdx
L1211:
	cmpq	$-128, %rax
	jl	L1212
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rbx
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1213
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1213
	subq	$1, %rax
	addq	%rcx, %rdx
L1213:
	cmpq	$127, %rax
	jle	L1214
L1212:
	movl	$19809, %edi
	call	__m3_fault
L1214:
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rbx
	movzbl	168+_MM_Divide(%rip), %eax
	movzbl	%al, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1215
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1215
	subq	$1, %rax
	addq	%rcx, %rdx
L1215:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI2057:
	ret
LFE565:
.no_dead_strip _Divide__uDivide_param_i8_u8
_Divide__uDivide_param_i8_u8:
LFB566:
	pushq	%rbp
LCFI2058:
	movq	%rsp, %rbp
LCFI2059:
	movb	%dil, -1(%rbp)
	movb	%sil, -2(%rbp)
	movsbq	-1(%rbp), %rax
	movzbl	-2(%rbp), %edx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI2060:
	ret
LFE566:
.no_dead_strip _Divide__Divide_param_i8_u8
_Divide__Divide_param_i8_u8:
LFB567:
	pushq	%rbp
LCFI2061:
	movq	%rsp, %rbp
LCFI2062:
	pushq	%rbx
LCFI2063:
	subq	$24, %rsp
LCFI2064:
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp), %rbx
	movzbl	-18(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1218
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1218
	subq	$1, %rax
	addq	%rcx, %rdx
L1218:
	cmpq	$-128, %rax
	jl	L1219
	movsbq	-17(%rbp), %rbx
	movzbl	-18(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1220
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1220
	subq	$1, %rax
	addq	%rcx, %rdx
L1220:
	cmpq	$127, %rax
	jle	L1221
L1219:
	movl	$19873, %edi
	call	__m3_fault
L1221:
	movsbq	-17(%rbp), %rbx
	movzbl	-18(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1222
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1222
	subq	$1, %rax
	addq	%rcx, %rdx
L1222:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI2065:
	ret
LFE567:
.no_dead_strip _Divide__uDivide_var_i8_i64
_Divide__uDivide_var_i8_i64:
LFB568:
	pushq	%rbp
LCFI2066:
	movq	%rsp, %rbp
LCFI2067:
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rax
	movq	176+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI2068:
	ret
LFE568:
.no_dead_strip _Divide__Divide_var_i8_i64
_Divide__Divide_var_i8_i64:
LFB569:
	pushq	%rbp
LCFI2069:
	movq	%rsp, %rbp
LCFI2070:
	pushq	%rbx
LCFI2071:
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rbx
	movq	176+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1225
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1225
	subq	$1, %rax
	addq	%rcx, %rdx
L1225:
	popq	%rbx
	leave
LCFI2072:
	ret
LFE569:
.no_dead_strip _Divide__uDivide_param_i8_i64
_Divide__uDivide_param_i8_i64:
LFB570:
	pushq	%rbp
LCFI2073:
	movq	%rsp, %rbp
LCFI2074:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movsbq	-1(%rbp), %rax
	movq	-16(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI2075:
	ret
LFE570:
.no_dead_strip _Divide__Divide_param_i8_i64
_Divide__Divide_param_i8_i64:
LFB571:
	pushq	%rbp
LCFI2076:
	movq	%rsp, %rbp
LCFI2077:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movsbq	-1(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L1228
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L1228
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L1228:
	leave
LCFI2078:
	ret
LFE571:
.no_dead_strip _Divide__uDivide_var_i8_L
_Divide__uDivide_var_i8_L:
LFB572:
	pushq	%rbp
LCFI2079:
	movq	%rsp, %rbp
LCFI2080:
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rax
	movq	192+_MM_Divide(%rip), %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI2081:
	ret
LFE572:
.no_dead_strip _Divide__Divide_var_i8_L
_Divide__Divide_var_i8_L:
LFB573:
	pushq	%rbp
LCFI2082:
	movq	%rsp, %rbp
LCFI2083:
	pushq	%rbx
LCFI2084:
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rbx
	movq	192+_MM_Divide(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1231
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1231
	subq	$1, %rax
	addq	%rcx, %rdx
L1231:
	popq	%rbx
	leave
LCFI2085:
	ret
LFE573:
.no_dead_strip _Divide__uDivide_param_i8_L
_Divide__uDivide_param_i8_L:
LFB574:
	pushq	%rbp
LCFI2086:
	movq	%rsp, %rbp
LCFI2087:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movsbq	-1(%rbp), %rax
	movq	-16(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI2088:
	ret
LFE574:
.no_dead_strip _Divide__Divide_param_i8_L
_Divide__Divide_param_i8_L:
LFB575:
	pushq	%rbp
LCFI2089:
	movq	%rsp, %rbp
LCFI2090:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movsbq	-1(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L1234
	xorq	-16(%rbp), %rcx
	testq	%rcx, %rcx
	jns	L1234
	subq	$1, %rax
	addq	-16(%rbp), %rdx
L1234:
	leave
LCFI2091:
	ret
LFE575:
.no_dead_strip _Divide__uDivide_var_i8_i8
_Divide__uDivide_var_i8_i8:
LFB576:
	pushq	%rbp
LCFI2092:
	movq	%rsp, %rbp
LCFI2093:
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rax
	movzbl	200+_MM_Divide(%rip), %edx
	movsbq	%dl, %rdx
	movq	%rdx, -8(%rbp)
	movl	$0, %edx
	divq	-8(%rbp)
	leave
LCFI2094:
	ret
LFE576:
.no_dead_strip _Divide__Divide_var_i8_i8
_Divide__Divide_var_i8_i8:
LFB577:
	pushq	%rbp
LCFI2095:
	movq	%rsp, %rbp
LCFI2096:
	pushq	%rbx
LCFI2097:
	subq	$8, %rsp
LCFI2098:
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rbx
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1237
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1237
	subq	$1, %rax
	addq	%rcx, %rdx
L1237:
	cmpq	$-128, %rax
	jl	L1238
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rbx
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1239
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1239
	subq	$1, %rax
	addq	%rcx, %rdx
L1239:
	cmpq	$127, %rax
	jle	L1240
L1238:
	movl	$20193, %edi
	call	__m3_fault
L1240:
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rbx
	movzbl	200+_MM_Divide(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1241
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1241
	subq	$1, %rax
	addq	%rcx, %rdx
L1241:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI2099:
	ret
LFE577:
.no_dead_strip _Divide__uDivide_param_i8_i8
_Divide__uDivide_param_i8_i8:
LFB578:
	pushq	%rbp
LCFI2100:
	movq	%rsp, %rbp
LCFI2101:
	movb	%dil, -1(%rbp)
	movb	%sil, -2(%rbp)
	movsbq	-1(%rbp), %rax
	movsbq	-2(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movl	$0, %edx
	divq	-24(%rbp)
	leave
LCFI2102:
	ret
LFE578:
.no_dead_strip _Divide__Divide_param_i8_i8
_Divide__Divide_param_i8_i8:
LFB579:
	pushq	%rbp
LCFI2103:
	movq	%rsp, %rbp
LCFI2104:
	pushq	%rbx
LCFI2105:
	subq	$24, %rsp
LCFI2106:
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp), %rbx
	movsbq	-18(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1244
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1244
	subq	$1, %rax
	addq	%rcx, %rdx
L1244:
	cmpq	$-128, %rax
	jl	L1245
	movsbq	-17(%rbp), %rbx
	movsbq	-18(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1246
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1246
	subq	$1, %rax
	addq	%rcx, %rdx
L1246:
	cmpq	$127, %rax
	jle	L1247
L1245:
	movl	$20257, %edi
	call	__m3_fault
L1247:
	movsbq	-17(%rbp), %rbx
	movsbq	-18(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1248
	xorq	%rcx, %rbx
	testq	%rbx, %rbx
	jns	L1248
	subq	$1, %rax
	addq	%rcx, %rdx
L1248:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI2107:
	ret
LFE579:
.globl _Divide_M3
.no_dead_strip _Divide_M3
_Divide_M3:
LFB580:
	pushq	%rbp
LCFI2108:
	movq	%rsp, %rbp
LCFI2109:
	movq	%rdi, -8(%rbp)
	leaq	_MM_Divide(%rip), %rax
	leave
LCFI2110:
	ret
LFE580:
__m3_fault:
LFB581:
	pushq	%rbp
LCFI2111:
	movq	%rsp, %rbp
LCFI2112:
	subq	$16, %rsp
LCFI2113:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	%rax, %rsi
	leaq	_MM_Divide(%rip), %rdi
	call	_RTHooks__ReportFault
	leave
LCFI2114:
	ret
LFE581:
	.const_data
	.align 5
_L_1:
	.ascii "Divide_M3"
	.space 1
	.ascii "Divide_param_i8_i8"
	.space 1
	.ascii "uDivide_param_i8_i8"
	.space 1
	.ascii "Divide_var_i8_i8"
	.space 1
	.ascii "uDivide_var_i8_i8"
	.space 1
	.ascii "Divide_param_i8_L"
	.space 1
	.ascii "uDivide_param_i8_L"
	.space 1
	.ascii "Divide_var_i8_L"
	.space 1
	.ascii "uDivide_var_i8_L"
	.space 1
	.ascii "Divide_param_i8_i64"
	.space 1
	.ascii "uDivide_param_i8_i64"
	.space 1
	.ascii "Divide_var_i8_i64"
	.space 1
	.ascii "uDivide_var_i8_i64"
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
	.ascii "Divide_param_i8_i32"
	.space 1
	.ascii "uDivide_param_i8_i32"
	.space 1
	.ascii "Divide_var_i8_i32"
	.space 1
	.ascii "uDivide_var_i8_i32"
	.space 1
	.ascii "Divide_param_i8_u64"
	.space 1
	.ascii "uDivide_param_i8_u64"
	.space 1
	.ascii "Divide_var_i8_u64"
	.space 1
	.ascii "uDivide_var_i8_u64"
	.space 1
	.ascii "Divide_param_i8_i16"
	.space 1
	.ascii "uDivide_param_i8_i16"
	.space 1
	.ascii "Divide_var_i8_i16"
	.space 1
	.ascii "uDivide_var_i8_i16"
	.space 1
	.ascii "Divide_param_i8_I"
	.space 1
	.ascii "uDivide_param_i8_I"
	.space 1
	.ascii "Divide_var_i8_I"
	.space 1
	.ascii "uDivide_var_i8_I"
	.space 1
	.ascii "Divide_param_i8_LC"
	.space 1
	.ascii "uDivide_param_i8_LC"
	.space 1
	.ascii "Divide_var_i8_LC"
	.space 1
	.ascii "uDivide_var_i8_LC"
	.space 1
	.ascii "Divide_param_i8_u32"
	.space 1
	.ascii "uDivide_param_i8_u32"
	.space 1
	.ascii "Divide_var_i8_u32"
	.space 1
	.ascii "uDivide_var_i8_u32"
	.space 1
	.ascii "Divide_param_i8_u16"
	.space 1
	.ascii "uDivide_param_i8_u16"
	.space 1
	.ascii "Divide_var_i8_u16"
	.space 1
	.ascii "uDivide_var_i8_u16"
	.space 1
	.ascii "Divide_param_L_i8"
	.space 1
	.ascii "uDivide_param_L_i8"
	.space 1
	.ascii "Divide_var_L_i8"
	.space 1
	.ascii "uDivide_var_L_i8"
	.space 1
	.ascii "Divide_param_L_L"
	.space 1
	.ascii "uDivide_param_L_L"
	.space 1
	.ascii "Divide_var_L_L"
	.space 1
	.ascii "uDivide_var_L_L"
	.space 1
	.ascii "Divide_param_L_i64"
	.space 1
	.ascii "uDivide_param_L_i64"
	.space 1
	.ascii "Divide_var_L_i64"
	.space 1
	.ascii "uDivide_var_L_i64"
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
	.ascii "Divide_param_L_i32"
	.space 1
	.ascii "uDivide_param_L_i32"
	.space 1
	.ascii "Divide_var_L_i32"
	.space 1
	.ascii "uDivide_var_L_i32"
	.space 1
	.ascii "Divide_param_L_u64"
	.space 1
	.ascii "uDivide_param_L_u64"
	.space 1
	.ascii "Divide_var_L_u64"
	.space 1
	.ascii "uDivide_var_L_u64"
	.space 1
	.ascii "Divide_param_L_i16"
	.space 1
	.ascii "uDivide_param_L_i16"
	.space 1
	.ascii "Divide_var_L_i16"
	.space 1
	.ascii "uDivide_var_L_i16"
	.space 1
	.ascii "Divide_param_L_I"
	.space 1
	.ascii "uDivide_param_L_I"
	.space 1
	.ascii "Divide_var_L_I"
	.space 1
	.ascii "uDivide_var_L_I"
	.space 1
	.ascii "Divide_param_L_LC"
	.space 1
	.ascii "uDivide_param_L_LC"
	.space 1
	.ascii "Divide_var_L_LC"
	.space 1
	.ascii "uDivide_var_L_LC"
	.space 1
	.ascii "Divide_param_L_u32"
	.space 1
	.ascii "uDivide_param_L_u32"
	.space 1
	.ascii "Divide_var_L_u32"
	.space 1
	.ascii "uDivide_var_L_u32"
	.space 1
	.ascii "Divide_param_L_u16"
	.space 1
	.ascii "uDivide_param_L_u16"
	.space 1
	.ascii "Divide_var_L_u16"
	.space 1
	.ascii "uDivide_var_L_u16"
	.space 1
	.ascii "Divide_param_f32_f32"
	.space 1
	.ascii "Divide_var_f32_f32"
	.space 1
	.ascii "Divide_param_i64_i8"
	.space 1
	.ascii "uDivide_param_i64_i8"
	.space 1
	.ascii "Divide_var_i64_i8"
	.space 1
	.ascii "uDivide_var_i64_i8"
	.space 1
	.ascii "Divide_param_i64_L"
	.space 1
	.ascii "uDivide_param_i64_L"
	.space 1
	.ascii "Divide_var_i64_L"
	.space 1
	.ascii "uDivide_var_i64_L"
	.space 1
	.ascii "Divide_param_i64_i64"
	.space 1
	.ascii "uDivide_param_i64_i64"
	.space 1
	.ascii "Divide_var_i64_i64"
	.space 1
	.ascii "uDivide_var_i64_i64"
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
	.ascii "Divide_param_i64_i32"
	.space 1
	.ascii "uDivide_param_i64_i32"
	.space 1
	.ascii "Divide_var_i64_i32"
	.space 1
	.ascii "uDivide_var_i64_i32"
	.space 1
	.ascii "Divide_param_i64_u64"
	.space 1
	.ascii "uDivide_param_i64_u64"
	.space 1
	.ascii "Divide_var_i64_u64"
	.space 1
	.ascii "uDivide_var_i64_u64"
	.space 1
	.ascii "Divide_param_i64_i16"
	.space 1
	.ascii "uDivide_param_i64_i16"
	.space 1
	.ascii "Divide_var_i64_i16"
	.space 1
	.ascii "uDivide_var_i64_i16"
	.space 1
	.ascii "Divide_param_i64_I"
	.space 1
	.ascii "uDivide_param_i64_I"
	.space 1
	.ascii "Divide_var_i64_I"
	.space 1
	.ascii "uDivide_var_i64_I"
	.space 1
	.ascii "Divide_param_i64_LC"
	.space 1
	.ascii "uDivide_param_i64_LC"
	.space 1
	.ascii "Divide_var_i64_LC"
	.space 1
	.ascii "uDivide_var_i64_LC"
	.space 1
	.ascii "Divide_param_i64_u32"
	.space 1
	.ascii "uDivide_param_i64_u32"
	.space 1
	.ascii "Divide_var_i64_u32"
	.space 1
	.ascii "uDivide_var_i64_u32"
	.space 1
	.ascii "Divide_param_i64_u16"
	.space 1
	.ascii "uDivide_param_i64_u16"
	.space 1
	.ascii "Divide_var_i64_u16"
	.space 1
	.ascii "uDivide_var_i64_u16"
	.space 1
	.ascii "Divide_param_u8_i8"
	.space 1
	.ascii "uDivide_param_u8_i8"
	.space 1
	.ascii "Divide_var_u8_i8"
	.space 1
	.ascii "uDivide_var_u8_i8"
	.space 1
	.ascii "Divide_param_u8_L"
	.space 1
	.ascii "uDivide_param_u8_L"
	.space 1
	.ascii "Divide_var_u8_L"
	.space 1
	.ascii "uDivide_var_u8_L"
	.space 1
	.ascii "Divide_param_u8_i64"
	.space 1
	.ascii "uDivide_param_u8_i64"
	.space 1
	.ascii "Divide_var_u8_i64"
	.space 1
	.ascii "uDivide_var_u8_i64"
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
	.ascii "Divide_param_u8_i32"
	.space 1
	.ascii "uDivide_param_u8_i32"
	.space 1
	.ascii "Divide_var_u8_i32"
	.space 1
	.ascii "uDivide_var_u8_i32"
	.space 1
	.ascii "Divide_param_u8_u64"
	.space 1
	.ascii "uDivide_param_u8_u64"
	.space 1
	.ascii "Divide_var_u8_u64"
	.space 1
	.ascii "uDivide_var_u8_u64"
	.space 1
	.ascii "Divide_param_u8_i16"
	.space 1
	.ascii "uDivide_param_u8_i16"
	.space 1
	.ascii "Divide_var_u8_i16"
	.space 1
	.ascii "uDivide_var_u8_i16"
	.space 1
	.ascii "Divide_param_u8_I"
	.space 1
	.ascii "uDivide_param_u8_I"
	.space 1
	.ascii "Divide_var_u8_I"
	.space 1
	.ascii "uDivide_var_u8_I"
	.space 1
	.ascii "Divide_param_u8_LC"
	.space 1
	.ascii "uDivide_param_u8_LC"
	.space 1
	.ascii "Divide_var_u8_LC"
	.space 1
	.ascii "uDivide_var_u8_LC"
	.space 1
	.ascii "Divide_param_u8_u32"
	.space 1
	.ascii "uDivide_param_u8_u32"
	.space 1
	.ascii "Divide_var_u8_u32"
	.space 1
	.ascii "uDivide_var_u8_u32"
	.space 1
	.ascii "Divide_param_u8_u16"
	.space 1
	.ascii "uDivide_param_u8_u16"
	.space 1
	.ascii "Divide_var_u8_u16"
	.space 1
	.ascii "uDivide_var_u8_u16"
	.space 1
	.ascii "Divide_param_C_i8"
	.space 1
	.ascii "uDivide_param_C_i8"
	.space 1
	.ascii "Divide_var_C_i8"
	.space 1
	.ascii "uDivide_var_C_i8"
	.space 1
	.ascii "Divide_param_C_L"
	.space 1
	.ascii "uDivide_param_C_L"
	.space 1
	.ascii "Divide_var_C_L"
	.space 1
	.ascii "uDivide_var_C_L"
	.space 1
	.ascii "Divide_param_C_i64"
	.space 1
	.ascii "uDivide_param_C_i64"
	.space 1
	.ascii "Divide_var_C_i64"
	.space 1
	.ascii "uDivide_var_C_i64"
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
	.ascii "Divide_param_C_i32"
	.space 1
	.ascii "uDivide_param_C_i32"
	.space 1
	.ascii "Divide_var_C_i32"
	.space 1
	.ascii "uDivide_var_C_i32"
	.space 1
	.ascii "Divide_param_C_u64"
	.space 1
	.ascii "uDivide_param_C_u64"
	.space 1
	.ascii "Divide_var_C_u64"
	.space 1
	.ascii "uDivide_var_C_u64"
	.space 1
	.ascii "Divide_param_C_i16"
	.space 1
	.ascii "uDivide_param_C_i16"
	.space 1
	.ascii "Divide_var_C_i16"
	.space 1
	.ascii "uDivide_var_C_i16"
	.space 1
	.ascii "Divide_param_C_I"
	.space 1
	.ascii "uDivide_param_C_I"
	.space 1
	.ascii "Divide_var_C_I"
	.space 1
	.ascii "uDivide_var_C_I"
	.space 1
	.ascii "Divide_param_C_LC"
	.space 1
	.ascii "uDivide_param_C_LC"
	.space 1
	.ascii "Divide_var_C_LC"
	.space 1
	.ascii "uDivide_var_C_LC"
	.space 1
	.ascii "Divide_param_C_u32"
	.space 1
	.ascii "uDivide_param_C_u32"
	.space 1
	.ascii "Divide_var_C_u32"
	.space 1
	.ascii "uDivide_var_C_u32"
	.space 1
	.ascii "Divide_param_C_u16"
	.space 1
	.ascii "uDivide_param_C_u16"
	.space 1
	.ascii "Divide_var_C_u16"
	.space 1
	.ascii "uDivide_var_C_u16"
	.space 1
	.ascii "Divide_param_i32_i8"
	.space 1
	.ascii "uDivide_param_i32_i8"
	.space 1
	.ascii "Divide_var_i32_i8"
	.space 1
	.ascii "uDivide_var_i32_i8"
	.space 1
	.ascii "Divide_param_i32_L"
	.space 1
	.ascii "uDivide_param_i32_L"
	.space 1
	.ascii "Divide_var_i32_L"
	.space 1
	.ascii "uDivide_var_i32_L"
	.space 1
	.ascii "Divide_param_i32_i64"
	.space 1
	.ascii "uDivide_param_i32_i64"
	.space 1
	.ascii "Divide_var_i32_i64"
	.space 1
	.ascii "uDivide_var_i32_i64"
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
	.ascii "Divide_param_i32_i32"
	.space 1
	.ascii "uDivide_param_i32_i32"
	.space 1
	.ascii "Divide_var_i32_i32"
	.space 1
	.ascii "uDivide_var_i32_i32"
	.space 1
	.ascii "Divide_param_i32_u64"
	.space 1
	.ascii "uDivide_param_i32_u64"
	.space 1
	.ascii "Divide_var_i32_u64"
	.space 1
	.ascii "uDivide_var_i32_u64"
	.space 1
	.ascii "Divide_param_i32_i16"
	.space 1
	.ascii "uDivide_param_i32_i16"
	.space 1
	.ascii "Divide_var_i32_i16"
	.space 1
	.ascii "uDivide_var_i32_i16"
	.space 1
	.ascii "Divide_param_i32_I"
	.space 1
	.ascii "uDivide_param_i32_I"
	.space 1
	.ascii "Divide_var_i32_I"
	.space 1
	.ascii "uDivide_var_i32_I"
	.space 1
	.ascii "Divide_param_i32_LC"
	.space 1
	.ascii "uDivide_param_i32_LC"
	.space 1
	.ascii "Divide_var_i32_LC"
	.space 1
	.ascii "uDivide_var_i32_LC"
	.space 1
	.ascii "Divide_param_i32_u32"
	.space 1
	.ascii "uDivide_param_i32_u32"
	.space 1
	.ascii "Divide_var_i32_u32"
	.space 1
	.ascii "uDivide_var_i32_u32"
	.space 1
	.ascii "Divide_param_i32_u16"
	.space 1
	.ascii "uDivide_param_i32_u16"
	.space 1
	.ascii "Divide_var_i32_u16"
	.space 1
	.ascii "uDivide_var_i32_u16"
	.space 1
	.ascii "Divide_param_f64_f64"
	.space 1
	.ascii "Divide_var_f64_f64"
	.space 1
	.ascii "Divide_param_u64_i8"
	.space 1
	.ascii "uDivide_param_u64_i8"
	.space 1
	.ascii "Divide_var_u64_i8"
	.space 1
	.ascii "uDivide_var_u64_i8"
	.space 1
	.ascii "Divide_param_u64_L"
	.space 1
	.ascii "uDivide_param_u64_L"
	.space 1
	.ascii "Divide_var_u64_L"
	.space 1
	.ascii "uDivide_var_u64_L"
	.space 1
	.ascii "Divide_param_u64_i64"
	.space 1
	.ascii "uDivide_param_u64_i64"
	.space 1
	.ascii "Divide_var_u64_i64"
	.space 1
	.ascii "uDivide_var_u64_i64"
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
	.ascii "Divide_param_u64_i32"
	.space 1
	.ascii "uDivide_param_u64_i32"
	.space 1
	.ascii "Divide_var_u64_i32"
	.space 1
	.ascii "uDivide_var_u64_i32"
	.space 1
	.ascii "Divide_param_u64_u64"
	.space 1
	.ascii "uDivide_param_u64_u64"
	.space 1
	.ascii "Divide_var_u64_u64"
	.space 1
	.ascii "uDivide_var_u64_u64"
	.space 1
	.ascii "Divide_param_u64_i16"
	.space 1
	.ascii "uDivide_param_u64_i16"
	.space 1
	.ascii "Divide_var_u64_i16"
	.space 1
	.ascii "uDivide_var_u64_i16"
	.space 1
	.ascii "Divide_param_u64_I"
	.space 1
	.ascii "uDivide_param_u64_I"
	.space 1
	.ascii "Divide_var_u64_I"
	.space 1
	.ascii "uDivide_var_u64_I"
	.space 1
	.ascii "Divide_param_u64_LC"
	.space 1
	.ascii "uDivide_param_u64_LC"
	.space 1
	.ascii "Divide_var_u64_LC"
	.space 1
	.ascii "uDivide_var_u64_LC"
	.space 1
	.ascii "Divide_param_u64_u32"
	.space 1
	.ascii "uDivide_param_u64_u32"
	.space 1
	.ascii "Divide_var_u64_u32"
	.space 1
	.ascii "uDivide_var_u64_u32"
	.space 1
	.ascii "Divide_param_u64_u16"
	.space 1
	.ascii "uDivide_param_u64_u16"
	.space 1
	.ascii "Divide_var_u64_u16"
	.space 1
	.ascii "uDivide_var_u64_u16"
	.space 1
	.ascii "Divide_param_i16_i8"
	.space 1
	.ascii "uDivide_param_i16_i8"
	.space 1
	.ascii "Divide_var_i16_i8"
	.space 1
	.ascii "uDivide_var_i16_i8"
	.space 1
	.ascii "Divide_param_i16_L"
	.space 1
	.ascii "uDivide_param_i16_L"
	.space 1
	.ascii "Divide_var_i16_L"
	.space 1
	.ascii "uDivide_var_i16_L"
	.space 1
	.ascii "Divide_param_i16_i64"
	.space 1
	.ascii "uDivide_param_i16_i64"
	.space 1
	.ascii "Divide_var_i16_i64"
	.space 1
	.ascii "uDivide_var_i16_i64"
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
	.ascii "Divide_param_i16_i32"
	.space 1
	.ascii "uDivide_param_i16_i32"
	.space 1
	.ascii "Divide_var_i16_i32"
	.space 1
	.ascii "uDivide_var_i16_i32"
	.space 1
	.ascii "Divide_param_i16_u64"
	.space 1
	.ascii "uDivide_param_i16_u64"
	.space 1
	.ascii "Divide_var_i16_u64"
	.space 1
	.ascii "uDivide_var_i16_u64"
	.space 1
	.ascii "Divide_param_i16_i16"
	.space 1
	.ascii "uDivide_param_i16_i16"
	.space 1
	.ascii "Divide_var_i16_i16"
	.space 1
	.ascii "uDivide_var_i16_i16"
	.space 1
	.ascii "Divide_param_i16_I"
	.space 1
	.ascii "uDivide_param_i16_I"
	.space 1
	.ascii "Divide_var_i16_I"
	.space 1
	.ascii "uDivide_var_i16_I"
	.space 1
	.ascii "Divide_param_i16_LC"
	.space 1
	.ascii "uDivide_param_i16_LC"
	.space 1
	.ascii "Divide_var_i16_LC"
	.space 1
	.ascii "uDivide_var_i16_LC"
	.space 1
	.ascii "Divide_param_i16_u32"
	.space 1
	.ascii "uDivide_param_i16_u32"
	.space 1
	.ascii "Divide_var_i16_u32"
	.space 1
	.ascii "uDivide_var_i16_u32"
	.space 1
	.ascii "Divide_param_i16_u16"
	.space 1
	.ascii "uDivide_param_i16_u16"
	.space 1
	.ascii "Divide_var_i16_u16"
	.space 1
	.ascii "uDivide_var_i16_u16"
	.space 1
	.ascii "Divide_param_I_i8"
	.space 1
	.ascii "uDivide_param_I_i8"
	.space 1
	.ascii "Divide_var_I_i8"
	.space 1
	.ascii "uDivide_var_I_i8"
	.space 1
	.ascii "Divide_param_I_L"
	.space 1
	.ascii "uDivide_param_I_L"
	.space 1
	.ascii "Divide_var_I_L"
	.space 1
	.ascii "uDivide_var_I_L"
	.space 1
	.ascii "Divide_param_I_i64"
	.space 1
	.ascii "uDivide_param_I_i64"
	.space 1
	.ascii "Divide_var_I_i64"
	.space 1
	.ascii "uDivide_var_I_i64"
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
	.ascii "Divide_param_I_i32"
	.space 1
	.ascii "uDivide_param_I_i32"
	.space 1
	.ascii "Divide_var_I_i32"
	.space 1
	.ascii "uDivide_var_I_i32"
	.space 1
	.ascii "Divide_param_I_u64"
	.space 1
	.ascii "uDivide_param_I_u64"
	.space 1
	.ascii "Divide_var_I_u64"
	.space 1
	.ascii "uDivide_var_I_u64"
	.space 1
	.ascii "Divide_param_I_i16"
	.space 1
	.ascii "uDivide_param_I_i16"
	.space 1
	.ascii "Divide_var_I_i16"
	.space 1
	.ascii "uDivide_var_I_i16"
	.space 1
	.ascii "Divide_param_I_I"
	.space 1
	.ascii "uDivide_param_I_I"
	.space 1
	.ascii "Divide_var_I_I"
	.space 1
	.ascii "uDivide_var_I_I"
	.space 1
	.ascii "Divide_param_I_LC"
	.space 1
	.ascii "uDivide_param_I_LC"
	.space 1
	.ascii "Divide_var_I_LC"
	.space 1
	.ascii "uDivide_var_I_LC"
	.space 1
	.ascii "Divide_param_I_u32"
	.space 1
	.ascii "uDivide_param_I_u32"
	.space 1
	.ascii "Divide_var_I_u32"
	.space 1
	.ascii "uDivide_var_I_u32"
	.space 1
	.ascii "Divide_param_I_u16"
	.space 1
	.ascii "uDivide_param_I_u16"
	.space 1
	.ascii "Divide_var_I_u16"
	.space 1
	.ascii "uDivide_var_I_u16"
	.space 1
	.ascii "Divide_param_LC_i8"
	.space 1
	.ascii "uDivide_param_LC_i8"
	.space 1
	.ascii "Divide_var_LC_i8"
	.space 1
	.ascii "uDivide_var_LC_i8"
	.space 1
	.ascii "Divide_param_LC_L"
	.space 1
	.ascii "uDivide_param_LC_L"
	.space 1
	.ascii "Divide_var_LC_L"
	.space 1
	.ascii "uDivide_var_LC_L"
	.space 1
	.ascii "Divide_param_LC_i64"
	.space 1
	.ascii "uDivide_param_LC_i64"
	.space 1
	.ascii "Divide_var_LC_i64"
	.space 1
	.ascii "uDivide_var_LC_i64"
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
	.ascii "Divide_param_LC_i32"
	.space 1
	.ascii "uDivide_param_LC_i32"
	.space 1
	.ascii "Divide_var_LC_i32"
	.space 1
	.ascii "uDivide_var_LC_i32"
	.space 1
	.ascii "Divide_param_LC_u64"
	.space 1
	.ascii "uDivide_param_LC_u64"
	.space 1
	.ascii "Divide_var_LC_u64"
	.space 1
	.ascii "uDivide_var_LC_u64"
	.space 1
	.ascii "Divide_param_LC_i16"
	.space 1
	.ascii "uDivide_param_LC_i16"
	.space 1
	.ascii "Divide_var_LC_i16"
	.space 1
	.ascii "uDivide_var_LC_i16"
	.space 1
	.ascii "Divide_param_LC_I"
	.space 1
	.ascii "uDivide_param_LC_I"
	.space 1
	.ascii "Divide_var_LC_I"
	.space 1
	.ascii "uDivide_var_LC_I"
	.space 1
	.ascii "Divide_param_LC_LC"
	.space 1
	.ascii "uDivide_param_LC_LC"
	.space 1
	.ascii "Divide_var_LC_LC"
	.space 1
	.ascii "uDivide_var_LC_LC"
	.space 1
	.ascii "Divide_param_LC_u32"
	.space 1
	.ascii "uDivide_param_LC_u32"
	.space 1
	.ascii "Divide_var_LC_u32"
	.space 1
	.ascii "uDivide_var_LC_u32"
	.space 1
	.ascii "Divide_param_LC_u16"
	.space 1
	.ascii "uDivide_param_LC_u16"
	.space 1
	.ascii "Divide_var_LC_u16"
	.space 1
	.ascii "uDivide_var_LC_u16"
	.space 1
	.ascii "Divide_param_u32_i8"
	.space 1
	.ascii "uDivide_param_u32_i8"
	.space 1
	.ascii "Divide_var_u32_i8"
	.space 1
	.ascii "uDivide_var_u32_i8"
	.space 1
	.ascii "Divide_param_u32_L"
	.space 1
	.ascii "uDivide_param_u32_L"
	.space 1
	.ascii "Divide_var_u32_L"
	.space 1
	.ascii "uDivide_var_u32_L"
	.space 1
	.ascii "Divide_param_u32_i64"
	.space 1
	.ascii "uDivide_param_u32_i64"
	.space 1
	.ascii "Divide_var_u32_i64"
	.space 1
	.ascii "uDivide_var_u32_i64"
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
	.ascii "Divide_param_u32_i32"
	.space 1
	.ascii "uDivide_param_u32_i32"
	.space 1
	.ascii "Divide_var_u32_i32"
	.space 1
	.ascii "uDivide_var_u32_i32"
	.space 1
	.ascii "Divide_param_u32_u64"
	.space 1
	.ascii "uDivide_param_u32_u64"
	.space 1
	.ascii "Divide_var_u32_u64"
	.space 1
	.ascii "uDivide_var_u32_u64"
	.space 1
	.ascii "Divide_param_u32_i16"
	.space 1
	.ascii "uDivide_param_u32_i16"
	.space 1
	.ascii "Divide_var_u32_i16"
	.space 1
	.ascii "uDivide_var_u32_i16"
	.space 1
	.ascii "Divide_param_u32_I"
	.space 1
	.ascii "uDivide_param_u32_I"
	.space 1
	.ascii "Divide_var_u32_I"
	.space 1
	.ascii "uDivide_var_u32_I"
	.space 1
	.ascii "Divide_param_u32_LC"
	.space 1
	.ascii "uDivide_param_u32_LC"
	.space 1
	.ascii "Divide_var_u32_LC"
	.space 1
	.ascii "uDivide_var_u32_LC"
	.space 1
	.ascii "Divide_param_u32_u32"
	.space 1
	.ascii "uDivide_param_u32_u32"
	.space 1
	.ascii "Divide_var_u32_u32"
	.space 1
	.ascii "uDivide_var_u32_u32"
	.space 1
	.ascii "Divide_param_u32_u16"
	.space 1
	.ascii "uDivide_param_u32_u16"
	.space 1
	.ascii "Divide_var_u32_u16"
	.space 1
	.ascii "uDivide_var_u32_u16"
	.space 1
	.ascii "Divide_param_u16_i8"
	.space 1
	.ascii "uDivide_param_u16_i8"
	.space 1
	.ascii "Divide_var_u16_i8"
	.space 1
	.ascii "uDivide_var_u16_i8"
	.space 1
	.ascii "Divide_param_u16_L"
	.space 1
	.ascii "uDivide_param_u16_L"
	.space 1
	.ascii "Divide_var_u16_L"
	.space 1
	.ascii "uDivide_var_u16_L"
	.space 1
	.ascii "Divide_param_u16_i64"
	.space 1
	.ascii "uDivide_param_u16_i64"
	.space 1
	.ascii "Divide_var_u16_i64"
	.space 1
	.ascii "uDivide_var_u16_i64"
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
	.ascii "Divide_param_u16_i32"
	.space 1
	.ascii "uDivide_param_u16_i32"
	.space 1
	.ascii "Divide_var_u16_i32"
	.space 1
	.ascii "uDivide_var_u16_i32"
	.space 1
	.ascii "Divide_param_u16_u64"
	.space 1
	.ascii "uDivide_param_u16_u64"
	.space 1
	.ascii "Divide_var_u16_u64"
	.space 1
	.ascii "uDivide_var_u16_u64"
	.space 1
	.ascii "Divide_param_u16_i16"
	.space 1
	.ascii "uDivide_param_u16_i16"
	.space 1
	.ascii "Divide_var_u16_i16"
	.space 1
	.ascii "uDivide_var_u16_i16"
	.space 1
	.ascii "Divide_param_u16_I"
	.space 1
	.ascii "uDivide_param_u16_I"
	.space 1
	.ascii "Divide_var_u16_I"
	.space 1
	.ascii "uDivide_var_u16_I"
	.space 1
	.ascii "Divide_param_u16_LC"
	.space 1
	.ascii "uDivide_param_u16_LC"
	.space 1
	.ascii "Divide_var_u16_LC"
	.space 1
	.ascii "uDivide_var_u16_LC"
	.space 1
	.ascii "Divide_param_u16_u32"
	.space 1
	.ascii "uDivide_param_u16_u32"
	.space 1
	.ascii "Divide_var_u16_u32"
	.space 1
	.ascii "uDivide_var_u16_u32"
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
	.quad	_Divide__Divide_param_i8_i8
	.quad	_L_1+10
	.quad	_Divide__uDivide_param_i8_i8
	.quad	_L_1+29
	.quad	_Divide__Divide_var_i8_i8
	.quad	_L_1+49
	.quad	_Divide__uDivide_var_i8_i8
	.quad	_L_1+66
	.quad	_Divide__Divide_param_i8_L
	.quad	_L_1+84
	.quad	_Divide__uDivide_param_i8_L
	.quad	_L_1+102
	.quad	_Divide__Divide_var_i8_L
	.quad	_L_1+121
	.quad	_Divide__uDivide_var_i8_L
	.quad	_L_1+137
	.quad	_Divide__Divide_param_i8_i64
	.quad	_L_1+154
	.quad	_Divide__uDivide_param_i8_i64
	.quad	_L_1+174
	.quad	_Divide__Divide_var_i8_i64
	.quad	_L_1+195
	.quad	_Divide__uDivide_var_i8_i64
	.quad	_L_1+213
	.quad	_Divide__Divide_param_i8_u8
	.quad	_L_1+232
	.quad	_Divide__uDivide_param_i8_u8
	.quad	_L_1+251
	.quad	_Divide__Divide_var_i8_u8
	.quad	_L_1+271
	.quad	_Divide__uDivide_var_i8_u8
	.quad	_L_1+288
	.quad	_Divide__Divide_param_i8_C
	.quad	_L_1+306
	.quad	_Divide__uDivide_param_i8_C
	.quad	_L_1+324
	.quad	_Divide__Divide_var_i8_C
	.quad	_L_1+343
	.quad	_Divide__uDivide_var_i8_C
	.quad	_L_1+359
	.quad	_Divide__Divide_param_i8_i32
	.quad	_L_1+376
	.quad	_Divide__uDivide_param_i8_i32
	.quad	_L_1+396
	.quad	_Divide__Divide_var_i8_i32
	.quad	_L_1+417
	.quad	_Divide__uDivide_var_i8_i32
	.quad	_L_1+435
	.quad	_Divide__Divide_param_i8_u64
	.quad	_L_1+454
	.quad	_Divide__uDivide_param_i8_u64
	.quad	_L_1+474
	.quad	_Divide__Divide_var_i8_u64
	.quad	_L_1+495
	.quad	_Divide__uDivide_var_i8_u64
	.quad	_L_1+513
	.quad	_Divide__Divide_param_i8_i16
	.quad	_L_1+532
	.quad	_Divide__uDivide_param_i8_i16
	.quad	_L_1+552
	.quad	_Divide__Divide_var_i8_i16
	.quad	_L_1+573
	.quad	_Divide__uDivide_var_i8_i16
	.quad	_L_1+591
	.quad	_Divide__Divide_param_i8_I
	.quad	_L_1+610
	.quad	_Divide__uDivide_param_i8_I
	.quad	_L_1+628
	.quad	_Divide__Divide_var_i8_I
	.quad	_L_1+647
	.quad	_Divide__uDivide_var_i8_I
	.quad	_L_1+663
	.quad	_Divide__Divide_param_i8_LC
	.quad	_L_1+680
	.quad	_Divide__uDivide_param_i8_LC
	.quad	_L_1+699
	.quad	_Divide__Divide_var_i8_LC
	.quad	_L_1+719
	.quad	_Divide__uDivide_var_i8_LC
	.quad	_L_1+736
	.quad	_Divide__Divide_param_i8_u32
	.quad	_L_1+754
	.quad	_Divide__uDivide_param_i8_u32
	.quad	_L_1+774
	.quad	_Divide__Divide_var_i8_u32
	.quad	_L_1+795
	.quad	_Divide__uDivide_var_i8_u32
	.quad	_L_1+813
	.quad	_Divide__Divide_param_i8_u16
	.quad	_L_1+832
	.quad	_Divide__uDivide_param_i8_u16
	.quad	_L_1+852
	.quad	_Divide__Divide_var_i8_u16
	.quad	_L_1+873
	.quad	_Divide__uDivide_var_i8_u16
	.quad	_L_1+891
	.quad	_Divide__Divide_param_L_i8
	.quad	_L_1+910
	.quad	_Divide__uDivide_param_L_i8
	.quad	_L_1+928
	.quad	_Divide__Divide_var_L_i8
	.quad	_L_1+947
	.quad	_Divide__uDivide_var_L_i8
	.quad	_L_1+963
	.quad	_Divide__Divide_param_L_L
	.quad	_L_1+980
	.quad	_Divide__uDivide_param_L_L
	.quad	_L_1+997
	.quad	_Divide__Divide_var_L_L
	.quad	_L_1+1015
	.quad	_Divide__uDivide_var_L_L
	.quad	_L_1+1030
	.quad	_Divide__Divide_param_L_i64
	.quad	_L_1+1046
	.quad	_Divide__uDivide_param_L_i64
	.quad	_L_1+1065
	.quad	_Divide__Divide_var_L_i64
	.quad	_L_1+1085
	.quad	_Divide__uDivide_var_L_i64
	.quad	_L_1+1102
	.quad	_Divide__Divide_param_L_u8
	.quad	_L_1+1120
	.quad	_Divide__uDivide_param_L_u8
	.quad	_L_1+1138
	.quad	_Divide__Divide_var_L_u8
	.quad	_L_1+1157
	.quad	_Divide__uDivide_var_L_u8
	.quad	_L_1+1173
	.quad	_Divide__Divide_param_L_C
	.quad	_L_1+1190
	.quad	_Divide__uDivide_param_L_C
	.quad	_L_1+1207
	.quad	_Divide__Divide_var_L_C
	.quad	_L_1+1225
	.quad	_Divide__uDivide_var_L_C
	.quad	_L_1+1240
	.quad	_Divide__Divide_param_L_i32
	.quad	_L_1+1256
	.quad	_Divide__uDivide_param_L_i32
	.quad	_L_1+1275
	.quad	_Divide__Divide_var_L_i32
	.quad	_L_1+1295
	.quad	_Divide__uDivide_var_L_i32
	.quad	_L_1+1312
	.quad	_Divide__Divide_param_L_u64
	.quad	_L_1+1330
	.quad	_Divide__uDivide_param_L_u64
	.quad	_L_1+1349
	.quad	_Divide__Divide_var_L_u64
	.quad	_L_1+1369
	.quad	_Divide__uDivide_var_L_u64
	.quad	_L_1+1386
	.quad	_Divide__Divide_param_L_i16
	.quad	_L_1+1404
	.quad	_Divide__uDivide_param_L_i16
	.quad	_L_1+1423
	.quad	_Divide__Divide_var_L_i16
	.quad	_L_1+1443
	.quad	_Divide__uDivide_var_L_i16
	.quad	_L_1+1460
	.quad	_Divide__Divide_param_L_I
	.quad	_L_1+1478
	.quad	_Divide__uDivide_param_L_I
	.quad	_L_1+1495
	.quad	_Divide__Divide_var_L_I
	.quad	_L_1+1513
	.quad	_Divide__uDivide_var_L_I
	.quad	_L_1+1528
	.quad	_Divide__Divide_param_L_LC
	.quad	_L_1+1544
	.quad	_Divide__uDivide_param_L_LC
	.quad	_L_1+1562
	.quad	_Divide__Divide_var_L_LC
	.quad	_L_1+1581
	.quad	_Divide__uDivide_var_L_LC
	.quad	_L_1+1597
	.quad	_Divide__Divide_param_L_u32
	.quad	_L_1+1614
	.quad	_Divide__uDivide_param_L_u32
	.quad	_L_1+1633
	.quad	_Divide__Divide_var_L_u32
	.quad	_L_1+1653
	.quad	_Divide__uDivide_var_L_u32
	.quad	_L_1+1670
	.quad	_Divide__Divide_param_L_u16
	.quad	_L_1+1688
	.quad	_Divide__uDivide_param_L_u16
	.quad	_L_1+1707
	.quad	_Divide__Divide_var_L_u16
	.quad	_L_1+1727
	.quad	_Divide__uDivide_var_L_u16
	.quad	_L_1+1744
	.quad	_Divide__Divide_param_f32_f32
	.quad	_L_1+1762
	.quad	_Divide__Divide_var_f32_f32
	.quad	_L_1+1783
	.quad	_Divide__Divide_param_i64_i8
	.quad	_L_1+1802
	.quad	_Divide__uDivide_param_i64_i8
	.quad	_L_1+1822
	.quad	_Divide__Divide_var_i64_i8
	.quad	_L_1+1843
	.quad	_Divide__uDivide_var_i64_i8
	.quad	_L_1+1861
	.quad	_Divide__Divide_param_i64_L
	.quad	_L_1+1880
	.quad	_Divide__uDivide_param_i64_L
	.quad	_L_1+1899
	.quad	_Divide__Divide_var_i64_L
	.quad	_L_1+1919
	.quad	_Divide__uDivide_var_i64_L
	.quad	_L_1+1936
	.quad	_Divide__Divide_param_i64_i64
	.quad	_L_1+1954
	.quad	_Divide__uDivide_param_i64_i64
	.quad	_L_1+1975
	.quad	_Divide__Divide_var_i64_i64
	.quad	_L_1+1997
	.quad	_Divide__uDivide_var_i64_i64
	.quad	_L_1+2016
	.quad	_Divide__Divide_param_i64_u8
	.quad	_L_1+2036
	.quad	_Divide__uDivide_param_i64_u8
	.quad	_L_1+2056
	.quad	_Divide__Divide_var_i64_u8
	.quad	_L_1+2077
	.quad	_Divide__uDivide_var_i64_u8
	.quad	_L_1+2095
	.quad	_Divide__Divide_param_i64_C
	.quad	_L_1+2114
	.quad	_Divide__uDivide_param_i64_C
	.quad	_L_1+2133
	.quad	_Divide__Divide_var_i64_C
	.quad	_L_1+2153
	.quad	_Divide__uDivide_var_i64_C
	.quad	_L_1+2170
	.quad	_Divide__Divide_param_i64_i32
	.quad	_L_1+2188
	.quad	_Divide__uDivide_param_i64_i32
	.quad	_L_1+2209
	.quad	_Divide__Divide_var_i64_i32
	.quad	_L_1+2231
	.quad	_Divide__uDivide_var_i64_i32
	.quad	_L_1+2250
	.quad	_Divide__Divide_param_i64_u64
	.quad	_L_1+2270
	.quad	_Divide__uDivide_param_i64_u64
	.quad	_L_1+2291
	.quad	_Divide__Divide_var_i64_u64
	.quad	_L_1+2313
	.quad	_Divide__uDivide_var_i64_u64
	.quad	_L_1+2332
	.quad	_Divide__Divide_param_i64_i16
	.quad	_L_1+2352
	.quad	_Divide__uDivide_param_i64_i16
	.quad	_L_1+2373
	.quad	_Divide__Divide_var_i64_i16
	.quad	_L_1+2395
	.quad	_Divide__uDivide_var_i64_i16
	.quad	_L_1+2414
	.quad	_Divide__Divide_param_i64_I
	.quad	_L_1+2434
	.quad	_Divide__uDivide_param_i64_I
	.quad	_L_1+2453
	.quad	_Divide__Divide_var_i64_I
	.quad	_L_1+2473
	.quad	_Divide__uDivide_var_i64_I
	.quad	_L_1+2490
	.quad	_Divide__Divide_param_i64_LC
	.quad	_L_1+2508
	.quad	_Divide__uDivide_param_i64_LC
	.quad	_L_1+2528
	.quad	_Divide__Divide_var_i64_LC
	.quad	_L_1+2549
	.quad	_Divide__uDivide_var_i64_LC
	.quad	_L_1+2567
	.quad	_Divide__Divide_param_i64_u32
	.quad	_L_1+2586
	.quad	_Divide__uDivide_param_i64_u32
	.quad	_L_1+2607
	.quad	_Divide__Divide_var_i64_u32
	.quad	_L_1+2629
	.quad	_Divide__uDivide_var_i64_u32
	.quad	_L_1+2648
	.quad	_Divide__Divide_param_i64_u16
	.quad	_L_1+2668
	.quad	_Divide__uDivide_param_i64_u16
	.quad	_L_1+2689
	.quad	_Divide__Divide_var_i64_u16
	.quad	_L_1+2711
	.quad	_Divide__uDivide_var_i64_u16
	.quad	_L_1+2730
	.quad	_Divide__Divide_param_u8_i8
	.quad	_L_1+2750
	.quad	_Divide__uDivide_param_u8_i8
	.quad	_L_1+2769
	.quad	_Divide__Divide_var_u8_i8
	.quad	_L_1+2789
	.quad	_Divide__uDivide_var_u8_i8
	.quad	_L_1+2806
	.quad	_Divide__Divide_param_u8_L
	.quad	_L_1+2824
	.quad	_Divide__uDivide_param_u8_L
	.quad	_L_1+2842
	.quad	_Divide__Divide_var_u8_L
	.quad	_L_1+2861
	.quad	_Divide__uDivide_var_u8_L
	.quad	_L_1+2877
	.quad	_Divide__Divide_param_u8_i64
	.quad	_L_1+2894
	.quad	_Divide__uDivide_param_u8_i64
	.quad	_L_1+2914
	.quad	_Divide__Divide_var_u8_i64
	.quad	_L_1+2935
	.quad	_Divide__uDivide_var_u8_i64
	.quad	_L_1+2953
	.quad	_Divide__Divide_param_u8_u8
	.quad	_L_1+2972
	.quad	_Divide__uDivide_param_u8_u8
	.quad	_L_1+2991
	.quad	_Divide__Divide_var_u8_u8
	.quad	_L_1+3011
	.quad	_Divide__uDivide_var_u8_u8
	.quad	_L_1+3028
	.quad	_Divide__Divide_param_u8_C
	.quad	_L_1+3046
	.quad	_Divide__uDivide_param_u8_C
	.quad	_L_1+3064
	.quad	_Divide__Divide_var_u8_C
	.quad	_L_1+3083
	.quad	_Divide__uDivide_var_u8_C
	.quad	_L_1+3099
	.quad	_Divide__Divide_param_u8_i32
	.quad	_L_1+3116
	.quad	_Divide__uDivide_param_u8_i32
	.quad	_L_1+3136
	.quad	_Divide__Divide_var_u8_i32
	.quad	_L_1+3157
	.quad	_Divide__uDivide_var_u8_i32
	.quad	_L_1+3175
	.quad	_Divide__Divide_param_u8_u64
	.quad	_L_1+3194
	.quad	_Divide__uDivide_param_u8_u64
	.quad	_L_1+3214
	.quad	_Divide__Divide_var_u8_u64
	.quad	_L_1+3235
	.quad	_Divide__uDivide_var_u8_u64
	.quad	_L_1+3253
	.quad	_Divide__Divide_param_u8_i16
	.quad	_L_1+3272
	.quad	_Divide__uDivide_param_u8_i16
	.quad	_L_1+3292
	.quad	_Divide__Divide_var_u8_i16
	.quad	_L_1+3313
	.quad	_Divide__uDivide_var_u8_i16
	.quad	_L_1+3331
	.quad	_Divide__Divide_param_u8_I
	.quad	_L_1+3350
	.quad	_Divide__uDivide_param_u8_I
	.quad	_L_1+3368
	.quad	_Divide__Divide_var_u8_I
	.quad	_L_1+3387
	.quad	_Divide__uDivide_var_u8_I
	.quad	_L_1+3403
	.quad	_Divide__Divide_param_u8_LC
	.quad	_L_1+3420
	.quad	_Divide__uDivide_param_u8_LC
	.quad	_L_1+3439
	.quad	_Divide__Divide_var_u8_LC
	.quad	_L_1+3459
	.quad	_Divide__uDivide_var_u8_LC
	.quad	_L_1+3476
	.quad	_Divide__Divide_param_u8_u32
	.quad	_L_1+3494
	.quad	_Divide__uDivide_param_u8_u32
	.quad	_L_1+3514
	.quad	_Divide__Divide_var_u8_u32
	.quad	_L_1+3535
	.quad	_Divide__uDivide_var_u8_u32
	.quad	_L_1+3553
	.quad	_Divide__Divide_param_u8_u16
	.quad	_L_1+3572
	.quad	_Divide__uDivide_param_u8_u16
	.quad	_L_1+3592
	.quad	_Divide__Divide_var_u8_u16
	.quad	_L_1+3613
	.quad	_Divide__uDivide_var_u8_u16
	.quad	_L_1+3631
	.quad	_Divide__Divide_param_C_i8
	.quad	_L_1+3650
	.quad	_Divide__uDivide_param_C_i8
	.quad	_L_1+3668
	.quad	_Divide__Divide_var_C_i8
	.quad	_L_1+3687
	.quad	_Divide__uDivide_var_C_i8
	.quad	_L_1+3703
	.quad	_Divide__Divide_param_C_L
	.quad	_L_1+3720
	.quad	_Divide__uDivide_param_C_L
	.quad	_L_1+3737
	.quad	_Divide__Divide_var_C_L
	.quad	_L_1+3755
	.quad	_Divide__uDivide_var_C_L
	.quad	_L_1+3770
	.quad	_Divide__Divide_param_C_i64
	.quad	_L_1+3786
	.quad	_Divide__uDivide_param_C_i64
	.quad	_L_1+3805
	.quad	_Divide__Divide_var_C_i64
	.quad	_L_1+3825
	.quad	_Divide__uDivide_var_C_i64
	.quad	_L_1+3842
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
	.quad	_Divide__Divide_param_C_i32
	.quad	_L_1+3996
	.quad	_Divide__uDivide_param_C_i32
	.quad	_L_1+4015
	.quad	_Divide__Divide_var_C_i32
	.quad	_L_1+4035
	.quad	_Divide__uDivide_var_C_i32
	.quad	_L_1+4052
	.quad	_Divide__Divide_param_C_u64
	.quad	_L_1+4070
	.quad	_Divide__uDivide_param_C_u64
	.quad	_L_1+4089
	.quad	_Divide__Divide_var_C_u64
	.quad	_L_1+4109
	.quad	_Divide__uDivide_var_C_u64
	.quad	_L_1+4126
	.quad	_Divide__Divide_param_C_i16
	.quad	_L_1+4144
	.quad	_Divide__uDivide_param_C_i16
	.quad	_L_1+4163
	.quad	_Divide__Divide_var_C_i16
	.quad	_L_1+4183
	.quad	_Divide__uDivide_var_C_i16
	.quad	_L_1+4200
	.quad	_Divide__Divide_param_C_I
	.quad	_L_1+4218
	.quad	_Divide__uDivide_param_C_I
	.quad	_L_1+4235
	.quad	_Divide__Divide_var_C_I
	.quad	_L_1+4253
	.quad	_Divide__uDivide_var_C_I
	.quad	_L_1+4268
	.quad	_Divide__Divide_param_C_LC
	.quad	_L_1+4284
	.quad	_Divide__uDivide_param_C_LC
	.quad	_L_1+4302
	.quad	_Divide__Divide_var_C_LC
	.quad	_L_1+4321
	.quad	_Divide__uDivide_var_C_LC
	.quad	_L_1+4337
	.quad	_Divide__Divide_param_C_u32
	.quad	_L_1+4354
	.quad	_Divide__uDivide_param_C_u32
	.quad	_L_1+4373
	.quad	_Divide__Divide_var_C_u32
	.quad	_L_1+4393
	.quad	_Divide__uDivide_var_C_u32
	.quad	_L_1+4410
	.quad	_Divide__Divide_param_C_u16
	.quad	_L_1+4428
	.quad	_Divide__uDivide_param_C_u16
	.quad	_L_1+4447
	.quad	_Divide__Divide_var_C_u16
	.quad	_L_1+4467
	.quad	_Divide__uDivide_var_C_u16
	.quad	_L_1+4484
	.quad	_Divide__Divide_param_i32_i8
	.quad	_L_1+4502
	.quad	_Divide__uDivide_param_i32_i8
	.quad	_L_1+4522
	.quad	_Divide__Divide_var_i32_i8
	.quad	_L_1+4543
	.quad	_Divide__uDivide_var_i32_i8
	.quad	_L_1+4561
	.quad	_Divide__Divide_param_i32_L
	.quad	_L_1+4580
	.quad	_Divide__uDivide_param_i32_L
	.quad	_L_1+4599
	.quad	_Divide__Divide_var_i32_L
	.quad	_L_1+4619
	.quad	_Divide__uDivide_var_i32_L
	.quad	_L_1+4636
	.quad	_Divide__Divide_param_i32_i64
	.quad	_L_1+4654
	.quad	_Divide__uDivide_param_i32_i64
	.quad	_L_1+4675
	.quad	_Divide__Divide_var_i32_i64
	.quad	_L_1+4697
	.quad	_Divide__uDivide_var_i32_i64
	.quad	_L_1+4716
	.quad	_Divide__Divide_param_i32_u8
	.quad	_L_1+4736
	.quad	_Divide__uDivide_param_i32_u8
	.quad	_L_1+4756
	.quad	_Divide__Divide_var_i32_u8
	.quad	_L_1+4777
	.quad	_Divide__uDivide_var_i32_u8
	.quad	_L_1+4795
	.quad	_Divide__Divide_param_i32_C
	.quad	_L_1+4814
	.quad	_Divide__uDivide_param_i32_C
	.quad	_L_1+4833
	.quad	_Divide__Divide_var_i32_C
	.quad	_L_1+4853
	.quad	_Divide__uDivide_var_i32_C
	.quad	_L_1+4870
	.quad	_Divide__Divide_param_i32_i32
	.quad	_L_1+4888
	.quad	_Divide__uDivide_param_i32_i32
	.quad	_L_1+4909
	.quad	_Divide__Divide_var_i32_i32
	.quad	_L_1+4931
	.quad	_Divide__uDivide_var_i32_i32
	.quad	_L_1+4950
	.quad	_Divide__Divide_param_i32_u64
	.quad	_L_1+4970
	.quad	_Divide__uDivide_param_i32_u64
	.quad	_L_1+4991
	.quad	_Divide__Divide_var_i32_u64
	.quad	_L_1+5013
	.quad	_Divide__uDivide_var_i32_u64
	.quad	_L_1+5032
	.quad	_Divide__Divide_param_i32_i16
	.quad	_L_1+5052
	.quad	_Divide__uDivide_param_i32_i16
	.quad	_L_1+5073
	.quad	_Divide__Divide_var_i32_i16
	.quad	_L_1+5095
	.quad	_Divide__uDivide_var_i32_i16
	.quad	_L_1+5114
	.quad	_Divide__Divide_param_i32_I
	.quad	_L_1+5134
	.quad	_Divide__uDivide_param_i32_I
	.quad	_L_1+5153
	.quad	_Divide__Divide_var_i32_I
	.quad	_L_1+5173
	.quad	_Divide__uDivide_var_i32_I
	.quad	_L_1+5190
	.quad	_Divide__Divide_param_i32_LC
	.quad	_L_1+5208
	.quad	_Divide__uDivide_param_i32_LC
	.quad	_L_1+5228
	.quad	_Divide__Divide_var_i32_LC
	.quad	_L_1+5249
	.quad	_Divide__uDivide_var_i32_LC
	.quad	_L_1+5267
	.quad	_Divide__Divide_param_i32_u32
	.quad	_L_1+5286
	.quad	_Divide__uDivide_param_i32_u32
	.quad	_L_1+5307
	.quad	_Divide__Divide_var_i32_u32
	.quad	_L_1+5329
	.quad	_Divide__uDivide_var_i32_u32
	.quad	_L_1+5348
	.quad	_Divide__Divide_param_i32_u16
	.quad	_L_1+5368
	.quad	_Divide__uDivide_param_i32_u16
	.quad	_L_1+5389
	.quad	_Divide__Divide_var_i32_u16
	.quad	_L_1+5411
	.quad	_Divide__uDivide_var_i32_u16
	.quad	_L_1+5430
	.quad	_Divide__Divide_param_f64_f64
	.quad	_L_1+5450
	.quad	_Divide__Divide_var_f64_f64
	.quad	_L_1+5471
	.quad	_Divide__Divide_param_u64_i8
	.quad	_L_1+5490
	.quad	_Divide__uDivide_param_u64_i8
	.quad	_L_1+5510
	.quad	_Divide__Divide_var_u64_i8
	.quad	_L_1+5531
	.quad	_Divide__uDivide_var_u64_i8
	.quad	_L_1+5549
	.quad	_Divide__Divide_param_u64_L
	.quad	_L_1+5568
	.quad	_Divide__uDivide_param_u64_L
	.quad	_L_1+5587
	.quad	_Divide__Divide_var_u64_L
	.quad	_L_1+5607
	.quad	_Divide__uDivide_var_u64_L
	.quad	_L_1+5624
	.quad	_Divide__Divide_param_u64_i64
	.quad	_L_1+5642
	.quad	_Divide__uDivide_param_u64_i64
	.quad	_L_1+5663
	.quad	_Divide__Divide_var_u64_i64
	.quad	_L_1+5685
	.quad	_Divide__uDivide_var_u64_i64
	.quad	_L_1+5704
	.quad	_Divide__Divide_param_u64_u8
	.quad	_L_1+5724
	.quad	_Divide__uDivide_param_u64_u8
	.quad	_L_1+5744
	.quad	_Divide__Divide_var_u64_u8
	.quad	_L_1+5765
	.quad	_Divide__uDivide_var_u64_u8
	.quad	_L_1+5783
	.quad	_Divide__Divide_param_u64_C
	.quad	_L_1+5802
	.quad	_Divide__uDivide_param_u64_C
	.quad	_L_1+5821
	.quad	_Divide__Divide_var_u64_C
	.quad	_L_1+5841
	.quad	_Divide__uDivide_var_u64_C
	.quad	_L_1+5858
	.quad	_Divide__Divide_param_u64_i32
	.quad	_L_1+5876
	.quad	_Divide__uDivide_param_u64_i32
	.quad	_L_1+5897
	.quad	_Divide__Divide_var_u64_i32
	.quad	_L_1+5919
	.quad	_Divide__uDivide_var_u64_i32
	.quad	_L_1+5938
	.quad	_Divide__Divide_param_u64_u64
	.quad	_L_1+5958
	.quad	_Divide__uDivide_param_u64_u64
	.quad	_L_1+5979
	.quad	_Divide__Divide_var_u64_u64
	.quad	_L_1+6001
	.quad	_Divide__uDivide_var_u64_u64
	.quad	_L_1+6020
	.quad	_Divide__Divide_param_u64_i16
	.quad	_L_1+6040
	.quad	_Divide__uDivide_param_u64_i16
	.quad	_L_1+6061
	.quad	_Divide__Divide_var_u64_i16
	.quad	_L_1+6083
	.quad	_Divide__uDivide_var_u64_i16
	.quad	_L_1+6102
	.quad	_Divide__Divide_param_u64_I
	.quad	_L_1+6122
	.quad	_Divide__uDivide_param_u64_I
	.quad	_L_1+6141
	.quad	_Divide__Divide_var_u64_I
	.quad	_L_1+6161
	.quad	_Divide__uDivide_var_u64_I
	.quad	_L_1+6178
	.quad	_Divide__Divide_param_u64_LC
	.quad	_L_1+6196
	.quad	_Divide__uDivide_param_u64_LC
	.quad	_L_1+6216
	.quad	_Divide__Divide_var_u64_LC
	.quad	_L_1+6237
	.quad	_Divide__uDivide_var_u64_LC
	.quad	_L_1+6255
	.quad	_Divide__Divide_param_u64_u32
	.quad	_L_1+6274
	.quad	_Divide__uDivide_param_u64_u32
	.quad	_L_1+6295
	.quad	_Divide__Divide_var_u64_u32
	.quad	_L_1+6317
	.quad	_Divide__uDivide_var_u64_u32
	.quad	_L_1+6336
	.quad	_Divide__Divide_param_u64_u16
	.quad	_L_1+6356
	.quad	_Divide__uDivide_param_u64_u16
	.quad	_L_1+6377
	.quad	_Divide__Divide_var_u64_u16
	.quad	_L_1+6399
	.quad	_Divide__uDivide_var_u64_u16
	.quad	_L_1+6418
	.quad	_Divide__Divide_param_i16_i8
	.quad	_L_1+6438
	.quad	_Divide__uDivide_param_i16_i8
	.quad	_L_1+6458
	.quad	_Divide__Divide_var_i16_i8
	.quad	_L_1+6479
	.quad	_Divide__uDivide_var_i16_i8
	.quad	_L_1+6497
	.quad	_Divide__Divide_param_i16_L
	.quad	_L_1+6516
	.quad	_Divide__uDivide_param_i16_L
	.quad	_L_1+6535
	.quad	_Divide__Divide_var_i16_L
	.quad	_L_1+6555
	.quad	_Divide__uDivide_var_i16_L
	.quad	_L_1+6572
	.quad	_Divide__Divide_param_i16_i64
	.quad	_L_1+6590
	.quad	_Divide__uDivide_param_i16_i64
	.quad	_L_1+6611
	.quad	_Divide__Divide_var_i16_i64
	.quad	_L_1+6633
	.quad	_Divide__uDivide_var_i16_i64
	.quad	_L_1+6652
	.quad	_Divide__Divide_param_i16_u8
	.quad	_L_1+6672
	.quad	_Divide__uDivide_param_i16_u8
	.quad	_L_1+6692
	.quad	_Divide__Divide_var_i16_u8
	.quad	_L_1+6713
	.quad	_Divide__uDivide_var_i16_u8
	.quad	_L_1+6731
	.quad	_Divide__Divide_param_i16_C
	.quad	_L_1+6750
	.quad	_Divide__uDivide_param_i16_C
	.quad	_L_1+6769
	.quad	_Divide__Divide_var_i16_C
	.quad	_L_1+6789
	.quad	_Divide__uDivide_var_i16_C
	.quad	_L_1+6806
	.quad	_Divide__Divide_param_i16_i32
	.quad	_L_1+6824
	.quad	_Divide__uDivide_param_i16_i32
	.quad	_L_1+6845
	.quad	_Divide__Divide_var_i16_i32
	.quad	_L_1+6867
	.quad	_Divide__uDivide_var_i16_i32
	.quad	_L_1+6886
	.quad	_Divide__Divide_param_i16_u64
	.quad	_L_1+6906
	.quad	_Divide__uDivide_param_i16_u64
	.quad	_L_1+6927
	.quad	_Divide__Divide_var_i16_u64
	.quad	_L_1+6949
	.quad	_Divide__uDivide_var_i16_u64
	.quad	_L_1+6968
	.quad	_Divide__Divide_param_i16_i16
	.quad	_L_1+6988
	.quad	_Divide__uDivide_param_i16_i16
	.quad	_L_1+7009
	.quad	_Divide__Divide_var_i16_i16
	.quad	_L_1+7031
	.quad	_Divide__uDivide_var_i16_i16
	.quad	_L_1+7050
	.quad	_Divide__Divide_param_i16_I
	.quad	_L_1+7070
	.quad	_Divide__uDivide_param_i16_I
	.quad	_L_1+7089
	.quad	_Divide__Divide_var_i16_I
	.quad	_L_1+7109
	.quad	_Divide__uDivide_var_i16_I
	.quad	_L_1+7126
	.quad	_Divide__Divide_param_i16_LC
	.quad	_L_1+7144
	.quad	_Divide__uDivide_param_i16_LC
	.quad	_L_1+7164
	.quad	_Divide__Divide_var_i16_LC
	.quad	_L_1+7185
	.quad	_Divide__uDivide_var_i16_LC
	.quad	_L_1+7203
	.quad	_Divide__Divide_param_i16_u32
	.quad	_L_1+7222
	.quad	_Divide__uDivide_param_i16_u32
	.quad	_L_1+7243
	.quad	_Divide__Divide_var_i16_u32
	.quad	_L_1+7265
	.quad	_Divide__uDivide_var_i16_u32
	.quad	_L_1+7284
	.quad	_Divide__Divide_param_i16_u16
	.quad	_L_1+7304
	.quad	_Divide__uDivide_param_i16_u16
	.quad	_L_1+7325
	.quad	_Divide__Divide_var_i16_u16
	.quad	_L_1+7347
	.quad	_Divide__uDivide_var_i16_u16
	.quad	_L_1+7366
	.quad	_Divide__Divide_param_I_i8
	.quad	_L_1+7386
	.quad	_Divide__uDivide_param_I_i8
	.quad	_L_1+7404
	.quad	_Divide__Divide_var_I_i8
	.quad	_L_1+7423
	.quad	_Divide__uDivide_var_I_i8
	.quad	_L_1+7439
	.quad	_Divide__Divide_param_I_L
	.quad	_L_1+7456
	.quad	_Divide__uDivide_param_I_L
	.quad	_L_1+7473
	.quad	_Divide__Divide_var_I_L
	.quad	_L_1+7491
	.quad	_Divide__uDivide_var_I_L
	.quad	_L_1+7506
	.quad	_Divide__Divide_param_I_i64
	.quad	_L_1+7522
	.quad	_Divide__uDivide_param_I_i64
	.quad	_L_1+7541
	.quad	_Divide__Divide_var_I_i64
	.quad	_L_1+7561
	.quad	_Divide__uDivide_var_I_i64
	.quad	_L_1+7578
	.quad	_Divide__Divide_param_I_u8
	.quad	_L_1+7596
	.quad	_Divide__uDivide_param_I_u8
	.quad	_L_1+7614
	.quad	_Divide__Divide_var_I_u8
	.quad	_L_1+7633
	.quad	_Divide__uDivide_var_I_u8
	.quad	_L_1+7649
	.quad	_Divide__Divide_param_I_C
	.quad	_L_1+7666
	.quad	_Divide__uDivide_param_I_C
	.quad	_L_1+7683
	.quad	_Divide__Divide_var_I_C
	.quad	_L_1+7701
	.quad	_Divide__uDivide_var_I_C
	.quad	_L_1+7716
	.quad	_Divide__Divide_param_I_i32
	.quad	_L_1+7732
	.quad	_Divide__uDivide_param_I_i32
	.quad	_L_1+7751
	.quad	_Divide__Divide_var_I_i32
	.quad	_L_1+7771
	.quad	_Divide__uDivide_var_I_i32
	.quad	_L_1+7788
	.quad	_Divide__Divide_param_I_u64
	.quad	_L_1+7806
	.quad	_Divide__uDivide_param_I_u64
	.quad	_L_1+7825
	.quad	_Divide__Divide_var_I_u64
	.quad	_L_1+7845
	.quad	_Divide__uDivide_var_I_u64
	.quad	_L_1+7862
	.quad	_Divide__Divide_param_I_i16
	.quad	_L_1+7880
	.quad	_Divide__uDivide_param_I_i16
	.quad	_L_1+7899
	.quad	_Divide__Divide_var_I_i16
	.quad	_L_1+7919
	.quad	_Divide__uDivide_var_I_i16
	.quad	_L_1+7936
	.quad	_Divide__Divide_param_I_I
	.quad	_L_1+7954
	.quad	_Divide__uDivide_param_I_I
	.quad	_L_1+7971
	.quad	_Divide__Divide_var_I_I
	.quad	_L_1+7989
	.quad	_Divide__uDivide_var_I_I
	.quad	_L_1+8004
	.quad	_Divide__Divide_param_I_LC
	.quad	_L_1+8020
	.quad	_Divide__uDivide_param_I_LC
	.quad	_L_1+8038
	.quad	_Divide__Divide_var_I_LC
	.quad	_L_1+8057
	.quad	_Divide__uDivide_var_I_LC
	.quad	_L_1+8073
	.quad	_Divide__Divide_param_I_u32
	.quad	_L_1+8090
	.quad	_Divide__uDivide_param_I_u32
	.quad	_L_1+8109
	.quad	_Divide__Divide_var_I_u32
	.quad	_L_1+8129
	.quad	_Divide__uDivide_var_I_u32
	.quad	_L_1+8146
	.quad	_Divide__Divide_param_I_u16
	.quad	_L_1+8164
	.quad	_Divide__uDivide_param_I_u16
	.quad	_L_1+8183
	.quad	_Divide__Divide_var_I_u16
	.quad	_L_1+8203
	.quad	_Divide__uDivide_var_I_u16
	.quad	_L_1+8220
	.quad	_Divide__Divide_param_LC_i8
	.quad	_L_1+8238
	.quad	_Divide__uDivide_param_LC_i8
	.quad	_L_1+8257
	.quad	_Divide__Divide_var_LC_i8
	.quad	_L_1+8277
	.quad	_Divide__uDivide_var_LC_i8
	.quad	_L_1+8294
	.quad	_Divide__Divide_param_LC_L
	.quad	_L_1+8312
	.quad	_Divide__uDivide_param_LC_L
	.quad	_L_1+8330
	.quad	_Divide__Divide_var_LC_L
	.quad	_L_1+8349
	.quad	_Divide__uDivide_var_LC_L
	.quad	_L_1+8365
	.quad	_Divide__Divide_param_LC_i64
	.quad	_L_1+8382
	.quad	_Divide__uDivide_param_LC_i64
	.quad	_L_1+8402
	.quad	_Divide__Divide_var_LC_i64
	.quad	_L_1+8423
	.quad	_Divide__uDivide_var_LC_i64
	.quad	_L_1+8441
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
	.quad	_Divide__Divide_param_LC_i32
	.quad	_L_1+8604
	.quad	_Divide__uDivide_param_LC_i32
	.quad	_L_1+8624
	.quad	_Divide__Divide_var_LC_i32
	.quad	_L_1+8645
	.quad	_Divide__uDivide_var_LC_i32
	.quad	_L_1+8663
	.quad	_Divide__Divide_param_LC_u64
	.quad	_L_1+8682
	.quad	_Divide__uDivide_param_LC_u64
	.quad	_L_1+8702
	.quad	_Divide__Divide_var_LC_u64
	.quad	_L_1+8723
	.quad	_Divide__uDivide_var_LC_u64
	.quad	_L_1+8741
	.quad	_Divide__Divide_param_LC_i16
	.quad	_L_1+8760
	.quad	_Divide__uDivide_param_LC_i16
	.quad	_L_1+8780
	.quad	_Divide__Divide_var_LC_i16
	.quad	_L_1+8801
	.quad	_Divide__uDivide_var_LC_i16
	.quad	_L_1+8819
	.quad	_Divide__Divide_param_LC_I
	.quad	_L_1+8838
	.quad	_Divide__uDivide_param_LC_I
	.quad	_L_1+8856
	.quad	_Divide__Divide_var_LC_I
	.quad	_L_1+8875
	.quad	_Divide__uDivide_var_LC_I
	.quad	_L_1+8891
	.quad	_Divide__Divide_param_LC_LC
	.quad	_L_1+8908
	.quad	_Divide__uDivide_param_LC_LC
	.quad	_L_1+8927
	.quad	_Divide__Divide_var_LC_LC
	.quad	_L_1+8947
	.quad	_Divide__uDivide_var_LC_LC
	.quad	_L_1+8964
	.quad	_Divide__Divide_param_LC_u32
	.quad	_L_1+8982
	.quad	_Divide__uDivide_param_LC_u32
	.quad	_L_1+9002
	.quad	_Divide__Divide_var_LC_u32
	.quad	_L_1+9023
	.quad	_Divide__uDivide_var_LC_u32
	.quad	_L_1+9041
	.quad	_Divide__Divide_param_LC_u16
	.quad	_L_1+9060
	.quad	_Divide__uDivide_param_LC_u16
	.quad	_L_1+9080
	.quad	_Divide__Divide_var_LC_u16
	.quad	_L_1+9101
	.quad	_Divide__uDivide_var_LC_u16
	.quad	_L_1+9119
	.quad	_Divide__Divide_param_u32_i8
	.quad	_L_1+9138
	.quad	_Divide__uDivide_param_u32_i8
	.quad	_L_1+9158
	.quad	_Divide__Divide_var_u32_i8
	.quad	_L_1+9179
	.quad	_Divide__uDivide_var_u32_i8
	.quad	_L_1+9197
	.quad	_Divide__Divide_param_u32_L
	.quad	_L_1+9216
	.quad	_Divide__uDivide_param_u32_L
	.quad	_L_1+9235
	.quad	_Divide__Divide_var_u32_L
	.quad	_L_1+9255
	.quad	_Divide__uDivide_var_u32_L
	.quad	_L_1+9272
	.quad	_Divide__Divide_param_u32_i64
	.quad	_L_1+9290
	.quad	_Divide__uDivide_param_u32_i64
	.quad	_L_1+9311
	.quad	_Divide__Divide_var_u32_i64
	.quad	_L_1+9333
	.quad	_Divide__uDivide_var_u32_i64
	.quad	_L_1+9352
	.quad	_Divide__Divide_param_u32_u8
	.quad	_L_1+9372
	.quad	_Divide__uDivide_param_u32_u8
	.quad	_L_1+9392
	.quad	_Divide__Divide_var_u32_u8
	.quad	_L_1+9413
	.quad	_Divide__uDivide_var_u32_u8
	.quad	_L_1+9431
	.quad	_Divide__Divide_param_u32_C
	.quad	_L_1+9450
	.quad	_Divide__uDivide_param_u32_C
	.quad	_L_1+9469
	.quad	_Divide__Divide_var_u32_C
	.quad	_L_1+9489
	.quad	_Divide__uDivide_var_u32_C
	.quad	_L_1+9506
	.quad	_Divide__Divide_param_u32_i32
	.quad	_L_1+9524
	.quad	_Divide__uDivide_param_u32_i32
	.quad	_L_1+9545
	.quad	_Divide__Divide_var_u32_i32
	.quad	_L_1+9567
	.quad	_Divide__uDivide_var_u32_i32
	.quad	_L_1+9586
	.quad	_Divide__Divide_param_u32_u64
	.quad	_L_1+9606
	.quad	_Divide__uDivide_param_u32_u64
	.quad	_L_1+9627
	.quad	_Divide__Divide_var_u32_u64
	.quad	_L_1+9649
	.quad	_Divide__uDivide_var_u32_u64
	.quad	_L_1+9668
	.quad	_Divide__Divide_param_u32_i16
	.quad	_L_1+9688
	.quad	_Divide__uDivide_param_u32_i16
	.quad	_L_1+9709
	.quad	_Divide__Divide_var_u32_i16
	.quad	_L_1+9731
	.quad	_Divide__uDivide_var_u32_i16
	.quad	_L_1+9750
	.quad	_Divide__Divide_param_u32_I
	.quad	_L_1+9770
	.quad	_Divide__uDivide_param_u32_I
	.quad	_L_1+9789
	.quad	_Divide__Divide_var_u32_I
	.quad	_L_1+9809
	.quad	_Divide__uDivide_var_u32_I
	.quad	_L_1+9826
	.quad	_Divide__Divide_param_u32_LC
	.quad	_L_1+9844
	.quad	_Divide__uDivide_param_u32_LC
	.quad	_L_1+9864
	.quad	_Divide__Divide_var_u32_LC
	.quad	_L_1+9885
	.quad	_Divide__uDivide_var_u32_LC
	.quad	_L_1+9903
	.quad	_Divide__Divide_param_u32_u32
	.quad	_L_1+9922
	.quad	_Divide__uDivide_param_u32_u32
	.quad	_L_1+9943
	.quad	_Divide__Divide_var_u32_u32
	.quad	_L_1+9965
	.quad	_Divide__uDivide_var_u32_u32
	.quad	_L_1+9984
	.quad	_Divide__Divide_param_u32_u16
	.quad	_L_1+10004
	.quad	_Divide__uDivide_param_u32_u16
	.quad	_L_1+10025
	.quad	_Divide__Divide_var_u32_u16
	.quad	_L_1+10047
	.quad	_Divide__uDivide_var_u32_u16
	.quad	_L_1+10066
	.quad	_Divide__Divide_param_u16_i8
	.quad	_L_1+10086
	.quad	_Divide__uDivide_param_u16_i8
	.quad	_L_1+10106
	.quad	_Divide__Divide_var_u16_i8
	.quad	_L_1+10127
	.quad	_Divide__uDivide_var_u16_i8
	.quad	_L_1+10145
	.quad	_Divide__Divide_param_u16_L
	.quad	_L_1+10164
	.quad	_Divide__uDivide_param_u16_L
	.quad	_L_1+10183
	.quad	_Divide__Divide_var_u16_L
	.quad	_L_1+10203
	.quad	_Divide__uDivide_var_u16_L
	.quad	_L_1+10220
	.quad	_Divide__Divide_param_u16_i64
	.quad	_L_1+10238
	.quad	_Divide__uDivide_param_u16_i64
	.quad	_L_1+10259
	.quad	_Divide__Divide_var_u16_i64
	.quad	_L_1+10281
	.quad	_Divide__uDivide_var_u16_i64
	.quad	_L_1+10300
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
	.quad	_Divide__Divide_param_u16_i32
	.quad	_L_1+10472
	.quad	_Divide__uDivide_param_u16_i32
	.quad	_L_1+10493
	.quad	_Divide__Divide_var_u16_i32
	.quad	_L_1+10515
	.quad	_Divide__uDivide_var_u16_i32
	.quad	_L_1+10534
	.quad	_Divide__Divide_param_u16_u64
	.quad	_L_1+10554
	.quad	_Divide__uDivide_param_u16_u64
	.quad	_L_1+10575
	.quad	_Divide__Divide_var_u16_u64
	.quad	_L_1+10597
	.quad	_Divide__uDivide_var_u16_u64
	.quad	_L_1+10616
	.quad	_Divide__Divide_param_u16_i16
	.quad	_L_1+10636
	.quad	_Divide__uDivide_param_u16_i16
	.quad	_L_1+10657
	.quad	_Divide__Divide_var_u16_i16
	.quad	_L_1+10679
	.quad	_Divide__uDivide_var_u16_i16
	.quad	_L_1+10698
	.quad	_Divide__Divide_param_u16_I
	.quad	_L_1+10718
	.quad	_Divide__uDivide_param_u16_I
	.quad	_L_1+10737
	.quad	_Divide__Divide_var_u16_I
	.quad	_L_1+10757
	.quad	_Divide__uDivide_var_u16_I
	.quad	_L_1+10774
	.quad	_Divide__Divide_param_u16_LC
	.quad	_L_1+10792
	.quad	_Divide__uDivide_param_u16_LC
	.quad	_L_1+10812
	.quad	_Divide__Divide_var_u16_LC
	.quad	_L_1+10833
	.quad	_Divide__uDivide_var_u16_LC
	.quad	_L_1+10851
	.quad	_Divide__Divide_param_u16_u32
	.quad	_L_1+10870
	.quad	_Divide__uDivide_param_u16_u32
	.quad	_L_1+10891
	.quad	_Divide__Divide_var_u16_u32
	.quad	_L_1+10913
	.quad	_Divide__uDivide_var_u16_u32
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
	.quad	_MM_Divide+224
	.space 8
	.quad	_Divide_M3
	.quad	3
	.word	241
	.space 2
	.long	242
	.quad	243
	.quad	244
	.word	245
	.space 6
	.quad	246
	.long	-1649267442
	.long	1081010159
	.long	249
	.space 4
	.quad	250
	.byte	-5
	.space 7
	.quad	252
	.long	1132282118
	.space 4
	.quad	255
	.byte	0
	.space 31
	.quad	_Divide_I3
	.quad	_MM_Divide+248
	.space 8
	.quad	_Long_I3
	.quad	_MM_Divide+272
	.space 8
	.quad	_Word_I3
	.quad	_MM_Divide+296
	.space 8
	.quad	_Cstdint_I3
	.quad	_MM_Divide+320
	.space 8
	.quad	_RTHooks_I3
	.space 8
_Divide__uDivide_var_u16_u16.eh = 0
.no_dead_strip _Divide__uDivide_var_u16_u16.eh
_Divide__uDivide_param_u16_u16.eh = 0
.no_dead_strip _Divide__uDivide_param_u16_u16.eh
_Divide__uDivide_var_u16_u32.eh = 0
.no_dead_strip _Divide__uDivide_var_u16_u32.eh
_Divide__uDivide_param_u16_u32.eh = 0
.no_dead_strip _Divide__uDivide_param_u16_u32.eh
_Divide__uDivide_var_u16_LC.eh = 0
.no_dead_strip _Divide__uDivide_var_u16_LC.eh
_Divide__Divide_var_u16_LC.eh = 0
.no_dead_strip _Divide__Divide_var_u16_LC.eh
_Divide__uDivide_param_u16_LC.eh = 0
.no_dead_strip _Divide__uDivide_param_u16_LC.eh
_Divide__Divide_param_u16_LC.eh = 0
.no_dead_strip _Divide__Divide_param_u16_LC.eh
_Divide__uDivide_var_u16_I.eh = 0
.no_dead_strip _Divide__uDivide_var_u16_I.eh
_Divide__uDivide_param_u16_I.eh = 0
.no_dead_strip _Divide__uDivide_param_u16_I.eh
_Divide__uDivide_var_u16_i16.eh = 0
.no_dead_strip _Divide__uDivide_var_u16_i16.eh
_Divide__uDivide_param_u16_i16.eh = 0
.no_dead_strip _Divide__uDivide_param_u16_i16.eh
_Divide__uDivide_var_u16_u64.eh = 0
.no_dead_strip _Divide__uDivide_var_u16_u64.eh
_Divide__Divide_var_u16_u64.eh = 0
.no_dead_strip _Divide__Divide_var_u16_u64.eh
_Divide__uDivide_param_u16_u64.eh = 0
.no_dead_strip _Divide__uDivide_param_u16_u64.eh
_Divide__Divide_param_u16_u64.eh = 0
.no_dead_strip _Divide__Divide_param_u16_u64.eh
_Divide__uDivide_var_u16_i32.eh = 0
.no_dead_strip _Divide__uDivide_var_u16_i32.eh
_Divide__uDivide_param_u16_i32.eh = 0
.no_dead_strip _Divide__uDivide_param_u16_i32.eh
_Divide__uDivide_var_u16_C.eh = 0
.no_dead_strip _Divide__uDivide_var_u16_C.eh
_Divide__uDivide_param_u16_C.eh = 0
.no_dead_strip _Divide__uDivide_param_u16_C.eh
_Divide__uDivide_var_u16_u8.eh = 0
.no_dead_strip _Divide__uDivide_var_u16_u8.eh
_Divide__uDivide_param_u16_u8.eh = 0
.no_dead_strip _Divide__uDivide_param_u16_u8.eh
_Divide__uDivide_var_u16_i64.eh = 0
.no_dead_strip _Divide__uDivide_var_u16_i64.eh
_Divide__Divide_var_u16_i64.eh = 0
.no_dead_strip _Divide__Divide_var_u16_i64.eh
_Divide__uDivide_param_u16_i64.eh = 0
.no_dead_strip _Divide__uDivide_param_u16_i64.eh
_Divide__Divide_param_u16_i64.eh = 0
.no_dead_strip _Divide__Divide_param_u16_i64.eh
_Divide__uDivide_var_u16_L.eh = 0
.no_dead_strip _Divide__uDivide_var_u16_L.eh
_Divide__Divide_var_u16_L.eh = 0
.no_dead_strip _Divide__Divide_var_u16_L.eh
_Divide__uDivide_param_u16_L.eh = 0
.no_dead_strip _Divide__uDivide_param_u16_L.eh
_Divide__Divide_param_u16_L.eh = 0
.no_dead_strip _Divide__Divide_param_u16_L.eh
_Divide__uDivide_var_u16_i8.eh = 0
.no_dead_strip _Divide__uDivide_var_u16_i8.eh
_Divide__uDivide_param_u16_i8.eh = 0
.no_dead_strip _Divide__uDivide_param_u16_i8.eh
_Divide__uDivide_var_u32_u16.eh = 0
.no_dead_strip _Divide__uDivide_var_u32_u16.eh
_Divide__uDivide_param_u32_u16.eh = 0
.no_dead_strip _Divide__uDivide_param_u32_u16.eh
_Divide__uDivide_var_u32_u32.eh = 0
.no_dead_strip _Divide__uDivide_var_u32_u32.eh
_Divide__uDivide_param_u32_u32.eh = 0
.no_dead_strip _Divide__uDivide_param_u32_u32.eh
_Divide__uDivide_var_u32_LC.eh = 0
.no_dead_strip _Divide__uDivide_var_u32_LC.eh
_Divide__Divide_var_u32_LC.eh = 0
.no_dead_strip _Divide__Divide_var_u32_LC.eh
_Divide__uDivide_param_u32_LC.eh = 0
.no_dead_strip _Divide__uDivide_param_u32_LC.eh
_Divide__Divide_param_u32_LC.eh = 0
.no_dead_strip _Divide__Divide_param_u32_LC.eh
_Divide__uDivide_var_u32_I.eh = 0
.no_dead_strip _Divide__uDivide_var_u32_I.eh
_Divide__uDivide_param_u32_I.eh = 0
.no_dead_strip _Divide__uDivide_param_u32_I.eh
_Divide__uDivide_var_u32_i16.eh = 0
.no_dead_strip _Divide__uDivide_var_u32_i16.eh
_Divide__uDivide_param_u32_i16.eh = 0
.no_dead_strip _Divide__uDivide_param_u32_i16.eh
_Divide__uDivide_var_u32_u64.eh = 0
.no_dead_strip _Divide__uDivide_var_u32_u64.eh
_Divide__Divide_var_u32_u64.eh = 0
.no_dead_strip _Divide__Divide_var_u32_u64.eh
_Divide__uDivide_param_u32_u64.eh = 0
.no_dead_strip _Divide__uDivide_param_u32_u64.eh
_Divide__Divide_param_u32_u64.eh = 0
.no_dead_strip _Divide__Divide_param_u32_u64.eh
_Divide__uDivide_var_u32_i32.eh = 0
.no_dead_strip _Divide__uDivide_var_u32_i32.eh
_Divide__uDivide_param_u32_i32.eh = 0
.no_dead_strip _Divide__uDivide_param_u32_i32.eh
_Divide__uDivide_var_u32_C.eh = 0
.no_dead_strip _Divide__uDivide_var_u32_C.eh
_Divide__uDivide_param_u32_C.eh = 0
.no_dead_strip _Divide__uDivide_param_u32_C.eh
_Divide__uDivide_var_u32_u8.eh = 0
.no_dead_strip _Divide__uDivide_var_u32_u8.eh
_Divide__uDivide_param_u32_u8.eh = 0
.no_dead_strip _Divide__uDivide_param_u32_u8.eh
_Divide__uDivide_var_u32_i64.eh = 0
.no_dead_strip _Divide__uDivide_var_u32_i64.eh
_Divide__Divide_var_u32_i64.eh = 0
.no_dead_strip _Divide__Divide_var_u32_i64.eh
_Divide__uDivide_param_u32_i64.eh = 0
.no_dead_strip _Divide__uDivide_param_u32_i64.eh
_Divide__Divide_param_u32_i64.eh = 0
.no_dead_strip _Divide__Divide_param_u32_i64.eh
_Divide__uDivide_var_u32_L.eh = 0
.no_dead_strip _Divide__uDivide_var_u32_L.eh
_Divide__Divide_var_u32_L.eh = 0
.no_dead_strip _Divide__Divide_var_u32_L.eh
_Divide__uDivide_param_u32_L.eh = 0
.no_dead_strip _Divide__uDivide_param_u32_L.eh
_Divide__Divide_param_u32_L.eh = 0
.no_dead_strip _Divide__Divide_param_u32_L.eh
_Divide__uDivide_var_u32_i8.eh = 0
.no_dead_strip _Divide__uDivide_var_u32_i8.eh
_Divide__uDivide_param_u32_i8.eh = 0
.no_dead_strip _Divide__uDivide_param_u32_i8.eh
_Divide__uDivide_var_LC_u16.eh = 0
.no_dead_strip _Divide__uDivide_var_LC_u16.eh
_Divide__Divide_var_LC_u16.eh = 0
.no_dead_strip _Divide__Divide_var_LC_u16.eh
_Divide__uDivide_param_LC_u16.eh = 0
.no_dead_strip _Divide__uDivide_param_LC_u16.eh
_Divide__Divide_param_LC_u16.eh = 0
.no_dead_strip _Divide__Divide_param_LC_u16.eh
_Divide__uDivide_var_LC_u32.eh = 0
.no_dead_strip _Divide__uDivide_var_LC_u32.eh
_Divide__Divide_var_LC_u32.eh = 0
.no_dead_strip _Divide__Divide_var_LC_u32.eh
_Divide__uDivide_param_LC_u32.eh = 0
.no_dead_strip _Divide__uDivide_param_LC_u32.eh
_Divide__Divide_param_LC_u32.eh = 0
.no_dead_strip _Divide__Divide_param_LC_u32.eh
_Divide__uDivide_var_LC_LC.eh = 0
.no_dead_strip _Divide__uDivide_var_LC_LC.eh
_Divide__Divide_var_LC_LC.eh = 0
.no_dead_strip _Divide__Divide_var_LC_LC.eh
_Divide__uDivide_param_LC_LC.eh = 0
.no_dead_strip _Divide__uDivide_param_LC_LC.eh
_Divide__Divide_param_LC_LC.eh = 0
.no_dead_strip _Divide__Divide_param_LC_LC.eh
_Divide__uDivide_var_LC_I.eh = 0
.no_dead_strip _Divide__uDivide_var_LC_I.eh
_Divide__Divide_var_LC_I.eh = 0
.no_dead_strip _Divide__Divide_var_LC_I.eh
_Divide__uDivide_param_LC_I.eh = 0
.no_dead_strip _Divide__uDivide_param_LC_I.eh
_Divide__Divide_param_LC_I.eh = 0
.no_dead_strip _Divide__Divide_param_LC_I.eh
_Divide__uDivide_var_LC_i16.eh = 0
.no_dead_strip _Divide__uDivide_var_LC_i16.eh
_Divide__Divide_var_LC_i16.eh = 0
.no_dead_strip _Divide__Divide_var_LC_i16.eh
_Divide__uDivide_param_LC_i16.eh = 0
.no_dead_strip _Divide__uDivide_param_LC_i16.eh
_Divide__Divide_param_LC_i16.eh = 0
.no_dead_strip _Divide__Divide_param_LC_i16.eh
_Divide__uDivide_var_LC_u64.eh = 0
.no_dead_strip _Divide__uDivide_var_LC_u64.eh
_Divide__Divide_var_LC_u64.eh = 0
.no_dead_strip _Divide__Divide_var_LC_u64.eh
_Divide__uDivide_param_LC_u64.eh = 0
.no_dead_strip _Divide__uDivide_param_LC_u64.eh
_Divide__Divide_param_LC_u64.eh = 0
.no_dead_strip _Divide__Divide_param_LC_u64.eh
_Divide__uDivide_var_LC_i32.eh = 0
.no_dead_strip _Divide__uDivide_var_LC_i32.eh
_Divide__Divide_var_LC_i32.eh = 0
.no_dead_strip _Divide__Divide_var_LC_i32.eh
_Divide__uDivide_param_LC_i32.eh = 0
.no_dead_strip _Divide__uDivide_param_LC_i32.eh
_Divide__Divide_param_LC_i32.eh = 0
.no_dead_strip _Divide__Divide_param_LC_i32.eh
_Divide__uDivide_var_LC_C.eh = 0
.no_dead_strip _Divide__uDivide_var_LC_C.eh
_Divide__Divide_var_LC_C.eh = 0
.no_dead_strip _Divide__Divide_var_LC_C.eh
_Divide__uDivide_param_LC_C.eh = 0
.no_dead_strip _Divide__uDivide_param_LC_C.eh
_Divide__Divide_param_LC_C.eh = 0
.no_dead_strip _Divide__Divide_param_LC_C.eh
_Divide__uDivide_var_LC_u8.eh = 0
.no_dead_strip _Divide__uDivide_var_LC_u8.eh
_Divide__Divide_var_LC_u8.eh = 0
.no_dead_strip _Divide__Divide_var_LC_u8.eh
_Divide__uDivide_param_LC_u8.eh = 0
.no_dead_strip _Divide__uDivide_param_LC_u8.eh
_Divide__Divide_param_LC_u8.eh = 0
.no_dead_strip _Divide__Divide_param_LC_u8.eh
_Divide__uDivide_var_LC_i64.eh = 0
.no_dead_strip _Divide__uDivide_var_LC_i64.eh
_Divide__Divide_var_LC_i64.eh = 0
.no_dead_strip _Divide__Divide_var_LC_i64.eh
_Divide__uDivide_param_LC_i64.eh = 0
.no_dead_strip _Divide__uDivide_param_LC_i64.eh
_Divide__Divide_param_LC_i64.eh = 0
.no_dead_strip _Divide__Divide_param_LC_i64.eh
_Divide__uDivide_var_LC_L.eh = 0
.no_dead_strip _Divide__uDivide_var_LC_L.eh
_Divide__Divide_var_LC_L.eh = 0
.no_dead_strip _Divide__Divide_var_LC_L.eh
_Divide__uDivide_param_LC_L.eh = 0
.no_dead_strip _Divide__uDivide_param_LC_L.eh
_Divide__Divide_param_LC_L.eh = 0
.no_dead_strip _Divide__Divide_param_LC_L.eh
_Divide__uDivide_var_LC_i8.eh = 0
.no_dead_strip _Divide__uDivide_var_LC_i8.eh
_Divide__Divide_var_LC_i8.eh = 0
.no_dead_strip _Divide__Divide_var_LC_i8.eh
_Divide__uDivide_param_LC_i8.eh = 0
.no_dead_strip _Divide__uDivide_param_LC_i8.eh
_Divide__Divide_param_LC_i8.eh = 0
.no_dead_strip _Divide__Divide_param_LC_i8.eh
_Divide__uDivide_var_I_u16.eh = 0
.no_dead_strip _Divide__uDivide_var_I_u16.eh
_Divide__Divide_var_I_u16.eh = 0
.no_dead_strip _Divide__Divide_var_I_u16.eh
_Divide__uDivide_param_I_u16.eh = 0
.no_dead_strip _Divide__uDivide_param_I_u16.eh
_Divide__Divide_param_I_u16.eh = 0
.no_dead_strip _Divide__Divide_param_I_u16.eh
_Divide__uDivide_var_I_u32.eh = 0
.no_dead_strip _Divide__uDivide_var_I_u32.eh
_Divide__Divide_var_I_u32.eh = 0
.no_dead_strip _Divide__Divide_var_I_u32.eh
_Divide__uDivide_param_I_u32.eh = 0
.no_dead_strip _Divide__uDivide_param_I_u32.eh
_Divide__Divide_param_I_u32.eh = 0
.no_dead_strip _Divide__Divide_param_I_u32.eh
_Divide__uDivide_var_I_LC.eh = 0
.no_dead_strip _Divide__uDivide_var_I_LC.eh
_Divide__Divide_var_I_LC.eh = 0
.no_dead_strip _Divide__Divide_var_I_LC.eh
_Divide__uDivide_param_I_LC.eh = 0
.no_dead_strip _Divide__uDivide_param_I_LC.eh
_Divide__Divide_param_I_LC.eh = 0
.no_dead_strip _Divide__Divide_param_I_LC.eh
_Divide__uDivide_var_I_I.eh = 0
.no_dead_strip _Divide__uDivide_var_I_I.eh
_Divide__Divide_var_I_I.eh = 0
.no_dead_strip _Divide__Divide_var_I_I.eh
_Divide__uDivide_param_I_I.eh = 0
.no_dead_strip _Divide__uDivide_param_I_I.eh
_Divide__Divide_param_I_I.eh = 0
.no_dead_strip _Divide__Divide_param_I_I.eh
_Divide__uDivide_var_I_i16.eh = 0
.no_dead_strip _Divide__uDivide_var_I_i16.eh
_Divide__Divide_var_I_i16.eh = 0
.no_dead_strip _Divide__Divide_var_I_i16.eh
_Divide__uDivide_param_I_i16.eh = 0
.no_dead_strip _Divide__uDivide_param_I_i16.eh
_Divide__Divide_param_I_i16.eh = 0
.no_dead_strip _Divide__Divide_param_I_i16.eh
_Divide__uDivide_var_I_u64.eh = 0
.no_dead_strip _Divide__uDivide_var_I_u64.eh
_Divide__Divide_var_I_u64.eh = 0
.no_dead_strip _Divide__Divide_var_I_u64.eh
_Divide__uDivide_param_I_u64.eh = 0
.no_dead_strip _Divide__uDivide_param_I_u64.eh
_Divide__Divide_param_I_u64.eh = 0
.no_dead_strip _Divide__Divide_param_I_u64.eh
_Divide__uDivide_var_I_i32.eh = 0
.no_dead_strip _Divide__uDivide_var_I_i32.eh
_Divide__Divide_var_I_i32.eh = 0
.no_dead_strip _Divide__Divide_var_I_i32.eh
_Divide__uDivide_param_I_i32.eh = 0
.no_dead_strip _Divide__uDivide_param_I_i32.eh
_Divide__Divide_param_I_i32.eh = 0
.no_dead_strip _Divide__Divide_param_I_i32.eh
_Divide__uDivide_var_I_C.eh = 0
.no_dead_strip _Divide__uDivide_var_I_C.eh
_Divide__Divide_var_I_C.eh = 0
.no_dead_strip _Divide__Divide_var_I_C.eh
_Divide__uDivide_param_I_C.eh = 0
.no_dead_strip _Divide__uDivide_param_I_C.eh
_Divide__Divide_param_I_C.eh = 0
.no_dead_strip _Divide__Divide_param_I_C.eh
_Divide__uDivide_var_I_u8.eh = 0
.no_dead_strip _Divide__uDivide_var_I_u8.eh
_Divide__Divide_var_I_u8.eh = 0
.no_dead_strip _Divide__Divide_var_I_u8.eh
_Divide__uDivide_param_I_u8.eh = 0
.no_dead_strip _Divide__uDivide_param_I_u8.eh
_Divide__Divide_param_I_u8.eh = 0
.no_dead_strip _Divide__Divide_param_I_u8.eh
_Divide__uDivide_var_I_i64.eh = 0
.no_dead_strip _Divide__uDivide_var_I_i64.eh
_Divide__Divide_var_I_i64.eh = 0
.no_dead_strip _Divide__Divide_var_I_i64.eh
_Divide__uDivide_param_I_i64.eh = 0
.no_dead_strip _Divide__uDivide_param_I_i64.eh
_Divide__Divide_param_I_i64.eh = 0
.no_dead_strip _Divide__Divide_param_I_i64.eh
_Divide__uDivide_var_I_L.eh = 0
.no_dead_strip _Divide__uDivide_var_I_L.eh
_Divide__Divide_var_I_L.eh = 0
.no_dead_strip _Divide__Divide_var_I_L.eh
_Divide__uDivide_param_I_L.eh = 0
.no_dead_strip _Divide__uDivide_param_I_L.eh
_Divide__Divide_param_I_L.eh = 0
.no_dead_strip _Divide__Divide_param_I_L.eh
_Divide__uDivide_var_I_i8.eh = 0
.no_dead_strip _Divide__uDivide_var_I_i8.eh
_Divide__Divide_var_I_i8.eh = 0
.no_dead_strip _Divide__Divide_var_I_i8.eh
_Divide__uDivide_param_I_i8.eh = 0
.no_dead_strip _Divide__uDivide_param_I_i8.eh
_Divide__Divide_param_I_i8.eh = 0
.no_dead_strip _Divide__Divide_param_I_i8.eh
_Divide__uDivide_var_i16_u16.eh = 0
.no_dead_strip _Divide__uDivide_var_i16_u16.eh
_Divide__uDivide_param_i16_u16.eh = 0
.no_dead_strip _Divide__uDivide_param_i16_u16.eh
_Divide__uDivide_var_i16_u32.eh = 0
.no_dead_strip _Divide__uDivide_var_i16_u32.eh
_Divide__uDivide_param_i16_u32.eh = 0
.no_dead_strip _Divide__uDivide_param_i16_u32.eh
_Divide__uDivide_var_i16_LC.eh = 0
.no_dead_strip _Divide__uDivide_var_i16_LC.eh
_Divide__Divide_var_i16_LC.eh = 0
.no_dead_strip _Divide__Divide_var_i16_LC.eh
_Divide__uDivide_param_i16_LC.eh = 0
.no_dead_strip _Divide__uDivide_param_i16_LC.eh
_Divide__Divide_param_i16_LC.eh = 0
.no_dead_strip _Divide__Divide_param_i16_LC.eh
_Divide__uDivide_var_i16_I.eh = 0
.no_dead_strip _Divide__uDivide_var_i16_I.eh
_Divide__uDivide_param_i16_I.eh = 0
.no_dead_strip _Divide__uDivide_param_i16_I.eh
_Divide__uDivide_var_i16_i16.eh = 0
.no_dead_strip _Divide__uDivide_var_i16_i16.eh
_Divide__uDivide_param_i16_i16.eh = 0
.no_dead_strip _Divide__uDivide_param_i16_i16.eh
_Divide__uDivide_var_i16_u64.eh = 0
.no_dead_strip _Divide__uDivide_var_i16_u64.eh
_Divide__Divide_var_i16_u64.eh = 0
.no_dead_strip _Divide__Divide_var_i16_u64.eh
_Divide__uDivide_param_i16_u64.eh = 0
.no_dead_strip _Divide__uDivide_param_i16_u64.eh
_Divide__Divide_param_i16_u64.eh = 0
.no_dead_strip _Divide__Divide_param_i16_u64.eh
_Divide__uDivide_var_i16_i32.eh = 0
.no_dead_strip _Divide__uDivide_var_i16_i32.eh
_Divide__uDivide_param_i16_i32.eh = 0
.no_dead_strip _Divide__uDivide_param_i16_i32.eh
_Divide__uDivide_var_i16_C.eh = 0
.no_dead_strip _Divide__uDivide_var_i16_C.eh
_Divide__uDivide_param_i16_C.eh = 0
.no_dead_strip _Divide__uDivide_param_i16_C.eh
_Divide__uDivide_var_i16_u8.eh = 0
.no_dead_strip _Divide__uDivide_var_i16_u8.eh
_Divide__uDivide_param_i16_u8.eh = 0
.no_dead_strip _Divide__uDivide_param_i16_u8.eh
_Divide__uDivide_var_i16_i64.eh = 0
.no_dead_strip _Divide__uDivide_var_i16_i64.eh
_Divide__Divide_var_i16_i64.eh = 0
.no_dead_strip _Divide__Divide_var_i16_i64.eh
_Divide__uDivide_param_i16_i64.eh = 0
.no_dead_strip _Divide__uDivide_param_i16_i64.eh
_Divide__Divide_param_i16_i64.eh = 0
.no_dead_strip _Divide__Divide_param_i16_i64.eh
_Divide__uDivide_var_i16_L.eh = 0
.no_dead_strip _Divide__uDivide_var_i16_L.eh
_Divide__Divide_var_i16_L.eh = 0
.no_dead_strip _Divide__Divide_var_i16_L.eh
_Divide__uDivide_param_i16_L.eh = 0
.no_dead_strip _Divide__uDivide_param_i16_L.eh
_Divide__Divide_param_i16_L.eh = 0
.no_dead_strip _Divide__Divide_param_i16_L.eh
_Divide__uDivide_var_i16_i8.eh = 0
.no_dead_strip _Divide__uDivide_var_i16_i8.eh
_Divide__uDivide_param_i16_i8.eh = 0
.no_dead_strip _Divide__uDivide_param_i16_i8.eh
_Divide__uDivide_var_u64_u16.eh = 0
.no_dead_strip _Divide__uDivide_var_u64_u16.eh
_Divide__Divide_var_u64_u16.eh = 0
.no_dead_strip _Divide__Divide_var_u64_u16.eh
_Divide__uDivide_param_u64_u16.eh = 0
.no_dead_strip _Divide__uDivide_param_u64_u16.eh
_Divide__Divide_param_u64_u16.eh = 0
.no_dead_strip _Divide__Divide_param_u64_u16.eh
_Divide__uDivide_var_u64_u32.eh = 0
.no_dead_strip _Divide__uDivide_var_u64_u32.eh
_Divide__Divide_var_u64_u32.eh = 0
.no_dead_strip _Divide__Divide_var_u64_u32.eh
_Divide__uDivide_param_u64_u32.eh = 0
.no_dead_strip _Divide__uDivide_param_u64_u32.eh
_Divide__Divide_param_u64_u32.eh = 0
.no_dead_strip _Divide__Divide_param_u64_u32.eh
_Divide__uDivide_var_u64_LC.eh = 0
.no_dead_strip _Divide__uDivide_var_u64_LC.eh
_Divide__Divide_var_u64_LC.eh = 0
.no_dead_strip _Divide__Divide_var_u64_LC.eh
_Divide__uDivide_param_u64_LC.eh = 0
.no_dead_strip _Divide__uDivide_param_u64_LC.eh
_Divide__Divide_param_u64_LC.eh = 0
.no_dead_strip _Divide__Divide_param_u64_LC.eh
_Divide__uDivide_var_u64_I.eh = 0
.no_dead_strip _Divide__uDivide_var_u64_I.eh
_Divide__Divide_var_u64_I.eh = 0
.no_dead_strip _Divide__Divide_var_u64_I.eh
_Divide__uDivide_param_u64_I.eh = 0
.no_dead_strip _Divide__uDivide_param_u64_I.eh
_Divide__Divide_param_u64_I.eh = 0
.no_dead_strip _Divide__Divide_param_u64_I.eh
_Divide__uDivide_var_u64_i16.eh = 0
.no_dead_strip _Divide__uDivide_var_u64_i16.eh
_Divide__Divide_var_u64_i16.eh = 0
.no_dead_strip _Divide__Divide_var_u64_i16.eh
_Divide__uDivide_param_u64_i16.eh = 0
.no_dead_strip _Divide__uDivide_param_u64_i16.eh
_Divide__Divide_param_u64_i16.eh = 0
.no_dead_strip _Divide__Divide_param_u64_i16.eh
_Divide__uDivide_var_u64_u64.eh = 0
.no_dead_strip _Divide__uDivide_var_u64_u64.eh
_Divide__Divide_var_u64_u64.eh = 0
.no_dead_strip _Divide__Divide_var_u64_u64.eh
_Divide__uDivide_param_u64_u64.eh = 0
.no_dead_strip _Divide__uDivide_param_u64_u64.eh
_Divide__Divide_param_u64_u64.eh = 0
.no_dead_strip _Divide__Divide_param_u64_u64.eh
_Divide__uDivide_var_u64_i32.eh = 0
.no_dead_strip _Divide__uDivide_var_u64_i32.eh
_Divide__Divide_var_u64_i32.eh = 0
.no_dead_strip _Divide__Divide_var_u64_i32.eh
_Divide__uDivide_param_u64_i32.eh = 0
.no_dead_strip _Divide__uDivide_param_u64_i32.eh
_Divide__Divide_param_u64_i32.eh = 0
.no_dead_strip _Divide__Divide_param_u64_i32.eh
_Divide__uDivide_var_u64_C.eh = 0
.no_dead_strip _Divide__uDivide_var_u64_C.eh
_Divide__Divide_var_u64_C.eh = 0
.no_dead_strip _Divide__Divide_var_u64_C.eh
_Divide__uDivide_param_u64_C.eh = 0
.no_dead_strip _Divide__uDivide_param_u64_C.eh
_Divide__Divide_param_u64_C.eh = 0
.no_dead_strip _Divide__Divide_param_u64_C.eh
_Divide__uDivide_var_u64_u8.eh = 0
.no_dead_strip _Divide__uDivide_var_u64_u8.eh
_Divide__Divide_var_u64_u8.eh = 0
.no_dead_strip _Divide__Divide_var_u64_u8.eh
_Divide__uDivide_param_u64_u8.eh = 0
.no_dead_strip _Divide__uDivide_param_u64_u8.eh
_Divide__Divide_param_u64_u8.eh = 0
.no_dead_strip _Divide__Divide_param_u64_u8.eh
_Divide__uDivide_var_u64_i64.eh = 0
.no_dead_strip _Divide__uDivide_var_u64_i64.eh
_Divide__Divide_var_u64_i64.eh = 0
.no_dead_strip _Divide__Divide_var_u64_i64.eh
_Divide__uDivide_param_u64_i64.eh = 0
.no_dead_strip _Divide__uDivide_param_u64_i64.eh
_Divide__Divide_param_u64_i64.eh = 0
.no_dead_strip _Divide__Divide_param_u64_i64.eh
_Divide__uDivide_var_u64_L.eh = 0
.no_dead_strip _Divide__uDivide_var_u64_L.eh
_Divide__Divide_var_u64_L.eh = 0
.no_dead_strip _Divide__Divide_var_u64_L.eh
_Divide__uDivide_param_u64_L.eh = 0
.no_dead_strip _Divide__uDivide_param_u64_L.eh
_Divide__Divide_param_u64_L.eh = 0
.no_dead_strip _Divide__Divide_param_u64_L.eh
_Divide__uDivide_var_u64_i8.eh = 0
.no_dead_strip _Divide__uDivide_var_u64_i8.eh
_Divide__Divide_var_u64_i8.eh = 0
.no_dead_strip _Divide__Divide_var_u64_i8.eh
_Divide__uDivide_param_u64_i8.eh = 0
.no_dead_strip _Divide__uDivide_param_u64_i8.eh
_Divide__Divide_param_u64_i8.eh = 0
.no_dead_strip _Divide__Divide_param_u64_i8.eh
_Divide__Divide_var_f64_f64.eh = 0
.no_dead_strip _Divide__Divide_var_f64_f64.eh
_Divide__Divide_param_f64_f64.eh = 0
.no_dead_strip _Divide__Divide_param_f64_f64.eh
_Divide__uDivide_var_i32_u16.eh = 0
.no_dead_strip _Divide__uDivide_var_i32_u16.eh
_Divide__uDivide_param_i32_u16.eh = 0
.no_dead_strip _Divide__uDivide_param_i32_u16.eh
_Divide__uDivide_var_i32_u32.eh = 0
.no_dead_strip _Divide__uDivide_var_i32_u32.eh
_Divide__uDivide_param_i32_u32.eh = 0
.no_dead_strip _Divide__uDivide_param_i32_u32.eh
_Divide__uDivide_var_i32_LC.eh = 0
.no_dead_strip _Divide__uDivide_var_i32_LC.eh
_Divide__Divide_var_i32_LC.eh = 0
.no_dead_strip _Divide__Divide_var_i32_LC.eh
_Divide__uDivide_param_i32_LC.eh = 0
.no_dead_strip _Divide__uDivide_param_i32_LC.eh
_Divide__Divide_param_i32_LC.eh = 0
.no_dead_strip _Divide__Divide_param_i32_LC.eh
_Divide__uDivide_var_i32_I.eh = 0
.no_dead_strip _Divide__uDivide_var_i32_I.eh
_Divide__uDivide_param_i32_I.eh = 0
.no_dead_strip _Divide__uDivide_param_i32_I.eh
_Divide__uDivide_var_i32_i16.eh = 0
.no_dead_strip _Divide__uDivide_var_i32_i16.eh
_Divide__uDivide_param_i32_i16.eh = 0
.no_dead_strip _Divide__uDivide_param_i32_i16.eh
_Divide__uDivide_var_i32_u64.eh = 0
.no_dead_strip _Divide__uDivide_var_i32_u64.eh
_Divide__Divide_var_i32_u64.eh = 0
.no_dead_strip _Divide__Divide_var_i32_u64.eh
_Divide__uDivide_param_i32_u64.eh = 0
.no_dead_strip _Divide__uDivide_param_i32_u64.eh
_Divide__Divide_param_i32_u64.eh = 0
.no_dead_strip _Divide__Divide_param_i32_u64.eh
_Divide__uDivide_var_i32_i32.eh = 0
.no_dead_strip _Divide__uDivide_var_i32_i32.eh
_Divide__uDivide_param_i32_i32.eh = 0
.no_dead_strip _Divide__uDivide_param_i32_i32.eh
_Divide__uDivide_var_i32_C.eh = 0
.no_dead_strip _Divide__uDivide_var_i32_C.eh
_Divide__uDivide_param_i32_C.eh = 0
.no_dead_strip _Divide__uDivide_param_i32_C.eh
_Divide__uDivide_var_i32_u8.eh = 0
.no_dead_strip _Divide__uDivide_var_i32_u8.eh
_Divide__uDivide_param_i32_u8.eh = 0
.no_dead_strip _Divide__uDivide_param_i32_u8.eh
_Divide__uDivide_var_i32_i64.eh = 0
.no_dead_strip _Divide__uDivide_var_i32_i64.eh
_Divide__Divide_var_i32_i64.eh = 0
.no_dead_strip _Divide__Divide_var_i32_i64.eh
_Divide__uDivide_param_i32_i64.eh = 0
.no_dead_strip _Divide__uDivide_param_i32_i64.eh
_Divide__Divide_param_i32_i64.eh = 0
.no_dead_strip _Divide__Divide_param_i32_i64.eh
_Divide__uDivide_var_i32_L.eh = 0
.no_dead_strip _Divide__uDivide_var_i32_L.eh
_Divide__Divide_var_i32_L.eh = 0
.no_dead_strip _Divide__Divide_var_i32_L.eh
_Divide__uDivide_param_i32_L.eh = 0
.no_dead_strip _Divide__uDivide_param_i32_L.eh
_Divide__Divide_param_i32_L.eh = 0
.no_dead_strip _Divide__Divide_param_i32_L.eh
_Divide__uDivide_var_i32_i8.eh = 0
.no_dead_strip _Divide__uDivide_var_i32_i8.eh
_Divide__uDivide_param_i32_i8.eh = 0
.no_dead_strip _Divide__uDivide_param_i32_i8.eh
_Divide__uDivide_var_C_u16.eh = 0
.no_dead_strip _Divide__uDivide_var_C_u16.eh
_Divide__uDivide_param_C_u16.eh = 0
.no_dead_strip _Divide__uDivide_param_C_u16.eh
_Divide__uDivide_var_C_u32.eh = 0
.no_dead_strip _Divide__uDivide_var_C_u32.eh
_Divide__uDivide_param_C_u32.eh = 0
.no_dead_strip _Divide__uDivide_param_C_u32.eh
_Divide__uDivide_var_C_LC.eh = 0
.no_dead_strip _Divide__uDivide_var_C_LC.eh
_Divide__Divide_var_C_LC.eh = 0
.no_dead_strip _Divide__Divide_var_C_LC.eh
_Divide__uDivide_param_C_LC.eh = 0
.no_dead_strip _Divide__uDivide_param_C_LC.eh
_Divide__Divide_param_C_LC.eh = 0
.no_dead_strip _Divide__Divide_param_C_LC.eh
_Divide__uDivide_var_C_I.eh = 0
.no_dead_strip _Divide__uDivide_var_C_I.eh
_Divide__uDivide_param_C_I.eh = 0
.no_dead_strip _Divide__uDivide_param_C_I.eh
_Divide__uDivide_var_C_i16.eh = 0
.no_dead_strip _Divide__uDivide_var_C_i16.eh
_Divide__uDivide_param_C_i16.eh = 0
.no_dead_strip _Divide__uDivide_param_C_i16.eh
_Divide__uDivide_var_C_u64.eh = 0
.no_dead_strip _Divide__uDivide_var_C_u64.eh
_Divide__Divide_var_C_u64.eh = 0
.no_dead_strip _Divide__Divide_var_C_u64.eh
_Divide__uDivide_param_C_u64.eh = 0
.no_dead_strip _Divide__uDivide_param_C_u64.eh
_Divide__Divide_param_C_u64.eh = 0
.no_dead_strip _Divide__Divide_param_C_u64.eh
_Divide__uDivide_var_C_i32.eh = 0
.no_dead_strip _Divide__uDivide_var_C_i32.eh
_Divide__uDivide_param_C_i32.eh = 0
.no_dead_strip _Divide__uDivide_param_C_i32.eh
_Divide__uDivide_var_C_C.eh = 0
.no_dead_strip _Divide__uDivide_var_C_C.eh
_Divide__uDivide_param_C_C.eh = 0
.no_dead_strip _Divide__uDivide_param_C_C.eh
_Divide__uDivide_var_C_u8.eh = 0
.no_dead_strip _Divide__uDivide_var_C_u8.eh
_Divide__uDivide_param_C_u8.eh = 0
.no_dead_strip _Divide__uDivide_param_C_u8.eh
_Divide__uDivide_var_C_i64.eh = 0
.no_dead_strip _Divide__uDivide_var_C_i64.eh
_Divide__Divide_var_C_i64.eh = 0
.no_dead_strip _Divide__Divide_var_C_i64.eh
_Divide__uDivide_param_C_i64.eh = 0
.no_dead_strip _Divide__uDivide_param_C_i64.eh
_Divide__Divide_param_C_i64.eh = 0
.no_dead_strip _Divide__Divide_param_C_i64.eh
_Divide__uDivide_var_C_L.eh = 0
.no_dead_strip _Divide__uDivide_var_C_L.eh
_Divide__Divide_var_C_L.eh = 0
.no_dead_strip _Divide__Divide_var_C_L.eh
_Divide__uDivide_param_C_L.eh = 0
.no_dead_strip _Divide__uDivide_param_C_L.eh
_Divide__Divide_param_C_L.eh = 0
.no_dead_strip _Divide__Divide_param_C_L.eh
_Divide__uDivide_var_C_i8.eh = 0
.no_dead_strip _Divide__uDivide_var_C_i8.eh
_Divide__uDivide_param_C_i8.eh = 0
.no_dead_strip _Divide__uDivide_param_C_i8.eh
_Divide__uDivide_var_u8_u16.eh = 0
.no_dead_strip _Divide__uDivide_var_u8_u16.eh
_Divide__uDivide_param_u8_u16.eh = 0
.no_dead_strip _Divide__uDivide_param_u8_u16.eh
_Divide__uDivide_var_u8_u32.eh = 0
.no_dead_strip _Divide__uDivide_var_u8_u32.eh
_Divide__uDivide_param_u8_u32.eh = 0
.no_dead_strip _Divide__uDivide_param_u8_u32.eh
_Divide__uDivide_var_u8_LC.eh = 0
.no_dead_strip _Divide__uDivide_var_u8_LC.eh
_Divide__Divide_var_u8_LC.eh = 0
.no_dead_strip _Divide__Divide_var_u8_LC.eh
_Divide__uDivide_param_u8_LC.eh = 0
.no_dead_strip _Divide__uDivide_param_u8_LC.eh
_Divide__Divide_param_u8_LC.eh = 0
.no_dead_strip _Divide__Divide_param_u8_LC.eh
_Divide__uDivide_var_u8_I.eh = 0
.no_dead_strip _Divide__uDivide_var_u8_I.eh
_Divide__uDivide_param_u8_I.eh = 0
.no_dead_strip _Divide__uDivide_param_u8_I.eh
_Divide__uDivide_var_u8_i16.eh = 0
.no_dead_strip _Divide__uDivide_var_u8_i16.eh
_Divide__uDivide_param_u8_i16.eh = 0
.no_dead_strip _Divide__uDivide_param_u8_i16.eh
_Divide__uDivide_var_u8_u64.eh = 0
.no_dead_strip _Divide__uDivide_var_u8_u64.eh
_Divide__Divide_var_u8_u64.eh = 0
.no_dead_strip _Divide__Divide_var_u8_u64.eh
_Divide__uDivide_param_u8_u64.eh = 0
.no_dead_strip _Divide__uDivide_param_u8_u64.eh
_Divide__Divide_param_u8_u64.eh = 0
.no_dead_strip _Divide__Divide_param_u8_u64.eh
_Divide__uDivide_var_u8_i32.eh = 0
.no_dead_strip _Divide__uDivide_var_u8_i32.eh
_Divide__uDivide_param_u8_i32.eh = 0
.no_dead_strip _Divide__uDivide_param_u8_i32.eh
_Divide__uDivide_var_u8_C.eh = 0
.no_dead_strip _Divide__uDivide_var_u8_C.eh
_Divide__uDivide_param_u8_C.eh = 0
.no_dead_strip _Divide__uDivide_param_u8_C.eh
_Divide__uDivide_var_u8_u8.eh = 0
.no_dead_strip _Divide__uDivide_var_u8_u8.eh
_Divide__uDivide_param_u8_u8.eh = 0
.no_dead_strip _Divide__uDivide_param_u8_u8.eh
_Divide__uDivide_var_u8_i64.eh = 0
.no_dead_strip _Divide__uDivide_var_u8_i64.eh
_Divide__Divide_var_u8_i64.eh = 0
.no_dead_strip _Divide__Divide_var_u8_i64.eh
_Divide__uDivide_param_u8_i64.eh = 0
.no_dead_strip _Divide__uDivide_param_u8_i64.eh
_Divide__Divide_param_u8_i64.eh = 0
.no_dead_strip _Divide__Divide_param_u8_i64.eh
_Divide__uDivide_var_u8_L.eh = 0
.no_dead_strip _Divide__uDivide_var_u8_L.eh
_Divide__Divide_var_u8_L.eh = 0
.no_dead_strip _Divide__Divide_var_u8_L.eh
_Divide__uDivide_param_u8_L.eh = 0
.no_dead_strip _Divide__uDivide_param_u8_L.eh
_Divide__Divide_param_u8_L.eh = 0
.no_dead_strip _Divide__Divide_param_u8_L.eh
_Divide__uDivide_var_u8_i8.eh = 0
.no_dead_strip _Divide__uDivide_var_u8_i8.eh
_Divide__uDivide_param_u8_i8.eh = 0
.no_dead_strip _Divide__uDivide_param_u8_i8.eh
_Divide__uDivide_var_i64_u16.eh = 0
.no_dead_strip _Divide__uDivide_var_i64_u16.eh
_Divide__Divide_var_i64_u16.eh = 0
.no_dead_strip _Divide__Divide_var_i64_u16.eh
_Divide__uDivide_param_i64_u16.eh = 0
.no_dead_strip _Divide__uDivide_param_i64_u16.eh
_Divide__Divide_param_i64_u16.eh = 0
.no_dead_strip _Divide__Divide_param_i64_u16.eh
_Divide__uDivide_var_i64_u32.eh = 0
.no_dead_strip _Divide__uDivide_var_i64_u32.eh
_Divide__Divide_var_i64_u32.eh = 0
.no_dead_strip _Divide__Divide_var_i64_u32.eh
_Divide__uDivide_param_i64_u32.eh = 0
.no_dead_strip _Divide__uDivide_param_i64_u32.eh
_Divide__Divide_param_i64_u32.eh = 0
.no_dead_strip _Divide__Divide_param_i64_u32.eh
_Divide__uDivide_var_i64_LC.eh = 0
.no_dead_strip _Divide__uDivide_var_i64_LC.eh
_Divide__Divide_var_i64_LC.eh = 0
.no_dead_strip _Divide__Divide_var_i64_LC.eh
_Divide__uDivide_param_i64_LC.eh = 0
.no_dead_strip _Divide__uDivide_param_i64_LC.eh
_Divide__Divide_param_i64_LC.eh = 0
.no_dead_strip _Divide__Divide_param_i64_LC.eh
_Divide__uDivide_var_i64_I.eh = 0
.no_dead_strip _Divide__uDivide_var_i64_I.eh
_Divide__Divide_var_i64_I.eh = 0
.no_dead_strip _Divide__Divide_var_i64_I.eh
_Divide__uDivide_param_i64_I.eh = 0
.no_dead_strip _Divide__uDivide_param_i64_I.eh
_Divide__Divide_param_i64_I.eh = 0
.no_dead_strip _Divide__Divide_param_i64_I.eh
_Divide__uDivide_var_i64_i16.eh = 0
.no_dead_strip _Divide__uDivide_var_i64_i16.eh
_Divide__Divide_var_i64_i16.eh = 0
.no_dead_strip _Divide__Divide_var_i64_i16.eh
_Divide__uDivide_param_i64_i16.eh = 0
.no_dead_strip _Divide__uDivide_param_i64_i16.eh
_Divide__Divide_param_i64_i16.eh = 0
.no_dead_strip _Divide__Divide_param_i64_i16.eh
_Divide__uDivide_var_i64_u64.eh = 0
.no_dead_strip _Divide__uDivide_var_i64_u64.eh
_Divide__Divide_var_i64_u64.eh = 0
.no_dead_strip _Divide__Divide_var_i64_u64.eh
_Divide__uDivide_param_i64_u64.eh = 0
.no_dead_strip _Divide__uDivide_param_i64_u64.eh
_Divide__Divide_param_i64_u64.eh = 0
.no_dead_strip _Divide__Divide_param_i64_u64.eh
_Divide__uDivide_var_i64_i32.eh = 0
.no_dead_strip _Divide__uDivide_var_i64_i32.eh
_Divide__Divide_var_i64_i32.eh = 0
.no_dead_strip _Divide__Divide_var_i64_i32.eh
_Divide__uDivide_param_i64_i32.eh = 0
.no_dead_strip _Divide__uDivide_param_i64_i32.eh
_Divide__Divide_param_i64_i32.eh = 0
.no_dead_strip _Divide__Divide_param_i64_i32.eh
_Divide__uDivide_var_i64_C.eh = 0
.no_dead_strip _Divide__uDivide_var_i64_C.eh
_Divide__Divide_var_i64_C.eh = 0
.no_dead_strip _Divide__Divide_var_i64_C.eh
_Divide__uDivide_param_i64_C.eh = 0
.no_dead_strip _Divide__uDivide_param_i64_C.eh
_Divide__Divide_param_i64_C.eh = 0
.no_dead_strip _Divide__Divide_param_i64_C.eh
_Divide__uDivide_var_i64_u8.eh = 0
.no_dead_strip _Divide__uDivide_var_i64_u8.eh
_Divide__Divide_var_i64_u8.eh = 0
.no_dead_strip _Divide__Divide_var_i64_u8.eh
_Divide__uDivide_param_i64_u8.eh = 0
.no_dead_strip _Divide__uDivide_param_i64_u8.eh
_Divide__Divide_param_i64_u8.eh = 0
.no_dead_strip _Divide__Divide_param_i64_u8.eh
_Divide__uDivide_var_i64_i64.eh = 0
.no_dead_strip _Divide__uDivide_var_i64_i64.eh
_Divide__Divide_var_i64_i64.eh = 0
.no_dead_strip _Divide__Divide_var_i64_i64.eh
_Divide__uDivide_param_i64_i64.eh = 0
.no_dead_strip _Divide__uDivide_param_i64_i64.eh
_Divide__Divide_param_i64_i64.eh = 0
.no_dead_strip _Divide__Divide_param_i64_i64.eh
_Divide__uDivide_var_i64_L.eh = 0
.no_dead_strip _Divide__uDivide_var_i64_L.eh
_Divide__Divide_var_i64_L.eh = 0
.no_dead_strip _Divide__Divide_var_i64_L.eh
_Divide__uDivide_param_i64_L.eh = 0
.no_dead_strip _Divide__uDivide_param_i64_L.eh
_Divide__Divide_param_i64_L.eh = 0
.no_dead_strip _Divide__Divide_param_i64_L.eh
_Divide__uDivide_var_i64_i8.eh = 0
.no_dead_strip _Divide__uDivide_var_i64_i8.eh
_Divide__Divide_var_i64_i8.eh = 0
.no_dead_strip _Divide__Divide_var_i64_i8.eh
_Divide__uDivide_param_i64_i8.eh = 0
.no_dead_strip _Divide__uDivide_param_i64_i8.eh
_Divide__Divide_param_i64_i8.eh = 0
.no_dead_strip _Divide__Divide_param_i64_i8.eh
_Divide__Divide_var_f32_f32.eh = 0
.no_dead_strip _Divide__Divide_var_f32_f32.eh
_Divide__Divide_param_f32_f32.eh = 0
.no_dead_strip _Divide__Divide_param_f32_f32.eh
_Divide__uDivide_var_L_u16.eh = 0
.no_dead_strip _Divide__uDivide_var_L_u16.eh
_Divide__Divide_var_L_u16.eh = 0
.no_dead_strip _Divide__Divide_var_L_u16.eh
_Divide__uDivide_param_L_u16.eh = 0
.no_dead_strip _Divide__uDivide_param_L_u16.eh
_Divide__Divide_param_L_u16.eh = 0
.no_dead_strip _Divide__Divide_param_L_u16.eh
_Divide__uDivide_var_L_u32.eh = 0
.no_dead_strip _Divide__uDivide_var_L_u32.eh
_Divide__Divide_var_L_u32.eh = 0
.no_dead_strip _Divide__Divide_var_L_u32.eh
_Divide__uDivide_param_L_u32.eh = 0
.no_dead_strip _Divide__uDivide_param_L_u32.eh
_Divide__Divide_param_L_u32.eh = 0
.no_dead_strip _Divide__Divide_param_L_u32.eh
_Divide__uDivide_var_L_LC.eh = 0
.no_dead_strip _Divide__uDivide_var_L_LC.eh
_Divide__Divide_var_L_LC.eh = 0
.no_dead_strip _Divide__Divide_var_L_LC.eh
_Divide__uDivide_param_L_LC.eh = 0
.no_dead_strip _Divide__uDivide_param_L_LC.eh
_Divide__Divide_param_L_LC.eh = 0
.no_dead_strip _Divide__Divide_param_L_LC.eh
_Divide__uDivide_var_L_I.eh = 0
.no_dead_strip _Divide__uDivide_var_L_I.eh
_Divide__Divide_var_L_I.eh = 0
.no_dead_strip _Divide__Divide_var_L_I.eh
_Divide__uDivide_param_L_I.eh = 0
.no_dead_strip _Divide__uDivide_param_L_I.eh
_Divide__Divide_param_L_I.eh = 0
.no_dead_strip _Divide__Divide_param_L_I.eh
_Divide__uDivide_var_L_i16.eh = 0
.no_dead_strip _Divide__uDivide_var_L_i16.eh
_Divide__Divide_var_L_i16.eh = 0
.no_dead_strip _Divide__Divide_var_L_i16.eh
_Divide__uDivide_param_L_i16.eh = 0
.no_dead_strip _Divide__uDivide_param_L_i16.eh
_Divide__Divide_param_L_i16.eh = 0
.no_dead_strip _Divide__Divide_param_L_i16.eh
_Divide__uDivide_var_L_u64.eh = 0
.no_dead_strip _Divide__uDivide_var_L_u64.eh
_Divide__Divide_var_L_u64.eh = 0
.no_dead_strip _Divide__Divide_var_L_u64.eh
_Divide__uDivide_param_L_u64.eh = 0
.no_dead_strip _Divide__uDivide_param_L_u64.eh
_Divide__Divide_param_L_u64.eh = 0
.no_dead_strip _Divide__Divide_param_L_u64.eh
_Divide__uDivide_var_L_i32.eh = 0
.no_dead_strip _Divide__uDivide_var_L_i32.eh
_Divide__Divide_var_L_i32.eh = 0
.no_dead_strip _Divide__Divide_var_L_i32.eh
_Divide__uDivide_param_L_i32.eh = 0
.no_dead_strip _Divide__uDivide_param_L_i32.eh
_Divide__Divide_param_L_i32.eh = 0
.no_dead_strip _Divide__Divide_param_L_i32.eh
_Divide__uDivide_var_L_C.eh = 0
.no_dead_strip _Divide__uDivide_var_L_C.eh
_Divide__Divide_var_L_C.eh = 0
.no_dead_strip _Divide__Divide_var_L_C.eh
_Divide__uDivide_param_L_C.eh = 0
.no_dead_strip _Divide__uDivide_param_L_C.eh
_Divide__Divide_param_L_C.eh = 0
.no_dead_strip _Divide__Divide_param_L_C.eh
_Divide__uDivide_var_L_u8.eh = 0
.no_dead_strip _Divide__uDivide_var_L_u8.eh
_Divide__Divide_var_L_u8.eh = 0
.no_dead_strip _Divide__Divide_var_L_u8.eh
_Divide__uDivide_param_L_u8.eh = 0
.no_dead_strip _Divide__uDivide_param_L_u8.eh
_Divide__Divide_param_L_u8.eh = 0
.no_dead_strip _Divide__Divide_param_L_u8.eh
_Divide__uDivide_var_L_i64.eh = 0
.no_dead_strip _Divide__uDivide_var_L_i64.eh
_Divide__Divide_var_L_i64.eh = 0
.no_dead_strip _Divide__Divide_var_L_i64.eh
_Divide__uDivide_param_L_i64.eh = 0
.no_dead_strip _Divide__uDivide_param_L_i64.eh
_Divide__Divide_param_L_i64.eh = 0
.no_dead_strip _Divide__Divide_param_L_i64.eh
_Divide__uDivide_var_L_L.eh = 0
.no_dead_strip _Divide__uDivide_var_L_L.eh
_Divide__Divide_var_L_L.eh = 0
.no_dead_strip _Divide__Divide_var_L_L.eh
_Divide__uDivide_param_L_L.eh = 0
.no_dead_strip _Divide__uDivide_param_L_L.eh
_Divide__Divide_param_L_L.eh = 0
.no_dead_strip _Divide__Divide_param_L_L.eh
_Divide__uDivide_var_L_i8.eh = 0
.no_dead_strip _Divide__uDivide_var_L_i8.eh
_Divide__Divide_var_L_i8.eh = 0
.no_dead_strip _Divide__Divide_var_L_i8.eh
_Divide__uDivide_param_L_i8.eh = 0
.no_dead_strip _Divide__uDivide_param_L_i8.eh
_Divide__Divide_param_L_i8.eh = 0
.no_dead_strip _Divide__Divide_param_L_i8.eh
_Divide__uDivide_var_i8_u16.eh = 0
.no_dead_strip _Divide__uDivide_var_i8_u16.eh
_Divide__uDivide_param_i8_u16.eh = 0
.no_dead_strip _Divide__uDivide_param_i8_u16.eh
_Divide__uDivide_var_i8_u32.eh = 0
.no_dead_strip _Divide__uDivide_var_i8_u32.eh
_Divide__uDivide_param_i8_u32.eh = 0
.no_dead_strip _Divide__uDivide_param_i8_u32.eh
_Divide__uDivide_var_i8_LC.eh = 0
.no_dead_strip _Divide__uDivide_var_i8_LC.eh
_Divide__Divide_var_i8_LC.eh = 0
.no_dead_strip _Divide__Divide_var_i8_LC.eh
_Divide__uDivide_param_i8_LC.eh = 0
.no_dead_strip _Divide__uDivide_param_i8_LC.eh
_Divide__Divide_param_i8_LC.eh = 0
.no_dead_strip _Divide__Divide_param_i8_LC.eh
_Divide__uDivide_var_i8_I.eh = 0
.no_dead_strip _Divide__uDivide_var_i8_I.eh
_Divide__uDivide_param_i8_I.eh = 0
.no_dead_strip _Divide__uDivide_param_i8_I.eh
_Divide__uDivide_var_i8_i16.eh = 0
.no_dead_strip _Divide__uDivide_var_i8_i16.eh
_Divide__uDivide_param_i8_i16.eh = 0
.no_dead_strip _Divide__uDivide_param_i8_i16.eh
_Divide__uDivide_var_i8_u64.eh = 0
.no_dead_strip _Divide__uDivide_var_i8_u64.eh
_Divide__Divide_var_i8_u64.eh = 0
.no_dead_strip _Divide__Divide_var_i8_u64.eh
_Divide__uDivide_param_i8_u64.eh = 0
.no_dead_strip _Divide__uDivide_param_i8_u64.eh
_Divide__Divide_param_i8_u64.eh = 0
.no_dead_strip _Divide__Divide_param_i8_u64.eh
_Divide__uDivide_var_i8_i32.eh = 0
.no_dead_strip _Divide__uDivide_var_i8_i32.eh
_Divide__uDivide_param_i8_i32.eh = 0
.no_dead_strip _Divide__uDivide_param_i8_i32.eh
_Divide__uDivide_var_i8_C.eh = 0
.no_dead_strip _Divide__uDivide_var_i8_C.eh
_Divide__uDivide_param_i8_C.eh = 0
.no_dead_strip _Divide__uDivide_param_i8_C.eh
_Divide__uDivide_var_i8_u8.eh = 0
.no_dead_strip _Divide__uDivide_var_i8_u8.eh
_Divide__uDivide_param_i8_u8.eh = 0
.no_dead_strip _Divide__uDivide_param_i8_u8.eh
_Divide__uDivide_var_i8_i64.eh = 0
.no_dead_strip _Divide__uDivide_var_i8_i64.eh
_Divide__Divide_var_i8_i64.eh = 0
.no_dead_strip _Divide__Divide_var_i8_i64.eh
_Divide__uDivide_param_i8_i64.eh = 0
.no_dead_strip _Divide__uDivide_param_i8_i64.eh
_Divide__Divide_param_i8_i64.eh = 0
.no_dead_strip _Divide__Divide_param_i8_i64.eh
_Divide__uDivide_var_i8_L.eh = 0
.no_dead_strip _Divide__uDivide_var_i8_L.eh
_Divide__Divide_var_i8_L.eh = 0
.no_dead_strip _Divide__Divide_var_i8_L.eh
_Divide__uDivide_param_i8_L.eh = 0
.no_dead_strip _Divide__uDivide_param_i8_L.eh
_Divide__Divide_param_i8_L.eh = 0
.no_dead_strip _Divide__Divide_param_i8_L.eh
_Divide__uDivide_var_i8_i8.eh = 0
.no_dead_strip _Divide__uDivide_var_i8_i8.eh
_Divide__uDivide_param_i8_i8.eh = 0
.no_dead_strip _Divide__uDivide_param_i8_i8.eh
.globl _Divide_M3.eh
_Divide_M3.eh = 0
.no_dead_strip _Divide_M3.eh
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
_Divide__Divide_var_u16_u16.eh:
LSFDE1:
	.set L$set$1,LEFDE1-LASFDE1
	.long L$set$1
LASFDE1:
	.long	LASFDE1-EH_frame1
	.quad	LFB1-.
	.set L$set$2,LFE1-LFB1
	.quad L$set$2
	.byte	0x0
	.byte	0x4
	.set L$set$3,LCFI3-LFB1
	.long L$set$3
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$4,LCFI4-LCFI3
	.long L$set$4
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$5,LCFI6-LCFI4
	.long L$set$5
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$6,LCFI7-LCFI6
	.long L$set$6
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE1:
_Divide__Divide_param_u16_u16.eh:
LSFDE3:
	.set L$set$7,LEFDE3-LASFDE3
	.long L$set$7
LASFDE3:
	.long	LASFDE3-EH_frame1
	.quad	LFB3-.
	.set L$set$8,LFE3-LFB3
	.quad L$set$8
	.byte	0x0
	.byte	0x4
	.set L$set$9,LCFI11-LFB3
	.long L$set$9
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$10,LCFI12-LCFI11
	.long L$set$10
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$11,LCFI14-LCFI12
	.long L$set$11
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$12,LCFI15-LCFI14
	.long L$set$12
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE3:
_Divide__Divide_var_u16_u32.eh:
LSFDE5:
	.set L$set$13,LEFDE5-LASFDE5
	.long L$set$13
LASFDE5:
	.long	LASFDE5-EH_frame1
	.quad	LFB5-.
	.set L$set$14,LFE5-LFB5
	.quad L$set$14
	.byte	0x0
	.byte	0x4
	.set L$set$15,LCFI19-LFB5
	.long L$set$15
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$16,LCFI20-LCFI19
	.long L$set$16
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$17,LCFI22-LCFI20
	.long L$set$17
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$18,LCFI23-LCFI22
	.long L$set$18
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE5:
_Divide__Divide_param_u16_u32.eh:
LSFDE7:
	.set L$set$19,LEFDE7-LASFDE7
	.long L$set$19
LASFDE7:
	.long	LASFDE7-EH_frame1
	.quad	LFB7-.
	.set L$set$20,LFE7-LFB7
	.quad L$set$20
	.byte	0x0
	.byte	0x4
	.set L$set$21,LCFI27-LFB7
	.long L$set$21
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$22,LCFI28-LCFI27
	.long L$set$22
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$23,LCFI30-LCFI28
	.long L$set$23
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$24,LCFI31-LCFI30
	.long L$set$24
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE7:
_Divide__Divide_var_u16_I.eh:
LSFDE9:
	.set L$set$25,LEFDE9-LASFDE9
	.long L$set$25
LASFDE9:
	.long	LASFDE9-EH_frame1
	.quad	LFB13-.
	.set L$set$26,LFE13-LFB13
	.quad L$set$26
	.byte	0x0
	.byte	0x4
	.set L$set$27,LCFI49-LFB13
	.long L$set$27
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$28,LCFI50-LCFI49
	.long L$set$28
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$29,LCFI52-LCFI50
	.long L$set$29
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$30,LCFI53-LCFI52
	.long L$set$30
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE9:
_Divide__Divide_param_u16_I.eh:
LSFDE11:
	.set L$set$31,LEFDE11-LASFDE11
	.long L$set$31
LASFDE11:
	.long	LASFDE11-EH_frame1
	.quad	LFB15-.
	.set L$set$32,LFE15-LFB15
	.quad L$set$32
	.byte	0x0
	.byte	0x4
	.set L$set$33,LCFI57-LFB15
	.long L$set$33
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$34,LCFI58-LCFI57
	.long L$set$34
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$35,LCFI60-LCFI58
	.long L$set$35
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE11:
_Divide__Divide_var_u16_i16.eh:
LSFDE13:
	.set L$set$36,LEFDE13-LASFDE13
	.long L$set$36
LASFDE13:
	.long	LASFDE13-EH_frame1
	.quad	LFB17-.
	.set L$set$37,LFE17-LFB17
	.quad L$set$37
	.byte	0x0
	.byte	0x4
	.set L$set$38,LCFI64-LFB17
	.long L$set$38
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$39,LCFI65-LCFI64
	.long L$set$39
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$40,LCFI67-LCFI65
	.long L$set$40
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$41,LCFI68-LCFI67
	.long L$set$41
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE13:
_Divide__Divide_param_u16_i16.eh:
LSFDE15:
	.set L$set$42,LEFDE15-LASFDE15
	.long L$set$42
LASFDE15:
	.long	LASFDE15-EH_frame1
	.quad	LFB19-.
	.set L$set$43,LFE19-LFB19
	.quad L$set$43
	.byte	0x0
	.byte	0x4
	.set L$set$44,LCFI72-LFB19
	.long L$set$44
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$45,LCFI73-LCFI72
	.long L$set$45
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$46,LCFI75-LCFI73
	.long L$set$46
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$47,LCFI76-LCFI75
	.long L$set$47
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE15:
_Divide__Divide_var_u16_i32.eh:
LSFDE17:
	.set L$set$48,LEFDE17-LASFDE17
	.long L$set$48
LASFDE17:
	.long	LASFDE17-EH_frame1
	.quad	LFB25-.
	.set L$set$49,LFE25-LFB25
	.quad L$set$49
	.byte	0x0
	.byte	0x4
	.set L$set$50,LCFI93-LFB25
	.long L$set$50
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$51,LCFI94-LCFI93
	.long L$set$51
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$52,LCFI96-LCFI94
	.long L$set$52
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$53,LCFI97-LCFI96
	.long L$set$53
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE17:
_Divide__Divide_param_u16_i32.eh:
LSFDE19:
	.set L$set$54,LEFDE19-LASFDE19
	.long L$set$54
LASFDE19:
	.long	LASFDE19-EH_frame1
	.quad	LFB27-.
	.set L$set$55,LFE27-LFB27
	.quad L$set$55
	.byte	0x0
	.byte	0x4
	.set L$set$56,LCFI101-LFB27
	.long L$set$56
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$57,LCFI102-LCFI101
	.long L$set$57
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$58,LCFI104-LCFI102
	.long L$set$58
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$59,LCFI105-LCFI104
	.long L$set$59
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE19:
_Divide__Divide_var_u16_C.eh:
LSFDE21:
	.set L$set$60,LEFDE21-LASFDE21
	.long L$set$60
LASFDE21:
	.long	LASFDE21-EH_frame1
	.quad	LFB29-.
	.set L$set$61,LFE29-LFB29
	.quad L$set$61
	.byte	0x0
	.byte	0x4
	.set L$set$62,LCFI109-LFB29
	.long L$set$62
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$63,LCFI110-LCFI109
	.long L$set$63
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$64,LCFI112-LCFI110
	.long L$set$64
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$65,LCFI113-LCFI112
	.long L$set$65
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE21:
_Divide__Divide_param_u16_C.eh:
LSFDE23:
	.set L$set$66,LEFDE23-LASFDE23
	.long L$set$66
LASFDE23:
	.long	LASFDE23-EH_frame1
	.quad	LFB31-.
	.set L$set$67,LFE31-LFB31
	.quad L$set$67
	.byte	0x0
	.byte	0x4
	.set L$set$68,LCFI117-LFB31
	.long L$set$68
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$69,LCFI118-LCFI117
	.long L$set$69
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$70,LCFI120-LCFI118
	.long L$set$70
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$71,LCFI121-LCFI120
	.long L$set$71
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE23:
_Divide__Divide_var_u16_u8.eh:
LSFDE25:
	.set L$set$72,LEFDE25-LASFDE25
	.long L$set$72
LASFDE25:
	.long	LASFDE25-EH_frame1
	.quad	LFB33-.
	.set L$set$73,LFE33-LFB33
	.quad L$set$73
	.byte	0x0
	.byte	0x4
	.set L$set$74,LCFI125-LFB33
	.long L$set$74
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$75,LCFI126-LCFI125
	.long L$set$75
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$76,LCFI128-LCFI126
	.long L$set$76
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$77,LCFI129-LCFI128
	.long L$set$77
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE25:
_Divide__Divide_param_u16_u8.eh:
LSFDE27:
	.set L$set$78,LEFDE27-LASFDE27
	.long L$set$78
LASFDE27:
	.long	LASFDE27-EH_frame1
	.quad	LFB35-.
	.set L$set$79,LFE35-LFB35
	.quad L$set$79
	.byte	0x0
	.byte	0x4
	.set L$set$80,LCFI133-LFB35
	.long L$set$80
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$81,LCFI134-LCFI133
	.long L$set$81
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$82,LCFI136-LCFI134
	.long L$set$82
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$83,LCFI137-LCFI136
	.long L$set$83
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE27:
_Divide__Divide_var_u16_i8.eh:
LSFDE29:
	.set L$set$84,LEFDE29-LASFDE29
	.long L$set$84
LASFDE29:
	.long	LASFDE29-EH_frame1
	.quad	LFB45-.
	.set L$set$85,LFE45-LFB45
	.quad L$set$85
	.byte	0x0
	.byte	0x4
	.set L$set$86,LCFI167-LFB45
	.long L$set$86
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$87,LCFI168-LCFI167
	.long L$set$87
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$88,LCFI170-LCFI168
	.long L$set$88
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$89,LCFI171-LCFI170
	.long L$set$89
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE29:
_Divide__Divide_param_u16_i8.eh:
LSFDE31:
	.set L$set$90,LEFDE31-LASFDE31
	.long L$set$90
LASFDE31:
	.long	LASFDE31-EH_frame1
	.quad	LFB47-.
	.set L$set$91,LFE47-LFB47
	.quad L$set$91
	.byte	0x0
	.byte	0x4
	.set L$set$92,LCFI175-LFB47
	.long L$set$92
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$93,LCFI176-LCFI175
	.long L$set$93
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$94,LCFI178-LCFI176
	.long L$set$94
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$95,LCFI179-LCFI178
	.long L$set$95
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE31:
_Divide__Divide_var_u32_u16.eh:
LSFDE33:
	.set L$set$96,LEFDE33-LASFDE33
	.long L$set$96
LASFDE33:
	.long	LASFDE33-EH_frame1
	.quad	LFB49-.
	.set L$set$97,LFE49-LFB49
	.quad L$set$97
	.byte	0x0
	.byte	0x4
	.set L$set$98,LCFI183-LFB49
	.long L$set$98
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$99,LCFI184-LCFI183
	.long L$set$99
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$100,LCFI186-LCFI184
	.long L$set$100
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$101,LCFI187-LCFI186
	.long L$set$101
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE33:
_Divide__Divide_param_u32_u16.eh:
LSFDE35:
	.set L$set$102,LEFDE35-LASFDE35
	.long L$set$102
LASFDE35:
	.long	LASFDE35-EH_frame1
	.quad	LFB51-.
	.set L$set$103,LFE51-LFB51
	.quad L$set$103
	.byte	0x0
	.byte	0x4
	.set L$set$104,LCFI191-LFB51
	.long L$set$104
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$105,LCFI192-LCFI191
	.long L$set$105
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$106,LCFI194-LCFI192
	.long L$set$106
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$107,LCFI195-LCFI194
	.long L$set$107
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE35:
_Divide__Divide_var_u32_u32.eh:
LSFDE37:
	.set L$set$108,LEFDE37-LASFDE37
	.long L$set$108
LASFDE37:
	.long	LASFDE37-EH_frame1
	.quad	LFB53-.
	.set L$set$109,LFE53-LFB53
	.quad L$set$109
	.byte	0x0
	.byte	0x4
	.set L$set$110,LCFI199-LFB53
	.long L$set$110
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$111,LCFI200-LCFI199
	.long L$set$111
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$112,LCFI202-LCFI200
	.long L$set$112
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$113,LCFI203-LCFI202
	.long L$set$113
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE37:
_Divide__Divide_param_u32_u32.eh:
LSFDE39:
	.set L$set$114,LEFDE39-LASFDE39
	.long L$set$114
LASFDE39:
	.long	LASFDE39-EH_frame1
	.quad	LFB55-.
	.set L$set$115,LFE55-LFB55
	.quad L$set$115
	.byte	0x0
	.byte	0x4
	.set L$set$116,LCFI207-LFB55
	.long L$set$116
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$117,LCFI208-LCFI207
	.long L$set$117
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$118,LCFI210-LCFI208
	.long L$set$118
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$119,LCFI211-LCFI210
	.long L$set$119
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE39:
_Divide__Divide_var_u32_I.eh:
LSFDE41:
	.set L$set$120,LEFDE41-LASFDE41
	.long L$set$120
LASFDE41:
	.long	LASFDE41-EH_frame1
	.quad	LFB61-.
	.set L$set$121,LFE61-LFB61
	.quad L$set$121
	.byte	0x0
	.byte	0x4
	.set L$set$122,LCFI229-LFB61
	.long L$set$122
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$123,LCFI230-LCFI229
	.long L$set$123
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$124,LCFI232-LCFI230
	.long L$set$124
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$125,LCFI233-LCFI232
	.long L$set$125
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE41:
_Divide__Divide_param_u32_I.eh:
LSFDE43:
	.set L$set$126,LEFDE43-LASFDE43
	.long L$set$126
LASFDE43:
	.long	LASFDE43-EH_frame1
	.quad	LFB63-.
	.set L$set$127,LFE63-LFB63
	.quad L$set$127
	.byte	0x0
	.byte	0x4
	.set L$set$128,LCFI237-LFB63
	.long L$set$128
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$129,LCFI238-LCFI237
	.long L$set$129
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$130,LCFI240-LCFI238
	.long L$set$130
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE43:
_Divide__Divide_var_u32_i16.eh:
LSFDE45:
	.set L$set$131,LEFDE45-LASFDE45
	.long L$set$131
LASFDE45:
	.long	LASFDE45-EH_frame1
	.quad	LFB65-.
	.set L$set$132,LFE65-LFB65
	.quad L$set$132
	.byte	0x0
	.byte	0x4
	.set L$set$133,LCFI244-LFB65
	.long L$set$133
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$134,LCFI245-LCFI244
	.long L$set$134
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$135,LCFI247-LCFI245
	.long L$set$135
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$136,LCFI248-LCFI247
	.long L$set$136
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE45:
_Divide__Divide_param_u32_i16.eh:
LSFDE47:
	.set L$set$137,LEFDE47-LASFDE47
	.long L$set$137
LASFDE47:
	.long	LASFDE47-EH_frame1
	.quad	LFB67-.
	.set L$set$138,LFE67-LFB67
	.quad L$set$138
	.byte	0x0
	.byte	0x4
	.set L$set$139,LCFI252-LFB67
	.long L$set$139
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$140,LCFI253-LCFI252
	.long L$set$140
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$141,LCFI255-LCFI253
	.long L$set$141
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$142,LCFI256-LCFI255
	.long L$set$142
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE47:
_Divide__Divide_var_u32_i32.eh:
LSFDE49:
	.set L$set$143,LEFDE49-LASFDE49
	.long L$set$143
LASFDE49:
	.long	LASFDE49-EH_frame1
	.quad	LFB73-.
	.set L$set$144,LFE73-LFB73
	.quad L$set$144
	.byte	0x0
	.byte	0x4
	.set L$set$145,LCFI273-LFB73
	.long L$set$145
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$146,LCFI274-LCFI273
	.long L$set$146
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$147,LCFI276-LCFI274
	.long L$set$147
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$148,LCFI277-LCFI276
	.long L$set$148
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE49:
_Divide__Divide_param_u32_i32.eh:
LSFDE51:
	.set L$set$149,LEFDE51-LASFDE51
	.long L$set$149
LASFDE51:
	.long	LASFDE51-EH_frame1
	.quad	LFB75-.
	.set L$set$150,LFE75-LFB75
	.quad L$set$150
	.byte	0x0
	.byte	0x4
	.set L$set$151,LCFI281-LFB75
	.long L$set$151
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$152,LCFI282-LCFI281
	.long L$set$152
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$153,LCFI284-LCFI282
	.long L$set$153
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$154,LCFI285-LCFI284
	.long L$set$154
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE51:
_Divide__Divide_var_u32_C.eh:
LSFDE53:
	.set L$set$155,LEFDE53-LASFDE53
	.long L$set$155
LASFDE53:
	.long	LASFDE53-EH_frame1
	.quad	LFB77-.
	.set L$set$156,LFE77-LFB77
	.quad L$set$156
	.byte	0x0
	.byte	0x4
	.set L$set$157,LCFI289-LFB77
	.long L$set$157
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$158,LCFI290-LCFI289
	.long L$set$158
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$159,LCFI292-LCFI290
	.long L$set$159
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$160,LCFI293-LCFI292
	.long L$set$160
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE53:
_Divide__Divide_param_u32_C.eh:
LSFDE55:
	.set L$set$161,LEFDE55-LASFDE55
	.long L$set$161
LASFDE55:
	.long	LASFDE55-EH_frame1
	.quad	LFB79-.
	.set L$set$162,LFE79-LFB79
	.quad L$set$162
	.byte	0x0
	.byte	0x4
	.set L$set$163,LCFI297-LFB79
	.long L$set$163
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$164,LCFI298-LCFI297
	.long L$set$164
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$165,LCFI300-LCFI298
	.long L$set$165
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$166,LCFI301-LCFI300
	.long L$set$166
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE55:
_Divide__Divide_var_u32_u8.eh:
LSFDE57:
	.set L$set$167,LEFDE57-LASFDE57
	.long L$set$167
LASFDE57:
	.long	LASFDE57-EH_frame1
	.quad	LFB81-.
	.set L$set$168,LFE81-LFB81
	.quad L$set$168
	.byte	0x0
	.byte	0x4
	.set L$set$169,LCFI305-LFB81
	.long L$set$169
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$170,LCFI306-LCFI305
	.long L$set$170
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$171,LCFI308-LCFI306
	.long L$set$171
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$172,LCFI309-LCFI308
	.long L$set$172
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE57:
_Divide__Divide_param_u32_u8.eh:
LSFDE59:
	.set L$set$173,LEFDE59-LASFDE59
	.long L$set$173
LASFDE59:
	.long	LASFDE59-EH_frame1
	.quad	LFB83-.
	.set L$set$174,LFE83-LFB83
	.quad L$set$174
	.byte	0x0
	.byte	0x4
	.set L$set$175,LCFI313-LFB83
	.long L$set$175
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$176,LCFI314-LCFI313
	.long L$set$176
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$177,LCFI316-LCFI314
	.long L$set$177
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$178,LCFI317-LCFI316
	.long L$set$178
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE59:
_Divide__Divide_var_u32_i8.eh:
LSFDE61:
	.set L$set$179,LEFDE61-LASFDE61
	.long L$set$179
LASFDE61:
	.long	LASFDE61-EH_frame1
	.quad	LFB93-.
	.set L$set$180,LFE93-LFB93
	.quad L$set$180
	.byte	0x0
	.byte	0x4
	.set L$set$181,LCFI347-LFB93
	.long L$set$181
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$182,LCFI348-LCFI347
	.long L$set$182
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$183,LCFI350-LCFI348
	.long L$set$183
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$184,LCFI351-LCFI350
	.long L$set$184
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE61:
_Divide__Divide_param_u32_i8.eh:
LSFDE63:
	.set L$set$185,LEFDE63-LASFDE63
	.long L$set$185
LASFDE63:
	.long	LASFDE63-EH_frame1
	.quad	LFB95-.
	.set L$set$186,LFE95-LFB95
	.quad L$set$186
	.byte	0x0
	.byte	0x4
	.set L$set$187,LCFI355-LFB95
	.long L$set$187
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$188,LCFI356-LCFI355
	.long L$set$188
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$189,LCFI358-LCFI356
	.long L$set$189
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$190,LCFI359-LCFI358
	.long L$set$190
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE63:
_Divide__Divide_var_i16_u16.eh:
LSFDE65:
	.set L$set$191,LEFDE65-LASFDE65
	.long L$set$191
LASFDE65:
	.long	LASFDE65-EH_frame1
	.quad	LFB193-.
	.set L$set$192,LFE193-LFB193
	.quad L$set$192
	.byte	0x0
	.byte	0x4
	.set L$set$193,LCFI695-LFB193
	.long L$set$193
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$194,LCFI696-LCFI695
	.long L$set$194
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$195,LCFI698-LCFI696
	.long L$set$195
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$196,LCFI699-LCFI698
	.long L$set$196
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE65:
_Divide__Divide_param_i16_u16.eh:
LSFDE67:
	.set L$set$197,LEFDE67-LASFDE67
	.long L$set$197
LASFDE67:
	.long	LASFDE67-EH_frame1
	.quad	LFB195-.
	.set L$set$198,LFE195-LFB195
	.quad L$set$198
	.byte	0x0
	.byte	0x4
	.set L$set$199,LCFI703-LFB195
	.long L$set$199
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$200,LCFI704-LCFI703
	.long L$set$200
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$201,LCFI706-LCFI704
	.long L$set$201
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$202,LCFI707-LCFI706
	.long L$set$202
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE67:
_Divide__Divide_var_i16_u32.eh:
LSFDE69:
	.set L$set$203,LEFDE69-LASFDE69
	.long L$set$203
LASFDE69:
	.long	LASFDE69-EH_frame1
	.quad	LFB197-.
	.set L$set$204,LFE197-LFB197
	.quad L$set$204
	.byte	0x0
	.byte	0x4
	.set L$set$205,LCFI711-LFB197
	.long L$set$205
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$206,LCFI712-LCFI711
	.long L$set$206
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$207,LCFI714-LCFI712
	.long L$set$207
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$208,LCFI715-LCFI714
	.long L$set$208
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE69:
_Divide__Divide_param_i16_u32.eh:
LSFDE71:
	.set L$set$209,LEFDE71-LASFDE71
	.long L$set$209
LASFDE71:
	.long	LASFDE71-EH_frame1
	.quad	LFB199-.
	.set L$set$210,LFE199-LFB199
	.quad L$set$210
	.byte	0x0
	.byte	0x4
	.set L$set$211,LCFI719-LFB199
	.long L$set$211
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$212,LCFI720-LCFI719
	.long L$set$212
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$213,LCFI722-LCFI720
	.long L$set$213
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$214,LCFI723-LCFI722
	.long L$set$214
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE71:
_Divide__Divide_var_i16_I.eh:
LSFDE73:
	.set L$set$215,LEFDE73-LASFDE73
	.long L$set$215
LASFDE73:
	.long	LASFDE73-EH_frame1
	.quad	LFB205-.
	.set L$set$216,LFE205-LFB205
	.quad L$set$216
	.byte	0x0
	.byte	0x4
	.set L$set$217,LCFI741-LFB205
	.long L$set$217
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$218,LCFI742-LCFI741
	.long L$set$218
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$219,LCFI744-LCFI742
	.long L$set$219
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$220,LCFI745-LCFI744
	.long L$set$220
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE73:
_Divide__Divide_param_i16_I.eh:
LSFDE75:
	.set L$set$221,LEFDE75-LASFDE75
	.long L$set$221
LASFDE75:
	.long	LASFDE75-EH_frame1
	.quad	LFB207-.
	.set L$set$222,LFE207-LFB207
	.quad L$set$222
	.byte	0x0
	.byte	0x4
	.set L$set$223,LCFI749-LFB207
	.long L$set$223
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$224,LCFI750-LCFI749
	.long L$set$224
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$225,LCFI752-LCFI750
	.long L$set$225
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE75:
_Divide__Divide_var_i16_i16.eh:
LSFDE77:
	.set L$set$226,LEFDE77-LASFDE77
	.long L$set$226
LASFDE77:
	.long	LASFDE77-EH_frame1
	.quad	LFB209-.
	.set L$set$227,LFE209-LFB209
	.quad L$set$227
	.byte	0x0
	.byte	0x4
	.set L$set$228,LCFI756-LFB209
	.long L$set$228
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$229,LCFI757-LCFI756
	.long L$set$229
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$230,LCFI759-LCFI757
	.long L$set$230
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$231,LCFI760-LCFI759
	.long L$set$231
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE77:
_Divide__Divide_param_i16_i16.eh:
LSFDE79:
	.set L$set$232,LEFDE79-LASFDE79
	.long L$set$232
LASFDE79:
	.long	LASFDE79-EH_frame1
	.quad	LFB211-.
	.set L$set$233,LFE211-LFB211
	.quad L$set$233
	.byte	0x0
	.byte	0x4
	.set L$set$234,LCFI764-LFB211
	.long L$set$234
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$235,LCFI765-LCFI764
	.long L$set$235
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$236,LCFI767-LCFI765
	.long L$set$236
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$237,LCFI768-LCFI767
	.long L$set$237
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE79:
_Divide__Divide_var_i16_i32.eh:
LSFDE81:
	.set L$set$238,LEFDE81-LASFDE81
	.long L$set$238
LASFDE81:
	.long	LASFDE81-EH_frame1
	.quad	LFB217-.
	.set L$set$239,LFE217-LFB217
	.quad L$set$239
	.byte	0x0
	.byte	0x4
	.set L$set$240,LCFI785-LFB217
	.long L$set$240
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$241,LCFI786-LCFI785
	.long L$set$241
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$242,LCFI788-LCFI786
	.long L$set$242
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$243,LCFI789-LCFI788
	.long L$set$243
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE81:
_Divide__Divide_param_i16_i32.eh:
LSFDE83:
	.set L$set$244,LEFDE83-LASFDE83
	.long L$set$244
LASFDE83:
	.long	LASFDE83-EH_frame1
	.quad	LFB219-.
	.set L$set$245,LFE219-LFB219
	.quad L$set$245
	.byte	0x0
	.byte	0x4
	.set L$set$246,LCFI793-LFB219
	.long L$set$246
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$247,LCFI794-LCFI793
	.long L$set$247
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$248,LCFI796-LCFI794
	.long L$set$248
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$249,LCFI797-LCFI796
	.long L$set$249
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE83:
_Divide__Divide_var_i16_C.eh:
LSFDE85:
	.set L$set$250,LEFDE85-LASFDE85
	.long L$set$250
LASFDE85:
	.long	LASFDE85-EH_frame1
	.quad	LFB221-.
	.set L$set$251,LFE221-LFB221
	.quad L$set$251
	.byte	0x0
	.byte	0x4
	.set L$set$252,LCFI801-LFB221
	.long L$set$252
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$253,LCFI802-LCFI801
	.long L$set$253
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$254,LCFI804-LCFI802
	.long L$set$254
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$255,LCFI805-LCFI804
	.long L$set$255
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE85:
_Divide__Divide_param_i16_C.eh:
LSFDE87:
	.set L$set$256,LEFDE87-LASFDE87
	.long L$set$256
LASFDE87:
	.long	LASFDE87-EH_frame1
	.quad	LFB223-.
	.set L$set$257,LFE223-LFB223
	.quad L$set$257
	.byte	0x0
	.byte	0x4
	.set L$set$258,LCFI809-LFB223
	.long L$set$258
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$259,LCFI810-LCFI809
	.long L$set$259
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$260,LCFI812-LCFI810
	.long L$set$260
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$261,LCFI813-LCFI812
	.long L$set$261
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE87:
_Divide__Divide_var_i16_u8.eh:
LSFDE89:
	.set L$set$262,LEFDE89-LASFDE89
	.long L$set$262
LASFDE89:
	.long	LASFDE89-EH_frame1
	.quad	LFB225-.
	.set L$set$263,LFE225-LFB225
	.quad L$set$263
	.byte	0x0
	.byte	0x4
	.set L$set$264,LCFI817-LFB225
	.long L$set$264
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$265,LCFI818-LCFI817
	.long L$set$265
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$266,LCFI820-LCFI818
	.long L$set$266
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$267,LCFI821-LCFI820
	.long L$set$267
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE89:
_Divide__Divide_param_i16_u8.eh:
LSFDE91:
	.set L$set$268,LEFDE91-LASFDE91
	.long L$set$268
LASFDE91:
	.long	LASFDE91-EH_frame1
	.quad	LFB227-.
	.set L$set$269,LFE227-LFB227
	.quad L$set$269
	.byte	0x0
	.byte	0x4
	.set L$set$270,LCFI825-LFB227
	.long L$set$270
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$271,LCFI826-LCFI825
	.long L$set$271
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$272,LCFI828-LCFI826
	.long L$set$272
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$273,LCFI829-LCFI828
	.long L$set$273
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE91:
_Divide__Divide_var_i16_i8.eh:
LSFDE93:
	.set L$set$274,LEFDE93-LASFDE93
	.long L$set$274
LASFDE93:
	.long	LASFDE93-EH_frame1
	.quad	LFB237-.
	.set L$set$275,LFE237-LFB237
	.quad L$set$275
	.byte	0x0
	.byte	0x4
	.set L$set$276,LCFI859-LFB237
	.long L$set$276
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$277,LCFI860-LCFI859
	.long L$set$277
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$278,LCFI862-LCFI860
	.long L$set$278
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$279,LCFI863-LCFI862
	.long L$set$279
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE93:
_Divide__Divide_param_i16_i8.eh:
LSFDE95:
	.set L$set$280,LEFDE95-LASFDE95
	.long L$set$280
LASFDE95:
	.long	LASFDE95-EH_frame1
	.quad	LFB239-.
	.set L$set$281,LFE239-LFB239
	.quad L$set$281
	.byte	0x0
	.byte	0x4
	.set L$set$282,LCFI867-LFB239
	.long L$set$282
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$283,LCFI868-LCFI867
	.long L$set$283
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$284,LCFI870-LCFI868
	.long L$set$284
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$285,LCFI871-LCFI870
	.long L$set$285
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE95:
_Divide__Divide_var_i32_u16.eh:
LSFDE97:
	.set L$set$286,LEFDE97-LASFDE97
	.long L$set$286
LASFDE97:
	.long	LASFDE97-EH_frame1
	.quad	LFB291-.
	.set L$set$287,LFE291-LFB291
	.quad L$set$287
	.byte	0x0
	.byte	0x4
	.set L$set$288,LCFI1049-LFB291
	.long L$set$288
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$289,LCFI1050-LCFI1049
	.long L$set$289
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$290,LCFI1052-LCFI1050
	.long L$set$290
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$291,LCFI1053-LCFI1052
	.long L$set$291
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE97:
_Divide__Divide_param_i32_u16.eh:
LSFDE99:
	.set L$set$292,LEFDE99-LASFDE99
	.long L$set$292
LASFDE99:
	.long	LASFDE99-EH_frame1
	.quad	LFB293-.
	.set L$set$293,LFE293-LFB293
	.quad L$set$293
	.byte	0x0
	.byte	0x4
	.set L$set$294,LCFI1057-LFB293
	.long L$set$294
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$295,LCFI1058-LCFI1057
	.long L$set$295
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$296,LCFI1060-LCFI1058
	.long L$set$296
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$297,LCFI1061-LCFI1060
	.long L$set$297
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE99:
_Divide__Divide_var_i32_u32.eh:
LSFDE101:
	.set L$set$298,LEFDE101-LASFDE101
	.long L$set$298
LASFDE101:
	.long	LASFDE101-EH_frame1
	.quad	LFB295-.
	.set L$set$299,LFE295-LFB295
	.quad L$set$299
	.byte	0x0
	.byte	0x4
	.set L$set$300,LCFI1065-LFB295
	.long L$set$300
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$301,LCFI1066-LCFI1065
	.long L$set$301
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$302,LCFI1068-LCFI1066
	.long L$set$302
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$303,LCFI1069-LCFI1068
	.long L$set$303
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE101:
_Divide__Divide_param_i32_u32.eh:
LSFDE103:
	.set L$set$304,LEFDE103-LASFDE103
	.long L$set$304
LASFDE103:
	.long	LASFDE103-EH_frame1
	.quad	LFB297-.
	.set L$set$305,LFE297-LFB297
	.quad L$set$305
	.byte	0x0
	.byte	0x4
	.set L$set$306,LCFI1073-LFB297
	.long L$set$306
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$307,LCFI1074-LCFI1073
	.long L$set$307
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$308,LCFI1076-LCFI1074
	.long L$set$308
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$309,LCFI1077-LCFI1076
	.long L$set$309
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE103:
_Divide__Divide_var_i32_I.eh:
LSFDE105:
	.set L$set$310,LEFDE105-LASFDE105
	.long L$set$310
LASFDE105:
	.long	LASFDE105-EH_frame1
	.quad	LFB303-.
	.set L$set$311,LFE303-LFB303
	.quad L$set$311
	.byte	0x0
	.byte	0x4
	.set L$set$312,LCFI1095-LFB303
	.long L$set$312
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$313,LCFI1096-LCFI1095
	.long L$set$313
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$314,LCFI1098-LCFI1096
	.long L$set$314
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$315,LCFI1099-LCFI1098
	.long L$set$315
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE105:
_Divide__Divide_param_i32_I.eh:
LSFDE107:
	.set L$set$316,LEFDE107-LASFDE107
	.long L$set$316
LASFDE107:
	.long	LASFDE107-EH_frame1
	.quad	LFB305-.
	.set L$set$317,LFE305-LFB305
	.quad L$set$317
	.byte	0x0
	.byte	0x4
	.set L$set$318,LCFI1103-LFB305
	.long L$set$318
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$319,LCFI1104-LCFI1103
	.long L$set$319
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$320,LCFI1106-LCFI1104
	.long L$set$320
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE107:
_Divide__Divide_var_i32_i16.eh:
LSFDE109:
	.set L$set$321,LEFDE109-LASFDE109
	.long L$set$321
LASFDE109:
	.long	LASFDE109-EH_frame1
	.quad	LFB307-.
	.set L$set$322,LFE307-LFB307
	.quad L$set$322
	.byte	0x0
	.byte	0x4
	.set L$set$323,LCFI1110-LFB307
	.long L$set$323
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$324,LCFI1111-LCFI1110
	.long L$set$324
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$325,LCFI1113-LCFI1111
	.long L$set$325
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$326,LCFI1114-LCFI1113
	.long L$set$326
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE109:
_Divide__Divide_param_i32_i16.eh:
LSFDE111:
	.set L$set$327,LEFDE111-LASFDE111
	.long L$set$327
LASFDE111:
	.long	LASFDE111-EH_frame1
	.quad	LFB309-.
	.set L$set$328,LFE309-LFB309
	.quad L$set$328
	.byte	0x0
	.byte	0x4
	.set L$set$329,LCFI1118-LFB309
	.long L$set$329
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$330,LCFI1119-LCFI1118
	.long L$set$330
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$331,LCFI1121-LCFI1119
	.long L$set$331
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$332,LCFI1122-LCFI1121
	.long L$set$332
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE111:
_Divide__Divide_var_i32_i32.eh:
LSFDE113:
	.set L$set$333,LEFDE113-LASFDE113
	.long L$set$333
LASFDE113:
	.long	LASFDE113-EH_frame1
	.quad	LFB315-.
	.set L$set$334,LFE315-LFB315
	.quad L$set$334
	.byte	0x0
	.byte	0x4
	.set L$set$335,LCFI1139-LFB315
	.long L$set$335
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$336,LCFI1140-LCFI1139
	.long L$set$336
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$337,LCFI1142-LCFI1140
	.long L$set$337
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$338,LCFI1143-LCFI1142
	.long L$set$338
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE113:
_Divide__Divide_param_i32_i32.eh:
LSFDE115:
	.set L$set$339,LEFDE115-LASFDE115
	.long L$set$339
LASFDE115:
	.long	LASFDE115-EH_frame1
	.quad	LFB317-.
	.set L$set$340,LFE317-LFB317
	.quad L$set$340
	.byte	0x0
	.byte	0x4
	.set L$set$341,LCFI1147-LFB317
	.long L$set$341
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$342,LCFI1148-LCFI1147
	.long L$set$342
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$343,LCFI1150-LCFI1148
	.long L$set$343
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$344,LCFI1151-LCFI1150
	.long L$set$344
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE115:
_Divide__Divide_var_i32_C.eh:
LSFDE117:
	.set L$set$345,LEFDE117-LASFDE117
	.long L$set$345
LASFDE117:
	.long	LASFDE117-EH_frame1
	.quad	LFB319-.
	.set L$set$346,LFE319-LFB319
	.quad L$set$346
	.byte	0x0
	.byte	0x4
	.set L$set$347,LCFI1155-LFB319
	.long L$set$347
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$348,LCFI1156-LCFI1155
	.long L$set$348
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$349,LCFI1158-LCFI1156
	.long L$set$349
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$350,LCFI1159-LCFI1158
	.long L$set$350
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE117:
_Divide__Divide_param_i32_C.eh:
LSFDE119:
	.set L$set$351,LEFDE119-LASFDE119
	.long L$set$351
LASFDE119:
	.long	LASFDE119-EH_frame1
	.quad	LFB321-.
	.set L$set$352,LFE321-LFB321
	.quad L$set$352
	.byte	0x0
	.byte	0x4
	.set L$set$353,LCFI1163-LFB321
	.long L$set$353
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$354,LCFI1164-LCFI1163
	.long L$set$354
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$355,LCFI1166-LCFI1164
	.long L$set$355
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$356,LCFI1167-LCFI1166
	.long L$set$356
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE119:
_Divide__Divide_var_i32_u8.eh:
LSFDE121:
	.set L$set$357,LEFDE121-LASFDE121
	.long L$set$357
LASFDE121:
	.long	LASFDE121-EH_frame1
	.quad	LFB323-.
	.set L$set$358,LFE323-LFB323
	.quad L$set$358
	.byte	0x0
	.byte	0x4
	.set L$set$359,LCFI1171-LFB323
	.long L$set$359
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$360,LCFI1172-LCFI1171
	.long L$set$360
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$361,LCFI1174-LCFI1172
	.long L$set$361
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$362,LCFI1175-LCFI1174
	.long L$set$362
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE121:
_Divide__Divide_param_i32_u8.eh:
LSFDE123:
	.set L$set$363,LEFDE123-LASFDE123
	.long L$set$363
LASFDE123:
	.long	LASFDE123-EH_frame1
	.quad	LFB325-.
	.set L$set$364,LFE325-LFB325
	.quad L$set$364
	.byte	0x0
	.byte	0x4
	.set L$set$365,LCFI1179-LFB325
	.long L$set$365
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$366,LCFI1180-LCFI1179
	.long L$set$366
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$367,LCFI1182-LCFI1180
	.long L$set$367
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$368,LCFI1183-LCFI1182
	.long L$set$368
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE123:
_Divide__Divide_var_i32_i8.eh:
LSFDE125:
	.set L$set$369,LEFDE125-LASFDE125
	.long L$set$369
LASFDE125:
	.long	LASFDE125-EH_frame1
	.quad	LFB335-.
	.set L$set$370,LFE335-LFB335
	.quad L$set$370
	.byte	0x0
	.byte	0x4
	.set L$set$371,LCFI1213-LFB335
	.long L$set$371
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$372,LCFI1214-LCFI1213
	.long L$set$372
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$373,LCFI1216-LCFI1214
	.long L$set$373
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$374,LCFI1217-LCFI1216
	.long L$set$374
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE125:
_Divide__Divide_param_i32_i8.eh:
LSFDE127:
	.set L$set$375,LEFDE127-LASFDE127
	.long L$set$375
LASFDE127:
	.long	LASFDE127-EH_frame1
	.quad	LFB337-.
	.set L$set$376,LFE337-LFB337
	.quad L$set$376
	.byte	0x0
	.byte	0x4
	.set L$set$377,LCFI1221-LFB337
	.long L$set$377
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$378,LCFI1222-LCFI1221
	.long L$set$378
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$379,LCFI1224-LCFI1222
	.long L$set$379
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$380,LCFI1225-LCFI1224
	.long L$set$380
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE127:
_Divide__Divide_var_C_u16.eh:
LSFDE129:
	.set L$set$381,LEFDE129-LASFDE129
	.long L$set$381
LASFDE129:
	.long	LASFDE129-EH_frame1
	.quad	LFB339-.
	.set L$set$382,LFE339-LFB339
	.quad L$set$382
	.byte	0x0
	.byte	0x4
	.set L$set$383,LCFI1229-LFB339
	.long L$set$383
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$384,LCFI1230-LCFI1229
	.long L$set$384
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$385,LCFI1232-LCFI1230
	.long L$set$385
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$386,LCFI1233-LCFI1232
	.long L$set$386
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE129:
_Divide__Divide_param_C_u16.eh:
LSFDE131:
	.set L$set$387,LEFDE131-LASFDE131
	.long L$set$387
LASFDE131:
	.long	LASFDE131-EH_frame1
	.quad	LFB341-.
	.set L$set$388,LFE341-LFB341
	.quad L$set$388
	.byte	0x0
	.byte	0x4
	.set L$set$389,LCFI1237-LFB341
	.long L$set$389
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$390,LCFI1238-LCFI1237
	.long L$set$390
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$391,LCFI1240-LCFI1238
	.long L$set$391
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$392,LCFI1241-LCFI1240
	.long L$set$392
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE131:
_Divide__Divide_var_C_u32.eh:
LSFDE133:
	.set L$set$393,LEFDE133-LASFDE133
	.long L$set$393
LASFDE133:
	.long	LASFDE133-EH_frame1
	.quad	LFB343-.
	.set L$set$394,LFE343-LFB343
	.quad L$set$394
	.byte	0x0
	.byte	0x4
	.set L$set$395,LCFI1245-LFB343
	.long L$set$395
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$396,LCFI1246-LCFI1245
	.long L$set$396
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$397,LCFI1248-LCFI1246
	.long L$set$397
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$398,LCFI1249-LCFI1248
	.long L$set$398
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE133:
_Divide__Divide_param_C_u32.eh:
LSFDE135:
	.set L$set$399,LEFDE135-LASFDE135
	.long L$set$399
LASFDE135:
	.long	LASFDE135-EH_frame1
	.quad	LFB345-.
	.set L$set$400,LFE345-LFB345
	.quad L$set$400
	.byte	0x0
	.byte	0x4
	.set L$set$401,LCFI1253-LFB345
	.long L$set$401
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$402,LCFI1254-LCFI1253
	.long L$set$402
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$403,LCFI1256-LCFI1254
	.long L$set$403
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$404,LCFI1257-LCFI1256
	.long L$set$404
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE135:
_Divide__Divide_var_C_I.eh:
LSFDE137:
	.set L$set$405,LEFDE137-LASFDE137
	.long L$set$405
LASFDE137:
	.long	LASFDE137-EH_frame1
	.quad	LFB351-.
	.set L$set$406,LFE351-LFB351
	.quad L$set$406
	.byte	0x0
	.byte	0x4
	.set L$set$407,LCFI1275-LFB351
	.long L$set$407
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$408,LCFI1276-LCFI1275
	.long L$set$408
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$409,LCFI1278-LCFI1276
	.long L$set$409
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$410,LCFI1279-LCFI1278
	.long L$set$410
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE137:
_Divide__Divide_param_C_I.eh:
LSFDE139:
	.set L$set$411,LEFDE139-LASFDE139
	.long L$set$411
LASFDE139:
	.long	LASFDE139-EH_frame1
	.quad	LFB353-.
	.set L$set$412,LFE353-LFB353
	.quad L$set$412
	.byte	0x0
	.byte	0x4
	.set L$set$413,LCFI1283-LFB353
	.long L$set$413
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$414,LCFI1284-LCFI1283
	.long L$set$414
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$415,LCFI1286-LCFI1284
	.long L$set$415
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE139:
_Divide__Divide_var_C_i16.eh:
LSFDE141:
	.set L$set$416,LEFDE141-LASFDE141
	.long L$set$416
LASFDE141:
	.long	LASFDE141-EH_frame1
	.quad	LFB355-.
	.set L$set$417,LFE355-LFB355
	.quad L$set$417
	.byte	0x0
	.byte	0x4
	.set L$set$418,LCFI1290-LFB355
	.long L$set$418
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$419,LCFI1291-LCFI1290
	.long L$set$419
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$420,LCFI1293-LCFI1291
	.long L$set$420
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$421,LCFI1294-LCFI1293
	.long L$set$421
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE141:
_Divide__Divide_param_C_i16.eh:
LSFDE143:
	.set L$set$422,LEFDE143-LASFDE143
	.long L$set$422
LASFDE143:
	.long	LASFDE143-EH_frame1
	.quad	LFB357-.
	.set L$set$423,LFE357-LFB357
	.quad L$set$423
	.byte	0x0
	.byte	0x4
	.set L$set$424,LCFI1298-LFB357
	.long L$set$424
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$425,LCFI1299-LCFI1298
	.long L$set$425
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$426,LCFI1301-LCFI1299
	.long L$set$426
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$427,LCFI1302-LCFI1301
	.long L$set$427
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE143:
_Divide__Divide_var_C_i32.eh:
LSFDE145:
	.set L$set$428,LEFDE145-LASFDE145
	.long L$set$428
LASFDE145:
	.long	LASFDE145-EH_frame1
	.quad	LFB363-.
	.set L$set$429,LFE363-LFB363
	.quad L$set$429
	.byte	0x0
	.byte	0x4
	.set L$set$430,LCFI1319-LFB363
	.long L$set$430
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$431,LCFI1320-LCFI1319
	.long L$set$431
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$432,LCFI1322-LCFI1320
	.long L$set$432
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$433,LCFI1323-LCFI1322
	.long L$set$433
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE145:
_Divide__Divide_param_C_i32.eh:
LSFDE147:
	.set L$set$434,LEFDE147-LASFDE147
	.long L$set$434
LASFDE147:
	.long	LASFDE147-EH_frame1
	.quad	LFB365-.
	.set L$set$435,LFE365-LFB365
	.quad L$set$435
	.byte	0x0
	.byte	0x4
	.set L$set$436,LCFI1327-LFB365
	.long L$set$436
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$437,LCFI1328-LCFI1327
	.long L$set$437
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$438,LCFI1330-LCFI1328
	.long L$set$438
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$439,LCFI1331-LCFI1330
	.long L$set$439
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE147:
_Divide__Divide_var_C_C.eh:
LSFDE149:
	.set L$set$440,LEFDE149-LASFDE149
	.long L$set$440
LASFDE149:
	.long	LASFDE149-EH_frame1
	.quad	LFB367-.
	.set L$set$441,LFE367-LFB367
	.quad L$set$441
	.byte	0x0
	.byte	0x4
	.set L$set$442,LCFI1335-LFB367
	.long L$set$442
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$443,LCFI1336-LCFI1335
	.long L$set$443
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$444,LCFI1338-LCFI1336
	.long L$set$444
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$445,LCFI1339-LCFI1338
	.long L$set$445
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE149:
_Divide__Divide_param_C_C.eh:
LSFDE151:
	.set L$set$446,LEFDE151-LASFDE151
	.long L$set$446
LASFDE151:
	.long	LASFDE151-EH_frame1
	.quad	LFB369-.
	.set L$set$447,LFE369-LFB369
	.quad L$set$447
	.byte	0x0
	.byte	0x4
	.set L$set$448,LCFI1343-LFB369
	.long L$set$448
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$449,LCFI1344-LCFI1343
	.long L$set$449
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$450,LCFI1346-LCFI1344
	.long L$set$450
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$451,LCFI1347-LCFI1346
	.long L$set$451
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE151:
_Divide__Divide_var_C_u8.eh:
LSFDE153:
	.set L$set$452,LEFDE153-LASFDE153
	.long L$set$452
LASFDE153:
	.long	LASFDE153-EH_frame1
	.quad	LFB371-.
	.set L$set$453,LFE371-LFB371
	.quad L$set$453
	.byte	0x0
	.byte	0x4
	.set L$set$454,LCFI1351-LFB371
	.long L$set$454
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$455,LCFI1352-LCFI1351
	.long L$set$455
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$456,LCFI1354-LCFI1352
	.long L$set$456
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$457,LCFI1355-LCFI1354
	.long L$set$457
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE153:
_Divide__Divide_param_C_u8.eh:
LSFDE155:
	.set L$set$458,LEFDE155-LASFDE155
	.long L$set$458
LASFDE155:
	.long	LASFDE155-EH_frame1
	.quad	LFB373-.
	.set L$set$459,LFE373-LFB373
	.quad L$set$459
	.byte	0x0
	.byte	0x4
	.set L$set$460,LCFI1359-LFB373
	.long L$set$460
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$461,LCFI1360-LCFI1359
	.long L$set$461
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$462,LCFI1362-LCFI1360
	.long L$set$462
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$463,LCFI1363-LCFI1362
	.long L$set$463
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE155:
_Divide__Divide_var_C_i8.eh:
LSFDE157:
	.set L$set$464,LEFDE157-LASFDE157
	.long L$set$464
LASFDE157:
	.long	LASFDE157-EH_frame1
	.quad	LFB383-.
	.set L$set$465,LFE383-LFB383
	.quad L$set$465
	.byte	0x0
	.byte	0x4
	.set L$set$466,LCFI1393-LFB383
	.long L$set$466
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$467,LCFI1394-LCFI1393
	.long L$set$467
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$468,LCFI1396-LCFI1394
	.long L$set$468
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$469,LCFI1397-LCFI1396
	.long L$set$469
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE157:
_Divide__Divide_param_C_i8.eh:
LSFDE159:
	.set L$set$470,LEFDE159-LASFDE159
	.long L$set$470
LASFDE159:
	.long	LASFDE159-EH_frame1
	.quad	LFB385-.
	.set L$set$471,LFE385-LFB385
	.quad L$set$471
	.byte	0x0
	.byte	0x4
	.set L$set$472,LCFI1401-LFB385
	.long L$set$472
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$473,LCFI1402-LCFI1401
	.long L$set$473
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$474,LCFI1404-LCFI1402
	.long L$set$474
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$475,LCFI1405-LCFI1404
	.long L$set$475
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE159:
_Divide__Divide_var_u8_u16.eh:
LSFDE161:
	.set L$set$476,LEFDE161-LASFDE161
	.long L$set$476
LASFDE161:
	.long	LASFDE161-EH_frame1
	.quad	LFB387-.
	.set L$set$477,LFE387-LFB387
	.quad L$set$477
	.byte	0x0
	.byte	0x4
	.set L$set$478,LCFI1409-LFB387
	.long L$set$478
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$479,LCFI1410-LCFI1409
	.long L$set$479
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$480,LCFI1412-LCFI1410
	.long L$set$480
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$481,LCFI1413-LCFI1412
	.long L$set$481
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE161:
_Divide__Divide_param_u8_u16.eh:
LSFDE163:
	.set L$set$482,LEFDE163-LASFDE163
	.long L$set$482
LASFDE163:
	.long	LASFDE163-EH_frame1
	.quad	LFB389-.
	.set L$set$483,LFE389-LFB389
	.quad L$set$483
	.byte	0x0
	.byte	0x4
	.set L$set$484,LCFI1417-LFB389
	.long L$set$484
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$485,LCFI1418-LCFI1417
	.long L$set$485
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$486,LCFI1420-LCFI1418
	.long L$set$486
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$487,LCFI1421-LCFI1420
	.long L$set$487
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE163:
_Divide__Divide_var_u8_u32.eh:
LSFDE165:
	.set L$set$488,LEFDE165-LASFDE165
	.long L$set$488
LASFDE165:
	.long	LASFDE165-EH_frame1
	.quad	LFB391-.
	.set L$set$489,LFE391-LFB391
	.quad L$set$489
	.byte	0x0
	.byte	0x4
	.set L$set$490,LCFI1425-LFB391
	.long L$set$490
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$491,LCFI1426-LCFI1425
	.long L$set$491
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$492,LCFI1428-LCFI1426
	.long L$set$492
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$493,LCFI1429-LCFI1428
	.long L$set$493
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE165:
_Divide__Divide_param_u8_u32.eh:
LSFDE167:
	.set L$set$494,LEFDE167-LASFDE167
	.long L$set$494
LASFDE167:
	.long	LASFDE167-EH_frame1
	.quad	LFB393-.
	.set L$set$495,LFE393-LFB393
	.quad L$set$495
	.byte	0x0
	.byte	0x4
	.set L$set$496,LCFI1433-LFB393
	.long L$set$496
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$497,LCFI1434-LCFI1433
	.long L$set$497
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$498,LCFI1436-LCFI1434
	.long L$set$498
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$499,LCFI1437-LCFI1436
	.long L$set$499
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE167:
_Divide__Divide_var_u8_I.eh:
LSFDE169:
	.set L$set$500,LEFDE169-LASFDE169
	.long L$set$500
LASFDE169:
	.long	LASFDE169-EH_frame1
	.quad	LFB399-.
	.set L$set$501,LFE399-LFB399
	.quad L$set$501
	.byte	0x0
	.byte	0x4
	.set L$set$502,LCFI1455-LFB399
	.long L$set$502
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$503,LCFI1456-LCFI1455
	.long L$set$503
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$504,LCFI1458-LCFI1456
	.long L$set$504
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$505,LCFI1459-LCFI1458
	.long L$set$505
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE169:
_Divide__Divide_param_u8_I.eh:
LSFDE171:
	.set L$set$506,LEFDE171-LASFDE171
	.long L$set$506
LASFDE171:
	.long	LASFDE171-EH_frame1
	.quad	LFB401-.
	.set L$set$507,LFE401-LFB401
	.quad L$set$507
	.byte	0x0
	.byte	0x4
	.set L$set$508,LCFI1463-LFB401
	.long L$set$508
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$509,LCFI1464-LCFI1463
	.long L$set$509
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$510,LCFI1466-LCFI1464
	.long L$set$510
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE171:
_Divide__Divide_var_u8_i16.eh:
LSFDE173:
	.set L$set$511,LEFDE173-LASFDE173
	.long L$set$511
LASFDE173:
	.long	LASFDE173-EH_frame1
	.quad	LFB403-.
	.set L$set$512,LFE403-LFB403
	.quad L$set$512
	.byte	0x0
	.byte	0x4
	.set L$set$513,LCFI1470-LFB403
	.long L$set$513
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$514,LCFI1471-LCFI1470
	.long L$set$514
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$515,LCFI1473-LCFI1471
	.long L$set$515
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$516,LCFI1474-LCFI1473
	.long L$set$516
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE173:
_Divide__Divide_param_u8_i16.eh:
LSFDE175:
	.set L$set$517,LEFDE175-LASFDE175
	.long L$set$517
LASFDE175:
	.long	LASFDE175-EH_frame1
	.quad	LFB405-.
	.set L$set$518,LFE405-LFB405
	.quad L$set$518
	.byte	0x0
	.byte	0x4
	.set L$set$519,LCFI1478-LFB405
	.long L$set$519
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$520,LCFI1479-LCFI1478
	.long L$set$520
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$521,LCFI1481-LCFI1479
	.long L$set$521
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$522,LCFI1482-LCFI1481
	.long L$set$522
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE175:
_Divide__Divide_var_u8_i32.eh:
LSFDE177:
	.set L$set$523,LEFDE177-LASFDE177
	.long L$set$523
LASFDE177:
	.long	LASFDE177-EH_frame1
	.quad	LFB411-.
	.set L$set$524,LFE411-LFB411
	.quad L$set$524
	.byte	0x0
	.byte	0x4
	.set L$set$525,LCFI1499-LFB411
	.long L$set$525
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$526,LCFI1500-LCFI1499
	.long L$set$526
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$527,LCFI1502-LCFI1500
	.long L$set$527
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$528,LCFI1503-LCFI1502
	.long L$set$528
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE177:
_Divide__Divide_param_u8_i32.eh:
LSFDE179:
	.set L$set$529,LEFDE179-LASFDE179
	.long L$set$529
LASFDE179:
	.long	LASFDE179-EH_frame1
	.quad	LFB413-.
	.set L$set$530,LFE413-LFB413
	.quad L$set$530
	.byte	0x0
	.byte	0x4
	.set L$set$531,LCFI1507-LFB413
	.long L$set$531
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$532,LCFI1508-LCFI1507
	.long L$set$532
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$533,LCFI1510-LCFI1508
	.long L$set$533
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$534,LCFI1511-LCFI1510
	.long L$set$534
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE179:
_Divide__Divide_var_u8_C.eh:
LSFDE181:
	.set L$set$535,LEFDE181-LASFDE181
	.long L$set$535
LASFDE181:
	.long	LASFDE181-EH_frame1
	.quad	LFB415-.
	.set L$set$536,LFE415-LFB415
	.quad L$set$536
	.byte	0x0
	.byte	0x4
	.set L$set$537,LCFI1515-LFB415
	.long L$set$537
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$538,LCFI1516-LCFI1515
	.long L$set$538
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$539,LCFI1518-LCFI1516
	.long L$set$539
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$540,LCFI1519-LCFI1518
	.long L$set$540
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE181:
_Divide__Divide_param_u8_C.eh:
LSFDE183:
	.set L$set$541,LEFDE183-LASFDE183
	.long L$set$541
LASFDE183:
	.long	LASFDE183-EH_frame1
	.quad	LFB417-.
	.set L$set$542,LFE417-LFB417
	.quad L$set$542
	.byte	0x0
	.byte	0x4
	.set L$set$543,LCFI1523-LFB417
	.long L$set$543
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$544,LCFI1524-LCFI1523
	.long L$set$544
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$545,LCFI1526-LCFI1524
	.long L$set$545
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$546,LCFI1527-LCFI1526
	.long L$set$546
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE183:
_Divide__Divide_var_u8_u8.eh:
LSFDE185:
	.set L$set$547,LEFDE185-LASFDE185
	.long L$set$547
LASFDE185:
	.long	LASFDE185-EH_frame1
	.quad	LFB419-.
	.set L$set$548,LFE419-LFB419
	.quad L$set$548
	.byte	0x0
	.byte	0x4
	.set L$set$549,LCFI1531-LFB419
	.long L$set$549
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$550,LCFI1532-LCFI1531
	.long L$set$550
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$551,LCFI1534-LCFI1532
	.long L$set$551
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$552,LCFI1535-LCFI1534
	.long L$set$552
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE185:
_Divide__Divide_param_u8_u8.eh:
LSFDE187:
	.set L$set$553,LEFDE187-LASFDE187
	.long L$set$553
LASFDE187:
	.long	LASFDE187-EH_frame1
	.quad	LFB421-.
	.set L$set$554,LFE421-LFB421
	.quad L$set$554
	.byte	0x0
	.byte	0x4
	.set L$set$555,LCFI1539-LFB421
	.long L$set$555
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$556,LCFI1540-LCFI1539
	.long L$set$556
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$557,LCFI1542-LCFI1540
	.long L$set$557
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$558,LCFI1543-LCFI1542
	.long L$set$558
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE187:
_Divide__Divide_var_u8_i8.eh:
LSFDE189:
	.set L$set$559,LEFDE189-LASFDE189
	.long L$set$559
LASFDE189:
	.long	LASFDE189-EH_frame1
	.quad	LFB431-.
	.set L$set$560,LFE431-LFB431
	.quad L$set$560
	.byte	0x0
	.byte	0x4
	.set L$set$561,LCFI1573-LFB431
	.long L$set$561
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$562,LCFI1574-LCFI1573
	.long L$set$562
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$563,LCFI1576-LCFI1574
	.long L$set$563
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$564,LCFI1577-LCFI1576
	.long L$set$564
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE189:
_Divide__Divide_param_u8_i8.eh:
LSFDE191:
	.set L$set$565,LEFDE191-LASFDE191
	.long L$set$565
LASFDE191:
	.long	LASFDE191-EH_frame1
	.quad	LFB433-.
	.set L$set$566,LFE433-LFB433
	.quad L$set$566
	.byte	0x0
	.byte	0x4
	.set L$set$567,LCFI1581-LFB433
	.long L$set$567
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$568,LCFI1582-LCFI1581
	.long L$set$568
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$569,LCFI1584-LCFI1582
	.long L$set$569
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$570,LCFI1585-LCFI1584
	.long L$set$570
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE191:
_Divide__Divide_var_i8_u16.eh:
LSFDE193:
	.set L$set$571,LEFDE193-LASFDE193
	.long L$set$571
LASFDE193:
	.long	LASFDE193-EH_frame1
	.quad	LFB533-.
	.set L$set$572,LFE533-LFB533
	.quad L$set$572
	.byte	0x0
	.byte	0x4
	.set L$set$573,LCFI1931-LFB533
	.long L$set$573
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$574,LCFI1932-LCFI1931
	.long L$set$574
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$575,LCFI1934-LCFI1932
	.long L$set$575
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$576,LCFI1935-LCFI1934
	.long L$set$576
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE193:
_Divide__Divide_param_i8_u16.eh:
LSFDE195:
	.set L$set$577,LEFDE195-LASFDE195
	.long L$set$577
LASFDE195:
	.long	LASFDE195-EH_frame1
	.quad	LFB535-.
	.set L$set$578,LFE535-LFB535
	.quad L$set$578
	.byte	0x0
	.byte	0x4
	.set L$set$579,LCFI1939-LFB535
	.long L$set$579
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$580,LCFI1940-LCFI1939
	.long L$set$580
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$581,LCFI1942-LCFI1940
	.long L$set$581
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$582,LCFI1943-LCFI1942
	.long L$set$582
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE195:
_Divide__Divide_var_i8_u32.eh:
LSFDE197:
	.set L$set$583,LEFDE197-LASFDE197
	.long L$set$583
LASFDE197:
	.long	LASFDE197-EH_frame1
	.quad	LFB537-.
	.set L$set$584,LFE537-LFB537
	.quad L$set$584
	.byte	0x0
	.byte	0x4
	.set L$set$585,LCFI1947-LFB537
	.long L$set$585
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$586,LCFI1948-LCFI1947
	.long L$set$586
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$587,LCFI1950-LCFI1948
	.long L$set$587
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$588,LCFI1951-LCFI1950
	.long L$set$588
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE197:
_Divide__Divide_param_i8_u32.eh:
LSFDE199:
	.set L$set$589,LEFDE199-LASFDE199
	.long L$set$589
LASFDE199:
	.long	LASFDE199-EH_frame1
	.quad	LFB539-.
	.set L$set$590,LFE539-LFB539
	.quad L$set$590
	.byte	0x0
	.byte	0x4
	.set L$set$591,LCFI1955-LFB539
	.long L$set$591
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$592,LCFI1956-LCFI1955
	.long L$set$592
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$593,LCFI1958-LCFI1956
	.long L$set$593
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$594,LCFI1959-LCFI1958
	.long L$set$594
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE199:
_Divide__Divide_var_i8_I.eh:
LSFDE201:
	.set L$set$595,LEFDE201-LASFDE201
	.long L$set$595
LASFDE201:
	.long	LASFDE201-EH_frame1
	.quad	LFB545-.
	.set L$set$596,LFE545-LFB545
	.quad L$set$596
	.byte	0x0
	.byte	0x4
	.set L$set$597,LCFI1977-LFB545
	.long L$set$597
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$598,LCFI1978-LCFI1977
	.long L$set$598
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$599,LCFI1980-LCFI1978
	.long L$set$599
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$600,LCFI1981-LCFI1980
	.long L$set$600
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE201:
_Divide__Divide_param_i8_I.eh:
LSFDE203:
	.set L$set$601,LEFDE203-LASFDE203
	.long L$set$601
LASFDE203:
	.long	LASFDE203-EH_frame1
	.quad	LFB547-.
	.set L$set$602,LFE547-LFB547
	.quad L$set$602
	.byte	0x0
	.byte	0x4
	.set L$set$603,LCFI1985-LFB547
	.long L$set$603
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$604,LCFI1986-LCFI1985
	.long L$set$604
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$605,LCFI1988-LCFI1986
	.long L$set$605
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE203:
_Divide__Divide_var_i8_i16.eh:
LSFDE205:
	.set L$set$606,LEFDE205-LASFDE205
	.long L$set$606
LASFDE205:
	.long	LASFDE205-EH_frame1
	.quad	LFB549-.
	.set L$set$607,LFE549-LFB549
	.quad L$set$607
	.byte	0x0
	.byte	0x4
	.set L$set$608,LCFI1992-LFB549
	.long L$set$608
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$609,LCFI1993-LCFI1992
	.long L$set$609
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$610,LCFI1995-LCFI1993
	.long L$set$610
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$611,LCFI1996-LCFI1995
	.long L$set$611
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE205:
_Divide__Divide_param_i8_i16.eh:
LSFDE207:
	.set L$set$612,LEFDE207-LASFDE207
	.long L$set$612
LASFDE207:
	.long	LASFDE207-EH_frame1
	.quad	LFB551-.
	.set L$set$613,LFE551-LFB551
	.quad L$set$613
	.byte	0x0
	.byte	0x4
	.set L$set$614,LCFI2000-LFB551
	.long L$set$614
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$615,LCFI2001-LCFI2000
	.long L$set$615
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$616,LCFI2003-LCFI2001
	.long L$set$616
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$617,LCFI2004-LCFI2003
	.long L$set$617
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE207:
_Divide__Divide_var_i8_i32.eh:
LSFDE209:
	.set L$set$618,LEFDE209-LASFDE209
	.long L$set$618
LASFDE209:
	.long	LASFDE209-EH_frame1
	.quad	LFB557-.
	.set L$set$619,LFE557-LFB557
	.quad L$set$619
	.byte	0x0
	.byte	0x4
	.set L$set$620,LCFI2021-LFB557
	.long L$set$620
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$621,LCFI2022-LCFI2021
	.long L$set$621
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$622,LCFI2024-LCFI2022
	.long L$set$622
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$623,LCFI2025-LCFI2024
	.long L$set$623
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE209:
_Divide__Divide_param_i8_i32.eh:
LSFDE211:
	.set L$set$624,LEFDE211-LASFDE211
	.long L$set$624
LASFDE211:
	.long	LASFDE211-EH_frame1
	.quad	LFB559-.
	.set L$set$625,LFE559-LFB559
	.quad L$set$625
	.byte	0x0
	.byte	0x4
	.set L$set$626,LCFI2029-LFB559
	.long L$set$626
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$627,LCFI2030-LCFI2029
	.long L$set$627
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$628,LCFI2032-LCFI2030
	.long L$set$628
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$629,LCFI2033-LCFI2032
	.long L$set$629
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE211:
_Divide__Divide_var_i8_C.eh:
LSFDE213:
	.set L$set$630,LEFDE213-LASFDE213
	.long L$set$630
LASFDE213:
	.long	LASFDE213-EH_frame1
	.quad	LFB561-.
	.set L$set$631,LFE561-LFB561
	.quad L$set$631
	.byte	0x0
	.byte	0x4
	.set L$set$632,LCFI2037-LFB561
	.long L$set$632
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$633,LCFI2038-LCFI2037
	.long L$set$633
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$634,LCFI2040-LCFI2038
	.long L$set$634
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$635,LCFI2041-LCFI2040
	.long L$set$635
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE213:
_Divide__Divide_param_i8_C.eh:
LSFDE215:
	.set L$set$636,LEFDE215-LASFDE215
	.long L$set$636
LASFDE215:
	.long	LASFDE215-EH_frame1
	.quad	LFB563-.
	.set L$set$637,LFE563-LFB563
	.quad L$set$637
	.byte	0x0
	.byte	0x4
	.set L$set$638,LCFI2045-LFB563
	.long L$set$638
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$639,LCFI2046-LCFI2045
	.long L$set$639
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$640,LCFI2048-LCFI2046
	.long L$set$640
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$641,LCFI2049-LCFI2048
	.long L$set$641
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE215:
_Divide__Divide_var_i8_u8.eh:
LSFDE217:
	.set L$set$642,LEFDE217-LASFDE217
	.long L$set$642
LASFDE217:
	.long	LASFDE217-EH_frame1
	.quad	LFB565-.
	.set L$set$643,LFE565-LFB565
	.quad L$set$643
	.byte	0x0
	.byte	0x4
	.set L$set$644,LCFI2053-LFB565
	.long L$set$644
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$645,LCFI2054-LCFI2053
	.long L$set$645
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$646,LCFI2056-LCFI2054
	.long L$set$646
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$647,LCFI2057-LCFI2056
	.long L$set$647
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE217:
_Divide__Divide_param_i8_u8.eh:
LSFDE219:
	.set L$set$648,LEFDE219-LASFDE219
	.long L$set$648
LASFDE219:
	.long	LASFDE219-EH_frame1
	.quad	LFB567-.
	.set L$set$649,LFE567-LFB567
	.quad L$set$649
	.byte	0x0
	.byte	0x4
	.set L$set$650,LCFI2061-LFB567
	.long L$set$650
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$651,LCFI2062-LCFI2061
	.long L$set$651
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$652,LCFI2064-LCFI2062
	.long L$set$652
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$653,LCFI2065-LCFI2064
	.long L$set$653
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE219:
_Divide__Divide_var_i8_i8.eh:
LSFDE221:
	.set L$set$654,LEFDE221-LASFDE221
	.long L$set$654
LASFDE221:
	.long	LASFDE221-EH_frame1
	.quad	LFB577-.
	.set L$set$655,LFE577-LFB577
	.quad L$set$655
	.byte	0x0
	.byte	0x4
	.set L$set$656,LCFI2095-LFB577
	.long L$set$656
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$657,LCFI2096-LCFI2095
	.long L$set$657
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$658,LCFI2098-LCFI2096
	.long L$set$658
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$659,LCFI2099-LCFI2098
	.long L$set$659
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE221:
_Divide__Divide_param_i8_i8.eh:
LSFDE223:
	.set L$set$660,LEFDE223-LASFDE223
	.long L$set$660
LASFDE223:
	.long	LASFDE223-EH_frame1
	.quad	LFB579-.
	.set L$set$661,LFE579-LFB579
	.quad L$set$661
	.byte	0x0
	.byte	0x4
	.set L$set$662,LCFI2103-LFB579
	.long L$set$662
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$663,LCFI2104-LCFI2103
	.long L$set$663
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$664,LCFI2106-LCFI2104
	.long L$set$664
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$665,LCFI2107-LCFI2106
	.long L$set$665
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE223:
__m3_fault.eh:
LSFDE225:
	.set L$set$666,LEFDE225-LASFDE225
	.long L$set$666
LASFDE225:
	.long	LASFDE225-EH_frame1
	.quad	LFB581-.
	.set L$set$667,LFE581-LFB581
	.quad L$set$667
	.byte	0x0
	.byte	0x4
	.set L$set$668,LCFI2111-LFB581
	.long L$set$668
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$669,LCFI2112-LCFI2111
	.long L$set$669
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$670,LCFI2114-LCFI2112
	.long L$set$670
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE225:
	.subsections_via_symbols
