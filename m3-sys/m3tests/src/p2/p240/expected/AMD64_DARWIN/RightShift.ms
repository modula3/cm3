	.text
.no_dead_strip _RightShift__uRightShift_var_u16_I
_RightShift__uRightShift_var_u16_I:
LFB0:
	pushq	%rbp
LCFI0:
	movq	%rsp, %rbp
LCFI1:
	pushq	%rbx
LCFI2:
	subq	$8, %rsp
LCFI3:
	movq	120+_MM_RightShift(%rip), %rax
	testq	%rax, %rax
	js	L2
	movq	120+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jle	L3
L2:
	movl	$1729, %edi
	call	__m3_fault
L3:
	movq	120+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jg	L4
	movzwl	104+_MM_RightShift(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_RightShift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L5
L4:
	movl	$0, %eax
L5:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI4:
	ret
LFE0:
.no_dead_strip _RightShift__uRightShift_param_u16_I
_RightShift__uRightShift_param_u16_I:
LFB1:
	pushq	%rbp
LCFI5:
	movq	%rsp, %rbp
LCFI6:
	pushq	%rbx
LCFI7:
	subq	$24, %rsp
LCFI8:
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	cmpq	$0, -32(%rbp)
	js	L7
	cmpq	$63, -32(%rbp)
	jle	L8
L7:
	movl	$1761, %edi
	call	__m3_fault
L8:
	cmpq	$63, -32(%rbp)
	jg	L9
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L10
L9:
	movl	$0, %eax
L10:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI9:
	ret
LFE1:
.no_dead_strip _RightShift__uRightShift_var_u16_C
_RightShift__uRightShift_var_u16_C:
LFB2:
	pushq	%rbp
LCFI10:
	movq	%rsp, %rbp
LCFI11:
	pushq	%rbx
LCFI12:
	subq	$8, %rsp
LCFI13:
	movq	160+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jle	L12
	movl	$1793, %edi
	call	__m3_fault
L12:
	movq	160+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jg	L13
	movzwl	104+_MM_RightShift(%rip), %eax
	movzwl	%ax, %edx
	movq	160+_MM_RightShift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L14
L13:
	movl	$0, %eax
L14:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI14:
	ret
LFE2:
.no_dead_strip _RightShift__uRightShift_param_u16_C
_RightShift__uRightShift_param_u16_C:
LFB3:
	pushq	%rbp
LCFI15:
	movq	%rsp, %rbp
LCFI16:
	pushq	%rbx
LCFI17:
	subq	$24, %rsp
LCFI18:
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	$63, %rax
	jle	L16
	movl	$1825, %edi
	call	__m3_fault
L16:
	movq	-32(%rbp), %rax
	cmpq	$63, %rax
	jg	L17
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L18
L17:
	movl	$0, %eax
L18:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI19:
	ret
LFE3:
.no_dead_strip _RightShift__uRightShift_var_u32_I
_RightShift__uRightShift_var_u32_I:
LFB4:
	pushq	%rbp
LCFI20:
	movq	%rsp, %rbp
LCFI21:
	pushq	%rbx
LCFI22:
	subq	$8, %rsp
LCFI23:
	movq	120+_MM_RightShift(%rip), %rax
	testq	%rax, %rax
	js	L20
	movq	120+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jle	L21
L20:
	movl	$1857, %edi
	call	__m3_fault
L21:
	movq	120+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jg	L22
	movl	108+_MM_RightShift(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_RightShift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L23
L22:
	movl	$0, %eax
L23:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI24:
	ret
LFE4:
.no_dead_strip _RightShift__uRightShift_param_u32_I
_RightShift__uRightShift_param_u32_I:
LFB5:
	pushq	%rbp
LCFI25:
	movq	%rsp, %rbp
LCFI26:
	pushq	%rbx
LCFI27:
	subq	$24, %rsp
LCFI28:
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	cmpq	$0, -32(%rbp)
	js	L25
	cmpq	$63, -32(%rbp)
	jle	L26
L25:
	movl	$1889, %edi
	call	__m3_fault
L26:
	cmpq	$63, -32(%rbp)
	jg	L27
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L28
L27:
	movl	$0, %eax
L28:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI29:
	ret
LFE5:
.no_dead_strip _RightShift__uRightShift_var_u32_C
_RightShift__uRightShift_var_u32_C:
LFB6:
	pushq	%rbp
LCFI30:
	movq	%rsp, %rbp
LCFI31:
	pushq	%rbx
LCFI32:
	subq	$8, %rsp
LCFI33:
	movq	160+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jle	L30
	movl	$1921, %edi
	call	__m3_fault
L30:
	movq	160+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jg	L31
	movl	108+_MM_RightShift(%rip), %eax
	mov	%eax, %edx
	movq	160+_MM_RightShift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L32
L31:
	movl	$0, %eax
L32:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI34:
	ret
LFE6:
.no_dead_strip _RightShift__uRightShift_param_u32_C
_RightShift__uRightShift_param_u32_C:
LFB7:
	pushq	%rbp
LCFI35:
	movq	%rsp, %rbp
LCFI36:
	pushq	%rbx
LCFI37:
	subq	$24, %rsp
LCFI38:
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	$63, %rax
	jle	L34
	movl	$1953, %edi
	call	__m3_fault
L34:
	movq	-32(%rbp), %rax
	cmpq	$63, %rax
	jg	L35
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L36
L35:
	movl	$0, %eax
L36:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI39:
	ret
LFE7:
.no_dead_strip _RightShift__uRightShift_var_LC_I
_RightShift__uRightShift_var_LC_I:
LFB8:
	pushq	%rbp
LCFI40:
	movq	%rsp, %rbp
LCFI41:
	pushq	%rbx
LCFI42:
	subq	$8, %rsp
LCFI43:
	movq	120+_MM_RightShift(%rip), %rax
	testq	%rax, %rax
	js	L38
	movq	120+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jle	L39
L38:
	movl	$1985, %edi
	call	__m3_fault
L39:
	movq	120+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jg	L40
	movq	112+_MM_RightShift(%rip), %rdx
	movq	120+_MM_RightShift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L41
L40:
	movl	$0, %eax
L41:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI44:
	ret
LFE8:
.no_dead_strip _RightShift__uRightShift_param_LC_I
_RightShift__uRightShift_param_LC_I:
LFB9:
	pushq	%rbp
LCFI45:
	movq	%rsp, %rbp
LCFI46:
	pushq	%rbx
LCFI47:
	subq	$24, %rsp
LCFI48:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	cmpq	$0, -32(%rbp)
	js	L43
	cmpq	$63, -32(%rbp)
	jle	L44
L43:
	movl	$2017, %edi
	call	__m3_fault
L44:
	cmpq	$63, -32(%rbp)
	jg	L45
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rdx
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L46
L45:
	movl	$0, %eax
L46:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI49:
	ret
LFE9:
.no_dead_strip _RightShift__uRightShift_var_LC_C
_RightShift__uRightShift_var_LC_C:
LFB10:
	pushq	%rbp
LCFI50:
	movq	%rsp, %rbp
LCFI51:
	pushq	%rbx
LCFI52:
	subq	$8, %rsp
LCFI53:
	movq	160+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jle	L48
	movl	$2049, %edi
	call	__m3_fault
L48:
	movq	160+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jg	L49
	movq	112+_MM_RightShift(%rip), %rdx
	movq	160+_MM_RightShift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L50
L49:
	movl	$0, %eax
L50:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI54:
	ret
LFE10:
.no_dead_strip _RightShift__uRightShift_param_LC_C
_RightShift__uRightShift_param_LC_C:
LFB11:
	pushq	%rbp
LCFI55:
	movq	%rsp, %rbp
LCFI56:
	pushq	%rbx
LCFI57:
	subq	$24, %rsp
LCFI58:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	$63, %rax
	jle	L52
	movl	$2081, %edi
	call	__m3_fault
L52:
	movq	-32(%rbp), %rax
	cmpq	$63, %rax
	jg	L53
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rdx
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L54
L53:
	movl	$0, %eax
L54:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI59:
	ret
LFE11:
.no_dead_strip _RightShift__uRightShift_var_I_I
_RightShift__uRightShift_var_I_I:
LFB12:
	pushq	%rbp
LCFI60:
	movq	%rsp, %rbp
LCFI61:
	pushq	%rbx
LCFI62:
	subq	$8, %rsp
LCFI63:
	movq	120+_MM_RightShift(%rip), %rax
	testq	%rax, %rax
	js	L56
	movq	120+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jle	L57
L56:
	movl	$2113, %edi
	call	__m3_fault
L57:
	movq	120+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jg	L58
	movq	120+_MM_RightShift(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_RightShift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L59
L58:
	movl	$0, %eax
L59:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI64:
	ret
LFE12:
.no_dead_strip _RightShift__uRightShift_param_I_I
_RightShift__uRightShift_param_I_I:
LFB13:
	pushq	%rbp
LCFI65:
	movq	%rsp, %rbp
LCFI66:
	pushq	%rbx
LCFI67:
	subq	$24, %rsp
LCFI68:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	cmpq	$0, -32(%rbp)
	js	L61
	cmpq	$63, -32(%rbp)
	jle	L62
L61:
	movl	$2145, %edi
	call	__m3_fault
L62:
	cmpq	$63, -32(%rbp)
	jg	L63
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L64
L63:
	movl	$0, %eax
L64:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI69:
	ret
LFE13:
.no_dead_strip _RightShift__uRightShift_var_I_C
_RightShift__uRightShift_var_I_C:
LFB14:
	pushq	%rbp
LCFI70:
	movq	%rsp, %rbp
LCFI71:
	pushq	%rbx
LCFI72:
	subq	$8, %rsp
LCFI73:
	movq	160+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jle	L66
	movl	$2177, %edi
	call	__m3_fault
L66:
	movq	160+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jg	L67
	movq	120+_MM_RightShift(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_RightShift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L68
L67:
	movl	$0, %eax
L68:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI74:
	ret
LFE14:
.no_dead_strip _RightShift__uRightShift_param_I_C
_RightShift__uRightShift_param_I_C:
LFB15:
	pushq	%rbp
LCFI75:
	movq	%rsp, %rbp
LCFI76:
	pushq	%rbx
LCFI77:
	subq	$24, %rsp
LCFI78:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	$63, %rax
	jle	L70
	movl	$2209, %edi
	call	__m3_fault
L70:
	movq	-32(%rbp), %rax
	cmpq	$63, %rax
	jg	L71
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L72
L71:
	movl	$0, %eax
L72:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI79:
	ret
LFE15:
.no_dead_strip _RightShift__uRightShift_var_i16_I
_RightShift__uRightShift_var_i16_I:
LFB16:
	pushq	%rbp
LCFI80:
	movq	%rsp, %rbp
LCFI81:
	pushq	%rbx
LCFI82:
	subq	$8, %rsp
LCFI83:
	movq	120+_MM_RightShift(%rip), %rax
	testq	%rax, %rax
	js	L74
	movq	120+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jle	L75
L74:
	movl	$2241, %edi
	call	__m3_fault
L75:
	movq	120+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jg	L76
	movzwl	128+_MM_RightShift(%rip), %eax
	movswq	%ax, %rdx
	movq	120+_MM_RightShift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L77
L76:
	movl	$0, %eax
L77:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI84:
	ret
LFE16:
.no_dead_strip _RightShift__uRightShift_param_i16_I
_RightShift__uRightShift_param_i16_I:
LFB17:
	pushq	%rbp
LCFI85:
	movq	%rsp, %rbp
LCFI86:
	pushq	%rbx
LCFI87:
	subq	$24, %rsp
LCFI88:
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	cmpq	$0, -32(%rbp)
	js	L79
	cmpq	$63, -32(%rbp)
	jle	L80
L79:
	movl	$2273, %edi
	call	__m3_fault
L80:
	cmpq	$63, -32(%rbp)
	jg	L81
	movswq	-18(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L82
L81:
	movl	$0, %eax
L82:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI89:
	ret
LFE17:
.no_dead_strip _RightShift__uRightShift_var_i16_C
_RightShift__uRightShift_var_i16_C:
LFB18:
	pushq	%rbp
LCFI90:
	movq	%rsp, %rbp
LCFI91:
	pushq	%rbx
LCFI92:
	subq	$8, %rsp
LCFI93:
	movq	160+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jle	L84
	movl	$2305, %edi
	call	__m3_fault
L84:
	movq	160+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jg	L85
	movzwl	128+_MM_RightShift(%rip), %eax
	movswq	%ax, %rdx
	movq	160+_MM_RightShift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L86
L85:
	movl	$0, %eax
L86:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI94:
	ret
LFE18:
.no_dead_strip _RightShift__uRightShift_param_i16_C
_RightShift__uRightShift_param_i16_C:
LFB19:
	pushq	%rbp
LCFI95:
	movq	%rsp, %rbp
LCFI96:
	pushq	%rbx
LCFI97:
	subq	$24, %rsp
LCFI98:
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	$63, %rax
	jle	L88
	movl	$2337, %edi
	call	__m3_fault
L88:
	movq	-32(%rbp), %rax
	cmpq	$63, %rax
	jg	L89
	movswq	-18(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L90
L89:
	movl	$0, %eax
L90:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI99:
	ret
LFE19:
.no_dead_strip _RightShift__uRightShift_var_u64_I
_RightShift__uRightShift_var_u64_I:
LFB20:
	pushq	%rbp
LCFI100:
	movq	%rsp, %rbp
LCFI101:
	pushq	%rbx
LCFI102:
	subq	$8, %rsp
LCFI103:
	movq	120+_MM_RightShift(%rip), %rax
	testq	%rax, %rax
	js	L92
	movq	120+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jle	L93
L92:
	movl	$2369, %edi
	call	__m3_fault
L93:
	movq	120+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jg	L94
	movq	136+_MM_RightShift(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_RightShift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L95
L94:
	movl	$0, %eax
L95:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI104:
	ret
LFE20:
.no_dead_strip _RightShift__uRightShift_param_u64_I
_RightShift__uRightShift_param_u64_I:
LFB21:
	pushq	%rbp
LCFI105:
	movq	%rsp, %rbp
LCFI106:
	pushq	%rbx
LCFI107:
	subq	$24, %rsp
LCFI108:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	cmpq	$0, -32(%rbp)
	js	L97
	cmpq	$63, -32(%rbp)
	jle	L98
L97:
	movl	$2401, %edi
	call	__m3_fault
L98:
	cmpq	$63, -32(%rbp)
	jg	L99
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L100
L99:
	movl	$0, %eax
L100:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI109:
	ret
LFE21:
.no_dead_strip _RightShift__uRightShift_var_u64_C
_RightShift__uRightShift_var_u64_C:
LFB22:
	pushq	%rbp
LCFI110:
	movq	%rsp, %rbp
LCFI111:
	pushq	%rbx
LCFI112:
	subq	$8, %rsp
LCFI113:
	movq	160+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jle	L102
	movl	$2433, %edi
	call	__m3_fault
L102:
	movq	160+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jg	L103
	movq	136+_MM_RightShift(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_RightShift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L104
L103:
	movl	$0, %eax
L104:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI114:
	ret
LFE22:
.no_dead_strip _RightShift__uRightShift_param_u64_C
_RightShift__uRightShift_param_u64_C:
LFB23:
	pushq	%rbp
LCFI115:
	movq	%rsp, %rbp
LCFI116:
	pushq	%rbx
LCFI117:
	subq	$24, %rsp
LCFI118:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	$63, %rax
	jle	L106
	movl	$2465, %edi
	call	__m3_fault
L106:
	movq	-32(%rbp), %rax
	cmpq	$63, %rax
	jg	L107
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L108
L107:
	movl	$0, %eax
L108:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI119:
	ret
LFE23:
.no_dead_strip _RightShift__uRightShift_var_i32_I
_RightShift__uRightShift_var_i32_I:
LFB24:
	pushq	%rbp
LCFI120:
	movq	%rsp, %rbp
LCFI121:
	pushq	%rbx
LCFI122:
	subq	$8, %rsp
LCFI123:
	movq	120+_MM_RightShift(%rip), %rax
	testq	%rax, %rax
	js	L110
	movq	120+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jle	L111
L110:
	movl	$2497, %edi
	call	__m3_fault
L111:
	movq	120+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jg	L112
	movl	152+_MM_RightShift(%rip), %eax
	movslq	%eax, %rdx
	movq	120+_MM_RightShift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L113
L112:
	movl	$0, %eax
L113:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI124:
	ret
LFE24:
.no_dead_strip _RightShift__uRightShift_param_i32_I
_RightShift__uRightShift_param_i32_I:
LFB25:
	pushq	%rbp
LCFI125:
	movq	%rsp, %rbp
LCFI126:
	pushq	%rbx
LCFI127:
	subq	$24, %rsp
LCFI128:
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	cmpq	$0, -32(%rbp)
	js	L115
	cmpq	$63, -32(%rbp)
	jle	L116
L115:
	movl	$2529, %edi
	call	__m3_fault
L116:
	cmpq	$63, -32(%rbp)
	jg	L117
	movl	-20(%rbp), %eax
	movslq	%eax, %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L118
L117:
	movl	$0, %eax
L118:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI129:
	ret
LFE25:
.no_dead_strip _RightShift__uRightShift_var_i32_C
_RightShift__uRightShift_var_i32_C:
LFB26:
	pushq	%rbp
LCFI130:
	movq	%rsp, %rbp
LCFI131:
	pushq	%rbx
LCFI132:
	subq	$8, %rsp
LCFI133:
	movq	160+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jle	L120
	movl	$2561, %edi
	call	__m3_fault
L120:
	movq	160+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jg	L121
	movl	152+_MM_RightShift(%rip), %eax
	movslq	%eax, %rdx
	movq	160+_MM_RightShift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L122
L121:
	movl	$0, %eax
L122:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI134:
	ret
LFE26:
.no_dead_strip _RightShift__uRightShift_param_i32_C
_RightShift__uRightShift_param_i32_C:
LFB27:
	pushq	%rbp
LCFI135:
	movq	%rsp, %rbp
LCFI136:
	pushq	%rbx
LCFI137:
	subq	$24, %rsp
LCFI138:
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	$63, %rax
	jle	L124
	movl	$2593, %edi
	call	__m3_fault
L124:
	movq	-32(%rbp), %rax
	cmpq	$63, %rax
	jg	L125
	movl	-20(%rbp), %eax
	movslq	%eax, %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L126
L125:
	movl	$0, %eax
L126:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI139:
	ret
LFE27:
.no_dead_strip _RightShift__uRightShift_var_C_I
_RightShift__uRightShift_var_C_I:
LFB28:
	pushq	%rbp
LCFI140:
	movq	%rsp, %rbp
LCFI141:
	pushq	%rbx
LCFI142:
	subq	$8, %rsp
LCFI143:
	movq	120+_MM_RightShift(%rip), %rax
	testq	%rax, %rax
	js	L128
	movq	120+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jle	L129
L128:
	movl	$2625, %edi
	call	__m3_fault
L129:
	movq	120+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jg	L130
	movq	160+_MM_RightShift(%rip), %rdx
	movq	120+_MM_RightShift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L131
L130:
	movl	$0, %eax
L131:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI144:
	ret
LFE28:
.no_dead_strip _RightShift__uRightShift_param_C_I
_RightShift__uRightShift_param_C_I:
LFB29:
	pushq	%rbp
LCFI145:
	movq	%rsp, %rbp
LCFI146:
	pushq	%rbx
LCFI147:
	subq	$24, %rsp
LCFI148:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	cmpq	$0, -32(%rbp)
	js	L133
	cmpq	$63, -32(%rbp)
	jle	L134
L133:
	movl	$2657, %edi
	call	__m3_fault
L134:
	cmpq	$63, -32(%rbp)
	jg	L135
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rdx
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L136
L135:
	movl	$0, %eax
L136:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI149:
	ret
LFE29:
.no_dead_strip _RightShift__uRightShift_var_C_C
_RightShift__uRightShift_var_C_C:
LFB30:
	pushq	%rbp
LCFI150:
	movq	%rsp, %rbp
LCFI151:
	pushq	%rbx
LCFI152:
	subq	$8, %rsp
LCFI153:
	movq	160+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jle	L138
	movl	$2689, %edi
	call	__m3_fault
L138:
	movq	160+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jg	L139
	movq	160+_MM_RightShift(%rip), %rdx
	movq	160+_MM_RightShift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L140
L139:
	movl	$0, %eax
L140:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI154:
	ret
LFE30:
.no_dead_strip _RightShift__uRightShift_param_C_C
_RightShift__uRightShift_param_C_C:
LFB31:
	pushq	%rbp
LCFI155:
	movq	%rsp, %rbp
LCFI156:
	pushq	%rbx
LCFI157:
	subq	$24, %rsp
LCFI158:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	$63, %rax
	jle	L142
	movl	$2721, %edi
	call	__m3_fault
L142:
	movq	-32(%rbp), %rax
	cmpq	$63, %rax
	jg	L143
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rdx
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L144
L143:
	movl	$0, %eax
L144:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI159:
	ret
LFE31:
.no_dead_strip _RightShift__uRightShift_var_u8_I
_RightShift__uRightShift_var_u8_I:
LFB32:
	pushq	%rbp
LCFI160:
	movq	%rsp, %rbp
LCFI161:
	pushq	%rbx
LCFI162:
	subq	$8, %rsp
LCFI163:
	movq	120+_MM_RightShift(%rip), %rax
	testq	%rax, %rax
	js	L146
	movq	120+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jle	L147
L146:
	movl	$2753, %edi
	call	__m3_fault
L147:
	movq	120+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jg	L148
	movzbl	168+_MM_RightShift(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_RightShift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L149
L148:
	movl	$0, %eax
L149:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI164:
	ret
LFE32:
.no_dead_strip _RightShift__uRightShift_param_u8_I
_RightShift__uRightShift_param_u8_I:
LFB33:
	pushq	%rbp
LCFI165:
	movq	%rsp, %rbp
LCFI166:
	pushq	%rbx
LCFI167:
	subq	$24, %rsp
LCFI168:
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	cmpq	$0, -32(%rbp)
	js	L151
	cmpq	$63, -32(%rbp)
	jle	L152
L151:
	movl	$2785, %edi
	call	__m3_fault
L152:
	cmpq	$63, -32(%rbp)
	jg	L153
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L154
L153:
	movl	$0, %eax
L154:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI169:
	ret
LFE33:
.no_dead_strip _RightShift__uRightShift_var_u8_C
_RightShift__uRightShift_var_u8_C:
LFB34:
	pushq	%rbp
LCFI170:
	movq	%rsp, %rbp
LCFI171:
	pushq	%rbx
LCFI172:
	subq	$8, %rsp
LCFI173:
	movq	160+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jle	L156
	movl	$2817, %edi
	call	__m3_fault
L156:
	movq	160+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jg	L157
	movzbl	168+_MM_RightShift(%rip), %eax
	movzbl	%al, %edx
	movq	160+_MM_RightShift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L158
L157:
	movl	$0, %eax
L158:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI174:
	ret
LFE34:
.no_dead_strip _RightShift__uRightShift_param_u8_C
_RightShift__uRightShift_param_u8_C:
LFB35:
	pushq	%rbp
LCFI175:
	movq	%rsp, %rbp
LCFI176:
	pushq	%rbx
LCFI177:
	subq	$24, %rsp
LCFI178:
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	$63, %rax
	jle	L160
	movl	$2849, %edi
	call	__m3_fault
L160:
	movq	-32(%rbp), %rax
	cmpq	$63, %rax
	jg	L161
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L162
L161:
	movl	$0, %eax
L162:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI179:
	ret
LFE35:
.no_dead_strip _RightShift__uRightShift_var_i64_I
_RightShift__uRightShift_var_i64_I:
LFB36:
	pushq	%rbp
LCFI180:
	movq	%rsp, %rbp
LCFI181:
	pushq	%rbx
LCFI182:
	subq	$8, %rsp
LCFI183:
	movq	120+_MM_RightShift(%rip), %rax
	testq	%rax, %rax
	js	L164
	movq	120+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jle	L165
L164:
	movl	$2881, %edi
	call	__m3_fault
L165:
	movq	120+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jg	L166
	movq	176+_MM_RightShift(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_RightShift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L167
L166:
	movl	$0, %eax
L167:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI184:
	ret
LFE36:
.no_dead_strip _RightShift__uRightShift_param_i64_I
_RightShift__uRightShift_param_i64_I:
LFB37:
	pushq	%rbp
LCFI185:
	movq	%rsp, %rbp
LCFI186:
	pushq	%rbx
LCFI187:
	subq	$24, %rsp
LCFI188:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	cmpq	$0, -32(%rbp)
	js	L169
	cmpq	$63, -32(%rbp)
	jle	L170
L169:
	movl	$2913, %edi
	call	__m3_fault
L170:
	cmpq	$63, -32(%rbp)
	jg	L171
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L172
L171:
	movl	$0, %eax
L172:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI189:
	ret
LFE37:
.no_dead_strip _RightShift__uRightShift_var_i64_C
_RightShift__uRightShift_var_i64_C:
LFB38:
	pushq	%rbp
LCFI190:
	movq	%rsp, %rbp
LCFI191:
	pushq	%rbx
LCFI192:
	subq	$8, %rsp
LCFI193:
	movq	160+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jle	L174
	movl	$2945, %edi
	call	__m3_fault
L174:
	movq	160+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jg	L175
	movq	176+_MM_RightShift(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_RightShift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L176
L175:
	movl	$0, %eax
L176:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI194:
	ret
LFE38:
.no_dead_strip _RightShift__uRightShift_param_i64_C
_RightShift__uRightShift_param_i64_C:
LFB39:
	pushq	%rbp
LCFI195:
	movq	%rsp, %rbp
LCFI196:
	pushq	%rbx
LCFI197:
	subq	$24, %rsp
LCFI198:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	$63, %rax
	jle	L178
	movl	$2977, %edi
	call	__m3_fault
L178:
	movq	-32(%rbp), %rax
	cmpq	$63, %rax
	jg	L179
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L180
L179:
	movl	$0, %eax
L180:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI199:
	ret
LFE39:
.no_dead_strip _RightShift__uRightShift_var_L_I
_RightShift__uRightShift_var_L_I:
LFB40:
	pushq	%rbp
LCFI200:
	movq	%rsp, %rbp
LCFI201:
	pushq	%rbx
LCFI202:
	subq	$8, %rsp
LCFI203:
	movq	120+_MM_RightShift(%rip), %rax
	testq	%rax, %rax
	js	L182
	movq	120+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jle	L183
L182:
	movl	$3009, %edi
	call	__m3_fault
L183:
	movq	120+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jg	L184
	movq	192+_MM_RightShift(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_RightShift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L185
L184:
	movl	$0, %eax
L185:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI204:
	ret
LFE40:
.no_dead_strip _RightShift__uRightShift_param_L_I
_RightShift__uRightShift_param_L_I:
LFB41:
	pushq	%rbp
LCFI205:
	movq	%rsp, %rbp
LCFI206:
	pushq	%rbx
LCFI207:
	subq	$24, %rsp
LCFI208:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	cmpq	$0, -32(%rbp)
	js	L187
	cmpq	$63, -32(%rbp)
	jle	L188
L187:
	movl	$3041, %edi
	call	__m3_fault
L188:
	cmpq	$63, -32(%rbp)
	jg	L189
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L190
L189:
	movl	$0, %eax
L190:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI209:
	ret
LFE41:
.no_dead_strip _RightShift__uRightShift_var_L_C
_RightShift__uRightShift_var_L_C:
LFB42:
	pushq	%rbp
LCFI210:
	movq	%rsp, %rbp
LCFI211:
	pushq	%rbx
LCFI212:
	subq	$8, %rsp
LCFI213:
	movq	160+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jle	L192
	movl	$3073, %edi
	call	__m3_fault
L192:
	movq	160+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jg	L193
	movq	192+_MM_RightShift(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_RightShift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L194
L193:
	movl	$0, %eax
L194:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI214:
	ret
LFE42:
.no_dead_strip _RightShift__uRightShift_param_L_C
_RightShift__uRightShift_param_L_C:
LFB43:
	pushq	%rbp
LCFI215:
	movq	%rsp, %rbp
LCFI216:
	pushq	%rbx
LCFI217:
	subq	$24, %rsp
LCFI218:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	$63, %rax
	jle	L196
	movl	$3105, %edi
	call	__m3_fault
L196:
	movq	-32(%rbp), %rax
	cmpq	$63, %rax
	jg	L197
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L198
L197:
	movl	$0, %eax
L198:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI219:
	ret
LFE43:
.no_dead_strip _RightShift__uRightShift_var_i8_I
_RightShift__uRightShift_var_i8_I:
LFB44:
	pushq	%rbp
LCFI220:
	movq	%rsp, %rbp
LCFI221:
	pushq	%rbx
LCFI222:
	subq	$8, %rsp
LCFI223:
	movq	120+_MM_RightShift(%rip), %rax
	testq	%rax, %rax
	js	L200
	movq	120+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jle	L201
L200:
	movl	$3137, %edi
	call	__m3_fault
L201:
	movq	120+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jg	L202
	movzbl	200+_MM_RightShift(%rip), %eax
	movsbq	%al, %rdx
	movq	120+_MM_RightShift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L203
L202:
	movl	$0, %eax
L203:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI224:
	ret
LFE44:
.no_dead_strip _RightShift__uRightShift_param_i8_I
_RightShift__uRightShift_param_i8_I:
LFB45:
	pushq	%rbp
LCFI225:
	movq	%rsp, %rbp
LCFI226:
	pushq	%rbx
LCFI227:
	subq	$24, %rsp
LCFI228:
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	cmpq	$0, -32(%rbp)
	js	L205
	cmpq	$63, -32(%rbp)
	jle	L206
L205:
	movl	$3169, %edi
	call	__m3_fault
L206:
	cmpq	$63, -32(%rbp)
	jg	L207
	movsbq	-17(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L208
L207:
	movl	$0, %eax
L208:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI229:
	ret
LFE45:
.no_dead_strip _RightShift__uRightShift_var_i8_C
_RightShift__uRightShift_var_i8_C:
LFB46:
	pushq	%rbp
LCFI230:
	movq	%rsp, %rbp
LCFI231:
	pushq	%rbx
LCFI232:
	subq	$8, %rsp
LCFI233:
	movq	160+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jle	L210
	movl	$3201, %edi
	call	__m3_fault
L210:
	movq	160+_MM_RightShift(%rip), %rax
	cmpq	$63, %rax
	jg	L211
	movzbl	200+_MM_RightShift(%rip), %eax
	movsbq	%al, %rdx
	movq	160+_MM_RightShift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L212
L211:
	movl	$0, %eax
L212:
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI234:
	ret
LFE46:
.no_dead_strip _RightShift__uRightShift_param_i8_C
_RightShift__uRightShift_param_i8_C:
LFB47:
	pushq	%rbp
LCFI235:
	movq	%rsp, %rbp
LCFI236:
	pushq	%rbx
LCFI237:
	subq	$24, %rsp
LCFI238:
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	$63, %rax
	jle	L214
	movl	$3233, %edi
	call	__m3_fault
L214:
	movq	-32(%rbp), %rax
	cmpq	$63, %rax
	jg	L215
	movsbq	-17(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L216
L215:
	movl	$0, %eax
L216:
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI239:
	ret
LFE47:
.globl _RightShift_M3
.no_dead_strip _RightShift_M3
_RightShift_M3:
LFB48:
	pushq	%rbp
LCFI240:
	movq	%rsp, %rbp
LCFI241:
	movq	%rdi, -8(%rbp)
	leaq	_MM_RightShift(%rip), %rax
	leave
LCFI242:
	ret
LFE48:
__m3_fault:
LFB49:
	pushq	%rbp
LCFI243:
	movq	%rsp, %rbp
LCFI244:
	subq	$16, %rsp
LCFI245:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	%rax, %rsi
	leaq	_MM_RightShift(%rip), %rdi
	call	_RTHooks__ReportFault
	leave
LCFI246:
	ret
LFE49:
	.const_data
	.align 5
_L_1:
	.ascii "RightShift_M3"
	.space 1
	.ascii "uRightShift_param_i8_C"
	.space 1
	.ascii "uRightShift_var_i8_C"
	.space 1
	.ascii "uRightShift_param_i8_I"
	.space 1
	.ascii "uRightShift_var_i8_I"
	.space 1
	.ascii "uRightShift_param_L_C"
	.space 1
	.ascii "uRightShift_var_L_C"
	.space 1
	.ascii "uRightShift_param_L_I"
	.space 1
	.ascii "uRightShift_var_L_I"
	.space 1
	.ascii "uRightShift_param_i64_C"
	.space 1
	.ascii "uRightShift_var_i64_C"
	.space 1
	.ascii "uRightShift_param_i64_I"
	.space 1
	.ascii "uRightShift_var_i64_I"
	.space 1
	.ascii "uRightShift_param_u8_C"
	.space 1
	.ascii "uRightShift_var_u8_C"
	.space 1
	.ascii "uRightShift_param_u8_I"
	.space 1
	.ascii "uRightShift_var_u8_I"
	.space 1
	.ascii "uRightShift_param_C_C"
	.space 1
	.ascii "uRightShift_var_C_C"
	.space 1
	.ascii "uRightShift_param_C_I"
	.space 1
	.ascii "uRightShift_var_C_I"
	.space 1
	.ascii "uRightShift_param_i32_C"
	.space 1
	.ascii "uRightShift_var_i32_C"
	.space 1
	.ascii "uRightShift_param_i32_I"
	.space 1
	.ascii "uRightShift_var_i32_I"
	.space 1
	.ascii "uRightShift_param_u64_C"
	.space 1
	.ascii "uRightShift_var_u64_C"
	.space 1
	.ascii "uRightShift_param_u64_I"
	.space 1
	.ascii "uRightShift_var_u64_I"
	.space 1
	.ascii "uRightShift_param_i16_C"
	.space 1
	.ascii "uRightShift_var_i16_C"
	.space 1
	.ascii "uRightShift_param_i16_I"
	.space 1
	.ascii "uRightShift_var_i16_I"
	.space 1
	.ascii "uRightShift_param_I_C"
	.space 1
	.ascii "uRightShift_var_I_C"
	.space 1
	.ascii "uRightShift_param_I_I"
	.space 1
	.ascii "uRightShift_var_I_I"
	.space 1
	.ascii "uRightShift_param_LC_C"
	.space 1
	.ascii "uRightShift_var_LC_C"
	.space 1
	.ascii "uRightShift_param_LC_I"
	.space 1
	.ascii "uRightShift_var_LC_I"
	.space 1
	.ascii "uRightShift_param_u32_C"
	.space 1
	.ascii "uRightShift_var_u32_C"
	.space 1
	.ascii "uRightShift_param_u32_I"
	.space 1
	.ascii "uRightShift_var_u32_I"
	.space 1
	.ascii "uRightShift_param_u16_C"
	.space 1
	.ascii "uRightShift_var_u16_C"
	.space 1
	.ascii "uRightShift_param_u16_I"
	.space 1
	.ascii "uRightShift_var_u16_I"
	.space 7
	.quad	_RightShift_M3
	.quad	_L_1
	.quad	_RightShift__uRightShift_param_i8_C
	.quad	_L_1+14
	.quad	_RightShift__uRightShift_var_i8_C
	.quad	_L_1+37
	.quad	_RightShift__uRightShift_param_i8_I
	.quad	_L_1+58
	.quad	_RightShift__uRightShift_var_i8_I
	.quad	_L_1+81
	.quad	_RightShift__uRightShift_param_L_C
	.quad	_L_1+102
	.quad	_RightShift__uRightShift_var_L_C
	.quad	_L_1+124
	.quad	_RightShift__uRightShift_param_L_I
	.quad	_L_1+144
	.quad	_RightShift__uRightShift_var_L_I
	.quad	_L_1+166
	.quad	_RightShift__uRightShift_param_i64_C
	.quad	_L_1+186
	.quad	_RightShift__uRightShift_var_i64_C
	.quad	_L_1+210
	.quad	_RightShift__uRightShift_param_i64_I
	.quad	_L_1+232
	.quad	_RightShift__uRightShift_var_i64_I
	.quad	_L_1+256
	.quad	_RightShift__uRightShift_param_u8_C
	.quad	_L_1+278
	.quad	_RightShift__uRightShift_var_u8_C
	.quad	_L_1+301
	.quad	_RightShift__uRightShift_param_u8_I
	.quad	_L_1+322
	.quad	_RightShift__uRightShift_var_u8_I
	.quad	_L_1+345
	.quad	_RightShift__uRightShift_param_C_C
	.quad	_L_1+366
	.quad	_RightShift__uRightShift_var_C_C
	.quad	_L_1+388
	.quad	_RightShift__uRightShift_param_C_I
	.quad	_L_1+408
	.quad	_RightShift__uRightShift_var_C_I
	.quad	_L_1+430
	.quad	_RightShift__uRightShift_param_i32_C
	.quad	_L_1+450
	.quad	_RightShift__uRightShift_var_i32_C
	.quad	_L_1+474
	.quad	_RightShift__uRightShift_param_i32_I
	.quad	_L_1+496
	.quad	_RightShift__uRightShift_var_i32_I
	.quad	_L_1+520
	.quad	_RightShift__uRightShift_param_u64_C
	.quad	_L_1+542
	.quad	_RightShift__uRightShift_var_u64_C
	.quad	_L_1+566
	.quad	_RightShift__uRightShift_param_u64_I
	.quad	_L_1+588
	.quad	_RightShift__uRightShift_var_u64_I
	.quad	_L_1+612
	.quad	_RightShift__uRightShift_param_i16_C
	.quad	_L_1+634
	.quad	_RightShift__uRightShift_var_i16_C
	.quad	_L_1+658
	.quad	_RightShift__uRightShift_param_i16_I
	.quad	_L_1+680
	.quad	_RightShift__uRightShift_var_i16_I
	.quad	_L_1+704
	.quad	_RightShift__uRightShift_param_I_C
	.quad	_L_1+726
	.quad	_RightShift__uRightShift_var_I_C
	.quad	_L_1+748
	.quad	_RightShift__uRightShift_param_I_I
	.quad	_L_1+768
	.quad	_RightShift__uRightShift_var_I_I
	.quad	_L_1+790
	.quad	_RightShift__uRightShift_param_LC_C
	.quad	_L_1+810
	.quad	_RightShift__uRightShift_var_LC_C
	.quad	_L_1+833
	.quad	_RightShift__uRightShift_param_LC_I
	.quad	_L_1+854
	.quad	_RightShift__uRightShift_var_LC_I
	.quad	_L_1+877
	.quad	_RightShift__uRightShift_param_u32_C
	.quad	_L_1+898
	.quad	_RightShift__uRightShift_var_u32_C
	.quad	_L_1+922
	.quad	_RightShift__uRightShift_param_u32_I
	.quad	_L_1+944
	.quad	_RightShift__uRightShift_var_u32_I
	.quad	_L_1+968
	.quad	_RightShift__uRightShift_param_u16_C
	.quad	_L_1+990
	.quad	_RightShift__uRightShift_var_u16_C
	.quad	_L_1+1014
	.quad	_RightShift__uRightShift_param_u16_I
	.quad	_L_1+1036
	.quad	_RightShift__uRightShift_var_u16_I
	.quad	_L_1+1060
	.space 8
	.ascii "../AMD64_DARWIN/RightShift.m3"
	.space 3
	.data
	.align 5
_MM_RightShift:
	.quad	_L_1+1880
	.space 32
	.quad	_L_1+1088
	.space 24
	.quad	_MM_RightShift+224
	.space 8
	.quad	_RightShift_M3
	.quad	3
	.word	657
	.space 2
	.long	658
	.quad	659
	.quad	660
	.word	661
	.space 6
	.quad	662
	.long	-549755814
	.long	1082441039
	.long	665
	.space 4
	.quad	666
	.byte	-101
	.space 7
	.quad	668
	.long	1143433953
	.space 4
	.quad	671
	.byte	-96
	.space 31
	.quad	_RightShift_I3
	.quad	_MM_RightShift+248
	.space 8
	.quad	_Long_I3
	.quad	_MM_RightShift+272
	.space 8
	.quad	_Word_I3
	.quad	_MM_RightShift+296
	.space 8
	.quad	_Cstdint_I3
	.quad	_MM_RightShift+320
	.space 8
	.quad	_RTHooks_I3
	.space 8
.globl _RightShift_M3.eh
_RightShift_M3.eh = 0
.no_dead_strip _RightShift_M3.eh
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
_RightShift__uRightShift_var_u16_I.eh:
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
_RightShift__uRightShift_param_u16_I.eh:
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
_RightShift__uRightShift_var_u16_C.eh:
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
_RightShift__uRightShift_param_u16_C.eh:
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
_RightShift__uRightShift_var_u32_I.eh:
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
_RightShift__uRightShift_param_u32_I.eh:
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
_RightShift__uRightShift_var_u32_C.eh:
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
_RightShift__uRightShift_param_u32_C.eh:
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
_RightShift__uRightShift_var_LC_I.eh:
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
_RightShift__uRightShift_param_LC_I.eh:
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
_RightShift__uRightShift_var_LC_C.eh:
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
_RightShift__uRightShift_param_LC_C.eh:
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
_RightShift__uRightShift_var_I_I.eh:
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
_RightShift__uRightShift_param_I_I.eh:
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
_RightShift__uRightShift_var_I_C.eh:
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
_RightShift__uRightShift_param_I_C.eh:
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
_RightShift__uRightShift_var_i16_I.eh:
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
_RightShift__uRightShift_param_i16_I.eh:
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
_RightShift__uRightShift_var_i16_C.eh:
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
_RightShift__uRightShift_param_i16_C.eh:
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
_RightShift__uRightShift_var_u64_I.eh:
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
_RightShift__uRightShift_param_u64_I.eh:
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
_RightShift__uRightShift_var_u64_C.eh:
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
_RightShift__uRightShift_param_u64_C.eh:
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
_RightShift__uRightShift_var_i32_I.eh:
LSFDE49:
	.set L$set$145,LEFDE49-LASFDE49
	.long L$set$145
LASFDE49:
	.long	LASFDE49-EH_frame1
	.quad	LFB24-.
	.set L$set$146,LFE24-LFB24
	.quad L$set$146
	.byte	0x0
	.byte	0x4
	.set L$set$147,LCFI120-LFB24
	.long L$set$147
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$148,LCFI121-LCFI120
	.long L$set$148
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$149,LCFI123-LCFI121
	.long L$set$149
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$150,LCFI124-LCFI123
	.long L$set$150
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE49:
_RightShift__uRightShift_param_i32_I.eh:
LSFDE51:
	.set L$set$151,LEFDE51-LASFDE51
	.long L$set$151
LASFDE51:
	.long	LASFDE51-EH_frame1
	.quad	LFB25-.
	.set L$set$152,LFE25-LFB25
	.quad L$set$152
	.byte	0x0
	.byte	0x4
	.set L$set$153,LCFI125-LFB25
	.long L$set$153
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$154,LCFI126-LCFI125
	.long L$set$154
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$155,LCFI128-LCFI126
	.long L$set$155
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$156,LCFI129-LCFI128
	.long L$set$156
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE51:
_RightShift__uRightShift_var_i32_C.eh:
LSFDE53:
	.set L$set$157,LEFDE53-LASFDE53
	.long L$set$157
LASFDE53:
	.long	LASFDE53-EH_frame1
	.quad	LFB26-.
	.set L$set$158,LFE26-LFB26
	.quad L$set$158
	.byte	0x0
	.byte	0x4
	.set L$set$159,LCFI130-LFB26
	.long L$set$159
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$160,LCFI131-LCFI130
	.long L$set$160
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$161,LCFI133-LCFI131
	.long L$set$161
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$162,LCFI134-LCFI133
	.long L$set$162
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE53:
_RightShift__uRightShift_param_i32_C.eh:
LSFDE55:
	.set L$set$163,LEFDE55-LASFDE55
	.long L$set$163
LASFDE55:
	.long	LASFDE55-EH_frame1
	.quad	LFB27-.
	.set L$set$164,LFE27-LFB27
	.quad L$set$164
	.byte	0x0
	.byte	0x4
	.set L$set$165,LCFI135-LFB27
	.long L$set$165
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$166,LCFI136-LCFI135
	.long L$set$166
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$167,LCFI138-LCFI136
	.long L$set$167
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$168,LCFI139-LCFI138
	.long L$set$168
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE55:
_RightShift__uRightShift_var_C_I.eh:
LSFDE57:
	.set L$set$169,LEFDE57-LASFDE57
	.long L$set$169
LASFDE57:
	.long	LASFDE57-EH_frame1
	.quad	LFB28-.
	.set L$set$170,LFE28-LFB28
	.quad L$set$170
	.byte	0x0
	.byte	0x4
	.set L$set$171,LCFI140-LFB28
	.long L$set$171
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$172,LCFI141-LCFI140
	.long L$set$172
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$173,LCFI143-LCFI141
	.long L$set$173
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$174,LCFI144-LCFI143
	.long L$set$174
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE57:
_RightShift__uRightShift_param_C_I.eh:
LSFDE59:
	.set L$set$175,LEFDE59-LASFDE59
	.long L$set$175
LASFDE59:
	.long	LASFDE59-EH_frame1
	.quad	LFB29-.
	.set L$set$176,LFE29-LFB29
	.quad L$set$176
	.byte	0x0
	.byte	0x4
	.set L$set$177,LCFI145-LFB29
	.long L$set$177
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$178,LCFI146-LCFI145
	.long L$set$178
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$179,LCFI148-LCFI146
	.long L$set$179
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$180,LCFI149-LCFI148
	.long L$set$180
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE59:
_RightShift__uRightShift_var_C_C.eh:
LSFDE61:
	.set L$set$181,LEFDE61-LASFDE61
	.long L$set$181
LASFDE61:
	.long	LASFDE61-EH_frame1
	.quad	LFB30-.
	.set L$set$182,LFE30-LFB30
	.quad L$set$182
	.byte	0x0
	.byte	0x4
	.set L$set$183,LCFI150-LFB30
	.long L$set$183
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$184,LCFI151-LCFI150
	.long L$set$184
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$185,LCFI153-LCFI151
	.long L$set$185
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$186,LCFI154-LCFI153
	.long L$set$186
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE61:
_RightShift__uRightShift_param_C_C.eh:
LSFDE63:
	.set L$set$187,LEFDE63-LASFDE63
	.long L$set$187
LASFDE63:
	.long	LASFDE63-EH_frame1
	.quad	LFB31-.
	.set L$set$188,LFE31-LFB31
	.quad L$set$188
	.byte	0x0
	.byte	0x4
	.set L$set$189,LCFI155-LFB31
	.long L$set$189
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$190,LCFI156-LCFI155
	.long L$set$190
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$191,LCFI158-LCFI156
	.long L$set$191
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$192,LCFI159-LCFI158
	.long L$set$192
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE63:
_RightShift__uRightShift_var_u8_I.eh:
LSFDE65:
	.set L$set$193,LEFDE65-LASFDE65
	.long L$set$193
LASFDE65:
	.long	LASFDE65-EH_frame1
	.quad	LFB32-.
	.set L$set$194,LFE32-LFB32
	.quad L$set$194
	.byte	0x0
	.byte	0x4
	.set L$set$195,LCFI160-LFB32
	.long L$set$195
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$196,LCFI161-LCFI160
	.long L$set$196
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$197,LCFI163-LCFI161
	.long L$set$197
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$198,LCFI164-LCFI163
	.long L$set$198
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE65:
_RightShift__uRightShift_param_u8_I.eh:
LSFDE67:
	.set L$set$199,LEFDE67-LASFDE67
	.long L$set$199
LASFDE67:
	.long	LASFDE67-EH_frame1
	.quad	LFB33-.
	.set L$set$200,LFE33-LFB33
	.quad L$set$200
	.byte	0x0
	.byte	0x4
	.set L$set$201,LCFI165-LFB33
	.long L$set$201
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$202,LCFI166-LCFI165
	.long L$set$202
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$203,LCFI168-LCFI166
	.long L$set$203
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$204,LCFI169-LCFI168
	.long L$set$204
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE67:
_RightShift__uRightShift_var_u8_C.eh:
LSFDE69:
	.set L$set$205,LEFDE69-LASFDE69
	.long L$set$205
LASFDE69:
	.long	LASFDE69-EH_frame1
	.quad	LFB34-.
	.set L$set$206,LFE34-LFB34
	.quad L$set$206
	.byte	0x0
	.byte	0x4
	.set L$set$207,LCFI170-LFB34
	.long L$set$207
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$208,LCFI171-LCFI170
	.long L$set$208
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$209,LCFI173-LCFI171
	.long L$set$209
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$210,LCFI174-LCFI173
	.long L$set$210
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE69:
_RightShift__uRightShift_param_u8_C.eh:
LSFDE71:
	.set L$set$211,LEFDE71-LASFDE71
	.long L$set$211
LASFDE71:
	.long	LASFDE71-EH_frame1
	.quad	LFB35-.
	.set L$set$212,LFE35-LFB35
	.quad L$set$212
	.byte	0x0
	.byte	0x4
	.set L$set$213,LCFI175-LFB35
	.long L$set$213
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$214,LCFI176-LCFI175
	.long L$set$214
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$215,LCFI178-LCFI176
	.long L$set$215
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$216,LCFI179-LCFI178
	.long L$set$216
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE71:
_RightShift__uRightShift_var_i64_I.eh:
LSFDE73:
	.set L$set$217,LEFDE73-LASFDE73
	.long L$set$217
LASFDE73:
	.long	LASFDE73-EH_frame1
	.quad	LFB36-.
	.set L$set$218,LFE36-LFB36
	.quad L$set$218
	.byte	0x0
	.byte	0x4
	.set L$set$219,LCFI180-LFB36
	.long L$set$219
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$220,LCFI181-LCFI180
	.long L$set$220
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$221,LCFI183-LCFI181
	.long L$set$221
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$222,LCFI184-LCFI183
	.long L$set$222
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE73:
_RightShift__uRightShift_param_i64_I.eh:
LSFDE75:
	.set L$set$223,LEFDE75-LASFDE75
	.long L$set$223
LASFDE75:
	.long	LASFDE75-EH_frame1
	.quad	LFB37-.
	.set L$set$224,LFE37-LFB37
	.quad L$set$224
	.byte	0x0
	.byte	0x4
	.set L$set$225,LCFI185-LFB37
	.long L$set$225
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$226,LCFI186-LCFI185
	.long L$set$226
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$227,LCFI188-LCFI186
	.long L$set$227
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$228,LCFI189-LCFI188
	.long L$set$228
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE75:
_RightShift__uRightShift_var_i64_C.eh:
LSFDE77:
	.set L$set$229,LEFDE77-LASFDE77
	.long L$set$229
LASFDE77:
	.long	LASFDE77-EH_frame1
	.quad	LFB38-.
	.set L$set$230,LFE38-LFB38
	.quad L$set$230
	.byte	0x0
	.byte	0x4
	.set L$set$231,LCFI190-LFB38
	.long L$set$231
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$232,LCFI191-LCFI190
	.long L$set$232
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$233,LCFI193-LCFI191
	.long L$set$233
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$234,LCFI194-LCFI193
	.long L$set$234
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE77:
_RightShift__uRightShift_param_i64_C.eh:
LSFDE79:
	.set L$set$235,LEFDE79-LASFDE79
	.long L$set$235
LASFDE79:
	.long	LASFDE79-EH_frame1
	.quad	LFB39-.
	.set L$set$236,LFE39-LFB39
	.quad L$set$236
	.byte	0x0
	.byte	0x4
	.set L$set$237,LCFI195-LFB39
	.long L$set$237
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$238,LCFI196-LCFI195
	.long L$set$238
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$239,LCFI198-LCFI196
	.long L$set$239
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$240,LCFI199-LCFI198
	.long L$set$240
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE79:
_RightShift__uRightShift_var_L_I.eh:
LSFDE81:
	.set L$set$241,LEFDE81-LASFDE81
	.long L$set$241
LASFDE81:
	.long	LASFDE81-EH_frame1
	.quad	LFB40-.
	.set L$set$242,LFE40-LFB40
	.quad L$set$242
	.byte	0x0
	.byte	0x4
	.set L$set$243,LCFI200-LFB40
	.long L$set$243
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$244,LCFI201-LCFI200
	.long L$set$244
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$245,LCFI203-LCFI201
	.long L$set$245
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$246,LCFI204-LCFI203
	.long L$set$246
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE81:
_RightShift__uRightShift_param_L_I.eh:
LSFDE83:
	.set L$set$247,LEFDE83-LASFDE83
	.long L$set$247
LASFDE83:
	.long	LASFDE83-EH_frame1
	.quad	LFB41-.
	.set L$set$248,LFE41-LFB41
	.quad L$set$248
	.byte	0x0
	.byte	0x4
	.set L$set$249,LCFI205-LFB41
	.long L$set$249
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$250,LCFI206-LCFI205
	.long L$set$250
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$251,LCFI208-LCFI206
	.long L$set$251
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$252,LCFI209-LCFI208
	.long L$set$252
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE83:
_RightShift__uRightShift_var_L_C.eh:
LSFDE85:
	.set L$set$253,LEFDE85-LASFDE85
	.long L$set$253
LASFDE85:
	.long	LASFDE85-EH_frame1
	.quad	LFB42-.
	.set L$set$254,LFE42-LFB42
	.quad L$set$254
	.byte	0x0
	.byte	0x4
	.set L$set$255,LCFI210-LFB42
	.long L$set$255
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$256,LCFI211-LCFI210
	.long L$set$256
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$257,LCFI213-LCFI211
	.long L$set$257
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$258,LCFI214-LCFI213
	.long L$set$258
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE85:
_RightShift__uRightShift_param_L_C.eh:
LSFDE87:
	.set L$set$259,LEFDE87-LASFDE87
	.long L$set$259
LASFDE87:
	.long	LASFDE87-EH_frame1
	.quad	LFB43-.
	.set L$set$260,LFE43-LFB43
	.quad L$set$260
	.byte	0x0
	.byte	0x4
	.set L$set$261,LCFI215-LFB43
	.long L$set$261
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$262,LCFI216-LCFI215
	.long L$set$262
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$263,LCFI218-LCFI216
	.long L$set$263
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$264,LCFI219-LCFI218
	.long L$set$264
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE87:
_RightShift__uRightShift_var_i8_I.eh:
LSFDE89:
	.set L$set$265,LEFDE89-LASFDE89
	.long L$set$265
LASFDE89:
	.long	LASFDE89-EH_frame1
	.quad	LFB44-.
	.set L$set$266,LFE44-LFB44
	.quad L$set$266
	.byte	0x0
	.byte	0x4
	.set L$set$267,LCFI220-LFB44
	.long L$set$267
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$268,LCFI221-LCFI220
	.long L$set$268
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$269,LCFI223-LCFI221
	.long L$set$269
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$270,LCFI224-LCFI223
	.long L$set$270
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE89:
_RightShift__uRightShift_param_i8_I.eh:
LSFDE91:
	.set L$set$271,LEFDE91-LASFDE91
	.long L$set$271
LASFDE91:
	.long	LASFDE91-EH_frame1
	.quad	LFB45-.
	.set L$set$272,LFE45-LFB45
	.quad L$set$272
	.byte	0x0
	.byte	0x4
	.set L$set$273,LCFI225-LFB45
	.long L$set$273
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$274,LCFI226-LCFI225
	.long L$set$274
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$275,LCFI228-LCFI226
	.long L$set$275
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$276,LCFI229-LCFI228
	.long L$set$276
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE91:
_RightShift__uRightShift_var_i8_C.eh:
LSFDE93:
	.set L$set$277,LEFDE93-LASFDE93
	.long L$set$277
LASFDE93:
	.long	LASFDE93-EH_frame1
	.quad	LFB46-.
	.set L$set$278,LFE46-LFB46
	.quad L$set$278
	.byte	0x0
	.byte	0x4
	.set L$set$279,LCFI230-LFB46
	.long L$set$279
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$280,LCFI231-LCFI230
	.long L$set$280
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$281,LCFI233-LCFI231
	.long L$set$281
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$282,LCFI234-LCFI233
	.long L$set$282
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE93:
_RightShift__uRightShift_param_i8_C.eh:
LSFDE95:
	.set L$set$283,LEFDE95-LASFDE95
	.long L$set$283
LASFDE95:
	.long	LASFDE95-EH_frame1
	.quad	LFB47-.
	.set L$set$284,LFE47-LFB47
	.quad L$set$284
	.byte	0x0
	.byte	0x4
	.set L$set$285,LCFI235-LFB47
	.long L$set$285
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$286,LCFI236-LCFI235
	.long L$set$286
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$287,LCFI238-LCFI236
	.long L$set$287
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$288,LCFI239-LCFI238
	.long L$set$288
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE95:
__m3_fault.eh:
LSFDE97:
	.set L$set$289,LEFDE97-LASFDE97
	.long L$set$289
LASFDE97:
	.long	LASFDE97-EH_frame1
	.quad	LFB49-.
	.set L$set$290,LFE49-LFB49
	.quad L$set$290
	.byte	0x0
	.byte	0x4
	.set L$set$291,LCFI243-LFB49
	.long L$set$291
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$292,LCFI244-LCFI243
	.long L$set$292
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$293,LCFI246-LCFI244
	.long L$set$293
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE97:
	.subsections_via_symbols
