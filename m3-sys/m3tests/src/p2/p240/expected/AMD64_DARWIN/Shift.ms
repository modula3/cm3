	.text
.no_dead_strip _Shift__uShift_var_u16_I
_Shift__uShift_var_u16_I:
LFB0:
	pushq	%rbp
LCFI0:
	movq	%rsp, %rbp
LCFI1:
	pushq	%rbx
LCFI2:
	movq	120+_MM_Shift(%rip), %rax
	testq	%rax, %rax
	js	L2
	movq	120+_MM_Shift(%rip), %rax
	cmpq	$63, %rax
	jg	L3
	movzwl	104+_MM_Shift(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_Shift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L4
L3:
	movl	$0, %eax
L4:
	jmp	L5
L2:
	movq	120+_MM_Shift(%rip), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L6
	movzwl	104+_MM_Shift(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_Shift(%rip), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L7
L6:
	movl	$0, %eax
L7:
L5:
	popq	%rbx
	leave
LCFI3:
	ret
LFE0:
.no_dead_strip _Shift__uShift_param_u16_I
_Shift__uShift_param_u16_I:
LFB1:
	pushq	%rbp
LCFI4:
	movq	%rsp, %rbp
LCFI5:
	pushq	%rbx
LCFI6:
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	cmpq	$0, -32(%rbp)
	js	L9
	cmpq	$63, -32(%rbp)
	jg	L10
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L11
L10:
	movl	$0, %eax
L11:
	jmp	L12
L9:
	movq	-32(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L13
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L14
L13:
	movl	$0, %eax
L14:
L12:
	popq	%rbx
	leave
LCFI7:
	ret
LFE1:
.no_dead_strip _Shift__uShift_var_u16_C
_Shift__uShift_var_u16_C:
LFB2:
	pushq	%rbp
LCFI8:
	movq	%rsp, %rbp
LCFI9:
	pushq	%rbx
LCFI10:
	movq	160+_MM_Shift(%rip), %rax
	testq	%rax, %rax
	js	L16
	movq	160+_MM_Shift(%rip), %rax
	cmpq	$63, %rax
	jg	L17
	movzwl	104+_MM_Shift(%rip), %eax
	movzwl	%ax, %edx
	movq	160+_MM_Shift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L18
L17:
	movl	$0, %eax
L18:
	jmp	L19
L16:
	movq	160+_MM_Shift(%rip), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L20
	movzwl	104+_MM_Shift(%rip), %eax
	movzwl	%ax, %edx
	movq	160+_MM_Shift(%rip), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L21
L20:
	movl	$0, %eax
L21:
L19:
	popq	%rbx
	leave
LCFI11:
	ret
LFE2:
.no_dead_strip _Shift__uShift_param_u16_C
_Shift__uShift_param_u16_C:
LFB3:
	pushq	%rbp
LCFI12:
	movq	%rsp, %rbp
LCFI13:
	pushq	%rbx
LCFI14:
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	testq	%rax, %rax
	js	L23
	movq	-32(%rbp), %rax
	cmpq	$63, %rax
	jg	L24
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L25
L24:
	movl	$0, %eax
L25:
	jmp	L26
L23:
	movq	-32(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L27
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L28
L27:
	movl	$0, %eax
L28:
L26:
	popq	%rbx
	leave
LCFI15:
	ret
LFE3:
.no_dead_strip _Shift__uShift_var_u32_I
_Shift__uShift_var_u32_I:
LFB4:
	pushq	%rbp
LCFI16:
	movq	%rsp, %rbp
LCFI17:
	pushq	%rbx
LCFI18:
	movq	120+_MM_Shift(%rip), %rax
	testq	%rax, %rax
	js	L30
	movq	120+_MM_Shift(%rip), %rax
	cmpq	$63, %rax
	jg	L31
	movl	108+_MM_Shift(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_Shift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L32
L31:
	movl	$0, %eax
L32:
	jmp	L33
L30:
	movq	120+_MM_Shift(%rip), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L34
	movl	108+_MM_Shift(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_Shift(%rip), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L35
L34:
	movl	$0, %eax
L35:
L33:
	popq	%rbx
	leave
LCFI19:
	ret
LFE4:
.no_dead_strip _Shift__uShift_param_u32_I
_Shift__uShift_param_u32_I:
LFB5:
	pushq	%rbp
LCFI20:
	movq	%rsp, %rbp
LCFI21:
	pushq	%rbx
LCFI22:
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	cmpq	$0, -32(%rbp)
	js	L37
	cmpq	$63, -32(%rbp)
	jg	L38
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L39
L38:
	movl	$0, %eax
L39:
	jmp	L40
L37:
	movq	-32(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L41
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L42
L41:
	movl	$0, %eax
L42:
L40:
	popq	%rbx
	leave
LCFI23:
	ret
LFE5:
.no_dead_strip _Shift__uShift_var_u32_C
_Shift__uShift_var_u32_C:
LFB6:
	pushq	%rbp
LCFI24:
	movq	%rsp, %rbp
LCFI25:
	pushq	%rbx
LCFI26:
	movq	160+_MM_Shift(%rip), %rax
	testq	%rax, %rax
	js	L44
	movq	160+_MM_Shift(%rip), %rax
	cmpq	$63, %rax
	jg	L45
	movl	108+_MM_Shift(%rip), %eax
	mov	%eax, %edx
	movq	160+_MM_Shift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L46
L45:
	movl	$0, %eax
L46:
	jmp	L47
L44:
	movq	160+_MM_Shift(%rip), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L48
	movl	108+_MM_Shift(%rip), %eax
	mov	%eax, %edx
	movq	160+_MM_Shift(%rip), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L49
L48:
	movl	$0, %eax
L49:
L47:
	popq	%rbx
	leave
LCFI27:
	ret
LFE6:
.no_dead_strip _Shift__uShift_param_u32_C
_Shift__uShift_param_u32_C:
LFB7:
	pushq	%rbp
LCFI28:
	movq	%rsp, %rbp
LCFI29:
	pushq	%rbx
LCFI30:
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	testq	%rax, %rax
	js	L51
	movq	-32(%rbp), %rax
	cmpq	$63, %rax
	jg	L52
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L53
L52:
	movl	$0, %eax
L53:
	jmp	L54
L51:
	movq	-32(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L55
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L56
L55:
	movl	$0, %eax
L56:
L54:
	popq	%rbx
	leave
LCFI31:
	ret
LFE7:
.no_dead_strip _Shift__uShift_var_LC_I
_Shift__uShift_var_LC_I:
LFB8:
	pushq	%rbp
LCFI32:
	movq	%rsp, %rbp
LCFI33:
	pushq	%rbx
LCFI34:
	movq	120+_MM_Shift(%rip), %rax
	testq	%rax, %rax
	js	L58
	movq	120+_MM_Shift(%rip), %rax
	cmpq	$63, %rax
	jg	L59
	movq	112+_MM_Shift(%rip), %rdx
	movq	120+_MM_Shift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L60
L59:
	movl	$0, %eax
L60:
	jmp	L61
L58:
	movq	120+_MM_Shift(%rip), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L62
	movq	112+_MM_Shift(%rip), %rdx
	movq	120+_MM_Shift(%rip), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L63
L62:
	movl	$0, %eax
L63:
L61:
	popq	%rbx
	leave
LCFI35:
	ret
LFE8:
.no_dead_strip _Shift__uShift_param_LC_I
_Shift__uShift_param_LC_I:
LFB9:
	pushq	%rbp
LCFI36:
	movq	%rsp, %rbp
LCFI37:
	pushq	%rbx
LCFI38:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	cmpq	$0, -32(%rbp)
	js	L65
	cmpq	$63, -32(%rbp)
	jg	L66
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rdx
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L67
L66:
	movl	$0, %eax
L67:
	jmp	L68
L65:
	movq	-32(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L69
	movq	-32(%rbp), %rax
	negq	%rax
	movq	-24(%rbp), %rdx
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L70
L69:
	movl	$0, %eax
L70:
L68:
	popq	%rbx
	leave
LCFI39:
	ret
LFE9:
.no_dead_strip _Shift__uShift_var_LC_C
_Shift__uShift_var_LC_C:
LFB10:
	pushq	%rbp
LCFI40:
	movq	%rsp, %rbp
LCFI41:
	pushq	%rbx
LCFI42:
	movq	160+_MM_Shift(%rip), %rax
	testq	%rax, %rax
	js	L72
	movq	160+_MM_Shift(%rip), %rax
	cmpq	$63, %rax
	jg	L73
	movq	112+_MM_Shift(%rip), %rdx
	movq	160+_MM_Shift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L74
L73:
	movl	$0, %eax
L74:
	jmp	L75
L72:
	movq	160+_MM_Shift(%rip), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L76
	movq	112+_MM_Shift(%rip), %rdx
	movq	160+_MM_Shift(%rip), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L77
L76:
	movl	$0, %eax
L77:
L75:
	popq	%rbx
	leave
LCFI43:
	ret
LFE10:
.no_dead_strip _Shift__uShift_param_LC_C
_Shift__uShift_param_LC_C:
LFB11:
	pushq	%rbp
LCFI44:
	movq	%rsp, %rbp
LCFI45:
	pushq	%rbx
LCFI46:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	testq	%rax, %rax
	js	L79
	movq	-32(%rbp), %rax
	cmpq	$63, %rax
	jg	L80
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rdx
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L81
L80:
	movl	$0, %eax
L81:
	jmp	L82
L79:
	movq	-32(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L83
	movq	-32(%rbp), %rax
	negq	%rax
	movq	-24(%rbp), %rdx
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L84
L83:
	movl	$0, %eax
L84:
L82:
	popq	%rbx
	leave
LCFI47:
	ret
LFE11:
.no_dead_strip _Shift__uShift_var_I_I
_Shift__uShift_var_I_I:
LFB12:
	pushq	%rbp
LCFI48:
	movq	%rsp, %rbp
LCFI49:
	pushq	%rbx
LCFI50:
	movq	120+_MM_Shift(%rip), %rax
	testq	%rax, %rax
	js	L86
	movq	120+_MM_Shift(%rip), %rax
	cmpq	$63, %rax
	jg	L87
	movq	120+_MM_Shift(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Shift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L88
L87:
	movl	$0, %eax
L88:
	jmp	L89
L86:
	movq	120+_MM_Shift(%rip), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L90
	movq	120+_MM_Shift(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Shift(%rip), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L91
L90:
	movl	$0, %eax
L91:
L89:
	popq	%rbx
	leave
LCFI51:
	ret
LFE12:
.no_dead_strip _Shift__uShift_param_I_I
_Shift__uShift_param_I_I:
LFB13:
	pushq	%rbp
LCFI52:
	movq	%rsp, %rbp
LCFI53:
	pushq	%rbx
LCFI54:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	cmpq	$0, -32(%rbp)
	js	L93
	cmpq	$63, -32(%rbp)
	jg	L94
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L95
L94:
	movl	$0, %eax
L95:
	jmp	L96
L93:
	movq	-32(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L97
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L98
L97:
	movl	$0, %eax
L98:
L96:
	popq	%rbx
	leave
LCFI55:
	ret
LFE13:
.no_dead_strip _Shift__uShift_var_I_C
_Shift__uShift_var_I_C:
LFB14:
	pushq	%rbp
LCFI56:
	movq	%rsp, %rbp
LCFI57:
	pushq	%rbx
LCFI58:
	movq	160+_MM_Shift(%rip), %rax
	testq	%rax, %rax
	js	L100
	movq	160+_MM_Shift(%rip), %rax
	cmpq	$63, %rax
	jg	L101
	movq	120+_MM_Shift(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Shift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L102
L101:
	movl	$0, %eax
L102:
	jmp	L103
L100:
	movq	160+_MM_Shift(%rip), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L104
	movq	120+_MM_Shift(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Shift(%rip), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L105
L104:
	movl	$0, %eax
L105:
L103:
	popq	%rbx
	leave
LCFI59:
	ret
LFE14:
.no_dead_strip _Shift__uShift_param_I_C
_Shift__uShift_param_I_C:
LFB15:
	pushq	%rbp
LCFI60:
	movq	%rsp, %rbp
LCFI61:
	pushq	%rbx
LCFI62:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	testq	%rax, %rax
	js	L107
	movq	-32(%rbp), %rax
	cmpq	$63, %rax
	jg	L108
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L109
L108:
	movl	$0, %eax
L109:
	jmp	L110
L107:
	movq	-32(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L111
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L112
L111:
	movl	$0, %eax
L112:
L110:
	popq	%rbx
	leave
LCFI63:
	ret
LFE15:
.no_dead_strip _Shift__uShift_var_i16_I
_Shift__uShift_var_i16_I:
LFB16:
	pushq	%rbp
LCFI64:
	movq	%rsp, %rbp
LCFI65:
	pushq	%rbx
LCFI66:
	movq	120+_MM_Shift(%rip), %rax
	testq	%rax, %rax
	js	L114
	movq	120+_MM_Shift(%rip), %rax
	cmpq	$63, %rax
	jg	L115
	movzwl	128+_MM_Shift(%rip), %eax
	movswq	%ax, %rdx
	movq	120+_MM_Shift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L116
L115:
	movl	$0, %eax
L116:
	jmp	L117
L114:
	movq	120+_MM_Shift(%rip), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L118
	movzwl	128+_MM_Shift(%rip), %eax
	movswq	%ax, %rdx
	movq	120+_MM_Shift(%rip), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L119
L118:
	movl	$0, %eax
L119:
L117:
	popq	%rbx
	leave
LCFI67:
	ret
LFE16:
.no_dead_strip _Shift__uShift_param_i16_I
_Shift__uShift_param_i16_I:
LFB17:
	pushq	%rbp
LCFI68:
	movq	%rsp, %rbp
LCFI69:
	pushq	%rbx
LCFI70:
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	cmpq	$0, -32(%rbp)
	js	L121
	cmpq	$63, -32(%rbp)
	jg	L122
	movswq	-18(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L123
L122:
	movl	$0, %eax
L123:
	jmp	L124
L121:
	movq	-32(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L125
	movswq	-18(%rbp), %rdx
	movq	-32(%rbp), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L126
L125:
	movl	$0, %eax
L126:
L124:
	popq	%rbx
	leave
LCFI71:
	ret
LFE17:
.no_dead_strip _Shift__uShift_var_i16_C
_Shift__uShift_var_i16_C:
LFB18:
	pushq	%rbp
LCFI72:
	movq	%rsp, %rbp
LCFI73:
	pushq	%rbx
LCFI74:
	movq	160+_MM_Shift(%rip), %rax
	testq	%rax, %rax
	js	L128
	movq	160+_MM_Shift(%rip), %rax
	cmpq	$63, %rax
	jg	L129
	movzwl	128+_MM_Shift(%rip), %eax
	movswq	%ax, %rdx
	movq	160+_MM_Shift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L130
L129:
	movl	$0, %eax
L130:
	jmp	L131
L128:
	movq	160+_MM_Shift(%rip), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L132
	movzwl	128+_MM_Shift(%rip), %eax
	movswq	%ax, %rdx
	movq	160+_MM_Shift(%rip), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L133
L132:
	movl	$0, %eax
L133:
L131:
	popq	%rbx
	leave
LCFI75:
	ret
LFE18:
.no_dead_strip _Shift__uShift_param_i16_C
_Shift__uShift_param_i16_C:
LFB19:
	pushq	%rbp
LCFI76:
	movq	%rsp, %rbp
LCFI77:
	pushq	%rbx
LCFI78:
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	testq	%rax, %rax
	js	L135
	movq	-32(%rbp), %rax
	cmpq	$63, %rax
	jg	L136
	movswq	-18(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L137
L136:
	movl	$0, %eax
L137:
	jmp	L138
L135:
	movq	-32(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L139
	movswq	-18(%rbp), %rdx
	movq	-32(%rbp), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L140
L139:
	movl	$0, %eax
L140:
L138:
	popq	%rbx
	leave
LCFI79:
	ret
LFE19:
.no_dead_strip _Shift__uShift_var_u64_I
_Shift__uShift_var_u64_I:
LFB20:
	pushq	%rbp
LCFI80:
	movq	%rsp, %rbp
LCFI81:
	pushq	%rbx
LCFI82:
	movq	120+_MM_Shift(%rip), %rax
	testq	%rax, %rax
	js	L142
	movq	120+_MM_Shift(%rip), %rax
	cmpq	$63, %rax
	jg	L143
	movq	136+_MM_Shift(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Shift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L144
L143:
	movl	$0, %eax
L144:
	jmp	L145
L142:
	movq	120+_MM_Shift(%rip), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L146
	movq	136+_MM_Shift(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Shift(%rip), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L147
L146:
	movl	$0, %eax
L147:
L145:
	popq	%rbx
	leave
LCFI83:
	ret
LFE20:
.no_dead_strip _Shift__uShift_param_u64_I
_Shift__uShift_param_u64_I:
LFB21:
	pushq	%rbp
LCFI84:
	movq	%rsp, %rbp
LCFI85:
	pushq	%rbx
LCFI86:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	cmpq	$0, -32(%rbp)
	js	L149
	cmpq	$63, -32(%rbp)
	jg	L150
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L151
L150:
	movl	$0, %eax
L151:
	jmp	L152
L149:
	movq	-32(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L153
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L154
L153:
	movl	$0, %eax
L154:
L152:
	popq	%rbx
	leave
LCFI87:
	ret
LFE21:
.no_dead_strip _Shift__uShift_var_u64_C
_Shift__uShift_var_u64_C:
LFB22:
	pushq	%rbp
LCFI88:
	movq	%rsp, %rbp
LCFI89:
	pushq	%rbx
LCFI90:
	movq	160+_MM_Shift(%rip), %rax
	testq	%rax, %rax
	js	L156
	movq	160+_MM_Shift(%rip), %rax
	cmpq	$63, %rax
	jg	L157
	movq	136+_MM_Shift(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Shift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L158
L157:
	movl	$0, %eax
L158:
	jmp	L159
L156:
	movq	160+_MM_Shift(%rip), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L160
	movq	136+_MM_Shift(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Shift(%rip), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L161
L160:
	movl	$0, %eax
L161:
L159:
	popq	%rbx
	leave
LCFI91:
	ret
LFE22:
.no_dead_strip _Shift__uShift_param_u64_C
_Shift__uShift_param_u64_C:
LFB23:
	pushq	%rbp
LCFI92:
	movq	%rsp, %rbp
LCFI93:
	pushq	%rbx
LCFI94:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	testq	%rax, %rax
	js	L163
	movq	-32(%rbp), %rax
	cmpq	$63, %rax
	jg	L164
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L165
L164:
	movl	$0, %eax
L165:
	jmp	L166
L163:
	movq	-32(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L167
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L168
L167:
	movl	$0, %eax
L168:
L166:
	popq	%rbx
	leave
LCFI95:
	ret
LFE23:
.no_dead_strip _Shift__uShift_var_i32_I
_Shift__uShift_var_i32_I:
LFB24:
	pushq	%rbp
LCFI96:
	movq	%rsp, %rbp
LCFI97:
	pushq	%rbx
LCFI98:
	movq	120+_MM_Shift(%rip), %rax
	testq	%rax, %rax
	js	L170
	movq	120+_MM_Shift(%rip), %rax
	cmpq	$63, %rax
	jg	L171
	movl	152+_MM_Shift(%rip), %eax
	movslq	%eax, %rdx
	movq	120+_MM_Shift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L172
L171:
	movl	$0, %eax
L172:
	jmp	L173
L170:
	movq	120+_MM_Shift(%rip), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L174
	movl	152+_MM_Shift(%rip), %eax
	movslq	%eax, %rdx
	movq	120+_MM_Shift(%rip), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L175
L174:
	movl	$0, %eax
L175:
L173:
	popq	%rbx
	leave
LCFI99:
	ret
LFE24:
.no_dead_strip _Shift__uShift_param_i32_I
_Shift__uShift_param_i32_I:
LFB25:
	pushq	%rbp
LCFI100:
	movq	%rsp, %rbp
LCFI101:
	pushq	%rbx
LCFI102:
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	cmpq	$0, -32(%rbp)
	js	L177
	cmpq	$63, -32(%rbp)
	jg	L178
	movl	-20(%rbp), %eax
	movslq	%eax, %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L179
L178:
	movl	$0, %eax
L179:
	jmp	L180
L177:
	movq	-32(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L181
	movl	-20(%rbp), %eax
	movslq	%eax, %rdx
	movq	-32(%rbp), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L182
L181:
	movl	$0, %eax
L182:
L180:
	popq	%rbx
	leave
LCFI103:
	ret
LFE25:
.no_dead_strip _Shift__uShift_var_i32_C
_Shift__uShift_var_i32_C:
LFB26:
	pushq	%rbp
LCFI104:
	movq	%rsp, %rbp
LCFI105:
	pushq	%rbx
LCFI106:
	movq	160+_MM_Shift(%rip), %rax
	testq	%rax, %rax
	js	L184
	movq	160+_MM_Shift(%rip), %rax
	cmpq	$63, %rax
	jg	L185
	movl	152+_MM_Shift(%rip), %eax
	movslq	%eax, %rdx
	movq	160+_MM_Shift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L186
L185:
	movl	$0, %eax
L186:
	jmp	L187
L184:
	movq	160+_MM_Shift(%rip), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L188
	movl	152+_MM_Shift(%rip), %eax
	movslq	%eax, %rdx
	movq	160+_MM_Shift(%rip), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L189
L188:
	movl	$0, %eax
L189:
L187:
	popq	%rbx
	leave
LCFI107:
	ret
LFE26:
.no_dead_strip _Shift__uShift_param_i32_C
_Shift__uShift_param_i32_C:
LFB27:
	pushq	%rbp
LCFI108:
	movq	%rsp, %rbp
LCFI109:
	pushq	%rbx
LCFI110:
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	testq	%rax, %rax
	js	L191
	movq	-32(%rbp), %rax
	cmpq	$63, %rax
	jg	L192
	movl	-20(%rbp), %eax
	movslq	%eax, %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L193
L192:
	movl	$0, %eax
L193:
	jmp	L194
L191:
	movq	-32(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L195
	movl	-20(%rbp), %eax
	movslq	%eax, %rdx
	movq	-32(%rbp), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L196
L195:
	movl	$0, %eax
L196:
L194:
	popq	%rbx
	leave
LCFI111:
	ret
LFE27:
.no_dead_strip _Shift__uShift_var_C_I
_Shift__uShift_var_C_I:
LFB28:
	pushq	%rbp
LCFI112:
	movq	%rsp, %rbp
LCFI113:
	pushq	%rbx
LCFI114:
	movq	120+_MM_Shift(%rip), %rax
	testq	%rax, %rax
	js	L198
	movq	120+_MM_Shift(%rip), %rax
	cmpq	$63, %rax
	jg	L199
	movq	160+_MM_Shift(%rip), %rdx
	movq	120+_MM_Shift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L200
L199:
	movl	$0, %eax
L200:
	jmp	L201
L198:
	movq	120+_MM_Shift(%rip), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L202
	movq	160+_MM_Shift(%rip), %rdx
	movq	120+_MM_Shift(%rip), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L203
L202:
	movl	$0, %eax
L203:
L201:
	popq	%rbx
	leave
LCFI115:
	ret
LFE28:
.no_dead_strip _Shift__uShift_param_C_I
_Shift__uShift_param_C_I:
LFB29:
	pushq	%rbp
LCFI116:
	movq	%rsp, %rbp
LCFI117:
	pushq	%rbx
LCFI118:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	cmpq	$0, -32(%rbp)
	js	L205
	cmpq	$63, -32(%rbp)
	jg	L206
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rdx
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L207
L206:
	movl	$0, %eax
L207:
	jmp	L208
L205:
	movq	-32(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L209
	movq	-32(%rbp), %rax
	negq	%rax
	movq	-24(%rbp), %rdx
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L210
L209:
	movl	$0, %eax
L210:
L208:
	popq	%rbx
	leave
LCFI119:
	ret
LFE29:
.no_dead_strip _Shift__uShift_var_C_C
_Shift__uShift_var_C_C:
LFB30:
	pushq	%rbp
LCFI120:
	movq	%rsp, %rbp
LCFI121:
	pushq	%rbx
LCFI122:
	movq	160+_MM_Shift(%rip), %rax
	testq	%rax, %rax
	js	L212
	movq	160+_MM_Shift(%rip), %rax
	cmpq	$63, %rax
	jg	L213
	movq	160+_MM_Shift(%rip), %rdx
	movq	160+_MM_Shift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L214
L213:
	movl	$0, %eax
L214:
	jmp	L215
L212:
	movq	160+_MM_Shift(%rip), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L216
	movq	160+_MM_Shift(%rip), %rdx
	movq	160+_MM_Shift(%rip), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L217
L216:
	movl	$0, %eax
L217:
L215:
	popq	%rbx
	leave
LCFI123:
	ret
LFE30:
.no_dead_strip _Shift__uShift_param_C_C
_Shift__uShift_param_C_C:
LFB31:
	pushq	%rbp
LCFI124:
	movq	%rsp, %rbp
LCFI125:
	pushq	%rbx
LCFI126:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	testq	%rax, %rax
	js	L219
	movq	-32(%rbp), %rax
	cmpq	$63, %rax
	jg	L220
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rdx
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L221
L220:
	movl	$0, %eax
L221:
	jmp	L222
L219:
	movq	-32(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L223
	movq	-32(%rbp), %rax
	negq	%rax
	movq	-24(%rbp), %rdx
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L224
L223:
	movl	$0, %eax
L224:
L222:
	popq	%rbx
	leave
LCFI127:
	ret
LFE31:
.no_dead_strip _Shift__uShift_var_u8_I
_Shift__uShift_var_u8_I:
LFB32:
	pushq	%rbp
LCFI128:
	movq	%rsp, %rbp
LCFI129:
	pushq	%rbx
LCFI130:
	movq	120+_MM_Shift(%rip), %rax
	testq	%rax, %rax
	js	L226
	movq	120+_MM_Shift(%rip), %rax
	cmpq	$63, %rax
	jg	L227
	movzbl	168+_MM_Shift(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_Shift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L228
L227:
	movl	$0, %eax
L228:
	jmp	L229
L226:
	movq	120+_MM_Shift(%rip), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L230
	movzbl	168+_MM_Shift(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_Shift(%rip), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L231
L230:
	movl	$0, %eax
L231:
L229:
	popq	%rbx
	leave
LCFI131:
	ret
LFE32:
.no_dead_strip _Shift__uShift_param_u8_I
_Shift__uShift_param_u8_I:
LFB33:
	pushq	%rbp
LCFI132:
	movq	%rsp, %rbp
LCFI133:
	pushq	%rbx
LCFI134:
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	cmpq	$0, -32(%rbp)
	js	L233
	cmpq	$63, -32(%rbp)
	jg	L234
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L235
L234:
	movl	$0, %eax
L235:
	jmp	L236
L233:
	movq	-32(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L237
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L238
L237:
	movl	$0, %eax
L238:
L236:
	popq	%rbx
	leave
LCFI135:
	ret
LFE33:
.no_dead_strip _Shift__uShift_var_u8_C
_Shift__uShift_var_u8_C:
LFB34:
	pushq	%rbp
LCFI136:
	movq	%rsp, %rbp
LCFI137:
	pushq	%rbx
LCFI138:
	movq	160+_MM_Shift(%rip), %rax
	testq	%rax, %rax
	js	L240
	movq	160+_MM_Shift(%rip), %rax
	cmpq	$63, %rax
	jg	L241
	movzbl	168+_MM_Shift(%rip), %eax
	movzbl	%al, %edx
	movq	160+_MM_Shift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L242
L241:
	movl	$0, %eax
L242:
	jmp	L243
L240:
	movq	160+_MM_Shift(%rip), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L244
	movzbl	168+_MM_Shift(%rip), %eax
	movzbl	%al, %edx
	movq	160+_MM_Shift(%rip), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L245
L244:
	movl	$0, %eax
L245:
L243:
	popq	%rbx
	leave
LCFI139:
	ret
LFE34:
.no_dead_strip _Shift__uShift_param_u8_C
_Shift__uShift_param_u8_C:
LFB35:
	pushq	%rbp
LCFI140:
	movq	%rsp, %rbp
LCFI141:
	pushq	%rbx
LCFI142:
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	testq	%rax, %rax
	js	L247
	movq	-32(%rbp), %rax
	cmpq	$63, %rax
	jg	L248
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L249
L248:
	movl	$0, %eax
L249:
	jmp	L250
L247:
	movq	-32(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L251
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L252
L251:
	movl	$0, %eax
L252:
L250:
	popq	%rbx
	leave
LCFI143:
	ret
LFE35:
.no_dead_strip _Shift__uShift_var_i64_I
_Shift__uShift_var_i64_I:
LFB36:
	pushq	%rbp
LCFI144:
	movq	%rsp, %rbp
LCFI145:
	pushq	%rbx
LCFI146:
	movq	120+_MM_Shift(%rip), %rax
	testq	%rax, %rax
	js	L254
	movq	120+_MM_Shift(%rip), %rax
	cmpq	$63, %rax
	jg	L255
	movq	176+_MM_Shift(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Shift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L256
L255:
	movl	$0, %eax
L256:
	jmp	L257
L254:
	movq	120+_MM_Shift(%rip), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L258
	movq	176+_MM_Shift(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Shift(%rip), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L259
L258:
	movl	$0, %eax
L259:
L257:
	popq	%rbx
	leave
LCFI147:
	ret
LFE36:
.no_dead_strip _Shift__uShift_param_i64_I
_Shift__uShift_param_i64_I:
LFB37:
	pushq	%rbp
LCFI148:
	movq	%rsp, %rbp
LCFI149:
	pushq	%rbx
LCFI150:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	cmpq	$0, -32(%rbp)
	js	L261
	cmpq	$63, -32(%rbp)
	jg	L262
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L263
L262:
	movl	$0, %eax
L263:
	jmp	L264
L261:
	movq	-32(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L265
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L266
L265:
	movl	$0, %eax
L266:
L264:
	popq	%rbx
	leave
LCFI151:
	ret
LFE37:
.no_dead_strip _Shift__uShift_var_i64_C
_Shift__uShift_var_i64_C:
LFB38:
	pushq	%rbp
LCFI152:
	movq	%rsp, %rbp
LCFI153:
	pushq	%rbx
LCFI154:
	movq	160+_MM_Shift(%rip), %rax
	testq	%rax, %rax
	js	L268
	movq	160+_MM_Shift(%rip), %rax
	cmpq	$63, %rax
	jg	L269
	movq	176+_MM_Shift(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Shift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L270
L269:
	movl	$0, %eax
L270:
	jmp	L271
L268:
	movq	160+_MM_Shift(%rip), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L272
	movq	176+_MM_Shift(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Shift(%rip), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L273
L272:
	movl	$0, %eax
L273:
L271:
	popq	%rbx
	leave
LCFI155:
	ret
LFE38:
.no_dead_strip _Shift__uShift_param_i64_C
_Shift__uShift_param_i64_C:
LFB39:
	pushq	%rbp
LCFI156:
	movq	%rsp, %rbp
LCFI157:
	pushq	%rbx
LCFI158:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	testq	%rax, %rax
	js	L275
	movq	-32(%rbp), %rax
	cmpq	$63, %rax
	jg	L276
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L277
L276:
	movl	$0, %eax
L277:
	jmp	L278
L275:
	movq	-32(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L279
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L280
L279:
	movl	$0, %eax
L280:
L278:
	popq	%rbx
	leave
LCFI159:
	ret
LFE39:
.no_dead_strip _Shift__uShift_var_L_I
_Shift__uShift_var_L_I:
LFB40:
	pushq	%rbp
LCFI160:
	movq	%rsp, %rbp
LCFI161:
	pushq	%rbx
LCFI162:
	movq	120+_MM_Shift(%rip), %rax
	testq	%rax, %rax
	js	L282
	movq	120+_MM_Shift(%rip), %rax
	cmpq	$63, %rax
	jg	L283
	movq	192+_MM_Shift(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Shift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L284
L283:
	movl	$0, %eax
L284:
	jmp	L285
L282:
	movq	120+_MM_Shift(%rip), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L286
	movq	192+_MM_Shift(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Shift(%rip), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L287
L286:
	movl	$0, %eax
L287:
L285:
	popq	%rbx
	leave
LCFI163:
	ret
LFE40:
.no_dead_strip _Shift__uShift_param_L_I
_Shift__uShift_param_L_I:
LFB41:
	pushq	%rbp
LCFI164:
	movq	%rsp, %rbp
LCFI165:
	pushq	%rbx
LCFI166:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	cmpq	$0, -32(%rbp)
	js	L289
	cmpq	$63, -32(%rbp)
	jg	L290
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L291
L290:
	movl	$0, %eax
L291:
	jmp	L292
L289:
	movq	-32(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L293
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L294
L293:
	movl	$0, %eax
L294:
L292:
	popq	%rbx
	leave
LCFI167:
	ret
LFE41:
.no_dead_strip _Shift__uShift_var_L_C
_Shift__uShift_var_L_C:
LFB42:
	pushq	%rbp
LCFI168:
	movq	%rsp, %rbp
LCFI169:
	pushq	%rbx
LCFI170:
	movq	160+_MM_Shift(%rip), %rax
	testq	%rax, %rax
	js	L296
	movq	160+_MM_Shift(%rip), %rax
	cmpq	$63, %rax
	jg	L297
	movq	192+_MM_Shift(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Shift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L298
L297:
	movl	$0, %eax
L298:
	jmp	L299
L296:
	movq	160+_MM_Shift(%rip), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L300
	movq	192+_MM_Shift(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Shift(%rip), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L301
L300:
	movl	$0, %eax
L301:
L299:
	popq	%rbx
	leave
LCFI171:
	ret
LFE42:
.no_dead_strip _Shift__uShift_param_L_C
_Shift__uShift_param_L_C:
LFB43:
	pushq	%rbp
LCFI172:
	movq	%rsp, %rbp
LCFI173:
	pushq	%rbx
LCFI174:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	testq	%rax, %rax
	js	L303
	movq	-32(%rbp), %rax
	cmpq	$63, %rax
	jg	L304
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L305
L304:
	movl	$0, %eax
L305:
	jmp	L306
L303:
	movq	-32(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L307
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L308
L307:
	movl	$0, %eax
L308:
L306:
	popq	%rbx
	leave
LCFI175:
	ret
LFE43:
.no_dead_strip _Shift__uShift_var_i8_I
_Shift__uShift_var_i8_I:
LFB44:
	pushq	%rbp
LCFI176:
	movq	%rsp, %rbp
LCFI177:
	pushq	%rbx
LCFI178:
	movq	120+_MM_Shift(%rip), %rax
	testq	%rax, %rax
	js	L310
	movq	120+_MM_Shift(%rip), %rax
	cmpq	$63, %rax
	jg	L311
	movzbl	200+_MM_Shift(%rip), %eax
	movsbq	%al, %rdx
	movq	120+_MM_Shift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L312
L311:
	movl	$0, %eax
L312:
	jmp	L313
L310:
	movq	120+_MM_Shift(%rip), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L314
	movzbl	200+_MM_Shift(%rip), %eax
	movsbq	%al, %rdx
	movq	120+_MM_Shift(%rip), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L315
L314:
	movl	$0, %eax
L315:
L313:
	popq	%rbx
	leave
LCFI179:
	ret
LFE44:
.no_dead_strip _Shift__uShift_param_i8_I
_Shift__uShift_param_i8_I:
LFB45:
	pushq	%rbp
LCFI180:
	movq	%rsp, %rbp
LCFI181:
	pushq	%rbx
LCFI182:
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	cmpq	$0, -32(%rbp)
	js	L317
	cmpq	$63, -32(%rbp)
	jg	L318
	movsbq	-17(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L319
L318:
	movl	$0, %eax
L319:
	jmp	L320
L317:
	movq	-32(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L321
	movsbq	-17(%rbp), %rdx
	movq	-32(%rbp), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L322
L321:
	movl	$0, %eax
L322:
L320:
	popq	%rbx
	leave
LCFI183:
	ret
LFE45:
.no_dead_strip _Shift__uShift_var_i8_C
_Shift__uShift_var_i8_C:
LFB46:
	pushq	%rbp
LCFI184:
	movq	%rsp, %rbp
LCFI185:
	pushq	%rbx
LCFI186:
	movq	160+_MM_Shift(%rip), %rax
	testq	%rax, %rax
	js	L324
	movq	160+_MM_Shift(%rip), %rax
	cmpq	$63, %rax
	jg	L325
	movzbl	200+_MM_Shift(%rip), %eax
	movsbq	%al, %rdx
	movq	160+_MM_Shift(%rip), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L326
L325:
	movl	$0, %eax
L326:
	jmp	L327
L324:
	movq	160+_MM_Shift(%rip), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L328
	movzbl	200+_MM_Shift(%rip), %eax
	movsbq	%al, %rdx
	movq	160+_MM_Shift(%rip), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L329
L328:
	movl	$0, %eax
L329:
L327:
	popq	%rbx
	leave
LCFI187:
	ret
LFE46:
.no_dead_strip _Shift__uShift_param_i8_C
_Shift__uShift_param_i8_C:
LFB47:
	pushq	%rbp
LCFI188:
	movq	%rsp, %rbp
LCFI189:
	pushq	%rbx
LCFI190:
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	testq	%rax, %rax
	js	L331
	movq	-32(%rbp), %rax
	cmpq	$63, %rax
	jg	L332
	movsbq	-17(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	salq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L333
L332:
	movl	$0, %eax
L333:
	jmp	L334
L331:
	movq	-32(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L335
	movsbq	-17(%rbp), %rdx
	movq	-32(%rbp), %rax
	negq	%rax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	shrq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L336
L335:
	movl	$0, %eax
L336:
L334:
	popq	%rbx
	leave
LCFI191:
	ret
LFE47:
.globl _Shift_M3
.no_dead_strip _Shift_M3
_Shift_M3:
LFB48:
	pushq	%rbp
LCFI192:
	movq	%rsp, %rbp
LCFI193:
	movq	%rdi, -8(%rbp)
	leaq	_MM_Shift(%rip), %rax
	leave
LCFI194:
	ret
LFE48:
	.const_data
	.align 5
_L_1:
	.ascii "Shift_M3"
	.space 1
	.ascii "uShift_param_i8_C"
	.space 1
	.ascii "uShift_var_i8_C"
	.space 1
	.ascii "uShift_param_i8_I"
	.space 1
	.ascii "uShift_var_i8_I"
	.space 1
	.ascii "uShift_param_L_C"
	.space 1
	.ascii "uShift_var_L_C"
	.space 1
	.ascii "uShift_param_L_I"
	.space 1
	.ascii "uShift_var_L_I"
	.space 1
	.ascii "uShift_param_i64_C"
	.space 1
	.ascii "uShift_var_i64_C"
	.space 1
	.ascii "uShift_param_i64_I"
	.space 1
	.ascii "uShift_var_i64_I"
	.space 1
	.ascii "uShift_param_u8_C"
	.space 1
	.ascii "uShift_var_u8_C"
	.space 1
	.ascii "uShift_param_u8_I"
	.space 1
	.ascii "uShift_var_u8_I"
	.space 1
	.ascii "uShift_param_C_C"
	.space 1
	.ascii "uShift_var_C_C"
	.space 1
	.ascii "uShift_param_C_I"
	.space 1
	.ascii "uShift_var_C_I"
	.space 1
	.ascii "uShift_param_i32_C"
	.space 1
	.ascii "uShift_var_i32_C"
	.space 1
	.ascii "uShift_param_i32_I"
	.space 1
	.ascii "uShift_var_i32_I"
	.space 1
	.ascii "uShift_param_u64_C"
	.space 1
	.ascii "uShift_var_u64_C"
	.space 1
	.ascii "uShift_param_u64_I"
	.space 1
	.ascii "uShift_var_u64_I"
	.space 1
	.ascii "uShift_param_i16_C"
	.space 1
	.ascii "uShift_var_i16_C"
	.space 1
	.ascii "uShift_param_i16_I"
	.space 1
	.ascii "uShift_var_i16_I"
	.space 1
	.ascii "uShift_param_I_C"
	.space 1
	.ascii "uShift_var_I_C"
	.space 1
	.ascii "uShift_param_I_I"
	.space 1
	.ascii "uShift_var_I_I"
	.space 1
	.ascii "uShift_param_LC_C"
	.space 1
	.ascii "uShift_var_LC_C"
	.space 1
	.ascii "uShift_param_LC_I"
	.space 1
	.ascii "uShift_var_LC_I"
	.space 1
	.ascii "uShift_param_u32_C"
	.space 1
	.ascii "uShift_var_u32_C"
	.space 1
	.ascii "uShift_param_u32_I"
	.space 1
	.ascii "uShift_var_u32_I"
	.space 1
	.ascii "uShift_param_u16_C"
	.space 1
	.ascii "uShift_var_u16_C"
	.space 1
	.ascii "uShift_param_u16_I"
	.space 1
	.ascii "uShift_var_u16_I"
	.space 4
	.quad	_Shift_M3
	.quad	_L_1
	.quad	_Shift__uShift_param_i8_C
	.quad	_L_1+9
	.quad	_Shift__uShift_var_i8_C
	.quad	_L_1+27
	.quad	_Shift__uShift_param_i8_I
	.quad	_L_1+43
	.quad	_Shift__uShift_var_i8_I
	.quad	_L_1+61
	.quad	_Shift__uShift_param_L_C
	.quad	_L_1+77
	.quad	_Shift__uShift_var_L_C
	.quad	_L_1+94
	.quad	_Shift__uShift_param_L_I
	.quad	_L_1+109
	.quad	_Shift__uShift_var_L_I
	.quad	_L_1+126
	.quad	_Shift__uShift_param_i64_C
	.quad	_L_1+141
	.quad	_Shift__uShift_var_i64_C
	.quad	_L_1+160
	.quad	_Shift__uShift_param_i64_I
	.quad	_L_1+177
	.quad	_Shift__uShift_var_i64_I
	.quad	_L_1+196
	.quad	_Shift__uShift_param_u8_C
	.quad	_L_1+213
	.quad	_Shift__uShift_var_u8_C
	.quad	_L_1+231
	.quad	_Shift__uShift_param_u8_I
	.quad	_L_1+247
	.quad	_Shift__uShift_var_u8_I
	.quad	_L_1+265
	.quad	_Shift__uShift_param_C_C
	.quad	_L_1+281
	.quad	_Shift__uShift_var_C_C
	.quad	_L_1+298
	.quad	_Shift__uShift_param_C_I
	.quad	_L_1+313
	.quad	_Shift__uShift_var_C_I
	.quad	_L_1+330
	.quad	_Shift__uShift_param_i32_C
	.quad	_L_1+345
	.quad	_Shift__uShift_var_i32_C
	.quad	_L_1+364
	.quad	_Shift__uShift_param_i32_I
	.quad	_L_1+381
	.quad	_Shift__uShift_var_i32_I
	.quad	_L_1+400
	.quad	_Shift__uShift_param_u64_C
	.quad	_L_1+417
	.quad	_Shift__uShift_var_u64_C
	.quad	_L_1+436
	.quad	_Shift__uShift_param_u64_I
	.quad	_L_1+453
	.quad	_Shift__uShift_var_u64_I
	.quad	_L_1+472
	.quad	_Shift__uShift_param_i16_C
	.quad	_L_1+489
	.quad	_Shift__uShift_var_i16_C
	.quad	_L_1+508
	.quad	_Shift__uShift_param_i16_I
	.quad	_L_1+525
	.quad	_Shift__uShift_var_i16_I
	.quad	_L_1+544
	.quad	_Shift__uShift_param_I_C
	.quad	_L_1+561
	.quad	_Shift__uShift_var_I_C
	.quad	_L_1+578
	.quad	_Shift__uShift_param_I_I
	.quad	_L_1+593
	.quad	_Shift__uShift_var_I_I
	.quad	_L_1+610
	.quad	_Shift__uShift_param_LC_C
	.quad	_L_1+625
	.quad	_Shift__uShift_var_LC_C
	.quad	_L_1+643
	.quad	_Shift__uShift_param_LC_I
	.quad	_L_1+659
	.quad	_Shift__uShift_var_LC_I
	.quad	_L_1+677
	.quad	_Shift__uShift_param_u32_C
	.quad	_L_1+693
	.quad	_Shift__uShift_var_u32_C
	.quad	_L_1+712
	.quad	_Shift__uShift_param_u32_I
	.quad	_L_1+729
	.quad	_Shift__uShift_var_u32_I
	.quad	_L_1+748
	.quad	_Shift__uShift_param_u16_C
	.quad	_L_1+765
	.quad	_Shift__uShift_var_u16_C
	.quad	_L_1+784
	.quad	_Shift__uShift_param_u16_I
	.quad	_L_1+801
	.quad	_Shift__uShift_var_u16_I
	.quad	_L_1+820
	.space 8
	.ascii "../AMD64_DARWIN/Shift.m3"
	.space 8
	.data
	.align 5
_MM_Shift:
	.quad	_L_1+1632
	.space 32
	.quad	_L_1+840
	.space 24
	.quad	_MM_Shift+224
	.space 8
	.quad	_Shift_M3
	.quad	3
	.word	689
	.space 2
	.long	690
	.quad	691
	.quad	692
	.word	693
	.space 6
	.quad	694
	.long	1752346657
	.long	1082506641
	.long	697
	.space 4
	.quad	698
	.byte	-69
	.space 7
	.quad	700
	.long	1143958766
	.space 4
	.quad	703
	.byte	-64
	.space 31
	.quad	_Shift_I3
	.quad	_MM_Shift+248
	.space 8
	.quad	_Long_I3
	.quad	_MM_Shift+272
	.space 8
	.quad	_Word_I3
	.quad	_MM_Shift+296
	.space 8
	.quad	_Cstdint_I3
	.quad	_MM_Shift+320
	.space 8
	.quad	_RTHooks_I3
	.space 8
_Shift__uShift_var_u16_I.eh = 0
.no_dead_strip _Shift__uShift_var_u16_I.eh
_Shift__uShift_param_u16_I.eh = 0
.no_dead_strip _Shift__uShift_param_u16_I.eh
_Shift__uShift_var_u16_C.eh = 0
.no_dead_strip _Shift__uShift_var_u16_C.eh
_Shift__uShift_param_u16_C.eh = 0
.no_dead_strip _Shift__uShift_param_u16_C.eh
_Shift__uShift_var_u32_I.eh = 0
.no_dead_strip _Shift__uShift_var_u32_I.eh
_Shift__uShift_param_u32_I.eh = 0
.no_dead_strip _Shift__uShift_param_u32_I.eh
_Shift__uShift_var_u32_C.eh = 0
.no_dead_strip _Shift__uShift_var_u32_C.eh
_Shift__uShift_param_u32_C.eh = 0
.no_dead_strip _Shift__uShift_param_u32_C.eh
_Shift__uShift_var_LC_I.eh = 0
.no_dead_strip _Shift__uShift_var_LC_I.eh
_Shift__uShift_param_LC_I.eh = 0
.no_dead_strip _Shift__uShift_param_LC_I.eh
_Shift__uShift_var_LC_C.eh = 0
.no_dead_strip _Shift__uShift_var_LC_C.eh
_Shift__uShift_param_LC_C.eh = 0
.no_dead_strip _Shift__uShift_param_LC_C.eh
_Shift__uShift_var_I_I.eh = 0
.no_dead_strip _Shift__uShift_var_I_I.eh
_Shift__uShift_param_I_I.eh = 0
.no_dead_strip _Shift__uShift_param_I_I.eh
_Shift__uShift_var_I_C.eh = 0
.no_dead_strip _Shift__uShift_var_I_C.eh
_Shift__uShift_param_I_C.eh = 0
.no_dead_strip _Shift__uShift_param_I_C.eh
_Shift__uShift_var_i16_I.eh = 0
.no_dead_strip _Shift__uShift_var_i16_I.eh
_Shift__uShift_param_i16_I.eh = 0
.no_dead_strip _Shift__uShift_param_i16_I.eh
_Shift__uShift_var_i16_C.eh = 0
.no_dead_strip _Shift__uShift_var_i16_C.eh
_Shift__uShift_param_i16_C.eh = 0
.no_dead_strip _Shift__uShift_param_i16_C.eh
_Shift__uShift_var_u64_I.eh = 0
.no_dead_strip _Shift__uShift_var_u64_I.eh
_Shift__uShift_param_u64_I.eh = 0
.no_dead_strip _Shift__uShift_param_u64_I.eh
_Shift__uShift_var_u64_C.eh = 0
.no_dead_strip _Shift__uShift_var_u64_C.eh
_Shift__uShift_param_u64_C.eh = 0
.no_dead_strip _Shift__uShift_param_u64_C.eh
_Shift__uShift_var_i32_I.eh = 0
.no_dead_strip _Shift__uShift_var_i32_I.eh
_Shift__uShift_param_i32_I.eh = 0
.no_dead_strip _Shift__uShift_param_i32_I.eh
_Shift__uShift_var_i32_C.eh = 0
.no_dead_strip _Shift__uShift_var_i32_C.eh
_Shift__uShift_param_i32_C.eh = 0
.no_dead_strip _Shift__uShift_param_i32_C.eh
_Shift__uShift_var_C_I.eh = 0
.no_dead_strip _Shift__uShift_var_C_I.eh
_Shift__uShift_param_C_I.eh = 0
.no_dead_strip _Shift__uShift_param_C_I.eh
_Shift__uShift_var_C_C.eh = 0
.no_dead_strip _Shift__uShift_var_C_C.eh
_Shift__uShift_param_C_C.eh = 0
.no_dead_strip _Shift__uShift_param_C_C.eh
_Shift__uShift_var_u8_I.eh = 0
.no_dead_strip _Shift__uShift_var_u8_I.eh
_Shift__uShift_param_u8_I.eh = 0
.no_dead_strip _Shift__uShift_param_u8_I.eh
_Shift__uShift_var_u8_C.eh = 0
.no_dead_strip _Shift__uShift_var_u8_C.eh
_Shift__uShift_param_u8_C.eh = 0
.no_dead_strip _Shift__uShift_param_u8_C.eh
_Shift__uShift_var_i64_I.eh = 0
.no_dead_strip _Shift__uShift_var_i64_I.eh
_Shift__uShift_param_i64_I.eh = 0
.no_dead_strip _Shift__uShift_param_i64_I.eh
_Shift__uShift_var_i64_C.eh = 0
.no_dead_strip _Shift__uShift_var_i64_C.eh
_Shift__uShift_param_i64_C.eh = 0
.no_dead_strip _Shift__uShift_param_i64_C.eh
_Shift__uShift_var_L_I.eh = 0
.no_dead_strip _Shift__uShift_var_L_I.eh
_Shift__uShift_param_L_I.eh = 0
.no_dead_strip _Shift__uShift_param_L_I.eh
_Shift__uShift_var_L_C.eh = 0
.no_dead_strip _Shift__uShift_var_L_C.eh
_Shift__uShift_param_L_C.eh = 0
.no_dead_strip _Shift__uShift_param_L_C.eh
_Shift__uShift_var_i8_I.eh = 0
.no_dead_strip _Shift__uShift_var_i8_I.eh
_Shift__uShift_param_i8_I.eh = 0
.no_dead_strip _Shift__uShift_param_i8_I.eh
_Shift__uShift_var_i8_C.eh = 0
.no_dead_strip _Shift__uShift_var_i8_C.eh
_Shift__uShift_param_i8_C.eh = 0
.no_dead_strip _Shift__uShift_param_i8_C.eh
.globl _Shift_M3.eh
_Shift_M3.eh = 0
.no_dead_strip _Shift_M3.eh
	.subsections_via_symbols
