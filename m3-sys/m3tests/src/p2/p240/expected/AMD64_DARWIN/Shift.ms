	.text
.globl _Shift__uShift_var_u16_I
	.private_extern _Shift__uShift_var_u16_I
_Shift__uShift_var_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Shift(%rip), %rax
	movq	%rax, -16(%rbp)
	movzwl	104+_MM_Shift(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L2
	cmpq	$63, -16(%rbp)
	jg	L3
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -48(%rbp)
	jmp	L4
L3:
	movq	$0, -48(%rbp)
L4:
	movq	-48(%rbp), %rax
	movq	%rax, -56(%rbp)
	jmp	L5
L2:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L6
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L7
L6:
	movq	$0, -40(%rbp)
L7:
	movq	-40(%rbp), %rax
	movq	%rax, -56(%rbp)
L5:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_param_u16_I
	.private_extern _Shift__uShift_param_u16_I
_Shift__uShift_param_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -34(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	movswq	-34(%rbp),%rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L10
	cmpq	$63, -16(%rbp)
	jg	L11
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -64(%rbp)
	jmp	L12
L11:
	movq	$0, -64(%rbp)
L12:
	movq	-64(%rbp), %rax
	movq	%rax, -72(%rbp)
	jmp	L13
L10:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L14
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L15
L14:
	movq	$0, -56(%rbp)
L15:
	movq	-56(%rbp), %rax
	movq	%rax, -72(%rbp)
L13:
	movq	-72(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_var_u16_C
	.private_extern _Shift__uShift_var_u16_C
_Shift__uShift_var_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Shift(%rip), %rax
	movq	%rax, -16(%rbp)
	movzwl	104+_MM_Shift(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L18
	cmpq	$63, -16(%rbp)
	jg	L19
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -48(%rbp)
	jmp	L20
L19:
	movq	$0, -48(%rbp)
L20:
	movq	-48(%rbp), %rax
	movq	%rax, -56(%rbp)
	jmp	L21
L18:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L22
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L23
L22:
	movq	$0, -40(%rbp)
L23:
	movq	-40(%rbp), %rax
	movq	%rax, -56(%rbp)
L21:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_param_u16_C
	.private_extern _Shift__uShift_param_u16_C
_Shift__uShift_param_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -34(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	movswq	-34(%rbp),%rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L26
	cmpq	$63, -16(%rbp)
	jg	L27
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -64(%rbp)
	jmp	L28
L27:
	movq	$0, -64(%rbp)
L28:
	movq	-64(%rbp), %rax
	movq	%rax, -72(%rbp)
	jmp	L29
L26:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L30
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L31
L30:
	movq	$0, -56(%rbp)
L31:
	movq	-56(%rbp), %rax
	movq	%rax, -72(%rbp)
L29:
	movq	-72(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_var_u64_I
	.private_extern _Shift__uShift_var_u64_I
_Shift__uShift_var_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Shift(%rip), %rax
	movq	%rax, -16(%rbp)
	movq	112+_MM_Shift(%rip), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L34
	cmpq	$63, -16(%rbp)
	jg	L35
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -48(%rbp)
	jmp	L36
L35:
	movq	$0, -48(%rbp)
L36:
	movq	-48(%rbp), %rax
	movq	%rax, -56(%rbp)
	jmp	L37
L34:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L38
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L39
L38:
	movq	$0, -40(%rbp)
L39:
	movq	-40(%rbp), %rax
	movq	%rax, -56(%rbp)
L37:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_param_u64_I
	.private_extern _Shift__uShift_param_u64_I
_Shift__uShift_param_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -40(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	movq	-40(%rbp), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L42
	cmpq	$63, -16(%rbp)
	jg	L43
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -64(%rbp)
	jmp	L44
L43:
	movq	$0, -64(%rbp)
L44:
	movq	-64(%rbp), %rax
	movq	%rax, -72(%rbp)
	jmp	L45
L42:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L46
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L47
L46:
	movq	$0, -56(%rbp)
L47:
	movq	-56(%rbp), %rax
	movq	%rax, -72(%rbp)
L45:
	movq	-72(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_var_u64_C
	.private_extern _Shift__uShift_var_u64_C
_Shift__uShift_var_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Shift(%rip), %rax
	movq	%rax, -16(%rbp)
	movq	112+_MM_Shift(%rip), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L50
	cmpq	$63, -16(%rbp)
	jg	L51
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -48(%rbp)
	jmp	L52
L51:
	movq	$0, -48(%rbp)
L52:
	movq	-48(%rbp), %rax
	movq	%rax, -56(%rbp)
	jmp	L53
L50:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L54
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L55
L54:
	movq	$0, -40(%rbp)
L55:
	movq	-40(%rbp), %rax
	movq	%rax, -56(%rbp)
L53:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_param_u64_C
	.private_extern _Shift__uShift_param_u64_C
_Shift__uShift_param_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -40(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	movq	-40(%rbp), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L58
	cmpq	$63, -16(%rbp)
	jg	L59
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -64(%rbp)
	jmp	L60
L59:
	movq	$0, -64(%rbp)
L60:
	movq	-64(%rbp), %rax
	movq	%rax, -72(%rbp)
	jmp	L61
L58:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L62
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L63
L62:
	movq	$0, -56(%rbp)
L63:
	movq	-56(%rbp), %rax
	movq	%rax, -72(%rbp)
L61:
	movq	-72(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_var_LC_I
	.private_extern _Shift__uShift_var_LC_I
_Shift__uShift_var_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Shift(%rip), %rax
	movq	%rax, -16(%rbp)
	movq	120+_MM_Shift(%rip), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L66
	cmpq	$63, -16(%rbp)
	jg	L67
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -48(%rbp)
	jmp	L68
L67:
	movq	$0, -48(%rbp)
L68:
	movq	-48(%rbp), %rax
	movq	%rax, -56(%rbp)
	jmp	L69
L66:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L70
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L71
L70:
	movq	$0, -40(%rbp)
L71:
	movq	-40(%rbp), %rax
	movq	%rax, -56(%rbp)
L69:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_param_LC_I
	.private_extern _Shift__uShift_param_LC_I
_Shift__uShift_param_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -40(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	movq	-40(%rbp), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L74
	cmpq	$63, -16(%rbp)
	jg	L75
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -64(%rbp)
	jmp	L76
L75:
	movq	$0, -64(%rbp)
L76:
	movq	-64(%rbp), %rax
	movq	%rax, -72(%rbp)
	jmp	L77
L74:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L78
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L79
L78:
	movq	$0, -56(%rbp)
L79:
	movq	-56(%rbp), %rax
	movq	%rax, -72(%rbp)
L77:
	movq	-72(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_var_LC_C
	.private_extern _Shift__uShift_var_LC_C
_Shift__uShift_var_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Shift(%rip), %rax
	movq	%rax, -16(%rbp)
	movq	120+_MM_Shift(%rip), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L82
	cmpq	$63, -16(%rbp)
	jg	L83
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -48(%rbp)
	jmp	L84
L83:
	movq	$0, -48(%rbp)
L84:
	movq	-48(%rbp), %rax
	movq	%rax, -56(%rbp)
	jmp	L85
L82:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L86
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L87
L86:
	movq	$0, -40(%rbp)
L87:
	movq	-40(%rbp), %rax
	movq	%rax, -56(%rbp)
L85:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_param_LC_C
	.private_extern _Shift__uShift_param_LC_C
_Shift__uShift_param_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -40(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	movq	-40(%rbp), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L90
	cmpq	$63, -16(%rbp)
	jg	L91
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -64(%rbp)
	jmp	L92
L91:
	movq	$0, -64(%rbp)
L92:
	movq	-64(%rbp), %rax
	movq	%rax, -72(%rbp)
	jmp	L93
L90:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L94
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L95
L94:
	movq	$0, -56(%rbp)
L95:
	movq	-56(%rbp), %rax
	movq	%rax, -72(%rbp)
L93:
	movq	-72(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_var_i16_I
	.private_extern _Shift__uShift_var_i16_I
_Shift__uShift_var_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Shift(%rip), %rax
	movq	%rax, -16(%rbp)
	movzwl	136+_MM_Shift(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L98
	cmpq	$63, -16(%rbp)
	jg	L99
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -48(%rbp)
	jmp	L100
L99:
	movq	$0, -48(%rbp)
L100:
	movq	-48(%rbp), %rax
	movq	%rax, -56(%rbp)
	jmp	L101
L98:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L102
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L103
L102:
	movq	$0, -40(%rbp)
L103:
	movq	-40(%rbp), %rax
	movq	%rax, -56(%rbp)
L101:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_param_i16_I
	.private_extern _Shift__uShift_param_i16_I
_Shift__uShift_param_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -34(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	movzwl	-34(%rbp), %eax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L106
	cmpq	$63, -16(%rbp)
	jg	L107
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -64(%rbp)
	jmp	L108
L107:
	movq	$0, -64(%rbp)
L108:
	movq	-64(%rbp), %rax
	movq	%rax, -72(%rbp)
	jmp	L109
L106:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L110
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L111
L110:
	movq	$0, -56(%rbp)
L111:
	movq	-56(%rbp), %rax
	movq	%rax, -72(%rbp)
L109:
	movq	-72(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_var_i16_C
	.private_extern _Shift__uShift_var_i16_C
_Shift__uShift_var_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Shift(%rip), %rax
	movq	%rax, -16(%rbp)
	movzwl	136+_MM_Shift(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L114
	cmpq	$63, -16(%rbp)
	jg	L115
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -48(%rbp)
	jmp	L116
L115:
	movq	$0, -48(%rbp)
L116:
	movq	-48(%rbp), %rax
	movq	%rax, -56(%rbp)
	jmp	L117
L114:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L118
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L119
L118:
	movq	$0, -40(%rbp)
L119:
	movq	-40(%rbp), %rax
	movq	%rax, -56(%rbp)
L117:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_param_i16_C
	.private_extern _Shift__uShift_param_i16_C
_Shift__uShift_param_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -34(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	movzwl	-34(%rbp), %eax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L122
	cmpq	$63, -16(%rbp)
	jg	L123
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -64(%rbp)
	jmp	L124
L123:
	movq	$0, -64(%rbp)
L124:
	movq	-64(%rbp), %rax
	movq	%rax, -72(%rbp)
	jmp	L125
L122:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L126
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L127
L126:
	movq	$0, -56(%rbp)
L127:
	movq	-56(%rbp), %rax
	movq	%rax, -72(%rbp)
L125:
	movq	-72(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_var_i32_I
	.private_extern _Shift__uShift_var_i32_I
_Shift__uShift_var_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Shift(%rip), %rax
	movq	%rax, -16(%rbp)
	movl	140+_MM_Shift(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L130
	cmpq	$63, -16(%rbp)
	jg	L131
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -48(%rbp)
	jmp	L132
L131:
	movq	$0, -48(%rbp)
L132:
	movq	-48(%rbp), %rax
	movq	%rax, -56(%rbp)
	jmp	L133
L130:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L134
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L135
L134:
	movq	$0, -40(%rbp)
L135:
	movq	-40(%rbp), %rax
	movq	%rax, -56(%rbp)
L133:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_param_i32_I
	.private_extern _Shift__uShift_param_i32_I
_Shift__uShift_param_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -36(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	mov	-36(%rbp), %eax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L138
	cmpq	$63, -16(%rbp)
	jg	L139
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -64(%rbp)
	jmp	L140
L139:
	movq	$0, -64(%rbp)
L140:
	movq	-64(%rbp), %rax
	movq	%rax, -72(%rbp)
	jmp	L141
L138:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L142
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L143
L142:
	movq	$0, -56(%rbp)
L143:
	movq	-56(%rbp), %rax
	movq	%rax, -72(%rbp)
L141:
	movq	-72(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_var_i32_C
	.private_extern _Shift__uShift_var_i32_C
_Shift__uShift_var_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Shift(%rip), %rax
	movq	%rax, -16(%rbp)
	movl	140+_MM_Shift(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L146
	cmpq	$63, -16(%rbp)
	jg	L147
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -48(%rbp)
	jmp	L148
L147:
	movq	$0, -48(%rbp)
L148:
	movq	-48(%rbp), %rax
	movq	%rax, -56(%rbp)
	jmp	L149
L146:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L150
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L151
L150:
	movq	$0, -40(%rbp)
L151:
	movq	-40(%rbp), %rax
	movq	%rax, -56(%rbp)
L149:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_param_i32_C
	.private_extern _Shift__uShift_param_i32_C
_Shift__uShift_param_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -36(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	mov	-36(%rbp), %eax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L154
	cmpq	$63, -16(%rbp)
	jg	L155
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -64(%rbp)
	jmp	L156
L155:
	movq	$0, -64(%rbp)
L156:
	movq	-64(%rbp), %rax
	movq	%rax, -72(%rbp)
	jmp	L157
L154:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L158
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L159
L158:
	movq	$0, -56(%rbp)
L159:
	movq	-56(%rbp), %rax
	movq	%rax, -72(%rbp)
L157:
	movq	-72(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_var_I_I
	.private_extern _Shift__uShift_var_I_I
_Shift__uShift_var_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Shift(%rip), %rax
	movq	%rax, -16(%rbp)
	movq	144+_MM_Shift(%rip), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L162
	cmpq	$63, -16(%rbp)
	jg	L163
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -48(%rbp)
	jmp	L164
L163:
	movq	$0, -48(%rbp)
L164:
	movq	-48(%rbp), %rax
	movq	%rax, -56(%rbp)
	jmp	L165
L162:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L166
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L167
L166:
	movq	$0, -40(%rbp)
L167:
	movq	-40(%rbp), %rax
	movq	%rax, -56(%rbp)
L165:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_param_I_I
	.private_extern _Shift__uShift_param_I_I
_Shift__uShift_param_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -40(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	movq	-40(%rbp), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L170
	cmpq	$63, -16(%rbp)
	jg	L171
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -64(%rbp)
	jmp	L172
L171:
	movq	$0, -64(%rbp)
L172:
	movq	-64(%rbp), %rax
	movq	%rax, -72(%rbp)
	jmp	L173
L170:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L174
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L175
L174:
	movq	$0, -56(%rbp)
L175:
	movq	-56(%rbp), %rax
	movq	%rax, -72(%rbp)
L173:
	movq	-72(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_var_I_C
	.private_extern _Shift__uShift_var_I_C
_Shift__uShift_var_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Shift(%rip), %rax
	movq	%rax, -16(%rbp)
	movq	144+_MM_Shift(%rip), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L178
	cmpq	$63, -16(%rbp)
	jg	L179
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -48(%rbp)
	jmp	L180
L179:
	movq	$0, -48(%rbp)
L180:
	movq	-48(%rbp), %rax
	movq	%rax, -56(%rbp)
	jmp	L181
L178:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L182
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L183
L182:
	movq	$0, -40(%rbp)
L183:
	movq	-40(%rbp), %rax
	movq	%rax, -56(%rbp)
L181:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_param_I_C
	.private_extern _Shift__uShift_param_I_C
_Shift__uShift_param_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -40(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	movq	-40(%rbp), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L186
	cmpq	$63, -16(%rbp)
	jg	L187
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -64(%rbp)
	jmp	L188
L187:
	movq	$0, -64(%rbp)
L188:
	movq	-64(%rbp), %rax
	movq	%rax, -72(%rbp)
	jmp	L189
L186:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L190
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L191
L190:
	movq	$0, -56(%rbp)
L191:
	movq	-56(%rbp), %rax
	movq	%rax, -72(%rbp)
L189:
	movq	-72(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_var_i64_I
	.private_extern _Shift__uShift_var_i64_I
_Shift__uShift_var_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Shift(%rip), %rax
	movq	%rax, -16(%rbp)
	movq	152+_MM_Shift(%rip), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L194
	cmpq	$63, -16(%rbp)
	jg	L195
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -48(%rbp)
	jmp	L196
L195:
	movq	$0, -48(%rbp)
L196:
	movq	-48(%rbp), %rax
	movq	%rax, -56(%rbp)
	jmp	L197
L194:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L198
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L199
L198:
	movq	$0, -40(%rbp)
L199:
	movq	-40(%rbp), %rax
	movq	%rax, -56(%rbp)
L197:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_param_i64_I
	.private_extern _Shift__uShift_param_i64_I
_Shift__uShift_param_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -40(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	movq	-40(%rbp), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L202
	cmpq	$63, -16(%rbp)
	jg	L203
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -64(%rbp)
	jmp	L204
L203:
	movq	$0, -64(%rbp)
L204:
	movq	-64(%rbp), %rax
	movq	%rax, -72(%rbp)
	jmp	L205
L202:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L206
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L207
L206:
	movq	$0, -56(%rbp)
L207:
	movq	-56(%rbp), %rax
	movq	%rax, -72(%rbp)
L205:
	movq	-72(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_var_i64_C
	.private_extern _Shift__uShift_var_i64_C
_Shift__uShift_var_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Shift(%rip), %rax
	movq	%rax, -16(%rbp)
	movq	152+_MM_Shift(%rip), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L210
	cmpq	$63, -16(%rbp)
	jg	L211
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -48(%rbp)
	jmp	L212
L211:
	movq	$0, -48(%rbp)
L212:
	movq	-48(%rbp), %rax
	movq	%rax, -56(%rbp)
	jmp	L213
L210:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L214
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L215
L214:
	movq	$0, -40(%rbp)
L215:
	movq	-40(%rbp), %rax
	movq	%rax, -56(%rbp)
L213:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_param_i64_C
	.private_extern _Shift__uShift_param_i64_C
_Shift__uShift_param_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -40(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	movq	-40(%rbp), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L218
	cmpq	$63, -16(%rbp)
	jg	L219
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -64(%rbp)
	jmp	L220
L219:
	movq	$0, -64(%rbp)
L220:
	movq	-64(%rbp), %rax
	movq	%rax, -72(%rbp)
	jmp	L221
L218:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L222
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L223
L222:
	movq	$0, -56(%rbp)
L223:
	movq	-56(%rbp), %rax
	movq	%rax, -72(%rbp)
L221:
	movq	-72(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_var_C_I
	.private_extern _Shift__uShift_var_C_I
_Shift__uShift_var_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Shift(%rip), %rax
	movq	%rax, -16(%rbp)
	movq	160+_MM_Shift(%rip), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L226
	cmpq	$63, -16(%rbp)
	jg	L227
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -48(%rbp)
	jmp	L228
L227:
	movq	$0, -48(%rbp)
L228:
	movq	-48(%rbp), %rax
	movq	%rax, -56(%rbp)
	jmp	L229
L226:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L230
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L231
L230:
	movq	$0, -40(%rbp)
L231:
	movq	-40(%rbp), %rax
	movq	%rax, -56(%rbp)
L229:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_param_C_I
	.private_extern _Shift__uShift_param_C_I
_Shift__uShift_param_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -40(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	movq	-40(%rbp), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L234
	cmpq	$63, -16(%rbp)
	jg	L235
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -64(%rbp)
	jmp	L236
L235:
	movq	$0, -64(%rbp)
L236:
	movq	-64(%rbp), %rax
	movq	%rax, -72(%rbp)
	jmp	L237
L234:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L238
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L239
L238:
	movq	$0, -56(%rbp)
L239:
	movq	-56(%rbp), %rax
	movq	%rax, -72(%rbp)
L237:
	movq	-72(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_var_C_C
	.private_extern _Shift__uShift_var_C_C
_Shift__uShift_var_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Shift(%rip), %rax
	movq	%rax, -16(%rbp)
	movq	160+_MM_Shift(%rip), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L242
	cmpq	$63, -16(%rbp)
	jg	L243
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -48(%rbp)
	jmp	L244
L243:
	movq	$0, -48(%rbp)
L244:
	movq	-48(%rbp), %rax
	movq	%rax, -56(%rbp)
	jmp	L245
L242:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L246
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L247
L246:
	movq	$0, -40(%rbp)
L247:
	movq	-40(%rbp), %rax
	movq	%rax, -56(%rbp)
L245:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_param_C_C
	.private_extern _Shift__uShift_param_C_C
_Shift__uShift_param_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -40(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	movq	-40(%rbp), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L250
	cmpq	$63, -16(%rbp)
	jg	L251
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -64(%rbp)
	jmp	L252
L251:
	movq	$0, -64(%rbp)
L252:
	movq	-64(%rbp), %rax
	movq	%rax, -72(%rbp)
	jmp	L253
L250:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L254
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L255
L254:
	movq	$0, -56(%rbp)
L255:
	movq	-56(%rbp), %rax
	movq	%rax, -72(%rbp)
L253:
	movq	-72(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_var_u8_I
	.private_extern _Shift__uShift_var_u8_I
_Shift__uShift_var_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Shift(%rip), %rax
	movq	%rax, -16(%rbp)
	movzbl	172+_MM_Shift(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L258
	cmpq	$63, -16(%rbp)
	jg	L259
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -48(%rbp)
	jmp	L260
L259:
	movq	$0, -48(%rbp)
L260:
	movq	-48(%rbp), %rax
	movq	%rax, -56(%rbp)
	jmp	L261
L258:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L262
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L263
L262:
	movq	$0, -40(%rbp)
L263:
	movq	-40(%rbp), %rax
	movq	%rax, -56(%rbp)
L261:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_param_u8_I
	.private_extern _Shift__uShift_param_u8_I
_Shift__uShift_param_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -33(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	movsbq	-33(%rbp),%rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L266
	cmpq	$63, -16(%rbp)
	jg	L267
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -64(%rbp)
	jmp	L268
L267:
	movq	$0, -64(%rbp)
L268:
	movq	-64(%rbp), %rax
	movq	%rax, -72(%rbp)
	jmp	L269
L266:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L270
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L271
L270:
	movq	$0, -56(%rbp)
L271:
	movq	-56(%rbp), %rax
	movq	%rax, -72(%rbp)
L269:
	movq	-72(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_var_u8_C
	.private_extern _Shift__uShift_var_u8_C
_Shift__uShift_var_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Shift(%rip), %rax
	movq	%rax, -16(%rbp)
	movzbl	172+_MM_Shift(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L274
	cmpq	$63, -16(%rbp)
	jg	L275
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -48(%rbp)
	jmp	L276
L275:
	movq	$0, -48(%rbp)
L276:
	movq	-48(%rbp), %rax
	movq	%rax, -56(%rbp)
	jmp	L277
L274:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L278
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L279
L278:
	movq	$0, -40(%rbp)
L279:
	movq	-40(%rbp), %rax
	movq	%rax, -56(%rbp)
L277:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_param_u8_C
	.private_extern _Shift__uShift_param_u8_C
_Shift__uShift_param_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -33(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	movsbq	-33(%rbp),%rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L282
	cmpq	$63, -16(%rbp)
	jg	L283
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -64(%rbp)
	jmp	L284
L283:
	movq	$0, -64(%rbp)
L284:
	movq	-64(%rbp), %rax
	movq	%rax, -72(%rbp)
	jmp	L285
L282:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L286
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L287
L286:
	movq	$0, -56(%rbp)
L287:
	movq	-56(%rbp), %rax
	movq	%rax, -72(%rbp)
L285:
	movq	-72(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_var_i8_I
	.private_extern _Shift__uShift_var_i8_I
_Shift__uShift_var_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Shift(%rip), %rax
	movq	%rax, -16(%rbp)
	movzbl	173+_MM_Shift(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L290
	cmpq	$63, -16(%rbp)
	jg	L291
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -48(%rbp)
	jmp	L292
L291:
	movq	$0, -48(%rbp)
L292:
	movq	-48(%rbp), %rax
	movq	%rax, -56(%rbp)
	jmp	L293
L290:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L294
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L295
L294:
	movq	$0, -40(%rbp)
L295:
	movq	-40(%rbp), %rax
	movq	%rax, -56(%rbp)
L293:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_param_i8_I
	.private_extern _Shift__uShift_param_i8_I
_Shift__uShift_param_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -33(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	movzbl	-33(%rbp), %eax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L298
	cmpq	$63, -16(%rbp)
	jg	L299
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -64(%rbp)
	jmp	L300
L299:
	movq	$0, -64(%rbp)
L300:
	movq	-64(%rbp), %rax
	movq	%rax, -72(%rbp)
	jmp	L301
L298:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L302
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L303
L302:
	movq	$0, -56(%rbp)
L303:
	movq	-56(%rbp), %rax
	movq	%rax, -72(%rbp)
L301:
	movq	-72(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_var_i8_C
	.private_extern _Shift__uShift_var_i8_C
_Shift__uShift_var_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Shift(%rip), %rax
	movq	%rax, -16(%rbp)
	movzbl	173+_MM_Shift(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L306
	cmpq	$63, -16(%rbp)
	jg	L307
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -48(%rbp)
	jmp	L308
L307:
	movq	$0, -48(%rbp)
L308:
	movq	-48(%rbp), %rax
	movq	%rax, -56(%rbp)
	jmp	L309
L306:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L310
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L311
L310:
	movq	$0, -40(%rbp)
L311:
	movq	-40(%rbp), %rax
	movq	%rax, -56(%rbp)
L309:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_param_i8_C
	.private_extern _Shift__uShift_param_i8_C
_Shift__uShift_param_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -33(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	movzbl	-33(%rbp), %eax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L314
	cmpq	$63, -16(%rbp)
	jg	L315
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -64(%rbp)
	jmp	L316
L315:
	movq	$0, -64(%rbp)
L316:
	movq	-64(%rbp), %rax
	movq	%rax, -72(%rbp)
	jmp	L317
L314:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L318
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L319
L318:
	movq	$0, -56(%rbp)
L319:
	movq	-56(%rbp), %rax
	movq	%rax, -72(%rbp)
L317:
	movq	-72(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_var_L_I
	.private_extern _Shift__uShift_var_L_I
_Shift__uShift_var_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Shift(%rip), %rax
	movq	%rax, -16(%rbp)
	movq	176+_MM_Shift(%rip), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L322
	cmpq	$63, -16(%rbp)
	jg	L323
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -48(%rbp)
	jmp	L324
L323:
	movq	$0, -48(%rbp)
L324:
	movq	-48(%rbp), %rax
	movq	%rax, -56(%rbp)
	jmp	L325
L322:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L326
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L327
L326:
	movq	$0, -40(%rbp)
L327:
	movq	-40(%rbp), %rax
	movq	%rax, -56(%rbp)
L325:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_param_L_I
	.private_extern _Shift__uShift_param_L_I
_Shift__uShift_param_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -40(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	movq	-40(%rbp), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L330
	cmpq	$63, -16(%rbp)
	jg	L331
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -64(%rbp)
	jmp	L332
L331:
	movq	$0, -64(%rbp)
L332:
	movq	-64(%rbp), %rax
	movq	%rax, -72(%rbp)
	jmp	L333
L330:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L334
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L335
L334:
	movq	$0, -56(%rbp)
L335:
	movq	-56(%rbp), %rax
	movq	%rax, -72(%rbp)
L333:
	movq	-72(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_var_L_C
	.private_extern _Shift__uShift_var_L_C
_Shift__uShift_var_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Shift(%rip), %rax
	movq	%rax, -16(%rbp)
	movq	176+_MM_Shift(%rip), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L338
	cmpq	$63, -16(%rbp)
	jg	L339
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -48(%rbp)
	jmp	L340
L339:
	movq	$0, -48(%rbp)
L340:
	movq	-48(%rbp), %rax
	movq	%rax, -56(%rbp)
	jmp	L341
L338:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L342
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L343
L342:
	movq	$0, -40(%rbp)
L343:
	movq	-40(%rbp), %rax
	movq	%rax, -56(%rbp)
L341:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_param_L_C
	.private_extern _Shift__uShift_param_L_C
_Shift__uShift_param_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -40(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	movq	-40(%rbp), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L346
	cmpq	$63, -16(%rbp)
	jg	L347
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -64(%rbp)
	jmp	L348
L347:
	movq	$0, -64(%rbp)
L348:
	movq	-64(%rbp), %rax
	movq	%rax, -72(%rbp)
	jmp	L349
L346:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L350
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L351
L350:
	movq	$0, -56(%rbp)
L351:
	movq	-56(%rbp), %rax
	movq	%rax, -72(%rbp)
L349:
	movq	-72(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_var_u32_I
	.private_extern _Shift__uShift_var_u32_I
_Shift__uShift_var_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Shift(%rip), %rax
	movq	%rax, -16(%rbp)
	movl	184+_MM_Shift(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L354
	cmpq	$63, -16(%rbp)
	jg	L355
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -48(%rbp)
	jmp	L356
L355:
	movq	$0, -48(%rbp)
L356:
	movq	-48(%rbp), %rax
	movq	%rax, -56(%rbp)
	jmp	L357
L354:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L358
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L359
L358:
	movq	$0, -40(%rbp)
L359:
	movq	-40(%rbp), %rax
	movq	%rax, -56(%rbp)
L357:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_param_u32_I
	.private_extern _Shift__uShift_param_u32_I
_Shift__uShift_param_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -36(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	movl	-36(%rbp), %eax
	cltq
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L362
	cmpq	$63, -16(%rbp)
	jg	L363
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -64(%rbp)
	jmp	L364
L363:
	movq	$0, -64(%rbp)
L364:
	movq	-64(%rbp), %rax
	movq	%rax, -72(%rbp)
	jmp	L365
L362:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L366
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L367
L366:
	movq	$0, -56(%rbp)
L367:
	movq	-56(%rbp), %rax
	movq	%rax, -72(%rbp)
L365:
	movq	-72(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_var_u32_C
	.private_extern _Shift__uShift_var_u32_C
_Shift__uShift_var_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Shift(%rip), %rax
	movq	%rax, -16(%rbp)
	movl	184+_MM_Shift(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L370
	cmpq	$63, -16(%rbp)
	jg	L371
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -48(%rbp)
	jmp	L372
L371:
	movq	$0, -48(%rbp)
L372:
	movq	-48(%rbp), %rax
	movq	%rax, -56(%rbp)
	jmp	L373
L370:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L374
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L375
L374:
	movq	$0, -40(%rbp)
L375:
	movq	-40(%rbp), %rax
	movq	%rax, -56(%rbp)
L373:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Shift__uShift_param_u32_C
	.private_extern _Shift__uShift_param_u32_C
_Shift__uShift_param_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -36(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	movl	-36(%rbp), %eax
	cltq
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L378
	cmpq	$63, -16(%rbp)
	jg	L379
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -64(%rbp)
	jmp	L380
L379:
	movq	$0, -64(%rbp)
L380:
	movq	-64(%rbp), %rax
	movq	%rax, -72(%rbp)
	jmp	L381
L378:
	movq	-16(%rbp), %rax
	negq	%rax
	cmpq	$63, %rax
	jg	L382
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	negq	%rcx
	movq	-24(%rbp), %rax
	shrq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L383
L382:
	movq	$0, -56(%rbp)
L383:
	movq	-56(%rbp), %rax
	movq	%rax, -72(%rbp)
L381:
	movq	-72(%rbp), %rax
	leave
	ret
.globl _Shift_M3
_Shift_M3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	leaq	_MM_Shift(%rip), %rax
	leave
	ret
	.const_data
	.align 5
_L_1:
	.ascii "Shift_M3"
	.space 1
	.ascii "uShift_param_u32_C"
	.space 1
	.ascii "uShift_var_u32_C"
	.space 1
	.ascii "uShift_param_u32_I"
	.space 1
	.ascii "uShift_var_u32_I"
	.space 1
	.ascii "uShift_param_L_C"
	.space 1
	.ascii "uShift_var_L_C"
	.space 1
	.ascii "uShift_param_L_I"
	.space 1
	.ascii "uShift_var_L_I"
	.space 1
	.ascii "uShift_param_i8_C"
	.space 1
	.ascii "uShift_var_i8_C"
	.space 1
	.ascii "uShift_param_i8_I"
	.space 1
	.ascii "uShift_var_i8_I"
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
	.ascii "uShift_param_i64_C"
	.space 1
	.ascii "uShift_var_i64_C"
	.space 1
	.ascii "uShift_param_i64_I"
	.space 1
	.ascii "uShift_var_i64_I"
	.space 1
	.ascii "uShift_param_I_C"
	.space 1
	.ascii "uShift_var_I_C"
	.space 1
	.ascii "uShift_param_I_I"
	.space 1
	.ascii "uShift_var_I_I"
	.space 1
	.ascii "uShift_param_i32_C"
	.space 1
	.ascii "uShift_var_i32_C"
	.space 1
	.ascii "uShift_param_i32_I"
	.space 1
	.ascii "uShift_var_i32_I"
	.space 1
	.ascii "uShift_param_i16_C"
	.space 1
	.ascii "uShift_var_i16_C"
	.space 1
	.ascii "uShift_param_i16_I"
	.space 1
	.ascii "uShift_var_i16_I"
	.space 1
	.ascii "uShift_param_LC_C"
	.space 1
	.ascii "uShift_var_LC_C"
	.space 1
	.ascii "uShift_param_LC_I"
	.space 1
	.ascii "uShift_var_LC_I"
	.space 1
	.ascii "uShift_param_u64_C"
	.space 1
	.ascii "uShift_var_u64_C"
	.space 1
	.ascii "uShift_param_u64_I"
	.space 1
	.ascii "uShift_var_u64_I"
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
	.quad	_Shift__uShift_param_u32_C
	.quad	_L_1+9
	.quad	_Shift__uShift_var_u32_C
	.quad	_L_1+28
	.quad	_Shift__uShift_param_u32_I
	.quad	_L_1+45
	.quad	_Shift__uShift_var_u32_I
	.quad	_L_1+64
	.quad	_Shift__uShift_param_L_C
	.quad	_L_1+81
	.quad	_Shift__uShift_var_L_C
	.quad	_L_1+98
	.quad	_Shift__uShift_param_L_I
	.quad	_L_1+113
	.quad	_Shift__uShift_var_L_I
	.quad	_L_1+130
	.quad	_Shift__uShift_param_i8_C
	.quad	_L_1+145
	.quad	_Shift__uShift_var_i8_C
	.quad	_L_1+163
	.quad	_Shift__uShift_param_i8_I
	.quad	_L_1+179
	.quad	_Shift__uShift_var_i8_I
	.quad	_L_1+197
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
	.quad	_Shift__uShift_param_i64_C
	.quad	_L_1+345
	.quad	_Shift__uShift_var_i64_C
	.quad	_L_1+364
	.quad	_Shift__uShift_param_i64_I
	.quad	_L_1+381
	.quad	_Shift__uShift_var_i64_I
	.quad	_L_1+400
	.quad	_Shift__uShift_param_I_C
	.quad	_L_1+417
	.quad	_Shift__uShift_var_I_C
	.quad	_L_1+434
	.quad	_Shift__uShift_param_I_I
	.quad	_L_1+449
	.quad	_Shift__uShift_var_I_I
	.quad	_L_1+466
	.quad	_Shift__uShift_param_i32_C
	.quad	_L_1+481
	.quad	_Shift__uShift_var_i32_C
	.quad	_L_1+500
	.quad	_Shift__uShift_param_i32_I
	.quad	_L_1+517
	.quad	_Shift__uShift_var_i32_I
	.quad	_L_1+536
	.quad	_Shift__uShift_param_i16_C
	.quad	_L_1+553
	.quad	_Shift__uShift_var_i16_C
	.quad	_L_1+572
	.quad	_Shift__uShift_param_i16_I
	.quad	_L_1+589
	.quad	_Shift__uShift_var_i16_I
	.quad	_L_1+608
	.quad	_Shift__uShift_param_LC_C
	.quad	_L_1+625
	.quad	_Shift__uShift_var_LC_C
	.quad	_L_1+643
	.quad	_Shift__uShift_param_LC_I
	.quad	_L_1+659
	.quad	_Shift__uShift_var_LC_I
	.quad	_L_1+677
	.quad	_Shift__uShift_param_u64_C
	.quad	_L_1+693
	.quad	_Shift__uShift_var_u64_C
	.quad	_L_1+712
	.quad	_Shift__uShift_param_u64_I
	.quad	_L_1+729
	.quad	_Shift__uShift_var_u64_I
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
	.quad	_MM_Shift+208
	.space 8
	.quad	_Shift_M3
	.quad	3
	.word	689
	.space 6
	.quad	690
	.quad	691
	.long	1133871366
	.long	1082500491
	.word	694
	.space 2
	.long	695
	.quad	696
	.quad	697
	.quad	698
	.long	1143925965
	.byte	-67
	.byte	-66
	.space 2
	.quad	703
	.long	704
	.space 28
	.quad	_Shift_I3
	.quad	_MM_Shift+232
	.space 8
	.quad	_Long_I3
	.quad	_MM_Shift+256
	.space 8
	.quad	_Word_I3
	.quad	_MM_Shift+280
	.space 8
	.quad	_Cstdint_I3
	.quad	_MM_Shift+304
	.space 8
	.quad	_RTHooks_I3
	.space 8
	.subsections_via_symbols
