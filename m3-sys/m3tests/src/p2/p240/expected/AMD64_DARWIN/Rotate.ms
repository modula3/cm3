	.text
.no_dead_strip _Rotate__uRotate_var_u16_I
_Rotate__uRotate_var_u16_I:
LFB0:
	pushq	%rbp
LCFI0:
	movq	%rsp, %rbp
LCFI1:
	pushq	%rbx
LCFI2:
	movq	120+_MM_Rotate(%rip), %rax
	testq	%rax, %rax
	js	L2
	movzwl	104+_MM_Rotate(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_Rotate(%rip), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L3
L2:
	movzwl	104+_MM_Rotate(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_Rotate(%rip), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L3:
	popq	%rbx
	leave
LCFI3:
	ret
LFE0:
.no_dead_strip _Rotate__uRotate_param_u16_I
_Rotate__uRotate_param_u16_I:
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
	js	L5
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L6
L5:
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L6:
	popq	%rbx
	leave
LCFI7:
	ret
LFE1:
.no_dead_strip _Rotate__uRotate_var_u16_C
_Rotate__uRotate_var_u16_C:
LFB2:
	pushq	%rbp
LCFI8:
	movq	%rsp, %rbp
LCFI9:
	pushq	%rbx
LCFI10:
	movq	160+_MM_Rotate(%rip), %rax
	testq	%rax, %rax
	js	L8
	movzwl	104+_MM_Rotate(%rip), %eax
	movzwl	%ax, %edx
	movq	160+_MM_Rotate(%rip), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L9
L8:
	movzwl	104+_MM_Rotate(%rip), %eax
	movzwl	%ax, %edx
	movq	160+_MM_Rotate(%rip), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L9:
	popq	%rbx
	leave
LCFI11:
	ret
LFE2:
.no_dead_strip _Rotate__uRotate_param_u16_C
_Rotate__uRotate_param_u16_C:
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
	js	L11
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L12
L11:
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L12:
	popq	%rbx
	leave
LCFI15:
	ret
LFE3:
.no_dead_strip _Rotate__uRotate_var_u32_I
_Rotate__uRotate_var_u32_I:
LFB4:
	pushq	%rbp
LCFI16:
	movq	%rsp, %rbp
LCFI17:
	pushq	%rbx
LCFI18:
	movq	120+_MM_Rotate(%rip), %rax
	testq	%rax, %rax
	js	L14
	movl	108+_MM_Rotate(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_Rotate(%rip), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L15
L14:
	movl	108+_MM_Rotate(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_Rotate(%rip), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L15:
	popq	%rbx
	leave
LCFI19:
	ret
LFE4:
.no_dead_strip _Rotate__uRotate_param_u32_I
_Rotate__uRotate_param_u32_I:
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
	js	L17
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L18
L17:
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L18:
	popq	%rbx
	leave
LCFI23:
	ret
LFE5:
.no_dead_strip _Rotate__uRotate_var_u32_C
_Rotate__uRotate_var_u32_C:
LFB6:
	pushq	%rbp
LCFI24:
	movq	%rsp, %rbp
LCFI25:
	pushq	%rbx
LCFI26:
	movq	160+_MM_Rotate(%rip), %rax
	testq	%rax, %rax
	js	L20
	movl	108+_MM_Rotate(%rip), %eax
	mov	%eax, %edx
	movq	160+_MM_Rotate(%rip), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L21
L20:
	movl	108+_MM_Rotate(%rip), %eax
	mov	%eax, %edx
	movq	160+_MM_Rotate(%rip), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L21:
	popq	%rbx
	leave
LCFI27:
	ret
LFE6:
.no_dead_strip _Rotate__uRotate_param_u32_C
_Rotate__uRotate_param_u32_C:
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
	js	L23
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L24
L23:
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L24:
	popq	%rbx
	leave
LCFI31:
	ret
LFE7:
.no_dead_strip _Rotate__uRotate_var_LC_I
_Rotate__uRotate_var_LC_I:
LFB8:
	pushq	%rbp
LCFI32:
	movq	%rsp, %rbp
LCFI33:
	pushq	%rbx
LCFI34:
	movq	120+_MM_Rotate(%rip), %rax
	testq	%rax, %rax
	js	L26
	movq	112+_MM_Rotate(%rip), %rdx
	movq	120+_MM_Rotate(%rip), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L27
L26:
	movq	112+_MM_Rotate(%rip), %rdx
	movq	120+_MM_Rotate(%rip), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L27:
	popq	%rbx
	leave
LCFI35:
	ret
LFE8:
.no_dead_strip _Rotate__uRotate_param_LC_I
_Rotate__uRotate_param_LC_I:
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
	js	L29
	movq	-32(%rbp), %rax
	andl	$63, %eax
	movq	-24(%rbp), %rdx
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L30
L29:
	movq	-32(%rbp), %rax
	negq	%rax
	andl	$63, %eax
	movq	-24(%rbp), %rdx
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L30:
	popq	%rbx
	leave
LCFI39:
	ret
LFE9:
.no_dead_strip _Rotate__uRotate_var_LC_C
_Rotate__uRotate_var_LC_C:
LFB10:
	pushq	%rbp
LCFI40:
	movq	%rsp, %rbp
LCFI41:
	pushq	%rbx
LCFI42:
	movq	160+_MM_Rotate(%rip), %rax
	testq	%rax, %rax
	js	L32
	movq	112+_MM_Rotate(%rip), %rdx
	movq	160+_MM_Rotate(%rip), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L33
L32:
	movq	112+_MM_Rotate(%rip), %rdx
	movq	160+_MM_Rotate(%rip), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L33:
	popq	%rbx
	leave
LCFI43:
	ret
LFE10:
.no_dead_strip _Rotate__uRotate_param_LC_C
_Rotate__uRotate_param_LC_C:
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
	js	L35
	movq	-32(%rbp), %rax
	andl	$63, %eax
	movq	-24(%rbp), %rdx
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L36
L35:
	movq	-32(%rbp), %rax
	negq	%rax
	andl	$63, %eax
	movq	-24(%rbp), %rdx
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L36:
	popq	%rbx
	leave
LCFI47:
	ret
LFE11:
.no_dead_strip _Rotate__uRotate_var_I_I
_Rotate__uRotate_var_I_I:
LFB12:
	pushq	%rbp
LCFI48:
	movq	%rsp, %rbp
LCFI49:
	pushq	%rbx
LCFI50:
	movq	120+_MM_Rotate(%rip), %rax
	testq	%rax, %rax
	js	L38
	movq	120+_MM_Rotate(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Rotate(%rip), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L39
L38:
	movq	120+_MM_Rotate(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Rotate(%rip), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L39:
	popq	%rbx
	leave
LCFI51:
	ret
LFE12:
.no_dead_strip _Rotate__uRotate_param_I_I
_Rotate__uRotate_param_I_I:
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
	js	L41
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L42
L41:
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L42:
	popq	%rbx
	leave
LCFI55:
	ret
LFE13:
.no_dead_strip _Rotate__uRotate_var_I_C
_Rotate__uRotate_var_I_C:
LFB14:
	pushq	%rbp
LCFI56:
	movq	%rsp, %rbp
LCFI57:
	pushq	%rbx
LCFI58:
	movq	160+_MM_Rotate(%rip), %rax
	testq	%rax, %rax
	js	L44
	movq	120+_MM_Rotate(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Rotate(%rip), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L45
L44:
	movq	120+_MM_Rotate(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Rotate(%rip), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L45:
	popq	%rbx
	leave
LCFI59:
	ret
LFE14:
.no_dead_strip _Rotate__uRotate_param_I_C
_Rotate__uRotate_param_I_C:
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
	js	L47
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L48
L47:
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L48:
	popq	%rbx
	leave
LCFI63:
	ret
LFE15:
.no_dead_strip _Rotate__uRotate_var_i16_I
_Rotate__uRotate_var_i16_I:
LFB16:
	pushq	%rbp
LCFI64:
	movq	%rsp, %rbp
LCFI65:
	pushq	%rbx
LCFI66:
	movq	120+_MM_Rotate(%rip), %rax
	testq	%rax, %rax
	js	L50
	movzwl	128+_MM_Rotate(%rip), %eax
	movswq	%ax, %rdx
	movq	120+_MM_Rotate(%rip), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L51
L50:
	movzwl	128+_MM_Rotate(%rip), %eax
	movswq	%ax, %rdx
	movq	120+_MM_Rotate(%rip), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L51:
	popq	%rbx
	leave
LCFI67:
	ret
LFE16:
.no_dead_strip _Rotate__uRotate_param_i16_I
_Rotate__uRotate_param_i16_I:
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
	js	L53
	movswq	-18(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L54
L53:
	movswq	-18(%rbp), %rdx
	movq	-32(%rbp), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L54:
	popq	%rbx
	leave
LCFI71:
	ret
LFE17:
.no_dead_strip _Rotate__uRotate_var_i16_C
_Rotate__uRotate_var_i16_C:
LFB18:
	pushq	%rbp
LCFI72:
	movq	%rsp, %rbp
LCFI73:
	pushq	%rbx
LCFI74:
	movq	160+_MM_Rotate(%rip), %rax
	testq	%rax, %rax
	js	L56
	movzwl	128+_MM_Rotate(%rip), %eax
	movswq	%ax, %rdx
	movq	160+_MM_Rotate(%rip), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L57
L56:
	movzwl	128+_MM_Rotate(%rip), %eax
	movswq	%ax, %rdx
	movq	160+_MM_Rotate(%rip), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L57:
	popq	%rbx
	leave
LCFI75:
	ret
LFE18:
.no_dead_strip _Rotate__uRotate_param_i16_C
_Rotate__uRotate_param_i16_C:
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
	js	L59
	movswq	-18(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L60
L59:
	movswq	-18(%rbp), %rdx
	movq	-32(%rbp), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L60:
	popq	%rbx
	leave
LCFI79:
	ret
LFE19:
.no_dead_strip _Rotate__uRotate_var_u64_I
_Rotate__uRotate_var_u64_I:
LFB20:
	pushq	%rbp
LCFI80:
	movq	%rsp, %rbp
LCFI81:
	pushq	%rbx
LCFI82:
	movq	120+_MM_Rotate(%rip), %rax
	testq	%rax, %rax
	js	L62
	movq	136+_MM_Rotate(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Rotate(%rip), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L63
L62:
	movq	136+_MM_Rotate(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Rotate(%rip), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L63:
	popq	%rbx
	leave
LCFI83:
	ret
LFE20:
.no_dead_strip _Rotate__uRotate_param_u64_I
_Rotate__uRotate_param_u64_I:
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
	js	L65
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L66
L65:
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L66:
	popq	%rbx
	leave
LCFI87:
	ret
LFE21:
.no_dead_strip _Rotate__uRotate_var_u64_C
_Rotate__uRotate_var_u64_C:
LFB22:
	pushq	%rbp
LCFI88:
	movq	%rsp, %rbp
LCFI89:
	pushq	%rbx
LCFI90:
	movq	160+_MM_Rotate(%rip), %rax
	testq	%rax, %rax
	js	L68
	movq	136+_MM_Rotate(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Rotate(%rip), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L69
L68:
	movq	136+_MM_Rotate(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Rotate(%rip), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L69:
	popq	%rbx
	leave
LCFI91:
	ret
LFE22:
.no_dead_strip _Rotate__uRotate_param_u64_C
_Rotate__uRotate_param_u64_C:
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
	js	L71
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L72
L71:
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L72:
	popq	%rbx
	leave
LCFI95:
	ret
LFE23:
.no_dead_strip _Rotate__uRotate_var_i32_I
_Rotate__uRotate_var_i32_I:
LFB24:
	pushq	%rbp
LCFI96:
	movq	%rsp, %rbp
LCFI97:
	pushq	%rbx
LCFI98:
	movq	120+_MM_Rotate(%rip), %rax
	testq	%rax, %rax
	js	L74
	movl	152+_MM_Rotate(%rip), %eax
	movslq	%eax, %rdx
	movq	120+_MM_Rotate(%rip), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L75
L74:
	movl	152+_MM_Rotate(%rip), %eax
	movslq	%eax, %rdx
	movq	120+_MM_Rotate(%rip), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L75:
	popq	%rbx
	leave
LCFI99:
	ret
LFE24:
.no_dead_strip _Rotate__uRotate_param_i32_I
_Rotate__uRotate_param_i32_I:
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
	js	L77
	movl	-20(%rbp), %eax
	movslq	%eax, %rdx
	movq	-32(%rbp), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L78
L77:
	movl	-20(%rbp), %eax
	movslq	%eax, %rdx
	movq	-32(%rbp), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L78:
	popq	%rbx
	leave
LCFI103:
	ret
LFE25:
.no_dead_strip _Rotate__uRotate_var_i32_C
_Rotate__uRotate_var_i32_C:
LFB26:
	pushq	%rbp
LCFI104:
	movq	%rsp, %rbp
LCFI105:
	pushq	%rbx
LCFI106:
	movq	160+_MM_Rotate(%rip), %rax
	testq	%rax, %rax
	js	L80
	movl	152+_MM_Rotate(%rip), %eax
	movslq	%eax, %rdx
	movq	160+_MM_Rotate(%rip), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L81
L80:
	movl	152+_MM_Rotate(%rip), %eax
	movslq	%eax, %rdx
	movq	160+_MM_Rotate(%rip), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L81:
	popq	%rbx
	leave
LCFI107:
	ret
LFE26:
.no_dead_strip _Rotate__uRotate_param_i32_C
_Rotate__uRotate_param_i32_C:
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
	js	L83
	movl	-20(%rbp), %eax
	movslq	%eax, %rdx
	movq	-32(%rbp), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L84
L83:
	movl	-20(%rbp), %eax
	movslq	%eax, %rdx
	movq	-32(%rbp), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L84:
	popq	%rbx
	leave
LCFI111:
	ret
LFE27:
.no_dead_strip _Rotate__uRotate_var_C_I
_Rotate__uRotate_var_C_I:
LFB28:
	pushq	%rbp
LCFI112:
	movq	%rsp, %rbp
LCFI113:
	pushq	%rbx
LCFI114:
	movq	120+_MM_Rotate(%rip), %rax
	testq	%rax, %rax
	js	L86
	movq	160+_MM_Rotate(%rip), %rdx
	movq	120+_MM_Rotate(%rip), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L87
L86:
	movq	160+_MM_Rotate(%rip), %rdx
	movq	120+_MM_Rotate(%rip), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L87:
	popq	%rbx
	leave
LCFI115:
	ret
LFE28:
.no_dead_strip _Rotate__uRotate_param_C_I
_Rotate__uRotate_param_C_I:
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
	js	L89
	movq	-32(%rbp), %rax
	andl	$63, %eax
	movq	-24(%rbp), %rdx
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L90
L89:
	movq	-32(%rbp), %rax
	negq	%rax
	andl	$63, %eax
	movq	-24(%rbp), %rdx
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L90:
	popq	%rbx
	leave
LCFI119:
	ret
LFE29:
.no_dead_strip _Rotate__uRotate_var_C_C
_Rotate__uRotate_var_C_C:
LFB30:
	pushq	%rbp
LCFI120:
	movq	%rsp, %rbp
LCFI121:
	pushq	%rbx
LCFI122:
	movq	160+_MM_Rotate(%rip), %rax
	testq	%rax, %rax
	js	L92
	movq	160+_MM_Rotate(%rip), %rdx
	movq	160+_MM_Rotate(%rip), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L93
L92:
	movq	160+_MM_Rotate(%rip), %rdx
	movq	160+_MM_Rotate(%rip), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L93:
	popq	%rbx
	leave
LCFI123:
	ret
LFE30:
.no_dead_strip _Rotate__uRotate_param_C_C
_Rotate__uRotate_param_C_C:
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
	js	L95
	movq	-32(%rbp), %rax
	andl	$63, %eax
	movq	-24(%rbp), %rdx
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L96
L95:
	movq	-32(%rbp), %rax
	negq	%rax
	andl	$63, %eax
	movq	-24(%rbp), %rdx
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L96:
	popq	%rbx
	leave
LCFI127:
	ret
LFE31:
.no_dead_strip _Rotate__uRotate_var_u8_I
_Rotate__uRotate_var_u8_I:
LFB32:
	pushq	%rbp
LCFI128:
	movq	%rsp, %rbp
LCFI129:
	pushq	%rbx
LCFI130:
	movq	120+_MM_Rotate(%rip), %rax
	testq	%rax, %rax
	js	L98
	movzbl	168+_MM_Rotate(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_Rotate(%rip), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L99
L98:
	movzbl	168+_MM_Rotate(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_Rotate(%rip), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L99:
	popq	%rbx
	leave
LCFI131:
	ret
LFE32:
.no_dead_strip _Rotate__uRotate_param_u8_I
_Rotate__uRotate_param_u8_I:
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
	js	L101
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L102
L101:
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L102:
	popq	%rbx
	leave
LCFI135:
	ret
LFE33:
.no_dead_strip _Rotate__uRotate_var_u8_C
_Rotate__uRotate_var_u8_C:
LFB34:
	pushq	%rbp
LCFI136:
	movq	%rsp, %rbp
LCFI137:
	pushq	%rbx
LCFI138:
	movq	160+_MM_Rotate(%rip), %rax
	testq	%rax, %rax
	js	L104
	movzbl	168+_MM_Rotate(%rip), %eax
	movzbl	%al, %edx
	movq	160+_MM_Rotate(%rip), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L105
L104:
	movzbl	168+_MM_Rotate(%rip), %eax
	movzbl	%al, %edx
	movq	160+_MM_Rotate(%rip), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L105:
	popq	%rbx
	leave
LCFI139:
	ret
LFE34:
.no_dead_strip _Rotate__uRotate_param_u8_C
_Rotate__uRotate_param_u8_C:
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
	js	L107
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L108
L107:
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L108:
	popq	%rbx
	leave
LCFI143:
	ret
LFE35:
.no_dead_strip _Rotate__uRotate_var_i64_I
_Rotate__uRotate_var_i64_I:
LFB36:
	pushq	%rbp
LCFI144:
	movq	%rsp, %rbp
LCFI145:
	pushq	%rbx
LCFI146:
	movq	120+_MM_Rotate(%rip), %rax
	testq	%rax, %rax
	js	L110
	movq	176+_MM_Rotate(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Rotate(%rip), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L111
L110:
	movq	176+_MM_Rotate(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Rotate(%rip), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L111:
	popq	%rbx
	leave
LCFI147:
	ret
LFE36:
.no_dead_strip _Rotate__uRotate_param_i64_I
_Rotate__uRotate_param_i64_I:
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
	js	L113
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L114
L113:
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L114:
	popq	%rbx
	leave
LCFI151:
	ret
LFE37:
.no_dead_strip _Rotate__uRotate_var_i64_C
_Rotate__uRotate_var_i64_C:
LFB38:
	pushq	%rbp
LCFI152:
	movq	%rsp, %rbp
LCFI153:
	pushq	%rbx
LCFI154:
	movq	160+_MM_Rotate(%rip), %rax
	testq	%rax, %rax
	js	L116
	movq	176+_MM_Rotate(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Rotate(%rip), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L117
L116:
	movq	176+_MM_Rotate(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Rotate(%rip), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L117:
	popq	%rbx
	leave
LCFI155:
	ret
LFE38:
.no_dead_strip _Rotate__uRotate_param_i64_C
_Rotate__uRotate_param_i64_C:
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
	js	L119
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L120
L119:
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L120:
	popq	%rbx
	leave
LCFI159:
	ret
LFE39:
.no_dead_strip _Rotate__uRotate_var_L_I
_Rotate__uRotate_var_L_I:
LFB40:
	pushq	%rbp
LCFI160:
	movq	%rsp, %rbp
LCFI161:
	pushq	%rbx
LCFI162:
	movq	120+_MM_Rotate(%rip), %rax
	testq	%rax, %rax
	js	L122
	movq	192+_MM_Rotate(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Rotate(%rip), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L123
L122:
	movq	192+_MM_Rotate(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Rotate(%rip), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L123:
	popq	%rbx
	leave
LCFI163:
	ret
LFE40:
.no_dead_strip _Rotate__uRotate_param_L_I
_Rotate__uRotate_param_L_I:
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
	js	L125
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L126
L125:
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L126:
	popq	%rbx
	leave
LCFI167:
	ret
LFE41:
.no_dead_strip _Rotate__uRotate_var_L_C
_Rotate__uRotate_var_L_C:
LFB42:
	pushq	%rbp
LCFI168:
	movq	%rsp, %rbp
LCFI169:
	pushq	%rbx
LCFI170:
	movq	160+_MM_Rotate(%rip), %rax
	testq	%rax, %rax
	js	L128
	movq	192+_MM_Rotate(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Rotate(%rip), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L129
L128:
	movq	192+_MM_Rotate(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Rotate(%rip), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L129:
	popq	%rbx
	leave
LCFI171:
	ret
LFE42:
.no_dead_strip _Rotate__uRotate_param_L_C
_Rotate__uRotate_param_L_C:
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
	js	L131
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L132
L131:
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L132:
	popq	%rbx
	leave
LCFI175:
	ret
LFE43:
.no_dead_strip _Rotate__uRotate_var_i8_I
_Rotate__uRotate_var_i8_I:
LFB44:
	pushq	%rbp
LCFI176:
	movq	%rsp, %rbp
LCFI177:
	pushq	%rbx
LCFI178:
	movq	120+_MM_Rotate(%rip), %rax
	testq	%rax, %rax
	js	L134
	movzbl	200+_MM_Rotate(%rip), %eax
	movsbq	%al, %rdx
	movq	120+_MM_Rotate(%rip), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L135
L134:
	movzbl	200+_MM_Rotate(%rip), %eax
	movsbq	%al, %rdx
	movq	120+_MM_Rotate(%rip), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L135:
	popq	%rbx
	leave
LCFI179:
	ret
LFE44:
.no_dead_strip _Rotate__uRotate_param_i8_I
_Rotate__uRotate_param_i8_I:
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
	js	L137
	movsbq	-17(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L138
L137:
	movsbq	-17(%rbp), %rdx
	movq	-32(%rbp), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L138:
	popq	%rbx
	leave
LCFI183:
	ret
LFE45:
.no_dead_strip _Rotate__uRotate_var_i8_C
_Rotate__uRotate_var_i8_C:
LFB46:
	pushq	%rbp
LCFI184:
	movq	%rsp, %rbp
LCFI185:
	pushq	%rbx
LCFI186:
	movq	160+_MM_Rotate(%rip), %rax
	testq	%rax, %rax
	js	L140
	movzbl	200+_MM_Rotate(%rip), %eax
	movsbq	%al, %rdx
	movq	160+_MM_Rotate(%rip), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L141
L140:
	movzbl	200+_MM_Rotate(%rip), %eax
	movsbq	%al, %rdx
	movq	160+_MM_Rotate(%rip), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L141:
	popq	%rbx
	leave
LCFI187:
	ret
LFE46:
.no_dead_strip _Rotate__uRotate_param_i8_C
_Rotate__uRotate_param_i8_C:
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
	js	L143
	movsbq	-17(%rbp), %rdx
	movq	-32(%rbp), %rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rolq	%cl, %rbx
	movq	%rbx, %rax
	jmp	L144
L143:
	movsbq	-17(%rbp), %rdx
	movq	-32(%rbp), %rax
	negq	%rax
	andl	$63, %eax
	movq	%rdx, %rbx
	movl	%eax, %ecx
	rorq	%cl, %rbx
	movq	%rbx, %rax
L144:
	popq	%rbx
	leave
LCFI191:
	ret
LFE47:
.globl _Rotate_M3
.no_dead_strip _Rotate_M3
_Rotate_M3:
LFB48:
	pushq	%rbp
LCFI192:
	movq	%rsp, %rbp
LCFI193:
	movq	%rdi, -8(%rbp)
	leaq	_MM_Rotate(%rip), %rax
	leave
LCFI194:
	ret
LFE48:
	.const_data
	.align 5
_L_1:
	.ascii "Rotate_M3"
	.space 1
	.ascii "uRotate_param_i8_C"
	.space 1
	.ascii "uRotate_var_i8_C"
	.space 1
	.ascii "uRotate_param_i8_I"
	.space 1
	.ascii "uRotate_var_i8_I"
	.space 1
	.ascii "uRotate_param_L_C"
	.space 1
	.ascii "uRotate_var_L_C"
	.space 1
	.ascii "uRotate_param_L_I"
	.space 1
	.ascii "uRotate_var_L_I"
	.space 1
	.ascii "uRotate_param_i64_C"
	.space 1
	.ascii "uRotate_var_i64_C"
	.space 1
	.ascii "uRotate_param_i64_I"
	.space 1
	.ascii "uRotate_var_i64_I"
	.space 1
	.ascii "uRotate_param_u8_C"
	.space 1
	.ascii "uRotate_var_u8_C"
	.space 1
	.ascii "uRotate_param_u8_I"
	.space 1
	.ascii "uRotate_var_u8_I"
	.space 1
	.ascii "uRotate_param_C_C"
	.space 1
	.ascii "uRotate_var_C_C"
	.space 1
	.ascii "uRotate_param_C_I"
	.space 1
	.ascii "uRotate_var_C_I"
	.space 1
	.ascii "uRotate_param_i32_C"
	.space 1
	.ascii "uRotate_var_i32_C"
	.space 1
	.ascii "uRotate_param_i32_I"
	.space 1
	.ascii "uRotate_var_i32_I"
	.space 1
	.ascii "uRotate_param_u64_C"
	.space 1
	.ascii "uRotate_var_u64_C"
	.space 1
	.ascii "uRotate_param_u64_I"
	.space 1
	.ascii "uRotate_var_u64_I"
	.space 1
	.ascii "uRotate_param_i16_C"
	.space 1
	.ascii "uRotate_var_i16_C"
	.space 1
	.ascii "uRotate_param_i16_I"
	.space 1
	.ascii "uRotate_var_i16_I"
	.space 1
	.ascii "uRotate_param_I_C"
	.space 1
	.ascii "uRotate_var_I_C"
	.space 1
	.ascii "uRotate_param_I_I"
	.space 1
	.ascii "uRotate_var_I_I"
	.space 1
	.ascii "uRotate_param_LC_C"
	.space 1
	.ascii "uRotate_var_LC_C"
	.space 1
	.ascii "uRotate_param_LC_I"
	.space 1
	.ascii "uRotate_var_LC_I"
	.space 1
	.ascii "uRotate_param_u32_C"
	.space 1
	.ascii "uRotate_var_u32_C"
	.space 1
	.ascii "uRotate_param_u32_I"
	.space 1
	.ascii "uRotate_var_u32_I"
	.space 1
	.ascii "uRotate_param_u16_C"
	.space 1
	.ascii "uRotate_var_u16_C"
	.space 1
	.ascii "uRotate_param_u16_I"
	.space 1
	.ascii "uRotate_var_u16_I"
	.space 3
	.quad	_Rotate_M3
	.quad	_L_1
	.quad	_Rotate__uRotate_param_i8_C
	.quad	_L_1+10
	.quad	_Rotate__uRotate_var_i8_C
	.quad	_L_1+29
	.quad	_Rotate__uRotate_param_i8_I
	.quad	_L_1+46
	.quad	_Rotate__uRotate_var_i8_I
	.quad	_L_1+65
	.quad	_Rotate__uRotate_param_L_C
	.quad	_L_1+82
	.quad	_Rotate__uRotate_var_L_C
	.quad	_L_1+100
	.quad	_Rotate__uRotate_param_L_I
	.quad	_L_1+116
	.quad	_Rotate__uRotate_var_L_I
	.quad	_L_1+134
	.quad	_Rotate__uRotate_param_i64_C
	.quad	_L_1+150
	.quad	_Rotate__uRotate_var_i64_C
	.quad	_L_1+170
	.quad	_Rotate__uRotate_param_i64_I
	.quad	_L_1+188
	.quad	_Rotate__uRotate_var_i64_I
	.quad	_L_1+208
	.quad	_Rotate__uRotate_param_u8_C
	.quad	_L_1+226
	.quad	_Rotate__uRotate_var_u8_C
	.quad	_L_1+245
	.quad	_Rotate__uRotate_param_u8_I
	.quad	_L_1+262
	.quad	_Rotate__uRotate_var_u8_I
	.quad	_L_1+281
	.quad	_Rotate__uRotate_param_C_C
	.quad	_L_1+298
	.quad	_Rotate__uRotate_var_C_C
	.quad	_L_1+316
	.quad	_Rotate__uRotate_param_C_I
	.quad	_L_1+332
	.quad	_Rotate__uRotate_var_C_I
	.quad	_L_1+350
	.quad	_Rotate__uRotate_param_i32_C
	.quad	_L_1+366
	.quad	_Rotate__uRotate_var_i32_C
	.quad	_L_1+386
	.quad	_Rotate__uRotate_param_i32_I
	.quad	_L_1+404
	.quad	_Rotate__uRotate_var_i32_I
	.quad	_L_1+424
	.quad	_Rotate__uRotate_param_u64_C
	.quad	_L_1+442
	.quad	_Rotate__uRotate_var_u64_C
	.quad	_L_1+462
	.quad	_Rotate__uRotate_param_u64_I
	.quad	_L_1+480
	.quad	_Rotate__uRotate_var_u64_I
	.quad	_L_1+500
	.quad	_Rotate__uRotate_param_i16_C
	.quad	_L_1+518
	.quad	_Rotate__uRotate_var_i16_C
	.quad	_L_1+538
	.quad	_Rotate__uRotate_param_i16_I
	.quad	_L_1+556
	.quad	_Rotate__uRotate_var_i16_I
	.quad	_L_1+576
	.quad	_Rotate__uRotate_param_I_C
	.quad	_L_1+594
	.quad	_Rotate__uRotate_var_I_C
	.quad	_L_1+612
	.quad	_Rotate__uRotate_param_I_I
	.quad	_L_1+628
	.quad	_Rotate__uRotate_var_I_I
	.quad	_L_1+646
	.quad	_Rotate__uRotate_param_LC_C
	.quad	_L_1+662
	.quad	_Rotate__uRotate_var_LC_C
	.quad	_L_1+681
	.quad	_Rotate__uRotate_param_LC_I
	.quad	_L_1+698
	.quad	_Rotate__uRotate_var_LC_I
	.quad	_L_1+717
	.quad	_Rotate__uRotate_param_u32_C
	.quad	_L_1+734
	.quad	_Rotate__uRotate_var_u32_C
	.quad	_L_1+754
	.quad	_Rotate__uRotate_param_u32_I
	.quad	_L_1+772
	.quad	_Rotate__uRotate_var_u32_I
	.quad	_L_1+792
	.quad	_Rotate__uRotate_param_u16_C
	.quad	_L_1+810
	.quad	_Rotate__uRotate_var_u16_C
	.quad	_L_1+830
	.quad	_Rotate__uRotate_param_u16_I
	.quad	_L_1+848
	.quad	_Rotate__uRotate_var_u16_I
	.quad	_L_1+868
	.space 8
	.ascii "../AMD64_DARWIN/Rotate.m3"
	.space 7
	.data
	.align 5
_MM_Rotate:
	.quad	_L_1+1680
	.space 32
	.quad	_L_1+888
	.space 24
	.quad	_MM_Rotate+224
	.space 8
	.quad	_Rotate_M3
	.quad	3
	.word	785
	.space 2
	.long	786
	.quad	787
	.quad	788
	.word	789
	.space 6
	.quad	790
	.long	68719477
	.long	1082703446
	.long	793
	.space 4
	.quad	794
	.byte	27
	.space 7
	.quad	796
	.long	1145533202
	.space 4
	.quad	799
	.byte	32
	.space 31
	.quad	_Rotate_I3
	.quad	_MM_Rotate+248
	.space 8
	.quad	_Long_I3
	.quad	_MM_Rotate+272
	.space 8
	.quad	_Word_I3
	.quad	_MM_Rotate+296
	.space 8
	.quad	_Cstdint_I3
	.quad	_MM_Rotate+320
	.space 8
	.quad	_RTHooks_I3
	.space 8
_Rotate__uRotate_var_u16_I.eh = 0
.no_dead_strip _Rotate__uRotate_var_u16_I.eh
_Rotate__uRotate_param_u16_I.eh = 0
.no_dead_strip _Rotate__uRotate_param_u16_I.eh
_Rotate__uRotate_var_u16_C.eh = 0
.no_dead_strip _Rotate__uRotate_var_u16_C.eh
_Rotate__uRotate_param_u16_C.eh = 0
.no_dead_strip _Rotate__uRotate_param_u16_C.eh
_Rotate__uRotate_var_u32_I.eh = 0
.no_dead_strip _Rotate__uRotate_var_u32_I.eh
_Rotate__uRotate_param_u32_I.eh = 0
.no_dead_strip _Rotate__uRotate_param_u32_I.eh
_Rotate__uRotate_var_u32_C.eh = 0
.no_dead_strip _Rotate__uRotate_var_u32_C.eh
_Rotate__uRotate_param_u32_C.eh = 0
.no_dead_strip _Rotate__uRotate_param_u32_C.eh
_Rotate__uRotate_var_LC_I.eh = 0
.no_dead_strip _Rotate__uRotate_var_LC_I.eh
_Rotate__uRotate_param_LC_I.eh = 0
.no_dead_strip _Rotate__uRotate_param_LC_I.eh
_Rotate__uRotate_var_LC_C.eh = 0
.no_dead_strip _Rotate__uRotate_var_LC_C.eh
_Rotate__uRotate_param_LC_C.eh = 0
.no_dead_strip _Rotate__uRotate_param_LC_C.eh
_Rotate__uRotate_var_I_I.eh = 0
.no_dead_strip _Rotate__uRotate_var_I_I.eh
_Rotate__uRotate_param_I_I.eh = 0
.no_dead_strip _Rotate__uRotate_param_I_I.eh
_Rotate__uRotate_var_I_C.eh = 0
.no_dead_strip _Rotate__uRotate_var_I_C.eh
_Rotate__uRotate_param_I_C.eh = 0
.no_dead_strip _Rotate__uRotate_param_I_C.eh
_Rotate__uRotate_var_i16_I.eh = 0
.no_dead_strip _Rotate__uRotate_var_i16_I.eh
_Rotate__uRotate_param_i16_I.eh = 0
.no_dead_strip _Rotate__uRotate_param_i16_I.eh
_Rotate__uRotate_var_i16_C.eh = 0
.no_dead_strip _Rotate__uRotate_var_i16_C.eh
_Rotate__uRotate_param_i16_C.eh = 0
.no_dead_strip _Rotate__uRotate_param_i16_C.eh
_Rotate__uRotate_var_u64_I.eh = 0
.no_dead_strip _Rotate__uRotate_var_u64_I.eh
_Rotate__uRotate_param_u64_I.eh = 0
.no_dead_strip _Rotate__uRotate_param_u64_I.eh
_Rotate__uRotate_var_u64_C.eh = 0
.no_dead_strip _Rotate__uRotate_var_u64_C.eh
_Rotate__uRotate_param_u64_C.eh = 0
.no_dead_strip _Rotate__uRotate_param_u64_C.eh
_Rotate__uRotate_var_i32_I.eh = 0
.no_dead_strip _Rotate__uRotate_var_i32_I.eh
_Rotate__uRotate_param_i32_I.eh = 0
.no_dead_strip _Rotate__uRotate_param_i32_I.eh
_Rotate__uRotate_var_i32_C.eh = 0
.no_dead_strip _Rotate__uRotate_var_i32_C.eh
_Rotate__uRotate_param_i32_C.eh = 0
.no_dead_strip _Rotate__uRotate_param_i32_C.eh
_Rotate__uRotate_var_C_I.eh = 0
.no_dead_strip _Rotate__uRotate_var_C_I.eh
_Rotate__uRotate_param_C_I.eh = 0
.no_dead_strip _Rotate__uRotate_param_C_I.eh
_Rotate__uRotate_var_C_C.eh = 0
.no_dead_strip _Rotate__uRotate_var_C_C.eh
_Rotate__uRotate_param_C_C.eh = 0
.no_dead_strip _Rotate__uRotate_param_C_C.eh
_Rotate__uRotate_var_u8_I.eh = 0
.no_dead_strip _Rotate__uRotate_var_u8_I.eh
_Rotate__uRotate_param_u8_I.eh = 0
.no_dead_strip _Rotate__uRotate_param_u8_I.eh
_Rotate__uRotate_var_u8_C.eh = 0
.no_dead_strip _Rotate__uRotate_var_u8_C.eh
_Rotate__uRotate_param_u8_C.eh = 0
.no_dead_strip _Rotate__uRotate_param_u8_C.eh
_Rotate__uRotate_var_i64_I.eh = 0
.no_dead_strip _Rotate__uRotate_var_i64_I.eh
_Rotate__uRotate_param_i64_I.eh = 0
.no_dead_strip _Rotate__uRotate_param_i64_I.eh
_Rotate__uRotate_var_i64_C.eh = 0
.no_dead_strip _Rotate__uRotate_var_i64_C.eh
_Rotate__uRotate_param_i64_C.eh = 0
.no_dead_strip _Rotate__uRotate_param_i64_C.eh
_Rotate__uRotate_var_L_I.eh = 0
.no_dead_strip _Rotate__uRotate_var_L_I.eh
_Rotate__uRotate_param_L_I.eh = 0
.no_dead_strip _Rotate__uRotate_param_L_I.eh
_Rotate__uRotate_var_L_C.eh = 0
.no_dead_strip _Rotate__uRotate_var_L_C.eh
_Rotate__uRotate_param_L_C.eh = 0
.no_dead_strip _Rotate__uRotate_param_L_C.eh
_Rotate__uRotate_var_i8_I.eh = 0
.no_dead_strip _Rotate__uRotate_var_i8_I.eh
_Rotate__uRotate_param_i8_I.eh = 0
.no_dead_strip _Rotate__uRotate_param_i8_I.eh
_Rotate__uRotate_var_i8_C.eh = 0
.no_dead_strip _Rotate__uRotate_var_i8_C.eh
_Rotate__uRotate_param_i8_C.eh = 0
.no_dead_strip _Rotate__uRotate_param_i8_C.eh
.globl _Rotate_M3.eh
_Rotate_M3.eh = 0
.no_dead_strip _Rotate_M3.eh
	.subsections_via_symbols
