	.text
.globl _Rotate__uRotate_var_i64_I
	.private_extern _Rotate__uRotate_var_i64_I
_Rotate__uRotate_var_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Rotate(%rip), %rax
	movq	%rax, -16(%rbp)
	movq	104+_MM_Rotate(%rip), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L2
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L3
L2:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -40(%rbp)
L3:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_param_i64_I
	.private_extern _Rotate__uRotate_param_i64_I
_Rotate__uRotate_param_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -40(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	movq	-40(%rbp), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L6
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L7
L6:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -56(%rbp)
L7:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_var_i64_C
	.private_extern _Rotate__uRotate_var_i64_C
_Rotate__uRotate_var_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Rotate(%rip), %rax
	movq	%rax, -16(%rbp)
	movq	104+_MM_Rotate(%rip), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L10
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L11
L10:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -40(%rbp)
L11:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_param_i64_C
	.private_extern _Rotate__uRotate_param_i64_C
_Rotate__uRotate_param_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -40(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	movq	-40(%rbp), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L14
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L15
L14:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -56(%rbp)
L15:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_var_LC_I
	.private_extern _Rotate__uRotate_var_LC_I
_Rotate__uRotate_var_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Rotate(%rip), %rax
	movq	%rax, -16(%rbp)
	movq	112+_MM_Rotate(%rip), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L18
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L19
L18:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -40(%rbp)
L19:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_param_LC_I
	.private_extern _Rotate__uRotate_param_LC_I
_Rotate__uRotate_param_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -40(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	movq	-40(%rbp), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L22
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L23
L22:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -56(%rbp)
L23:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_var_LC_C
	.private_extern _Rotate__uRotate_var_LC_C
_Rotate__uRotate_var_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Rotate(%rip), %rax
	movq	%rax, -16(%rbp)
	movq	112+_MM_Rotate(%rip), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L26
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L27
L26:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -40(%rbp)
L27:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_param_LC_C
	.private_extern _Rotate__uRotate_param_LC_C
_Rotate__uRotate_param_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -40(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	movq	-40(%rbp), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L30
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L31
L30:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -56(%rbp)
L31:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_var_u8_I
	.private_extern _Rotate__uRotate_var_u8_I
_Rotate__uRotate_var_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Rotate(%rip), %rax
	movq	%rax, -16(%rbp)
	movzbl	124+_MM_Rotate(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L34
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L35
L34:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -40(%rbp)
L35:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_param_u8_I
	.private_extern _Rotate__uRotate_param_u8_I
_Rotate__uRotate_param_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -33(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	movsbq	-33(%rbp),%rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L38
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L39
L38:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -56(%rbp)
L39:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_var_u8_C
	.private_extern _Rotate__uRotate_var_u8_C
_Rotate__uRotate_var_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Rotate(%rip), %rax
	movq	%rax, -16(%rbp)
	movzbl	124+_MM_Rotate(%rip), %eax
	movsbq	%al,%rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L42
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L43
L42:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -40(%rbp)
L43:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_param_u8_C
	.private_extern _Rotate__uRotate_param_u8_C
_Rotate__uRotate_param_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -33(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	movsbq	-33(%rbp),%rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L46
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L47
L46:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -56(%rbp)
L47:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_var_I_I
	.private_extern _Rotate__uRotate_var_I_I
_Rotate__uRotate_var_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Rotate(%rip), %rax
	movq	%rax, -16(%rbp)
	movq	128+_MM_Rotate(%rip), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L50
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L51
L50:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -40(%rbp)
L51:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_param_I_I
	.private_extern _Rotate__uRotate_param_I_I
_Rotate__uRotate_param_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -40(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	movq	-40(%rbp), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L54
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L55
L54:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -56(%rbp)
L55:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_var_I_C
	.private_extern _Rotate__uRotate_var_I_C
_Rotate__uRotate_var_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Rotate(%rip), %rax
	movq	%rax, -16(%rbp)
	movq	128+_MM_Rotate(%rip), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L58
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L59
L58:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -40(%rbp)
L59:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_param_I_C
	.private_extern _Rotate__uRotate_param_I_C
_Rotate__uRotate_param_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -40(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	movq	-40(%rbp), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L62
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L63
L62:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -56(%rbp)
L63:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_var_i8_I
	.private_extern _Rotate__uRotate_var_i8_I
_Rotate__uRotate_var_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Rotate(%rip), %rax
	movq	%rax, -16(%rbp)
	movzbl	136+_MM_Rotate(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L66
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L67
L66:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -40(%rbp)
L67:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_param_i8_I
	.private_extern _Rotate__uRotate_param_i8_I
_Rotate__uRotate_param_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -33(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	movzbl	-33(%rbp), %eax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L70
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L71
L70:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -56(%rbp)
L71:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_var_i8_C
	.private_extern _Rotate__uRotate_var_i8_C
_Rotate__uRotate_var_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Rotate(%rip), %rax
	movq	%rax, -16(%rbp)
	movzbl	136+_MM_Rotate(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L74
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L75
L74:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -40(%rbp)
L75:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_param_i8_C
	.private_extern _Rotate__uRotate_param_i8_C
_Rotate__uRotate_param_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -33(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	movzbl	-33(%rbp), %eax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L78
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L79
L78:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -56(%rbp)
L79:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_var_u32_I
	.private_extern _Rotate__uRotate_var_u32_I
_Rotate__uRotate_var_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Rotate(%rip), %rax
	movq	%rax, -16(%rbp)
	movl	140+_MM_Rotate(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L82
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L83
L82:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -40(%rbp)
L83:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_param_u32_I
	.private_extern _Rotate__uRotate_param_u32_I
_Rotate__uRotate_param_u32_I:
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
	js	L86
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L87
L86:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -56(%rbp)
L87:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_var_u32_C
	.private_extern _Rotate__uRotate_var_u32_C
_Rotate__uRotate_var_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Rotate(%rip), %rax
	movq	%rax, -16(%rbp)
	movl	140+_MM_Rotate(%rip), %eax
	cltq
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L90
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L91
L90:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -40(%rbp)
L91:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_param_u32_C
	.private_extern _Rotate__uRotate_param_u32_C
_Rotate__uRotate_param_u32_C:
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
	js	L94
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L95
L94:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -56(%rbp)
L95:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_var_C_I
	.private_extern _Rotate__uRotate_var_C_I
_Rotate__uRotate_var_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Rotate(%rip), %rax
	movq	%rax, -16(%rbp)
	movq	144+_MM_Rotate(%rip), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L98
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L99
L98:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -40(%rbp)
L99:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_param_C_I
	.private_extern _Rotate__uRotate_param_C_I
_Rotate__uRotate_param_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -40(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	movq	-40(%rbp), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L102
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L103
L102:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -56(%rbp)
L103:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_var_C_C
	.private_extern _Rotate__uRotate_var_C_C
_Rotate__uRotate_var_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Rotate(%rip), %rax
	movq	%rax, -16(%rbp)
	movq	144+_MM_Rotate(%rip), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L106
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L107
L106:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -40(%rbp)
L107:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_param_C_C
	.private_extern _Rotate__uRotate_param_C_C
_Rotate__uRotate_param_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -40(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	movq	-40(%rbp), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L110
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L111
L110:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -56(%rbp)
L111:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_var_u16_I
	.private_extern _Rotate__uRotate_var_u16_I
_Rotate__uRotate_var_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Rotate(%rip), %rax
	movq	%rax, -16(%rbp)
	movzwl	152+_MM_Rotate(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L114
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L115
L114:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -40(%rbp)
L115:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_param_u16_I
	.private_extern _Rotate__uRotate_param_u16_I
_Rotate__uRotate_param_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -34(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	movswq	-34(%rbp),%rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L118
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L119
L118:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -56(%rbp)
L119:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_var_u16_C
	.private_extern _Rotate__uRotate_var_u16_C
_Rotate__uRotate_var_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Rotate(%rip), %rax
	movq	%rax, -16(%rbp)
	movzwl	152+_MM_Rotate(%rip), %eax
	movswq	%ax,%rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L122
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L123
L122:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -40(%rbp)
L123:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_param_u16_C
	.private_extern _Rotate__uRotate_param_u16_C
_Rotate__uRotate_param_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -34(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	movswq	-34(%rbp),%rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L126
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L127
L126:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -56(%rbp)
L127:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_var_u64_I
	.private_extern _Rotate__uRotate_var_u64_I
_Rotate__uRotate_var_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Rotate(%rip), %rax
	movq	%rax, -16(%rbp)
	movq	160+_MM_Rotate(%rip), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L130
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L131
L130:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -40(%rbp)
L131:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_param_u64_I
	.private_extern _Rotate__uRotate_param_u64_I
_Rotate__uRotate_param_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -40(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	movq	-40(%rbp), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L134
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L135
L134:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -56(%rbp)
L135:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_var_u64_C
	.private_extern _Rotate__uRotate_var_u64_C
_Rotate__uRotate_var_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Rotate(%rip), %rax
	movq	%rax, -16(%rbp)
	movq	160+_MM_Rotate(%rip), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L138
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L139
L138:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -40(%rbp)
L139:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_param_u64_C
	.private_extern _Rotate__uRotate_param_u64_C
_Rotate__uRotate_param_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -40(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	movq	-40(%rbp), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L142
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L143
L142:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -56(%rbp)
L143:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_var_L_I
	.private_extern _Rotate__uRotate_var_L_I
_Rotate__uRotate_var_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Rotate(%rip), %rax
	movq	%rax, -16(%rbp)
	movq	176+_MM_Rotate(%rip), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L146
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L147
L146:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -40(%rbp)
L147:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_param_L_I
	.private_extern _Rotate__uRotate_param_L_I
_Rotate__uRotate_param_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -40(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	movq	-40(%rbp), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L150
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L151
L150:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -56(%rbp)
L151:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_var_L_C
	.private_extern _Rotate__uRotate_var_L_C
_Rotate__uRotate_var_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Rotate(%rip), %rax
	movq	%rax, -16(%rbp)
	movq	176+_MM_Rotate(%rip), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L154
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L155
L154:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -40(%rbp)
L155:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_param_L_C
	.private_extern _Rotate__uRotate_param_L_C
_Rotate__uRotate_param_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -40(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	movq	-40(%rbp), %rax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L158
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L159
L158:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -56(%rbp)
L159:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_var_i16_I
	.private_extern _Rotate__uRotate_var_i16_I
_Rotate__uRotate_var_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Rotate(%rip), %rax
	movq	%rax, -16(%rbp)
	movzwl	184+_MM_Rotate(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L162
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L163
L162:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -40(%rbp)
L163:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_param_i16_I
	.private_extern _Rotate__uRotate_param_i16_I
_Rotate__uRotate_param_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -34(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	movzwl	-34(%rbp), %eax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L166
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L167
L166:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -56(%rbp)
L167:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_var_i16_C
	.private_extern _Rotate__uRotate_var_i16_C
_Rotate__uRotate_var_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Rotate(%rip), %rax
	movq	%rax, -16(%rbp)
	movzwl	184+_MM_Rotate(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L170
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L171
L170:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -40(%rbp)
L171:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_param_i16_C
	.private_extern _Rotate__uRotate_param_i16_C
_Rotate__uRotate_param_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -34(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	movzwl	-34(%rbp), %eax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L174
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L175
L174:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -56(%rbp)
L175:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_var_i32_I
	.private_extern _Rotate__uRotate_var_i32_I
_Rotate__uRotate_var_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Rotate(%rip), %rax
	movq	%rax, -16(%rbp)
	movl	188+_MM_Rotate(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L178
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L179
L178:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -40(%rbp)
L179:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_param_i32_I
	.private_extern _Rotate__uRotate_param_i32_I
_Rotate__uRotate_param_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -36(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	mov	-36(%rbp), %eax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L182
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L183
L182:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -56(%rbp)
L183:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_var_i32_C
	.private_extern _Rotate__uRotate_var_i32_C
_Rotate__uRotate_var_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Rotate(%rip), %rax
	movq	%rax, -16(%rbp)
	movl	188+_MM_Rotate(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L186
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L187
L186:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -40(%rbp)
L187:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _Rotate__uRotate_param_i32_C
	.private_extern _Rotate__uRotate_param_i32_C
_Rotate__uRotate_param_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -36(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	mov	-36(%rbp), %eax
	movq	%rax, -24(%rbp)
	cmpq	$0, -16(%rbp)
	js	L190
	movq	-16(%rbp), %rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rolq	%cl, %rax
	movq	%rax, -56(%rbp)
	jmp	L191
L190:
	movq	-16(%rbp), %rax
	negq	%rax
	movq	%rax, %rcx
	andl	$63, %ecx
	movq	-24(%rbp), %rax
	rorq	%cl, %rax
	movq	%rax, -56(%rbp)
L191:
	movq	-56(%rbp), %rax
	leave
	ret
.globl _Rotate_M3
_Rotate_M3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	leaq	_MM_Rotate(%rip), %rax
	leave
	ret
	.const_data
	.align 5
_L_1:
	.ascii "Rotate_M3"
	.space 1
	.ascii "uRotate_param_i32_C"
	.space 1
	.ascii "uRotate_var_i32_C"
	.space 1
	.ascii "uRotate_param_i32_I"
	.space 1
	.ascii "uRotate_var_i32_I"
	.space 1
	.ascii "uRotate_param_i16_C"
	.space 1
	.ascii "uRotate_var_i16_C"
	.space 1
	.ascii "uRotate_param_i16_I"
	.space 1
	.ascii "uRotate_var_i16_I"
	.space 1
	.ascii "uRotate_param_L_C"
	.space 1
	.ascii "uRotate_var_L_C"
	.space 1
	.ascii "uRotate_param_L_I"
	.space 1
	.ascii "uRotate_var_L_I"
	.space 1
	.ascii "uRotate_param_u64_C"
	.space 1
	.ascii "uRotate_var_u64_C"
	.space 1
	.ascii "uRotate_param_u64_I"
	.space 1
	.ascii "uRotate_var_u64_I"
	.space 1
	.ascii "uRotate_param_u16_C"
	.space 1
	.ascii "uRotate_var_u16_C"
	.space 1
	.ascii "uRotate_param_u16_I"
	.space 1
	.ascii "uRotate_var_u16_I"
	.space 1
	.ascii "uRotate_param_C_C"
	.space 1
	.ascii "uRotate_var_C_C"
	.space 1
	.ascii "uRotate_param_C_I"
	.space 1
	.ascii "uRotate_var_C_I"
	.space 1
	.ascii "uRotate_param_u32_C"
	.space 1
	.ascii "uRotate_var_u32_C"
	.space 1
	.ascii "uRotate_param_u32_I"
	.space 1
	.ascii "uRotate_var_u32_I"
	.space 1
	.ascii "uRotate_param_i8_C"
	.space 1
	.ascii "uRotate_var_i8_C"
	.space 1
	.ascii "uRotate_param_i8_I"
	.space 1
	.ascii "uRotate_var_i8_I"
	.space 1
	.ascii "uRotate_param_I_C"
	.space 1
	.ascii "uRotate_var_I_C"
	.space 1
	.ascii "uRotate_param_I_I"
	.space 1
	.ascii "uRotate_var_I_I"
	.space 1
	.ascii "uRotate_param_u8_C"
	.space 1
	.ascii "uRotate_var_u8_C"
	.space 1
	.ascii "uRotate_param_u8_I"
	.space 1
	.ascii "uRotate_var_u8_I"
	.space 1
	.ascii "uRotate_param_LC_C"
	.space 1
	.ascii "uRotate_var_LC_C"
	.space 1
	.ascii "uRotate_param_LC_I"
	.space 1
	.ascii "uRotate_var_LC_I"
	.space 1
	.ascii "uRotate_param_i64_C"
	.space 1
	.ascii "uRotate_var_i64_C"
	.space 1
	.ascii "uRotate_param_i64_I"
	.space 1
	.ascii "uRotate_var_i64_I"
	.space 3
	.quad	_Rotate_M3
	.quad	_L_1
	.quad	_Rotate__uRotate_param_i32_C
	.quad	_L_1+10
	.quad	_Rotate__uRotate_var_i32_C
	.quad	_L_1+30
	.quad	_Rotate__uRotate_param_i32_I
	.quad	_L_1+48
	.quad	_Rotate__uRotate_var_i32_I
	.quad	_L_1+68
	.quad	_Rotate__uRotate_param_i16_C
	.quad	_L_1+86
	.quad	_Rotate__uRotate_var_i16_C
	.quad	_L_1+106
	.quad	_Rotate__uRotate_param_i16_I
	.quad	_L_1+124
	.quad	_Rotate__uRotate_var_i16_I
	.quad	_L_1+144
	.quad	_Rotate__uRotate_param_L_C
	.quad	_L_1+162
	.quad	_Rotate__uRotate_var_L_C
	.quad	_L_1+180
	.quad	_Rotate__uRotate_param_L_I
	.quad	_L_1+196
	.quad	_Rotate__uRotate_var_L_I
	.quad	_L_1+214
	.quad	_Rotate__uRotate_param_u64_C
	.quad	_L_1+230
	.quad	_Rotate__uRotate_var_u64_C
	.quad	_L_1+250
	.quad	_Rotate__uRotate_param_u64_I
	.quad	_L_1+268
	.quad	_Rotate__uRotate_var_u64_I
	.quad	_L_1+288
	.quad	_Rotate__uRotate_param_u16_C
	.quad	_L_1+306
	.quad	_Rotate__uRotate_var_u16_C
	.quad	_L_1+326
	.quad	_Rotate__uRotate_param_u16_I
	.quad	_L_1+344
	.quad	_Rotate__uRotate_var_u16_I
	.quad	_L_1+364
	.quad	_Rotate__uRotate_param_C_C
	.quad	_L_1+382
	.quad	_Rotate__uRotate_var_C_C
	.quad	_L_1+400
	.quad	_Rotate__uRotate_param_C_I
	.quad	_L_1+416
	.quad	_Rotate__uRotate_var_C_I
	.quad	_L_1+434
	.quad	_Rotate__uRotate_param_u32_C
	.quad	_L_1+450
	.quad	_Rotate__uRotate_var_u32_C
	.quad	_L_1+470
	.quad	_Rotate__uRotate_param_u32_I
	.quad	_L_1+488
	.quad	_Rotate__uRotate_var_u32_I
	.quad	_L_1+508
	.quad	_Rotate__uRotate_param_i8_C
	.quad	_L_1+526
	.quad	_Rotate__uRotate_var_i8_C
	.quad	_L_1+545
	.quad	_Rotate__uRotate_param_i8_I
	.quad	_L_1+562
	.quad	_Rotate__uRotate_var_i8_I
	.quad	_L_1+581
	.quad	_Rotate__uRotate_param_I_C
	.quad	_L_1+598
	.quad	_Rotate__uRotate_var_I_C
	.quad	_L_1+616
	.quad	_Rotate__uRotate_param_I_I
	.quad	_L_1+632
	.quad	_Rotate__uRotate_var_I_I
	.quad	_L_1+650
	.quad	_Rotate__uRotate_param_u8_C
	.quad	_L_1+666
	.quad	_Rotate__uRotate_var_u8_C
	.quad	_L_1+685
	.quad	_Rotate__uRotate_param_u8_I
	.quad	_L_1+702
	.quad	_Rotate__uRotate_var_u8_I
	.quad	_L_1+721
	.quad	_Rotate__uRotate_param_LC_C
	.quad	_L_1+738
	.quad	_Rotate__uRotate_var_LC_C
	.quad	_L_1+757
	.quad	_Rotate__uRotate_param_LC_I
	.quad	_L_1+774
	.quad	_Rotate__uRotate_var_LC_I
	.quad	_L_1+793
	.quad	_Rotate__uRotate_param_i64_C
	.quad	_L_1+810
	.quad	_Rotate__uRotate_var_i64_C
	.quad	_L_1+830
	.quad	_Rotate__uRotate_param_i64_I
	.quad	_L_1+848
	.quad	_Rotate__uRotate_var_i64_I
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
	.quad	_MM_Rotate+208
	.space 8
	.quad	_Rotate_M3
	.quad	3
	.quad	785
	.quad	786
	.long	1145369199
	.byte	21
	.space 3
	.quad	790
	.byte	23
	.space 3
	.long	792
	.quad	793
	.word	794
	.space 6
	.quad	795
	.long	1099511628
	.long	1082713696
	.quad	798
	.word	799
	.space 2
	.long	800
	.space 24
	.quad	_Rotate_I3
	.quad	_MM_Rotate+232
	.space 8
	.quad	_Long_I3
	.quad	_MM_Rotate+256
	.space 8
	.quad	_Word_I3
	.quad	_MM_Rotate+280
	.space 8
	.quad	_Cstdint_I3
	.quad	_MM_Rotate+304
	.space 8
	.quad	_RTHooks_I3
	.space 8
	.subsections_via_symbols
