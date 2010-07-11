	.text
.globl _LeftShift__uLeftShift_var_u16_I
	.private_extern _LeftShift__uLeftShift_var_u16_I
_LeftShift__uLeftShift_var_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	144+_MM_LeftShift(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L2
	cmpq	$63, -16(%rbp)
	jle	L3
L2:
	movl	$1729, %edi
	call	__m3_fault
L3:
	cmpq	$63, -16(%rbp)
	jg	L4
	movzwl	104+_MM_LeftShift(%rip), %eax
	movswq	%ax,%rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -24(%rbp)
	jmp	L5
L4:
	movq	$0, -24(%rbp)
L5:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_param_u16_I
	.private_extern _LeftShift__uLeftShift_param_u16_I
_LeftShift__uLeftShift_param_u16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L8
	cmpq	$63, -16(%rbp)
	jle	L9
L8:
	movl	$1761, %edi
	call	__m3_fault
L9:
	cmpq	$63, -16(%rbp)
	jg	L10
	movswq	-18(%rbp),%rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L11
L10:
	movq	$0, -40(%rbp)
L11:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_var_u16_C
	.private_extern _LeftShift__uLeftShift_var_u16_C
_LeftShift__uLeftShift_var_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	160+_MM_LeftShift(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L14
	movl	$1793, %edi
	call	__m3_fault
L14:
	cmpq	$63, -16(%rbp)
	jg	L15
	movzwl	104+_MM_LeftShift(%rip), %eax
	movswq	%ax,%rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -24(%rbp)
	jmp	L16
L15:
	movq	$0, -24(%rbp)
L16:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_param_u16_C
	.private_extern _LeftShift__uLeftShift_param_u16_C
_LeftShift__uLeftShift_param_u16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L19
	movl	$1825, %edi
	call	__m3_fault
L19:
	cmpq	$63, -16(%rbp)
	jg	L20
	movswq	-18(%rbp),%rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L21
L20:
	movq	$0, -40(%rbp)
L21:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_var_u64_I
	.private_extern _LeftShift__uLeftShift_var_u64_I
_LeftShift__uLeftShift_var_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	144+_MM_LeftShift(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L24
	cmpq	$63, -16(%rbp)
	jle	L25
L24:
	movl	$1857, %edi
	call	__m3_fault
L25:
	cmpq	$63, -16(%rbp)
	jg	L26
	movq	112+_MM_LeftShift(%rip), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -24(%rbp)
	jmp	L27
L26:
	movq	$0, -24(%rbp)
L27:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_param_u64_I
	.private_extern _LeftShift__uLeftShift_param_u64_I
_LeftShift__uLeftShift_param_u64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L30
	cmpq	$63, -16(%rbp)
	jle	L31
L30:
	movl	$1889, %edi
	call	__m3_fault
L31:
	cmpq	$63, -16(%rbp)
	jg	L32
	movq	-24(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L33
L32:
	movq	$0, -40(%rbp)
L33:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_var_u64_C
	.private_extern _LeftShift__uLeftShift_var_u64_C
_LeftShift__uLeftShift_var_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	160+_MM_LeftShift(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L36
	movl	$1921, %edi
	call	__m3_fault
L36:
	cmpq	$63, -16(%rbp)
	jg	L37
	movq	112+_MM_LeftShift(%rip), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -24(%rbp)
	jmp	L38
L37:
	movq	$0, -24(%rbp)
L38:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_param_u64_C
	.private_extern _LeftShift__uLeftShift_param_u64_C
_LeftShift__uLeftShift_param_u64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L41
	movl	$1953, %edi
	call	__m3_fault
L41:
	cmpq	$63, -16(%rbp)
	jg	L42
	movq	-24(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L43
L42:
	movq	$0, -40(%rbp)
L43:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_var_LC_I
	.private_extern _LeftShift__uLeftShift_var_LC_I
_LeftShift__uLeftShift_var_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	144+_MM_LeftShift(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L46
	cmpq	$63, -16(%rbp)
	jle	L47
L46:
	movl	$1985, %edi
	call	__m3_fault
L47:
	cmpq	$63, -16(%rbp)
	jg	L48
	movq	120+_MM_LeftShift(%rip), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -24(%rbp)
	jmp	L49
L48:
	movq	$0, -24(%rbp)
L49:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_param_LC_I
	.private_extern _LeftShift__uLeftShift_param_LC_I
_LeftShift__uLeftShift_param_LC_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L52
	cmpq	$63, -16(%rbp)
	jle	L53
L52:
	movl	$2017, %edi
	call	__m3_fault
L53:
	cmpq	$63, -16(%rbp)
	jg	L54
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L55
L54:
	movq	$0, -40(%rbp)
L55:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_var_LC_C
	.private_extern _LeftShift__uLeftShift_var_LC_C
_LeftShift__uLeftShift_var_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	160+_MM_LeftShift(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L58
	movl	$2049, %edi
	call	__m3_fault
L58:
	cmpq	$63, -16(%rbp)
	jg	L59
	movq	120+_MM_LeftShift(%rip), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -24(%rbp)
	jmp	L60
L59:
	movq	$0, -24(%rbp)
L60:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_param_LC_C
	.private_extern _LeftShift__uLeftShift_param_LC_C
_LeftShift__uLeftShift_param_LC_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L63
	movl	$2081, %edi
	call	__m3_fault
L63:
	cmpq	$63, -16(%rbp)
	jg	L64
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L65
L64:
	movq	$0, -40(%rbp)
L65:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_var_i16_I
	.private_extern _LeftShift__uLeftShift_var_i16_I
_LeftShift__uLeftShift_var_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	144+_MM_LeftShift(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L68
	cmpq	$63, -16(%rbp)
	jle	L69
L68:
	movl	$2113, %edi
	call	__m3_fault
L69:
	cmpq	$63, -16(%rbp)
	jg	L70
	movzwl	136+_MM_LeftShift(%rip), %eax
	movzwl	%ax, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -24(%rbp)
	jmp	L71
L70:
	movq	$0, -24(%rbp)
L71:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_param_i16_I
	.private_extern _LeftShift__uLeftShift_param_i16_I
_LeftShift__uLeftShift_param_i16_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L74
	cmpq	$63, -16(%rbp)
	jle	L75
L74:
	movl	$2145, %edi
	call	__m3_fault
L75:
	cmpq	$63, -16(%rbp)
	jg	L76
	movzwl	-18(%rbp), %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L77
L76:
	movq	$0, -40(%rbp)
L77:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_var_i16_C
	.private_extern _LeftShift__uLeftShift_var_i16_C
_LeftShift__uLeftShift_var_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	160+_MM_LeftShift(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L80
	movl	$2177, %edi
	call	__m3_fault
L80:
	cmpq	$63, -16(%rbp)
	jg	L81
	movzwl	136+_MM_LeftShift(%rip), %eax
	movzwl	%ax, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -24(%rbp)
	jmp	L82
L81:
	movq	$0, -24(%rbp)
L82:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_param_i16_C
	.private_extern _LeftShift__uLeftShift_param_i16_C
_LeftShift__uLeftShift_param_i16_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L85
	movl	$2209, %edi
	call	__m3_fault
L85:
	cmpq	$63, -16(%rbp)
	jg	L86
	movzwl	-18(%rbp), %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L87
L86:
	movq	$0, -40(%rbp)
L87:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_var_i32_I
	.private_extern _LeftShift__uLeftShift_var_i32_I
_LeftShift__uLeftShift_var_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	144+_MM_LeftShift(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L90
	cmpq	$63, -16(%rbp)
	jle	L91
L90:
	movl	$2241, %edi
	call	__m3_fault
L91:
	cmpq	$63, -16(%rbp)
	jg	L92
	movl	140+_MM_LeftShift(%rip), %eax
	mov	%eax, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -24(%rbp)
	jmp	L93
L92:
	movq	$0, -24(%rbp)
L93:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_param_i32_I
	.private_extern _LeftShift__uLeftShift_param_i32_I
_LeftShift__uLeftShift_param_i32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L96
	cmpq	$63, -16(%rbp)
	jle	L97
L96:
	movl	$2273, %edi
	call	__m3_fault
L97:
	cmpq	$63, -16(%rbp)
	jg	L98
	mov	-20(%rbp), %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L99
L98:
	movq	$0, -40(%rbp)
L99:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_var_i32_C
	.private_extern _LeftShift__uLeftShift_var_i32_C
_LeftShift__uLeftShift_var_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	160+_MM_LeftShift(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L102
	movl	$2305, %edi
	call	__m3_fault
L102:
	cmpq	$63, -16(%rbp)
	jg	L103
	movl	140+_MM_LeftShift(%rip), %eax
	mov	%eax, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -24(%rbp)
	jmp	L104
L103:
	movq	$0, -24(%rbp)
L104:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_param_i32_C
	.private_extern _LeftShift__uLeftShift_param_i32_C
_LeftShift__uLeftShift_param_i32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L107
	movl	$2337, %edi
	call	__m3_fault
L107:
	cmpq	$63, -16(%rbp)
	jg	L108
	mov	-20(%rbp), %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L109
L108:
	movq	$0, -40(%rbp)
L109:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_var_I_I
	.private_extern _LeftShift__uLeftShift_var_I_I
_LeftShift__uLeftShift_var_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	144+_MM_LeftShift(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L112
	cmpq	$63, -16(%rbp)
	jle	L113
L112:
	movl	$2369, %edi
	call	__m3_fault
L113:
	cmpq	$63, -16(%rbp)
	jg	L114
	movq	144+_MM_LeftShift(%rip), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -24(%rbp)
	jmp	L115
L114:
	movq	$0, -24(%rbp)
L115:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_param_I_I
	.private_extern _LeftShift__uLeftShift_param_I_I
_LeftShift__uLeftShift_param_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L118
	cmpq	$63, -16(%rbp)
	jle	L119
L118:
	movl	$2401, %edi
	call	__m3_fault
L119:
	cmpq	$63, -16(%rbp)
	jg	L120
	movq	-24(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L121
L120:
	movq	$0, -40(%rbp)
L121:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_var_I_C
	.private_extern _LeftShift__uLeftShift_var_I_C
_LeftShift__uLeftShift_var_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	160+_MM_LeftShift(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L124
	movl	$2433, %edi
	call	__m3_fault
L124:
	cmpq	$63, -16(%rbp)
	jg	L125
	movq	144+_MM_LeftShift(%rip), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -24(%rbp)
	jmp	L126
L125:
	movq	$0, -24(%rbp)
L126:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_param_I_C
	.private_extern _LeftShift__uLeftShift_param_I_C
_LeftShift__uLeftShift_param_I_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L129
	movl	$2465, %edi
	call	__m3_fault
L129:
	cmpq	$63, -16(%rbp)
	jg	L130
	movq	-24(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L131
L130:
	movq	$0, -40(%rbp)
L131:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_var_i64_I
	.private_extern _LeftShift__uLeftShift_var_i64_I
_LeftShift__uLeftShift_var_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	144+_MM_LeftShift(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L134
	cmpq	$63, -16(%rbp)
	jle	L135
L134:
	movl	$2497, %edi
	call	__m3_fault
L135:
	cmpq	$63, -16(%rbp)
	jg	L136
	movq	152+_MM_LeftShift(%rip), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -24(%rbp)
	jmp	L137
L136:
	movq	$0, -24(%rbp)
L137:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_param_i64_I
	.private_extern _LeftShift__uLeftShift_param_i64_I
_LeftShift__uLeftShift_param_i64_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L140
	cmpq	$63, -16(%rbp)
	jle	L141
L140:
	movl	$2529, %edi
	call	__m3_fault
L141:
	cmpq	$63, -16(%rbp)
	jg	L142
	movq	-24(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L143
L142:
	movq	$0, -40(%rbp)
L143:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_var_i64_C
	.private_extern _LeftShift__uLeftShift_var_i64_C
_LeftShift__uLeftShift_var_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	160+_MM_LeftShift(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L146
	movl	$2561, %edi
	call	__m3_fault
L146:
	cmpq	$63, -16(%rbp)
	jg	L147
	movq	152+_MM_LeftShift(%rip), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -24(%rbp)
	jmp	L148
L147:
	movq	$0, -24(%rbp)
L148:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_param_i64_C
	.private_extern _LeftShift__uLeftShift_param_i64_C
_LeftShift__uLeftShift_param_i64_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L151
	movl	$2593, %edi
	call	__m3_fault
L151:
	cmpq	$63, -16(%rbp)
	jg	L152
	movq	-24(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L153
L152:
	movq	$0, -40(%rbp)
L153:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_var_C_I
	.private_extern _LeftShift__uLeftShift_var_C_I
_LeftShift__uLeftShift_var_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	144+_MM_LeftShift(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L156
	cmpq	$63, -16(%rbp)
	jle	L157
L156:
	movl	$2625, %edi
	call	__m3_fault
L157:
	cmpq	$63, -16(%rbp)
	jg	L158
	movq	160+_MM_LeftShift(%rip), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -24(%rbp)
	jmp	L159
L158:
	movq	$0, -24(%rbp)
L159:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_param_C_I
	.private_extern _LeftShift__uLeftShift_param_C_I
_LeftShift__uLeftShift_param_C_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L162
	cmpq	$63, -16(%rbp)
	jle	L163
L162:
	movl	$2657, %edi
	call	__m3_fault
L163:
	cmpq	$63, -16(%rbp)
	jg	L164
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L165
L164:
	movq	$0, -40(%rbp)
L165:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_var_C_C
	.private_extern _LeftShift__uLeftShift_var_C_C
_LeftShift__uLeftShift_var_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	160+_MM_LeftShift(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L168
	movl	$2689, %edi
	call	__m3_fault
L168:
	cmpq	$63, -16(%rbp)
	jg	L169
	movq	160+_MM_LeftShift(%rip), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -24(%rbp)
	jmp	L170
L169:
	movq	$0, -24(%rbp)
L170:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_param_C_C
	.private_extern _LeftShift__uLeftShift_param_C_C
_LeftShift__uLeftShift_param_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L173
	movl	$2721, %edi
	call	__m3_fault
L173:
	cmpq	$63, -16(%rbp)
	jg	L174
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	salq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L175
L174:
	movq	$0, -40(%rbp)
L175:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_var_u8_I
	.private_extern _LeftShift__uLeftShift_var_u8_I
_LeftShift__uLeftShift_var_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	144+_MM_LeftShift(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L178
	cmpq	$63, -16(%rbp)
	jle	L179
L178:
	movl	$2753, %edi
	call	__m3_fault
L179:
	cmpq	$63, -16(%rbp)
	jg	L180
	movzbl	172+_MM_LeftShift(%rip), %eax
	movsbq	%al,%rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -24(%rbp)
	jmp	L181
L180:
	movq	$0, -24(%rbp)
L181:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_param_u8_I
	.private_extern _LeftShift__uLeftShift_param_u8_I
_LeftShift__uLeftShift_param_u8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L184
	cmpq	$63, -16(%rbp)
	jle	L185
L184:
	movl	$2785, %edi
	call	__m3_fault
L185:
	cmpq	$63, -16(%rbp)
	jg	L186
	movsbq	-17(%rbp),%rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L187
L186:
	movq	$0, -40(%rbp)
L187:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_var_u8_C
	.private_extern _LeftShift__uLeftShift_var_u8_C
_LeftShift__uLeftShift_var_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	160+_MM_LeftShift(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L190
	movl	$2817, %edi
	call	__m3_fault
L190:
	cmpq	$63, -16(%rbp)
	jg	L191
	movzbl	172+_MM_LeftShift(%rip), %eax
	movsbq	%al,%rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -24(%rbp)
	jmp	L192
L191:
	movq	$0, -24(%rbp)
L192:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_param_u8_C
	.private_extern _LeftShift__uLeftShift_param_u8_C
_LeftShift__uLeftShift_param_u8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L195
	movl	$2849, %edi
	call	__m3_fault
L195:
	cmpq	$63, -16(%rbp)
	jg	L196
	movsbq	-17(%rbp),%rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L197
L196:
	movq	$0, -40(%rbp)
L197:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_var_i8_I
	.private_extern _LeftShift__uLeftShift_var_i8_I
_LeftShift__uLeftShift_var_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	144+_MM_LeftShift(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L200
	cmpq	$63, -16(%rbp)
	jle	L201
L200:
	movl	$2881, %edi
	call	__m3_fault
L201:
	cmpq	$63, -16(%rbp)
	jg	L202
	movzbl	173+_MM_LeftShift(%rip), %eax
	movzbl	%al, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -24(%rbp)
	jmp	L203
L202:
	movq	$0, -24(%rbp)
L203:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_param_i8_I
	.private_extern _LeftShift__uLeftShift_param_i8_I
_LeftShift__uLeftShift_param_i8_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L206
	cmpq	$63, -16(%rbp)
	jle	L207
L206:
	movl	$2913, %edi
	call	__m3_fault
L207:
	cmpq	$63, -16(%rbp)
	jg	L208
	movzbl	-17(%rbp), %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L209
L208:
	movq	$0, -40(%rbp)
L209:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_var_i8_C
	.private_extern _LeftShift__uLeftShift_var_i8_C
_LeftShift__uLeftShift_var_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	160+_MM_LeftShift(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L212
	movl	$2945, %edi
	call	__m3_fault
L212:
	cmpq	$63, -16(%rbp)
	jg	L213
	movzbl	173+_MM_LeftShift(%rip), %eax
	movzbl	%al, %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -24(%rbp)
	jmp	L214
L213:
	movq	$0, -24(%rbp)
L214:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_param_i8_C
	.private_extern _LeftShift__uLeftShift_param_i8_C
_LeftShift__uLeftShift_param_i8_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L217
	movl	$2977, %edi
	call	__m3_fault
L217:
	cmpq	$63, -16(%rbp)
	jg	L218
	movzbl	-17(%rbp), %eax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L219
L218:
	movq	$0, -40(%rbp)
L219:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_var_L_I
	.private_extern _LeftShift__uLeftShift_var_L_I
_LeftShift__uLeftShift_var_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	144+_MM_LeftShift(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L222
	cmpq	$63, -16(%rbp)
	jle	L223
L222:
	movl	$3009, %edi
	call	__m3_fault
L223:
	cmpq	$63, -16(%rbp)
	jg	L224
	movq	176+_MM_LeftShift(%rip), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -24(%rbp)
	jmp	L225
L224:
	movq	$0, -24(%rbp)
L225:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_param_L_I
	.private_extern _LeftShift__uLeftShift_param_L_I
_LeftShift__uLeftShift_param_L_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L228
	cmpq	$63, -16(%rbp)
	jle	L229
L228:
	movl	$3041, %edi
	call	__m3_fault
L229:
	cmpq	$63, -16(%rbp)
	jg	L230
	movq	-24(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L231
L230:
	movq	$0, -40(%rbp)
L231:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_var_L_C
	.private_extern _LeftShift__uLeftShift_var_L_C
_LeftShift__uLeftShift_var_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	160+_MM_LeftShift(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L234
	movl	$3073, %edi
	call	__m3_fault
L234:
	cmpq	$63, -16(%rbp)
	jg	L235
	movq	176+_MM_LeftShift(%rip), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -24(%rbp)
	jmp	L236
L235:
	movq	$0, -24(%rbp)
L236:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_param_L_C
	.private_extern _LeftShift__uLeftShift_param_L_C
_LeftShift__uLeftShift_param_L_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L239
	movl	$3105, %edi
	call	__m3_fault
L239:
	cmpq	$63, -16(%rbp)
	jg	L240
	movq	-24(%rbp), %rax
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L241
L240:
	movq	$0, -40(%rbp)
L241:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_var_u32_I
	.private_extern _LeftShift__uLeftShift_var_u32_I
_LeftShift__uLeftShift_var_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	144+_MM_LeftShift(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L244
	cmpq	$63, -16(%rbp)
	jle	L245
L244:
	movl	$3137, %edi
	call	__m3_fault
L245:
	cmpq	$63, -16(%rbp)
	jg	L246
	movl	184+_MM_LeftShift(%rip), %eax
	cltq
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -24(%rbp)
	jmp	L247
L246:
	movq	$0, -24(%rbp)
L247:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_param_u32_I
	.private_extern _LeftShift__uLeftShift_param_u32_I
_LeftShift__uLeftShift_param_u32_I:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L250
	cmpq	$63, -16(%rbp)
	jle	L251
L250:
	movl	$3169, %edi
	call	__m3_fault
L251:
	cmpq	$63, -16(%rbp)
	jg	L252
	movl	-20(%rbp), %eax
	cltq
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L253
L252:
	movq	$0, -40(%rbp)
L253:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_var_u32_C
	.private_extern _LeftShift__uLeftShift_var_u32_C
_LeftShift__uLeftShift_var_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	160+_MM_LeftShift(%rip), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L256
	movl	$3201, %edi
	call	__m3_fault
L256:
	cmpq	$63, -16(%rbp)
	jg	L257
	movl	184+_MM_LeftShift(%rip), %eax
	cltq
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -24(%rbp)
	jmp	L258
L257:
	movq	$0, -24(%rbp)
L258:
	movq	-24(%rbp), %rax
	leave
	ret
.globl _LeftShift__uLeftShift_param_u32_C
	.private_extern _LeftShift__uLeftShift_param_u32_C
_LeftShift__uLeftShift_param_u32_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$63, -16(%rbp)
	jle	L261
	movl	$3233, %edi
	call	__m3_fault
L261:
	cmpq	$63, -16(%rbp)
	jg	L262
	movl	-20(%rbp), %eax
	cltq
	movq	-16(%rbp), %rcx
	salq	%cl, %rax
	movq	%rax, -40(%rbp)
	jmp	L263
L262:
	movq	$0, -40(%rbp)
L263:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _LeftShift_M3
_LeftShift_M3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	leaq	_MM_LeftShift(%rip), %rax
	leave
	ret
__m3_fault:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rsi
	leaq	_MM_LeftShift(%rip), %rdi
	call	_RTHooks__ReportFault
	leave
	ret
	.const_data
	.align 5
_L_1:
	.ascii "LeftShift_M3"
	.space 1
	.ascii "uLeftShift_param_u32_C"
	.space 1
	.ascii "uLeftShift_var_u32_C"
	.space 1
	.ascii "uLeftShift_param_u32_I"
	.space 1
	.ascii "uLeftShift_var_u32_I"
	.space 1
	.ascii "uLeftShift_param_L_C"
	.space 1
	.ascii "uLeftShift_var_L_C"
	.space 1
	.ascii "uLeftShift_param_L_I"
	.space 1
	.ascii "uLeftShift_var_L_I"
	.space 1
	.ascii "uLeftShift_param_i8_C"
	.space 1
	.ascii "uLeftShift_var_i8_C"
	.space 1
	.ascii "uLeftShift_param_i8_I"
	.space 1
	.ascii "uLeftShift_var_i8_I"
	.space 1
	.ascii "uLeftShift_param_u8_C"
	.space 1
	.ascii "uLeftShift_var_u8_C"
	.space 1
	.ascii "uLeftShift_param_u8_I"
	.space 1
	.ascii "uLeftShift_var_u8_I"
	.space 1
	.ascii "uLeftShift_param_C_C"
	.space 1
	.ascii "uLeftShift_var_C_C"
	.space 1
	.ascii "uLeftShift_param_C_I"
	.space 1
	.ascii "uLeftShift_var_C_I"
	.space 1
	.ascii "uLeftShift_param_i64_C"
	.space 1
	.ascii "uLeftShift_var_i64_C"
	.space 1
	.ascii "uLeftShift_param_i64_I"
	.space 1
	.ascii "uLeftShift_var_i64_I"
	.space 1
	.ascii "uLeftShift_param_I_C"
	.space 1
	.ascii "uLeftShift_var_I_C"
	.space 1
	.ascii "uLeftShift_param_I_I"
	.space 1
	.ascii "uLeftShift_var_I_I"
	.space 1
	.ascii "uLeftShift_param_i32_C"
	.space 1
	.ascii "uLeftShift_var_i32_C"
	.space 1
	.ascii "uLeftShift_param_i32_I"
	.space 1
	.ascii "uLeftShift_var_i32_I"
	.space 1
	.ascii "uLeftShift_param_i16_C"
	.space 1
	.ascii "uLeftShift_var_i16_C"
	.space 1
	.ascii "uLeftShift_param_i16_I"
	.space 1
	.ascii "uLeftShift_var_i16_I"
	.space 1
	.ascii "uLeftShift_param_LC_C"
	.space 1
	.ascii "uLeftShift_var_LC_C"
	.space 1
	.ascii "uLeftShift_param_LC_I"
	.space 1
	.ascii "uLeftShift_var_LC_I"
	.space 1
	.ascii "uLeftShift_param_u64_C"
	.space 1
	.ascii "uLeftShift_var_u64_C"
	.space 1
	.ascii "uLeftShift_param_u64_I"
	.space 1
	.ascii "uLeftShift_var_u64_I"
	.space 1
	.ascii "uLeftShift_param_u16_C"
	.space 1
	.ascii "uLeftShift_var_u16_C"
	.space 1
	.ascii "uLeftShift_param_u16_I"
	.space 1
	.ascii "uLeftShift_var_u16_I"
	.space 8
	.quad	_LeftShift_M3
	.quad	_L_1
	.quad	_LeftShift__uLeftShift_param_u32_C
	.quad	_L_1+13
	.quad	_LeftShift__uLeftShift_var_u32_C
	.quad	_L_1+36
	.quad	_LeftShift__uLeftShift_param_u32_I
	.quad	_L_1+57
	.quad	_LeftShift__uLeftShift_var_u32_I
	.quad	_L_1+80
	.quad	_LeftShift__uLeftShift_param_L_C
	.quad	_L_1+101
	.quad	_LeftShift__uLeftShift_var_L_C
	.quad	_L_1+122
	.quad	_LeftShift__uLeftShift_param_L_I
	.quad	_L_1+141
	.quad	_LeftShift__uLeftShift_var_L_I
	.quad	_L_1+162
	.quad	_LeftShift__uLeftShift_param_i8_C
	.quad	_L_1+181
	.quad	_LeftShift__uLeftShift_var_i8_C
	.quad	_L_1+203
	.quad	_LeftShift__uLeftShift_param_i8_I
	.quad	_L_1+223
	.quad	_LeftShift__uLeftShift_var_i8_I
	.quad	_L_1+245
	.quad	_LeftShift__uLeftShift_param_u8_C
	.quad	_L_1+265
	.quad	_LeftShift__uLeftShift_var_u8_C
	.quad	_L_1+287
	.quad	_LeftShift__uLeftShift_param_u8_I
	.quad	_L_1+307
	.quad	_LeftShift__uLeftShift_var_u8_I
	.quad	_L_1+329
	.quad	_LeftShift__uLeftShift_param_C_C
	.quad	_L_1+349
	.quad	_LeftShift__uLeftShift_var_C_C
	.quad	_L_1+370
	.quad	_LeftShift__uLeftShift_param_C_I
	.quad	_L_1+389
	.quad	_LeftShift__uLeftShift_var_C_I
	.quad	_L_1+410
	.quad	_LeftShift__uLeftShift_param_i64_C
	.quad	_L_1+429
	.quad	_LeftShift__uLeftShift_var_i64_C
	.quad	_L_1+452
	.quad	_LeftShift__uLeftShift_param_i64_I
	.quad	_L_1+473
	.quad	_LeftShift__uLeftShift_var_i64_I
	.quad	_L_1+496
	.quad	_LeftShift__uLeftShift_param_I_C
	.quad	_L_1+517
	.quad	_LeftShift__uLeftShift_var_I_C
	.quad	_L_1+538
	.quad	_LeftShift__uLeftShift_param_I_I
	.quad	_L_1+557
	.quad	_LeftShift__uLeftShift_var_I_I
	.quad	_L_1+578
	.quad	_LeftShift__uLeftShift_param_i32_C
	.quad	_L_1+597
	.quad	_LeftShift__uLeftShift_var_i32_C
	.quad	_L_1+620
	.quad	_LeftShift__uLeftShift_param_i32_I
	.quad	_L_1+641
	.quad	_LeftShift__uLeftShift_var_i32_I
	.quad	_L_1+664
	.quad	_LeftShift__uLeftShift_param_i16_C
	.quad	_L_1+685
	.quad	_LeftShift__uLeftShift_var_i16_C
	.quad	_L_1+708
	.quad	_LeftShift__uLeftShift_param_i16_I
	.quad	_L_1+729
	.quad	_LeftShift__uLeftShift_var_i16_I
	.quad	_L_1+752
	.quad	_LeftShift__uLeftShift_param_LC_C
	.quad	_L_1+773
	.quad	_LeftShift__uLeftShift_var_LC_C
	.quad	_L_1+795
	.quad	_LeftShift__uLeftShift_param_LC_I
	.quad	_L_1+815
	.quad	_LeftShift__uLeftShift_var_LC_I
	.quad	_L_1+837
	.quad	_LeftShift__uLeftShift_param_u64_C
	.quad	_L_1+857
	.quad	_LeftShift__uLeftShift_var_u64_C
	.quad	_L_1+880
	.quad	_LeftShift__uLeftShift_param_u64_I
	.quad	_L_1+901
	.quad	_LeftShift__uLeftShift_var_u64_I
	.quad	_L_1+924
	.quad	_LeftShift__uLeftShift_param_u16_C
	.quad	_L_1+945
	.quad	_LeftShift__uLeftShift_var_u16_C
	.quad	_L_1+968
	.quad	_LeftShift__uLeftShift_param_u16_I
	.quad	_L_1+989
	.quad	_LeftShift__uLeftShift_var_u16_I
	.quad	_L_1+1012
	.space 8
	.ascii "../AMD64_DARWIN/LeftShift.m3"
	.space 4
	.data
	.align 5
_MM_LeftShift:
	.quad	_L_1+1832
	.space 32
	.quad	_L_1+1040
	.space 24
	.quad	_MM_LeftShift+208
	.space 8
	.quad	_LeftShift_M3
	.quad	3
	.word	625
	.space 6
	.quad	626
	.quad	627
	.long	824633721
	.long	1082369288
	.word	630
	.space 2
	.long	631
	.quad	632
	.quad	633
	.quad	634
	.long	1142876340
	.byte	125
	.byte	126
	.space 2
	.quad	639
	.long	640
	.space 28
	.quad	_LeftShift_I3
	.quad	_MM_LeftShift+232
	.space 8
	.quad	_Long_I3
	.quad	_MM_LeftShift+256
	.space 8
	.quad	_Word_I3
	.quad	_MM_LeftShift+280
	.space 8
	.quad	_Cstdint_I3
	.quad	_MM_LeftShift+304
	.space 8
	.quad	_RTHooks_I3
	.space 8
	.subsections_via_symbols
