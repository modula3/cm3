	.text
.globl _div_pow2__div_pow2_i32_0
	.private_extern _div_pow2__div_pow2_i32_0
_div_pow2__div_pow2_i32_0:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L2
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L3
L2:
	movl	$1761, %edi
	call	__m3_fault
L3:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_i32_1
	.private_extern _div_pow2__div_pow2_i32_1
_div_pow2__div_pow2_i32_1:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	%rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L6
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L7
L6:
	movl	$1825, %edi
	call	__m3_fault
L7:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_i32_2
	.private_extern _div_pow2__div_pow2_i32_2
_div_pow2__div_pow2_i32_2:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$2, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L10
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L11
L10:
	movl	$1889, %edi
	call	__m3_fault
L11:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_i32_3
	.private_extern _div_pow2__div_pow2_i32_3
_div_pow2__div_pow2_i32_3:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$3, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L14
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L15
L14:
	movl	$1953, %edi
	call	__m3_fault
L15:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_i32_4
	.private_extern _div_pow2__div_pow2_i32_4
_div_pow2__div_pow2_i32_4:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$4, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L18
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L19
L18:
	movl	$2017, %edi
	call	__m3_fault
L19:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_i32_5
	.private_extern _div_pow2__div_pow2_i32_5
_div_pow2__div_pow2_i32_5:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$5, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L22
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L23
L22:
	movl	$2081, %edi
	call	__m3_fault
L23:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_i32_6
	.private_extern _div_pow2__div_pow2_i32_6
_div_pow2__div_pow2_i32_6:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$6, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L26
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L27
L26:
	movl	$2145, %edi
	call	__m3_fault
L27:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_i32_7
	.private_extern _div_pow2__div_pow2_i32_7
_div_pow2__div_pow2_i32_7:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$7, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L30
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L31
L30:
	movl	$2209, %edi
	call	__m3_fault
L31:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_i32_8
	.private_extern _div_pow2__div_pow2_i32_8
_div_pow2__div_pow2_i32_8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$8, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L34
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L35
L34:
	movl	$2273, %edi
	call	__m3_fault
L35:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_i32_9
	.private_extern _div_pow2__div_pow2_i32_9
_div_pow2__div_pow2_i32_9:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$9, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L38
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L39
L38:
	movl	$2337, %edi
	call	__m3_fault
L39:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_i32_10
	.private_extern _div_pow2__div_pow2_i32_10
_div_pow2__div_pow2_i32_10:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$10, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L42
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L43
L42:
	movl	$2401, %edi
	call	__m3_fault
L43:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_i32_11
	.private_extern _div_pow2__div_pow2_i32_11
_div_pow2__div_pow2_i32_11:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$11, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L46
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L47
L46:
	movl	$2465, %edi
	call	__m3_fault
L47:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_i32_12
	.private_extern _div_pow2__div_pow2_i32_12
_div_pow2__div_pow2_i32_12:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$12, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L50
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L51
L50:
	movl	$2529, %edi
	call	__m3_fault
L51:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_i32_13
	.private_extern _div_pow2__div_pow2_i32_13
_div_pow2__div_pow2_i32_13:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$13, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L54
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L55
L54:
	movl	$2593, %edi
	call	__m3_fault
L55:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_i32_14
	.private_extern _div_pow2__div_pow2_i32_14
_div_pow2__div_pow2_i32_14:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$14, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L58
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L59
L58:
	movl	$2657, %edi
	call	__m3_fault
L59:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_i32_15
	.private_extern _div_pow2__div_pow2_i32_15
_div_pow2__div_pow2_i32_15:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$15, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L62
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L63
L62:
	movl	$2721, %edi
	call	__m3_fault
L63:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_i32_16
	.private_extern _div_pow2__div_pow2_i32_16
_div_pow2__div_pow2_i32_16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$16, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L66
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L67
L66:
	movl	$2785, %edi
	call	__m3_fault
L67:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_i32_17
	.private_extern _div_pow2__div_pow2_i32_17
_div_pow2__div_pow2_i32_17:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$17, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L70
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L71
L70:
	movl	$2849, %edi
	call	__m3_fault
L71:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_i32_18
	.private_extern _div_pow2__div_pow2_i32_18
_div_pow2__div_pow2_i32_18:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$18, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L74
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L75
L74:
	movl	$2913, %edi
	call	__m3_fault
L75:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_i32_19
	.private_extern _div_pow2__div_pow2_i32_19
_div_pow2__div_pow2_i32_19:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$19, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L78
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L79
L78:
	movl	$2977, %edi
	call	__m3_fault
L79:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_i32_20
	.private_extern _div_pow2__div_pow2_i32_20
_div_pow2__div_pow2_i32_20:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$20, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L82
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L83
L82:
	movl	$3041, %edi
	call	__m3_fault
L83:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_i32_21
	.private_extern _div_pow2__div_pow2_i32_21
_div_pow2__div_pow2_i32_21:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$21, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L86
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L87
L86:
	movl	$3105, %edi
	call	__m3_fault
L87:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_i32_22
	.private_extern _div_pow2__div_pow2_i32_22
_div_pow2__div_pow2_i32_22:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$22, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L90
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L91
L90:
	movl	$3169, %edi
	call	__m3_fault
L91:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_i32_23
	.private_extern _div_pow2__div_pow2_i32_23
_div_pow2__div_pow2_i32_23:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$23, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L94
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L95
L94:
	movl	$3233, %edi
	call	__m3_fault
L95:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_i32_24
	.private_extern _div_pow2__div_pow2_i32_24
_div_pow2__div_pow2_i32_24:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$24, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L98
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L99
L98:
	movl	$3297, %edi
	call	__m3_fault
L99:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_i32_25
	.private_extern _div_pow2__div_pow2_i32_25
_div_pow2__div_pow2_i32_25:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$25, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L102
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L103
L102:
	movl	$3361, %edi
	call	__m3_fault
L103:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_i32_26
	.private_extern _div_pow2__div_pow2_i32_26
_div_pow2__div_pow2_i32_26:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$26, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L106
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L107
L106:
	movl	$3425, %edi
	call	__m3_fault
L107:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_i32_27
	.private_extern _div_pow2__div_pow2_i32_27
_div_pow2__div_pow2_i32_27:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$27, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L110
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L111
L110:
	movl	$3489, %edi
	call	__m3_fault
L111:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_i32_28
	.private_extern _div_pow2__div_pow2_i32_28
_div_pow2__div_pow2_i32_28:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$28, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L114
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L115
L114:
	movl	$3553, %edi
	call	__m3_fault
L115:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_i32_29
	.private_extern _div_pow2__div_pow2_i32_29
_div_pow2__div_pow2_i32_29:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$29, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L118
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L119
L118:
	movl	$3617, %edi
	call	__m3_fault
L119:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_i32_30
	.private_extern _div_pow2__div_pow2_i32_30
_div_pow2__div_pow2_i32_30:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$30, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L122
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L123
L122:
	movl	$3681, %edi
	call	__m3_fault
L123:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_i32_31
	.private_extern _div_pow2__div_pow2_i32_31
_div_pow2__div_pow2_i32_31:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$31, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L126
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L127
L126:
	movl	$3745, %edi
	call	__m3_fault
L127:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_u32_0
	.private_extern _div_pow2__div_pow2_u32_0
_div_pow2__div_pow2_u32_0:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L130
	cmpq	$2147483647, -16(%rbp)
	jle	L131
L130:
	movl	$3809, %edi
	call	__m3_fault
L131:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_u32_1
	.private_extern _div_pow2__div_pow2_u32_1
_div_pow2__div_pow2_u32_1:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	%rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L134
	cmpq	$2147483647, -16(%rbp)
	jle	L135
L134:
	movl	$3873, %edi
	call	__m3_fault
L135:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_u32_2
	.private_extern _div_pow2__div_pow2_u32_2
_div_pow2__div_pow2_u32_2:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$2, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L138
	cmpq	$2147483647, -16(%rbp)
	jle	L139
L138:
	movl	$3937, %edi
	call	__m3_fault
L139:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_u32_3
	.private_extern _div_pow2__div_pow2_u32_3
_div_pow2__div_pow2_u32_3:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$3, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L142
	cmpq	$2147483647, -16(%rbp)
	jle	L143
L142:
	movl	$4001, %edi
	call	__m3_fault
L143:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_u32_4
	.private_extern _div_pow2__div_pow2_u32_4
_div_pow2__div_pow2_u32_4:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$4, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L146
	cmpq	$2147483647, -16(%rbp)
	jle	L147
L146:
	movl	$4065, %edi
	call	__m3_fault
L147:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_u32_5
	.private_extern _div_pow2__div_pow2_u32_5
_div_pow2__div_pow2_u32_5:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$5, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L150
	cmpq	$2147483647, -16(%rbp)
	jle	L151
L150:
	movl	$4129, %edi
	call	__m3_fault
L151:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_u32_6
	.private_extern _div_pow2__div_pow2_u32_6
_div_pow2__div_pow2_u32_6:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$6, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L154
	cmpq	$2147483647, -16(%rbp)
	jle	L155
L154:
	movl	$4193, %edi
	call	__m3_fault
L155:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_u32_7
	.private_extern _div_pow2__div_pow2_u32_7
_div_pow2__div_pow2_u32_7:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$7, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L158
	cmpq	$2147483647, -16(%rbp)
	jle	L159
L158:
	movl	$4257, %edi
	call	__m3_fault
L159:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_u32_8
	.private_extern _div_pow2__div_pow2_u32_8
_div_pow2__div_pow2_u32_8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$8, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L162
	cmpq	$2147483647, -16(%rbp)
	jle	L163
L162:
	movl	$4321, %edi
	call	__m3_fault
L163:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_u32_9
	.private_extern _div_pow2__div_pow2_u32_9
_div_pow2__div_pow2_u32_9:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$9, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L166
	cmpq	$2147483647, -16(%rbp)
	jle	L167
L166:
	movl	$4385, %edi
	call	__m3_fault
L167:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_u32_10
	.private_extern _div_pow2__div_pow2_u32_10
_div_pow2__div_pow2_u32_10:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$10, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L170
	cmpq	$2147483647, -16(%rbp)
	jle	L171
L170:
	movl	$4449, %edi
	call	__m3_fault
L171:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_u32_11
	.private_extern _div_pow2__div_pow2_u32_11
_div_pow2__div_pow2_u32_11:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$11, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L174
	cmpq	$2147483647, -16(%rbp)
	jle	L175
L174:
	movl	$4513, %edi
	call	__m3_fault
L175:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_u32_12
	.private_extern _div_pow2__div_pow2_u32_12
_div_pow2__div_pow2_u32_12:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$12, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L178
	cmpq	$2147483647, -16(%rbp)
	jle	L179
L178:
	movl	$4577, %edi
	call	__m3_fault
L179:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_u32_13
	.private_extern _div_pow2__div_pow2_u32_13
_div_pow2__div_pow2_u32_13:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$13, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L182
	cmpq	$2147483647, -16(%rbp)
	jle	L183
L182:
	movl	$4641, %edi
	call	__m3_fault
L183:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_u32_14
	.private_extern _div_pow2__div_pow2_u32_14
_div_pow2__div_pow2_u32_14:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$14, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L186
	cmpq	$2147483647, -16(%rbp)
	jle	L187
L186:
	movl	$4705, %edi
	call	__m3_fault
L187:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_u32_15
	.private_extern _div_pow2__div_pow2_u32_15
_div_pow2__div_pow2_u32_15:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$15, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L190
	cmpq	$2147483647, -16(%rbp)
	jle	L191
L190:
	movl	$4769, %edi
	call	__m3_fault
L191:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_u32_16
	.private_extern _div_pow2__div_pow2_u32_16
_div_pow2__div_pow2_u32_16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$16, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L194
	cmpq	$2147483647, -16(%rbp)
	jle	L195
L194:
	movl	$4833, %edi
	call	__m3_fault
L195:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_u32_17
	.private_extern _div_pow2__div_pow2_u32_17
_div_pow2__div_pow2_u32_17:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$17, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L198
	cmpq	$2147483647, -16(%rbp)
	jle	L199
L198:
	movl	$4897, %edi
	call	__m3_fault
L199:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_u32_18
	.private_extern _div_pow2__div_pow2_u32_18
_div_pow2__div_pow2_u32_18:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$18, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L202
	cmpq	$2147483647, -16(%rbp)
	jle	L203
L202:
	movl	$4961, %edi
	call	__m3_fault
L203:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_u32_19
	.private_extern _div_pow2__div_pow2_u32_19
_div_pow2__div_pow2_u32_19:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$19, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L206
	cmpq	$2147483647, -16(%rbp)
	jle	L207
L206:
	movl	$5025, %edi
	call	__m3_fault
L207:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_u32_20
	.private_extern _div_pow2__div_pow2_u32_20
_div_pow2__div_pow2_u32_20:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$20, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L210
	cmpq	$2147483647, -16(%rbp)
	jle	L211
L210:
	movl	$5089, %edi
	call	__m3_fault
L211:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_u32_21
	.private_extern _div_pow2__div_pow2_u32_21
_div_pow2__div_pow2_u32_21:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$21, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L214
	cmpq	$2147483647, -16(%rbp)
	jle	L215
L214:
	movl	$5153, %edi
	call	__m3_fault
L215:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_u32_22
	.private_extern _div_pow2__div_pow2_u32_22
_div_pow2__div_pow2_u32_22:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$22, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L218
	cmpq	$2147483647, -16(%rbp)
	jle	L219
L218:
	movl	$5217, %edi
	call	__m3_fault
L219:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_u32_23
	.private_extern _div_pow2__div_pow2_u32_23
_div_pow2__div_pow2_u32_23:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$23, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L222
	cmpq	$2147483647, -16(%rbp)
	jle	L223
L222:
	movl	$5281, %edi
	call	__m3_fault
L223:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_u32_24
	.private_extern _div_pow2__div_pow2_u32_24
_div_pow2__div_pow2_u32_24:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$24, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L226
	cmpq	$2147483647, -16(%rbp)
	jle	L227
L226:
	movl	$5345, %edi
	call	__m3_fault
L227:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_u32_25
	.private_extern _div_pow2__div_pow2_u32_25
_div_pow2__div_pow2_u32_25:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$25, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L230
	cmpq	$2147483647, -16(%rbp)
	jle	L231
L230:
	movl	$5409, %edi
	call	__m3_fault
L231:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_u32_26
	.private_extern _div_pow2__div_pow2_u32_26
_div_pow2__div_pow2_u32_26:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$26, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L234
	cmpq	$2147483647, -16(%rbp)
	jle	L235
L234:
	movl	$5473, %edi
	call	__m3_fault
L235:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_u32_27
	.private_extern _div_pow2__div_pow2_u32_27
_div_pow2__div_pow2_u32_27:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$27, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L238
	cmpq	$2147483647, -16(%rbp)
	jle	L239
L238:
	movl	$5537, %edi
	call	__m3_fault
L239:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_u32_28
	.private_extern _div_pow2__div_pow2_u32_28
_div_pow2__div_pow2_u32_28:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$28, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L242
	cmpq	$2147483647, -16(%rbp)
	jle	L243
L242:
	movl	$5601, %edi
	call	__m3_fault
L243:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_u32_29
	.private_extern _div_pow2__div_pow2_u32_29
_div_pow2__div_pow2_u32_29:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$29, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L246
	cmpq	$2147483647, -16(%rbp)
	jle	L247
L246:
	movl	$5665, %edi
	call	__m3_fault
L247:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_u32_30
	.private_extern _div_pow2__div_pow2_u32_30
_div_pow2__div_pow2_u32_30:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$30, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L250
	cmpq	$2147483647, -16(%rbp)
	jle	L251
L250:
	movl	$5729, %edi
	call	__m3_fault
L251:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_u32_31
	.private_extern _div_pow2__div_pow2_u32_31
_div_pow2__div_pow2_u32_31:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$31, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L254
	cmpq	$2147483647, -16(%rbp)
	jle	L255
L254:
	movl	$5793, %edi
	call	__m3_fault
L255:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_0
	.private_extern _div_pow2__div_pow2_i64_0
_div_pow2__div_pow2_i64_0:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_1
	.private_extern _div_pow2__div_pow2_i64_1
_div_pow2__div_pow2_i64_1:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	%rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_2
	.private_extern _div_pow2__div_pow2_i64_2
_div_pow2__div_pow2_i64_2:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$2, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_3
	.private_extern _div_pow2__div_pow2_i64_3
_div_pow2__div_pow2_i64_3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$3, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_4
	.private_extern _div_pow2__div_pow2_i64_4
_div_pow2__div_pow2_i64_4:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$4, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_5
	.private_extern _div_pow2__div_pow2_i64_5
_div_pow2__div_pow2_i64_5:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$5, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_6
	.private_extern _div_pow2__div_pow2_i64_6
_div_pow2__div_pow2_i64_6:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$6, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_7
	.private_extern _div_pow2__div_pow2_i64_7
_div_pow2__div_pow2_i64_7:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$7, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_8
	.private_extern _div_pow2__div_pow2_i64_8
_div_pow2__div_pow2_i64_8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$8, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_9
	.private_extern _div_pow2__div_pow2_i64_9
_div_pow2__div_pow2_i64_9:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$9, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_10
	.private_extern _div_pow2__div_pow2_i64_10
_div_pow2__div_pow2_i64_10:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$10, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_11
	.private_extern _div_pow2__div_pow2_i64_11
_div_pow2__div_pow2_i64_11:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$11, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_12
	.private_extern _div_pow2__div_pow2_i64_12
_div_pow2__div_pow2_i64_12:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$12, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_13
	.private_extern _div_pow2__div_pow2_i64_13
_div_pow2__div_pow2_i64_13:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$13, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_14
	.private_extern _div_pow2__div_pow2_i64_14
_div_pow2__div_pow2_i64_14:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$14, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_15
	.private_extern _div_pow2__div_pow2_i64_15
_div_pow2__div_pow2_i64_15:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$15, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_16
	.private_extern _div_pow2__div_pow2_i64_16
_div_pow2__div_pow2_i64_16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$16, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_17
	.private_extern _div_pow2__div_pow2_i64_17
_div_pow2__div_pow2_i64_17:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$17, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_18
	.private_extern _div_pow2__div_pow2_i64_18
_div_pow2__div_pow2_i64_18:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$18, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_19
	.private_extern _div_pow2__div_pow2_i64_19
_div_pow2__div_pow2_i64_19:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$19, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_20
	.private_extern _div_pow2__div_pow2_i64_20
_div_pow2__div_pow2_i64_20:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$20, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_21
	.private_extern _div_pow2__div_pow2_i64_21
_div_pow2__div_pow2_i64_21:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$21, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_22
	.private_extern _div_pow2__div_pow2_i64_22
_div_pow2__div_pow2_i64_22:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$22, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_23
	.private_extern _div_pow2__div_pow2_i64_23
_div_pow2__div_pow2_i64_23:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$23, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_24
	.private_extern _div_pow2__div_pow2_i64_24
_div_pow2__div_pow2_i64_24:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$24, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_25
	.private_extern _div_pow2__div_pow2_i64_25
_div_pow2__div_pow2_i64_25:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$25, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_26
	.private_extern _div_pow2__div_pow2_i64_26
_div_pow2__div_pow2_i64_26:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$26, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_27
	.private_extern _div_pow2__div_pow2_i64_27
_div_pow2__div_pow2_i64_27:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$27, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_28
	.private_extern _div_pow2__div_pow2_i64_28
_div_pow2__div_pow2_i64_28:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$28, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_29
	.private_extern _div_pow2__div_pow2_i64_29
_div_pow2__div_pow2_i64_29:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$29, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_30
	.private_extern _div_pow2__div_pow2_i64_30
_div_pow2__div_pow2_i64_30:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$30, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_31
	.private_extern _div_pow2__div_pow2_i64_31
_div_pow2__div_pow2_i64_31:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$31, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_32
	.private_extern _div_pow2__div_pow2_i64_32
_div_pow2__div_pow2_i64_32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$32, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_33
	.private_extern _div_pow2__div_pow2_i64_33
_div_pow2__div_pow2_i64_33:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$33, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_34
	.private_extern _div_pow2__div_pow2_i64_34
_div_pow2__div_pow2_i64_34:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$34, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_35
	.private_extern _div_pow2__div_pow2_i64_35
_div_pow2__div_pow2_i64_35:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$35, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_36
	.private_extern _div_pow2__div_pow2_i64_36
_div_pow2__div_pow2_i64_36:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$36, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_37
	.private_extern _div_pow2__div_pow2_i64_37
_div_pow2__div_pow2_i64_37:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$37, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_38
	.private_extern _div_pow2__div_pow2_i64_38
_div_pow2__div_pow2_i64_38:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$38, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_39
	.private_extern _div_pow2__div_pow2_i64_39
_div_pow2__div_pow2_i64_39:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$39, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_40
	.private_extern _div_pow2__div_pow2_i64_40
_div_pow2__div_pow2_i64_40:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$40, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_41
	.private_extern _div_pow2__div_pow2_i64_41
_div_pow2__div_pow2_i64_41:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$41, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_42
	.private_extern _div_pow2__div_pow2_i64_42
_div_pow2__div_pow2_i64_42:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$42, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_43
	.private_extern _div_pow2__div_pow2_i64_43
_div_pow2__div_pow2_i64_43:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$43, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_44
	.private_extern _div_pow2__div_pow2_i64_44
_div_pow2__div_pow2_i64_44:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$44, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_45
	.private_extern _div_pow2__div_pow2_i64_45
_div_pow2__div_pow2_i64_45:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$45, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_46
	.private_extern _div_pow2__div_pow2_i64_46
_div_pow2__div_pow2_i64_46:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$46, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_47
	.private_extern _div_pow2__div_pow2_i64_47
_div_pow2__div_pow2_i64_47:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$47, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_48
	.private_extern _div_pow2__div_pow2_i64_48
_div_pow2__div_pow2_i64_48:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$48, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_49
	.private_extern _div_pow2__div_pow2_i64_49
_div_pow2__div_pow2_i64_49:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$49, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_50
	.private_extern _div_pow2__div_pow2_i64_50
_div_pow2__div_pow2_i64_50:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$50, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_51
	.private_extern _div_pow2__div_pow2_i64_51
_div_pow2__div_pow2_i64_51:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$51, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_52
	.private_extern _div_pow2__div_pow2_i64_52
_div_pow2__div_pow2_i64_52:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$52, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_53
	.private_extern _div_pow2__div_pow2_i64_53
_div_pow2__div_pow2_i64_53:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$53, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_54
	.private_extern _div_pow2__div_pow2_i64_54
_div_pow2__div_pow2_i64_54:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$54, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_55
	.private_extern _div_pow2__div_pow2_i64_55
_div_pow2__div_pow2_i64_55:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$55, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_56
	.private_extern _div_pow2__div_pow2_i64_56
_div_pow2__div_pow2_i64_56:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$56, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_57
	.private_extern _div_pow2__div_pow2_i64_57
_div_pow2__div_pow2_i64_57:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$57, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_58
	.private_extern _div_pow2__div_pow2_i64_58
_div_pow2__div_pow2_i64_58:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$58, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_59
	.private_extern _div_pow2__div_pow2_i64_59
_div_pow2__div_pow2_i64_59:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$59, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_60
	.private_extern _div_pow2__div_pow2_i64_60
_div_pow2__div_pow2_i64_60:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$60, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_61
	.private_extern _div_pow2__div_pow2_i64_61
_div_pow2__div_pow2_i64_61:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$61, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_62
	.private_extern _div_pow2__div_pow2_i64_62
_div_pow2__div_pow2_i64_62:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$62, %rax
	leave
	ret
.globl _div_pow2__div_pow2_i64_63
	.private_extern _div_pow2__div_pow2_i64_63
_div_pow2__div_pow2_i64_63:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leaq	-1(%rax), %rdx
	orq	%rdx, %rax
	movq	%rax, %rcx
	sarq	$63, %rcx
	subq	%rcx, %rdx
	movabsq	$-9223372036854775808, %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %edx
	movq	%rcx, %rax
	notq	%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_0
	.private_extern _div_pow2__div_pow2_u64_0
_div_pow2__div_pow2_u64_0:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_1
	.private_extern _div_pow2__div_pow2_u64_1
_div_pow2__div_pow2_u64_1:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	%rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_2
	.private_extern _div_pow2__div_pow2_u64_2
_div_pow2__div_pow2_u64_2:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$2, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_3
	.private_extern _div_pow2__div_pow2_u64_3
_div_pow2__div_pow2_u64_3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$3, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_4
	.private_extern _div_pow2__div_pow2_u64_4
_div_pow2__div_pow2_u64_4:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$4, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_5
	.private_extern _div_pow2__div_pow2_u64_5
_div_pow2__div_pow2_u64_5:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$5, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_6
	.private_extern _div_pow2__div_pow2_u64_6
_div_pow2__div_pow2_u64_6:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$6, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_7
	.private_extern _div_pow2__div_pow2_u64_7
_div_pow2__div_pow2_u64_7:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$7, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_8
	.private_extern _div_pow2__div_pow2_u64_8
_div_pow2__div_pow2_u64_8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$8, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_9
	.private_extern _div_pow2__div_pow2_u64_9
_div_pow2__div_pow2_u64_9:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$9, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_10
	.private_extern _div_pow2__div_pow2_u64_10
_div_pow2__div_pow2_u64_10:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$10, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_11
	.private_extern _div_pow2__div_pow2_u64_11
_div_pow2__div_pow2_u64_11:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$11, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_12
	.private_extern _div_pow2__div_pow2_u64_12
_div_pow2__div_pow2_u64_12:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$12, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_13
	.private_extern _div_pow2__div_pow2_u64_13
_div_pow2__div_pow2_u64_13:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$13, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_14
	.private_extern _div_pow2__div_pow2_u64_14
_div_pow2__div_pow2_u64_14:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$14, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_15
	.private_extern _div_pow2__div_pow2_u64_15
_div_pow2__div_pow2_u64_15:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$15, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_16
	.private_extern _div_pow2__div_pow2_u64_16
_div_pow2__div_pow2_u64_16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$16, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_17
	.private_extern _div_pow2__div_pow2_u64_17
_div_pow2__div_pow2_u64_17:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$17, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_18
	.private_extern _div_pow2__div_pow2_u64_18
_div_pow2__div_pow2_u64_18:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$18, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_19
	.private_extern _div_pow2__div_pow2_u64_19
_div_pow2__div_pow2_u64_19:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$19, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_20
	.private_extern _div_pow2__div_pow2_u64_20
_div_pow2__div_pow2_u64_20:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$20, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_21
	.private_extern _div_pow2__div_pow2_u64_21
_div_pow2__div_pow2_u64_21:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$21, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_22
	.private_extern _div_pow2__div_pow2_u64_22
_div_pow2__div_pow2_u64_22:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$22, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_23
	.private_extern _div_pow2__div_pow2_u64_23
_div_pow2__div_pow2_u64_23:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$23, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_24
	.private_extern _div_pow2__div_pow2_u64_24
_div_pow2__div_pow2_u64_24:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$24, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_25
	.private_extern _div_pow2__div_pow2_u64_25
_div_pow2__div_pow2_u64_25:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$25, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_26
	.private_extern _div_pow2__div_pow2_u64_26
_div_pow2__div_pow2_u64_26:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$26, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_27
	.private_extern _div_pow2__div_pow2_u64_27
_div_pow2__div_pow2_u64_27:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$27, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_28
	.private_extern _div_pow2__div_pow2_u64_28
_div_pow2__div_pow2_u64_28:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$28, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_29
	.private_extern _div_pow2__div_pow2_u64_29
_div_pow2__div_pow2_u64_29:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$29, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_30
	.private_extern _div_pow2__div_pow2_u64_30
_div_pow2__div_pow2_u64_30:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$30, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_31
	.private_extern _div_pow2__div_pow2_u64_31
_div_pow2__div_pow2_u64_31:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$31, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_32
	.private_extern _div_pow2__div_pow2_u64_32
_div_pow2__div_pow2_u64_32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$32, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_33
	.private_extern _div_pow2__div_pow2_u64_33
_div_pow2__div_pow2_u64_33:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$33, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_34
	.private_extern _div_pow2__div_pow2_u64_34
_div_pow2__div_pow2_u64_34:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$34, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_35
	.private_extern _div_pow2__div_pow2_u64_35
_div_pow2__div_pow2_u64_35:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$35, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_36
	.private_extern _div_pow2__div_pow2_u64_36
_div_pow2__div_pow2_u64_36:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$36, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_37
	.private_extern _div_pow2__div_pow2_u64_37
_div_pow2__div_pow2_u64_37:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$37, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_38
	.private_extern _div_pow2__div_pow2_u64_38
_div_pow2__div_pow2_u64_38:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$38, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_39
	.private_extern _div_pow2__div_pow2_u64_39
_div_pow2__div_pow2_u64_39:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$39, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_40
	.private_extern _div_pow2__div_pow2_u64_40
_div_pow2__div_pow2_u64_40:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$40, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_41
	.private_extern _div_pow2__div_pow2_u64_41
_div_pow2__div_pow2_u64_41:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$41, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_42
	.private_extern _div_pow2__div_pow2_u64_42
_div_pow2__div_pow2_u64_42:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$42, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_43
	.private_extern _div_pow2__div_pow2_u64_43
_div_pow2__div_pow2_u64_43:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$43, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_44
	.private_extern _div_pow2__div_pow2_u64_44
_div_pow2__div_pow2_u64_44:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$44, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_45
	.private_extern _div_pow2__div_pow2_u64_45
_div_pow2__div_pow2_u64_45:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$45, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_46
	.private_extern _div_pow2__div_pow2_u64_46
_div_pow2__div_pow2_u64_46:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$46, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_47
	.private_extern _div_pow2__div_pow2_u64_47
_div_pow2__div_pow2_u64_47:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$47, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_48
	.private_extern _div_pow2__div_pow2_u64_48
_div_pow2__div_pow2_u64_48:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$48, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_49
	.private_extern _div_pow2__div_pow2_u64_49
_div_pow2__div_pow2_u64_49:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$49, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_50
	.private_extern _div_pow2__div_pow2_u64_50
_div_pow2__div_pow2_u64_50:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$50, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_51
	.private_extern _div_pow2__div_pow2_u64_51
_div_pow2__div_pow2_u64_51:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$51, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_52
	.private_extern _div_pow2__div_pow2_u64_52
_div_pow2__div_pow2_u64_52:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$52, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_53
	.private_extern _div_pow2__div_pow2_u64_53
_div_pow2__div_pow2_u64_53:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$53, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_54
	.private_extern _div_pow2__div_pow2_u64_54
_div_pow2__div_pow2_u64_54:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$54, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_55
	.private_extern _div_pow2__div_pow2_u64_55
_div_pow2__div_pow2_u64_55:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$55, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_56
	.private_extern _div_pow2__div_pow2_u64_56
_div_pow2__div_pow2_u64_56:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$56, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_57
	.private_extern _div_pow2__div_pow2_u64_57
_div_pow2__div_pow2_u64_57:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$57, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_58
	.private_extern _div_pow2__div_pow2_u64_58
_div_pow2__div_pow2_u64_58:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$58, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_59
	.private_extern _div_pow2__div_pow2_u64_59
_div_pow2__div_pow2_u64_59:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$59, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_60
	.private_extern _div_pow2__div_pow2_u64_60
_div_pow2__div_pow2_u64_60:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$60, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_61
	.private_extern _div_pow2__div_pow2_u64_61
_div_pow2__div_pow2_u64_61:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$61, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_62
	.private_extern _div_pow2__div_pow2_u64_62
_div_pow2__div_pow2_u64_62:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$62, %rax
	leave
	ret
.globl _div_pow2__div_pow2_u64_63
	.private_extern _div_pow2__div_pow2_u64_63
_div_pow2__div_pow2_u64_63:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leaq	-1(%rax), %rdx
	orq	%rdx, %rax
	movq	%rax, %rcx
	sarq	$63, %rcx
	subq	%rcx, %rdx
	movabsq	$-9223372036854775808, %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %edx
	movq	%rcx, %rax
	notq	%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_0
	.private_extern _div_pow2__div_pow2_I_0
_div_pow2__div_pow2_I_0:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_1
	.private_extern _div_pow2__div_pow2_I_1
_div_pow2__div_pow2_I_1:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	%rax
	leave
	ret
.globl _div_pow2__div_pow2_I_2
	.private_extern _div_pow2__div_pow2_I_2
_div_pow2__div_pow2_I_2:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$2, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_3
	.private_extern _div_pow2__div_pow2_I_3
_div_pow2__div_pow2_I_3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$3, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_4
	.private_extern _div_pow2__div_pow2_I_4
_div_pow2__div_pow2_I_4:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$4, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_5
	.private_extern _div_pow2__div_pow2_I_5
_div_pow2__div_pow2_I_5:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$5, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_6
	.private_extern _div_pow2__div_pow2_I_6
_div_pow2__div_pow2_I_6:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$6, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_7
	.private_extern _div_pow2__div_pow2_I_7
_div_pow2__div_pow2_I_7:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$7, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_8
	.private_extern _div_pow2__div_pow2_I_8
_div_pow2__div_pow2_I_8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$8, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_9
	.private_extern _div_pow2__div_pow2_I_9
_div_pow2__div_pow2_I_9:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$9, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_10
	.private_extern _div_pow2__div_pow2_I_10
_div_pow2__div_pow2_I_10:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$10, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_11
	.private_extern _div_pow2__div_pow2_I_11
_div_pow2__div_pow2_I_11:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$11, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_12
	.private_extern _div_pow2__div_pow2_I_12
_div_pow2__div_pow2_I_12:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$12, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_13
	.private_extern _div_pow2__div_pow2_I_13
_div_pow2__div_pow2_I_13:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$13, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_14
	.private_extern _div_pow2__div_pow2_I_14
_div_pow2__div_pow2_I_14:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$14, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_15
	.private_extern _div_pow2__div_pow2_I_15
_div_pow2__div_pow2_I_15:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$15, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_16
	.private_extern _div_pow2__div_pow2_I_16
_div_pow2__div_pow2_I_16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$16, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_17
	.private_extern _div_pow2__div_pow2_I_17
_div_pow2__div_pow2_I_17:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$17, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_18
	.private_extern _div_pow2__div_pow2_I_18
_div_pow2__div_pow2_I_18:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$18, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_19
	.private_extern _div_pow2__div_pow2_I_19
_div_pow2__div_pow2_I_19:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$19, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_20
	.private_extern _div_pow2__div_pow2_I_20
_div_pow2__div_pow2_I_20:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$20, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_21
	.private_extern _div_pow2__div_pow2_I_21
_div_pow2__div_pow2_I_21:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$21, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_22
	.private_extern _div_pow2__div_pow2_I_22
_div_pow2__div_pow2_I_22:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$22, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_23
	.private_extern _div_pow2__div_pow2_I_23
_div_pow2__div_pow2_I_23:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$23, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_24
	.private_extern _div_pow2__div_pow2_I_24
_div_pow2__div_pow2_I_24:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$24, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_25
	.private_extern _div_pow2__div_pow2_I_25
_div_pow2__div_pow2_I_25:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$25, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_26
	.private_extern _div_pow2__div_pow2_I_26
_div_pow2__div_pow2_I_26:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$26, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_27
	.private_extern _div_pow2__div_pow2_I_27
_div_pow2__div_pow2_I_27:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$27, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_28
	.private_extern _div_pow2__div_pow2_I_28
_div_pow2__div_pow2_I_28:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$28, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_29
	.private_extern _div_pow2__div_pow2_I_29
_div_pow2__div_pow2_I_29:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$29, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_30
	.private_extern _div_pow2__div_pow2_I_30
_div_pow2__div_pow2_I_30:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$30, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_31
	.private_extern _div_pow2__div_pow2_I_31
_div_pow2__div_pow2_I_31:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$31, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_32
	.private_extern _div_pow2__div_pow2_I_32
_div_pow2__div_pow2_I_32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$32, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_33
	.private_extern _div_pow2__div_pow2_I_33
_div_pow2__div_pow2_I_33:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$33, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_34
	.private_extern _div_pow2__div_pow2_I_34
_div_pow2__div_pow2_I_34:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$34, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_35
	.private_extern _div_pow2__div_pow2_I_35
_div_pow2__div_pow2_I_35:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$35, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_36
	.private_extern _div_pow2__div_pow2_I_36
_div_pow2__div_pow2_I_36:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$36, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_37
	.private_extern _div_pow2__div_pow2_I_37
_div_pow2__div_pow2_I_37:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$37, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_38
	.private_extern _div_pow2__div_pow2_I_38
_div_pow2__div_pow2_I_38:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$38, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_39
	.private_extern _div_pow2__div_pow2_I_39
_div_pow2__div_pow2_I_39:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$39, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_40
	.private_extern _div_pow2__div_pow2_I_40
_div_pow2__div_pow2_I_40:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$40, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_41
	.private_extern _div_pow2__div_pow2_I_41
_div_pow2__div_pow2_I_41:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$41, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_42
	.private_extern _div_pow2__div_pow2_I_42
_div_pow2__div_pow2_I_42:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$42, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_43
	.private_extern _div_pow2__div_pow2_I_43
_div_pow2__div_pow2_I_43:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$43, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_44
	.private_extern _div_pow2__div_pow2_I_44
_div_pow2__div_pow2_I_44:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$44, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_45
	.private_extern _div_pow2__div_pow2_I_45
_div_pow2__div_pow2_I_45:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$45, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_46
	.private_extern _div_pow2__div_pow2_I_46
_div_pow2__div_pow2_I_46:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$46, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_47
	.private_extern _div_pow2__div_pow2_I_47
_div_pow2__div_pow2_I_47:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$47, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_48
	.private_extern _div_pow2__div_pow2_I_48
_div_pow2__div_pow2_I_48:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$48, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_49
	.private_extern _div_pow2__div_pow2_I_49
_div_pow2__div_pow2_I_49:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$49, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_50
	.private_extern _div_pow2__div_pow2_I_50
_div_pow2__div_pow2_I_50:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$50, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_51
	.private_extern _div_pow2__div_pow2_I_51
_div_pow2__div_pow2_I_51:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$51, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_52
	.private_extern _div_pow2__div_pow2_I_52
_div_pow2__div_pow2_I_52:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$52, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_53
	.private_extern _div_pow2__div_pow2_I_53
_div_pow2__div_pow2_I_53:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$53, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_54
	.private_extern _div_pow2__div_pow2_I_54
_div_pow2__div_pow2_I_54:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$54, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_55
	.private_extern _div_pow2__div_pow2_I_55
_div_pow2__div_pow2_I_55:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$55, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_56
	.private_extern _div_pow2__div_pow2_I_56
_div_pow2__div_pow2_I_56:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$56, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_57
	.private_extern _div_pow2__div_pow2_I_57
_div_pow2__div_pow2_I_57:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$57, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_58
	.private_extern _div_pow2__div_pow2_I_58
_div_pow2__div_pow2_I_58:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$58, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_59
	.private_extern _div_pow2__div_pow2_I_59
_div_pow2__div_pow2_I_59:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$59, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_60
	.private_extern _div_pow2__div_pow2_I_60
_div_pow2__div_pow2_I_60:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$60, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_61
	.private_extern _div_pow2__div_pow2_I_61
_div_pow2__div_pow2_I_61:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$61, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_62
	.private_extern _div_pow2__div_pow2_I_62
_div_pow2__div_pow2_I_62:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$62, %rax
	leave
	ret
.globl _div_pow2__div_pow2_I_63
	.private_extern _div_pow2__div_pow2_I_63
_div_pow2__div_pow2_I_63:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leaq	-1(%rax), %rdx
	orq	%rdx, %rax
	movq	%rax, %rcx
	sarq	$63, %rcx
	subq	%rcx, %rdx
	movabsq	$-9223372036854775808, %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %edx
	movq	%rcx, %rax
	notq	%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_0
	.private_extern _div_pow2__div_pow2_C_0
_div_pow2__div_pow2_C_0:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L642
	movl	$18145, %edi
	call	__m3_fault
L642:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_1
	.private_extern _div_pow2__div_pow2_C_1
_div_pow2__div_pow2_C_1:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	%rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L645
	movl	$18209, %edi
	call	__m3_fault
L645:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_2
	.private_extern _div_pow2__div_pow2_C_2
_div_pow2__div_pow2_C_2:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$2, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L648
	movl	$18273, %edi
	call	__m3_fault
L648:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_3
	.private_extern _div_pow2__div_pow2_C_3
_div_pow2__div_pow2_C_3:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$3, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L651
	movl	$18337, %edi
	call	__m3_fault
L651:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_4
	.private_extern _div_pow2__div_pow2_C_4
_div_pow2__div_pow2_C_4:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$4, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L654
	movl	$18401, %edi
	call	__m3_fault
L654:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_5
	.private_extern _div_pow2__div_pow2_C_5
_div_pow2__div_pow2_C_5:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$5, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L657
	movl	$18465, %edi
	call	__m3_fault
L657:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_6
	.private_extern _div_pow2__div_pow2_C_6
_div_pow2__div_pow2_C_6:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$6, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L660
	movl	$18529, %edi
	call	__m3_fault
L660:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_7
	.private_extern _div_pow2__div_pow2_C_7
_div_pow2__div_pow2_C_7:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$7, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L663
	movl	$18593, %edi
	call	__m3_fault
L663:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_8
	.private_extern _div_pow2__div_pow2_C_8
_div_pow2__div_pow2_C_8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$8, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L666
	movl	$18657, %edi
	call	__m3_fault
L666:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_9
	.private_extern _div_pow2__div_pow2_C_9
_div_pow2__div_pow2_C_9:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$9, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L669
	movl	$18721, %edi
	call	__m3_fault
L669:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_10
	.private_extern _div_pow2__div_pow2_C_10
_div_pow2__div_pow2_C_10:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$10, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L672
	movl	$18785, %edi
	call	__m3_fault
L672:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_11
	.private_extern _div_pow2__div_pow2_C_11
_div_pow2__div_pow2_C_11:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$11, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L675
	movl	$18849, %edi
	call	__m3_fault
L675:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_12
	.private_extern _div_pow2__div_pow2_C_12
_div_pow2__div_pow2_C_12:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$12, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L678
	movl	$18913, %edi
	call	__m3_fault
L678:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_13
	.private_extern _div_pow2__div_pow2_C_13
_div_pow2__div_pow2_C_13:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$13, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L681
	movl	$18977, %edi
	call	__m3_fault
L681:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_14
	.private_extern _div_pow2__div_pow2_C_14
_div_pow2__div_pow2_C_14:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$14, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L684
	movl	$19041, %edi
	call	__m3_fault
L684:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_15
	.private_extern _div_pow2__div_pow2_C_15
_div_pow2__div_pow2_C_15:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$15, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L687
	movl	$19105, %edi
	call	__m3_fault
L687:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_16
	.private_extern _div_pow2__div_pow2_C_16
_div_pow2__div_pow2_C_16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$16, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L690
	movl	$19169, %edi
	call	__m3_fault
L690:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_17
	.private_extern _div_pow2__div_pow2_C_17
_div_pow2__div_pow2_C_17:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$17, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L693
	movl	$19233, %edi
	call	__m3_fault
L693:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_18
	.private_extern _div_pow2__div_pow2_C_18
_div_pow2__div_pow2_C_18:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$18, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L696
	movl	$19297, %edi
	call	__m3_fault
L696:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_19
	.private_extern _div_pow2__div_pow2_C_19
_div_pow2__div_pow2_C_19:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$19, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L699
	movl	$19361, %edi
	call	__m3_fault
L699:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_20
	.private_extern _div_pow2__div_pow2_C_20
_div_pow2__div_pow2_C_20:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$20, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L702
	movl	$19425, %edi
	call	__m3_fault
L702:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_21
	.private_extern _div_pow2__div_pow2_C_21
_div_pow2__div_pow2_C_21:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$21, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L705
	movl	$19489, %edi
	call	__m3_fault
L705:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_22
	.private_extern _div_pow2__div_pow2_C_22
_div_pow2__div_pow2_C_22:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$22, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L708
	movl	$19553, %edi
	call	__m3_fault
L708:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_23
	.private_extern _div_pow2__div_pow2_C_23
_div_pow2__div_pow2_C_23:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$23, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L711
	movl	$19617, %edi
	call	__m3_fault
L711:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_24
	.private_extern _div_pow2__div_pow2_C_24
_div_pow2__div_pow2_C_24:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$24, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L714
	movl	$19681, %edi
	call	__m3_fault
L714:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_25
	.private_extern _div_pow2__div_pow2_C_25
_div_pow2__div_pow2_C_25:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$25, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L717
	movl	$19745, %edi
	call	__m3_fault
L717:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_26
	.private_extern _div_pow2__div_pow2_C_26
_div_pow2__div_pow2_C_26:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$26, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L720
	movl	$19809, %edi
	call	__m3_fault
L720:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_27
	.private_extern _div_pow2__div_pow2_C_27
_div_pow2__div_pow2_C_27:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$27, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L723
	movl	$19873, %edi
	call	__m3_fault
L723:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_28
	.private_extern _div_pow2__div_pow2_C_28
_div_pow2__div_pow2_C_28:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$28, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L726
	movl	$19937, %edi
	call	__m3_fault
L726:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_29
	.private_extern _div_pow2__div_pow2_C_29
_div_pow2__div_pow2_C_29:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$29, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L729
	movl	$20001, %edi
	call	__m3_fault
L729:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_30
	.private_extern _div_pow2__div_pow2_C_30
_div_pow2__div_pow2_C_30:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$30, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L732
	movl	$20065, %edi
	call	__m3_fault
L732:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_31
	.private_extern _div_pow2__div_pow2_C_31
_div_pow2__div_pow2_C_31:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$31, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L735
	movl	$20129, %edi
	call	__m3_fault
L735:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_32
	.private_extern _div_pow2__div_pow2_C_32
_div_pow2__div_pow2_C_32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$32, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L738
	movl	$20193, %edi
	call	__m3_fault
L738:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_33
	.private_extern _div_pow2__div_pow2_C_33
_div_pow2__div_pow2_C_33:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$33, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L741
	movl	$20257, %edi
	call	__m3_fault
L741:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_34
	.private_extern _div_pow2__div_pow2_C_34
_div_pow2__div_pow2_C_34:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$34, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L744
	movl	$20321, %edi
	call	__m3_fault
L744:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_35
	.private_extern _div_pow2__div_pow2_C_35
_div_pow2__div_pow2_C_35:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$35, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L747
	movl	$20385, %edi
	call	__m3_fault
L747:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_36
	.private_extern _div_pow2__div_pow2_C_36
_div_pow2__div_pow2_C_36:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$36, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L750
	movl	$20449, %edi
	call	__m3_fault
L750:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_37
	.private_extern _div_pow2__div_pow2_C_37
_div_pow2__div_pow2_C_37:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$37, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L753
	movl	$20513, %edi
	call	__m3_fault
L753:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_38
	.private_extern _div_pow2__div_pow2_C_38
_div_pow2__div_pow2_C_38:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$38, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L756
	movl	$20577, %edi
	call	__m3_fault
L756:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_39
	.private_extern _div_pow2__div_pow2_C_39
_div_pow2__div_pow2_C_39:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$39, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L759
	movl	$20641, %edi
	call	__m3_fault
L759:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_40
	.private_extern _div_pow2__div_pow2_C_40
_div_pow2__div_pow2_C_40:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$40, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L762
	movl	$20705, %edi
	call	__m3_fault
L762:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_41
	.private_extern _div_pow2__div_pow2_C_41
_div_pow2__div_pow2_C_41:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$41, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L765
	movl	$20769, %edi
	call	__m3_fault
L765:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_42
	.private_extern _div_pow2__div_pow2_C_42
_div_pow2__div_pow2_C_42:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$42, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L768
	movl	$20833, %edi
	call	__m3_fault
L768:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_43
	.private_extern _div_pow2__div_pow2_C_43
_div_pow2__div_pow2_C_43:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$43, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L771
	movl	$20897, %edi
	call	__m3_fault
L771:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_44
	.private_extern _div_pow2__div_pow2_C_44
_div_pow2__div_pow2_C_44:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$44, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L774
	movl	$20961, %edi
	call	__m3_fault
L774:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_45
	.private_extern _div_pow2__div_pow2_C_45
_div_pow2__div_pow2_C_45:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$45, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L777
	movl	$21025, %edi
	call	__m3_fault
L777:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_46
	.private_extern _div_pow2__div_pow2_C_46
_div_pow2__div_pow2_C_46:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$46, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L780
	movl	$21089, %edi
	call	__m3_fault
L780:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_47
	.private_extern _div_pow2__div_pow2_C_47
_div_pow2__div_pow2_C_47:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$47, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L783
	movl	$21153, %edi
	call	__m3_fault
L783:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_48
	.private_extern _div_pow2__div_pow2_C_48
_div_pow2__div_pow2_C_48:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$48, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L786
	movl	$21217, %edi
	call	__m3_fault
L786:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_49
	.private_extern _div_pow2__div_pow2_C_49
_div_pow2__div_pow2_C_49:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$49, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L789
	movl	$21281, %edi
	call	__m3_fault
L789:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_50
	.private_extern _div_pow2__div_pow2_C_50
_div_pow2__div_pow2_C_50:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$50, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L792
	movl	$21345, %edi
	call	__m3_fault
L792:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_51
	.private_extern _div_pow2__div_pow2_C_51
_div_pow2__div_pow2_C_51:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$51, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L795
	movl	$21409, %edi
	call	__m3_fault
L795:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_52
	.private_extern _div_pow2__div_pow2_C_52
_div_pow2__div_pow2_C_52:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$52, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L798
	movl	$21473, %edi
	call	__m3_fault
L798:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_53
	.private_extern _div_pow2__div_pow2_C_53
_div_pow2__div_pow2_C_53:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$53, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L801
	movl	$21537, %edi
	call	__m3_fault
L801:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_54
	.private_extern _div_pow2__div_pow2_C_54
_div_pow2__div_pow2_C_54:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$54, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L804
	movl	$21601, %edi
	call	__m3_fault
L804:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_55
	.private_extern _div_pow2__div_pow2_C_55
_div_pow2__div_pow2_C_55:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$55, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L807
	movl	$21665, %edi
	call	__m3_fault
L807:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_56
	.private_extern _div_pow2__div_pow2_C_56
_div_pow2__div_pow2_C_56:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$56, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L810
	movl	$21729, %edi
	call	__m3_fault
L810:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_57
	.private_extern _div_pow2__div_pow2_C_57
_div_pow2__div_pow2_C_57:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$57, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L813
	movl	$21793, %edi
	call	__m3_fault
L813:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_58
	.private_extern _div_pow2__div_pow2_C_58
_div_pow2__div_pow2_C_58:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$58, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L816
	movl	$21857, %edi
	call	__m3_fault
L816:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_59
	.private_extern _div_pow2__div_pow2_C_59
_div_pow2__div_pow2_C_59:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$59, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L819
	movl	$21921, %edi
	call	__m3_fault
L819:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_60
	.private_extern _div_pow2__div_pow2_C_60
_div_pow2__div_pow2_C_60:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$60, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L822
	movl	$21985, %edi
	call	__m3_fault
L822:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_61
	.private_extern _div_pow2__div_pow2_C_61
_div_pow2__div_pow2_C_61:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$61, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L825
	movl	$22049, %edi
	call	__m3_fault
L825:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_62
	.private_extern _div_pow2__div_pow2_C_62
_div_pow2__div_pow2_C_62:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$62, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L828
	movl	$22113, %edi
	call	__m3_fault
L828:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_C_63
	.private_extern _div_pow2__div_pow2_C_63
_div_pow2__div_pow2_C_63:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leaq	-1(%rax), %rdx
	orq	%rdx, %rax
	movq	%rax, %rcx
	sarq	$63, %rcx
	subq	%rcx, %rdx
	movabsq	$-9223372036854775808, %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %edx
	movq	%rcx, %rax
	notq	%rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L831
	movl	$22177, %edi
	call	__m3_fault
L831:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_0
	.private_extern _div_pow2__div_pow2_L_0
_div_pow2__div_pow2_L_0:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_1
	.private_extern _div_pow2__div_pow2_L_1
_div_pow2__div_pow2_L_1:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	%rax
	leave
	ret
.globl _div_pow2__div_pow2_L_2
	.private_extern _div_pow2__div_pow2_L_2
_div_pow2__div_pow2_L_2:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$2, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_3
	.private_extern _div_pow2__div_pow2_L_3
_div_pow2__div_pow2_L_3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$3, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_4
	.private_extern _div_pow2__div_pow2_L_4
_div_pow2__div_pow2_L_4:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$4, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_5
	.private_extern _div_pow2__div_pow2_L_5
_div_pow2__div_pow2_L_5:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$5, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_6
	.private_extern _div_pow2__div_pow2_L_6
_div_pow2__div_pow2_L_6:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$6, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_7
	.private_extern _div_pow2__div_pow2_L_7
_div_pow2__div_pow2_L_7:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$7, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_8
	.private_extern _div_pow2__div_pow2_L_8
_div_pow2__div_pow2_L_8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$8, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_9
	.private_extern _div_pow2__div_pow2_L_9
_div_pow2__div_pow2_L_9:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$9, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_10
	.private_extern _div_pow2__div_pow2_L_10
_div_pow2__div_pow2_L_10:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$10, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_11
	.private_extern _div_pow2__div_pow2_L_11
_div_pow2__div_pow2_L_11:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$11, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_12
	.private_extern _div_pow2__div_pow2_L_12
_div_pow2__div_pow2_L_12:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$12, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_13
	.private_extern _div_pow2__div_pow2_L_13
_div_pow2__div_pow2_L_13:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$13, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_14
	.private_extern _div_pow2__div_pow2_L_14
_div_pow2__div_pow2_L_14:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$14, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_15
	.private_extern _div_pow2__div_pow2_L_15
_div_pow2__div_pow2_L_15:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$15, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_16
	.private_extern _div_pow2__div_pow2_L_16
_div_pow2__div_pow2_L_16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$16, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_17
	.private_extern _div_pow2__div_pow2_L_17
_div_pow2__div_pow2_L_17:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$17, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_18
	.private_extern _div_pow2__div_pow2_L_18
_div_pow2__div_pow2_L_18:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$18, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_19
	.private_extern _div_pow2__div_pow2_L_19
_div_pow2__div_pow2_L_19:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$19, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_20
	.private_extern _div_pow2__div_pow2_L_20
_div_pow2__div_pow2_L_20:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$20, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_21
	.private_extern _div_pow2__div_pow2_L_21
_div_pow2__div_pow2_L_21:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$21, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_22
	.private_extern _div_pow2__div_pow2_L_22
_div_pow2__div_pow2_L_22:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$22, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_23
	.private_extern _div_pow2__div_pow2_L_23
_div_pow2__div_pow2_L_23:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$23, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_24
	.private_extern _div_pow2__div_pow2_L_24
_div_pow2__div_pow2_L_24:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$24, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_25
	.private_extern _div_pow2__div_pow2_L_25
_div_pow2__div_pow2_L_25:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$25, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_26
	.private_extern _div_pow2__div_pow2_L_26
_div_pow2__div_pow2_L_26:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$26, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_27
	.private_extern _div_pow2__div_pow2_L_27
_div_pow2__div_pow2_L_27:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$27, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_28
	.private_extern _div_pow2__div_pow2_L_28
_div_pow2__div_pow2_L_28:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$28, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_29
	.private_extern _div_pow2__div_pow2_L_29
_div_pow2__div_pow2_L_29:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$29, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_30
	.private_extern _div_pow2__div_pow2_L_30
_div_pow2__div_pow2_L_30:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$30, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_31
	.private_extern _div_pow2__div_pow2_L_31
_div_pow2__div_pow2_L_31:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$31, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_32
	.private_extern _div_pow2__div_pow2_L_32
_div_pow2__div_pow2_L_32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$32, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_33
	.private_extern _div_pow2__div_pow2_L_33
_div_pow2__div_pow2_L_33:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$33, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_34
	.private_extern _div_pow2__div_pow2_L_34
_div_pow2__div_pow2_L_34:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$34, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_35
	.private_extern _div_pow2__div_pow2_L_35
_div_pow2__div_pow2_L_35:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$35, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_36
	.private_extern _div_pow2__div_pow2_L_36
_div_pow2__div_pow2_L_36:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$36, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_37
	.private_extern _div_pow2__div_pow2_L_37
_div_pow2__div_pow2_L_37:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$37, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_38
	.private_extern _div_pow2__div_pow2_L_38
_div_pow2__div_pow2_L_38:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$38, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_39
	.private_extern _div_pow2__div_pow2_L_39
_div_pow2__div_pow2_L_39:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$39, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_40
	.private_extern _div_pow2__div_pow2_L_40
_div_pow2__div_pow2_L_40:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$40, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_41
	.private_extern _div_pow2__div_pow2_L_41
_div_pow2__div_pow2_L_41:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$41, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_42
	.private_extern _div_pow2__div_pow2_L_42
_div_pow2__div_pow2_L_42:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$42, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_43
	.private_extern _div_pow2__div_pow2_L_43
_div_pow2__div_pow2_L_43:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$43, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_44
	.private_extern _div_pow2__div_pow2_L_44
_div_pow2__div_pow2_L_44:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$44, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_45
	.private_extern _div_pow2__div_pow2_L_45
_div_pow2__div_pow2_L_45:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$45, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_46
	.private_extern _div_pow2__div_pow2_L_46
_div_pow2__div_pow2_L_46:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$46, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_47
	.private_extern _div_pow2__div_pow2_L_47
_div_pow2__div_pow2_L_47:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$47, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_48
	.private_extern _div_pow2__div_pow2_L_48
_div_pow2__div_pow2_L_48:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$48, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_49
	.private_extern _div_pow2__div_pow2_L_49
_div_pow2__div_pow2_L_49:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$49, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_50
	.private_extern _div_pow2__div_pow2_L_50
_div_pow2__div_pow2_L_50:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$50, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_51
	.private_extern _div_pow2__div_pow2_L_51
_div_pow2__div_pow2_L_51:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$51, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_52
	.private_extern _div_pow2__div_pow2_L_52
_div_pow2__div_pow2_L_52:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$52, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_53
	.private_extern _div_pow2__div_pow2_L_53
_div_pow2__div_pow2_L_53:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$53, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_54
	.private_extern _div_pow2__div_pow2_L_54
_div_pow2__div_pow2_L_54:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$54, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_55
	.private_extern _div_pow2__div_pow2_L_55
_div_pow2__div_pow2_L_55:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$55, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_56
	.private_extern _div_pow2__div_pow2_L_56
_div_pow2__div_pow2_L_56:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$56, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_57
	.private_extern _div_pow2__div_pow2_L_57
_div_pow2__div_pow2_L_57:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$57, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_58
	.private_extern _div_pow2__div_pow2_L_58
_div_pow2__div_pow2_L_58:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$58, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_59
	.private_extern _div_pow2__div_pow2_L_59
_div_pow2__div_pow2_L_59:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$59, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_60
	.private_extern _div_pow2__div_pow2_L_60
_div_pow2__div_pow2_L_60:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$60, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_61
	.private_extern _div_pow2__div_pow2_L_61
_div_pow2__div_pow2_L_61:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$61, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_62
	.private_extern _div_pow2__div_pow2_L_62
_div_pow2__div_pow2_L_62:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$62, %rax
	leave
	ret
.globl _div_pow2__div_pow2_L_63
	.private_extern _div_pow2__div_pow2_L_63
_div_pow2__div_pow2_L_63:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leaq	-1(%rax), %rdx
	orq	%rdx, %rax
	movq	%rax, %rcx
	sarq	$63, %rcx
	subq	%rcx, %rdx
	movabsq	$-9223372036854775808, %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %edx
	movq	%rcx, %rax
	notq	%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_0
	.private_extern _div_pow2__div_pow2_LC_0
_div_pow2__div_pow2_LC_0:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L962
	movl	$26337, %edi
	call	__m3_fault
L962:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_1
	.private_extern _div_pow2__div_pow2_LC_1
_div_pow2__div_pow2_LC_1:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	%rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L965
	movl	$26401, %edi
	call	__m3_fault
L965:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_2
	.private_extern _div_pow2__div_pow2_LC_2
_div_pow2__div_pow2_LC_2:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$2, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L968
	movl	$26465, %edi
	call	__m3_fault
L968:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_3
	.private_extern _div_pow2__div_pow2_LC_3
_div_pow2__div_pow2_LC_3:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$3, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L971
	movl	$26529, %edi
	call	__m3_fault
L971:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_4
	.private_extern _div_pow2__div_pow2_LC_4
_div_pow2__div_pow2_LC_4:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$4, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L974
	movl	$26593, %edi
	call	__m3_fault
L974:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_5
	.private_extern _div_pow2__div_pow2_LC_5
_div_pow2__div_pow2_LC_5:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$5, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L977
	movl	$26657, %edi
	call	__m3_fault
L977:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_6
	.private_extern _div_pow2__div_pow2_LC_6
_div_pow2__div_pow2_LC_6:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$6, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L980
	movl	$26721, %edi
	call	__m3_fault
L980:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_7
	.private_extern _div_pow2__div_pow2_LC_7
_div_pow2__div_pow2_LC_7:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$7, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L983
	movl	$26785, %edi
	call	__m3_fault
L983:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_8
	.private_extern _div_pow2__div_pow2_LC_8
_div_pow2__div_pow2_LC_8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$8, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L986
	movl	$26849, %edi
	call	__m3_fault
L986:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_9
	.private_extern _div_pow2__div_pow2_LC_9
_div_pow2__div_pow2_LC_9:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$9, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L989
	movl	$26913, %edi
	call	__m3_fault
L989:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_10
	.private_extern _div_pow2__div_pow2_LC_10
_div_pow2__div_pow2_LC_10:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$10, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L992
	movl	$26977, %edi
	call	__m3_fault
L992:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_11
	.private_extern _div_pow2__div_pow2_LC_11
_div_pow2__div_pow2_LC_11:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$11, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L995
	movl	$27041, %edi
	call	__m3_fault
L995:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_12
	.private_extern _div_pow2__div_pow2_LC_12
_div_pow2__div_pow2_LC_12:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$12, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L998
	movl	$27105, %edi
	call	__m3_fault
L998:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_13
	.private_extern _div_pow2__div_pow2_LC_13
_div_pow2__div_pow2_LC_13:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$13, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1001
	movl	$27169, %edi
	call	__m3_fault
L1001:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_14
	.private_extern _div_pow2__div_pow2_LC_14
_div_pow2__div_pow2_LC_14:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$14, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1004
	movl	$27233, %edi
	call	__m3_fault
L1004:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_15
	.private_extern _div_pow2__div_pow2_LC_15
_div_pow2__div_pow2_LC_15:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$15, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1007
	movl	$27297, %edi
	call	__m3_fault
L1007:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_16
	.private_extern _div_pow2__div_pow2_LC_16
_div_pow2__div_pow2_LC_16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$16, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1010
	movl	$27361, %edi
	call	__m3_fault
L1010:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_17
	.private_extern _div_pow2__div_pow2_LC_17
_div_pow2__div_pow2_LC_17:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$17, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1013
	movl	$27425, %edi
	call	__m3_fault
L1013:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_18
	.private_extern _div_pow2__div_pow2_LC_18
_div_pow2__div_pow2_LC_18:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$18, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1016
	movl	$27489, %edi
	call	__m3_fault
L1016:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_19
	.private_extern _div_pow2__div_pow2_LC_19
_div_pow2__div_pow2_LC_19:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$19, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1019
	movl	$27553, %edi
	call	__m3_fault
L1019:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_20
	.private_extern _div_pow2__div_pow2_LC_20
_div_pow2__div_pow2_LC_20:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$20, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1022
	movl	$27617, %edi
	call	__m3_fault
L1022:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_21
	.private_extern _div_pow2__div_pow2_LC_21
_div_pow2__div_pow2_LC_21:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$21, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1025
	movl	$27681, %edi
	call	__m3_fault
L1025:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_22
	.private_extern _div_pow2__div_pow2_LC_22
_div_pow2__div_pow2_LC_22:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$22, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1028
	movl	$27745, %edi
	call	__m3_fault
L1028:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_23
	.private_extern _div_pow2__div_pow2_LC_23
_div_pow2__div_pow2_LC_23:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$23, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1031
	movl	$27809, %edi
	call	__m3_fault
L1031:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_24
	.private_extern _div_pow2__div_pow2_LC_24
_div_pow2__div_pow2_LC_24:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$24, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1034
	movl	$27873, %edi
	call	__m3_fault
L1034:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_25
	.private_extern _div_pow2__div_pow2_LC_25
_div_pow2__div_pow2_LC_25:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$25, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1037
	movl	$27937, %edi
	call	__m3_fault
L1037:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_26
	.private_extern _div_pow2__div_pow2_LC_26
_div_pow2__div_pow2_LC_26:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$26, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1040
	movl	$28001, %edi
	call	__m3_fault
L1040:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_27
	.private_extern _div_pow2__div_pow2_LC_27
_div_pow2__div_pow2_LC_27:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$27, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1043
	movl	$28065, %edi
	call	__m3_fault
L1043:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_28
	.private_extern _div_pow2__div_pow2_LC_28
_div_pow2__div_pow2_LC_28:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$28, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1046
	movl	$28129, %edi
	call	__m3_fault
L1046:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_29
	.private_extern _div_pow2__div_pow2_LC_29
_div_pow2__div_pow2_LC_29:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$29, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1049
	movl	$28193, %edi
	call	__m3_fault
L1049:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_30
	.private_extern _div_pow2__div_pow2_LC_30
_div_pow2__div_pow2_LC_30:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$30, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1052
	movl	$28257, %edi
	call	__m3_fault
L1052:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_31
	.private_extern _div_pow2__div_pow2_LC_31
_div_pow2__div_pow2_LC_31:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$31, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1055
	movl	$28321, %edi
	call	__m3_fault
L1055:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_32
	.private_extern _div_pow2__div_pow2_LC_32
_div_pow2__div_pow2_LC_32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$32, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1058
	movl	$28385, %edi
	call	__m3_fault
L1058:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_33
	.private_extern _div_pow2__div_pow2_LC_33
_div_pow2__div_pow2_LC_33:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$33, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1061
	movl	$28449, %edi
	call	__m3_fault
L1061:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_34
	.private_extern _div_pow2__div_pow2_LC_34
_div_pow2__div_pow2_LC_34:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$34, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1064
	movl	$28513, %edi
	call	__m3_fault
L1064:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_35
	.private_extern _div_pow2__div_pow2_LC_35
_div_pow2__div_pow2_LC_35:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$35, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1067
	movl	$28577, %edi
	call	__m3_fault
L1067:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_36
	.private_extern _div_pow2__div_pow2_LC_36
_div_pow2__div_pow2_LC_36:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$36, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1070
	movl	$28641, %edi
	call	__m3_fault
L1070:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_37
	.private_extern _div_pow2__div_pow2_LC_37
_div_pow2__div_pow2_LC_37:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$37, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1073
	movl	$28705, %edi
	call	__m3_fault
L1073:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_38
	.private_extern _div_pow2__div_pow2_LC_38
_div_pow2__div_pow2_LC_38:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$38, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1076
	movl	$28769, %edi
	call	__m3_fault
L1076:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_39
	.private_extern _div_pow2__div_pow2_LC_39
_div_pow2__div_pow2_LC_39:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$39, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1079
	movl	$28833, %edi
	call	__m3_fault
L1079:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_40
	.private_extern _div_pow2__div_pow2_LC_40
_div_pow2__div_pow2_LC_40:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$40, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1082
	movl	$28897, %edi
	call	__m3_fault
L1082:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_41
	.private_extern _div_pow2__div_pow2_LC_41
_div_pow2__div_pow2_LC_41:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$41, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1085
	movl	$28961, %edi
	call	__m3_fault
L1085:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_42
	.private_extern _div_pow2__div_pow2_LC_42
_div_pow2__div_pow2_LC_42:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$42, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1088
	movl	$29025, %edi
	call	__m3_fault
L1088:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_43
	.private_extern _div_pow2__div_pow2_LC_43
_div_pow2__div_pow2_LC_43:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$43, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1091
	movl	$29089, %edi
	call	__m3_fault
L1091:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_44
	.private_extern _div_pow2__div_pow2_LC_44
_div_pow2__div_pow2_LC_44:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$44, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1094
	movl	$29153, %edi
	call	__m3_fault
L1094:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_45
	.private_extern _div_pow2__div_pow2_LC_45
_div_pow2__div_pow2_LC_45:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$45, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1097
	movl	$29217, %edi
	call	__m3_fault
L1097:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_46
	.private_extern _div_pow2__div_pow2_LC_46
_div_pow2__div_pow2_LC_46:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$46, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1100
	movl	$29281, %edi
	call	__m3_fault
L1100:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_47
	.private_extern _div_pow2__div_pow2_LC_47
_div_pow2__div_pow2_LC_47:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$47, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1103
	movl	$29345, %edi
	call	__m3_fault
L1103:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_48
	.private_extern _div_pow2__div_pow2_LC_48
_div_pow2__div_pow2_LC_48:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$48, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1106
	movl	$29409, %edi
	call	__m3_fault
L1106:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_49
	.private_extern _div_pow2__div_pow2_LC_49
_div_pow2__div_pow2_LC_49:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$49, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1109
	movl	$29473, %edi
	call	__m3_fault
L1109:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_50
	.private_extern _div_pow2__div_pow2_LC_50
_div_pow2__div_pow2_LC_50:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$50, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1112
	movl	$29537, %edi
	call	__m3_fault
L1112:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_51
	.private_extern _div_pow2__div_pow2_LC_51
_div_pow2__div_pow2_LC_51:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$51, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1115
	movl	$29601, %edi
	call	__m3_fault
L1115:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_52
	.private_extern _div_pow2__div_pow2_LC_52
_div_pow2__div_pow2_LC_52:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$52, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1118
	movl	$29665, %edi
	call	__m3_fault
L1118:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_53
	.private_extern _div_pow2__div_pow2_LC_53
_div_pow2__div_pow2_LC_53:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$53, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1121
	movl	$29729, %edi
	call	__m3_fault
L1121:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_54
	.private_extern _div_pow2__div_pow2_LC_54
_div_pow2__div_pow2_LC_54:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$54, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1124
	movl	$29793, %edi
	call	__m3_fault
L1124:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_55
	.private_extern _div_pow2__div_pow2_LC_55
_div_pow2__div_pow2_LC_55:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$55, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1127
	movl	$29857, %edi
	call	__m3_fault
L1127:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_56
	.private_extern _div_pow2__div_pow2_LC_56
_div_pow2__div_pow2_LC_56:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$56, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1130
	movl	$29921, %edi
	call	__m3_fault
L1130:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_57
	.private_extern _div_pow2__div_pow2_LC_57
_div_pow2__div_pow2_LC_57:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$57, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1133
	movl	$29985, %edi
	call	__m3_fault
L1133:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_58
	.private_extern _div_pow2__div_pow2_LC_58
_div_pow2__div_pow2_LC_58:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$58, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1136
	movl	$30049, %edi
	call	__m3_fault
L1136:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_59
	.private_extern _div_pow2__div_pow2_LC_59
_div_pow2__div_pow2_LC_59:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$59, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1139
	movl	$30113, %edi
	call	__m3_fault
L1139:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_60
	.private_extern _div_pow2__div_pow2_LC_60
_div_pow2__div_pow2_LC_60:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$60, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1142
	movl	$30177, %edi
	call	__m3_fault
L1142:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_61
	.private_extern _div_pow2__div_pow2_LC_61
_div_pow2__div_pow2_LC_61:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$61, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1145
	movl	$30241, %edi
	call	__m3_fault
L1145:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_62
	.private_extern _div_pow2__div_pow2_LC_62
_div_pow2__div_pow2_LC_62:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$62, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1148
	movl	$30305, %edi
	call	__m3_fault
L1148:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2__div_pow2_LC_63
	.private_extern _div_pow2__div_pow2_LC_63
_div_pow2__div_pow2_LC_63:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leaq	-1(%rax), %rdx
	orq	%rdx, %rax
	movq	%rax, %rcx
	sarq	$63, %rcx
	subq	%rcx, %rdx
	movabsq	$-9223372036854775808, %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %edx
	movq	%rcx, %rax
	notq	%rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1151
	movl	$30369, %edi
	call	__m3_fault
L1151:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _div_pow2_M3
_div_pow2_M3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	leaq	_MM_div_pow2(%rip), %rax
	leave
	ret
__m3_fault:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rsi
	leaq	_MM_div_pow2(%rip), %rdi
	call	_RTHooks__ReportFault
	leave
	ret
	.const_data
	.align 5
_L_1:
	.ascii "div_pow2_M3"
	.space 1
	.ascii "div_pow2_LC_63"
	.space 1
	.ascii "div_pow2_LC_62"
	.space 1
	.ascii "div_pow2_LC_61"
	.space 1
	.ascii "div_pow2_LC_60"
	.space 1
	.ascii "div_pow2_LC_59"
	.space 1
	.ascii "div_pow2_LC_58"
	.space 1
	.ascii "div_pow2_LC_57"
	.space 1
	.ascii "div_pow2_LC_56"
	.space 1
	.ascii "div_pow2_LC_55"
	.space 1
	.ascii "div_pow2_LC_54"
	.space 1
	.ascii "div_pow2_LC_53"
	.space 1
	.ascii "div_pow2_LC_52"
	.space 1
	.ascii "div_pow2_LC_51"
	.space 1
	.ascii "div_pow2_LC_50"
	.space 1
	.ascii "div_pow2_LC_49"
	.space 1
	.ascii "div_pow2_LC_48"
	.space 1
	.ascii "div_pow2_LC_47"
	.space 1
	.ascii "div_pow2_LC_46"
	.space 1
	.ascii "div_pow2_LC_45"
	.space 1
	.ascii "div_pow2_LC_44"
	.space 1
	.ascii "div_pow2_LC_43"
	.space 1
	.ascii "div_pow2_LC_42"
	.space 1
	.ascii "div_pow2_LC_41"
	.space 1
	.ascii "div_pow2_LC_40"
	.space 1
	.ascii "div_pow2_LC_39"
	.space 1
	.ascii "div_pow2_LC_38"
	.space 1
	.ascii "div_pow2_LC_37"
	.space 1
	.ascii "div_pow2_LC_36"
	.space 1
	.ascii "div_pow2_LC_35"
	.space 1
	.ascii "div_pow2_LC_34"
	.space 1
	.ascii "div_pow2_LC_33"
	.space 1
	.ascii "div_pow2_LC_32"
	.space 1
	.ascii "div_pow2_LC_31"
	.space 1
	.ascii "div_pow2_LC_30"
	.space 1
	.ascii "div_pow2_LC_29"
	.space 1
	.ascii "div_pow2_LC_28"
	.space 1
	.ascii "div_pow2_LC_27"
	.space 1
	.ascii "div_pow2_LC_26"
	.space 1
	.ascii "div_pow2_LC_25"
	.space 1
	.ascii "div_pow2_LC_24"
	.space 1
	.ascii "div_pow2_LC_23"
	.space 1
	.ascii "div_pow2_LC_22"
	.space 1
	.ascii "div_pow2_LC_21"
	.space 1
	.ascii "div_pow2_LC_20"
	.space 1
	.ascii "div_pow2_LC_19"
	.space 1
	.ascii "div_pow2_LC_18"
	.space 1
	.ascii "div_pow2_LC_17"
	.space 1
	.ascii "div_pow2_LC_16"
	.space 1
	.ascii "div_pow2_LC_15"
	.space 1
	.ascii "div_pow2_LC_14"
	.space 1
	.ascii "div_pow2_LC_13"
	.space 1
	.ascii "div_pow2_LC_12"
	.space 1
	.ascii "div_pow2_LC_11"
	.space 1
	.ascii "div_pow2_LC_10"
	.space 1
	.ascii "div_pow2_LC_9"
	.space 1
	.ascii "div_pow2_LC_8"
	.space 1
	.ascii "div_pow2_LC_7"
	.space 1
	.ascii "div_pow2_LC_6"
	.space 1
	.ascii "div_pow2_LC_5"
	.space 1
	.ascii "div_pow2_LC_4"
	.space 1
	.ascii "div_pow2_LC_3"
	.space 1
	.ascii "div_pow2_LC_2"
	.space 1
	.ascii "div_pow2_LC_1"
	.space 1
	.ascii "div_pow2_LC_0"
	.space 1
	.ascii "div_pow2_L_63"
	.space 1
	.ascii "div_pow2_L_62"
	.space 1
	.ascii "div_pow2_L_61"
	.space 1
	.ascii "div_pow2_L_60"
	.space 1
	.ascii "div_pow2_L_59"
	.space 1
	.ascii "div_pow2_L_58"
	.space 1
	.ascii "div_pow2_L_57"
	.space 1
	.ascii "div_pow2_L_56"
	.space 1
	.ascii "div_pow2_L_55"
	.space 1
	.ascii "div_pow2_L_54"
	.space 1
	.ascii "div_pow2_L_53"
	.space 1
	.ascii "div_pow2_L_52"
	.space 1
	.ascii "div_pow2_L_51"
	.space 1
	.ascii "div_pow2_L_50"
	.space 1
	.ascii "div_pow2_L_49"
	.space 1
	.ascii "div_pow2_L_48"
	.space 1
	.ascii "div_pow2_L_47"
	.space 1
	.ascii "div_pow2_L_46"
	.space 1
	.ascii "div_pow2_L_45"
	.space 1
	.ascii "div_pow2_L_44"
	.space 1
	.ascii "div_pow2_L_43"
	.space 1
	.ascii "div_pow2_L_42"
	.space 1
	.ascii "div_pow2_L_41"
	.space 1
	.ascii "div_pow2_L_40"
	.space 1
	.ascii "div_pow2_L_39"
	.space 1
	.ascii "div_pow2_L_38"
	.space 1
	.ascii "div_pow2_L_37"
	.space 1
	.ascii "div_pow2_L_36"
	.space 1
	.ascii "div_pow2_L_35"
	.space 1
	.ascii "div_pow2_L_34"
	.space 1
	.ascii "div_pow2_L_33"
	.space 1
	.ascii "div_pow2_L_32"
	.space 1
	.ascii "div_pow2_L_31"
	.space 1
	.ascii "div_pow2_L_30"
	.space 1
	.ascii "div_pow2_L_29"
	.space 1
	.ascii "div_pow2_L_28"
	.space 1
	.ascii "div_pow2_L_27"
	.space 1
	.ascii "div_pow2_L_26"
	.space 1
	.ascii "div_pow2_L_25"
	.space 1
	.ascii "div_pow2_L_24"
	.space 1
	.ascii "div_pow2_L_23"
	.space 1
	.ascii "div_pow2_L_22"
	.space 1
	.ascii "div_pow2_L_21"
	.space 1
	.ascii "div_pow2_L_20"
	.space 1
	.ascii "div_pow2_L_19"
	.space 1
	.ascii "div_pow2_L_18"
	.space 1
	.ascii "div_pow2_L_17"
	.space 1
	.ascii "div_pow2_L_16"
	.space 1
	.ascii "div_pow2_L_15"
	.space 1
	.ascii "div_pow2_L_14"
	.space 1
	.ascii "div_pow2_L_13"
	.space 1
	.ascii "div_pow2_L_12"
	.space 1
	.ascii "div_pow2_L_11"
	.space 1
	.ascii "div_pow2_L_10"
	.space 1
	.ascii "div_pow2_L_9"
	.space 1
	.ascii "div_pow2_L_8"
	.space 1
	.ascii "div_pow2_L_7"
	.space 1
	.ascii "div_pow2_L_6"
	.space 1
	.ascii "div_pow2_L_5"
	.space 1
	.ascii "div_pow2_L_4"
	.space 1
	.ascii "div_pow2_L_3"
	.space 1
	.ascii "div_pow2_L_2"
	.space 1
	.ascii "div_pow2_L_1"
	.space 1
	.ascii "div_pow2_L_0"
	.space 1
	.ascii "div_pow2_C_63"
	.space 1
	.ascii "div_pow2_C_62"
	.space 1
	.ascii "div_pow2_C_61"
	.space 1
	.ascii "div_pow2_C_60"
	.space 1
	.ascii "div_pow2_C_59"
	.space 1
	.ascii "div_pow2_C_58"
	.space 1
	.ascii "div_pow2_C_57"
	.space 1
	.ascii "div_pow2_C_56"
	.space 1
	.ascii "div_pow2_C_55"
	.space 1
	.ascii "div_pow2_C_54"
	.space 1
	.ascii "div_pow2_C_53"
	.space 1
	.ascii "div_pow2_C_52"
	.space 1
	.ascii "div_pow2_C_51"
	.space 1
	.ascii "div_pow2_C_50"
	.space 1
	.ascii "div_pow2_C_49"
	.space 1
	.ascii "div_pow2_C_48"
	.space 1
	.ascii "div_pow2_C_47"
	.space 1
	.ascii "div_pow2_C_46"
	.space 1
	.ascii "div_pow2_C_45"
	.space 1
	.ascii "div_pow2_C_44"
	.space 1
	.ascii "div_pow2_C_43"
	.space 1
	.ascii "div_pow2_C_42"
	.space 1
	.ascii "div_pow2_C_41"
	.space 1
	.ascii "div_pow2_C_40"
	.space 1
	.ascii "div_pow2_C_39"
	.space 1
	.ascii "div_pow2_C_38"
	.space 1
	.ascii "div_pow2_C_37"
	.space 1
	.ascii "div_pow2_C_36"
	.space 1
	.ascii "div_pow2_C_35"
	.space 1
	.ascii "div_pow2_C_34"
	.space 1
	.ascii "div_pow2_C_33"
	.space 1
	.ascii "div_pow2_C_32"
	.space 1
	.ascii "div_pow2_C_31"
	.space 1
	.ascii "div_pow2_C_30"
	.space 1
	.ascii "div_pow2_C_29"
	.space 1
	.ascii "div_pow2_C_28"
	.space 1
	.ascii "div_pow2_C_27"
	.space 1
	.ascii "div_pow2_C_26"
	.space 1
	.ascii "div_pow2_C_25"
	.space 1
	.ascii "div_pow2_C_24"
	.space 1
	.ascii "div_pow2_C_23"
	.space 1
	.ascii "div_pow2_C_22"
	.space 1
	.ascii "div_pow2_C_21"
	.space 1
	.ascii "div_pow2_C_20"
	.space 1
	.ascii "div_pow2_C_19"
	.space 1
	.ascii "div_pow2_C_18"
	.space 1
	.ascii "div_pow2_C_17"
	.space 1
	.ascii "div_pow2_C_16"
	.space 1
	.ascii "div_pow2_C_15"
	.space 1
	.ascii "div_pow2_C_14"
	.space 1
	.ascii "div_pow2_C_13"
	.space 1
	.ascii "div_pow2_C_12"
	.space 1
	.ascii "div_pow2_C_11"
	.space 1
	.ascii "div_pow2_C_10"
	.space 1
	.ascii "div_pow2_C_9"
	.space 1
	.ascii "div_pow2_C_8"
	.space 1
	.ascii "div_pow2_C_7"
	.space 1
	.ascii "div_pow2_C_6"
	.space 1
	.ascii "div_pow2_C_5"
	.space 1
	.ascii "div_pow2_C_4"
	.space 1
	.ascii "div_pow2_C_3"
	.space 1
	.ascii "div_pow2_C_2"
	.space 1
	.ascii "div_pow2_C_1"
	.space 1
	.ascii "div_pow2_C_0"
	.space 1
	.ascii "div_pow2_I_63"
	.space 1
	.ascii "div_pow2_I_62"
	.space 1
	.ascii "div_pow2_I_61"
	.space 1
	.ascii "div_pow2_I_60"
	.space 1
	.ascii "div_pow2_I_59"
	.space 1
	.ascii "div_pow2_I_58"
	.space 1
	.ascii "div_pow2_I_57"
	.space 1
	.ascii "div_pow2_I_56"
	.space 1
	.ascii "div_pow2_I_55"
	.space 1
	.ascii "div_pow2_I_54"
	.space 1
	.ascii "div_pow2_I_53"
	.space 1
	.ascii "div_pow2_I_52"
	.space 1
	.ascii "div_pow2_I_51"
	.space 1
	.ascii "div_pow2_I_50"
	.space 1
	.ascii "div_pow2_I_49"
	.space 1
	.ascii "div_pow2_I_48"
	.space 1
	.ascii "div_pow2_I_47"
	.space 1
	.ascii "div_pow2_I_46"
	.space 1
	.ascii "div_pow2_I_45"
	.space 1
	.ascii "div_pow2_I_44"
	.space 1
	.ascii "div_pow2_I_43"
	.space 1
	.ascii "div_pow2_I_42"
	.space 1
	.ascii "div_pow2_I_41"
	.space 1
	.ascii "div_pow2_I_40"
	.space 1
	.ascii "div_pow2_I_39"
	.space 1
	.ascii "div_pow2_I_38"
	.space 1
	.ascii "div_pow2_I_37"
	.space 1
	.ascii "div_pow2_I_36"
	.space 1
	.ascii "div_pow2_I_35"
	.space 1
	.ascii "div_pow2_I_34"
	.space 1
	.ascii "div_pow2_I_33"
	.space 1
	.ascii "div_pow2_I_32"
	.space 1
	.ascii "div_pow2_I_31"
	.space 1
	.ascii "div_pow2_I_30"
	.space 1
	.ascii "div_pow2_I_29"
	.space 1
	.ascii "div_pow2_I_28"
	.space 1
	.ascii "div_pow2_I_27"
	.space 1
	.ascii "div_pow2_I_26"
	.space 1
	.ascii "div_pow2_I_25"
	.space 1
	.ascii "div_pow2_I_24"
	.space 1
	.ascii "div_pow2_I_23"
	.space 1
	.ascii "div_pow2_I_22"
	.space 1
	.ascii "div_pow2_I_21"
	.space 1
	.ascii "div_pow2_I_20"
	.space 1
	.ascii "div_pow2_I_19"
	.space 1
	.ascii "div_pow2_I_18"
	.space 1
	.ascii "div_pow2_I_17"
	.space 1
	.ascii "div_pow2_I_16"
	.space 1
	.ascii "div_pow2_I_15"
	.space 1
	.ascii "div_pow2_I_14"
	.space 1
	.ascii "div_pow2_I_13"
	.space 1
	.ascii "div_pow2_I_12"
	.space 1
	.ascii "div_pow2_I_11"
	.space 1
	.ascii "div_pow2_I_10"
	.space 1
	.ascii "div_pow2_I_9"
	.space 1
	.ascii "div_pow2_I_8"
	.space 1
	.ascii "div_pow2_I_7"
	.space 1
	.ascii "div_pow2_I_6"
	.space 1
	.ascii "div_pow2_I_5"
	.space 1
	.ascii "div_pow2_I_4"
	.space 1
	.ascii "div_pow2_I_3"
	.space 1
	.ascii "div_pow2_I_2"
	.space 1
	.ascii "div_pow2_I_1"
	.space 1
	.ascii "div_pow2_I_0"
	.space 1
	.ascii "div_pow2_u64_63"
	.space 1
	.ascii "div_pow2_u64_62"
	.space 1
	.ascii "div_pow2_u64_61"
	.space 1
	.ascii "div_pow2_u64_60"
	.space 1
	.ascii "div_pow2_u64_59"
	.space 1
	.ascii "div_pow2_u64_58"
	.space 1
	.ascii "div_pow2_u64_57"
	.space 1
	.ascii "div_pow2_u64_56"
	.space 1
	.ascii "div_pow2_u64_55"
	.space 1
	.ascii "div_pow2_u64_54"
	.space 1
	.ascii "div_pow2_u64_53"
	.space 1
	.ascii "div_pow2_u64_52"
	.space 1
	.ascii "div_pow2_u64_51"
	.space 1
	.ascii "div_pow2_u64_50"
	.space 1
	.ascii "div_pow2_u64_49"
	.space 1
	.ascii "div_pow2_u64_48"
	.space 1
	.ascii "div_pow2_u64_47"
	.space 1
	.ascii "div_pow2_u64_46"
	.space 1
	.ascii "div_pow2_u64_45"
	.space 1
	.ascii "div_pow2_u64_44"
	.space 1
	.ascii "div_pow2_u64_43"
	.space 1
	.ascii "div_pow2_u64_42"
	.space 1
	.ascii "div_pow2_u64_41"
	.space 1
	.ascii "div_pow2_u64_40"
	.space 1
	.ascii "div_pow2_u64_39"
	.space 1
	.ascii "div_pow2_u64_38"
	.space 1
	.ascii "div_pow2_u64_37"
	.space 1
	.ascii "div_pow2_u64_36"
	.space 1
	.ascii "div_pow2_u64_35"
	.space 1
	.ascii "div_pow2_u64_34"
	.space 1
	.ascii "div_pow2_u64_33"
	.space 1
	.ascii "div_pow2_u64_32"
	.space 1
	.ascii "div_pow2_u64_31"
	.space 1
	.ascii "div_pow2_u64_30"
	.space 1
	.ascii "div_pow2_u64_29"
	.space 1
	.ascii "div_pow2_u64_28"
	.space 1
	.ascii "div_pow2_u64_27"
	.space 1
	.ascii "div_pow2_u64_26"
	.space 1
	.ascii "div_pow2_u64_25"
	.space 1
	.ascii "div_pow2_u64_24"
	.space 1
	.ascii "div_pow2_u64_23"
	.space 1
	.ascii "div_pow2_u64_22"
	.space 1
	.ascii "div_pow2_u64_21"
	.space 1
	.ascii "div_pow2_u64_20"
	.space 1
	.ascii "div_pow2_u64_19"
	.space 1
	.ascii "div_pow2_u64_18"
	.space 1
	.ascii "div_pow2_u64_17"
	.space 1
	.ascii "div_pow2_u64_16"
	.space 1
	.ascii "div_pow2_u64_15"
	.space 1
	.ascii "div_pow2_u64_14"
	.space 1
	.ascii "div_pow2_u64_13"
	.space 1
	.ascii "div_pow2_u64_12"
	.space 1
	.ascii "div_pow2_u64_11"
	.space 1
	.ascii "div_pow2_u64_10"
	.space 1
	.ascii "div_pow2_u64_9"
	.space 1
	.ascii "div_pow2_u64_8"
	.space 1
	.ascii "div_pow2_u64_7"
	.space 1
	.ascii "div_pow2_u64_6"
	.space 1
	.ascii "div_pow2_u64_5"
	.space 1
	.ascii "div_pow2_u64_4"
	.space 1
	.ascii "div_pow2_u64_3"
	.space 1
	.ascii "div_pow2_u64_2"
	.space 1
	.ascii "div_pow2_u64_1"
	.space 1
	.ascii "div_pow2_u64_0"
	.space 1
	.ascii "div_pow2_i64_63"
	.space 1
	.ascii "div_pow2_i64_62"
	.space 1
	.ascii "div_pow2_i64_61"
	.space 1
	.ascii "div_pow2_i64_60"
	.space 1
	.ascii "div_pow2_i64_59"
	.space 1
	.ascii "div_pow2_i64_58"
	.space 1
	.ascii "div_pow2_i64_57"
	.space 1
	.ascii "div_pow2_i64_56"
	.space 1
	.ascii "div_pow2_i64_55"
	.space 1
	.ascii "div_pow2_i64_54"
	.space 1
	.ascii "div_pow2_i64_53"
	.space 1
	.ascii "div_pow2_i64_52"
	.space 1
	.ascii "div_pow2_i64_51"
	.space 1
	.ascii "div_pow2_i64_50"
	.space 1
	.ascii "div_pow2_i64_49"
	.space 1
	.ascii "div_pow2_i64_48"
	.space 1
	.ascii "div_pow2_i64_47"
	.space 1
	.ascii "div_pow2_i64_46"
	.space 1
	.ascii "div_pow2_i64_45"
	.space 1
	.ascii "div_pow2_i64_44"
	.space 1
	.ascii "div_pow2_i64_43"
	.space 1
	.ascii "div_pow2_i64_42"
	.space 1
	.ascii "div_pow2_i64_41"
	.space 1
	.ascii "div_pow2_i64_40"
	.space 1
	.ascii "div_pow2_i64_39"
	.space 1
	.ascii "div_pow2_i64_38"
	.space 1
	.ascii "div_pow2_i64_37"
	.space 1
	.ascii "div_pow2_i64_36"
	.space 1
	.ascii "div_pow2_i64_35"
	.space 1
	.ascii "div_pow2_i64_34"
	.space 1
	.ascii "div_pow2_i64_33"
	.space 1
	.ascii "div_pow2_i64_32"
	.space 1
	.ascii "div_pow2_i64_31"
	.space 1
	.ascii "div_pow2_i64_30"
	.space 1
	.ascii "div_pow2_i64_29"
	.space 1
	.ascii "div_pow2_i64_28"
	.space 1
	.ascii "div_pow2_i64_27"
	.space 1
	.ascii "div_pow2_i64_26"
	.space 1
	.ascii "div_pow2_i64_25"
	.space 1
	.ascii "div_pow2_i64_24"
	.space 1
	.ascii "div_pow2_i64_23"
	.space 1
	.ascii "div_pow2_i64_22"
	.space 1
	.ascii "div_pow2_i64_21"
	.space 1
	.ascii "div_pow2_i64_20"
	.space 1
	.ascii "div_pow2_i64_19"
	.space 1
	.ascii "div_pow2_i64_18"
	.space 1
	.ascii "div_pow2_i64_17"
	.space 1
	.ascii "div_pow2_i64_16"
	.space 1
	.ascii "div_pow2_i64_15"
	.space 1
	.ascii "div_pow2_i64_14"
	.space 1
	.ascii "div_pow2_i64_13"
	.space 1
	.ascii "div_pow2_i64_12"
	.space 1
	.ascii "div_pow2_i64_11"
	.space 1
	.ascii "div_pow2_i64_10"
	.space 1
	.ascii "div_pow2_i64_9"
	.space 1
	.ascii "div_pow2_i64_8"
	.space 1
	.ascii "div_pow2_i64_7"
	.space 1
	.ascii "div_pow2_i64_6"
	.space 1
	.ascii "div_pow2_i64_5"
	.space 1
	.ascii "div_pow2_i64_4"
	.space 1
	.ascii "div_pow2_i64_3"
	.space 1
	.ascii "div_pow2_i64_2"
	.space 1
	.ascii "div_pow2_i64_1"
	.space 1
	.ascii "div_pow2_i64_0"
	.space 1
	.ascii "div_pow2_u32_31"
	.space 1
	.ascii "div_pow2_u32_30"
	.space 1
	.ascii "div_pow2_u32_29"
	.space 1
	.ascii "div_pow2_u32_28"
	.space 1
	.ascii "div_pow2_u32_27"
	.space 1
	.ascii "div_pow2_u32_26"
	.space 1
	.ascii "div_pow2_u32_25"
	.space 1
	.ascii "div_pow2_u32_24"
	.space 1
	.ascii "div_pow2_u32_23"
	.space 1
	.ascii "div_pow2_u32_22"
	.space 1
	.ascii "div_pow2_u32_21"
	.space 1
	.ascii "div_pow2_u32_20"
	.space 1
	.ascii "div_pow2_u32_19"
	.space 1
	.ascii "div_pow2_u32_18"
	.space 1
	.ascii "div_pow2_u32_17"
	.space 1
	.ascii "div_pow2_u32_16"
	.space 1
	.ascii "div_pow2_u32_15"
	.space 1
	.ascii "div_pow2_u32_14"
	.space 1
	.ascii "div_pow2_u32_13"
	.space 1
	.ascii "div_pow2_u32_12"
	.space 1
	.ascii "div_pow2_u32_11"
	.space 1
	.ascii "div_pow2_u32_10"
	.space 1
	.ascii "div_pow2_u32_9"
	.space 1
	.ascii "div_pow2_u32_8"
	.space 1
	.ascii "div_pow2_u32_7"
	.space 1
	.ascii "div_pow2_u32_6"
	.space 1
	.ascii "div_pow2_u32_5"
	.space 1
	.ascii "div_pow2_u32_4"
	.space 1
	.ascii "div_pow2_u32_3"
	.space 1
	.ascii "div_pow2_u32_2"
	.space 1
	.ascii "div_pow2_u32_1"
	.space 1
	.ascii "div_pow2_u32_0"
	.space 1
	.ascii "div_pow2_i32_31"
	.space 1
	.ascii "div_pow2_i32_30"
	.space 1
	.ascii "div_pow2_i32_29"
	.space 1
	.ascii "div_pow2_i32_28"
	.space 1
	.ascii "div_pow2_i32_27"
	.space 1
	.ascii "div_pow2_i32_26"
	.space 1
	.ascii "div_pow2_i32_25"
	.space 1
	.ascii "div_pow2_i32_24"
	.space 1
	.ascii "div_pow2_i32_23"
	.space 1
	.ascii "div_pow2_i32_22"
	.space 1
	.ascii "div_pow2_i32_21"
	.space 1
	.ascii "div_pow2_i32_20"
	.space 1
	.ascii "div_pow2_i32_19"
	.space 1
	.ascii "div_pow2_i32_18"
	.space 1
	.ascii "div_pow2_i32_17"
	.space 1
	.ascii "div_pow2_i32_16"
	.space 1
	.ascii "div_pow2_i32_15"
	.space 1
	.ascii "div_pow2_i32_14"
	.space 1
	.ascii "div_pow2_i32_13"
	.space 1
	.ascii "div_pow2_i32_12"
	.space 1
	.ascii "div_pow2_i32_11"
	.space 1
	.ascii "div_pow2_i32_10"
	.space 1
	.ascii "div_pow2_i32_9"
	.space 1
	.ascii "div_pow2_i32_8"
	.space 1
	.ascii "div_pow2_i32_7"
	.space 1
	.ascii "div_pow2_i32_6"
	.space 1
	.ascii "div_pow2_i32_5"
	.space 1
	.ascii "div_pow2_i32_4"
	.space 1
	.ascii "div_pow2_i32_3"
	.space 1
	.ascii "div_pow2_i32_2"
	.space 1
	.ascii "div_pow2_i32_1"
	.space 1
	.ascii "div_pow2_i32_0"
	.space 5
	.quad	_div_pow2_M3
	.quad	_L_1
	.quad	_div_pow2__div_pow2_LC_63
	.quad	_L_1+12
	.quad	_div_pow2__div_pow2_LC_62
	.quad	_L_1+27
	.quad	_div_pow2__div_pow2_LC_61
	.quad	_L_1+42
	.quad	_div_pow2__div_pow2_LC_60
	.quad	_L_1+57
	.quad	_div_pow2__div_pow2_LC_59
	.quad	_L_1+72
	.quad	_div_pow2__div_pow2_LC_58
	.quad	_L_1+87
	.quad	_div_pow2__div_pow2_LC_57
	.quad	_L_1+102
	.quad	_div_pow2__div_pow2_LC_56
	.quad	_L_1+117
	.quad	_div_pow2__div_pow2_LC_55
	.quad	_L_1+132
	.quad	_div_pow2__div_pow2_LC_54
	.quad	_L_1+147
	.quad	_div_pow2__div_pow2_LC_53
	.quad	_L_1+162
	.quad	_div_pow2__div_pow2_LC_52
	.quad	_L_1+177
	.quad	_div_pow2__div_pow2_LC_51
	.quad	_L_1+192
	.quad	_div_pow2__div_pow2_LC_50
	.quad	_L_1+207
	.quad	_div_pow2__div_pow2_LC_49
	.quad	_L_1+222
	.quad	_div_pow2__div_pow2_LC_48
	.quad	_L_1+237
	.quad	_div_pow2__div_pow2_LC_47
	.quad	_L_1+252
	.quad	_div_pow2__div_pow2_LC_46
	.quad	_L_1+267
	.quad	_div_pow2__div_pow2_LC_45
	.quad	_L_1+282
	.quad	_div_pow2__div_pow2_LC_44
	.quad	_L_1+297
	.quad	_div_pow2__div_pow2_LC_43
	.quad	_L_1+312
	.quad	_div_pow2__div_pow2_LC_42
	.quad	_L_1+327
	.quad	_div_pow2__div_pow2_LC_41
	.quad	_L_1+342
	.quad	_div_pow2__div_pow2_LC_40
	.quad	_L_1+357
	.quad	_div_pow2__div_pow2_LC_39
	.quad	_L_1+372
	.quad	_div_pow2__div_pow2_LC_38
	.quad	_L_1+387
	.quad	_div_pow2__div_pow2_LC_37
	.quad	_L_1+402
	.quad	_div_pow2__div_pow2_LC_36
	.quad	_L_1+417
	.quad	_div_pow2__div_pow2_LC_35
	.quad	_L_1+432
	.quad	_div_pow2__div_pow2_LC_34
	.quad	_L_1+447
	.quad	_div_pow2__div_pow2_LC_33
	.quad	_L_1+462
	.quad	_div_pow2__div_pow2_LC_32
	.quad	_L_1+477
	.quad	_div_pow2__div_pow2_LC_31
	.quad	_L_1+492
	.quad	_div_pow2__div_pow2_LC_30
	.quad	_L_1+507
	.quad	_div_pow2__div_pow2_LC_29
	.quad	_L_1+522
	.quad	_div_pow2__div_pow2_LC_28
	.quad	_L_1+537
	.quad	_div_pow2__div_pow2_LC_27
	.quad	_L_1+552
	.quad	_div_pow2__div_pow2_LC_26
	.quad	_L_1+567
	.quad	_div_pow2__div_pow2_LC_25
	.quad	_L_1+582
	.quad	_div_pow2__div_pow2_LC_24
	.quad	_L_1+597
	.quad	_div_pow2__div_pow2_LC_23
	.quad	_L_1+612
	.quad	_div_pow2__div_pow2_LC_22
	.quad	_L_1+627
	.quad	_div_pow2__div_pow2_LC_21
	.quad	_L_1+642
	.quad	_div_pow2__div_pow2_LC_20
	.quad	_L_1+657
	.quad	_div_pow2__div_pow2_LC_19
	.quad	_L_1+672
	.quad	_div_pow2__div_pow2_LC_18
	.quad	_L_1+687
	.quad	_div_pow2__div_pow2_LC_17
	.quad	_L_1+702
	.quad	_div_pow2__div_pow2_LC_16
	.quad	_L_1+717
	.quad	_div_pow2__div_pow2_LC_15
	.quad	_L_1+732
	.quad	_div_pow2__div_pow2_LC_14
	.quad	_L_1+747
	.quad	_div_pow2__div_pow2_LC_13
	.quad	_L_1+762
	.quad	_div_pow2__div_pow2_LC_12
	.quad	_L_1+777
	.quad	_div_pow2__div_pow2_LC_11
	.quad	_L_1+792
	.quad	_div_pow2__div_pow2_LC_10
	.quad	_L_1+807
	.quad	_div_pow2__div_pow2_LC_9
	.quad	_L_1+822
	.quad	_div_pow2__div_pow2_LC_8
	.quad	_L_1+836
	.quad	_div_pow2__div_pow2_LC_7
	.quad	_L_1+850
	.quad	_div_pow2__div_pow2_LC_6
	.quad	_L_1+864
	.quad	_div_pow2__div_pow2_LC_5
	.quad	_L_1+878
	.quad	_div_pow2__div_pow2_LC_4
	.quad	_L_1+892
	.quad	_div_pow2__div_pow2_LC_3
	.quad	_L_1+906
	.quad	_div_pow2__div_pow2_LC_2
	.quad	_L_1+920
	.quad	_div_pow2__div_pow2_LC_1
	.quad	_L_1+934
	.quad	_div_pow2__div_pow2_LC_0
	.quad	_L_1+948
	.quad	_div_pow2__div_pow2_L_63
	.quad	_L_1+962
	.quad	_div_pow2__div_pow2_L_62
	.quad	_L_1+976
	.quad	_div_pow2__div_pow2_L_61
	.quad	_L_1+990
	.quad	_div_pow2__div_pow2_L_60
	.quad	_L_1+1004
	.quad	_div_pow2__div_pow2_L_59
	.quad	_L_1+1018
	.quad	_div_pow2__div_pow2_L_58
	.quad	_L_1+1032
	.quad	_div_pow2__div_pow2_L_57
	.quad	_L_1+1046
	.quad	_div_pow2__div_pow2_L_56
	.quad	_L_1+1060
	.quad	_div_pow2__div_pow2_L_55
	.quad	_L_1+1074
	.quad	_div_pow2__div_pow2_L_54
	.quad	_L_1+1088
	.quad	_div_pow2__div_pow2_L_53
	.quad	_L_1+1102
	.quad	_div_pow2__div_pow2_L_52
	.quad	_L_1+1116
	.quad	_div_pow2__div_pow2_L_51
	.quad	_L_1+1130
	.quad	_div_pow2__div_pow2_L_50
	.quad	_L_1+1144
	.quad	_div_pow2__div_pow2_L_49
	.quad	_L_1+1158
	.quad	_div_pow2__div_pow2_L_48
	.quad	_L_1+1172
	.quad	_div_pow2__div_pow2_L_47
	.quad	_L_1+1186
	.quad	_div_pow2__div_pow2_L_46
	.quad	_L_1+1200
	.quad	_div_pow2__div_pow2_L_45
	.quad	_L_1+1214
	.quad	_div_pow2__div_pow2_L_44
	.quad	_L_1+1228
	.quad	_div_pow2__div_pow2_L_43
	.quad	_L_1+1242
	.quad	_div_pow2__div_pow2_L_42
	.quad	_L_1+1256
	.quad	_div_pow2__div_pow2_L_41
	.quad	_L_1+1270
	.quad	_div_pow2__div_pow2_L_40
	.quad	_L_1+1284
	.quad	_div_pow2__div_pow2_L_39
	.quad	_L_1+1298
	.quad	_div_pow2__div_pow2_L_38
	.quad	_L_1+1312
	.quad	_div_pow2__div_pow2_L_37
	.quad	_L_1+1326
	.quad	_div_pow2__div_pow2_L_36
	.quad	_L_1+1340
	.quad	_div_pow2__div_pow2_L_35
	.quad	_L_1+1354
	.quad	_div_pow2__div_pow2_L_34
	.quad	_L_1+1368
	.quad	_div_pow2__div_pow2_L_33
	.quad	_L_1+1382
	.quad	_div_pow2__div_pow2_L_32
	.quad	_L_1+1396
	.quad	_div_pow2__div_pow2_L_31
	.quad	_L_1+1410
	.quad	_div_pow2__div_pow2_L_30
	.quad	_L_1+1424
	.quad	_div_pow2__div_pow2_L_29
	.quad	_L_1+1438
	.quad	_div_pow2__div_pow2_L_28
	.quad	_L_1+1452
	.quad	_div_pow2__div_pow2_L_27
	.quad	_L_1+1466
	.quad	_div_pow2__div_pow2_L_26
	.quad	_L_1+1480
	.quad	_div_pow2__div_pow2_L_25
	.quad	_L_1+1494
	.quad	_div_pow2__div_pow2_L_24
	.quad	_L_1+1508
	.quad	_div_pow2__div_pow2_L_23
	.quad	_L_1+1522
	.quad	_div_pow2__div_pow2_L_22
	.quad	_L_1+1536
	.quad	_div_pow2__div_pow2_L_21
	.quad	_L_1+1550
	.quad	_div_pow2__div_pow2_L_20
	.quad	_L_1+1564
	.quad	_div_pow2__div_pow2_L_19
	.quad	_L_1+1578
	.quad	_div_pow2__div_pow2_L_18
	.quad	_L_1+1592
	.quad	_div_pow2__div_pow2_L_17
	.quad	_L_1+1606
	.quad	_div_pow2__div_pow2_L_16
	.quad	_L_1+1620
	.quad	_div_pow2__div_pow2_L_15
	.quad	_L_1+1634
	.quad	_div_pow2__div_pow2_L_14
	.quad	_L_1+1648
	.quad	_div_pow2__div_pow2_L_13
	.quad	_L_1+1662
	.quad	_div_pow2__div_pow2_L_12
	.quad	_L_1+1676
	.quad	_div_pow2__div_pow2_L_11
	.quad	_L_1+1690
	.quad	_div_pow2__div_pow2_L_10
	.quad	_L_1+1704
	.quad	_div_pow2__div_pow2_L_9
	.quad	_L_1+1718
	.quad	_div_pow2__div_pow2_L_8
	.quad	_L_1+1731
	.quad	_div_pow2__div_pow2_L_7
	.quad	_L_1+1744
	.quad	_div_pow2__div_pow2_L_6
	.quad	_L_1+1757
	.quad	_div_pow2__div_pow2_L_5
	.quad	_L_1+1770
	.quad	_div_pow2__div_pow2_L_4
	.quad	_L_1+1783
	.quad	_div_pow2__div_pow2_L_3
	.quad	_L_1+1796
	.quad	_div_pow2__div_pow2_L_2
	.quad	_L_1+1809
	.quad	_div_pow2__div_pow2_L_1
	.quad	_L_1+1822
	.quad	_div_pow2__div_pow2_L_0
	.quad	_L_1+1835
	.quad	_div_pow2__div_pow2_C_63
	.quad	_L_1+1848
	.quad	_div_pow2__div_pow2_C_62
	.quad	_L_1+1862
	.quad	_div_pow2__div_pow2_C_61
	.quad	_L_1+1876
	.quad	_div_pow2__div_pow2_C_60
	.quad	_L_1+1890
	.quad	_div_pow2__div_pow2_C_59
	.quad	_L_1+1904
	.quad	_div_pow2__div_pow2_C_58
	.quad	_L_1+1918
	.quad	_div_pow2__div_pow2_C_57
	.quad	_L_1+1932
	.quad	_div_pow2__div_pow2_C_56
	.quad	_L_1+1946
	.quad	_div_pow2__div_pow2_C_55
	.quad	_L_1+1960
	.quad	_div_pow2__div_pow2_C_54
	.quad	_L_1+1974
	.quad	_div_pow2__div_pow2_C_53
	.quad	_L_1+1988
	.quad	_div_pow2__div_pow2_C_52
	.quad	_L_1+2002
	.quad	_div_pow2__div_pow2_C_51
	.quad	_L_1+2016
	.quad	_div_pow2__div_pow2_C_50
	.quad	_L_1+2030
	.quad	_div_pow2__div_pow2_C_49
	.quad	_L_1+2044
	.quad	_div_pow2__div_pow2_C_48
	.quad	_L_1+2058
	.quad	_div_pow2__div_pow2_C_47
	.quad	_L_1+2072
	.quad	_div_pow2__div_pow2_C_46
	.quad	_L_1+2086
	.quad	_div_pow2__div_pow2_C_45
	.quad	_L_1+2100
	.quad	_div_pow2__div_pow2_C_44
	.quad	_L_1+2114
	.quad	_div_pow2__div_pow2_C_43
	.quad	_L_1+2128
	.quad	_div_pow2__div_pow2_C_42
	.quad	_L_1+2142
	.quad	_div_pow2__div_pow2_C_41
	.quad	_L_1+2156
	.quad	_div_pow2__div_pow2_C_40
	.quad	_L_1+2170
	.quad	_div_pow2__div_pow2_C_39
	.quad	_L_1+2184
	.quad	_div_pow2__div_pow2_C_38
	.quad	_L_1+2198
	.quad	_div_pow2__div_pow2_C_37
	.quad	_L_1+2212
	.quad	_div_pow2__div_pow2_C_36
	.quad	_L_1+2226
	.quad	_div_pow2__div_pow2_C_35
	.quad	_L_1+2240
	.quad	_div_pow2__div_pow2_C_34
	.quad	_L_1+2254
	.quad	_div_pow2__div_pow2_C_33
	.quad	_L_1+2268
	.quad	_div_pow2__div_pow2_C_32
	.quad	_L_1+2282
	.quad	_div_pow2__div_pow2_C_31
	.quad	_L_1+2296
	.quad	_div_pow2__div_pow2_C_30
	.quad	_L_1+2310
	.quad	_div_pow2__div_pow2_C_29
	.quad	_L_1+2324
	.quad	_div_pow2__div_pow2_C_28
	.quad	_L_1+2338
	.quad	_div_pow2__div_pow2_C_27
	.quad	_L_1+2352
	.quad	_div_pow2__div_pow2_C_26
	.quad	_L_1+2366
	.quad	_div_pow2__div_pow2_C_25
	.quad	_L_1+2380
	.quad	_div_pow2__div_pow2_C_24
	.quad	_L_1+2394
	.quad	_div_pow2__div_pow2_C_23
	.quad	_L_1+2408
	.quad	_div_pow2__div_pow2_C_22
	.quad	_L_1+2422
	.quad	_div_pow2__div_pow2_C_21
	.quad	_L_1+2436
	.quad	_div_pow2__div_pow2_C_20
	.quad	_L_1+2450
	.quad	_div_pow2__div_pow2_C_19
	.quad	_L_1+2464
	.quad	_div_pow2__div_pow2_C_18
	.quad	_L_1+2478
	.quad	_div_pow2__div_pow2_C_17
	.quad	_L_1+2492
	.quad	_div_pow2__div_pow2_C_16
	.quad	_L_1+2506
	.quad	_div_pow2__div_pow2_C_15
	.quad	_L_1+2520
	.quad	_div_pow2__div_pow2_C_14
	.quad	_L_1+2534
	.quad	_div_pow2__div_pow2_C_13
	.quad	_L_1+2548
	.quad	_div_pow2__div_pow2_C_12
	.quad	_L_1+2562
	.quad	_div_pow2__div_pow2_C_11
	.quad	_L_1+2576
	.quad	_div_pow2__div_pow2_C_10
	.quad	_L_1+2590
	.quad	_div_pow2__div_pow2_C_9
	.quad	_L_1+2604
	.quad	_div_pow2__div_pow2_C_8
	.quad	_L_1+2617
	.quad	_div_pow2__div_pow2_C_7
	.quad	_L_1+2630
	.quad	_div_pow2__div_pow2_C_6
	.quad	_L_1+2643
	.quad	_div_pow2__div_pow2_C_5
	.quad	_L_1+2656
	.quad	_div_pow2__div_pow2_C_4
	.quad	_L_1+2669
	.quad	_div_pow2__div_pow2_C_3
	.quad	_L_1+2682
	.quad	_div_pow2__div_pow2_C_2
	.quad	_L_1+2695
	.quad	_div_pow2__div_pow2_C_1
	.quad	_L_1+2708
	.quad	_div_pow2__div_pow2_C_0
	.quad	_L_1+2721
	.quad	_div_pow2__div_pow2_I_63
	.quad	_L_1+2734
	.quad	_div_pow2__div_pow2_I_62
	.quad	_L_1+2748
	.quad	_div_pow2__div_pow2_I_61
	.quad	_L_1+2762
	.quad	_div_pow2__div_pow2_I_60
	.quad	_L_1+2776
	.quad	_div_pow2__div_pow2_I_59
	.quad	_L_1+2790
	.quad	_div_pow2__div_pow2_I_58
	.quad	_L_1+2804
	.quad	_div_pow2__div_pow2_I_57
	.quad	_L_1+2818
	.quad	_div_pow2__div_pow2_I_56
	.quad	_L_1+2832
	.quad	_div_pow2__div_pow2_I_55
	.quad	_L_1+2846
	.quad	_div_pow2__div_pow2_I_54
	.quad	_L_1+2860
	.quad	_div_pow2__div_pow2_I_53
	.quad	_L_1+2874
	.quad	_div_pow2__div_pow2_I_52
	.quad	_L_1+2888
	.quad	_div_pow2__div_pow2_I_51
	.quad	_L_1+2902
	.quad	_div_pow2__div_pow2_I_50
	.quad	_L_1+2916
	.quad	_div_pow2__div_pow2_I_49
	.quad	_L_1+2930
	.quad	_div_pow2__div_pow2_I_48
	.quad	_L_1+2944
	.quad	_div_pow2__div_pow2_I_47
	.quad	_L_1+2958
	.quad	_div_pow2__div_pow2_I_46
	.quad	_L_1+2972
	.quad	_div_pow2__div_pow2_I_45
	.quad	_L_1+2986
	.quad	_div_pow2__div_pow2_I_44
	.quad	_L_1+3000
	.quad	_div_pow2__div_pow2_I_43
	.quad	_L_1+3014
	.quad	_div_pow2__div_pow2_I_42
	.quad	_L_1+3028
	.quad	_div_pow2__div_pow2_I_41
	.quad	_L_1+3042
	.quad	_div_pow2__div_pow2_I_40
	.quad	_L_1+3056
	.quad	_div_pow2__div_pow2_I_39
	.quad	_L_1+3070
	.quad	_div_pow2__div_pow2_I_38
	.quad	_L_1+3084
	.quad	_div_pow2__div_pow2_I_37
	.quad	_L_1+3098
	.quad	_div_pow2__div_pow2_I_36
	.quad	_L_1+3112
	.quad	_div_pow2__div_pow2_I_35
	.quad	_L_1+3126
	.quad	_div_pow2__div_pow2_I_34
	.quad	_L_1+3140
	.quad	_div_pow2__div_pow2_I_33
	.quad	_L_1+3154
	.quad	_div_pow2__div_pow2_I_32
	.quad	_L_1+3168
	.quad	_div_pow2__div_pow2_I_31
	.quad	_L_1+3182
	.quad	_div_pow2__div_pow2_I_30
	.quad	_L_1+3196
	.quad	_div_pow2__div_pow2_I_29
	.quad	_L_1+3210
	.quad	_div_pow2__div_pow2_I_28
	.quad	_L_1+3224
	.quad	_div_pow2__div_pow2_I_27
	.quad	_L_1+3238
	.quad	_div_pow2__div_pow2_I_26
	.quad	_L_1+3252
	.quad	_div_pow2__div_pow2_I_25
	.quad	_L_1+3266
	.quad	_div_pow2__div_pow2_I_24
	.quad	_L_1+3280
	.quad	_div_pow2__div_pow2_I_23
	.quad	_L_1+3294
	.quad	_div_pow2__div_pow2_I_22
	.quad	_L_1+3308
	.quad	_div_pow2__div_pow2_I_21
	.quad	_L_1+3322
	.quad	_div_pow2__div_pow2_I_20
	.quad	_L_1+3336
	.quad	_div_pow2__div_pow2_I_19
	.quad	_L_1+3350
	.quad	_div_pow2__div_pow2_I_18
	.quad	_L_1+3364
	.quad	_div_pow2__div_pow2_I_17
	.quad	_L_1+3378
	.quad	_div_pow2__div_pow2_I_16
	.quad	_L_1+3392
	.quad	_div_pow2__div_pow2_I_15
	.quad	_L_1+3406
	.quad	_div_pow2__div_pow2_I_14
	.quad	_L_1+3420
	.quad	_div_pow2__div_pow2_I_13
	.quad	_L_1+3434
	.quad	_div_pow2__div_pow2_I_12
	.quad	_L_1+3448
	.quad	_div_pow2__div_pow2_I_11
	.quad	_L_1+3462
	.quad	_div_pow2__div_pow2_I_10
	.quad	_L_1+3476
	.quad	_div_pow2__div_pow2_I_9
	.quad	_L_1+3490
	.quad	_div_pow2__div_pow2_I_8
	.quad	_L_1+3503
	.quad	_div_pow2__div_pow2_I_7
	.quad	_L_1+3516
	.quad	_div_pow2__div_pow2_I_6
	.quad	_L_1+3529
	.quad	_div_pow2__div_pow2_I_5
	.quad	_L_1+3542
	.quad	_div_pow2__div_pow2_I_4
	.quad	_L_1+3555
	.quad	_div_pow2__div_pow2_I_3
	.quad	_L_1+3568
	.quad	_div_pow2__div_pow2_I_2
	.quad	_L_1+3581
	.quad	_div_pow2__div_pow2_I_1
	.quad	_L_1+3594
	.quad	_div_pow2__div_pow2_I_0
	.quad	_L_1+3607
	.quad	_div_pow2__div_pow2_u64_63
	.quad	_L_1+3620
	.quad	_div_pow2__div_pow2_u64_62
	.quad	_L_1+3636
	.quad	_div_pow2__div_pow2_u64_61
	.quad	_L_1+3652
	.quad	_div_pow2__div_pow2_u64_60
	.quad	_L_1+3668
	.quad	_div_pow2__div_pow2_u64_59
	.quad	_L_1+3684
	.quad	_div_pow2__div_pow2_u64_58
	.quad	_L_1+3700
	.quad	_div_pow2__div_pow2_u64_57
	.quad	_L_1+3716
	.quad	_div_pow2__div_pow2_u64_56
	.quad	_L_1+3732
	.quad	_div_pow2__div_pow2_u64_55
	.quad	_L_1+3748
	.quad	_div_pow2__div_pow2_u64_54
	.quad	_L_1+3764
	.quad	_div_pow2__div_pow2_u64_53
	.quad	_L_1+3780
	.quad	_div_pow2__div_pow2_u64_52
	.quad	_L_1+3796
	.quad	_div_pow2__div_pow2_u64_51
	.quad	_L_1+3812
	.quad	_div_pow2__div_pow2_u64_50
	.quad	_L_1+3828
	.quad	_div_pow2__div_pow2_u64_49
	.quad	_L_1+3844
	.quad	_div_pow2__div_pow2_u64_48
	.quad	_L_1+3860
	.quad	_div_pow2__div_pow2_u64_47
	.quad	_L_1+3876
	.quad	_div_pow2__div_pow2_u64_46
	.quad	_L_1+3892
	.quad	_div_pow2__div_pow2_u64_45
	.quad	_L_1+3908
	.quad	_div_pow2__div_pow2_u64_44
	.quad	_L_1+3924
	.quad	_div_pow2__div_pow2_u64_43
	.quad	_L_1+3940
	.quad	_div_pow2__div_pow2_u64_42
	.quad	_L_1+3956
	.quad	_div_pow2__div_pow2_u64_41
	.quad	_L_1+3972
	.quad	_div_pow2__div_pow2_u64_40
	.quad	_L_1+3988
	.quad	_div_pow2__div_pow2_u64_39
	.quad	_L_1+4004
	.quad	_div_pow2__div_pow2_u64_38
	.quad	_L_1+4020
	.quad	_div_pow2__div_pow2_u64_37
	.quad	_L_1+4036
	.quad	_div_pow2__div_pow2_u64_36
	.quad	_L_1+4052
	.quad	_div_pow2__div_pow2_u64_35
	.quad	_L_1+4068
	.quad	_div_pow2__div_pow2_u64_34
	.quad	_L_1+4084
	.quad	_div_pow2__div_pow2_u64_33
	.quad	_L_1+4100
	.quad	_div_pow2__div_pow2_u64_32
	.quad	_L_1+4116
	.quad	_div_pow2__div_pow2_u64_31
	.quad	_L_1+4132
	.quad	_div_pow2__div_pow2_u64_30
	.quad	_L_1+4148
	.quad	_div_pow2__div_pow2_u64_29
	.quad	_L_1+4164
	.quad	_div_pow2__div_pow2_u64_28
	.quad	_L_1+4180
	.quad	_div_pow2__div_pow2_u64_27
	.quad	_L_1+4196
	.quad	_div_pow2__div_pow2_u64_26
	.quad	_L_1+4212
	.quad	_div_pow2__div_pow2_u64_25
	.quad	_L_1+4228
	.quad	_div_pow2__div_pow2_u64_24
	.quad	_L_1+4244
	.quad	_div_pow2__div_pow2_u64_23
	.quad	_L_1+4260
	.quad	_div_pow2__div_pow2_u64_22
	.quad	_L_1+4276
	.quad	_div_pow2__div_pow2_u64_21
	.quad	_L_1+4292
	.quad	_div_pow2__div_pow2_u64_20
	.quad	_L_1+4308
	.quad	_div_pow2__div_pow2_u64_19
	.quad	_L_1+4324
	.quad	_div_pow2__div_pow2_u64_18
	.quad	_L_1+4340
	.quad	_div_pow2__div_pow2_u64_17
	.quad	_L_1+4356
	.quad	_div_pow2__div_pow2_u64_16
	.quad	_L_1+4372
	.quad	_div_pow2__div_pow2_u64_15
	.quad	_L_1+4388
	.quad	_div_pow2__div_pow2_u64_14
	.quad	_L_1+4404
	.quad	_div_pow2__div_pow2_u64_13
	.quad	_L_1+4420
	.quad	_div_pow2__div_pow2_u64_12
	.quad	_L_1+4436
	.quad	_div_pow2__div_pow2_u64_11
	.quad	_L_1+4452
	.quad	_div_pow2__div_pow2_u64_10
	.quad	_L_1+4468
	.quad	_div_pow2__div_pow2_u64_9
	.quad	_L_1+4484
	.quad	_div_pow2__div_pow2_u64_8
	.quad	_L_1+4499
	.quad	_div_pow2__div_pow2_u64_7
	.quad	_L_1+4514
	.quad	_div_pow2__div_pow2_u64_6
	.quad	_L_1+4529
	.quad	_div_pow2__div_pow2_u64_5
	.quad	_L_1+4544
	.quad	_div_pow2__div_pow2_u64_4
	.quad	_L_1+4559
	.quad	_div_pow2__div_pow2_u64_3
	.quad	_L_1+4574
	.quad	_div_pow2__div_pow2_u64_2
	.quad	_L_1+4589
	.quad	_div_pow2__div_pow2_u64_1
	.quad	_L_1+4604
	.quad	_div_pow2__div_pow2_u64_0
	.quad	_L_1+4619
	.quad	_div_pow2__div_pow2_i64_63
	.quad	_L_1+4634
	.quad	_div_pow2__div_pow2_i64_62
	.quad	_L_1+4650
	.quad	_div_pow2__div_pow2_i64_61
	.quad	_L_1+4666
	.quad	_div_pow2__div_pow2_i64_60
	.quad	_L_1+4682
	.quad	_div_pow2__div_pow2_i64_59
	.quad	_L_1+4698
	.quad	_div_pow2__div_pow2_i64_58
	.quad	_L_1+4714
	.quad	_div_pow2__div_pow2_i64_57
	.quad	_L_1+4730
	.quad	_div_pow2__div_pow2_i64_56
	.quad	_L_1+4746
	.quad	_div_pow2__div_pow2_i64_55
	.quad	_L_1+4762
	.quad	_div_pow2__div_pow2_i64_54
	.quad	_L_1+4778
	.quad	_div_pow2__div_pow2_i64_53
	.quad	_L_1+4794
	.quad	_div_pow2__div_pow2_i64_52
	.quad	_L_1+4810
	.quad	_div_pow2__div_pow2_i64_51
	.quad	_L_1+4826
	.quad	_div_pow2__div_pow2_i64_50
	.quad	_L_1+4842
	.quad	_div_pow2__div_pow2_i64_49
	.quad	_L_1+4858
	.quad	_div_pow2__div_pow2_i64_48
	.quad	_L_1+4874
	.quad	_div_pow2__div_pow2_i64_47
	.quad	_L_1+4890
	.quad	_div_pow2__div_pow2_i64_46
	.quad	_L_1+4906
	.quad	_div_pow2__div_pow2_i64_45
	.quad	_L_1+4922
	.quad	_div_pow2__div_pow2_i64_44
	.quad	_L_1+4938
	.quad	_div_pow2__div_pow2_i64_43
	.quad	_L_1+4954
	.quad	_div_pow2__div_pow2_i64_42
	.quad	_L_1+4970
	.quad	_div_pow2__div_pow2_i64_41
	.quad	_L_1+4986
	.quad	_div_pow2__div_pow2_i64_40
	.quad	_L_1+5002
	.quad	_div_pow2__div_pow2_i64_39
	.quad	_L_1+5018
	.quad	_div_pow2__div_pow2_i64_38
	.quad	_L_1+5034
	.quad	_div_pow2__div_pow2_i64_37
	.quad	_L_1+5050
	.quad	_div_pow2__div_pow2_i64_36
	.quad	_L_1+5066
	.quad	_div_pow2__div_pow2_i64_35
	.quad	_L_1+5082
	.quad	_div_pow2__div_pow2_i64_34
	.quad	_L_1+5098
	.quad	_div_pow2__div_pow2_i64_33
	.quad	_L_1+5114
	.quad	_div_pow2__div_pow2_i64_32
	.quad	_L_1+5130
	.quad	_div_pow2__div_pow2_i64_31
	.quad	_L_1+5146
	.quad	_div_pow2__div_pow2_i64_30
	.quad	_L_1+5162
	.quad	_div_pow2__div_pow2_i64_29
	.quad	_L_1+5178
	.quad	_div_pow2__div_pow2_i64_28
	.quad	_L_1+5194
	.quad	_div_pow2__div_pow2_i64_27
	.quad	_L_1+5210
	.quad	_div_pow2__div_pow2_i64_26
	.quad	_L_1+5226
	.quad	_div_pow2__div_pow2_i64_25
	.quad	_L_1+5242
	.quad	_div_pow2__div_pow2_i64_24
	.quad	_L_1+5258
	.quad	_div_pow2__div_pow2_i64_23
	.quad	_L_1+5274
	.quad	_div_pow2__div_pow2_i64_22
	.quad	_L_1+5290
	.quad	_div_pow2__div_pow2_i64_21
	.quad	_L_1+5306
	.quad	_div_pow2__div_pow2_i64_20
	.quad	_L_1+5322
	.quad	_div_pow2__div_pow2_i64_19
	.quad	_L_1+5338
	.quad	_div_pow2__div_pow2_i64_18
	.quad	_L_1+5354
	.quad	_div_pow2__div_pow2_i64_17
	.quad	_L_1+5370
	.quad	_div_pow2__div_pow2_i64_16
	.quad	_L_1+5386
	.quad	_div_pow2__div_pow2_i64_15
	.quad	_L_1+5402
	.quad	_div_pow2__div_pow2_i64_14
	.quad	_L_1+5418
	.quad	_div_pow2__div_pow2_i64_13
	.quad	_L_1+5434
	.quad	_div_pow2__div_pow2_i64_12
	.quad	_L_1+5450
	.quad	_div_pow2__div_pow2_i64_11
	.quad	_L_1+5466
	.quad	_div_pow2__div_pow2_i64_10
	.quad	_L_1+5482
	.quad	_div_pow2__div_pow2_i64_9
	.quad	_L_1+5498
	.quad	_div_pow2__div_pow2_i64_8
	.quad	_L_1+5513
	.quad	_div_pow2__div_pow2_i64_7
	.quad	_L_1+5528
	.quad	_div_pow2__div_pow2_i64_6
	.quad	_L_1+5543
	.quad	_div_pow2__div_pow2_i64_5
	.quad	_L_1+5558
	.quad	_div_pow2__div_pow2_i64_4
	.quad	_L_1+5573
	.quad	_div_pow2__div_pow2_i64_3
	.quad	_L_1+5588
	.quad	_div_pow2__div_pow2_i64_2
	.quad	_L_1+5603
	.quad	_div_pow2__div_pow2_i64_1
	.quad	_L_1+5618
	.quad	_div_pow2__div_pow2_i64_0
	.quad	_L_1+5633
	.quad	_div_pow2__div_pow2_u32_31
	.quad	_L_1+5648
	.quad	_div_pow2__div_pow2_u32_30
	.quad	_L_1+5664
	.quad	_div_pow2__div_pow2_u32_29
	.quad	_L_1+5680
	.quad	_div_pow2__div_pow2_u32_28
	.quad	_L_1+5696
	.quad	_div_pow2__div_pow2_u32_27
	.quad	_L_1+5712
	.quad	_div_pow2__div_pow2_u32_26
	.quad	_L_1+5728
	.quad	_div_pow2__div_pow2_u32_25
	.quad	_L_1+5744
	.quad	_div_pow2__div_pow2_u32_24
	.quad	_L_1+5760
	.quad	_div_pow2__div_pow2_u32_23
	.quad	_L_1+5776
	.quad	_div_pow2__div_pow2_u32_22
	.quad	_L_1+5792
	.quad	_div_pow2__div_pow2_u32_21
	.quad	_L_1+5808
	.quad	_div_pow2__div_pow2_u32_20
	.quad	_L_1+5824
	.quad	_div_pow2__div_pow2_u32_19
	.quad	_L_1+5840
	.quad	_div_pow2__div_pow2_u32_18
	.quad	_L_1+5856
	.quad	_div_pow2__div_pow2_u32_17
	.quad	_L_1+5872
	.quad	_div_pow2__div_pow2_u32_16
	.quad	_L_1+5888
	.quad	_div_pow2__div_pow2_u32_15
	.quad	_L_1+5904
	.quad	_div_pow2__div_pow2_u32_14
	.quad	_L_1+5920
	.quad	_div_pow2__div_pow2_u32_13
	.quad	_L_1+5936
	.quad	_div_pow2__div_pow2_u32_12
	.quad	_L_1+5952
	.quad	_div_pow2__div_pow2_u32_11
	.quad	_L_1+5968
	.quad	_div_pow2__div_pow2_u32_10
	.quad	_L_1+5984
	.quad	_div_pow2__div_pow2_u32_9
	.quad	_L_1+6000
	.quad	_div_pow2__div_pow2_u32_8
	.quad	_L_1+6015
	.quad	_div_pow2__div_pow2_u32_7
	.quad	_L_1+6030
	.quad	_div_pow2__div_pow2_u32_6
	.quad	_L_1+6045
	.quad	_div_pow2__div_pow2_u32_5
	.quad	_L_1+6060
	.quad	_div_pow2__div_pow2_u32_4
	.quad	_L_1+6075
	.quad	_div_pow2__div_pow2_u32_3
	.quad	_L_1+6090
	.quad	_div_pow2__div_pow2_u32_2
	.quad	_L_1+6105
	.quad	_div_pow2__div_pow2_u32_1
	.quad	_L_1+6120
	.quad	_div_pow2__div_pow2_u32_0
	.quad	_L_1+6135
	.quad	_div_pow2__div_pow2_i32_31
	.quad	_L_1+6150
	.quad	_div_pow2__div_pow2_i32_30
	.quad	_L_1+6166
	.quad	_div_pow2__div_pow2_i32_29
	.quad	_L_1+6182
	.quad	_div_pow2__div_pow2_i32_28
	.quad	_L_1+6198
	.quad	_div_pow2__div_pow2_i32_27
	.quad	_L_1+6214
	.quad	_div_pow2__div_pow2_i32_26
	.quad	_L_1+6230
	.quad	_div_pow2__div_pow2_i32_25
	.quad	_L_1+6246
	.quad	_div_pow2__div_pow2_i32_24
	.quad	_L_1+6262
	.quad	_div_pow2__div_pow2_i32_23
	.quad	_L_1+6278
	.quad	_div_pow2__div_pow2_i32_22
	.quad	_L_1+6294
	.quad	_div_pow2__div_pow2_i32_21
	.quad	_L_1+6310
	.quad	_div_pow2__div_pow2_i32_20
	.quad	_L_1+6326
	.quad	_div_pow2__div_pow2_i32_19
	.quad	_L_1+6342
	.quad	_div_pow2__div_pow2_i32_18
	.quad	_L_1+6358
	.quad	_div_pow2__div_pow2_i32_17
	.quad	_L_1+6374
	.quad	_div_pow2__div_pow2_i32_16
	.quad	_L_1+6390
	.quad	_div_pow2__div_pow2_i32_15
	.quad	_L_1+6406
	.quad	_div_pow2__div_pow2_i32_14
	.quad	_L_1+6422
	.quad	_div_pow2__div_pow2_i32_13
	.quad	_L_1+6438
	.quad	_div_pow2__div_pow2_i32_12
	.quad	_L_1+6454
	.quad	_div_pow2__div_pow2_i32_11
	.quad	_L_1+6470
	.quad	_div_pow2__div_pow2_i32_10
	.quad	_L_1+6486
	.quad	_div_pow2__div_pow2_i32_9
	.quad	_L_1+6502
	.quad	_div_pow2__div_pow2_i32_8
	.quad	_L_1+6517
	.quad	_div_pow2__div_pow2_i32_7
	.quad	_L_1+6532
	.quad	_div_pow2__div_pow2_i32_6
	.quad	_L_1+6547
	.quad	_div_pow2__div_pow2_i32_5
	.quad	_L_1+6562
	.quad	_div_pow2__div_pow2_i32_4
	.quad	_L_1+6577
	.quad	_div_pow2__div_pow2_i32_3
	.quad	_L_1+6592
	.quad	_div_pow2__div_pow2_i32_2
	.quad	_L_1+6607
	.quad	_div_pow2__div_pow2_i32_1
	.quad	_L_1+6622
	.quad	_div_pow2__div_pow2_i32_0
	.quad	_L_1+6637
	.space 8
	.ascii "../AMD64_DARWIN/div_pow2.m3"
	.space 5
	.data
	.align 5
_MM_div_pow2:
	.quad	_L_1+13848
	.space 32
	.quad	_L_1+6656
	.space 24
	.quad	_MM_div_pow2+208
	.space 8
	.quad	_div_pow2_M3
	.quad	3
	.quad	913
	.quad	914
	.long	1147468448
	.byte	-107
	.space 3
	.quad	918
	.byte	-105
	.space 3
	.long	920
	.quad	921
	.word	922
	.space 6
	.quad	923
	.long	1717986918
	.long	1082976102
	.quad	926
	.word	927
	.space 2
	.long	928
	.space 24
	.quad	_div_pow2_I3
	.quad	_MM_div_pow2+232
	.space 8
	.quad	_Long_I3
	.quad	_MM_div_pow2+256
	.space 8
	.quad	_Word_I3
	.quad	_MM_div_pow2+280
	.space 8
	.quad	_Cstdint_I3
	.quad	_MM_div_pow2+304
	.space 8
	.quad	_RTHooks_I3
	.space 8
	.subsections_via_symbols
