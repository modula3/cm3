	.text
.globl _divmod_pow2__div_pow2_i32_0
	.private_extern _divmod_pow2__div_pow2_i32_0
_divmod_pow2__div_pow2_i32_0:
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
.globl _divmod_pow2__mod_pow2_i32_0
	.private_extern _divmod_pow2__mod_pow2_i32_0
_divmod_pow2__mod_pow2_i32_0:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	$0, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i32_1
	.private_extern _divmod_pow2__div_pow2_i32_1
_divmod_pow2__div_pow2_i32_1:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	%rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L8
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L9
L8:
	movl	$1889, %edi
	call	__m3_fault
L9:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i32_1
	.private_extern _divmod_pow2__mod_pow2_i32_1
_divmod_pow2__mod_pow2_i32_1:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	andl	$1, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i32_2
	.private_extern _divmod_pow2__div_pow2_i32_2
_divmod_pow2__div_pow2_i32_2:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$2, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L14
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L15
L14:
	movl	$2017, %edi
	call	__m3_fault
L15:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i32_2
	.private_extern _divmod_pow2__mod_pow2_i32_2
_divmod_pow2__mod_pow2_i32_2:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	andl	$3, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i32_3
	.private_extern _divmod_pow2__div_pow2_i32_3
_divmod_pow2__div_pow2_i32_3:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$3, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L20
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L21
L20:
	movl	$2145, %edi
	call	__m3_fault
L21:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i32_3
	.private_extern _divmod_pow2__mod_pow2_i32_3
_divmod_pow2__mod_pow2_i32_3:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	andl	$7, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i32_4
	.private_extern _divmod_pow2__div_pow2_i32_4
_divmod_pow2__div_pow2_i32_4:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$4, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L26
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L27
L26:
	movl	$2273, %edi
	call	__m3_fault
L27:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i32_4
	.private_extern _divmod_pow2__mod_pow2_i32_4
_divmod_pow2__mod_pow2_i32_4:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	andl	$15, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i32_5
	.private_extern _divmod_pow2__div_pow2_i32_5
_divmod_pow2__div_pow2_i32_5:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$5, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L32
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L33
L32:
	movl	$2401, %edi
	call	__m3_fault
L33:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i32_5
	.private_extern _divmod_pow2__mod_pow2_i32_5
_divmod_pow2__mod_pow2_i32_5:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	andl	$31, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i32_6
	.private_extern _divmod_pow2__div_pow2_i32_6
_divmod_pow2__div_pow2_i32_6:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$6, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L38
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L39
L38:
	movl	$2529, %edi
	call	__m3_fault
L39:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i32_6
	.private_extern _divmod_pow2__mod_pow2_i32_6
_divmod_pow2__mod_pow2_i32_6:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	andl	$63, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i32_7
	.private_extern _divmod_pow2__div_pow2_i32_7
_divmod_pow2__div_pow2_i32_7:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$7, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L44
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L45
L44:
	movl	$2657, %edi
	call	__m3_fault
L45:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i32_7
	.private_extern _divmod_pow2__mod_pow2_i32_7
_divmod_pow2__mod_pow2_i32_7:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	andl	$127, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i32_8
	.private_extern _divmod_pow2__div_pow2_i32_8
_divmod_pow2__div_pow2_i32_8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$8, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L50
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L51
L50:
	movl	$2785, %edi
	call	__m3_fault
L51:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i32_8
	.private_extern _divmod_pow2__mod_pow2_i32_8
_divmod_pow2__mod_pow2_i32_8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	andl	$255, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i32_9
	.private_extern _divmod_pow2__div_pow2_i32_9
_divmod_pow2__div_pow2_i32_9:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$9, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L56
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L57
L56:
	movl	$2913, %edi
	call	__m3_fault
L57:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i32_9
	.private_extern _divmod_pow2__mod_pow2_i32_9
_divmod_pow2__mod_pow2_i32_9:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	andl	$511, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i32_10
	.private_extern _divmod_pow2__div_pow2_i32_10
_divmod_pow2__div_pow2_i32_10:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$10, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L62
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L63
L62:
	movl	$3041, %edi
	call	__m3_fault
L63:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i32_10
	.private_extern _divmod_pow2__mod_pow2_i32_10
_divmod_pow2__mod_pow2_i32_10:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	andl	$1023, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i32_11
	.private_extern _divmod_pow2__div_pow2_i32_11
_divmod_pow2__div_pow2_i32_11:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$11, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L68
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L69
L68:
	movl	$3169, %edi
	call	__m3_fault
L69:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i32_11
	.private_extern _divmod_pow2__mod_pow2_i32_11
_divmod_pow2__mod_pow2_i32_11:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	andl	$2047, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i32_12
	.private_extern _divmod_pow2__div_pow2_i32_12
_divmod_pow2__div_pow2_i32_12:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$12, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L74
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L75
L74:
	movl	$3297, %edi
	call	__m3_fault
L75:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i32_12
	.private_extern _divmod_pow2__mod_pow2_i32_12
_divmod_pow2__mod_pow2_i32_12:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	andl	$4095, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i32_13
	.private_extern _divmod_pow2__div_pow2_i32_13
_divmod_pow2__div_pow2_i32_13:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$13, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L80
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L81
L80:
	movl	$3425, %edi
	call	__m3_fault
L81:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i32_13
	.private_extern _divmod_pow2__mod_pow2_i32_13
_divmod_pow2__mod_pow2_i32_13:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	andl	$8191, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i32_14
	.private_extern _divmod_pow2__div_pow2_i32_14
_divmod_pow2__div_pow2_i32_14:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$14, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L86
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L87
L86:
	movl	$3553, %edi
	call	__m3_fault
L87:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i32_14
	.private_extern _divmod_pow2__mod_pow2_i32_14
_divmod_pow2__mod_pow2_i32_14:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	andl	$16383, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i32_15
	.private_extern _divmod_pow2__div_pow2_i32_15
_divmod_pow2__div_pow2_i32_15:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$15, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L92
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L93
L92:
	movl	$3681, %edi
	call	__m3_fault
L93:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i32_15
	.private_extern _divmod_pow2__mod_pow2_i32_15
_divmod_pow2__mod_pow2_i32_15:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	andl	$32767, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i32_16
	.private_extern _divmod_pow2__div_pow2_i32_16
_divmod_pow2__div_pow2_i32_16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$16, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L98
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L99
L98:
	movl	$3809, %edi
	call	__m3_fault
L99:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i32_16
	.private_extern _divmod_pow2__mod_pow2_i32_16
_divmod_pow2__mod_pow2_i32_16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	andl	$65535, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i32_17
	.private_extern _divmod_pow2__div_pow2_i32_17
_divmod_pow2__div_pow2_i32_17:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$17, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L104
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L105
L104:
	movl	$3937, %edi
	call	__m3_fault
L105:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i32_17
	.private_extern _divmod_pow2__mod_pow2_i32_17
_divmod_pow2__mod_pow2_i32_17:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	andl	$131071, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i32_18
	.private_extern _divmod_pow2__div_pow2_i32_18
_divmod_pow2__div_pow2_i32_18:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$18, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L110
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L111
L110:
	movl	$4065, %edi
	call	__m3_fault
L111:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i32_18
	.private_extern _divmod_pow2__mod_pow2_i32_18
_divmod_pow2__mod_pow2_i32_18:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	andl	$262143, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i32_19
	.private_extern _divmod_pow2__div_pow2_i32_19
_divmod_pow2__div_pow2_i32_19:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$19, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L116
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L117
L116:
	movl	$4193, %edi
	call	__m3_fault
L117:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i32_19
	.private_extern _divmod_pow2__mod_pow2_i32_19
_divmod_pow2__mod_pow2_i32_19:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	andl	$524287, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i32_20
	.private_extern _divmod_pow2__div_pow2_i32_20
_divmod_pow2__div_pow2_i32_20:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$20, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L122
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L123
L122:
	movl	$4321, %edi
	call	__m3_fault
L123:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i32_20
	.private_extern _divmod_pow2__mod_pow2_i32_20
_divmod_pow2__mod_pow2_i32_20:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	andl	$1048575, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i32_21
	.private_extern _divmod_pow2__div_pow2_i32_21
_divmod_pow2__div_pow2_i32_21:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$21, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L128
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L129
L128:
	movl	$4449, %edi
	call	__m3_fault
L129:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i32_21
	.private_extern _divmod_pow2__mod_pow2_i32_21
_divmod_pow2__mod_pow2_i32_21:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	andl	$2097151, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i32_22
	.private_extern _divmod_pow2__div_pow2_i32_22
_divmod_pow2__div_pow2_i32_22:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$22, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L134
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L135
L134:
	movl	$4577, %edi
	call	__m3_fault
L135:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i32_22
	.private_extern _divmod_pow2__mod_pow2_i32_22
_divmod_pow2__mod_pow2_i32_22:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	andl	$4194303, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i32_23
	.private_extern _divmod_pow2__div_pow2_i32_23
_divmod_pow2__div_pow2_i32_23:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$23, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L140
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L141
L140:
	movl	$4705, %edi
	call	__m3_fault
L141:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i32_23
	.private_extern _divmod_pow2__mod_pow2_i32_23
_divmod_pow2__mod_pow2_i32_23:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	andl	$8388607, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i32_24
	.private_extern _divmod_pow2__div_pow2_i32_24
_divmod_pow2__div_pow2_i32_24:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$24, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L146
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L147
L146:
	movl	$4833, %edi
	call	__m3_fault
L147:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i32_24
	.private_extern _divmod_pow2__mod_pow2_i32_24
_divmod_pow2__mod_pow2_i32_24:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	andl	$16777215, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i32_25
	.private_extern _divmod_pow2__div_pow2_i32_25
_divmod_pow2__div_pow2_i32_25:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$25, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L152
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L153
L152:
	movl	$4961, %edi
	call	__m3_fault
L153:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i32_25
	.private_extern _divmod_pow2__mod_pow2_i32_25
_divmod_pow2__mod_pow2_i32_25:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	andl	$33554431, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i32_26
	.private_extern _divmod_pow2__div_pow2_i32_26
_divmod_pow2__div_pow2_i32_26:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$26, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L158
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L159
L158:
	movl	$5089, %edi
	call	__m3_fault
L159:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i32_26
	.private_extern _divmod_pow2__mod_pow2_i32_26
_divmod_pow2__mod_pow2_i32_26:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	andl	$67108863, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i32_27
	.private_extern _divmod_pow2__div_pow2_i32_27
_divmod_pow2__div_pow2_i32_27:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$27, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L164
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L165
L164:
	movl	$5217, %edi
	call	__m3_fault
L165:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i32_27
	.private_extern _divmod_pow2__mod_pow2_i32_27
_divmod_pow2__mod_pow2_i32_27:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	andl	$134217727, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i32_28
	.private_extern _divmod_pow2__div_pow2_i32_28
_divmod_pow2__div_pow2_i32_28:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$28, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L170
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L171
L170:
	movl	$5345, %edi
	call	__m3_fault
L171:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i32_28
	.private_extern _divmod_pow2__mod_pow2_i32_28
_divmod_pow2__mod_pow2_i32_28:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	andl	$268435455, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i32_29
	.private_extern _divmod_pow2__div_pow2_i32_29
_divmod_pow2__div_pow2_i32_29:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$29, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L176
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L177
L176:
	movl	$5473, %edi
	call	__m3_fault
L177:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i32_29
	.private_extern _divmod_pow2__mod_pow2_i32_29
_divmod_pow2__mod_pow2_i32_29:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	andl	$536870911, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i32_30
	.private_extern _divmod_pow2__div_pow2_i32_30
_divmod_pow2__div_pow2_i32_30:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$30, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L182
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L183
L182:
	movl	$5601, %edi
	call	__m3_fault
L183:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i32_30
	.private_extern _divmod_pow2__mod_pow2_i32_30
_divmod_pow2__mod_pow2_i32_30:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	andl	$1073741823, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i32_31
	.private_extern _divmod_pow2__div_pow2_i32_31
_divmod_pow2__div_pow2_i32_31:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	sarq	$31, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L188
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L189
L188:
	movl	$5729, %edi
	call	__m3_fault
L189:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i32_31
	.private_extern _divmod_pow2__mod_pow2_i32_31
_divmod_pow2__mod_pow2_i32_31:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	andl	$2147483647, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u32_0
	.private_extern _divmod_pow2__div_pow2_u32_0
_divmod_pow2__div_pow2_u32_0:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L194
	cmpq	$2147483647, -16(%rbp)
	jle	L195
L194:
	movl	$5857, %edi
	call	__m3_fault
L195:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u32_0
	.private_extern _divmod_pow2__mod_pow2_u32_0
_divmod_pow2__mod_pow2_u32_0:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	$0, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u32_1
	.private_extern _divmod_pow2__div_pow2_u32_1
_divmod_pow2__div_pow2_u32_1:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	%rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L200
	cmpq	$2147483647, -16(%rbp)
	jle	L201
L200:
	movl	$5985, %edi
	call	__m3_fault
L201:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u32_1
	.private_extern _divmod_pow2__mod_pow2_u32_1
_divmod_pow2__mod_pow2_u32_1:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	andl	$1, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u32_2
	.private_extern _divmod_pow2__div_pow2_u32_2
_divmod_pow2__div_pow2_u32_2:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$2, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L206
	cmpq	$2147483647, -16(%rbp)
	jle	L207
L206:
	movl	$6113, %edi
	call	__m3_fault
L207:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u32_2
	.private_extern _divmod_pow2__mod_pow2_u32_2
_divmod_pow2__mod_pow2_u32_2:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	andl	$3, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u32_3
	.private_extern _divmod_pow2__div_pow2_u32_3
_divmod_pow2__div_pow2_u32_3:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$3, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L212
	cmpq	$2147483647, -16(%rbp)
	jle	L213
L212:
	movl	$6241, %edi
	call	__m3_fault
L213:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u32_3
	.private_extern _divmod_pow2__mod_pow2_u32_3
_divmod_pow2__mod_pow2_u32_3:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	andl	$7, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u32_4
	.private_extern _divmod_pow2__div_pow2_u32_4
_divmod_pow2__div_pow2_u32_4:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$4, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L218
	cmpq	$2147483647, -16(%rbp)
	jle	L219
L218:
	movl	$6369, %edi
	call	__m3_fault
L219:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u32_4
	.private_extern _divmod_pow2__mod_pow2_u32_4
_divmod_pow2__mod_pow2_u32_4:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	andl	$15, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u32_5
	.private_extern _divmod_pow2__div_pow2_u32_5
_divmod_pow2__div_pow2_u32_5:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$5, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L224
	cmpq	$2147483647, -16(%rbp)
	jle	L225
L224:
	movl	$6497, %edi
	call	__m3_fault
L225:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u32_5
	.private_extern _divmod_pow2__mod_pow2_u32_5
_divmod_pow2__mod_pow2_u32_5:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	andl	$31, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u32_6
	.private_extern _divmod_pow2__div_pow2_u32_6
_divmod_pow2__div_pow2_u32_6:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$6, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L230
	cmpq	$2147483647, -16(%rbp)
	jle	L231
L230:
	movl	$6625, %edi
	call	__m3_fault
L231:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u32_6
	.private_extern _divmod_pow2__mod_pow2_u32_6
_divmod_pow2__mod_pow2_u32_6:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	andl	$63, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u32_7
	.private_extern _divmod_pow2__div_pow2_u32_7
_divmod_pow2__div_pow2_u32_7:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$7, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L236
	cmpq	$2147483647, -16(%rbp)
	jle	L237
L236:
	movl	$6753, %edi
	call	__m3_fault
L237:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u32_7
	.private_extern _divmod_pow2__mod_pow2_u32_7
_divmod_pow2__mod_pow2_u32_7:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	andl	$127, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u32_8
	.private_extern _divmod_pow2__div_pow2_u32_8
_divmod_pow2__div_pow2_u32_8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$8, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L242
	cmpq	$2147483647, -16(%rbp)
	jle	L243
L242:
	movl	$6881, %edi
	call	__m3_fault
L243:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u32_8
	.private_extern _divmod_pow2__mod_pow2_u32_8
_divmod_pow2__mod_pow2_u32_8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	andl	$255, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u32_9
	.private_extern _divmod_pow2__div_pow2_u32_9
_divmod_pow2__div_pow2_u32_9:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$9, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L248
	cmpq	$2147483647, -16(%rbp)
	jle	L249
L248:
	movl	$7009, %edi
	call	__m3_fault
L249:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u32_9
	.private_extern _divmod_pow2__mod_pow2_u32_9
_divmod_pow2__mod_pow2_u32_9:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	andl	$511, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u32_10
	.private_extern _divmod_pow2__div_pow2_u32_10
_divmod_pow2__div_pow2_u32_10:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$10, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L254
	cmpq	$2147483647, -16(%rbp)
	jle	L255
L254:
	movl	$7137, %edi
	call	__m3_fault
L255:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u32_10
	.private_extern _divmod_pow2__mod_pow2_u32_10
_divmod_pow2__mod_pow2_u32_10:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	andl	$1023, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u32_11
	.private_extern _divmod_pow2__div_pow2_u32_11
_divmod_pow2__div_pow2_u32_11:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$11, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L260
	cmpq	$2147483647, -16(%rbp)
	jle	L261
L260:
	movl	$7265, %edi
	call	__m3_fault
L261:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u32_11
	.private_extern _divmod_pow2__mod_pow2_u32_11
_divmod_pow2__mod_pow2_u32_11:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	andl	$2047, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u32_12
	.private_extern _divmod_pow2__div_pow2_u32_12
_divmod_pow2__div_pow2_u32_12:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$12, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L266
	cmpq	$2147483647, -16(%rbp)
	jle	L267
L266:
	movl	$7393, %edi
	call	__m3_fault
L267:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u32_12
	.private_extern _divmod_pow2__mod_pow2_u32_12
_divmod_pow2__mod_pow2_u32_12:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	andl	$4095, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u32_13
	.private_extern _divmod_pow2__div_pow2_u32_13
_divmod_pow2__div_pow2_u32_13:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$13, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L272
	cmpq	$2147483647, -16(%rbp)
	jle	L273
L272:
	movl	$7521, %edi
	call	__m3_fault
L273:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u32_13
	.private_extern _divmod_pow2__mod_pow2_u32_13
_divmod_pow2__mod_pow2_u32_13:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	andl	$8191, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u32_14
	.private_extern _divmod_pow2__div_pow2_u32_14
_divmod_pow2__div_pow2_u32_14:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$14, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L278
	cmpq	$2147483647, -16(%rbp)
	jle	L279
L278:
	movl	$7649, %edi
	call	__m3_fault
L279:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u32_14
	.private_extern _divmod_pow2__mod_pow2_u32_14
_divmod_pow2__mod_pow2_u32_14:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	andl	$16383, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u32_15
	.private_extern _divmod_pow2__div_pow2_u32_15
_divmod_pow2__div_pow2_u32_15:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$15, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L284
	cmpq	$2147483647, -16(%rbp)
	jle	L285
L284:
	movl	$7777, %edi
	call	__m3_fault
L285:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u32_15
	.private_extern _divmod_pow2__mod_pow2_u32_15
_divmod_pow2__mod_pow2_u32_15:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	andl	$32767, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u32_16
	.private_extern _divmod_pow2__div_pow2_u32_16
_divmod_pow2__div_pow2_u32_16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$16, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L290
	cmpq	$2147483647, -16(%rbp)
	jle	L291
L290:
	movl	$7905, %edi
	call	__m3_fault
L291:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u32_16
	.private_extern _divmod_pow2__mod_pow2_u32_16
_divmod_pow2__mod_pow2_u32_16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	andl	$65535, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u32_17
	.private_extern _divmod_pow2__div_pow2_u32_17
_divmod_pow2__div_pow2_u32_17:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$17, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L296
	cmpq	$2147483647, -16(%rbp)
	jle	L297
L296:
	movl	$8033, %edi
	call	__m3_fault
L297:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u32_17
	.private_extern _divmod_pow2__mod_pow2_u32_17
_divmod_pow2__mod_pow2_u32_17:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	andl	$131071, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u32_18
	.private_extern _divmod_pow2__div_pow2_u32_18
_divmod_pow2__div_pow2_u32_18:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$18, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L302
	cmpq	$2147483647, -16(%rbp)
	jle	L303
L302:
	movl	$8161, %edi
	call	__m3_fault
L303:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u32_18
	.private_extern _divmod_pow2__mod_pow2_u32_18
_divmod_pow2__mod_pow2_u32_18:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	andl	$262143, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u32_19
	.private_extern _divmod_pow2__div_pow2_u32_19
_divmod_pow2__div_pow2_u32_19:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$19, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L308
	cmpq	$2147483647, -16(%rbp)
	jle	L309
L308:
	movl	$8289, %edi
	call	__m3_fault
L309:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u32_19
	.private_extern _divmod_pow2__mod_pow2_u32_19
_divmod_pow2__mod_pow2_u32_19:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	andl	$524287, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u32_20
	.private_extern _divmod_pow2__div_pow2_u32_20
_divmod_pow2__div_pow2_u32_20:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$20, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L314
	cmpq	$2147483647, -16(%rbp)
	jle	L315
L314:
	movl	$8417, %edi
	call	__m3_fault
L315:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u32_20
	.private_extern _divmod_pow2__mod_pow2_u32_20
_divmod_pow2__mod_pow2_u32_20:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	andl	$1048575, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u32_21
	.private_extern _divmod_pow2__div_pow2_u32_21
_divmod_pow2__div_pow2_u32_21:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$21, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L320
	cmpq	$2147483647, -16(%rbp)
	jle	L321
L320:
	movl	$8545, %edi
	call	__m3_fault
L321:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u32_21
	.private_extern _divmod_pow2__mod_pow2_u32_21
_divmod_pow2__mod_pow2_u32_21:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	andl	$2097151, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u32_22
	.private_extern _divmod_pow2__div_pow2_u32_22
_divmod_pow2__div_pow2_u32_22:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$22, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L326
	cmpq	$2147483647, -16(%rbp)
	jle	L327
L326:
	movl	$8673, %edi
	call	__m3_fault
L327:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u32_22
	.private_extern _divmod_pow2__mod_pow2_u32_22
_divmod_pow2__mod_pow2_u32_22:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	andl	$4194303, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u32_23
	.private_extern _divmod_pow2__div_pow2_u32_23
_divmod_pow2__div_pow2_u32_23:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$23, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L332
	cmpq	$2147483647, -16(%rbp)
	jle	L333
L332:
	movl	$8801, %edi
	call	__m3_fault
L333:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u32_23
	.private_extern _divmod_pow2__mod_pow2_u32_23
_divmod_pow2__mod_pow2_u32_23:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	andl	$8388607, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u32_24
	.private_extern _divmod_pow2__div_pow2_u32_24
_divmod_pow2__div_pow2_u32_24:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$24, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L338
	cmpq	$2147483647, -16(%rbp)
	jle	L339
L338:
	movl	$8929, %edi
	call	__m3_fault
L339:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u32_24
	.private_extern _divmod_pow2__mod_pow2_u32_24
_divmod_pow2__mod_pow2_u32_24:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	andl	$16777215, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u32_25
	.private_extern _divmod_pow2__div_pow2_u32_25
_divmod_pow2__div_pow2_u32_25:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$25, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L344
	cmpq	$2147483647, -16(%rbp)
	jle	L345
L344:
	movl	$9057, %edi
	call	__m3_fault
L345:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u32_25
	.private_extern _divmod_pow2__mod_pow2_u32_25
_divmod_pow2__mod_pow2_u32_25:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	andl	$33554431, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u32_26
	.private_extern _divmod_pow2__div_pow2_u32_26
_divmod_pow2__div_pow2_u32_26:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$26, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L350
	cmpq	$2147483647, -16(%rbp)
	jle	L351
L350:
	movl	$9185, %edi
	call	__m3_fault
L351:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u32_26
	.private_extern _divmod_pow2__mod_pow2_u32_26
_divmod_pow2__mod_pow2_u32_26:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	andl	$67108863, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u32_27
	.private_extern _divmod_pow2__div_pow2_u32_27
_divmod_pow2__div_pow2_u32_27:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$27, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L356
	cmpq	$2147483647, -16(%rbp)
	jle	L357
L356:
	movl	$9313, %edi
	call	__m3_fault
L357:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u32_27
	.private_extern _divmod_pow2__mod_pow2_u32_27
_divmod_pow2__mod_pow2_u32_27:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	andl	$134217727, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u32_28
	.private_extern _divmod_pow2__div_pow2_u32_28
_divmod_pow2__div_pow2_u32_28:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$28, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L362
	cmpq	$2147483647, -16(%rbp)
	jle	L363
L362:
	movl	$9441, %edi
	call	__m3_fault
L363:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u32_28
	.private_extern _divmod_pow2__mod_pow2_u32_28
_divmod_pow2__mod_pow2_u32_28:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	andl	$268435455, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u32_29
	.private_extern _divmod_pow2__div_pow2_u32_29
_divmod_pow2__div_pow2_u32_29:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$29, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L368
	cmpq	$2147483647, -16(%rbp)
	jle	L369
L368:
	movl	$9569, %edi
	call	__m3_fault
L369:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u32_29
	.private_extern _divmod_pow2__mod_pow2_u32_29
_divmod_pow2__mod_pow2_u32_29:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	andl	$536870911, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u32_30
	.private_extern _divmod_pow2__div_pow2_u32_30
_divmod_pow2__div_pow2_u32_30:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$30, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L374
	cmpq	$2147483647, -16(%rbp)
	jle	L375
L374:
	movl	$9697, %edi
	call	__m3_fault
L375:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u32_30
	.private_extern _divmod_pow2__mod_pow2_u32_30
_divmod_pow2__mod_pow2_u32_30:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	andl	$1073741823, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u32_31
	.private_extern _divmod_pow2__div_pow2_u32_31
_divmod_pow2__div_pow2_u32_31:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	sarq	$31, %rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L380
	cmpq	$2147483647, -16(%rbp)
	jle	L381
L380:
	movl	$9825, %edi
	call	__m3_fault
L381:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u32_31
	.private_extern _divmod_pow2__mod_pow2_u32_31
_divmod_pow2__mod_pow2_u32_31:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	andl	$2147483647, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_0
	.private_extern _divmod_pow2__div_pow2_i64_0
_divmod_pow2__div_pow2_i64_0:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_0
	.private_extern _divmod_pow2__mod_pow2_i64_0
_divmod_pow2__mod_pow2_i64_0:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	$0, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_1
	.private_extern _divmod_pow2__div_pow2_i64_1
_divmod_pow2__div_pow2_i64_1:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	%rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_1
	.private_extern _divmod_pow2__mod_pow2_i64_1
_divmod_pow2__mod_pow2_i64_1:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$1, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_2
	.private_extern _divmod_pow2__div_pow2_i64_2
_divmod_pow2__div_pow2_i64_2:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$2, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_2
	.private_extern _divmod_pow2__mod_pow2_i64_2
_divmod_pow2__mod_pow2_i64_2:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$3, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_3
	.private_extern _divmod_pow2__div_pow2_i64_3
_divmod_pow2__div_pow2_i64_3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$3, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_3
	.private_extern _divmod_pow2__mod_pow2_i64_3
_divmod_pow2__mod_pow2_i64_3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$7, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_4
	.private_extern _divmod_pow2__div_pow2_i64_4
_divmod_pow2__div_pow2_i64_4:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$4, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_4
	.private_extern _divmod_pow2__mod_pow2_i64_4
_divmod_pow2__mod_pow2_i64_4:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$15, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_5
	.private_extern _divmod_pow2__div_pow2_i64_5
_divmod_pow2__div_pow2_i64_5:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$5, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_5
	.private_extern _divmod_pow2__mod_pow2_i64_5
_divmod_pow2__mod_pow2_i64_5:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$31, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_6
	.private_extern _divmod_pow2__div_pow2_i64_6
_divmod_pow2__div_pow2_i64_6:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$6, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_6
	.private_extern _divmod_pow2__mod_pow2_i64_6
_divmod_pow2__mod_pow2_i64_6:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$63, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_7
	.private_extern _divmod_pow2__div_pow2_i64_7
_divmod_pow2__div_pow2_i64_7:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$7, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_7
	.private_extern _divmod_pow2__mod_pow2_i64_7
_divmod_pow2__mod_pow2_i64_7:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$127, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_8
	.private_extern _divmod_pow2__div_pow2_i64_8
_divmod_pow2__div_pow2_i64_8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$8, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_8
	.private_extern _divmod_pow2__mod_pow2_i64_8
_divmod_pow2__mod_pow2_i64_8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$255, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_9
	.private_extern _divmod_pow2__div_pow2_i64_9
_divmod_pow2__div_pow2_i64_9:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$9, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_9
	.private_extern _divmod_pow2__mod_pow2_i64_9
_divmod_pow2__mod_pow2_i64_9:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$511, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_10
	.private_extern _divmod_pow2__div_pow2_i64_10
_divmod_pow2__div_pow2_i64_10:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$10, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_10
	.private_extern _divmod_pow2__mod_pow2_i64_10
_divmod_pow2__mod_pow2_i64_10:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$1023, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_11
	.private_extern _divmod_pow2__div_pow2_i64_11
_divmod_pow2__div_pow2_i64_11:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$11, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_11
	.private_extern _divmod_pow2__mod_pow2_i64_11
_divmod_pow2__mod_pow2_i64_11:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$2047, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_12
	.private_extern _divmod_pow2__div_pow2_i64_12
_divmod_pow2__div_pow2_i64_12:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$12, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_12
	.private_extern _divmod_pow2__mod_pow2_i64_12
_divmod_pow2__mod_pow2_i64_12:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$4095, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_13
	.private_extern _divmod_pow2__div_pow2_i64_13
_divmod_pow2__div_pow2_i64_13:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$13, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_13
	.private_extern _divmod_pow2__mod_pow2_i64_13
_divmod_pow2__mod_pow2_i64_13:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$8191, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_14
	.private_extern _divmod_pow2__div_pow2_i64_14
_divmod_pow2__div_pow2_i64_14:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$14, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_14
	.private_extern _divmod_pow2__mod_pow2_i64_14
_divmod_pow2__mod_pow2_i64_14:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$16383, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_15
	.private_extern _divmod_pow2__div_pow2_i64_15
_divmod_pow2__div_pow2_i64_15:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$15, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_15
	.private_extern _divmod_pow2__mod_pow2_i64_15
_divmod_pow2__mod_pow2_i64_15:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$32767, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_16
	.private_extern _divmod_pow2__div_pow2_i64_16
_divmod_pow2__div_pow2_i64_16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$16, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_16
	.private_extern _divmod_pow2__mod_pow2_i64_16
_divmod_pow2__mod_pow2_i64_16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$65535, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_17
	.private_extern _divmod_pow2__div_pow2_i64_17
_divmod_pow2__div_pow2_i64_17:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$17, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_17
	.private_extern _divmod_pow2__mod_pow2_i64_17
_divmod_pow2__mod_pow2_i64_17:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$131071, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_18
	.private_extern _divmod_pow2__div_pow2_i64_18
_divmod_pow2__div_pow2_i64_18:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$18, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_18
	.private_extern _divmod_pow2__mod_pow2_i64_18
_divmod_pow2__mod_pow2_i64_18:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$262143, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_19
	.private_extern _divmod_pow2__div_pow2_i64_19
_divmod_pow2__div_pow2_i64_19:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$19, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_19
	.private_extern _divmod_pow2__mod_pow2_i64_19
_divmod_pow2__mod_pow2_i64_19:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$524287, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_20
	.private_extern _divmod_pow2__div_pow2_i64_20
_divmod_pow2__div_pow2_i64_20:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$20, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_20
	.private_extern _divmod_pow2__mod_pow2_i64_20
_divmod_pow2__mod_pow2_i64_20:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$1048575, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_21
	.private_extern _divmod_pow2__div_pow2_i64_21
_divmod_pow2__div_pow2_i64_21:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$21, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_21
	.private_extern _divmod_pow2__mod_pow2_i64_21
_divmod_pow2__mod_pow2_i64_21:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$2097151, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_22
	.private_extern _divmod_pow2__div_pow2_i64_22
_divmod_pow2__div_pow2_i64_22:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$22, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_22
	.private_extern _divmod_pow2__mod_pow2_i64_22
_divmod_pow2__mod_pow2_i64_22:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$4194303, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_23
	.private_extern _divmod_pow2__div_pow2_i64_23
_divmod_pow2__div_pow2_i64_23:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$23, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_23
	.private_extern _divmod_pow2__mod_pow2_i64_23
_divmod_pow2__mod_pow2_i64_23:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$8388607, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_24
	.private_extern _divmod_pow2__div_pow2_i64_24
_divmod_pow2__div_pow2_i64_24:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$24, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_24
	.private_extern _divmod_pow2__mod_pow2_i64_24
_divmod_pow2__mod_pow2_i64_24:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$16777215, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_25
	.private_extern _divmod_pow2__div_pow2_i64_25
_divmod_pow2__div_pow2_i64_25:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$25, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_25
	.private_extern _divmod_pow2__mod_pow2_i64_25
_divmod_pow2__mod_pow2_i64_25:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$33554431, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_26
	.private_extern _divmod_pow2__div_pow2_i64_26
_divmod_pow2__div_pow2_i64_26:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$26, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_26
	.private_extern _divmod_pow2__mod_pow2_i64_26
_divmod_pow2__mod_pow2_i64_26:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$67108863, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_27
	.private_extern _divmod_pow2__div_pow2_i64_27
_divmod_pow2__div_pow2_i64_27:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$27, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_27
	.private_extern _divmod_pow2__mod_pow2_i64_27
_divmod_pow2__mod_pow2_i64_27:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$134217727, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_28
	.private_extern _divmod_pow2__div_pow2_i64_28
_divmod_pow2__div_pow2_i64_28:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$28, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_28
	.private_extern _divmod_pow2__mod_pow2_i64_28
_divmod_pow2__mod_pow2_i64_28:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$268435455, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_29
	.private_extern _divmod_pow2__div_pow2_i64_29
_divmod_pow2__div_pow2_i64_29:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$29, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_29
	.private_extern _divmod_pow2__mod_pow2_i64_29
_divmod_pow2__mod_pow2_i64_29:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$536870911, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_30
	.private_extern _divmod_pow2__div_pow2_i64_30
_divmod_pow2__div_pow2_i64_30:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$30, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_30
	.private_extern _divmod_pow2__mod_pow2_i64_30
_divmod_pow2__mod_pow2_i64_30:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$1073741823, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_31
	.private_extern _divmod_pow2__div_pow2_i64_31
_divmod_pow2__div_pow2_i64_31:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$31, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_31
	.private_extern _divmod_pow2__mod_pow2_i64_31
_divmod_pow2__mod_pow2_i64_31:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$2147483647, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_32
	.private_extern _divmod_pow2__div_pow2_i64_32
_divmod_pow2__div_pow2_i64_32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$32, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_32
	.private_extern _divmod_pow2__mod_pow2_i64_32
_divmod_pow2__mod_pow2_i64_32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$4294967295, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_33
	.private_extern _divmod_pow2__div_pow2_i64_33
_divmod_pow2__div_pow2_i64_33:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$33, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_33
	.private_extern _divmod_pow2__mod_pow2_i64_33
_divmod_pow2__mod_pow2_i64_33:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$8589934591, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_34
	.private_extern _divmod_pow2__div_pow2_i64_34
_divmod_pow2__div_pow2_i64_34:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$34, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_34
	.private_extern _divmod_pow2__mod_pow2_i64_34
_divmod_pow2__mod_pow2_i64_34:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$17179869183, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_35
	.private_extern _divmod_pow2__div_pow2_i64_35
_divmod_pow2__div_pow2_i64_35:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$35, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_35
	.private_extern _divmod_pow2__mod_pow2_i64_35
_divmod_pow2__mod_pow2_i64_35:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$34359738367, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_36
	.private_extern _divmod_pow2__div_pow2_i64_36
_divmod_pow2__div_pow2_i64_36:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$36, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_36
	.private_extern _divmod_pow2__mod_pow2_i64_36
_divmod_pow2__mod_pow2_i64_36:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$68719476735, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_37
	.private_extern _divmod_pow2__div_pow2_i64_37
_divmod_pow2__div_pow2_i64_37:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$37, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_37
	.private_extern _divmod_pow2__mod_pow2_i64_37
_divmod_pow2__mod_pow2_i64_37:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$137438953471, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_38
	.private_extern _divmod_pow2__div_pow2_i64_38
_divmod_pow2__div_pow2_i64_38:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$38, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_38
	.private_extern _divmod_pow2__mod_pow2_i64_38
_divmod_pow2__mod_pow2_i64_38:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$274877906943, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_39
	.private_extern _divmod_pow2__div_pow2_i64_39
_divmod_pow2__div_pow2_i64_39:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$39, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_39
	.private_extern _divmod_pow2__mod_pow2_i64_39
_divmod_pow2__mod_pow2_i64_39:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$549755813887, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_40
	.private_extern _divmod_pow2__div_pow2_i64_40
_divmod_pow2__div_pow2_i64_40:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$40, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_40
	.private_extern _divmod_pow2__mod_pow2_i64_40
_divmod_pow2__mod_pow2_i64_40:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$1099511627775, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_41
	.private_extern _divmod_pow2__div_pow2_i64_41
_divmod_pow2__div_pow2_i64_41:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$41, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_41
	.private_extern _divmod_pow2__mod_pow2_i64_41
_divmod_pow2__mod_pow2_i64_41:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$2199023255551, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_42
	.private_extern _divmod_pow2__div_pow2_i64_42
_divmod_pow2__div_pow2_i64_42:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$42, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_42
	.private_extern _divmod_pow2__mod_pow2_i64_42
_divmod_pow2__mod_pow2_i64_42:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$4398046511103, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_43
	.private_extern _divmod_pow2__div_pow2_i64_43
_divmod_pow2__div_pow2_i64_43:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$43, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_43
	.private_extern _divmod_pow2__mod_pow2_i64_43
_divmod_pow2__mod_pow2_i64_43:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$8796093022207, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_44
	.private_extern _divmod_pow2__div_pow2_i64_44
_divmod_pow2__div_pow2_i64_44:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$44, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_44
	.private_extern _divmod_pow2__mod_pow2_i64_44
_divmod_pow2__mod_pow2_i64_44:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$17592186044415, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_45
	.private_extern _divmod_pow2__div_pow2_i64_45
_divmod_pow2__div_pow2_i64_45:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$45, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_45
	.private_extern _divmod_pow2__mod_pow2_i64_45
_divmod_pow2__mod_pow2_i64_45:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$35184372088831, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_46
	.private_extern _divmod_pow2__div_pow2_i64_46
_divmod_pow2__div_pow2_i64_46:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$46, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_46
	.private_extern _divmod_pow2__mod_pow2_i64_46
_divmod_pow2__mod_pow2_i64_46:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$70368744177663, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_47
	.private_extern _divmod_pow2__div_pow2_i64_47
_divmod_pow2__div_pow2_i64_47:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$47, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_47
	.private_extern _divmod_pow2__mod_pow2_i64_47
_divmod_pow2__mod_pow2_i64_47:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$140737488355327, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_48
	.private_extern _divmod_pow2__div_pow2_i64_48
_divmod_pow2__div_pow2_i64_48:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$48, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_48
	.private_extern _divmod_pow2__mod_pow2_i64_48
_divmod_pow2__mod_pow2_i64_48:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$281474976710655, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_49
	.private_extern _divmod_pow2__div_pow2_i64_49
_divmod_pow2__div_pow2_i64_49:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$49, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_49
	.private_extern _divmod_pow2__mod_pow2_i64_49
_divmod_pow2__mod_pow2_i64_49:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$562949953421311, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_50
	.private_extern _divmod_pow2__div_pow2_i64_50
_divmod_pow2__div_pow2_i64_50:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$50, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_50
	.private_extern _divmod_pow2__mod_pow2_i64_50
_divmod_pow2__mod_pow2_i64_50:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$1125899906842623, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_51
	.private_extern _divmod_pow2__div_pow2_i64_51
_divmod_pow2__div_pow2_i64_51:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$51, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_51
	.private_extern _divmod_pow2__mod_pow2_i64_51
_divmod_pow2__mod_pow2_i64_51:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$2251799813685247, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_52
	.private_extern _divmod_pow2__div_pow2_i64_52
_divmod_pow2__div_pow2_i64_52:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$52, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_52
	.private_extern _divmod_pow2__mod_pow2_i64_52
_divmod_pow2__mod_pow2_i64_52:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$4503599627370495, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_53
	.private_extern _divmod_pow2__div_pow2_i64_53
_divmod_pow2__div_pow2_i64_53:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$53, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_53
	.private_extern _divmod_pow2__mod_pow2_i64_53
_divmod_pow2__mod_pow2_i64_53:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$9007199254740991, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_54
	.private_extern _divmod_pow2__div_pow2_i64_54
_divmod_pow2__div_pow2_i64_54:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$54, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_54
	.private_extern _divmod_pow2__mod_pow2_i64_54
_divmod_pow2__mod_pow2_i64_54:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$18014398509481983, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_55
	.private_extern _divmod_pow2__div_pow2_i64_55
_divmod_pow2__div_pow2_i64_55:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$55, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_55
	.private_extern _divmod_pow2__mod_pow2_i64_55
_divmod_pow2__mod_pow2_i64_55:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$36028797018963967, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_56
	.private_extern _divmod_pow2__div_pow2_i64_56
_divmod_pow2__div_pow2_i64_56:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$56, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_56
	.private_extern _divmod_pow2__mod_pow2_i64_56
_divmod_pow2__mod_pow2_i64_56:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$72057594037927935, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_57
	.private_extern _divmod_pow2__div_pow2_i64_57
_divmod_pow2__div_pow2_i64_57:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$57, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_57
	.private_extern _divmod_pow2__mod_pow2_i64_57
_divmod_pow2__mod_pow2_i64_57:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$144115188075855871, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_58
	.private_extern _divmod_pow2__div_pow2_i64_58
_divmod_pow2__div_pow2_i64_58:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$58, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_58
	.private_extern _divmod_pow2__mod_pow2_i64_58
_divmod_pow2__mod_pow2_i64_58:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$288230376151711743, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_59
	.private_extern _divmod_pow2__div_pow2_i64_59
_divmod_pow2__div_pow2_i64_59:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$59, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_59
	.private_extern _divmod_pow2__mod_pow2_i64_59
_divmod_pow2__mod_pow2_i64_59:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$576460752303423487, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_60
	.private_extern _divmod_pow2__div_pow2_i64_60
_divmod_pow2__div_pow2_i64_60:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$60, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_60
	.private_extern _divmod_pow2__mod_pow2_i64_60
_divmod_pow2__mod_pow2_i64_60:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$1152921504606846975, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_61
	.private_extern _divmod_pow2__div_pow2_i64_61
_divmod_pow2__div_pow2_i64_61:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$61, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_61
	.private_extern _divmod_pow2__mod_pow2_i64_61
_divmod_pow2__mod_pow2_i64_61:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$2305843009213693951, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_62
	.private_extern _divmod_pow2__div_pow2_i64_62
_divmod_pow2__div_pow2_i64_62:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$62, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_i64_62
	.private_extern _divmod_pow2__mod_pow2_i64_62
_divmod_pow2__mod_pow2_i64_62:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$4611686018427387903, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_i64_63
	.private_extern _divmod_pow2__div_pow2_i64_63
_divmod_pow2__div_pow2_i64_63:
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
.globl _divmod_pow2__mod_pow2_i64_63
	.private_extern _divmod_pow2__mod_pow2_i64_63
_divmod_pow2__mod_pow2_i64_63:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rcx
	leaq	-1(%rcx), %rdx
	movq	%rcx, %rax
	orq	%rdx, %rax
	movq	%rax, %rsi
	sarq	$63, %rsi
	subq	%rsi, %rdx
	movabsq	$-9223372036854775808, %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %edx
	movq	%rsi, %rax
	notq	%rax
	leaq	(%rdx,%rax), %rax
	salq	$63, %rax
	movq	%rcx, %rdx
	subq	%rax, %rdx
	movq	%rdx, %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_0
	.private_extern _divmod_pow2__div_pow2_u64_0
_divmod_pow2__div_pow2_u64_0:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_0
	.private_extern _divmod_pow2__mod_pow2_u64_0
_divmod_pow2__mod_pow2_u64_0:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	$0, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_1
	.private_extern _divmod_pow2__div_pow2_u64_1
_divmod_pow2__div_pow2_u64_1:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	%rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_1
	.private_extern _divmod_pow2__mod_pow2_u64_1
_divmod_pow2__mod_pow2_u64_1:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$1, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_2
	.private_extern _divmod_pow2__div_pow2_u64_2
_divmod_pow2__div_pow2_u64_2:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$2, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_2
	.private_extern _divmod_pow2__mod_pow2_u64_2
_divmod_pow2__mod_pow2_u64_2:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$3, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_3
	.private_extern _divmod_pow2__div_pow2_u64_3
_divmod_pow2__div_pow2_u64_3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$3, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_3
	.private_extern _divmod_pow2__mod_pow2_u64_3
_divmod_pow2__mod_pow2_u64_3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$7, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_4
	.private_extern _divmod_pow2__div_pow2_u64_4
_divmod_pow2__div_pow2_u64_4:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$4, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_4
	.private_extern _divmod_pow2__mod_pow2_u64_4
_divmod_pow2__mod_pow2_u64_4:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$15, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_5
	.private_extern _divmod_pow2__div_pow2_u64_5
_divmod_pow2__div_pow2_u64_5:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$5, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_5
	.private_extern _divmod_pow2__mod_pow2_u64_5
_divmod_pow2__mod_pow2_u64_5:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$31, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_6
	.private_extern _divmod_pow2__div_pow2_u64_6
_divmod_pow2__div_pow2_u64_6:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$6, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_6
	.private_extern _divmod_pow2__mod_pow2_u64_6
_divmod_pow2__mod_pow2_u64_6:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$63, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_7
	.private_extern _divmod_pow2__div_pow2_u64_7
_divmod_pow2__div_pow2_u64_7:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$7, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_7
	.private_extern _divmod_pow2__mod_pow2_u64_7
_divmod_pow2__mod_pow2_u64_7:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$127, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_8
	.private_extern _divmod_pow2__div_pow2_u64_8
_divmod_pow2__div_pow2_u64_8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$8, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_8
	.private_extern _divmod_pow2__mod_pow2_u64_8
_divmod_pow2__mod_pow2_u64_8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$255, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_9
	.private_extern _divmod_pow2__div_pow2_u64_9
_divmod_pow2__div_pow2_u64_9:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$9, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_9
	.private_extern _divmod_pow2__mod_pow2_u64_9
_divmod_pow2__mod_pow2_u64_9:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$511, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_10
	.private_extern _divmod_pow2__div_pow2_u64_10
_divmod_pow2__div_pow2_u64_10:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$10, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_10
	.private_extern _divmod_pow2__mod_pow2_u64_10
_divmod_pow2__mod_pow2_u64_10:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$1023, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_11
	.private_extern _divmod_pow2__div_pow2_u64_11
_divmod_pow2__div_pow2_u64_11:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$11, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_11
	.private_extern _divmod_pow2__mod_pow2_u64_11
_divmod_pow2__mod_pow2_u64_11:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$2047, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_12
	.private_extern _divmod_pow2__div_pow2_u64_12
_divmod_pow2__div_pow2_u64_12:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$12, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_12
	.private_extern _divmod_pow2__mod_pow2_u64_12
_divmod_pow2__mod_pow2_u64_12:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$4095, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_13
	.private_extern _divmod_pow2__div_pow2_u64_13
_divmod_pow2__div_pow2_u64_13:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$13, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_13
	.private_extern _divmod_pow2__mod_pow2_u64_13
_divmod_pow2__mod_pow2_u64_13:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$8191, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_14
	.private_extern _divmod_pow2__div_pow2_u64_14
_divmod_pow2__div_pow2_u64_14:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$14, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_14
	.private_extern _divmod_pow2__mod_pow2_u64_14
_divmod_pow2__mod_pow2_u64_14:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$16383, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_15
	.private_extern _divmod_pow2__div_pow2_u64_15
_divmod_pow2__div_pow2_u64_15:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$15, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_15
	.private_extern _divmod_pow2__mod_pow2_u64_15
_divmod_pow2__mod_pow2_u64_15:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$32767, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_16
	.private_extern _divmod_pow2__div_pow2_u64_16
_divmod_pow2__div_pow2_u64_16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$16, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_16
	.private_extern _divmod_pow2__mod_pow2_u64_16
_divmod_pow2__mod_pow2_u64_16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$65535, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_17
	.private_extern _divmod_pow2__div_pow2_u64_17
_divmod_pow2__div_pow2_u64_17:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$17, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_17
	.private_extern _divmod_pow2__mod_pow2_u64_17
_divmod_pow2__mod_pow2_u64_17:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$131071, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_18
	.private_extern _divmod_pow2__div_pow2_u64_18
_divmod_pow2__div_pow2_u64_18:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$18, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_18
	.private_extern _divmod_pow2__mod_pow2_u64_18
_divmod_pow2__mod_pow2_u64_18:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$262143, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_19
	.private_extern _divmod_pow2__div_pow2_u64_19
_divmod_pow2__div_pow2_u64_19:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$19, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_19
	.private_extern _divmod_pow2__mod_pow2_u64_19
_divmod_pow2__mod_pow2_u64_19:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$524287, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_20
	.private_extern _divmod_pow2__div_pow2_u64_20
_divmod_pow2__div_pow2_u64_20:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$20, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_20
	.private_extern _divmod_pow2__mod_pow2_u64_20
_divmod_pow2__mod_pow2_u64_20:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$1048575, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_21
	.private_extern _divmod_pow2__div_pow2_u64_21
_divmod_pow2__div_pow2_u64_21:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$21, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_21
	.private_extern _divmod_pow2__mod_pow2_u64_21
_divmod_pow2__mod_pow2_u64_21:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$2097151, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_22
	.private_extern _divmod_pow2__div_pow2_u64_22
_divmod_pow2__div_pow2_u64_22:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$22, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_22
	.private_extern _divmod_pow2__mod_pow2_u64_22
_divmod_pow2__mod_pow2_u64_22:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$4194303, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_23
	.private_extern _divmod_pow2__div_pow2_u64_23
_divmod_pow2__div_pow2_u64_23:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$23, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_23
	.private_extern _divmod_pow2__mod_pow2_u64_23
_divmod_pow2__mod_pow2_u64_23:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$8388607, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_24
	.private_extern _divmod_pow2__div_pow2_u64_24
_divmod_pow2__div_pow2_u64_24:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$24, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_24
	.private_extern _divmod_pow2__mod_pow2_u64_24
_divmod_pow2__mod_pow2_u64_24:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$16777215, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_25
	.private_extern _divmod_pow2__div_pow2_u64_25
_divmod_pow2__div_pow2_u64_25:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$25, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_25
	.private_extern _divmod_pow2__mod_pow2_u64_25
_divmod_pow2__mod_pow2_u64_25:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$33554431, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_26
	.private_extern _divmod_pow2__div_pow2_u64_26
_divmod_pow2__div_pow2_u64_26:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$26, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_26
	.private_extern _divmod_pow2__mod_pow2_u64_26
_divmod_pow2__mod_pow2_u64_26:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$67108863, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_27
	.private_extern _divmod_pow2__div_pow2_u64_27
_divmod_pow2__div_pow2_u64_27:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$27, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_27
	.private_extern _divmod_pow2__mod_pow2_u64_27
_divmod_pow2__mod_pow2_u64_27:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$134217727, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_28
	.private_extern _divmod_pow2__div_pow2_u64_28
_divmod_pow2__div_pow2_u64_28:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$28, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_28
	.private_extern _divmod_pow2__mod_pow2_u64_28
_divmod_pow2__mod_pow2_u64_28:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$268435455, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_29
	.private_extern _divmod_pow2__div_pow2_u64_29
_divmod_pow2__div_pow2_u64_29:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$29, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_29
	.private_extern _divmod_pow2__mod_pow2_u64_29
_divmod_pow2__mod_pow2_u64_29:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$536870911, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_30
	.private_extern _divmod_pow2__div_pow2_u64_30
_divmod_pow2__div_pow2_u64_30:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$30, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_30
	.private_extern _divmod_pow2__mod_pow2_u64_30
_divmod_pow2__mod_pow2_u64_30:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$1073741823, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_31
	.private_extern _divmod_pow2__div_pow2_u64_31
_divmod_pow2__div_pow2_u64_31:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$31, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_31
	.private_extern _divmod_pow2__mod_pow2_u64_31
_divmod_pow2__mod_pow2_u64_31:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$2147483647, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_32
	.private_extern _divmod_pow2__div_pow2_u64_32
_divmod_pow2__div_pow2_u64_32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$32, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_32
	.private_extern _divmod_pow2__mod_pow2_u64_32
_divmod_pow2__mod_pow2_u64_32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$4294967295, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_33
	.private_extern _divmod_pow2__div_pow2_u64_33
_divmod_pow2__div_pow2_u64_33:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$33, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_33
	.private_extern _divmod_pow2__mod_pow2_u64_33
_divmod_pow2__mod_pow2_u64_33:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$8589934591, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_34
	.private_extern _divmod_pow2__div_pow2_u64_34
_divmod_pow2__div_pow2_u64_34:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$34, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_34
	.private_extern _divmod_pow2__mod_pow2_u64_34
_divmod_pow2__mod_pow2_u64_34:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$17179869183, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_35
	.private_extern _divmod_pow2__div_pow2_u64_35
_divmod_pow2__div_pow2_u64_35:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$35, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_35
	.private_extern _divmod_pow2__mod_pow2_u64_35
_divmod_pow2__mod_pow2_u64_35:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$34359738367, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_36
	.private_extern _divmod_pow2__div_pow2_u64_36
_divmod_pow2__div_pow2_u64_36:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$36, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_36
	.private_extern _divmod_pow2__mod_pow2_u64_36
_divmod_pow2__mod_pow2_u64_36:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$68719476735, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_37
	.private_extern _divmod_pow2__div_pow2_u64_37
_divmod_pow2__div_pow2_u64_37:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$37, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_37
	.private_extern _divmod_pow2__mod_pow2_u64_37
_divmod_pow2__mod_pow2_u64_37:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$137438953471, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_38
	.private_extern _divmod_pow2__div_pow2_u64_38
_divmod_pow2__div_pow2_u64_38:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$38, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_38
	.private_extern _divmod_pow2__mod_pow2_u64_38
_divmod_pow2__mod_pow2_u64_38:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$274877906943, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_39
	.private_extern _divmod_pow2__div_pow2_u64_39
_divmod_pow2__div_pow2_u64_39:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$39, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_39
	.private_extern _divmod_pow2__mod_pow2_u64_39
_divmod_pow2__mod_pow2_u64_39:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$549755813887, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_40
	.private_extern _divmod_pow2__div_pow2_u64_40
_divmod_pow2__div_pow2_u64_40:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$40, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_40
	.private_extern _divmod_pow2__mod_pow2_u64_40
_divmod_pow2__mod_pow2_u64_40:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$1099511627775, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_41
	.private_extern _divmod_pow2__div_pow2_u64_41
_divmod_pow2__div_pow2_u64_41:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$41, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_41
	.private_extern _divmod_pow2__mod_pow2_u64_41
_divmod_pow2__mod_pow2_u64_41:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$2199023255551, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_42
	.private_extern _divmod_pow2__div_pow2_u64_42
_divmod_pow2__div_pow2_u64_42:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$42, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_42
	.private_extern _divmod_pow2__mod_pow2_u64_42
_divmod_pow2__mod_pow2_u64_42:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$4398046511103, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_43
	.private_extern _divmod_pow2__div_pow2_u64_43
_divmod_pow2__div_pow2_u64_43:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$43, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_43
	.private_extern _divmod_pow2__mod_pow2_u64_43
_divmod_pow2__mod_pow2_u64_43:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$8796093022207, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_44
	.private_extern _divmod_pow2__div_pow2_u64_44
_divmod_pow2__div_pow2_u64_44:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$44, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_44
	.private_extern _divmod_pow2__mod_pow2_u64_44
_divmod_pow2__mod_pow2_u64_44:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$17592186044415, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_45
	.private_extern _divmod_pow2__div_pow2_u64_45
_divmod_pow2__div_pow2_u64_45:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$45, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_45
	.private_extern _divmod_pow2__mod_pow2_u64_45
_divmod_pow2__mod_pow2_u64_45:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$35184372088831, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_46
	.private_extern _divmod_pow2__div_pow2_u64_46
_divmod_pow2__div_pow2_u64_46:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$46, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_46
	.private_extern _divmod_pow2__mod_pow2_u64_46
_divmod_pow2__mod_pow2_u64_46:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$70368744177663, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_47
	.private_extern _divmod_pow2__div_pow2_u64_47
_divmod_pow2__div_pow2_u64_47:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$47, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_47
	.private_extern _divmod_pow2__mod_pow2_u64_47
_divmod_pow2__mod_pow2_u64_47:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$140737488355327, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_48
	.private_extern _divmod_pow2__div_pow2_u64_48
_divmod_pow2__div_pow2_u64_48:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$48, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_48
	.private_extern _divmod_pow2__mod_pow2_u64_48
_divmod_pow2__mod_pow2_u64_48:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$281474976710655, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_49
	.private_extern _divmod_pow2__div_pow2_u64_49
_divmod_pow2__div_pow2_u64_49:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$49, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_49
	.private_extern _divmod_pow2__mod_pow2_u64_49
_divmod_pow2__mod_pow2_u64_49:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$562949953421311, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_50
	.private_extern _divmod_pow2__div_pow2_u64_50
_divmod_pow2__div_pow2_u64_50:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$50, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_50
	.private_extern _divmod_pow2__mod_pow2_u64_50
_divmod_pow2__mod_pow2_u64_50:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$1125899906842623, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_51
	.private_extern _divmod_pow2__div_pow2_u64_51
_divmod_pow2__div_pow2_u64_51:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$51, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_51
	.private_extern _divmod_pow2__mod_pow2_u64_51
_divmod_pow2__mod_pow2_u64_51:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$2251799813685247, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_52
	.private_extern _divmod_pow2__div_pow2_u64_52
_divmod_pow2__div_pow2_u64_52:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$52, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_52
	.private_extern _divmod_pow2__mod_pow2_u64_52
_divmod_pow2__mod_pow2_u64_52:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$4503599627370495, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_53
	.private_extern _divmod_pow2__div_pow2_u64_53
_divmod_pow2__div_pow2_u64_53:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$53, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_53
	.private_extern _divmod_pow2__mod_pow2_u64_53
_divmod_pow2__mod_pow2_u64_53:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$9007199254740991, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_54
	.private_extern _divmod_pow2__div_pow2_u64_54
_divmod_pow2__div_pow2_u64_54:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$54, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_54
	.private_extern _divmod_pow2__mod_pow2_u64_54
_divmod_pow2__mod_pow2_u64_54:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$18014398509481983, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_55
	.private_extern _divmod_pow2__div_pow2_u64_55
_divmod_pow2__div_pow2_u64_55:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$55, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_55
	.private_extern _divmod_pow2__mod_pow2_u64_55
_divmod_pow2__mod_pow2_u64_55:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$36028797018963967, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_56
	.private_extern _divmod_pow2__div_pow2_u64_56
_divmod_pow2__div_pow2_u64_56:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$56, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_56
	.private_extern _divmod_pow2__mod_pow2_u64_56
_divmod_pow2__mod_pow2_u64_56:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$72057594037927935, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_57
	.private_extern _divmod_pow2__div_pow2_u64_57
_divmod_pow2__div_pow2_u64_57:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$57, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_57
	.private_extern _divmod_pow2__mod_pow2_u64_57
_divmod_pow2__mod_pow2_u64_57:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$144115188075855871, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_58
	.private_extern _divmod_pow2__div_pow2_u64_58
_divmod_pow2__div_pow2_u64_58:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$58, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_58
	.private_extern _divmod_pow2__mod_pow2_u64_58
_divmod_pow2__mod_pow2_u64_58:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$288230376151711743, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_59
	.private_extern _divmod_pow2__div_pow2_u64_59
_divmod_pow2__div_pow2_u64_59:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$59, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_59
	.private_extern _divmod_pow2__mod_pow2_u64_59
_divmod_pow2__mod_pow2_u64_59:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$576460752303423487, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_60
	.private_extern _divmod_pow2__div_pow2_u64_60
_divmod_pow2__div_pow2_u64_60:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$60, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_60
	.private_extern _divmod_pow2__mod_pow2_u64_60
_divmod_pow2__mod_pow2_u64_60:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$1152921504606846975, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_61
	.private_extern _divmod_pow2__div_pow2_u64_61
_divmod_pow2__div_pow2_u64_61:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$61, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_61
	.private_extern _divmod_pow2__mod_pow2_u64_61
_divmod_pow2__mod_pow2_u64_61:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$2305843009213693951, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_62
	.private_extern _divmod_pow2__div_pow2_u64_62
_divmod_pow2__div_pow2_u64_62:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$62, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_u64_62
	.private_extern _divmod_pow2__mod_pow2_u64_62
_divmod_pow2__mod_pow2_u64_62:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$4611686018427387903, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_u64_63
	.private_extern _divmod_pow2__div_pow2_u64_63
_divmod_pow2__div_pow2_u64_63:
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
.globl _divmod_pow2__mod_pow2_u64_63
	.private_extern _divmod_pow2__mod_pow2_u64_63
_divmod_pow2__mod_pow2_u64_63:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rcx
	leaq	-1(%rcx), %rdx
	movq	%rcx, %rax
	orq	%rdx, %rax
	movq	%rax, %rsi
	sarq	$63, %rsi
	subq	%rsi, %rdx
	movabsq	$-9223372036854775808, %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %edx
	movq	%rsi, %rax
	notq	%rax
	leaq	(%rdx,%rax), %rax
	salq	$63, %rax
	movq	%rcx, %rdx
	subq	%rax, %rdx
	movq	%rdx, %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_0
	.private_extern _divmod_pow2__div_pow2_I_0
_divmod_pow2__div_pow2_I_0:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_0
	.private_extern _divmod_pow2__mod_pow2_I_0
_divmod_pow2__mod_pow2_I_0:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	$0, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_1
	.private_extern _divmod_pow2__div_pow2_I_1
_divmod_pow2__div_pow2_I_1:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	%rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_1
	.private_extern _divmod_pow2__mod_pow2_I_1
_divmod_pow2__mod_pow2_I_1:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$1, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_2
	.private_extern _divmod_pow2__div_pow2_I_2
_divmod_pow2__div_pow2_I_2:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$2, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_2
	.private_extern _divmod_pow2__mod_pow2_I_2
_divmod_pow2__mod_pow2_I_2:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$3, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_3
	.private_extern _divmod_pow2__div_pow2_I_3
_divmod_pow2__div_pow2_I_3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$3, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_3
	.private_extern _divmod_pow2__mod_pow2_I_3
_divmod_pow2__mod_pow2_I_3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$7, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_4
	.private_extern _divmod_pow2__div_pow2_I_4
_divmod_pow2__div_pow2_I_4:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$4, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_4
	.private_extern _divmod_pow2__mod_pow2_I_4
_divmod_pow2__mod_pow2_I_4:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$15, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_5
	.private_extern _divmod_pow2__div_pow2_I_5
_divmod_pow2__div_pow2_I_5:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$5, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_5
	.private_extern _divmod_pow2__mod_pow2_I_5
_divmod_pow2__mod_pow2_I_5:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$31, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_6
	.private_extern _divmod_pow2__div_pow2_I_6
_divmod_pow2__div_pow2_I_6:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$6, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_6
	.private_extern _divmod_pow2__mod_pow2_I_6
_divmod_pow2__mod_pow2_I_6:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$63, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_7
	.private_extern _divmod_pow2__div_pow2_I_7
_divmod_pow2__div_pow2_I_7:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$7, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_7
	.private_extern _divmod_pow2__mod_pow2_I_7
_divmod_pow2__mod_pow2_I_7:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$127, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_8
	.private_extern _divmod_pow2__div_pow2_I_8
_divmod_pow2__div_pow2_I_8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$8, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_8
	.private_extern _divmod_pow2__mod_pow2_I_8
_divmod_pow2__mod_pow2_I_8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$255, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_9
	.private_extern _divmod_pow2__div_pow2_I_9
_divmod_pow2__div_pow2_I_9:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$9, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_9
	.private_extern _divmod_pow2__mod_pow2_I_9
_divmod_pow2__mod_pow2_I_9:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$511, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_10
	.private_extern _divmod_pow2__div_pow2_I_10
_divmod_pow2__div_pow2_I_10:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$10, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_10
	.private_extern _divmod_pow2__mod_pow2_I_10
_divmod_pow2__mod_pow2_I_10:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$1023, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_11
	.private_extern _divmod_pow2__div_pow2_I_11
_divmod_pow2__div_pow2_I_11:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$11, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_11
	.private_extern _divmod_pow2__mod_pow2_I_11
_divmod_pow2__mod_pow2_I_11:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$2047, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_12
	.private_extern _divmod_pow2__div_pow2_I_12
_divmod_pow2__div_pow2_I_12:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$12, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_12
	.private_extern _divmod_pow2__mod_pow2_I_12
_divmod_pow2__mod_pow2_I_12:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$4095, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_13
	.private_extern _divmod_pow2__div_pow2_I_13
_divmod_pow2__div_pow2_I_13:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$13, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_13
	.private_extern _divmod_pow2__mod_pow2_I_13
_divmod_pow2__mod_pow2_I_13:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$8191, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_14
	.private_extern _divmod_pow2__div_pow2_I_14
_divmod_pow2__div_pow2_I_14:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$14, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_14
	.private_extern _divmod_pow2__mod_pow2_I_14
_divmod_pow2__mod_pow2_I_14:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$16383, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_15
	.private_extern _divmod_pow2__div_pow2_I_15
_divmod_pow2__div_pow2_I_15:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$15, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_15
	.private_extern _divmod_pow2__mod_pow2_I_15
_divmod_pow2__mod_pow2_I_15:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$32767, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_16
	.private_extern _divmod_pow2__div_pow2_I_16
_divmod_pow2__div_pow2_I_16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$16, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_16
	.private_extern _divmod_pow2__mod_pow2_I_16
_divmod_pow2__mod_pow2_I_16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$65535, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_17
	.private_extern _divmod_pow2__div_pow2_I_17
_divmod_pow2__div_pow2_I_17:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$17, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_17
	.private_extern _divmod_pow2__mod_pow2_I_17
_divmod_pow2__mod_pow2_I_17:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$131071, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_18
	.private_extern _divmod_pow2__div_pow2_I_18
_divmod_pow2__div_pow2_I_18:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$18, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_18
	.private_extern _divmod_pow2__mod_pow2_I_18
_divmod_pow2__mod_pow2_I_18:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$262143, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_19
	.private_extern _divmod_pow2__div_pow2_I_19
_divmod_pow2__div_pow2_I_19:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$19, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_19
	.private_extern _divmod_pow2__mod_pow2_I_19
_divmod_pow2__mod_pow2_I_19:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$524287, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_20
	.private_extern _divmod_pow2__div_pow2_I_20
_divmod_pow2__div_pow2_I_20:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$20, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_20
	.private_extern _divmod_pow2__mod_pow2_I_20
_divmod_pow2__mod_pow2_I_20:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$1048575, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_21
	.private_extern _divmod_pow2__div_pow2_I_21
_divmod_pow2__div_pow2_I_21:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$21, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_21
	.private_extern _divmod_pow2__mod_pow2_I_21
_divmod_pow2__mod_pow2_I_21:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$2097151, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_22
	.private_extern _divmod_pow2__div_pow2_I_22
_divmod_pow2__div_pow2_I_22:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$22, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_22
	.private_extern _divmod_pow2__mod_pow2_I_22
_divmod_pow2__mod_pow2_I_22:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$4194303, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_23
	.private_extern _divmod_pow2__div_pow2_I_23
_divmod_pow2__div_pow2_I_23:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$23, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_23
	.private_extern _divmod_pow2__mod_pow2_I_23
_divmod_pow2__mod_pow2_I_23:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$8388607, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_24
	.private_extern _divmod_pow2__div_pow2_I_24
_divmod_pow2__div_pow2_I_24:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$24, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_24
	.private_extern _divmod_pow2__mod_pow2_I_24
_divmod_pow2__mod_pow2_I_24:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$16777215, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_25
	.private_extern _divmod_pow2__div_pow2_I_25
_divmod_pow2__div_pow2_I_25:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$25, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_25
	.private_extern _divmod_pow2__mod_pow2_I_25
_divmod_pow2__mod_pow2_I_25:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$33554431, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_26
	.private_extern _divmod_pow2__div_pow2_I_26
_divmod_pow2__div_pow2_I_26:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$26, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_26
	.private_extern _divmod_pow2__mod_pow2_I_26
_divmod_pow2__mod_pow2_I_26:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$67108863, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_27
	.private_extern _divmod_pow2__div_pow2_I_27
_divmod_pow2__div_pow2_I_27:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$27, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_27
	.private_extern _divmod_pow2__mod_pow2_I_27
_divmod_pow2__mod_pow2_I_27:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$134217727, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_28
	.private_extern _divmod_pow2__div_pow2_I_28
_divmod_pow2__div_pow2_I_28:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$28, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_28
	.private_extern _divmod_pow2__mod_pow2_I_28
_divmod_pow2__mod_pow2_I_28:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$268435455, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_29
	.private_extern _divmod_pow2__div_pow2_I_29
_divmod_pow2__div_pow2_I_29:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$29, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_29
	.private_extern _divmod_pow2__mod_pow2_I_29
_divmod_pow2__mod_pow2_I_29:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$536870911, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_30
	.private_extern _divmod_pow2__div_pow2_I_30
_divmod_pow2__div_pow2_I_30:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$30, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_30
	.private_extern _divmod_pow2__mod_pow2_I_30
_divmod_pow2__mod_pow2_I_30:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$1073741823, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_31
	.private_extern _divmod_pow2__div_pow2_I_31
_divmod_pow2__div_pow2_I_31:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$31, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_31
	.private_extern _divmod_pow2__mod_pow2_I_31
_divmod_pow2__mod_pow2_I_31:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$2147483647, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_32
	.private_extern _divmod_pow2__div_pow2_I_32
_divmod_pow2__div_pow2_I_32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$32, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_32
	.private_extern _divmod_pow2__mod_pow2_I_32
_divmod_pow2__mod_pow2_I_32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$4294967295, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_33
	.private_extern _divmod_pow2__div_pow2_I_33
_divmod_pow2__div_pow2_I_33:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$33, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_33
	.private_extern _divmod_pow2__mod_pow2_I_33
_divmod_pow2__mod_pow2_I_33:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$8589934591, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_34
	.private_extern _divmod_pow2__div_pow2_I_34
_divmod_pow2__div_pow2_I_34:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$34, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_34
	.private_extern _divmod_pow2__mod_pow2_I_34
_divmod_pow2__mod_pow2_I_34:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$17179869183, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_35
	.private_extern _divmod_pow2__div_pow2_I_35
_divmod_pow2__div_pow2_I_35:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$35, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_35
	.private_extern _divmod_pow2__mod_pow2_I_35
_divmod_pow2__mod_pow2_I_35:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$34359738367, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_36
	.private_extern _divmod_pow2__div_pow2_I_36
_divmod_pow2__div_pow2_I_36:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$36, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_36
	.private_extern _divmod_pow2__mod_pow2_I_36
_divmod_pow2__mod_pow2_I_36:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$68719476735, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_37
	.private_extern _divmod_pow2__div_pow2_I_37
_divmod_pow2__div_pow2_I_37:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$37, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_37
	.private_extern _divmod_pow2__mod_pow2_I_37
_divmod_pow2__mod_pow2_I_37:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$137438953471, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_38
	.private_extern _divmod_pow2__div_pow2_I_38
_divmod_pow2__div_pow2_I_38:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$38, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_38
	.private_extern _divmod_pow2__mod_pow2_I_38
_divmod_pow2__mod_pow2_I_38:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$274877906943, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_39
	.private_extern _divmod_pow2__div_pow2_I_39
_divmod_pow2__div_pow2_I_39:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$39, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_39
	.private_extern _divmod_pow2__mod_pow2_I_39
_divmod_pow2__mod_pow2_I_39:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$549755813887, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_40
	.private_extern _divmod_pow2__div_pow2_I_40
_divmod_pow2__div_pow2_I_40:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$40, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_40
	.private_extern _divmod_pow2__mod_pow2_I_40
_divmod_pow2__mod_pow2_I_40:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$1099511627775, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_41
	.private_extern _divmod_pow2__div_pow2_I_41
_divmod_pow2__div_pow2_I_41:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$41, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_41
	.private_extern _divmod_pow2__mod_pow2_I_41
_divmod_pow2__mod_pow2_I_41:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$2199023255551, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_42
	.private_extern _divmod_pow2__div_pow2_I_42
_divmod_pow2__div_pow2_I_42:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$42, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_42
	.private_extern _divmod_pow2__mod_pow2_I_42
_divmod_pow2__mod_pow2_I_42:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$4398046511103, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_43
	.private_extern _divmod_pow2__div_pow2_I_43
_divmod_pow2__div_pow2_I_43:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$43, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_43
	.private_extern _divmod_pow2__mod_pow2_I_43
_divmod_pow2__mod_pow2_I_43:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$8796093022207, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_44
	.private_extern _divmod_pow2__div_pow2_I_44
_divmod_pow2__div_pow2_I_44:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$44, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_44
	.private_extern _divmod_pow2__mod_pow2_I_44
_divmod_pow2__mod_pow2_I_44:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$17592186044415, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_45
	.private_extern _divmod_pow2__div_pow2_I_45
_divmod_pow2__div_pow2_I_45:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$45, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_45
	.private_extern _divmod_pow2__mod_pow2_I_45
_divmod_pow2__mod_pow2_I_45:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$35184372088831, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_46
	.private_extern _divmod_pow2__div_pow2_I_46
_divmod_pow2__div_pow2_I_46:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$46, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_46
	.private_extern _divmod_pow2__mod_pow2_I_46
_divmod_pow2__mod_pow2_I_46:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$70368744177663, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_47
	.private_extern _divmod_pow2__div_pow2_I_47
_divmod_pow2__div_pow2_I_47:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$47, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_47
	.private_extern _divmod_pow2__mod_pow2_I_47
_divmod_pow2__mod_pow2_I_47:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$140737488355327, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_48
	.private_extern _divmod_pow2__div_pow2_I_48
_divmod_pow2__div_pow2_I_48:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$48, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_48
	.private_extern _divmod_pow2__mod_pow2_I_48
_divmod_pow2__mod_pow2_I_48:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$281474976710655, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_49
	.private_extern _divmod_pow2__div_pow2_I_49
_divmod_pow2__div_pow2_I_49:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$49, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_49
	.private_extern _divmod_pow2__mod_pow2_I_49
_divmod_pow2__mod_pow2_I_49:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$562949953421311, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_50
	.private_extern _divmod_pow2__div_pow2_I_50
_divmod_pow2__div_pow2_I_50:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$50, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_50
	.private_extern _divmod_pow2__mod_pow2_I_50
_divmod_pow2__mod_pow2_I_50:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$1125899906842623, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_51
	.private_extern _divmod_pow2__div_pow2_I_51
_divmod_pow2__div_pow2_I_51:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$51, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_51
	.private_extern _divmod_pow2__mod_pow2_I_51
_divmod_pow2__mod_pow2_I_51:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$2251799813685247, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_52
	.private_extern _divmod_pow2__div_pow2_I_52
_divmod_pow2__div_pow2_I_52:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$52, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_52
	.private_extern _divmod_pow2__mod_pow2_I_52
_divmod_pow2__mod_pow2_I_52:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$4503599627370495, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_53
	.private_extern _divmod_pow2__div_pow2_I_53
_divmod_pow2__div_pow2_I_53:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$53, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_53
	.private_extern _divmod_pow2__mod_pow2_I_53
_divmod_pow2__mod_pow2_I_53:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$9007199254740991, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_54
	.private_extern _divmod_pow2__div_pow2_I_54
_divmod_pow2__div_pow2_I_54:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$54, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_54
	.private_extern _divmod_pow2__mod_pow2_I_54
_divmod_pow2__mod_pow2_I_54:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$18014398509481983, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_55
	.private_extern _divmod_pow2__div_pow2_I_55
_divmod_pow2__div_pow2_I_55:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$55, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_55
	.private_extern _divmod_pow2__mod_pow2_I_55
_divmod_pow2__mod_pow2_I_55:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$36028797018963967, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_56
	.private_extern _divmod_pow2__div_pow2_I_56
_divmod_pow2__div_pow2_I_56:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$56, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_56
	.private_extern _divmod_pow2__mod_pow2_I_56
_divmod_pow2__mod_pow2_I_56:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$72057594037927935, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_57
	.private_extern _divmod_pow2__div_pow2_I_57
_divmod_pow2__div_pow2_I_57:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$57, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_57
	.private_extern _divmod_pow2__mod_pow2_I_57
_divmod_pow2__mod_pow2_I_57:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$144115188075855871, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_58
	.private_extern _divmod_pow2__div_pow2_I_58
_divmod_pow2__div_pow2_I_58:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$58, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_58
	.private_extern _divmod_pow2__mod_pow2_I_58
_divmod_pow2__mod_pow2_I_58:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$288230376151711743, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_59
	.private_extern _divmod_pow2__div_pow2_I_59
_divmod_pow2__div_pow2_I_59:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$59, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_59
	.private_extern _divmod_pow2__mod_pow2_I_59
_divmod_pow2__mod_pow2_I_59:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$576460752303423487, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_60
	.private_extern _divmod_pow2__div_pow2_I_60
_divmod_pow2__div_pow2_I_60:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$60, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_60
	.private_extern _divmod_pow2__mod_pow2_I_60
_divmod_pow2__mod_pow2_I_60:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$1152921504606846975, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_61
	.private_extern _divmod_pow2__div_pow2_I_61
_divmod_pow2__div_pow2_I_61:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$61, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_61
	.private_extern _divmod_pow2__mod_pow2_I_61
_divmod_pow2__mod_pow2_I_61:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$2305843009213693951, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_62
	.private_extern _divmod_pow2__div_pow2_I_62
_divmod_pow2__div_pow2_I_62:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$62, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_I_62
	.private_extern _divmod_pow2__mod_pow2_I_62
_divmod_pow2__mod_pow2_I_62:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$4611686018427387903, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_I_63
	.private_extern _divmod_pow2__div_pow2_I_63
_divmod_pow2__div_pow2_I_63:
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
.globl _divmod_pow2__mod_pow2_I_63
	.private_extern _divmod_pow2__mod_pow2_I_63
_divmod_pow2__mod_pow2_I_63:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rcx
	leaq	-1(%rcx), %rdx
	movq	%rcx, %rax
	orq	%rdx, %rax
	movq	%rax, %rsi
	sarq	$63, %rsi
	subq	%rsi, %rdx
	movabsq	$-9223372036854775808, %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %edx
	movq	%rsi, %rax
	notq	%rax
	leaq	(%rdx,%rax), %rax
	salq	$63, %rax
	movq	%rcx, %rdx
	subq	%rax, %rdx
	movq	%rdx, %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_0
	.private_extern _divmod_pow2__div_pow2_C_0
_divmod_pow2__div_pow2_C_0:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1154
	movl	$34529, %edi
	call	__m3_fault
L1154:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_0
	.private_extern _divmod_pow2__mod_pow2_C_0
_divmod_pow2__mod_pow2_C_0:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	$0, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_1
	.private_extern _divmod_pow2__div_pow2_C_1
_divmod_pow2__div_pow2_C_1:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	%rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1159
	movl	$34657, %edi
	call	__m3_fault
L1159:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_1
	.private_extern _divmod_pow2__mod_pow2_C_1
_divmod_pow2__mod_pow2_C_1:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$1, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_2
	.private_extern _divmod_pow2__div_pow2_C_2
_divmod_pow2__div_pow2_C_2:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$2, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1164
	movl	$34785, %edi
	call	__m3_fault
L1164:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_2
	.private_extern _divmod_pow2__mod_pow2_C_2
_divmod_pow2__mod_pow2_C_2:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$3, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_3
	.private_extern _divmod_pow2__div_pow2_C_3
_divmod_pow2__div_pow2_C_3:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$3, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1169
	movl	$34913, %edi
	call	__m3_fault
L1169:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_3
	.private_extern _divmod_pow2__mod_pow2_C_3
_divmod_pow2__mod_pow2_C_3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$7, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_4
	.private_extern _divmod_pow2__div_pow2_C_4
_divmod_pow2__div_pow2_C_4:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$4, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1174
	movl	$35041, %edi
	call	__m3_fault
L1174:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_4
	.private_extern _divmod_pow2__mod_pow2_C_4
_divmod_pow2__mod_pow2_C_4:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$15, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_5
	.private_extern _divmod_pow2__div_pow2_C_5
_divmod_pow2__div_pow2_C_5:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$5, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1179
	movl	$35169, %edi
	call	__m3_fault
L1179:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_5
	.private_extern _divmod_pow2__mod_pow2_C_5
_divmod_pow2__mod_pow2_C_5:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$31, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_6
	.private_extern _divmod_pow2__div_pow2_C_6
_divmod_pow2__div_pow2_C_6:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$6, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1184
	movl	$35297, %edi
	call	__m3_fault
L1184:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_6
	.private_extern _divmod_pow2__mod_pow2_C_6
_divmod_pow2__mod_pow2_C_6:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$63, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_7
	.private_extern _divmod_pow2__div_pow2_C_7
_divmod_pow2__div_pow2_C_7:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$7, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1189
	movl	$35425, %edi
	call	__m3_fault
L1189:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_7
	.private_extern _divmod_pow2__mod_pow2_C_7
_divmod_pow2__mod_pow2_C_7:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$127, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_8
	.private_extern _divmod_pow2__div_pow2_C_8
_divmod_pow2__div_pow2_C_8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$8, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1194
	movl	$35553, %edi
	call	__m3_fault
L1194:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_8
	.private_extern _divmod_pow2__mod_pow2_C_8
_divmod_pow2__mod_pow2_C_8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$255, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_9
	.private_extern _divmod_pow2__div_pow2_C_9
_divmod_pow2__div_pow2_C_9:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$9, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1199
	movl	$35681, %edi
	call	__m3_fault
L1199:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_9
	.private_extern _divmod_pow2__mod_pow2_C_9
_divmod_pow2__mod_pow2_C_9:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$511, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_10
	.private_extern _divmod_pow2__div_pow2_C_10
_divmod_pow2__div_pow2_C_10:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$10, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1204
	movl	$35809, %edi
	call	__m3_fault
L1204:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_10
	.private_extern _divmod_pow2__mod_pow2_C_10
_divmod_pow2__mod_pow2_C_10:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$1023, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_11
	.private_extern _divmod_pow2__div_pow2_C_11
_divmod_pow2__div_pow2_C_11:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$11, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1209
	movl	$35937, %edi
	call	__m3_fault
L1209:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_11
	.private_extern _divmod_pow2__mod_pow2_C_11
_divmod_pow2__mod_pow2_C_11:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$2047, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_12
	.private_extern _divmod_pow2__div_pow2_C_12
_divmod_pow2__div_pow2_C_12:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$12, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1214
	movl	$36065, %edi
	call	__m3_fault
L1214:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_12
	.private_extern _divmod_pow2__mod_pow2_C_12
_divmod_pow2__mod_pow2_C_12:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$4095, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_13
	.private_extern _divmod_pow2__div_pow2_C_13
_divmod_pow2__div_pow2_C_13:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$13, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1219
	movl	$36193, %edi
	call	__m3_fault
L1219:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_13
	.private_extern _divmod_pow2__mod_pow2_C_13
_divmod_pow2__mod_pow2_C_13:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$8191, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_14
	.private_extern _divmod_pow2__div_pow2_C_14
_divmod_pow2__div_pow2_C_14:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$14, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1224
	movl	$36321, %edi
	call	__m3_fault
L1224:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_14
	.private_extern _divmod_pow2__mod_pow2_C_14
_divmod_pow2__mod_pow2_C_14:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$16383, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_15
	.private_extern _divmod_pow2__div_pow2_C_15
_divmod_pow2__div_pow2_C_15:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$15, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1229
	movl	$36449, %edi
	call	__m3_fault
L1229:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_15
	.private_extern _divmod_pow2__mod_pow2_C_15
_divmod_pow2__mod_pow2_C_15:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$32767, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_16
	.private_extern _divmod_pow2__div_pow2_C_16
_divmod_pow2__div_pow2_C_16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$16, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1234
	movl	$36577, %edi
	call	__m3_fault
L1234:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_16
	.private_extern _divmod_pow2__mod_pow2_C_16
_divmod_pow2__mod_pow2_C_16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$65535, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_17
	.private_extern _divmod_pow2__div_pow2_C_17
_divmod_pow2__div_pow2_C_17:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$17, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1239
	movl	$36705, %edi
	call	__m3_fault
L1239:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_17
	.private_extern _divmod_pow2__mod_pow2_C_17
_divmod_pow2__mod_pow2_C_17:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$131071, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_18
	.private_extern _divmod_pow2__div_pow2_C_18
_divmod_pow2__div_pow2_C_18:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$18, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1244
	movl	$36833, %edi
	call	__m3_fault
L1244:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_18
	.private_extern _divmod_pow2__mod_pow2_C_18
_divmod_pow2__mod_pow2_C_18:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$262143, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_19
	.private_extern _divmod_pow2__div_pow2_C_19
_divmod_pow2__div_pow2_C_19:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$19, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1249
	movl	$36961, %edi
	call	__m3_fault
L1249:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_19
	.private_extern _divmod_pow2__mod_pow2_C_19
_divmod_pow2__mod_pow2_C_19:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$524287, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_20
	.private_extern _divmod_pow2__div_pow2_C_20
_divmod_pow2__div_pow2_C_20:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$20, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1254
	movl	$37089, %edi
	call	__m3_fault
L1254:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_20
	.private_extern _divmod_pow2__mod_pow2_C_20
_divmod_pow2__mod_pow2_C_20:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$1048575, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_21
	.private_extern _divmod_pow2__div_pow2_C_21
_divmod_pow2__div_pow2_C_21:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$21, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1259
	movl	$37217, %edi
	call	__m3_fault
L1259:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_21
	.private_extern _divmod_pow2__mod_pow2_C_21
_divmod_pow2__mod_pow2_C_21:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$2097151, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_22
	.private_extern _divmod_pow2__div_pow2_C_22
_divmod_pow2__div_pow2_C_22:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$22, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1264
	movl	$37345, %edi
	call	__m3_fault
L1264:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_22
	.private_extern _divmod_pow2__mod_pow2_C_22
_divmod_pow2__mod_pow2_C_22:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$4194303, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_23
	.private_extern _divmod_pow2__div_pow2_C_23
_divmod_pow2__div_pow2_C_23:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$23, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1269
	movl	$37473, %edi
	call	__m3_fault
L1269:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_23
	.private_extern _divmod_pow2__mod_pow2_C_23
_divmod_pow2__mod_pow2_C_23:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$8388607, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_24
	.private_extern _divmod_pow2__div_pow2_C_24
_divmod_pow2__div_pow2_C_24:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$24, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1274
	movl	$37601, %edi
	call	__m3_fault
L1274:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_24
	.private_extern _divmod_pow2__mod_pow2_C_24
_divmod_pow2__mod_pow2_C_24:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$16777215, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_25
	.private_extern _divmod_pow2__div_pow2_C_25
_divmod_pow2__div_pow2_C_25:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$25, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1279
	movl	$37729, %edi
	call	__m3_fault
L1279:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_25
	.private_extern _divmod_pow2__mod_pow2_C_25
_divmod_pow2__mod_pow2_C_25:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$33554431, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_26
	.private_extern _divmod_pow2__div_pow2_C_26
_divmod_pow2__div_pow2_C_26:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$26, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1284
	movl	$37857, %edi
	call	__m3_fault
L1284:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_26
	.private_extern _divmod_pow2__mod_pow2_C_26
_divmod_pow2__mod_pow2_C_26:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$67108863, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_27
	.private_extern _divmod_pow2__div_pow2_C_27
_divmod_pow2__div_pow2_C_27:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$27, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1289
	movl	$37985, %edi
	call	__m3_fault
L1289:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_27
	.private_extern _divmod_pow2__mod_pow2_C_27
_divmod_pow2__mod_pow2_C_27:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$134217727, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_28
	.private_extern _divmod_pow2__div_pow2_C_28
_divmod_pow2__div_pow2_C_28:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$28, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1294
	movl	$38113, %edi
	call	__m3_fault
L1294:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_28
	.private_extern _divmod_pow2__mod_pow2_C_28
_divmod_pow2__mod_pow2_C_28:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$268435455, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_29
	.private_extern _divmod_pow2__div_pow2_C_29
_divmod_pow2__div_pow2_C_29:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$29, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1299
	movl	$38241, %edi
	call	__m3_fault
L1299:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_29
	.private_extern _divmod_pow2__mod_pow2_C_29
_divmod_pow2__mod_pow2_C_29:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$536870911, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_30
	.private_extern _divmod_pow2__div_pow2_C_30
_divmod_pow2__div_pow2_C_30:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$30, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1304
	movl	$38369, %edi
	call	__m3_fault
L1304:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_30
	.private_extern _divmod_pow2__mod_pow2_C_30
_divmod_pow2__mod_pow2_C_30:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$1073741823, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_31
	.private_extern _divmod_pow2__div_pow2_C_31
_divmod_pow2__div_pow2_C_31:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$31, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1309
	movl	$38497, %edi
	call	__m3_fault
L1309:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_31
	.private_extern _divmod_pow2__mod_pow2_C_31
_divmod_pow2__mod_pow2_C_31:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$2147483647, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_32
	.private_extern _divmod_pow2__div_pow2_C_32
_divmod_pow2__div_pow2_C_32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$32, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1314
	movl	$38625, %edi
	call	__m3_fault
L1314:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_32
	.private_extern _divmod_pow2__mod_pow2_C_32
_divmod_pow2__mod_pow2_C_32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$4294967295, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_33
	.private_extern _divmod_pow2__div_pow2_C_33
_divmod_pow2__div_pow2_C_33:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$33, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1319
	movl	$38753, %edi
	call	__m3_fault
L1319:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_33
	.private_extern _divmod_pow2__mod_pow2_C_33
_divmod_pow2__mod_pow2_C_33:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$8589934591, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_34
	.private_extern _divmod_pow2__div_pow2_C_34
_divmod_pow2__div_pow2_C_34:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$34, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1324
	movl	$38881, %edi
	call	__m3_fault
L1324:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_34
	.private_extern _divmod_pow2__mod_pow2_C_34
_divmod_pow2__mod_pow2_C_34:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$17179869183, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_35
	.private_extern _divmod_pow2__div_pow2_C_35
_divmod_pow2__div_pow2_C_35:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$35, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1329
	movl	$39009, %edi
	call	__m3_fault
L1329:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_35
	.private_extern _divmod_pow2__mod_pow2_C_35
_divmod_pow2__mod_pow2_C_35:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$34359738367, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_36
	.private_extern _divmod_pow2__div_pow2_C_36
_divmod_pow2__div_pow2_C_36:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$36, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1334
	movl	$39137, %edi
	call	__m3_fault
L1334:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_36
	.private_extern _divmod_pow2__mod_pow2_C_36
_divmod_pow2__mod_pow2_C_36:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$68719476735, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_37
	.private_extern _divmod_pow2__div_pow2_C_37
_divmod_pow2__div_pow2_C_37:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$37, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1339
	movl	$39265, %edi
	call	__m3_fault
L1339:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_37
	.private_extern _divmod_pow2__mod_pow2_C_37
_divmod_pow2__mod_pow2_C_37:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$137438953471, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_38
	.private_extern _divmod_pow2__div_pow2_C_38
_divmod_pow2__div_pow2_C_38:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$38, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1344
	movl	$39393, %edi
	call	__m3_fault
L1344:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_38
	.private_extern _divmod_pow2__mod_pow2_C_38
_divmod_pow2__mod_pow2_C_38:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$274877906943, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_39
	.private_extern _divmod_pow2__div_pow2_C_39
_divmod_pow2__div_pow2_C_39:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$39, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1349
	movl	$39521, %edi
	call	__m3_fault
L1349:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_39
	.private_extern _divmod_pow2__mod_pow2_C_39
_divmod_pow2__mod_pow2_C_39:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$549755813887, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_40
	.private_extern _divmod_pow2__div_pow2_C_40
_divmod_pow2__div_pow2_C_40:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$40, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1354
	movl	$39649, %edi
	call	__m3_fault
L1354:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_40
	.private_extern _divmod_pow2__mod_pow2_C_40
_divmod_pow2__mod_pow2_C_40:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$1099511627775, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_41
	.private_extern _divmod_pow2__div_pow2_C_41
_divmod_pow2__div_pow2_C_41:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$41, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1359
	movl	$39777, %edi
	call	__m3_fault
L1359:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_41
	.private_extern _divmod_pow2__mod_pow2_C_41
_divmod_pow2__mod_pow2_C_41:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$2199023255551, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_42
	.private_extern _divmod_pow2__div_pow2_C_42
_divmod_pow2__div_pow2_C_42:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$42, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1364
	movl	$39905, %edi
	call	__m3_fault
L1364:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_42
	.private_extern _divmod_pow2__mod_pow2_C_42
_divmod_pow2__mod_pow2_C_42:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$4398046511103, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_43
	.private_extern _divmod_pow2__div_pow2_C_43
_divmod_pow2__div_pow2_C_43:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$43, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1369
	movl	$40033, %edi
	call	__m3_fault
L1369:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_43
	.private_extern _divmod_pow2__mod_pow2_C_43
_divmod_pow2__mod_pow2_C_43:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$8796093022207, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_44
	.private_extern _divmod_pow2__div_pow2_C_44
_divmod_pow2__div_pow2_C_44:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$44, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1374
	movl	$40161, %edi
	call	__m3_fault
L1374:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_44
	.private_extern _divmod_pow2__mod_pow2_C_44
_divmod_pow2__mod_pow2_C_44:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$17592186044415, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_45
	.private_extern _divmod_pow2__div_pow2_C_45
_divmod_pow2__div_pow2_C_45:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$45, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1379
	movl	$40289, %edi
	call	__m3_fault
L1379:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_45
	.private_extern _divmod_pow2__mod_pow2_C_45
_divmod_pow2__mod_pow2_C_45:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$35184372088831, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_46
	.private_extern _divmod_pow2__div_pow2_C_46
_divmod_pow2__div_pow2_C_46:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$46, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1384
	movl	$40417, %edi
	call	__m3_fault
L1384:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_46
	.private_extern _divmod_pow2__mod_pow2_C_46
_divmod_pow2__mod_pow2_C_46:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$70368744177663, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_47
	.private_extern _divmod_pow2__div_pow2_C_47
_divmod_pow2__div_pow2_C_47:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$47, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1389
	movl	$40545, %edi
	call	__m3_fault
L1389:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_47
	.private_extern _divmod_pow2__mod_pow2_C_47
_divmod_pow2__mod_pow2_C_47:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$140737488355327, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_48
	.private_extern _divmod_pow2__div_pow2_C_48
_divmod_pow2__div_pow2_C_48:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$48, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1394
	movl	$40673, %edi
	call	__m3_fault
L1394:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_48
	.private_extern _divmod_pow2__mod_pow2_C_48
_divmod_pow2__mod_pow2_C_48:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$281474976710655, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_49
	.private_extern _divmod_pow2__div_pow2_C_49
_divmod_pow2__div_pow2_C_49:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$49, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1399
	movl	$40801, %edi
	call	__m3_fault
L1399:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_49
	.private_extern _divmod_pow2__mod_pow2_C_49
_divmod_pow2__mod_pow2_C_49:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$562949953421311, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_50
	.private_extern _divmod_pow2__div_pow2_C_50
_divmod_pow2__div_pow2_C_50:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$50, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1404
	movl	$40929, %edi
	call	__m3_fault
L1404:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_50
	.private_extern _divmod_pow2__mod_pow2_C_50
_divmod_pow2__mod_pow2_C_50:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$1125899906842623, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_51
	.private_extern _divmod_pow2__div_pow2_C_51
_divmod_pow2__div_pow2_C_51:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$51, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1409
	movl	$41057, %edi
	call	__m3_fault
L1409:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_51
	.private_extern _divmod_pow2__mod_pow2_C_51
_divmod_pow2__mod_pow2_C_51:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$2251799813685247, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_52
	.private_extern _divmod_pow2__div_pow2_C_52
_divmod_pow2__div_pow2_C_52:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$52, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1414
	movl	$41185, %edi
	call	__m3_fault
L1414:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_52
	.private_extern _divmod_pow2__mod_pow2_C_52
_divmod_pow2__mod_pow2_C_52:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$4503599627370495, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_53
	.private_extern _divmod_pow2__div_pow2_C_53
_divmod_pow2__div_pow2_C_53:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$53, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1419
	movl	$41313, %edi
	call	__m3_fault
L1419:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_53
	.private_extern _divmod_pow2__mod_pow2_C_53
_divmod_pow2__mod_pow2_C_53:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$9007199254740991, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_54
	.private_extern _divmod_pow2__div_pow2_C_54
_divmod_pow2__div_pow2_C_54:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$54, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1424
	movl	$41441, %edi
	call	__m3_fault
L1424:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_54
	.private_extern _divmod_pow2__mod_pow2_C_54
_divmod_pow2__mod_pow2_C_54:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$18014398509481983, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_55
	.private_extern _divmod_pow2__div_pow2_C_55
_divmod_pow2__div_pow2_C_55:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$55, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1429
	movl	$41569, %edi
	call	__m3_fault
L1429:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_55
	.private_extern _divmod_pow2__mod_pow2_C_55
_divmod_pow2__mod_pow2_C_55:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$36028797018963967, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_56
	.private_extern _divmod_pow2__div_pow2_C_56
_divmod_pow2__div_pow2_C_56:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$56, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1434
	movl	$41697, %edi
	call	__m3_fault
L1434:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_56
	.private_extern _divmod_pow2__mod_pow2_C_56
_divmod_pow2__mod_pow2_C_56:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$72057594037927935, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_57
	.private_extern _divmod_pow2__div_pow2_C_57
_divmod_pow2__div_pow2_C_57:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$57, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1439
	movl	$41825, %edi
	call	__m3_fault
L1439:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_57
	.private_extern _divmod_pow2__mod_pow2_C_57
_divmod_pow2__mod_pow2_C_57:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$144115188075855871, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_58
	.private_extern _divmod_pow2__div_pow2_C_58
_divmod_pow2__div_pow2_C_58:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$58, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1444
	movl	$41953, %edi
	call	__m3_fault
L1444:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_58
	.private_extern _divmod_pow2__mod_pow2_C_58
_divmod_pow2__mod_pow2_C_58:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$288230376151711743, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_59
	.private_extern _divmod_pow2__div_pow2_C_59
_divmod_pow2__div_pow2_C_59:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$59, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1449
	movl	$42081, %edi
	call	__m3_fault
L1449:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_59
	.private_extern _divmod_pow2__mod_pow2_C_59
_divmod_pow2__mod_pow2_C_59:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$576460752303423487, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_60
	.private_extern _divmod_pow2__div_pow2_C_60
_divmod_pow2__div_pow2_C_60:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$60, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1454
	movl	$42209, %edi
	call	__m3_fault
L1454:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_60
	.private_extern _divmod_pow2__mod_pow2_C_60
_divmod_pow2__mod_pow2_C_60:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$1152921504606846975, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_61
	.private_extern _divmod_pow2__div_pow2_C_61
_divmod_pow2__div_pow2_C_61:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$61, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1459
	movl	$42337, %edi
	call	__m3_fault
L1459:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_61
	.private_extern _divmod_pow2__mod_pow2_C_61
_divmod_pow2__mod_pow2_C_61:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$2305843009213693951, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_62
	.private_extern _divmod_pow2__div_pow2_C_62
_divmod_pow2__div_pow2_C_62:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$62, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1464
	movl	$42465, %edi
	call	__m3_fault
L1464:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_62
	.private_extern _divmod_pow2__mod_pow2_C_62
_divmod_pow2__mod_pow2_C_62:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$4611686018427387903, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_C_63
	.private_extern _divmod_pow2__div_pow2_C_63
_divmod_pow2__div_pow2_C_63:
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
	jns	L1469
	movl	$42593, %edi
	call	__m3_fault
L1469:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_C_63
	.private_extern _divmod_pow2__mod_pow2_C_63
_divmod_pow2__mod_pow2_C_63:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rcx
	leaq	-1(%rcx), %rdx
	movq	%rcx, %rax
	orq	%rdx, %rax
	movq	%rax, %rsi
	sarq	$63, %rsi
	subq	%rsi, %rdx
	movabsq	$-9223372036854775808, %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %edx
	movq	%rsi, %rax
	notq	%rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	salq	$63, %rax
	movq	%rcx, %rdx
	subq	%rax, %rdx
	movq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1472
	movl	$42657, %edi
	call	__m3_fault
L1472:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_0
	.private_extern _divmod_pow2__div_pow2_L_0
_divmod_pow2__div_pow2_L_0:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_0
	.private_extern _divmod_pow2__mod_pow2_L_0
_divmod_pow2__mod_pow2_L_0:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	$0, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_1
	.private_extern _divmod_pow2__div_pow2_L_1
_divmod_pow2__div_pow2_L_1:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	%rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_1
	.private_extern _divmod_pow2__mod_pow2_L_1
_divmod_pow2__mod_pow2_L_1:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$1, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_2
	.private_extern _divmod_pow2__div_pow2_L_2
_divmod_pow2__div_pow2_L_2:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$2, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_2
	.private_extern _divmod_pow2__mod_pow2_L_2
_divmod_pow2__mod_pow2_L_2:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$3, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_3
	.private_extern _divmod_pow2__div_pow2_L_3
_divmod_pow2__div_pow2_L_3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$3, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_3
	.private_extern _divmod_pow2__mod_pow2_L_3
_divmod_pow2__mod_pow2_L_3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$7, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_4
	.private_extern _divmod_pow2__div_pow2_L_4
_divmod_pow2__div_pow2_L_4:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$4, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_4
	.private_extern _divmod_pow2__mod_pow2_L_4
_divmod_pow2__mod_pow2_L_4:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$15, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_5
	.private_extern _divmod_pow2__div_pow2_L_5
_divmod_pow2__div_pow2_L_5:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$5, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_5
	.private_extern _divmod_pow2__mod_pow2_L_5
_divmod_pow2__mod_pow2_L_5:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$31, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_6
	.private_extern _divmod_pow2__div_pow2_L_6
_divmod_pow2__div_pow2_L_6:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$6, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_6
	.private_extern _divmod_pow2__mod_pow2_L_6
_divmod_pow2__mod_pow2_L_6:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$63, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_7
	.private_extern _divmod_pow2__div_pow2_L_7
_divmod_pow2__div_pow2_L_7:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$7, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_7
	.private_extern _divmod_pow2__mod_pow2_L_7
_divmod_pow2__mod_pow2_L_7:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$127, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_8
	.private_extern _divmod_pow2__div_pow2_L_8
_divmod_pow2__div_pow2_L_8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$8, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_8
	.private_extern _divmod_pow2__mod_pow2_L_8
_divmod_pow2__mod_pow2_L_8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$255, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_9
	.private_extern _divmod_pow2__div_pow2_L_9
_divmod_pow2__div_pow2_L_9:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$9, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_9
	.private_extern _divmod_pow2__mod_pow2_L_9
_divmod_pow2__mod_pow2_L_9:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$511, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_10
	.private_extern _divmod_pow2__div_pow2_L_10
_divmod_pow2__div_pow2_L_10:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$10, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_10
	.private_extern _divmod_pow2__mod_pow2_L_10
_divmod_pow2__mod_pow2_L_10:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$1023, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_11
	.private_extern _divmod_pow2__div_pow2_L_11
_divmod_pow2__div_pow2_L_11:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$11, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_11
	.private_extern _divmod_pow2__mod_pow2_L_11
_divmod_pow2__mod_pow2_L_11:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$2047, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_12
	.private_extern _divmod_pow2__div_pow2_L_12
_divmod_pow2__div_pow2_L_12:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$12, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_12
	.private_extern _divmod_pow2__mod_pow2_L_12
_divmod_pow2__mod_pow2_L_12:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$4095, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_13
	.private_extern _divmod_pow2__div_pow2_L_13
_divmod_pow2__div_pow2_L_13:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$13, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_13
	.private_extern _divmod_pow2__mod_pow2_L_13
_divmod_pow2__mod_pow2_L_13:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$8191, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_14
	.private_extern _divmod_pow2__div_pow2_L_14
_divmod_pow2__div_pow2_L_14:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$14, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_14
	.private_extern _divmod_pow2__mod_pow2_L_14
_divmod_pow2__mod_pow2_L_14:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$16383, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_15
	.private_extern _divmod_pow2__div_pow2_L_15
_divmod_pow2__div_pow2_L_15:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$15, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_15
	.private_extern _divmod_pow2__mod_pow2_L_15
_divmod_pow2__mod_pow2_L_15:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$32767, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_16
	.private_extern _divmod_pow2__div_pow2_L_16
_divmod_pow2__div_pow2_L_16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$16, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_16
	.private_extern _divmod_pow2__mod_pow2_L_16
_divmod_pow2__mod_pow2_L_16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$65535, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_17
	.private_extern _divmod_pow2__div_pow2_L_17
_divmod_pow2__div_pow2_L_17:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$17, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_17
	.private_extern _divmod_pow2__mod_pow2_L_17
_divmod_pow2__mod_pow2_L_17:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$131071, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_18
	.private_extern _divmod_pow2__div_pow2_L_18
_divmod_pow2__div_pow2_L_18:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$18, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_18
	.private_extern _divmod_pow2__mod_pow2_L_18
_divmod_pow2__mod_pow2_L_18:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$262143, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_19
	.private_extern _divmod_pow2__div_pow2_L_19
_divmod_pow2__div_pow2_L_19:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$19, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_19
	.private_extern _divmod_pow2__mod_pow2_L_19
_divmod_pow2__mod_pow2_L_19:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$524287, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_20
	.private_extern _divmod_pow2__div_pow2_L_20
_divmod_pow2__div_pow2_L_20:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$20, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_20
	.private_extern _divmod_pow2__mod_pow2_L_20
_divmod_pow2__mod_pow2_L_20:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$1048575, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_21
	.private_extern _divmod_pow2__div_pow2_L_21
_divmod_pow2__div_pow2_L_21:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$21, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_21
	.private_extern _divmod_pow2__mod_pow2_L_21
_divmod_pow2__mod_pow2_L_21:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$2097151, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_22
	.private_extern _divmod_pow2__div_pow2_L_22
_divmod_pow2__div_pow2_L_22:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$22, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_22
	.private_extern _divmod_pow2__mod_pow2_L_22
_divmod_pow2__mod_pow2_L_22:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$4194303, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_23
	.private_extern _divmod_pow2__div_pow2_L_23
_divmod_pow2__div_pow2_L_23:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$23, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_23
	.private_extern _divmod_pow2__mod_pow2_L_23
_divmod_pow2__mod_pow2_L_23:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$8388607, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_24
	.private_extern _divmod_pow2__div_pow2_L_24
_divmod_pow2__div_pow2_L_24:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$24, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_24
	.private_extern _divmod_pow2__mod_pow2_L_24
_divmod_pow2__mod_pow2_L_24:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$16777215, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_25
	.private_extern _divmod_pow2__div_pow2_L_25
_divmod_pow2__div_pow2_L_25:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$25, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_25
	.private_extern _divmod_pow2__mod_pow2_L_25
_divmod_pow2__mod_pow2_L_25:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$33554431, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_26
	.private_extern _divmod_pow2__div_pow2_L_26
_divmod_pow2__div_pow2_L_26:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$26, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_26
	.private_extern _divmod_pow2__mod_pow2_L_26
_divmod_pow2__mod_pow2_L_26:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$67108863, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_27
	.private_extern _divmod_pow2__div_pow2_L_27
_divmod_pow2__div_pow2_L_27:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$27, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_27
	.private_extern _divmod_pow2__mod_pow2_L_27
_divmod_pow2__mod_pow2_L_27:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$134217727, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_28
	.private_extern _divmod_pow2__div_pow2_L_28
_divmod_pow2__div_pow2_L_28:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$28, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_28
	.private_extern _divmod_pow2__mod_pow2_L_28
_divmod_pow2__mod_pow2_L_28:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$268435455, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_29
	.private_extern _divmod_pow2__div_pow2_L_29
_divmod_pow2__div_pow2_L_29:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$29, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_29
	.private_extern _divmod_pow2__mod_pow2_L_29
_divmod_pow2__mod_pow2_L_29:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$536870911, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_30
	.private_extern _divmod_pow2__div_pow2_L_30
_divmod_pow2__div_pow2_L_30:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$30, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_30
	.private_extern _divmod_pow2__mod_pow2_L_30
_divmod_pow2__mod_pow2_L_30:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$1073741823, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_31
	.private_extern _divmod_pow2__div_pow2_L_31
_divmod_pow2__div_pow2_L_31:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$31, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_31
	.private_extern _divmod_pow2__mod_pow2_L_31
_divmod_pow2__mod_pow2_L_31:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$2147483647, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_32
	.private_extern _divmod_pow2__div_pow2_L_32
_divmod_pow2__div_pow2_L_32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$32, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_32
	.private_extern _divmod_pow2__mod_pow2_L_32
_divmod_pow2__mod_pow2_L_32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$4294967295, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_33
	.private_extern _divmod_pow2__div_pow2_L_33
_divmod_pow2__div_pow2_L_33:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$33, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_33
	.private_extern _divmod_pow2__mod_pow2_L_33
_divmod_pow2__mod_pow2_L_33:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$8589934591, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_34
	.private_extern _divmod_pow2__div_pow2_L_34
_divmod_pow2__div_pow2_L_34:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$34, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_34
	.private_extern _divmod_pow2__mod_pow2_L_34
_divmod_pow2__mod_pow2_L_34:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$17179869183, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_35
	.private_extern _divmod_pow2__div_pow2_L_35
_divmod_pow2__div_pow2_L_35:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$35, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_35
	.private_extern _divmod_pow2__mod_pow2_L_35
_divmod_pow2__mod_pow2_L_35:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$34359738367, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_36
	.private_extern _divmod_pow2__div_pow2_L_36
_divmod_pow2__div_pow2_L_36:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$36, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_36
	.private_extern _divmod_pow2__mod_pow2_L_36
_divmod_pow2__mod_pow2_L_36:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$68719476735, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_37
	.private_extern _divmod_pow2__div_pow2_L_37
_divmod_pow2__div_pow2_L_37:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$37, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_37
	.private_extern _divmod_pow2__mod_pow2_L_37
_divmod_pow2__mod_pow2_L_37:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$137438953471, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_38
	.private_extern _divmod_pow2__div_pow2_L_38
_divmod_pow2__div_pow2_L_38:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$38, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_38
	.private_extern _divmod_pow2__mod_pow2_L_38
_divmod_pow2__mod_pow2_L_38:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$274877906943, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_39
	.private_extern _divmod_pow2__div_pow2_L_39
_divmod_pow2__div_pow2_L_39:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$39, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_39
	.private_extern _divmod_pow2__mod_pow2_L_39
_divmod_pow2__mod_pow2_L_39:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$549755813887, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_40
	.private_extern _divmod_pow2__div_pow2_L_40
_divmod_pow2__div_pow2_L_40:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$40, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_40
	.private_extern _divmod_pow2__mod_pow2_L_40
_divmod_pow2__mod_pow2_L_40:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$1099511627775, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_41
	.private_extern _divmod_pow2__div_pow2_L_41
_divmod_pow2__div_pow2_L_41:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$41, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_41
	.private_extern _divmod_pow2__mod_pow2_L_41
_divmod_pow2__mod_pow2_L_41:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$2199023255551, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_42
	.private_extern _divmod_pow2__div_pow2_L_42
_divmod_pow2__div_pow2_L_42:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$42, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_42
	.private_extern _divmod_pow2__mod_pow2_L_42
_divmod_pow2__mod_pow2_L_42:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$4398046511103, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_43
	.private_extern _divmod_pow2__div_pow2_L_43
_divmod_pow2__div_pow2_L_43:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$43, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_43
	.private_extern _divmod_pow2__mod_pow2_L_43
_divmod_pow2__mod_pow2_L_43:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$8796093022207, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_44
	.private_extern _divmod_pow2__div_pow2_L_44
_divmod_pow2__div_pow2_L_44:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$44, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_44
	.private_extern _divmod_pow2__mod_pow2_L_44
_divmod_pow2__mod_pow2_L_44:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$17592186044415, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_45
	.private_extern _divmod_pow2__div_pow2_L_45
_divmod_pow2__div_pow2_L_45:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$45, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_45
	.private_extern _divmod_pow2__mod_pow2_L_45
_divmod_pow2__mod_pow2_L_45:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$35184372088831, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_46
	.private_extern _divmod_pow2__div_pow2_L_46
_divmod_pow2__div_pow2_L_46:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$46, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_46
	.private_extern _divmod_pow2__mod_pow2_L_46
_divmod_pow2__mod_pow2_L_46:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$70368744177663, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_47
	.private_extern _divmod_pow2__div_pow2_L_47
_divmod_pow2__div_pow2_L_47:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$47, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_47
	.private_extern _divmod_pow2__mod_pow2_L_47
_divmod_pow2__mod_pow2_L_47:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$140737488355327, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_48
	.private_extern _divmod_pow2__div_pow2_L_48
_divmod_pow2__div_pow2_L_48:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$48, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_48
	.private_extern _divmod_pow2__mod_pow2_L_48
_divmod_pow2__mod_pow2_L_48:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$281474976710655, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_49
	.private_extern _divmod_pow2__div_pow2_L_49
_divmod_pow2__div_pow2_L_49:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$49, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_49
	.private_extern _divmod_pow2__mod_pow2_L_49
_divmod_pow2__mod_pow2_L_49:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$562949953421311, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_50
	.private_extern _divmod_pow2__div_pow2_L_50
_divmod_pow2__div_pow2_L_50:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$50, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_50
	.private_extern _divmod_pow2__mod_pow2_L_50
_divmod_pow2__mod_pow2_L_50:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$1125899906842623, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_51
	.private_extern _divmod_pow2__div_pow2_L_51
_divmod_pow2__div_pow2_L_51:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$51, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_51
	.private_extern _divmod_pow2__mod_pow2_L_51
_divmod_pow2__mod_pow2_L_51:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$2251799813685247, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_52
	.private_extern _divmod_pow2__div_pow2_L_52
_divmod_pow2__div_pow2_L_52:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$52, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_52
	.private_extern _divmod_pow2__mod_pow2_L_52
_divmod_pow2__mod_pow2_L_52:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$4503599627370495, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_53
	.private_extern _divmod_pow2__div_pow2_L_53
_divmod_pow2__div_pow2_L_53:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$53, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_53
	.private_extern _divmod_pow2__mod_pow2_L_53
_divmod_pow2__mod_pow2_L_53:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$9007199254740991, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_54
	.private_extern _divmod_pow2__div_pow2_L_54
_divmod_pow2__div_pow2_L_54:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$54, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_54
	.private_extern _divmod_pow2__mod_pow2_L_54
_divmod_pow2__mod_pow2_L_54:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$18014398509481983, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_55
	.private_extern _divmod_pow2__div_pow2_L_55
_divmod_pow2__div_pow2_L_55:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$55, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_55
	.private_extern _divmod_pow2__mod_pow2_L_55
_divmod_pow2__mod_pow2_L_55:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$36028797018963967, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_56
	.private_extern _divmod_pow2__div_pow2_L_56
_divmod_pow2__div_pow2_L_56:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$56, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_56
	.private_extern _divmod_pow2__mod_pow2_L_56
_divmod_pow2__mod_pow2_L_56:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$72057594037927935, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_57
	.private_extern _divmod_pow2__div_pow2_L_57
_divmod_pow2__div_pow2_L_57:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$57, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_57
	.private_extern _divmod_pow2__mod_pow2_L_57
_divmod_pow2__mod_pow2_L_57:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$144115188075855871, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_58
	.private_extern _divmod_pow2__div_pow2_L_58
_divmod_pow2__div_pow2_L_58:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$58, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_58
	.private_extern _divmod_pow2__mod_pow2_L_58
_divmod_pow2__mod_pow2_L_58:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$288230376151711743, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_59
	.private_extern _divmod_pow2__div_pow2_L_59
_divmod_pow2__div_pow2_L_59:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$59, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_59
	.private_extern _divmod_pow2__mod_pow2_L_59
_divmod_pow2__mod_pow2_L_59:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$576460752303423487, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_60
	.private_extern _divmod_pow2__div_pow2_L_60
_divmod_pow2__div_pow2_L_60:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$60, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_60
	.private_extern _divmod_pow2__mod_pow2_L_60
_divmod_pow2__mod_pow2_L_60:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$1152921504606846975, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_61
	.private_extern _divmod_pow2__div_pow2_L_61
_divmod_pow2__div_pow2_L_61:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$61, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_61
	.private_extern _divmod_pow2__mod_pow2_L_61
_divmod_pow2__mod_pow2_L_61:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$2305843009213693951, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_62
	.private_extern _divmod_pow2__div_pow2_L_62
_divmod_pow2__div_pow2_L_62:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$62, %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_L_62
	.private_extern _divmod_pow2__mod_pow2_L_62
_divmod_pow2__mod_pow2_L_62:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$4611686018427387903, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_L_63
	.private_extern _divmod_pow2__div_pow2_L_63
_divmod_pow2__div_pow2_L_63:
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
.globl _divmod_pow2__mod_pow2_L_63
	.private_extern _divmod_pow2__mod_pow2_L_63
_divmod_pow2__mod_pow2_L_63:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rcx
	leaq	-1(%rcx), %rdx
	movq	%rcx, %rax
	orq	%rdx, %rax
	movq	%rax, %rsi
	sarq	$63, %rsi
	subq	%rsi, %rdx
	movabsq	$-9223372036854775808, %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %edx
	movq	%rsi, %rax
	notq	%rax
	leaq	(%rdx,%rax), %rax
	salq	$63, %rax
	movq	%rcx, %rdx
	subq	%rax, %rdx
	movq	%rdx, %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_0
	.private_extern _divmod_pow2__div_pow2_LC_0
_divmod_pow2__div_pow2_LC_0:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1731
	movl	$50913, %edi
	call	__m3_fault
L1731:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_0
	.private_extern _divmod_pow2__mod_pow2_LC_0
_divmod_pow2__mod_pow2_LC_0:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	$0, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_1
	.private_extern _divmod_pow2__div_pow2_LC_1
_divmod_pow2__div_pow2_LC_1:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	%rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1736
	movl	$51041, %edi
	call	__m3_fault
L1736:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_1
	.private_extern _divmod_pow2__mod_pow2_LC_1
_divmod_pow2__mod_pow2_LC_1:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$1, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_2
	.private_extern _divmod_pow2__div_pow2_LC_2
_divmod_pow2__div_pow2_LC_2:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$2, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1741
	movl	$51169, %edi
	call	__m3_fault
L1741:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_2
	.private_extern _divmod_pow2__mod_pow2_LC_2
_divmod_pow2__mod_pow2_LC_2:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$3, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_3
	.private_extern _divmod_pow2__div_pow2_LC_3
_divmod_pow2__div_pow2_LC_3:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$3, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1746
	movl	$51297, %edi
	call	__m3_fault
L1746:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_3
	.private_extern _divmod_pow2__mod_pow2_LC_3
_divmod_pow2__mod_pow2_LC_3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$7, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_4
	.private_extern _divmod_pow2__div_pow2_LC_4
_divmod_pow2__div_pow2_LC_4:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$4, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1751
	movl	$51425, %edi
	call	__m3_fault
L1751:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_4
	.private_extern _divmod_pow2__mod_pow2_LC_4
_divmod_pow2__mod_pow2_LC_4:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$15, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_5
	.private_extern _divmod_pow2__div_pow2_LC_5
_divmod_pow2__div_pow2_LC_5:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$5, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1756
	movl	$51553, %edi
	call	__m3_fault
L1756:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_5
	.private_extern _divmod_pow2__mod_pow2_LC_5
_divmod_pow2__mod_pow2_LC_5:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$31, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_6
	.private_extern _divmod_pow2__div_pow2_LC_6
_divmod_pow2__div_pow2_LC_6:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$6, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1761
	movl	$51681, %edi
	call	__m3_fault
L1761:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_6
	.private_extern _divmod_pow2__mod_pow2_LC_6
_divmod_pow2__mod_pow2_LC_6:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$63, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_7
	.private_extern _divmod_pow2__div_pow2_LC_7
_divmod_pow2__div_pow2_LC_7:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$7, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1766
	movl	$51809, %edi
	call	__m3_fault
L1766:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_7
	.private_extern _divmod_pow2__mod_pow2_LC_7
_divmod_pow2__mod_pow2_LC_7:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$127, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_8
	.private_extern _divmod_pow2__div_pow2_LC_8
_divmod_pow2__div_pow2_LC_8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$8, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1771
	movl	$51937, %edi
	call	__m3_fault
L1771:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_8
	.private_extern _divmod_pow2__mod_pow2_LC_8
_divmod_pow2__mod_pow2_LC_8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$255, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_9
	.private_extern _divmod_pow2__div_pow2_LC_9
_divmod_pow2__div_pow2_LC_9:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$9, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1776
	movl	$52065, %edi
	call	__m3_fault
L1776:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_9
	.private_extern _divmod_pow2__mod_pow2_LC_9
_divmod_pow2__mod_pow2_LC_9:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$511, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_10
	.private_extern _divmod_pow2__div_pow2_LC_10
_divmod_pow2__div_pow2_LC_10:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$10, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1781
	movl	$52193, %edi
	call	__m3_fault
L1781:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_10
	.private_extern _divmod_pow2__mod_pow2_LC_10
_divmod_pow2__mod_pow2_LC_10:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$1023, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_11
	.private_extern _divmod_pow2__div_pow2_LC_11
_divmod_pow2__div_pow2_LC_11:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$11, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1786
	movl	$52321, %edi
	call	__m3_fault
L1786:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_11
	.private_extern _divmod_pow2__mod_pow2_LC_11
_divmod_pow2__mod_pow2_LC_11:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$2047, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_12
	.private_extern _divmod_pow2__div_pow2_LC_12
_divmod_pow2__div_pow2_LC_12:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$12, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1791
	movl	$52449, %edi
	call	__m3_fault
L1791:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_12
	.private_extern _divmod_pow2__mod_pow2_LC_12
_divmod_pow2__mod_pow2_LC_12:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$4095, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_13
	.private_extern _divmod_pow2__div_pow2_LC_13
_divmod_pow2__div_pow2_LC_13:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$13, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1796
	movl	$52577, %edi
	call	__m3_fault
L1796:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_13
	.private_extern _divmod_pow2__mod_pow2_LC_13
_divmod_pow2__mod_pow2_LC_13:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$8191, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_14
	.private_extern _divmod_pow2__div_pow2_LC_14
_divmod_pow2__div_pow2_LC_14:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$14, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1801
	movl	$52705, %edi
	call	__m3_fault
L1801:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_14
	.private_extern _divmod_pow2__mod_pow2_LC_14
_divmod_pow2__mod_pow2_LC_14:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$16383, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_15
	.private_extern _divmod_pow2__div_pow2_LC_15
_divmod_pow2__div_pow2_LC_15:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$15, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1806
	movl	$52833, %edi
	call	__m3_fault
L1806:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_15
	.private_extern _divmod_pow2__mod_pow2_LC_15
_divmod_pow2__mod_pow2_LC_15:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$32767, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_16
	.private_extern _divmod_pow2__div_pow2_LC_16
_divmod_pow2__div_pow2_LC_16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$16, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1811
	movl	$52961, %edi
	call	__m3_fault
L1811:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_16
	.private_extern _divmod_pow2__mod_pow2_LC_16
_divmod_pow2__mod_pow2_LC_16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$65535, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_17
	.private_extern _divmod_pow2__div_pow2_LC_17
_divmod_pow2__div_pow2_LC_17:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$17, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1816
	movl	$53089, %edi
	call	__m3_fault
L1816:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_17
	.private_extern _divmod_pow2__mod_pow2_LC_17
_divmod_pow2__mod_pow2_LC_17:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$131071, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_18
	.private_extern _divmod_pow2__div_pow2_LC_18
_divmod_pow2__div_pow2_LC_18:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$18, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1821
	movl	$53217, %edi
	call	__m3_fault
L1821:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_18
	.private_extern _divmod_pow2__mod_pow2_LC_18
_divmod_pow2__mod_pow2_LC_18:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$262143, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_19
	.private_extern _divmod_pow2__div_pow2_LC_19
_divmod_pow2__div_pow2_LC_19:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$19, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1826
	movl	$53345, %edi
	call	__m3_fault
L1826:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_19
	.private_extern _divmod_pow2__mod_pow2_LC_19
_divmod_pow2__mod_pow2_LC_19:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$524287, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_20
	.private_extern _divmod_pow2__div_pow2_LC_20
_divmod_pow2__div_pow2_LC_20:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$20, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1831
	movl	$53473, %edi
	call	__m3_fault
L1831:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_20
	.private_extern _divmod_pow2__mod_pow2_LC_20
_divmod_pow2__mod_pow2_LC_20:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$1048575, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_21
	.private_extern _divmod_pow2__div_pow2_LC_21
_divmod_pow2__div_pow2_LC_21:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$21, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1836
	movl	$53601, %edi
	call	__m3_fault
L1836:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_21
	.private_extern _divmod_pow2__mod_pow2_LC_21
_divmod_pow2__mod_pow2_LC_21:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$2097151, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_22
	.private_extern _divmod_pow2__div_pow2_LC_22
_divmod_pow2__div_pow2_LC_22:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$22, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1841
	movl	$53729, %edi
	call	__m3_fault
L1841:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_22
	.private_extern _divmod_pow2__mod_pow2_LC_22
_divmod_pow2__mod_pow2_LC_22:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$4194303, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_23
	.private_extern _divmod_pow2__div_pow2_LC_23
_divmod_pow2__div_pow2_LC_23:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$23, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1846
	movl	$53857, %edi
	call	__m3_fault
L1846:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_23
	.private_extern _divmod_pow2__mod_pow2_LC_23
_divmod_pow2__mod_pow2_LC_23:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$8388607, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_24
	.private_extern _divmod_pow2__div_pow2_LC_24
_divmod_pow2__div_pow2_LC_24:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$24, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1851
	movl	$53985, %edi
	call	__m3_fault
L1851:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_24
	.private_extern _divmod_pow2__mod_pow2_LC_24
_divmod_pow2__mod_pow2_LC_24:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$16777215, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_25
	.private_extern _divmod_pow2__div_pow2_LC_25
_divmod_pow2__div_pow2_LC_25:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$25, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1856
	movl	$54113, %edi
	call	__m3_fault
L1856:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_25
	.private_extern _divmod_pow2__mod_pow2_LC_25
_divmod_pow2__mod_pow2_LC_25:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$33554431, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_26
	.private_extern _divmod_pow2__div_pow2_LC_26
_divmod_pow2__div_pow2_LC_26:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$26, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1861
	movl	$54241, %edi
	call	__m3_fault
L1861:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_26
	.private_extern _divmod_pow2__mod_pow2_LC_26
_divmod_pow2__mod_pow2_LC_26:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$67108863, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_27
	.private_extern _divmod_pow2__div_pow2_LC_27
_divmod_pow2__div_pow2_LC_27:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$27, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1866
	movl	$54369, %edi
	call	__m3_fault
L1866:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_27
	.private_extern _divmod_pow2__mod_pow2_LC_27
_divmod_pow2__mod_pow2_LC_27:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$134217727, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_28
	.private_extern _divmod_pow2__div_pow2_LC_28
_divmod_pow2__div_pow2_LC_28:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$28, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1871
	movl	$54497, %edi
	call	__m3_fault
L1871:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_28
	.private_extern _divmod_pow2__mod_pow2_LC_28
_divmod_pow2__mod_pow2_LC_28:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$268435455, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_29
	.private_extern _divmod_pow2__div_pow2_LC_29
_divmod_pow2__div_pow2_LC_29:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$29, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1876
	movl	$54625, %edi
	call	__m3_fault
L1876:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_29
	.private_extern _divmod_pow2__mod_pow2_LC_29
_divmod_pow2__mod_pow2_LC_29:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$536870911, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_30
	.private_extern _divmod_pow2__div_pow2_LC_30
_divmod_pow2__div_pow2_LC_30:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$30, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1881
	movl	$54753, %edi
	call	__m3_fault
L1881:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_30
	.private_extern _divmod_pow2__mod_pow2_LC_30
_divmod_pow2__mod_pow2_LC_30:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$1073741823, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_31
	.private_extern _divmod_pow2__div_pow2_LC_31
_divmod_pow2__div_pow2_LC_31:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$31, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1886
	movl	$54881, %edi
	call	__m3_fault
L1886:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_31
	.private_extern _divmod_pow2__mod_pow2_LC_31
_divmod_pow2__mod_pow2_LC_31:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$2147483647, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_32
	.private_extern _divmod_pow2__div_pow2_LC_32
_divmod_pow2__div_pow2_LC_32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$32, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1891
	movl	$55009, %edi
	call	__m3_fault
L1891:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_32
	.private_extern _divmod_pow2__mod_pow2_LC_32
_divmod_pow2__mod_pow2_LC_32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	andl	$4294967295, %eax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_33
	.private_extern _divmod_pow2__div_pow2_LC_33
_divmod_pow2__div_pow2_LC_33:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$33, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1896
	movl	$55137, %edi
	call	__m3_fault
L1896:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_33
	.private_extern _divmod_pow2__mod_pow2_LC_33
_divmod_pow2__mod_pow2_LC_33:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$8589934591, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_34
	.private_extern _divmod_pow2__div_pow2_LC_34
_divmod_pow2__div_pow2_LC_34:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$34, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1901
	movl	$55265, %edi
	call	__m3_fault
L1901:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_34
	.private_extern _divmod_pow2__mod_pow2_LC_34
_divmod_pow2__mod_pow2_LC_34:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$17179869183, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_35
	.private_extern _divmod_pow2__div_pow2_LC_35
_divmod_pow2__div_pow2_LC_35:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$35, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1906
	movl	$55393, %edi
	call	__m3_fault
L1906:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_35
	.private_extern _divmod_pow2__mod_pow2_LC_35
_divmod_pow2__mod_pow2_LC_35:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$34359738367, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_36
	.private_extern _divmod_pow2__div_pow2_LC_36
_divmod_pow2__div_pow2_LC_36:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$36, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1911
	movl	$55521, %edi
	call	__m3_fault
L1911:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_36
	.private_extern _divmod_pow2__mod_pow2_LC_36
_divmod_pow2__mod_pow2_LC_36:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$68719476735, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_37
	.private_extern _divmod_pow2__div_pow2_LC_37
_divmod_pow2__div_pow2_LC_37:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$37, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1916
	movl	$55649, %edi
	call	__m3_fault
L1916:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_37
	.private_extern _divmod_pow2__mod_pow2_LC_37
_divmod_pow2__mod_pow2_LC_37:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$137438953471, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_38
	.private_extern _divmod_pow2__div_pow2_LC_38
_divmod_pow2__div_pow2_LC_38:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$38, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1921
	movl	$55777, %edi
	call	__m3_fault
L1921:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_38
	.private_extern _divmod_pow2__mod_pow2_LC_38
_divmod_pow2__mod_pow2_LC_38:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$274877906943, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_39
	.private_extern _divmod_pow2__div_pow2_LC_39
_divmod_pow2__div_pow2_LC_39:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$39, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1926
	movl	$55905, %edi
	call	__m3_fault
L1926:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_39
	.private_extern _divmod_pow2__mod_pow2_LC_39
_divmod_pow2__mod_pow2_LC_39:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$549755813887, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_40
	.private_extern _divmod_pow2__div_pow2_LC_40
_divmod_pow2__div_pow2_LC_40:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$40, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1931
	movl	$56033, %edi
	call	__m3_fault
L1931:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_40
	.private_extern _divmod_pow2__mod_pow2_LC_40
_divmod_pow2__mod_pow2_LC_40:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$1099511627775, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_41
	.private_extern _divmod_pow2__div_pow2_LC_41
_divmod_pow2__div_pow2_LC_41:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$41, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1936
	movl	$56161, %edi
	call	__m3_fault
L1936:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_41
	.private_extern _divmod_pow2__mod_pow2_LC_41
_divmod_pow2__mod_pow2_LC_41:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$2199023255551, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_42
	.private_extern _divmod_pow2__div_pow2_LC_42
_divmod_pow2__div_pow2_LC_42:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$42, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1941
	movl	$56289, %edi
	call	__m3_fault
L1941:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_42
	.private_extern _divmod_pow2__mod_pow2_LC_42
_divmod_pow2__mod_pow2_LC_42:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$4398046511103, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_43
	.private_extern _divmod_pow2__div_pow2_LC_43
_divmod_pow2__div_pow2_LC_43:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$43, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1946
	movl	$56417, %edi
	call	__m3_fault
L1946:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_43
	.private_extern _divmod_pow2__mod_pow2_LC_43
_divmod_pow2__mod_pow2_LC_43:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$8796093022207, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_44
	.private_extern _divmod_pow2__div_pow2_LC_44
_divmod_pow2__div_pow2_LC_44:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$44, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1951
	movl	$56545, %edi
	call	__m3_fault
L1951:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_44
	.private_extern _divmod_pow2__mod_pow2_LC_44
_divmod_pow2__mod_pow2_LC_44:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$17592186044415, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_45
	.private_extern _divmod_pow2__div_pow2_LC_45
_divmod_pow2__div_pow2_LC_45:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$45, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1956
	movl	$56673, %edi
	call	__m3_fault
L1956:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_45
	.private_extern _divmod_pow2__mod_pow2_LC_45
_divmod_pow2__mod_pow2_LC_45:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$35184372088831, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_46
	.private_extern _divmod_pow2__div_pow2_LC_46
_divmod_pow2__div_pow2_LC_46:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$46, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1961
	movl	$56801, %edi
	call	__m3_fault
L1961:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_46
	.private_extern _divmod_pow2__mod_pow2_LC_46
_divmod_pow2__mod_pow2_LC_46:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$70368744177663, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_47
	.private_extern _divmod_pow2__div_pow2_LC_47
_divmod_pow2__div_pow2_LC_47:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$47, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1966
	movl	$56929, %edi
	call	__m3_fault
L1966:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_47
	.private_extern _divmod_pow2__mod_pow2_LC_47
_divmod_pow2__mod_pow2_LC_47:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$140737488355327, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_48
	.private_extern _divmod_pow2__div_pow2_LC_48
_divmod_pow2__div_pow2_LC_48:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$48, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1971
	movl	$57057, %edi
	call	__m3_fault
L1971:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_48
	.private_extern _divmod_pow2__mod_pow2_LC_48
_divmod_pow2__mod_pow2_LC_48:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$281474976710655, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_49
	.private_extern _divmod_pow2__div_pow2_LC_49
_divmod_pow2__div_pow2_LC_49:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$49, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1976
	movl	$57185, %edi
	call	__m3_fault
L1976:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_49
	.private_extern _divmod_pow2__mod_pow2_LC_49
_divmod_pow2__mod_pow2_LC_49:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$562949953421311, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_50
	.private_extern _divmod_pow2__div_pow2_LC_50
_divmod_pow2__div_pow2_LC_50:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$50, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1981
	movl	$57313, %edi
	call	__m3_fault
L1981:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_50
	.private_extern _divmod_pow2__mod_pow2_LC_50
_divmod_pow2__mod_pow2_LC_50:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$1125899906842623, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_51
	.private_extern _divmod_pow2__div_pow2_LC_51
_divmod_pow2__div_pow2_LC_51:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$51, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1986
	movl	$57441, %edi
	call	__m3_fault
L1986:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_51
	.private_extern _divmod_pow2__mod_pow2_LC_51
_divmod_pow2__mod_pow2_LC_51:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$2251799813685247, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_52
	.private_extern _divmod_pow2__div_pow2_LC_52
_divmod_pow2__div_pow2_LC_52:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$52, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1991
	movl	$57569, %edi
	call	__m3_fault
L1991:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_52
	.private_extern _divmod_pow2__mod_pow2_LC_52
_divmod_pow2__mod_pow2_LC_52:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$4503599627370495, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_53
	.private_extern _divmod_pow2__div_pow2_LC_53
_divmod_pow2__div_pow2_LC_53:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$53, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L1996
	movl	$57697, %edi
	call	__m3_fault
L1996:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_53
	.private_extern _divmod_pow2__mod_pow2_LC_53
_divmod_pow2__mod_pow2_LC_53:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$9007199254740991, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_54
	.private_extern _divmod_pow2__div_pow2_LC_54
_divmod_pow2__div_pow2_LC_54:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$54, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L2001
	movl	$57825, %edi
	call	__m3_fault
L2001:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_54
	.private_extern _divmod_pow2__mod_pow2_LC_54
_divmod_pow2__mod_pow2_LC_54:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$18014398509481983, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_55
	.private_extern _divmod_pow2__div_pow2_LC_55
_divmod_pow2__div_pow2_LC_55:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$55, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L2006
	movl	$57953, %edi
	call	__m3_fault
L2006:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_55
	.private_extern _divmod_pow2__mod_pow2_LC_55
_divmod_pow2__mod_pow2_LC_55:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$36028797018963967, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_56
	.private_extern _divmod_pow2__div_pow2_LC_56
_divmod_pow2__div_pow2_LC_56:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$56, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L2011
	movl	$58081, %edi
	call	__m3_fault
L2011:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_56
	.private_extern _divmod_pow2__mod_pow2_LC_56
_divmod_pow2__mod_pow2_LC_56:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$72057594037927935, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_57
	.private_extern _divmod_pow2__div_pow2_LC_57
_divmod_pow2__div_pow2_LC_57:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$57, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L2016
	movl	$58209, %edi
	call	__m3_fault
L2016:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_57
	.private_extern _divmod_pow2__mod_pow2_LC_57
_divmod_pow2__mod_pow2_LC_57:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$144115188075855871, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_58
	.private_extern _divmod_pow2__div_pow2_LC_58
_divmod_pow2__div_pow2_LC_58:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$58, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L2021
	movl	$58337, %edi
	call	__m3_fault
L2021:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_58
	.private_extern _divmod_pow2__mod_pow2_LC_58
_divmod_pow2__mod_pow2_LC_58:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$288230376151711743, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_59
	.private_extern _divmod_pow2__div_pow2_LC_59
_divmod_pow2__div_pow2_LC_59:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$59, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L2026
	movl	$58465, %edi
	call	__m3_fault
L2026:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_59
	.private_extern _divmod_pow2__mod_pow2_LC_59
_divmod_pow2__mod_pow2_LC_59:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$576460752303423487, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_60
	.private_extern _divmod_pow2__div_pow2_LC_60
_divmod_pow2__div_pow2_LC_60:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$60, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L2031
	movl	$58593, %edi
	call	__m3_fault
L2031:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_60
	.private_extern _divmod_pow2__mod_pow2_LC_60
_divmod_pow2__mod_pow2_LC_60:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$1152921504606846975, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_61
	.private_extern _divmod_pow2__div_pow2_LC_61
_divmod_pow2__div_pow2_LC_61:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$61, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L2036
	movl	$58721, %edi
	call	__m3_fault
L2036:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_61
	.private_extern _divmod_pow2__mod_pow2_LC_61
_divmod_pow2__mod_pow2_LC_61:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$2305843009213693951, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_62
	.private_extern _divmod_pow2__div_pow2_LC_62
_divmod_pow2__div_pow2_LC_62:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	sarq	$62, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L2041
	movl	$58849, %edi
	call	__m3_fault
L2041:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_62
	.private_extern _divmod_pow2__mod_pow2_LC_62
_divmod_pow2__mod_pow2_LC_62:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movabsq	$4611686018427387903, %rax
	andq	-24(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__div_pow2_LC_63
	.private_extern _divmod_pow2__div_pow2_LC_63
_divmod_pow2__div_pow2_LC_63:
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
	jns	L2046
	movl	$58977, %edi
	call	__m3_fault
L2046:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2__mod_pow2_LC_63
	.private_extern _divmod_pow2__mod_pow2_LC_63
_divmod_pow2__mod_pow2_LC_63:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rcx
	leaq	-1(%rcx), %rdx
	movq	%rcx, %rax
	orq	%rdx, %rax
	movq	%rax, %rsi
	sarq	$63, %rsi
	subq	%rsi, %rdx
	movabsq	$-9223372036854775808, %rax
	cmpq	%rax, %rdx
	sete	%al
	movzbl	%al, %edx
	movq	%rsi, %rax
	notq	%rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	salq	$63, %rax
	movq	%rcx, %rdx
	subq	%rax, %rdx
	movq	%rdx, %rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L2049
	movl	$59041, %edi
	call	__m3_fault
L2049:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _divmod_pow2_M3
_divmod_pow2_M3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	leaq	_MM_divmod_pow2(%rip), %rax
	leave
	ret
__m3_fault:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rsi
	leaq	_MM_divmod_pow2(%rip), %rdi
	call	_RTHooks__ReportFault
	leave
	ret
	.const_data
	.align 5
_L_1:
	.ascii "divmod_pow2_M3"
	.space 1
	.ascii "mod_pow2_LC_63"
	.space 1
	.ascii "div_pow2_LC_63"
	.space 1
	.ascii "mod_pow2_LC_62"
	.space 1
	.ascii "div_pow2_LC_62"
	.space 1
	.ascii "mod_pow2_LC_61"
	.space 1
	.ascii "div_pow2_LC_61"
	.space 1
	.ascii "mod_pow2_LC_60"
	.space 1
	.ascii "div_pow2_LC_60"
	.space 1
	.ascii "mod_pow2_LC_59"
	.space 1
	.ascii "div_pow2_LC_59"
	.space 1
	.ascii "mod_pow2_LC_58"
	.space 1
	.ascii "div_pow2_LC_58"
	.space 1
	.ascii "mod_pow2_LC_57"
	.space 1
	.ascii "div_pow2_LC_57"
	.space 1
	.ascii "mod_pow2_LC_56"
	.space 1
	.ascii "div_pow2_LC_56"
	.space 1
	.ascii "mod_pow2_LC_55"
	.space 1
	.ascii "div_pow2_LC_55"
	.space 1
	.ascii "mod_pow2_LC_54"
	.space 1
	.ascii "div_pow2_LC_54"
	.space 1
	.ascii "mod_pow2_LC_53"
	.space 1
	.ascii "div_pow2_LC_53"
	.space 1
	.ascii "mod_pow2_LC_52"
	.space 1
	.ascii "div_pow2_LC_52"
	.space 1
	.ascii "mod_pow2_LC_51"
	.space 1
	.ascii "div_pow2_LC_51"
	.space 1
	.ascii "mod_pow2_LC_50"
	.space 1
	.ascii "div_pow2_LC_50"
	.space 1
	.ascii "mod_pow2_LC_49"
	.space 1
	.ascii "div_pow2_LC_49"
	.space 1
	.ascii "mod_pow2_LC_48"
	.space 1
	.ascii "div_pow2_LC_48"
	.space 1
	.ascii "mod_pow2_LC_47"
	.space 1
	.ascii "div_pow2_LC_47"
	.space 1
	.ascii "mod_pow2_LC_46"
	.space 1
	.ascii "div_pow2_LC_46"
	.space 1
	.ascii "mod_pow2_LC_45"
	.space 1
	.ascii "div_pow2_LC_45"
	.space 1
	.ascii "mod_pow2_LC_44"
	.space 1
	.ascii "div_pow2_LC_44"
	.space 1
	.ascii "mod_pow2_LC_43"
	.space 1
	.ascii "div_pow2_LC_43"
	.space 1
	.ascii "mod_pow2_LC_42"
	.space 1
	.ascii "div_pow2_LC_42"
	.space 1
	.ascii "mod_pow2_LC_41"
	.space 1
	.ascii "div_pow2_LC_41"
	.space 1
	.ascii "mod_pow2_LC_40"
	.space 1
	.ascii "div_pow2_LC_40"
	.space 1
	.ascii "mod_pow2_LC_39"
	.space 1
	.ascii "div_pow2_LC_39"
	.space 1
	.ascii "mod_pow2_LC_38"
	.space 1
	.ascii "div_pow2_LC_38"
	.space 1
	.ascii "mod_pow2_LC_37"
	.space 1
	.ascii "div_pow2_LC_37"
	.space 1
	.ascii "mod_pow2_LC_36"
	.space 1
	.ascii "div_pow2_LC_36"
	.space 1
	.ascii "mod_pow2_LC_35"
	.space 1
	.ascii "div_pow2_LC_35"
	.space 1
	.ascii "mod_pow2_LC_34"
	.space 1
	.ascii "div_pow2_LC_34"
	.space 1
	.ascii "mod_pow2_LC_33"
	.space 1
	.ascii "div_pow2_LC_33"
	.space 1
	.ascii "mod_pow2_LC_32"
	.space 1
	.ascii "div_pow2_LC_32"
	.space 1
	.ascii "mod_pow2_LC_31"
	.space 1
	.ascii "div_pow2_LC_31"
	.space 1
	.ascii "mod_pow2_LC_30"
	.space 1
	.ascii "div_pow2_LC_30"
	.space 1
	.ascii "mod_pow2_LC_29"
	.space 1
	.ascii "div_pow2_LC_29"
	.space 1
	.ascii "mod_pow2_LC_28"
	.space 1
	.ascii "div_pow2_LC_28"
	.space 1
	.ascii "mod_pow2_LC_27"
	.space 1
	.ascii "div_pow2_LC_27"
	.space 1
	.ascii "mod_pow2_LC_26"
	.space 1
	.ascii "div_pow2_LC_26"
	.space 1
	.ascii "mod_pow2_LC_25"
	.space 1
	.ascii "div_pow2_LC_25"
	.space 1
	.ascii "mod_pow2_LC_24"
	.space 1
	.ascii "div_pow2_LC_24"
	.space 1
	.ascii "mod_pow2_LC_23"
	.space 1
	.ascii "div_pow2_LC_23"
	.space 1
	.ascii "mod_pow2_LC_22"
	.space 1
	.ascii "div_pow2_LC_22"
	.space 1
	.ascii "mod_pow2_LC_21"
	.space 1
	.ascii "div_pow2_LC_21"
	.space 1
	.ascii "mod_pow2_LC_20"
	.space 1
	.ascii "div_pow2_LC_20"
	.space 1
	.ascii "mod_pow2_LC_19"
	.space 1
	.ascii "div_pow2_LC_19"
	.space 1
	.ascii "mod_pow2_LC_18"
	.space 1
	.ascii "div_pow2_LC_18"
	.space 1
	.ascii "mod_pow2_LC_17"
	.space 1
	.ascii "div_pow2_LC_17"
	.space 1
	.ascii "mod_pow2_LC_16"
	.space 1
	.ascii "div_pow2_LC_16"
	.space 1
	.ascii "mod_pow2_LC_15"
	.space 1
	.ascii "div_pow2_LC_15"
	.space 1
	.ascii "mod_pow2_LC_14"
	.space 1
	.ascii "div_pow2_LC_14"
	.space 1
	.ascii "mod_pow2_LC_13"
	.space 1
	.ascii "div_pow2_LC_13"
	.space 1
	.ascii "mod_pow2_LC_12"
	.space 1
	.ascii "div_pow2_LC_12"
	.space 1
	.ascii "mod_pow2_LC_11"
	.space 1
	.ascii "div_pow2_LC_11"
	.space 1
	.ascii "mod_pow2_LC_10"
	.space 1
	.ascii "div_pow2_LC_10"
	.space 1
	.ascii "mod_pow2_LC_9"
	.space 1
	.ascii "div_pow2_LC_9"
	.space 1
	.ascii "mod_pow2_LC_8"
	.space 1
	.ascii "div_pow2_LC_8"
	.space 1
	.ascii "mod_pow2_LC_7"
	.space 1
	.ascii "div_pow2_LC_7"
	.space 1
	.ascii "mod_pow2_LC_6"
	.space 1
	.ascii "div_pow2_LC_6"
	.space 1
	.ascii "mod_pow2_LC_5"
	.space 1
	.ascii "div_pow2_LC_5"
	.space 1
	.ascii "mod_pow2_LC_4"
	.space 1
	.ascii "div_pow2_LC_4"
	.space 1
	.ascii "mod_pow2_LC_3"
	.space 1
	.ascii "div_pow2_LC_3"
	.space 1
	.ascii "mod_pow2_LC_2"
	.space 1
	.ascii "div_pow2_LC_2"
	.space 1
	.ascii "mod_pow2_LC_1"
	.space 1
	.ascii "div_pow2_LC_1"
	.space 1
	.ascii "mod_pow2_LC_0"
	.space 1
	.ascii "div_pow2_LC_0"
	.space 1
	.ascii "mod_pow2_L_63"
	.space 1
	.ascii "div_pow2_L_63"
	.space 1
	.ascii "mod_pow2_L_62"
	.space 1
	.ascii "div_pow2_L_62"
	.space 1
	.ascii "mod_pow2_L_61"
	.space 1
	.ascii "div_pow2_L_61"
	.space 1
	.ascii "mod_pow2_L_60"
	.space 1
	.ascii "div_pow2_L_60"
	.space 1
	.ascii "mod_pow2_L_59"
	.space 1
	.ascii "div_pow2_L_59"
	.space 1
	.ascii "mod_pow2_L_58"
	.space 1
	.ascii "div_pow2_L_58"
	.space 1
	.ascii "mod_pow2_L_57"
	.space 1
	.ascii "div_pow2_L_57"
	.space 1
	.ascii "mod_pow2_L_56"
	.space 1
	.ascii "div_pow2_L_56"
	.space 1
	.ascii "mod_pow2_L_55"
	.space 1
	.ascii "div_pow2_L_55"
	.space 1
	.ascii "mod_pow2_L_54"
	.space 1
	.ascii "div_pow2_L_54"
	.space 1
	.ascii "mod_pow2_L_53"
	.space 1
	.ascii "div_pow2_L_53"
	.space 1
	.ascii "mod_pow2_L_52"
	.space 1
	.ascii "div_pow2_L_52"
	.space 1
	.ascii "mod_pow2_L_51"
	.space 1
	.ascii "div_pow2_L_51"
	.space 1
	.ascii "mod_pow2_L_50"
	.space 1
	.ascii "div_pow2_L_50"
	.space 1
	.ascii "mod_pow2_L_49"
	.space 1
	.ascii "div_pow2_L_49"
	.space 1
	.ascii "mod_pow2_L_48"
	.space 1
	.ascii "div_pow2_L_48"
	.space 1
	.ascii "mod_pow2_L_47"
	.space 1
	.ascii "div_pow2_L_47"
	.space 1
	.ascii "mod_pow2_L_46"
	.space 1
	.ascii "div_pow2_L_46"
	.space 1
	.ascii "mod_pow2_L_45"
	.space 1
	.ascii "div_pow2_L_45"
	.space 1
	.ascii "mod_pow2_L_44"
	.space 1
	.ascii "div_pow2_L_44"
	.space 1
	.ascii "mod_pow2_L_43"
	.space 1
	.ascii "div_pow2_L_43"
	.space 1
	.ascii "mod_pow2_L_42"
	.space 1
	.ascii "div_pow2_L_42"
	.space 1
	.ascii "mod_pow2_L_41"
	.space 1
	.ascii "div_pow2_L_41"
	.space 1
	.ascii "mod_pow2_L_40"
	.space 1
	.ascii "div_pow2_L_40"
	.space 1
	.ascii "mod_pow2_L_39"
	.space 1
	.ascii "div_pow2_L_39"
	.space 1
	.ascii "mod_pow2_L_38"
	.space 1
	.ascii "div_pow2_L_38"
	.space 1
	.ascii "mod_pow2_L_37"
	.space 1
	.ascii "div_pow2_L_37"
	.space 1
	.ascii "mod_pow2_L_36"
	.space 1
	.ascii "div_pow2_L_36"
	.space 1
	.ascii "mod_pow2_L_35"
	.space 1
	.ascii "div_pow2_L_35"
	.space 1
	.ascii "mod_pow2_L_34"
	.space 1
	.ascii "div_pow2_L_34"
	.space 1
	.ascii "mod_pow2_L_33"
	.space 1
	.ascii "div_pow2_L_33"
	.space 1
	.ascii "mod_pow2_L_32"
	.space 1
	.ascii "div_pow2_L_32"
	.space 1
	.ascii "mod_pow2_L_31"
	.space 1
	.ascii "div_pow2_L_31"
	.space 1
	.ascii "mod_pow2_L_30"
	.space 1
	.ascii "div_pow2_L_30"
	.space 1
	.ascii "mod_pow2_L_29"
	.space 1
	.ascii "div_pow2_L_29"
	.space 1
	.ascii "mod_pow2_L_28"
	.space 1
	.ascii "div_pow2_L_28"
	.space 1
	.ascii "mod_pow2_L_27"
	.space 1
	.ascii "div_pow2_L_27"
	.space 1
	.ascii "mod_pow2_L_26"
	.space 1
	.ascii "div_pow2_L_26"
	.space 1
	.ascii "mod_pow2_L_25"
	.space 1
	.ascii "div_pow2_L_25"
	.space 1
	.ascii "mod_pow2_L_24"
	.space 1
	.ascii "div_pow2_L_24"
	.space 1
	.ascii "mod_pow2_L_23"
	.space 1
	.ascii "div_pow2_L_23"
	.space 1
	.ascii "mod_pow2_L_22"
	.space 1
	.ascii "div_pow2_L_22"
	.space 1
	.ascii "mod_pow2_L_21"
	.space 1
	.ascii "div_pow2_L_21"
	.space 1
	.ascii "mod_pow2_L_20"
	.space 1
	.ascii "div_pow2_L_20"
	.space 1
	.ascii "mod_pow2_L_19"
	.space 1
	.ascii "div_pow2_L_19"
	.space 1
	.ascii "mod_pow2_L_18"
	.space 1
	.ascii "div_pow2_L_18"
	.space 1
	.ascii "mod_pow2_L_17"
	.space 1
	.ascii "div_pow2_L_17"
	.space 1
	.ascii "mod_pow2_L_16"
	.space 1
	.ascii "div_pow2_L_16"
	.space 1
	.ascii "mod_pow2_L_15"
	.space 1
	.ascii "div_pow2_L_15"
	.space 1
	.ascii "mod_pow2_L_14"
	.space 1
	.ascii "div_pow2_L_14"
	.space 1
	.ascii "mod_pow2_L_13"
	.space 1
	.ascii "div_pow2_L_13"
	.space 1
	.ascii "mod_pow2_L_12"
	.space 1
	.ascii "div_pow2_L_12"
	.space 1
	.ascii "mod_pow2_L_11"
	.space 1
	.ascii "div_pow2_L_11"
	.space 1
	.ascii "mod_pow2_L_10"
	.space 1
	.ascii "div_pow2_L_10"
	.space 1
	.ascii "mod_pow2_L_9"
	.space 1
	.ascii "div_pow2_L_9"
	.space 1
	.ascii "mod_pow2_L_8"
	.space 1
	.ascii "div_pow2_L_8"
	.space 1
	.ascii "mod_pow2_L_7"
	.space 1
	.ascii "div_pow2_L_7"
	.space 1
	.ascii "mod_pow2_L_6"
	.space 1
	.ascii "div_pow2_L_6"
	.space 1
	.ascii "mod_pow2_L_5"
	.space 1
	.ascii "div_pow2_L_5"
	.space 1
	.ascii "mod_pow2_L_4"
	.space 1
	.ascii "div_pow2_L_4"
	.space 1
	.ascii "mod_pow2_L_3"
	.space 1
	.ascii "div_pow2_L_3"
	.space 1
	.ascii "mod_pow2_L_2"
	.space 1
	.ascii "div_pow2_L_2"
	.space 1
	.ascii "mod_pow2_L_1"
	.space 1
	.ascii "div_pow2_L_1"
	.space 1
	.ascii "mod_pow2_L_0"
	.space 1
	.ascii "div_pow2_L_0"
	.space 1
	.ascii "mod_pow2_C_63"
	.space 1
	.ascii "div_pow2_C_63"
	.space 1
	.ascii "mod_pow2_C_62"
	.space 1
	.ascii "div_pow2_C_62"
	.space 1
	.ascii "mod_pow2_C_61"
	.space 1
	.ascii "div_pow2_C_61"
	.space 1
	.ascii "mod_pow2_C_60"
	.space 1
	.ascii "div_pow2_C_60"
	.space 1
	.ascii "mod_pow2_C_59"
	.space 1
	.ascii "div_pow2_C_59"
	.space 1
	.ascii "mod_pow2_C_58"
	.space 1
	.ascii "div_pow2_C_58"
	.space 1
	.ascii "mod_pow2_C_57"
	.space 1
	.ascii "div_pow2_C_57"
	.space 1
	.ascii "mod_pow2_C_56"
	.space 1
	.ascii "div_pow2_C_56"
	.space 1
	.ascii "mod_pow2_C_55"
	.space 1
	.ascii "div_pow2_C_55"
	.space 1
	.ascii "mod_pow2_C_54"
	.space 1
	.ascii "div_pow2_C_54"
	.space 1
	.ascii "mod_pow2_C_53"
	.space 1
	.ascii "div_pow2_C_53"
	.space 1
	.ascii "mod_pow2_C_52"
	.space 1
	.ascii "div_pow2_C_52"
	.space 1
	.ascii "mod_pow2_C_51"
	.space 1
	.ascii "div_pow2_C_51"
	.space 1
	.ascii "mod_pow2_C_50"
	.space 1
	.ascii "div_pow2_C_50"
	.space 1
	.ascii "mod_pow2_C_49"
	.space 1
	.ascii "div_pow2_C_49"
	.space 1
	.ascii "mod_pow2_C_48"
	.space 1
	.ascii "div_pow2_C_48"
	.space 1
	.ascii "mod_pow2_C_47"
	.space 1
	.ascii "div_pow2_C_47"
	.space 1
	.ascii "mod_pow2_C_46"
	.space 1
	.ascii "div_pow2_C_46"
	.space 1
	.ascii "mod_pow2_C_45"
	.space 1
	.ascii "div_pow2_C_45"
	.space 1
	.ascii "mod_pow2_C_44"
	.space 1
	.ascii "div_pow2_C_44"
	.space 1
	.ascii "mod_pow2_C_43"
	.space 1
	.ascii "div_pow2_C_43"
	.space 1
	.ascii "mod_pow2_C_42"
	.space 1
	.ascii "div_pow2_C_42"
	.space 1
	.ascii "mod_pow2_C_41"
	.space 1
	.ascii "div_pow2_C_41"
	.space 1
	.ascii "mod_pow2_C_40"
	.space 1
	.ascii "div_pow2_C_40"
	.space 1
	.ascii "mod_pow2_C_39"
	.space 1
	.ascii "div_pow2_C_39"
	.space 1
	.ascii "mod_pow2_C_38"
	.space 1
	.ascii "div_pow2_C_38"
	.space 1
	.ascii "mod_pow2_C_37"
	.space 1
	.ascii "div_pow2_C_37"
	.space 1
	.ascii "mod_pow2_C_36"
	.space 1
	.ascii "div_pow2_C_36"
	.space 1
	.ascii "mod_pow2_C_35"
	.space 1
	.ascii "div_pow2_C_35"
	.space 1
	.ascii "mod_pow2_C_34"
	.space 1
	.ascii "div_pow2_C_34"
	.space 1
	.ascii "mod_pow2_C_33"
	.space 1
	.ascii "div_pow2_C_33"
	.space 1
	.ascii "mod_pow2_C_32"
	.space 1
	.ascii "div_pow2_C_32"
	.space 1
	.ascii "mod_pow2_C_31"
	.space 1
	.ascii "div_pow2_C_31"
	.space 1
	.ascii "mod_pow2_C_30"
	.space 1
	.ascii "div_pow2_C_30"
	.space 1
	.ascii "mod_pow2_C_29"
	.space 1
	.ascii "div_pow2_C_29"
	.space 1
	.ascii "mod_pow2_C_28"
	.space 1
	.ascii "div_pow2_C_28"
	.space 1
	.ascii "mod_pow2_C_27"
	.space 1
	.ascii "div_pow2_C_27"
	.space 1
	.ascii "mod_pow2_C_26"
	.space 1
	.ascii "div_pow2_C_26"
	.space 1
	.ascii "mod_pow2_C_25"
	.space 1
	.ascii "div_pow2_C_25"
	.space 1
	.ascii "mod_pow2_C_24"
	.space 1
	.ascii "div_pow2_C_24"
	.space 1
	.ascii "mod_pow2_C_23"
	.space 1
	.ascii "div_pow2_C_23"
	.space 1
	.ascii "mod_pow2_C_22"
	.space 1
	.ascii "div_pow2_C_22"
	.space 1
	.ascii "mod_pow2_C_21"
	.space 1
	.ascii "div_pow2_C_21"
	.space 1
	.ascii "mod_pow2_C_20"
	.space 1
	.ascii "div_pow2_C_20"
	.space 1
	.ascii "mod_pow2_C_19"
	.space 1
	.ascii "div_pow2_C_19"
	.space 1
	.ascii "mod_pow2_C_18"
	.space 1
	.ascii "div_pow2_C_18"
	.space 1
	.ascii "mod_pow2_C_17"
	.space 1
	.ascii "div_pow2_C_17"
	.space 1
	.ascii "mod_pow2_C_16"
	.space 1
	.ascii "div_pow2_C_16"
	.space 1
	.ascii "mod_pow2_C_15"
	.space 1
	.ascii "div_pow2_C_15"
	.space 1
	.ascii "mod_pow2_C_14"
	.space 1
	.ascii "div_pow2_C_14"
	.space 1
	.ascii "mod_pow2_C_13"
	.space 1
	.ascii "div_pow2_C_13"
	.space 1
	.ascii "mod_pow2_C_12"
	.space 1
	.ascii "div_pow2_C_12"
	.space 1
	.ascii "mod_pow2_C_11"
	.space 1
	.ascii "div_pow2_C_11"
	.space 1
	.ascii "mod_pow2_C_10"
	.space 1
	.ascii "div_pow2_C_10"
	.space 1
	.ascii "mod_pow2_C_9"
	.space 1
	.ascii "div_pow2_C_9"
	.space 1
	.ascii "mod_pow2_C_8"
	.space 1
	.ascii "div_pow2_C_8"
	.space 1
	.ascii "mod_pow2_C_7"
	.space 1
	.ascii "div_pow2_C_7"
	.space 1
	.ascii "mod_pow2_C_6"
	.space 1
	.ascii "div_pow2_C_6"
	.space 1
	.ascii "mod_pow2_C_5"
	.space 1
	.ascii "div_pow2_C_5"
	.space 1
	.ascii "mod_pow2_C_4"
	.space 1
	.ascii "div_pow2_C_4"
	.space 1
	.ascii "mod_pow2_C_3"
	.space 1
	.ascii "div_pow2_C_3"
	.space 1
	.ascii "mod_pow2_C_2"
	.space 1
	.ascii "div_pow2_C_2"
	.space 1
	.ascii "mod_pow2_C_1"
	.space 1
	.ascii "div_pow2_C_1"
	.space 1
	.ascii "mod_pow2_C_0"
	.space 1
	.ascii "div_pow2_C_0"
	.space 1
	.ascii "mod_pow2_I_63"
	.space 1
	.ascii "div_pow2_I_63"
	.space 1
	.ascii "mod_pow2_I_62"
	.space 1
	.ascii "div_pow2_I_62"
	.space 1
	.ascii "mod_pow2_I_61"
	.space 1
	.ascii "div_pow2_I_61"
	.space 1
	.ascii "mod_pow2_I_60"
	.space 1
	.ascii "div_pow2_I_60"
	.space 1
	.ascii "mod_pow2_I_59"
	.space 1
	.ascii "div_pow2_I_59"
	.space 1
	.ascii "mod_pow2_I_58"
	.space 1
	.ascii "div_pow2_I_58"
	.space 1
	.ascii "mod_pow2_I_57"
	.space 1
	.ascii "div_pow2_I_57"
	.space 1
	.ascii "mod_pow2_I_56"
	.space 1
	.ascii "div_pow2_I_56"
	.space 1
	.ascii "mod_pow2_I_55"
	.space 1
	.ascii "div_pow2_I_55"
	.space 1
	.ascii "mod_pow2_I_54"
	.space 1
	.ascii "div_pow2_I_54"
	.space 1
	.ascii "mod_pow2_I_53"
	.space 1
	.ascii "div_pow2_I_53"
	.space 1
	.ascii "mod_pow2_I_52"
	.space 1
	.ascii "div_pow2_I_52"
	.space 1
	.ascii "mod_pow2_I_51"
	.space 1
	.ascii "div_pow2_I_51"
	.space 1
	.ascii "mod_pow2_I_50"
	.space 1
	.ascii "div_pow2_I_50"
	.space 1
	.ascii "mod_pow2_I_49"
	.space 1
	.ascii "div_pow2_I_49"
	.space 1
	.ascii "mod_pow2_I_48"
	.space 1
	.ascii "div_pow2_I_48"
	.space 1
	.ascii "mod_pow2_I_47"
	.space 1
	.ascii "div_pow2_I_47"
	.space 1
	.ascii "mod_pow2_I_46"
	.space 1
	.ascii "div_pow2_I_46"
	.space 1
	.ascii "mod_pow2_I_45"
	.space 1
	.ascii "div_pow2_I_45"
	.space 1
	.ascii "mod_pow2_I_44"
	.space 1
	.ascii "div_pow2_I_44"
	.space 1
	.ascii "mod_pow2_I_43"
	.space 1
	.ascii "div_pow2_I_43"
	.space 1
	.ascii "mod_pow2_I_42"
	.space 1
	.ascii "div_pow2_I_42"
	.space 1
	.ascii "mod_pow2_I_41"
	.space 1
	.ascii "div_pow2_I_41"
	.space 1
	.ascii "mod_pow2_I_40"
	.space 1
	.ascii "div_pow2_I_40"
	.space 1
	.ascii "mod_pow2_I_39"
	.space 1
	.ascii "div_pow2_I_39"
	.space 1
	.ascii "mod_pow2_I_38"
	.space 1
	.ascii "div_pow2_I_38"
	.space 1
	.ascii "mod_pow2_I_37"
	.space 1
	.ascii "div_pow2_I_37"
	.space 1
	.ascii "mod_pow2_I_36"
	.space 1
	.ascii "div_pow2_I_36"
	.space 1
	.ascii "mod_pow2_I_35"
	.space 1
	.ascii "div_pow2_I_35"
	.space 1
	.ascii "mod_pow2_I_34"
	.space 1
	.ascii "div_pow2_I_34"
	.space 1
	.ascii "mod_pow2_I_33"
	.space 1
	.ascii "div_pow2_I_33"
	.space 1
	.ascii "mod_pow2_I_32"
	.space 1
	.ascii "div_pow2_I_32"
	.space 1
	.ascii "mod_pow2_I_31"
	.space 1
	.ascii "div_pow2_I_31"
	.space 1
	.ascii "mod_pow2_I_30"
	.space 1
	.ascii "div_pow2_I_30"
	.space 1
	.ascii "mod_pow2_I_29"
	.space 1
	.ascii "div_pow2_I_29"
	.space 1
	.ascii "mod_pow2_I_28"
	.space 1
	.ascii "div_pow2_I_28"
	.space 1
	.ascii "mod_pow2_I_27"
	.space 1
	.ascii "div_pow2_I_27"
	.space 1
	.ascii "mod_pow2_I_26"
	.space 1
	.ascii "div_pow2_I_26"
	.space 1
	.ascii "mod_pow2_I_25"
	.space 1
	.ascii "div_pow2_I_25"
	.space 1
	.ascii "mod_pow2_I_24"
	.space 1
	.ascii "div_pow2_I_24"
	.space 1
	.ascii "mod_pow2_I_23"
	.space 1
	.ascii "div_pow2_I_23"
	.space 1
	.ascii "mod_pow2_I_22"
	.space 1
	.ascii "div_pow2_I_22"
	.space 1
	.ascii "mod_pow2_I_21"
	.space 1
	.ascii "div_pow2_I_21"
	.space 1
	.ascii "mod_pow2_I_20"
	.space 1
	.ascii "div_pow2_I_20"
	.space 1
	.ascii "mod_pow2_I_19"
	.space 1
	.ascii "div_pow2_I_19"
	.space 1
	.ascii "mod_pow2_I_18"
	.space 1
	.ascii "div_pow2_I_18"
	.space 1
	.ascii "mod_pow2_I_17"
	.space 1
	.ascii "div_pow2_I_17"
	.space 1
	.ascii "mod_pow2_I_16"
	.space 1
	.ascii "div_pow2_I_16"
	.space 1
	.ascii "mod_pow2_I_15"
	.space 1
	.ascii "div_pow2_I_15"
	.space 1
	.ascii "mod_pow2_I_14"
	.space 1
	.ascii "div_pow2_I_14"
	.space 1
	.ascii "mod_pow2_I_13"
	.space 1
	.ascii "div_pow2_I_13"
	.space 1
	.ascii "mod_pow2_I_12"
	.space 1
	.ascii "div_pow2_I_12"
	.space 1
	.ascii "mod_pow2_I_11"
	.space 1
	.ascii "div_pow2_I_11"
	.space 1
	.ascii "mod_pow2_I_10"
	.space 1
	.ascii "div_pow2_I_10"
	.space 1
	.ascii "mod_pow2_I_9"
	.space 1
	.ascii "div_pow2_I_9"
	.space 1
	.ascii "mod_pow2_I_8"
	.space 1
	.ascii "div_pow2_I_8"
	.space 1
	.ascii "mod_pow2_I_7"
	.space 1
	.ascii "div_pow2_I_7"
	.space 1
	.ascii "mod_pow2_I_6"
	.space 1
	.ascii "div_pow2_I_6"
	.space 1
	.ascii "mod_pow2_I_5"
	.space 1
	.ascii "div_pow2_I_5"
	.space 1
	.ascii "mod_pow2_I_4"
	.space 1
	.ascii "div_pow2_I_4"
	.space 1
	.ascii "mod_pow2_I_3"
	.space 1
	.ascii "div_pow2_I_3"
	.space 1
	.ascii "mod_pow2_I_2"
	.space 1
	.ascii "div_pow2_I_2"
	.space 1
	.ascii "mod_pow2_I_1"
	.space 1
	.ascii "div_pow2_I_1"
	.space 1
	.ascii "mod_pow2_I_0"
	.space 1
	.ascii "div_pow2_I_0"
	.space 1
	.ascii "mod_pow2_u64_63"
	.space 1
	.ascii "div_pow2_u64_63"
	.space 1
	.ascii "mod_pow2_u64_62"
	.space 1
	.ascii "div_pow2_u64_62"
	.space 1
	.ascii "mod_pow2_u64_61"
	.space 1
	.ascii "div_pow2_u64_61"
	.space 1
	.ascii "mod_pow2_u64_60"
	.space 1
	.ascii "div_pow2_u64_60"
	.space 1
	.ascii "mod_pow2_u64_59"
	.space 1
	.ascii "div_pow2_u64_59"
	.space 1
	.ascii "mod_pow2_u64_58"
	.space 1
	.ascii "div_pow2_u64_58"
	.space 1
	.ascii "mod_pow2_u64_57"
	.space 1
	.ascii "div_pow2_u64_57"
	.space 1
	.ascii "mod_pow2_u64_56"
	.space 1
	.ascii "div_pow2_u64_56"
	.space 1
	.ascii "mod_pow2_u64_55"
	.space 1
	.ascii "div_pow2_u64_55"
	.space 1
	.ascii "mod_pow2_u64_54"
	.space 1
	.ascii "div_pow2_u64_54"
	.space 1
	.ascii "mod_pow2_u64_53"
	.space 1
	.ascii "div_pow2_u64_53"
	.space 1
	.ascii "mod_pow2_u64_52"
	.space 1
	.ascii "div_pow2_u64_52"
	.space 1
	.ascii "mod_pow2_u64_51"
	.space 1
	.ascii "div_pow2_u64_51"
	.space 1
	.ascii "mod_pow2_u64_50"
	.space 1
	.ascii "div_pow2_u64_50"
	.space 1
	.ascii "mod_pow2_u64_49"
	.space 1
	.ascii "div_pow2_u64_49"
	.space 1
	.ascii "mod_pow2_u64_48"
	.space 1
	.ascii "div_pow2_u64_48"
	.space 1
	.ascii "mod_pow2_u64_47"
	.space 1
	.ascii "div_pow2_u64_47"
	.space 1
	.ascii "mod_pow2_u64_46"
	.space 1
	.ascii "div_pow2_u64_46"
	.space 1
	.ascii "mod_pow2_u64_45"
	.space 1
	.ascii "div_pow2_u64_45"
	.space 1
	.ascii "mod_pow2_u64_44"
	.space 1
	.ascii "div_pow2_u64_44"
	.space 1
	.ascii "mod_pow2_u64_43"
	.space 1
	.ascii "div_pow2_u64_43"
	.space 1
	.ascii "mod_pow2_u64_42"
	.space 1
	.ascii "div_pow2_u64_42"
	.space 1
	.ascii "mod_pow2_u64_41"
	.space 1
	.ascii "div_pow2_u64_41"
	.space 1
	.ascii "mod_pow2_u64_40"
	.space 1
	.ascii "div_pow2_u64_40"
	.space 1
	.ascii "mod_pow2_u64_39"
	.space 1
	.ascii "div_pow2_u64_39"
	.space 1
	.ascii "mod_pow2_u64_38"
	.space 1
	.ascii "div_pow2_u64_38"
	.space 1
	.ascii "mod_pow2_u64_37"
	.space 1
	.ascii "div_pow2_u64_37"
	.space 1
	.ascii "mod_pow2_u64_36"
	.space 1
	.ascii "div_pow2_u64_36"
	.space 1
	.ascii "mod_pow2_u64_35"
	.space 1
	.ascii "div_pow2_u64_35"
	.space 1
	.ascii "mod_pow2_u64_34"
	.space 1
	.ascii "div_pow2_u64_34"
	.space 1
	.ascii "mod_pow2_u64_33"
	.space 1
	.ascii "div_pow2_u64_33"
	.space 1
	.ascii "mod_pow2_u64_32"
	.space 1
	.ascii "div_pow2_u64_32"
	.space 1
	.ascii "mod_pow2_u64_31"
	.space 1
	.ascii "div_pow2_u64_31"
	.space 1
	.ascii "mod_pow2_u64_30"
	.space 1
	.ascii "div_pow2_u64_30"
	.space 1
	.ascii "mod_pow2_u64_29"
	.space 1
	.ascii "div_pow2_u64_29"
	.space 1
	.ascii "mod_pow2_u64_28"
	.space 1
	.ascii "div_pow2_u64_28"
	.space 1
	.ascii "mod_pow2_u64_27"
	.space 1
	.ascii "div_pow2_u64_27"
	.space 1
	.ascii "mod_pow2_u64_26"
	.space 1
	.ascii "div_pow2_u64_26"
	.space 1
	.ascii "mod_pow2_u64_25"
	.space 1
	.ascii "div_pow2_u64_25"
	.space 1
	.ascii "mod_pow2_u64_24"
	.space 1
	.ascii "div_pow2_u64_24"
	.space 1
	.ascii "mod_pow2_u64_23"
	.space 1
	.ascii "div_pow2_u64_23"
	.space 1
	.ascii "mod_pow2_u64_22"
	.space 1
	.ascii "div_pow2_u64_22"
	.space 1
	.ascii "mod_pow2_u64_21"
	.space 1
	.ascii "div_pow2_u64_21"
	.space 1
	.ascii "mod_pow2_u64_20"
	.space 1
	.ascii "div_pow2_u64_20"
	.space 1
	.ascii "mod_pow2_u64_19"
	.space 1
	.ascii "div_pow2_u64_19"
	.space 1
	.ascii "mod_pow2_u64_18"
	.space 1
	.ascii "div_pow2_u64_18"
	.space 1
	.ascii "mod_pow2_u64_17"
	.space 1
	.ascii "div_pow2_u64_17"
	.space 1
	.ascii "mod_pow2_u64_16"
	.space 1
	.ascii "div_pow2_u64_16"
	.space 1
	.ascii "mod_pow2_u64_15"
	.space 1
	.ascii "div_pow2_u64_15"
	.space 1
	.ascii "mod_pow2_u64_14"
	.space 1
	.ascii "div_pow2_u64_14"
	.space 1
	.ascii "mod_pow2_u64_13"
	.space 1
	.ascii "div_pow2_u64_13"
	.space 1
	.ascii "mod_pow2_u64_12"
	.space 1
	.ascii "div_pow2_u64_12"
	.space 1
	.ascii "mod_pow2_u64_11"
	.space 1
	.ascii "div_pow2_u64_11"
	.space 1
	.ascii "mod_pow2_u64_10"
	.space 1
	.ascii "div_pow2_u64_10"
	.space 1
	.ascii "mod_pow2_u64_9"
	.space 1
	.ascii "div_pow2_u64_9"
	.space 1
	.ascii "mod_pow2_u64_8"
	.space 1
	.ascii "div_pow2_u64_8"
	.space 1
	.ascii "mod_pow2_u64_7"
	.space 1
	.ascii "div_pow2_u64_7"
	.space 1
	.ascii "mod_pow2_u64_6"
	.space 1
	.ascii "div_pow2_u64_6"
	.space 1
	.ascii "mod_pow2_u64_5"
	.space 1
	.ascii "div_pow2_u64_5"
	.space 1
	.ascii "mod_pow2_u64_4"
	.space 1
	.ascii "div_pow2_u64_4"
	.space 1
	.ascii "mod_pow2_u64_3"
	.space 1
	.ascii "div_pow2_u64_3"
	.space 1
	.ascii "mod_pow2_u64_2"
	.space 1
	.ascii "div_pow2_u64_2"
	.space 1
	.ascii "mod_pow2_u64_1"
	.space 1
	.ascii "div_pow2_u64_1"
	.space 1
	.ascii "mod_pow2_u64_0"
	.space 1
	.ascii "div_pow2_u64_0"
	.space 1
	.ascii "mod_pow2_i64_63"
	.space 1
	.ascii "div_pow2_i64_63"
	.space 1
	.ascii "mod_pow2_i64_62"
	.space 1
	.ascii "div_pow2_i64_62"
	.space 1
	.ascii "mod_pow2_i64_61"
	.space 1
	.ascii "div_pow2_i64_61"
	.space 1
	.ascii "mod_pow2_i64_60"
	.space 1
	.ascii "div_pow2_i64_60"
	.space 1
	.ascii "mod_pow2_i64_59"
	.space 1
	.ascii "div_pow2_i64_59"
	.space 1
	.ascii "mod_pow2_i64_58"
	.space 1
	.ascii "div_pow2_i64_58"
	.space 1
	.ascii "mod_pow2_i64_57"
	.space 1
	.ascii "div_pow2_i64_57"
	.space 1
	.ascii "mod_pow2_i64_56"
	.space 1
	.ascii "div_pow2_i64_56"
	.space 1
	.ascii "mod_pow2_i64_55"
	.space 1
	.ascii "div_pow2_i64_55"
	.space 1
	.ascii "mod_pow2_i64_54"
	.space 1
	.ascii "div_pow2_i64_54"
	.space 1
	.ascii "mod_pow2_i64_53"
	.space 1
	.ascii "div_pow2_i64_53"
	.space 1
	.ascii "mod_pow2_i64_52"
	.space 1
	.ascii "div_pow2_i64_52"
	.space 1
	.ascii "mod_pow2_i64_51"
	.space 1
	.ascii "div_pow2_i64_51"
	.space 1
	.ascii "mod_pow2_i64_50"
	.space 1
	.ascii "div_pow2_i64_50"
	.space 1
	.ascii "mod_pow2_i64_49"
	.space 1
	.ascii "div_pow2_i64_49"
	.space 1
	.ascii "mod_pow2_i64_48"
	.space 1
	.ascii "div_pow2_i64_48"
	.space 1
	.ascii "mod_pow2_i64_47"
	.space 1
	.ascii "div_pow2_i64_47"
	.space 1
	.ascii "mod_pow2_i64_46"
	.space 1
	.ascii "div_pow2_i64_46"
	.space 1
	.ascii "mod_pow2_i64_45"
	.space 1
	.ascii "div_pow2_i64_45"
	.space 1
	.ascii "mod_pow2_i64_44"
	.space 1
	.ascii "div_pow2_i64_44"
	.space 1
	.ascii "mod_pow2_i64_43"
	.space 1
	.ascii "div_pow2_i64_43"
	.space 1
	.ascii "mod_pow2_i64_42"
	.space 1
	.ascii "div_pow2_i64_42"
	.space 1
	.ascii "mod_pow2_i64_41"
	.space 1
	.ascii "div_pow2_i64_41"
	.space 1
	.ascii "mod_pow2_i64_40"
	.space 1
	.ascii "div_pow2_i64_40"
	.space 1
	.ascii "mod_pow2_i64_39"
	.space 1
	.ascii "div_pow2_i64_39"
	.space 1
	.ascii "mod_pow2_i64_38"
	.space 1
	.ascii "div_pow2_i64_38"
	.space 1
	.ascii "mod_pow2_i64_37"
	.space 1
	.ascii "div_pow2_i64_37"
	.space 1
	.ascii "mod_pow2_i64_36"
	.space 1
	.ascii "div_pow2_i64_36"
	.space 1
	.ascii "mod_pow2_i64_35"
	.space 1
	.ascii "div_pow2_i64_35"
	.space 1
	.ascii "mod_pow2_i64_34"
	.space 1
	.ascii "div_pow2_i64_34"
	.space 1
	.ascii "mod_pow2_i64_33"
	.space 1
	.ascii "div_pow2_i64_33"
	.space 1
	.ascii "mod_pow2_i64_32"
	.space 1
	.ascii "div_pow2_i64_32"
	.space 1
	.ascii "mod_pow2_i64_31"
	.space 1
	.ascii "div_pow2_i64_31"
	.space 1
	.ascii "mod_pow2_i64_30"
	.space 1
	.ascii "div_pow2_i64_30"
	.space 1
	.ascii "mod_pow2_i64_29"
	.space 1
	.ascii "div_pow2_i64_29"
	.space 1
	.ascii "mod_pow2_i64_28"
	.space 1
	.ascii "div_pow2_i64_28"
	.space 1
	.ascii "mod_pow2_i64_27"
	.space 1
	.ascii "div_pow2_i64_27"
	.space 1
	.ascii "mod_pow2_i64_26"
	.space 1
	.ascii "div_pow2_i64_26"
	.space 1
	.ascii "mod_pow2_i64_25"
	.space 1
	.ascii "div_pow2_i64_25"
	.space 1
	.ascii "mod_pow2_i64_24"
	.space 1
	.ascii "div_pow2_i64_24"
	.space 1
	.ascii "mod_pow2_i64_23"
	.space 1
	.ascii "div_pow2_i64_23"
	.space 1
	.ascii "mod_pow2_i64_22"
	.space 1
	.ascii "div_pow2_i64_22"
	.space 1
	.ascii "mod_pow2_i64_21"
	.space 1
	.ascii "div_pow2_i64_21"
	.space 1
	.ascii "mod_pow2_i64_20"
	.space 1
	.ascii "div_pow2_i64_20"
	.space 1
	.ascii "mod_pow2_i64_19"
	.space 1
	.ascii "div_pow2_i64_19"
	.space 1
	.ascii "mod_pow2_i64_18"
	.space 1
	.ascii "div_pow2_i64_18"
	.space 1
	.ascii "mod_pow2_i64_17"
	.space 1
	.ascii "div_pow2_i64_17"
	.space 1
	.ascii "mod_pow2_i64_16"
	.space 1
	.ascii "div_pow2_i64_16"
	.space 1
	.ascii "mod_pow2_i64_15"
	.space 1
	.ascii "div_pow2_i64_15"
	.space 1
	.ascii "mod_pow2_i64_14"
	.space 1
	.ascii "div_pow2_i64_14"
	.space 1
	.ascii "mod_pow2_i64_13"
	.space 1
	.ascii "div_pow2_i64_13"
	.space 1
	.ascii "mod_pow2_i64_12"
	.space 1
	.ascii "div_pow2_i64_12"
	.space 1
	.ascii "mod_pow2_i64_11"
	.space 1
	.ascii "div_pow2_i64_11"
	.space 1
	.ascii "mod_pow2_i64_10"
	.space 1
	.ascii "div_pow2_i64_10"
	.space 1
	.ascii "mod_pow2_i64_9"
	.space 1
	.ascii "div_pow2_i64_9"
	.space 1
	.ascii "mod_pow2_i64_8"
	.space 1
	.ascii "div_pow2_i64_8"
	.space 1
	.ascii "mod_pow2_i64_7"
	.space 1
	.ascii "div_pow2_i64_7"
	.space 1
	.ascii "mod_pow2_i64_6"
	.space 1
	.ascii "div_pow2_i64_6"
	.space 1
	.ascii "mod_pow2_i64_5"
	.space 1
	.ascii "div_pow2_i64_5"
	.space 1
	.ascii "mod_pow2_i64_4"
	.space 1
	.ascii "div_pow2_i64_4"
	.space 1
	.ascii "mod_pow2_i64_3"
	.space 1
	.ascii "div_pow2_i64_3"
	.space 1
	.ascii "mod_pow2_i64_2"
	.space 1
	.ascii "div_pow2_i64_2"
	.space 1
	.ascii "mod_pow2_i64_1"
	.space 1
	.ascii "div_pow2_i64_1"
	.space 1
	.ascii "mod_pow2_i64_0"
	.space 1
	.ascii "div_pow2_i64_0"
	.space 1
	.ascii "mod_pow2_u32_31"
	.space 1
	.ascii "div_pow2_u32_31"
	.space 1
	.ascii "mod_pow2_u32_30"
	.space 1
	.ascii "div_pow2_u32_30"
	.space 1
	.ascii "mod_pow2_u32_29"
	.space 1
	.ascii "div_pow2_u32_29"
	.space 1
	.ascii "mod_pow2_u32_28"
	.space 1
	.ascii "div_pow2_u32_28"
	.space 1
	.ascii "mod_pow2_u32_27"
	.space 1
	.ascii "div_pow2_u32_27"
	.space 1
	.ascii "mod_pow2_u32_26"
	.space 1
	.ascii "div_pow2_u32_26"
	.space 1
	.ascii "mod_pow2_u32_25"
	.space 1
	.ascii "div_pow2_u32_25"
	.space 1
	.ascii "mod_pow2_u32_24"
	.space 1
	.ascii "div_pow2_u32_24"
	.space 1
	.ascii "mod_pow2_u32_23"
	.space 1
	.ascii "div_pow2_u32_23"
	.space 1
	.ascii "mod_pow2_u32_22"
	.space 1
	.ascii "div_pow2_u32_22"
	.space 1
	.ascii "mod_pow2_u32_21"
	.space 1
	.ascii "div_pow2_u32_21"
	.space 1
	.ascii "mod_pow2_u32_20"
	.space 1
	.ascii "div_pow2_u32_20"
	.space 1
	.ascii "mod_pow2_u32_19"
	.space 1
	.ascii "div_pow2_u32_19"
	.space 1
	.ascii "mod_pow2_u32_18"
	.space 1
	.ascii "div_pow2_u32_18"
	.space 1
	.ascii "mod_pow2_u32_17"
	.space 1
	.ascii "div_pow2_u32_17"
	.space 1
	.ascii "mod_pow2_u32_16"
	.space 1
	.ascii "div_pow2_u32_16"
	.space 1
	.ascii "mod_pow2_u32_15"
	.space 1
	.ascii "div_pow2_u32_15"
	.space 1
	.ascii "mod_pow2_u32_14"
	.space 1
	.ascii "div_pow2_u32_14"
	.space 1
	.ascii "mod_pow2_u32_13"
	.space 1
	.ascii "div_pow2_u32_13"
	.space 1
	.ascii "mod_pow2_u32_12"
	.space 1
	.ascii "div_pow2_u32_12"
	.space 1
	.ascii "mod_pow2_u32_11"
	.space 1
	.ascii "div_pow2_u32_11"
	.space 1
	.ascii "mod_pow2_u32_10"
	.space 1
	.ascii "div_pow2_u32_10"
	.space 1
	.ascii "mod_pow2_u32_9"
	.space 1
	.ascii "div_pow2_u32_9"
	.space 1
	.ascii "mod_pow2_u32_8"
	.space 1
	.ascii "div_pow2_u32_8"
	.space 1
	.ascii "mod_pow2_u32_7"
	.space 1
	.ascii "div_pow2_u32_7"
	.space 1
	.ascii "mod_pow2_u32_6"
	.space 1
	.ascii "div_pow2_u32_6"
	.space 1
	.ascii "mod_pow2_u32_5"
	.space 1
	.ascii "div_pow2_u32_5"
	.space 1
	.ascii "mod_pow2_u32_4"
	.space 1
	.ascii "div_pow2_u32_4"
	.space 1
	.ascii "mod_pow2_u32_3"
	.space 1
	.ascii "div_pow2_u32_3"
	.space 1
	.ascii "mod_pow2_u32_2"
	.space 1
	.ascii "div_pow2_u32_2"
	.space 1
	.ascii "mod_pow2_u32_1"
	.space 1
	.ascii "div_pow2_u32_1"
	.space 1
	.ascii "mod_pow2_u32_0"
	.space 1
	.ascii "div_pow2_u32_0"
	.space 1
	.ascii "mod_pow2_i32_31"
	.space 1
	.ascii "div_pow2_i32_31"
	.space 1
	.ascii "mod_pow2_i32_30"
	.space 1
	.ascii "div_pow2_i32_30"
	.space 1
	.ascii "mod_pow2_i32_29"
	.space 1
	.ascii "div_pow2_i32_29"
	.space 1
	.ascii "mod_pow2_i32_28"
	.space 1
	.ascii "div_pow2_i32_28"
	.space 1
	.ascii "mod_pow2_i32_27"
	.space 1
	.ascii "div_pow2_i32_27"
	.space 1
	.ascii "mod_pow2_i32_26"
	.space 1
	.ascii "div_pow2_i32_26"
	.space 1
	.ascii "mod_pow2_i32_25"
	.space 1
	.ascii "div_pow2_i32_25"
	.space 1
	.ascii "mod_pow2_i32_24"
	.space 1
	.ascii "div_pow2_i32_24"
	.space 1
	.ascii "mod_pow2_i32_23"
	.space 1
	.ascii "div_pow2_i32_23"
	.space 1
	.ascii "mod_pow2_i32_22"
	.space 1
	.ascii "div_pow2_i32_22"
	.space 1
	.ascii "mod_pow2_i32_21"
	.space 1
	.ascii "div_pow2_i32_21"
	.space 1
	.ascii "mod_pow2_i32_20"
	.space 1
	.ascii "div_pow2_i32_20"
	.space 1
	.ascii "mod_pow2_i32_19"
	.space 1
	.ascii "div_pow2_i32_19"
	.space 1
	.ascii "mod_pow2_i32_18"
	.space 1
	.ascii "div_pow2_i32_18"
	.space 1
	.ascii "mod_pow2_i32_17"
	.space 1
	.ascii "div_pow2_i32_17"
	.space 1
	.ascii "mod_pow2_i32_16"
	.space 1
	.ascii "div_pow2_i32_16"
	.space 1
	.ascii "mod_pow2_i32_15"
	.space 1
	.ascii "div_pow2_i32_15"
	.space 1
	.ascii "mod_pow2_i32_14"
	.space 1
	.ascii "div_pow2_i32_14"
	.space 1
	.ascii "mod_pow2_i32_13"
	.space 1
	.ascii "div_pow2_i32_13"
	.space 1
	.ascii "mod_pow2_i32_12"
	.space 1
	.ascii "div_pow2_i32_12"
	.space 1
	.ascii "mod_pow2_i32_11"
	.space 1
	.ascii "div_pow2_i32_11"
	.space 1
	.ascii "mod_pow2_i32_10"
	.space 1
	.ascii "div_pow2_i32_10"
	.space 1
	.ascii "mod_pow2_i32_9"
	.space 1
	.ascii "div_pow2_i32_9"
	.space 1
	.ascii "mod_pow2_i32_8"
	.space 1
	.ascii "div_pow2_i32_8"
	.space 1
	.ascii "mod_pow2_i32_7"
	.space 1
	.ascii "div_pow2_i32_7"
	.space 1
	.ascii "mod_pow2_i32_6"
	.space 1
	.ascii "div_pow2_i32_6"
	.space 1
	.ascii "mod_pow2_i32_5"
	.space 1
	.ascii "div_pow2_i32_5"
	.space 1
	.ascii "mod_pow2_i32_4"
	.space 1
	.ascii "div_pow2_i32_4"
	.space 1
	.ascii "mod_pow2_i32_3"
	.space 1
	.ascii "div_pow2_i32_3"
	.space 1
	.ascii "mod_pow2_i32_2"
	.space 1
	.ascii "div_pow2_i32_2"
	.space 1
	.ascii "mod_pow2_i32_1"
	.space 1
	.ascii "div_pow2_i32_1"
	.space 1
	.ascii "mod_pow2_i32_0"
	.space 1
	.ascii "div_pow2_i32_0"
	.space 2
	.quad	_divmod_pow2_M3
	.quad	_L_1
	.quad	_divmod_pow2__mod_pow2_LC_63
	.quad	_L_1+15
	.quad	_divmod_pow2__div_pow2_LC_63
	.quad	_L_1+30
	.quad	_divmod_pow2__mod_pow2_LC_62
	.quad	_L_1+45
	.quad	_divmod_pow2__div_pow2_LC_62
	.quad	_L_1+60
	.quad	_divmod_pow2__mod_pow2_LC_61
	.quad	_L_1+75
	.quad	_divmod_pow2__div_pow2_LC_61
	.quad	_L_1+90
	.quad	_divmod_pow2__mod_pow2_LC_60
	.quad	_L_1+105
	.quad	_divmod_pow2__div_pow2_LC_60
	.quad	_L_1+120
	.quad	_divmod_pow2__mod_pow2_LC_59
	.quad	_L_1+135
	.quad	_divmod_pow2__div_pow2_LC_59
	.quad	_L_1+150
	.quad	_divmod_pow2__mod_pow2_LC_58
	.quad	_L_1+165
	.quad	_divmod_pow2__div_pow2_LC_58
	.quad	_L_1+180
	.quad	_divmod_pow2__mod_pow2_LC_57
	.quad	_L_1+195
	.quad	_divmod_pow2__div_pow2_LC_57
	.quad	_L_1+210
	.quad	_divmod_pow2__mod_pow2_LC_56
	.quad	_L_1+225
	.quad	_divmod_pow2__div_pow2_LC_56
	.quad	_L_1+240
	.quad	_divmod_pow2__mod_pow2_LC_55
	.quad	_L_1+255
	.quad	_divmod_pow2__div_pow2_LC_55
	.quad	_L_1+270
	.quad	_divmod_pow2__mod_pow2_LC_54
	.quad	_L_1+285
	.quad	_divmod_pow2__div_pow2_LC_54
	.quad	_L_1+300
	.quad	_divmod_pow2__mod_pow2_LC_53
	.quad	_L_1+315
	.quad	_divmod_pow2__div_pow2_LC_53
	.quad	_L_1+330
	.quad	_divmod_pow2__mod_pow2_LC_52
	.quad	_L_1+345
	.quad	_divmod_pow2__div_pow2_LC_52
	.quad	_L_1+360
	.quad	_divmod_pow2__mod_pow2_LC_51
	.quad	_L_1+375
	.quad	_divmod_pow2__div_pow2_LC_51
	.quad	_L_1+390
	.quad	_divmod_pow2__mod_pow2_LC_50
	.quad	_L_1+405
	.quad	_divmod_pow2__div_pow2_LC_50
	.quad	_L_1+420
	.quad	_divmod_pow2__mod_pow2_LC_49
	.quad	_L_1+435
	.quad	_divmod_pow2__div_pow2_LC_49
	.quad	_L_1+450
	.quad	_divmod_pow2__mod_pow2_LC_48
	.quad	_L_1+465
	.quad	_divmod_pow2__div_pow2_LC_48
	.quad	_L_1+480
	.quad	_divmod_pow2__mod_pow2_LC_47
	.quad	_L_1+495
	.quad	_divmod_pow2__div_pow2_LC_47
	.quad	_L_1+510
	.quad	_divmod_pow2__mod_pow2_LC_46
	.quad	_L_1+525
	.quad	_divmod_pow2__div_pow2_LC_46
	.quad	_L_1+540
	.quad	_divmod_pow2__mod_pow2_LC_45
	.quad	_L_1+555
	.quad	_divmod_pow2__div_pow2_LC_45
	.quad	_L_1+570
	.quad	_divmod_pow2__mod_pow2_LC_44
	.quad	_L_1+585
	.quad	_divmod_pow2__div_pow2_LC_44
	.quad	_L_1+600
	.quad	_divmod_pow2__mod_pow2_LC_43
	.quad	_L_1+615
	.quad	_divmod_pow2__div_pow2_LC_43
	.quad	_L_1+630
	.quad	_divmod_pow2__mod_pow2_LC_42
	.quad	_L_1+645
	.quad	_divmod_pow2__div_pow2_LC_42
	.quad	_L_1+660
	.quad	_divmod_pow2__mod_pow2_LC_41
	.quad	_L_1+675
	.quad	_divmod_pow2__div_pow2_LC_41
	.quad	_L_1+690
	.quad	_divmod_pow2__mod_pow2_LC_40
	.quad	_L_1+705
	.quad	_divmod_pow2__div_pow2_LC_40
	.quad	_L_1+720
	.quad	_divmod_pow2__mod_pow2_LC_39
	.quad	_L_1+735
	.quad	_divmod_pow2__div_pow2_LC_39
	.quad	_L_1+750
	.quad	_divmod_pow2__mod_pow2_LC_38
	.quad	_L_1+765
	.quad	_divmod_pow2__div_pow2_LC_38
	.quad	_L_1+780
	.quad	_divmod_pow2__mod_pow2_LC_37
	.quad	_L_1+795
	.quad	_divmod_pow2__div_pow2_LC_37
	.quad	_L_1+810
	.quad	_divmod_pow2__mod_pow2_LC_36
	.quad	_L_1+825
	.quad	_divmod_pow2__div_pow2_LC_36
	.quad	_L_1+840
	.quad	_divmod_pow2__mod_pow2_LC_35
	.quad	_L_1+855
	.quad	_divmod_pow2__div_pow2_LC_35
	.quad	_L_1+870
	.quad	_divmod_pow2__mod_pow2_LC_34
	.quad	_L_1+885
	.quad	_divmod_pow2__div_pow2_LC_34
	.quad	_L_1+900
	.quad	_divmod_pow2__mod_pow2_LC_33
	.quad	_L_1+915
	.quad	_divmod_pow2__div_pow2_LC_33
	.quad	_L_1+930
	.quad	_divmod_pow2__mod_pow2_LC_32
	.quad	_L_1+945
	.quad	_divmod_pow2__div_pow2_LC_32
	.quad	_L_1+960
	.quad	_divmod_pow2__mod_pow2_LC_31
	.quad	_L_1+975
	.quad	_divmod_pow2__div_pow2_LC_31
	.quad	_L_1+990
	.quad	_divmod_pow2__mod_pow2_LC_30
	.quad	_L_1+1005
	.quad	_divmod_pow2__div_pow2_LC_30
	.quad	_L_1+1020
	.quad	_divmod_pow2__mod_pow2_LC_29
	.quad	_L_1+1035
	.quad	_divmod_pow2__div_pow2_LC_29
	.quad	_L_1+1050
	.quad	_divmod_pow2__mod_pow2_LC_28
	.quad	_L_1+1065
	.quad	_divmod_pow2__div_pow2_LC_28
	.quad	_L_1+1080
	.quad	_divmod_pow2__mod_pow2_LC_27
	.quad	_L_1+1095
	.quad	_divmod_pow2__div_pow2_LC_27
	.quad	_L_1+1110
	.quad	_divmod_pow2__mod_pow2_LC_26
	.quad	_L_1+1125
	.quad	_divmod_pow2__div_pow2_LC_26
	.quad	_L_1+1140
	.quad	_divmod_pow2__mod_pow2_LC_25
	.quad	_L_1+1155
	.quad	_divmod_pow2__div_pow2_LC_25
	.quad	_L_1+1170
	.quad	_divmod_pow2__mod_pow2_LC_24
	.quad	_L_1+1185
	.quad	_divmod_pow2__div_pow2_LC_24
	.quad	_L_1+1200
	.quad	_divmod_pow2__mod_pow2_LC_23
	.quad	_L_1+1215
	.quad	_divmod_pow2__div_pow2_LC_23
	.quad	_L_1+1230
	.quad	_divmod_pow2__mod_pow2_LC_22
	.quad	_L_1+1245
	.quad	_divmod_pow2__div_pow2_LC_22
	.quad	_L_1+1260
	.quad	_divmod_pow2__mod_pow2_LC_21
	.quad	_L_1+1275
	.quad	_divmod_pow2__div_pow2_LC_21
	.quad	_L_1+1290
	.quad	_divmod_pow2__mod_pow2_LC_20
	.quad	_L_1+1305
	.quad	_divmod_pow2__div_pow2_LC_20
	.quad	_L_1+1320
	.quad	_divmod_pow2__mod_pow2_LC_19
	.quad	_L_1+1335
	.quad	_divmod_pow2__div_pow2_LC_19
	.quad	_L_1+1350
	.quad	_divmod_pow2__mod_pow2_LC_18
	.quad	_L_1+1365
	.quad	_divmod_pow2__div_pow2_LC_18
	.quad	_L_1+1380
	.quad	_divmod_pow2__mod_pow2_LC_17
	.quad	_L_1+1395
	.quad	_divmod_pow2__div_pow2_LC_17
	.quad	_L_1+1410
	.quad	_divmod_pow2__mod_pow2_LC_16
	.quad	_L_1+1425
	.quad	_divmod_pow2__div_pow2_LC_16
	.quad	_L_1+1440
	.quad	_divmod_pow2__mod_pow2_LC_15
	.quad	_L_1+1455
	.quad	_divmod_pow2__div_pow2_LC_15
	.quad	_L_1+1470
	.quad	_divmod_pow2__mod_pow2_LC_14
	.quad	_L_1+1485
	.quad	_divmod_pow2__div_pow2_LC_14
	.quad	_L_1+1500
	.quad	_divmod_pow2__mod_pow2_LC_13
	.quad	_L_1+1515
	.quad	_divmod_pow2__div_pow2_LC_13
	.quad	_L_1+1530
	.quad	_divmod_pow2__mod_pow2_LC_12
	.quad	_L_1+1545
	.quad	_divmod_pow2__div_pow2_LC_12
	.quad	_L_1+1560
	.quad	_divmod_pow2__mod_pow2_LC_11
	.quad	_L_1+1575
	.quad	_divmod_pow2__div_pow2_LC_11
	.quad	_L_1+1590
	.quad	_divmod_pow2__mod_pow2_LC_10
	.quad	_L_1+1605
	.quad	_divmod_pow2__div_pow2_LC_10
	.quad	_L_1+1620
	.quad	_divmod_pow2__mod_pow2_LC_9
	.quad	_L_1+1635
	.quad	_divmod_pow2__div_pow2_LC_9
	.quad	_L_1+1649
	.quad	_divmod_pow2__mod_pow2_LC_8
	.quad	_L_1+1663
	.quad	_divmod_pow2__div_pow2_LC_8
	.quad	_L_1+1677
	.quad	_divmod_pow2__mod_pow2_LC_7
	.quad	_L_1+1691
	.quad	_divmod_pow2__div_pow2_LC_7
	.quad	_L_1+1705
	.quad	_divmod_pow2__mod_pow2_LC_6
	.quad	_L_1+1719
	.quad	_divmod_pow2__div_pow2_LC_6
	.quad	_L_1+1733
	.quad	_divmod_pow2__mod_pow2_LC_5
	.quad	_L_1+1747
	.quad	_divmod_pow2__div_pow2_LC_5
	.quad	_L_1+1761
	.quad	_divmod_pow2__mod_pow2_LC_4
	.quad	_L_1+1775
	.quad	_divmod_pow2__div_pow2_LC_4
	.quad	_L_1+1789
	.quad	_divmod_pow2__mod_pow2_LC_3
	.quad	_L_1+1803
	.quad	_divmod_pow2__div_pow2_LC_3
	.quad	_L_1+1817
	.quad	_divmod_pow2__mod_pow2_LC_2
	.quad	_L_1+1831
	.quad	_divmod_pow2__div_pow2_LC_2
	.quad	_L_1+1845
	.quad	_divmod_pow2__mod_pow2_LC_1
	.quad	_L_1+1859
	.quad	_divmod_pow2__div_pow2_LC_1
	.quad	_L_1+1873
	.quad	_divmod_pow2__mod_pow2_LC_0
	.quad	_L_1+1887
	.quad	_divmod_pow2__div_pow2_LC_0
	.quad	_L_1+1901
	.quad	_divmod_pow2__mod_pow2_L_63
	.quad	_L_1+1915
	.quad	_divmod_pow2__div_pow2_L_63
	.quad	_L_1+1929
	.quad	_divmod_pow2__mod_pow2_L_62
	.quad	_L_1+1943
	.quad	_divmod_pow2__div_pow2_L_62
	.quad	_L_1+1957
	.quad	_divmod_pow2__mod_pow2_L_61
	.quad	_L_1+1971
	.quad	_divmod_pow2__div_pow2_L_61
	.quad	_L_1+1985
	.quad	_divmod_pow2__mod_pow2_L_60
	.quad	_L_1+1999
	.quad	_divmod_pow2__div_pow2_L_60
	.quad	_L_1+2013
	.quad	_divmod_pow2__mod_pow2_L_59
	.quad	_L_1+2027
	.quad	_divmod_pow2__div_pow2_L_59
	.quad	_L_1+2041
	.quad	_divmod_pow2__mod_pow2_L_58
	.quad	_L_1+2055
	.quad	_divmod_pow2__div_pow2_L_58
	.quad	_L_1+2069
	.quad	_divmod_pow2__mod_pow2_L_57
	.quad	_L_1+2083
	.quad	_divmod_pow2__div_pow2_L_57
	.quad	_L_1+2097
	.quad	_divmod_pow2__mod_pow2_L_56
	.quad	_L_1+2111
	.quad	_divmod_pow2__div_pow2_L_56
	.quad	_L_1+2125
	.quad	_divmod_pow2__mod_pow2_L_55
	.quad	_L_1+2139
	.quad	_divmod_pow2__div_pow2_L_55
	.quad	_L_1+2153
	.quad	_divmod_pow2__mod_pow2_L_54
	.quad	_L_1+2167
	.quad	_divmod_pow2__div_pow2_L_54
	.quad	_L_1+2181
	.quad	_divmod_pow2__mod_pow2_L_53
	.quad	_L_1+2195
	.quad	_divmod_pow2__div_pow2_L_53
	.quad	_L_1+2209
	.quad	_divmod_pow2__mod_pow2_L_52
	.quad	_L_1+2223
	.quad	_divmod_pow2__div_pow2_L_52
	.quad	_L_1+2237
	.quad	_divmod_pow2__mod_pow2_L_51
	.quad	_L_1+2251
	.quad	_divmod_pow2__div_pow2_L_51
	.quad	_L_1+2265
	.quad	_divmod_pow2__mod_pow2_L_50
	.quad	_L_1+2279
	.quad	_divmod_pow2__div_pow2_L_50
	.quad	_L_1+2293
	.quad	_divmod_pow2__mod_pow2_L_49
	.quad	_L_1+2307
	.quad	_divmod_pow2__div_pow2_L_49
	.quad	_L_1+2321
	.quad	_divmod_pow2__mod_pow2_L_48
	.quad	_L_1+2335
	.quad	_divmod_pow2__div_pow2_L_48
	.quad	_L_1+2349
	.quad	_divmod_pow2__mod_pow2_L_47
	.quad	_L_1+2363
	.quad	_divmod_pow2__div_pow2_L_47
	.quad	_L_1+2377
	.quad	_divmod_pow2__mod_pow2_L_46
	.quad	_L_1+2391
	.quad	_divmod_pow2__div_pow2_L_46
	.quad	_L_1+2405
	.quad	_divmod_pow2__mod_pow2_L_45
	.quad	_L_1+2419
	.quad	_divmod_pow2__div_pow2_L_45
	.quad	_L_1+2433
	.quad	_divmod_pow2__mod_pow2_L_44
	.quad	_L_1+2447
	.quad	_divmod_pow2__div_pow2_L_44
	.quad	_L_1+2461
	.quad	_divmod_pow2__mod_pow2_L_43
	.quad	_L_1+2475
	.quad	_divmod_pow2__div_pow2_L_43
	.quad	_L_1+2489
	.quad	_divmod_pow2__mod_pow2_L_42
	.quad	_L_1+2503
	.quad	_divmod_pow2__div_pow2_L_42
	.quad	_L_1+2517
	.quad	_divmod_pow2__mod_pow2_L_41
	.quad	_L_1+2531
	.quad	_divmod_pow2__div_pow2_L_41
	.quad	_L_1+2545
	.quad	_divmod_pow2__mod_pow2_L_40
	.quad	_L_1+2559
	.quad	_divmod_pow2__div_pow2_L_40
	.quad	_L_1+2573
	.quad	_divmod_pow2__mod_pow2_L_39
	.quad	_L_1+2587
	.quad	_divmod_pow2__div_pow2_L_39
	.quad	_L_1+2601
	.quad	_divmod_pow2__mod_pow2_L_38
	.quad	_L_1+2615
	.quad	_divmod_pow2__div_pow2_L_38
	.quad	_L_1+2629
	.quad	_divmod_pow2__mod_pow2_L_37
	.quad	_L_1+2643
	.quad	_divmod_pow2__div_pow2_L_37
	.quad	_L_1+2657
	.quad	_divmod_pow2__mod_pow2_L_36
	.quad	_L_1+2671
	.quad	_divmod_pow2__div_pow2_L_36
	.quad	_L_1+2685
	.quad	_divmod_pow2__mod_pow2_L_35
	.quad	_L_1+2699
	.quad	_divmod_pow2__div_pow2_L_35
	.quad	_L_1+2713
	.quad	_divmod_pow2__mod_pow2_L_34
	.quad	_L_1+2727
	.quad	_divmod_pow2__div_pow2_L_34
	.quad	_L_1+2741
	.quad	_divmod_pow2__mod_pow2_L_33
	.quad	_L_1+2755
	.quad	_divmod_pow2__div_pow2_L_33
	.quad	_L_1+2769
	.quad	_divmod_pow2__mod_pow2_L_32
	.quad	_L_1+2783
	.quad	_divmod_pow2__div_pow2_L_32
	.quad	_L_1+2797
	.quad	_divmod_pow2__mod_pow2_L_31
	.quad	_L_1+2811
	.quad	_divmod_pow2__div_pow2_L_31
	.quad	_L_1+2825
	.quad	_divmod_pow2__mod_pow2_L_30
	.quad	_L_1+2839
	.quad	_divmod_pow2__div_pow2_L_30
	.quad	_L_1+2853
	.quad	_divmod_pow2__mod_pow2_L_29
	.quad	_L_1+2867
	.quad	_divmod_pow2__div_pow2_L_29
	.quad	_L_1+2881
	.quad	_divmod_pow2__mod_pow2_L_28
	.quad	_L_1+2895
	.quad	_divmod_pow2__div_pow2_L_28
	.quad	_L_1+2909
	.quad	_divmod_pow2__mod_pow2_L_27
	.quad	_L_1+2923
	.quad	_divmod_pow2__div_pow2_L_27
	.quad	_L_1+2937
	.quad	_divmod_pow2__mod_pow2_L_26
	.quad	_L_1+2951
	.quad	_divmod_pow2__div_pow2_L_26
	.quad	_L_1+2965
	.quad	_divmod_pow2__mod_pow2_L_25
	.quad	_L_1+2979
	.quad	_divmod_pow2__div_pow2_L_25
	.quad	_L_1+2993
	.quad	_divmod_pow2__mod_pow2_L_24
	.quad	_L_1+3007
	.quad	_divmod_pow2__div_pow2_L_24
	.quad	_L_1+3021
	.quad	_divmod_pow2__mod_pow2_L_23
	.quad	_L_1+3035
	.quad	_divmod_pow2__div_pow2_L_23
	.quad	_L_1+3049
	.quad	_divmod_pow2__mod_pow2_L_22
	.quad	_L_1+3063
	.quad	_divmod_pow2__div_pow2_L_22
	.quad	_L_1+3077
	.quad	_divmod_pow2__mod_pow2_L_21
	.quad	_L_1+3091
	.quad	_divmod_pow2__div_pow2_L_21
	.quad	_L_1+3105
	.quad	_divmod_pow2__mod_pow2_L_20
	.quad	_L_1+3119
	.quad	_divmod_pow2__div_pow2_L_20
	.quad	_L_1+3133
	.quad	_divmod_pow2__mod_pow2_L_19
	.quad	_L_1+3147
	.quad	_divmod_pow2__div_pow2_L_19
	.quad	_L_1+3161
	.quad	_divmod_pow2__mod_pow2_L_18
	.quad	_L_1+3175
	.quad	_divmod_pow2__div_pow2_L_18
	.quad	_L_1+3189
	.quad	_divmod_pow2__mod_pow2_L_17
	.quad	_L_1+3203
	.quad	_divmod_pow2__div_pow2_L_17
	.quad	_L_1+3217
	.quad	_divmod_pow2__mod_pow2_L_16
	.quad	_L_1+3231
	.quad	_divmod_pow2__div_pow2_L_16
	.quad	_L_1+3245
	.quad	_divmod_pow2__mod_pow2_L_15
	.quad	_L_1+3259
	.quad	_divmod_pow2__div_pow2_L_15
	.quad	_L_1+3273
	.quad	_divmod_pow2__mod_pow2_L_14
	.quad	_L_1+3287
	.quad	_divmod_pow2__div_pow2_L_14
	.quad	_L_1+3301
	.quad	_divmod_pow2__mod_pow2_L_13
	.quad	_L_1+3315
	.quad	_divmod_pow2__div_pow2_L_13
	.quad	_L_1+3329
	.quad	_divmod_pow2__mod_pow2_L_12
	.quad	_L_1+3343
	.quad	_divmod_pow2__div_pow2_L_12
	.quad	_L_1+3357
	.quad	_divmod_pow2__mod_pow2_L_11
	.quad	_L_1+3371
	.quad	_divmod_pow2__div_pow2_L_11
	.quad	_L_1+3385
	.quad	_divmod_pow2__mod_pow2_L_10
	.quad	_L_1+3399
	.quad	_divmod_pow2__div_pow2_L_10
	.quad	_L_1+3413
	.quad	_divmod_pow2__mod_pow2_L_9
	.quad	_L_1+3427
	.quad	_divmod_pow2__div_pow2_L_9
	.quad	_L_1+3440
	.quad	_divmod_pow2__mod_pow2_L_8
	.quad	_L_1+3453
	.quad	_divmod_pow2__div_pow2_L_8
	.quad	_L_1+3466
	.quad	_divmod_pow2__mod_pow2_L_7
	.quad	_L_1+3479
	.quad	_divmod_pow2__div_pow2_L_7
	.quad	_L_1+3492
	.quad	_divmod_pow2__mod_pow2_L_6
	.quad	_L_1+3505
	.quad	_divmod_pow2__div_pow2_L_6
	.quad	_L_1+3518
	.quad	_divmod_pow2__mod_pow2_L_5
	.quad	_L_1+3531
	.quad	_divmod_pow2__div_pow2_L_5
	.quad	_L_1+3544
	.quad	_divmod_pow2__mod_pow2_L_4
	.quad	_L_1+3557
	.quad	_divmod_pow2__div_pow2_L_4
	.quad	_L_1+3570
	.quad	_divmod_pow2__mod_pow2_L_3
	.quad	_L_1+3583
	.quad	_divmod_pow2__div_pow2_L_3
	.quad	_L_1+3596
	.quad	_divmod_pow2__mod_pow2_L_2
	.quad	_L_1+3609
	.quad	_divmod_pow2__div_pow2_L_2
	.quad	_L_1+3622
	.quad	_divmod_pow2__mod_pow2_L_1
	.quad	_L_1+3635
	.quad	_divmod_pow2__div_pow2_L_1
	.quad	_L_1+3648
	.quad	_divmod_pow2__mod_pow2_L_0
	.quad	_L_1+3661
	.quad	_divmod_pow2__div_pow2_L_0
	.quad	_L_1+3674
	.quad	_divmod_pow2__mod_pow2_C_63
	.quad	_L_1+3687
	.quad	_divmod_pow2__div_pow2_C_63
	.quad	_L_1+3701
	.quad	_divmod_pow2__mod_pow2_C_62
	.quad	_L_1+3715
	.quad	_divmod_pow2__div_pow2_C_62
	.quad	_L_1+3729
	.quad	_divmod_pow2__mod_pow2_C_61
	.quad	_L_1+3743
	.quad	_divmod_pow2__div_pow2_C_61
	.quad	_L_1+3757
	.quad	_divmod_pow2__mod_pow2_C_60
	.quad	_L_1+3771
	.quad	_divmod_pow2__div_pow2_C_60
	.quad	_L_1+3785
	.quad	_divmod_pow2__mod_pow2_C_59
	.quad	_L_1+3799
	.quad	_divmod_pow2__div_pow2_C_59
	.quad	_L_1+3813
	.quad	_divmod_pow2__mod_pow2_C_58
	.quad	_L_1+3827
	.quad	_divmod_pow2__div_pow2_C_58
	.quad	_L_1+3841
	.quad	_divmod_pow2__mod_pow2_C_57
	.quad	_L_1+3855
	.quad	_divmod_pow2__div_pow2_C_57
	.quad	_L_1+3869
	.quad	_divmod_pow2__mod_pow2_C_56
	.quad	_L_1+3883
	.quad	_divmod_pow2__div_pow2_C_56
	.quad	_L_1+3897
	.quad	_divmod_pow2__mod_pow2_C_55
	.quad	_L_1+3911
	.quad	_divmod_pow2__div_pow2_C_55
	.quad	_L_1+3925
	.quad	_divmod_pow2__mod_pow2_C_54
	.quad	_L_1+3939
	.quad	_divmod_pow2__div_pow2_C_54
	.quad	_L_1+3953
	.quad	_divmod_pow2__mod_pow2_C_53
	.quad	_L_1+3967
	.quad	_divmod_pow2__div_pow2_C_53
	.quad	_L_1+3981
	.quad	_divmod_pow2__mod_pow2_C_52
	.quad	_L_1+3995
	.quad	_divmod_pow2__div_pow2_C_52
	.quad	_L_1+4009
	.quad	_divmod_pow2__mod_pow2_C_51
	.quad	_L_1+4023
	.quad	_divmod_pow2__div_pow2_C_51
	.quad	_L_1+4037
	.quad	_divmod_pow2__mod_pow2_C_50
	.quad	_L_1+4051
	.quad	_divmod_pow2__div_pow2_C_50
	.quad	_L_1+4065
	.quad	_divmod_pow2__mod_pow2_C_49
	.quad	_L_1+4079
	.quad	_divmod_pow2__div_pow2_C_49
	.quad	_L_1+4093
	.quad	_divmod_pow2__mod_pow2_C_48
	.quad	_L_1+4107
	.quad	_divmod_pow2__div_pow2_C_48
	.quad	_L_1+4121
	.quad	_divmod_pow2__mod_pow2_C_47
	.quad	_L_1+4135
	.quad	_divmod_pow2__div_pow2_C_47
	.quad	_L_1+4149
	.quad	_divmod_pow2__mod_pow2_C_46
	.quad	_L_1+4163
	.quad	_divmod_pow2__div_pow2_C_46
	.quad	_L_1+4177
	.quad	_divmod_pow2__mod_pow2_C_45
	.quad	_L_1+4191
	.quad	_divmod_pow2__div_pow2_C_45
	.quad	_L_1+4205
	.quad	_divmod_pow2__mod_pow2_C_44
	.quad	_L_1+4219
	.quad	_divmod_pow2__div_pow2_C_44
	.quad	_L_1+4233
	.quad	_divmod_pow2__mod_pow2_C_43
	.quad	_L_1+4247
	.quad	_divmod_pow2__div_pow2_C_43
	.quad	_L_1+4261
	.quad	_divmod_pow2__mod_pow2_C_42
	.quad	_L_1+4275
	.quad	_divmod_pow2__div_pow2_C_42
	.quad	_L_1+4289
	.quad	_divmod_pow2__mod_pow2_C_41
	.quad	_L_1+4303
	.quad	_divmod_pow2__div_pow2_C_41
	.quad	_L_1+4317
	.quad	_divmod_pow2__mod_pow2_C_40
	.quad	_L_1+4331
	.quad	_divmod_pow2__div_pow2_C_40
	.quad	_L_1+4345
	.quad	_divmod_pow2__mod_pow2_C_39
	.quad	_L_1+4359
	.quad	_divmod_pow2__div_pow2_C_39
	.quad	_L_1+4373
	.quad	_divmod_pow2__mod_pow2_C_38
	.quad	_L_1+4387
	.quad	_divmod_pow2__div_pow2_C_38
	.quad	_L_1+4401
	.quad	_divmod_pow2__mod_pow2_C_37
	.quad	_L_1+4415
	.quad	_divmod_pow2__div_pow2_C_37
	.quad	_L_1+4429
	.quad	_divmod_pow2__mod_pow2_C_36
	.quad	_L_1+4443
	.quad	_divmod_pow2__div_pow2_C_36
	.quad	_L_1+4457
	.quad	_divmod_pow2__mod_pow2_C_35
	.quad	_L_1+4471
	.quad	_divmod_pow2__div_pow2_C_35
	.quad	_L_1+4485
	.quad	_divmod_pow2__mod_pow2_C_34
	.quad	_L_1+4499
	.quad	_divmod_pow2__div_pow2_C_34
	.quad	_L_1+4513
	.quad	_divmod_pow2__mod_pow2_C_33
	.quad	_L_1+4527
	.quad	_divmod_pow2__div_pow2_C_33
	.quad	_L_1+4541
	.quad	_divmod_pow2__mod_pow2_C_32
	.quad	_L_1+4555
	.quad	_divmod_pow2__div_pow2_C_32
	.quad	_L_1+4569
	.quad	_divmod_pow2__mod_pow2_C_31
	.quad	_L_1+4583
	.quad	_divmod_pow2__div_pow2_C_31
	.quad	_L_1+4597
	.quad	_divmod_pow2__mod_pow2_C_30
	.quad	_L_1+4611
	.quad	_divmod_pow2__div_pow2_C_30
	.quad	_L_1+4625
	.quad	_divmod_pow2__mod_pow2_C_29
	.quad	_L_1+4639
	.quad	_divmod_pow2__div_pow2_C_29
	.quad	_L_1+4653
	.quad	_divmod_pow2__mod_pow2_C_28
	.quad	_L_1+4667
	.quad	_divmod_pow2__div_pow2_C_28
	.quad	_L_1+4681
	.quad	_divmod_pow2__mod_pow2_C_27
	.quad	_L_1+4695
	.quad	_divmod_pow2__div_pow2_C_27
	.quad	_L_1+4709
	.quad	_divmod_pow2__mod_pow2_C_26
	.quad	_L_1+4723
	.quad	_divmod_pow2__div_pow2_C_26
	.quad	_L_1+4737
	.quad	_divmod_pow2__mod_pow2_C_25
	.quad	_L_1+4751
	.quad	_divmod_pow2__div_pow2_C_25
	.quad	_L_1+4765
	.quad	_divmod_pow2__mod_pow2_C_24
	.quad	_L_1+4779
	.quad	_divmod_pow2__div_pow2_C_24
	.quad	_L_1+4793
	.quad	_divmod_pow2__mod_pow2_C_23
	.quad	_L_1+4807
	.quad	_divmod_pow2__div_pow2_C_23
	.quad	_L_1+4821
	.quad	_divmod_pow2__mod_pow2_C_22
	.quad	_L_1+4835
	.quad	_divmod_pow2__div_pow2_C_22
	.quad	_L_1+4849
	.quad	_divmod_pow2__mod_pow2_C_21
	.quad	_L_1+4863
	.quad	_divmod_pow2__div_pow2_C_21
	.quad	_L_1+4877
	.quad	_divmod_pow2__mod_pow2_C_20
	.quad	_L_1+4891
	.quad	_divmod_pow2__div_pow2_C_20
	.quad	_L_1+4905
	.quad	_divmod_pow2__mod_pow2_C_19
	.quad	_L_1+4919
	.quad	_divmod_pow2__div_pow2_C_19
	.quad	_L_1+4933
	.quad	_divmod_pow2__mod_pow2_C_18
	.quad	_L_1+4947
	.quad	_divmod_pow2__div_pow2_C_18
	.quad	_L_1+4961
	.quad	_divmod_pow2__mod_pow2_C_17
	.quad	_L_1+4975
	.quad	_divmod_pow2__div_pow2_C_17
	.quad	_L_1+4989
	.quad	_divmod_pow2__mod_pow2_C_16
	.quad	_L_1+5003
	.quad	_divmod_pow2__div_pow2_C_16
	.quad	_L_1+5017
	.quad	_divmod_pow2__mod_pow2_C_15
	.quad	_L_1+5031
	.quad	_divmod_pow2__div_pow2_C_15
	.quad	_L_1+5045
	.quad	_divmod_pow2__mod_pow2_C_14
	.quad	_L_1+5059
	.quad	_divmod_pow2__div_pow2_C_14
	.quad	_L_1+5073
	.quad	_divmod_pow2__mod_pow2_C_13
	.quad	_L_1+5087
	.quad	_divmod_pow2__div_pow2_C_13
	.quad	_L_1+5101
	.quad	_divmod_pow2__mod_pow2_C_12
	.quad	_L_1+5115
	.quad	_divmod_pow2__div_pow2_C_12
	.quad	_L_1+5129
	.quad	_divmod_pow2__mod_pow2_C_11
	.quad	_L_1+5143
	.quad	_divmod_pow2__div_pow2_C_11
	.quad	_L_1+5157
	.quad	_divmod_pow2__mod_pow2_C_10
	.quad	_L_1+5171
	.quad	_divmod_pow2__div_pow2_C_10
	.quad	_L_1+5185
	.quad	_divmod_pow2__mod_pow2_C_9
	.quad	_L_1+5199
	.quad	_divmod_pow2__div_pow2_C_9
	.quad	_L_1+5212
	.quad	_divmod_pow2__mod_pow2_C_8
	.quad	_L_1+5225
	.quad	_divmod_pow2__div_pow2_C_8
	.quad	_L_1+5238
	.quad	_divmod_pow2__mod_pow2_C_7
	.quad	_L_1+5251
	.quad	_divmod_pow2__div_pow2_C_7
	.quad	_L_1+5264
	.quad	_divmod_pow2__mod_pow2_C_6
	.quad	_L_1+5277
	.quad	_divmod_pow2__div_pow2_C_6
	.quad	_L_1+5290
	.quad	_divmod_pow2__mod_pow2_C_5
	.quad	_L_1+5303
	.quad	_divmod_pow2__div_pow2_C_5
	.quad	_L_1+5316
	.quad	_divmod_pow2__mod_pow2_C_4
	.quad	_L_1+5329
	.quad	_divmod_pow2__div_pow2_C_4
	.quad	_L_1+5342
	.quad	_divmod_pow2__mod_pow2_C_3
	.quad	_L_1+5355
	.quad	_divmod_pow2__div_pow2_C_3
	.quad	_L_1+5368
	.quad	_divmod_pow2__mod_pow2_C_2
	.quad	_L_1+5381
	.quad	_divmod_pow2__div_pow2_C_2
	.quad	_L_1+5394
	.quad	_divmod_pow2__mod_pow2_C_1
	.quad	_L_1+5407
	.quad	_divmod_pow2__div_pow2_C_1
	.quad	_L_1+5420
	.quad	_divmod_pow2__mod_pow2_C_0
	.quad	_L_1+5433
	.quad	_divmod_pow2__div_pow2_C_0
	.quad	_L_1+5446
	.quad	_divmod_pow2__mod_pow2_I_63
	.quad	_L_1+5459
	.quad	_divmod_pow2__div_pow2_I_63
	.quad	_L_1+5473
	.quad	_divmod_pow2__mod_pow2_I_62
	.quad	_L_1+5487
	.quad	_divmod_pow2__div_pow2_I_62
	.quad	_L_1+5501
	.quad	_divmod_pow2__mod_pow2_I_61
	.quad	_L_1+5515
	.quad	_divmod_pow2__div_pow2_I_61
	.quad	_L_1+5529
	.quad	_divmod_pow2__mod_pow2_I_60
	.quad	_L_1+5543
	.quad	_divmod_pow2__div_pow2_I_60
	.quad	_L_1+5557
	.quad	_divmod_pow2__mod_pow2_I_59
	.quad	_L_1+5571
	.quad	_divmod_pow2__div_pow2_I_59
	.quad	_L_1+5585
	.quad	_divmod_pow2__mod_pow2_I_58
	.quad	_L_1+5599
	.quad	_divmod_pow2__div_pow2_I_58
	.quad	_L_1+5613
	.quad	_divmod_pow2__mod_pow2_I_57
	.quad	_L_1+5627
	.quad	_divmod_pow2__div_pow2_I_57
	.quad	_L_1+5641
	.quad	_divmod_pow2__mod_pow2_I_56
	.quad	_L_1+5655
	.quad	_divmod_pow2__div_pow2_I_56
	.quad	_L_1+5669
	.quad	_divmod_pow2__mod_pow2_I_55
	.quad	_L_1+5683
	.quad	_divmod_pow2__div_pow2_I_55
	.quad	_L_1+5697
	.quad	_divmod_pow2__mod_pow2_I_54
	.quad	_L_1+5711
	.quad	_divmod_pow2__div_pow2_I_54
	.quad	_L_1+5725
	.quad	_divmod_pow2__mod_pow2_I_53
	.quad	_L_1+5739
	.quad	_divmod_pow2__div_pow2_I_53
	.quad	_L_1+5753
	.quad	_divmod_pow2__mod_pow2_I_52
	.quad	_L_1+5767
	.quad	_divmod_pow2__div_pow2_I_52
	.quad	_L_1+5781
	.quad	_divmod_pow2__mod_pow2_I_51
	.quad	_L_1+5795
	.quad	_divmod_pow2__div_pow2_I_51
	.quad	_L_1+5809
	.quad	_divmod_pow2__mod_pow2_I_50
	.quad	_L_1+5823
	.quad	_divmod_pow2__div_pow2_I_50
	.quad	_L_1+5837
	.quad	_divmod_pow2__mod_pow2_I_49
	.quad	_L_1+5851
	.quad	_divmod_pow2__div_pow2_I_49
	.quad	_L_1+5865
	.quad	_divmod_pow2__mod_pow2_I_48
	.quad	_L_1+5879
	.quad	_divmod_pow2__div_pow2_I_48
	.quad	_L_1+5893
	.quad	_divmod_pow2__mod_pow2_I_47
	.quad	_L_1+5907
	.quad	_divmod_pow2__div_pow2_I_47
	.quad	_L_1+5921
	.quad	_divmod_pow2__mod_pow2_I_46
	.quad	_L_1+5935
	.quad	_divmod_pow2__div_pow2_I_46
	.quad	_L_1+5949
	.quad	_divmod_pow2__mod_pow2_I_45
	.quad	_L_1+5963
	.quad	_divmod_pow2__div_pow2_I_45
	.quad	_L_1+5977
	.quad	_divmod_pow2__mod_pow2_I_44
	.quad	_L_1+5991
	.quad	_divmod_pow2__div_pow2_I_44
	.quad	_L_1+6005
	.quad	_divmod_pow2__mod_pow2_I_43
	.quad	_L_1+6019
	.quad	_divmod_pow2__div_pow2_I_43
	.quad	_L_1+6033
	.quad	_divmod_pow2__mod_pow2_I_42
	.quad	_L_1+6047
	.quad	_divmod_pow2__div_pow2_I_42
	.quad	_L_1+6061
	.quad	_divmod_pow2__mod_pow2_I_41
	.quad	_L_1+6075
	.quad	_divmod_pow2__div_pow2_I_41
	.quad	_L_1+6089
	.quad	_divmod_pow2__mod_pow2_I_40
	.quad	_L_1+6103
	.quad	_divmod_pow2__div_pow2_I_40
	.quad	_L_1+6117
	.quad	_divmod_pow2__mod_pow2_I_39
	.quad	_L_1+6131
	.quad	_divmod_pow2__div_pow2_I_39
	.quad	_L_1+6145
	.quad	_divmod_pow2__mod_pow2_I_38
	.quad	_L_1+6159
	.quad	_divmod_pow2__div_pow2_I_38
	.quad	_L_1+6173
	.quad	_divmod_pow2__mod_pow2_I_37
	.quad	_L_1+6187
	.quad	_divmod_pow2__div_pow2_I_37
	.quad	_L_1+6201
	.quad	_divmod_pow2__mod_pow2_I_36
	.quad	_L_1+6215
	.quad	_divmod_pow2__div_pow2_I_36
	.quad	_L_1+6229
	.quad	_divmod_pow2__mod_pow2_I_35
	.quad	_L_1+6243
	.quad	_divmod_pow2__div_pow2_I_35
	.quad	_L_1+6257
	.quad	_divmod_pow2__mod_pow2_I_34
	.quad	_L_1+6271
	.quad	_divmod_pow2__div_pow2_I_34
	.quad	_L_1+6285
	.quad	_divmod_pow2__mod_pow2_I_33
	.quad	_L_1+6299
	.quad	_divmod_pow2__div_pow2_I_33
	.quad	_L_1+6313
	.quad	_divmod_pow2__mod_pow2_I_32
	.quad	_L_1+6327
	.quad	_divmod_pow2__div_pow2_I_32
	.quad	_L_1+6341
	.quad	_divmod_pow2__mod_pow2_I_31
	.quad	_L_1+6355
	.quad	_divmod_pow2__div_pow2_I_31
	.quad	_L_1+6369
	.quad	_divmod_pow2__mod_pow2_I_30
	.quad	_L_1+6383
	.quad	_divmod_pow2__div_pow2_I_30
	.quad	_L_1+6397
	.quad	_divmod_pow2__mod_pow2_I_29
	.quad	_L_1+6411
	.quad	_divmod_pow2__div_pow2_I_29
	.quad	_L_1+6425
	.quad	_divmod_pow2__mod_pow2_I_28
	.quad	_L_1+6439
	.quad	_divmod_pow2__div_pow2_I_28
	.quad	_L_1+6453
	.quad	_divmod_pow2__mod_pow2_I_27
	.quad	_L_1+6467
	.quad	_divmod_pow2__div_pow2_I_27
	.quad	_L_1+6481
	.quad	_divmod_pow2__mod_pow2_I_26
	.quad	_L_1+6495
	.quad	_divmod_pow2__div_pow2_I_26
	.quad	_L_1+6509
	.quad	_divmod_pow2__mod_pow2_I_25
	.quad	_L_1+6523
	.quad	_divmod_pow2__div_pow2_I_25
	.quad	_L_1+6537
	.quad	_divmod_pow2__mod_pow2_I_24
	.quad	_L_1+6551
	.quad	_divmod_pow2__div_pow2_I_24
	.quad	_L_1+6565
	.quad	_divmod_pow2__mod_pow2_I_23
	.quad	_L_1+6579
	.quad	_divmod_pow2__div_pow2_I_23
	.quad	_L_1+6593
	.quad	_divmod_pow2__mod_pow2_I_22
	.quad	_L_1+6607
	.quad	_divmod_pow2__div_pow2_I_22
	.quad	_L_1+6621
	.quad	_divmod_pow2__mod_pow2_I_21
	.quad	_L_1+6635
	.quad	_divmod_pow2__div_pow2_I_21
	.quad	_L_1+6649
	.quad	_divmod_pow2__mod_pow2_I_20
	.quad	_L_1+6663
	.quad	_divmod_pow2__div_pow2_I_20
	.quad	_L_1+6677
	.quad	_divmod_pow2__mod_pow2_I_19
	.quad	_L_1+6691
	.quad	_divmod_pow2__div_pow2_I_19
	.quad	_L_1+6705
	.quad	_divmod_pow2__mod_pow2_I_18
	.quad	_L_1+6719
	.quad	_divmod_pow2__div_pow2_I_18
	.quad	_L_1+6733
	.quad	_divmod_pow2__mod_pow2_I_17
	.quad	_L_1+6747
	.quad	_divmod_pow2__div_pow2_I_17
	.quad	_L_1+6761
	.quad	_divmod_pow2__mod_pow2_I_16
	.quad	_L_1+6775
	.quad	_divmod_pow2__div_pow2_I_16
	.quad	_L_1+6789
	.quad	_divmod_pow2__mod_pow2_I_15
	.quad	_L_1+6803
	.quad	_divmod_pow2__div_pow2_I_15
	.quad	_L_1+6817
	.quad	_divmod_pow2__mod_pow2_I_14
	.quad	_L_1+6831
	.quad	_divmod_pow2__div_pow2_I_14
	.quad	_L_1+6845
	.quad	_divmod_pow2__mod_pow2_I_13
	.quad	_L_1+6859
	.quad	_divmod_pow2__div_pow2_I_13
	.quad	_L_1+6873
	.quad	_divmod_pow2__mod_pow2_I_12
	.quad	_L_1+6887
	.quad	_divmod_pow2__div_pow2_I_12
	.quad	_L_1+6901
	.quad	_divmod_pow2__mod_pow2_I_11
	.quad	_L_1+6915
	.quad	_divmod_pow2__div_pow2_I_11
	.quad	_L_1+6929
	.quad	_divmod_pow2__mod_pow2_I_10
	.quad	_L_1+6943
	.quad	_divmod_pow2__div_pow2_I_10
	.quad	_L_1+6957
	.quad	_divmod_pow2__mod_pow2_I_9
	.quad	_L_1+6971
	.quad	_divmod_pow2__div_pow2_I_9
	.quad	_L_1+6984
	.quad	_divmod_pow2__mod_pow2_I_8
	.quad	_L_1+6997
	.quad	_divmod_pow2__div_pow2_I_8
	.quad	_L_1+7010
	.quad	_divmod_pow2__mod_pow2_I_7
	.quad	_L_1+7023
	.quad	_divmod_pow2__div_pow2_I_7
	.quad	_L_1+7036
	.quad	_divmod_pow2__mod_pow2_I_6
	.quad	_L_1+7049
	.quad	_divmod_pow2__div_pow2_I_6
	.quad	_L_1+7062
	.quad	_divmod_pow2__mod_pow2_I_5
	.quad	_L_1+7075
	.quad	_divmod_pow2__div_pow2_I_5
	.quad	_L_1+7088
	.quad	_divmod_pow2__mod_pow2_I_4
	.quad	_L_1+7101
	.quad	_divmod_pow2__div_pow2_I_4
	.quad	_L_1+7114
	.quad	_divmod_pow2__mod_pow2_I_3
	.quad	_L_1+7127
	.quad	_divmod_pow2__div_pow2_I_3
	.quad	_L_1+7140
	.quad	_divmod_pow2__mod_pow2_I_2
	.quad	_L_1+7153
	.quad	_divmod_pow2__div_pow2_I_2
	.quad	_L_1+7166
	.quad	_divmod_pow2__mod_pow2_I_1
	.quad	_L_1+7179
	.quad	_divmod_pow2__div_pow2_I_1
	.quad	_L_1+7192
	.quad	_divmod_pow2__mod_pow2_I_0
	.quad	_L_1+7205
	.quad	_divmod_pow2__div_pow2_I_0
	.quad	_L_1+7218
	.quad	_divmod_pow2__mod_pow2_u64_63
	.quad	_L_1+7231
	.quad	_divmod_pow2__div_pow2_u64_63
	.quad	_L_1+7247
	.quad	_divmod_pow2__mod_pow2_u64_62
	.quad	_L_1+7263
	.quad	_divmod_pow2__div_pow2_u64_62
	.quad	_L_1+7279
	.quad	_divmod_pow2__mod_pow2_u64_61
	.quad	_L_1+7295
	.quad	_divmod_pow2__div_pow2_u64_61
	.quad	_L_1+7311
	.quad	_divmod_pow2__mod_pow2_u64_60
	.quad	_L_1+7327
	.quad	_divmod_pow2__div_pow2_u64_60
	.quad	_L_1+7343
	.quad	_divmod_pow2__mod_pow2_u64_59
	.quad	_L_1+7359
	.quad	_divmod_pow2__div_pow2_u64_59
	.quad	_L_1+7375
	.quad	_divmod_pow2__mod_pow2_u64_58
	.quad	_L_1+7391
	.quad	_divmod_pow2__div_pow2_u64_58
	.quad	_L_1+7407
	.quad	_divmod_pow2__mod_pow2_u64_57
	.quad	_L_1+7423
	.quad	_divmod_pow2__div_pow2_u64_57
	.quad	_L_1+7439
	.quad	_divmod_pow2__mod_pow2_u64_56
	.quad	_L_1+7455
	.quad	_divmod_pow2__div_pow2_u64_56
	.quad	_L_1+7471
	.quad	_divmod_pow2__mod_pow2_u64_55
	.quad	_L_1+7487
	.quad	_divmod_pow2__div_pow2_u64_55
	.quad	_L_1+7503
	.quad	_divmod_pow2__mod_pow2_u64_54
	.quad	_L_1+7519
	.quad	_divmod_pow2__div_pow2_u64_54
	.quad	_L_1+7535
	.quad	_divmod_pow2__mod_pow2_u64_53
	.quad	_L_1+7551
	.quad	_divmod_pow2__div_pow2_u64_53
	.quad	_L_1+7567
	.quad	_divmod_pow2__mod_pow2_u64_52
	.quad	_L_1+7583
	.quad	_divmod_pow2__div_pow2_u64_52
	.quad	_L_1+7599
	.quad	_divmod_pow2__mod_pow2_u64_51
	.quad	_L_1+7615
	.quad	_divmod_pow2__div_pow2_u64_51
	.quad	_L_1+7631
	.quad	_divmod_pow2__mod_pow2_u64_50
	.quad	_L_1+7647
	.quad	_divmod_pow2__div_pow2_u64_50
	.quad	_L_1+7663
	.quad	_divmod_pow2__mod_pow2_u64_49
	.quad	_L_1+7679
	.quad	_divmod_pow2__div_pow2_u64_49
	.quad	_L_1+7695
	.quad	_divmod_pow2__mod_pow2_u64_48
	.quad	_L_1+7711
	.quad	_divmod_pow2__div_pow2_u64_48
	.quad	_L_1+7727
	.quad	_divmod_pow2__mod_pow2_u64_47
	.quad	_L_1+7743
	.quad	_divmod_pow2__div_pow2_u64_47
	.quad	_L_1+7759
	.quad	_divmod_pow2__mod_pow2_u64_46
	.quad	_L_1+7775
	.quad	_divmod_pow2__div_pow2_u64_46
	.quad	_L_1+7791
	.quad	_divmod_pow2__mod_pow2_u64_45
	.quad	_L_1+7807
	.quad	_divmod_pow2__div_pow2_u64_45
	.quad	_L_1+7823
	.quad	_divmod_pow2__mod_pow2_u64_44
	.quad	_L_1+7839
	.quad	_divmod_pow2__div_pow2_u64_44
	.quad	_L_1+7855
	.quad	_divmod_pow2__mod_pow2_u64_43
	.quad	_L_1+7871
	.quad	_divmod_pow2__div_pow2_u64_43
	.quad	_L_1+7887
	.quad	_divmod_pow2__mod_pow2_u64_42
	.quad	_L_1+7903
	.quad	_divmod_pow2__div_pow2_u64_42
	.quad	_L_1+7919
	.quad	_divmod_pow2__mod_pow2_u64_41
	.quad	_L_1+7935
	.quad	_divmod_pow2__div_pow2_u64_41
	.quad	_L_1+7951
	.quad	_divmod_pow2__mod_pow2_u64_40
	.quad	_L_1+7967
	.quad	_divmod_pow2__div_pow2_u64_40
	.quad	_L_1+7983
	.quad	_divmod_pow2__mod_pow2_u64_39
	.quad	_L_1+7999
	.quad	_divmod_pow2__div_pow2_u64_39
	.quad	_L_1+8015
	.quad	_divmod_pow2__mod_pow2_u64_38
	.quad	_L_1+8031
	.quad	_divmod_pow2__div_pow2_u64_38
	.quad	_L_1+8047
	.quad	_divmod_pow2__mod_pow2_u64_37
	.quad	_L_1+8063
	.quad	_divmod_pow2__div_pow2_u64_37
	.quad	_L_1+8079
	.quad	_divmod_pow2__mod_pow2_u64_36
	.quad	_L_1+8095
	.quad	_divmod_pow2__div_pow2_u64_36
	.quad	_L_1+8111
	.quad	_divmod_pow2__mod_pow2_u64_35
	.quad	_L_1+8127
	.quad	_divmod_pow2__div_pow2_u64_35
	.quad	_L_1+8143
	.quad	_divmod_pow2__mod_pow2_u64_34
	.quad	_L_1+8159
	.quad	_divmod_pow2__div_pow2_u64_34
	.quad	_L_1+8175
	.quad	_divmod_pow2__mod_pow2_u64_33
	.quad	_L_1+8191
	.quad	_divmod_pow2__div_pow2_u64_33
	.quad	_L_1+8207
	.quad	_divmod_pow2__mod_pow2_u64_32
	.quad	_L_1+8223
	.quad	_divmod_pow2__div_pow2_u64_32
	.quad	_L_1+8239
	.quad	_divmod_pow2__mod_pow2_u64_31
	.quad	_L_1+8255
	.quad	_divmod_pow2__div_pow2_u64_31
	.quad	_L_1+8271
	.quad	_divmod_pow2__mod_pow2_u64_30
	.quad	_L_1+8287
	.quad	_divmod_pow2__div_pow2_u64_30
	.quad	_L_1+8303
	.quad	_divmod_pow2__mod_pow2_u64_29
	.quad	_L_1+8319
	.quad	_divmod_pow2__div_pow2_u64_29
	.quad	_L_1+8335
	.quad	_divmod_pow2__mod_pow2_u64_28
	.quad	_L_1+8351
	.quad	_divmod_pow2__div_pow2_u64_28
	.quad	_L_1+8367
	.quad	_divmod_pow2__mod_pow2_u64_27
	.quad	_L_1+8383
	.quad	_divmod_pow2__div_pow2_u64_27
	.quad	_L_1+8399
	.quad	_divmod_pow2__mod_pow2_u64_26
	.quad	_L_1+8415
	.quad	_divmod_pow2__div_pow2_u64_26
	.quad	_L_1+8431
	.quad	_divmod_pow2__mod_pow2_u64_25
	.quad	_L_1+8447
	.quad	_divmod_pow2__div_pow2_u64_25
	.quad	_L_1+8463
	.quad	_divmod_pow2__mod_pow2_u64_24
	.quad	_L_1+8479
	.quad	_divmod_pow2__div_pow2_u64_24
	.quad	_L_1+8495
	.quad	_divmod_pow2__mod_pow2_u64_23
	.quad	_L_1+8511
	.quad	_divmod_pow2__div_pow2_u64_23
	.quad	_L_1+8527
	.quad	_divmod_pow2__mod_pow2_u64_22
	.quad	_L_1+8543
	.quad	_divmod_pow2__div_pow2_u64_22
	.quad	_L_1+8559
	.quad	_divmod_pow2__mod_pow2_u64_21
	.quad	_L_1+8575
	.quad	_divmod_pow2__div_pow2_u64_21
	.quad	_L_1+8591
	.quad	_divmod_pow2__mod_pow2_u64_20
	.quad	_L_1+8607
	.quad	_divmod_pow2__div_pow2_u64_20
	.quad	_L_1+8623
	.quad	_divmod_pow2__mod_pow2_u64_19
	.quad	_L_1+8639
	.quad	_divmod_pow2__div_pow2_u64_19
	.quad	_L_1+8655
	.quad	_divmod_pow2__mod_pow2_u64_18
	.quad	_L_1+8671
	.quad	_divmod_pow2__div_pow2_u64_18
	.quad	_L_1+8687
	.quad	_divmod_pow2__mod_pow2_u64_17
	.quad	_L_1+8703
	.quad	_divmod_pow2__div_pow2_u64_17
	.quad	_L_1+8719
	.quad	_divmod_pow2__mod_pow2_u64_16
	.quad	_L_1+8735
	.quad	_divmod_pow2__div_pow2_u64_16
	.quad	_L_1+8751
	.quad	_divmod_pow2__mod_pow2_u64_15
	.quad	_L_1+8767
	.quad	_divmod_pow2__div_pow2_u64_15
	.quad	_L_1+8783
	.quad	_divmod_pow2__mod_pow2_u64_14
	.quad	_L_1+8799
	.quad	_divmod_pow2__div_pow2_u64_14
	.quad	_L_1+8815
	.quad	_divmod_pow2__mod_pow2_u64_13
	.quad	_L_1+8831
	.quad	_divmod_pow2__div_pow2_u64_13
	.quad	_L_1+8847
	.quad	_divmod_pow2__mod_pow2_u64_12
	.quad	_L_1+8863
	.quad	_divmod_pow2__div_pow2_u64_12
	.quad	_L_1+8879
	.quad	_divmod_pow2__mod_pow2_u64_11
	.quad	_L_1+8895
	.quad	_divmod_pow2__div_pow2_u64_11
	.quad	_L_1+8911
	.quad	_divmod_pow2__mod_pow2_u64_10
	.quad	_L_1+8927
	.quad	_divmod_pow2__div_pow2_u64_10
	.quad	_L_1+8943
	.quad	_divmod_pow2__mod_pow2_u64_9
	.quad	_L_1+8959
	.quad	_divmod_pow2__div_pow2_u64_9
	.quad	_L_1+8974
	.quad	_divmod_pow2__mod_pow2_u64_8
	.quad	_L_1+8989
	.quad	_divmod_pow2__div_pow2_u64_8
	.quad	_L_1+9004
	.quad	_divmod_pow2__mod_pow2_u64_7
	.quad	_L_1+9019
	.quad	_divmod_pow2__div_pow2_u64_7
	.quad	_L_1+9034
	.quad	_divmod_pow2__mod_pow2_u64_6
	.quad	_L_1+9049
	.quad	_divmod_pow2__div_pow2_u64_6
	.quad	_L_1+9064
	.quad	_divmod_pow2__mod_pow2_u64_5
	.quad	_L_1+9079
	.quad	_divmod_pow2__div_pow2_u64_5
	.quad	_L_1+9094
	.quad	_divmod_pow2__mod_pow2_u64_4
	.quad	_L_1+9109
	.quad	_divmod_pow2__div_pow2_u64_4
	.quad	_L_1+9124
	.quad	_divmod_pow2__mod_pow2_u64_3
	.quad	_L_1+9139
	.quad	_divmod_pow2__div_pow2_u64_3
	.quad	_L_1+9154
	.quad	_divmod_pow2__mod_pow2_u64_2
	.quad	_L_1+9169
	.quad	_divmod_pow2__div_pow2_u64_2
	.quad	_L_1+9184
	.quad	_divmod_pow2__mod_pow2_u64_1
	.quad	_L_1+9199
	.quad	_divmod_pow2__div_pow2_u64_1
	.quad	_L_1+9214
	.quad	_divmod_pow2__mod_pow2_u64_0
	.quad	_L_1+9229
	.quad	_divmod_pow2__div_pow2_u64_0
	.quad	_L_1+9244
	.quad	_divmod_pow2__mod_pow2_i64_63
	.quad	_L_1+9259
	.quad	_divmod_pow2__div_pow2_i64_63
	.quad	_L_1+9275
	.quad	_divmod_pow2__mod_pow2_i64_62
	.quad	_L_1+9291
	.quad	_divmod_pow2__div_pow2_i64_62
	.quad	_L_1+9307
	.quad	_divmod_pow2__mod_pow2_i64_61
	.quad	_L_1+9323
	.quad	_divmod_pow2__div_pow2_i64_61
	.quad	_L_1+9339
	.quad	_divmod_pow2__mod_pow2_i64_60
	.quad	_L_1+9355
	.quad	_divmod_pow2__div_pow2_i64_60
	.quad	_L_1+9371
	.quad	_divmod_pow2__mod_pow2_i64_59
	.quad	_L_1+9387
	.quad	_divmod_pow2__div_pow2_i64_59
	.quad	_L_1+9403
	.quad	_divmod_pow2__mod_pow2_i64_58
	.quad	_L_1+9419
	.quad	_divmod_pow2__div_pow2_i64_58
	.quad	_L_1+9435
	.quad	_divmod_pow2__mod_pow2_i64_57
	.quad	_L_1+9451
	.quad	_divmod_pow2__div_pow2_i64_57
	.quad	_L_1+9467
	.quad	_divmod_pow2__mod_pow2_i64_56
	.quad	_L_1+9483
	.quad	_divmod_pow2__div_pow2_i64_56
	.quad	_L_1+9499
	.quad	_divmod_pow2__mod_pow2_i64_55
	.quad	_L_1+9515
	.quad	_divmod_pow2__div_pow2_i64_55
	.quad	_L_1+9531
	.quad	_divmod_pow2__mod_pow2_i64_54
	.quad	_L_1+9547
	.quad	_divmod_pow2__div_pow2_i64_54
	.quad	_L_1+9563
	.quad	_divmod_pow2__mod_pow2_i64_53
	.quad	_L_1+9579
	.quad	_divmod_pow2__div_pow2_i64_53
	.quad	_L_1+9595
	.quad	_divmod_pow2__mod_pow2_i64_52
	.quad	_L_1+9611
	.quad	_divmod_pow2__div_pow2_i64_52
	.quad	_L_1+9627
	.quad	_divmod_pow2__mod_pow2_i64_51
	.quad	_L_1+9643
	.quad	_divmod_pow2__div_pow2_i64_51
	.quad	_L_1+9659
	.quad	_divmod_pow2__mod_pow2_i64_50
	.quad	_L_1+9675
	.quad	_divmod_pow2__div_pow2_i64_50
	.quad	_L_1+9691
	.quad	_divmod_pow2__mod_pow2_i64_49
	.quad	_L_1+9707
	.quad	_divmod_pow2__div_pow2_i64_49
	.quad	_L_1+9723
	.quad	_divmod_pow2__mod_pow2_i64_48
	.quad	_L_1+9739
	.quad	_divmod_pow2__div_pow2_i64_48
	.quad	_L_1+9755
	.quad	_divmod_pow2__mod_pow2_i64_47
	.quad	_L_1+9771
	.quad	_divmod_pow2__div_pow2_i64_47
	.quad	_L_1+9787
	.quad	_divmod_pow2__mod_pow2_i64_46
	.quad	_L_1+9803
	.quad	_divmod_pow2__div_pow2_i64_46
	.quad	_L_1+9819
	.quad	_divmod_pow2__mod_pow2_i64_45
	.quad	_L_1+9835
	.quad	_divmod_pow2__div_pow2_i64_45
	.quad	_L_1+9851
	.quad	_divmod_pow2__mod_pow2_i64_44
	.quad	_L_1+9867
	.quad	_divmod_pow2__div_pow2_i64_44
	.quad	_L_1+9883
	.quad	_divmod_pow2__mod_pow2_i64_43
	.quad	_L_1+9899
	.quad	_divmod_pow2__div_pow2_i64_43
	.quad	_L_1+9915
	.quad	_divmod_pow2__mod_pow2_i64_42
	.quad	_L_1+9931
	.quad	_divmod_pow2__div_pow2_i64_42
	.quad	_L_1+9947
	.quad	_divmod_pow2__mod_pow2_i64_41
	.quad	_L_1+9963
	.quad	_divmod_pow2__div_pow2_i64_41
	.quad	_L_1+9979
	.quad	_divmod_pow2__mod_pow2_i64_40
	.quad	_L_1+9995
	.quad	_divmod_pow2__div_pow2_i64_40
	.quad	_L_1+10011
	.quad	_divmod_pow2__mod_pow2_i64_39
	.quad	_L_1+10027
	.quad	_divmod_pow2__div_pow2_i64_39
	.quad	_L_1+10043
	.quad	_divmod_pow2__mod_pow2_i64_38
	.quad	_L_1+10059
	.quad	_divmod_pow2__div_pow2_i64_38
	.quad	_L_1+10075
	.quad	_divmod_pow2__mod_pow2_i64_37
	.quad	_L_1+10091
	.quad	_divmod_pow2__div_pow2_i64_37
	.quad	_L_1+10107
	.quad	_divmod_pow2__mod_pow2_i64_36
	.quad	_L_1+10123
	.quad	_divmod_pow2__div_pow2_i64_36
	.quad	_L_1+10139
	.quad	_divmod_pow2__mod_pow2_i64_35
	.quad	_L_1+10155
	.quad	_divmod_pow2__div_pow2_i64_35
	.quad	_L_1+10171
	.quad	_divmod_pow2__mod_pow2_i64_34
	.quad	_L_1+10187
	.quad	_divmod_pow2__div_pow2_i64_34
	.quad	_L_1+10203
	.quad	_divmod_pow2__mod_pow2_i64_33
	.quad	_L_1+10219
	.quad	_divmod_pow2__div_pow2_i64_33
	.quad	_L_1+10235
	.quad	_divmod_pow2__mod_pow2_i64_32
	.quad	_L_1+10251
	.quad	_divmod_pow2__div_pow2_i64_32
	.quad	_L_1+10267
	.quad	_divmod_pow2__mod_pow2_i64_31
	.quad	_L_1+10283
	.quad	_divmod_pow2__div_pow2_i64_31
	.quad	_L_1+10299
	.quad	_divmod_pow2__mod_pow2_i64_30
	.quad	_L_1+10315
	.quad	_divmod_pow2__div_pow2_i64_30
	.quad	_L_1+10331
	.quad	_divmod_pow2__mod_pow2_i64_29
	.quad	_L_1+10347
	.quad	_divmod_pow2__div_pow2_i64_29
	.quad	_L_1+10363
	.quad	_divmod_pow2__mod_pow2_i64_28
	.quad	_L_1+10379
	.quad	_divmod_pow2__div_pow2_i64_28
	.quad	_L_1+10395
	.quad	_divmod_pow2__mod_pow2_i64_27
	.quad	_L_1+10411
	.quad	_divmod_pow2__div_pow2_i64_27
	.quad	_L_1+10427
	.quad	_divmod_pow2__mod_pow2_i64_26
	.quad	_L_1+10443
	.quad	_divmod_pow2__div_pow2_i64_26
	.quad	_L_1+10459
	.quad	_divmod_pow2__mod_pow2_i64_25
	.quad	_L_1+10475
	.quad	_divmod_pow2__div_pow2_i64_25
	.quad	_L_1+10491
	.quad	_divmod_pow2__mod_pow2_i64_24
	.quad	_L_1+10507
	.quad	_divmod_pow2__div_pow2_i64_24
	.quad	_L_1+10523
	.quad	_divmod_pow2__mod_pow2_i64_23
	.quad	_L_1+10539
	.quad	_divmod_pow2__div_pow2_i64_23
	.quad	_L_1+10555
	.quad	_divmod_pow2__mod_pow2_i64_22
	.quad	_L_1+10571
	.quad	_divmod_pow2__div_pow2_i64_22
	.quad	_L_1+10587
	.quad	_divmod_pow2__mod_pow2_i64_21
	.quad	_L_1+10603
	.quad	_divmod_pow2__div_pow2_i64_21
	.quad	_L_1+10619
	.quad	_divmod_pow2__mod_pow2_i64_20
	.quad	_L_1+10635
	.quad	_divmod_pow2__div_pow2_i64_20
	.quad	_L_1+10651
	.quad	_divmod_pow2__mod_pow2_i64_19
	.quad	_L_1+10667
	.quad	_divmod_pow2__div_pow2_i64_19
	.quad	_L_1+10683
	.quad	_divmod_pow2__mod_pow2_i64_18
	.quad	_L_1+10699
	.quad	_divmod_pow2__div_pow2_i64_18
	.quad	_L_1+10715
	.quad	_divmod_pow2__mod_pow2_i64_17
	.quad	_L_1+10731
	.quad	_divmod_pow2__div_pow2_i64_17
	.quad	_L_1+10747
	.quad	_divmod_pow2__mod_pow2_i64_16
	.quad	_L_1+10763
	.quad	_divmod_pow2__div_pow2_i64_16
	.quad	_L_1+10779
	.quad	_divmod_pow2__mod_pow2_i64_15
	.quad	_L_1+10795
	.quad	_divmod_pow2__div_pow2_i64_15
	.quad	_L_1+10811
	.quad	_divmod_pow2__mod_pow2_i64_14
	.quad	_L_1+10827
	.quad	_divmod_pow2__div_pow2_i64_14
	.quad	_L_1+10843
	.quad	_divmod_pow2__mod_pow2_i64_13
	.quad	_L_1+10859
	.quad	_divmod_pow2__div_pow2_i64_13
	.quad	_L_1+10875
	.quad	_divmod_pow2__mod_pow2_i64_12
	.quad	_L_1+10891
	.quad	_divmod_pow2__div_pow2_i64_12
	.quad	_L_1+10907
	.quad	_divmod_pow2__mod_pow2_i64_11
	.quad	_L_1+10923
	.quad	_divmod_pow2__div_pow2_i64_11
	.quad	_L_1+10939
	.quad	_divmod_pow2__mod_pow2_i64_10
	.quad	_L_1+10955
	.quad	_divmod_pow2__div_pow2_i64_10
	.quad	_L_1+10971
	.quad	_divmod_pow2__mod_pow2_i64_9
	.quad	_L_1+10987
	.quad	_divmod_pow2__div_pow2_i64_9
	.quad	_L_1+11002
	.quad	_divmod_pow2__mod_pow2_i64_8
	.quad	_L_1+11017
	.quad	_divmod_pow2__div_pow2_i64_8
	.quad	_L_1+11032
	.quad	_divmod_pow2__mod_pow2_i64_7
	.quad	_L_1+11047
	.quad	_divmod_pow2__div_pow2_i64_7
	.quad	_L_1+11062
	.quad	_divmod_pow2__mod_pow2_i64_6
	.quad	_L_1+11077
	.quad	_divmod_pow2__div_pow2_i64_6
	.quad	_L_1+11092
	.quad	_divmod_pow2__mod_pow2_i64_5
	.quad	_L_1+11107
	.quad	_divmod_pow2__div_pow2_i64_5
	.quad	_L_1+11122
	.quad	_divmod_pow2__mod_pow2_i64_4
	.quad	_L_1+11137
	.quad	_divmod_pow2__div_pow2_i64_4
	.quad	_L_1+11152
	.quad	_divmod_pow2__mod_pow2_i64_3
	.quad	_L_1+11167
	.quad	_divmod_pow2__div_pow2_i64_3
	.quad	_L_1+11182
	.quad	_divmod_pow2__mod_pow2_i64_2
	.quad	_L_1+11197
	.quad	_divmod_pow2__div_pow2_i64_2
	.quad	_L_1+11212
	.quad	_divmod_pow2__mod_pow2_i64_1
	.quad	_L_1+11227
	.quad	_divmod_pow2__div_pow2_i64_1
	.quad	_L_1+11242
	.quad	_divmod_pow2__mod_pow2_i64_0
	.quad	_L_1+11257
	.quad	_divmod_pow2__div_pow2_i64_0
	.quad	_L_1+11272
	.quad	_divmod_pow2__mod_pow2_u32_31
	.quad	_L_1+11287
	.quad	_divmod_pow2__div_pow2_u32_31
	.quad	_L_1+11303
	.quad	_divmod_pow2__mod_pow2_u32_30
	.quad	_L_1+11319
	.quad	_divmod_pow2__div_pow2_u32_30
	.quad	_L_1+11335
	.quad	_divmod_pow2__mod_pow2_u32_29
	.quad	_L_1+11351
	.quad	_divmod_pow2__div_pow2_u32_29
	.quad	_L_1+11367
	.quad	_divmod_pow2__mod_pow2_u32_28
	.quad	_L_1+11383
	.quad	_divmod_pow2__div_pow2_u32_28
	.quad	_L_1+11399
	.quad	_divmod_pow2__mod_pow2_u32_27
	.quad	_L_1+11415
	.quad	_divmod_pow2__div_pow2_u32_27
	.quad	_L_1+11431
	.quad	_divmod_pow2__mod_pow2_u32_26
	.quad	_L_1+11447
	.quad	_divmod_pow2__div_pow2_u32_26
	.quad	_L_1+11463
	.quad	_divmod_pow2__mod_pow2_u32_25
	.quad	_L_1+11479
	.quad	_divmod_pow2__div_pow2_u32_25
	.quad	_L_1+11495
	.quad	_divmod_pow2__mod_pow2_u32_24
	.quad	_L_1+11511
	.quad	_divmod_pow2__div_pow2_u32_24
	.quad	_L_1+11527
	.quad	_divmod_pow2__mod_pow2_u32_23
	.quad	_L_1+11543
	.quad	_divmod_pow2__div_pow2_u32_23
	.quad	_L_1+11559
	.quad	_divmod_pow2__mod_pow2_u32_22
	.quad	_L_1+11575
	.quad	_divmod_pow2__div_pow2_u32_22
	.quad	_L_1+11591
	.quad	_divmod_pow2__mod_pow2_u32_21
	.quad	_L_1+11607
	.quad	_divmod_pow2__div_pow2_u32_21
	.quad	_L_1+11623
	.quad	_divmod_pow2__mod_pow2_u32_20
	.quad	_L_1+11639
	.quad	_divmod_pow2__div_pow2_u32_20
	.quad	_L_1+11655
	.quad	_divmod_pow2__mod_pow2_u32_19
	.quad	_L_1+11671
	.quad	_divmod_pow2__div_pow2_u32_19
	.quad	_L_1+11687
	.quad	_divmod_pow2__mod_pow2_u32_18
	.quad	_L_1+11703
	.quad	_divmod_pow2__div_pow2_u32_18
	.quad	_L_1+11719
	.quad	_divmod_pow2__mod_pow2_u32_17
	.quad	_L_1+11735
	.quad	_divmod_pow2__div_pow2_u32_17
	.quad	_L_1+11751
	.quad	_divmod_pow2__mod_pow2_u32_16
	.quad	_L_1+11767
	.quad	_divmod_pow2__div_pow2_u32_16
	.quad	_L_1+11783
	.quad	_divmod_pow2__mod_pow2_u32_15
	.quad	_L_1+11799
	.quad	_divmod_pow2__div_pow2_u32_15
	.quad	_L_1+11815
	.quad	_divmod_pow2__mod_pow2_u32_14
	.quad	_L_1+11831
	.quad	_divmod_pow2__div_pow2_u32_14
	.quad	_L_1+11847
	.quad	_divmod_pow2__mod_pow2_u32_13
	.quad	_L_1+11863
	.quad	_divmod_pow2__div_pow2_u32_13
	.quad	_L_1+11879
	.quad	_divmod_pow2__mod_pow2_u32_12
	.quad	_L_1+11895
	.quad	_divmod_pow2__div_pow2_u32_12
	.quad	_L_1+11911
	.quad	_divmod_pow2__mod_pow2_u32_11
	.quad	_L_1+11927
	.quad	_divmod_pow2__div_pow2_u32_11
	.quad	_L_1+11943
	.quad	_divmod_pow2__mod_pow2_u32_10
	.quad	_L_1+11959
	.quad	_divmod_pow2__div_pow2_u32_10
	.quad	_L_1+11975
	.quad	_divmod_pow2__mod_pow2_u32_9
	.quad	_L_1+11991
	.quad	_divmod_pow2__div_pow2_u32_9
	.quad	_L_1+12006
	.quad	_divmod_pow2__mod_pow2_u32_8
	.quad	_L_1+12021
	.quad	_divmod_pow2__div_pow2_u32_8
	.quad	_L_1+12036
	.quad	_divmod_pow2__mod_pow2_u32_7
	.quad	_L_1+12051
	.quad	_divmod_pow2__div_pow2_u32_7
	.quad	_L_1+12066
	.quad	_divmod_pow2__mod_pow2_u32_6
	.quad	_L_1+12081
	.quad	_divmod_pow2__div_pow2_u32_6
	.quad	_L_1+12096
	.quad	_divmod_pow2__mod_pow2_u32_5
	.quad	_L_1+12111
	.quad	_divmod_pow2__div_pow2_u32_5
	.quad	_L_1+12126
	.quad	_divmod_pow2__mod_pow2_u32_4
	.quad	_L_1+12141
	.quad	_divmod_pow2__div_pow2_u32_4
	.quad	_L_1+12156
	.quad	_divmod_pow2__mod_pow2_u32_3
	.quad	_L_1+12171
	.quad	_divmod_pow2__div_pow2_u32_3
	.quad	_L_1+12186
	.quad	_divmod_pow2__mod_pow2_u32_2
	.quad	_L_1+12201
	.quad	_divmod_pow2__div_pow2_u32_2
	.quad	_L_1+12216
	.quad	_divmod_pow2__mod_pow2_u32_1
	.quad	_L_1+12231
	.quad	_divmod_pow2__div_pow2_u32_1
	.quad	_L_1+12246
	.quad	_divmod_pow2__mod_pow2_u32_0
	.quad	_L_1+12261
	.quad	_divmod_pow2__div_pow2_u32_0
	.quad	_L_1+12276
	.quad	_divmod_pow2__mod_pow2_i32_31
	.quad	_L_1+12291
	.quad	_divmod_pow2__div_pow2_i32_31
	.quad	_L_1+12307
	.quad	_divmod_pow2__mod_pow2_i32_30
	.quad	_L_1+12323
	.quad	_divmod_pow2__div_pow2_i32_30
	.quad	_L_1+12339
	.quad	_divmod_pow2__mod_pow2_i32_29
	.quad	_L_1+12355
	.quad	_divmod_pow2__div_pow2_i32_29
	.quad	_L_1+12371
	.quad	_divmod_pow2__mod_pow2_i32_28
	.quad	_L_1+12387
	.quad	_divmod_pow2__div_pow2_i32_28
	.quad	_L_1+12403
	.quad	_divmod_pow2__mod_pow2_i32_27
	.quad	_L_1+12419
	.quad	_divmod_pow2__div_pow2_i32_27
	.quad	_L_1+12435
	.quad	_divmod_pow2__mod_pow2_i32_26
	.quad	_L_1+12451
	.quad	_divmod_pow2__div_pow2_i32_26
	.quad	_L_1+12467
	.quad	_divmod_pow2__mod_pow2_i32_25
	.quad	_L_1+12483
	.quad	_divmod_pow2__div_pow2_i32_25
	.quad	_L_1+12499
	.quad	_divmod_pow2__mod_pow2_i32_24
	.quad	_L_1+12515
	.quad	_divmod_pow2__div_pow2_i32_24
	.quad	_L_1+12531
	.quad	_divmod_pow2__mod_pow2_i32_23
	.quad	_L_1+12547
	.quad	_divmod_pow2__div_pow2_i32_23
	.quad	_L_1+12563
	.quad	_divmod_pow2__mod_pow2_i32_22
	.quad	_L_1+12579
	.quad	_divmod_pow2__div_pow2_i32_22
	.quad	_L_1+12595
	.quad	_divmod_pow2__mod_pow2_i32_21
	.quad	_L_1+12611
	.quad	_divmod_pow2__div_pow2_i32_21
	.quad	_L_1+12627
	.quad	_divmod_pow2__mod_pow2_i32_20
	.quad	_L_1+12643
	.quad	_divmod_pow2__div_pow2_i32_20
	.quad	_L_1+12659
	.quad	_divmod_pow2__mod_pow2_i32_19
	.quad	_L_1+12675
	.quad	_divmod_pow2__div_pow2_i32_19
	.quad	_L_1+12691
	.quad	_divmod_pow2__mod_pow2_i32_18
	.quad	_L_1+12707
	.quad	_divmod_pow2__div_pow2_i32_18
	.quad	_L_1+12723
	.quad	_divmod_pow2__mod_pow2_i32_17
	.quad	_L_1+12739
	.quad	_divmod_pow2__div_pow2_i32_17
	.quad	_L_1+12755
	.quad	_divmod_pow2__mod_pow2_i32_16
	.quad	_L_1+12771
	.quad	_divmod_pow2__div_pow2_i32_16
	.quad	_L_1+12787
	.quad	_divmod_pow2__mod_pow2_i32_15
	.quad	_L_1+12803
	.quad	_divmod_pow2__div_pow2_i32_15
	.quad	_L_1+12819
	.quad	_divmod_pow2__mod_pow2_i32_14
	.quad	_L_1+12835
	.quad	_divmod_pow2__div_pow2_i32_14
	.quad	_L_1+12851
	.quad	_divmod_pow2__mod_pow2_i32_13
	.quad	_L_1+12867
	.quad	_divmod_pow2__div_pow2_i32_13
	.quad	_L_1+12883
	.quad	_divmod_pow2__mod_pow2_i32_12
	.quad	_L_1+12899
	.quad	_divmod_pow2__div_pow2_i32_12
	.quad	_L_1+12915
	.quad	_divmod_pow2__mod_pow2_i32_11
	.quad	_L_1+12931
	.quad	_divmod_pow2__div_pow2_i32_11
	.quad	_L_1+12947
	.quad	_divmod_pow2__mod_pow2_i32_10
	.quad	_L_1+12963
	.quad	_divmod_pow2__div_pow2_i32_10
	.quad	_L_1+12979
	.quad	_divmod_pow2__mod_pow2_i32_9
	.quad	_L_1+12995
	.quad	_divmod_pow2__div_pow2_i32_9
	.quad	_L_1+13010
	.quad	_divmod_pow2__mod_pow2_i32_8
	.quad	_L_1+13025
	.quad	_divmod_pow2__div_pow2_i32_8
	.quad	_L_1+13040
	.quad	_divmod_pow2__mod_pow2_i32_7
	.quad	_L_1+13055
	.quad	_divmod_pow2__div_pow2_i32_7
	.quad	_L_1+13070
	.quad	_divmod_pow2__mod_pow2_i32_6
	.quad	_L_1+13085
	.quad	_divmod_pow2__div_pow2_i32_6
	.quad	_L_1+13100
	.quad	_divmod_pow2__mod_pow2_i32_5
	.quad	_L_1+13115
	.quad	_divmod_pow2__div_pow2_i32_5
	.quad	_L_1+13130
	.quad	_divmod_pow2__mod_pow2_i32_4
	.quad	_L_1+13145
	.quad	_divmod_pow2__div_pow2_i32_4
	.quad	_L_1+13160
	.quad	_divmod_pow2__mod_pow2_i32_3
	.quad	_L_1+13175
	.quad	_divmod_pow2__div_pow2_i32_3
	.quad	_L_1+13190
	.quad	_divmod_pow2__mod_pow2_i32_2
	.quad	_L_1+13205
	.quad	_divmod_pow2__div_pow2_i32_2
	.quad	_L_1+13220
	.quad	_divmod_pow2__mod_pow2_i32_1
	.quad	_L_1+13235
	.quad	_divmod_pow2__div_pow2_i32_1
	.quad	_L_1+13250
	.quad	_divmod_pow2__mod_pow2_i32_0
	.quad	_L_1+13265
	.quad	_divmod_pow2__div_pow2_i32_0
	.quad	_L_1+13280
	.space 8
	.ascii "../AMD64_DARWIN/divmod_pow2.m3"
	.space 2
	.data
	.align 5
_MM_divmod_pow2:
	.quad	_L_1+27656
	.space 32
	.quad	_L_1+13296
	.space 24
	.quad	_MM_divmod_pow2+216
	.space 8
	.quad	_divmod_pow2_M3
	.quad	3
	.word	81
	.space 6
	.quad	82
	.long	2405181686
	.long	1079309762
	.quad	85
	.quad	86
	.byte	87
	.space 7
	.quad	88
	.long	1119079629
	.long	91
	.byte	92
	.space 7
	.quad	93
	.quad	94
	.word	95
	.space 2
	.long	96
	.space 24
	.quad	_divmod_pow2_I3
	.quad	_MM_divmod_pow2+240
	.space 8
	.quad	_Long_I3
	.quad	_MM_divmod_pow2+264
	.space 8
	.quad	_Word_I3
	.quad	_MM_divmod_pow2+288
	.space 8
	.quad	_Cstdint_I3
	.quad	_MM_divmod_pow2+312
	.space 8
	.quad	_RTHooks_I3
	.space 8
	.subsections_via_symbols
