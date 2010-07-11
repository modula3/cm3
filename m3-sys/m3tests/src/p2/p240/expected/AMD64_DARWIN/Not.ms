	.text
.globl _Not__uNot_var_i64_i64
	.private_extern _Not__uNot_var_i64_i64
_Not__uNot_var_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Not(%rip), %rax
	notq	%rax
	leave
	ret
.globl _Not__Not_var_i64_i64
	.private_extern _Not__Not_var_i64_i64
_Not__Not_var_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	104+_MM_Not(%rip), %rax
	negq	%rax
	leave
	ret
.globl _Not__uNot_param_i64_i64
	.private_extern _Not__uNot_param_i64_i64
_Not__uNot_param_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	notq	%rax
	leave
	ret
.globl _Not__Not_param_i64_i64
	.private_extern _Not__Not_param_i64_i64
_Not__Not_param_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	negq	%rax
	leave
	ret
.globl _Not__uNot_var_LC_LC
	.private_extern _Not__uNot_var_LC_LC
_Not__uNot_var_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Not(%rip), %rax
	notq	%rax
	leave
	ret
.globl _Not__Not_var_LC_LC
	.private_extern _Not__Not_var_LC_LC
_Not__Not_var_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Not(%rip), %rax
	negq	%rax
	leave
	ret
.globl _Not__uNot_param_LC_LC
	.private_extern _Not__uNot_param_LC_LC
_Not__uNot_param_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	notq	%rax
	leave
	ret
.globl _Not__Not_param_LC_LC
	.private_extern _Not__Not_param_LC_LC
_Not__Not_param_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	negq	%rax
	leave
	ret
.globl _Not__Not_var_f32_f32
	.private_extern _Not__Not_var_f32_f32
_Not__Not_var_f32_f32:
	pushq	%rbp
	movq	%rsp, %rbp
	movss	120+_MM_Not(%rip), %xmm1
	movss	LC0(%rip), %xmm0
	xorps	%xmm1, %xmm0
	leave
	ret
.globl _Not__Not_param_f32_f32
	.private_extern _Not__Not_param_f32_f32
_Not__Not_param_f32_f32:
	pushq	%rbp
	movq	%rsp, %rbp
	movss	%xmm0, -20(%rbp)
	movss	%xmm1, -24(%rbp)
	movss	-24(%rbp), %xmm1
	movss	LC0(%rip), %xmm0
	xorps	%xmm1, %xmm0
	leave
	ret
.globl _Not__uNot_var_u8_u8
	.private_extern _Not__uNot_var_u8_u8
_Not__uNot_var_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	124+_MM_Not(%rip), %eax
	movsbq	%al,%rax
	notq	%rax
	leave
	ret
.globl _Not__Not_var_u8_u8
	.private_extern _Not__Not_var_u8_u8
_Not__Not_var_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	124+_MM_Not(%rip), %eax
	movsbq	%al,%rax
	negq	%rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L24
	cmpq	$127, -16(%rbp)
	jle	L25
L24:
	movl	$2081, %edi
	call	__m3_fault
L25:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Not__uNot_param_u8_u8
	.private_extern _Not__uNot_param_u8_u8
_Not__uNot_param_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rax
	notq	%rax
	leave
	ret
.globl _Not__Not_param_u8_u8
	.private_extern _Not__Not_param_u8_u8
_Not__Not_param_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-18(%rbp),%rax
	negq	%rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L30
	cmpq	$127, -16(%rbp)
	jle	L31
L30:
	movl	$2145, %edi
	call	__m3_fault
L31:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Not__uNot_var_I_I
	.private_extern _Not__uNot_var_I_I
_Not__uNot_var_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Not(%rip), %rax
	notq	%rax
	leave
	ret
.globl _Not__Not_var_I_I
	.private_extern _Not__Not_var_I_I
_Not__Not_var_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_Not(%rip), %rax
	negq	%rax
	leave
	ret
.globl _Not__uNot_param_I_I
	.private_extern _Not__uNot_param_I_I
_Not__uNot_param_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	notq	%rax
	leave
	ret
.globl _Not__Not_param_I_I
	.private_extern _Not__Not_param_I_I
_Not__Not_param_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	negq	%rax
	leave
	ret
.globl _Not__uNot_var_i8_i8
	.private_extern _Not__uNot_var_i8_i8
_Not__uNot_var_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	136+_MM_Not(%rip), %eax
	movzbl	%al, %eax
	notq	%rax
	leave
	ret
.globl _Not__Not_var_i8_i8
	.private_extern _Not__Not_var_i8_i8
_Not__Not_var_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	136+_MM_Not(%rip), %eax
	movzbl	%al, %eax
	negq	%rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L44
	cmpq	$255, -16(%rbp)
	jle	L45
L44:
	movl	$2337, %edi
	call	__m3_fault
L45:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Not__uNot_param_i8_i8
	.private_extern _Not__uNot_param_i8_i8
_Not__uNot_param_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %eax
	notq	%rax
	leave
	ret
.globl _Not__Not_param_i8_i8
	.private_extern _Not__Not_param_i8_i8
_Not__Not_param_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-18(%rbp), %eax
	negq	%rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L50
	cmpq	$255, -16(%rbp)
	jle	L51
L50:
	movl	$2401, %edi
	call	__m3_fault
L51:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Not__uNot_var_u32_u32
	.private_extern _Not__uNot_var_u32_u32
_Not__uNot_var_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Not(%rip), %eax
	cltq
	notq	%rax
	leave
	ret
.globl _Not__Not_var_u32_u32
	.private_extern _Not__Not_var_u32_u32
_Not__Not_var_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	140+_MM_Not(%rip), %eax
	cltq
	negq	%rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L56
	cmpq	$2147483647, -16(%rbp)
	jle	L57
L56:
	movl	$2465, %edi
	call	__m3_fault
L57:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Not__uNot_param_u32_u32
	.private_extern _Not__uNot_param_u32_u32
_Not__uNot_param_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	cltq
	notq	%rax
	leave
	ret
.globl _Not__Not_param_u32_u32
	.private_extern _Not__Not_param_u32_u32
_Not__Not_param_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-24(%rbp), %eax
	cltq
	negq	%rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L62
	cmpq	$2147483647, -16(%rbp)
	jle	L63
L62:
	movl	$2529, %edi
	call	__m3_fault
L63:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Not__uNot_var_C_C
	.private_extern _Not__uNot_var_C_C
_Not__uNot_var_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Not(%rip), %rax
	notq	%rax
	leave
	ret
.globl _Not__Not_var_C_C
	.private_extern _Not__Not_var_C_C
_Not__Not_var_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	144+_MM_Not(%rip), %rax
	negq	%rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L68
	movl	$2593, %edi
	call	__m3_fault
L68:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Not__uNot_param_C_C
	.private_extern _Not__uNot_param_C_C
_Not__uNot_param_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	notq	%rax
	leave
	ret
.globl _Not__Not_param_C_C
	.private_extern _Not__Not_param_C_C
_Not__Not_param_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	negq	%rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L73
	movl	$2657, %edi
	call	__m3_fault
L73:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Not__uNot_var_u16_u16
	.private_extern _Not__uNot_var_u16_u16
_Not__uNot_var_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	152+_MM_Not(%rip), %eax
	movswq	%ax,%rax
	notq	%rax
	leave
	ret
.globl _Not__Not_var_u16_u16
	.private_extern _Not__Not_var_u16_u16
_Not__Not_var_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	152+_MM_Not(%rip), %eax
	movswq	%ax,%rax
	negq	%rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L78
	cmpq	$32767, -16(%rbp)
	jle	L79
L78:
	movl	$2721, %edi
	call	__m3_fault
L79:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Not__uNot_param_u16_u16
	.private_extern _Not__uNot_param_u16_u16
_Not__uNot_param_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rax
	notq	%rax
	leave
	ret
.globl _Not__Not_param_u16_u16
	.private_extern _Not__Not_param_u16_u16
_Not__Not_param_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-20(%rbp),%rax
	negq	%rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L84
	cmpq	$32767, -16(%rbp)
	jle	L85
L84:
	movl	$2785, %edi
	call	__m3_fault
L85:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Not__uNot_var_u64_u64
	.private_extern _Not__uNot_var_u64_u64
_Not__uNot_var_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Not(%rip), %rax
	notq	%rax
	leave
	ret
.globl _Not__Not_var_u64_u64
	.private_extern _Not__Not_var_u64_u64
_Not__Not_var_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Not(%rip), %rax
	negq	%rax
	leave
	ret
.globl _Not__uNot_param_u64_u64
	.private_extern _Not__uNot_param_u64_u64
_Not__uNot_param_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	notq	%rax
	leave
	ret
.globl _Not__Not_param_u64_u64
	.private_extern _Not__Not_param_u64_u64
_Not__Not_param_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	negq	%rax
	leave
	ret
.globl _Not__Not_var_f64_f64
	.private_extern _Not__Not_var_f64_f64
_Not__Not_var_f64_f64:
	pushq	%rbp
	movq	%rsp, %rbp
	movsd	168+_MM_Not(%rip), %xmm1
	movsd	LC1(%rip), %xmm0
	xorpd	%xmm1, %xmm0
	leave
	ret
.globl _Not__Not_param_f64_f64
	.private_extern _Not__Not_param_f64_f64
_Not__Not_param_f64_f64:
	pushq	%rbp
	movq	%rsp, %rbp
	movsd	%xmm0, -24(%rbp)
	movsd	%xmm1, -32(%rbp)
	movsd	-32(%rbp), %xmm1
	movsd	LC1(%rip), %xmm0
	xorpd	%xmm1, %xmm0
	leave
	ret
.globl _Not__uNot_var_L_L
	.private_extern _Not__uNot_var_L_L
_Not__uNot_var_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Not(%rip), %rax
	notq	%rax
	leave
	ret
.globl _Not__Not_var_L_L
	.private_extern _Not__Not_var_L_L
_Not__Not_var_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	176+_MM_Not(%rip), %rax
	negq	%rax
	leave
	ret
.globl _Not__uNot_param_L_L
	.private_extern _Not__uNot_param_L_L
_Not__uNot_param_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	notq	%rax
	leave
	ret
.globl _Not__Not_param_L_L
	.private_extern _Not__Not_param_L_L
_Not__Not_param_L_L:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	negq	%rax
	leave
	ret
.globl _Not__uNot_var_i16_i16
	.private_extern _Not__uNot_var_i16_i16
_Not__uNot_var_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	184+_MM_Not(%rip), %eax
	movzwl	%ax, %eax
	notq	%rax
	leave
	ret
.globl _Not__Not_var_i16_i16
	.private_extern _Not__Not_var_i16_i16
_Not__Not_var_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	184+_MM_Not(%rip), %eax
	movzwl	%ax, %eax
	negq	%rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L110
	cmpq	$65535, -16(%rbp)
	jle	L111
L110:
	movl	$3169, %edi
	call	__m3_fault
L111:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Not__uNot_param_i16_i16
	.private_extern _Not__uNot_param_i16_i16
_Not__uNot_param_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %eax
	notq	%rax
	leave
	ret
.globl _Not__Not_param_i16_i16
	.private_extern _Not__Not_param_i16_i16
_Not__Not_param_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-20(%rbp), %eax
	negq	%rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L116
	cmpq	$65535, -16(%rbp)
	jle	L117
L116:
	movl	$3233, %edi
	call	__m3_fault
L117:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Not__uNot_var_i32_i32
	.private_extern _Not__uNot_var_i32_i32
_Not__uNot_var_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	188+_MM_Not(%rip), %eax
	mov	%eax, %eax
	notq	%rax
	leave
	ret
.globl _Not__Not_var_i32_i32
	.private_extern _Not__Not_var_i32_i32
_Not__Not_var_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	188+_MM_Not(%rip), %eax
	mov	%eax, %eax
	negq	%rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L122
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L123
L122:
	movl	$3297, %edi
	call	__m3_fault
L123:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Not__uNot_param_i32_i32
	.private_extern _Not__uNot_param_i32_i32
_Not__uNot_param_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %eax
	notq	%rax
	leave
	ret
.globl _Not__Not_param_i32_i32
	.private_extern _Not__Not_param_i32_i32
_Not__Not_param_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-24(%rbp), %eax
	negq	%rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L128
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L129
L128:
	movl	$3361, %edi
	call	__m3_fault
L129:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Not_M3
_Not_M3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	leaq	_MM_Not(%rip), %rax
	leave
	ret
__m3_fault:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rsi
	leaq	_MM_Not(%rip), %rdi
	call	_RTHooks__ReportFault
	leave
	ret
	.const_data
	.align 5
_L_1:
	.ascii "Not_M3"
	.space 1
	.ascii "Not_param_i32_i32"
	.space 1
	.ascii "uNot_param_i32_i32"
	.space 1
	.ascii "Not_var_i32_i32"
	.space 1
	.ascii "uNot_var_i32_i32"
	.space 1
	.ascii "Not_param_i16_i16"
	.space 1
	.ascii "uNot_param_i16_i16"
	.space 1
	.ascii "Not_var_i16_i16"
	.space 1
	.ascii "uNot_var_i16_i16"
	.space 1
	.ascii "Not_param_L_L"
	.space 1
	.ascii "uNot_param_L_L"
	.space 1
	.ascii "Not_var_L_L"
	.space 1
	.ascii "uNot_var_L_L"
	.space 1
	.ascii "Not_param_f64_f64"
	.space 1
	.ascii "Not_var_f64_f64"
	.space 1
	.ascii "Not_param_u64_u64"
	.space 1
	.ascii "uNot_param_u64_u64"
	.space 1
	.ascii "Not_var_u64_u64"
	.space 1
	.ascii "uNot_var_u64_u64"
	.space 1
	.ascii "Not_param_u16_u16"
	.space 1
	.ascii "uNot_param_u16_u16"
	.space 1
	.ascii "Not_var_u16_u16"
	.space 1
	.ascii "uNot_var_u16_u16"
	.space 1
	.ascii "Not_param_C_C"
	.space 1
	.ascii "uNot_param_C_C"
	.space 1
	.ascii "Not_var_C_C"
	.space 1
	.ascii "uNot_var_C_C"
	.space 1
	.ascii "Not_param_u32_u32"
	.space 1
	.ascii "uNot_param_u32_u32"
	.space 1
	.ascii "Not_var_u32_u32"
	.space 1
	.ascii "uNot_var_u32_u32"
	.space 1
	.ascii "Not_param_i8_i8"
	.space 1
	.ascii "uNot_param_i8_i8"
	.space 1
	.ascii "Not_var_i8_i8"
	.space 1
	.ascii "uNot_var_i8_i8"
	.space 1
	.ascii "Not_param_I_I"
	.space 1
	.ascii "uNot_param_I_I"
	.space 1
	.ascii "Not_var_I_I"
	.space 1
	.ascii "uNot_var_I_I"
	.space 1
	.ascii "Not_param_u8_u8"
	.space 1
	.ascii "uNot_param_u8_u8"
	.space 1
	.ascii "Not_var_u8_u8"
	.space 1
	.ascii "uNot_var_u8_u8"
	.space 1
	.ascii "Not_param_f32_f32"
	.space 1
	.ascii "Not_var_f32_f32"
	.space 1
	.ascii "Not_param_LC_LC"
	.space 1
	.ascii "uNot_param_LC_LC"
	.space 1
	.ascii "Not_var_LC_LC"
	.space 1
	.ascii "uNot_var_LC_LC"
	.space 1
	.ascii "Not_param_i64_i64"
	.space 1
	.ascii "uNot_param_i64_i64"
	.space 1
	.ascii "Not_var_i64_i64"
	.space 1
	.ascii "uNot_var_i64_i64"
	.space 6
	.quad	_Not_M3
	.quad	_L_1
	.quad	_Not__Not_param_i32_i32
	.quad	_L_1+7
	.quad	_Not__uNot_param_i32_i32
	.quad	_L_1+25
	.quad	_Not__Not_var_i32_i32
	.quad	_L_1+44
	.quad	_Not__uNot_var_i32_i32
	.quad	_L_1+60
	.quad	_Not__Not_param_i16_i16
	.quad	_L_1+77
	.quad	_Not__uNot_param_i16_i16
	.quad	_L_1+95
	.quad	_Not__Not_var_i16_i16
	.quad	_L_1+114
	.quad	_Not__uNot_var_i16_i16
	.quad	_L_1+130
	.quad	_Not__Not_param_L_L
	.quad	_L_1+147
	.quad	_Not__uNot_param_L_L
	.quad	_L_1+161
	.quad	_Not__Not_var_L_L
	.quad	_L_1+176
	.quad	_Not__uNot_var_L_L
	.quad	_L_1+188
	.quad	_Not__Not_param_f64_f64
	.quad	_L_1+201
	.quad	_Not__Not_var_f64_f64
	.quad	_L_1+219
	.quad	_Not__Not_param_u64_u64
	.quad	_L_1+235
	.quad	_Not__uNot_param_u64_u64
	.quad	_L_1+253
	.quad	_Not__Not_var_u64_u64
	.quad	_L_1+272
	.quad	_Not__uNot_var_u64_u64
	.quad	_L_1+288
	.quad	_Not__Not_param_u16_u16
	.quad	_L_1+305
	.quad	_Not__uNot_param_u16_u16
	.quad	_L_1+323
	.quad	_Not__Not_var_u16_u16
	.quad	_L_1+342
	.quad	_Not__uNot_var_u16_u16
	.quad	_L_1+358
	.quad	_Not__Not_param_C_C
	.quad	_L_1+375
	.quad	_Not__uNot_param_C_C
	.quad	_L_1+389
	.quad	_Not__Not_var_C_C
	.quad	_L_1+404
	.quad	_Not__uNot_var_C_C
	.quad	_L_1+416
	.quad	_Not__Not_param_u32_u32
	.quad	_L_1+429
	.quad	_Not__uNot_param_u32_u32
	.quad	_L_1+447
	.quad	_Not__Not_var_u32_u32
	.quad	_L_1+466
	.quad	_Not__uNot_var_u32_u32
	.quad	_L_1+482
	.quad	_Not__Not_param_i8_i8
	.quad	_L_1+499
	.quad	_Not__uNot_param_i8_i8
	.quad	_L_1+515
	.quad	_Not__Not_var_i8_i8
	.quad	_L_1+532
	.quad	_Not__uNot_var_i8_i8
	.quad	_L_1+546
	.quad	_Not__Not_param_I_I
	.quad	_L_1+561
	.quad	_Not__uNot_param_I_I
	.quad	_L_1+575
	.quad	_Not__Not_var_I_I
	.quad	_L_1+590
	.quad	_Not__uNot_var_I_I
	.quad	_L_1+602
	.quad	_Not__Not_param_u8_u8
	.quad	_L_1+615
	.quad	_Not__uNot_param_u8_u8
	.quad	_L_1+631
	.quad	_Not__Not_var_u8_u8
	.quad	_L_1+648
	.quad	_Not__uNot_var_u8_u8
	.quad	_L_1+662
	.quad	_Not__Not_param_f32_f32
	.quad	_L_1+677
	.quad	_Not__Not_var_f32_f32
	.quad	_L_1+695
	.quad	_Not__Not_param_LC_LC
	.quad	_L_1+711
	.quad	_Not__uNot_param_LC_LC
	.quad	_L_1+727
	.quad	_Not__Not_var_LC_LC
	.quad	_L_1+744
	.quad	_Not__uNot_var_LC_LC
	.quad	_L_1+758
	.quad	_Not__Not_param_i64_i64
	.quad	_L_1+773
	.quad	_Not__uNot_param_i64_i64
	.quad	_L_1+791
	.quad	_Not__Not_var_i64_i64
	.quad	_L_1+810
	.quad	_Not__uNot_var_i64_i64
	.quad	_L_1+826
	.space 8
	.ascii "../AMD64_DARWIN/Not.m3"
	.space 2
	.data
	.align 5
_MM_Not:
	.quad	_L_1+1704
	.space 32
	.quad	_L_1+848
	.space 24
	.quad	_MM_Not+208
	.space 8
	.quad	_Not_M3
	.quad	3
	.quad	593
	.quad	594
	.long	1142220325
	.byte	85
	.space 3
	.quad	598
	.byte	87
	.space 3
	.long	600
	.quad	601
	.word	602
	.space 6
	.quad	603
	.long	171798692
	.long	1082320087
	.quad	606
	.word	607
	.space 2
	.long	608
	.space 24
	.quad	_Not_I3
	.quad	_MM_Not+232
	.space 8
	.quad	_Long_I3
	.quad	_MM_Not+256
	.space 8
	.quad	_Word_I3
	.quad	_MM_Not+280
	.space 8
	.quad	_Cstdint_I3
	.quad	_MM_Not+304
	.space 8
	.quad	_RTHooks_I3
	.space 8
	.literal16
	.align 4
LC0:
	.long	2147483648
	.long	0
	.long	0
	.long	0
	.align 4
LC1:
	.long	0
	.long	-2147483648
	.long	0
	.long	0
	.subsections_via_symbols
