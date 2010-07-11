	.text
.globl _Not__uNot_var_u16_u16
	.private_extern _Not__uNot_var_u16_u16
_Not__uNot_var_u16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	104+_MM_Not(%rip), %eax
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
	movzwl	104+_MM_Not(%rip), %eax
	movswq	%ax,%rax
	negq	%rax
	movq	%rax, -16(%rbp)
	cmpq	$-32768, -16(%rbp)
	jl	L4
	cmpq	$32767, -16(%rbp)
	jle	L5
L4:
	movl	$1761, %edi
	call	__m3_fault
L5:
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
	jl	L10
	cmpq	$32767, -16(%rbp)
	jle	L11
L10:
	movl	$1825, %edi
	call	__m3_fault
L11:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Not__uNot_var_u64_u64
	.private_extern _Not__uNot_var_u64_u64
_Not__uNot_var_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Not(%rip), %rax
	notq	%rax
	leave
	ret
.globl _Not__Not_var_u64_u64
	.private_extern _Not__Not_var_u64_u64
_Not__Not_var_u64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Not(%rip), %rax
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
.globl _Not__uNot_var_LC_LC
	.private_extern _Not__uNot_var_LC_LC
_Not__uNot_var_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Not(%rip), %rax
	notq	%rax
	leave
	ret
.globl _Not__Not_var_LC_LC
	.private_extern _Not__Not_var_LC_LC
_Not__Not_var_LC_LC:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_Not(%rip), %rax
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
.globl _Not__Not_var_f64_f64
	.private_extern _Not__Not_var_f64_f64
_Not__Not_var_f64_f64:
	pushq	%rbp
	movq	%rsp, %rbp
	movsd	128+_MM_Not(%rip), %xmm1
	movsd	LC0(%rip), %xmm0
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
	movsd	LC0(%rip), %xmm0
	xorpd	%xmm1, %xmm0
	leave
	ret
.globl _Not__uNot_var_i16_i16
	.private_extern _Not__uNot_var_i16_i16
_Not__uNot_var_i16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	136+_MM_Not(%rip), %eax
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
	movzwl	136+_MM_Not(%rip), %eax
	movzwl	%ax, %eax
	negq	%rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L36
	cmpq	$65535, -16(%rbp)
	jle	L37
L36:
	movl	$2209, %edi
	call	__m3_fault
L37:
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
	js	L42
	cmpq	$65535, -16(%rbp)
	jle	L43
L42:
	movl	$2273, %edi
	call	__m3_fault
L43:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Not__uNot_var_i32_i32
	.private_extern _Not__uNot_var_i32_i32
_Not__uNot_var_i32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	140+_MM_Not(%rip), %eax
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
	movl	140+_MM_Not(%rip), %eax
	mov	%eax, %eax
	negq	%rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L48
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L49
L48:
	movl	$2337, %edi
	call	__m3_fault
L49:
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
	js	L54
	movl	$4294967295, %eax
	cmpq	%rax, -16(%rbp)
	jle	L55
L54:
	movl	$2401, %edi
	call	__m3_fault
L55:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Not__uNot_var_I_I
	.private_extern _Not__uNot_var_I_I
_Not__uNot_var_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Not(%rip), %rax
	notq	%rax
	leave
	ret
.globl _Not__Not_var_I_I
	.private_extern _Not__Not_var_I_I
_Not__Not_var_I_I:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	144+_MM_Not(%rip), %rax
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
.globl _Not__uNot_var_i64_i64
	.private_extern _Not__uNot_var_i64_i64
_Not__uNot_var_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Not(%rip), %rax
	notq	%rax
	leave
	ret
.globl _Not__Not_var_i64_i64
	.private_extern _Not__Not_var_i64_i64
_Not__Not_var_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	152+_MM_Not(%rip), %rax
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
.globl _Not__uNot_var_C_C
	.private_extern _Not__uNot_var_C_C
_Not__uNot_var_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	160+_MM_Not(%rip), %rax
	notq	%rax
	leave
	ret
.globl _Not__Not_var_C_C
	.private_extern _Not__Not_var_C_C
_Not__Not_var_C_C:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	160+_MM_Not(%rip), %rax
	negq	%rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jns	L76
	movl	$2721, %edi
	call	__m3_fault
L76:
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
	jns	L81
	movl	$2785, %edi
	call	__m3_fault
L81:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Not__Not_var_f32_f32
	.private_extern _Not__Not_var_f32_f32
_Not__Not_var_f32_f32:
	pushq	%rbp
	movq	%rsp, %rbp
	movss	168+_MM_Not(%rip), %xmm1
	movss	LC1(%rip), %xmm0
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
	movss	LC1(%rip), %xmm0
	xorps	%xmm1, %xmm0
	leave
	ret
.globl _Not__uNot_var_u8_u8
	.private_extern _Not__uNot_var_u8_u8
_Not__uNot_var_u8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	172+_MM_Not(%rip), %eax
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
	movzbl	172+_MM_Not(%rip), %eax
	movsbq	%al,%rax
	negq	%rax
	movq	%rax, -16(%rbp)
	cmpq	$-128, -16(%rbp)
	jl	L90
	cmpq	$127, -16(%rbp)
	jle	L91
L90:
	movl	$2913, %edi
	call	__m3_fault
L91:
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
	jl	L96
	cmpq	$127, -16(%rbp)
	jle	L97
L96:
	movl	$2977, %edi
	call	__m3_fault
L97:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Not__uNot_var_i8_i8
	.private_extern _Not__uNot_var_i8_i8
_Not__uNot_var_i8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	173+_MM_Not(%rip), %eax
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
	movzbl	173+_MM_Not(%rip), %eax
	movzbl	%al, %eax
	negq	%rax
	movq	%rax, -16(%rbp)
	cmpq	$0, -16(%rbp)
	js	L102
	cmpq	$255, -16(%rbp)
	jle	L103
L102:
	movl	$3041, %edi
	call	__m3_fault
L103:
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
	js	L108
	cmpq	$255, -16(%rbp)
	jle	L109
L108:
	movl	$3105, %edi
	call	__m3_fault
L109:
	movq	-16(%rbp), %rax
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
.globl _Not__uNot_var_u32_u32
	.private_extern _Not__uNot_var_u32_u32
_Not__uNot_var_u32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	184+_MM_Not(%rip), %eax
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
	movl	184+_MM_Not(%rip), %eax
	cltq
	negq	%rax
	movq	%rax, -16(%rbp)
	cmpq	$-2147483648, -16(%rbp)
	jl	L122
	cmpq	$2147483647, -16(%rbp)
	jle	L123
L122:
	movl	$3297, %edi
	call	__m3_fault
L123:
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
	jl	L128
	cmpq	$2147483647, -16(%rbp)
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
	.ascii "Not_param_u32_u32"
	.space 1
	.ascii "uNot_param_u32_u32"
	.space 1
	.ascii "Not_var_u32_u32"
	.space 1
	.ascii "uNot_var_u32_u32"
	.space 1
	.ascii "Not_param_L_L"
	.space 1
	.ascii "uNot_param_L_L"
	.space 1
	.ascii "Not_var_L_L"
	.space 1
	.ascii "uNot_var_L_L"
	.space 1
	.ascii "Not_param_i8_i8"
	.space 1
	.ascii "uNot_param_i8_i8"
	.space 1
	.ascii "Not_var_i8_i8"
	.space 1
	.ascii "uNot_var_i8_i8"
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
	.ascii "Not_param_C_C"
	.space 1
	.ascii "uNot_param_C_C"
	.space 1
	.ascii "Not_var_C_C"
	.space 1
	.ascii "uNot_var_C_C"
	.space 1
	.ascii "Not_param_i64_i64"
	.space 1
	.ascii "uNot_param_i64_i64"
	.space 1
	.ascii "Not_var_i64_i64"
	.space 1
	.ascii "uNot_var_i64_i64"
	.space 1
	.ascii "Not_param_I_I"
	.space 1
	.ascii "uNot_param_I_I"
	.space 1
	.ascii "Not_var_I_I"
	.space 1
	.ascii "uNot_var_I_I"
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
	.ascii "Not_param_f64_f64"
	.space 1
	.ascii "Not_var_f64_f64"
	.space 1
	.ascii "Not_param_LC_LC"
	.space 1
	.ascii "uNot_param_LC_LC"
	.space 1
	.ascii "Not_var_LC_LC"
	.space 1
	.ascii "uNot_var_LC_LC"
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
	.space 6
	.quad	_Not_M3
	.quad	_L_1
	.quad	_Not__Not_param_u32_u32
	.quad	_L_1+7
	.quad	_Not__uNot_param_u32_u32
	.quad	_L_1+25
	.quad	_Not__Not_var_u32_u32
	.quad	_L_1+44
	.quad	_Not__uNot_var_u32_u32
	.quad	_L_1+60
	.quad	_Not__Not_param_L_L
	.quad	_L_1+77
	.quad	_Not__uNot_param_L_L
	.quad	_L_1+91
	.quad	_Not__Not_var_L_L
	.quad	_L_1+106
	.quad	_Not__uNot_var_L_L
	.quad	_L_1+118
	.quad	_Not__Not_param_i8_i8
	.quad	_L_1+131
	.quad	_Not__uNot_param_i8_i8
	.quad	_L_1+147
	.quad	_Not__Not_var_i8_i8
	.quad	_L_1+164
	.quad	_Not__uNot_var_i8_i8
	.quad	_L_1+178
	.quad	_Not__Not_param_u8_u8
	.quad	_L_1+193
	.quad	_Not__uNot_param_u8_u8
	.quad	_L_1+209
	.quad	_Not__Not_var_u8_u8
	.quad	_L_1+226
	.quad	_Not__uNot_var_u8_u8
	.quad	_L_1+240
	.quad	_Not__Not_param_f32_f32
	.quad	_L_1+255
	.quad	_Not__Not_var_f32_f32
	.quad	_L_1+273
	.quad	_Not__Not_param_C_C
	.quad	_L_1+289
	.quad	_Not__uNot_param_C_C
	.quad	_L_1+303
	.quad	_Not__Not_var_C_C
	.quad	_L_1+318
	.quad	_Not__uNot_var_C_C
	.quad	_L_1+330
	.quad	_Not__Not_param_i64_i64
	.quad	_L_1+343
	.quad	_Not__uNot_param_i64_i64
	.quad	_L_1+361
	.quad	_Not__Not_var_i64_i64
	.quad	_L_1+380
	.quad	_Not__uNot_var_i64_i64
	.quad	_L_1+396
	.quad	_Not__Not_param_I_I
	.quad	_L_1+413
	.quad	_Not__uNot_param_I_I
	.quad	_L_1+427
	.quad	_Not__Not_var_I_I
	.quad	_L_1+442
	.quad	_Not__uNot_var_I_I
	.quad	_L_1+454
	.quad	_Not__Not_param_i32_i32
	.quad	_L_1+467
	.quad	_Not__uNot_param_i32_i32
	.quad	_L_1+485
	.quad	_Not__Not_var_i32_i32
	.quad	_L_1+504
	.quad	_Not__uNot_var_i32_i32
	.quad	_L_1+520
	.quad	_Not__Not_param_i16_i16
	.quad	_L_1+537
	.quad	_Not__uNot_param_i16_i16
	.quad	_L_1+555
	.quad	_Not__Not_var_i16_i16
	.quad	_L_1+574
	.quad	_Not__uNot_var_i16_i16
	.quad	_L_1+590
	.quad	_Not__Not_param_f64_f64
	.quad	_L_1+607
	.quad	_Not__Not_var_f64_f64
	.quad	_L_1+625
	.quad	_Not__Not_param_LC_LC
	.quad	_L_1+641
	.quad	_Not__uNot_param_LC_LC
	.quad	_L_1+657
	.quad	_Not__Not_var_LC_LC
	.quad	_L_1+674
	.quad	_Not__uNot_var_LC_LC
	.quad	_L_1+688
	.quad	_Not__Not_param_u64_u64
	.quad	_L_1+703
	.quad	_Not__uNot_param_u64_u64
	.quad	_L_1+721
	.quad	_Not__Not_var_u64_u64
	.quad	_L_1+740
	.quad	_Not__uNot_var_u64_u64
	.quad	_L_1+756
	.quad	_Not__Not_param_u16_u16
	.quad	_L_1+773
	.quad	_Not__uNot_param_u16_u16
	.quad	_L_1+791
	.quad	_Not__Not_var_u16_u16
	.quad	_L_1+810
	.quad	_Not__uNot_var_u16_u16
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
	.word	593
	.space 6
	.quad	594
	.quad	595
	.long	2817498546
	.long	1082303686
	.word	598
	.space 2
	.long	599
	.quad	600
	.quad	601
	.quad	602
	.long	1142351528
	.byte	93
	.byte	94
	.space 2
	.quad	607
	.long	608
	.space 28
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
	.long	0
	.long	-2147483648
	.long	0
	.long	0
	.align 4
LC1:
	.long	2147483648
	.long	0
	.long	0
	.long	0
	.subsections_via_symbols
