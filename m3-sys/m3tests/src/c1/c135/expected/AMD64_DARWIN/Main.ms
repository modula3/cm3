	.text
.globl _Main__ret_ki8
	.private_extern _Main__ret_ki8
_Main__ret_ki8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$1, %eax
	leave
	ret
.globl _Main__ret_ki16
	.private_extern _Main__ret_ki16
_Main__ret_ki16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$2, %eax
	leave
	ret
.globl _Main__ret_ki32
	.private_extern _Main__ret_ki32
_Main__ret_ki32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$3, %eax
	leave
	ret
.globl _Main__ret_ki64
	.private_extern _Main__ret_ki64
_Main__ret_ki64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$4, %eax
	leave
	ret
.globl _Main__ret_ku8
	.private_extern _Main__ret_ku8
_Main__ret_ku8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$5, %eax
	leave
	ret
.globl _Main__ret_ku16
	.private_extern _Main__ret_ku16
_Main__ret_ku16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$6, %eax
	leave
	ret
.globl _Main__ret_ku32
	.private_extern _Main__ret_ku32
_Main__ret_ku32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$7, %eax
	leave
	ret
.globl _Main__ret_ku64
	.private_extern _Main__ret_ku64
_Main__ret_ku64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$8, %eax
	leave
	ret
.globl _Main__ret_pi8_i8
	.private_extern _Main__ret_pi8_i8
_Main__ret_pi8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movzbl	-17(%rbp), %eax
	movsbq	%al,%rax
	leave
	ret
.globl _Main__ret_pi8_i16
	.private_extern _Main__ret_pi8_i16
_Main__ret_pi8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movzbl	-17(%rbp), %eax
	movsbq	%al,%rax
	leave
	ret
.globl _Main__ret_pi8_i32
	.private_extern _Main__ret_pi8_i32
_Main__ret_pi8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movzbl	-17(%rbp), %eax
	movsbq	%al,%rax
	leave
	ret
.globl _Main__ret_pi8_i64
	.private_extern _Main__ret_pi8_i64
_Main__ret_pi8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movzbl	-17(%rbp), %eax
	movsbq	%al,%rax
	leave
	ret
.globl _Main__ret_pi8_u8
	.private_extern _Main__ret_pi8_u8
_Main__ret_pi8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movzbl	-17(%rbp), %eax
	movzbl	%al, %eax
	leave
	ret
.globl _Main__ret_pi8_u16
	.private_extern _Main__ret_pi8_u16
_Main__ret_pi8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movzbl	-17(%rbp), %eax
	movsbq	%al,%rax
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L28
	movl	$1697, %edi
	call	__m3_fault
L28:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Main__ret_pi8_u32
	.private_extern _Main__ret_pi8_u32
_Main__ret_pi8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movzbl	-17(%rbp), %eax
	movsbq	%al,%rax
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L31
	movl	$1729, %edi
	call	__m3_fault
L31:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Main__ret_pi8_u64
	.private_extern _Main__ret_pi8_u64
_Main__ret_pi8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movzbl	-17(%rbp), %eax
	movsbq	%al,%rax
	leave
	ret
.globl _Main__ret_pi16_i8
	.private_extern _Main__ret_pi16_i8
_Main__ret_pi16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movzwl	-18(%rbp), %eax
	movswq	%ax,%rax
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	cmpq	$-128, %rax
	jl	L36
	movq	-16(%rbp), %rax
	cmpq	$127, %rax
	jle	L37
L36:
	movl	$1825, %edi
	call	__m3_fault
L37:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Main__ret_pi16_i16
	.private_extern _Main__ret_pi16_i16
_Main__ret_pi16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movzwl	-18(%rbp), %eax
	movswq	%ax,%rax
	leave
	ret
.globl _Main__ret_pi16_i32
	.private_extern _Main__ret_pi16_i32
_Main__ret_pi16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movzwl	-18(%rbp), %eax
	movswq	%ax,%rax
	leave
	ret
.globl _Main__ret_pi16_i64
	.private_extern _Main__ret_pi16_i64
_Main__ret_pi16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movzwl	-18(%rbp), %eax
	movswq	%ax,%rax
	leave
	ret
.globl _Main__ret_pi16_u8
	.private_extern _Main__ret_pi16_u8
_Main__ret_pi16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movzwl	-18(%rbp), %eax
	movzwl	%ax, %eax
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	cmpq	$255, %rax
	jle	L46
	movl	$1953, %edi
	call	__m3_fault
L46:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Main__ret_pi16_u16
	.private_extern _Main__ret_pi16_u16
_Main__ret_pi16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movzwl	-18(%rbp), %eax
	movswq	%ax,%rax
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L49
	movl	$1985, %edi
	call	__m3_fault
L49:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Main__ret_pi16_u32
	.private_extern _Main__ret_pi16_u32
_Main__ret_pi16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movzwl	-18(%rbp), %eax
	movswq	%ax,%rax
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L52
	movl	$2017, %edi
	call	__m3_fault
L52:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Main__ret_pi16_u64
	.private_extern _Main__ret_pi16_u64
_Main__ret_pi16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movzwl	-18(%rbp), %eax
	movswq	%ax,%rax
	leave
	ret
.globl _Main__ret_pi32_i8
	.private_extern _Main__ret_pi32_i8
_Main__ret_pi32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	cmpq	$-128, %rax
	jl	L57
	movq	-16(%rbp), %rax
	cmpq	$127, %rax
	jle	L58
L57:
	movl	$2145, %edi
	call	__m3_fault
L58:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Main__ret_pi32_i16
	.private_extern _Main__ret_pi32_i16
_Main__ret_pi32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	cmpq	$-32768, %rax
	jl	L61
	movq	-16(%rbp), %rax
	cmpq	$32767, %rax
	jle	L62
L61:
	movl	$2177, %edi
	call	__m3_fault
L62:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Main__ret_pi32_i32
	.private_extern _Main__ret_pi32_i32
_Main__ret_pi32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	leave
	ret
.globl _Main__ret_pi32_i64
	.private_extern _Main__ret_pi32_i64
_Main__ret_pi32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	leave
	ret
.globl _Main__ret_pi32_u8
	.private_extern _Main__ret_pi32_u8
_Main__ret_pi32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	mov	%eax, %eax
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	cmpq	$255, %rax
	jle	L69
	movl	$2273, %edi
	call	__m3_fault
L69:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Main__ret_pi32_u16
	.private_extern _Main__ret_pi32_u16
_Main__ret_pi32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	testq	%rax, %rax
	js	L72
	movq	-16(%rbp), %rax
	cmpq	$65535, %rax
	jle	L73
L72:
	movl	$2305, %edi
	call	__m3_fault
L73:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Main__ret_pi32_u32
	.private_extern _Main__ret_pi32_u32
_Main__ret_pi32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L76
	movl	$2337, %edi
	call	__m3_fault
L76:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Main__ret_pi32_u64
	.private_extern _Main__ret_pi32_u64
_Main__ret_pi32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	leave
	ret
.globl _Main__ret_pi64_i8
	.private_extern _Main__ret_pi64_i8
_Main__ret_pi64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	cmpq	$-128, %rax
	jl	L81
	movq	-16(%rbp), %rax
	cmpq	$127, %rax
	jle	L82
L81:
	movl	$2465, %edi
	call	__m3_fault
L82:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Main__ret_pi64_i16
	.private_extern _Main__ret_pi64_i16
_Main__ret_pi64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	cmpq	$-32768, %rax
	jl	L85
	movq	-16(%rbp), %rax
	cmpq	$32767, %rax
	jle	L86
L85:
	movl	$2497, %edi
	call	__m3_fault
L86:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Main__ret_pi64_i32
	.private_extern _Main__ret_pi64_i32
_Main__ret_pi64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	cmpq	$-2147483648, %rax
	jl	L89
	movq	-16(%rbp), %rax
	cmpq	$2147483647, %rax
	jle	L90
L89:
	movl	$2529, %edi
	call	__m3_fault
L90:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Main__ret_pi64_i64
	.private_extern _Main__ret_pi64_i64
_Main__ret_pi64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _Main__ret_pi64_u8
	.private_extern _Main__ret_pi64_u8
_Main__ret_pi64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	testq	%rax, %rax
	js	L95
	movq	-16(%rbp), %rax
	cmpq	$255, %rax
	jle	L96
L95:
	movl	$2593, %edi
	call	__m3_fault
L96:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Main__ret_pi64_u16
	.private_extern _Main__ret_pi64_u16
_Main__ret_pi64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	testq	%rax, %rax
	js	L99
	movq	-16(%rbp), %rax
	cmpq	$65535, %rax
	jle	L100
L99:
	movl	$2625, %edi
	call	__m3_fault
L100:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Main__ret_pi64_u32
	.private_extern _Main__ret_pi64_u32
_Main__ret_pi64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	testq	%rax, %rax
	js	L103
	movq	-16(%rbp), %rdx
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L104
L103:
	movl	$2657, %edi
	call	__m3_fault
L104:
	movq	-16(%rbp), %rax
	leave
	ret
.globl _Main__ret_pi64_u64
	.private_extern _Main__ret_pi64_u64
_Main__ret_pi64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _Main__add_vi8_vi8
	.private_extern _Main__add_vi8_vi8
_Main__add_vi8_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_Main(%rip), %eax
	movsbq	%al,%rdx
	movzbl	104+_MM_Main(%rip), %eax
	movsbq	%al,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Main__add_vi8_vi16
	.private_extern _Main__add_vi8_vi16
_Main__add_vi8_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_Main(%rip), %eax
	movsbq	%al,%rdx
	movzwl	106+_MM_Main(%rip), %eax
	movswq	%ax,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Main__add_vi8_vi32
	.private_extern _Main__add_vi8_vi32
_Main__add_vi8_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_Main(%rip), %eax
	movsbq	%al,%rdx
	movl	108+_MM_Main(%rip), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Main__add_vi8_vi64
	.private_extern _Main__add_vi8_vi64
_Main__add_vi8_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_Main(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_Main(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Main__add_vi16_vi8
	.private_extern _Main__add_vi16_vi8
_Main__add_vi16_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_Main(%rip), %eax
	movswq	%ax,%rdx
	movzbl	104+_MM_Main(%rip), %eax
	movsbq	%al,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Main__add_vi16_vi16
	.private_extern _Main__add_vi16_vi16
_Main__add_vi16_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_Main(%rip), %eax
	movswq	%ax,%rdx
	movzwl	106+_MM_Main(%rip), %eax
	movswq	%ax,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Main__add_vi16_vi32
	.private_extern _Main__add_vi16_vi32
_Main__add_vi16_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_Main(%rip), %eax
	movswq	%ax,%rdx
	movl	108+_MM_Main(%rip), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Main__add_vi16_vi64
	.private_extern _Main__add_vi16_vi64
_Main__add_vi16_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_Main(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_Main(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Main__add_vi32_vi8
	.private_extern _Main__add_vi32_vi8
_Main__add_vi32_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	108+_MM_Main(%rip), %eax
	movslq	%eax,%rdx
	movzbl	104+_MM_Main(%rip), %eax
	movsbq	%al,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Main__add_vi32_vi16
	.private_extern _Main__add_vi32_vi16
_Main__add_vi32_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	108+_MM_Main(%rip), %eax
	movslq	%eax,%rdx
	movzwl	106+_MM_Main(%rip), %eax
	movswq	%ax,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Main__add_vi32_vi32
	.private_extern _Main__add_vi32_vi32
_Main__add_vi32_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	108+_MM_Main(%rip), %eax
	movslq	%eax,%rdx
	movl	108+_MM_Main(%rip), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Main__add_vi32_vi64
	.private_extern _Main__add_vi32_vi64
_Main__add_vi32_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	108+_MM_Main(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_Main(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Main__add_vi64_vi8
	.private_extern _Main__add_vi64_vi8
_Main__add_vi64_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_Main(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_Main(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Main__add_vi64_vi16
	.private_extern _Main__add_vi64_vi16
_Main__add_vi64_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_Main(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_Main(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Main__add_vi64_vi32
	.private_extern _Main__add_vi64_vi32
_Main__add_vi64_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	108+_MM_Main(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_Main(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Main__add_vi64_vi64
	.private_extern _Main__add_vi64_vi64
_Main__add_vi64_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Main(%rip), %rdx
	movq	112+_MM_Main(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _Main__and_vi8_vi8
	.private_extern _Main__and_vi8_vi8
_Main__and_vi8_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_Main(%rip), %eax
	movsbq	%al,%rdx
	movzbl	104+_MM_Main(%rip), %eax
	movsbq	%al,%rax
	andq	%rdx, %rax
	leave
	ret
.globl _Main__and_vi8_vi16
	.private_extern _Main__and_vi8_vi16
_Main__and_vi8_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_Main(%rip), %eax
	movsbq	%al,%rdx
	movzwl	106+_MM_Main(%rip), %eax
	movswq	%ax,%rax
	andq	%rdx, %rax
	leave
	ret
.globl _Main__and_vi8_vi32
	.private_extern _Main__and_vi8_vi32
_Main__and_vi8_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_Main(%rip), %eax
	movsbq	%al,%rdx
	movl	108+_MM_Main(%rip), %eax
	cltq
	andq	%rdx, %rax
	leave
	ret
.globl _Main__and_vi8_vi64
	.private_extern _Main__and_vi8_vi64
_Main__and_vi8_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_Main(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_Main(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _Main__and_vi16_vi8
	.private_extern _Main__and_vi16_vi8
_Main__and_vi16_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_Main(%rip), %eax
	movswq	%ax,%rdx
	movzbl	104+_MM_Main(%rip), %eax
	movsbq	%al,%rax
	andq	%rdx, %rax
	leave
	ret
.globl _Main__and_vi16_vi16
	.private_extern _Main__and_vi16_vi16
_Main__and_vi16_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_Main(%rip), %eax
	movswq	%ax,%rdx
	movzwl	106+_MM_Main(%rip), %eax
	movswq	%ax,%rax
	andq	%rdx, %rax
	leave
	ret
.globl _Main__and_vi16_vi32
	.private_extern _Main__and_vi16_vi32
_Main__and_vi16_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_Main(%rip), %eax
	movswq	%ax,%rdx
	movl	108+_MM_Main(%rip), %eax
	cltq
	andq	%rdx, %rax
	leave
	ret
.globl _Main__and_vi16_vi64
	.private_extern _Main__and_vi16_vi64
_Main__and_vi16_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_Main(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_Main(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _Main__and_vi32_vi8
	.private_extern _Main__and_vi32_vi8
_Main__and_vi32_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	108+_MM_Main(%rip), %eax
	movslq	%eax,%rdx
	movzbl	104+_MM_Main(%rip), %eax
	movsbq	%al,%rax
	andq	%rdx, %rax
	leave
	ret
.globl _Main__and_vi32_vi16
	.private_extern _Main__and_vi32_vi16
_Main__and_vi32_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	108+_MM_Main(%rip), %eax
	movslq	%eax,%rdx
	movzwl	106+_MM_Main(%rip), %eax
	movswq	%ax,%rax
	andq	%rdx, %rax
	leave
	ret
.globl _Main__and_vi32_vi32
	.private_extern _Main__and_vi32_vi32
_Main__and_vi32_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	108+_MM_Main(%rip), %eax
	movslq	%eax,%rdx
	movl	108+_MM_Main(%rip), %eax
	cltq
	andq	%rdx, %rax
	leave
	ret
.globl _Main__and_vi32_vi64
	.private_extern _Main__and_vi32_vi64
_Main__and_vi32_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	108+_MM_Main(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_Main(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _Main__and_vi64_vi8
	.private_extern _Main__and_vi64_vi8
_Main__and_vi64_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_Main(%rip), %eax
	movsbq	%al,%rdx
	movq	112+_MM_Main(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _Main__and_vi64_vi16
	.private_extern _Main__and_vi64_vi16
_Main__and_vi64_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_Main(%rip), %eax
	movswq	%ax,%rdx
	movq	112+_MM_Main(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _Main__and_vi64_vi32
	.private_extern _Main__and_vi64_vi32
_Main__and_vi64_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	108+_MM_Main(%rip), %eax
	movslq	%eax,%rdx
	movq	112+_MM_Main(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _Main__and_vi64_vi64
	.private_extern _Main__and_vi64_vi64
_Main__and_vi64_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	112+_MM_Main(%rip), %rdx
	movq	112+_MM_Main(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _Main_M3
_Main_M3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	leaq	_MM_Main(%rip), %rax
	leave
	ret
__m3_fault:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rsi
	leaq	_MM_Main(%rip), %rdi
	call	_RTHooks__ReportFault
	leave
	ret
	.const_data
	.align 5
_L_1:
	.ascii "Main_M3"
	.space 1
	.ascii "and_vi64_vi64"
	.space 1
	.ascii "and_vi64_vi32"
	.space 1
	.ascii "and_vi64_vi16"
	.space 1
	.ascii "and_vi64_vi8"
	.space 1
	.ascii "and_vi32_vi64"
	.space 1
	.ascii "and_vi32_vi32"
	.space 1
	.ascii "and_vi32_vi16"
	.space 1
	.ascii "and_vi32_vi8"
	.space 1
	.ascii "and_vi16_vi64"
	.space 1
	.ascii "and_vi16_vi32"
	.space 1
	.ascii "and_vi16_vi16"
	.space 1
	.ascii "and_vi16_vi8"
	.space 1
	.ascii "and_vi8_vi64"
	.space 1
	.ascii "and_vi8_vi32"
	.space 1
	.ascii "and_vi8_vi16"
	.space 1
	.ascii "and_vi8_vi8"
	.space 1
	.ascii "add_vi64_vi64"
	.space 1
	.ascii "add_vi64_vi32"
	.space 1
	.ascii "add_vi64_vi16"
	.space 1
	.ascii "add_vi64_vi8"
	.space 1
	.ascii "add_vi32_vi64"
	.space 1
	.ascii "add_vi32_vi32"
	.space 1
	.ascii "add_vi32_vi16"
	.space 1
	.ascii "add_vi32_vi8"
	.space 1
	.ascii "add_vi16_vi64"
	.space 1
	.ascii "add_vi16_vi32"
	.space 1
	.ascii "add_vi16_vi16"
	.space 1
	.ascii "add_vi16_vi8"
	.space 1
	.ascii "add_vi8_vi64"
	.space 1
	.ascii "add_vi8_vi32"
	.space 1
	.ascii "add_vi8_vi16"
	.space 1
	.ascii "add_vi8_vi8"
	.space 1
	.ascii "ret_pi64_u64"
	.space 1
	.ascii "ret_pi64_u32"
	.space 1
	.ascii "ret_pi64_u16"
	.space 1
	.ascii "ret_pi64_u8"
	.space 1
	.ascii "ret_pi64_i64"
	.space 1
	.ascii "ret_pi64_i32"
	.space 1
	.ascii "ret_pi64_i16"
	.space 1
	.ascii "ret_pi64_i8"
	.space 1
	.ascii "ret_pi32_u64"
	.space 1
	.ascii "ret_pi32_u32"
	.space 1
	.ascii "ret_pi32_u16"
	.space 1
	.ascii "ret_pi32_u8"
	.space 1
	.ascii "ret_pi32_i64"
	.space 1
	.ascii "ret_pi32_i32"
	.space 1
	.ascii "ret_pi32_i16"
	.space 1
	.ascii "ret_pi32_i8"
	.space 1
	.ascii "ret_pi16_u64"
	.space 1
	.ascii "ret_pi16_u32"
	.space 1
	.ascii "ret_pi16_u16"
	.space 1
	.ascii "ret_pi16_u8"
	.space 1
	.ascii "ret_pi16_i64"
	.space 1
	.ascii "ret_pi16_i32"
	.space 1
	.ascii "ret_pi16_i16"
	.space 1
	.ascii "ret_pi16_i8"
	.space 1
	.ascii "ret_pi8_u64"
	.space 1
	.ascii "ret_pi8_u32"
	.space 1
	.ascii "ret_pi8_u16"
	.space 1
	.ascii "ret_pi8_u8"
	.space 1
	.ascii "ret_pi8_i64"
	.space 1
	.ascii "ret_pi8_i32"
	.space 1
	.ascii "ret_pi8_i16"
	.space 1
	.ascii "ret_pi8_i8"
	.space 1
	.ascii "ret_ku64"
	.space 1
	.ascii "ret_ku32"
	.space 1
	.ascii "ret_ku16"
	.space 1
	.ascii "ret_ku8"
	.space 1
	.ascii "ret_ki64"
	.space 1
	.ascii "ret_ki32"
	.space 1
	.ascii "ret_ki16"
	.space 1
	.ascii "ret_ki8"
	.space 3
	.quad	_Main_M3
	.quad	_L_1
	.quad	_Main__and_vi64_vi64
	.quad	_L_1+8
	.quad	_Main__and_vi64_vi32
	.quad	_L_1+22
	.quad	_Main__and_vi64_vi16
	.quad	_L_1+36
	.quad	_Main__and_vi64_vi8
	.quad	_L_1+50
	.quad	_Main__and_vi32_vi64
	.quad	_L_1+63
	.quad	_Main__and_vi32_vi32
	.quad	_L_1+77
	.quad	_Main__and_vi32_vi16
	.quad	_L_1+91
	.quad	_Main__and_vi32_vi8
	.quad	_L_1+105
	.quad	_Main__and_vi16_vi64
	.quad	_L_1+118
	.quad	_Main__and_vi16_vi32
	.quad	_L_1+132
	.quad	_Main__and_vi16_vi16
	.quad	_L_1+146
	.quad	_Main__and_vi16_vi8
	.quad	_L_1+160
	.quad	_Main__and_vi8_vi64
	.quad	_L_1+173
	.quad	_Main__and_vi8_vi32
	.quad	_L_1+186
	.quad	_Main__and_vi8_vi16
	.quad	_L_1+199
	.quad	_Main__and_vi8_vi8
	.quad	_L_1+212
	.quad	_Main__add_vi64_vi64
	.quad	_L_1+224
	.quad	_Main__add_vi64_vi32
	.quad	_L_1+238
	.quad	_Main__add_vi64_vi16
	.quad	_L_1+252
	.quad	_Main__add_vi64_vi8
	.quad	_L_1+266
	.quad	_Main__add_vi32_vi64
	.quad	_L_1+279
	.quad	_Main__add_vi32_vi32
	.quad	_L_1+293
	.quad	_Main__add_vi32_vi16
	.quad	_L_1+307
	.quad	_Main__add_vi32_vi8
	.quad	_L_1+321
	.quad	_Main__add_vi16_vi64
	.quad	_L_1+334
	.quad	_Main__add_vi16_vi32
	.quad	_L_1+348
	.quad	_Main__add_vi16_vi16
	.quad	_L_1+362
	.quad	_Main__add_vi16_vi8
	.quad	_L_1+376
	.quad	_Main__add_vi8_vi64
	.quad	_L_1+389
	.quad	_Main__add_vi8_vi32
	.quad	_L_1+402
	.quad	_Main__add_vi8_vi16
	.quad	_L_1+415
	.quad	_Main__add_vi8_vi8
	.quad	_L_1+428
	.quad	_Main__ret_pi64_u64
	.quad	_L_1+440
	.quad	_Main__ret_pi64_u32
	.quad	_L_1+453
	.quad	_Main__ret_pi64_u16
	.quad	_L_1+466
	.quad	_Main__ret_pi64_u8
	.quad	_L_1+479
	.quad	_Main__ret_pi64_i64
	.quad	_L_1+491
	.quad	_Main__ret_pi64_i32
	.quad	_L_1+504
	.quad	_Main__ret_pi64_i16
	.quad	_L_1+517
	.quad	_Main__ret_pi64_i8
	.quad	_L_1+530
	.quad	_Main__ret_pi32_u64
	.quad	_L_1+542
	.quad	_Main__ret_pi32_u32
	.quad	_L_1+555
	.quad	_Main__ret_pi32_u16
	.quad	_L_1+568
	.quad	_Main__ret_pi32_u8
	.quad	_L_1+581
	.quad	_Main__ret_pi32_i64
	.quad	_L_1+593
	.quad	_Main__ret_pi32_i32
	.quad	_L_1+606
	.quad	_Main__ret_pi32_i16
	.quad	_L_1+619
	.quad	_Main__ret_pi32_i8
	.quad	_L_1+632
	.quad	_Main__ret_pi16_u64
	.quad	_L_1+644
	.quad	_Main__ret_pi16_u32
	.quad	_L_1+657
	.quad	_Main__ret_pi16_u16
	.quad	_L_1+670
	.quad	_Main__ret_pi16_u8
	.quad	_L_1+683
	.quad	_Main__ret_pi16_i64
	.quad	_L_1+695
	.quad	_Main__ret_pi16_i32
	.quad	_L_1+708
	.quad	_Main__ret_pi16_i16
	.quad	_L_1+721
	.quad	_Main__ret_pi16_i8
	.quad	_L_1+734
	.quad	_Main__ret_pi8_u64
	.quad	_L_1+746
	.quad	_Main__ret_pi8_u32
	.quad	_L_1+758
	.quad	_Main__ret_pi8_u16
	.quad	_L_1+770
	.quad	_Main__ret_pi8_u8
	.quad	_L_1+782
	.quad	_Main__ret_pi8_i64
	.quad	_L_1+793
	.quad	_Main__ret_pi8_i32
	.quad	_L_1+805
	.quad	_Main__ret_pi8_i16
	.quad	_L_1+817
	.quad	_Main__ret_pi8_i8
	.quad	_L_1+829
	.quad	_Main__ret_ku64
	.quad	_L_1+840
	.quad	_Main__ret_ku32
	.quad	_L_1+849
	.quad	_Main__ret_ku16
	.quad	_L_1+858
	.quad	_Main__ret_ku8
	.quad	_L_1+867
	.quad	_Main__ret_ki64
	.quad	_L_1+875
	.quad	_Main__ret_ki32
	.quad	_L_1+884
	.quad	_Main__ret_ki16
	.quad	_L_1+893
	.quad	_Main__ret_ki8
	.quad	_L_1+902
	.space 8
	.ascii "../Main.m3"
	.space 6
	.data
	.align 5
_MM_Main:
	.quad	_L_1+2088
	.space 32
	.quad	_L_1+912
	.space 24
	.quad	_MM_Main+136
	.space 8
	.quad	_Main_M3
	.quad	3
	.byte	1
	.space 1
	.word	2
	.long	3
	.quad	4
	.byte	9
	.space 1
	.word	10
	.long	11
	.quad	12
	.space 8
	.quad	_Main_I3
	.quad	_MM_Main+160
	.space 8
	.quad	_Long_I3
	.quad	_MM_Main+184
	.space 8
	.quad	_Word_I3
	.quad	_MM_Main+208
	.space 8
	.quad	_Cstdint_I3
	.quad	_MM_Main+232
	.space 8
	.quad	_RTHooks_I3
	.space 8
	.subsections_via_symbols
