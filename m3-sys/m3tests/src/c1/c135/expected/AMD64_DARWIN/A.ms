	.text
.globl _A__ret_vi8
	.private_extern _A__ret_vi8
_A__ret_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rax
	leave
	ret
.globl _A__ret_ki8
	.private_extern _A__ret_ki8
_A__ret_ki8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$3, %eax
	leave
	ret
.globl _A__ret_pi8_i8
	.private_extern _A__ret_pi8_i8
_A__ret_pi8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movsbq	-17(%rbp),%rax
	leave
	ret
.globl _A__ret_pi8_u8
	.private_extern _A__ret_pi8_u8
_A__ret_pi8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movzbl	-17(%rbp), %eax
	leave
	ret
.globl _A__ret_pi8_i16
	.private_extern _A__ret_pi8_i16
_A__ret_pi8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movswq	-18(%rbp),%rax
	leave
	ret
.globl _A__ret_pi8_u16
	.private_extern _A__ret_pi8_u16
_A__ret_pi8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movzwl	-18(%rbp), %eax
	leave
	ret
.globl _A__ret_pi8_i32
	.private_extern _A__ret_pi8_i32
_A__ret_pi8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	leave
	ret
.globl _A__ret_pi8_u32
	.private_extern _A__ret_pi8_u32
_A__ret_pi8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	leave
	ret
.globl _A__ret_pi8_i64
	.private_extern _A__ret_pi8_i64
_A__ret_pi8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _A__ret_pi8_u64
	.private_extern _A__ret_pi8_u64
_A__ret_pi8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _A__add_vi8_vi8
	.private_extern _A__add_vi8_vi8
_A__add_vi8_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pi8_pi8
	.private_extern _A__add_pi8_pi8
_A__add_pi8_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rdx
	movsbq	-18(%rbp),%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_vi8_vu8
	.private_extern _A__add_vi8_vu8
_A__add_vi8_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pi8_pu8
	.private_extern _A__add_pi8_pu8
_A__add_pi8_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rdx
	movzbl	-18(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_vi8_vi16
	.private_extern _A__add_vi8_vi16
_A__add_vi8_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pi8_pi16
	.private_extern _A__add_pi8_pi16
_A__add_pi8_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movswq	-20(%rbp),%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_vi8_vu16
	.private_extern _A__add_vi8_vu16
_A__add_vi8_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pi8_pu16
	.private_extern _A__add_pi8_pu16
_A__add_pi8_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_vi8_vi32
	.private_extern _A__add_vi8_vi32
_A__add_vi8_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movl	112+_MM_A(%rip), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pi8_pi32
	.private_extern _A__add_pi8_pi32
_A__add_pi8_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_vi8_vu32
	.private_extern _A__add_vi8_vu32
_A__add_vi8_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pi8_pu32
	.private_extern _A__add_pi8_pu32
_A__add_pi8_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	mov	-24(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_vi8_vi64
	.private_extern _A__add_vi8_vi64
_A__add_vi8_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	120+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pi8_pi64
	.private_extern _A__add_pi8_pi64
_A__add_pi8_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _A__add_vi8_vu64
	.private_extern _A__add_vi8_vu64
_A__add_vi8_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	128+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pi8_pu64
	.private_extern _A__add_pi8_pu64
_A__add_pi8_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _A__sub_vi8_vi8
	.private_extern _A__sub_vi8_vi8
_A__sub_vi8_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _A__sub_pi8_pi8
	.private_extern _A__sub_pi8_pi8
_A__sub_pi8_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rdx
	movsbq	-18(%rbp),%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_vi8_vu8
	.private_extern _A__sub_vi8_vu8
_A__sub_vi8_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pi8_pu8
	.private_extern _A__sub_pi8_pu8
_A__sub_pi8_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rdx
	movzbl	-18(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_vi8_vi16
	.private_extern _A__sub_vi8_vi16
_A__sub_vi8_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pi8_pi16
	.private_extern _A__sub_pi8_pi16
_A__sub_pi8_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movswq	-20(%rbp),%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_vi8_vu16
	.private_extern _A__sub_vi8_vu16
_A__sub_vi8_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pi8_pu16
	.private_extern _A__sub_pi8_pu16
_A__sub_pi8_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_vi8_vi32
	.private_extern _A__sub_vi8_vi32
_A__sub_vi8_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movl	112+_MM_A(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pi8_pi32
	.private_extern _A__sub_pi8_pi32
_A__sub_pi8_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_vi8_vu32
	.private_extern _A__sub_vi8_vu32
_A__sub_vi8_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pi8_pu32
	.private_extern _A__sub_pi8_pu32
_A__sub_pi8_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	mov	-24(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_vi8_vi64
	.private_extern _A__sub_vi8_vi64
_A__sub_vi8_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	120+_MM_A(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pi8_pi64
	.private_extern _A__sub_pi8_pi64
_A__sub_pi8_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _A__sub_vi8_vu64
	.private_extern _A__sub_vi8_vu64
_A__sub_vi8_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	128+_MM_A(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pi8_pu64
	.private_extern _A__sub_pi8_pu64
_A__sub_pi8_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _A__and_vi8_vi8
	.private_extern _A__and_vi8_vi8
_A__and_vi8_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rax
	leave
	ret
.globl _A__and_pi8_pi8
	.private_extern _A__and_pi8_pi8
_A__and_pi8_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rdx
	movsbq	-18(%rbp),%rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vi8_vu8
	.private_extern _A__and_vi8_vu8
_A__and_vi8_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pi8_pu8
	.private_extern _A__and_pi8_pu8
_A__and_pi8_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rdx
	movzbl	-18(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vi8_vi16
	.private_extern _A__and_vi8_vi16
_A__and_vi8_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pi8_pi16
	.private_extern _A__and_pi8_pi16
_A__and_pi8_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movswq	-20(%rbp),%rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vi8_vu16
	.private_extern _A__and_vi8_vu16
_A__and_vi8_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pi8_pu16
	.private_extern _A__and_pi8_pu16
_A__and_pi8_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vi8_vi32
	.private_extern _A__and_vi8_vi32
_A__and_vi8_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movl	112+_MM_A(%rip), %eax
	cltq
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pi8_pi32
	.private_extern _A__and_pi8_pi32
_A__and_pi8_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vi8_vu32
	.private_extern _A__and_vi8_vu32
_A__and_vi8_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pi8_pu32
	.private_extern _A__and_pi8_pu32
_A__and_pi8_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	mov	-24(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vi8_vi64
	.private_extern _A__and_vi8_vi64
_A__and_vi8_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	120+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pi8_pi64
	.private_extern _A__and_pi8_pi64
_A__and_pi8_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vi8_vu64
	.private_extern _A__and_vi8_vu64
_A__and_vi8_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	128+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pi8_pu64
	.private_extern _A__and_pi8_pu64
_A__and_pi8_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__or_vi8_vi8
	.private_extern _A__or_vi8_vi8
_A__or_vi8_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rax
	leave
	ret
.globl _A__or_pi8_pi8
	.private_extern _A__or_pi8_pi8
_A__or_pi8_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rdx
	movsbq	-18(%rbp),%rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vi8_vu8
	.private_extern _A__or_vi8_vu8
_A__or_vi8_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pi8_pu8
	.private_extern _A__or_pi8_pu8
_A__or_pi8_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rdx
	movzbl	-18(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vi8_vi16
	.private_extern _A__or_vi8_vi16
_A__or_vi8_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pi8_pi16
	.private_extern _A__or_pi8_pi16
_A__or_pi8_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movswq	-20(%rbp),%rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vi8_vu16
	.private_extern _A__or_vi8_vu16
_A__or_vi8_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pi8_pu16
	.private_extern _A__or_pi8_pu16
_A__or_pi8_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vi8_vi32
	.private_extern _A__or_vi8_vi32
_A__or_vi8_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movl	112+_MM_A(%rip), %eax
	cltq
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pi8_pi32
	.private_extern _A__or_pi8_pi32
_A__or_pi8_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vi8_vu32
	.private_extern _A__or_vi8_vu32
_A__or_vi8_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pi8_pu32
	.private_extern _A__or_pi8_pu32
_A__or_pi8_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	mov	-24(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vi8_vi64
	.private_extern _A__or_vi8_vi64
_A__or_vi8_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	120+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pi8_pi64
	.private_extern _A__or_pi8_pi64
_A__or_pi8_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vi8_vu64
	.private_extern _A__or_vi8_vu64
_A__or_vi8_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	128+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pi8_pu64
	.private_extern _A__or_pi8_pu64
_A__or_pi8_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__xor_vi8_vi8
	.private_extern _A__xor_vi8_vi8
_A__xor_vi8_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _A__xor_pi8_pi8
	.private_extern _A__xor_pi8_pi8
_A__xor_pi8_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rdx
	movsbq	-18(%rbp),%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vi8_vu8
	.private_extern _A__xor_vi8_vu8
_A__xor_vi8_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pi8_pu8
	.private_extern _A__xor_pi8_pu8
_A__xor_pi8_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rdx
	movzbl	-18(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vi8_vi16
	.private_extern _A__xor_vi8_vi16
_A__xor_vi8_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pi8_pi16
	.private_extern _A__xor_pi8_pi16
_A__xor_pi8_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movswq	-20(%rbp),%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vi8_vu16
	.private_extern _A__xor_vi8_vu16
_A__xor_vi8_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pi8_pu16
	.private_extern _A__xor_pi8_pu16
_A__xor_pi8_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vi8_vi32
	.private_extern _A__xor_vi8_vi32
_A__xor_vi8_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movl	112+_MM_A(%rip), %eax
	cltq
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pi8_pi32
	.private_extern _A__xor_pi8_pi32
_A__xor_pi8_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vi8_vu32
	.private_extern _A__xor_vi8_vu32
_A__xor_vi8_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pi8_pu32
	.private_extern _A__xor_pi8_pu32
_A__xor_pi8_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	mov	-24(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vi8_vi64
	.private_extern _A__xor_vi8_vi64
_A__xor_vi8_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	120+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pi8_pi64
	.private_extern _A__xor_pi8_pi64
_A__xor_pi8_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vi8_vu64
	.private_extern _A__xor_vi8_vu64
_A__xor_vi8_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	128+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pi8_pu64
	.private_extern _A__xor_pi8_pu64
_A__xor_pi8_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rdx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__mult_vi8_vi8
	.private_extern _A__mult_vi8_vi8
_A__mult_vi8_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pi8_pi8
	.private_extern _A__mult_pi8_pi8
_A__mult_pi8_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rdx
	movsbq	-18(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_vi8_vu8
	.private_extern _A__mult_vi8_vu8
_A__mult_vi8_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pi8_pu8
	.private_extern _A__mult_pi8_pu8
_A__mult_pi8_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rdx
	movzbl	-18(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_vi8_vi16
	.private_extern _A__mult_vi8_vi16
_A__mult_vi8_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pi8_pi16
	.private_extern _A__mult_pi8_pi16
_A__mult_pi8_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movswq	-20(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_vi8_vu16
	.private_extern _A__mult_vi8_vu16
_A__mult_vi8_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pi8_pu16
	.private_extern _A__mult_pi8_pu16
_A__mult_pi8_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_vi8_vi32
	.private_extern _A__mult_vi8_vi32
_A__mult_vi8_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movl	112+_MM_A(%rip), %eax
	cltq
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pi8_pi32
	.private_extern _A__mult_pi8_pi32
_A__mult_pi8_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_vi8_vu32
	.private_extern _A__mult_vi8_vu32
_A__mult_vi8_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pi8_pu32
	.private_extern _A__mult_pi8_pu32
_A__mult_pi8_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rdx
	mov	-24(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_vi8_vi64
	.private_extern _A__mult_vi8_vi64
_A__mult_vi8_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	120+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pi8_pi64
	.private_extern _A__mult_pi8_pi64
_A__mult_pi8_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _A__mult_vi8_vu64
	.private_extern _A__mult_vi8_vu64
_A__mult_vi8_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	128+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pi8_pu64
	.private_extern _A__mult_pi8_pu64
_A__mult_pi8_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _A__div_vi8_vi8
	.private_extern _A__div_vi8_vi8
_A__div_vi8_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rsi
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_pi8_pi8
	.private_extern _A__div_pi8_pi8
_A__div_pi8_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rsi
	movsbq	-18(%rbp),%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_vi8_vu8
	.private_extern _A__div_vi8_vu8
_A__div_vi8_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rsi
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_pi8_pu8
	.private_extern _A__div_pi8_pu8
_A__div_pi8_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rsi
	movzbl	-18(%rbp), %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_vi8_vi16
	.private_extern _A__div_vi8_vi16
_A__div_vi8_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rsi
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_pi8_pi16
	.private_extern _A__div_pi8_pi16
_A__div_pi8_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rsi
	movswq	-20(%rbp),%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_vi8_vu16
	.private_extern _A__div_vi8_vu16
_A__div_vi8_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rsi
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_pi8_pu16
	.private_extern _A__div_pi8_pu16
_A__div_pi8_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rsi
	movzwl	-20(%rbp), %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_vi8_vi32
	.private_extern _A__div_vi8_vi32
_A__div_vi8_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rsi
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_pi8_pi32
	.private_extern _A__div_pi8_pi32
_A__div_pi8_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rsi
	movl	-24(%rbp), %eax
	movslq	%eax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_vi8_vu32
	.private_extern _A__div_vi8_vu32
_A__div_vi8_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rsi
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_pi8_pu32
	.private_extern _A__div_pi8_pu32
_A__div_pi8_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rsi
	mov	-24(%rbp), %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_vi8_vi64
	.private_extern _A__div_vi8_vi64
_A__div_vi8_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rsi
	movq	120+_MM_A(%rip), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_pi8_pi64
	.private_extern _A__div_pi8_pi64
_A__div_pi8_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rsi
	movq	-32(%rbp), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_vi8_vu64
	.private_extern _A__div_vi8_vu64
_A__div_vi8_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rsi
	movq	128+_MM_A(%rip), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_pi8_pu64
	.private_extern _A__div_pi8_pu64
_A__div_pi8_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rsi
	movq	-32(%rbp), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__mod_vi8_vi8
	.private_extern _A__mod_vi8_vi8
_A__mod_vi8_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rsi
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pi8_pi8
	.private_extern _A__mod_pi8_pi8
_A__mod_pi8_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rsi
	movsbq	-18(%rbp),%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vi8_vu8
	.private_extern _A__mod_vi8_vu8
_A__mod_vi8_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rsi
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pi8_pu8
	.private_extern _A__mod_pi8_pu8
_A__mod_pi8_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp),%rsi
	movzbl	-18(%rbp), %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vi8_vi16
	.private_extern _A__mod_vi8_vi16
_A__mod_vi8_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rsi
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pi8_pi16
	.private_extern _A__mod_pi8_pi16
_A__mod_pi8_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rsi
	movswq	-20(%rbp),%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vi8_vu16
	.private_extern _A__mod_vi8_vu16
_A__mod_vi8_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rsi
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pi8_pu16
	.private_extern _A__mod_pi8_pu16
_A__mod_pi8_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp),%rsi
	movzwl	-20(%rbp), %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vi8_vi32
	.private_extern _A__mod_vi8_vi32
_A__mod_vi8_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rsi
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pi8_pi32
	.private_extern _A__mod_pi8_pi32
_A__mod_pi8_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rsi
	movl	-24(%rbp), %eax
	movslq	%eax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vi8_vu32
	.private_extern _A__mod_vi8_vu32
_A__mod_vi8_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rsi
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pi8_pu32
	.private_extern _A__mod_pi8_pu32
_A__mod_pi8_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp),%rsi
	mov	-24(%rbp), %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vi8_vi64
	.private_extern _A__mod_vi8_vi64
_A__mod_vi8_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rsi
	movq	120+_MM_A(%rip), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pi8_pi64
	.private_extern _A__mod_pi8_pi64
_A__mod_pi8_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rsi
	movq	-32(%rbp), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vi8_vu64
	.private_extern _A__mod_vi8_vu64
_A__mod_vi8_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rsi
	movq	128+_MM_A(%rip), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pi8_pu64
	.private_extern _A__mod_pi8_pu64
_A__mod_pi8_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp),%rsi
	movq	-32(%rbp), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__ret_vu8
	.private_extern _A__ret_vu8
_A__ret_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	leave
	ret
.globl _A__ret_ku8
	.private_extern _A__ret_ku8
_A__ret_ku8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$6, %eax
	leave
	ret
.globl _A__ret_pu8_i8
	.private_extern _A__ret_pu8_i8
_A__ret_pu8_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movsbq	-17(%rbp),%rax
	leave
	ret
.globl _A__ret_pu8_u8
	.private_extern _A__ret_pu8_u8
_A__ret_pu8_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movzbl	-17(%rbp), %eax
	leave
	ret
.globl _A__ret_pu8_i16
	.private_extern _A__ret_pu8_i16
_A__ret_pu8_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movswq	-18(%rbp),%rax
	leave
	ret
.globl _A__ret_pu8_u16
	.private_extern _A__ret_pu8_u16
_A__ret_pu8_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movzwl	-18(%rbp), %eax
	leave
	ret
.globl _A__ret_pu8_i32
	.private_extern _A__ret_pu8_i32
_A__ret_pu8_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	leave
	ret
.globl _A__ret_pu8_u32
	.private_extern _A__ret_pu8_u32
_A__ret_pu8_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	leave
	ret
.globl _A__ret_pu8_i64
	.private_extern _A__ret_pu8_i64
_A__ret_pu8_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _A__ret_pu8_u64
	.private_extern _A__ret_pu8_u64
_A__ret_pu8_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _A__add_vu8_vi8
	.private_extern _A__add_vu8_vi8
_A__add_vu8_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pu8_pi8
	.private_extern _A__add_pu8_pi8
_A__add_pu8_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %edx
	movsbq	-18(%rbp),%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_vu8_vu8
	.private_extern _A__add_vu8_vu8
_A__add_vu8_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pu8_pu8
	.private_extern _A__add_pu8_pu8
_A__add_pu8_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %edx
	movzbl	-18(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_vu8_vi16
	.private_extern _A__add_vu8_vi16
_A__add_vu8_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pu8_pi16
	.private_extern _A__add_pu8_pi16
_A__add_pu8_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movswq	-20(%rbp),%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_vu8_vu16
	.private_extern _A__add_vu8_vu16
_A__add_vu8_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pu8_pu16
	.private_extern _A__add_pu8_pu16
_A__add_pu8_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movzwl	-20(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_vu8_vi32
	.private_extern _A__add_vu8_vi32
_A__add_vu8_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movl	112+_MM_A(%rip), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pu8_pi32
	.private_extern _A__add_pu8_pi32
_A__add_pu8_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_vu8_vu32
	.private_extern _A__add_vu8_vu32
_A__add_vu8_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pu8_pu32
	.private_extern _A__add_pu8_pu32
_A__add_pu8_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	mov	-24(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_vu8_vi64
	.private_extern _A__add_vu8_vi64
_A__add_vu8_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pu8_pi64
	.private_extern _A__add_pu8_pi64
_A__add_pu8_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _A__add_vu8_vu64
	.private_extern _A__add_vu8_vu64
_A__add_vu8_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	128+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pu8_pu64
	.private_extern _A__add_pu8_pu64
_A__add_pu8_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _A__sub_vu8_vi8
	.private_extern _A__sub_vu8_vi8
_A__sub_vu8_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pu8_pi8
	.private_extern _A__sub_pu8_pi8
_A__sub_pu8_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %edx
	movsbq	-18(%rbp),%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_vu8_vu8
	.private_extern _A__sub_vu8_vu8
_A__sub_vu8_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _A__sub_pu8_pu8
	.private_extern _A__sub_pu8_pu8
_A__sub_pu8_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %edx
	movzbl	-18(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_vu8_vi16
	.private_extern _A__sub_vu8_vi16
_A__sub_vu8_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pu8_pi16
	.private_extern _A__sub_pu8_pi16
_A__sub_pu8_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movswq	-20(%rbp),%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_vu8_vu16
	.private_extern _A__sub_vu8_vu16
_A__sub_vu8_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pu8_pu16
	.private_extern _A__sub_pu8_pu16
_A__sub_pu8_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movzwl	-20(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_vu8_vi32
	.private_extern _A__sub_vu8_vi32
_A__sub_vu8_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movl	112+_MM_A(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pu8_pi32
	.private_extern _A__sub_pu8_pi32
_A__sub_pu8_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_vu8_vu32
	.private_extern _A__sub_vu8_vu32
_A__sub_vu8_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pu8_pu32
	.private_extern _A__sub_pu8_pu32
_A__sub_pu8_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	mov	-24(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_vu8_vi64
	.private_extern _A__sub_vu8_vi64
_A__sub_vu8_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_A(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pu8_pi64
	.private_extern _A__sub_pu8_pi64
_A__sub_pu8_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _A__sub_vu8_vu64
	.private_extern _A__sub_vu8_vu64
_A__sub_vu8_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	128+_MM_A(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pu8_pu64
	.private_extern _A__sub_pu8_pu64
_A__sub_pu8_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _A__and_vu8_vi8
	.private_extern _A__and_vu8_vi8
_A__and_vu8_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pu8_pi8
	.private_extern _A__and_pu8_pi8
_A__and_pu8_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %edx
	movsbq	-18(%rbp),%rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vu8_vu8
	.private_extern _A__and_vu8_vu8
_A__and_vu8_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	leave
	ret
.globl _A__and_pu8_pu8
	.private_extern _A__and_pu8_pu8
_A__and_pu8_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %edx
	movzbl	-18(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vu8_vi16
	.private_extern _A__and_vu8_vi16
_A__and_vu8_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pu8_pi16
	.private_extern _A__and_pu8_pi16
_A__and_pu8_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movswq	-20(%rbp),%rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vu8_vu16
	.private_extern _A__and_vu8_vu16
_A__and_vu8_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pu8_pu16
	.private_extern _A__and_pu8_pu16
_A__and_pu8_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movzwl	-20(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vu8_vi32
	.private_extern _A__and_vu8_vi32
_A__and_vu8_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movl	112+_MM_A(%rip), %eax
	cltq
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pu8_pi32
	.private_extern _A__and_pu8_pi32
_A__and_pu8_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vu8_vu32
	.private_extern _A__and_vu8_vu32
_A__and_vu8_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pu8_pu32
	.private_extern _A__and_pu8_pu32
_A__and_pu8_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	mov	-24(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vu8_vi64
	.private_extern _A__and_vu8_vi64
_A__and_vu8_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pu8_pi64
	.private_extern _A__and_pu8_pi64
_A__and_pu8_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vu8_vu64
	.private_extern _A__and_vu8_vu64
_A__and_vu8_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	128+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pu8_pu64
	.private_extern _A__and_pu8_pu64
_A__and_pu8_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__or_vu8_vi8
	.private_extern _A__or_vu8_vi8
_A__or_vu8_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pu8_pi8
	.private_extern _A__or_pu8_pi8
_A__or_pu8_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %edx
	movsbq	-18(%rbp),%rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vu8_vu8
	.private_extern _A__or_vu8_vu8
_A__or_vu8_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	leave
	ret
.globl _A__or_pu8_pu8
	.private_extern _A__or_pu8_pu8
_A__or_pu8_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %edx
	movzbl	-18(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vu8_vi16
	.private_extern _A__or_vu8_vi16
_A__or_vu8_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pu8_pi16
	.private_extern _A__or_pu8_pi16
_A__or_pu8_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movswq	-20(%rbp),%rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vu8_vu16
	.private_extern _A__or_vu8_vu16
_A__or_vu8_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pu8_pu16
	.private_extern _A__or_pu8_pu16
_A__or_pu8_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movzwl	-20(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vu8_vi32
	.private_extern _A__or_vu8_vi32
_A__or_vu8_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movl	112+_MM_A(%rip), %eax
	cltq
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pu8_pi32
	.private_extern _A__or_pu8_pi32
_A__or_pu8_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vu8_vu32
	.private_extern _A__or_vu8_vu32
_A__or_vu8_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pu8_pu32
	.private_extern _A__or_pu8_pu32
_A__or_pu8_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	mov	-24(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vu8_vi64
	.private_extern _A__or_vu8_vi64
_A__or_vu8_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pu8_pi64
	.private_extern _A__or_pu8_pi64
_A__or_pu8_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vu8_vu64
	.private_extern _A__or_vu8_vu64
_A__or_vu8_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	128+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pu8_pu64
	.private_extern _A__or_pu8_pu64
_A__or_pu8_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__xor_vu8_vi8
	.private_extern _A__xor_vu8_vi8
_A__xor_vu8_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pu8_pi8
	.private_extern _A__xor_pu8_pi8
_A__xor_pu8_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %edx
	movsbq	-18(%rbp),%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vu8_vu8
	.private_extern _A__xor_vu8_vu8
_A__xor_vu8_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _A__xor_pu8_pu8
	.private_extern _A__xor_pu8_pu8
_A__xor_pu8_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %edx
	movzbl	-18(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vu8_vi16
	.private_extern _A__xor_vu8_vi16
_A__xor_vu8_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pu8_pi16
	.private_extern _A__xor_pu8_pi16
_A__xor_pu8_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movswq	-20(%rbp),%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vu8_vu16
	.private_extern _A__xor_vu8_vu16
_A__xor_vu8_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pu8_pu16
	.private_extern _A__xor_pu8_pu16
_A__xor_pu8_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movzwl	-20(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vu8_vi32
	.private_extern _A__xor_vu8_vi32
_A__xor_vu8_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movl	112+_MM_A(%rip), %eax
	cltq
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pu8_pi32
	.private_extern _A__xor_pu8_pi32
_A__xor_pu8_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vu8_vu32
	.private_extern _A__xor_vu8_vu32
_A__xor_vu8_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pu8_pu32
	.private_extern _A__xor_pu8_pu32
_A__xor_pu8_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	mov	-24(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vu8_vi64
	.private_extern _A__xor_vu8_vi64
_A__xor_vu8_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pu8_pi64
	.private_extern _A__xor_pu8_pi64
_A__xor_pu8_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vu8_vu64
	.private_extern _A__xor_vu8_vu64
_A__xor_vu8_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	128+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pu8_pu64
	.private_extern _A__xor_pu8_pu64
_A__xor_pu8_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %edx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__mult_vu8_vi8
	.private_extern _A__mult_vu8_vi8
_A__mult_vu8_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pu8_pi8
	.private_extern _A__mult_pu8_pi8
_A__mult_pu8_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %edx
	movsbq	-18(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_vu8_vu8
	.private_extern _A__mult_vu8_vu8
_A__mult_vu8_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pu8_pu8
	.private_extern _A__mult_pu8_pu8
_A__mult_pu8_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %edx
	movzbl	-18(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_vu8_vi16
	.private_extern _A__mult_vu8_vi16
_A__mult_vu8_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pu8_pi16
	.private_extern _A__mult_pu8_pi16
_A__mult_pu8_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movswq	-20(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_vu8_vu16
	.private_extern _A__mult_vu8_vu16
_A__mult_vu8_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pu8_pu16
	.private_extern _A__mult_pu8_pu16
_A__mult_pu8_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %edx
	movzwl	-20(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_vu8_vi32
	.private_extern _A__mult_vu8_vi32
_A__mult_vu8_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movl	112+_MM_A(%rip), %eax
	cltq
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pu8_pi32
	.private_extern _A__mult_pu8_pi32
_A__mult_pu8_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_vu8_vu32
	.private_extern _A__mult_vu8_vu32
_A__mult_vu8_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pu8_pu32
	.private_extern _A__mult_pu8_pu32
_A__mult_pu8_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %edx
	mov	-24(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_vu8_vi64
	.private_extern _A__mult_vu8_vi64
_A__mult_vu8_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pu8_pi64
	.private_extern _A__mult_pu8_pi64
_A__mult_pu8_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _A__mult_vu8_vu64
	.private_extern _A__mult_vu8_vu64
_A__mult_vu8_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	128+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pu8_pu64
	.private_extern _A__mult_pu8_pu64
_A__mult_pu8_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %eax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _A__div_vu8_vi8
	.private_extern _A__div_vu8_vi8
_A__div_vu8_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %esi
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_pu8_pi8
	.private_extern _A__div_pu8_pi8
_A__div_pu8_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %esi
	movsbq	-18(%rbp),%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_vu8_vu8
	.private_extern _A__div_vu8_vu8
_A__div_vu8_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -40(%rbp)
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	movq	-40(%rbp), %rdx
	movq	%rax, %rcx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	%rcx
	leave
	ret
.globl _A__div_pu8_pu8
	.private_extern _A__div_pu8_pu8
_A__div_pu8_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %eax
	movq	%rax, -40(%rbp)
	movzbl	-18(%rbp), %edx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L496
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L496
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L496:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _A__div_vu8_vi16
	.private_extern _A__div_vu8_vi16
_A__div_vu8_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %esi
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_pu8_pi16
	.private_extern _A__div_pu8_pi16
_A__div_pu8_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %esi
	movswq	-20(%rbp),%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_vu8_vu16
	.private_extern _A__div_vu8_vu16
_A__div_vu8_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L503
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L503
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L503:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _A__div_pu8_pu16
	.private_extern _A__div_pu8_pu16
_A__div_pu8_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %eax
	movq	%rax, -40(%rbp)
	movzwl	-20(%rbp), %edx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L506
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L506
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L506:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _A__div_vu8_vi32
	.private_extern _A__div_vu8_vi32
_A__div_vu8_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %esi
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_pu8_pi32
	.private_extern _A__div_pu8_pi32
_A__div_pu8_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %esi
	movl	-24(%rbp), %eax
	movslq	%eax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_vu8_vu32
	.private_extern _A__div_vu8_vu32
_A__div_vu8_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L513
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L513
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L513:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _A__div_pu8_pu32
	.private_extern _A__div_pu8_pu32
_A__div_pu8_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %eax
	movq	%rax, -40(%rbp)
	mov	-24(%rbp), %edx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L516
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L516
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L516:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _A__div_vu8_vi64
	.private_extern _A__div_vu8_vi64
_A__div_vu8_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %esi
	movq	120+_MM_A(%rip), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_pu8_pi64
	.private_extern _A__div_pu8_pi64
_A__div_pu8_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %esi
	movq	-32(%rbp), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_vu8_vu64
	.private_extern _A__div_vu8_vu64
_A__div_vu8_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %esi
	movq	128+_MM_A(%rip), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_pu8_pu64
	.private_extern _A__div_pu8_pu64
_A__div_pu8_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %esi
	movq	-32(%rbp), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__mod_vu8_vi8
	.private_extern _A__mod_vu8_vi8
_A__mod_vu8_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %esi
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pu8_pi8
	.private_extern _A__mod_pu8_pi8
_A__mod_pu8_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %esi
	movsbq	-18(%rbp),%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vu8_vu8
	.private_extern _A__mod_vu8_vu8
_A__mod_vu8_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L531
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L531
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L531:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _A__mod_pu8_pu8
	.private_extern _A__mod_pu8_pu8
_A__mod_pu8_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %eax
	movq	%rax, -40(%rbp)
	movzbl	-18(%rbp), %edx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L534
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L534
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L534:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _A__mod_vu8_vi16
	.private_extern _A__mod_vu8_vi16
_A__mod_vu8_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %esi
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pu8_pi16
	.private_extern _A__mod_pu8_pi16
_A__mod_pu8_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %esi
	movswq	-20(%rbp),%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vu8_vu16
	.private_extern _A__mod_vu8_vu16
_A__mod_vu8_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L541
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L541
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L541:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _A__mod_pu8_pu16
	.private_extern _A__mod_pu8_pu16
_A__mod_pu8_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %eax
	movq	%rax, -40(%rbp)
	movzwl	-20(%rbp), %edx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L544
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L544
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L544:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _A__mod_vu8_vi32
	.private_extern _A__mod_vu8_vi32
_A__mod_vu8_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %esi
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pu8_pi32
	.private_extern _A__mod_pu8_pi32
_A__mod_pu8_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %esi
	movl	-24(%rbp), %eax
	movslq	%eax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vu8_vu32
	.private_extern _A__mod_vu8_vu32
_A__mod_vu8_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -32(%rbp)
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L551
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L551
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L551:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _A__mod_pu8_pu32
	.private_extern _A__mod_pu8_pu32
_A__mod_pu8_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %eax
	movq	%rax, -40(%rbp)
	mov	-24(%rbp), %edx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L554
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L554
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L554:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _A__mod_vu8_vi64
	.private_extern _A__mod_vu8_vi64
_A__mod_vu8_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %esi
	movq	120+_MM_A(%rip), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pu8_pi64
	.private_extern _A__mod_pu8_pi64
_A__mod_pu8_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %esi
	movq	-32(%rbp), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vu8_vu64
	.private_extern _A__mod_vu8_vu64
_A__mod_vu8_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %esi
	movq	128+_MM_A(%rip), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pu8_pu64
	.private_extern _A__mod_pu8_pu64
_A__mod_pu8_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %esi
	movq	-32(%rbp), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__ret_vi16
	.private_extern _A__ret_vi16
_A__ret_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rax
	leave
	ret
.globl _A__ret_ki16
	.private_extern _A__ret_ki16
_A__ret_ki16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$9, %eax
	leave
	ret
.globl _A__ret_pi16_i8
	.private_extern _A__ret_pi16_i8
_A__ret_pi16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movsbq	-17(%rbp),%rax
	leave
	ret
.globl _A__ret_pi16_u8
	.private_extern _A__ret_pi16_u8
_A__ret_pi16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movzbl	-17(%rbp), %eax
	leave
	ret
.globl _A__ret_pi16_i16
	.private_extern _A__ret_pi16_i16
_A__ret_pi16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movswq	-18(%rbp),%rax
	leave
	ret
.globl _A__ret_pi16_u16
	.private_extern _A__ret_pi16_u16
_A__ret_pi16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movzwl	-18(%rbp), %eax
	leave
	ret
.globl _A__ret_pi16_i32
	.private_extern _A__ret_pi16_i32
_A__ret_pi16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	leave
	ret
.globl _A__ret_pi16_u32
	.private_extern _A__ret_pi16_u32
_A__ret_pi16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	leave
	ret
.globl _A__ret_pi16_i64
	.private_extern _A__ret_pi16_i64
_A__ret_pi16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _A__ret_pi16_u64
	.private_extern _A__ret_pi16_u64
_A__ret_pi16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _A__add_vi16_vi8
	.private_extern _A__add_vi16_vi8
_A__add_vi16_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pi16_pi8
	.private_extern _A__add_pi16_pi8
_A__add_pi16_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rdx
	movsbq	-19(%rbp),%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_vi16_vu8
	.private_extern _A__add_vi16_vu8
_A__add_vi16_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pi16_pu8
	.private_extern _A__add_pi16_pu8
_A__add_pi16_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rdx
	movzbl	-19(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_vi16_vi16
	.private_extern _A__add_vi16_vi16
_A__add_vi16_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pi16_pi16
	.private_extern _A__add_pi16_pi16
_A__add_pi16_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rdx
	movswq	-20(%rbp),%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_vi16_vu16
	.private_extern _A__add_vi16_vu16
_A__add_vi16_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pi16_pu16
	.private_extern _A__add_pi16_pu16
_A__add_pi16_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_vi16_vi32
	.private_extern _A__add_vi16_vi32
_A__add_vi16_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movl	112+_MM_A(%rip), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pi16_pi32
	.private_extern _A__add_pi16_pi32
_A__add_pi16_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_vi16_vu32
	.private_extern _A__add_vi16_vu32
_A__add_vi16_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pi16_pu32
	.private_extern _A__add_pi16_pu32
_A__add_pi16_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	mov	-24(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_vi16_vi64
	.private_extern _A__add_vi16_vi64
_A__add_vi16_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	120+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pi16_pi64
	.private_extern _A__add_pi16_pi64
_A__add_pi16_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _A__add_vi16_vu64
	.private_extern _A__add_vi16_vu64
_A__add_vi16_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	128+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pi16_pu64
	.private_extern _A__add_pi16_pu64
_A__add_pi16_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _A__sub_vi16_vi8
	.private_extern _A__sub_vi16_vi8
_A__sub_vi16_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pi16_pi8
	.private_extern _A__sub_pi16_pi8
_A__sub_pi16_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rdx
	movsbq	-19(%rbp),%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_vi16_vu8
	.private_extern _A__sub_vi16_vu8
_A__sub_vi16_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pi16_pu8
	.private_extern _A__sub_pi16_pu8
_A__sub_pi16_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rdx
	movzbl	-19(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_vi16_vi16
	.private_extern _A__sub_vi16_vi16
_A__sub_vi16_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _A__sub_pi16_pi16
	.private_extern _A__sub_pi16_pi16
_A__sub_pi16_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rdx
	movswq	-20(%rbp),%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_vi16_vu16
	.private_extern _A__sub_vi16_vu16
_A__sub_vi16_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pi16_pu16
	.private_extern _A__sub_pi16_pu16
_A__sub_pi16_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_vi16_vi32
	.private_extern _A__sub_vi16_vi32
_A__sub_vi16_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movl	112+_MM_A(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pi16_pi32
	.private_extern _A__sub_pi16_pi32
_A__sub_pi16_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_vi16_vu32
	.private_extern _A__sub_vi16_vu32
_A__sub_vi16_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pi16_pu32
	.private_extern _A__sub_pi16_pu32
_A__sub_pi16_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	mov	-24(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_vi16_vi64
	.private_extern _A__sub_vi16_vi64
_A__sub_vi16_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	120+_MM_A(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pi16_pi64
	.private_extern _A__sub_pi16_pi64
_A__sub_pi16_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _A__sub_vi16_vu64
	.private_extern _A__sub_vi16_vu64
_A__sub_vi16_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	128+_MM_A(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pi16_pu64
	.private_extern _A__sub_pi16_pu64
_A__sub_pi16_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _A__and_vi16_vi8
	.private_extern _A__and_vi16_vi8
_A__and_vi16_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pi16_pi8
	.private_extern _A__and_pi16_pi8
_A__and_pi16_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rdx
	movsbq	-19(%rbp),%rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vi16_vu8
	.private_extern _A__and_vi16_vu8
_A__and_vi16_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pi16_pu8
	.private_extern _A__and_pi16_pu8
_A__and_pi16_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rdx
	movzbl	-19(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vi16_vi16
	.private_extern _A__and_vi16_vi16
_A__and_vi16_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rax
	leave
	ret
.globl _A__and_pi16_pi16
	.private_extern _A__and_pi16_pi16
_A__and_pi16_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rdx
	movswq	-20(%rbp),%rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vi16_vu16
	.private_extern _A__and_vi16_vu16
_A__and_vi16_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pi16_pu16
	.private_extern _A__and_pi16_pu16
_A__and_pi16_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vi16_vi32
	.private_extern _A__and_vi16_vi32
_A__and_vi16_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movl	112+_MM_A(%rip), %eax
	cltq
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pi16_pi32
	.private_extern _A__and_pi16_pi32
_A__and_pi16_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vi16_vu32
	.private_extern _A__and_vi16_vu32
_A__and_vi16_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pi16_pu32
	.private_extern _A__and_pi16_pu32
_A__and_pi16_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	mov	-24(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vi16_vi64
	.private_extern _A__and_vi16_vi64
_A__and_vi16_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	120+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pi16_pi64
	.private_extern _A__and_pi16_pi64
_A__and_pi16_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vi16_vu64
	.private_extern _A__and_vi16_vu64
_A__and_vi16_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	128+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pi16_pu64
	.private_extern _A__and_pi16_pu64
_A__and_pi16_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__or_vi16_vi8
	.private_extern _A__or_vi16_vi8
_A__or_vi16_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pi16_pi8
	.private_extern _A__or_pi16_pi8
_A__or_pi16_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rdx
	movsbq	-19(%rbp),%rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vi16_vu8
	.private_extern _A__or_vi16_vu8
_A__or_vi16_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pi16_pu8
	.private_extern _A__or_pi16_pu8
_A__or_pi16_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rdx
	movzbl	-19(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vi16_vi16
	.private_extern _A__or_vi16_vi16
_A__or_vi16_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rax
	leave
	ret
.globl _A__or_pi16_pi16
	.private_extern _A__or_pi16_pi16
_A__or_pi16_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rdx
	movswq	-20(%rbp),%rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vi16_vu16
	.private_extern _A__or_vi16_vu16
_A__or_vi16_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pi16_pu16
	.private_extern _A__or_pi16_pu16
_A__or_pi16_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vi16_vi32
	.private_extern _A__or_vi16_vi32
_A__or_vi16_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movl	112+_MM_A(%rip), %eax
	cltq
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pi16_pi32
	.private_extern _A__or_pi16_pi32
_A__or_pi16_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vi16_vu32
	.private_extern _A__or_vi16_vu32
_A__or_vi16_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pi16_pu32
	.private_extern _A__or_pi16_pu32
_A__or_pi16_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	mov	-24(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vi16_vi64
	.private_extern _A__or_vi16_vi64
_A__or_vi16_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	120+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pi16_pi64
	.private_extern _A__or_pi16_pi64
_A__or_pi16_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vi16_vu64
	.private_extern _A__or_vi16_vu64
_A__or_vi16_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	128+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pi16_pu64
	.private_extern _A__or_pi16_pu64
_A__or_pi16_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__xor_vi16_vi8
	.private_extern _A__xor_vi16_vi8
_A__xor_vi16_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pi16_pi8
	.private_extern _A__xor_pi16_pi8
_A__xor_pi16_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rdx
	movsbq	-19(%rbp),%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vi16_vu8
	.private_extern _A__xor_vi16_vu8
_A__xor_vi16_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pi16_pu8
	.private_extern _A__xor_pi16_pu8
_A__xor_pi16_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rdx
	movzbl	-19(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vi16_vi16
	.private_extern _A__xor_vi16_vi16
_A__xor_vi16_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _A__xor_pi16_pi16
	.private_extern _A__xor_pi16_pi16
_A__xor_pi16_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rdx
	movswq	-20(%rbp),%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vi16_vu16
	.private_extern _A__xor_vi16_vu16
_A__xor_vi16_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pi16_pu16
	.private_extern _A__xor_pi16_pu16
_A__xor_pi16_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vi16_vi32
	.private_extern _A__xor_vi16_vi32
_A__xor_vi16_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movl	112+_MM_A(%rip), %eax
	cltq
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pi16_pi32
	.private_extern _A__xor_pi16_pi32
_A__xor_pi16_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vi16_vu32
	.private_extern _A__xor_vi16_vu32
_A__xor_vi16_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pi16_pu32
	.private_extern _A__xor_pi16_pu32
_A__xor_pi16_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	mov	-24(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vi16_vi64
	.private_extern _A__xor_vi16_vi64
_A__xor_vi16_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	120+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pi16_pi64
	.private_extern _A__xor_pi16_pi64
_A__xor_pi16_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vi16_vu64
	.private_extern _A__xor_vi16_vu64
_A__xor_vi16_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	128+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pi16_pu64
	.private_extern _A__xor_pi16_pu64
_A__xor_pi16_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rdx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__mult_vi16_vi8
	.private_extern _A__mult_vi16_vi8
_A__mult_vi16_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pi16_pi8
	.private_extern _A__mult_pi16_pi8
_A__mult_pi16_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rdx
	movsbq	-19(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_vi16_vu8
	.private_extern _A__mult_vi16_vu8
_A__mult_vi16_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pi16_pu8
	.private_extern _A__mult_pi16_pu8
_A__mult_pi16_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rdx
	movzbl	-19(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_vi16_vi16
	.private_extern _A__mult_vi16_vi16
_A__mult_vi16_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pi16_pi16
	.private_extern _A__mult_pi16_pi16
_A__mult_pi16_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rdx
	movswq	-20(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_vi16_vu16
	.private_extern _A__mult_vi16_vu16
_A__mult_vi16_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pi16_pu16
	.private_extern _A__mult_pi16_pu16
_A__mult_pi16_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rdx
	movzwl	-20(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_vi16_vi32
	.private_extern _A__mult_vi16_vi32
_A__mult_vi16_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movl	112+_MM_A(%rip), %eax
	cltq
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pi16_pi32
	.private_extern _A__mult_pi16_pi32
_A__mult_pi16_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	movl	-24(%rbp), %eax
	cltq
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_vi16_vu32
	.private_extern _A__mult_vi16_vu32
_A__mult_vi16_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pi16_pu32
	.private_extern _A__mult_pi16_pu32
_A__mult_pi16_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rdx
	mov	-24(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_vi16_vi64
	.private_extern _A__mult_vi16_vi64
_A__mult_vi16_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	120+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pi16_pi64
	.private_extern _A__mult_pi16_pi64
_A__mult_pi16_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _A__mult_vi16_vu64
	.private_extern _A__mult_vi16_vu64
_A__mult_vi16_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	128+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pi16_pu64
	.private_extern _A__mult_pi16_pu64
_A__mult_pi16_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _A__div_vi16_vi8
	.private_extern _A__div_vi16_vi8
_A__div_vi16_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rsi
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_pi16_pi8
	.private_extern _A__div_pi16_pi8
_A__div_pi16_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rsi
	movsbq	-19(%rbp),%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_vi16_vu8
	.private_extern _A__div_vi16_vu8
_A__div_vi16_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rsi
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_pi16_pu8
	.private_extern _A__div_pi16_pu8
_A__div_pi16_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rsi
	movzbl	-19(%rbp), %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_vi16_vi16
	.private_extern _A__div_vi16_vi16
_A__div_vi16_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rsi
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_pi16_pi16
	.private_extern _A__div_pi16_pi16
_A__div_pi16_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rsi
	movswq	-20(%rbp),%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_vi16_vu16
	.private_extern _A__div_vi16_vu16
_A__div_vi16_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rsi
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_pi16_pu16
	.private_extern _A__div_pi16_pu16
_A__div_pi16_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rsi
	movzwl	-20(%rbp), %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_vi16_vi32
	.private_extern _A__div_vi16_vi32
_A__div_vi16_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rsi
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_pi16_pi32
	.private_extern _A__div_pi16_pi32
_A__div_pi16_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rsi
	movl	-24(%rbp), %eax
	movslq	%eax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_vi16_vu32
	.private_extern _A__div_vi16_vu32
_A__div_vi16_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rsi
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_pi16_pu32
	.private_extern _A__div_pi16_pu32
_A__div_pi16_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rsi
	mov	-24(%rbp), %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_vi16_vi64
	.private_extern _A__div_vi16_vi64
_A__div_vi16_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rsi
	movq	120+_MM_A(%rip), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_pi16_pi64
	.private_extern _A__div_pi16_pi64
_A__div_pi16_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rsi
	movq	-32(%rbp), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_vi16_vu64
	.private_extern _A__div_vi16_vu64
_A__div_vi16_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rsi
	movq	128+_MM_A(%rip), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_pi16_pu64
	.private_extern _A__div_pi16_pu64
_A__div_pi16_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rsi
	movq	-32(%rbp), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__mod_vi16_vi8
	.private_extern _A__mod_vi16_vi8
_A__mod_vi16_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rsi
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pi16_pi8
	.private_extern _A__mod_pi16_pi8
_A__mod_pi16_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rsi
	movsbq	-19(%rbp),%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vi16_vu8
	.private_extern _A__mod_vi16_vu8
_A__mod_vi16_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rsi
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pi16_pu8
	.private_extern _A__mod_pi16_pu8
_A__mod_pi16_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp),%rsi
	movzbl	-19(%rbp), %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vi16_vi16
	.private_extern _A__mod_vi16_vi16
_A__mod_vi16_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rsi
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pi16_pi16
	.private_extern _A__mod_pi16_pi16
_A__mod_pi16_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rsi
	movswq	-20(%rbp),%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vi16_vu16
	.private_extern _A__mod_vi16_vu16
_A__mod_vi16_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rsi
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pi16_pu16
	.private_extern _A__mod_pi16_pu16
_A__mod_pi16_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp),%rsi
	movzwl	-20(%rbp), %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vi16_vi32
	.private_extern _A__mod_vi16_vi32
_A__mod_vi16_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rsi
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pi16_pi32
	.private_extern _A__mod_pi16_pi32
_A__mod_pi16_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rsi
	movl	-24(%rbp), %eax
	movslq	%eax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vi16_vu32
	.private_extern _A__mod_vi16_vu32
_A__mod_vi16_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rsi
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pi16_pu32
	.private_extern _A__mod_pi16_pu32
_A__mod_pi16_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp),%rsi
	mov	-24(%rbp), %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vi16_vi64
	.private_extern _A__mod_vi16_vi64
_A__mod_vi16_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rsi
	movq	120+_MM_A(%rip), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pi16_pi64
	.private_extern _A__mod_pi16_pi64
_A__mod_pi16_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rsi
	movq	-32(%rbp), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vi16_vu64
	.private_extern _A__mod_vi16_vu64
_A__mod_vi16_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rsi
	movq	128+_MM_A(%rip), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pi16_pu64
	.private_extern _A__mod_pi16_pu64
_A__mod_pi16_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp),%rsi
	movq	-32(%rbp), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__ret_vu16
	.private_extern _A__ret_vu16
_A__ret_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	leave
	ret
.globl _A__ret_ku16
	.private_extern _A__ret_ku16
_A__ret_ku16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$12, %eax
	leave
	ret
.globl _A__ret_pu16_i8
	.private_extern _A__ret_pu16_i8
_A__ret_pu16_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movsbq	-17(%rbp),%rax
	leave
	ret
.globl _A__ret_pu16_u8
	.private_extern _A__ret_pu16_u8
_A__ret_pu16_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movzbl	-17(%rbp), %eax
	leave
	ret
.globl _A__ret_pu16_i16
	.private_extern _A__ret_pu16_i16
_A__ret_pu16_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movswq	-18(%rbp),%rax
	leave
	ret
.globl _A__ret_pu16_u16
	.private_extern _A__ret_pu16_u16
_A__ret_pu16_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movzwl	-18(%rbp), %eax
	leave
	ret
.globl _A__ret_pu16_i32
	.private_extern _A__ret_pu16_i32
_A__ret_pu16_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	leave
	ret
.globl _A__ret_pu16_u32
	.private_extern _A__ret_pu16_u32
_A__ret_pu16_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	leave
	ret
.globl _A__ret_pu16_i64
	.private_extern _A__ret_pu16_i64
_A__ret_pu16_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _A__ret_pu16_u64
	.private_extern _A__ret_pu16_u64
_A__ret_pu16_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _A__add_vu16_vi8
	.private_extern _A__add_vu16_vi8
_A__add_vu16_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pu16_pi8
	.private_extern _A__add_pu16_pi8
_A__add_pu16_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %edx
	movsbq	-19(%rbp),%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_vu16_vu8
	.private_extern _A__add_vu16_vu8
_A__add_vu16_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pu16_pu8
	.private_extern _A__add_pu16_pu8
_A__add_pu16_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %edx
	movzbl	-19(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_vu16_vi16
	.private_extern _A__add_vu16_vi16
_A__add_vu16_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pu16_pi16
	.private_extern _A__add_pu16_pi16
_A__add_pu16_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %edx
	movswq	-20(%rbp),%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_vu16_vu16
	.private_extern _A__add_vu16_vu16
_A__add_vu16_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pu16_pu16
	.private_extern _A__add_pu16_pu16
_A__add_pu16_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %edx
	movzwl	-20(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_vu16_vi32
	.private_extern _A__add_vu16_vi32
_A__add_vu16_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movl	112+_MM_A(%rip), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pu16_pi32
	.private_extern _A__add_pu16_pi32
_A__add_pu16_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_vu16_vu32
	.private_extern _A__add_vu16_vu32
_A__add_vu16_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pu16_pu32
	.private_extern _A__add_pu16_pu32
_A__add_pu16_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	mov	-24(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_vu16_vi64
	.private_extern _A__add_vu16_vi64
_A__add_vu16_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pu16_pi64
	.private_extern _A__add_pu16_pi64
_A__add_pu16_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _A__add_vu16_vu64
	.private_extern _A__add_vu16_vu64
_A__add_vu16_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	128+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pu16_pu64
	.private_extern _A__add_pu16_pu64
_A__add_pu16_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _A__sub_vu16_vi8
	.private_extern _A__sub_vu16_vi8
_A__sub_vu16_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pu16_pi8
	.private_extern _A__sub_pu16_pi8
_A__sub_pu16_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %edx
	movsbq	-19(%rbp),%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_vu16_vu8
	.private_extern _A__sub_vu16_vu8
_A__sub_vu16_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pu16_pu8
	.private_extern _A__sub_pu16_pu8
_A__sub_pu16_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %edx
	movzbl	-19(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_vu16_vi16
	.private_extern _A__sub_vu16_vi16
_A__sub_vu16_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pu16_pi16
	.private_extern _A__sub_pu16_pi16
_A__sub_pu16_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %edx
	movswq	-20(%rbp),%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_vu16_vu16
	.private_extern _A__sub_vu16_vu16
_A__sub_vu16_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _A__sub_pu16_pu16
	.private_extern _A__sub_pu16_pu16
_A__sub_pu16_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %edx
	movzwl	-20(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_vu16_vi32
	.private_extern _A__sub_vu16_vi32
_A__sub_vu16_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movl	112+_MM_A(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pu16_pi32
	.private_extern _A__sub_pu16_pi32
_A__sub_pu16_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_vu16_vu32
	.private_extern _A__sub_vu16_vu32
_A__sub_vu16_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pu16_pu32
	.private_extern _A__sub_pu16_pu32
_A__sub_pu16_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	mov	-24(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_vu16_vi64
	.private_extern _A__sub_vu16_vi64
_A__sub_vu16_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_A(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pu16_pi64
	.private_extern _A__sub_pu16_pi64
_A__sub_pu16_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _A__sub_vu16_vu64
	.private_extern _A__sub_vu16_vu64
_A__sub_vu16_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	128+_MM_A(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pu16_pu64
	.private_extern _A__sub_pu16_pu64
_A__sub_pu16_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _A__and_vu16_vi8
	.private_extern _A__and_vu16_vi8
_A__and_vu16_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pu16_pi8
	.private_extern _A__and_pu16_pi8
_A__and_pu16_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %edx
	movsbq	-19(%rbp),%rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vu16_vu8
	.private_extern _A__and_vu16_vu8
_A__and_vu16_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pu16_pu8
	.private_extern _A__and_pu16_pu8
_A__and_pu16_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %edx
	movzbl	-19(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vu16_vi16
	.private_extern _A__and_vu16_vi16
_A__and_vu16_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pu16_pi16
	.private_extern _A__and_pu16_pi16
_A__and_pu16_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %edx
	movswq	-20(%rbp),%rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vu16_vu16
	.private_extern _A__and_vu16_vu16
_A__and_vu16_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	leave
	ret
.globl _A__and_pu16_pu16
	.private_extern _A__and_pu16_pu16
_A__and_pu16_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %edx
	movzwl	-20(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vu16_vi32
	.private_extern _A__and_vu16_vi32
_A__and_vu16_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movl	112+_MM_A(%rip), %eax
	cltq
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pu16_pi32
	.private_extern _A__and_pu16_pi32
_A__and_pu16_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vu16_vu32
	.private_extern _A__and_vu16_vu32
_A__and_vu16_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pu16_pu32
	.private_extern _A__and_pu16_pu32
_A__and_pu16_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	mov	-24(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vu16_vi64
	.private_extern _A__and_vu16_vi64
_A__and_vu16_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pu16_pi64
	.private_extern _A__and_pu16_pi64
_A__and_pu16_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vu16_vu64
	.private_extern _A__and_vu16_vu64
_A__and_vu16_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	128+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pu16_pu64
	.private_extern _A__and_pu16_pu64
_A__and_pu16_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__or_vu16_vi8
	.private_extern _A__or_vu16_vi8
_A__or_vu16_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pu16_pi8
	.private_extern _A__or_pu16_pi8
_A__or_pu16_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %edx
	movsbq	-19(%rbp),%rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vu16_vu8
	.private_extern _A__or_vu16_vu8
_A__or_vu16_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pu16_pu8
	.private_extern _A__or_pu16_pu8
_A__or_pu16_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %edx
	movzbl	-19(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vu16_vi16
	.private_extern _A__or_vu16_vi16
_A__or_vu16_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pu16_pi16
	.private_extern _A__or_pu16_pi16
_A__or_pu16_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %edx
	movswq	-20(%rbp),%rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vu16_vu16
	.private_extern _A__or_vu16_vu16
_A__or_vu16_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	leave
	ret
.globl _A__or_pu16_pu16
	.private_extern _A__or_pu16_pu16
_A__or_pu16_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %edx
	movzwl	-20(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vu16_vi32
	.private_extern _A__or_vu16_vi32
_A__or_vu16_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movl	112+_MM_A(%rip), %eax
	cltq
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pu16_pi32
	.private_extern _A__or_pu16_pi32
_A__or_pu16_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vu16_vu32
	.private_extern _A__or_vu16_vu32
_A__or_vu16_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pu16_pu32
	.private_extern _A__or_pu16_pu32
_A__or_pu16_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	mov	-24(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vu16_vi64
	.private_extern _A__or_vu16_vi64
_A__or_vu16_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pu16_pi64
	.private_extern _A__or_pu16_pi64
_A__or_pu16_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vu16_vu64
	.private_extern _A__or_vu16_vu64
_A__or_vu16_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	128+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pu16_pu64
	.private_extern _A__or_pu16_pu64
_A__or_pu16_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__xor_vu16_vi8
	.private_extern _A__xor_vu16_vi8
_A__xor_vu16_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pu16_pi8
	.private_extern _A__xor_pu16_pi8
_A__xor_pu16_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %edx
	movsbq	-19(%rbp),%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vu16_vu8
	.private_extern _A__xor_vu16_vu8
_A__xor_vu16_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pu16_pu8
	.private_extern _A__xor_pu16_pu8
_A__xor_pu16_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %edx
	movzbl	-19(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vu16_vi16
	.private_extern _A__xor_vu16_vi16
_A__xor_vu16_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pu16_pi16
	.private_extern _A__xor_pu16_pi16
_A__xor_pu16_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %edx
	movswq	-20(%rbp),%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vu16_vu16
	.private_extern _A__xor_vu16_vu16
_A__xor_vu16_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _A__xor_pu16_pu16
	.private_extern _A__xor_pu16_pu16
_A__xor_pu16_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %edx
	movzwl	-20(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vu16_vi32
	.private_extern _A__xor_vu16_vi32
_A__xor_vu16_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movl	112+_MM_A(%rip), %eax
	cltq
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pu16_pi32
	.private_extern _A__xor_pu16_pi32
_A__xor_pu16_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vu16_vu32
	.private_extern _A__xor_vu16_vu32
_A__xor_vu16_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pu16_pu32
	.private_extern _A__xor_pu16_pu32
_A__xor_pu16_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	mov	-24(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vu16_vi64
	.private_extern _A__xor_vu16_vi64
_A__xor_vu16_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pu16_pi64
	.private_extern _A__xor_pu16_pi64
_A__xor_pu16_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vu16_vu64
	.private_extern _A__xor_vu16_vu64
_A__xor_vu16_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	128+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pu16_pu64
	.private_extern _A__xor_pu16_pu64
_A__xor_pu16_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %edx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__mult_vu16_vi8
	.private_extern _A__mult_vu16_vi8
_A__mult_vu16_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pu16_pi8
	.private_extern _A__mult_pu16_pi8
_A__mult_pu16_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %edx
	movsbq	-19(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_vu16_vu8
	.private_extern _A__mult_vu16_vu8
_A__mult_vu16_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pu16_pu8
	.private_extern _A__mult_pu16_pu8
_A__mult_pu16_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %edx
	movzbl	-19(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_vu16_vi16
	.private_extern _A__mult_vu16_vi16
_A__mult_vu16_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pu16_pi16
	.private_extern _A__mult_pu16_pi16
_A__mult_pu16_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %edx
	movswq	-20(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_vu16_vu16
	.private_extern _A__mult_vu16_vu16
_A__mult_vu16_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pu16_pu16
	.private_extern _A__mult_pu16_pu16
_A__mult_pu16_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %edx
	movzwl	-20(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_vu16_vi32
	.private_extern _A__mult_vu16_vi32
_A__mult_vu16_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movl	112+_MM_A(%rip), %eax
	cltq
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pu16_pi32
	.private_extern _A__mult_pu16_pi32
_A__mult_pu16_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_vu16_vu32
	.private_extern _A__mult_vu16_vu32
_A__mult_vu16_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pu16_pu32
	.private_extern _A__mult_pu16_pu32
_A__mult_pu16_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %edx
	mov	-24(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_vu16_vi64
	.private_extern _A__mult_vu16_vi64
_A__mult_vu16_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pu16_pi64
	.private_extern _A__mult_pu16_pi64
_A__mult_pu16_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _A__mult_vu16_vu64
	.private_extern _A__mult_vu16_vu64
_A__mult_vu16_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	128+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pu16_pu64
	.private_extern _A__mult_pu16_pu64
_A__mult_pu16_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %eax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _A__div_vu16_vi8
	.private_extern _A__div_vu16_vi8
_A__div_vu16_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %esi
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_pu16_pi8
	.private_extern _A__div_pu16_pi8
_A__div_pu16_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %esi
	movsbq	-19(%rbp),%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_vu16_vu8
	.private_extern _A__div_vu16_vu8
_A__div_vu16_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1057
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1057
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1057:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _A__div_pu16_pu8
	.private_extern _A__div_pu16_pu8
_A__div_pu16_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %eax
	movq	%rax, -40(%rbp)
	movzbl	-19(%rbp), %edx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1060
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1060
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1060:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _A__div_vu16_vi16
	.private_extern _A__div_vu16_vi16
_A__div_vu16_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %esi
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_pu16_pi16
	.private_extern _A__div_pu16_pi16
_A__div_pu16_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %esi
	movswq	-20(%rbp),%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_vu16_vu16
	.private_extern _A__div_vu16_vu16
_A__div_vu16_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -40(%rbp)
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	movq	-40(%rbp), %rdx
	movq	%rax, %rcx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	%rcx
	leave
	ret
.globl _A__div_pu16_pu16
	.private_extern _A__div_pu16_pu16
_A__div_pu16_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %eax
	movq	%rax, -40(%rbp)
	movzwl	-20(%rbp), %edx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1069
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1069
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1069:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _A__div_vu16_vi32
	.private_extern _A__div_vu16_vi32
_A__div_vu16_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %esi
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_pu16_pi32
	.private_extern _A__div_pu16_pi32
_A__div_pu16_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %esi
	movl	-24(%rbp), %eax
	movslq	%eax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_vu16_vu32
	.private_extern _A__div_vu16_vu32
_A__div_vu16_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1076
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1076
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1076:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _A__div_pu16_pu32
	.private_extern _A__div_pu16_pu32
_A__div_pu16_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %eax
	movq	%rax, -40(%rbp)
	mov	-24(%rbp), %edx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1079
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1079
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1079:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _A__div_vu16_vi64
	.private_extern _A__div_vu16_vi64
_A__div_vu16_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %esi
	movq	120+_MM_A(%rip), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_pu16_pi64
	.private_extern _A__div_pu16_pi64
_A__div_pu16_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %esi
	movq	-32(%rbp), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_vu16_vu64
	.private_extern _A__div_vu16_vu64
_A__div_vu16_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %esi
	movq	128+_MM_A(%rip), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_pu16_pu64
	.private_extern _A__div_pu16_pu64
_A__div_pu16_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %esi
	movq	-32(%rbp), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__mod_vu16_vi8
	.private_extern _A__mod_vu16_vi8
_A__mod_vu16_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %esi
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pu16_pi8
	.private_extern _A__mod_pu16_pi8
_A__mod_pu16_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %esi
	movsbq	-19(%rbp),%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vu16_vu8
	.private_extern _A__mod_vu16_vu8
_A__mod_vu16_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1094
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1094
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1094:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _A__mod_pu16_pu8
	.private_extern _A__mod_pu16_pu8
_A__mod_pu16_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %eax
	movq	%rax, -40(%rbp)
	movzbl	-19(%rbp), %edx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1097
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1097
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1097:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _A__mod_vu16_vi16
	.private_extern _A__mod_vu16_vi16
_A__mod_vu16_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %esi
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pu16_pi16
	.private_extern _A__mod_pu16_pi16
_A__mod_pu16_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %esi
	movswq	-20(%rbp),%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vu16_vu16
	.private_extern _A__mod_vu16_vu16
_A__mod_vu16_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1104
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1104
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1104:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _A__mod_pu16_pu16
	.private_extern _A__mod_pu16_pu16
_A__mod_pu16_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %eax
	movq	%rax, -40(%rbp)
	movzwl	-20(%rbp), %edx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1107
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1107
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1107:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _A__mod_vu16_vi32
	.private_extern _A__mod_vu16_vi32
_A__mod_vu16_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %esi
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pu16_pi32
	.private_extern _A__mod_pu16_pi32
_A__mod_pu16_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %esi
	movl	-24(%rbp), %eax
	movslq	%eax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vu16_vu32
	.private_extern _A__mod_vu16_vu32
_A__mod_vu16_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -32(%rbp)
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1114
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1114
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1114:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _A__mod_pu16_pu32
	.private_extern _A__mod_pu16_pu32
_A__mod_pu16_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %eax
	movq	%rax, -40(%rbp)
	mov	-24(%rbp), %edx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1117
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1117
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1117:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _A__mod_vu16_vi64
	.private_extern _A__mod_vu16_vi64
_A__mod_vu16_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %esi
	movq	120+_MM_A(%rip), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pu16_pi64
	.private_extern _A__mod_pu16_pi64
_A__mod_pu16_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %esi
	movq	-32(%rbp), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vu16_vu64
	.private_extern _A__mod_vu16_vu64
_A__mod_vu16_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %esi
	movq	128+_MM_A(%rip), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pu16_pu64
	.private_extern _A__mod_pu16_pu64
_A__mod_pu16_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %esi
	movq	-32(%rbp), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__ret_vi32
	.private_extern _A__ret_vi32
_A__ret_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	cltq
	leave
	ret
.globl _A__ret_ki32
	.private_extern _A__ret_ki32
_A__ret_ki32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$15, %eax
	leave
	ret
.globl _A__ret_pi32_i8
	.private_extern _A__ret_pi32_i8
_A__ret_pi32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movsbq	-17(%rbp),%rax
	leave
	ret
.globl _A__ret_pi32_u8
	.private_extern _A__ret_pi32_u8
_A__ret_pi32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movzbl	-17(%rbp), %eax
	leave
	ret
.globl _A__ret_pi32_i16
	.private_extern _A__ret_pi32_i16
_A__ret_pi32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movswq	-18(%rbp),%rax
	leave
	ret
.globl _A__ret_pi32_u16
	.private_extern _A__ret_pi32_u16
_A__ret_pi32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movzwl	-18(%rbp), %eax
	leave
	ret
.globl _A__ret_pi32_i32
	.private_extern _A__ret_pi32_i32
_A__ret_pi32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	leave
	ret
.globl _A__ret_pi32_u32
	.private_extern _A__ret_pi32_u32
_A__ret_pi32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	leave
	ret
.globl _A__ret_pi32_i64
	.private_extern _A__ret_pi32_i64
_A__ret_pi32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _A__ret_pi32_u64
	.private_extern _A__ret_pi32_u64
_A__ret_pi32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _A__add_vi32_vi8
	.private_extern _A__add_vi32_vi8
_A__add_vi32_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pi32_pi8
	.private_extern _A__add_pi32_pi8
_A__add_pi32_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movsbq	-21(%rbp),%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_vi32_vu8
	.private_extern _A__add_vi32_vu8
_A__add_vi32_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pi32_pu8
	.private_extern _A__add_pi32_pu8
_A__add_pi32_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movzbl	-21(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_vi32_vi16
	.private_extern _A__add_vi32_vi16
_A__add_vi32_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pi32_pi16
	.private_extern _A__add_pi32_pi16
_A__add_pi32_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movswq	-22(%rbp),%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_vi32_vu16
	.private_extern _A__add_vi32_vu16
_A__add_vi32_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pi32_pu16
	.private_extern _A__add_pi32_pu16
_A__add_pi32_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movzwl	-22(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_vi32_vi32
	.private_extern _A__add_vi32_vi32
_A__add_vi32_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movl	112+_MM_A(%rip), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pi32_pi32
	.private_extern _A__add_pi32_pi32
_A__add_pi32_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movl	-24(%rbp), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_vi32_vu32
	.private_extern _A__add_vi32_vu32
_A__add_vi32_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pi32_pu32
	.private_extern _A__add_pi32_pu32
_A__add_pi32_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	mov	-24(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_vi32_vi64
	.private_extern _A__add_vi32_vi64
_A__add_vi32_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	120+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pi32_pi64
	.private_extern _A__add_pi32_pi64
_A__add_pi32_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	addq	-32(%rbp), %rax
	leave
	ret
.globl _A__add_vi32_vu64
	.private_extern _A__add_vi32_vu64
_A__add_vi32_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	128+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pi32_pu64
	.private_extern _A__add_pi32_pu64
_A__add_pi32_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	addq	-32(%rbp), %rax
	leave
	ret
.globl _A__sub_vi32_vi8
	.private_extern _A__sub_vi32_vi8
_A__sub_vi32_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pi32_pi8
	.private_extern _A__sub_pi32_pi8
_A__sub_pi32_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movsbq	-21(%rbp),%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_vi32_vu8
	.private_extern _A__sub_vi32_vu8
_A__sub_vi32_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pi32_pu8
	.private_extern _A__sub_pi32_pu8
_A__sub_pi32_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movzbl	-21(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_vi32_vi16
	.private_extern _A__sub_vi32_vi16
_A__sub_vi32_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pi32_pi16
	.private_extern _A__sub_pi32_pi16
_A__sub_pi32_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movswq	-22(%rbp),%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_vi32_vu16
	.private_extern _A__sub_vi32_vu16
_A__sub_vi32_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pi32_pu16
	.private_extern _A__sub_pi32_pu16
_A__sub_pi32_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movzwl	-22(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_vi32_vi32
	.private_extern _A__sub_vi32_vi32
_A__sub_vi32_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _A__sub_pi32_pi32
	.private_extern _A__sub_pi32_pi32
_A__sub_pi32_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movl	-24(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_vi32_vu32
	.private_extern _A__sub_vi32_vu32
_A__sub_vi32_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pi32_pu32
	.private_extern _A__sub_pi32_pu32
_A__sub_pi32_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	mov	-24(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_vi32_vi64
	.private_extern _A__sub_vi32_vi64
_A__sub_vi32_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	120+_MM_A(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pi32_pi64
	.private_extern _A__sub_pi32_pi64
_A__sub_pi32_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	subq	-32(%rbp), %rax
	leave
	ret
.globl _A__sub_vi32_vu64
	.private_extern _A__sub_vi32_vu64
_A__sub_vi32_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	128+_MM_A(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pi32_pu64
	.private_extern _A__sub_pi32_pu64
_A__sub_pi32_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	subq	-32(%rbp), %rax
	leave
	ret
.globl _A__and_vi32_vi8
	.private_extern _A__and_vi32_vi8
_A__and_vi32_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pi32_pi8
	.private_extern _A__and_pi32_pi8
_A__and_pi32_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movsbq	-21(%rbp),%rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vi32_vu8
	.private_extern _A__and_vi32_vu8
_A__and_vi32_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pi32_pu8
	.private_extern _A__and_pi32_pu8
_A__and_pi32_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movzbl	-21(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vi32_vi16
	.private_extern _A__and_vi32_vi16
_A__and_vi32_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pi32_pi16
	.private_extern _A__and_pi32_pi16
_A__and_pi32_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movswq	-22(%rbp),%rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vi32_vu16
	.private_extern _A__and_vi32_vu16
_A__and_vi32_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pi32_pu16
	.private_extern _A__and_pi32_pu16
_A__and_pi32_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movzwl	-22(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vi32_vi32
	.private_extern _A__and_vi32_vi32
_A__and_vi32_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	cltq
	leave
	ret
.globl _A__and_pi32_pi32
	.private_extern _A__and_pi32_pi32
_A__and_pi32_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movl	-24(%rbp), %eax
	cltq
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vi32_vu32
	.private_extern _A__and_vi32_vu32
_A__and_vi32_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pi32_pu32
	.private_extern _A__and_pi32_pu32
_A__and_pi32_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	mov	-24(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vi32_vi64
	.private_extern _A__and_vi32_vi64
_A__and_vi32_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	120+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pi32_pi64
	.private_extern _A__and_pi32_pi64
_A__and_pi32_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vi32_vu64
	.private_extern _A__and_vi32_vu64
_A__and_vi32_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	128+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pi32_pu64
	.private_extern _A__and_pi32_pu64
_A__and_pi32_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__or_vi32_vi8
	.private_extern _A__or_vi32_vi8
_A__or_vi32_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pi32_pi8
	.private_extern _A__or_pi32_pi8
_A__or_pi32_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movsbq	-21(%rbp),%rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vi32_vu8
	.private_extern _A__or_vi32_vu8
_A__or_vi32_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pi32_pu8
	.private_extern _A__or_pi32_pu8
_A__or_pi32_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movzbl	-21(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vi32_vi16
	.private_extern _A__or_vi32_vi16
_A__or_vi32_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pi32_pi16
	.private_extern _A__or_pi32_pi16
_A__or_pi32_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movswq	-22(%rbp),%rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vi32_vu16
	.private_extern _A__or_vi32_vu16
_A__or_vi32_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pi32_pu16
	.private_extern _A__or_pi32_pu16
_A__or_pi32_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movzwl	-22(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vi32_vi32
	.private_extern _A__or_vi32_vi32
_A__or_vi32_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	cltq
	leave
	ret
.globl _A__or_pi32_pi32
	.private_extern _A__or_pi32_pi32
_A__or_pi32_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movl	-24(%rbp), %eax
	cltq
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vi32_vu32
	.private_extern _A__or_vi32_vu32
_A__or_vi32_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pi32_pu32
	.private_extern _A__or_pi32_pu32
_A__or_pi32_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	mov	-24(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vi32_vi64
	.private_extern _A__or_vi32_vi64
_A__or_vi32_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	120+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pi32_pi64
	.private_extern _A__or_pi32_pi64
_A__or_pi32_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vi32_vu64
	.private_extern _A__or_vi32_vu64
_A__or_vi32_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	128+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pi32_pu64
	.private_extern _A__or_pi32_pu64
_A__or_pi32_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__xor_vi32_vi8
	.private_extern _A__xor_vi32_vi8
_A__xor_vi32_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pi32_pi8
	.private_extern _A__xor_pi32_pi8
_A__xor_pi32_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movsbq	-21(%rbp),%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vi32_vu8
	.private_extern _A__xor_vi32_vu8
_A__xor_vi32_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pi32_pu8
	.private_extern _A__xor_pi32_pu8
_A__xor_pi32_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movzbl	-21(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vi32_vi16
	.private_extern _A__xor_vi32_vi16
_A__xor_vi32_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pi32_pi16
	.private_extern _A__xor_pi32_pi16
_A__xor_pi32_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movswq	-22(%rbp),%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vi32_vu16
	.private_extern _A__xor_vi32_vu16
_A__xor_vi32_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pi32_pu16
	.private_extern _A__xor_pi32_pu16
_A__xor_pi32_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movzwl	-22(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vi32_vi32
	.private_extern _A__xor_vi32_vi32
_A__xor_vi32_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _A__xor_pi32_pi32
	.private_extern _A__xor_pi32_pi32
_A__xor_pi32_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movl	-24(%rbp), %eax
	cltq
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vi32_vu32
	.private_extern _A__xor_vi32_vu32
_A__xor_vi32_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pi32_pu32
	.private_extern _A__xor_pi32_pu32
_A__xor_pi32_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	mov	-24(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vi32_vi64
	.private_extern _A__xor_vi32_vi64
_A__xor_vi32_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	120+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pi32_pi64
	.private_extern _A__xor_pi32_pi64
_A__xor_pi32_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vi32_vu64
	.private_extern _A__xor_vi32_vu64
_A__xor_vi32_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	128+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pi32_pu64
	.private_extern _A__xor_pi32_pu64
_A__xor_pi32_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__mult_vi32_vi8
	.private_extern _A__mult_vi32_vi8
_A__mult_vi32_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pi32_pi8
	.private_extern _A__mult_pi32_pi8
_A__mult_pi32_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movsbq	-21(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_vi32_vu8
	.private_extern _A__mult_vi32_vu8
_A__mult_vi32_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pi32_pu8
	.private_extern _A__mult_pi32_pu8
_A__mult_pi32_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movzbl	-21(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_vi32_vi16
	.private_extern _A__mult_vi32_vi16
_A__mult_vi32_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pi32_pi16
	.private_extern _A__mult_pi32_pi16
_A__mult_pi32_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movswq	-22(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_vi32_vu16
	.private_extern _A__mult_vi32_vu16
_A__mult_vi32_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pi32_pu16
	.private_extern _A__mult_pi32_pu16
_A__mult_pi32_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movzwl	-22(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_vi32_vi32
	.private_extern _A__mult_vi32_vi32
_A__mult_vi32_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movl	112+_MM_A(%rip), %eax
	cltq
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pi32_pi32
	.private_extern _A__mult_pi32_pi32
_A__mult_pi32_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	movl	-24(%rbp), %eax
	cltq
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_vi32_vu32
	.private_extern _A__mult_vi32_vu32
_A__mult_vi32_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pi32_pu32
	.private_extern _A__mult_pi32_pu32
_A__mult_pi32_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rdx
	mov	-24(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_vi32_vi64
	.private_extern _A__mult_vi32_vi64
_A__mult_vi32_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	120+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pi32_pi64
	.private_extern _A__mult_pi32_pi64
_A__mult_pi32_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _A__mult_vi32_vu64
	.private_extern _A__mult_vi32_vu64
_A__mult_vi32_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	128+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pi32_pu64
	.private_extern _A__mult_pi32_pu64
_A__mult_pi32_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	cltq
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _A__div_vi32_vi8
	.private_extern _A__div_vi32_vi8
_A__div_vi32_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rsi
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_pi32_pi8
	.private_extern _A__div_pi32_pi8
_A__div_pi32_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rsi
	movsbq	-21(%rbp),%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_vi32_vu8
	.private_extern _A__div_vi32_vu8
_A__div_vi32_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rsi
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_pi32_pu8
	.private_extern _A__div_pi32_pu8
_A__div_pi32_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rsi
	movzbl	-21(%rbp), %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_vi32_vi16
	.private_extern _A__div_vi32_vi16
_A__div_vi32_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rsi
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_pi32_pi16
	.private_extern _A__div_pi32_pi16
_A__div_pi32_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rsi
	movswq	-22(%rbp),%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_vi32_vu16
	.private_extern _A__div_vi32_vu16
_A__div_vi32_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rsi
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_pi32_pu16
	.private_extern _A__div_pi32_pu16
_A__div_pi32_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rsi
	movzwl	-22(%rbp), %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_vi32_vi32
	.private_extern _A__div_vi32_vi32
_A__div_vi32_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rsi
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_pi32_pi32
	.private_extern _A__div_pi32_pi32
_A__div_pi32_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rsi
	movl	-24(%rbp), %eax
	movslq	%eax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_vi32_vu32
	.private_extern _A__div_vi32_vu32
_A__div_vi32_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rsi
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_pi32_pu32
	.private_extern _A__div_pi32_pu32
_A__div_pi32_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rsi
	mov	-24(%rbp), %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_vi32_vi64
	.private_extern _A__div_vi32_vi64
_A__div_vi32_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rsi
	movq	120+_MM_A(%rip), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_pi32_pi64
	.private_extern _A__div_pi32_pi64
_A__div_pi32_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rsi
	movq	-32(%rbp), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_vi32_vu64
	.private_extern _A__div_vi32_vu64
_A__div_vi32_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rsi
	movq	128+_MM_A(%rip), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_pi32_pu64
	.private_extern _A__div_pi32_pu64
_A__div_pi32_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rsi
	movq	-32(%rbp), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__mod_vi32_vi8
	.private_extern _A__mod_vi32_vi8
_A__mod_vi32_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rsi
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pi32_pi8
	.private_extern _A__mod_pi32_pi8
_A__mod_pi32_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rsi
	movsbq	-21(%rbp),%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vi32_vu8
	.private_extern _A__mod_vi32_vu8
_A__mod_vi32_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rsi
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pi32_pu8
	.private_extern _A__mod_pi32_pu8
_A__mod_pi32_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rsi
	movzbl	-21(%rbp), %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vi32_vi16
	.private_extern _A__mod_vi32_vi16
_A__mod_vi32_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rsi
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pi32_pi16
	.private_extern _A__mod_pi32_pi16
_A__mod_pi32_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rsi
	movswq	-22(%rbp),%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vi32_vu16
	.private_extern _A__mod_vi32_vu16
_A__mod_vi32_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rsi
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pi32_pu16
	.private_extern _A__mod_pi32_pu16
_A__mod_pi32_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rsi
	movzwl	-22(%rbp), %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vi32_vi32
	.private_extern _A__mod_vi32_vi32
_A__mod_vi32_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rsi
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pi32_pi32
	.private_extern _A__mod_pi32_pi32
_A__mod_pi32_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rsi
	movl	-24(%rbp), %eax
	movslq	%eax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vi32_vu32
	.private_extern _A__mod_vi32_vu32
_A__mod_vi32_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rsi
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pi32_pu32
	.private_extern _A__mod_pi32_pu32
_A__mod_pi32_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rsi
	mov	-24(%rbp), %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vi32_vi64
	.private_extern _A__mod_vi32_vi64
_A__mod_vi32_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rsi
	movq	120+_MM_A(%rip), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pi32_pi64
	.private_extern _A__mod_pi32_pi64
_A__mod_pi32_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rsi
	movq	-32(%rbp), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vi32_vu64
	.private_extern _A__mod_vi32_vu64
_A__mod_vi32_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rsi
	movq	128+_MM_A(%rip), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pi32_pu64
	.private_extern _A__mod_pi32_pu64
_A__mod_pi32_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax,%rsi
	movq	-32(%rbp), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__ret_vu32
	.private_extern _A__ret_vu32
_A__ret_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	leave
	ret
.globl _A__ret_ku32
	.private_extern _A__ret_ku32
_A__ret_ku32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$18, %eax
	leave
	ret
.globl _A__ret_pu32_i8
	.private_extern _A__ret_pu32_i8
_A__ret_pu32_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movsbq	-17(%rbp),%rax
	leave
	ret
.globl _A__ret_pu32_u8
	.private_extern _A__ret_pu32_u8
_A__ret_pu32_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movzbl	-17(%rbp), %eax
	leave
	ret
.globl _A__ret_pu32_i16
	.private_extern _A__ret_pu32_i16
_A__ret_pu32_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movswq	-18(%rbp),%rax
	leave
	ret
.globl _A__ret_pu32_u16
	.private_extern _A__ret_pu32_u16
_A__ret_pu32_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movzwl	-18(%rbp), %eax
	leave
	ret
.globl _A__ret_pu32_i32
	.private_extern _A__ret_pu32_i32
_A__ret_pu32_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	leave
	ret
.globl _A__ret_pu32_u32
	.private_extern _A__ret_pu32_u32
_A__ret_pu32_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	leave
	ret
.globl _A__ret_pu32_i64
	.private_extern _A__ret_pu32_i64
_A__ret_pu32_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _A__ret_pu32_u64
	.private_extern _A__ret_pu32_u64
_A__ret_pu32_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _A__add_vu32_vi8
	.private_extern _A__add_vu32_vi8
_A__add_vu32_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pu32_pi8
	.private_extern _A__add_pu32_pi8
_A__add_pu32_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %edx
	movsbq	-21(%rbp),%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_vu32_vu8
	.private_extern _A__add_vu32_vu8
_A__add_vu32_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pu32_pu8
	.private_extern _A__add_pu32_pu8
_A__add_pu32_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %edx
	movzbl	-21(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_vu32_vi16
	.private_extern _A__add_vu32_vi16
_A__add_vu32_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pu32_pi16
	.private_extern _A__add_pu32_pi16
_A__add_pu32_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %edx
	movswq	-22(%rbp),%rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_vu32_vu16
	.private_extern _A__add_vu32_vu16
_A__add_vu32_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pu32_pu16
	.private_extern _A__add_pu32_pu16
_A__add_pu32_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %edx
	movzwl	-22(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_vu32_vi32
	.private_extern _A__add_vu32_vi32
_A__add_vu32_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movl	112+_MM_A(%rip), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pu32_pi32
	.private_extern _A__add_pu32_pi32
_A__add_pu32_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_vu32_vu32
	.private_extern _A__add_vu32_vu32
_A__add_vu32_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pu32_pu32
	.private_extern _A__add_pu32_pu32
_A__add_pu32_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %edx
	mov	-24(%rbp), %eax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_vu32_vi64
	.private_extern _A__add_vu32_vi64
_A__add_vu32_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pu32_pi64
	.private_extern _A__add_pu32_pi64
_A__add_pu32_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _A__add_vu32_vu64
	.private_extern _A__add_vu32_vu64
_A__add_vu32_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	128+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pu32_pu64
	.private_extern _A__add_pu32_pu64
_A__add_pu32_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	addq	-32(%rbp), %rax
	leave
	ret
.globl _A__sub_vu32_vi8
	.private_extern _A__sub_vu32_vi8
_A__sub_vu32_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pu32_pi8
	.private_extern _A__sub_pu32_pi8
_A__sub_pu32_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %edx
	movsbq	-21(%rbp),%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_vu32_vu8
	.private_extern _A__sub_vu32_vu8
_A__sub_vu32_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pu32_pu8
	.private_extern _A__sub_pu32_pu8
_A__sub_pu32_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %edx
	movzbl	-21(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_vu32_vi16
	.private_extern _A__sub_vu32_vi16
_A__sub_vu32_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pu32_pi16
	.private_extern _A__sub_pu32_pi16
_A__sub_pu32_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %edx
	movswq	-22(%rbp),%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_vu32_vu16
	.private_extern _A__sub_vu32_vu16
_A__sub_vu32_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pu32_pu16
	.private_extern _A__sub_pu32_pu16
_A__sub_pu32_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %edx
	movzwl	-22(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_vu32_vi32
	.private_extern _A__sub_vu32_vi32
_A__sub_vu32_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movl	112+_MM_A(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pu32_pi32
	.private_extern _A__sub_pu32_pi32
_A__sub_pu32_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_vu32_vu32
	.private_extern _A__sub_vu32_vu32
_A__sub_vu32_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _A__sub_pu32_pu32
	.private_extern _A__sub_pu32_pu32
_A__sub_pu32_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %edx
	mov	-24(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_vu32_vi64
	.private_extern _A__sub_vu32_vi64
_A__sub_vu32_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_A(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pu32_pi64
	.private_extern _A__sub_pu32_pi64
_A__sub_pu32_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _A__sub_vu32_vu64
	.private_extern _A__sub_vu32_vu64
_A__sub_vu32_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	128+_MM_A(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pu32_pu64
	.private_extern _A__sub_pu32_pu64
_A__sub_pu32_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	subq	-32(%rbp), %rax
	leave
	ret
.globl _A__and_vu32_vi8
	.private_extern _A__and_vu32_vi8
_A__and_vu32_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pu32_pi8
	.private_extern _A__and_pu32_pi8
_A__and_pu32_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %edx
	movsbq	-21(%rbp),%rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vu32_vu8
	.private_extern _A__and_vu32_vu8
_A__and_vu32_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pu32_pu8
	.private_extern _A__and_pu32_pu8
_A__and_pu32_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %edx
	movzbl	-21(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vu32_vi16
	.private_extern _A__and_vu32_vi16
_A__and_vu32_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pu32_pi16
	.private_extern _A__and_pu32_pi16
_A__and_pu32_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %edx
	movswq	-22(%rbp),%rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vu32_vu16
	.private_extern _A__and_vu32_vu16
_A__and_vu32_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pu32_pu16
	.private_extern _A__and_pu32_pu16
_A__and_pu32_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %edx
	movzwl	-22(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vu32_vi32
	.private_extern _A__and_vu32_vi32
_A__and_vu32_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movl	112+_MM_A(%rip), %eax
	cltq
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pu32_pi32
	.private_extern _A__and_pu32_pi32
_A__and_pu32_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vu32_vu32
	.private_extern _A__and_vu32_vu32
_A__and_vu32_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	leave
	ret
.globl _A__and_pu32_pu32
	.private_extern _A__and_pu32_pu32
_A__and_pu32_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %edx
	mov	-24(%rbp), %eax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vu32_vi64
	.private_extern _A__and_vu32_vi64
_A__and_vu32_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pu32_pi64
	.private_extern _A__and_pu32_pi64
_A__and_pu32_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vu32_vu64
	.private_extern _A__and_vu32_vu64
_A__and_vu32_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	128+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pu32_pu64
	.private_extern _A__and_pu32_pu64
_A__and_pu32_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__or_vu32_vi8
	.private_extern _A__or_vu32_vi8
_A__or_vu32_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pu32_pi8
	.private_extern _A__or_pu32_pi8
_A__or_pu32_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %edx
	movsbq	-21(%rbp),%rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vu32_vu8
	.private_extern _A__or_vu32_vu8
_A__or_vu32_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pu32_pu8
	.private_extern _A__or_pu32_pu8
_A__or_pu32_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %edx
	movzbl	-21(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vu32_vi16
	.private_extern _A__or_vu32_vi16
_A__or_vu32_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pu32_pi16
	.private_extern _A__or_pu32_pi16
_A__or_pu32_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %edx
	movswq	-22(%rbp),%rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vu32_vu16
	.private_extern _A__or_vu32_vu16
_A__or_vu32_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pu32_pu16
	.private_extern _A__or_pu32_pu16
_A__or_pu32_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %edx
	movzwl	-22(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vu32_vi32
	.private_extern _A__or_vu32_vi32
_A__or_vu32_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movl	112+_MM_A(%rip), %eax
	cltq
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pu32_pi32
	.private_extern _A__or_pu32_pi32
_A__or_pu32_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vu32_vu32
	.private_extern _A__or_vu32_vu32
_A__or_vu32_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	leave
	ret
.globl _A__or_pu32_pu32
	.private_extern _A__or_pu32_pu32
_A__or_pu32_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %edx
	mov	-24(%rbp), %eax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vu32_vi64
	.private_extern _A__or_vu32_vi64
_A__or_vu32_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pu32_pi64
	.private_extern _A__or_pu32_pi64
_A__or_pu32_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vu32_vu64
	.private_extern _A__or_vu32_vu64
_A__or_vu32_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	128+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pu32_pu64
	.private_extern _A__or_pu32_pu64
_A__or_pu32_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__xor_vu32_vi8
	.private_extern _A__xor_vu32_vi8
_A__xor_vu32_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pu32_pi8
	.private_extern _A__xor_pu32_pi8
_A__xor_pu32_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %edx
	movsbq	-21(%rbp),%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vu32_vu8
	.private_extern _A__xor_vu32_vu8
_A__xor_vu32_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pu32_pu8
	.private_extern _A__xor_pu32_pu8
_A__xor_pu32_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %edx
	movzbl	-21(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vu32_vi16
	.private_extern _A__xor_vu32_vi16
_A__xor_vu32_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pu32_pi16
	.private_extern _A__xor_pu32_pi16
_A__xor_pu32_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %edx
	movswq	-22(%rbp),%rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vu32_vu16
	.private_extern _A__xor_vu32_vu16
_A__xor_vu32_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pu32_pu16
	.private_extern _A__xor_pu32_pu16
_A__xor_pu32_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %edx
	movzwl	-22(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vu32_vi32
	.private_extern _A__xor_vu32_vi32
_A__xor_vu32_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movl	112+_MM_A(%rip), %eax
	cltq
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pu32_pi32
	.private_extern _A__xor_pu32_pi32
_A__xor_pu32_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vu32_vu32
	.private_extern _A__xor_vu32_vu32
_A__xor_vu32_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _A__xor_pu32_pu32
	.private_extern _A__xor_pu32_pu32
_A__xor_pu32_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %edx
	mov	-24(%rbp), %eax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vu32_vi64
	.private_extern _A__xor_vu32_vi64
_A__xor_vu32_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pu32_pi64
	.private_extern _A__xor_pu32_pi64
_A__xor_pu32_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vu32_vu64
	.private_extern _A__xor_vu32_vu64
_A__xor_vu32_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	128+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pu32_pu64
	.private_extern _A__xor_pu32_pu64
_A__xor_pu32_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %edx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__mult_vu32_vi8
	.private_extern _A__mult_vu32_vi8
_A__mult_vu32_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pu32_pi8
	.private_extern _A__mult_pu32_pi8
_A__mult_pu32_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %edx
	movsbq	-21(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_vu32_vu8
	.private_extern _A__mult_vu32_vu8
_A__mult_vu32_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pu32_pu8
	.private_extern _A__mult_pu32_pu8
_A__mult_pu32_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %edx
	movzbl	-21(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_vu32_vi16
	.private_extern _A__mult_vu32_vi16
_A__mult_vu32_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pu32_pi16
	.private_extern _A__mult_pu32_pi16
_A__mult_pu32_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %edx
	movswq	-22(%rbp),%rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_vu32_vu16
	.private_extern _A__mult_vu32_vu16
_A__mult_vu32_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pu32_pu16
	.private_extern _A__mult_pu32_pu16
_A__mult_pu32_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %edx
	movzwl	-22(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_vu32_vi32
	.private_extern _A__mult_vu32_vi32
_A__mult_vu32_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movl	112+_MM_A(%rip), %eax
	cltq
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pu32_pi32
	.private_extern _A__mult_pu32_pi32
_A__mult_pu32_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %edx
	movl	-24(%rbp), %eax
	cltq
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_vu32_vu32
	.private_extern _A__mult_vu32_vu32
_A__mult_vu32_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pu32_pu32
	.private_extern _A__mult_pu32_pu32
_A__mult_pu32_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %edx
	mov	-24(%rbp), %eax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_vu32_vi64
	.private_extern _A__mult_vu32_vi64
_A__mult_vu32_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pu32_pi64
	.private_extern _A__mult_pu32_pi64
_A__mult_pu32_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _A__mult_vu32_vu64
	.private_extern _A__mult_vu32_vu64
_A__mult_vu32_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	128+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pu32_pu64
	.private_extern _A__mult_pu32_pu64
_A__mult_pu32_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %eax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _A__div_vu32_vi8
	.private_extern _A__div_vu32_vi8
_A__div_vu32_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %esi
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_pu32_pi8
	.private_extern _A__div_pu32_pi8
_A__div_pu32_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %esi
	movsbq	-21(%rbp),%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_vu32_vu8
	.private_extern _A__div_vu32_vu8
_A__div_vu32_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1620
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1620
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1620:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _A__div_pu32_pu8
	.private_extern _A__div_pu32_pu8
_A__div_pu32_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %eax
	movq	%rax, -40(%rbp)
	movzbl	-21(%rbp), %edx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1623
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1623
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1623:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _A__div_vu32_vi16
	.private_extern _A__div_vu32_vi16
_A__div_vu32_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %esi
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_pu32_pi16
	.private_extern _A__div_pu32_pi16
_A__div_pu32_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %esi
	movswq	-22(%rbp),%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_vu32_vu16
	.private_extern _A__div_vu32_vu16
_A__div_vu32_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rax, -40(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1630
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1630
	subq	$1, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -48(%rbp)
L1630:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _A__div_pu32_pu16
	.private_extern _A__div_pu32_pu16
_A__div_pu32_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %eax
	movq	%rax, -40(%rbp)
	movzwl	-22(%rbp), %edx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1633
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1633
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1633:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _A__div_vu32_vi32
	.private_extern _A__div_vu32_vi32
_A__div_vu32_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %esi
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_pu32_pi32
	.private_extern _A__div_pu32_pi32
_A__div_pu32_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %esi
	movl	-24(%rbp), %eax
	movslq	%eax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_vu32_vu32
	.private_extern _A__div_vu32_vu32
_A__div_vu32_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -40(%rbp)
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	movq	-40(%rbp), %rdx
	movq	%rax, %rcx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	%rcx
	leave
	ret
.globl _A__div_pu32_pu32
	.private_extern _A__div_pu32_pu32
_A__div_pu32_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %eax
	movq	%rax, -40(%rbp)
	mov	-24(%rbp), %edx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, -48(%rbp)
	cmpq	$0, -56(%rbp)
	je	L1642
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1642
	subq	$1, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -56(%rbp)
L1642:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _A__div_vu32_vi64
	.private_extern _A__div_vu32_vi64
_A__div_vu32_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %esi
	movq	120+_MM_A(%rip), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_pu32_pi64
	.private_extern _A__div_pu32_pi64
_A__div_pu32_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %esi
	movq	-32(%rbp), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_vu32_vu64
	.private_extern _A__div_vu32_vu64
_A__div_vu32_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %esi
	movq	128+_MM_A(%rip), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_pu32_pu64
	.private_extern _A__div_pu32_pu64
_A__div_pu32_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %esi
	movq	-32(%rbp), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__mod_vu32_vi8
	.private_extern _A__mod_vu32_vi8
_A__mod_vu32_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %esi
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pu32_pi8
	.private_extern _A__mod_pu32_pi8
_A__mod_pu32_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %esi
	movsbq	-21(%rbp),%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vu32_vu8
	.private_extern _A__mod_vu32_vu8
_A__mod_vu32_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1657
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1657
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1657:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _A__mod_pu32_pu8
	.private_extern _A__mod_pu32_pu8
_A__mod_pu32_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %eax
	movq	%rax, -40(%rbp)
	movzbl	-21(%rbp), %edx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1660
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1660
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1660:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _A__mod_vu32_vi16
	.private_extern _A__mod_vu32_vi16
_A__mod_vu32_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %esi
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pu32_pi16
	.private_extern _A__mod_pu32_pi16
_A__mod_pu32_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %esi
	movswq	-22(%rbp),%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vu32_vu16
	.private_extern _A__mod_vu32_vu16
_A__mod_vu32_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1667
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1667
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1667:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _A__mod_pu32_pu16
	.private_extern _A__mod_pu32_pu16
_A__mod_pu32_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %eax
	movq	%rax, -40(%rbp)
	movzwl	-22(%rbp), %edx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1670
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1670
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1670:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _A__mod_vu32_vi32
	.private_extern _A__mod_vu32_vi32
_A__mod_vu32_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %esi
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pu32_pi32
	.private_extern _A__mod_pu32_pi32
_A__mod_pu32_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %esi
	movl	-24(%rbp), %eax
	movslq	%eax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vu32_vu32
	.private_extern _A__mod_vu32_vu32
_A__mod_vu32_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -32(%rbp)
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	movq	%rax, -24(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-24(%rbp)
	movq	%rdx, -40(%rbp)
	cmpq	$0, -40(%rbp)
	je	L1677
	movq	-32(%rbp), %rax
	xorq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	L1677
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
L1677:
	movq	-40(%rbp), %rax
	leave
	ret
.globl _A__mod_pu32_pu32
	.private_extern _A__mod_pu32_pu32
_A__mod_pu32_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %eax
	movq	%rax, -40(%rbp)
	mov	-24(%rbp), %edx
	movq	%rdx, -32(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	-32(%rbp)
	movq	%rdx, -48(%rbp)
	cmpq	$0, -48(%rbp)
	je	L1680
	movq	-40(%rbp), %rax
	xorq	-32(%rbp), %rax
	testq	%rax, %rax
	jns	L1680
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
L1680:
	movq	-48(%rbp), %rax
	leave
	ret
.globl _A__mod_vu32_vi64
	.private_extern _A__mod_vu32_vi64
_A__mod_vu32_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %esi
	movq	120+_MM_A(%rip), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pu32_pi64
	.private_extern _A__mod_pu32_pi64
_A__mod_pu32_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %esi
	movq	-32(%rbp), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vu32_vu64
	.private_extern _A__mod_vu32_vu64
_A__mod_vu32_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %esi
	movq	128+_MM_A(%rip), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pu32_pu64
	.private_extern _A__mod_pu32_pu64
_A__mod_pu32_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %esi
	movq	-32(%rbp), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__ret_vi64
	.private_extern _A__ret_vi64
_A__ret_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_A(%rip), %rax
	leave
	ret
.globl _A__ret_ki64
	.private_extern _A__ret_ki64
_A__ret_ki64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$21, %eax
	leave
	ret
.globl _A__ret_pi64_i8
	.private_extern _A__ret_pi64_i8
_A__ret_pi64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movsbq	-17(%rbp),%rax
	leave
	ret
.globl _A__ret_pi64_u8
	.private_extern _A__ret_pi64_u8
_A__ret_pi64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movzbl	-17(%rbp), %eax
	leave
	ret
.globl _A__ret_pi64_i16
	.private_extern _A__ret_pi64_i16
_A__ret_pi64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movswq	-18(%rbp),%rax
	leave
	ret
.globl _A__ret_pi64_u16
	.private_extern _A__ret_pi64_u16
_A__ret_pi64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movzwl	-18(%rbp), %eax
	leave
	ret
.globl _A__ret_pi64_i32
	.private_extern _A__ret_pi64_i32
_A__ret_pi64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	leave
	ret
.globl _A__ret_pi64_u32
	.private_extern _A__ret_pi64_u32
_A__ret_pi64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	leave
	ret
.globl _A__ret_pi64_i64
	.private_extern _A__ret_pi64_i64
_A__ret_pi64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _A__ret_pi64_u64
	.private_extern _A__ret_pi64_u64
_A__ret_pi64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _A__add_vi64_vi8
	.private_extern _A__add_vi64_vi8
_A__add_vi64_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	120+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pi64_pi8
	.private_extern _A__add_pi64_pi8
_A__add_pi64_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _A__add_vi64_vu8
	.private_extern _A__add_vi64_vu8
_A__add_vi64_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pi64_pu8
	.private_extern _A__add_pi64_pu8
_A__add_pi64_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _A__add_vi64_vi16
	.private_extern _A__add_vi64_vi16
_A__add_vi64_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	120+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pi64_pi16
	.private_extern _A__add_pi64_pi16
_A__add_pi64_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _A__add_vi64_vu16
	.private_extern _A__add_vi64_vu16
_A__add_vi64_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pi64_pu16
	.private_extern _A__add_pi64_pu16
_A__add_pi64_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _A__add_vi64_vi32
	.private_extern _A__add_vi64_vi32
_A__add_vi64_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	120+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pi64_pi32
	.private_extern _A__add_pi64_pi32
_A__add_pi64_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	addq	-24(%rbp), %rax
	leave
	ret
.globl _A__add_vi64_vu32
	.private_extern _A__add_vi64_vu32
_A__add_vi64_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pi64_pu32
	.private_extern _A__add_pi64_pu32
_A__add_pi64_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _A__add_vi64_vi64
	.private_extern _A__add_vi64_vi64
_A__add_vi64_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_A(%rip), %rdx
	movq	120+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pi64_pi64
	.private_extern _A__add_pi64_pi64
_A__add_pi64_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	addq	%rdx, %rax
	leave
	ret
.globl _A__add_vi64_vu64
	.private_extern _A__add_vi64_vu64
_A__add_vi64_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_A(%rip), %rdx
	movq	128+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pi64_pu64
	.private_extern _A__add_pi64_pu64
_A__add_pi64_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	addq	%rdx, %rax
	leave
	ret
.globl _A__sub_vi64_vi8
	.private_extern _A__sub_vi64_vi8
_A__sub_vi64_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_A(%rip), %rdx
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pi64_pi8
	.private_extern _A__sub_pi64_pi8
_A__sub_pi64_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _A__sub_vi64_vu8
	.private_extern _A__sub_vi64_vu8
_A__sub_vi64_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_A(%rip), %rdx
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pi64_pu8
	.private_extern _A__sub_pi64_pu8
_A__sub_pi64_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _A__sub_vi64_vi16
	.private_extern _A__sub_vi64_vi16
_A__sub_vi64_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_A(%rip), %rdx
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pi64_pi16
	.private_extern _A__sub_pi64_pi16
_A__sub_pi64_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _A__sub_vi64_vu16
	.private_extern _A__sub_vi64_vu16
_A__sub_vi64_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_A(%rip), %rdx
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pi64_pu16
	.private_extern _A__sub_pi64_pu16
_A__sub_pi64_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _A__sub_vi64_vi32
	.private_extern _A__sub_vi64_vi32
_A__sub_vi64_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_A(%rip), %rdx
	movl	112+_MM_A(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pi64_pi32
	.private_extern _A__sub_pi64_pi32
_A__sub_pi64_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _A__sub_vi64_vu32
	.private_extern _A__sub_vi64_vu32
_A__sub_vi64_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_A(%rip), %rdx
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pi64_pu32
	.private_extern _A__sub_pi64_pu32
_A__sub_pi64_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _A__sub_vi64_vi64
	.private_extern _A__sub_vi64_vi64
_A__sub_vi64_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _A__sub_pi64_pi64
	.private_extern _A__sub_pi64_pi64
_A__sub_pi64_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _A__sub_vi64_vu64
	.private_extern _A__sub_vi64_vu64
_A__sub_vi64_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_A(%rip), %rdx
	movq	128+_MM_A(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pi64_pu64
	.private_extern _A__sub_pi64_pu64
_A__sub_pi64_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _A__and_vi64_vi8
	.private_extern _A__and_vi64_vi8
_A__and_vi64_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	120+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pi64_pi8
	.private_extern _A__and_pi64_pi8
_A__and_pi64_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vi64_vu8
	.private_extern _A__and_vi64_vu8
_A__and_vi64_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pi64_pu8
	.private_extern _A__and_pi64_pu8
_A__and_pi64_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vi64_vi16
	.private_extern _A__and_vi64_vi16
_A__and_vi64_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	120+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pi64_pi16
	.private_extern _A__and_pi64_pi16
_A__and_pi64_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vi64_vu16
	.private_extern _A__and_vi64_vu16
_A__and_vi64_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pi64_pu16
	.private_extern _A__and_pi64_pu16
_A__and_pi64_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vi64_vi32
	.private_extern _A__and_vi64_vi32
_A__and_vi64_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	120+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pi64_pi32
	.private_extern _A__and_pi64_pi32
_A__and_pi64_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vi64_vu32
	.private_extern _A__and_vi64_vu32
_A__and_vi64_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pi64_pu32
	.private_extern _A__and_pi64_pu32
_A__and_pi64_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vi64_vi64
	.private_extern _A__and_vi64_vi64
_A__and_vi64_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_A(%rip), %rax
	leave
	ret
.globl _A__and_pi64_pi64
	.private_extern _A__and_pi64_pi64
_A__and_pi64_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vi64_vu64
	.private_extern _A__and_vi64_vu64
_A__and_vi64_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_A(%rip), %rax
	movq	%rax, %rdx
	movq	128+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pi64_pu64
	.private_extern _A__and_pi64_pu64
_A__and_pi64_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__or_vi64_vi8
	.private_extern _A__or_vi64_vi8
_A__or_vi64_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	120+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pi64_pi8
	.private_extern _A__or_pi64_pi8
_A__or_pi64_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vi64_vu8
	.private_extern _A__or_vi64_vu8
_A__or_vi64_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pi64_pu8
	.private_extern _A__or_pi64_pu8
_A__or_pi64_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vi64_vi16
	.private_extern _A__or_vi64_vi16
_A__or_vi64_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	120+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pi64_pi16
	.private_extern _A__or_pi64_pi16
_A__or_pi64_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vi64_vu16
	.private_extern _A__or_vi64_vu16
_A__or_vi64_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pi64_pu16
	.private_extern _A__or_pi64_pu16
_A__or_pi64_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vi64_vi32
	.private_extern _A__or_vi64_vi32
_A__or_vi64_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	120+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pi64_pi32
	.private_extern _A__or_pi64_pi32
_A__or_pi64_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vi64_vu32
	.private_extern _A__or_vi64_vu32
_A__or_vi64_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pi64_pu32
	.private_extern _A__or_pi64_pu32
_A__or_pi64_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vi64_vi64
	.private_extern _A__or_vi64_vi64
_A__or_vi64_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_A(%rip), %rax
	leave
	ret
.globl _A__or_pi64_pi64
	.private_extern _A__or_pi64_pi64
_A__or_pi64_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vi64_vu64
	.private_extern _A__or_vi64_vu64
_A__or_vi64_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_A(%rip), %rax
	movq	%rax, %rdx
	movq	128+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pi64_pu64
	.private_extern _A__or_pi64_pu64
_A__or_pi64_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__xor_vi64_vi8
	.private_extern _A__xor_vi64_vi8
_A__xor_vi64_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	120+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pi64_pi8
	.private_extern _A__xor_pi64_pi8
_A__xor_pi64_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vi64_vu8
	.private_extern _A__xor_vi64_vu8
_A__xor_vi64_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pi64_pu8
	.private_extern _A__xor_pi64_pu8
_A__xor_pi64_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vi64_vi16
	.private_extern _A__xor_vi64_vi16
_A__xor_vi64_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	120+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pi64_pi16
	.private_extern _A__xor_pi64_pi16
_A__xor_pi64_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vi64_vu16
	.private_extern _A__xor_vi64_vu16
_A__xor_vi64_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pi64_pu16
	.private_extern _A__xor_pi64_pu16
_A__xor_pi64_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vi64_vi32
	.private_extern _A__xor_vi64_vi32
_A__xor_vi64_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	120+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pi64_pi32
	.private_extern _A__xor_pi64_pi32
_A__xor_pi64_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vi64_vu32
	.private_extern _A__xor_vi64_vu32
_A__xor_vi64_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pi64_pu32
	.private_extern _A__xor_pi64_pu32
_A__xor_pi64_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vi64_vi64
	.private_extern _A__xor_vi64_vi64
_A__xor_vi64_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _A__xor_pi64_pi64
	.private_extern _A__xor_pi64_pi64
_A__xor_pi64_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vi64_vu64
	.private_extern _A__xor_vi64_vu64
_A__xor_vi64_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_A(%rip), %rax
	movq	%rax, %rdx
	movq	128+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pi64_pu64
	.private_extern _A__xor_pi64_pu64
_A__xor_pi64_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__mult_vi64_vi8
	.private_extern _A__mult_vi64_vi8
_A__mult_vi64_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	120+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pi64_pi8
	.private_extern _A__mult_pi64_pi8
_A__mult_pi64_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _A__mult_vi64_vu8
	.private_extern _A__mult_vi64_vu8
_A__mult_vi64_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pi64_pu8
	.private_extern _A__mult_pi64_pu8
_A__mult_pi64_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _A__mult_vi64_vi16
	.private_extern _A__mult_vi64_vi16
_A__mult_vi64_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	120+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pi64_pi16
	.private_extern _A__mult_pi64_pi16
_A__mult_pi64_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _A__mult_vi64_vu16
	.private_extern _A__mult_vi64_vu16
_A__mult_vi64_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pi64_pu16
	.private_extern _A__mult_pi64_pu16
_A__mult_pi64_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _A__mult_vi64_vi32
	.private_extern _A__mult_vi64_vi32
_A__mult_vi64_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	120+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pi64_pi32
	.private_extern _A__mult_pi64_pi32
_A__mult_pi64_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _A__mult_vi64_vu32
	.private_extern _A__mult_vi64_vu32
_A__mult_vi64_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pi64_pu32
	.private_extern _A__mult_pi64_pu32
_A__mult_pi64_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _A__mult_vi64_vi64
	.private_extern _A__mult_vi64_vi64
_A__mult_vi64_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_A(%rip), %rdx
	movq	120+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pi64_pi64
	.private_extern _A__mult_pi64_pi64
_A__mult_pi64_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _A__mult_vi64_vu64
	.private_extern _A__mult_vi64_vu64
_A__mult_vi64_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	120+_MM_A(%rip), %rdx
	movq	128+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pi64_pu64
	.private_extern _A__mult_pi64_pu64
_A__mult_pi64_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _A__div_vi64_vi8
	.private_extern _A__div_vi64_vi8
_A__div_vi64_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	120+_MM_A(%rip), %rsi
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_pi64_pi8
	.private_extern _A__div_pi64_pi8
_A__div_pi64_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdi
	movq	-24(%rbp), %rsi
	call	_m3_divL
	leave
	ret
.globl _A__div_vi64_vu8
	.private_extern _A__div_vi64_vu8
_A__div_vi64_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	120+_MM_A(%rip), %rsi
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_pi64_pu8
	.private_extern _A__div_pi64_pu8
_A__div_pi64_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edi
	movq	-24(%rbp), %rsi
	call	_m3_divL
	leave
	ret
.globl _A__div_vi64_vi16
	.private_extern _A__div_vi64_vi16
_A__div_vi64_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	120+_MM_A(%rip), %rsi
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_pi64_pi16
	.private_extern _A__div_pi64_pi16
_A__div_pi64_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdi
	movq	-24(%rbp), %rsi
	call	_m3_divL
	leave
	ret
.globl _A__div_vi64_vu16
	.private_extern _A__div_vi64_vu16
_A__div_vi64_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	120+_MM_A(%rip), %rsi
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_pi64_pu16
	.private_extern _A__div_pi64_pu16
_A__div_pi64_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edi
	movq	-24(%rbp), %rsi
	call	_m3_divL
	leave
	ret
.globl _A__div_vi64_vi32
	.private_extern _A__div_vi64_vi32
_A__div_vi64_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	120+_MM_A(%rip), %rsi
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_pi64_pi32
	.private_extern _A__div_pi64_pi32
_A__div_pi64_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdi
	movq	-24(%rbp), %rsi
	call	_m3_divL
	leave
	ret
.globl _A__div_vi64_vu32
	.private_extern _A__div_vi64_vu32
_A__div_vi64_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	120+_MM_A(%rip), %rsi
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_pi64_pu32
	.private_extern _A__div_pi64_pu32
_A__div_pi64_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edi
	movq	-24(%rbp), %rsi
	call	_m3_divL
	leave
	ret
.globl _A__div_vi64_vi64
	.private_extern _A__div_vi64_vi64
_A__div_vi64_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	120+_MM_A(%rip), %rsi
	movq	120+_MM_A(%rip), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_pi64_pi64
	.private_extern _A__div_pi64_pi64
_A__div_pi64_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rsi
	movq	-32(%rbp), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_vi64_vu64
	.private_extern _A__div_vi64_vu64
_A__div_vi64_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	120+_MM_A(%rip), %rsi
	movq	128+_MM_A(%rip), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_pi64_pu64
	.private_extern _A__div_pi64_pu64
_A__div_pi64_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rsi
	movq	-32(%rbp), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__mod_vi64_vi8
	.private_extern _A__mod_vi64_vi8
_A__mod_vi64_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	120+_MM_A(%rip), %rsi
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pi64_pi8
	.private_extern _A__mod_pi64_pi8
_A__mod_pi64_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdi
	movq	-24(%rbp), %rsi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vi64_vu8
	.private_extern _A__mod_vi64_vu8
_A__mod_vi64_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	120+_MM_A(%rip), %rsi
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pi64_pu8
	.private_extern _A__mod_pi64_pu8
_A__mod_pi64_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edi
	movq	-24(%rbp), %rsi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vi64_vi16
	.private_extern _A__mod_vi64_vi16
_A__mod_vi64_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	120+_MM_A(%rip), %rsi
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pi64_pi16
	.private_extern _A__mod_pi64_pi16
_A__mod_pi64_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdi
	movq	-24(%rbp), %rsi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vi64_vu16
	.private_extern _A__mod_vi64_vu16
_A__mod_vi64_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	120+_MM_A(%rip), %rsi
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pi64_pu16
	.private_extern _A__mod_pi64_pu16
_A__mod_pi64_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edi
	movq	-24(%rbp), %rsi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vi64_vi32
	.private_extern _A__mod_vi64_vi32
_A__mod_vi64_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	120+_MM_A(%rip), %rsi
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pi64_pi32
	.private_extern _A__mod_pi64_pi32
_A__mod_pi64_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdi
	movq	-24(%rbp), %rsi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vi64_vu32
	.private_extern _A__mod_vi64_vu32
_A__mod_vi64_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	120+_MM_A(%rip), %rsi
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pi64_pu32
	.private_extern _A__mod_pi64_pu32
_A__mod_pi64_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edi
	movq	-24(%rbp), %rsi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vi64_vi64
	.private_extern _A__mod_vi64_vi64
_A__mod_vi64_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	120+_MM_A(%rip), %rsi
	movq	120+_MM_A(%rip), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pi64_pi64
	.private_extern _A__mod_pi64_pi64
_A__mod_pi64_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rsi
	movq	-32(%rbp), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vi64_vu64
	.private_extern _A__mod_vi64_vu64
_A__mod_vi64_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	120+_MM_A(%rip), %rsi
	movq	128+_MM_A(%rip), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pi64_pu64
	.private_extern _A__mod_pi64_pu64
_A__mod_pi64_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rsi
	movq	-32(%rbp), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__ret_vu64
	.private_extern _A__ret_vu64
_A__ret_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_A(%rip), %rax
	leave
	ret
.globl _A__ret_ku64
	.private_extern _A__ret_ku64
_A__ret_ku64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$24, %eax
	leave
	ret
.globl _A__ret_pu64_i8
	.private_extern _A__ret_pu64_i8
_A__ret_pu64_i8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movsbq	-17(%rbp),%rax
	leave
	ret
.globl _A__ret_pu64_u8
	.private_extern _A__ret_pu64_u8
_A__ret_pu64_u8:
	pushq	%rbp
	movq	%rsp, %rbp
	movb	%dil, -17(%rbp)
	movzbl	-17(%rbp), %eax
	leave
	ret
.globl _A__ret_pu64_i16
	.private_extern _A__ret_pu64_i16
_A__ret_pu64_i16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movswq	-18(%rbp),%rax
	leave
	ret
.globl _A__ret_pu64_u16
	.private_extern _A__ret_pu64_u16
_A__ret_pu64_u16:
	pushq	%rbp
	movq	%rsp, %rbp
	movw	%di, -18(%rbp)
	movzwl	-18(%rbp), %eax
	leave
	ret
.globl _A__ret_pu64_i32
	.private_extern _A__ret_pu64_i32
_A__ret_pu64_i32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	leave
	ret
.globl _A__ret_pu64_u32
	.private_extern _A__ret_pu64_u32
_A__ret_pu64_u32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -20(%rbp)
	mov	-20(%rbp), %eax
	leave
	ret
.globl _A__ret_pu64_i64
	.private_extern _A__ret_pu64_i64
_A__ret_pu64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _A__ret_pu64_u64
	.private_extern _A__ret_pu64_u64
_A__ret_pu64_u64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	leave
	ret
.globl _A__add_vu64_vi8
	.private_extern _A__add_vu64_vi8
_A__add_vu64_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	128+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pu64_pi8
	.private_extern _A__add_pu64_pi8
_A__add_pu64_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _A__add_vu64_vu8
	.private_extern _A__add_vu64_vu8
_A__add_vu64_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	128+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pu64_pu8
	.private_extern _A__add_pu64_pu8
_A__add_pu64_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _A__add_vu64_vi16
	.private_extern _A__add_vu64_vi16
_A__add_vu64_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	128+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pu64_pi16
	.private_extern _A__add_pu64_pi16
_A__add_pu64_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _A__add_vu64_vu16
	.private_extern _A__add_vu64_vu16
_A__add_vu64_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	128+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pu64_pu16
	.private_extern _A__add_pu64_pu16
_A__add_pu64_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _A__add_vu64_vi32
	.private_extern _A__add_vu64_vi32
_A__add_vu64_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	128+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pu64_pi32
	.private_extern _A__add_pu64_pi32
_A__add_pu64_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	addq	-24(%rbp), %rax
	leave
	ret
.globl _A__add_vu64_vu32
	.private_extern _A__add_vu64_vu32
_A__add_vu64_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	128+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pu64_pu32
	.private_extern _A__add_pu64_pu32
_A__add_pu64_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	addq	-24(%rbp), %rax
	leave
	ret
.globl _A__add_vu64_vi64
	.private_extern _A__add_vu64_vi64
_A__add_vu64_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_A(%rip), %rdx
	movq	120+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pu64_pi64
	.private_extern _A__add_pu64_pi64
_A__add_pu64_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	addq	%rdx, %rax
	leave
	ret
.globl _A__add_vu64_vu64
	.private_extern _A__add_vu64_vu64
_A__add_vu64_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_A(%rip), %rdx
	movq	128+_MM_A(%rip), %rax
	leaq	(%rdx,%rax), %rax
	leave
	ret
.globl _A__add_pu64_pu64
	.private_extern _A__add_pu64_pu64
_A__add_pu64_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	addq	%rdx, %rax
	leave
	ret
.globl _A__sub_vu64_vi8
	.private_extern _A__sub_vu64_vi8
_A__sub_vu64_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_A(%rip), %rdx
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pu64_pi8
	.private_extern _A__sub_pu64_pi8
_A__sub_pu64_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _A__sub_vu64_vu8
	.private_extern _A__sub_vu64_vu8
_A__sub_vu64_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_A(%rip), %rdx
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pu64_pu8
	.private_extern _A__sub_pu64_pu8
_A__sub_pu64_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _A__sub_vu64_vi16
	.private_extern _A__sub_vu64_vi16
_A__sub_vu64_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_A(%rip), %rdx
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pu64_pi16
	.private_extern _A__sub_pu64_pi16
_A__sub_pu64_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _A__sub_vu64_vu16
	.private_extern _A__sub_vu64_vu16
_A__sub_vu64_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_A(%rip), %rdx
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pu64_pu16
	.private_extern _A__sub_pu64_pu16
_A__sub_pu64_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _A__sub_vu64_vi32
	.private_extern _A__sub_vu64_vi32
_A__sub_vu64_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_A(%rip), %rdx
	movl	112+_MM_A(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pu64_pi32
	.private_extern _A__sub_pu64_pi32
_A__sub_pu64_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _A__sub_vu64_vu32
	.private_extern _A__sub_vu64_vu32
_A__sub_vu64_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_A(%rip), %rdx
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pu64_pu32
	.private_extern _A__sub_pu64_pu32
_A__sub_pu64_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _A__sub_vu64_vi64
	.private_extern _A__sub_vu64_vi64
_A__sub_vu64_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_A(%rip), %rdx
	movq	120+_MM_A(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
	ret
.globl _A__sub_pu64_pi64
	.private_extern _A__sub_pu64_pi64
_A__sub_pu64_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _A__sub_vu64_vu64
	.private_extern _A__sub_vu64_vu64
_A__sub_vu64_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _A__sub_pu64_pu64
	.private_extern _A__sub_pu64_pu64
_A__sub_pu64_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	subq	%rdx, %rax
	leave
	ret
.globl _A__and_vu64_vi8
	.private_extern _A__and_vu64_vi8
_A__and_vu64_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	128+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pu64_pi8
	.private_extern _A__and_pu64_pi8
_A__and_pu64_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vu64_vu8
	.private_extern _A__and_vu64_vu8
_A__and_vu64_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	128+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pu64_pu8
	.private_extern _A__and_pu64_pu8
_A__and_pu64_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vu64_vi16
	.private_extern _A__and_vu64_vi16
_A__and_vu64_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	128+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pu64_pi16
	.private_extern _A__and_pu64_pi16
_A__and_pu64_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vu64_vu16
	.private_extern _A__and_vu64_vu16
_A__and_vu64_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	128+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pu64_pu16
	.private_extern _A__and_pu64_pu16
_A__and_pu64_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vu64_vi32
	.private_extern _A__and_vu64_vi32
_A__and_vu64_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	128+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pu64_pi32
	.private_extern _A__and_pu64_pi32
_A__and_pu64_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vu64_vu32
	.private_extern _A__and_vu64_vu32
_A__and_vu64_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	128+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pu64_pu32
	.private_extern _A__and_pu64_pu32
_A__and_pu64_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vu64_vi64
	.private_extern _A__and_vu64_vi64
_A__and_vu64_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_A(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_A(%rip), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_pu64_pi64
	.private_extern _A__and_pu64_pi64
_A__and_pu64_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__and_vu64_vu64
	.private_extern _A__and_vu64_vu64
_A__and_vu64_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_A(%rip), %rax
	leave
	ret
.globl _A__and_pu64_pu64
	.private_extern _A__and_pu64_pu64
_A__and_pu64_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	andq	%rdx, %rax
	leave
	ret
.globl _A__or_vu64_vi8
	.private_extern _A__or_vu64_vi8
_A__or_vu64_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	128+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pu64_pi8
	.private_extern _A__or_pu64_pi8
_A__or_pu64_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vu64_vu8
	.private_extern _A__or_vu64_vu8
_A__or_vu64_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	128+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pu64_pu8
	.private_extern _A__or_pu64_pu8
_A__or_pu64_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vu64_vi16
	.private_extern _A__or_vu64_vi16
_A__or_vu64_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	128+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pu64_pi16
	.private_extern _A__or_pu64_pi16
_A__or_pu64_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vu64_vu16
	.private_extern _A__or_vu64_vu16
_A__or_vu64_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	128+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pu64_pu16
	.private_extern _A__or_pu64_pu16
_A__or_pu64_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vu64_vi32
	.private_extern _A__or_vu64_vi32
_A__or_vu64_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	128+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pu64_pi32
	.private_extern _A__or_pu64_pi32
_A__or_pu64_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vu64_vu32
	.private_extern _A__or_vu64_vu32
_A__or_vu64_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	128+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pu64_pu32
	.private_extern _A__or_pu64_pu32
_A__or_pu64_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vu64_vi64
	.private_extern _A__or_vu64_vi64
_A__or_vu64_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_A(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_A(%rip), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_pu64_pi64
	.private_extern _A__or_pu64_pi64
_A__or_pu64_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__or_vu64_vu64
	.private_extern _A__or_vu64_vu64
_A__or_vu64_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_A(%rip), %rax
	leave
	ret
.globl _A__or_pu64_pu64
	.private_extern _A__or_pu64_pu64
_A__or_pu64_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	orq	%rdx, %rax
	leave
	ret
.globl _A__xor_vu64_vi8
	.private_extern _A__xor_vu64_vi8
_A__xor_vu64_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	128+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pu64_pi8
	.private_extern _A__xor_pu64_pi8
_A__xor_pu64_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vu64_vu8
	.private_extern _A__xor_vu64_vu8
_A__xor_vu64_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	128+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pu64_pu8
	.private_extern _A__xor_pu64_pu8
_A__xor_pu64_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vu64_vi16
	.private_extern _A__xor_vu64_vi16
_A__xor_vu64_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	128+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pu64_pi16
	.private_extern _A__xor_pu64_pi16
_A__xor_pu64_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vu64_vu16
	.private_extern _A__xor_vu64_vu16
_A__xor_vu64_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	128+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pu64_pu16
	.private_extern _A__xor_pu64_pu16
_A__xor_pu64_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vu64_vi32
	.private_extern _A__xor_vu64_vi32
_A__xor_vu64_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	128+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pu64_pi32
	.private_extern _A__xor_pu64_pi32
_A__xor_pu64_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vu64_vu32
	.private_extern _A__xor_vu64_vu32
_A__xor_vu64_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	128+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pu64_pu32
	.private_extern _A__xor_pu64_pu32
_A__xor_pu64_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edx
	movq	-24(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vu64_vi64
	.private_extern _A__xor_vu64_vi64
_A__xor_vu64_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_A(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_A(%rip), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_pu64_pi64
	.private_extern _A__xor_pu64_pi64
_A__xor_pu64_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__xor_vu64_vu64
	.private_extern _A__xor_vu64_vu64
_A__xor_vu64_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	leave
	ret
.globl _A__xor_pu64_pu64
	.private_extern _A__xor_pu64_pu64
_A__xor_pu64_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rax
	xorq	%rdx, %rax
	leave
	ret
.globl _A__mult_vu64_vi8
	.private_extern _A__mult_vu64_vi8
_A__mult_vu64_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdx
	movq	128+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pu64_pi8
	.private_extern _A__mult_pu64_pi8
_A__mult_pu64_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _A__mult_vu64_vu8
	.private_extern _A__mult_vu64_vu8
_A__mult_vu64_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edx
	movq	128+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pu64_pu8
	.private_extern _A__mult_pu64_pu8
_A__mult_pu64_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _A__mult_vu64_vi16
	.private_extern _A__mult_vu64_vi16
_A__mult_vu64_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdx
	movq	128+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pu64_pi16
	.private_extern _A__mult_pu64_pi16
_A__mult_pu64_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _A__mult_vu64_vu16
	.private_extern _A__mult_vu64_vu16
_A__mult_vu64_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edx
	movq	128+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pu64_pu16
	.private_extern _A__mult_pu64_pu16
_A__mult_pu64_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _A__mult_vu64_vi32
	.private_extern _A__mult_vu64_vi32
_A__mult_vu64_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdx
	movq	128+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pu64_pi32
	.private_extern _A__mult_pu64_pi32
_A__mult_pu64_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _A__mult_vu64_vu32
	.private_extern _A__mult_vu64_vu32
_A__mult_vu64_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edx
	movq	128+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pu64_pu32
	.private_extern _A__mult_pu64_pu32
_A__mult_pu64_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %eax
	imulq	-24(%rbp), %rax
	leave
	ret
.globl _A__mult_vu64_vi64
	.private_extern _A__mult_vu64_vi64
_A__mult_vu64_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_A(%rip), %rdx
	movq	120+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pu64_pi64
	.private_extern _A__mult_pu64_pi64
_A__mult_pu64_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _A__mult_vu64_vu64
	.private_extern _A__mult_vu64_vu64
_A__mult_vu64_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	128+_MM_A(%rip), %rdx
	movq	128+_MM_A(%rip), %rax
	imulq	%rdx, %rax
	leave
	ret
.globl _A__mult_pu64_pu64
	.private_extern _A__mult_pu64_pu64
_A__mult_pu64_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	imulq	-32(%rbp), %rax
	leave
	ret
.globl _A__div_vu64_vi8
	.private_extern _A__div_vu64_vi8
_A__div_vu64_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	128+_MM_A(%rip), %rsi
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_pu64_pi8
	.private_extern _A__div_pu64_pi8
_A__div_pu64_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdi
	movq	-24(%rbp), %rsi
	call	_m3_divL
	leave
	ret
.globl _A__div_vu64_vu8
	.private_extern _A__div_vu64_vu8
_A__div_vu64_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	128+_MM_A(%rip), %rsi
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_pu64_pu8
	.private_extern _A__div_pu64_pu8
_A__div_pu64_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edi
	movq	-24(%rbp), %rsi
	call	_m3_divL
	leave
	ret
.globl _A__div_vu64_vi16
	.private_extern _A__div_vu64_vi16
_A__div_vu64_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	128+_MM_A(%rip), %rsi
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_pu64_pi16
	.private_extern _A__div_pu64_pi16
_A__div_pu64_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdi
	movq	-24(%rbp), %rsi
	call	_m3_divL
	leave
	ret
.globl _A__div_vu64_vu16
	.private_extern _A__div_vu64_vu16
_A__div_vu64_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	128+_MM_A(%rip), %rsi
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_pu64_pu16
	.private_extern _A__div_pu64_pu16
_A__div_pu64_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edi
	movq	-24(%rbp), %rsi
	call	_m3_divL
	leave
	ret
.globl _A__div_vu64_vi32
	.private_extern _A__div_vu64_vi32
_A__div_vu64_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	128+_MM_A(%rip), %rsi
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_pu64_pi32
	.private_extern _A__div_pu64_pi32
_A__div_pu64_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdi
	movq	-24(%rbp), %rsi
	call	_m3_divL
	leave
	ret
.globl _A__div_vu64_vu32
	.private_extern _A__div_vu64_vu32
_A__div_vu64_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	128+_MM_A(%rip), %rsi
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edi
	call	_m3_divL
	leave
	ret
.globl _A__div_pu64_pu32
	.private_extern _A__div_pu64_pu32
_A__div_pu64_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edi
	movq	-24(%rbp), %rsi
	call	_m3_divL
	leave
	ret
.globl _A__div_vu64_vi64
	.private_extern _A__div_vu64_vi64
_A__div_vu64_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	128+_MM_A(%rip), %rsi
	movq	120+_MM_A(%rip), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_pu64_pi64
	.private_extern _A__div_pu64_pi64
_A__div_pu64_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rsi
	movq	-32(%rbp), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_vu64_vu64
	.private_extern _A__div_vu64_vu64
_A__div_vu64_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	128+_MM_A(%rip), %rsi
	movq	128+_MM_A(%rip), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__div_pu64_pu64
	.private_extern _A__div_pu64_pu64
_A__div_pu64_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rsi
	movq	-32(%rbp), %rdi
	call	_m3_divL
	leave
	ret
.globl _A__mod_vu64_vi8
	.private_extern _A__mod_vu64_vi8
_A__mod_vu64_vi8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	128+_MM_A(%rip), %rsi
	movzbl	104+_MM_A(%rip), %eax
	movsbq	%al,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pu64_pi8
	.private_extern _A__mod_pu64_pi8
_A__mod_pu64_pi8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp),%rdi
	movq	-24(%rbp), %rsi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vu64_vu8
	.private_extern _A__mod_vu64_vu8
_A__mod_vu64_vu8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	128+_MM_A(%rip), %rsi
	movzbl	105+_MM_A(%rip), %eax
	movzbl	%al, %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pu64_pu8
	.private_extern _A__mod_pu64_pu8
_A__mod_pu64_pu8:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %edi
	movq	-24(%rbp), %rsi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vu64_vi16
	.private_extern _A__mod_vu64_vi16
_A__mod_vu64_vi16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	128+_MM_A(%rip), %rsi
	movzwl	106+_MM_A(%rip), %eax
	movswq	%ax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pu64_pi16
	.private_extern _A__mod_pu64_pi16
_A__mod_pu64_pi16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp),%rdi
	movq	-24(%rbp), %rsi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vu64_vu16
	.private_extern _A__mod_vu64_vu16
_A__mod_vu64_vu16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	128+_MM_A(%rip), %rsi
	movzwl	108+_MM_A(%rip), %eax
	movzwl	%ax, %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pu64_pu16
	.private_extern _A__mod_pu64_pu16
_A__mod_pu64_pu16:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %edi
	movq	-24(%rbp), %rsi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vu64_vi32
	.private_extern _A__mod_vu64_vi32
_A__mod_vu64_vi32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	128+_MM_A(%rip), %rsi
	movl	112+_MM_A(%rip), %eax
	movslq	%eax,%rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pu64_pi32
	.private_extern _A__mod_pu64_pi32
_A__mod_pu64_pi32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax,%rdi
	movq	-24(%rbp), %rsi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vu64_vu32
	.private_extern _A__mod_vu64_vu32
_A__mod_vu64_vu32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	128+_MM_A(%rip), %rsi
	movl	116+_MM_A(%rip), %eax
	mov	%eax, %edi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pu64_pu32
	.private_extern _A__mod_pu64_pu32
_A__mod_pu64_pu32:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %edi
	movq	-24(%rbp), %rsi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vu64_vi64
	.private_extern _A__mod_vu64_vi64
_A__mod_vu64_vi64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	128+_MM_A(%rip), %rsi
	movq	120+_MM_A(%rip), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pu64_pi64
	.private_extern _A__mod_pu64_pi64
_A__mod_pu64_pi64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rsi
	movq	-32(%rbp), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_vu64_vu64
	.private_extern _A__mod_vu64_vu64
_A__mod_vu64_vu64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	128+_MM_A(%rip), %rsi
	movq	128+_MM_A(%rip), %rdi
	call	_m3_modL
	leave
	ret
.globl _A__mod_pu64_pu64
	.private_extern _A__mod_pu64_pu64
_A__mod_pu64_pu64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rsi
	movq	-32(%rbp), %rdi
	call	_m3_modL
	leave
	ret
.globl _A_M3
_A_M3:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	leaq	_MM_A(%rip), %rax
	leave
	ret
	.const_data
	.align 5
_L_1:
	.ascii "A_M3"
	.space 1
	.ascii "mod_pu64_pu64"
	.space 1
	.ascii "mod_vu64_vu64"
	.space 1
	.ascii "mod_pu64_pi64"
	.space 1
	.ascii "mod_vu64_vi64"
	.space 1
	.ascii "mod_pu64_pu32"
	.space 1
	.ascii "mod_vu64_vu32"
	.space 1
	.ascii "mod_pu64_pi32"
	.space 1
	.ascii "mod_vu64_vi32"
	.space 1
	.ascii "mod_pu64_pu16"
	.space 1
	.ascii "mod_vu64_vu16"
	.space 1
	.ascii "mod_pu64_pi16"
	.space 1
	.ascii "mod_vu64_vi16"
	.space 1
	.ascii "mod_pu64_pu8"
	.space 1
	.ascii "mod_vu64_vu8"
	.space 1
	.ascii "mod_pu64_pi8"
	.space 1
	.ascii "mod_vu64_vi8"
	.space 1
	.ascii "div_pu64_pu64"
	.space 1
	.ascii "div_vu64_vu64"
	.space 1
	.ascii "div_pu64_pi64"
	.space 1
	.ascii "div_vu64_vi64"
	.space 1
	.ascii "div_pu64_pu32"
	.space 1
	.ascii "div_vu64_vu32"
	.space 1
	.ascii "div_pu64_pi32"
	.space 1
	.ascii "div_vu64_vi32"
	.space 1
	.ascii "div_pu64_pu16"
	.space 1
	.ascii "div_vu64_vu16"
	.space 1
	.ascii "div_pu64_pi16"
	.space 1
	.ascii "div_vu64_vi16"
	.space 1
	.ascii "div_pu64_pu8"
	.space 1
	.ascii "div_vu64_vu8"
	.space 1
	.ascii "div_pu64_pi8"
	.space 1
	.ascii "div_vu64_vi8"
	.space 1
	.ascii "mult_pu64_pu64"
	.space 1
	.ascii "mult_vu64_vu64"
	.space 1
	.ascii "mult_pu64_pi64"
	.space 1
	.ascii "mult_vu64_vi64"
	.space 1
	.ascii "mult_pu64_pu32"
	.space 1
	.ascii "mult_vu64_vu32"
	.space 1
	.ascii "mult_pu64_pi32"
	.space 1
	.ascii "mult_vu64_vi32"
	.space 1
	.ascii "mult_pu64_pu16"
	.space 1
	.ascii "mult_vu64_vu16"
	.space 1
	.ascii "mult_pu64_pi16"
	.space 1
	.ascii "mult_vu64_vi16"
	.space 1
	.ascii "mult_pu64_pu8"
	.space 1
	.ascii "mult_vu64_vu8"
	.space 1
	.ascii "mult_pu64_pi8"
	.space 1
	.ascii "mult_vu64_vi8"
	.space 1
	.ascii "xor_pu64_pu64"
	.space 1
	.ascii "xor_vu64_vu64"
	.space 1
	.ascii "xor_pu64_pi64"
	.space 1
	.ascii "xor_vu64_vi64"
	.space 1
	.ascii "xor_pu64_pu32"
	.space 1
	.ascii "xor_vu64_vu32"
	.space 1
	.ascii "xor_pu64_pi32"
	.space 1
	.ascii "xor_vu64_vi32"
	.space 1
	.ascii "xor_pu64_pu16"
	.space 1
	.ascii "xor_vu64_vu16"
	.space 1
	.ascii "xor_pu64_pi16"
	.space 1
	.ascii "xor_vu64_vi16"
	.space 1
	.ascii "xor_pu64_pu8"
	.space 1
	.ascii "xor_vu64_vu8"
	.space 1
	.ascii "xor_pu64_pi8"
	.space 1
	.ascii "xor_vu64_vi8"
	.space 1
	.ascii "or_pu64_pu64"
	.space 1
	.ascii "or_vu64_vu64"
	.space 1
	.ascii "or_pu64_pi64"
	.space 1
	.ascii "or_vu64_vi64"
	.space 1
	.ascii "or_pu64_pu32"
	.space 1
	.ascii "or_vu64_vu32"
	.space 1
	.ascii "or_pu64_pi32"
	.space 1
	.ascii "or_vu64_vi32"
	.space 1
	.ascii "or_pu64_pu16"
	.space 1
	.ascii "or_vu64_vu16"
	.space 1
	.ascii "or_pu64_pi16"
	.space 1
	.ascii "or_vu64_vi16"
	.space 1
	.ascii "or_pu64_pu8"
	.space 1
	.ascii "or_vu64_vu8"
	.space 1
	.ascii "or_pu64_pi8"
	.space 1
	.ascii "or_vu64_vi8"
	.space 1
	.ascii "and_pu64_pu64"
	.space 1
	.ascii "and_vu64_vu64"
	.space 1
	.ascii "and_pu64_pi64"
	.space 1
	.ascii "and_vu64_vi64"
	.space 1
	.ascii "and_pu64_pu32"
	.space 1
	.ascii "and_vu64_vu32"
	.space 1
	.ascii "and_pu64_pi32"
	.space 1
	.ascii "and_vu64_vi32"
	.space 1
	.ascii "and_pu64_pu16"
	.space 1
	.ascii "and_vu64_vu16"
	.space 1
	.ascii "and_pu64_pi16"
	.space 1
	.ascii "and_vu64_vi16"
	.space 1
	.ascii "and_pu64_pu8"
	.space 1
	.ascii "and_vu64_vu8"
	.space 1
	.ascii "and_pu64_pi8"
	.space 1
	.ascii "and_vu64_vi8"
	.space 1
	.ascii "sub_pu64_pu64"
	.space 1
	.ascii "sub_vu64_vu64"
	.space 1
	.ascii "sub_pu64_pi64"
	.space 1
	.ascii "sub_vu64_vi64"
	.space 1
	.ascii "sub_pu64_pu32"
	.space 1
	.ascii "sub_vu64_vu32"
	.space 1
	.ascii "sub_pu64_pi32"
	.space 1
	.ascii "sub_vu64_vi32"
	.space 1
	.ascii "sub_pu64_pu16"
	.space 1
	.ascii "sub_vu64_vu16"
	.space 1
	.ascii "sub_pu64_pi16"
	.space 1
	.ascii "sub_vu64_vi16"
	.space 1
	.ascii "sub_pu64_pu8"
	.space 1
	.ascii "sub_vu64_vu8"
	.space 1
	.ascii "sub_pu64_pi8"
	.space 1
	.ascii "sub_vu64_vi8"
	.space 1
	.ascii "add_pu64_pu64"
	.space 1
	.ascii "add_vu64_vu64"
	.space 1
	.ascii "add_pu64_pi64"
	.space 1
	.ascii "add_vu64_vi64"
	.space 1
	.ascii "add_pu64_pu32"
	.space 1
	.ascii "add_vu64_vu32"
	.space 1
	.ascii "add_pu64_pi32"
	.space 1
	.ascii "add_vu64_vi32"
	.space 1
	.ascii "add_pu64_pu16"
	.space 1
	.ascii "add_vu64_vu16"
	.space 1
	.ascii "add_pu64_pi16"
	.space 1
	.ascii "add_vu64_vi16"
	.space 1
	.ascii "add_pu64_pu8"
	.space 1
	.ascii "add_vu64_vu8"
	.space 1
	.ascii "add_pu64_pi8"
	.space 1
	.ascii "add_vu64_vi8"
	.space 1
	.ascii "ret_pu64_u64"
	.space 1
	.ascii "ret_pu64_i64"
	.space 1
	.ascii "ret_pu64_u32"
	.space 1
	.ascii "ret_pu64_i32"
	.space 1
	.ascii "ret_pu64_u16"
	.space 1
	.ascii "ret_pu64_i16"
	.space 1
	.ascii "ret_pu64_u8"
	.space 1
	.ascii "ret_pu64_i8"
	.space 1
	.ascii "ret_ku64"
	.space 1
	.ascii "ret_vu64"
	.space 1
	.ascii "mod_pi64_pu64"
	.space 1
	.ascii "mod_vi64_vu64"
	.space 1
	.ascii "mod_pi64_pi64"
	.space 1
	.ascii "mod_vi64_vi64"
	.space 1
	.ascii "mod_pi64_pu32"
	.space 1
	.ascii "mod_vi64_vu32"
	.space 1
	.ascii "mod_pi64_pi32"
	.space 1
	.ascii "mod_vi64_vi32"
	.space 1
	.ascii "mod_pi64_pu16"
	.space 1
	.ascii "mod_vi64_vu16"
	.space 1
	.ascii "mod_pi64_pi16"
	.space 1
	.ascii "mod_vi64_vi16"
	.space 1
	.ascii "mod_pi64_pu8"
	.space 1
	.ascii "mod_vi64_vu8"
	.space 1
	.ascii "mod_pi64_pi8"
	.space 1
	.ascii "mod_vi64_vi8"
	.space 1
	.ascii "div_pi64_pu64"
	.space 1
	.ascii "div_vi64_vu64"
	.space 1
	.ascii "div_pi64_pi64"
	.space 1
	.ascii "div_vi64_vi64"
	.space 1
	.ascii "div_pi64_pu32"
	.space 1
	.ascii "div_vi64_vu32"
	.space 1
	.ascii "div_pi64_pi32"
	.space 1
	.ascii "div_vi64_vi32"
	.space 1
	.ascii "div_pi64_pu16"
	.space 1
	.ascii "div_vi64_vu16"
	.space 1
	.ascii "div_pi64_pi16"
	.space 1
	.ascii "div_vi64_vi16"
	.space 1
	.ascii "div_pi64_pu8"
	.space 1
	.ascii "div_vi64_vu8"
	.space 1
	.ascii "div_pi64_pi8"
	.space 1
	.ascii "div_vi64_vi8"
	.space 1
	.ascii "mult_pi64_pu64"
	.space 1
	.ascii "mult_vi64_vu64"
	.space 1
	.ascii "mult_pi64_pi64"
	.space 1
	.ascii "mult_vi64_vi64"
	.space 1
	.ascii "mult_pi64_pu32"
	.space 1
	.ascii "mult_vi64_vu32"
	.space 1
	.ascii "mult_pi64_pi32"
	.space 1
	.ascii "mult_vi64_vi32"
	.space 1
	.ascii "mult_pi64_pu16"
	.space 1
	.ascii "mult_vi64_vu16"
	.space 1
	.ascii "mult_pi64_pi16"
	.space 1
	.ascii "mult_vi64_vi16"
	.space 1
	.ascii "mult_pi64_pu8"
	.space 1
	.ascii "mult_vi64_vu8"
	.space 1
	.ascii "mult_pi64_pi8"
	.space 1
	.ascii "mult_vi64_vi8"
	.space 1
	.ascii "xor_pi64_pu64"
	.space 1
	.ascii "xor_vi64_vu64"
	.space 1
	.ascii "xor_pi64_pi64"
	.space 1
	.ascii "xor_vi64_vi64"
	.space 1
	.ascii "xor_pi64_pu32"
	.space 1
	.ascii "xor_vi64_vu32"
	.space 1
	.ascii "xor_pi64_pi32"
	.space 1
	.ascii "xor_vi64_vi32"
	.space 1
	.ascii "xor_pi64_pu16"
	.space 1
	.ascii "xor_vi64_vu16"
	.space 1
	.ascii "xor_pi64_pi16"
	.space 1
	.ascii "xor_vi64_vi16"
	.space 1
	.ascii "xor_pi64_pu8"
	.space 1
	.ascii "xor_vi64_vu8"
	.space 1
	.ascii "xor_pi64_pi8"
	.space 1
	.ascii "xor_vi64_vi8"
	.space 1
	.ascii "or_pi64_pu64"
	.space 1
	.ascii "or_vi64_vu64"
	.space 1
	.ascii "or_pi64_pi64"
	.space 1
	.ascii "or_vi64_vi64"
	.space 1
	.ascii "or_pi64_pu32"
	.space 1
	.ascii "or_vi64_vu32"
	.space 1
	.ascii "or_pi64_pi32"
	.space 1
	.ascii "or_vi64_vi32"
	.space 1
	.ascii "or_pi64_pu16"
	.space 1
	.ascii "or_vi64_vu16"
	.space 1
	.ascii "or_pi64_pi16"
	.space 1
	.ascii "or_vi64_vi16"
	.space 1
	.ascii "or_pi64_pu8"
	.space 1
	.ascii "or_vi64_vu8"
	.space 1
	.ascii "or_pi64_pi8"
	.space 1
	.ascii "or_vi64_vi8"
	.space 1
	.ascii "and_pi64_pu64"
	.space 1
	.ascii "and_vi64_vu64"
	.space 1
	.ascii "and_pi64_pi64"
	.space 1
	.ascii "and_vi64_vi64"
	.space 1
	.ascii "and_pi64_pu32"
	.space 1
	.ascii "and_vi64_vu32"
	.space 1
	.ascii "and_pi64_pi32"
	.space 1
	.ascii "and_vi64_vi32"
	.space 1
	.ascii "and_pi64_pu16"
	.space 1
	.ascii "and_vi64_vu16"
	.space 1
	.ascii "and_pi64_pi16"
	.space 1
	.ascii "and_vi64_vi16"
	.space 1
	.ascii "and_pi64_pu8"
	.space 1
	.ascii "and_vi64_vu8"
	.space 1
	.ascii "and_pi64_pi8"
	.space 1
	.ascii "and_vi64_vi8"
	.space 1
	.ascii "sub_pi64_pu64"
	.space 1
	.ascii "sub_vi64_vu64"
	.space 1
	.ascii "sub_pi64_pi64"
	.space 1
	.ascii "sub_vi64_vi64"
	.space 1
	.ascii "sub_pi64_pu32"
	.space 1
	.ascii "sub_vi64_vu32"
	.space 1
	.ascii "sub_pi64_pi32"
	.space 1
	.ascii "sub_vi64_vi32"
	.space 1
	.ascii "sub_pi64_pu16"
	.space 1
	.ascii "sub_vi64_vu16"
	.space 1
	.ascii "sub_pi64_pi16"
	.space 1
	.ascii "sub_vi64_vi16"
	.space 1
	.ascii "sub_pi64_pu8"
	.space 1
	.ascii "sub_vi64_vu8"
	.space 1
	.ascii "sub_pi64_pi8"
	.space 1
	.ascii "sub_vi64_vi8"
	.space 1
	.ascii "add_pi64_pu64"
	.space 1
	.ascii "add_vi64_vu64"
	.space 1
	.ascii "add_pi64_pi64"
	.space 1
	.ascii "add_vi64_vi64"
	.space 1
	.ascii "add_pi64_pu32"
	.space 1
	.ascii "add_vi64_vu32"
	.space 1
	.ascii "add_pi64_pi32"
	.space 1
	.ascii "add_vi64_vi32"
	.space 1
	.ascii "add_pi64_pu16"
	.space 1
	.ascii "add_vi64_vu16"
	.space 1
	.ascii "add_pi64_pi16"
	.space 1
	.ascii "add_vi64_vi16"
	.space 1
	.ascii "add_pi64_pu8"
	.space 1
	.ascii "add_vi64_vu8"
	.space 1
	.ascii "add_pi64_pi8"
	.space 1
	.ascii "add_vi64_vi8"
	.space 1
	.ascii "ret_pi64_u64"
	.space 1
	.ascii "ret_pi64_i64"
	.space 1
	.ascii "ret_pi64_u32"
	.space 1
	.ascii "ret_pi64_i32"
	.space 1
	.ascii "ret_pi64_u16"
	.space 1
	.ascii "ret_pi64_i16"
	.space 1
	.ascii "ret_pi64_u8"
	.space 1
	.ascii "ret_pi64_i8"
	.space 1
	.ascii "ret_ki64"
	.space 1
	.ascii "ret_vi64"
	.space 1
	.ascii "mod_pu32_pu64"
	.space 1
	.ascii "mod_vu32_vu64"
	.space 1
	.ascii "mod_pu32_pi64"
	.space 1
	.ascii "mod_vu32_vi64"
	.space 1
	.ascii "mod_pu32_pu32"
	.space 1
	.ascii "mod_vu32_vu32"
	.space 1
	.ascii "mod_pu32_pi32"
	.space 1
	.ascii "mod_vu32_vi32"
	.space 1
	.ascii "mod_pu32_pu16"
	.space 1
	.ascii "mod_vu32_vu16"
	.space 1
	.ascii "mod_pu32_pi16"
	.space 1
	.ascii "mod_vu32_vi16"
	.space 1
	.ascii "mod_pu32_pu8"
	.space 1
	.ascii "mod_vu32_vu8"
	.space 1
	.ascii "mod_pu32_pi8"
	.space 1
	.ascii "mod_vu32_vi8"
	.space 1
	.ascii "div_pu32_pu64"
	.space 1
	.ascii "div_vu32_vu64"
	.space 1
	.ascii "div_pu32_pi64"
	.space 1
	.ascii "div_vu32_vi64"
	.space 1
	.ascii "div_pu32_pu32"
	.space 1
	.ascii "div_vu32_vu32"
	.space 1
	.ascii "div_pu32_pi32"
	.space 1
	.ascii "div_vu32_vi32"
	.space 1
	.ascii "div_pu32_pu16"
	.space 1
	.ascii "div_vu32_vu16"
	.space 1
	.ascii "div_pu32_pi16"
	.space 1
	.ascii "div_vu32_vi16"
	.space 1
	.ascii "div_pu32_pu8"
	.space 1
	.ascii "div_vu32_vu8"
	.space 1
	.ascii "div_pu32_pi8"
	.space 1
	.ascii "div_vu32_vi8"
	.space 1
	.ascii "mult_pu32_pu64"
	.space 1
	.ascii "mult_vu32_vu64"
	.space 1
	.ascii "mult_pu32_pi64"
	.space 1
	.ascii "mult_vu32_vi64"
	.space 1
	.ascii "mult_pu32_pu32"
	.space 1
	.ascii "mult_vu32_vu32"
	.space 1
	.ascii "mult_pu32_pi32"
	.space 1
	.ascii "mult_vu32_vi32"
	.space 1
	.ascii "mult_pu32_pu16"
	.space 1
	.ascii "mult_vu32_vu16"
	.space 1
	.ascii "mult_pu32_pi16"
	.space 1
	.ascii "mult_vu32_vi16"
	.space 1
	.ascii "mult_pu32_pu8"
	.space 1
	.ascii "mult_vu32_vu8"
	.space 1
	.ascii "mult_pu32_pi8"
	.space 1
	.ascii "mult_vu32_vi8"
	.space 1
	.ascii "xor_pu32_pu64"
	.space 1
	.ascii "xor_vu32_vu64"
	.space 1
	.ascii "xor_pu32_pi64"
	.space 1
	.ascii "xor_vu32_vi64"
	.space 1
	.ascii "xor_pu32_pu32"
	.space 1
	.ascii "xor_vu32_vu32"
	.space 1
	.ascii "xor_pu32_pi32"
	.space 1
	.ascii "xor_vu32_vi32"
	.space 1
	.ascii "xor_pu32_pu16"
	.space 1
	.ascii "xor_vu32_vu16"
	.space 1
	.ascii "xor_pu32_pi16"
	.space 1
	.ascii "xor_vu32_vi16"
	.space 1
	.ascii "xor_pu32_pu8"
	.space 1
	.ascii "xor_vu32_vu8"
	.space 1
	.ascii "xor_pu32_pi8"
	.space 1
	.ascii "xor_vu32_vi8"
	.space 1
	.ascii "or_pu32_pu64"
	.space 1
	.ascii "or_vu32_vu64"
	.space 1
	.ascii "or_pu32_pi64"
	.space 1
	.ascii "or_vu32_vi64"
	.space 1
	.ascii "or_pu32_pu32"
	.space 1
	.ascii "or_vu32_vu32"
	.space 1
	.ascii "or_pu32_pi32"
	.space 1
	.ascii "or_vu32_vi32"
	.space 1
	.ascii "or_pu32_pu16"
	.space 1
	.ascii "or_vu32_vu16"
	.space 1
	.ascii "or_pu32_pi16"
	.space 1
	.ascii "or_vu32_vi16"
	.space 1
	.ascii "or_pu32_pu8"
	.space 1
	.ascii "or_vu32_vu8"
	.space 1
	.ascii "or_pu32_pi8"
	.space 1
	.ascii "or_vu32_vi8"
	.space 1
	.ascii "and_pu32_pu64"
	.space 1
	.ascii "and_vu32_vu64"
	.space 1
	.ascii "and_pu32_pi64"
	.space 1
	.ascii "and_vu32_vi64"
	.space 1
	.ascii "and_pu32_pu32"
	.space 1
	.ascii "and_vu32_vu32"
	.space 1
	.ascii "and_pu32_pi32"
	.space 1
	.ascii "and_vu32_vi32"
	.space 1
	.ascii "and_pu32_pu16"
	.space 1
	.ascii "and_vu32_vu16"
	.space 1
	.ascii "and_pu32_pi16"
	.space 1
	.ascii "and_vu32_vi16"
	.space 1
	.ascii "and_pu32_pu8"
	.space 1
	.ascii "and_vu32_vu8"
	.space 1
	.ascii "and_pu32_pi8"
	.space 1
	.ascii "and_vu32_vi8"
	.space 1
	.ascii "sub_pu32_pu64"
	.space 1
	.ascii "sub_vu32_vu64"
	.space 1
	.ascii "sub_pu32_pi64"
	.space 1
	.ascii "sub_vu32_vi64"
	.space 1
	.ascii "sub_pu32_pu32"
	.space 1
	.ascii "sub_vu32_vu32"
	.space 1
	.ascii "sub_pu32_pi32"
	.space 1
	.ascii "sub_vu32_vi32"
	.space 1
	.ascii "sub_pu32_pu16"
	.space 1
	.ascii "sub_vu32_vu16"
	.space 1
	.ascii "sub_pu32_pi16"
	.space 1
	.ascii "sub_vu32_vi16"
	.space 1
	.ascii "sub_pu32_pu8"
	.space 1
	.ascii "sub_vu32_vu8"
	.space 1
	.ascii "sub_pu32_pi8"
	.space 1
	.ascii "sub_vu32_vi8"
	.space 1
	.ascii "add_pu32_pu64"
	.space 1
	.ascii "add_vu32_vu64"
	.space 1
	.ascii "add_pu32_pi64"
	.space 1
	.ascii "add_vu32_vi64"
	.space 1
	.ascii "add_pu32_pu32"
	.space 1
	.ascii "add_vu32_vu32"
	.space 1
	.ascii "add_pu32_pi32"
	.space 1
	.ascii "add_vu32_vi32"
	.space 1
	.ascii "add_pu32_pu16"
	.space 1
	.ascii "add_vu32_vu16"
	.space 1
	.ascii "add_pu32_pi16"
	.space 1
	.ascii "add_vu32_vi16"
	.space 1
	.ascii "add_pu32_pu8"
	.space 1
	.ascii "add_vu32_vu8"
	.space 1
	.ascii "add_pu32_pi8"
	.space 1
	.ascii "add_vu32_vi8"
	.space 1
	.ascii "ret_pu32_u64"
	.space 1
	.ascii "ret_pu32_i64"
	.space 1
	.ascii "ret_pu32_u32"
	.space 1
	.ascii "ret_pu32_i32"
	.space 1
	.ascii "ret_pu32_u16"
	.space 1
	.ascii "ret_pu32_i16"
	.space 1
	.ascii "ret_pu32_u8"
	.space 1
	.ascii "ret_pu32_i8"
	.space 1
	.ascii "ret_ku32"
	.space 1
	.ascii "ret_vu32"
	.space 1
	.ascii "mod_pi32_pu64"
	.space 1
	.ascii "mod_vi32_vu64"
	.space 1
	.ascii "mod_pi32_pi64"
	.space 1
	.ascii "mod_vi32_vi64"
	.space 1
	.ascii "mod_pi32_pu32"
	.space 1
	.ascii "mod_vi32_vu32"
	.space 1
	.ascii "mod_pi32_pi32"
	.space 1
	.ascii "mod_vi32_vi32"
	.space 1
	.ascii "mod_pi32_pu16"
	.space 1
	.ascii "mod_vi32_vu16"
	.space 1
	.ascii "mod_pi32_pi16"
	.space 1
	.ascii "mod_vi32_vi16"
	.space 1
	.ascii "mod_pi32_pu8"
	.space 1
	.ascii "mod_vi32_vu8"
	.space 1
	.ascii "mod_pi32_pi8"
	.space 1
	.ascii "mod_vi32_vi8"
	.space 1
	.ascii "div_pi32_pu64"
	.space 1
	.ascii "div_vi32_vu64"
	.space 1
	.ascii "div_pi32_pi64"
	.space 1
	.ascii "div_vi32_vi64"
	.space 1
	.ascii "div_pi32_pu32"
	.space 1
	.ascii "div_vi32_vu32"
	.space 1
	.ascii "div_pi32_pi32"
	.space 1
	.ascii "div_vi32_vi32"
	.space 1
	.ascii "div_pi32_pu16"
	.space 1
	.ascii "div_vi32_vu16"
	.space 1
	.ascii "div_pi32_pi16"
	.space 1
	.ascii "div_vi32_vi16"
	.space 1
	.ascii "div_pi32_pu8"
	.space 1
	.ascii "div_vi32_vu8"
	.space 1
	.ascii "div_pi32_pi8"
	.space 1
	.ascii "div_vi32_vi8"
	.space 1
	.ascii "mult_pi32_pu64"
	.space 1
	.ascii "mult_vi32_vu64"
	.space 1
	.ascii "mult_pi32_pi64"
	.space 1
	.ascii "mult_vi32_vi64"
	.space 1
	.ascii "mult_pi32_pu32"
	.space 1
	.ascii "mult_vi32_vu32"
	.space 1
	.ascii "mult_pi32_pi32"
	.space 1
	.ascii "mult_vi32_vi32"
	.space 1
	.ascii "mult_pi32_pu16"
	.space 1
	.ascii "mult_vi32_vu16"
	.space 1
	.ascii "mult_pi32_pi16"
	.space 1
	.ascii "mult_vi32_vi16"
	.space 1
	.ascii "mult_pi32_pu8"
	.space 1
	.ascii "mult_vi32_vu8"
	.space 1
	.ascii "mult_pi32_pi8"
	.space 1
	.ascii "mult_vi32_vi8"
	.space 1
	.ascii "xor_pi32_pu64"
	.space 1
	.ascii "xor_vi32_vu64"
	.space 1
	.ascii "xor_pi32_pi64"
	.space 1
	.ascii "xor_vi32_vi64"
	.space 1
	.ascii "xor_pi32_pu32"
	.space 1
	.ascii "xor_vi32_vu32"
	.space 1
	.ascii "xor_pi32_pi32"
	.space 1
	.ascii "xor_vi32_vi32"
	.space 1
	.ascii "xor_pi32_pu16"
	.space 1
	.ascii "xor_vi32_vu16"
	.space 1
	.ascii "xor_pi32_pi16"
	.space 1
	.ascii "xor_vi32_vi16"
	.space 1
	.ascii "xor_pi32_pu8"
	.space 1
	.ascii "xor_vi32_vu8"
	.space 1
	.ascii "xor_pi32_pi8"
	.space 1
	.ascii "xor_vi32_vi8"
	.space 1
	.ascii "or_pi32_pu64"
	.space 1
	.ascii "or_vi32_vu64"
	.space 1
	.ascii "or_pi32_pi64"
	.space 1
	.ascii "or_vi32_vi64"
	.space 1
	.ascii "or_pi32_pu32"
	.space 1
	.ascii "or_vi32_vu32"
	.space 1
	.ascii "or_pi32_pi32"
	.space 1
	.ascii "or_vi32_vi32"
	.space 1
	.ascii "or_pi32_pu16"
	.space 1
	.ascii "or_vi32_vu16"
	.space 1
	.ascii "or_pi32_pi16"
	.space 1
	.ascii "or_vi32_vi16"
	.space 1
	.ascii "or_pi32_pu8"
	.space 1
	.ascii "or_vi32_vu8"
	.space 1
	.ascii "or_pi32_pi8"
	.space 1
	.ascii "or_vi32_vi8"
	.space 1
	.ascii "and_pi32_pu64"
	.space 1
	.ascii "and_vi32_vu64"
	.space 1
	.ascii "and_pi32_pi64"
	.space 1
	.ascii "and_vi32_vi64"
	.space 1
	.ascii "and_pi32_pu32"
	.space 1
	.ascii "and_vi32_vu32"
	.space 1
	.ascii "and_pi32_pi32"
	.space 1
	.ascii "and_vi32_vi32"
	.space 1
	.ascii "and_pi32_pu16"
	.space 1
	.ascii "and_vi32_vu16"
	.space 1
	.ascii "and_pi32_pi16"
	.space 1
	.ascii "and_vi32_vi16"
	.space 1
	.ascii "and_pi32_pu8"
	.space 1
	.ascii "and_vi32_vu8"
	.space 1
	.ascii "and_pi32_pi8"
	.space 1
	.ascii "and_vi32_vi8"
	.space 1
	.ascii "sub_pi32_pu64"
	.space 1
	.ascii "sub_vi32_vu64"
	.space 1
	.ascii "sub_pi32_pi64"
	.space 1
	.ascii "sub_vi32_vi64"
	.space 1
	.ascii "sub_pi32_pu32"
	.space 1
	.ascii "sub_vi32_vu32"
	.space 1
	.ascii "sub_pi32_pi32"
	.space 1
	.ascii "sub_vi32_vi32"
	.space 1
	.ascii "sub_pi32_pu16"
	.space 1
	.ascii "sub_vi32_vu16"
	.space 1
	.ascii "sub_pi32_pi16"
	.space 1
	.ascii "sub_vi32_vi16"
	.space 1
	.ascii "sub_pi32_pu8"
	.space 1
	.ascii "sub_vi32_vu8"
	.space 1
	.ascii "sub_pi32_pi8"
	.space 1
	.ascii "sub_vi32_vi8"
	.space 1
	.ascii "add_pi32_pu64"
	.space 1
	.ascii "add_vi32_vu64"
	.space 1
	.ascii "add_pi32_pi64"
	.space 1
	.ascii "add_vi32_vi64"
	.space 1
	.ascii "add_pi32_pu32"
	.space 1
	.ascii "add_vi32_vu32"
	.space 1
	.ascii "add_pi32_pi32"
	.space 1
	.ascii "add_vi32_vi32"
	.space 1
	.ascii "add_pi32_pu16"
	.space 1
	.ascii "add_vi32_vu16"
	.space 1
	.ascii "add_pi32_pi16"
	.space 1
	.ascii "add_vi32_vi16"
	.space 1
	.ascii "add_pi32_pu8"
	.space 1
	.ascii "add_vi32_vu8"
	.space 1
	.ascii "add_pi32_pi8"
	.space 1
	.ascii "add_vi32_vi8"
	.space 1
	.ascii "ret_pi32_u64"
	.space 1
	.ascii "ret_pi32_i64"
	.space 1
	.ascii "ret_pi32_u32"
	.space 1
	.ascii "ret_pi32_i32"
	.space 1
	.ascii "ret_pi32_u16"
	.space 1
	.ascii "ret_pi32_i16"
	.space 1
	.ascii "ret_pi32_u8"
	.space 1
	.ascii "ret_pi32_i8"
	.space 1
	.ascii "ret_ki32"
	.space 1
	.ascii "ret_vi32"
	.space 1
	.ascii "mod_pu16_pu64"
	.space 1
	.ascii "mod_vu16_vu64"
	.space 1
	.ascii "mod_pu16_pi64"
	.space 1
	.ascii "mod_vu16_vi64"
	.space 1
	.ascii "mod_pu16_pu32"
	.space 1
	.ascii "mod_vu16_vu32"
	.space 1
	.ascii "mod_pu16_pi32"
	.space 1
	.ascii "mod_vu16_vi32"
	.space 1
	.ascii "mod_pu16_pu16"
	.space 1
	.ascii "mod_vu16_vu16"
	.space 1
	.ascii "mod_pu16_pi16"
	.space 1
	.ascii "mod_vu16_vi16"
	.space 1
	.ascii "mod_pu16_pu8"
	.space 1
	.ascii "mod_vu16_vu8"
	.space 1
	.ascii "mod_pu16_pi8"
	.space 1
	.ascii "mod_vu16_vi8"
	.space 1
	.ascii "div_pu16_pu64"
	.space 1
	.ascii "div_vu16_vu64"
	.space 1
	.ascii "div_pu16_pi64"
	.space 1
	.ascii "div_vu16_vi64"
	.space 1
	.ascii "div_pu16_pu32"
	.space 1
	.ascii "div_vu16_vu32"
	.space 1
	.ascii "div_pu16_pi32"
	.space 1
	.ascii "div_vu16_vi32"
	.space 1
	.ascii "div_pu16_pu16"
	.space 1
	.ascii "div_vu16_vu16"
	.space 1
	.ascii "div_pu16_pi16"
	.space 1
	.ascii "div_vu16_vi16"
	.space 1
	.ascii "div_pu16_pu8"
	.space 1
	.ascii "div_vu16_vu8"
	.space 1
	.ascii "div_pu16_pi8"
	.space 1
	.ascii "div_vu16_vi8"
	.space 1
	.ascii "mult_pu16_pu64"
	.space 1
	.ascii "mult_vu16_vu64"
	.space 1
	.ascii "mult_pu16_pi64"
	.space 1
	.ascii "mult_vu16_vi64"
	.space 1
	.ascii "mult_pu16_pu32"
	.space 1
	.ascii "mult_vu16_vu32"
	.space 1
	.ascii "mult_pu16_pi32"
	.space 1
	.ascii "mult_vu16_vi32"
	.space 1
	.ascii "mult_pu16_pu16"
	.space 1
	.ascii "mult_vu16_vu16"
	.space 1
	.ascii "mult_pu16_pi16"
	.space 1
	.ascii "mult_vu16_vi16"
	.space 1
	.ascii "mult_pu16_pu8"
	.space 1
	.ascii "mult_vu16_vu8"
	.space 1
	.ascii "mult_pu16_pi8"
	.space 1
	.ascii "mult_vu16_vi8"
	.space 1
	.ascii "xor_pu16_pu64"
	.space 1
	.ascii "xor_vu16_vu64"
	.space 1
	.ascii "xor_pu16_pi64"
	.space 1
	.ascii "xor_vu16_vi64"
	.space 1
	.ascii "xor_pu16_pu32"
	.space 1
	.ascii "xor_vu16_vu32"
	.space 1
	.ascii "xor_pu16_pi32"
	.space 1
	.ascii "xor_vu16_vi32"
	.space 1
	.ascii "xor_pu16_pu16"
	.space 1
	.ascii "xor_vu16_vu16"
	.space 1
	.ascii "xor_pu16_pi16"
	.space 1
	.ascii "xor_vu16_vi16"
	.space 1
	.ascii "xor_pu16_pu8"
	.space 1
	.ascii "xor_vu16_vu8"
	.space 1
	.ascii "xor_pu16_pi8"
	.space 1
	.ascii "xor_vu16_vi8"
	.space 1
	.ascii "or_pu16_pu64"
	.space 1
	.ascii "or_vu16_vu64"
	.space 1
	.ascii "or_pu16_pi64"
	.space 1
	.ascii "or_vu16_vi64"
	.space 1
	.ascii "or_pu16_pu32"
	.space 1
	.ascii "or_vu16_vu32"
	.space 1
	.ascii "or_pu16_pi32"
	.space 1
	.ascii "or_vu16_vi32"
	.space 1
	.ascii "or_pu16_pu16"
	.space 1
	.ascii "or_vu16_vu16"
	.space 1
	.ascii "or_pu16_pi16"
	.space 1
	.ascii "or_vu16_vi16"
	.space 1
	.ascii "or_pu16_pu8"
	.space 1
	.ascii "or_vu16_vu8"
	.space 1
	.ascii "or_pu16_pi8"
	.space 1
	.ascii "or_vu16_vi8"
	.space 1
	.ascii "and_pu16_pu64"
	.space 1
	.ascii "and_vu16_vu64"
	.space 1
	.ascii "and_pu16_pi64"
	.space 1
	.ascii "and_vu16_vi64"
	.space 1
	.ascii "and_pu16_pu32"
	.space 1
	.ascii "and_vu16_vu32"
	.space 1
	.ascii "and_pu16_pi32"
	.space 1
	.ascii "and_vu16_vi32"
	.space 1
	.ascii "and_pu16_pu16"
	.space 1
	.ascii "and_vu16_vu16"
	.space 1
	.ascii "and_pu16_pi16"
	.space 1
	.ascii "and_vu16_vi16"
	.space 1
	.ascii "and_pu16_pu8"
	.space 1
	.ascii "and_vu16_vu8"
	.space 1
	.ascii "and_pu16_pi8"
	.space 1
	.ascii "and_vu16_vi8"
	.space 1
	.ascii "sub_pu16_pu64"
	.space 1
	.ascii "sub_vu16_vu64"
	.space 1
	.ascii "sub_pu16_pi64"
	.space 1
	.ascii "sub_vu16_vi64"
	.space 1
	.ascii "sub_pu16_pu32"
	.space 1
	.ascii "sub_vu16_vu32"
	.space 1
	.ascii "sub_pu16_pi32"
	.space 1
	.ascii "sub_vu16_vi32"
	.space 1
	.ascii "sub_pu16_pu16"
	.space 1
	.ascii "sub_vu16_vu16"
	.space 1
	.ascii "sub_pu16_pi16"
	.space 1
	.ascii "sub_vu16_vi16"
	.space 1
	.ascii "sub_pu16_pu8"
	.space 1
	.ascii "sub_vu16_vu8"
	.space 1
	.ascii "sub_pu16_pi8"
	.space 1
	.ascii "sub_vu16_vi8"
	.space 1
	.ascii "add_pu16_pu64"
	.space 1
	.ascii "add_vu16_vu64"
	.space 1
	.ascii "add_pu16_pi64"
	.space 1
	.ascii "add_vu16_vi64"
	.space 1
	.ascii "add_pu16_pu32"
	.space 1
	.ascii "add_vu16_vu32"
	.space 1
	.ascii "add_pu16_pi32"
	.space 1
	.ascii "add_vu16_vi32"
	.space 1
	.ascii "add_pu16_pu16"
	.space 1
	.ascii "add_vu16_vu16"
	.space 1
	.ascii "add_pu16_pi16"
	.space 1
	.ascii "add_vu16_vi16"
	.space 1
	.ascii "add_pu16_pu8"
	.space 1
	.ascii "add_vu16_vu8"
	.space 1
	.ascii "add_pu16_pi8"
	.space 1
	.ascii "add_vu16_vi8"
	.space 1
	.ascii "ret_pu16_u64"
	.space 1
	.ascii "ret_pu16_i64"
	.space 1
	.ascii "ret_pu16_u32"
	.space 1
	.ascii "ret_pu16_i32"
	.space 1
	.ascii "ret_pu16_u16"
	.space 1
	.ascii "ret_pu16_i16"
	.space 1
	.ascii "ret_pu16_u8"
	.space 1
	.ascii "ret_pu16_i8"
	.space 1
	.ascii "ret_ku16"
	.space 1
	.ascii "ret_vu16"
	.space 1
	.ascii "mod_pi16_pu64"
	.space 1
	.ascii "mod_vi16_vu64"
	.space 1
	.ascii "mod_pi16_pi64"
	.space 1
	.ascii "mod_vi16_vi64"
	.space 1
	.ascii "mod_pi16_pu32"
	.space 1
	.ascii "mod_vi16_vu32"
	.space 1
	.ascii "mod_pi16_pi32"
	.space 1
	.ascii "mod_vi16_vi32"
	.space 1
	.ascii "mod_pi16_pu16"
	.space 1
	.ascii "mod_vi16_vu16"
	.space 1
	.ascii "mod_pi16_pi16"
	.space 1
	.ascii "mod_vi16_vi16"
	.space 1
	.ascii "mod_pi16_pu8"
	.space 1
	.ascii "mod_vi16_vu8"
	.space 1
	.ascii "mod_pi16_pi8"
	.space 1
	.ascii "mod_vi16_vi8"
	.space 1
	.ascii "div_pi16_pu64"
	.space 1
	.ascii "div_vi16_vu64"
	.space 1
	.ascii "div_pi16_pi64"
	.space 1
	.ascii "div_vi16_vi64"
	.space 1
	.ascii "div_pi16_pu32"
	.space 1
	.ascii "div_vi16_vu32"
	.space 1
	.ascii "div_pi16_pi32"
	.space 1
	.ascii "div_vi16_vi32"
	.space 1
	.ascii "div_pi16_pu16"
	.space 1
	.ascii "div_vi16_vu16"
	.space 1
	.ascii "div_pi16_pi16"
	.space 1
	.ascii "div_vi16_vi16"
	.space 1
	.ascii "div_pi16_pu8"
	.space 1
	.ascii "div_vi16_vu8"
	.space 1
	.ascii "div_pi16_pi8"
	.space 1
	.ascii "div_vi16_vi8"
	.space 1
	.ascii "mult_pi16_pu64"
	.space 1
	.ascii "mult_vi16_vu64"
	.space 1
	.ascii "mult_pi16_pi64"
	.space 1
	.ascii "mult_vi16_vi64"
	.space 1
	.ascii "mult_pi16_pu32"
	.space 1
	.ascii "mult_vi16_vu32"
	.space 1
	.ascii "mult_pi16_pi32"
	.space 1
	.ascii "mult_vi16_vi32"
	.space 1
	.ascii "mult_pi16_pu16"
	.space 1
	.ascii "mult_vi16_vu16"
	.space 1
	.ascii "mult_pi16_pi16"
	.space 1
	.ascii "mult_vi16_vi16"
	.space 1
	.ascii "mult_pi16_pu8"
	.space 1
	.ascii "mult_vi16_vu8"
	.space 1
	.ascii "mult_pi16_pi8"
	.space 1
	.ascii "mult_vi16_vi8"
	.space 1
	.ascii "xor_pi16_pu64"
	.space 1
	.ascii "xor_vi16_vu64"
	.space 1
	.ascii "xor_pi16_pi64"
	.space 1
	.ascii "xor_vi16_vi64"
	.space 1
	.ascii "xor_pi16_pu32"
	.space 1
	.ascii "xor_vi16_vu32"
	.space 1
	.ascii "xor_pi16_pi32"
	.space 1
	.ascii "xor_vi16_vi32"
	.space 1
	.ascii "xor_pi16_pu16"
	.space 1
	.ascii "xor_vi16_vu16"
	.space 1
	.ascii "xor_pi16_pi16"
	.space 1
	.ascii "xor_vi16_vi16"
	.space 1
	.ascii "xor_pi16_pu8"
	.space 1
	.ascii "xor_vi16_vu8"
	.space 1
	.ascii "xor_pi16_pi8"
	.space 1
	.ascii "xor_vi16_vi8"
	.space 1
	.ascii "or_pi16_pu64"
	.space 1
	.ascii "or_vi16_vu64"
	.space 1
	.ascii "or_pi16_pi64"
	.space 1
	.ascii "or_vi16_vi64"
	.space 1
	.ascii "or_pi16_pu32"
	.space 1
	.ascii "or_vi16_vu32"
	.space 1
	.ascii "or_pi16_pi32"
	.space 1
	.ascii "or_vi16_vi32"
	.space 1
	.ascii "or_pi16_pu16"
	.space 1
	.ascii "or_vi16_vu16"
	.space 1
	.ascii "or_pi16_pi16"
	.space 1
	.ascii "or_vi16_vi16"
	.space 1
	.ascii "or_pi16_pu8"
	.space 1
	.ascii "or_vi16_vu8"
	.space 1
	.ascii "or_pi16_pi8"
	.space 1
	.ascii "or_vi16_vi8"
	.space 1
	.ascii "and_pi16_pu64"
	.space 1
	.ascii "and_vi16_vu64"
	.space 1
	.ascii "and_pi16_pi64"
	.space 1
	.ascii "and_vi16_vi64"
	.space 1
	.ascii "and_pi16_pu32"
	.space 1
	.ascii "and_vi16_vu32"
	.space 1
	.ascii "and_pi16_pi32"
	.space 1
	.ascii "and_vi16_vi32"
	.space 1
	.ascii "and_pi16_pu16"
	.space 1
	.ascii "and_vi16_vu16"
	.space 1
	.ascii "and_pi16_pi16"
	.space 1
	.ascii "and_vi16_vi16"
	.space 1
	.ascii "and_pi16_pu8"
	.space 1
	.ascii "and_vi16_vu8"
	.space 1
	.ascii "and_pi16_pi8"
	.space 1
	.ascii "and_vi16_vi8"
	.space 1
	.ascii "sub_pi16_pu64"
	.space 1
	.ascii "sub_vi16_vu64"
	.space 1
	.ascii "sub_pi16_pi64"
	.space 1
	.ascii "sub_vi16_vi64"
	.space 1
	.ascii "sub_pi16_pu32"
	.space 1
	.ascii "sub_vi16_vu32"
	.space 1
	.ascii "sub_pi16_pi32"
	.space 1
	.ascii "sub_vi16_vi32"
	.space 1
	.ascii "sub_pi16_pu16"
	.space 1
	.ascii "sub_vi16_vu16"
	.space 1
	.ascii "sub_pi16_pi16"
	.space 1
	.ascii "sub_vi16_vi16"
	.space 1
	.ascii "sub_pi16_pu8"
	.space 1
	.ascii "sub_vi16_vu8"
	.space 1
	.ascii "sub_pi16_pi8"
	.space 1
	.ascii "sub_vi16_vi8"
	.space 1
	.ascii "add_pi16_pu64"
	.space 1
	.ascii "add_vi16_vu64"
	.space 1
	.ascii "add_pi16_pi64"
	.space 1
	.ascii "add_vi16_vi64"
	.space 1
	.ascii "add_pi16_pu32"
	.space 1
	.ascii "add_vi16_vu32"
	.space 1
	.ascii "add_pi16_pi32"
	.space 1
	.ascii "add_vi16_vi32"
	.space 1
	.ascii "add_pi16_pu16"
	.space 1
	.ascii "add_vi16_vu16"
	.space 1
	.ascii "add_pi16_pi16"
	.space 1
	.ascii "add_vi16_vi16"
	.space 1
	.ascii "add_pi16_pu8"
	.space 1
	.ascii "add_vi16_vu8"
	.space 1
	.ascii "add_pi16_pi8"
	.space 1
	.ascii "add_vi16_vi8"
	.space 1
	.ascii "ret_pi16_u64"
	.space 1
	.ascii "ret_pi16_i64"
	.space 1
	.ascii "ret_pi16_u32"
	.space 1
	.ascii "ret_pi16_i32"
	.space 1
	.ascii "ret_pi16_u16"
	.space 1
	.ascii "ret_pi16_i16"
	.space 1
	.ascii "ret_pi16_u8"
	.space 1
	.ascii "ret_pi16_i8"
	.space 1
	.ascii "ret_ki16"
	.space 1
	.ascii "ret_vi16"
	.space 1
	.ascii "mod_pu8_pu64"
	.space 1
	.ascii "mod_vu8_vu64"
	.space 1
	.ascii "mod_pu8_pi64"
	.space 1
	.ascii "mod_vu8_vi64"
	.space 1
	.ascii "mod_pu8_pu32"
	.space 1
	.ascii "mod_vu8_vu32"
	.space 1
	.ascii "mod_pu8_pi32"
	.space 1
	.ascii "mod_vu8_vi32"
	.space 1
	.ascii "mod_pu8_pu16"
	.space 1
	.ascii "mod_vu8_vu16"
	.space 1
	.ascii "mod_pu8_pi16"
	.space 1
	.ascii "mod_vu8_vi16"
	.space 1
	.ascii "mod_pu8_pu8"
	.space 1
	.ascii "mod_vu8_vu8"
	.space 1
	.ascii "mod_pu8_pi8"
	.space 1
	.ascii "mod_vu8_vi8"
	.space 1
	.ascii "div_pu8_pu64"
	.space 1
	.ascii "div_vu8_vu64"
	.space 1
	.ascii "div_pu8_pi64"
	.space 1
	.ascii "div_vu8_vi64"
	.space 1
	.ascii "div_pu8_pu32"
	.space 1
	.ascii "div_vu8_vu32"
	.space 1
	.ascii "div_pu8_pi32"
	.space 1
	.ascii "div_vu8_vi32"
	.space 1
	.ascii "div_pu8_pu16"
	.space 1
	.ascii "div_vu8_vu16"
	.space 1
	.ascii "div_pu8_pi16"
	.space 1
	.ascii "div_vu8_vi16"
	.space 1
	.ascii "div_pu8_pu8"
	.space 1
	.ascii "div_vu8_vu8"
	.space 1
	.ascii "div_pu8_pi8"
	.space 1
	.ascii "div_vu8_vi8"
	.space 1
	.ascii "mult_pu8_pu64"
	.space 1
	.ascii "mult_vu8_vu64"
	.space 1
	.ascii "mult_pu8_pi64"
	.space 1
	.ascii "mult_vu8_vi64"
	.space 1
	.ascii "mult_pu8_pu32"
	.space 1
	.ascii "mult_vu8_vu32"
	.space 1
	.ascii "mult_pu8_pi32"
	.space 1
	.ascii "mult_vu8_vi32"
	.space 1
	.ascii "mult_pu8_pu16"
	.space 1
	.ascii "mult_vu8_vu16"
	.space 1
	.ascii "mult_pu8_pi16"
	.space 1
	.ascii "mult_vu8_vi16"
	.space 1
	.ascii "mult_pu8_pu8"
	.space 1
	.ascii "mult_vu8_vu8"
	.space 1
	.ascii "mult_pu8_pi8"
	.space 1
	.ascii "mult_vu8_vi8"
	.space 1
	.ascii "xor_pu8_pu64"
	.space 1
	.ascii "xor_vu8_vu64"
	.space 1
	.ascii "xor_pu8_pi64"
	.space 1
	.ascii "xor_vu8_vi64"
	.space 1
	.ascii "xor_pu8_pu32"
	.space 1
	.ascii "xor_vu8_vu32"
	.space 1
	.ascii "xor_pu8_pi32"
	.space 1
	.ascii "xor_vu8_vi32"
	.space 1
	.ascii "xor_pu8_pu16"
	.space 1
	.ascii "xor_vu8_vu16"
	.space 1
	.ascii "xor_pu8_pi16"
	.space 1
	.ascii "xor_vu8_vi16"
	.space 1
	.ascii "xor_pu8_pu8"
	.space 1
	.ascii "xor_vu8_vu8"
	.space 1
	.ascii "xor_pu8_pi8"
	.space 1
	.ascii "xor_vu8_vi8"
	.space 1
	.ascii "or_pu8_pu64"
	.space 1
	.ascii "or_vu8_vu64"
	.space 1
	.ascii "or_pu8_pi64"
	.space 1
	.ascii "or_vu8_vi64"
	.space 1
	.ascii "or_pu8_pu32"
	.space 1
	.ascii "or_vu8_vu32"
	.space 1
	.ascii "or_pu8_pi32"
	.space 1
	.ascii "or_vu8_vi32"
	.space 1
	.ascii "or_pu8_pu16"
	.space 1
	.ascii "or_vu8_vu16"
	.space 1
	.ascii "or_pu8_pi16"
	.space 1
	.ascii "or_vu8_vi16"
	.space 1
	.ascii "or_pu8_pu8"
	.space 1
	.ascii "or_vu8_vu8"
	.space 1
	.ascii "or_pu8_pi8"
	.space 1
	.ascii "or_vu8_vi8"
	.space 1
	.ascii "and_pu8_pu64"
	.space 1
	.ascii "and_vu8_vu64"
	.space 1
	.ascii "and_pu8_pi64"
	.space 1
	.ascii "and_vu8_vi64"
	.space 1
	.ascii "and_pu8_pu32"
	.space 1
	.ascii "and_vu8_vu32"
	.space 1
	.ascii "and_pu8_pi32"
	.space 1
	.ascii "and_vu8_vi32"
	.space 1
	.ascii "and_pu8_pu16"
	.space 1
	.ascii "and_vu8_vu16"
	.space 1
	.ascii "and_pu8_pi16"
	.space 1
	.ascii "and_vu8_vi16"
	.space 1
	.ascii "and_pu8_pu8"
	.space 1
	.ascii "and_vu8_vu8"
	.space 1
	.ascii "and_pu8_pi8"
	.space 1
	.ascii "and_vu8_vi8"
	.space 1
	.ascii "sub_pu8_pu64"
	.space 1
	.ascii "sub_vu8_vu64"
	.space 1
	.ascii "sub_pu8_pi64"
	.space 1
	.ascii "sub_vu8_vi64"
	.space 1
	.ascii "sub_pu8_pu32"
	.space 1
	.ascii "sub_vu8_vu32"
	.space 1
	.ascii "sub_pu8_pi32"
	.space 1
	.ascii "sub_vu8_vi32"
	.space 1
	.ascii "sub_pu8_pu16"
	.space 1
	.ascii "sub_vu8_vu16"
	.space 1
	.ascii "sub_pu8_pi16"
	.space 1
	.ascii "sub_vu8_vi16"
	.space 1
	.ascii "sub_pu8_pu8"
	.space 1
	.ascii "sub_vu8_vu8"
	.space 1
	.ascii "sub_pu8_pi8"
	.space 1
	.ascii "sub_vu8_vi8"
	.space 1
	.ascii "add_pu8_pu64"
	.space 1
	.ascii "add_vu8_vu64"
	.space 1
	.ascii "add_pu8_pi64"
	.space 1
	.ascii "add_vu8_vi64"
	.space 1
	.ascii "add_pu8_pu32"
	.space 1
	.ascii "add_vu8_vu32"
	.space 1
	.ascii "add_pu8_pi32"
	.space 1
	.ascii "add_vu8_vi32"
	.space 1
	.ascii "add_pu8_pu16"
	.space 1
	.ascii "add_vu8_vu16"
	.space 1
	.ascii "add_pu8_pi16"
	.space 1
	.ascii "add_vu8_vi16"
	.space 1
	.ascii "add_pu8_pu8"
	.space 1
	.ascii "add_vu8_vu8"
	.space 1
	.ascii "add_pu8_pi8"
	.space 1
	.ascii "add_vu8_vi8"
	.space 1
	.ascii "ret_pu8_u64"
	.space 1
	.ascii "ret_pu8_i64"
	.space 1
	.ascii "ret_pu8_u32"
	.space 1
	.ascii "ret_pu8_i32"
	.space 1
	.ascii "ret_pu8_u16"
	.space 1
	.ascii "ret_pu8_i16"
	.space 1
	.ascii "ret_pu8_u8"
	.space 1
	.ascii "ret_pu8_i8"
	.space 1
	.ascii "ret_ku8"
	.space 1
	.ascii "ret_vu8"
	.space 1
	.ascii "mod_pi8_pu64"
	.space 1
	.ascii "mod_vi8_vu64"
	.space 1
	.ascii "mod_pi8_pi64"
	.space 1
	.ascii "mod_vi8_vi64"
	.space 1
	.ascii "mod_pi8_pu32"
	.space 1
	.ascii "mod_vi8_vu32"
	.space 1
	.ascii "mod_pi8_pi32"
	.space 1
	.ascii "mod_vi8_vi32"
	.space 1
	.ascii "mod_pi8_pu16"
	.space 1
	.ascii "mod_vi8_vu16"
	.space 1
	.ascii "mod_pi8_pi16"
	.space 1
	.ascii "mod_vi8_vi16"
	.space 1
	.ascii "mod_pi8_pu8"
	.space 1
	.ascii "mod_vi8_vu8"
	.space 1
	.ascii "mod_pi8_pi8"
	.space 1
	.ascii "mod_vi8_vi8"
	.space 1
	.ascii "div_pi8_pu64"
	.space 1
	.ascii "div_vi8_vu64"
	.space 1
	.ascii "div_pi8_pi64"
	.space 1
	.ascii "div_vi8_vi64"
	.space 1
	.ascii "div_pi8_pu32"
	.space 1
	.ascii "div_vi8_vu32"
	.space 1
	.ascii "div_pi8_pi32"
	.space 1
	.ascii "div_vi8_vi32"
	.space 1
	.ascii "div_pi8_pu16"
	.space 1
	.ascii "div_vi8_vu16"
	.space 1
	.ascii "div_pi8_pi16"
	.space 1
	.ascii "div_vi8_vi16"
	.space 1
	.ascii "div_pi8_pu8"
	.space 1
	.ascii "div_vi8_vu8"
	.space 1
	.ascii "div_pi8_pi8"
	.space 1
	.ascii "div_vi8_vi8"
	.space 1
	.ascii "mult_pi8_pu64"
	.space 1
	.ascii "mult_vi8_vu64"
	.space 1
	.ascii "mult_pi8_pi64"
	.space 1
	.ascii "mult_vi8_vi64"
	.space 1
	.ascii "mult_pi8_pu32"
	.space 1
	.ascii "mult_vi8_vu32"
	.space 1
	.ascii "mult_pi8_pi32"
	.space 1
	.ascii "mult_vi8_vi32"
	.space 1
	.ascii "mult_pi8_pu16"
	.space 1
	.ascii "mult_vi8_vu16"
	.space 1
	.ascii "mult_pi8_pi16"
	.space 1
	.ascii "mult_vi8_vi16"
	.space 1
	.ascii "mult_pi8_pu8"
	.space 1
	.ascii "mult_vi8_vu8"
	.space 1
	.ascii "mult_pi8_pi8"
	.space 1
	.ascii "mult_vi8_vi8"
	.space 1
	.ascii "xor_pi8_pu64"
	.space 1
	.ascii "xor_vi8_vu64"
	.space 1
	.ascii "xor_pi8_pi64"
	.space 1
	.ascii "xor_vi8_vi64"
	.space 1
	.ascii "xor_pi8_pu32"
	.space 1
	.ascii "xor_vi8_vu32"
	.space 1
	.ascii "xor_pi8_pi32"
	.space 1
	.ascii "xor_vi8_vi32"
	.space 1
	.ascii "xor_pi8_pu16"
	.space 1
	.ascii "xor_vi8_vu16"
	.space 1
	.ascii "xor_pi8_pi16"
	.space 1
	.ascii "xor_vi8_vi16"
	.space 1
	.ascii "xor_pi8_pu8"
	.space 1
	.ascii "xor_vi8_vu8"
	.space 1
	.ascii "xor_pi8_pi8"
	.space 1
	.ascii "xor_vi8_vi8"
	.space 1
	.ascii "or_pi8_pu64"
	.space 1
	.ascii "or_vi8_vu64"
	.space 1
	.ascii "or_pi8_pi64"
	.space 1
	.ascii "or_vi8_vi64"
	.space 1
	.ascii "or_pi8_pu32"
	.space 1
	.ascii "or_vi8_vu32"
	.space 1
	.ascii "or_pi8_pi32"
	.space 1
	.ascii "or_vi8_vi32"
	.space 1
	.ascii "or_pi8_pu16"
	.space 1
	.ascii "or_vi8_vu16"
	.space 1
	.ascii "or_pi8_pi16"
	.space 1
	.ascii "or_vi8_vi16"
	.space 1
	.ascii "or_pi8_pu8"
	.space 1
	.ascii "or_vi8_vu8"
	.space 1
	.ascii "or_pi8_pi8"
	.space 1
	.ascii "or_vi8_vi8"
	.space 1
	.ascii "and_pi8_pu64"
	.space 1
	.ascii "and_vi8_vu64"
	.space 1
	.ascii "and_pi8_pi64"
	.space 1
	.ascii "and_vi8_vi64"
	.space 1
	.ascii "and_pi8_pu32"
	.space 1
	.ascii "and_vi8_vu32"
	.space 1
	.ascii "and_pi8_pi32"
	.space 1
	.ascii "and_vi8_vi32"
	.space 1
	.ascii "and_pi8_pu16"
	.space 1
	.ascii "and_vi8_vu16"
	.space 1
	.ascii "and_pi8_pi16"
	.space 1
	.ascii "and_vi8_vi16"
	.space 1
	.ascii "and_pi8_pu8"
	.space 1
	.ascii "and_vi8_vu8"
	.space 1
	.ascii "and_pi8_pi8"
	.space 1
	.ascii "and_vi8_vi8"
	.space 1
	.ascii "sub_pi8_pu64"
	.space 1
	.ascii "sub_vi8_vu64"
	.space 1
	.ascii "sub_pi8_pi64"
	.space 1
	.ascii "sub_vi8_vi64"
	.space 1
	.ascii "sub_pi8_pu32"
	.space 1
	.ascii "sub_vi8_vu32"
	.space 1
	.ascii "sub_pi8_pi32"
	.space 1
	.ascii "sub_vi8_vi32"
	.space 1
	.ascii "sub_pi8_pu16"
	.space 1
	.ascii "sub_vi8_vu16"
	.space 1
	.ascii "sub_pi8_pi16"
	.space 1
	.ascii "sub_vi8_vi16"
	.space 1
	.ascii "sub_pi8_pu8"
	.space 1
	.ascii "sub_vi8_vu8"
	.space 1
	.ascii "sub_pi8_pi8"
	.space 1
	.ascii "sub_vi8_vi8"
	.space 1
	.ascii "add_pi8_pu64"
	.space 1
	.ascii "add_vi8_vu64"
	.space 1
	.ascii "add_pi8_pi64"
	.space 1
	.ascii "add_vi8_vi64"
	.space 1
	.ascii "add_pi8_pu32"
	.space 1
	.ascii "add_vi8_vu32"
	.space 1
	.ascii "add_pi8_pi32"
	.space 1
	.ascii "add_vi8_vi32"
	.space 1
	.ascii "add_pi8_pu16"
	.space 1
	.ascii "add_vi8_vu16"
	.space 1
	.ascii "add_pi8_pi16"
	.space 1
	.ascii "add_vi8_vi16"
	.space 1
	.ascii "add_pi8_pu8"
	.space 1
	.ascii "add_vi8_vu8"
	.space 1
	.ascii "add_pi8_pi8"
	.space 1
	.ascii "add_vi8_vi8"
	.space 1
	.ascii "ret_pi8_u64"
	.space 1
	.ascii "ret_pi8_i64"
	.space 1
	.ascii "ret_pi8_u32"
	.space 1
	.ascii "ret_pi8_i32"
	.space 1
	.ascii "ret_pi8_u16"
	.space 1
	.ascii "ret_pi8_i16"
	.space 1
	.ascii "ret_pi8_u8"
	.space 1
	.ascii "ret_pi8_i8"
	.space 1
	.ascii "ret_ki8"
	.space 1
	.ascii "ret_vi8"
	.space 8
	.quad	_A_M3
	.quad	_L_1
	.quad	_A__mod_pu64_pu64
	.quad	_L_1+5
	.quad	_A__mod_vu64_vu64
	.quad	_L_1+19
	.quad	_A__mod_pu64_pi64
	.quad	_L_1+33
	.quad	_A__mod_vu64_vi64
	.quad	_L_1+47
	.quad	_A__mod_pu64_pu32
	.quad	_L_1+61
	.quad	_A__mod_vu64_vu32
	.quad	_L_1+75
	.quad	_A__mod_pu64_pi32
	.quad	_L_1+89
	.quad	_A__mod_vu64_vi32
	.quad	_L_1+103
	.quad	_A__mod_pu64_pu16
	.quad	_L_1+117
	.quad	_A__mod_vu64_vu16
	.quad	_L_1+131
	.quad	_A__mod_pu64_pi16
	.quad	_L_1+145
	.quad	_A__mod_vu64_vi16
	.quad	_L_1+159
	.quad	_A__mod_pu64_pu8
	.quad	_L_1+173
	.quad	_A__mod_vu64_vu8
	.quad	_L_1+186
	.quad	_A__mod_pu64_pi8
	.quad	_L_1+199
	.quad	_A__mod_vu64_vi8
	.quad	_L_1+212
	.quad	_A__div_pu64_pu64
	.quad	_L_1+225
	.quad	_A__div_vu64_vu64
	.quad	_L_1+239
	.quad	_A__div_pu64_pi64
	.quad	_L_1+253
	.quad	_A__div_vu64_vi64
	.quad	_L_1+267
	.quad	_A__div_pu64_pu32
	.quad	_L_1+281
	.quad	_A__div_vu64_vu32
	.quad	_L_1+295
	.quad	_A__div_pu64_pi32
	.quad	_L_1+309
	.quad	_A__div_vu64_vi32
	.quad	_L_1+323
	.quad	_A__div_pu64_pu16
	.quad	_L_1+337
	.quad	_A__div_vu64_vu16
	.quad	_L_1+351
	.quad	_A__div_pu64_pi16
	.quad	_L_1+365
	.quad	_A__div_vu64_vi16
	.quad	_L_1+379
	.quad	_A__div_pu64_pu8
	.quad	_L_1+393
	.quad	_A__div_vu64_vu8
	.quad	_L_1+406
	.quad	_A__div_pu64_pi8
	.quad	_L_1+419
	.quad	_A__div_vu64_vi8
	.quad	_L_1+432
	.quad	_A__mult_pu64_pu64
	.quad	_L_1+445
	.quad	_A__mult_vu64_vu64
	.quad	_L_1+460
	.quad	_A__mult_pu64_pi64
	.quad	_L_1+475
	.quad	_A__mult_vu64_vi64
	.quad	_L_1+490
	.quad	_A__mult_pu64_pu32
	.quad	_L_1+505
	.quad	_A__mult_vu64_vu32
	.quad	_L_1+520
	.quad	_A__mult_pu64_pi32
	.quad	_L_1+535
	.quad	_A__mult_vu64_vi32
	.quad	_L_1+550
	.quad	_A__mult_pu64_pu16
	.quad	_L_1+565
	.quad	_A__mult_vu64_vu16
	.quad	_L_1+580
	.quad	_A__mult_pu64_pi16
	.quad	_L_1+595
	.quad	_A__mult_vu64_vi16
	.quad	_L_1+610
	.quad	_A__mult_pu64_pu8
	.quad	_L_1+625
	.quad	_A__mult_vu64_vu8
	.quad	_L_1+639
	.quad	_A__mult_pu64_pi8
	.quad	_L_1+653
	.quad	_A__mult_vu64_vi8
	.quad	_L_1+667
	.quad	_A__xor_pu64_pu64
	.quad	_L_1+681
	.quad	_A__xor_vu64_vu64
	.quad	_L_1+695
	.quad	_A__xor_pu64_pi64
	.quad	_L_1+709
	.quad	_A__xor_vu64_vi64
	.quad	_L_1+723
	.quad	_A__xor_pu64_pu32
	.quad	_L_1+737
	.quad	_A__xor_vu64_vu32
	.quad	_L_1+751
	.quad	_A__xor_pu64_pi32
	.quad	_L_1+765
	.quad	_A__xor_vu64_vi32
	.quad	_L_1+779
	.quad	_A__xor_pu64_pu16
	.quad	_L_1+793
	.quad	_A__xor_vu64_vu16
	.quad	_L_1+807
	.quad	_A__xor_pu64_pi16
	.quad	_L_1+821
	.quad	_A__xor_vu64_vi16
	.quad	_L_1+835
	.quad	_A__xor_pu64_pu8
	.quad	_L_1+849
	.quad	_A__xor_vu64_vu8
	.quad	_L_1+862
	.quad	_A__xor_pu64_pi8
	.quad	_L_1+875
	.quad	_A__xor_vu64_vi8
	.quad	_L_1+888
	.quad	_A__or_pu64_pu64
	.quad	_L_1+901
	.quad	_A__or_vu64_vu64
	.quad	_L_1+914
	.quad	_A__or_pu64_pi64
	.quad	_L_1+927
	.quad	_A__or_vu64_vi64
	.quad	_L_1+940
	.quad	_A__or_pu64_pu32
	.quad	_L_1+953
	.quad	_A__or_vu64_vu32
	.quad	_L_1+966
	.quad	_A__or_pu64_pi32
	.quad	_L_1+979
	.quad	_A__or_vu64_vi32
	.quad	_L_1+992
	.quad	_A__or_pu64_pu16
	.quad	_L_1+1005
	.quad	_A__or_vu64_vu16
	.quad	_L_1+1018
	.quad	_A__or_pu64_pi16
	.quad	_L_1+1031
	.quad	_A__or_vu64_vi16
	.quad	_L_1+1044
	.quad	_A__or_pu64_pu8
	.quad	_L_1+1057
	.quad	_A__or_vu64_vu8
	.quad	_L_1+1069
	.quad	_A__or_pu64_pi8
	.quad	_L_1+1081
	.quad	_A__or_vu64_vi8
	.quad	_L_1+1093
	.quad	_A__and_pu64_pu64
	.quad	_L_1+1105
	.quad	_A__and_vu64_vu64
	.quad	_L_1+1119
	.quad	_A__and_pu64_pi64
	.quad	_L_1+1133
	.quad	_A__and_vu64_vi64
	.quad	_L_1+1147
	.quad	_A__and_pu64_pu32
	.quad	_L_1+1161
	.quad	_A__and_vu64_vu32
	.quad	_L_1+1175
	.quad	_A__and_pu64_pi32
	.quad	_L_1+1189
	.quad	_A__and_vu64_vi32
	.quad	_L_1+1203
	.quad	_A__and_pu64_pu16
	.quad	_L_1+1217
	.quad	_A__and_vu64_vu16
	.quad	_L_1+1231
	.quad	_A__and_pu64_pi16
	.quad	_L_1+1245
	.quad	_A__and_vu64_vi16
	.quad	_L_1+1259
	.quad	_A__and_pu64_pu8
	.quad	_L_1+1273
	.quad	_A__and_vu64_vu8
	.quad	_L_1+1286
	.quad	_A__and_pu64_pi8
	.quad	_L_1+1299
	.quad	_A__and_vu64_vi8
	.quad	_L_1+1312
	.quad	_A__sub_pu64_pu64
	.quad	_L_1+1325
	.quad	_A__sub_vu64_vu64
	.quad	_L_1+1339
	.quad	_A__sub_pu64_pi64
	.quad	_L_1+1353
	.quad	_A__sub_vu64_vi64
	.quad	_L_1+1367
	.quad	_A__sub_pu64_pu32
	.quad	_L_1+1381
	.quad	_A__sub_vu64_vu32
	.quad	_L_1+1395
	.quad	_A__sub_pu64_pi32
	.quad	_L_1+1409
	.quad	_A__sub_vu64_vi32
	.quad	_L_1+1423
	.quad	_A__sub_pu64_pu16
	.quad	_L_1+1437
	.quad	_A__sub_vu64_vu16
	.quad	_L_1+1451
	.quad	_A__sub_pu64_pi16
	.quad	_L_1+1465
	.quad	_A__sub_vu64_vi16
	.quad	_L_1+1479
	.quad	_A__sub_pu64_pu8
	.quad	_L_1+1493
	.quad	_A__sub_vu64_vu8
	.quad	_L_1+1506
	.quad	_A__sub_pu64_pi8
	.quad	_L_1+1519
	.quad	_A__sub_vu64_vi8
	.quad	_L_1+1532
	.quad	_A__add_pu64_pu64
	.quad	_L_1+1545
	.quad	_A__add_vu64_vu64
	.quad	_L_1+1559
	.quad	_A__add_pu64_pi64
	.quad	_L_1+1573
	.quad	_A__add_vu64_vi64
	.quad	_L_1+1587
	.quad	_A__add_pu64_pu32
	.quad	_L_1+1601
	.quad	_A__add_vu64_vu32
	.quad	_L_1+1615
	.quad	_A__add_pu64_pi32
	.quad	_L_1+1629
	.quad	_A__add_vu64_vi32
	.quad	_L_1+1643
	.quad	_A__add_pu64_pu16
	.quad	_L_1+1657
	.quad	_A__add_vu64_vu16
	.quad	_L_1+1671
	.quad	_A__add_pu64_pi16
	.quad	_L_1+1685
	.quad	_A__add_vu64_vi16
	.quad	_L_1+1699
	.quad	_A__add_pu64_pu8
	.quad	_L_1+1713
	.quad	_A__add_vu64_vu8
	.quad	_L_1+1726
	.quad	_A__add_pu64_pi8
	.quad	_L_1+1739
	.quad	_A__add_vu64_vi8
	.quad	_L_1+1752
	.quad	_A__ret_pu64_u64
	.quad	_L_1+1765
	.quad	_A__ret_pu64_i64
	.quad	_L_1+1778
	.quad	_A__ret_pu64_u32
	.quad	_L_1+1791
	.quad	_A__ret_pu64_i32
	.quad	_L_1+1804
	.quad	_A__ret_pu64_u16
	.quad	_L_1+1817
	.quad	_A__ret_pu64_i16
	.quad	_L_1+1830
	.quad	_A__ret_pu64_u8
	.quad	_L_1+1843
	.quad	_A__ret_pu64_i8
	.quad	_L_1+1855
	.quad	_A__ret_ku64
	.quad	_L_1+1867
	.quad	_A__ret_vu64
	.quad	_L_1+1876
	.quad	_A__mod_pi64_pu64
	.quad	_L_1+1885
	.quad	_A__mod_vi64_vu64
	.quad	_L_1+1899
	.quad	_A__mod_pi64_pi64
	.quad	_L_1+1913
	.quad	_A__mod_vi64_vi64
	.quad	_L_1+1927
	.quad	_A__mod_pi64_pu32
	.quad	_L_1+1941
	.quad	_A__mod_vi64_vu32
	.quad	_L_1+1955
	.quad	_A__mod_pi64_pi32
	.quad	_L_1+1969
	.quad	_A__mod_vi64_vi32
	.quad	_L_1+1983
	.quad	_A__mod_pi64_pu16
	.quad	_L_1+1997
	.quad	_A__mod_vi64_vu16
	.quad	_L_1+2011
	.quad	_A__mod_pi64_pi16
	.quad	_L_1+2025
	.quad	_A__mod_vi64_vi16
	.quad	_L_1+2039
	.quad	_A__mod_pi64_pu8
	.quad	_L_1+2053
	.quad	_A__mod_vi64_vu8
	.quad	_L_1+2066
	.quad	_A__mod_pi64_pi8
	.quad	_L_1+2079
	.quad	_A__mod_vi64_vi8
	.quad	_L_1+2092
	.quad	_A__div_pi64_pu64
	.quad	_L_1+2105
	.quad	_A__div_vi64_vu64
	.quad	_L_1+2119
	.quad	_A__div_pi64_pi64
	.quad	_L_1+2133
	.quad	_A__div_vi64_vi64
	.quad	_L_1+2147
	.quad	_A__div_pi64_pu32
	.quad	_L_1+2161
	.quad	_A__div_vi64_vu32
	.quad	_L_1+2175
	.quad	_A__div_pi64_pi32
	.quad	_L_1+2189
	.quad	_A__div_vi64_vi32
	.quad	_L_1+2203
	.quad	_A__div_pi64_pu16
	.quad	_L_1+2217
	.quad	_A__div_vi64_vu16
	.quad	_L_1+2231
	.quad	_A__div_pi64_pi16
	.quad	_L_1+2245
	.quad	_A__div_vi64_vi16
	.quad	_L_1+2259
	.quad	_A__div_pi64_pu8
	.quad	_L_1+2273
	.quad	_A__div_vi64_vu8
	.quad	_L_1+2286
	.quad	_A__div_pi64_pi8
	.quad	_L_1+2299
	.quad	_A__div_vi64_vi8
	.quad	_L_1+2312
	.quad	_A__mult_pi64_pu64
	.quad	_L_1+2325
	.quad	_A__mult_vi64_vu64
	.quad	_L_1+2340
	.quad	_A__mult_pi64_pi64
	.quad	_L_1+2355
	.quad	_A__mult_vi64_vi64
	.quad	_L_1+2370
	.quad	_A__mult_pi64_pu32
	.quad	_L_1+2385
	.quad	_A__mult_vi64_vu32
	.quad	_L_1+2400
	.quad	_A__mult_pi64_pi32
	.quad	_L_1+2415
	.quad	_A__mult_vi64_vi32
	.quad	_L_1+2430
	.quad	_A__mult_pi64_pu16
	.quad	_L_1+2445
	.quad	_A__mult_vi64_vu16
	.quad	_L_1+2460
	.quad	_A__mult_pi64_pi16
	.quad	_L_1+2475
	.quad	_A__mult_vi64_vi16
	.quad	_L_1+2490
	.quad	_A__mult_pi64_pu8
	.quad	_L_1+2505
	.quad	_A__mult_vi64_vu8
	.quad	_L_1+2519
	.quad	_A__mult_pi64_pi8
	.quad	_L_1+2533
	.quad	_A__mult_vi64_vi8
	.quad	_L_1+2547
	.quad	_A__xor_pi64_pu64
	.quad	_L_1+2561
	.quad	_A__xor_vi64_vu64
	.quad	_L_1+2575
	.quad	_A__xor_pi64_pi64
	.quad	_L_1+2589
	.quad	_A__xor_vi64_vi64
	.quad	_L_1+2603
	.quad	_A__xor_pi64_pu32
	.quad	_L_1+2617
	.quad	_A__xor_vi64_vu32
	.quad	_L_1+2631
	.quad	_A__xor_pi64_pi32
	.quad	_L_1+2645
	.quad	_A__xor_vi64_vi32
	.quad	_L_1+2659
	.quad	_A__xor_pi64_pu16
	.quad	_L_1+2673
	.quad	_A__xor_vi64_vu16
	.quad	_L_1+2687
	.quad	_A__xor_pi64_pi16
	.quad	_L_1+2701
	.quad	_A__xor_vi64_vi16
	.quad	_L_1+2715
	.quad	_A__xor_pi64_pu8
	.quad	_L_1+2729
	.quad	_A__xor_vi64_vu8
	.quad	_L_1+2742
	.quad	_A__xor_pi64_pi8
	.quad	_L_1+2755
	.quad	_A__xor_vi64_vi8
	.quad	_L_1+2768
	.quad	_A__or_pi64_pu64
	.quad	_L_1+2781
	.quad	_A__or_vi64_vu64
	.quad	_L_1+2794
	.quad	_A__or_pi64_pi64
	.quad	_L_1+2807
	.quad	_A__or_vi64_vi64
	.quad	_L_1+2820
	.quad	_A__or_pi64_pu32
	.quad	_L_1+2833
	.quad	_A__or_vi64_vu32
	.quad	_L_1+2846
	.quad	_A__or_pi64_pi32
	.quad	_L_1+2859
	.quad	_A__or_vi64_vi32
	.quad	_L_1+2872
	.quad	_A__or_pi64_pu16
	.quad	_L_1+2885
	.quad	_A__or_vi64_vu16
	.quad	_L_1+2898
	.quad	_A__or_pi64_pi16
	.quad	_L_1+2911
	.quad	_A__or_vi64_vi16
	.quad	_L_1+2924
	.quad	_A__or_pi64_pu8
	.quad	_L_1+2937
	.quad	_A__or_vi64_vu8
	.quad	_L_1+2949
	.quad	_A__or_pi64_pi8
	.quad	_L_1+2961
	.quad	_A__or_vi64_vi8
	.quad	_L_1+2973
	.quad	_A__and_pi64_pu64
	.quad	_L_1+2985
	.quad	_A__and_vi64_vu64
	.quad	_L_1+2999
	.quad	_A__and_pi64_pi64
	.quad	_L_1+3013
	.quad	_A__and_vi64_vi64
	.quad	_L_1+3027
	.quad	_A__and_pi64_pu32
	.quad	_L_1+3041
	.quad	_A__and_vi64_vu32
	.quad	_L_1+3055
	.quad	_A__and_pi64_pi32
	.quad	_L_1+3069
	.quad	_A__and_vi64_vi32
	.quad	_L_1+3083
	.quad	_A__and_pi64_pu16
	.quad	_L_1+3097
	.quad	_A__and_vi64_vu16
	.quad	_L_1+3111
	.quad	_A__and_pi64_pi16
	.quad	_L_1+3125
	.quad	_A__and_vi64_vi16
	.quad	_L_1+3139
	.quad	_A__and_pi64_pu8
	.quad	_L_1+3153
	.quad	_A__and_vi64_vu8
	.quad	_L_1+3166
	.quad	_A__and_pi64_pi8
	.quad	_L_1+3179
	.quad	_A__and_vi64_vi8
	.quad	_L_1+3192
	.quad	_A__sub_pi64_pu64
	.quad	_L_1+3205
	.quad	_A__sub_vi64_vu64
	.quad	_L_1+3219
	.quad	_A__sub_pi64_pi64
	.quad	_L_1+3233
	.quad	_A__sub_vi64_vi64
	.quad	_L_1+3247
	.quad	_A__sub_pi64_pu32
	.quad	_L_1+3261
	.quad	_A__sub_vi64_vu32
	.quad	_L_1+3275
	.quad	_A__sub_pi64_pi32
	.quad	_L_1+3289
	.quad	_A__sub_vi64_vi32
	.quad	_L_1+3303
	.quad	_A__sub_pi64_pu16
	.quad	_L_1+3317
	.quad	_A__sub_vi64_vu16
	.quad	_L_1+3331
	.quad	_A__sub_pi64_pi16
	.quad	_L_1+3345
	.quad	_A__sub_vi64_vi16
	.quad	_L_1+3359
	.quad	_A__sub_pi64_pu8
	.quad	_L_1+3373
	.quad	_A__sub_vi64_vu8
	.quad	_L_1+3386
	.quad	_A__sub_pi64_pi8
	.quad	_L_1+3399
	.quad	_A__sub_vi64_vi8
	.quad	_L_1+3412
	.quad	_A__add_pi64_pu64
	.quad	_L_1+3425
	.quad	_A__add_vi64_vu64
	.quad	_L_1+3439
	.quad	_A__add_pi64_pi64
	.quad	_L_1+3453
	.quad	_A__add_vi64_vi64
	.quad	_L_1+3467
	.quad	_A__add_pi64_pu32
	.quad	_L_1+3481
	.quad	_A__add_vi64_vu32
	.quad	_L_1+3495
	.quad	_A__add_pi64_pi32
	.quad	_L_1+3509
	.quad	_A__add_vi64_vi32
	.quad	_L_1+3523
	.quad	_A__add_pi64_pu16
	.quad	_L_1+3537
	.quad	_A__add_vi64_vu16
	.quad	_L_1+3551
	.quad	_A__add_pi64_pi16
	.quad	_L_1+3565
	.quad	_A__add_vi64_vi16
	.quad	_L_1+3579
	.quad	_A__add_pi64_pu8
	.quad	_L_1+3593
	.quad	_A__add_vi64_vu8
	.quad	_L_1+3606
	.quad	_A__add_pi64_pi8
	.quad	_L_1+3619
	.quad	_A__add_vi64_vi8
	.quad	_L_1+3632
	.quad	_A__ret_pi64_u64
	.quad	_L_1+3645
	.quad	_A__ret_pi64_i64
	.quad	_L_1+3658
	.quad	_A__ret_pi64_u32
	.quad	_L_1+3671
	.quad	_A__ret_pi64_i32
	.quad	_L_1+3684
	.quad	_A__ret_pi64_u16
	.quad	_L_1+3697
	.quad	_A__ret_pi64_i16
	.quad	_L_1+3710
	.quad	_A__ret_pi64_u8
	.quad	_L_1+3723
	.quad	_A__ret_pi64_i8
	.quad	_L_1+3735
	.quad	_A__ret_ki64
	.quad	_L_1+3747
	.quad	_A__ret_vi64
	.quad	_L_1+3756
	.quad	_A__mod_pu32_pu64
	.quad	_L_1+3765
	.quad	_A__mod_vu32_vu64
	.quad	_L_1+3779
	.quad	_A__mod_pu32_pi64
	.quad	_L_1+3793
	.quad	_A__mod_vu32_vi64
	.quad	_L_1+3807
	.quad	_A__mod_pu32_pu32
	.quad	_L_1+3821
	.quad	_A__mod_vu32_vu32
	.quad	_L_1+3835
	.quad	_A__mod_pu32_pi32
	.quad	_L_1+3849
	.quad	_A__mod_vu32_vi32
	.quad	_L_1+3863
	.quad	_A__mod_pu32_pu16
	.quad	_L_1+3877
	.quad	_A__mod_vu32_vu16
	.quad	_L_1+3891
	.quad	_A__mod_pu32_pi16
	.quad	_L_1+3905
	.quad	_A__mod_vu32_vi16
	.quad	_L_1+3919
	.quad	_A__mod_pu32_pu8
	.quad	_L_1+3933
	.quad	_A__mod_vu32_vu8
	.quad	_L_1+3946
	.quad	_A__mod_pu32_pi8
	.quad	_L_1+3959
	.quad	_A__mod_vu32_vi8
	.quad	_L_1+3972
	.quad	_A__div_pu32_pu64
	.quad	_L_1+3985
	.quad	_A__div_vu32_vu64
	.quad	_L_1+3999
	.quad	_A__div_pu32_pi64
	.quad	_L_1+4013
	.quad	_A__div_vu32_vi64
	.quad	_L_1+4027
	.quad	_A__div_pu32_pu32
	.quad	_L_1+4041
	.quad	_A__div_vu32_vu32
	.quad	_L_1+4055
	.quad	_A__div_pu32_pi32
	.quad	_L_1+4069
	.quad	_A__div_vu32_vi32
	.quad	_L_1+4083
	.quad	_A__div_pu32_pu16
	.quad	_L_1+4097
	.quad	_A__div_vu32_vu16
	.quad	_L_1+4111
	.quad	_A__div_pu32_pi16
	.quad	_L_1+4125
	.quad	_A__div_vu32_vi16
	.quad	_L_1+4139
	.quad	_A__div_pu32_pu8
	.quad	_L_1+4153
	.quad	_A__div_vu32_vu8
	.quad	_L_1+4166
	.quad	_A__div_pu32_pi8
	.quad	_L_1+4179
	.quad	_A__div_vu32_vi8
	.quad	_L_1+4192
	.quad	_A__mult_pu32_pu64
	.quad	_L_1+4205
	.quad	_A__mult_vu32_vu64
	.quad	_L_1+4220
	.quad	_A__mult_pu32_pi64
	.quad	_L_1+4235
	.quad	_A__mult_vu32_vi64
	.quad	_L_1+4250
	.quad	_A__mult_pu32_pu32
	.quad	_L_1+4265
	.quad	_A__mult_vu32_vu32
	.quad	_L_1+4280
	.quad	_A__mult_pu32_pi32
	.quad	_L_1+4295
	.quad	_A__mult_vu32_vi32
	.quad	_L_1+4310
	.quad	_A__mult_pu32_pu16
	.quad	_L_1+4325
	.quad	_A__mult_vu32_vu16
	.quad	_L_1+4340
	.quad	_A__mult_pu32_pi16
	.quad	_L_1+4355
	.quad	_A__mult_vu32_vi16
	.quad	_L_1+4370
	.quad	_A__mult_pu32_pu8
	.quad	_L_1+4385
	.quad	_A__mult_vu32_vu8
	.quad	_L_1+4399
	.quad	_A__mult_pu32_pi8
	.quad	_L_1+4413
	.quad	_A__mult_vu32_vi8
	.quad	_L_1+4427
	.quad	_A__xor_pu32_pu64
	.quad	_L_1+4441
	.quad	_A__xor_vu32_vu64
	.quad	_L_1+4455
	.quad	_A__xor_pu32_pi64
	.quad	_L_1+4469
	.quad	_A__xor_vu32_vi64
	.quad	_L_1+4483
	.quad	_A__xor_pu32_pu32
	.quad	_L_1+4497
	.quad	_A__xor_vu32_vu32
	.quad	_L_1+4511
	.quad	_A__xor_pu32_pi32
	.quad	_L_1+4525
	.quad	_A__xor_vu32_vi32
	.quad	_L_1+4539
	.quad	_A__xor_pu32_pu16
	.quad	_L_1+4553
	.quad	_A__xor_vu32_vu16
	.quad	_L_1+4567
	.quad	_A__xor_pu32_pi16
	.quad	_L_1+4581
	.quad	_A__xor_vu32_vi16
	.quad	_L_1+4595
	.quad	_A__xor_pu32_pu8
	.quad	_L_1+4609
	.quad	_A__xor_vu32_vu8
	.quad	_L_1+4622
	.quad	_A__xor_pu32_pi8
	.quad	_L_1+4635
	.quad	_A__xor_vu32_vi8
	.quad	_L_1+4648
	.quad	_A__or_pu32_pu64
	.quad	_L_1+4661
	.quad	_A__or_vu32_vu64
	.quad	_L_1+4674
	.quad	_A__or_pu32_pi64
	.quad	_L_1+4687
	.quad	_A__or_vu32_vi64
	.quad	_L_1+4700
	.quad	_A__or_pu32_pu32
	.quad	_L_1+4713
	.quad	_A__or_vu32_vu32
	.quad	_L_1+4726
	.quad	_A__or_pu32_pi32
	.quad	_L_1+4739
	.quad	_A__or_vu32_vi32
	.quad	_L_1+4752
	.quad	_A__or_pu32_pu16
	.quad	_L_1+4765
	.quad	_A__or_vu32_vu16
	.quad	_L_1+4778
	.quad	_A__or_pu32_pi16
	.quad	_L_1+4791
	.quad	_A__or_vu32_vi16
	.quad	_L_1+4804
	.quad	_A__or_pu32_pu8
	.quad	_L_1+4817
	.quad	_A__or_vu32_vu8
	.quad	_L_1+4829
	.quad	_A__or_pu32_pi8
	.quad	_L_1+4841
	.quad	_A__or_vu32_vi8
	.quad	_L_1+4853
	.quad	_A__and_pu32_pu64
	.quad	_L_1+4865
	.quad	_A__and_vu32_vu64
	.quad	_L_1+4879
	.quad	_A__and_pu32_pi64
	.quad	_L_1+4893
	.quad	_A__and_vu32_vi64
	.quad	_L_1+4907
	.quad	_A__and_pu32_pu32
	.quad	_L_1+4921
	.quad	_A__and_vu32_vu32
	.quad	_L_1+4935
	.quad	_A__and_pu32_pi32
	.quad	_L_1+4949
	.quad	_A__and_vu32_vi32
	.quad	_L_1+4963
	.quad	_A__and_pu32_pu16
	.quad	_L_1+4977
	.quad	_A__and_vu32_vu16
	.quad	_L_1+4991
	.quad	_A__and_pu32_pi16
	.quad	_L_1+5005
	.quad	_A__and_vu32_vi16
	.quad	_L_1+5019
	.quad	_A__and_pu32_pu8
	.quad	_L_1+5033
	.quad	_A__and_vu32_vu8
	.quad	_L_1+5046
	.quad	_A__and_pu32_pi8
	.quad	_L_1+5059
	.quad	_A__and_vu32_vi8
	.quad	_L_1+5072
	.quad	_A__sub_pu32_pu64
	.quad	_L_1+5085
	.quad	_A__sub_vu32_vu64
	.quad	_L_1+5099
	.quad	_A__sub_pu32_pi64
	.quad	_L_1+5113
	.quad	_A__sub_vu32_vi64
	.quad	_L_1+5127
	.quad	_A__sub_pu32_pu32
	.quad	_L_1+5141
	.quad	_A__sub_vu32_vu32
	.quad	_L_1+5155
	.quad	_A__sub_pu32_pi32
	.quad	_L_1+5169
	.quad	_A__sub_vu32_vi32
	.quad	_L_1+5183
	.quad	_A__sub_pu32_pu16
	.quad	_L_1+5197
	.quad	_A__sub_vu32_vu16
	.quad	_L_1+5211
	.quad	_A__sub_pu32_pi16
	.quad	_L_1+5225
	.quad	_A__sub_vu32_vi16
	.quad	_L_1+5239
	.quad	_A__sub_pu32_pu8
	.quad	_L_1+5253
	.quad	_A__sub_vu32_vu8
	.quad	_L_1+5266
	.quad	_A__sub_pu32_pi8
	.quad	_L_1+5279
	.quad	_A__sub_vu32_vi8
	.quad	_L_1+5292
	.quad	_A__add_pu32_pu64
	.quad	_L_1+5305
	.quad	_A__add_vu32_vu64
	.quad	_L_1+5319
	.quad	_A__add_pu32_pi64
	.quad	_L_1+5333
	.quad	_A__add_vu32_vi64
	.quad	_L_1+5347
	.quad	_A__add_pu32_pu32
	.quad	_L_1+5361
	.quad	_A__add_vu32_vu32
	.quad	_L_1+5375
	.quad	_A__add_pu32_pi32
	.quad	_L_1+5389
	.quad	_A__add_vu32_vi32
	.quad	_L_1+5403
	.quad	_A__add_pu32_pu16
	.quad	_L_1+5417
	.quad	_A__add_vu32_vu16
	.quad	_L_1+5431
	.quad	_A__add_pu32_pi16
	.quad	_L_1+5445
	.quad	_A__add_vu32_vi16
	.quad	_L_1+5459
	.quad	_A__add_pu32_pu8
	.quad	_L_1+5473
	.quad	_A__add_vu32_vu8
	.quad	_L_1+5486
	.quad	_A__add_pu32_pi8
	.quad	_L_1+5499
	.quad	_A__add_vu32_vi8
	.quad	_L_1+5512
	.quad	_A__ret_pu32_u64
	.quad	_L_1+5525
	.quad	_A__ret_pu32_i64
	.quad	_L_1+5538
	.quad	_A__ret_pu32_u32
	.quad	_L_1+5551
	.quad	_A__ret_pu32_i32
	.quad	_L_1+5564
	.quad	_A__ret_pu32_u16
	.quad	_L_1+5577
	.quad	_A__ret_pu32_i16
	.quad	_L_1+5590
	.quad	_A__ret_pu32_u8
	.quad	_L_1+5603
	.quad	_A__ret_pu32_i8
	.quad	_L_1+5615
	.quad	_A__ret_ku32
	.quad	_L_1+5627
	.quad	_A__ret_vu32
	.quad	_L_1+5636
	.quad	_A__mod_pi32_pu64
	.quad	_L_1+5645
	.quad	_A__mod_vi32_vu64
	.quad	_L_1+5659
	.quad	_A__mod_pi32_pi64
	.quad	_L_1+5673
	.quad	_A__mod_vi32_vi64
	.quad	_L_1+5687
	.quad	_A__mod_pi32_pu32
	.quad	_L_1+5701
	.quad	_A__mod_vi32_vu32
	.quad	_L_1+5715
	.quad	_A__mod_pi32_pi32
	.quad	_L_1+5729
	.quad	_A__mod_vi32_vi32
	.quad	_L_1+5743
	.quad	_A__mod_pi32_pu16
	.quad	_L_1+5757
	.quad	_A__mod_vi32_vu16
	.quad	_L_1+5771
	.quad	_A__mod_pi32_pi16
	.quad	_L_1+5785
	.quad	_A__mod_vi32_vi16
	.quad	_L_1+5799
	.quad	_A__mod_pi32_pu8
	.quad	_L_1+5813
	.quad	_A__mod_vi32_vu8
	.quad	_L_1+5826
	.quad	_A__mod_pi32_pi8
	.quad	_L_1+5839
	.quad	_A__mod_vi32_vi8
	.quad	_L_1+5852
	.quad	_A__div_pi32_pu64
	.quad	_L_1+5865
	.quad	_A__div_vi32_vu64
	.quad	_L_1+5879
	.quad	_A__div_pi32_pi64
	.quad	_L_1+5893
	.quad	_A__div_vi32_vi64
	.quad	_L_1+5907
	.quad	_A__div_pi32_pu32
	.quad	_L_1+5921
	.quad	_A__div_vi32_vu32
	.quad	_L_1+5935
	.quad	_A__div_pi32_pi32
	.quad	_L_1+5949
	.quad	_A__div_vi32_vi32
	.quad	_L_1+5963
	.quad	_A__div_pi32_pu16
	.quad	_L_1+5977
	.quad	_A__div_vi32_vu16
	.quad	_L_1+5991
	.quad	_A__div_pi32_pi16
	.quad	_L_1+6005
	.quad	_A__div_vi32_vi16
	.quad	_L_1+6019
	.quad	_A__div_pi32_pu8
	.quad	_L_1+6033
	.quad	_A__div_vi32_vu8
	.quad	_L_1+6046
	.quad	_A__div_pi32_pi8
	.quad	_L_1+6059
	.quad	_A__div_vi32_vi8
	.quad	_L_1+6072
	.quad	_A__mult_pi32_pu64
	.quad	_L_1+6085
	.quad	_A__mult_vi32_vu64
	.quad	_L_1+6100
	.quad	_A__mult_pi32_pi64
	.quad	_L_1+6115
	.quad	_A__mult_vi32_vi64
	.quad	_L_1+6130
	.quad	_A__mult_pi32_pu32
	.quad	_L_1+6145
	.quad	_A__mult_vi32_vu32
	.quad	_L_1+6160
	.quad	_A__mult_pi32_pi32
	.quad	_L_1+6175
	.quad	_A__mult_vi32_vi32
	.quad	_L_1+6190
	.quad	_A__mult_pi32_pu16
	.quad	_L_1+6205
	.quad	_A__mult_vi32_vu16
	.quad	_L_1+6220
	.quad	_A__mult_pi32_pi16
	.quad	_L_1+6235
	.quad	_A__mult_vi32_vi16
	.quad	_L_1+6250
	.quad	_A__mult_pi32_pu8
	.quad	_L_1+6265
	.quad	_A__mult_vi32_vu8
	.quad	_L_1+6279
	.quad	_A__mult_pi32_pi8
	.quad	_L_1+6293
	.quad	_A__mult_vi32_vi8
	.quad	_L_1+6307
	.quad	_A__xor_pi32_pu64
	.quad	_L_1+6321
	.quad	_A__xor_vi32_vu64
	.quad	_L_1+6335
	.quad	_A__xor_pi32_pi64
	.quad	_L_1+6349
	.quad	_A__xor_vi32_vi64
	.quad	_L_1+6363
	.quad	_A__xor_pi32_pu32
	.quad	_L_1+6377
	.quad	_A__xor_vi32_vu32
	.quad	_L_1+6391
	.quad	_A__xor_pi32_pi32
	.quad	_L_1+6405
	.quad	_A__xor_vi32_vi32
	.quad	_L_1+6419
	.quad	_A__xor_pi32_pu16
	.quad	_L_1+6433
	.quad	_A__xor_vi32_vu16
	.quad	_L_1+6447
	.quad	_A__xor_pi32_pi16
	.quad	_L_1+6461
	.quad	_A__xor_vi32_vi16
	.quad	_L_1+6475
	.quad	_A__xor_pi32_pu8
	.quad	_L_1+6489
	.quad	_A__xor_vi32_vu8
	.quad	_L_1+6502
	.quad	_A__xor_pi32_pi8
	.quad	_L_1+6515
	.quad	_A__xor_vi32_vi8
	.quad	_L_1+6528
	.quad	_A__or_pi32_pu64
	.quad	_L_1+6541
	.quad	_A__or_vi32_vu64
	.quad	_L_1+6554
	.quad	_A__or_pi32_pi64
	.quad	_L_1+6567
	.quad	_A__or_vi32_vi64
	.quad	_L_1+6580
	.quad	_A__or_pi32_pu32
	.quad	_L_1+6593
	.quad	_A__or_vi32_vu32
	.quad	_L_1+6606
	.quad	_A__or_pi32_pi32
	.quad	_L_1+6619
	.quad	_A__or_vi32_vi32
	.quad	_L_1+6632
	.quad	_A__or_pi32_pu16
	.quad	_L_1+6645
	.quad	_A__or_vi32_vu16
	.quad	_L_1+6658
	.quad	_A__or_pi32_pi16
	.quad	_L_1+6671
	.quad	_A__or_vi32_vi16
	.quad	_L_1+6684
	.quad	_A__or_pi32_pu8
	.quad	_L_1+6697
	.quad	_A__or_vi32_vu8
	.quad	_L_1+6709
	.quad	_A__or_pi32_pi8
	.quad	_L_1+6721
	.quad	_A__or_vi32_vi8
	.quad	_L_1+6733
	.quad	_A__and_pi32_pu64
	.quad	_L_1+6745
	.quad	_A__and_vi32_vu64
	.quad	_L_1+6759
	.quad	_A__and_pi32_pi64
	.quad	_L_1+6773
	.quad	_A__and_vi32_vi64
	.quad	_L_1+6787
	.quad	_A__and_pi32_pu32
	.quad	_L_1+6801
	.quad	_A__and_vi32_vu32
	.quad	_L_1+6815
	.quad	_A__and_pi32_pi32
	.quad	_L_1+6829
	.quad	_A__and_vi32_vi32
	.quad	_L_1+6843
	.quad	_A__and_pi32_pu16
	.quad	_L_1+6857
	.quad	_A__and_vi32_vu16
	.quad	_L_1+6871
	.quad	_A__and_pi32_pi16
	.quad	_L_1+6885
	.quad	_A__and_vi32_vi16
	.quad	_L_1+6899
	.quad	_A__and_pi32_pu8
	.quad	_L_1+6913
	.quad	_A__and_vi32_vu8
	.quad	_L_1+6926
	.quad	_A__and_pi32_pi8
	.quad	_L_1+6939
	.quad	_A__and_vi32_vi8
	.quad	_L_1+6952
	.quad	_A__sub_pi32_pu64
	.quad	_L_1+6965
	.quad	_A__sub_vi32_vu64
	.quad	_L_1+6979
	.quad	_A__sub_pi32_pi64
	.quad	_L_1+6993
	.quad	_A__sub_vi32_vi64
	.quad	_L_1+7007
	.quad	_A__sub_pi32_pu32
	.quad	_L_1+7021
	.quad	_A__sub_vi32_vu32
	.quad	_L_1+7035
	.quad	_A__sub_pi32_pi32
	.quad	_L_1+7049
	.quad	_A__sub_vi32_vi32
	.quad	_L_1+7063
	.quad	_A__sub_pi32_pu16
	.quad	_L_1+7077
	.quad	_A__sub_vi32_vu16
	.quad	_L_1+7091
	.quad	_A__sub_pi32_pi16
	.quad	_L_1+7105
	.quad	_A__sub_vi32_vi16
	.quad	_L_1+7119
	.quad	_A__sub_pi32_pu8
	.quad	_L_1+7133
	.quad	_A__sub_vi32_vu8
	.quad	_L_1+7146
	.quad	_A__sub_pi32_pi8
	.quad	_L_1+7159
	.quad	_A__sub_vi32_vi8
	.quad	_L_1+7172
	.quad	_A__add_pi32_pu64
	.quad	_L_1+7185
	.quad	_A__add_vi32_vu64
	.quad	_L_1+7199
	.quad	_A__add_pi32_pi64
	.quad	_L_1+7213
	.quad	_A__add_vi32_vi64
	.quad	_L_1+7227
	.quad	_A__add_pi32_pu32
	.quad	_L_1+7241
	.quad	_A__add_vi32_vu32
	.quad	_L_1+7255
	.quad	_A__add_pi32_pi32
	.quad	_L_1+7269
	.quad	_A__add_vi32_vi32
	.quad	_L_1+7283
	.quad	_A__add_pi32_pu16
	.quad	_L_1+7297
	.quad	_A__add_vi32_vu16
	.quad	_L_1+7311
	.quad	_A__add_pi32_pi16
	.quad	_L_1+7325
	.quad	_A__add_vi32_vi16
	.quad	_L_1+7339
	.quad	_A__add_pi32_pu8
	.quad	_L_1+7353
	.quad	_A__add_vi32_vu8
	.quad	_L_1+7366
	.quad	_A__add_pi32_pi8
	.quad	_L_1+7379
	.quad	_A__add_vi32_vi8
	.quad	_L_1+7392
	.quad	_A__ret_pi32_u64
	.quad	_L_1+7405
	.quad	_A__ret_pi32_i64
	.quad	_L_1+7418
	.quad	_A__ret_pi32_u32
	.quad	_L_1+7431
	.quad	_A__ret_pi32_i32
	.quad	_L_1+7444
	.quad	_A__ret_pi32_u16
	.quad	_L_1+7457
	.quad	_A__ret_pi32_i16
	.quad	_L_1+7470
	.quad	_A__ret_pi32_u8
	.quad	_L_1+7483
	.quad	_A__ret_pi32_i8
	.quad	_L_1+7495
	.quad	_A__ret_ki32
	.quad	_L_1+7507
	.quad	_A__ret_vi32
	.quad	_L_1+7516
	.quad	_A__mod_pu16_pu64
	.quad	_L_1+7525
	.quad	_A__mod_vu16_vu64
	.quad	_L_1+7539
	.quad	_A__mod_pu16_pi64
	.quad	_L_1+7553
	.quad	_A__mod_vu16_vi64
	.quad	_L_1+7567
	.quad	_A__mod_pu16_pu32
	.quad	_L_1+7581
	.quad	_A__mod_vu16_vu32
	.quad	_L_1+7595
	.quad	_A__mod_pu16_pi32
	.quad	_L_1+7609
	.quad	_A__mod_vu16_vi32
	.quad	_L_1+7623
	.quad	_A__mod_pu16_pu16
	.quad	_L_1+7637
	.quad	_A__mod_vu16_vu16
	.quad	_L_1+7651
	.quad	_A__mod_pu16_pi16
	.quad	_L_1+7665
	.quad	_A__mod_vu16_vi16
	.quad	_L_1+7679
	.quad	_A__mod_pu16_pu8
	.quad	_L_1+7693
	.quad	_A__mod_vu16_vu8
	.quad	_L_1+7706
	.quad	_A__mod_pu16_pi8
	.quad	_L_1+7719
	.quad	_A__mod_vu16_vi8
	.quad	_L_1+7732
	.quad	_A__div_pu16_pu64
	.quad	_L_1+7745
	.quad	_A__div_vu16_vu64
	.quad	_L_1+7759
	.quad	_A__div_pu16_pi64
	.quad	_L_1+7773
	.quad	_A__div_vu16_vi64
	.quad	_L_1+7787
	.quad	_A__div_pu16_pu32
	.quad	_L_1+7801
	.quad	_A__div_vu16_vu32
	.quad	_L_1+7815
	.quad	_A__div_pu16_pi32
	.quad	_L_1+7829
	.quad	_A__div_vu16_vi32
	.quad	_L_1+7843
	.quad	_A__div_pu16_pu16
	.quad	_L_1+7857
	.quad	_A__div_vu16_vu16
	.quad	_L_1+7871
	.quad	_A__div_pu16_pi16
	.quad	_L_1+7885
	.quad	_A__div_vu16_vi16
	.quad	_L_1+7899
	.quad	_A__div_pu16_pu8
	.quad	_L_1+7913
	.quad	_A__div_vu16_vu8
	.quad	_L_1+7926
	.quad	_A__div_pu16_pi8
	.quad	_L_1+7939
	.quad	_A__div_vu16_vi8
	.quad	_L_1+7952
	.quad	_A__mult_pu16_pu64
	.quad	_L_1+7965
	.quad	_A__mult_vu16_vu64
	.quad	_L_1+7980
	.quad	_A__mult_pu16_pi64
	.quad	_L_1+7995
	.quad	_A__mult_vu16_vi64
	.quad	_L_1+8010
	.quad	_A__mult_pu16_pu32
	.quad	_L_1+8025
	.quad	_A__mult_vu16_vu32
	.quad	_L_1+8040
	.quad	_A__mult_pu16_pi32
	.quad	_L_1+8055
	.quad	_A__mult_vu16_vi32
	.quad	_L_1+8070
	.quad	_A__mult_pu16_pu16
	.quad	_L_1+8085
	.quad	_A__mult_vu16_vu16
	.quad	_L_1+8100
	.quad	_A__mult_pu16_pi16
	.quad	_L_1+8115
	.quad	_A__mult_vu16_vi16
	.quad	_L_1+8130
	.quad	_A__mult_pu16_pu8
	.quad	_L_1+8145
	.quad	_A__mult_vu16_vu8
	.quad	_L_1+8159
	.quad	_A__mult_pu16_pi8
	.quad	_L_1+8173
	.quad	_A__mult_vu16_vi8
	.quad	_L_1+8187
	.quad	_A__xor_pu16_pu64
	.quad	_L_1+8201
	.quad	_A__xor_vu16_vu64
	.quad	_L_1+8215
	.quad	_A__xor_pu16_pi64
	.quad	_L_1+8229
	.quad	_A__xor_vu16_vi64
	.quad	_L_1+8243
	.quad	_A__xor_pu16_pu32
	.quad	_L_1+8257
	.quad	_A__xor_vu16_vu32
	.quad	_L_1+8271
	.quad	_A__xor_pu16_pi32
	.quad	_L_1+8285
	.quad	_A__xor_vu16_vi32
	.quad	_L_1+8299
	.quad	_A__xor_pu16_pu16
	.quad	_L_1+8313
	.quad	_A__xor_vu16_vu16
	.quad	_L_1+8327
	.quad	_A__xor_pu16_pi16
	.quad	_L_1+8341
	.quad	_A__xor_vu16_vi16
	.quad	_L_1+8355
	.quad	_A__xor_pu16_pu8
	.quad	_L_1+8369
	.quad	_A__xor_vu16_vu8
	.quad	_L_1+8382
	.quad	_A__xor_pu16_pi8
	.quad	_L_1+8395
	.quad	_A__xor_vu16_vi8
	.quad	_L_1+8408
	.quad	_A__or_pu16_pu64
	.quad	_L_1+8421
	.quad	_A__or_vu16_vu64
	.quad	_L_1+8434
	.quad	_A__or_pu16_pi64
	.quad	_L_1+8447
	.quad	_A__or_vu16_vi64
	.quad	_L_1+8460
	.quad	_A__or_pu16_pu32
	.quad	_L_1+8473
	.quad	_A__or_vu16_vu32
	.quad	_L_1+8486
	.quad	_A__or_pu16_pi32
	.quad	_L_1+8499
	.quad	_A__or_vu16_vi32
	.quad	_L_1+8512
	.quad	_A__or_pu16_pu16
	.quad	_L_1+8525
	.quad	_A__or_vu16_vu16
	.quad	_L_1+8538
	.quad	_A__or_pu16_pi16
	.quad	_L_1+8551
	.quad	_A__or_vu16_vi16
	.quad	_L_1+8564
	.quad	_A__or_pu16_pu8
	.quad	_L_1+8577
	.quad	_A__or_vu16_vu8
	.quad	_L_1+8589
	.quad	_A__or_pu16_pi8
	.quad	_L_1+8601
	.quad	_A__or_vu16_vi8
	.quad	_L_1+8613
	.quad	_A__and_pu16_pu64
	.quad	_L_1+8625
	.quad	_A__and_vu16_vu64
	.quad	_L_1+8639
	.quad	_A__and_pu16_pi64
	.quad	_L_1+8653
	.quad	_A__and_vu16_vi64
	.quad	_L_1+8667
	.quad	_A__and_pu16_pu32
	.quad	_L_1+8681
	.quad	_A__and_vu16_vu32
	.quad	_L_1+8695
	.quad	_A__and_pu16_pi32
	.quad	_L_1+8709
	.quad	_A__and_vu16_vi32
	.quad	_L_1+8723
	.quad	_A__and_pu16_pu16
	.quad	_L_1+8737
	.quad	_A__and_vu16_vu16
	.quad	_L_1+8751
	.quad	_A__and_pu16_pi16
	.quad	_L_1+8765
	.quad	_A__and_vu16_vi16
	.quad	_L_1+8779
	.quad	_A__and_pu16_pu8
	.quad	_L_1+8793
	.quad	_A__and_vu16_vu8
	.quad	_L_1+8806
	.quad	_A__and_pu16_pi8
	.quad	_L_1+8819
	.quad	_A__and_vu16_vi8
	.quad	_L_1+8832
	.quad	_A__sub_pu16_pu64
	.quad	_L_1+8845
	.quad	_A__sub_vu16_vu64
	.quad	_L_1+8859
	.quad	_A__sub_pu16_pi64
	.quad	_L_1+8873
	.quad	_A__sub_vu16_vi64
	.quad	_L_1+8887
	.quad	_A__sub_pu16_pu32
	.quad	_L_1+8901
	.quad	_A__sub_vu16_vu32
	.quad	_L_1+8915
	.quad	_A__sub_pu16_pi32
	.quad	_L_1+8929
	.quad	_A__sub_vu16_vi32
	.quad	_L_1+8943
	.quad	_A__sub_pu16_pu16
	.quad	_L_1+8957
	.quad	_A__sub_vu16_vu16
	.quad	_L_1+8971
	.quad	_A__sub_pu16_pi16
	.quad	_L_1+8985
	.quad	_A__sub_vu16_vi16
	.quad	_L_1+8999
	.quad	_A__sub_pu16_pu8
	.quad	_L_1+9013
	.quad	_A__sub_vu16_vu8
	.quad	_L_1+9026
	.quad	_A__sub_pu16_pi8
	.quad	_L_1+9039
	.quad	_A__sub_vu16_vi8
	.quad	_L_1+9052
	.quad	_A__add_pu16_pu64
	.quad	_L_1+9065
	.quad	_A__add_vu16_vu64
	.quad	_L_1+9079
	.quad	_A__add_pu16_pi64
	.quad	_L_1+9093
	.quad	_A__add_vu16_vi64
	.quad	_L_1+9107
	.quad	_A__add_pu16_pu32
	.quad	_L_1+9121
	.quad	_A__add_vu16_vu32
	.quad	_L_1+9135
	.quad	_A__add_pu16_pi32
	.quad	_L_1+9149
	.quad	_A__add_vu16_vi32
	.quad	_L_1+9163
	.quad	_A__add_pu16_pu16
	.quad	_L_1+9177
	.quad	_A__add_vu16_vu16
	.quad	_L_1+9191
	.quad	_A__add_pu16_pi16
	.quad	_L_1+9205
	.quad	_A__add_vu16_vi16
	.quad	_L_1+9219
	.quad	_A__add_pu16_pu8
	.quad	_L_1+9233
	.quad	_A__add_vu16_vu8
	.quad	_L_1+9246
	.quad	_A__add_pu16_pi8
	.quad	_L_1+9259
	.quad	_A__add_vu16_vi8
	.quad	_L_1+9272
	.quad	_A__ret_pu16_u64
	.quad	_L_1+9285
	.quad	_A__ret_pu16_i64
	.quad	_L_1+9298
	.quad	_A__ret_pu16_u32
	.quad	_L_1+9311
	.quad	_A__ret_pu16_i32
	.quad	_L_1+9324
	.quad	_A__ret_pu16_u16
	.quad	_L_1+9337
	.quad	_A__ret_pu16_i16
	.quad	_L_1+9350
	.quad	_A__ret_pu16_u8
	.quad	_L_1+9363
	.quad	_A__ret_pu16_i8
	.quad	_L_1+9375
	.quad	_A__ret_ku16
	.quad	_L_1+9387
	.quad	_A__ret_vu16
	.quad	_L_1+9396
	.quad	_A__mod_pi16_pu64
	.quad	_L_1+9405
	.quad	_A__mod_vi16_vu64
	.quad	_L_1+9419
	.quad	_A__mod_pi16_pi64
	.quad	_L_1+9433
	.quad	_A__mod_vi16_vi64
	.quad	_L_1+9447
	.quad	_A__mod_pi16_pu32
	.quad	_L_1+9461
	.quad	_A__mod_vi16_vu32
	.quad	_L_1+9475
	.quad	_A__mod_pi16_pi32
	.quad	_L_1+9489
	.quad	_A__mod_vi16_vi32
	.quad	_L_1+9503
	.quad	_A__mod_pi16_pu16
	.quad	_L_1+9517
	.quad	_A__mod_vi16_vu16
	.quad	_L_1+9531
	.quad	_A__mod_pi16_pi16
	.quad	_L_1+9545
	.quad	_A__mod_vi16_vi16
	.quad	_L_1+9559
	.quad	_A__mod_pi16_pu8
	.quad	_L_1+9573
	.quad	_A__mod_vi16_vu8
	.quad	_L_1+9586
	.quad	_A__mod_pi16_pi8
	.quad	_L_1+9599
	.quad	_A__mod_vi16_vi8
	.quad	_L_1+9612
	.quad	_A__div_pi16_pu64
	.quad	_L_1+9625
	.quad	_A__div_vi16_vu64
	.quad	_L_1+9639
	.quad	_A__div_pi16_pi64
	.quad	_L_1+9653
	.quad	_A__div_vi16_vi64
	.quad	_L_1+9667
	.quad	_A__div_pi16_pu32
	.quad	_L_1+9681
	.quad	_A__div_vi16_vu32
	.quad	_L_1+9695
	.quad	_A__div_pi16_pi32
	.quad	_L_1+9709
	.quad	_A__div_vi16_vi32
	.quad	_L_1+9723
	.quad	_A__div_pi16_pu16
	.quad	_L_1+9737
	.quad	_A__div_vi16_vu16
	.quad	_L_1+9751
	.quad	_A__div_pi16_pi16
	.quad	_L_1+9765
	.quad	_A__div_vi16_vi16
	.quad	_L_1+9779
	.quad	_A__div_pi16_pu8
	.quad	_L_1+9793
	.quad	_A__div_vi16_vu8
	.quad	_L_1+9806
	.quad	_A__div_pi16_pi8
	.quad	_L_1+9819
	.quad	_A__div_vi16_vi8
	.quad	_L_1+9832
	.quad	_A__mult_pi16_pu64
	.quad	_L_1+9845
	.quad	_A__mult_vi16_vu64
	.quad	_L_1+9860
	.quad	_A__mult_pi16_pi64
	.quad	_L_1+9875
	.quad	_A__mult_vi16_vi64
	.quad	_L_1+9890
	.quad	_A__mult_pi16_pu32
	.quad	_L_1+9905
	.quad	_A__mult_vi16_vu32
	.quad	_L_1+9920
	.quad	_A__mult_pi16_pi32
	.quad	_L_1+9935
	.quad	_A__mult_vi16_vi32
	.quad	_L_1+9950
	.quad	_A__mult_pi16_pu16
	.quad	_L_1+9965
	.quad	_A__mult_vi16_vu16
	.quad	_L_1+9980
	.quad	_A__mult_pi16_pi16
	.quad	_L_1+9995
	.quad	_A__mult_vi16_vi16
	.quad	_L_1+10010
	.quad	_A__mult_pi16_pu8
	.quad	_L_1+10025
	.quad	_A__mult_vi16_vu8
	.quad	_L_1+10039
	.quad	_A__mult_pi16_pi8
	.quad	_L_1+10053
	.quad	_A__mult_vi16_vi8
	.quad	_L_1+10067
	.quad	_A__xor_pi16_pu64
	.quad	_L_1+10081
	.quad	_A__xor_vi16_vu64
	.quad	_L_1+10095
	.quad	_A__xor_pi16_pi64
	.quad	_L_1+10109
	.quad	_A__xor_vi16_vi64
	.quad	_L_1+10123
	.quad	_A__xor_pi16_pu32
	.quad	_L_1+10137
	.quad	_A__xor_vi16_vu32
	.quad	_L_1+10151
	.quad	_A__xor_pi16_pi32
	.quad	_L_1+10165
	.quad	_A__xor_vi16_vi32
	.quad	_L_1+10179
	.quad	_A__xor_pi16_pu16
	.quad	_L_1+10193
	.quad	_A__xor_vi16_vu16
	.quad	_L_1+10207
	.quad	_A__xor_pi16_pi16
	.quad	_L_1+10221
	.quad	_A__xor_vi16_vi16
	.quad	_L_1+10235
	.quad	_A__xor_pi16_pu8
	.quad	_L_1+10249
	.quad	_A__xor_vi16_vu8
	.quad	_L_1+10262
	.quad	_A__xor_pi16_pi8
	.quad	_L_1+10275
	.quad	_A__xor_vi16_vi8
	.quad	_L_1+10288
	.quad	_A__or_pi16_pu64
	.quad	_L_1+10301
	.quad	_A__or_vi16_vu64
	.quad	_L_1+10314
	.quad	_A__or_pi16_pi64
	.quad	_L_1+10327
	.quad	_A__or_vi16_vi64
	.quad	_L_1+10340
	.quad	_A__or_pi16_pu32
	.quad	_L_1+10353
	.quad	_A__or_vi16_vu32
	.quad	_L_1+10366
	.quad	_A__or_pi16_pi32
	.quad	_L_1+10379
	.quad	_A__or_vi16_vi32
	.quad	_L_1+10392
	.quad	_A__or_pi16_pu16
	.quad	_L_1+10405
	.quad	_A__or_vi16_vu16
	.quad	_L_1+10418
	.quad	_A__or_pi16_pi16
	.quad	_L_1+10431
	.quad	_A__or_vi16_vi16
	.quad	_L_1+10444
	.quad	_A__or_pi16_pu8
	.quad	_L_1+10457
	.quad	_A__or_vi16_vu8
	.quad	_L_1+10469
	.quad	_A__or_pi16_pi8
	.quad	_L_1+10481
	.quad	_A__or_vi16_vi8
	.quad	_L_1+10493
	.quad	_A__and_pi16_pu64
	.quad	_L_1+10505
	.quad	_A__and_vi16_vu64
	.quad	_L_1+10519
	.quad	_A__and_pi16_pi64
	.quad	_L_1+10533
	.quad	_A__and_vi16_vi64
	.quad	_L_1+10547
	.quad	_A__and_pi16_pu32
	.quad	_L_1+10561
	.quad	_A__and_vi16_vu32
	.quad	_L_1+10575
	.quad	_A__and_pi16_pi32
	.quad	_L_1+10589
	.quad	_A__and_vi16_vi32
	.quad	_L_1+10603
	.quad	_A__and_pi16_pu16
	.quad	_L_1+10617
	.quad	_A__and_vi16_vu16
	.quad	_L_1+10631
	.quad	_A__and_pi16_pi16
	.quad	_L_1+10645
	.quad	_A__and_vi16_vi16
	.quad	_L_1+10659
	.quad	_A__and_pi16_pu8
	.quad	_L_1+10673
	.quad	_A__and_vi16_vu8
	.quad	_L_1+10686
	.quad	_A__and_pi16_pi8
	.quad	_L_1+10699
	.quad	_A__and_vi16_vi8
	.quad	_L_1+10712
	.quad	_A__sub_pi16_pu64
	.quad	_L_1+10725
	.quad	_A__sub_vi16_vu64
	.quad	_L_1+10739
	.quad	_A__sub_pi16_pi64
	.quad	_L_1+10753
	.quad	_A__sub_vi16_vi64
	.quad	_L_1+10767
	.quad	_A__sub_pi16_pu32
	.quad	_L_1+10781
	.quad	_A__sub_vi16_vu32
	.quad	_L_1+10795
	.quad	_A__sub_pi16_pi32
	.quad	_L_1+10809
	.quad	_A__sub_vi16_vi32
	.quad	_L_1+10823
	.quad	_A__sub_pi16_pu16
	.quad	_L_1+10837
	.quad	_A__sub_vi16_vu16
	.quad	_L_1+10851
	.quad	_A__sub_pi16_pi16
	.quad	_L_1+10865
	.quad	_A__sub_vi16_vi16
	.quad	_L_1+10879
	.quad	_A__sub_pi16_pu8
	.quad	_L_1+10893
	.quad	_A__sub_vi16_vu8
	.quad	_L_1+10906
	.quad	_A__sub_pi16_pi8
	.quad	_L_1+10919
	.quad	_A__sub_vi16_vi8
	.quad	_L_1+10932
	.quad	_A__add_pi16_pu64
	.quad	_L_1+10945
	.quad	_A__add_vi16_vu64
	.quad	_L_1+10959
	.quad	_A__add_pi16_pi64
	.quad	_L_1+10973
	.quad	_A__add_vi16_vi64
	.quad	_L_1+10987
	.quad	_A__add_pi16_pu32
	.quad	_L_1+11001
	.quad	_A__add_vi16_vu32
	.quad	_L_1+11015
	.quad	_A__add_pi16_pi32
	.quad	_L_1+11029
	.quad	_A__add_vi16_vi32
	.quad	_L_1+11043
	.quad	_A__add_pi16_pu16
	.quad	_L_1+11057
	.quad	_A__add_vi16_vu16
	.quad	_L_1+11071
	.quad	_A__add_pi16_pi16
	.quad	_L_1+11085
	.quad	_A__add_vi16_vi16
	.quad	_L_1+11099
	.quad	_A__add_pi16_pu8
	.quad	_L_1+11113
	.quad	_A__add_vi16_vu8
	.quad	_L_1+11126
	.quad	_A__add_pi16_pi8
	.quad	_L_1+11139
	.quad	_A__add_vi16_vi8
	.quad	_L_1+11152
	.quad	_A__ret_pi16_u64
	.quad	_L_1+11165
	.quad	_A__ret_pi16_i64
	.quad	_L_1+11178
	.quad	_A__ret_pi16_u32
	.quad	_L_1+11191
	.quad	_A__ret_pi16_i32
	.quad	_L_1+11204
	.quad	_A__ret_pi16_u16
	.quad	_L_1+11217
	.quad	_A__ret_pi16_i16
	.quad	_L_1+11230
	.quad	_A__ret_pi16_u8
	.quad	_L_1+11243
	.quad	_A__ret_pi16_i8
	.quad	_L_1+11255
	.quad	_A__ret_ki16
	.quad	_L_1+11267
	.quad	_A__ret_vi16
	.quad	_L_1+11276
	.quad	_A__mod_pu8_pu64
	.quad	_L_1+11285
	.quad	_A__mod_vu8_vu64
	.quad	_L_1+11298
	.quad	_A__mod_pu8_pi64
	.quad	_L_1+11311
	.quad	_A__mod_vu8_vi64
	.quad	_L_1+11324
	.quad	_A__mod_pu8_pu32
	.quad	_L_1+11337
	.quad	_A__mod_vu8_vu32
	.quad	_L_1+11350
	.quad	_A__mod_pu8_pi32
	.quad	_L_1+11363
	.quad	_A__mod_vu8_vi32
	.quad	_L_1+11376
	.quad	_A__mod_pu8_pu16
	.quad	_L_1+11389
	.quad	_A__mod_vu8_vu16
	.quad	_L_1+11402
	.quad	_A__mod_pu8_pi16
	.quad	_L_1+11415
	.quad	_A__mod_vu8_vi16
	.quad	_L_1+11428
	.quad	_A__mod_pu8_pu8
	.quad	_L_1+11441
	.quad	_A__mod_vu8_vu8
	.quad	_L_1+11453
	.quad	_A__mod_pu8_pi8
	.quad	_L_1+11465
	.quad	_A__mod_vu8_vi8
	.quad	_L_1+11477
	.quad	_A__div_pu8_pu64
	.quad	_L_1+11489
	.quad	_A__div_vu8_vu64
	.quad	_L_1+11502
	.quad	_A__div_pu8_pi64
	.quad	_L_1+11515
	.quad	_A__div_vu8_vi64
	.quad	_L_1+11528
	.quad	_A__div_pu8_pu32
	.quad	_L_1+11541
	.quad	_A__div_vu8_vu32
	.quad	_L_1+11554
	.quad	_A__div_pu8_pi32
	.quad	_L_1+11567
	.quad	_A__div_vu8_vi32
	.quad	_L_1+11580
	.quad	_A__div_pu8_pu16
	.quad	_L_1+11593
	.quad	_A__div_vu8_vu16
	.quad	_L_1+11606
	.quad	_A__div_pu8_pi16
	.quad	_L_1+11619
	.quad	_A__div_vu8_vi16
	.quad	_L_1+11632
	.quad	_A__div_pu8_pu8
	.quad	_L_1+11645
	.quad	_A__div_vu8_vu8
	.quad	_L_1+11657
	.quad	_A__div_pu8_pi8
	.quad	_L_1+11669
	.quad	_A__div_vu8_vi8
	.quad	_L_1+11681
	.quad	_A__mult_pu8_pu64
	.quad	_L_1+11693
	.quad	_A__mult_vu8_vu64
	.quad	_L_1+11707
	.quad	_A__mult_pu8_pi64
	.quad	_L_1+11721
	.quad	_A__mult_vu8_vi64
	.quad	_L_1+11735
	.quad	_A__mult_pu8_pu32
	.quad	_L_1+11749
	.quad	_A__mult_vu8_vu32
	.quad	_L_1+11763
	.quad	_A__mult_pu8_pi32
	.quad	_L_1+11777
	.quad	_A__mult_vu8_vi32
	.quad	_L_1+11791
	.quad	_A__mult_pu8_pu16
	.quad	_L_1+11805
	.quad	_A__mult_vu8_vu16
	.quad	_L_1+11819
	.quad	_A__mult_pu8_pi16
	.quad	_L_1+11833
	.quad	_A__mult_vu8_vi16
	.quad	_L_1+11847
	.quad	_A__mult_pu8_pu8
	.quad	_L_1+11861
	.quad	_A__mult_vu8_vu8
	.quad	_L_1+11874
	.quad	_A__mult_pu8_pi8
	.quad	_L_1+11887
	.quad	_A__mult_vu8_vi8
	.quad	_L_1+11900
	.quad	_A__xor_pu8_pu64
	.quad	_L_1+11913
	.quad	_A__xor_vu8_vu64
	.quad	_L_1+11926
	.quad	_A__xor_pu8_pi64
	.quad	_L_1+11939
	.quad	_A__xor_vu8_vi64
	.quad	_L_1+11952
	.quad	_A__xor_pu8_pu32
	.quad	_L_1+11965
	.quad	_A__xor_vu8_vu32
	.quad	_L_1+11978
	.quad	_A__xor_pu8_pi32
	.quad	_L_1+11991
	.quad	_A__xor_vu8_vi32
	.quad	_L_1+12004
	.quad	_A__xor_pu8_pu16
	.quad	_L_1+12017
	.quad	_A__xor_vu8_vu16
	.quad	_L_1+12030
	.quad	_A__xor_pu8_pi16
	.quad	_L_1+12043
	.quad	_A__xor_vu8_vi16
	.quad	_L_1+12056
	.quad	_A__xor_pu8_pu8
	.quad	_L_1+12069
	.quad	_A__xor_vu8_vu8
	.quad	_L_1+12081
	.quad	_A__xor_pu8_pi8
	.quad	_L_1+12093
	.quad	_A__xor_vu8_vi8
	.quad	_L_1+12105
	.quad	_A__or_pu8_pu64
	.quad	_L_1+12117
	.quad	_A__or_vu8_vu64
	.quad	_L_1+12129
	.quad	_A__or_pu8_pi64
	.quad	_L_1+12141
	.quad	_A__or_vu8_vi64
	.quad	_L_1+12153
	.quad	_A__or_pu8_pu32
	.quad	_L_1+12165
	.quad	_A__or_vu8_vu32
	.quad	_L_1+12177
	.quad	_A__or_pu8_pi32
	.quad	_L_1+12189
	.quad	_A__or_vu8_vi32
	.quad	_L_1+12201
	.quad	_A__or_pu8_pu16
	.quad	_L_1+12213
	.quad	_A__or_vu8_vu16
	.quad	_L_1+12225
	.quad	_A__or_pu8_pi16
	.quad	_L_1+12237
	.quad	_A__or_vu8_vi16
	.quad	_L_1+12249
	.quad	_A__or_pu8_pu8
	.quad	_L_1+12261
	.quad	_A__or_vu8_vu8
	.quad	_L_1+12272
	.quad	_A__or_pu8_pi8
	.quad	_L_1+12283
	.quad	_A__or_vu8_vi8
	.quad	_L_1+12294
	.quad	_A__and_pu8_pu64
	.quad	_L_1+12305
	.quad	_A__and_vu8_vu64
	.quad	_L_1+12318
	.quad	_A__and_pu8_pi64
	.quad	_L_1+12331
	.quad	_A__and_vu8_vi64
	.quad	_L_1+12344
	.quad	_A__and_pu8_pu32
	.quad	_L_1+12357
	.quad	_A__and_vu8_vu32
	.quad	_L_1+12370
	.quad	_A__and_pu8_pi32
	.quad	_L_1+12383
	.quad	_A__and_vu8_vi32
	.quad	_L_1+12396
	.quad	_A__and_pu8_pu16
	.quad	_L_1+12409
	.quad	_A__and_vu8_vu16
	.quad	_L_1+12422
	.quad	_A__and_pu8_pi16
	.quad	_L_1+12435
	.quad	_A__and_vu8_vi16
	.quad	_L_1+12448
	.quad	_A__and_pu8_pu8
	.quad	_L_1+12461
	.quad	_A__and_vu8_vu8
	.quad	_L_1+12473
	.quad	_A__and_pu8_pi8
	.quad	_L_1+12485
	.quad	_A__and_vu8_vi8
	.quad	_L_1+12497
	.quad	_A__sub_pu8_pu64
	.quad	_L_1+12509
	.quad	_A__sub_vu8_vu64
	.quad	_L_1+12522
	.quad	_A__sub_pu8_pi64
	.quad	_L_1+12535
	.quad	_A__sub_vu8_vi64
	.quad	_L_1+12548
	.quad	_A__sub_pu8_pu32
	.quad	_L_1+12561
	.quad	_A__sub_vu8_vu32
	.quad	_L_1+12574
	.quad	_A__sub_pu8_pi32
	.quad	_L_1+12587
	.quad	_A__sub_vu8_vi32
	.quad	_L_1+12600
	.quad	_A__sub_pu8_pu16
	.quad	_L_1+12613
	.quad	_A__sub_vu8_vu16
	.quad	_L_1+12626
	.quad	_A__sub_pu8_pi16
	.quad	_L_1+12639
	.quad	_A__sub_vu8_vi16
	.quad	_L_1+12652
	.quad	_A__sub_pu8_pu8
	.quad	_L_1+12665
	.quad	_A__sub_vu8_vu8
	.quad	_L_1+12677
	.quad	_A__sub_pu8_pi8
	.quad	_L_1+12689
	.quad	_A__sub_vu8_vi8
	.quad	_L_1+12701
	.quad	_A__add_pu8_pu64
	.quad	_L_1+12713
	.quad	_A__add_vu8_vu64
	.quad	_L_1+12726
	.quad	_A__add_pu8_pi64
	.quad	_L_1+12739
	.quad	_A__add_vu8_vi64
	.quad	_L_1+12752
	.quad	_A__add_pu8_pu32
	.quad	_L_1+12765
	.quad	_A__add_vu8_vu32
	.quad	_L_1+12778
	.quad	_A__add_pu8_pi32
	.quad	_L_1+12791
	.quad	_A__add_vu8_vi32
	.quad	_L_1+12804
	.quad	_A__add_pu8_pu16
	.quad	_L_1+12817
	.quad	_A__add_vu8_vu16
	.quad	_L_1+12830
	.quad	_A__add_pu8_pi16
	.quad	_L_1+12843
	.quad	_A__add_vu8_vi16
	.quad	_L_1+12856
	.quad	_A__add_pu8_pu8
	.quad	_L_1+12869
	.quad	_A__add_vu8_vu8
	.quad	_L_1+12881
	.quad	_A__add_pu8_pi8
	.quad	_L_1+12893
	.quad	_A__add_vu8_vi8
	.quad	_L_1+12905
	.quad	_A__ret_pu8_u64
	.quad	_L_1+12917
	.quad	_A__ret_pu8_i64
	.quad	_L_1+12929
	.quad	_A__ret_pu8_u32
	.quad	_L_1+12941
	.quad	_A__ret_pu8_i32
	.quad	_L_1+12953
	.quad	_A__ret_pu8_u16
	.quad	_L_1+12965
	.quad	_A__ret_pu8_i16
	.quad	_L_1+12977
	.quad	_A__ret_pu8_u8
	.quad	_L_1+12989
	.quad	_A__ret_pu8_i8
	.quad	_L_1+13000
	.quad	_A__ret_ku8
	.quad	_L_1+13011
	.quad	_A__ret_vu8
	.quad	_L_1+13019
	.quad	_A__mod_pi8_pu64
	.quad	_L_1+13027
	.quad	_A__mod_vi8_vu64
	.quad	_L_1+13040
	.quad	_A__mod_pi8_pi64
	.quad	_L_1+13053
	.quad	_A__mod_vi8_vi64
	.quad	_L_1+13066
	.quad	_A__mod_pi8_pu32
	.quad	_L_1+13079
	.quad	_A__mod_vi8_vu32
	.quad	_L_1+13092
	.quad	_A__mod_pi8_pi32
	.quad	_L_1+13105
	.quad	_A__mod_vi8_vi32
	.quad	_L_1+13118
	.quad	_A__mod_pi8_pu16
	.quad	_L_1+13131
	.quad	_A__mod_vi8_vu16
	.quad	_L_1+13144
	.quad	_A__mod_pi8_pi16
	.quad	_L_1+13157
	.quad	_A__mod_vi8_vi16
	.quad	_L_1+13170
	.quad	_A__mod_pi8_pu8
	.quad	_L_1+13183
	.quad	_A__mod_vi8_vu8
	.quad	_L_1+13195
	.quad	_A__mod_pi8_pi8
	.quad	_L_1+13207
	.quad	_A__mod_vi8_vi8
	.quad	_L_1+13219
	.quad	_A__div_pi8_pu64
	.quad	_L_1+13231
	.quad	_A__div_vi8_vu64
	.quad	_L_1+13244
	.quad	_A__div_pi8_pi64
	.quad	_L_1+13257
	.quad	_A__div_vi8_vi64
	.quad	_L_1+13270
	.quad	_A__div_pi8_pu32
	.quad	_L_1+13283
	.quad	_A__div_vi8_vu32
	.quad	_L_1+13296
	.quad	_A__div_pi8_pi32
	.quad	_L_1+13309
	.quad	_A__div_vi8_vi32
	.quad	_L_1+13322
	.quad	_A__div_pi8_pu16
	.quad	_L_1+13335
	.quad	_A__div_vi8_vu16
	.quad	_L_1+13348
	.quad	_A__div_pi8_pi16
	.quad	_L_1+13361
	.quad	_A__div_vi8_vi16
	.quad	_L_1+13374
	.quad	_A__div_pi8_pu8
	.quad	_L_1+13387
	.quad	_A__div_vi8_vu8
	.quad	_L_1+13399
	.quad	_A__div_pi8_pi8
	.quad	_L_1+13411
	.quad	_A__div_vi8_vi8
	.quad	_L_1+13423
	.quad	_A__mult_pi8_pu64
	.quad	_L_1+13435
	.quad	_A__mult_vi8_vu64
	.quad	_L_1+13449
	.quad	_A__mult_pi8_pi64
	.quad	_L_1+13463
	.quad	_A__mult_vi8_vi64
	.quad	_L_1+13477
	.quad	_A__mult_pi8_pu32
	.quad	_L_1+13491
	.quad	_A__mult_vi8_vu32
	.quad	_L_1+13505
	.quad	_A__mult_pi8_pi32
	.quad	_L_1+13519
	.quad	_A__mult_vi8_vi32
	.quad	_L_1+13533
	.quad	_A__mult_pi8_pu16
	.quad	_L_1+13547
	.quad	_A__mult_vi8_vu16
	.quad	_L_1+13561
	.quad	_A__mult_pi8_pi16
	.quad	_L_1+13575
	.quad	_A__mult_vi8_vi16
	.quad	_L_1+13589
	.quad	_A__mult_pi8_pu8
	.quad	_L_1+13603
	.quad	_A__mult_vi8_vu8
	.quad	_L_1+13616
	.quad	_A__mult_pi8_pi8
	.quad	_L_1+13629
	.quad	_A__mult_vi8_vi8
	.quad	_L_1+13642
	.quad	_A__xor_pi8_pu64
	.quad	_L_1+13655
	.quad	_A__xor_vi8_vu64
	.quad	_L_1+13668
	.quad	_A__xor_pi8_pi64
	.quad	_L_1+13681
	.quad	_A__xor_vi8_vi64
	.quad	_L_1+13694
	.quad	_A__xor_pi8_pu32
	.quad	_L_1+13707
	.quad	_A__xor_vi8_vu32
	.quad	_L_1+13720
	.quad	_A__xor_pi8_pi32
	.quad	_L_1+13733
	.quad	_A__xor_vi8_vi32
	.quad	_L_1+13746
	.quad	_A__xor_pi8_pu16
	.quad	_L_1+13759
	.quad	_A__xor_vi8_vu16
	.quad	_L_1+13772
	.quad	_A__xor_pi8_pi16
	.quad	_L_1+13785
	.quad	_A__xor_vi8_vi16
	.quad	_L_1+13798
	.quad	_A__xor_pi8_pu8
	.quad	_L_1+13811
	.quad	_A__xor_vi8_vu8
	.quad	_L_1+13823
	.quad	_A__xor_pi8_pi8
	.quad	_L_1+13835
	.quad	_A__xor_vi8_vi8
	.quad	_L_1+13847
	.quad	_A__or_pi8_pu64
	.quad	_L_1+13859
	.quad	_A__or_vi8_vu64
	.quad	_L_1+13871
	.quad	_A__or_pi8_pi64
	.quad	_L_1+13883
	.quad	_A__or_vi8_vi64
	.quad	_L_1+13895
	.quad	_A__or_pi8_pu32
	.quad	_L_1+13907
	.quad	_A__or_vi8_vu32
	.quad	_L_1+13919
	.quad	_A__or_pi8_pi32
	.quad	_L_1+13931
	.quad	_A__or_vi8_vi32
	.quad	_L_1+13943
	.quad	_A__or_pi8_pu16
	.quad	_L_1+13955
	.quad	_A__or_vi8_vu16
	.quad	_L_1+13967
	.quad	_A__or_pi8_pi16
	.quad	_L_1+13979
	.quad	_A__or_vi8_vi16
	.quad	_L_1+13991
	.quad	_A__or_pi8_pu8
	.quad	_L_1+14003
	.quad	_A__or_vi8_vu8
	.quad	_L_1+14014
	.quad	_A__or_pi8_pi8
	.quad	_L_1+14025
	.quad	_A__or_vi8_vi8
	.quad	_L_1+14036
	.quad	_A__and_pi8_pu64
	.quad	_L_1+14047
	.quad	_A__and_vi8_vu64
	.quad	_L_1+14060
	.quad	_A__and_pi8_pi64
	.quad	_L_1+14073
	.quad	_A__and_vi8_vi64
	.quad	_L_1+14086
	.quad	_A__and_pi8_pu32
	.quad	_L_1+14099
	.quad	_A__and_vi8_vu32
	.quad	_L_1+14112
	.quad	_A__and_pi8_pi32
	.quad	_L_1+14125
	.quad	_A__and_vi8_vi32
	.quad	_L_1+14138
	.quad	_A__and_pi8_pu16
	.quad	_L_1+14151
	.quad	_A__and_vi8_vu16
	.quad	_L_1+14164
	.quad	_A__and_pi8_pi16
	.quad	_L_1+14177
	.quad	_A__and_vi8_vi16
	.quad	_L_1+14190
	.quad	_A__and_pi8_pu8
	.quad	_L_1+14203
	.quad	_A__and_vi8_vu8
	.quad	_L_1+14215
	.quad	_A__and_pi8_pi8
	.quad	_L_1+14227
	.quad	_A__and_vi8_vi8
	.quad	_L_1+14239
	.quad	_A__sub_pi8_pu64
	.quad	_L_1+14251
	.quad	_A__sub_vi8_vu64
	.quad	_L_1+14264
	.quad	_A__sub_pi8_pi64
	.quad	_L_1+14277
	.quad	_A__sub_vi8_vi64
	.quad	_L_1+14290
	.quad	_A__sub_pi8_pu32
	.quad	_L_1+14303
	.quad	_A__sub_vi8_vu32
	.quad	_L_1+14316
	.quad	_A__sub_pi8_pi32
	.quad	_L_1+14329
	.quad	_A__sub_vi8_vi32
	.quad	_L_1+14342
	.quad	_A__sub_pi8_pu16
	.quad	_L_1+14355
	.quad	_A__sub_vi8_vu16
	.quad	_L_1+14368
	.quad	_A__sub_pi8_pi16
	.quad	_L_1+14381
	.quad	_A__sub_vi8_vi16
	.quad	_L_1+14394
	.quad	_A__sub_pi8_pu8
	.quad	_L_1+14407
	.quad	_A__sub_vi8_vu8
	.quad	_L_1+14419
	.quad	_A__sub_pi8_pi8
	.quad	_L_1+14431
	.quad	_A__sub_vi8_vi8
	.quad	_L_1+14443
	.quad	_A__add_pi8_pu64
	.quad	_L_1+14455
	.quad	_A__add_vi8_vu64
	.quad	_L_1+14468
	.quad	_A__add_pi8_pi64
	.quad	_L_1+14481
	.quad	_A__add_vi8_vi64
	.quad	_L_1+14494
	.quad	_A__add_pi8_pu32
	.quad	_L_1+14507
	.quad	_A__add_vi8_vu32
	.quad	_L_1+14520
	.quad	_A__add_pi8_pi32
	.quad	_L_1+14533
	.quad	_A__add_vi8_vi32
	.quad	_L_1+14546
	.quad	_A__add_pi8_pu16
	.quad	_L_1+14559
	.quad	_A__add_vi8_vu16
	.quad	_L_1+14572
	.quad	_A__add_pi8_pi16
	.quad	_L_1+14585
	.quad	_A__add_vi8_vi16
	.quad	_L_1+14598
	.quad	_A__add_pi8_pu8
	.quad	_L_1+14611
	.quad	_A__add_vi8_vu8
	.quad	_L_1+14623
	.quad	_A__add_pi8_pi8
	.quad	_L_1+14635
	.quad	_A__add_vi8_vi8
	.quad	_L_1+14647
	.quad	_A__ret_pi8_u64
	.quad	_L_1+14659
	.quad	_A__ret_pi8_i64
	.quad	_L_1+14671
	.quad	_A__ret_pi8_u32
	.quad	_L_1+14683
	.quad	_A__ret_pi8_i32
	.quad	_L_1+14695
	.quad	_A__ret_pi8_u16
	.quad	_L_1+14707
	.quad	_A__ret_pi8_i16
	.quad	_L_1+14719
	.quad	_A__ret_pi8_u8
	.quad	_L_1+14731
	.quad	_A__ret_pi8_i8
	.quad	_L_1+14742
	.quad	_A__ret_ki8
	.quad	_L_1+14753
	.quad	_A__ret_vi8
	.quad	_L_1+14761
	.space 8
	.ascii "../AMD64_DARWIN/A.m3"
	.space 4
	.data
	.align 5
_MM_A:
	.quad	_L_1+32464
	.space 32
	.quad	_L_1+14776
	.space 24
	.quad	_MM_A+136
	.space 8
	.quad	_A_M3
	.quad	3
	.byte	1
	.byte	4
	.word	7
	.word	10
	.space 2
	.long	13
	.long	16
	.quad	19
	.quad	22
	.space 8
	.quad	_A_I3
	.quad	_MM_A+160
	.space 8
	.quad	_Long_I3
	.quad	_MM_A+184
	.space 8
	.quad	_Word_I3
	.quad	_MM_A+208
	.space 8
	.quad	_Cstdint_I3
	.quad	_MM_A+232
	.space 8
	.quad	_RTHooks_I3
	.space 8
	.subsections_via_symbols
