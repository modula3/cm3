	.file	"LT.mc"
	.section	".text"
	.align 4
	.type	LT__uLT_var_u16_u16, #function
	.proc	014
LT__uLT_var_u16_u16:
.LLFB0:
	save	%sp, -96, %sp
.LLCFI0:
	mov	0, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE0:
	.size	LT__uLT_var_u16_u16, .-LT__uLT_var_u16_u16
	.align 4
	.type	LT__LT_var_u16_u16, #function
	.proc	014
LT__LT_var_u16_u16:
.LLFB1:
	save	%sp, -96, %sp
.LLCFI1:
	mov	0, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1:
	.size	LT__LT_var_u16_u16, .-LT__LT_var_u16_u16
	.align 4
	.type	LT__uLT_param_u16_u16, #function
	.proc	014
LT__uLT_param_u16_u16:
.LLFB2:
	save	%sp, -96, %sp
.LLCFI2:
	sth	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE2:
	.size	LT__uLT_param_u16_u16, .-LT__uLT_param_u16_u16
	.align 4
	.type	LT__LT_param_u16_u16, #function
	.proc	014
LT__LT_param_u16_u16:
.LLFB3:
	save	%sp, -96, %sp
.LLCFI3:
	sth	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE3:
	.size	LT__LT_param_u16_u16, .-LT__LT_param_u16_u16
	.align 4
	.type	LT__uLT_var_u16_u64, #function
	.proc	014
LT__uLT_var_u16_u64:
.LLFB4:
	save	%sp, -96, %sp
.LLCFI4:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL6
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL7
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL6
	 nop
	cmp	%o3, %o5
.LL7:
	mov	0, %g1
.LL6:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE4:
	.size	LT__uLT_var_u16_u64, .-LT__uLT_var_u16_u64
	.align 4
	.type	LT__LT_var_u16_u64, #function
	.proc	014
LT__LT_var_u16_u64:
.LLFB5:
	save	%sp, -96, %sp
.LLCFI5:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL9
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL10
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL9
	 nop
	cmp	%o3, %o5
.LL10:
	mov	0, %g1
.LL9:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE5:
	.size	LT__LT_var_u16_u64, .-LT__LT_var_u16_u64
	.align 4
	.type	LT__uLT_param_u16_u64, #function
	.proc	014
LT__uLT_param_u16_u64:
.LLFB6:
	save	%sp, -104, %sp
.LLCFI6:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL12
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL13
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL12
	 nop
	cmp	%o3, %o5
.LL13:
	mov	0, %g1
.LL12:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE6:
	.size	LT__uLT_param_u16_u64, .-LT__uLT_param_u16_u64
	.align 4
	.type	LT__LT_param_u16_u64, #function
	.proc	014
LT__LT_param_u16_u64:
.LLFB7:
	save	%sp, -104, %sp
.LLCFI7:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bg	%icc, .LL15
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL16
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL15
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL16:
	mov	0, %g1
.LL15:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE7:
	.size	LT__LT_param_u16_u64, .-LT__LT_param_u16_u64
	.align 4
	.type	LT__uLT_var_u16_LC, #function
	.proc	014
LT__uLT_var_u16_LC:
.LLFB8:
	save	%sp, -96, %sp
.LLCFI8:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL18
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL19
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL18
	 nop
	cmp	%o3, %o5
.LL19:
	mov	0, %g1
.LL18:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE8:
	.size	LT__uLT_var_u16_LC, .-LT__uLT_var_u16_LC
	.align 4
	.type	LT__LT_var_u16_LC, #function
	.proc	014
LT__LT_var_u16_LC:
.LLFB9:
	save	%sp, -96, %sp
.LLCFI9:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL21
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL22
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL21
	 nop
	cmp	%o3, %o5
.LL22:
	mov	0, %g1
.LL21:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE9:
	.size	LT__LT_var_u16_LC, .-LT__LT_var_u16_LC
	.align 4
	.type	LT__uLT_param_u16_LC, #function
	.proc	014
LT__uLT_param_u16_LC:
.LLFB10:
	save	%sp, -104, %sp
.LLCFI10:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bgu	%icc, .LL24
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL25
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL24
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL25:
	mov	0, %g1
.LL24:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE10:
	.size	LT__uLT_param_u16_LC, .-LT__uLT_param_u16_LC
	.align 4
	.type	LT__LT_param_u16_LC, #function
	.proc	014
LT__LT_param_u16_LC:
.LLFB11:
	save	%sp, -104, %sp
.LLCFI11:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL27
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL28
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL27
	 nop
	cmp	%o3, %o5
.LL28:
	mov	0, %g1
.LL27:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE11:
	.size	LT__LT_param_u16_LC, .-LT__LT_param_u16_LC
	.align 4
	.type	LT__uLT_var_u16_i32, #function
	.proc	014
LT__uLT_var_u16_i32:
.LLFB12:
	save	%sp, -96, %sp
.LLCFI12:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE12:
	.size	LT__uLT_var_u16_i32, .-LT__uLT_var_u16_i32
	.align 4
	.type	LT__LT_var_u16_i32, #function
	.proc	014
LT__LT_var_u16_i32:
.LLFB13:
	save	%sp, -96, %sp
.LLCFI13:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE13:
	.size	LT__LT_var_u16_i32, .-LT__LT_var_u16_i32
	.align 4
	.type	LT__uLT_param_u16_i32, #function
	.proc	014
LT__uLT_param_u16_i32:
.LLFB14:
	save	%sp, -96, %sp
.LLCFI14:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE14:
	.size	LT__uLT_param_u16_i32, .-LT__uLT_param_u16_i32
	.align 4
	.type	LT__LT_param_u16_i32, #function
	.proc	014
LT__LT_param_u16_i32:
.LLFB15:
	save	%sp, -96, %sp
.LLCFI15:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE15:
	.size	LT__LT_param_u16_i32, .-LT__LT_param_u16_i32
	.align 4
	.type	LT__uLT_var_u16_i16, #function
	.proc	014
LT__uLT_var_u16_i16:
.LLFB16:
	save	%sp, -96, %sp
.LLCFI16:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE16:
	.size	LT__uLT_var_u16_i16, .-LT__uLT_var_u16_i16
	.align 4
	.type	LT__LT_var_u16_i16, #function
	.proc	014
LT__LT_var_u16_i16:
.LLFB17:
	save	%sp, -96, %sp
.LLCFI17:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE17:
	.size	LT__LT_var_u16_i16, .-LT__LT_var_u16_i16
	.align 4
	.type	LT__uLT_param_u16_i16, #function
	.proc	014
LT__uLT_param_u16_i16:
.LLFB18:
	save	%sp, -96, %sp
.LLCFI18:
	sth	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE18:
	.size	LT__uLT_param_u16_i16, .-LT__uLT_param_u16_i16
	.align 4
	.type	LT__LT_param_u16_i16, #function
	.proc	014
LT__LT_param_u16_i16:
.LLFB19:
	save	%sp, -96, %sp
.LLCFI19:
	sth	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE19:
	.size	LT__LT_param_u16_i16, .-LT__LT_param_u16_i16
	.align 4
	.type	LT__uLT_var_u16_I, #function
	.proc	014
LT__uLT_var_u16_I:
.LLFB20:
	save	%sp, -96, %sp
.LLCFI20:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE20:
	.size	LT__uLT_var_u16_I, .-LT__uLT_var_u16_I
	.align 4
	.type	LT__LT_var_u16_I, #function
	.proc	014
LT__LT_var_u16_I:
.LLFB21:
	save	%sp, -96, %sp
.LLCFI21:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE21:
	.size	LT__LT_var_u16_I, .-LT__LT_var_u16_I
	.align 4
	.type	LT__uLT_param_u16_I, #function
	.proc	014
LT__uLT_param_u16_I:
.LLFB22:
	save	%sp, -96, %sp
.LLCFI22:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE22:
	.size	LT__uLT_param_u16_I, .-LT__uLT_param_u16_I
	.align 4
	.type	LT__LT_param_u16_I, #function
	.proc	014
LT__LT_param_u16_I:
.LLFB23:
	save	%sp, -96, %sp
.LLCFI23:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE23:
	.size	LT__LT_param_u16_I, .-LT__LT_param_u16_I
	.align 4
	.type	LT__uLT_var_u16_i64, #function
	.proc	014
LT__uLT_var_u16_i64:
.LLFB24:
	save	%sp, -96, %sp
.LLCFI24:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL42
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL43
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL42
	 nop
	cmp	%o3, %o5
.LL43:
	mov	0, %g1
.LL42:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE24:
	.size	LT__uLT_var_u16_i64, .-LT__uLT_var_u16_i64
	.align 4
	.type	LT__LT_var_u16_i64, #function
	.proc	014
LT__LT_var_u16_i64:
.LLFB25:
	save	%sp, -96, %sp
.LLCFI25:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL45
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL46
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL45
	 nop
	cmp	%o3, %o5
.LL46:
	mov	0, %g1
.LL45:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE25:
	.size	LT__LT_var_u16_i64, .-LT__LT_var_u16_i64
	.align 4
	.type	LT__uLT_param_u16_i64, #function
	.proc	014
LT__uLT_param_u16_i64:
.LLFB26:
	save	%sp, -104, %sp
.LLCFI26:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL48
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL49
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL48
	 nop
	cmp	%o3, %o5
.LL49:
	mov	0, %g1
.LL48:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE26:
	.size	LT__uLT_param_u16_i64, .-LT__uLT_param_u16_i64
	.align 4
	.type	LT__LT_param_u16_i64, #function
	.proc	014
LT__LT_param_u16_i64:
.LLFB27:
	save	%sp, -104, %sp
.LLCFI27:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bg	%icc, .LL51
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL52
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL51
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL52:
	mov	0, %g1
.LL51:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE27:
	.size	LT__LT_param_u16_i64, .-LT__LT_param_u16_i64
	.align 4
	.type	LT__uLT_var_u16_C, #function
	.proc	014
LT__uLT_var_u16_C:
.LLFB28:
	save	%sp, -96, %sp
.LLCFI28:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE28:
	.size	LT__uLT_var_u16_C, .-LT__uLT_var_u16_C
	.align 4
	.type	LT__LT_var_u16_C, #function
	.proc	014
LT__LT_var_u16_C:
.LLFB29:
	save	%sp, -96, %sp
.LLCFI29:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE29:
	.size	LT__LT_var_u16_C, .-LT__LT_var_u16_C
	.align 4
	.type	LT__uLT_param_u16_C, #function
	.proc	014
LT__uLT_param_u16_C:
.LLFB30:
	save	%sp, -96, %sp
.LLCFI30:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE30:
	.size	LT__uLT_param_u16_C, .-LT__uLT_param_u16_C
	.align 4
	.type	LT__LT_param_u16_C, #function
	.proc	014
LT__LT_param_u16_C:
.LLFB31:
	save	%sp, -96, %sp
.LLCFI31:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE31:
	.size	LT__LT_param_u16_C, .-LT__LT_param_u16_C
	.align 4
	.type	LT__uLT_var_u16_u8, #function
	.proc	014
LT__uLT_var_u16_u8:
.LLFB32:
	save	%sp, -96, %sp
.LLCFI32:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE32:
	.size	LT__uLT_var_u16_u8, .-LT__uLT_var_u16_u8
	.align 4
	.type	LT__LT_var_u16_u8, #function
	.proc	014
LT__LT_var_u16_u8:
.LLFB33:
	save	%sp, -96, %sp
.LLCFI33:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE33:
	.size	LT__LT_var_u16_u8, .-LT__LT_var_u16_u8
	.align 4
	.type	LT__uLT_param_u16_u8, #function
	.proc	014
LT__uLT_param_u16_u8:
.LLFB34:
	save	%sp, -96, %sp
.LLCFI34:
	sth	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE34:
	.size	LT__uLT_param_u16_u8, .-LT__uLT_param_u16_u8
	.align 4
	.type	LT__LT_param_u16_u8, #function
	.proc	014
LT__LT_param_u16_u8:
.LLFB35:
	save	%sp, -96, %sp
.LLCFI35:
	sth	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE35:
	.size	LT__LT_param_u16_u8, .-LT__LT_param_u16_u8
	.align 4
	.type	LT__uLT_var_u16_L, #function
	.proc	014
LT__uLT_var_u16_L:
.LLFB36:
	save	%sp, -96, %sp
.LLCFI36:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL62
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL63
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL62
	 nop
	cmp	%o3, %o5
.LL63:
	mov	0, %g1
.LL62:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE36:
	.size	LT__uLT_var_u16_L, .-LT__uLT_var_u16_L
	.align 4
	.type	LT__LT_var_u16_L, #function
	.proc	014
LT__LT_var_u16_L:
.LLFB37:
	save	%sp, -96, %sp
.LLCFI37:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL65
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL66
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL65
	 nop
	cmp	%o3, %o5
.LL66:
	mov	0, %g1
.LL65:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE37:
	.size	LT__LT_var_u16_L, .-LT__LT_var_u16_L
	.align 4
	.type	LT__uLT_param_u16_L, #function
	.proc	014
LT__uLT_param_u16_L:
.LLFB38:
	save	%sp, -104, %sp
.LLCFI38:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL68
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL69
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL68
	 nop
	cmp	%o3, %o5
.LL69:
	mov	0, %g1
.LL68:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE38:
	.size	LT__uLT_param_u16_L, .-LT__uLT_param_u16_L
	.align 4
	.type	LT__LT_param_u16_L, #function
	.proc	014
LT__LT_param_u16_L:
.LLFB39:
	save	%sp, -104, %sp
.LLCFI39:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bg	%icc, .LL71
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL72
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL71
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL72:
	mov	0, %g1
.LL71:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE39:
	.size	LT__LT_param_u16_L, .-LT__LT_param_u16_L
	.align 4
	.type	LT__uLT_var_u16_i8, #function
	.proc	014
LT__uLT_var_u16_i8:
.LLFB40:
	save	%sp, -96, %sp
.LLCFI40:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE40:
	.size	LT__uLT_var_u16_i8, .-LT__uLT_var_u16_i8
	.align 4
	.type	LT__LT_var_u16_i8, #function
	.proc	014
LT__LT_var_u16_i8:
.LLFB41:
	save	%sp, -96, %sp
.LLCFI41:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE41:
	.size	LT__LT_var_u16_i8, .-LT__LT_var_u16_i8
	.align 4
	.type	LT__uLT_param_u16_i8, #function
	.proc	014
LT__uLT_param_u16_i8:
.LLFB42:
	save	%sp, -96, %sp
.LLCFI42:
	sth	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE42:
	.size	LT__uLT_param_u16_i8, .-LT__uLT_param_u16_i8
	.align 4
	.type	LT__LT_param_u16_i8, #function
	.proc	014
LT__LT_param_u16_i8:
.LLFB43:
	save	%sp, -96, %sp
.LLCFI43:
	sth	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE43:
	.size	LT__LT_param_u16_i8, .-LT__LT_param_u16_i8
	.align 4
	.type	LT__uLT_var_u16_u32, #function
	.proc	014
LT__uLT_var_u16_u32:
.LLFB44:
	save	%sp, -96, %sp
.LLCFI44:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE44:
	.size	LT__uLT_var_u16_u32, .-LT__uLT_var_u16_u32
	.align 4
	.type	LT__LT_var_u16_u32, #function
	.proc	014
LT__LT_var_u16_u32:
.LLFB45:
	save	%sp, -96, %sp
.LLCFI45:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE45:
	.size	LT__LT_var_u16_u32, .-LT__LT_var_u16_u32
	.align 4
	.type	LT__uLT_param_u16_u32, #function
	.proc	014
LT__uLT_param_u16_u32:
.LLFB46:
	save	%sp, -96, %sp
.LLCFI46:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE46:
	.size	LT__uLT_param_u16_u32, .-LT__uLT_param_u16_u32
	.align 4
	.type	LT__LT_param_u16_u32, #function
	.proc	014
LT__LT_param_u16_u32:
.LLFB47:
	save	%sp, -96, %sp
.LLCFI47:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE47:
	.size	LT__LT_param_u16_u32, .-LT__LT_param_u16_u32
	.align 4
	.type	LT__uLT_var_u64_u16, #function
	.proc	014
LT__uLT_var_u64_u16:
.LLFB48:
	save	%sp, -96, %sp
.LLCFI48:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL82
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL83
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL82
	 nop
	cmp	%o3, %o5
.LL83:
	mov	0, %g1
.LL82:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE48:
	.size	LT__uLT_var_u64_u16, .-LT__uLT_var_u64_u16
	.align 4
	.type	LT__LT_var_u64_u16, #function
	.proc	014
LT__LT_var_u64_u16:
.LLFB49:
	save	%sp, -96, %sp
.LLCFI49:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL85
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL86
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL85
	 nop
	cmp	%o3, %o5
.LL86:
	mov	0, %g1
.LL85:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE49:
	.size	LT__LT_var_u64_u16, .-LT__LT_var_u64_u16
	.align 4
	.type	LT__uLT_param_u64_u16, #function
	.proc	014
LT__uLT_param_u64_u16:
.LLFB50:
	save	%sp, -104, %sp
.LLCFI50:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL88
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL89
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL88
	 nop
	cmp	%o3, %o5
.LL89:
	mov	0, %g1
.LL88:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE50:
	.size	LT__uLT_param_u64_u16, .-LT__uLT_param_u64_u16
	.align 4
	.type	LT__LT_param_u64_u16, #function
	.proc	014
LT__LT_param_u64_u16:
.LLFB51:
	save	%sp, -104, %sp
.LLCFI51:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bl	%icc, .LL91
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL92
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL91
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL92:
	mov	0, %g1
.LL91:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE51:
	.size	LT__LT_param_u64_u16, .-LT__LT_param_u64_u16
	.align 4
	.type	LT__uLT_var_u64_u64, #function
	.proc	014
LT__uLT_var_u64_u64:
.LLFB52:
	save	%sp, -96, %sp
.LLCFI52:
	mov	0, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE52:
	.size	LT__uLT_var_u64_u64, .-LT__uLT_var_u64_u64
	.align 4
	.type	LT__LT_var_u64_u64, #function
	.proc	014
LT__LT_var_u64_u64:
.LLFB53:
	save	%sp, -96, %sp
.LLCFI53:
	mov	0, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE53:
	.size	LT__LT_var_u64_u64, .-LT__LT_var_u64_u64
	.align 4
	.type	LT__uLT_param_u64_u64, #function
	.proc	014
LT__uLT_param_u64_u64:
.LLFB54:
	save	%sp, -112, %sp
.LLCFI54:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	ldd	[%fp-16], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL96
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL97
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL96
	 nop
	cmp	%o3, %o5
.LL97:
	mov	0, %g1
.LL96:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE54:
	.size	LT__uLT_param_u64_u64, .-LT__uLT_param_u64_u64
	.align 4
	.type	LT__LT_param_u64_u64, #function
	.proc	014
LT__LT_param_u64_u64:
.LLFB55:
	save	%sp, -112, %sp
.LLCFI55:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	mov	1, %g1
	ld	[%fp-16], %o4
	ld	[%fp-8], %o5
	cmp	%o4, %o5
	bg	%icc, .LL99
	 nop
	ld	[%fp-16], %o4
	ld	[%fp-8], %o5
	cmp	%o4, %o5
	bne	%icc, .LL100
	 nop
	ld	[%fp-12], %o4
	ld	[%fp-4], %o5
	cmp	%o4, %o5
	bgu	%icc, .LL99
	 nop
	ld	[%fp-12], %o5
	ld	[%fp-4], %g1
	cmp	%o5, %g1
.LL100:
	mov	0, %g1
.LL99:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE55:
	.size	LT__LT_param_u64_u64, .-LT__LT_param_u64_u64
	.align 4
	.type	LT__uLT_var_u64_LC, #function
	.proc	014
LT__uLT_var_u64_LC:
.LLFB56:
	save	%sp, -96, %sp
.LLCFI56:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL102
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL103
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL102
	 nop
	cmp	%o3, %o5
.LL103:
	mov	0, %g1
.LL102:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE56:
	.size	LT__uLT_var_u64_LC, .-LT__uLT_var_u64_LC
	.align 4
	.type	LT__LT_var_u64_LC, #function
	.proc	014
LT__LT_var_u64_LC:
.LLFB57:
	save	%sp, -96, %sp
.LLCFI57:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL105
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL106
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL105
	 nop
	cmp	%o3, %o5
.LL106:
	mov	0, %g1
.LL105:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE57:
	.size	LT__LT_var_u64_LC, .-LT__LT_var_u64_LC
	.align 4
	.type	LT__uLT_param_u64_LC, #function
	.proc	014
LT__uLT_param_u64_LC:
.LLFB58:
	save	%sp, -112, %sp
.LLCFI58:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	mov	1, %g1
	ld	[%fp-16], %o3
	cmp	%o3, %o4
	bgu	%icc, .LL108
	 nop
	ld	[%fp-16], %o3
	cmp	%o3, %o4
	bne	%icc, .LL109
	 nop
	ld	[%fp-12], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL108
	 nop
	ld	[%fp-12], %g1
	cmp	%g1, %o5
.LL109:
	mov	0, %g1
.LL108:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE58:
	.size	LT__uLT_param_u64_LC, .-LT__uLT_param_u64_LC
	.align 4
	.type	LT__LT_param_u64_LC, #function
	.proc	014
LT__LT_param_u64_LC:
.LLFB59:
	save	%sp, -112, %sp
.LLCFI59:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bl	%icc, .LL111
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL112
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL111
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL112:
	mov	0, %g1
.LL111:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE59:
	.size	LT__LT_param_u64_LC, .-LT__LT_param_u64_LC
	.align 4
	.type	LT__uLT_var_u64_i32, #function
	.proc	014
LT__uLT_var_u64_i32:
.LLFB60:
	save	%sp, -96, %sp
.LLCFI60:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL114
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL115
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL114
	 nop
	cmp	%o5, %o3
.LL115:
	mov	0, %g1
.LL114:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE60:
	.size	LT__uLT_var_u64_i32, .-LT__uLT_var_u64_i32
	.align 4
	.type	LT__LT_var_u64_i32, #function
	.proc	014
LT__LT_var_u64_i32:
.LLFB61:
	save	%sp, -96, %sp
.LLCFI61:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL117
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL118
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL117
	 nop
	cmp	%o5, %o3
.LL118:
	mov	0, %g1
.LL117:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE61:
	.size	LT__LT_var_u64_i32, .-LT__LT_var_u64_i32
	.align 4
	.type	LT__uLT_param_u64_i32, #function
	.proc	014
LT__uLT_param_u64_i32:
.LLFB62:
	save	%sp, -104, %sp
.LLCFI62:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL120
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL121
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL120
	 nop
	cmp	%o5, %o3
.LL121:
	mov	0, %g1
.LL120:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE62:
	.size	LT__uLT_param_u64_i32, .-LT__uLT_param_u64_i32
	.align 4
	.type	LT__LT_param_u64_i32, #function
	.proc	014
LT__LT_param_u64_i32:
.LLFB63:
	save	%sp, -104, %sp
.LLCFI63:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bl	%icc, .LL123
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL124
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL123
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL124:
	mov	0, %g1
.LL123:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE63:
	.size	LT__LT_param_u64_i32, .-LT__LT_param_u64_i32
	.align 4
	.type	LT__uLT_var_u64_i16, #function
	.proc	014
LT__uLT_var_u64_i16:
.LLFB64:
	save	%sp, -96, %sp
.LLCFI64:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL126
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL127
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL126
	 nop
	cmp	%o5, %o3
.LL127:
	mov	0, %g1
.LL126:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE64:
	.size	LT__uLT_var_u64_i16, .-LT__uLT_var_u64_i16
	.align 4
	.type	LT__LT_var_u64_i16, #function
	.proc	014
LT__LT_var_u64_i16:
.LLFB65:
	save	%sp, -96, %sp
.LLCFI65:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL129
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL130
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL129
	 nop
	cmp	%o5, %o3
.LL130:
	mov	0, %g1
.LL129:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE65:
	.size	LT__LT_var_u64_i16, .-LT__LT_var_u64_i16
	.align 4
	.type	LT__uLT_param_u64_i16, #function
	.proc	014
LT__uLT_param_u64_i16:
.LLFB66:
	save	%sp, -104, %sp
.LLCFI66:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL132
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL133
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL132
	 nop
	cmp	%o5, %o3
.LL133:
	mov	0, %g1
.LL132:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE66:
	.size	LT__uLT_param_u64_i16, .-LT__uLT_param_u64_i16
	.align 4
	.type	LT__LT_param_u64_i16, #function
	.proc	014
LT__LT_param_u64_i16:
.LLFB67:
	save	%sp, -104, %sp
.LLCFI67:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bl	%icc, .LL135
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL136
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL135
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL136:
	mov	0, %g1
.LL135:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE67:
	.size	LT__LT_param_u64_i16, .-LT__LT_param_u64_i16
	.align 4
	.type	LT__uLT_var_u64_I, #function
	.proc	014
LT__uLT_var_u64_I:
.LLFB68:
	save	%sp, -96, %sp
.LLCFI68:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL138
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL139
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL138
	 nop
	cmp	%o5, %o3
.LL139:
	mov	0, %g1
.LL138:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE68:
	.size	LT__uLT_var_u64_I, .-LT__uLT_var_u64_I
	.align 4
	.type	LT__LT_var_u64_I, #function
	.proc	014
LT__LT_var_u64_I:
.LLFB69:
	save	%sp, -96, %sp
.LLCFI69:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL141
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL142
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL141
	 nop
	cmp	%o5, %o3
.LL142:
	mov	0, %g1
.LL141:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE69:
	.size	LT__LT_var_u64_I, .-LT__LT_var_u64_I
	.align 4
	.type	LT__uLT_param_u64_I, #function
	.proc	014
LT__uLT_param_u64_I:
.LLFB70:
	save	%sp, -104, %sp
.LLCFI70:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL144
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL145
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL144
	 nop
	cmp	%o5, %o3
.LL145:
	mov	0, %g1
.LL144:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE70:
	.size	LT__uLT_param_u64_I, .-LT__uLT_param_u64_I
	.align 4
	.type	LT__LT_param_u64_I, #function
	.proc	014
LT__LT_param_u64_I:
.LLFB71:
	save	%sp, -104, %sp
.LLCFI71:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bl	%icc, .LL147
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL148
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL147
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL148:
	mov	0, %g1
.LL147:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE71:
	.size	LT__LT_param_u64_I, .-LT__LT_param_u64_I
	.align 4
	.type	LT__uLT_var_u64_i64, #function
	.proc	014
LT__uLT_var_u64_i64:
.LLFB72:
	save	%sp, -96, %sp
.LLCFI72:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL150
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL151
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL150
	 nop
	cmp	%o3, %o5
.LL151:
	mov	0, %g1
.LL150:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE72:
	.size	LT__uLT_var_u64_i64, .-LT__uLT_var_u64_i64
	.align 4
	.type	LT__LT_var_u64_i64, #function
	.proc	014
LT__LT_var_u64_i64:
.LLFB73:
	save	%sp, -96, %sp
.LLCFI73:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL153
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL154
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL153
	 nop
	cmp	%o3, %o5
.LL154:
	mov	0, %g1
.LL153:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE73:
	.size	LT__LT_var_u64_i64, .-LT__LT_var_u64_i64
	.align 4
	.type	LT__uLT_param_u64_i64, #function
	.proc	014
LT__uLT_param_u64_i64:
.LLFB74:
	save	%sp, -112, %sp
.LLCFI74:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	ldd	[%fp-16], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL156
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL157
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL156
	 nop
	cmp	%o3, %o5
.LL157:
	mov	0, %g1
.LL156:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE74:
	.size	LT__uLT_param_u64_i64, .-LT__uLT_param_u64_i64
	.align 4
	.type	LT__LT_param_u64_i64, #function
	.proc	014
LT__LT_param_u64_i64:
.LLFB75:
	save	%sp, -112, %sp
.LLCFI75:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	mov	1, %g1
	ld	[%fp-16], %o4
	ld	[%fp-8], %o5
	cmp	%o4, %o5
	bg	%icc, .LL159
	 nop
	ld	[%fp-16], %o4
	ld	[%fp-8], %o5
	cmp	%o4, %o5
	bne	%icc, .LL160
	 nop
	ld	[%fp-12], %o4
	ld	[%fp-4], %o5
	cmp	%o4, %o5
	bgu	%icc, .LL159
	 nop
	ld	[%fp-12], %o5
	ld	[%fp-4], %g1
	cmp	%o5, %g1
.LL160:
	mov	0, %g1
.LL159:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE75:
	.size	LT__LT_param_u64_i64, .-LT__LT_param_u64_i64
	.align 4
	.type	LT__uLT_var_u64_C, #function
	.proc	014
LT__uLT_var_u64_C:
.LLFB76:
	save	%sp, -96, %sp
.LLCFI76:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL162
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL163
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL162
	 nop
	cmp	%o5, %o3
.LL163:
	mov	0, %g1
.LL162:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE76:
	.size	LT__uLT_var_u64_C, .-LT__uLT_var_u64_C
	.align 4
	.type	LT__LT_var_u64_C, #function
	.proc	014
LT__LT_var_u64_C:
.LLFB77:
	save	%sp, -96, %sp
.LLCFI77:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL165
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL166
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL165
	 nop
	cmp	%o5, %o3
.LL166:
	mov	0, %g1
.LL165:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE77:
	.size	LT__LT_var_u64_C, .-LT__LT_var_u64_C
	.align 4
	.type	LT__uLT_param_u64_C, #function
	.proc	014
LT__uLT_param_u64_C:
.LLFB78:
	save	%sp, -104, %sp
.LLCFI78:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL168
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL169
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL168
	 nop
	cmp	%o5, %o3
.LL169:
	mov	0, %g1
.LL168:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE78:
	.size	LT__uLT_param_u64_C, .-LT__uLT_param_u64_C
	.align 4
	.type	LT__LT_param_u64_C, #function
	.proc	014
LT__LT_param_u64_C:
.LLFB79:
	save	%sp, -104, %sp
.LLCFI79:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bl	%icc, .LL171
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL172
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL171
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL172:
	mov	0, %g1
.LL171:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE79:
	.size	LT__LT_param_u64_C, .-LT__LT_param_u64_C
	.align 4
	.type	LT__uLT_var_u64_u8, #function
	.proc	014
LT__uLT_var_u64_u8:
.LLFB80:
	save	%sp, -96, %sp
.LLCFI80:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL174
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL175
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL174
	 nop
	cmp	%o3, %o5
.LL175:
	mov	0, %g1
.LL174:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE80:
	.size	LT__uLT_var_u64_u8, .-LT__uLT_var_u64_u8
	.align 4
	.type	LT__LT_var_u64_u8, #function
	.proc	014
LT__LT_var_u64_u8:
.LLFB81:
	save	%sp, -96, %sp
.LLCFI81:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL177
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL178
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL177
	 nop
	cmp	%o3, %o5
.LL178:
	mov	0, %g1
.LL177:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE81:
	.size	LT__LT_var_u64_u8, .-LT__LT_var_u64_u8
	.align 4
	.type	LT__uLT_param_u64_u8, #function
	.proc	014
LT__uLT_param_u64_u8:
.LLFB82:
	save	%sp, -104, %sp
.LLCFI82:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL180
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL181
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL180
	 nop
	cmp	%o3, %o5
.LL181:
	mov	0, %g1
.LL180:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE82:
	.size	LT__uLT_param_u64_u8, .-LT__uLT_param_u64_u8
	.align 4
	.type	LT__LT_param_u64_u8, #function
	.proc	014
LT__LT_param_u64_u8:
.LLFB83:
	save	%sp, -104, %sp
.LLCFI83:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bl	%icc, .LL183
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL184
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL183
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL184:
	mov	0, %g1
.LL183:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE83:
	.size	LT__LT_param_u64_u8, .-LT__LT_param_u64_u8
	.align 4
	.type	LT__uLT_var_u64_L, #function
	.proc	014
LT__uLT_var_u64_L:
.LLFB84:
	save	%sp, -96, %sp
.LLCFI84:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL186
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL187
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL186
	 nop
	cmp	%o3, %o5
.LL187:
	mov	0, %g1
.LL186:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE84:
	.size	LT__uLT_var_u64_L, .-LT__uLT_var_u64_L
	.align 4
	.type	LT__LT_var_u64_L, #function
	.proc	014
LT__LT_var_u64_L:
.LLFB85:
	save	%sp, -96, %sp
.LLCFI85:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL189
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL190
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL189
	 nop
	cmp	%o3, %o5
.LL190:
	mov	0, %g1
.LL189:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE85:
	.size	LT__LT_var_u64_L, .-LT__LT_var_u64_L
	.align 4
	.type	LT__uLT_param_u64_L, #function
	.proc	014
LT__uLT_param_u64_L:
.LLFB86:
	save	%sp, -112, %sp
.LLCFI86:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	ldd	[%fp-16], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL192
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL193
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL192
	 nop
	cmp	%o3, %o5
.LL193:
	mov	0, %g1
.LL192:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE86:
	.size	LT__uLT_param_u64_L, .-LT__uLT_param_u64_L
	.align 4
	.type	LT__LT_param_u64_L, #function
	.proc	014
LT__LT_param_u64_L:
.LLFB87:
	save	%sp, -112, %sp
.LLCFI87:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	mov	1, %g1
	ld	[%fp-16], %o4
	ld	[%fp-8], %o5
	cmp	%o4, %o5
	bg	%icc, .LL195
	 nop
	ld	[%fp-16], %o4
	ld	[%fp-8], %o5
	cmp	%o4, %o5
	bne	%icc, .LL196
	 nop
	ld	[%fp-12], %o4
	ld	[%fp-4], %o5
	cmp	%o4, %o5
	bgu	%icc, .LL195
	 nop
	ld	[%fp-12], %o5
	ld	[%fp-4], %g1
	cmp	%o5, %g1
.LL196:
	mov	0, %g1
.LL195:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE87:
	.size	LT__LT_param_u64_L, .-LT__LT_param_u64_L
	.align 4
	.type	LT__uLT_var_u64_i8, #function
	.proc	014
LT__uLT_var_u64_i8:
.LLFB88:
	save	%sp, -96, %sp
.LLCFI88:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL198
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL199
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL198
	 nop
	cmp	%o5, %o3
.LL199:
	mov	0, %g1
.LL198:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE88:
	.size	LT__uLT_var_u64_i8, .-LT__uLT_var_u64_i8
	.align 4
	.type	LT__LT_var_u64_i8, #function
	.proc	014
LT__LT_var_u64_i8:
.LLFB89:
	save	%sp, -96, %sp
.LLCFI89:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL201
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL202
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL201
	 nop
	cmp	%o5, %o3
.LL202:
	mov	0, %g1
.LL201:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE89:
	.size	LT__LT_var_u64_i8, .-LT__LT_var_u64_i8
	.align 4
	.type	LT__uLT_param_u64_i8, #function
	.proc	014
LT__uLT_param_u64_i8:
.LLFB90:
	save	%sp, -104, %sp
.LLCFI90:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL204
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL205
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL204
	 nop
	cmp	%o5, %o3
.LL205:
	mov	0, %g1
.LL204:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE90:
	.size	LT__uLT_param_u64_i8, .-LT__uLT_param_u64_i8
	.align 4
	.type	LT__LT_param_u64_i8, #function
	.proc	014
LT__LT_param_u64_i8:
.LLFB91:
	save	%sp, -104, %sp
.LLCFI91:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bl	%icc, .LL207
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL208
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL207
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL208:
	mov	0, %g1
.LL207:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE91:
	.size	LT__LT_param_u64_i8, .-LT__LT_param_u64_i8
	.align 4
	.type	LT__uLT_var_u64_u32, #function
	.proc	014
LT__uLT_var_u64_u32:
.LLFB92:
	save	%sp, -96, %sp
.LLCFI92:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL210
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL211
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL210
	 nop
	cmp	%o5, %o3
.LL211:
	mov	0, %g1
.LL210:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE92:
	.size	LT__uLT_var_u64_u32, .-LT__uLT_var_u64_u32
	.align 4
	.type	LT__LT_var_u64_u32, #function
	.proc	014
LT__LT_var_u64_u32:
.LLFB93:
	save	%sp, -96, %sp
.LLCFI93:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL213
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL214
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL213
	 nop
	cmp	%o5, %o3
.LL214:
	mov	0, %g1
.LL213:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE93:
	.size	LT__LT_var_u64_u32, .-LT__LT_var_u64_u32
	.align 4
	.type	LT__uLT_param_u64_u32, #function
	.proc	014
LT__uLT_param_u64_u32:
.LLFB94:
	save	%sp, -104, %sp
.LLCFI94:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL216
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL217
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL216
	 nop
	cmp	%o5, %o3
.LL217:
	mov	0, %g1
.LL216:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE94:
	.size	LT__uLT_param_u64_u32, .-LT__uLT_param_u64_u32
	.align 4
	.type	LT__LT_param_u64_u32, #function
	.proc	014
LT__LT_param_u64_u32:
.LLFB95:
	save	%sp, -104, %sp
.LLCFI95:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bl	%icc, .LL219
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL220
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL219
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL220:
	mov	0, %g1
.LL219:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE95:
	.size	LT__LT_param_u64_u32, .-LT__LT_param_u64_u32
	.align 4
	.type	LT__uLT_var_LC_u16, #function
	.proc	014
LT__uLT_var_LC_u16:
.LLFB96:
	save	%sp, -96, %sp
.LLCFI96:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL222
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL223
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL222
	 nop
	cmp	%o3, %o5
.LL223:
	mov	0, %g1
.LL222:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE96:
	.size	LT__uLT_var_LC_u16, .-LT__uLT_var_LC_u16
	.align 4
	.type	LT__LT_var_LC_u16, #function
	.proc	014
LT__LT_var_LC_u16:
.LLFB97:
	save	%sp, -96, %sp
.LLCFI97:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL225
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL226
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL225
	 nop
	cmp	%o3, %o5
.LL226:
	mov	0, %g1
.LL225:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE97:
	.size	LT__LT_var_LC_u16, .-LT__LT_var_LC_u16
	.align 4
	.type	LT__uLT_param_LC_u16, #function
	.proc	014
LT__uLT_param_LC_u16:
.LLFB98:
	save	%sp, -104, %sp
.LLCFI98:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	blu	%icc, .LL228
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL229
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL228
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL229:
	mov	0, %g1
.LL228:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE98:
	.size	LT__uLT_param_LC_u16, .-LT__uLT_param_LC_u16
	.align 4
	.type	LT__LT_param_LC_u16, #function
	.proc	014
LT__LT_param_LC_u16:
.LLFB99:
	save	%sp, -104, %sp
.LLCFI99:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL231
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL232
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL231
	 nop
	cmp	%o3, %o5
.LL232:
	mov	0, %g1
.LL231:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE99:
	.size	LT__LT_param_LC_u16, .-LT__LT_param_LC_u16
	.align 4
	.type	LT__uLT_var_LC_u64, #function
	.proc	014
LT__uLT_var_LC_u64:
.LLFB100:
	save	%sp, -96, %sp
.LLCFI100:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL234
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL235
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL234
	 nop
	cmp	%o3, %o5
.LL235:
	mov	0, %g1
.LL234:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE100:
	.size	LT__uLT_var_LC_u64, .-LT__uLT_var_LC_u64
	.align 4
	.type	LT__LT_var_LC_u64, #function
	.proc	014
LT__LT_var_LC_u64:
.LLFB101:
	save	%sp, -96, %sp
.LLCFI101:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL237
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL238
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL237
	 nop
	cmp	%o3, %o5
.LL238:
	mov	0, %g1
.LL237:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE101:
	.size	LT__LT_var_LC_u64, .-LT__LT_var_LC_u64
	.align 4
	.type	LT__uLT_param_LC_u64, #function
	.proc	014
LT__uLT_param_LC_u64:
.LLFB102:
	save	%sp, -112, %sp
.LLCFI102:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	blu	%icc, .LL240
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL241
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL240
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL241:
	mov	0, %g1
.LL240:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE102:
	.size	LT__uLT_param_LC_u64, .-LT__uLT_param_LC_u64
	.align 4
	.type	LT__LT_param_LC_u64, #function
	.proc	014
LT__LT_param_LC_u64:
.LLFB103:
	save	%sp, -112, %sp
.LLCFI103:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	mov	1, %g1
	ld	[%fp-16], %o3
	cmp	%o3, %o4
	bg	%icc, .LL243
	 nop
	ld	[%fp-16], %o3
	cmp	%o3, %o4
	bne	%icc, .LL244
	 nop
	ld	[%fp-12], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL243
	 nop
	ld	[%fp-12], %g1
	cmp	%g1, %o5
.LL244:
	mov	0, %g1
.LL243:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE103:
	.size	LT__LT_param_LC_u64, .-LT__LT_param_LC_u64
	.align 4
	.type	LT__uLT_var_LC_LC, #function
	.proc	014
LT__uLT_var_LC_LC:
.LLFB104:
	save	%sp, -96, %sp
.LLCFI104:
	mov	0, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE104:
	.size	LT__uLT_var_LC_LC, .-LT__uLT_var_LC_LC
	.align 4
	.type	LT__LT_var_LC_LC, #function
	.proc	014
LT__LT_var_LC_LC:
.LLFB105:
	save	%sp, -96, %sp
.LLCFI105:
	mov	0, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE105:
	.size	LT__LT_var_LC_LC, .-LT__LT_var_LC_LC
	.align 4
	.type	LT__uLT_param_LC_LC, #function
	.proc	014
LT__uLT_param_LC_LC:
.LLFB106:
	save	%sp, -112, %sp
.LLCFI106:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	mov	1, %g1
	ld	[%fp-16], %o4
	ld	[%fp-8], %o5
	cmp	%o4, %o5
	bgu	%icc, .LL248
	 nop
	ld	[%fp-16], %o4
	ld	[%fp-8], %o5
	cmp	%o4, %o5
	bne	%icc, .LL249
	 nop
	ld	[%fp-12], %o4
	ld	[%fp-4], %o5
	cmp	%o4, %o5
	bgu	%icc, .LL248
	 nop
	ld	[%fp-12], %o5
	ld	[%fp-4], %g1
	cmp	%o5, %g1
.LL249:
	mov	0, %g1
.LL248:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE106:
	.size	LT__uLT_param_LC_LC, .-LT__uLT_param_LC_LC
	.align 4
	.type	LT__LT_param_LC_LC, #function
	.proc	014
LT__LT_param_LC_LC:
.LLFB107:
	save	%sp, -112, %sp
.LLCFI107:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	ldd	[%fp-16], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL251
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL252
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL251
	 nop
	cmp	%o3, %o5
.LL252:
	mov	0, %g1
.LL251:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE107:
	.size	LT__LT_param_LC_LC, .-LT__LT_param_LC_LC
	.align 4
	.type	LT__uLT_var_LC_i32, #function
	.proc	014
LT__uLT_var_LC_i32:
.LLFB108:
	save	%sp, -96, %sp
.LLCFI108:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL254
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL255
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL254
	 nop
	cmp	%o5, %o3
.LL255:
	mov	0, %g1
.LL254:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE108:
	.size	LT__uLT_var_LC_i32, .-LT__uLT_var_LC_i32
	.align 4
	.type	LT__LT_var_LC_i32, #function
	.proc	014
LT__LT_var_LC_i32:
.LLFB109:
	save	%sp, -96, %sp
.LLCFI109:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL257
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL258
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL257
	 nop
	cmp	%o5, %o3
.LL258:
	mov	0, %g1
.LL257:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE109:
	.size	LT__LT_var_LC_i32, .-LT__LT_var_LC_i32
	.align 4
	.type	LT__uLT_param_LC_i32, #function
	.proc	014
LT__uLT_param_LC_i32:
.LLFB110:
	save	%sp, -104, %sp
.LLCFI110:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	blu	%icc, .LL260
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL261
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL260
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL261:
	mov	0, %g1
.LL260:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE110:
	.size	LT__uLT_param_LC_i32, .-LT__uLT_param_LC_i32
	.align 4
	.type	LT__LT_param_LC_i32, #function
	.proc	014
LT__LT_param_LC_i32:
.LLFB111:
	save	%sp, -104, %sp
.LLCFI111:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL263
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL264
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL263
	 nop
	cmp	%o5, %o3
.LL264:
	mov	0, %g1
.LL263:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE111:
	.size	LT__LT_param_LC_i32, .-LT__LT_param_LC_i32
	.align 4
	.type	LT__uLT_var_LC_i16, #function
	.proc	014
LT__uLT_var_LC_i16:
.LLFB112:
	save	%sp, -96, %sp
.LLCFI112:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL266
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL267
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL266
	 nop
	cmp	%o5, %o3
.LL267:
	mov	0, %g1
.LL266:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE112:
	.size	LT__uLT_var_LC_i16, .-LT__uLT_var_LC_i16
	.align 4
	.type	LT__LT_var_LC_i16, #function
	.proc	014
LT__LT_var_LC_i16:
.LLFB113:
	save	%sp, -96, %sp
.LLCFI113:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL269
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL270
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL269
	 nop
	cmp	%o5, %o3
.LL270:
	mov	0, %g1
.LL269:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE113:
	.size	LT__LT_var_LC_i16, .-LT__LT_var_LC_i16
	.align 4
	.type	LT__uLT_param_LC_i16, #function
	.proc	014
LT__uLT_param_LC_i16:
.LLFB114:
	save	%sp, -104, %sp
.LLCFI114:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	blu	%icc, .LL272
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL273
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL272
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL273:
	mov	0, %g1
.LL272:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE114:
	.size	LT__uLT_param_LC_i16, .-LT__uLT_param_LC_i16
	.align 4
	.type	LT__LT_param_LC_i16, #function
	.proc	014
LT__LT_param_LC_i16:
.LLFB115:
	save	%sp, -104, %sp
.LLCFI115:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL275
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL276
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL275
	 nop
	cmp	%o5, %o3
.LL276:
	mov	0, %g1
.LL275:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE115:
	.size	LT__LT_param_LC_i16, .-LT__LT_param_LC_i16
	.align 4
	.type	LT__uLT_var_LC_I, #function
	.proc	014
LT__uLT_var_LC_I:
.LLFB116:
	save	%sp, -96, %sp
.LLCFI116:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL278
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL279
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL278
	 nop
	cmp	%o5, %o3
.LL279:
	mov	0, %g1
.LL278:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE116:
	.size	LT__uLT_var_LC_I, .-LT__uLT_var_LC_I
	.align 4
	.type	LT__LT_var_LC_I, #function
	.proc	014
LT__LT_var_LC_I:
.LLFB117:
	save	%sp, -96, %sp
.LLCFI117:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL281
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL282
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL281
	 nop
	cmp	%o5, %o3
.LL282:
	mov	0, %g1
.LL281:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE117:
	.size	LT__LT_var_LC_I, .-LT__LT_var_LC_I
	.align 4
	.type	LT__uLT_param_LC_I, #function
	.proc	014
LT__uLT_param_LC_I:
.LLFB118:
	save	%sp, -104, %sp
.LLCFI118:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	blu	%icc, .LL284
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL285
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL284
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL285:
	mov	0, %g1
.LL284:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE118:
	.size	LT__uLT_param_LC_I, .-LT__uLT_param_LC_I
	.align 4
	.type	LT__LT_param_LC_I, #function
	.proc	014
LT__LT_param_LC_I:
.LLFB119:
	save	%sp, -104, %sp
.LLCFI119:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL287
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL288
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL287
	 nop
	cmp	%o5, %o3
.LL288:
	mov	0, %g1
.LL287:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE119:
	.size	LT__LT_param_LC_I, .-LT__LT_param_LC_I
	.align 4
	.type	LT__uLT_var_LC_i64, #function
	.proc	014
LT__uLT_var_LC_i64:
.LLFB120:
	save	%sp, -96, %sp
.LLCFI120:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL290
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL291
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL290
	 nop
	cmp	%o3, %o5
.LL291:
	mov	0, %g1
.LL290:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE120:
	.size	LT__uLT_var_LC_i64, .-LT__uLT_var_LC_i64
	.align 4
	.type	LT__LT_var_LC_i64, #function
	.proc	014
LT__LT_var_LC_i64:
.LLFB121:
	save	%sp, -96, %sp
.LLCFI121:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL293
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL294
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL293
	 nop
	cmp	%o3, %o5
.LL294:
	mov	0, %g1
.LL293:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE121:
	.size	LT__LT_var_LC_i64, .-LT__LT_var_LC_i64
	.align 4
	.type	LT__uLT_param_LC_i64, #function
	.proc	014
LT__uLT_param_LC_i64:
.LLFB122:
	save	%sp, -112, %sp
.LLCFI122:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	blu	%icc, .LL296
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL297
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL296
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL297:
	mov	0, %g1
.LL296:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE122:
	.size	LT__uLT_param_LC_i64, .-LT__uLT_param_LC_i64
	.align 4
	.type	LT__LT_param_LC_i64, #function
	.proc	014
LT__LT_param_LC_i64:
.LLFB123:
	save	%sp, -112, %sp
.LLCFI123:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	mov	1, %g1
	ld	[%fp-16], %o3
	cmp	%o3, %o4
	bg	%icc, .LL299
	 nop
	ld	[%fp-16], %o3
	cmp	%o3, %o4
	bne	%icc, .LL300
	 nop
	ld	[%fp-12], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL299
	 nop
	ld	[%fp-12], %g1
	cmp	%g1, %o5
.LL300:
	mov	0, %g1
.LL299:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE123:
	.size	LT__LT_param_LC_i64, .-LT__LT_param_LC_i64
	.align 4
	.type	LT__uLT_var_LC_C, #function
	.proc	014
LT__uLT_var_LC_C:
.LLFB124:
	save	%sp, -96, %sp
.LLCFI124:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL302
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL303
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL302
	 nop
	cmp	%o5, %o3
.LL303:
	mov	0, %g1
.LL302:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE124:
	.size	LT__uLT_var_LC_C, .-LT__uLT_var_LC_C
	.align 4
	.type	LT__LT_var_LC_C, #function
	.proc	014
LT__LT_var_LC_C:
.LLFB125:
	save	%sp, -96, %sp
.LLCFI125:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL305
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL306
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL305
	 nop
	cmp	%o5, %o3
.LL306:
	mov	0, %g1
.LL305:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE125:
	.size	LT__LT_var_LC_C, .-LT__LT_var_LC_C
	.align 4
	.type	LT__uLT_param_LC_C, #function
	.proc	014
LT__uLT_param_LC_C:
.LLFB126:
	save	%sp, -104, %sp
.LLCFI126:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	blu	%icc, .LL308
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL309
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL308
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL309:
	mov	0, %g1
.LL308:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE126:
	.size	LT__uLT_param_LC_C, .-LT__uLT_param_LC_C
	.align 4
	.type	LT__LT_param_LC_C, #function
	.proc	014
LT__LT_param_LC_C:
.LLFB127:
	save	%sp, -104, %sp
.LLCFI127:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL311
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL312
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL311
	 nop
	cmp	%o5, %o3
.LL312:
	mov	0, %g1
.LL311:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE127:
	.size	LT__LT_param_LC_C, .-LT__LT_param_LC_C
	.align 4
	.type	LT__uLT_var_LC_u8, #function
	.proc	014
LT__uLT_var_LC_u8:
.LLFB128:
	save	%sp, -96, %sp
.LLCFI128:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL314
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL315
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL314
	 nop
	cmp	%o3, %o5
.LL315:
	mov	0, %g1
.LL314:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE128:
	.size	LT__uLT_var_LC_u8, .-LT__uLT_var_LC_u8
	.align 4
	.type	LT__LT_var_LC_u8, #function
	.proc	014
LT__LT_var_LC_u8:
.LLFB129:
	save	%sp, -96, %sp
.LLCFI129:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL317
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL318
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL317
	 nop
	cmp	%o3, %o5
.LL318:
	mov	0, %g1
.LL317:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE129:
	.size	LT__LT_var_LC_u8, .-LT__LT_var_LC_u8
	.align 4
	.type	LT__uLT_param_LC_u8, #function
	.proc	014
LT__uLT_param_LC_u8:
.LLFB130:
	save	%sp, -104, %sp
.LLCFI130:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	blu	%icc, .LL320
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL321
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL320
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL321:
	mov	0, %g1
.LL320:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE130:
	.size	LT__uLT_param_LC_u8, .-LT__uLT_param_LC_u8
	.align 4
	.type	LT__LT_param_LC_u8, #function
	.proc	014
LT__LT_param_LC_u8:
.LLFB131:
	save	%sp, -104, %sp
.LLCFI131:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL323
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL324
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL323
	 nop
	cmp	%o3, %o5
.LL324:
	mov	0, %g1
.LL323:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE131:
	.size	LT__LT_param_LC_u8, .-LT__LT_param_LC_u8
	.align 4
	.type	LT__uLT_var_LC_L, #function
	.proc	014
LT__uLT_var_LC_L:
.LLFB132:
	save	%sp, -96, %sp
.LLCFI132:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL326
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL327
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL326
	 nop
	cmp	%o3, %o5
.LL327:
	mov	0, %g1
.LL326:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE132:
	.size	LT__uLT_var_LC_L, .-LT__uLT_var_LC_L
	.align 4
	.type	LT__LT_var_LC_L, #function
	.proc	014
LT__LT_var_LC_L:
.LLFB133:
	save	%sp, -96, %sp
.LLCFI133:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL329
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL330
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL329
	 nop
	cmp	%o3, %o5
.LL330:
	mov	0, %g1
.LL329:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE133:
	.size	LT__LT_var_LC_L, .-LT__LT_var_LC_L
	.align 4
	.type	LT__uLT_param_LC_L, #function
	.proc	014
LT__uLT_param_LC_L:
.LLFB134:
	save	%sp, -112, %sp
.LLCFI134:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	blu	%icc, .LL332
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL333
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL332
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL333:
	mov	0, %g1
.LL332:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE134:
	.size	LT__uLT_param_LC_L, .-LT__uLT_param_LC_L
	.align 4
	.type	LT__LT_param_LC_L, #function
	.proc	014
LT__LT_param_LC_L:
.LLFB135:
	save	%sp, -112, %sp
.LLCFI135:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	mov	1, %g1
	ld	[%fp-16], %o3
	cmp	%o3, %o4
	bg	%icc, .LL335
	 nop
	ld	[%fp-16], %o3
	cmp	%o3, %o4
	bne	%icc, .LL336
	 nop
	ld	[%fp-12], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL335
	 nop
	ld	[%fp-12], %g1
	cmp	%g1, %o5
.LL336:
	mov	0, %g1
.LL335:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE135:
	.size	LT__LT_param_LC_L, .-LT__LT_param_LC_L
	.align 4
	.type	LT__uLT_var_LC_i8, #function
	.proc	014
LT__uLT_var_LC_i8:
.LLFB136:
	save	%sp, -96, %sp
.LLCFI136:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL338
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL339
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL338
	 nop
	cmp	%o5, %o3
.LL339:
	mov	0, %g1
.LL338:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE136:
	.size	LT__uLT_var_LC_i8, .-LT__uLT_var_LC_i8
	.align 4
	.type	LT__LT_var_LC_i8, #function
	.proc	014
LT__LT_var_LC_i8:
.LLFB137:
	save	%sp, -96, %sp
.LLCFI137:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL341
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL342
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL341
	 nop
	cmp	%o5, %o3
.LL342:
	mov	0, %g1
.LL341:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE137:
	.size	LT__LT_var_LC_i8, .-LT__LT_var_LC_i8
	.align 4
	.type	LT__uLT_param_LC_i8, #function
	.proc	014
LT__uLT_param_LC_i8:
.LLFB138:
	save	%sp, -104, %sp
.LLCFI138:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	blu	%icc, .LL344
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL345
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL344
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL345:
	mov	0, %g1
.LL344:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE138:
	.size	LT__uLT_param_LC_i8, .-LT__uLT_param_LC_i8
	.align 4
	.type	LT__LT_param_LC_i8, #function
	.proc	014
LT__LT_param_LC_i8:
.LLFB139:
	save	%sp, -104, %sp
.LLCFI139:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL347
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL348
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL347
	 nop
	cmp	%o5, %o3
.LL348:
	mov	0, %g1
.LL347:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE139:
	.size	LT__LT_param_LC_i8, .-LT__LT_param_LC_i8
	.align 4
	.type	LT__uLT_var_LC_u32, #function
	.proc	014
LT__uLT_var_LC_u32:
.LLFB140:
	save	%sp, -96, %sp
.LLCFI140:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL350
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL351
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL350
	 nop
	cmp	%o5, %o3
.LL351:
	mov	0, %g1
.LL350:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE140:
	.size	LT__uLT_var_LC_u32, .-LT__uLT_var_LC_u32
	.align 4
	.type	LT__LT_var_LC_u32, #function
	.proc	014
LT__LT_var_LC_u32:
.LLFB141:
	save	%sp, -96, %sp
.LLCFI141:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL353
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL354
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL353
	 nop
	cmp	%o5, %o3
.LL354:
	mov	0, %g1
.LL353:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE141:
	.size	LT__LT_var_LC_u32, .-LT__LT_var_LC_u32
	.align 4
	.type	LT__uLT_param_LC_u32, #function
	.proc	014
LT__uLT_param_LC_u32:
.LLFB142:
	save	%sp, -104, %sp
.LLCFI142:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	blu	%icc, .LL356
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL357
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL356
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL357:
	mov	0, %g1
.LL356:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE142:
	.size	LT__uLT_param_LC_u32, .-LT__uLT_param_LC_u32
	.align 4
	.type	LT__LT_param_LC_u32, #function
	.proc	014
LT__LT_param_LC_u32:
.LLFB143:
	save	%sp, -104, %sp
.LLCFI143:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL359
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL360
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL359
	 nop
	cmp	%o5, %o3
.LL360:
	mov	0, %g1
.LL359:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE143:
	.size	LT__LT_param_LC_u32, .-LT__LT_param_LC_u32
	.align 4
	.type	LT__uLT_var_i32_u16, #function
	.proc	014
LT__uLT_var_i32_u16:
.LLFB144:
	save	%sp, -96, %sp
.LLCFI144:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE144:
	.size	LT__uLT_var_i32_u16, .-LT__uLT_var_i32_u16
	.align 4
	.type	LT__LT_var_i32_u16, #function
	.proc	014
LT__LT_var_i32_u16:
.LLFB145:
	save	%sp, -96, %sp
.LLCFI145:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE145:
	.size	LT__LT_var_i32_u16, .-LT__LT_var_i32_u16
	.align 4
	.type	LT__uLT_param_i32_u16, #function
	.proc	014
LT__uLT_param_i32_u16:
.LLFB146:
	save	%sp, -96, %sp
.LLCFI146:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ld	[%fp+68], %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE146:
	.size	LT__uLT_param_i32_u16, .-LT__uLT_param_i32_u16
	.align 4
	.type	LT__LT_param_i32_u16, #function
	.proc	014
LT__LT_param_i32_u16:
.LLFB147:
	save	%sp, -96, %sp
.LLCFI147:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+68], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movg	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE147:
	.size	LT__LT_param_i32_u16, .-LT__LT_param_i32_u16
	.align 4
	.type	LT__uLT_var_i32_u64, #function
	.proc	014
LT__uLT_var_i32_u64:
.LLFB148:
	save	%sp, -96, %sp
.LLCFI148:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL366
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL367
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL366
	 nop
	cmp	%o3, %o5
.LL367:
	mov	0, %g1
.LL366:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE148:
	.size	LT__uLT_var_i32_u64, .-LT__uLT_var_i32_u64
	.align 4
	.type	LT__LT_var_i32_u64, #function
	.proc	014
LT__LT_var_i32_u64:
.LLFB149:
	save	%sp, -96, %sp
.LLCFI149:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL369
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL370
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL369
	 nop
	cmp	%o3, %o5
.LL370:
	mov	0, %g1
.LL369:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE149:
	.size	LT__LT_var_i32_u64, .-LT__LT_var_i32_u64
	.align 4
	.type	LT__uLT_param_i32_u64, #function
	.proc	014
LT__uLT_param_i32_u64:
.LLFB150:
	save	%sp, -104, %sp
.LLCFI150:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL372
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL373
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL372
	 nop
	cmp	%o3, %o5
.LL373:
	mov	0, %g1
.LL372:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE150:
	.size	LT__uLT_param_i32_u64, .-LT__uLT_param_i32_u64
	.align 4
	.type	LT__LT_param_i32_u64, #function
	.proc	014
LT__LT_param_i32_u64:
.LLFB151:
	save	%sp, -104, %sp
.LLCFI151:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bg	%icc, .LL375
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL376
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL375
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL376:
	mov	0, %g1
.LL375:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE151:
	.size	LT__LT_param_i32_u64, .-LT__LT_param_i32_u64
	.align 4
	.type	LT__uLT_var_i32_LC, #function
	.proc	014
LT__uLT_var_i32_LC:
.LLFB152:
	save	%sp, -96, %sp
.LLCFI152:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL378
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL379
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL378
	 nop
	cmp	%o3, %o5
.LL379:
	mov	0, %g1
.LL378:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE152:
	.size	LT__uLT_var_i32_LC, .-LT__uLT_var_i32_LC
	.align 4
	.type	LT__LT_var_i32_LC, #function
	.proc	014
LT__LT_var_i32_LC:
.LLFB153:
	save	%sp, -96, %sp
.LLCFI153:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL381
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL382
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL381
	 nop
	cmp	%o3, %o5
.LL382:
	mov	0, %g1
.LL381:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE153:
	.size	LT__LT_var_i32_LC, .-LT__LT_var_i32_LC
	.align 4
	.type	LT__uLT_param_i32_LC, #function
	.proc	014
LT__uLT_param_i32_LC:
.LLFB154:
	save	%sp, -104, %sp
.LLCFI154:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bgu	%icc, .LL384
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL385
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL384
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL385:
	mov	0, %g1
.LL384:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE154:
	.size	LT__uLT_param_i32_LC, .-LT__uLT_param_i32_LC
	.align 4
	.type	LT__LT_param_i32_LC, #function
	.proc	014
LT__LT_param_i32_LC:
.LLFB155:
	save	%sp, -104, %sp
.LLCFI155:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL387
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL388
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL387
	 nop
	cmp	%o3, %o5
.LL388:
	mov	0, %g1
.LL387:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE155:
	.size	LT__LT_param_i32_LC, .-LT__LT_param_i32_LC
	.align 4
	.type	LT__uLT_var_i32_i32, #function
	.proc	014
LT__uLT_var_i32_i32:
.LLFB156:
	save	%sp, -96, %sp
.LLCFI156:
	mov	0, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE156:
	.size	LT__uLT_var_i32_i32, .-LT__uLT_var_i32_i32
	.align 4
	.type	LT__LT_var_i32_i32, #function
	.proc	014
LT__LT_var_i32_i32:
.LLFB157:
	save	%sp, -96, %sp
.LLCFI157:
	mov	0, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE157:
	.size	LT__LT_var_i32_i32, .-LT__LT_var_i32_i32
	.align 4
	.type	LT__uLT_param_i32_i32, #function
	.proc	014
LT__uLT_param_i32_i32:
.LLFB158:
	save	%sp, -96, %sp
.LLCFI158:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE158:
	.size	LT__uLT_param_i32_i32, .-LT__uLT_param_i32_i32
	.align 4
	.type	LT__LT_param_i32_i32, #function
	.proc	014
LT__LT_param_i32_i32:
.LLFB159:
	save	%sp, -96, %sp
.LLCFI159:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE159:
	.size	LT__LT_param_i32_i32, .-LT__LT_param_i32_i32
	.align 4
	.type	LT__uLT_var_i32_i16, #function
	.proc	014
LT__uLT_var_i32_i16:
.LLFB160:
	save	%sp, -96, %sp
.LLCFI160:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE160:
	.size	LT__uLT_var_i32_i16, .-LT__uLT_var_i32_i16
	.align 4
	.type	LT__LT_var_i32_i16, #function
	.proc	014
LT__LT_var_i32_i16:
.LLFB161:
	save	%sp, -96, %sp
.LLCFI161:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE161:
	.size	LT__LT_var_i32_i16, .-LT__LT_var_i32_i16
	.align 4
	.type	LT__uLT_param_i32_i16, #function
	.proc	014
LT__uLT_param_i32_i16:
.LLFB162:
	save	%sp, -96, %sp
.LLCFI162:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ld	[%fp+68], %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE162:
	.size	LT__uLT_param_i32_i16, .-LT__uLT_param_i32_i16
	.align 4
	.type	LT__LT_param_i32_i16, #function
	.proc	014
LT__LT_param_i32_i16:
.LLFB163:
	save	%sp, -96, %sp
.LLCFI163:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+68], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movg	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE163:
	.size	LT__LT_param_i32_i16, .-LT__LT_param_i32_i16
	.align 4
	.type	LT__uLT_var_i32_I, #function
	.proc	014
LT__uLT_var_i32_I:
.LLFB164:
	save	%sp, -96, %sp
.LLCFI164:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE164:
	.size	LT__uLT_var_i32_I, .-LT__uLT_var_i32_I
	.align 4
	.type	LT__LT_var_i32_I, #function
	.proc	014
LT__LT_var_i32_I:
.LLFB165:
	save	%sp, -96, %sp
.LLCFI165:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE165:
	.size	LT__LT_var_i32_I, .-LT__LT_var_i32_I
	.align 4
	.type	LT__uLT_param_i32_I, #function
	.proc	014
LT__uLT_param_i32_I:
.LLFB166:
	save	%sp, -96, %sp
.LLCFI166:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE166:
	.size	LT__uLT_param_i32_I, .-LT__uLT_param_i32_I
	.align 4
	.type	LT__LT_param_i32_I, #function
	.proc	014
LT__LT_param_i32_I:
.LLFB167:
	save	%sp, -96, %sp
.LLCFI167:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE167:
	.size	LT__LT_param_i32_I, .-LT__LT_param_i32_I
	.align 4
	.type	LT__uLT_var_i32_i64, #function
	.proc	014
LT__uLT_var_i32_i64:
.LLFB168:
	save	%sp, -96, %sp
.LLCFI168:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL402
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL403
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL402
	 nop
	cmp	%o3, %o5
.LL403:
	mov	0, %g1
.LL402:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE168:
	.size	LT__uLT_var_i32_i64, .-LT__uLT_var_i32_i64
	.align 4
	.type	LT__LT_var_i32_i64, #function
	.proc	014
LT__LT_var_i32_i64:
.LLFB169:
	save	%sp, -96, %sp
.LLCFI169:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL405
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL406
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL405
	 nop
	cmp	%o3, %o5
.LL406:
	mov	0, %g1
.LL405:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE169:
	.size	LT__LT_var_i32_i64, .-LT__LT_var_i32_i64
	.align 4
	.type	LT__uLT_param_i32_i64, #function
	.proc	014
LT__uLT_param_i32_i64:
.LLFB170:
	save	%sp, -104, %sp
.LLCFI170:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL408
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL409
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL408
	 nop
	cmp	%o3, %o5
.LL409:
	mov	0, %g1
.LL408:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE170:
	.size	LT__uLT_param_i32_i64, .-LT__uLT_param_i32_i64
	.align 4
	.type	LT__LT_param_i32_i64, #function
	.proc	014
LT__LT_param_i32_i64:
.LLFB171:
	save	%sp, -104, %sp
.LLCFI171:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bg	%icc, .LL411
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL412
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL411
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL412:
	mov	0, %g1
.LL411:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE171:
	.size	LT__LT_param_i32_i64, .-LT__LT_param_i32_i64
	.align 4
	.type	LT__uLT_var_i32_C, #function
	.proc	014
LT__uLT_var_i32_C:
.LLFB172:
	save	%sp, -96, %sp
.LLCFI172:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE172:
	.size	LT__uLT_var_i32_C, .-LT__uLT_var_i32_C
	.align 4
	.type	LT__LT_var_i32_C, #function
	.proc	014
LT__LT_var_i32_C:
.LLFB173:
	save	%sp, -96, %sp
.LLCFI173:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE173:
	.size	LT__LT_var_i32_C, .-LT__LT_var_i32_C
	.align 4
	.type	LT__uLT_param_i32_C, #function
	.proc	014
LT__uLT_param_i32_C:
.LLFB174:
	save	%sp, -96, %sp
.LLCFI174:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE174:
	.size	LT__uLT_param_i32_C, .-LT__uLT_param_i32_C
	.align 4
	.type	LT__LT_param_i32_C, #function
	.proc	014
LT__LT_param_i32_C:
.LLFB175:
	save	%sp, -96, %sp
.LLCFI175:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movg	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE175:
	.size	LT__LT_param_i32_C, .-LT__LT_param_i32_C
	.align 4
	.type	LT__uLT_var_i32_u8, #function
	.proc	014
LT__uLT_var_i32_u8:
.LLFB176:
	save	%sp, -96, %sp
.LLCFI176:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE176:
	.size	LT__uLT_var_i32_u8, .-LT__uLT_var_i32_u8
	.align 4
	.type	LT__LT_var_i32_u8, #function
	.proc	014
LT__LT_var_i32_u8:
.LLFB177:
	save	%sp, -96, %sp
.LLCFI177:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE177:
	.size	LT__LT_var_i32_u8, .-LT__LT_var_i32_u8
	.align 4
	.type	LT__uLT_param_i32_u8, #function
	.proc	014
LT__uLT_param_i32_u8:
.LLFB178:
	save	%sp, -96, %sp
.LLCFI178:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE178:
	.size	LT__uLT_param_i32_u8, .-LT__uLT_param_i32_u8
	.align 4
	.type	LT__LT_param_i32_u8, #function
	.proc	014
LT__LT_param_i32_u8:
.LLFB179:
	save	%sp, -96, %sp
.LLCFI179:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+68], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movg	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE179:
	.size	LT__LT_param_i32_u8, .-LT__LT_param_i32_u8
	.align 4
	.type	LT__uLT_var_i32_L, #function
	.proc	014
LT__uLT_var_i32_L:
.LLFB180:
	save	%sp, -96, %sp
.LLCFI180:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL422
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL423
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL422
	 nop
	cmp	%o3, %o5
.LL423:
	mov	0, %g1
.LL422:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE180:
	.size	LT__uLT_var_i32_L, .-LT__uLT_var_i32_L
	.align 4
	.type	LT__LT_var_i32_L, #function
	.proc	014
LT__LT_var_i32_L:
.LLFB181:
	save	%sp, -96, %sp
.LLCFI181:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL425
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL426
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL425
	 nop
	cmp	%o3, %o5
.LL426:
	mov	0, %g1
.LL425:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE181:
	.size	LT__LT_var_i32_L, .-LT__LT_var_i32_L
	.align 4
	.type	LT__uLT_param_i32_L, #function
	.proc	014
LT__uLT_param_i32_L:
.LLFB182:
	save	%sp, -104, %sp
.LLCFI182:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL428
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL429
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL428
	 nop
	cmp	%o3, %o5
.LL429:
	mov	0, %g1
.LL428:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE182:
	.size	LT__uLT_param_i32_L, .-LT__uLT_param_i32_L
	.align 4
	.type	LT__LT_param_i32_L, #function
	.proc	014
LT__LT_param_i32_L:
.LLFB183:
	save	%sp, -104, %sp
.LLCFI183:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bg	%icc, .LL431
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL432
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL431
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL432:
	mov	0, %g1
.LL431:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE183:
	.size	LT__LT_param_i32_L, .-LT__LT_param_i32_L
	.align 4
	.type	LT__uLT_var_i32_i8, #function
	.proc	014
LT__uLT_var_i32_i8:
.LLFB184:
	save	%sp, -96, %sp
.LLCFI184:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE184:
	.size	LT__uLT_var_i32_i8, .-LT__uLT_var_i32_i8
	.align 4
	.type	LT__LT_var_i32_i8, #function
	.proc	014
LT__LT_var_i32_i8:
.LLFB185:
	save	%sp, -96, %sp
.LLCFI185:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE185:
	.size	LT__LT_var_i32_i8, .-LT__LT_var_i32_i8
	.align 4
	.type	LT__uLT_param_i32_i8, #function
	.proc	014
LT__uLT_param_i32_i8:
.LLFB186:
	save	%sp, -96, %sp
.LLCFI186:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE186:
	.size	LT__uLT_param_i32_i8, .-LT__uLT_param_i32_i8
	.align 4
	.type	LT__LT_param_i32_i8, #function
	.proc	014
LT__LT_param_i32_i8:
.LLFB187:
	save	%sp, -96, %sp
.LLCFI187:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+68], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movg	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE187:
	.size	LT__LT_param_i32_i8, .-LT__LT_param_i32_i8
	.align 4
	.type	LT__uLT_var_i32_u32, #function
	.proc	014
LT__uLT_var_i32_u32:
.LLFB188:
	save	%sp, -96, %sp
.LLCFI188:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE188:
	.size	LT__uLT_var_i32_u32, .-LT__uLT_var_i32_u32
	.align 4
	.type	LT__LT_var_i32_u32, #function
	.proc	014
LT__LT_var_i32_u32:
.LLFB189:
	save	%sp, -96, %sp
.LLCFI189:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE189:
	.size	LT__LT_var_i32_u32, .-LT__LT_var_i32_u32
	.align 4
	.type	LT__uLT_param_i32_u32, #function
	.proc	014
LT__uLT_param_i32_u32:
.LLFB190:
	save	%sp, -96, %sp
.LLCFI190:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE190:
	.size	LT__uLT_param_i32_u32, .-LT__uLT_param_i32_u32
	.align 4
	.type	LT__LT_param_i32_u32, #function
	.proc	014
LT__LT_param_i32_u32:
.LLFB191:
	save	%sp, -96, %sp
.LLCFI191:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE191:
	.size	LT__LT_param_i32_u32, .-LT__LT_param_i32_u32
	.align 4
	.type	LT__uLT_var_i16_u16, #function
	.proc	014
LT__uLT_var_i16_u16:
.LLFB192:
	save	%sp, -96, %sp
.LLCFI192:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE192:
	.size	LT__uLT_var_i16_u16, .-LT__uLT_var_i16_u16
	.align 4
	.type	LT__LT_var_i16_u16, #function
	.proc	014
LT__LT_var_i16_u16:
.LLFB193:
	save	%sp, -96, %sp
.LLCFI193:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE193:
	.size	LT__LT_var_i16_u16, .-LT__LT_var_i16_u16
	.align 4
	.type	LT__uLT_param_i16_u16, #function
	.proc	014
LT__uLT_param_i16_u16:
.LLFB194:
	save	%sp, -96, %sp
.LLCFI194:
	sth	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE194:
	.size	LT__uLT_param_i16_u16, .-LT__uLT_param_i16_u16
	.align 4
	.type	LT__LT_param_i16_u16, #function
	.proc	014
LT__LT_param_i16_u16:
.LLFB195:
	save	%sp, -96, %sp
.LLCFI195:
	sth	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE195:
	.size	LT__LT_param_i16_u16, .-LT__LT_param_i16_u16
	.align 4
	.type	LT__uLT_var_i16_u64, #function
	.proc	014
LT__uLT_var_i16_u64:
.LLFB196:
	save	%sp, -96, %sp
.LLCFI196:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL446
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL447
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL446
	 nop
	cmp	%o3, %o5
.LL447:
	mov	0, %g1
.LL446:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE196:
	.size	LT__uLT_var_i16_u64, .-LT__uLT_var_i16_u64
	.align 4
	.type	LT__LT_var_i16_u64, #function
	.proc	014
LT__LT_var_i16_u64:
.LLFB197:
	save	%sp, -96, %sp
.LLCFI197:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL449
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL450
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL449
	 nop
	cmp	%o3, %o5
.LL450:
	mov	0, %g1
.LL449:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE197:
	.size	LT__LT_var_i16_u64, .-LT__LT_var_i16_u64
	.align 4
	.type	LT__uLT_param_i16_u64, #function
	.proc	014
LT__uLT_param_i16_u64:
.LLFB198:
	save	%sp, -104, %sp
.LLCFI198:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL452
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL453
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL452
	 nop
	cmp	%o3, %o5
.LL453:
	mov	0, %g1
.LL452:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE198:
	.size	LT__uLT_param_i16_u64, .-LT__uLT_param_i16_u64
	.align 4
	.type	LT__LT_param_i16_u64, #function
	.proc	014
LT__LT_param_i16_u64:
.LLFB199:
	save	%sp, -104, %sp
.LLCFI199:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bg	%icc, .LL455
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL456
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL455
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL456:
	mov	0, %g1
.LL455:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE199:
	.size	LT__LT_param_i16_u64, .-LT__LT_param_i16_u64
	.align 4
	.type	LT__uLT_var_i16_LC, #function
	.proc	014
LT__uLT_var_i16_LC:
.LLFB200:
	save	%sp, -96, %sp
.LLCFI200:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL458
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL459
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL458
	 nop
	cmp	%o3, %o5
.LL459:
	mov	0, %g1
.LL458:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE200:
	.size	LT__uLT_var_i16_LC, .-LT__uLT_var_i16_LC
	.align 4
	.type	LT__LT_var_i16_LC, #function
	.proc	014
LT__LT_var_i16_LC:
.LLFB201:
	save	%sp, -96, %sp
.LLCFI201:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL461
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL462
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL461
	 nop
	cmp	%o3, %o5
.LL462:
	mov	0, %g1
.LL461:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE201:
	.size	LT__LT_var_i16_LC, .-LT__LT_var_i16_LC
	.align 4
	.type	LT__uLT_param_i16_LC, #function
	.proc	014
LT__uLT_param_i16_LC:
.LLFB202:
	save	%sp, -104, %sp
.LLCFI202:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bgu	%icc, .LL464
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL465
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL464
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL465:
	mov	0, %g1
.LL464:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE202:
	.size	LT__uLT_param_i16_LC, .-LT__uLT_param_i16_LC
	.align 4
	.type	LT__LT_param_i16_LC, #function
	.proc	014
LT__LT_param_i16_LC:
.LLFB203:
	save	%sp, -104, %sp
.LLCFI203:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL467
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL468
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL467
	 nop
	cmp	%o3, %o5
.LL468:
	mov	0, %g1
.LL467:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE203:
	.size	LT__LT_param_i16_LC, .-LT__LT_param_i16_LC
	.align 4
	.type	LT__uLT_var_i16_i32, #function
	.proc	014
LT__uLT_var_i16_i32:
.LLFB204:
	save	%sp, -96, %sp
.LLCFI204:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE204:
	.size	LT__uLT_var_i16_i32, .-LT__uLT_var_i16_i32
	.align 4
	.type	LT__LT_var_i16_i32, #function
	.proc	014
LT__LT_var_i16_i32:
.LLFB205:
	save	%sp, -96, %sp
.LLCFI205:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE205:
	.size	LT__LT_var_i16_i32, .-LT__LT_var_i16_i32
	.align 4
	.type	LT__uLT_param_i16_i32, #function
	.proc	014
LT__uLT_param_i16_i32:
.LLFB206:
	save	%sp, -96, %sp
.LLCFI206:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE206:
	.size	LT__uLT_param_i16_i32, .-LT__uLT_param_i16_i32
	.align 4
	.type	LT__LT_param_i16_i32, #function
	.proc	014
LT__LT_param_i16_i32:
.LLFB207:
	save	%sp, -96, %sp
.LLCFI207:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE207:
	.size	LT__LT_param_i16_i32, .-LT__LT_param_i16_i32
	.align 4
	.type	LT__uLT_var_i16_i16, #function
	.proc	014
LT__uLT_var_i16_i16:
.LLFB208:
	save	%sp, -96, %sp
.LLCFI208:
	mov	0, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE208:
	.size	LT__uLT_var_i16_i16, .-LT__uLT_var_i16_i16
	.align 4
	.type	LT__LT_var_i16_i16, #function
	.proc	014
LT__LT_var_i16_i16:
.LLFB209:
	save	%sp, -96, %sp
.LLCFI209:
	mov	0, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE209:
	.size	LT__LT_var_i16_i16, .-LT__LT_var_i16_i16
	.align 4
	.type	LT__uLT_param_i16_i16, #function
	.proc	014
LT__uLT_param_i16_i16:
.LLFB210:
	save	%sp, -96, %sp
.LLCFI210:
	sth	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE210:
	.size	LT__uLT_param_i16_i16, .-LT__uLT_param_i16_i16
	.align 4
	.type	LT__LT_param_i16_i16, #function
	.proc	014
LT__LT_param_i16_i16:
.LLFB211:
	save	%sp, -96, %sp
.LLCFI211:
	sth	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE211:
	.size	LT__LT_param_i16_i16, .-LT__LT_param_i16_i16
	.align 4
	.type	LT__uLT_var_i16_I, #function
	.proc	014
LT__uLT_var_i16_I:
.LLFB212:
	save	%sp, -96, %sp
.LLCFI212:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE212:
	.size	LT__uLT_var_i16_I, .-LT__uLT_var_i16_I
	.align 4
	.type	LT__LT_var_i16_I, #function
	.proc	014
LT__LT_var_i16_I:
.LLFB213:
	save	%sp, -96, %sp
.LLCFI213:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE213:
	.size	LT__LT_var_i16_I, .-LT__LT_var_i16_I
	.align 4
	.type	LT__uLT_param_i16_I, #function
	.proc	014
LT__uLT_param_i16_I:
.LLFB214:
	save	%sp, -96, %sp
.LLCFI214:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE214:
	.size	LT__uLT_param_i16_I, .-LT__uLT_param_i16_I
	.align 4
	.type	LT__LT_param_i16_I, #function
	.proc	014
LT__LT_param_i16_I:
.LLFB215:
	save	%sp, -96, %sp
.LLCFI215:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE215:
	.size	LT__LT_param_i16_I, .-LT__LT_param_i16_I
	.align 4
	.type	LT__uLT_var_i16_i64, #function
	.proc	014
LT__uLT_var_i16_i64:
.LLFB216:
	save	%sp, -96, %sp
.LLCFI216:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL482
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL483
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL482
	 nop
	cmp	%o3, %o5
.LL483:
	mov	0, %g1
.LL482:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE216:
	.size	LT__uLT_var_i16_i64, .-LT__uLT_var_i16_i64
	.align 4
	.type	LT__LT_var_i16_i64, #function
	.proc	014
LT__LT_var_i16_i64:
.LLFB217:
	save	%sp, -96, %sp
.LLCFI217:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL485
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL486
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL485
	 nop
	cmp	%o3, %o5
.LL486:
	mov	0, %g1
.LL485:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE217:
	.size	LT__LT_var_i16_i64, .-LT__LT_var_i16_i64
	.align 4
	.type	LT__uLT_param_i16_i64, #function
	.proc	014
LT__uLT_param_i16_i64:
.LLFB218:
	save	%sp, -104, %sp
.LLCFI218:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL488
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL489
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL488
	 nop
	cmp	%o3, %o5
.LL489:
	mov	0, %g1
.LL488:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE218:
	.size	LT__uLT_param_i16_i64, .-LT__uLT_param_i16_i64
	.align 4
	.type	LT__LT_param_i16_i64, #function
	.proc	014
LT__LT_param_i16_i64:
.LLFB219:
	save	%sp, -104, %sp
.LLCFI219:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bg	%icc, .LL491
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL492
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL491
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL492:
	mov	0, %g1
.LL491:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE219:
	.size	LT__LT_param_i16_i64, .-LT__LT_param_i16_i64
	.align 4
	.type	LT__uLT_var_i16_C, #function
	.proc	014
LT__uLT_var_i16_C:
.LLFB220:
	save	%sp, -96, %sp
.LLCFI220:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE220:
	.size	LT__uLT_var_i16_C, .-LT__uLT_var_i16_C
	.align 4
	.type	LT__LT_var_i16_C, #function
	.proc	014
LT__LT_var_i16_C:
.LLFB221:
	save	%sp, -96, %sp
.LLCFI221:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE221:
	.size	LT__LT_var_i16_C, .-LT__LT_var_i16_C
	.align 4
	.type	LT__uLT_param_i16_C, #function
	.proc	014
LT__uLT_param_i16_C:
.LLFB222:
	save	%sp, -96, %sp
.LLCFI222:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE222:
	.size	LT__uLT_param_i16_C, .-LT__uLT_param_i16_C
	.align 4
	.type	LT__LT_param_i16_C, #function
	.proc	014
LT__LT_param_i16_C:
.LLFB223:
	save	%sp, -96, %sp
.LLCFI223:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE223:
	.size	LT__LT_param_i16_C, .-LT__LT_param_i16_C
	.align 4
	.type	LT__uLT_var_i16_u8, #function
	.proc	014
LT__uLT_var_i16_u8:
.LLFB224:
	save	%sp, -96, %sp
.LLCFI224:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE224:
	.size	LT__uLT_var_i16_u8, .-LT__uLT_var_i16_u8
	.align 4
	.type	LT__LT_var_i16_u8, #function
	.proc	014
LT__LT_var_i16_u8:
.LLFB225:
	save	%sp, -96, %sp
.LLCFI225:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE225:
	.size	LT__LT_var_i16_u8, .-LT__LT_var_i16_u8
	.align 4
	.type	LT__uLT_param_i16_u8, #function
	.proc	014
LT__uLT_param_i16_u8:
.LLFB226:
	save	%sp, -96, %sp
.LLCFI226:
	sth	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE226:
	.size	LT__uLT_param_i16_u8, .-LT__uLT_param_i16_u8
	.align 4
	.type	LT__LT_param_i16_u8, #function
	.proc	014
LT__LT_param_i16_u8:
.LLFB227:
	save	%sp, -96, %sp
.LLCFI227:
	sth	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE227:
	.size	LT__LT_param_i16_u8, .-LT__LT_param_i16_u8
	.align 4
	.type	LT__uLT_var_i16_L, #function
	.proc	014
LT__uLT_var_i16_L:
.LLFB228:
	save	%sp, -96, %sp
.LLCFI228:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL502
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL503
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL502
	 nop
	cmp	%o3, %o5
.LL503:
	mov	0, %g1
.LL502:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE228:
	.size	LT__uLT_var_i16_L, .-LT__uLT_var_i16_L
	.align 4
	.type	LT__LT_var_i16_L, #function
	.proc	014
LT__LT_var_i16_L:
.LLFB229:
	save	%sp, -96, %sp
.LLCFI229:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL505
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL506
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL505
	 nop
	cmp	%o3, %o5
.LL506:
	mov	0, %g1
.LL505:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE229:
	.size	LT__LT_var_i16_L, .-LT__LT_var_i16_L
	.align 4
	.type	LT__uLT_param_i16_L, #function
	.proc	014
LT__uLT_param_i16_L:
.LLFB230:
	save	%sp, -104, %sp
.LLCFI230:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL508
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL509
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL508
	 nop
	cmp	%o3, %o5
.LL509:
	mov	0, %g1
.LL508:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE230:
	.size	LT__uLT_param_i16_L, .-LT__uLT_param_i16_L
	.align 4
	.type	LT__LT_param_i16_L, #function
	.proc	014
LT__LT_param_i16_L:
.LLFB231:
	save	%sp, -104, %sp
.LLCFI231:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bg	%icc, .LL511
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL512
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL511
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL512:
	mov	0, %g1
.LL511:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE231:
	.size	LT__LT_param_i16_L, .-LT__LT_param_i16_L
	.align 4
	.type	LT__uLT_var_i16_i8, #function
	.proc	014
LT__uLT_var_i16_i8:
.LLFB232:
	save	%sp, -96, %sp
.LLCFI232:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE232:
	.size	LT__uLT_var_i16_i8, .-LT__uLT_var_i16_i8
	.align 4
	.type	LT__LT_var_i16_i8, #function
	.proc	014
LT__LT_var_i16_i8:
.LLFB233:
	save	%sp, -96, %sp
.LLCFI233:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE233:
	.size	LT__LT_var_i16_i8, .-LT__LT_var_i16_i8
	.align 4
	.type	LT__uLT_param_i16_i8, #function
	.proc	014
LT__uLT_param_i16_i8:
.LLFB234:
	save	%sp, -96, %sp
.LLCFI234:
	sth	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE234:
	.size	LT__uLT_param_i16_i8, .-LT__uLT_param_i16_i8
	.align 4
	.type	LT__LT_param_i16_i8, #function
	.proc	014
LT__LT_param_i16_i8:
.LLFB235:
	save	%sp, -96, %sp
.LLCFI235:
	sth	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movg	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE235:
	.size	LT__LT_param_i16_i8, .-LT__LT_param_i16_i8
	.align 4
	.type	LT__uLT_var_i16_u32, #function
	.proc	014
LT__uLT_var_i16_u32:
.LLFB236:
	save	%sp, -96, %sp
.LLCFI236:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE236:
	.size	LT__uLT_var_i16_u32, .-LT__uLT_var_i16_u32
	.align 4
	.type	LT__LT_var_i16_u32, #function
	.proc	014
LT__LT_var_i16_u32:
.LLFB237:
	save	%sp, -96, %sp
.LLCFI237:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE237:
	.size	LT__LT_var_i16_u32, .-LT__LT_var_i16_u32
	.align 4
	.type	LT__uLT_param_i16_u32, #function
	.proc	014
LT__uLT_param_i16_u32:
.LLFB238:
	save	%sp, -96, %sp
.LLCFI238:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE238:
	.size	LT__uLT_param_i16_u32, .-LT__uLT_param_i16_u32
	.align 4
	.type	LT__LT_param_i16_u32, #function
	.proc	014
LT__LT_param_i16_u32:
.LLFB239:
	save	%sp, -96, %sp
.LLCFI239:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE239:
	.size	LT__LT_param_i16_u32, .-LT__LT_param_i16_u32
	.align 4
	.type	LT__uLT_var_I_u16, #function
	.proc	014
LT__uLT_var_I_u16:
.LLFB240:
	save	%sp, -96, %sp
.LLCFI240:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE240:
	.size	LT__uLT_var_I_u16, .-LT__uLT_var_I_u16
	.align 4
	.type	LT__LT_var_I_u16, #function
	.proc	014
LT__LT_var_I_u16:
.LLFB241:
	save	%sp, -96, %sp
.LLCFI241:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE241:
	.size	LT__LT_var_I_u16, .-LT__LT_var_I_u16
	.align 4
	.type	LT__uLT_param_I_u16, #function
	.proc	014
LT__uLT_param_I_u16:
.LLFB242:
	save	%sp, -96, %sp
.LLCFI242:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ld	[%fp+68], %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE242:
	.size	LT__uLT_param_I_u16, .-LT__uLT_param_I_u16
	.align 4
	.type	LT__LT_param_I_u16, #function
	.proc	014
LT__LT_param_I_u16:
.LLFB243:
	save	%sp, -96, %sp
.LLCFI243:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+68], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movg	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE243:
	.size	LT__LT_param_I_u16, .-LT__LT_param_I_u16
	.align 4
	.type	LT__uLT_var_I_u64, #function
	.proc	014
LT__uLT_var_I_u64:
.LLFB244:
	save	%sp, -96, %sp
.LLCFI244:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL526
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL527
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL526
	 nop
	cmp	%o3, %o5
.LL527:
	mov	0, %g1
.LL526:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE244:
	.size	LT__uLT_var_I_u64, .-LT__uLT_var_I_u64
	.align 4
	.type	LT__LT_var_I_u64, #function
	.proc	014
LT__LT_var_I_u64:
.LLFB245:
	save	%sp, -96, %sp
.LLCFI245:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL529
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL530
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL529
	 nop
	cmp	%o3, %o5
.LL530:
	mov	0, %g1
.LL529:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE245:
	.size	LT__LT_var_I_u64, .-LT__LT_var_I_u64
	.align 4
	.type	LT__uLT_param_I_u64, #function
	.proc	014
LT__uLT_param_I_u64:
.LLFB246:
	save	%sp, -104, %sp
.LLCFI246:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL532
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL533
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL532
	 nop
	cmp	%o3, %o5
.LL533:
	mov	0, %g1
.LL532:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE246:
	.size	LT__uLT_param_I_u64, .-LT__uLT_param_I_u64
	.align 4
	.type	LT__LT_param_I_u64, #function
	.proc	014
LT__LT_param_I_u64:
.LLFB247:
	save	%sp, -104, %sp
.LLCFI247:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bg	%icc, .LL535
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL536
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL535
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL536:
	mov	0, %g1
.LL535:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE247:
	.size	LT__LT_param_I_u64, .-LT__LT_param_I_u64
	.align 4
	.type	LT__uLT_var_I_LC, #function
	.proc	014
LT__uLT_var_I_LC:
.LLFB248:
	save	%sp, -96, %sp
.LLCFI248:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL538
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL539
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL538
	 nop
	cmp	%o3, %o5
.LL539:
	mov	0, %g1
.LL538:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE248:
	.size	LT__uLT_var_I_LC, .-LT__uLT_var_I_LC
	.align 4
	.type	LT__LT_var_I_LC, #function
	.proc	014
LT__LT_var_I_LC:
.LLFB249:
	save	%sp, -96, %sp
.LLCFI249:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL541
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL542
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL541
	 nop
	cmp	%o3, %o5
.LL542:
	mov	0, %g1
.LL541:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE249:
	.size	LT__LT_var_I_LC, .-LT__LT_var_I_LC
	.align 4
	.type	LT__uLT_param_I_LC, #function
	.proc	014
LT__uLT_param_I_LC:
.LLFB250:
	save	%sp, -104, %sp
.LLCFI250:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bgu	%icc, .LL544
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL545
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL544
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL545:
	mov	0, %g1
.LL544:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE250:
	.size	LT__uLT_param_I_LC, .-LT__uLT_param_I_LC
	.align 4
	.type	LT__LT_param_I_LC, #function
	.proc	014
LT__LT_param_I_LC:
.LLFB251:
	save	%sp, -104, %sp
.LLCFI251:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL547
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL548
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL547
	 nop
	cmp	%o3, %o5
.LL548:
	mov	0, %g1
.LL547:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE251:
	.size	LT__LT_param_I_LC, .-LT__LT_param_I_LC
	.align 4
	.type	LT__uLT_var_I_i32, #function
	.proc	014
LT__uLT_var_I_i32:
.LLFB252:
	save	%sp, -96, %sp
.LLCFI252:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE252:
	.size	LT__uLT_var_I_i32, .-LT__uLT_var_I_i32
	.align 4
	.type	LT__LT_var_I_i32, #function
	.proc	014
LT__LT_var_I_i32:
.LLFB253:
	save	%sp, -96, %sp
.LLCFI253:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE253:
	.size	LT__LT_var_I_i32, .-LT__LT_var_I_i32
	.align 4
	.type	LT__uLT_param_I_i32, #function
	.proc	014
LT__uLT_param_I_i32:
.LLFB254:
	save	%sp, -96, %sp
.LLCFI254:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE254:
	.size	LT__uLT_param_I_i32, .-LT__uLT_param_I_i32
	.align 4
	.type	LT__LT_param_I_i32, #function
	.proc	014
LT__LT_param_I_i32:
.LLFB255:
	save	%sp, -96, %sp
.LLCFI255:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE255:
	.size	LT__LT_param_I_i32, .-LT__LT_param_I_i32
	.align 4
	.type	LT__uLT_var_I_i16, #function
	.proc	014
LT__uLT_var_I_i16:
.LLFB256:
	save	%sp, -96, %sp
.LLCFI256:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE256:
	.size	LT__uLT_var_I_i16, .-LT__uLT_var_I_i16
	.align 4
	.type	LT__LT_var_I_i16, #function
	.proc	014
LT__LT_var_I_i16:
.LLFB257:
	save	%sp, -96, %sp
.LLCFI257:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE257:
	.size	LT__LT_var_I_i16, .-LT__LT_var_I_i16
	.align 4
	.type	LT__uLT_param_I_i16, #function
	.proc	014
LT__uLT_param_I_i16:
.LLFB258:
	save	%sp, -96, %sp
.LLCFI258:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ld	[%fp+68], %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE258:
	.size	LT__uLT_param_I_i16, .-LT__uLT_param_I_i16
	.align 4
	.type	LT__LT_param_I_i16, #function
	.proc	014
LT__LT_param_I_i16:
.LLFB259:
	save	%sp, -96, %sp
.LLCFI259:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+68], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movg	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE259:
	.size	LT__LT_param_I_i16, .-LT__LT_param_I_i16
	.align 4
	.type	LT__uLT_var_I_I, #function
	.proc	014
LT__uLT_var_I_I:
.LLFB260:
	save	%sp, -96, %sp
.LLCFI260:
	mov	0, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE260:
	.size	LT__uLT_var_I_I, .-LT__uLT_var_I_I
	.align 4
	.type	LT__LT_var_I_I, #function
	.proc	014
LT__LT_var_I_I:
.LLFB261:
	save	%sp, -96, %sp
.LLCFI261:
	mov	0, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE261:
	.size	LT__LT_var_I_I, .-LT__LT_var_I_I
	.align 4
	.type	LT__uLT_param_I_I, #function
	.proc	014
LT__uLT_param_I_I:
.LLFB262:
	save	%sp, -96, %sp
.LLCFI262:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE262:
	.size	LT__uLT_param_I_I, .-LT__uLT_param_I_I
	.align 4
	.type	LT__LT_param_I_I, #function
	.proc	014
LT__LT_param_I_I:
.LLFB263:
	save	%sp, -96, %sp
.LLCFI263:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE263:
	.size	LT__LT_param_I_I, .-LT__LT_param_I_I
	.align 4
	.type	LT__uLT_var_I_i64, #function
	.proc	014
LT__uLT_var_I_i64:
.LLFB264:
	save	%sp, -96, %sp
.LLCFI264:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL562
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL563
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL562
	 nop
	cmp	%o3, %o5
.LL563:
	mov	0, %g1
.LL562:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE264:
	.size	LT__uLT_var_I_i64, .-LT__uLT_var_I_i64
	.align 4
	.type	LT__LT_var_I_i64, #function
	.proc	014
LT__LT_var_I_i64:
.LLFB265:
	save	%sp, -96, %sp
.LLCFI265:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL565
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL566
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL565
	 nop
	cmp	%o3, %o5
.LL566:
	mov	0, %g1
.LL565:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE265:
	.size	LT__LT_var_I_i64, .-LT__LT_var_I_i64
	.align 4
	.type	LT__uLT_param_I_i64, #function
	.proc	014
LT__uLT_param_I_i64:
.LLFB266:
	save	%sp, -104, %sp
.LLCFI266:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL568
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL569
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL568
	 nop
	cmp	%o3, %o5
.LL569:
	mov	0, %g1
.LL568:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE266:
	.size	LT__uLT_param_I_i64, .-LT__uLT_param_I_i64
	.align 4
	.type	LT__LT_param_I_i64, #function
	.proc	014
LT__LT_param_I_i64:
.LLFB267:
	save	%sp, -104, %sp
.LLCFI267:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bg	%icc, .LL571
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL572
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL571
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL572:
	mov	0, %g1
.LL571:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE267:
	.size	LT__LT_param_I_i64, .-LT__LT_param_I_i64
	.align 4
	.type	LT__uLT_var_I_C, #function
	.proc	014
LT__uLT_var_I_C:
.LLFB268:
	save	%sp, -96, %sp
.LLCFI268:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE268:
	.size	LT__uLT_var_I_C, .-LT__uLT_var_I_C
	.align 4
	.type	LT__LT_var_I_C, #function
	.proc	014
LT__LT_var_I_C:
.LLFB269:
	save	%sp, -96, %sp
.LLCFI269:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE269:
	.size	LT__LT_var_I_C, .-LT__LT_var_I_C
	.align 4
	.type	LT__uLT_param_I_C, #function
	.proc	014
LT__uLT_param_I_C:
.LLFB270:
	save	%sp, -96, %sp
.LLCFI270:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE270:
	.size	LT__uLT_param_I_C, .-LT__uLT_param_I_C
	.align 4
	.type	LT__LT_param_I_C, #function
	.proc	014
LT__LT_param_I_C:
.LLFB271:
	save	%sp, -96, %sp
.LLCFI271:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movg	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE271:
	.size	LT__LT_param_I_C, .-LT__LT_param_I_C
	.align 4
	.type	LT__uLT_var_I_u8, #function
	.proc	014
LT__uLT_var_I_u8:
.LLFB272:
	save	%sp, -96, %sp
.LLCFI272:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE272:
	.size	LT__uLT_var_I_u8, .-LT__uLT_var_I_u8
	.align 4
	.type	LT__LT_var_I_u8, #function
	.proc	014
LT__LT_var_I_u8:
.LLFB273:
	save	%sp, -96, %sp
.LLCFI273:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE273:
	.size	LT__LT_var_I_u8, .-LT__LT_var_I_u8
	.align 4
	.type	LT__uLT_param_I_u8, #function
	.proc	014
LT__uLT_param_I_u8:
.LLFB274:
	save	%sp, -96, %sp
.LLCFI274:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE274:
	.size	LT__uLT_param_I_u8, .-LT__uLT_param_I_u8
	.align 4
	.type	LT__LT_param_I_u8, #function
	.proc	014
LT__LT_param_I_u8:
.LLFB275:
	save	%sp, -96, %sp
.LLCFI275:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+68], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movg	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE275:
	.size	LT__LT_param_I_u8, .-LT__LT_param_I_u8
	.align 4
	.type	LT__uLT_var_I_L, #function
	.proc	014
LT__uLT_var_I_L:
.LLFB276:
	save	%sp, -96, %sp
.LLCFI276:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL582
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL583
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL582
	 nop
	cmp	%o3, %o5
.LL583:
	mov	0, %g1
.LL582:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE276:
	.size	LT__uLT_var_I_L, .-LT__uLT_var_I_L
	.align 4
	.type	LT__LT_var_I_L, #function
	.proc	014
LT__LT_var_I_L:
.LLFB277:
	save	%sp, -96, %sp
.LLCFI277:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL585
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL586
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL585
	 nop
	cmp	%o3, %o5
.LL586:
	mov	0, %g1
.LL585:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE277:
	.size	LT__LT_var_I_L, .-LT__LT_var_I_L
	.align 4
	.type	LT__uLT_param_I_L, #function
	.proc	014
LT__uLT_param_I_L:
.LLFB278:
	save	%sp, -104, %sp
.LLCFI278:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL588
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL589
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL588
	 nop
	cmp	%o3, %o5
.LL589:
	mov	0, %g1
.LL588:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE278:
	.size	LT__uLT_param_I_L, .-LT__uLT_param_I_L
	.align 4
	.type	LT__LT_param_I_L, #function
	.proc	014
LT__LT_param_I_L:
.LLFB279:
	save	%sp, -104, %sp
.LLCFI279:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bg	%icc, .LL591
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL592
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL591
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL592:
	mov	0, %g1
.LL591:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE279:
	.size	LT__LT_param_I_L, .-LT__LT_param_I_L
	.align 4
	.type	LT__uLT_var_I_i8, #function
	.proc	014
LT__uLT_var_I_i8:
.LLFB280:
	save	%sp, -96, %sp
.LLCFI280:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE280:
	.size	LT__uLT_var_I_i8, .-LT__uLT_var_I_i8
	.align 4
	.type	LT__LT_var_I_i8, #function
	.proc	014
LT__LT_var_I_i8:
.LLFB281:
	save	%sp, -96, %sp
.LLCFI281:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE281:
	.size	LT__LT_var_I_i8, .-LT__LT_var_I_i8
	.align 4
	.type	LT__uLT_param_I_i8, #function
	.proc	014
LT__uLT_param_I_i8:
.LLFB282:
	save	%sp, -96, %sp
.LLCFI282:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE282:
	.size	LT__uLT_param_I_i8, .-LT__uLT_param_I_i8
	.align 4
	.type	LT__LT_param_I_i8, #function
	.proc	014
LT__LT_param_I_i8:
.LLFB283:
	save	%sp, -96, %sp
.LLCFI283:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+68], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movg	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE283:
	.size	LT__LT_param_I_i8, .-LT__LT_param_I_i8
	.align 4
	.type	LT__uLT_var_I_u32, #function
	.proc	014
LT__uLT_var_I_u32:
.LLFB284:
	save	%sp, -96, %sp
.LLCFI284:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE284:
	.size	LT__uLT_var_I_u32, .-LT__uLT_var_I_u32
	.align 4
	.type	LT__LT_var_I_u32, #function
	.proc	014
LT__LT_var_I_u32:
.LLFB285:
	save	%sp, -96, %sp
.LLCFI285:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE285:
	.size	LT__LT_var_I_u32, .-LT__LT_var_I_u32
	.align 4
	.type	LT__uLT_param_I_u32, #function
	.proc	014
LT__uLT_param_I_u32:
.LLFB286:
	save	%sp, -96, %sp
.LLCFI286:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE286:
	.size	LT__uLT_param_I_u32, .-LT__uLT_param_I_u32
	.align 4
	.type	LT__LT_param_I_u32, #function
	.proc	014
LT__LT_param_I_u32:
.LLFB287:
	save	%sp, -96, %sp
.LLCFI287:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE287:
	.size	LT__LT_param_I_u32, .-LT__LT_param_I_u32
	.align 4
	.type	LT__uLT_var_i64_u16, #function
	.proc	014
LT__uLT_var_i64_u16:
.LLFB288:
	save	%sp, -96, %sp
.LLCFI288:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL602
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL603
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL602
	 nop
	cmp	%o3, %o5
.LL603:
	mov	0, %g1
.LL602:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE288:
	.size	LT__uLT_var_i64_u16, .-LT__uLT_var_i64_u16
	.align 4
	.type	LT__LT_var_i64_u16, #function
	.proc	014
LT__LT_var_i64_u16:
.LLFB289:
	save	%sp, -96, %sp
.LLCFI289:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL605
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL606
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL605
	 nop
	cmp	%o3, %o5
.LL606:
	mov	0, %g1
.LL605:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE289:
	.size	LT__LT_var_i64_u16, .-LT__LT_var_i64_u16
	.align 4
	.type	LT__uLT_param_i64_u16, #function
	.proc	014
LT__uLT_param_i64_u16:
.LLFB290:
	save	%sp, -104, %sp
.LLCFI290:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL608
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL609
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL608
	 nop
	cmp	%o3, %o5
.LL609:
	mov	0, %g1
.LL608:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE290:
	.size	LT__uLT_param_i64_u16, .-LT__uLT_param_i64_u16
	.align 4
	.type	LT__LT_param_i64_u16, #function
	.proc	014
LT__LT_param_i64_u16:
.LLFB291:
	save	%sp, -104, %sp
.LLCFI291:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bl	%icc, .LL611
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL612
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL611
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL612:
	mov	0, %g1
.LL611:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE291:
	.size	LT__LT_param_i64_u16, .-LT__LT_param_i64_u16
	.align 4
	.type	LT__uLT_var_i64_u64, #function
	.proc	014
LT__uLT_var_i64_u64:
.LLFB292:
	save	%sp, -96, %sp
.LLCFI292:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL614
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL615
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL614
	 nop
	cmp	%o3, %o5
.LL615:
	mov	0, %g1
.LL614:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE292:
	.size	LT__uLT_var_i64_u64, .-LT__uLT_var_i64_u64
	.align 4
	.type	LT__LT_var_i64_u64, #function
	.proc	014
LT__LT_var_i64_u64:
.LLFB293:
	save	%sp, -96, %sp
.LLCFI293:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL617
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL618
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL617
	 nop
	cmp	%o3, %o5
.LL618:
	mov	0, %g1
.LL617:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE293:
	.size	LT__LT_var_i64_u64, .-LT__LT_var_i64_u64
	.align 4
	.type	LT__uLT_param_i64_u64, #function
	.proc	014
LT__uLT_param_i64_u64:
.LLFB294:
	save	%sp, -112, %sp
.LLCFI294:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	ldd	[%fp-16], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL620
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL621
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL620
	 nop
	cmp	%o3, %o5
.LL621:
	mov	0, %g1
.LL620:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE294:
	.size	LT__uLT_param_i64_u64, .-LT__uLT_param_i64_u64
	.align 4
	.type	LT__LT_param_i64_u64, #function
	.proc	014
LT__LT_param_i64_u64:
.LLFB295:
	save	%sp, -112, %sp
.LLCFI295:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	mov	1, %g1
	ld	[%fp-16], %o4
	ld	[%fp-8], %o5
	cmp	%o4, %o5
	bg	%icc, .LL623
	 nop
	ld	[%fp-16], %o4
	ld	[%fp-8], %o5
	cmp	%o4, %o5
	bne	%icc, .LL624
	 nop
	ld	[%fp-12], %o4
	ld	[%fp-4], %o5
	cmp	%o4, %o5
	bgu	%icc, .LL623
	 nop
	ld	[%fp-12], %o5
	ld	[%fp-4], %g1
	cmp	%o5, %g1
.LL624:
	mov	0, %g1
.LL623:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE295:
	.size	LT__LT_param_i64_u64, .-LT__LT_param_i64_u64
	.align 4
	.type	LT__uLT_var_i64_LC, #function
	.proc	014
LT__uLT_var_i64_LC:
.LLFB296:
	save	%sp, -96, %sp
.LLCFI296:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL626
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL627
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL626
	 nop
	cmp	%o3, %o5
.LL627:
	mov	0, %g1
.LL626:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE296:
	.size	LT__uLT_var_i64_LC, .-LT__uLT_var_i64_LC
	.align 4
	.type	LT__LT_var_i64_LC, #function
	.proc	014
LT__LT_var_i64_LC:
.LLFB297:
	save	%sp, -96, %sp
.LLCFI297:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL629
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL630
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL629
	 nop
	cmp	%o3, %o5
.LL630:
	mov	0, %g1
.LL629:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE297:
	.size	LT__LT_var_i64_LC, .-LT__LT_var_i64_LC
	.align 4
	.type	LT__uLT_param_i64_LC, #function
	.proc	014
LT__uLT_param_i64_LC:
.LLFB298:
	save	%sp, -112, %sp
.LLCFI298:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	mov	1, %g1
	ld	[%fp-16], %o3
	cmp	%o3, %o4
	bgu	%icc, .LL632
	 nop
	ld	[%fp-16], %o3
	cmp	%o3, %o4
	bne	%icc, .LL633
	 nop
	ld	[%fp-12], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL632
	 nop
	ld	[%fp-12], %g1
	cmp	%g1, %o5
.LL633:
	mov	0, %g1
.LL632:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE298:
	.size	LT__uLT_param_i64_LC, .-LT__uLT_param_i64_LC
	.align 4
	.type	LT__LT_param_i64_LC, #function
	.proc	014
LT__LT_param_i64_LC:
.LLFB299:
	save	%sp, -112, %sp
.LLCFI299:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bl	%icc, .LL635
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL636
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL635
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL636:
	mov	0, %g1
.LL635:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE299:
	.size	LT__LT_param_i64_LC, .-LT__LT_param_i64_LC
	.align 4
	.type	LT__uLT_var_i64_i32, #function
	.proc	014
LT__uLT_var_i64_i32:
.LLFB300:
	save	%sp, -96, %sp
.LLCFI300:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL638
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL639
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL638
	 nop
	cmp	%o5, %o3
.LL639:
	mov	0, %g1
.LL638:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE300:
	.size	LT__uLT_var_i64_i32, .-LT__uLT_var_i64_i32
	.align 4
	.type	LT__LT_var_i64_i32, #function
	.proc	014
LT__LT_var_i64_i32:
.LLFB301:
	save	%sp, -96, %sp
.LLCFI301:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL641
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL642
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL641
	 nop
	cmp	%o5, %o3
.LL642:
	mov	0, %g1
.LL641:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE301:
	.size	LT__LT_var_i64_i32, .-LT__LT_var_i64_i32
	.align 4
	.type	LT__uLT_param_i64_i32, #function
	.proc	014
LT__uLT_param_i64_i32:
.LLFB302:
	save	%sp, -104, %sp
.LLCFI302:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL644
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL645
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL644
	 nop
	cmp	%o5, %o3
.LL645:
	mov	0, %g1
.LL644:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE302:
	.size	LT__uLT_param_i64_i32, .-LT__uLT_param_i64_i32
	.align 4
	.type	LT__LT_param_i64_i32, #function
	.proc	014
LT__LT_param_i64_i32:
.LLFB303:
	save	%sp, -104, %sp
.LLCFI303:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bl	%icc, .LL647
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL648
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL647
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL648:
	mov	0, %g1
.LL647:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE303:
	.size	LT__LT_param_i64_i32, .-LT__LT_param_i64_i32
	.align 4
	.type	LT__uLT_var_i64_i16, #function
	.proc	014
LT__uLT_var_i64_i16:
.LLFB304:
	save	%sp, -96, %sp
.LLCFI304:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL650
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL651
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL650
	 nop
	cmp	%o5, %o3
.LL651:
	mov	0, %g1
.LL650:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE304:
	.size	LT__uLT_var_i64_i16, .-LT__uLT_var_i64_i16
	.align 4
	.type	LT__LT_var_i64_i16, #function
	.proc	014
LT__LT_var_i64_i16:
.LLFB305:
	save	%sp, -96, %sp
.LLCFI305:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL653
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL654
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL653
	 nop
	cmp	%o5, %o3
.LL654:
	mov	0, %g1
.LL653:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE305:
	.size	LT__LT_var_i64_i16, .-LT__LT_var_i64_i16
	.align 4
	.type	LT__uLT_param_i64_i16, #function
	.proc	014
LT__uLT_param_i64_i16:
.LLFB306:
	save	%sp, -104, %sp
.LLCFI306:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL656
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL657
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL656
	 nop
	cmp	%o5, %o3
.LL657:
	mov	0, %g1
.LL656:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE306:
	.size	LT__uLT_param_i64_i16, .-LT__uLT_param_i64_i16
	.align 4
	.type	LT__LT_param_i64_i16, #function
	.proc	014
LT__LT_param_i64_i16:
.LLFB307:
	save	%sp, -104, %sp
.LLCFI307:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bl	%icc, .LL659
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL660
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL659
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL660:
	mov	0, %g1
.LL659:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE307:
	.size	LT__LT_param_i64_i16, .-LT__LT_param_i64_i16
	.align 4
	.type	LT__uLT_var_i64_I, #function
	.proc	014
LT__uLT_var_i64_I:
.LLFB308:
	save	%sp, -96, %sp
.LLCFI308:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL662
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL663
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL662
	 nop
	cmp	%o5, %o3
.LL663:
	mov	0, %g1
.LL662:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE308:
	.size	LT__uLT_var_i64_I, .-LT__uLT_var_i64_I
	.align 4
	.type	LT__LT_var_i64_I, #function
	.proc	014
LT__LT_var_i64_I:
.LLFB309:
	save	%sp, -96, %sp
.LLCFI309:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL665
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL666
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL665
	 nop
	cmp	%o5, %o3
.LL666:
	mov	0, %g1
.LL665:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE309:
	.size	LT__LT_var_i64_I, .-LT__LT_var_i64_I
	.align 4
	.type	LT__uLT_param_i64_I, #function
	.proc	014
LT__uLT_param_i64_I:
.LLFB310:
	save	%sp, -104, %sp
.LLCFI310:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL668
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL669
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL668
	 nop
	cmp	%o5, %o3
.LL669:
	mov	0, %g1
.LL668:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE310:
	.size	LT__uLT_param_i64_I, .-LT__uLT_param_i64_I
	.align 4
	.type	LT__LT_param_i64_I, #function
	.proc	014
LT__LT_param_i64_I:
.LLFB311:
	save	%sp, -104, %sp
.LLCFI311:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bl	%icc, .LL671
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL672
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL671
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL672:
	mov	0, %g1
.LL671:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE311:
	.size	LT__LT_param_i64_I, .-LT__LT_param_i64_I
	.align 4
	.type	LT__uLT_var_i64_i64, #function
	.proc	014
LT__uLT_var_i64_i64:
.LLFB312:
	save	%sp, -96, %sp
.LLCFI312:
	mov	0, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE312:
	.size	LT__uLT_var_i64_i64, .-LT__uLT_var_i64_i64
	.align 4
	.type	LT__LT_var_i64_i64, #function
	.proc	014
LT__LT_var_i64_i64:
.LLFB313:
	save	%sp, -96, %sp
.LLCFI313:
	mov	0, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE313:
	.size	LT__LT_var_i64_i64, .-LT__LT_var_i64_i64
	.align 4
	.type	LT__uLT_param_i64_i64, #function
	.proc	014
LT__uLT_param_i64_i64:
.LLFB314:
	save	%sp, -112, %sp
.LLCFI314:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	ldd	[%fp-16], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL676
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL677
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL676
	 nop
	cmp	%o3, %o5
.LL677:
	mov	0, %g1
.LL676:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE314:
	.size	LT__uLT_param_i64_i64, .-LT__uLT_param_i64_i64
	.align 4
	.type	LT__LT_param_i64_i64, #function
	.proc	014
LT__LT_param_i64_i64:
.LLFB315:
	save	%sp, -112, %sp
.LLCFI315:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	mov	1, %g1
	ld	[%fp-16], %o4
	ld	[%fp-8], %o5
	cmp	%o4, %o5
	bg	%icc, .LL679
	 nop
	ld	[%fp-16], %o4
	ld	[%fp-8], %o5
	cmp	%o4, %o5
	bne	%icc, .LL680
	 nop
	ld	[%fp-12], %o4
	ld	[%fp-4], %o5
	cmp	%o4, %o5
	bgu	%icc, .LL679
	 nop
	ld	[%fp-12], %o5
	ld	[%fp-4], %g1
	cmp	%o5, %g1
.LL680:
	mov	0, %g1
.LL679:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE315:
	.size	LT__LT_param_i64_i64, .-LT__LT_param_i64_i64
	.align 4
	.type	LT__uLT_var_i64_C, #function
	.proc	014
LT__uLT_var_i64_C:
.LLFB316:
	save	%sp, -96, %sp
.LLCFI316:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL682
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL683
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL682
	 nop
	cmp	%o5, %o3
.LL683:
	mov	0, %g1
.LL682:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE316:
	.size	LT__uLT_var_i64_C, .-LT__uLT_var_i64_C
	.align 4
	.type	LT__LT_var_i64_C, #function
	.proc	014
LT__LT_var_i64_C:
.LLFB317:
	save	%sp, -96, %sp
.LLCFI317:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL685
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL686
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL685
	 nop
	cmp	%o5, %o3
.LL686:
	mov	0, %g1
.LL685:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE317:
	.size	LT__LT_var_i64_C, .-LT__LT_var_i64_C
	.align 4
	.type	LT__uLT_param_i64_C, #function
	.proc	014
LT__uLT_param_i64_C:
.LLFB318:
	save	%sp, -104, %sp
.LLCFI318:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL688
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL689
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL688
	 nop
	cmp	%o5, %o3
.LL689:
	mov	0, %g1
.LL688:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE318:
	.size	LT__uLT_param_i64_C, .-LT__uLT_param_i64_C
	.align 4
	.type	LT__LT_param_i64_C, #function
	.proc	014
LT__LT_param_i64_C:
.LLFB319:
	save	%sp, -104, %sp
.LLCFI319:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bl	%icc, .LL691
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL692
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL691
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL692:
	mov	0, %g1
.LL691:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE319:
	.size	LT__LT_param_i64_C, .-LT__LT_param_i64_C
	.align 4
	.type	LT__uLT_var_i64_u8, #function
	.proc	014
LT__uLT_var_i64_u8:
.LLFB320:
	save	%sp, -96, %sp
.LLCFI320:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL694
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL695
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL694
	 nop
	cmp	%o3, %o5
.LL695:
	mov	0, %g1
.LL694:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE320:
	.size	LT__uLT_var_i64_u8, .-LT__uLT_var_i64_u8
	.align 4
	.type	LT__LT_var_i64_u8, #function
	.proc	014
LT__LT_var_i64_u8:
.LLFB321:
	save	%sp, -96, %sp
.LLCFI321:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL697
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL698
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL697
	 nop
	cmp	%o3, %o5
.LL698:
	mov	0, %g1
.LL697:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE321:
	.size	LT__LT_var_i64_u8, .-LT__LT_var_i64_u8
	.align 4
	.type	LT__uLT_param_i64_u8, #function
	.proc	014
LT__uLT_param_i64_u8:
.LLFB322:
	save	%sp, -104, %sp
.LLCFI322:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL700
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL701
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL700
	 nop
	cmp	%o3, %o5
.LL701:
	mov	0, %g1
.LL700:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE322:
	.size	LT__uLT_param_i64_u8, .-LT__uLT_param_i64_u8
	.align 4
	.type	LT__LT_param_i64_u8, #function
	.proc	014
LT__LT_param_i64_u8:
.LLFB323:
	save	%sp, -104, %sp
.LLCFI323:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bl	%icc, .LL703
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL704
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL703
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL704:
	mov	0, %g1
.LL703:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE323:
	.size	LT__LT_param_i64_u8, .-LT__LT_param_i64_u8
	.align 4
	.type	LT__uLT_var_i64_L, #function
	.proc	014
LT__uLT_var_i64_L:
.LLFB324:
	save	%sp, -96, %sp
.LLCFI324:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL706
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL707
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL706
	 nop
	cmp	%o3, %o5
.LL707:
	mov	0, %g1
.LL706:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE324:
	.size	LT__uLT_var_i64_L, .-LT__uLT_var_i64_L
	.align 4
	.type	LT__LT_var_i64_L, #function
	.proc	014
LT__LT_var_i64_L:
.LLFB325:
	save	%sp, -96, %sp
.LLCFI325:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL709
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL710
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL709
	 nop
	cmp	%o3, %o5
.LL710:
	mov	0, %g1
.LL709:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE325:
	.size	LT__LT_var_i64_L, .-LT__LT_var_i64_L
	.align 4
	.type	LT__uLT_param_i64_L, #function
	.proc	014
LT__uLT_param_i64_L:
.LLFB326:
	save	%sp, -112, %sp
.LLCFI326:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	ldd	[%fp-16], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL712
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL713
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL712
	 nop
	cmp	%o3, %o5
.LL713:
	mov	0, %g1
.LL712:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE326:
	.size	LT__uLT_param_i64_L, .-LT__uLT_param_i64_L
	.align 4
	.type	LT__LT_param_i64_L, #function
	.proc	014
LT__LT_param_i64_L:
.LLFB327:
	save	%sp, -112, %sp
.LLCFI327:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	mov	1, %g1
	ld	[%fp-16], %o4
	ld	[%fp-8], %o5
	cmp	%o4, %o5
	bg	%icc, .LL715
	 nop
	ld	[%fp-16], %o4
	ld	[%fp-8], %o5
	cmp	%o4, %o5
	bne	%icc, .LL716
	 nop
	ld	[%fp-12], %o4
	ld	[%fp-4], %o5
	cmp	%o4, %o5
	bgu	%icc, .LL715
	 nop
	ld	[%fp-12], %o5
	ld	[%fp-4], %g1
	cmp	%o5, %g1
.LL716:
	mov	0, %g1
.LL715:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE327:
	.size	LT__LT_param_i64_L, .-LT__LT_param_i64_L
	.align 4
	.type	LT__uLT_var_i64_i8, #function
	.proc	014
LT__uLT_var_i64_i8:
.LLFB328:
	save	%sp, -96, %sp
.LLCFI328:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL718
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL719
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL718
	 nop
	cmp	%o5, %o3
.LL719:
	mov	0, %g1
.LL718:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE328:
	.size	LT__uLT_var_i64_i8, .-LT__uLT_var_i64_i8
	.align 4
	.type	LT__LT_var_i64_i8, #function
	.proc	014
LT__LT_var_i64_i8:
.LLFB329:
	save	%sp, -96, %sp
.LLCFI329:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL721
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL722
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL721
	 nop
	cmp	%o5, %o3
.LL722:
	mov	0, %g1
.LL721:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE329:
	.size	LT__LT_var_i64_i8, .-LT__LT_var_i64_i8
	.align 4
	.type	LT__uLT_param_i64_i8, #function
	.proc	014
LT__uLT_param_i64_i8:
.LLFB330:
	save	%sp, -104, %sp
.LLCFI330:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL724
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL725
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL724
	 nop
	cmp	%o5, %o3
.LL725:
	mov	0, %g1
.LL724:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE330:
	.size	LT__uLT_param_i64_i8, .-LT__uLT_param_i64_i8
	.align 4
	.type	LT__LT_param_i64_i8, #function
	.proc	014
LT__LT_param_i64_i8:
.LLFB331:
	save	%sp, -104, %sp
.LLCFI331:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bl	%icc, .LL727
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL728
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL727
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL728:
	mov	0, %g1
.LL727:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE331:
	.size	LT__LT_param_i64_i8, .-LT__LT_param_i64_i8
	.align 4
	.type	LT__uLT_var_i64_u32, #function
	.proc	014
LT__uLT_var_i64_u32:
.LLFB332:
	save	%sp, -96, %sp
.LLCFI332:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL730
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL731
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL730
	 nop
	cmp	%o5, %o3
.LL731:
	mov	0, %g1
.LL730:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE332:
	.size	LT__uLT_var_i64_u32, .-LT__uLT_var_i64_u32
	.align 4
	.type	LT__LT_var_i64_u32, #function
	.proc	014
LT__LT_var_i64_u32:
.LLFB333:
	save	%sp, -96, %sp
.LLCFI333:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL733
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL734
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL733
	 nop
	cmp	%o5, %o3
.LL734:
	mov	0, %g1
.LL733:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE333:
	.size	LT__LT_var_i64_u32, .-LT__LT_var_i64_u32
	.align 4
	.type	LT__uLT_param_i64_u32, #function
	.proc	014
LT__uLT_param_i64_u32:
.LLFB334:
	save	%sp, -104, %sp
.LLCFI334:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL736
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL737
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL736
	 nop
	cmp	%o5, %o3
.LL737:
	mov	0, %g1
.LL736:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE334:
	.size	LT__uLT_param_i64_u32, .-LT__uLT_param_i64_u32
	.align 4
	.type	LT__LT_param_i64_u32, #function
	.proc	014
LT__LT_param_i64_u32:
.LLFB335:
	save	%sp, -104, %sp
.LLCFI335:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bl	%icc, .LL739
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL740
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL739
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL740:
	mov	0, %g1
.LL739:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE335:
	.size	LT__LT_param_i64_u32, .-LT__LT_param_i64_u32
	.align 4
	.type	LT__uLT_var_C_u16, #function
	.proc	014
LT__uLT_var_C_u16:
.LLFB336:
	save	%sp, -96, %sp
.LLCFI336:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE336:
	.size	LT__uLT_var_C_u16, .-LT__uLT_var_C_u16
	.align 4
	.type	LT__LT_var_C_u16, #function
	.proc	014
LT__LT_var_C_u16:
.LLFB337:
	save	%sp, -96, %sp
.LLCFI337:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE337:
	.size	LT__LT_var_C_u16, .-LT__LT_var_C_u16
	.align 4
	.type	LT__uLT_param_C_u16, #function
	.proc	014
LT__uLT_param_C_u16:
.LLFB338:
	save	%sp, -96, %sp
.LLCFI338:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+68], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE338:
	.size	LT__uLT_param_C_u16, .-LT__uLT_param_C_u16
	.align 4
	.type	LT__LT_param_C_u16, #function
	.proc	014
LT__LT_param_C_u16:
.LLFB339:
	save	%sp, -96, %sp
.LLCFI339:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ld	[%fp+68], %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE339:
	.size	LT__LT_param_C_u16, .-LT__LT_param_C_u16
	.align 4
	.type	LT__uLT_var_C_u64, #function
	.proc	014
LT__uLT_var_C_u64:
.LLFB340:
	save	%sp, -96, %sp
.LLCFI340:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL746
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL747
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL746
	 nop
	cmp	%o3, %o5
.LL747:
	mov	0, %g1
.LL746:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE340:
	.size	LT__uLT_var_C_u64, .-LT__uLT_var_C_u64
	.align 4
	.type	LT__LT_var_C_u64, #function
	.proc	014
LT__LT_var_C_u64:
.LLFB341:
	save	%sp, -96, %sp
.LLCFI341:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL749
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL750
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL749
	 nop
	cmp	%o3, %o5
.LL750:
	mov	0, %g1
.LL749:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE341:
	.size	LT__LT_var_C_u64, .-LT__LT_var_C_u64
	.align 4
	.type	LT__uLT_param_C_u64, #function
	.proc	014
LT__uLT_param_C_u64:
.LLFB342:
	save	%sp, -104, %sp
.LLCFI342:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL752
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL753
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL752
	 nop
	cmp	%o3, %o5
.LL753:
	mov	0, %g1
.LL752:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE342:
	.size	LT__uLT_param_C_u64, .-LT__uLT_param_C_u64
	.align 4
	.type	LT__LT_param_C_u64, #function
	.proc	014
LT__LT_param_C_u64:
.LLFB343:
	save	%sp, -104, %sp
.LLCFI343:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bg	%icc, .LL755
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL756
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL755
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL756:
	mov	0, %g1
.LL755:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE343:
	.size	LT__LT_param_C_u64, .-LT__LT_param_C_u64
	.align 4
	.type	LT__uLT_var_C_LC, #function
	.proc	014
LT__uLT_var_C_LC:
.LLFB344:
	save	%sp, -96, %sp
.LLCFI344:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL758
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL759
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL758
	 nop
	cmp	%o3, %o5
.LL759:
	mov	0, %g1
.LL758:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE344:
	.size	LT__uLT_var_C_LC, .-LT__uLT_var_C_LC
	.align 4
	.type	LT__LT_var_C_LC, #function
	.proc	014
LT__LT_var_C_LC:
.LLFB345:
	save	%sp, -96, %sp
.LLCFI345:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL761
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL762
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL761
	 nop
	cmp	%o3, %o5
.LL762:
	mov	0, %g1
.LL761:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE345:
	.size	LT__LT_var_C_LC, .-LT__LT_var_C_LC
	.align 4
	.type	LT__uLT_param_C_LC, #function
	.proc	014
LT__uLT_param_C_LC:
.LLFB346:
	save	%sp, -104, %sp
.LLCFI346:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bgu	%icc, .LL764
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL765
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL764
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL765:
	mov	0, %g1
.LL764:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE346:
	.size	LT__uLT_param_C_LC, .-LT__uLT_param_C_LC
	.align 4
	.type	LT__LT_param_C_LC, #function
	.proc	014
LT__LT_param_C_LC:
.LLFB347:
	save	%sp, -104, %sp
.LLCFI347:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL767
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL768
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL767
	 nop
	cmp	%o3, %o5
.LL768:
	mov	0, %g1
.LL767:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE347:
	.size	LT__LT_param_C_LC, .-LT__LT_param_C_LC
	.align 4
	.type	LT__uLT_var_C_i32, #function
	.proc	014
LT__uLT_var_C_i32:
.LLFB348:
	save	%sp, -96, %sp
.LLCFI348:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE348:
	.size	LT__uLT_var_C_i32, .-LT__uLT_var_C_i32
	.align 4
	.type	LT__LT_var_C_i32, #function
	.proc	014
LT__LT_var_C_i32:
.LLFB349:
	save	%sp, -96, %sp
.LLCFI349:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE349:
	.size	LT__LT_var_C_i32, .-LT__LT_var_C_i32
	.align 4
	.type	LT__uLT_param_C_i32, #function
	.proc	014
LT__uLT_param_C_i32:
.LLFB350:
	save	%sp, -96, %sp
.LLCFI350:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE350:
	.size	LT__uLT_param_C_i32, .-LT__uLT_param_C_i32
	.align 4
	.type	LT__LT_param_C_i32, #function
	.proc	014
LT__LT_param_C_i32:
.LLFB351:
	save	%sp, -96, %sp
.LLCFI351:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE351:
	.size	LT__LT_param_C_i32, .-LT__LT_param_C_i32
	.align 4
	.type	LT__uLT_var_C_i16, #function
	.proc	014
LT__uLT_var_C_i16:
.LLFB352:
	save	%sp, -96, %sp
.LLCFI352:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE352:
	.size	LT__uLT_var_C_i16, .-LT__uLT_var_C_i16
	.align 4
	.type	LT__LT_var_C_i16, #function
	.proc	014
LT__LT_var_C_i16:
.LLFB353:
	save	%sp, -96, %sp
.LLCFI353:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE353:
	.size	LT__LT_var_C_i16, .-LT__LT_var_C_i16
	.align 4
	.type	LT__uLT_param_C_i16, #function
	.proc	014
LT__uLT_param_C_i16:
.LLFB354:
	save	%sp, -96, %sp
.LLCFI354:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+68], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE354:
	.size	LT__uLT_param_C_i16, .-LT__uLT_param_C_i16
	.align 4
	.type	LT__LT_param_C_i16, #function
	.proc	014
LT__LT_param_C_i16:
.LLFB355:
	save	%sp, -96, %sp
.LLCFI355:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ld	[%fp+68], %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE355:
	.size	LT__LT_param_C_i16, .-LT__LT_param_C_i16
	.align 4
	.type	LT__uLT_var_C_I, #function
	.proc	014
LT__uLT_var_C_I:
.LLFB356:
	save	%sp, -96, %sp
.LLCFI356:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE356:
	.size	LT__uLT_var_C_I, .-LT__uLT_var_C_I
	.align 4
	.type	LT__LT_var_C_I, #function
	.proc	014
LT__LT_var_C_I:
.LLFB357:
	save	%sp, -96, %sp
.LLCFI357:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE357:
	.size	LT__LT_var_C_I, .-LT__LT_var_C_I
	.align 4
	.type	LT__uLT_param_C_I, #function
	.proc	014
LT__uLT_param_C_I:
.LLFB358:
	save	%sp, -96, %sp
.LLCFI358:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE358:
	.size	LT__uLT_param_C_I, .-LT__uLT_param_C_I
	.align 4
	.type	LT__LT_param_C_I, #function
	.proc	014
LT__LT_param_C_I:
.LLFB359:
	save	%sp, -96, %sp
.LLCFI359:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE359:
	.size	LT__LT_param_C_I, .-LT__LT_param_C_I
	.align 4
	.type	LT__uLT_var_C_i64, #function
	.proc	014
LT__uLT_var_C_i64:
.LLFB360:
	save	%sp, -96, %sp
.LLCFI360:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL782
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL783
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL782
	 nop
	cmp	%o3, %o5
.LL783:
	mov	0, %g1
.LL782:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE360:
	.size	LT__uLT_var_C_i64, .-LT__uLT_var_C_i64
	.align 4
	.type	LT__LT_var_C_i64, #function
	.proc	014
LT__LT_var_C_i64:
.LLFB361:
	save	%sp, -96, %sp
.LLCFI361:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL785
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL786
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL785
	 nop
	cmp	%o3, %o5
.LL786:
	mov	0, %g1
.LL785:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE361:
	.size	LT__LT_var_C_i64, .-LT__LT_var_C_i64
	.align 4
	.type	LT__uLT_param_C_i64, #function
	.proc	014
LT__uLT_param_C_i64:
.LLFB362:
	save	%sp, -104, %sp
.LLCFI362:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL788
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL789
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL788
	 nop
	cmp	%o3, %o5
.LL789:
	mov	0, %g1
.LL788:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE362:
	.size	LT__uLT_param_C_i64, .-LT__uLT_param_C_i64
	.align 4
	.type	LT__LT_param_C_i64, #function
	.proc	014
LT__LT_param_C_i64:
.LLFB363:
	save	%sp, -104, %sp
.LLCFI363:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bg	%icc, .LL791
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL792
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL791
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL792:
	mov	0, %g1
.LL791:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE363:
	.size	LT__LT_param_C_i64, .-LT__LT_param_C_i64
	.align 4
	.type	LT__uLT_var_C_C, #function
	.proc	014
LT__uLT_var_C_C:
.LLFB364:
	save	%sp, -96, %sp
.LLCFI364:
	mov	0, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE364:
	.size	LT__uLT_var_C_C, .-LT__uLT_var_C_C
	.align 4
	.type	LT__LT_var_C_C, #function
	.proc	014
LT__LT_var_C_C:
.LLFB365:
	save	%sp, -96, %sp
.LLCFI365:
	mov	0, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE365:
	.size	LT__LT_var_C_C, .-LT__LT_var_C_C
	.align 4
	.type	LT__uLT_param_C_C, #function
	.proc	014
LT__uLT_param_C_C:
.LLFB366:
	save	%sp, -96, %sp
.LLCFI366:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE366:
	.size	LT__uLT_param_C_C, .-LT__uLT_param_C_C
	.align 4
	.type	LT__LT_param_C_C, #function
	.proc	014
LT__LT_param_C_C:
.LLFB367:
	save	%sp, -96, %sp
.LLCFI367:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE367:
	.size	LT__LT_param_C_C, .-LT__LT_param_C_C
	.align 4
	.type	LT__uLT_var_C_u8, #function
	.proc	014
LT__uLT_var_C_u8:
.LLFB368:
	save	%sp, -96, %sp
.LLCFI368:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE368:
	.size	LT__uLT_var_C_u8, .-LT__uLT_var_C_u8
	.align 4
	.type	LT__LT_var_C_u8, #function
	.proc	014
LT__LT_var_C_u8:
.LLFB369:
	save	%sp, -96, %sp
.LLCFI369:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE369:
	.size	LT__LT_var_C_u8, .-LT__LT_var_C_u8
	.align 4
	.type	LT__uLT_param_C_u8, #function
	.proc	014
LT__uLT_param_C_u8:
.LLFB370:
	save	%sp, -96, %sp
.LLCFI370:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+68], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE370:
	.size	LT__uLT_param_C_u8, .-LT__uLT_param_C_u8
	.align 4
	.type	LT__LT_param_C_u8, #function
	.proc	014
LT__LT_param_C_u8:
.LLFB371:
	save	%sp, -96, %sp
.LLCFI371:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE371:
	.size	LT__LT_param_C_u8, .-LT__LT_param_C_u8
	.align 4
	.type	LT__uLT_var_C_L, #function
	.proc	014
LT__uLT_var_C_L:
.LLFB372:
	save	%sp, -96, %sp
.LLCFI372:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL802
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL803
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL802
	 nop
	cmp	%o3, %o5
.LL803:
	mov	0, %g1
.LL802:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE372:
	.size	LT__uLT_var_C_L, .-LT__uLT_var_C_L
	.align 4
	.type	LT__LT_var_C_L, #function
	.proc	014
LT__LT_var_C_L:
.LLFB373:
	save	%sp, -96, %sp
.LLCFI373:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL805
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL806
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL805
	 nop
	cmp	%o3, %o5
.LL806:
	mov	0, %g1
.LL805:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE373:
	.size	LT__LT_var_C_L, .-LT__LT_var_C_L
	.align 4
	.type	LT__uLT_param_C_L, #function
	.proc	014
LT__uLT_param_C_L:
.LLFB374:
	save	%sp, -104, %sp
.LLCFI374:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL808
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL809
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL808
	 nop
	cmp	%o3, %o5
.LL809:
	mov	0, %g1
.LL808:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE374:
	.size	LT__uLT_param_C_L, .-LT__uLT_param_C_L
	.align 4
	.type	LT__LT_param_C_L, #function
	.proc	014
LT__LT_param_C_L:
.LLFB375:
	save	%sp, -104, %sp
.LLCFI375:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bg	%icc, .LL811
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL812
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL811
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL812:
	mov	0, %g1
.LL811:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE375:
	.size	LT__LT_param_C_L, .-LT__LT_param_C_L
	.align 4
	.type	LT__uLT_var_C_i8, #function
	.proc	014
LT__uLT_var_C_i8:
.LLFB376:
	save	%sp, -96, %sp
.LLCFI376:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE376:
	.size	LT__uLT_var_C_i8, .-LT__uLT_var_C_i8
	.align 4
	.type	LT__LT_var_C_i8, #function
	.proc	014
LT__LT_var_C_i8:
.LLFB377:
	save	%sp, -96, %sp
.LLCFI377:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE377:
	.size	LT__LT_var_C_i8, .-LT__LT_var_C_i8
	.align 4
	.type	LT__uLT_param_C_i8, #function
	.proc	014
LT__uLT_param_C_i8:
.LLFB378:
	save	%sp, -96, %sp
.LLCFI378:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+68], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE378:
	.size	LT__uLT_param_C_i8, .-LT__uLT_param_C_i8
	.align 4
	.type	LT__LT_param_C_i8, #function
	.proc	014
LT__LT_param_C_i8:
.LLFB379:
	save	%sp, -96, %sp
.LLCFI379:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE379:
	.size	LT__LT_param_C_i8, .-LT__LT_param_C_i8
	.align 4
	.type	LT__uLT_var_C_u32, #function
	.proc	014
LT__uLT_var_C_u32:
.LLFB380:
	save	%sp, -96, %sp
.LLCFI380:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE380:
	.size	LT__uLT_var_C_u32, .-LT__uLT_var_C_u32
	.align 4
	.type	LT__LT_var_C_u32, #function
	.proc	014
LT__LT_var_C_u32:
.LLFB381:
	save	%sp, -96, %sp
.LLCFI381:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE381:
	.size	LT__LT_var_C_u32, .-LT__LT_var_C_u32
	.align 4
	.type	LT__uLT_param_C_u32, #function
	.proc	014
LT__uLT_param_C_u32:
.LLFB382:
	save	%sp, -96, %sp
.LLCFI382:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE382:
	.size	LT__uLT_param_C_u32, .-LT__uLT_param_C_u32
	.align 4
	.type	LT__LT_param_C_u32, #function
	.proc	014
LT__LT_param_C_u32:
.LLFB383:
	save	%sp, -96, %sp
.LLCFI383:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE383:
	.size	LT__LT_param_C_u32, .-LT__LT_param_C_u32
	.align 4
	.type	LT__LT_var_f64_f64, #function
	.proc	014
LT__LT_var_f64_f64:
.LLFB384:
	save	%sp, -96, %sp
.LLCFI384:
	mov	0, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE384:
	.size	LT__LT_var_f64_f64, .-LT__LT_var_f64_f64
	.align 4
	.type	LT__LT_param_f64_f64, #function
	.proc	014
LT__LT_param_f64_f64:
.LLFB385:
	save	%sp, -112, %sp
.LLCFI385:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %f10
	ldd	[%fp-16], %f8
	fcmped	%fcc0, %f10, %f8
	mov	0, %g1
	movl	%fcc0, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE385:
	.size	LT__LT_param_f64_f64, .-LT__LT_param_f64_f64
	.align 4
	.type	LT__uLT_var_u8_u16, #function
	.proc	014
LT__uLT_var_u8_u16:
.LLFB386:
	save	%sp, -96, %sp
.LLCFI386:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE386:
	.size	LT__uLT_var_u8_u16, .-LT__uLT_var_u8_u16
	.align 4
	.type	LT__LT_var_u8_u16, #function
	.proc	014
LT__LT_var_u8_u16:
.LLFB387:
	save	%sp, -96, %sp
.LLCFI387:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE387:
	.size	LT__LT_var_u8_u16, .-LT__LT_var_u8_u16
	.align 4
	.type	LT__uLT_param_u8_u16, #function
	.proc	014
LT__uLT_param_u8_u16:
.LLFB388:
	save	%sp, -96, %sp
.LLCFI388:
	stb	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE388:
	.size	LT__uLT_param_u8_u16, .-LT__uLT_param_u8_u16
	.align 4
	.type	LT__LT_param_u8_u16, #function
	.proc	014
LT__LT_param_u8_u16:
.LLFB389:
	save	%sp, -96, %sp
.LLCFI389:
	stb	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE389:
	.size	LT__LT_param_u8_u16, .-LT__LT_param_u8_u16
	.align 4
	.type	LT__uLT_var_u8_u64, #function
	.proc	014
LT__uLT_var_u8_u64:
.LLFB390:
	save	%sp, -96, %sp
.LLCFI390:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL828
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL829
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL828
	 nop
	cmp	%o3, %o5
.LL829:
	mov	0, %g1
.LL828:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE390:
	.size	LT__uLT_var_u8_u64, .-LT__uLT_var_u8_u64
	.align 4
	.type	LT__LT_var_u8_u64, #function
	.proc	014
LT__LT_var_u8_u64:
.LLFB391:
	save	%sp, -96, %sp
.LLCFI391:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL831
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL832
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL831
	 nop
	cmp	%o3, %o5
.LL832:
	mov	0, %g1
.LL831:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE391:
	.size	LT__LT_var_u8_u64, .-LT__LT_var_u8_u64
	.align 4
	.type	LT__uLT_param_u8_u64, #function
	.proc	014
LT__uLT_param_u8_u64:
.LLFB392:
	save	%sp, -104, %sp
.LLCFI392:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL834
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL835
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL834
	 nop
	cmp	%o3, %o5
.LL835:
	mov	0, %g1
.LL834:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE392:
	.size	LT__uLT_param_u8_u64, .-LT__uLT_param_u8_u64
	.align 4
	.type	LT__LT_param_u8_u64, #function
	.proc	014
LT__LT_param_u8_u64:
.LLFB393:
	save	%sp, -104, %sp
.LLCFI393:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bg	%icc, .LL837
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL838
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL837
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL838:
	mov	0, %g1
.LL837:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE393:
	.size	LT__LT_param_u8_u64, .-LT__LT_param_u8_u64
	.align 4
	.type	LT__uLT_var_u8_LC, #function
	.proc	014
LT__uLT_var_u8_LC:
.LLFB394:
	save	%sp, -96, %sp
.LLCFI394:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL840
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL841
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL840
	 nop
	cmp	%o3, %o5
.LL841:
	mov	0, %g1
.LL840:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE394:
	.size	LT__uLT_var_u8_LC, .-LT__uLT_var_u8_LC
	.align 4
	.type	LT__LT_var_u8_LC, #function
	.proc	014
LT__LT_var_u8_LC:
.LLFB395:
	save	%sp, -96, %sp
.LLCFI395:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL843
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL844
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL843
	 nop
	cmp	%o3, %o5
.LL844:
	mov	0, %g1
.LL843:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE395:
	.size	LT__LT_var_u8_LC, .-LT__LT_var_u8_LC
	.align 4
	.type	LT__uLT_param_u8_LC, #function
	.proc	014
LT__uLT_param_u8_LC:
.LLFB396:
	save	%sp, -104, %sp
.LLCFI396:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bgu	%icc, .LL846
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL847
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL846
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL847:
	mov	0, %g1
.LL846:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE396:
	.size	LT__uLT_param_u8_LC, .-LT__uLT_param_u8_LC
	.align 4
	.type	LT__LT_param_u8_LC, #function
	.proc	014
LT__LT_param_u8_LC:
.LLFB397:
	save	%sp, -104, %sp
.LLCFI397:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL849
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL850
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL849
	 nop
	cmp	%o3, %o5
.LL850:
	mov	0, %g1
.LL849:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE397:
	.size	LT__LT_param_u8_LC, .-LT__LT_param_u8_LC
	.align 4
	.type	LT__uLT_var_u8_i32, #function
	.proc	014
LT__uLT_var_u8_i32:
.LLFB398:
	save	%sp, -96, %sp
.LLCFI398:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE398:
	.size	LT__uLT_var_u8_i32, .-LT__uLT_var_u8_i32
	.align 4
	.type	LT__LT_var_u8_i32, #function
	.proc	014
LT__LT_var_u8_i32:
.LLFB399:
	save	%sp, -96, %sp
.LLCFI399:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE399:
	.size	LT__LT_var_u8_i32, .-LT__LT_var_u8_i32
	.align 4
	.type	LT__uLT_param_u8_i32, #function
	.proc	014
LT__uLT_param_u8_i32:
.LLFB400:
	save	%sp, -96, %sp
.LLCFI400:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE400:
	.size	LT__uLT_param_u8_i32, .-LT__uLT_param_u8_i32
	.align 4
	.type	LT__LT_param_u8_i32, #function
	.proc	014
LT__LT_param_u8_i32:
.LLFB401:
	save	%sp, -96, %sp
.LLCFI401:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE401:
	.size	LT__LT_param_u8_i32, .-LT__LT_param_u8_i32
	.align 4
	.type	LT__uLT_var_u8_i16, #function
	.proc	014
LT__uLT_var_u8_i16:
.LLFB402:
	save	%sp, -96, %sp
.LLCFI402:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE402:
	.size	LT__uLT_var_u8_i16, .-LT__uLT_var_u8_i16
	.align 4
	.type	LT__LT_var_u8_i16, #function
	.proc	014
LT__LT_var_u8_i16:
.LLFB403:
	save	%sp, -96, %sp
.LLCFI403:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE403:
	.size	LT__LT_var_u8_i16, .-LT__LT_var_u8_i16
	.align 4
	.type	LT__uLT_param_u8_i16, #function
	.proc	014
LT__uLT_param_u8_i16:
.LLFB404:
	save	%sp, -96, %sp
.LLCFI404:
	stb	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE404:
	.size	LT__uLT_param_u8_i16, .-LT__uLT_param_u8_i16
	.align 4
	.type	LT__LT_param_u8_i16, #function
	.proc	014
LT__LT_param_u8_i16:
.LLFB405:
	save	%sp, -96, %sp
.LLCFI405:
	stb	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE405:
	.size	LT__LT_param_u8_i16, .-LT__LT_param_u8_i16
	.align 4
	.type	LT__uLT_var_u8_I, #function
	.proc	014
LT__uLT_var_u8_I:
.LLFB406:
	save	%sp, -96, %sp
.LLCFI406:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE406:
	.size	LT__uLT_var_u8_I, .-LT__uLT_var_u8_I
	.align 4
	.type	LT__LT_var_u8_I, #function
	.proc	014
LT__LT_var_u8_I:
.LLFB407:
	save	%sp, -96, %sp
.LLCFI407:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE407:
	.size	LT__LT_var_u8_I, .-LT__LT_var_u8_I
	.align 4
	.type	LT__uLT_param_u8_I, #function
	.proc	014
LT__uLT_param_u8_I:
.LLFB408:
	save	%sp, -96, %sp
.LLCFI408:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE408:
	.size	LT__uLT_param_u8_I, .-LT__uLT_param_u8_I
	.align 4
	.type	LT__LT_param_u8_I, #function
	.proc	014
LT__LT_param_u8_I:
.LLFB409:
	save	%sp, -96, %sp
.LLCFI409:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE409:
	.size	LT__LT_param_u8_I, .-LT__LT_param_u8_I
	.align 4
	.type	LT__uLT_var_u8_i64, #function
	.proc	014
LT__uLT_var_u8_i64:
.LLFB410:
	save	%sp, -96, %sp
.LLCFI410:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL864
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL865
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL864
	 nop
	cmp	%o3, %o5
.LL865:
	mov	0, %g1
.LL864:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE410:
	.size	LT__uLT_var_u8_i64, .-LT__uLT_var_u8_i64
	.align 4
	.type	LT__LT_var_u8_i64, #function
	.proc	014
LT__LT_var_u8_i64:
.LLFB411:
	save	%sp, -96, %sp
.LLCFI411:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL867
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL868
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL867
	 nop
	cmp	%o3, %o5
.LL868:
	mov	0, %g1
.LL867:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE411:
	.size	LT__LT_var_u8_i64, .-LT__LT_var_u8_i64
	.align 4
	.type	LT__uLT_param_u8_i64, #function
	.proc	014
LT__uLT_param_u8_i64:
.LLFB412:
	save	%sp, -104, %sp
.LLCFI412:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL870
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL871
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL870
	 nop
	cmp	%o3, %o5
.LL871:
	mov	0, %g1
.LL870:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE412:
	.size	LT__uLT_param_u8_i64, .-LT__uLT_param_u8_i64
	.align 4
	.type	LT__LT_param_u8_i64, #function
	.proc	014
LT__LT_param_u8_i64:
.LLFB413:
	save	%sp, -104, %sp
.LLCFI413:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bg	%icc, .LL873
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL874
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL873
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL874:
	mov	0, %g1
.LL873:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE413:
	.size	LT__LT_param_u8_i64, .-LT__LT_param_u8_i64
	.align 4
	.type	LT__uLT_var_u8_C, #function
	.proc	014
LT__uLT_var_u8_C:
.LLFB414:
	save	%sp, -96, %sp
.LLCFI414:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE414:
	.size	LT__uLT_var_u8_C, .-LT__uLT_var_u8_C
	.align 4
	.type	LT__LT_var_u8_C, #function
	.proc	014
LT__LT_var_u8_C:
.LLFB415:
	save	%sp, -96, %sp
.LLCFI415:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE415:
	.size	LT__LT_var_u8_C, .-LT__LT_var_u8_C
	.align 4
	.type	LT__uLT_param_u8_C, #function
	.proc	014
LT__uLT_param_u8_C:
.LLFB416:
	save	%sp, -96, %sp
.LLCFI416:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE416:
	.size	LT__uLT_param_u8_C, .-LT__uLT_param_u8_C
	.align 4
	.type	LT__LT_param_u8_C, #function
	.proc	014
LT__LT_param_u8_C:
.LLFB417:
	save	%sp, -96, %sp
.LLCFI417:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE417:
	.size	LT__LT_param_u8_C, .-LT__LT_param_u8_C
	.align 4
	.type	LT__uLT_var_u8_u8, #function
	.proc	014
LT__uLT_var_u8_u8:
.LLFB418:
	save	%sp, -96, %sp
.LLCFI418:
	mov	0, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE418:
	.size	LT__uLT_var_u8_u8, .-LT__uLT_var_u8_u8
	.align 4
	.type	LT__LT_var_u8_u8, #function
	.proc	014
LT__LT_var_u8_u8:
.LLFB419:
	save	%sp, -96, %sp
.LLCFI419:
	mov	0, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE419:
	.size	LT__LT_var_u8_u8, .-LT__LT_var_u8_u8
	.align 4
	.type	LT__uLT_param_u8_u8, #function
	.proc	014
LT__uLT_param_u8_u8:
.LLFB420:
	save	%sp, -96, %sp
.LLCFI420:
	stb	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE420:
	.size	LT__uLT_param_u8_u8, .-LT__uLT_param_u8_u8
	.align 4
	.type	LT__LT_param_u8_u8, #function
	.proc	014
LT__LT_param_u8_u8:
.LLFB421:
	save	%sp, -96, %sp
.LLCFI421:
	stb	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE421:
	.size	LT__LT_param_u8_u8, .-LT__LT_param_u8_u8
	.align 4
	.type	LT__uLT_var_u8_L, #function
	.proc	014
LT__uLT_var_u8_L:
.LLFB422:
	save	%sp, -96, %sp
.LLCFI422:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL884
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL885
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL884
	 nop
	cmp	%o3, %o5
.LL885:
	mov	0, %g1
.LL884:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE422:
	.size	LT__uLT_var_u8_L, .-LT__uLT_var_u8_L
	.align 4
	.type	LT__LT_var_u8_L, #function
	.proc	014
LT__LT_var_u8_L:
.LLFB423:
	save	%sp, -96, %sp
.LLCFI423:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL887
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL888
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL887
	 nop
	cmp	%o3, %o5
.LL888:
	mov	0, %g1
.LL887:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE423:
	.size	LT__LT_var_u8_L, .-LT__LT_var_u8_L
	.align 4
	.type	LT__uLT_param_u8_L, #function
	.proc	014
LT__uLT_param_u8_L:
.LLFB424:
	save	%sp, -104, %sp
.LLCFI424:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL890
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL891
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL890
	 nop
	cmp	%o3, %o5
.LL891:
	mov	0, %g1
.LL890:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE424:
	.size	LT__uLT_param_u8_L, .-LT__uLT_param_u8_L
	.align 4
	.type	LT__LT_param_u8_L, #function
	.proc	014
LT__LT_param_u8_L:
.LLFB425:
	save	%sp, -104, %sp
.LLCFI425:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bg	%icc, .LL893
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL894
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL893
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL894:
	mov	0, %g1
.LL893:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE425:
	.size	LT__LT_param_u8_L, .-LT__LT_param_u8_L
	.align 4
	.type	LT__uLT_var_u8_i8, #function
	.proc	014
LT__uLT_var_u8_i8:
.LLFB426:
	save	%sp, -96, %sp
.LLCFI426:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE426:
	.size	LT__uLT_var_u8_i8, .-LT__uLT_var_u8_i8
	.align 4
	.type	LT__LT_var_u8_i8, #function
	.proc	014
LT__LT_var_u8_i8:
.LLFB427:
	save	%sp, -96, %sp
.LLCFI427:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE427:
	.size	LT__LT_var_u8_i8, .-LT__LT_var_u8_i8
	.align 4
	.type	LT__uLT_param_u8_i8, #function
	.proc	014
LT__uLT_param_u8_i8:
.LLFB428:
	save	%sp, -96, %sp
.LLCFI428:
	stb	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE428:
	.size	LT__uLT_param_u8_i8, .-LT__uLT_param_u8_i8
	.align 4
	.type	LT__LT_param_u8_i8, #function
	.proc	014
LT__LT_param_u8_i8:
.LLFB429:
	save	%sp, -96, %sp
.LLCFI429:
	stb	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE429:
	.size	LT__LT_param_u8_i8, .-LT__LT_param_u8_i8
	.align 4
	.type	LT__uLT_var_u8_u32, #function
	.proc	014
LT__uLT_var_u8_u32:
.LLFB430:
	save	%sp, -96, %sp
.LLCFI430:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE430:
	.size	LT__uLT_var_u8_u32, .-LT__uLT_var_u8_u32
	.align 4
	.type	LT__LT_var_u8_u32, #function
	.proc	014
LT__LT_var_u8_u32:
.LLFB431:
	save	%sp, -96, %sp
.LLCFI431:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE431:
	.size	LT__LT_var_u8_u32, .-LT__LT_var_u8_u32
	.align 4
	.type	LT__uLT_param_u8_u32, #function
	.proc	014
LT__uLT_param_u8_u32:
.LLFB432:
	save	%sp, -96, %sp
.LLCFI432:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE432:
	.size	LT__uLT_param_u8_u32, .-LT__uLT_param_u8_u32
	.align 4
	.type	LT__LT_param_u8_u32, #function
	.proc	014
LT__LT_param_u8_u32:
.LLFB433:
	save	%sp, -96, %sp
.LLCFI433:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE433:
	.size	LT__LT_param_u8_u32, .-LT__LT_param_u8_u32
	.align 4
	.type	LT__uLT_var_L_u16, #function
	.proc	014
LT__uLT_var_L_u16:
.LLFB434:
	save	%sp, -96, %sp
.LLCFI434:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL904
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL905
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL904
	 nop
	cmp	%o3, %o5
.LL905:
	mov	0, %g1
.LL904:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE434:
	.size	LT__uLT_var_L_u16, .-LT__uLT_var_L_u16
	.align 4
	.type	LT__LT_var_L_u16, #function
	.proc	014
LT__LT_var_L_u16:
.LLFB435:
	save	%sp, -96, %sp
.LLCFI435:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL907
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL908
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL907
	 nop
	cmp	%o3, %o5
.LL908:
	mov	0, %g1
.LL907:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE435:
	.size	LT__LT_var_L_u16, .-LT__LT_var_L_u16
	.align 4
	.type	LT__uLT_param_L_u16, #function
	.proc	014
LT__uLT_param_L_u16:
.LLFB436:
	save	%sp, -104, %sp
.LLCFI436:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL910
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL911
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL910
	 nop
	cmp	%o3, %o5
.LL911:
	mov	0, %g1
.LL910:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE436:
	.size	LT__uLT_param_L_u16, .-LT__uLT_param_L_u16
	.align 4
	.type	LT__LT_param_L_u16, #function
	.proc	014
LT__LT_param_L_u16:
.LLFB437:
	save	%sp, -104, %sp
.LLCFI437:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bl	%icc, .LL913
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL914
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL913
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL914:
	mov	0, %g1
.LL913:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE437:
	.size	LT__LT_param_L_u16, .-LT__LT_param_L_u16
	.align 4
	.type	LT__uLT_var_L_u64, #function
	.proc	014
LT__uLT_var_L_u64:
.LLFB438:
	save	%sp, -96, %sp
.LLCFI438:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL916
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL917
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL916
	 nop
	cmp	%o3, %o5
.LL917:
	mov	0, %g1
.LL916:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE438:
	.size	LT__uLT_var_L_u64, .-LT__uLT_var_L_u64
	.align 4
	.type	LT__LT_var_L_u64, #function
	.proc	014
LT__LT_var_L_u64:
.LLFB439:
	save	%sp, -96, %sp
.LLCFI439:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL919
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL920
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL919
	 nop
	cmp	%o3, %o5
.LL920:
	mov	0, %g1
.LL919:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE439:
	.size	LT__LT_var_L_u64, .-LT__LT_var_L_u64
	.align 4
	.type	LT__uLT_param_L_u64, #function
	.proc	014
LT__uLT_param_L_u64:
.LLFB440:
	save	%sp, -112, %sp
.LLCFI440:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	ldd	[%fp-16], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL922
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL923
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL922
	 nop
	cmp	%o3, %o5
.LL923:
	mov	0, %g1
.LL922:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE440:
	.size	LT__uLT_param_L_u64, .-LT__uLT_param_L_u64
	.align 4
	.type	LT__LT_param_L_u64, #function
	.proc	014
LT__LT_param_L_u64:
.LLFB441:
	save	%sp, -112, %sp
.LLCFI441:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	mov	1, %g1
	ld	[%fp-16], %o4
	ld	[%fp-8], %o5
	cmp	%o4, %o5
	bg	%icc, .LL925
	 nop
	ld	[%fp-16], %o4
	ld	[%fp-8], %o5
	cmp	%o4, %o5
	bne	%icc, .LL926
	 nop
	ld	[%fp-12], %o4
	ld	[%fp-4], %o5
	cmp	%o4, %o5
	bgu	%icc, .LL925
	 nop
	ld	[%fp-12], %o5
	ld	[%fp-4], %g1
	cmp	%o5, %g1
.LL926:
	mov	0, %g1
.LL925:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE441:
	.size	LT__LT_param_L_u64, .-LT__LT_param_L_u64
	.align 4
	.type	LT__uLT_var_L_LC, #function
	.proc	014
LT__uLT_var_L_LC:
.LLFB442:
	save	%sp, -96, %sp
.LLCFI442:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL928
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL929
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL928
	 nop
	cmp	%o3, %o5
.LL929:
	mov	0, %g1
.LL928:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE442:
	.size	LT__uLT_var_L_LC, .-LT__uLT_var_L_LC
	.align 4
	.type	LT__LT_var_L_LC, #function
	.proc	014
LT__LT_var_L_LC:
.LLFB443:
	save	%sp, -96, %sp
.LLCFI443:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL931
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL932
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL931
	 nop
	cmp	%o3, %o5
.LL932:
	mov	0, %g1
.LL931:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE443:
	.size	LT__LT_var_L_LC, .-LT__LT_var_L_LC
	.align 4
	.type	LT__uLT_param_L_LC, #function
	.proc	014
LT__uLT_param_L_LC:
.LLFB444:
	save	%sp, -112, %sp
.LLCFI444:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	mov	1, %g1
	ld	[%fp-16], %o3
	cmp	%o3, %o4
	bgu	%icc, .LL934
	 nop
	ld	[%fp-16], %o3
	cmp	%o3, %o4
	bne	%icc, .LL935
	 nop
	ld	[%fp-12], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL934
	 nop
	ld	[%fp-12], %g1
	cmp	%g1, %o5
.LL935:
	mov	0, %g1
.LL934:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE444:
	.size	LT__uLT_param_L_LC, .-LT__uLT_param_L_LC
	.align 4
	.type	LT__LT_param_L_LC, #function
	.proc	014
LT__LT_param_L_LC:
.LLFB445:
	save	%sp, -112, %sp
.LLCFI445:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bl	%icc, .LL937
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL938
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL937
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL938:
	mov	0, %g1
.LL937:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE445:
	.size	LT__LT_param_L_LC, .-LT__LT_param_L_LC
	.align 4
	.type	LT__uLT_var_L_i32, #function
	.proc	014
LT__uLT_var_L_i32:
.LLFB446:
	save	%sp, -96, %sp
.LLCFI446:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL940
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL941
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL940
	 nop
	cmp	%o5, %o3
.LL941:
	mov	0, %g1
.LL940:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE446:
	.size	LT__uLT_var_L_i32, .-LT__uLT_var_L_i32
	.align 4
	.type	LT__LT_var_L_i32, #function
	.proc	014
LT__LT_var_L_i32:
.LLFB447:
	save	%sp, -96, %sp
.LLCFI447:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL943
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL944
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL943
	 nop
	cmp	%o5, %o3
.LL944:
	mov	0, %g1
.LL943:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE447:
	.size	LT__LT_var_L_i32, .-LT__LT_var_L_i32
	.align 4
	.type	LT__uLT_param_L_i32, #function
	.proc	014
LT__uLT_param_L_i32:
.LLFB448:
	save	%sp, -104, %sp
.LLCFI448:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL946
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL947
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL946
	 nop
	cmp	%o5, %o3
.LL947:
	mov	0, %g1
.LL946:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE448:
	.size	LT__uLT_param_L_i32, .-LT__uLT_param_L_i32
	.align 4
	.type	LT__LT_param_L_i32, #function
	.proc	014
LT__LT_param_L_i32:
.LLFB449:
	save	%sp, -104, %sp
.LLCFI449:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bl	%icc, .LL949
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL950
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL949
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL950:
	mov	0, %g1
.LL949:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE449:
	.size	LT__LT_param_L_i32, .-LT__LT_param_L_i32
	.align 4
	.type	LT__uLT_var_L_i16, #function
	.proc	014
LT__uLT_var_L_i16:
.LLFB450:
	save	%sp, -96, %sp
.LLCFI450:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL952
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL953
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL952
	 nop
	cmp	%o5, %o3
.LL953:
	mov	0, %g1
.LL952:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE450:
	.size	LT__uLT_var_L_i16, .-LT__uLT_var_L_i16
	.align 4
	.type	LT__LT_var_L_i16, #function
	.proc	014
LT__LT_var_L_i16:
.LLFB451:
	save	%sp, -96, %sp
.LLCFI451:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL955
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL956
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL955
	 nop
	cmp	%o5, %o3
.LL956:
	mov	0, %g1
.LL955:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE451:
	.size	LT__LT_var_L_i16, .-LT__LT_var_L_i16
	.align 4
	.type	LT__uLT_param_L_i16, #function
	.proc	014
LT__uLT_param_L_i16:
.LLFB452:
	save	%sp, -104, %sp
.LLCFI452:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL958
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL959
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL958
	 nop
	cmp	%o5, %o3
.LL959:
	mov	0, %g1
.LL958:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE452:
	.size	LT__uLT_param_L_i16, .-LT__uLT_param_L_i16
	.align 4
	.type	LT__LT_param_L_i16, #function
	.proc	014
LT__LT_param_L_i16:
.LLFB453:
	save	%sp, -104, %sp
.LLCFI453:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bl	%icc, .LL961
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL962
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL961
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL962:
	mov	0, %g1
.LL961:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE453:
	.size	LT__LT_param_L_i16, .-LT__LT_param_L_i16
	.align 4
	.type	LT__uLT_var_L_I, #function
	.proc	014
LT__uLT_var_L_I:
.LLFB454:
	save	%sp, -96, %sp
.LLCFI454:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL964
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL965
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL964
	 nop
	cmp	%o5, %o3
.LL965:
	mov	0, %g1
.LL964:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE454:
	.size	LT__uLT_var_L_I, .-LT__uLT_var_L_I
	.align 4
	.type	LT__LT_var_L_I, #function
	.proc	014
LT__LT_var_L_I:
.LLFB455:
	save	%sp, -96, %sp
.LLCFI455:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL967
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL968
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL967
	 nop
	cmp	%o5, %o3
.LL968:
	mov	0, %g1
.LL967:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE455:
	.size	LT__LT_var_L_I, .-LT__LT_var_L_I
	.align 4
	.type	LT__uLT_param_L_I, #function
	.proc	014
LT__uLT_param_L_I:
.LLFB456:
	save	%sp, -104, %sp
.LLCFI456:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL970
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL971
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL970
	 nop
	cmp	%o5, %o3
.LL971:
	mov	0, %g1
.LL970:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE456:
	.size	LT__uLT_param_L_I, .-LT__uLT_param_L_I
	.align 4
	.type	LT__LT_param_L_I, #function
	.proc	014
LT__LT_param_L_I:
.LLFB457:
	save	%sp, -104, %sp
.LLCFI457:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bl	%icc, .LL973
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL974
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL973
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL974:
	mov	0, %g1
.LL973:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE457:
	.size	LT__LT_param_L_I, .-LT__LT_param_L_I
	.align 4
	.type	LT__uLT_var_L_i64, #function
	.proc	014
LT__uLT_var_L_i64:
.LLFB458:
	save	%sp, -96, %sp
.LLCFI458:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL976
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL977
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL976
	 nop
	cmp	%o3, %o5
.LL977:
	mov	0, %g1
.LL976:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE458:
	.size	LT__uLT_var_L_i64, .-LT__uLT_var_L_i64
	.align 4
	.type	LT__LT_var_L_i64, #function
	.proc	014
LT__LT_var_L_i64:
.LLFB459:
	save	%sp, -96, %sp
.LLCFI459:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL979
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL980
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL979
	 nop
	cmp	%o3, %o5
.LL980:
	mov	0, %g1
.LL979:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE459:
	.size	LT__LT_var_L_i64, .-LT__LT_var_L_i64
	.align 4
	.type	LT__uLT_param_L_i64, #function
	.proc	014
LT__uLT_param_L_i64:
.LLFB460:
	save	%sp, -112, %sp
.LLCFI460:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	ldd	[%fp-16], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL982
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL983
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL982
	 nop
	cmp	%o3, %o5
.LL983:
	mov	0, %g1
.LL982:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE460:
	.size	LT__uLT_param_L_i64, .-LT__uLT_param_L_i64
	.align 4
	.type	LT__LT_param_L_i64, #function
	.proc	014
LT__LT_param_L_i64:
.LLFB461:
	save	%sp, -112, %sp
.LLCFI461:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	mov	1, %g1
	ld	[%fp-16], %o4
	ld	[%fp-8], %o5
	cmp	%o4, %o5
	bg	%icc, .LL985
	 nop
	ld	[%fp-16], %o4
	ld	[%fp-8], %o5
	cmp	%o4, %o5
	bne	%icc, .LL986
	 nop
	ld	[%fp-12], %o4
	ld	[%fp-4], %o5
	cmp	%o4, %o5
	bgu	%icc, .LL985
	 nop
	ld	[%fp-12], %o5
	ld	[%fp-4], %g1
	cmp	%o5, %g1
.LL986:
	mov	0, %g1
.LL985:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE461:
	.size	LT__LT_param_L_i64, .-LT__LT_param_L_i64
	.align 4
	.type	LT__uLT_var_L_C, #function
	.proc	014
LT__uLT_var_L_C:
.LLFB462:
	save	%sp, -96, %sp
.LLCFI462:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL988
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL989
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL988
	 nop
	cmp	%o5, %o3
.LL989:
	mov	0, %g1
.LL988:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE462:
	.size	LT__uLT_var_L_C, .-LT__uLT_var_L_C
	.align 4
	.type	LT__LT_var_L_C, #function
	.proc	014
LT__LT_var_L_C:
.LLFB463:
	save	%sp, -96, %sp
.LLCFI463:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL991
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL992
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL991
	 nop
	cmp	%o5, %o3
.LL992:
	mov	0, %g1
.LL991:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE463:
	.size	LT__LT_var_L_C, .-LT__LT_var_L_C
	.align 4
	.type	LT__uLT_param_L_C, #function
	.proc	014
LT__uLT_param_L_C:
.LLFB464:
	save	%sp, -104, %sp
.LLCFI464:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL994
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL995
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL994
	 nop
	cmp	%o5, %o3
.LL995:
	mov	0, %g1
.LL994:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE464:
	.size	LT__uLT_param_L_C, .-LT__uLT_param_L_C
	.align 4
	.type	LT__LT_param_L_C, #function
	.proc	014
LT__LT_param_L_C:
.LLFB465:
	save	%sp, -104, %sp
.LLCFI465:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bl	%icc, .LL997
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL998
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL997
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL998:
	mov	0, %g1
.LL997:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE465:
	.size	LT__LT_param_L_C, .-LT__LT_param_L_C
	.align 4
	.type	LT__uLT_var_L_u8, #function
	.proc	014
LT__uLT_var_L_u8:
.LLFB466:
	save	%sp, -96, %sp
.LLCFI466:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL1000
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL1001
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL1000
	 nop
	cmp	%o3, %o5
.LL1001:
	mov	0, %g1
.LL1000:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE466:
	.size	LT__uLT_var_L_u8, .-LT__uLT_var_L_u8
	.align 4
	.type	LT__LT_var_L_u8, #function
	.proc	014
LT__LT_var_L_u8:
.LLFB467:
	save	%sp, -96, %sp
.LLCFI467:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL1003
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL1004
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL1003
	 nop
	cmp	%o3, %o5
.LL1004:
	mov	0, %g1
.LL1003:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE467:
	.size	LT__LT_var_L_u8, .-LT__LT_var_L_u8
	.align 4
	.type	LT__uLT_param_L_u8, #function
	.proc	014
LT__uLT_param_L_u8:
.LLFB468:
	save	%sp, -104, %sp
.LLCFI468:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL1006
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL1007
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL1006
	 nop
	cmp	%o3, %o5
.LL1007:
	mov	0, %g1
.LL1006:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE468:
	.size	LT__uLT_param_L_u8, .-LT__uLT_param_L_u8
	.align 4
	.type	LT__LT_param_L_u8, #function
	.proc	014
LT__LT_param_L_u8:
.LLFB469:
	save	%sp, -104, %sp
.LLCFI469:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bl	%icc, .LL1009
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL1010
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL1009
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL1010:
	mov	0, %g1
.LL1009:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE469:
	.size	LT__LT_param_L_u8, .-LT__LT_param_L_u8
	.align 4
	.type	LT__uLT_var_L_L, #function
	.proc	014
LT__uLT_var_L_L:
.LLFB470:
	save	%sp, -96, %sp
.LLCFI470:
	mov	0, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE470:
	.size	LT__uLT_var_L_L, .-LT__uLT_var_L_L
	.align 4
	.type	LT__LT_var_L_L, #function
	.proc	014
LT__LT_var_L_L:
.LLFB471:
	save	%sp, -96, %sp
.LLCFI471:
	mov	0, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE471:
	.size	LT__LT_var_L_L, .-LT__LT_var_L_L
	.align 4
	.type	LT__uLT_param_L_L, #function
	.proc	014
LT__uLT_param_L_L:
.LLFB472:
	save	%sp, -112, %sp
.LLCFI472:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	ldd	[%fp-16], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL1014
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL1015
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL1014
	 nop
	cmp	%o3, %o5
.LL1015:
	mov	0, %g1
.LL1014:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE472:
	.size	LT__uLT_param_L_L, .-LT__uLT_param_L_L
	.align 4
	.type	LT__LT_param_L_L, #function
	.proc	014
LT__LT_param_L_L:
.LLFB473:
	save	%sp, -112, %sp
.LLCFI473:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	mov	1, %g1
	ld	[%fp-16], %o4
	ld	[%fp-8], %o5
	cmp	%o4, %o5
	bg	%icc, .LL1017
	 nop
	ld	[%fp-16], %o4
	ld	[%fp-8], %o5
	cmp	%o4, %o5
	bne	%icc, .LL1018
	 nop
	ld	[%fp-12], %o4
	ld	[%fp-4], %o5
	cmp	%o4, %o5
	bgu	%icc, .LL1017
	 nop
	ld	[%fp-12], %o5
	ld	[%fp-4], %g1
	cmp	%o5, %g1
.LL1018:
	mov	0, %g1
.LL1017:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE473:
	.size	LT__LT_param_L_L, .-LT__LT_param_L_L
	.align 4
	.type	LT__uLT_var_L_i8, #function
	.proc	014
LT__uLT_var_L_i8:
.LLFB474:
	save	%sp, -96, %sp
.LLCFI474:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL1020
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL1021
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL1020
	 nop
	cmp	%o5, %o3
.LL1021:
	mov	0, %g1
.LL1020:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE474:
	.size	LT__uLT_var_L_i8, .-LT__uLT_var_L_i8
	.align 4
	.type	LT__LT_var_L_i8, #function
	.proc	014
LT__LT_var_L_i8:
.LLFB475:
	save	%sp, -96, %sp
.LLCFI475:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL1023
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL1024
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL1023
	 nop
	cmp	%o5, %o3
.LL1024:
	mov	0, %g1
.LL1023:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE475:
	.size	LT__LT_var_L_i8, .-LT__LT_var_L_i8
	.align 4
	.type	LT__uLT_param_L_i8, #function
	.proc	014
LT__uLT_param_L_i8:
.LLFB476:
	save	%sp, -104, %sp
.LLCFI476:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL1026
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL1027
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL1026
	 nop
	cmp	%o5, %o3
.LL1027:
	mov	0, %g1
.LL1026:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE476:
	.size	LT__uLT_param_L_i8, .-LT__uLT_param_L_i8
	.align 4
	.type	LT__LT_param_L_i8, #function
	.proc	014
LT__LT_param_L_i8:
.LLFB477:
	save	%sp, -104, %sp
.LLCFI477:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bl	%icc, .LL1029
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL1030
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL1029
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL1030:
	mov	0, %g1
.LL1029:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE477:
	.size	LT__LT_param_L_i8, .-LT__LT_param_L_i8
	.align 4
	.type	LT__uLT_var_L_u32, #function
	.proc	014
LT__uLT_var_L_u32:
.LLFB478:
	save	%sp, -96, %sp
.LLCFI478:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL1032
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL1033
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL1032
	 nop
	cmp	%o5, %o3
.LL1033:
	mov	0, %g1
.LL1032:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE478:
	.size	LT__uLT_var_L_u32, .-LT__uLT_var_L_u32
	.align 4
	.type	LT__LT_var_L_u32, #function
	.proc	014
LT__LT_var_L_u32:
.LLFB479:
	save	%sp, -96, %sp
.LLCFI479:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL1035
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL1036
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL1035
	 nop
	cmp	%o5, %o3
.LL1036:
	mov	0, %g1
.LL1035:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE479:
	.size	LT__LT_var_L_u32, .-LT__LT_var_L_u32
	.align 4
	.type	LT__uLT_param_L_u32, #function
	.proc	014
LT__uLT_param_L_u32:
.LLFB480:
	save	%sp, -104, %sp
.LLCFI480:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL1038
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL1039
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL1038
	 nop
	cmp	%o5, %o3
.LL1039:
	mov	0, %g1
.LL1038:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE480:
	.size	LT__uLT_param_L_u32, .-LT__uLT_param_L_u32
	.align 4
	.type	LT__LT_param_L_u32, #function
	.proc	014
LT__LT_param_L_u32:
.LLFB481:
	save	%sp, -104, %sp
.LLCFI481:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bl	%icc, .LL1041
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL1042
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL1041
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL1042:
	mov	0, %g1
.LL1041:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE481:
	.size	LT__LT_param_L_u32, .-LT__LT_param_L_u32
	.align 4
	.type	LT__uLT_var_i8_u16, #function
	.proc	014
LT__uLT_var_i8_u16:
.LLFB482:
	save	%sp, -96, %sp
.LLCFI482:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE482:
	.size	LT__uLT_var_i8_u16, .-LT__uLT_var_i8_u16
	.align 4
	.type	LT__LT_var_i8_u16, #function
	.proc	014
LT__LT_var_i8_u16:
.LLFB483:
	save	%sp, -96, %sp
.LLCFI483:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE483:
	.size	LT__LT_var_i8_u16, .-LT__LT_var_i8_u16
	.align 4
	.type	LT__uLT_param_i8_u16, #function
	.proc	014
LT__uLT_param_i8_u16:
.LLFB484:
	save	%sp, -96, %sp
.LLCFI484:
	stb	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE484:
	.size	LT__uLT_param_i8_u16, .-LT__uLT_param_i8_u16
	.align 4
	.type	LT__LT_param_i8_u16, #function
	.proc	014
LT__LT_param_i8_u16:
.LLFB485:
	save	%sp, -96, %sp
.LLCFI485:
	stb	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE485:
	.size	LT__LT_param_i8_u16, .-LT__LT_param_i8_u16
	.align 4
	.type	LT__uLT_var_i8_u64, #function
	.proc	014
LT__uLT_var_i8_u64:
.LLFB486:
	save	%sp, -96, %sp
.LLCFI486:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL1048
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL1049
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL1048
	 nop
	cmp	%o3, %o5
.LL1049:
	mov	0, %g1
.LL1048:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE486:
	.size	LT__uLT_var_i8_u64, .-LT__uLT_var_i8_u64
	.align 4
	.type	LT__LT_var_i8_u64, #function
	.proc	014
LT__LT_var_i8_u64:
.LLFB487:
	save	%sp, -96, %sp
.LLCFI487:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL1051
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL1052
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL1051
	 nop
	cmp	%o3, %o5
.LL1052:
	mov	0, %g1
.LL1051:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE487:
	.size	LT__LT_var_i8_u64, .-LT__LT_var_i8_u64
	.align 4
	.type	LT__uLT_param_i8_u64, #function
	.proc	014
LT__uLT_param_i8_u64:
.LLFB488:
	save	%sp, -104, %sp
.LLCFI488:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL1054
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL1055
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL1054
	 nop
	cmp	%o3, %o5
.LL1055:
	mov	0, %g1
.LL1054:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE488:
	.size	LT__uLT_param_i8_u64, .-LT__uLT_param_i8_u64
	.align 4
	.type	LT__LT_param_i8_u64, #function
	.proc	014
LT__LT_param_i8_u64:
.LLFB489:
	save	%sp, -104, %sp
.LLCFI489:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bg	%icc, .LL1057
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL1058
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL1057
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL1058:
	mov	0, %g1
.LL1057:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE489:
	.size	LT__LT_param_i8_u64, .-LT__LT_param_i8_u64
	.align 4
	.type	LT__uLT_var_i8_LC, #function
	.proc	014
LT__uLT_var_i8_LC:
.LLFB490:
	save	%sp, -96, %sp
.LLCFI490:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL1060
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL1061
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL1060
	 nop
	cmp	%o3, %o5
.LL1061:
	mov	0, %g1
.LL1060:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE490:
	.size	LT__uLT_var_i8_LC, .-LT__uLT_var_i8_LC
	.align 4
	.type	LT__LT_var_i8_LC, #function
	.proc	014
LT__LT_var_i8_LC:
.LLFB491:
	save	%sp, -96, %sp
.LLCFI491:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL1063
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL1064
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL1063
	 nop
	cmp	%o3, %o5
.LL1064:
	mov	0, %g1
.LL1063:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE491:
	.size	LT__LT_var_i8_LC, .-LT__LT_var_i8_LC
	.align 4
	.type	LT__uLT_param_i8_LC, #function
	.proc	014
LT__uLT_param_i8_LC:
.LLFB492:
	save	%sp, -104, %sp
.LLCFI492:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bgu	%icc, .LL1066
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL1067
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL1066
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL1067:
	mov	0, %g1
.LL1066:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE492:
	.size	LT__uLT_param_i8_LC, .-LT__uLT_param_i8_LC
	.align 4
	.type	LT__LT_param_i8_LC, #function
	.proc	014
LT__LT_param_i8_LC:
.LLFB493:
	save	%sp, -104, %sp
.LLCFI493:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL1069
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL1070
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL1069
	 nop
	cmp	%o3, %o5
.LL1070:
	mov	0, %g1
.LL1069:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE493:
	.size	LT__LT_param_i8_LC, .-LT__LT_param_i8_LC
	.align 4
	.type	LT__uLT_var_i8_i32, #function
	.proc	014
LT__uLT_var_i8_i32:
.LLFB494:
	save	%sp, -96, %sp
.LLCFI494:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE494:
	.size	LT__uLT_var_i8_i32, .-LT__uLT_var_i8_i32
	.align 4
	.type	LT__LT_var_i8_i32, #function
	.proc	014
LT__LT_var_i8_i32:
.LLFB495:
	save	%sp, -96, %sp
.LLCFI495:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE495:
	.size	LT__LT_var_i8_i32, .-LT__LT_var_i8_i32
	.align 4
	.type	LT__uLT_param_i8_i32, #function
	.proc	014
LT__uLT_param_i8_i32:
.LLFB496:
	save	%sp, -96, %sp
.LLCFI496:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE496:
	.size	LT__uLT_param_i8_i32, .-LT__uLT_param_i8_i32
	.align 4
	.type	LT__LT_param_i8_i32, #function
	.proc	014
LT__LT_param_i8_i32:
.LLFB497:
	save	%sp, -96, %sp
.LLCFI497:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE497:
	.size	LT__LT_param_i8_i32, .-LT__LT_param_i8_i32
	.align 4
	.type	LT__uLT_var_i8_i16, #function
	.proc	014
LT__uLT_var_i8_i16:
.LLFB498:
	save	%sp, -96, %sp
.LLCFI498:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE498:
	.size	LT__uLT_var_i8_i16, .-LT__uLT_var_i8_i16
	.align 4
	.type	LT__LT_var_i8_i16, #function
	.proc	014
LT__LT_var_i8_i16:
.LLFB499:
	save	%sp, -96, %sp
.LLCFI499:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE499:
	.size	LT__LT_var_i8_i16, .-LT__LT_var_i8_i16
	.align 4
	.type	LT__uLT_param_i8_i16, #function
	.proc	014
LT__uLT_param_i8_i16:
.LLFB500:
	save	%sp, -96, %sp
.LLCFI500:
	stb	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE500:
	.size	LT__uLT_param_i8_i16, .-LT__uLT_param_i8_i16
	.align 4
	.type	LT__LT_param_i8_i16, #function
	.proc	014
LT__LT_param_i8_i16:
.LLFB501:
	save	%sp, -96, %sp
.LLCFI501:
	stb	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE501:
	.size	LT__LT_param_i8_i16, .-LT__LT_param_i8_i16
	.align 4
	.type	LT__uLT_var_i8_I, #function
	.proc	014
LT__uLT_var_i8_I:
.LLFB502:
	save	%sp, -96, %sp
.LLCFI502:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE502:
	.size	LT__uLT_var_i8_I, .-LT__uLT_var_i8_I
	.align 4
	.type	LT__LT_var_i8_I, #function
	.proc	014
LT__LT_var_i8_I:
.LLFB503:
	save	%sp, -96, %sp
.LLCFI503:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE503:
	.size	LT__LT_var_i8_I, .-LT__LT_var_i8_I
	.align 4
	.type	LT__uLT_param_i8_I, #function
	.proc	014
LT__uLT_param_i8_I:
.LLFB504:
	save	%sp, -96, %sp
.LLCFI504:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE504:
	.size	LT__uLT_param_i8_I, .-LT__uLT_param_i8_I
	.align 4
	.type	LT__LT_param_i8_I, #function
	.proc	014
LT__LT_param_i8_I:
.LLFB505:
	save	%sp, -96, %sp
.LLCFI505:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE505:
	.size	LT__LT_param_i8_I, .-LT__LT_param_i8_I
	.align 4
	.type	LT__uLT_var_i8_i64, #function
	.proc	014
LT__uLT_var_i8_i64:
.LLFB506:
	save	%sp, -96, %sp
.LLCFI506:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL1084
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL1085
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL1084
	 nop
	cmp	%o3, %o5
.LL1085:
	mov	0, %g1
.LL1084:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE506:
	.size	LT__uLT_var_i8_i64, .-LT__uLT_var_i8_i64
	.align 4
	.type	LT__LT_var_i8_i64, #function
	.proc	014
LT__LT_var_i8_i64:
.LLFB507:
	save	%sp, -96, %sp
.LLCFI507:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL1087
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL1088
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL1087
	 nop
	cmp	%o3, %o5
.LL1088:
	mov	0, %g1
.LL1087:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE507:
	.size	LT__LT_var_i8_i64, .-LT__LT_var_i8_i64
	.align 4
	.type	LT__uLT_param_i8_i64, #function
	.proc	014
LT__uLT_param_i8_i64:
.LLFB508:
	save	%sp, -104, %sp
.LLCFI508:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL1090
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL1091
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL1090
	 nop
	cmp	%o3, %o5
.LL1091:
	mov	0, %g1
.LL1090:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE508:
	.size	LT__uLT_param_i8_i64, .-LT__uLT_param_i8_i64
	.align 4
	.type	LT__LT_param_i8_i64, #function
	.proc	014
LT__LT_param_i8_i64:
.LLFB509:
	save	%sp, -104, %sp
.LLCFI509:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bg	%icc, .LL1093
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL1094
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL1093
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL1094:
	mov	0, %g1
.LL1093:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE509:
	.size	LT__LT_param_i8_i64, .-LT__LT_param_i8_i64
	.align 4
	.type	LT__uLT_var_i8_C, #function
	.proc	014
LT__uLT_var_i8_C:
.LLFB510:
	save	%sp, -96, %sp
.LLCFI510:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE510:
	.size	LT__uLT_var_i8_C, .-LT__uLT_var_i8_C
	.align 4
	.type	LT__LT_var_i8_C, #function
	.proc	014
LT__LT_var_i8_C:
.LLFB511:
	save	%sp, -96, %sp
.LLCFI511:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE511:
	.size	LT__LT_var_i8_C, .-LT__LT_var_i8_C
	.align 4
	.type	LT__uLT_param_i8_C, #function
	.proc	014
LT__uLT_param_i8_C:
.LLFB512:
	save	%sp, -96, %sp
.LLCFI512:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE512:
	.size	LT__uLT_param_i8_C, .-LT__uLT_param_i8_C
	.align 4
	.type	LT__LT_param_i8_C, #function
	.proc	014
LT__LT_param_i8_C:
.LLFB513:
	save	%sp, -96, %sp
.LLCFI513:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE513:
	.size	LT__LT_param_i8_C, .-LT__LT_param_i8_C
	.align 4
	.type	LT__uLT_var_i8_u8, #function
	.proc	014
LT__uLT_var_i8_u8:
.LLFB514:
	save	%sp, -96, %sp
.LLCFI514:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE514:
	.size	LT__uLT_var_i8_u8, .-LT__uLT_var_i8_u8
	.align 4
	.type	LT__LT_var_i8_u8, #function
	.proc	014
LT__LT_var_i8_u8:
.LLFB515:
	save	%sp, -96, %sp
.LLCFI515:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE515:
	.size	LT__LT_var_i8_u8, .-LT__LT_var_i8_u8
	.align 4
	.type	LT__uLT_param_i8_u8, #function
	.proc	014
LT__uLT_param_i8_u8:
.LLFB516:
	save	%sp, -96, %sp
.LLCFI516:
	stb	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE516:
	.size	LT__uLT_param_i8_u8, .-LT__uLT_param_i8_u8
	.align 4
	.type	LT__LT_param_i8_u8, #function
	.proc	014
LT__LT_param_i8_u8:
.LLFB517:
	save	%sp, -96, %sp
.LLCFI517:
	stb	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE517:
	.size	LT__LT_param_i8_u8, .-LT__LT_param_i8_u8
	.align 4
	.type	LT__uLT_var_i8_L, #function
	.proc	014
LT__uLT_var_i8_L:
.LLFB518:
	save	%sp, -96, %sp
.LLCFI518:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL1104
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL1105
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL1104
	 nop
	cmp	%o3, %o5
.LL1105:
	mov	0, %g1
.LL1104:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE518:
	.size	LT__uLT_var_i8_L, .-LT__uLT_var_i8_L
	.align 4
	.type	LT__LT_var_i8_L, #function
	.proc	014
LT__LT_var_i8_L:
.LLFB519:
	save	%sp, -96, %sp
.LLCFI519:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL1107
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL1108
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL1107
	 nop
	cmp	%o3, %o5
.LL1108:
	mov	0, %g1
.LL1107:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE519:
	.size	LT__LT_var_i8_L, .-LT__LT_var_i8_L
	.align 4
	.type	LT__uLT_param_i8_L, #function
	.proc	014
LT__uLT_param_i8_L:
.LLFB520:
	save	%sp, -104, %sp
.LLCFI520:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL1110
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL1111
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL1110
	 nop
	cmp	%o3, %o5
.LL1111:
	mov	0, %g1
.LL1110:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE520:
	.size	LT__uLT_param_i8_L, .-LT__uLT_param_i8_L
	.align 4
	.type	LT__LT_param_i8_L, #function
	.proc	014
LT__LT_param_i8_L:
.LLFB521:
	save	%sp, -104, %sp
.LLCFI521:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bg	%icc, .LL1113
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL1114
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL1113
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL1114:
	mov	0, %g1
.LL1113:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE521:
	.size	LT__LT_param_i8_L, .-LT__LT_param_i8_L
	.align 4
	.type	LT__uLT_var_i8_i8, #function
	.proc	014
LT__uLT_var_i8_i8:
.LLFB522:
	save	%sp, -96, %sp
.LLCFI522:
	mov	0, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE522:
	.size	LT__uLT_var_i8_i8, .-LT__uLT_var_i8_i8
	.align 4
	.type	LT__LT_var_i8_i8, #function
	.proc	014
LT__LT_var_i8_i8:
.LLFB523:
	save	%sp, -96, %sp
.LLCFI523:
	mov	0, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE523:
	.size	LT__LT_var_i8_i8, .-LT__LT_var_i8_i8
	.align 4
	.type	LT__uLT_param_i8_i8, #function
	.proc	014
LT__uLT_param_i8_i8:
.LLFB524:
	save	%sp, -96, %sp
.LLCFI524:
	stb	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE524:
	.size	LT__uLT_param_i8_i8, .-LT__uLT_param_i8_i8
	.align 4
	.type	LT__LT_param_i8_i8, #function
	.proc	014
LT__LT_param_i8_i8:
.LLFB525:
	save	%sp, -96, %sp
.LLCFI525:
	stb	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE525:
	.size	LT__LT_param_i8_i8, .-LT__LT_param_i8_i8
	.align 4
	.type	LT__uLT_var_i8_u32, #function
	.proc	014
LT__uLT_var_i8_u32:
.LLFB526:
	save	%sp, -96, %sp
.LLCFI526:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE526:
	.size	LT__uLT_var_i8_u32, .-LT__uLT_var_i8_u32
	.align 4
	.type	LT__LT_var_i8_u32, #function
	.proc	014
LT__LT_var_i8_u32:
.LLFB527:
	save	%sp, -96, %sp
.LLCFI527:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE527:
	.size	LT__LT_var_i8_u32, .-LT__LT_var_i8_u32
	.align 4
	.type	LT__uLT_param_i8_u32, #function
	.proc	014
LT__uLT_param_i8_u32:
.LLFB528:
	save	%sp, -96, %sp
.LLCFI528:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE528:
	.size	LT__uLT_param_i8_u32, .-LT__uLT_param_i8_u32
	.align 4
	.type	LT__LT_param_i8_u32, #function
	.proc	014
LT__LT_param_i8_u32:
.LLFB529:
	save	%sp, -96, %sp
.LLCFI529:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE529:
	.size	LT__LT_param_i8_u32, .-LT__LT_param_i8_u32
	.align 4
	.type	LT__uLT_var_u32_u16, #function
	.proc	014
LT__uLT_var_u32_u16:
.LLFB530:
	save	%sp, -96, %sp
.LLCFI530:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE530:
	.size	LT__uLT_var_u32_u16, .-LT__uLT_var_u32_u16
	.align 4
	.type	LT__LT_var_u32_u16, #function
	.proc	014
LT__LT_var_u32_u16:
.LLFB531:
	save	%sp, -96, %sp
.LLCFI531:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE531:
	.size	LT__LT_var_u32_u16, .-LT__LT_var_u32_u16
	.align 4
	.type	LT__uLT_param_u32_u16, #function
	.proc	014
LT__uLT_param_u32_u16:
.LLFB532:
	save	%sp, -96, %sp
.LLCFI532:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ld	[%fp+68], %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE532:
	.size	LT__uLT_param_u32_u16, .-LT__uLT_param_u32_u16
	.align 4
	.type	LT__LT_param_u32_u16, #function
	.proc	014
LT__LT_param_u32_u16:
.LLFB533:
	save	%sp, -96, %sp
.LLCFI533:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+68], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movg	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE533:
	.size	LT__LT_param_u32_u16, .-LT__LT_param_u32_u16
	.align 4
	.type	LT__uLT_var_u32_u64, #function
	.proc	014
LT__uLT_var_u32_u64:
.LLFB534:
	save	%sp, -96, %sp
.LLCFI534:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL1128
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL1129
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL1128
	 nop
	cmp	%o3, %o5
.LL1129:
	mov	0, %g1
.LL1128:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE534:
	.size	LT__uLT_var_u32_u64, .-LT__uLT_var_u32_u64
	.align 4
	.type	LT__LT_var_u32_u64, #function
	.proc	014
LT__LT_var_u32_u64:
.LLFB535:
	save	%sp, -96, %sp
.LLCFI535:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL1131
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL1132
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL1131
	 nop
	cmp	%o3, %o5
.LL1132:
	mov	0, %g1
.LL1131:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE535:
	.size	LT__LT_var_u32_u64, .-LT__LT_var_u32_u64
	.align 4
	.type	LT__uLT_param_u32_u64, #function
	.proc	014
LT__uLT_param_u32_u64:
.LLFB536:
	save	%sp, -104, %sp
.LLCFI536:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL1134
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL1135
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL1134
	 nop
	cmp	%o3, %o5
.LL1135:
	mov	0, %g1
.LL1134:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE536:
	.size	LT__uLT_param_u32_u64, .-LT__uLT_param_u32_u64
	.align 4
	.type	LT__LT_param_u32_u64, #function
	.proc	014
LT__LT_param_u32_u64:
.LLFB537:
	save	%sp, -104, %sp
.LLCFI537:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bg	%icc, .LL1137
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL1138
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL1137
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL1138:
	mov	0, %g1
.LL1137:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE537:
	.size	LT__LT_param_u32_u64, .-LT__LT_param_u32_u64
	.align 4
	.type	LT__uLT_var_u32_LC, #function
	.proc	014
LT__uLT_var_u32_LC:
.LLFB538:
	save	%sp, -96, %sp
.LLCFI538:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL1140
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL1141
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL1140
	 nop
	cmp	%o3, %o5
.LL1141:
	mov	0, %g1
.LL1140:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE538:
	.size	LT__uLT_var_u32_LC, .-LT__uLT_var_u32_LC
	.align 4
	.type	LT__LT_var_u32_LC, #function
	.proc	014
LT__LT_var_u32_LC:
.LLFB539:
	save	%sp, -96, %sp
.LLCFI539:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL1143
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL1144
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL1143
	 nop
	cmp	%o3, %o5
.LL1144:
	mov	0, %g1
.LL1143:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE539:
	.size	LT__LT_var_u32_LC, .-LT__LT_var_u32_LC
	.align 4
	.type	LT__uLT_param_u32_LC, #function
	.proc	014
LT__uLT_param_u32_LC:
.LLFB540:
	save	%sp, -104, %sp
.LLCFI540:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bgu	%icc, .LL1146
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL1147
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL1146
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL1147:
	mov	0, %g1
.LL1146:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE540:
	.size	LT__uLT_param_u32_LC, .-LT__uLT_param_u32_LC
	.align 4
	.type	LT__LT_param_u32_LC, #function
	.proc	014
LT__LT_param_u32_LC:
.LLFB541:
	save	%sp, -104, %sp
.LLCFI541:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL1149
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL1150
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL1149
	 nop
	cmp	%o3, %o5
.LL1150:
	mov	0, %g1
.LL1149:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE541:
	.size	LT__LT_param_u32_LC, .-LT__LT_param_u32_LC
	.align 4
	.type	LT__uLT_var_u32_i32, #function
	.proc	014
LT__uLT_var_u32_i32:
.LLFB542:
	save	%sp, -96, %sp
.LLCFI542:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE542:
	.size	LT__uLT_var_u32_i32, .-LT__uLT_var_u32_i32
	.align 4
	.type	LT__LT_var_u32_i32, #function
	.proc	014
LT__LT_var_u32_i32:
.LLFB543:
	save	%sp, -96, %sp
.LLCFI543:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE543:
	.size	LT__LT_var_u32_i32, .-LT__LT_var_u32_i32
	.align 4
	.type	LT__uLT_param_u32_i32, #function
	.proc	014
LT__uLT_param_u32_i32:
.LLFB544:
	save	%sp, -96, %sp
.LLCFI544:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE544:
	.size	LT__uLT_param_u32_i32, .-LT__uLT_param_u32_i32
	.align 4
	.type	LT__LT_param_u32_i32, #function
	.proc	014
LT__LT_param_u32_i32:
.LLFB545:
	save	%sp, -96, %sp
.LLCFI545:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE545:
	.size	LT__LT_param_u32_i32, .-LT__LT_param_u32_i32
	.align 4
	.type	LT__uLT_var_u32_i16, #function
	.proc	014
LT__uLT_var_u32_i16:
.LLFB546:
	save	%sp, -96, %sp
.LLCFI546:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE546:
	.size	LT__uLT_var_u32_i16, .-LT__uLT_var_u32_i16
	.align 4
	.type	LT__LT_var_u32_i16, #function
	.proc	014
LT__LT_var_u32_i16:
.LLFB547:
	save	%sp, -96, %sp
.LLCFI547:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE547:
	.size	LT__LT_var_u32_i16, .-LT__LT_var_u32_i16
	.align 4
	.type	LT__uLT_param_u32_i16, #function
	.proc	014
LT__uLT_param_u32_i16:
.LLFB548:
	save	%sp, -96, %sp
.LLCFI548:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ld	[%fp+68], %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE548:
	.size	LT__uLT_param_u32_i16, .-LT__uLT_param_u32_i16
	.align 4
	.type	LT__LT_param_u32_i16, #function
	.proc	014
LT__LT_param_u32_i16:
.LLFB549:
	save	%sp, -96, %sp
.LLCFI549:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+68], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movg	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE549:
	.size	LT__LT_param_u32_i16, .-LT__LT_param_u32_i16
	.align 4
	.type	LT__uLT_var_u32_I, #function
	.proc	014
LT__uLT_var_u32_I:
.LLFB550:
	save	%sp, -96, %sp
.LLCFI550:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE550:
	.size	LT__uLT_var_u32_I, .-LT__uLT_var_u32_I
	.align 4
	.type	LT__LT_var_u32_I, #function
	.proc	014
LT__LT_var_u32_I:
.LLFB551:
	save	%sp, -96, %sp
.LLCFI551:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+80], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE551:
	.size	LT__LT_var_u32_I, .-LT__LT_var_u32_I
	.align 4
	.type	LT__uLT_param_u32_I, #function
	.proc	014
LT__uLT_param_u32_I:
.LLFB552:
	save	%sp, -96, %sp
.LLCFI552:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE552:
	.size	LT__uLT_param_u32_I, .-LT__uLT_param_u32_I
	.align 4
	.type	LT__LT_param_u32_I, #function
	.proc	014
LT__LT_param_u32_I:
.LLFB553:
	save	%sp, -96, %sp
.LLCFI553:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE553:
	.size	LT__LT_param_u32_I, .-LT__LT_param_u32_I
	.align 4
	.type	LT__uLT_var_u32_i64, #function
	.proc	014
LT__uLT_var_u32_i64:
.LLFB554:
	save	%sp, -96, %sp
.LLCFI554:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL1164
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL1165
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL1164
	 nop
	cmp	%o3, %o5
.LL1165:
	mov	0, %g1
.LL1164:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE554:
	.size	LT__uLT_var_u32_i64, .-LT__uLT_var_u32_i64
	.align 4
	.type	LT__LT_var_u32_i64, #function
	.proc	014
LT__LT_var_u32_i64:
.LLFB555:
	save	%sp, -96, %sp
.LLCFI555:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL1167
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL1168
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL1167
	 nop
	cmp	%o3, %o5
.LL1168:
	mov	0, %g1
.LL1167:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE555:
	.size	LT__LT_var_u32_i64, .-LT__LT_var_u32_i64
	.align 4
	.type	LT__uLT_param_u32_i64, #function
	.proc	014
LT__uLT_param_u32_i64:
.LLFB556:
	save	%sp, -104, %sp
.LLCFI556:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL1170
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL1171
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL1170
	 nop
	cmp	%o3, %o5
.LL1171:
	mov	0, %g1
.LL1170:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE556:
	.size	LT__uLT_param_u32_i64, .-LT__uLT_param_u32_i64
	.align 4
	.type	LT__LT_param_u32_i64, #function
	.proc	014
LT__LT_param_u32_i64:
.LLFB557:
	save	%sp, -104, %sp
.LLCFI557:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bg	%icc, .LL1173
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL1174
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL1173
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL1174:
	mov	0, %g1
.LL1173:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE557:
	.size	LT__LT_param_u32_i64, .-LT__LT_param_u32_i64
	.align 4
	.type	LT__uLT_var_u32_C, #function
	.proc	014
LT__uLT_var_u32_C:
.LLFB558:
	save	%sp, -96, %sp
.LLCFI558:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE558:
	.size	LT__uLT_var_u32_C, .-LT__uLT_var_u32_C
	.align 4
	.type	LT__LT_var_u32_C, #function
	.proc	014
LT__LT_var_u32_C:
.LLFB559:
	save	%sp, -96, %sp
.LLCFI559:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+96], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE559:
	.size	LT__LT_var_u32_C, .-LT__LT_var_u32_C
	.align 4
	.type	LT__uLT_param_u32_C, #function
	.proc	014
LT__uLT_param_u32_C:
.LLFB560:
	save	%sp, -96, %sp
.LLCFI560:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE560:
	.size	LT__uLT_param_u32_C, .-LT__uLT_param_u32_C
	.align 4
	.type	LT__LT_param_u32_C, #function
	.proc	014
LT__LT_param_u32_C:
.LLFB561:
	save	%sp, -96, %sp
.LLCFI561:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movg	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE561:
	.size	LT__LT_param_u32_C, .-LT__LT_param_u32_C
	.align 4
	.type	LT__uLT_var_u32_u8, #function
	.proc	014
LT__uLT_var_u32_u8:
.LLFB562:
	save	%sp, -96, %sp
.LLCFI562:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE562:
	.size	LT__uLT_var_u32_u8, .-LT__uLT_var_u32_u8
	.align 4
	.type	LT__LT_var_u32_u8, #function
	.proc	014
LT__LT_var_u32_u8:
.LLFB563:
	save	%sp, -96, %sp
.LLCFI563:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE563:
	.size	LT__LT_var_u32_u8, .-LT__LT_var_u32_u8
	.align 4
	.type	LT__uLT_param_u32_u8, #function
	.proc	014
LT__uLT_param_u32_u8:
.LLFB564:
	save	%sp, -96, %sp
.LLCFI564:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE564:
	.size	LT__uLT_param_u32_u8, .-LT__uLT_param_u32_u8
	.align 4
	.type	LT__LT_param_u32_u8, #function
	.proc	014
LT__LT_param_u32_u8:
.LLFB565:
	save	%sp, -96, %sp
.LLCFI565:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+68], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movg	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE565:
	.size	LT__LT_param_u32_u8, .-LT__LT_param_u32_u8
	.align 4
	.type	LT__uLT_var_u32_L, #function
	.proc	014
LT__uLT_var_u32_L:
.LLFB566:
	save	%sp, -96, %sp
.LLCFI566:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL1184
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL1185
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL1184
	 nop
	cmp	%o3, %o5
.LL1185:
	mov	0, %g1
.LL1184:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE566:
	.size	LT__uLT_var_u32_L, .-LT__uLT_var_u32_L
	.align 4
	.type	LT__LT_var_u32_L, #function
	.proc	014
LT__LT_var_u32_L:
.LLFB567:
	save	%sp, -96, %sp
.LLCFI567:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL1187
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL1188
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL1187
	 nop
	cmp	%o3, %o5
.LL1188:
	mov	0, %g1
.LL1187:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE567:
	.size	LT__LT_var_u32_L, .-LT__LT_var_u32_L
	.align 4
	.type	LT__uLT_param_u32_L, #function
	.proc	014
LT__uLT_param_u32_L:
.LLFB568:
	save	%sp, -104, %sp
.LLCFI568:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL1190
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL1191
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL1190
	 nop
	cmp	%o3, %o5
.LL1191:
	mov	0, %g1
.LL1190:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE568:
	.size	LT__uLT_param_u32_L, .-LT__uLT_param_u32_L
	.align 4
	.type	LT__LT_param_u32_L, #function
	.proc	014
LT__LT_param_u32_L:
.LLFB569:
	save	%sp, -104, %sp
.LLCFI569:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bg	%icc, .LL1193
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL1194
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL1193
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, %o5
.LL1194:
	mov	0, %g1
.LL1193:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE569:
	.size	LT__LT_param_u32_L, .-LT__LT_param_u32_L
	.align 4
	.type	LT__uLT_var_u32_i8, #function
	.proc	014
LT__uLT_var_u32_i8:
.LLFB570:
	save	%sp, -96, %sp
.LLCFI570:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE570:
	.size	LT__uLT_var_u32_i8, .-LT__uLT_var_u32_i8
	.align 4
	.type	LT__LT_var_u32_i8, #function
	.proc	014
LT__LT_var_u32_i8:
.LLFB571:
	save	%sp, -96, %sp
.LLCFI571:
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+132], %o5
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE571:
	.size	LT__LT_var_u32_i8, .-LT__LT_var_u32_i8
	.align 4
	.type	LT__uLT_param_u32_i8, #function
	.proc	014
LT__uLT_param_u32_i8:
.LLFB572:
	save	%sp, -96, %sp
.LLCFI572:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE572:
	.size	LT__uLT_param_u32_i8, .-LT__uLT_param_u32_i8
	.align 4
	.type	LT__LT_param_u32_i8, #function
	.proc	014
LT__LT_param_u32_i8:
.LLFB573:
	save	%sp, -96, %sp
.LLCFI573:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+68], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movg	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE573:
	.size	LT__LT_param_u32_i8, .-LT__LT_param_u32_i8
	.align 4
	.type	LT__uLT_var_u32_u32, #function
	.proc	014
LT__uLT_var_u32_u32:
.LLFB574:
	save	%sp, -96, %sp
.LLCFI574:
	mov	0, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE574:
	.size	LT__uLT_var_u32_u32, .-LT__uLT_var_u32_u32
	.align 4
	.type	LT__LT_var_u32_u32, #function
	.proc	014
LT__LT_var_u32_u32:
.LLFB575:
	save	%sp, -96, %sp
.LLCFI575:
	mov	0, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE575:
	.size	LT__LT_var_u32_u32, .-LT__LT_var_u32_u32
	.align 4
	.type	LT__uLT_param_u32_u32, #function
	.proc	014
LT__uLT_param_u32_u32:
.LLFB576:
	save	%sp, -96, %sp
.LLCFI576:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movlu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE576:
	.size	LT__uLT_param_u32_u32, .-LT__uLT_param_u32_u32
	.align 4
	.type	LT__LT_param_u32_u32, #function
	.proc	014
LT__LT_param_u32_u32:
.LLFB577:
	save	%sp, -96, %sp
.LLCFI577:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movl	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE577:
	.size	LT__LT_param_u32_u32, .-LT__LT_param_u32_u32
	.align 4
	.type	LT__LT_var_f32_f32, #function
	.proc	014
LT__LT_var_f32_f32:
.LLFB578:
	save	%sp, -96, %sp
.LLCFI578:
	mov	0, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE578:
	.size	LT__LT_var_f32_f32, .-LT__LT_var_f32_f32
	.align 4
	.type	LT__LT_param_f32_f32, #function
	.proc	014
LT__LT_param_f32_f32:
.LLFB579:
	save	%sp, -96, %sp
.LLCFI579:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %f9
	ld	[%fp+72], %f8
	fcmpes	%fcc1, %f9, %f8
	mov	0, %g1
	movl	%fcc1, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE579:
	.size	LT__LT_param_f32_f32, .-LT__LT_param_f32_f32
	.align 4
	.global LT_M3
	.type	LT_M3, #function
	.proc	0120
LT_M3:
.LLFB580:
	save	%sp, -96, %sp
.LLCFI580:
	st	%i0, [%fp+68]
	sethi	%hi(MM_LT), %g1
	or	%g1, %lo(MM_LT), %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE580:
	.size	LT_M3, .-LT_M3
	.section	".data"
	.align 8
	.type	L_1, #object
	.size	L_1, 13384
L_1:
	.ascii	"LT_M3"
	.skip 1
	.ascii	"LT_param_f32_f32"
	.skip 1
	.ascii	"LT_var_f32_f32"
	.skip 1
	.ascii	"LT_param_u32_u32"
	.skip 1
	.ascii	"uLT_param_u32_u32"
	.skip 1
	.ascii	"LT_var_u32_u32"
	.skip 1
	.ascii	"uLT_var_u32_u32"
	.skip 1
	.ascii	"LT_param_u32_i8"
	.skip 1
	.ascii	"uLT_param_u32_i8"
	.skip 1
	.ascii	"LT_var_u32_i8"
	.skip 1
	.ascii	"uLT_var_u32_i8"
	.skip 1
	.ascii	"LT_param_u32_L"
	.skip 1
	.ascii	"uLT_param_u32_L"
	.skip 1
	.ascii	"LT_var_u32_L"
	.skip 1
	.ascii	"uLT_var_u32_L"
	.skip 1
	.ascii	"LT_param_u32_u8"
	.skip 1
	.ascii	"uLT_param_u32_u8"
	.skip 1
	.ascii	"LT_var_u32_u8"
	.skip 1
	.ascii	"uLT_var_u32_u8"
	.skip 1
	.ascii	"LT_param_u32_C"
	.skip 1
	.ascii	"uLT_param_u32_C"
	.skip 1
	.ascii	"LT_var_u32_C"
	.skip 1
	.ascii	"uLT_var_u32_C"
	.skip 1
	.ascii	"LT_param_u32_i64"
	.skip 1
	.ascii	"uLT_param_u32_i64"
	.skip 1
	.ascii	"LT_var_u32_i64"
	.skip 1
	.ascii	"uLT_var_u32_i64"
	.skip 1
	.ascii	"LT_param_u32_I"
	.skip 1
	.ascii	"uLT_param_u32_I"
	.skip 1
	.ascii	"LT_var_u32_I"
	.skip 1
	.ascii	"uLT_var_u32_I"
	.skip 1
	.ascii	"LT_param_u32_i16"
	.skip 1
	.ascii	"uLT_param_u32_i16"
	.skip 1
	.ascii	"LT_var_u32_i16"
	.skip 1
	.ascii	"uLT_var_u32_i16"
	.skip 1
	.ascii	"LT_param_u32_i32"
	.skip 1
	.ascii	"uLT_param_u32_i32"
	.skip 1
	.ascii	"LT_var_u32_i32"
	.skip 1
	.ascii	"uLT_var_u32_i32"
	.skip 1
	.ascii	"LT_param_u32_LC"
	.skip 1
	.ascii	"uLT_param_u32_LC"
	.skip 1
	.ascii	"LT_var_u32_LC"
	.skip 1
	.ascii	"uLT_var_u32_LC"
	.skip 1
	.ascii	"LT_param_u32_u64"
	.skip 1
	.ascii	"uLT_param_u32_u64"
	.skip 1
	.ascii	"LT_var_u32_u64"
	.skip 1
	.ascii	"uLT_var_u32_u64"
	.skip 1
	.ascii	"LT_param_u32_u16"
	.skip 1
	.ascii	"uLT_param_u32_u16"
	.skip 1
	.ascii	"LT_var_u32_u16"
	.skip 1
	.ascii	"uLT_var_u32_u16"
	.skip 1
	.ascii	"LT_param_i8_u32"
	.skip 1
	.ascii	"uLT_param_i8_u32"
	.skip 1
	.ascii	"LT_var_i8_u32"
	.skip 1
	.ascii	"uLT_var_i8_u32"
	.skip 1
	.ascii	"LT_param_i8_i8"
	.skip 1
	.ascii	"uLT_param_i8_i8"
	.skip 1
	.ascii	"LT_var_i8_i8"
	.skip 1
	.ascii	"uLT_var_i8_i8"
	.skip 1
	.ascii	"LT_param_i8_L"
	.skip 1
	.ascii	"uLT_param_i8_L"
	.skip 1
	.ascii	"LT_var_i8_L"
	.skip 1
	.ascii	"uLT_var_i8_L"
	.skip 1
	.ascii	"LT_param_i8_u8"
	.skip 1
	.ascii	"uLT_param_i8_u8"
	.skip 1
	.ascii	"LT_var_i8_u8"
	.skip 1
	.ascii	"uLT_var_i8_u8"
	.skip 1
	.ascii	"LT_param_i8_C"
	.skip 1
	.ascii	"uLT_param_i8_C"
	.skip 1
	.ascii	"LT_var_i8_C"
	.skip 1
	.ascii	"uLT_var_i8_C"
	.skip 1
	.ascii	"LT_param_i8_i64"
	.skip 1
	.ascii	"uLT_param_i8_i64"
	.skip 1
	.ascii	"LT_var_i8_i64"
	.skip 1
	.ascii	"uLT_var_i8_i64"
	.skip 1
	.ascii	"LT_param_i8_I"
	.skip 1
	.ascii	"uLT_param_i8_I"
	.skip 1
	.ascii	"LT_var_i8_I"
	.skip 1
	.ascii	"uLT_var_i8_I"
	.skip 1
	.ascii	"LT_param_i8_i16"
	.skip 1
	.ascii	"uLT_param_i8_i16"
	.skip 1
	.ascii	"LT_var_i8_i16"
	.skip 1
	.ascii	"uLT_var_i8_i16"
	.skip 1
	.ascii	"LT_param_i8_i32"
	.skip 1
	.ascii	"uLT_param_i8_i32"
	.skip 1
	.ascii	"LT_var_i8_i32"
	.skip 1
	.ascii	"uLT_var_i8_i32"
	.skip 1
	.ascii	"LT_param_i8_LC"
	.skip 1
	.ascii	"uLT_param_i8_LC"
	.skip 1
	.ascii	"LT_var_i8_LC"
	.skip 1
	.ascii	"uLT_var_i8_LC"
	.skip 1
	.ascii	"LT_param_i8_u64"
	.skip 1
	.ascii	"uLT_param_i8_u64"
	.skip 1
	.ascii	"LT_var_i8_u64"
	.skip 1
	.ascii	"uLT_var_i8_u64"
	.skip 1
	.ascii	"LT_param_i8_u16"
	.skip 1
	.ascii	"uLT_param_i8_u16"
	.skip 1
	.ascii	"LT_var_i8_u16"
	.skip 1
	.ascii	"uLT_var_i8_u16"
	.skip 1
	.ascii	"LT_param_L_u32"
	.skip 1
	.ascii	"uLT_param_L_u32"
	.skip 1
	.ascii	"LT_var_L_u32"
	.skip 1
	.ascii	"uLT_var_L_u32"
	.skip 1
	.ascii	"LT_param_L_i8"
	.skip 1
	.ascii	"uLT_param_L_i8"
	.skip 1
	.ascii	"LT_var_L_i8"
	.skip 1
	.ascii	"uLT_var_L_i8"
	.skip 1
	.ascii	"LT_param_L_L"
	.skip 1
	.ascii	"uLT_param_L_L"
	.skip 1
	.ascii	"LT_var_L_L"
	.skip 1
	.ascii	"uLT_var_L_L"
	.skip 1
	.ascii	"LT_param_L_u8"
	.skip 1
	.ascii	"uLT_param_L_u8"
	.skip 1
	.ascii	"LT_var_L_u8"
	.skip 1
	.ascii	"uLT_var_L_u8"
	.skip 1
	.ascii	"LT_param_L_C"
	.skip 1
	.ascii	"uLT_param_L_C"
	.skip 1
	.ascii	"LT_var_L_C"
	.skip 1
	.ascii	"uLT_var_L_C"
	.skip 1
	.ascii	"LT_param_L_i64"
	.skip 1
	.ascii	"uLT_param_L_i64"
	.skip 1
	.ascii	"LT_var_L_i64"
	.skip 1
	.ascii	"uLT_var_L_i64"
	.skip 1
	.ascii	"LT_param_L_I"
	.skip 1
	.ascii	"uLT_param_L_I"
	.skip 1
	.ascii	"LT_var_L_I"
	.skip 1
	.ascii	"uLT_var_L_I"
	.skip 1
	.ascii	"LT_param_L_i16"
	.skip 1
	.ascii	"uLT_param_L_i16"
	.skip 1
	.ascii	"LT_var_L_i16"
	.skip 1
	.ascii	"uLT_var_L_i16"
	.skip 1
	.ascii	"LT_param_L_i32"
	.skip 1
	.ascii	"uLT_param_L_i32"
	.skip 1
	.ascii	"LT_var_L_i32"
	.skip 1
	.ascii	"uLT_var_L_i32"
	.skip 1
	.ascii	"LT_param_L_LC"
	.skip 1
	.ascii	"uLT_param_L_LC"
	.skip 1
	.ascii	"LT_var_L_LC"
	.skip 1
	.ascii	"uLT_var_L_LC"
	.skip 1
	.ascii	"LT_param_L_u64"
	.skip 1
	.ascii	"uLT_param_L_u64"
	.skip 1
	.ascii	"LT_var_L_u64"
	.skip 1
	.ascii	"uLT_var_L_u64"
	.skip 1
	.ascii	"LT_param_L_u16"
	.skip 1
	.ascii	"uLT_param_L_u16"
	.skip 1
	.ascii	"LT_var_L_u16"
	.skip 1
	.ascii	"uLT_var_L_u16"
	.skip 1
	.ascii	"LT_param_u8_u32"
	.skip 1
	.ascii	"uLT_param_u8_u32"
	.skip 1
	.ascii	"LT_var_u8_u32"
	.skip 1
	.ascii	"uLT_var_u8_u32"
	.skip 1
	.ascii	"LT_param_u8_i8"
	.skip 1
	.ascii	"uLT_param_u8_i8"
	.skip 1
	.ascii	"LT_var_u8_i8"
	.skip 1
	.ascii	"uLT_var_u8_i8"
	.skip 1
	.ascii	"LT_param_u8_L"
	.skip 1
	.ascii	"uLT_param_u8_L"
	.skip 1
	.ascii	"LT_var_u8_L"
	.skip 1
	.ascii	"uLT_var_u8_L"
	.skip 1
	.ascii	"LT_param_u8_u8"
	.skip 1
	.ascii	"uLT_param_u8_u8"
	.skip 1
	.ascii	"LT_var_u8_u8"
	.skip 1
	.ascii	"uLT_var_u8_u8"
	.skip 1
	.ascii	"LT_param_u8_C"
	.skip 1
	.ascii	"uLT_param_u8_C"
	.skip 1
	.ascii	"LT_var_u8_C"
	.skip 1
	.ascii	"uLT_var_u8_C"
	.skip 1
	.ascii	"LT_param_u8_i64"
	.skip 1
	.ascii	"uLT_param_u8_i64"
	.skip 1
	.ascii	"LT_var_u8_i64"
	.skip 1
	.ascii	"uLT_var_u8_i64"
	.skip 1
	.ascii	"LT_param_u8_I"
	.skip 1
	.ascii	"uLT_param_u8_I"
	.skip 1
	.ascii	"LT_var_u8_I"
	.skip 1
	.ascii	"uLT_var_u8_I"
	.skip 1
	.ascii	"LT_param_u8_i16"
	.skip 1
	.ascii	"uLT_param_u8_i16"
	.skip 1
	.ascii	"LT_var_u8_i16"
	.skip 1
	.ascii	"uLT_var_u8_i16"
	.skip 1
	.ascii	"LT_param_u8_i32"
	.skip 1
	.ascii	"uLT_param_u8_i32"
	.skip 1
	.ascii	"LT_var_u8_i32"
	.skip 1
	.ascii	"uLT_var_u8_i32"
	.skip 1
	.ascii	"LT_param_u8_LC"
	.skip 1
	.ascii	"uLT_param_u8_LC"
	.skip 1
	.ascii	"LT_var_u8_LC"
	.skip 1
	.ascii	"uLT_var_u8_LC"
	.skip 1
	.ascii	"LT_param_u8_u64"
	.skip 1
	.ascii	"uLT_param_u8_u64"
	.skip 1
	.ascii	"LT_var_u8_u64"
	.skip 1
	.ascii	"uLT_var_u8_u64"
	.skip 1
	.ascii	"LT_param_u8_u16"
	.skip 1
	.ascii	"uLT_param_u8_u16"
	.skip 1
	.ascii	"LT_var_u8_u16"
	.skip 1
	.ascii	"uLT_var_u8_u16"
	.skip 1
	.ascii	"LT_param_f64_f64"
	.skip 1
	.ascii	"LT_var_f64_f64"
	.skip 1
	.ascii	"LT_param_C_u32"
	.skip 1
	.ascii	"uLT_param_C_u32"
	.skip 1
	.ascii	"LT_var_C_u32"
	.skip 1
	.ascii	"uLT_var_C_u32"
	.skip 1
	.ascii	"LT_param_C_i8"
	.skip 1
	.ascii	"uLT_param_C_i8"
	.skip 1
	.ascii	"LT_var_C_i8"
	.skip 1
	.ascii	"uLT_var_C_i8"
	.skip 1
	.ascii	"LT_param_C_L"
	.skip 1
	.ascii	"uLT_param_C_L"
	.skip 1
	.ascii	"LT_var_C_L"
	.skip 1
	.ascii	"uLT_var_C_L"
	.skip 1
	.ascii	"LT_param_C_u8"
	.skip 1
	.ascii	"uLT_param_C_u8"
	.skip 1
	.ascii	"LT_var_C_u8"
	.skip 1
	.ascii	"uLT_var_C_u8"
	.skip 1
	.ascii	"LT_param_C_C"
	.skip 1
	.ascii	"uLT_param_C_C"
	.skip 1
	.ascii	"LT_var_C_C"
	.skip 1
	.ascii	"uLT_var_C_C"
	.skip 1
	.ascii	"LT_param_C_i64"
	.skip 1
	.ascii	"uLT_param_C_i64"
	.skip 1
	.ascii	"LT_var_C_i64"
	.skip 1
	.ascii	"uLT_var_C_i64"
	.skip 1
	.ascii	"LT_param_C_I"
	.skip 1
	.ascii	"uLT_param_C_I"
	.skip 1
	.ascii	"LT_var_C_I"
	.skip 1
	.ascii	"uLT_var_C_I"
	.skip 1
	.ascii	"LT_param_C_i16"
	.skip 1
	.ascii	"uLT_param_C_i16"
	.skip 1
	.ascii	"LT_var_C_i16"
	.skip 1
	.ascii	"uLT_var_C_i16"
	.skip 1
	.ascii	"LT_param_C_i32"
	.skip 1
	.ascii	"uLT_param_C_i32"
	.skip 1
	.ascii	"LT_var_C_i32"
	.skip 1
	.ascii	"uLT_var_C_i32"
	.skip 1
	.ascii	"LT_param_C_LC"
	.skip 1
	.ascii	"uLT_param_C_LC"
	.skip 1
	.ascii	"LT_var_C_LC"
	.skip 1
	.ascii	"uLT_var_C_LC"
	.skip 1
	.ascii	"LT_param_C_u64"
	.skip 1
	.ascii	"uLT_param_C_u64"
	.skip 1
	.ascii	"LT_var_C_u64"
	.skip 1
	.ascii	"uLT_var_C_u64"
	.skip 1
	.ascii	"LT_param_C_u16"
	.skip 1
	.ascii	"uLT_param_C_u16"
	.skip 1
	.ascii	"LT_var_C_u16"
	.skip 1
	.ascii	"uLT_var_C_u16"
	.skip 1
	.ascii	"LT_param_i64_u32"
	.skip 1
	.ascii	"uLT_param_i64_u32"
	.skip 1
	.ascii	"LT_var_i64_u32"
	.skip 1
	.ascii	"uLT_var_i64_u32"
	.skip 1
	.ascii	"LT_param_i64_i8"
	.skip 1
	.ascii	"uLT_param_i64_i8"
	.skip 1
	.ascii	"LT_var_i64_i8"
	.skip 1
	.ascii	"uLT_var_i64_i8"
	.skip 1
	.ascii	"LT_param_i64_L"
	.skip 1
	.ascii	"uLT_param_i64_L"
	.skip 1
	.ascii	"LT_var_i64_L"
	.skip 1
	.ascii	"uLT_var_i64_L"
	.skip 1
	.ascii	"LT_param_i64_u8"
	.skip 1
	.ascii	"uLT_param_i64_u8"
	.skip 1
	.ascii	"LT_var_i64_u8"
	.skip 1
	.ascii	"uLT_var_i64_u8"
	.skip 1
	.ascii	"LT_param_i64_C"
	.skip 1
	.ascii	"uLT_param_i64_C"
	.skip 1
	.ascii	"LT_var_i64_C"
	.skip 1
	.ascii	"uLT_var_i64_C"
	.skip 1
	.ascii	"LT_param_i64_i64"
	.skip 1
	.ascii	"uLT_param_i64_i64"
	.skip 1
	.ascii	"LT_var_i64_i64"
	.skip 1
	.ascii	"uLT_var_i64_i64"
	.skip 1
	.ascii	"LT_param_i64_I"
	.skip 1
	.ascii	"uLT_param_i64_I"
	.skip 1
	.ascii	"LT_var_i64_I"
	.skip 1
	.ascii	"uLT_var_i64_I"
	.skip 1
	.ascii	"LT_param_i64_i16"
	.skip 1
	.ascii	"uLT_param_i64_i16"
	.skip 1
	.ascii	"LT_var_i64_i16"
	.skip 1
	.ascii	"uLT_var_i64_i16"
	.skip 1
	.ascii	"LT_param_i64_i32"
	.skip 1
	.ascii	"uLT_param_i64_i32"
	.skip 1
	.ascii	"LT_var_i64_i32"
	.skip 1
	.ascii	"uLT_var_i64_i32"
	.skip 1
	.ascii	"LT_param_i64_LC"
	.skip 1
	.ascii	"uLT_param_i64_LC"
	.skip 1
	.ascii	"LT_var_i64_LC"
	.skip 1
	.ascii	"uLT_var_i64_LC"
	.skip 1
	.ascii	"LT_param_i64_u64"
	.skip 1
	.ascii	"uLT_param_i64_u64"
	.skip 1
	.ascii	"LT_var_i64_u64"
	.skip 1
	.ascii	"uLT_var_i64_u64"
	.skip 1
	.ascii	"LT_param_i64_u16"
	.skip 1
	.ascii	"uLT_param_i64_u16"
	.skip 1
	.ascii	"LT_var_i64_u16"
	.skip 1
	.ascii	"uLT_var_i64_u16"
	.skip 1
	.ascii	"LT_param_I_u32"
	.skip 1
	.ascii	"uLT_param_I_u32"
	.skip 1
	.ascii	"LT_var_I_u32"
	.skip 1
	.ascii	"uLT_var_I_u32"
	.skip 1
	.ascii	"LT_param_I_i8"
	.skip 1
	.ascii	"uLT_param_I_i8"
	.skip 1
	.ascii	"LT_var_I_i8"
	.skip 1
	.ascii	"uLT_var_I_i8"
	.skip 1
	.ascii	"LT_param_I_L"
	.skip 1
	.ascii	"uLT_param_I_L"
	.skip 1
	.ascii	"LT_var_I_L"
	.skip 1
	.ascii	"uLT_var_I_L"
	.skip 1
	.ascii	"LT_param_I_u8"
	.skip 1
	.ascii	"uLT_param_I_u8"
	.skip 1
	.ascii	"LT_var_I_u8"
	.skip 1
	.ascii	"uLT_var_I_u8"
	.skip 1
	.ascii	"LT_param_I_C"
	.skip 1
	.ascii	"uLT_param_I_C"
	.skip 1
	.ascii	"LT_var_I_C"
	.skip 1
	.ascii	"uLT_var_I_C"
	.skip 1
	.ascii	"LT_param_I_i64"
	.skip 1
	.ascii	"uLT_param_I_i64"
	.skip 1
	.ascii	"LT_var_I_i64"
	.skip 1
	.ascii	"uLT_var_I_i64"
	.skip 1
	.ascii	"LT_param_I_I"
	.skip 1
	.ascii	"uLT_param_I_I"
	.skip 1
	.ascii	"LT_var_I_I"
	.skip 1
	.ascii	"uLT_var_I_I"
	.skip 1
	.ascii	"LT_param_I_i16"
	.skip 1
	.ascii	"uLT_param_I_i16"
	.skip 1
	.ascii	"LT_var_I_i16"
	.skip 1
	.ascii	"uLT_var_I_i16"
	.skip 1
	.ascii	"LT_param_I_i32"
	.skip 1
	.ascii	"uLT_param_I_i32"
	.skip 1
	.ascii	"LT_var_I_i32"
	.skip 1
	.ascii	"uLT_var_I_i32"
	.skip 1
	.ascii	"LT_param_I_LC"
	.skip 1
	.ascii	"uLT_param_I_LC"
	.skip 1
	.ascii	"LT_var_I_LC"
	.skip 1
	.ascii	"uLT_var_I_LC"
	.skip 1
	.ascii	"LT_param_I_u64"
	.skip 1
	.ascii	"uLT_param_I_u64"
	.skip 1
	.ascii	"LT_var_I_u64"
	.skip 1
	.ascii	"uLT_var_I_u64"
	.skip 1
	.ascii	"LT_param_I_u16"
	.skip 1
	.ascii	"uLT_param_I_u16"
	.skip 1
	.ascii	"LT_var_I_u16"
	.skip 1
	.ascii	"uLT_var_I_u16"
	.skip 1
	.ascii	"LT_param_i16_u32"
	.skip 1
	.ascii	"uLT_param_i16_u32"
	.skip 1
	.ascii	"LT_var_i16_u32"
	.skip 1
	.ascii	"uLT_var_i16_u32"
	.skip 1
	.ascii	"LT_param_i16_i8"
	.skip 1
	.ascii	"uLT_param_i16_i8"
	.skip 1
	.ascii	"LT_var_i16_i8"
	.skip 1
	.ascii	"uLT_var_i16_i8"
	.skip 1
	.ascii	"LT_param_i16_L"
	.skip 1
	.ascii	"uLT_param_i16_L"
	.skip 1
	.ascii	"LT_var_i16_L"
	.skip 1
	.ascii	"uLT_var_i16_L"
	.skip 1
	.ascii	"LT_param_i16_u8"
	.skip 1
	.ascii	"uLT_param_i16_u8"
	.skip 1
	.ascii	"LT_var_i16_u8"
	.skip 1
	.ascii	"uLT_var_i16_u8"
	.skip 1
	.ascii	"LT_param_i16_C"
	.skip 1
	.ascii	"uLT_param_i16_C"
	.skip 1
	.ascii	"LT_var_i16_C"
	.skip 1
	.ascii	"uLT_var_i16_C"
	.skip 1
	.ascii	"LT_param_i16_i64"
	.skip 1
	.ascii	"uLT_param_i16_i64"
	.skip 1
	.ascii	"LT_var_i16_i64"
	.skip 1
	.ascii	"uLT_var_i16_i64"
	.skip 1
	.ascii	"LT_param_i16_I"
	.skip 1
	.ascii	"uLT_param_i16_I"
	.skip 1
	.ascii	"LT_var_i16_I"
	.skip 1
	.ascii	"uLT_var_i16_I"
	.skip 1
	.ascii	"LT_param_i16_i16"
	.skip 1
	.ascii	"uLT_param_i16_i16"
	.skip 1
	.ascii	"LT_var_i16_i16"
	.skip 1
	.ascii	"uLT_var_i16_i16"
	.skip 1
	.ascii	"LT_param_i16_i32"
	.skip 1
	.ascii	"uLT_param_i16_i32"
	.skip 1
	.ascii	"LT_var_i16_i32"
	.skip 1
	.ascii	"uLT_var_i16_i32"
	.skip 1
	.ascii	"LT_param_i16_LC"
	.skip 1
	.ascii	"uLT_param_i16_LC"
	.skip 1
	.ascii	"LT_var_i16_LC"
	.skip 1
	.ascii	"uLT_var_i16_LC"
	.skip 1
	.ascii	"LT_param_i16_u64"
	.skip 1
	.ascii	"uLT_param_i16_u64"
	.skip 1
	.ascii	"LT_var_i16_u64"
	.skip 1
	.ascii	"uLT_var_i16_u64"
	.skip 1
	.ascii	"LT_param_i16_u16"
	.skip 1
	.ascii	"uLT_param_i16_u16"
	.skip 1
	.ascii	"LT_var_i16_u16"
	.skip 1
	.ascii	"uLT_var_i16_u16"
	.skip 1
	.ascii	"LT_param_i32_u32"
	.skip 1
	.ascii	"uLT_param_i32_u32"
	.skip 1
	.ascii	"LT_var_i32_u32"
	.skip 1
	.ascii	"uLT_var_i32_u32"
	.skip 1
	.ascii	"LT_param_i32_i8"
	.skip 1
	.ascii	"uLT_param_i32_i8"
	.skip 1
	.ascii	"LT_var_i32_i8"
	.skip 1
	.ascii	"uLT_var_i32_i8"
	.skip 1
	.ascii	"LT_param_i32_L"
	.skip 1
	.ascii	"uLT_param_i32_L"
	.skip 1
	.ascii	"LT_var_i32_L"
	.skip 1
	.ascii	"uLT_var_i32_L"
	.skip 1
	.ascii	"LT_param_i32_u8"
	.skip 1
	.ascii	"uLT_param_i32_u8"
	.skip 1
	.ascii	"LT_var_i32_u8"
	.skip 1
	.ascii	"uLT_var_i32_u8"
	.skip 1
	.ascii	"LT_param_i32_C"
	.skip 1
	.ascii	"uLT_param_i32_C"
	.skip 1
	.ascii	"LT_var_i32_C"
	.skip 1
	.ascii	"uLT_var_i32_C"
	.skip 1
	.ascii	"LT_param_i32_i64"
	.skip 1
	.ascii	"uLT_param_i32_i64"
	.skip 1
	.ascii	"LT_var_i32_i64"
	.skip 1
	.ascii	"uLT_var_i32_i64"
	.skip 1
	.ascii	"LT_param_i32_I"
	.skip 1
	.ascii	"uLT_param_i32_I"
	.skip 1
	.ascii	"LT_var_i32_I"
	.skip 1
	.ascii	"uLT_var_i32_I"
	.skip 1
	.ascii	"LT_param_i32_i16"
	.skip 1
	.ascii	"uLT_param_i32_i16"
	.skip 1
	.ascii	"LT_var_i32_i16"
	.skip 1
	.ascii	"uLT_var_i32_i16"
	.skip 1
	.ascii	"LT_param_i32_i32"
	.skip 1
	.ascii	"uLT_param_i32_i32"
	.skip 1
	.ascii	"LT_var_i32_i32"
	.skip 1
	.ascii	"uLT_var_i32_i32"
	.skip 1
	.ascii	"LT_param_i32_LC"
	.skip 1
	.ascii	"uLT_param_i32_LC"
	.skip 1
	.ascii	"LT_var_i32_LC"
	.skip 1
	.ascii	"uLT_var_i32_LC"
	.skip 1
	.ascii	"LT_param_i32_u64"
	.skip 1
	.ascii	"uLT_param_i32_u64"
	.skip 1
	.ascii	"LT_var_i32_u64"
	.skip 1
	.ascii	"uLT_var_i32_u64"
	.skip 1
	.ascii	"LT_param_i32_u16"
	.skip 1
	.ascii	"uLT_param_i32_u16"
	.skip 1
	.ascii	"LT_var_i32_u16"
	.skip 1
	.ascii	"uLT_var_i32_u16"
	.skip 1
	.ascii	"LT_param_LC_u32"
	.skip 1
	.ascii	"uLT_param_LC_u32"
	.skip 1
	.ascii	"LT_var_LC_u32"
	.skip 1
	.ascii	"uLT_var_LC_u32"
	.skip 1
	.ascii	"LT_param_LC_i8"
	.skip 1
	.ascii	"uLT_param_LC_i8"
	.skip 1
	.ascii	"LT_var_LC_i8"
	.skip 1
	.ascii	"uLT_var_LC_i8"
	.skip 1
	.ascii	"LT_param_LC_L"
	.skip 1
	.ascii	"uLT_param_LC_L"
	.skip 1
	.ascii	"LT_var_LC_L"
	.skip 1
	.ascii	"uLT_var_LC_L"
	.skip 1
	.ascii	"LT_param_LC_u8"
	.skip 1
	.ascii	"uLT_param_LC_u8"
	.skip 1
	.ascii	"LT_var_LC_u8"
	.skip 1
	.ascii	"uLT_var_LC_u8"
	.skip 1
	.ascii	"LT_param_LC_C"
	.skip 1
	.ascii	"uLT_param_LC_C"
	.skip 1
	.ascii	"LT_var_LC_C"
	.skip 1
	.ascii	"uLT_var_LC_C"
	.skip 1
	.ascii	"LT_param_LC_i64"
	.skip 1
	.ascii	"uLT_param_LC_i64"
	.skip 1
	.ascii	"LT_var_LC_i64"
	.skip 1
	.ascii	"uLT_var_LC_i64"
	.skip 1
	.ascii	"LT_param_LC_I"
	.skip 1
	.ascii	"uLT_param_LC_I"
	.skip 1
	.ascii	"LT_var_LC_I"
	.skip 1
	.ascii	"uLT_var_LC_I"
	.skip 1
	.ascii	"LT_param_LC_i16"
	.skip 1
	.ascii	"uLT_param_LC_i16"
	.skip 1
	.ascii	"LT_var_LC_i16"
	.skip 1
	.ascii	"uLT_var_LC_i16"
	.skip 1
	.ascii	"LT_param_LC_i32"
	.skip 1
	.ascii	"uLT_param_LC_i32"
	.skip 1
	.ascii	"LT_var_LC_i32"
	.skip 1
	.ascii	"uLT_var_LC_i32"
	.skip 1
	.ascii	"LT_param_LC_LC"
	.skip 1
	.ascii	"uLT_param_LC_LC"
	.skip 1
	.ascii	"LT_var_LC_LC"
	.skip 1
	.ascii	"uLT_var_LC_LC"
	.skip 1
	.ascii	"LT_param_LC_u64"
	.skip 1
	.ascii	"uLT_param_LC_u64"
	.skip 1
	.ascii	"LT_var_LC_u64"
	.skip 1
	.ascii	"uLT_var_LC_u64"
	.skip 1
	.ascii	"LT_param_LC_u16"
	.skip 1
	.ascii	"uLT_param_LC_u16"
	.skip 1
	.ascii	"LT_var_LC_u16"
	.skip 1
	.ascii	"uLT_var_LC_u16"
	.skip 1
	.ascii	"LT_param_u64_u32"
	.skip 1
	.ascii	"uLT_param_u64_u32"
	.skip 1
	.ascii	"LT_var_u64_u32"
	.skip 1
	.ascii	"uLT_var_u64_u32"
	.skip 1
	.ascii	"LT_param_u64_i8"
	.skip 1
	.ascii	"uLT_param_u64_i8"
	.skip 1
	.ascii	"LT_var_u64_i8"
	.skip 1
	.ascii	"uLT_var_u64_i8"
	.skip 1
	.ascii	"LT_param_u64_L"
	.skip 1
	.ascii	"uLT_param_u64_L"
	.skip 1
	.ascii	"LT_var_u64_L"
	.skip 1
	.ascii	"uLT_var_u64_L"
	.skip 1
	.ascii	"LT_param_u64_u8"
	.skip 1
	.ascii	"uLT_param_u64_u8"
	.skip 1
	.ascii	"LT_var_u64_u8"
	.skip 1
	.ascii	"uLT_var_u64_u8"
	.skip 1
	.ascii	"LT_param_u64_C"
	.skip 1
	.ascii	"uLT_param_u64_C"
	.skip 1
	.ascii	"LT_var_u64_C"
	.skip 1
	.ascii	"uLT_var_u64_C"
	.skip 1
	.ascii	"LT_param_u64_i64"
	.skip 1
	.ascii	"uLT_param_u64_i64"
	.skip 1
	.ascii	"LT_var_u64_i64"
	.skip 1
	.ascii	"uLT_var_u64_i64"
	.skip 1
	.ascii	"LT_param_u64_I"
	.skip 1
	.ascii	"uLT_param_u64_I"
	.skip 1
	.ascii	"LT_var_u64_I"
	.skip 1
	.ascii	"uLT_var_u64_I"
	.skip 1
	.ascii	"LT_param_u64_i16"
	.skip 1
	.ascii	"uLT_param_u64_i16"
	.skip 1
	.ascii	"LT_var_u64_i16"
	.skip 1
	.ascii	"uLT_var_u64_i16"
	.skip 1
	.ascii	"LT_param_u64_i32"
	.skip 1
	.ascii	"uLT_param_u64_i32"
	.skip 1
	.ascii	"LT_var_u64_i32"
	.skip 1
	.ascii	"uLT_var_u64_i32"
	.skip 1
	.ascii	"LT_param_u64_LC"
	.skip 1
	.ascii	"uLT_param_u64_LC"
	.skip 1
	.ascii	"LT_var_u64_LC"
	.skip 1
	.ascii	"uLT_var_u64_LC"
	.skip 1
	.ascii	"LT_param_u64_u64"
	.skip 1
	.ascii	"uLT_param_u64_u64"
	.skip 1
	.ascii	"LT_var_u64_u64"
	.skip 1
	.ascii	"uLT_var_u64_u64"
	.skip 1
	.ascii	"LT_param_u64_u16"
	.skip 1
	.ascii	"uLT_param_u64_u16"
	.skip 1
	.ascii	"LT_var_u64_u16"
	.skip 1
	.ascii	"uLT_var_u64_u16"
	.skip 1
	.ascii	"LT_param_u16_u32"
	.skip 1
	.ascii	"uLT_param_u16_u32"
	.skip 1
	.ascii	"LT_var_u16_u32"
	.skip 1
	.ascii	"uLT_var_u16_u32"
	.skip 1
	.ascii	"LT_param_u16_i8"
	.skip 1
	.ascii	"uLT_param_u16_i8"
	.skip 1
	.ascii	"LT_var_u16_i8"
	.skip 1
	.ascii	"uLT_var_u16_i8"
	.skip 1
	.ascii	"LT_param_u16_L"
	.skip 1
	.ascii	"uLT_param_u16_L"
	.skip 1
	.ascii	"LT_var_u16_L"
	.skip 1
	.ascii	"uLT_var_u16_L"
	.skip 1
	.ascii	"LT_param_u16_u8"
	.skip 1
	.ascii	"uLT_param_u16_u8"
	.skip 1
	.ascii	"LT_var_u16_u8"
	.skip 1
	.ascii	"uLT_var_u16_u8"
	.skip 1
	.ascii	"LT_param_u16_C"
	.skip 1
	.ascii	"uLT_param_u16_C"
	.skip 1
	.ascii	"LT_var_u16_C"
	.skip 1
	.ascii	"uLT_var_u16_C"
	.skip 1
	.ascii	"LT_param_u16_i64"
	.skip 1
	.ascii	"uLT_param_u16_i64"
	.skip 1
	.ascii	"LT_var_u16_i64"
	.skip 1
	.ascii	"uLT_var_u16_i64"
	.skip 1
	.ascii	"LT_param_u16_I"
	.skip 1
	.ascii	"uLT_param_u16_I"
	.skip 1
	.ascii	"LT_var_u16_I"
	.skip 1
	.ascii	"uLT_var_u16_I"
	.skip 1
	.ascii	"LT_param_u16_i16"
	.skip 1
	.ascii	"uLT_param_u16_i16"
	.skip 1
	.ascii	"LT_var_u16_i16"
	.skip 1
	.ascii	"uLT_var_u16_i16"
	.skip 1
	.ascii	"LT_param_u16_i32"
	.skip 1
	.ascii	"uLT_param_u16_i32"
	.skip 1
	.ascii	"LT_var_u16_i32"
	.skip 1
	.ascii	"uLT_var_u16_i32"
	.skip 1
	.ascii	"LT_param_u16_LC"
	.skip 1
	.ascii	"uLT_param_u16_LC"
	.skip 1
	.ascii	"LT_var_u16_LC"
	.skip 1
	.ascii	"uLT_var_u16_LC"
	.skip 1
	.ascii	"LT_param_u16_u64"
	.skip 1
	.ascii	"uLT_param_u16_u64"
	.skip 1
	.ascii	"LT_var_u16_u64"
	.skip 1
	.ascii	"uLT_var_u16_u64"
	.skip 1
	.ascii	"LT_param_u16_u16"
	.skip 1
	.ascii	"uLT_param_u16_u16"
	.skip 1
	.ascii	"LT_var_u16_u16"
	.skip 1
	.ascii	"uLT_var_u16_u16"
	.skip 3
	.long	LT_M3
	.long	L_1
	.long	LT__LT_param_f32_f32
	.long	L_1+6
	.long	LT__LT_var_f32_f32
	.long	L_1+23
	.long	LT__LT_param_u32_u32
	.long	L_1+38
	.long	LT__uLT_param_u32_u32
	.long	L_1+55
	.long	LT__LT_var_u32_u32
	.long	L_1+73
	.long	LT__uLT_var_u32_u32
	.long	L_1+88
	.long	LT__LT_param_u32_i8
	.long	L_1+104
	.long	LT__uLT_param_u32_i8
	.long	L_1+120
	.long	LT__LT_var_u32_i8
	.long	L_1+137
	.long	LT__uLT_var_u32_i8
	.long	L_1+151
	.long	LT__LT_param_u32_L
	.long	L_1+166
	.long	LT__uLT_param_u32_L
	.long	L_1+181
	.long	LT__LT_var_u32_L
	.long	L_1+197
	.long	LT__uLT_var_u32_L
	.long	L_1+210
	.long	LT__LT_param_u32_u8
	.long	L_1+224
	.long	LT__uLT_param_u32_u8
	.long	L_1+240
	.long	LT__LT_var_u32_u8
	.long	L_1+257
	.long	LT__uLT_var_u32_u8
	.long	L_1+271
	.long	LT__LT_param_u32_C
	.long	L_1+286
	.long	LT__uLT_param_u32_C
	.long	L_1+301
	.long	LT__LT_var_u32_C
	.long	L_1+317
	.long	LT__uLT_var_u32_C
	.long	L_1+330
	.long	LT__LT_param_u32_i64
	.long	L_1+344
	.long	LT__uLT_param_u32_i64
	.long	L_1+361
	.long	LT__LT_var_u32_i64
	.long	L_1+379
	.long	LT__uLT_var_u32_i64
	.long	L_1+394
	.long	LT__LT_param_u32_I
	.long	L_1+410
	.long	LT__uLT_param_u32_I
	.long	L_1+425
	.long	LT__LT_var_u32_I
	.long	L_1+441
	.long	LT__uLT_var_u32_I
	.long	L_1+454
	.long	LT__LT_param_u32_i16
	.long	L_1+468
	.long	LT__uLT_param_u32_i16
	.long	L_1+485
	.long	LT__LT_var_u32_i16
	.long	L_1+503
	.long	LT__uLT_var_u32_i16
	.long	L_1+518
	.long	LT__LT_param_u32_i32
	.long	L_1+534
	.long	LT__uLT_param_u32_i32
	.long	L_1+551
	.long	LT__LT_var_u32_i32
	.long	L_1+569
	.long	LT__uLT_var_u32_i32
	.long	L_1+584
	.long	LT__LT_param_u32_LC
	.long	L_1+600
	.long	LT__uLT_param_u32_LC
	.long	L_1+616
	.long	LT__LT_var_u32_LC
	.long	L_1+633
	.long	LT__uLT_var_u32_LC
	.long	L_1+647
	.long	LT__LT_param_u32_u64
	.long	L_1+662
	.long	LT__uLT_param_u32_u64
	.long	L_1+679
	.long	LT__LT_var_u32_u64
	.long	L_1+697
	.long	LT__uLT_var_u32_u64
	.long	L_1+712
	.long	LT__LT_param_u32_u16
	.long	L_1+728
	.long	LT__uLT_param_u32_u16
	.long	L_1+745
	.long	LT__LT_var_u32_u16
	.long	L_1+763
	.long	LT__uLT_var_u32_u16
	.long	L_1+778
	.long	LT__LT_param_i8_u32
	.long	L_1+794
	.long	LT__uLT_param_i8_u32
	.long	L_1+810
	.long	LT__LT_var_i8_u32
	.long	L_1+827
	.long	LT__uLT_var_i8_u32
	.long	L_1+841
	.long	LT__LT_param_i8_i8
	.long	L_1+856
	.long	LT__uLT_param_i8_i8
	.long	L_1+871
	.long	LT__LT_var_i8_i8
	.long	L_1+887
	.long	LT__uLT_var_i8_i8
	.long	L_1+900
	.long	LT__LT_param_i8_L
	.long	L_1+914
	.long	LT__uLT_param_i8_L
	.long	L_1+928
	.long	LT__LT_var_i8_L
	.long	L_1+943
	.long	LT__uLT_var_i8_L
	.long	L_1+955
	.long	LT__LT_param_i8_u8
	.long	L_1+968
	.long	LT__uLT_param_i8_u8
	.long	L_1+983
	.long	LT__LT_var_i8_u8
	.long	L_1+999
	.long	LT__uLT_var_i8_u8
	.long	L_1+1012
	.long	LT__LT_param_i8_C
	.long	L_1+1026
	.long	LT__uLT_param_i8_C
	.long	L_1+1040
	.long	LT__LT_var_i8_C
	.long	L_1+1055
	.long	LT__uLT_var_i8_C
	.long	L_1+1067
	.long	LT__LT_param_i8_i64
	.long	L_1+1080
	.long	LT__uLT_param_i8_i64
	.long	L_1+1096
	.long	LT__LT_var_i8_i64
	.long	L_1+1113
	.long	LT__uLT_var_i8_i64
	.long	L_1+1127
	.long	LT__LT_param_i8_I
	.long	L_1+1142
	.long	LT__uLT_param_i8_I
	.long	L_1+1156
	.long	LT__LT_var_i8_I
	.long	L_1+1171
	.long	LT__uLT_var_i8_I
	.long	L_1+1183
	.long	LT__LT_param_i8_i16
	.long	L_1+1196
	.long	LT__uLT_param_i8_i16
	.long	L_1+1212
	.long	LT__LT_var_i8_i16
	.long	L_1+1229
	.long	LT__uLT_var_i8_i16
	.long	L_1+1243
	.long	LT__LT_param_i8_i32
	.long	L_1+1258
	.long	LT__uLT_param_i8_i32
	.long	L_1+1274
	.long	LT__LT_var_i8_i32
	.long	L_1+1291
	.long	LT__uLT_var_i8_i32
	.long	L_1+1305
	.long	LT__LT_param_i8_LC
	.long	L_1+1320
	.long	LT__uLT_param_i8_LC
	.long	L_1+1335
	.long	LT__LT_var_i8_LC
	.long	L_1+1351
	.long	LT__uLT_var_i8_LC
	.long	L_1+1364
	.long	LT__LT_param_i8_u64
	.long	L_1+1378
	.long	LT__uLT_param_i8_u64
	.long	L_1+1394
	.long	LT__LT_var_i8_u64
	.long	L_1+1411
	.long	LT__uLT_var_i8_u64
	.long	L_1+1425
	.long	LT__LT_param_i8_u16
	.long	L_1+1440
	.long	LT__uLT_param_i8_u16
	.long	L_1+1456
	.long	LT__LT_var_i8_u16
	.long	L_1+1473
	.long	LT__uLT_var_i8_u16
	.long	L_1+1487
	.long	LT__LT_param_L_u32
	.long	L_1+1502
	.long	LT__uLT_param_L_u32
	.long	L_1+1517
	.long	LT__LT_var_L_u32
	.long	L_1+1533
	.long	LT__uLT_var_L_u32
	.long	L_1+1546
	.long	LT__LT_param_L_i8
	.long	L_1+1560
	.long	LT__uLT_param_L_i8
	.long	L_1+1574
	.long	LT__LT_var_L_i8
	.long	L_1+1589
	.long	LT__uLT_var_L_i8
	.long	L_1+1601
	.long	LT__LT_param_L_L
	.long	L_1+1614
	.long	LT__uLT_param_L_L
	.long	L_1+1627
	.long	LT__LT_var_L_L
	.long	L_1+1641
	.long	LT__uLT_var_L_L
	.long	L_1+1652
	.long	LT__LT_param_L_u8
	.long	L_1+1664
	.long	LT__uLT_param_L_u8
	.long	L_1+1678
	.long	LT__LT_var_L_u8
	.long	L_1+1693
	.long	LT__uLT_var_L_u8
	.long	L_1+1705
	.long	LT__LT_param_L_C
	.long	L_1+1718
	.long	LT__uLT_param_L_C
	.long	L_1+1731
	.long	LT__LT_var_L_C
	.long	L_1+1745
	.long	LT__uLT_var_L_C
	.long	L_1+1756
	.long	LT__LT_param_L_i64
	.long	L_1+1768
	.long	LT__uLT_param_L_i64
	.long	L_1+1783
	.long	LT__LT_var_L_i64
	.long	L_1+1799
	.long	LT__uLT_var_L_i64
	.long	L_1+1812
	.long	LT__LT_param_L_I
	.long	L_1+1826
	.long	LT__uLT_param_L_I
	.long	L_1+1839
	.long	LT__LT_var_L_I
	.long	L_1+1853
	.long	LT__uLT_var_L_I
	.long	L_1+1864
	.long	LT__LT_param_L_i16
	.long	L_1+1876
	.long	LT__uLT_param_L_i16
	.long	L_1+1891
	.long	LT__LT_var_L_i16
	.long	L_1+1907
	.long	LT__uLT_var_L_i16
	.long	L_1+1920
	.long	LT__LT_param_L_i32
	.long	L_1+1934
	.long	LT__uLT_param_L_i32
	.long	L_1+1949
	.long	LT__LT_var_L_i32
	.long	L_1+1965
	.long	LT__uLT_var_L_i32
	.long	L_1+1978
	.long	LT__LT_param_L_LC
	.long	L_1+1992
	.long	LT__uLT_param_L_LC
	.long	L_1+2006
	.long	LT__LT_var_L_LC
	.long	L_1+2021
	.long	LT__uLT_var_L_LC
	.long	L_1+2033
	.long	LT__LT_param_L_u64
	.long	L_1+2046
	.long	LT__uLT_param_L_u64
	.long	L_1+2061
	.long	LT__LT_var_L_u64
	.long	L_1+2077
	.long	LT__uLT_var_L_u64
	.long	L_1+2090
	.long	LT__LT_param_L_u16
	.long	L_1+2104
	.long	LT__uLT_param_L_u16
	.long	L_1+2119
	.long	LT__LT_var_L_u16
	.long	L_1+2135
	.long	LT__uLT_var_L_u16
	.long	L_1+2148
	.long	LT__LT_param_u8_u32
	.long	L_1+2162
	.long	LT__uLT_param_u8_u32
	.long	L_1+2178
	.long	LT__LT_var_u8_u32
	.long	L_1+2195
	.long	LT__uLT_var_u8_u32
	.long	L_1+2209
	.long	LT__LT_param_u8_i8
	.long	L_1+2224
	.long	LT__uLT_param_u8_i8
	.long	L_1+2239
	.long	LT__LT_var_u8_i8
	.long	L_1+2255
	.long	LT__uLT_var_u8_i8
	.long	L_1+2268
	.long	LT__LT_param_u8_L
	.long	L_1+2282
	.long	LT__uLT_param_u8_L
	.long	L_1+2296
	.long	LT__LT_var_u8_L
	.long	L_1+2311
	.long	LT__uLT_var_u8_L
	.long	L_1+2323
	.long	LT__LT_param_u8_u8
	.long	L_1+2336
	.long	LT__uLT_param_u8_u8
	.long	L_1+2351
	.long	LT__LT_var_u8_u8
	.long	L_1+2367
	.long	LT__uLT_var_u8_u8
	.long	L_1+2380
	.long	LT__LT_param_u8_C
	.long	L_1+2394
	.long	LT__uLT_param_u8_C
	.long	L_1+2408
	.long	LT__LT_var_u8_C
	.long	L_1+2423
	.long	LT__uLT_var_u8_C
	.long	L_1+2435
	.long	LT__LT_param_u8_i64
	.long	L_1+2448
	.long	LT__uLT_param_u8_i64
	.long	L_1+2464
	.long	LT__LT_var_u8_i64
	.long	L_1+2481
	.long	LT__uLT_var_u8_i64
	.long	L_1+2495
	.long	LT__LT_param_u8_I
	.long	L_1+2510
	.long	LT__uLT_param_u8_I
	.long	L_1+2524
	.long	LT__LT_var_u8_I
	.long	L_1+2539
	.long	LT__uLT_var_u8_I
	.long	L_1+2551
	.long	LT__LT_param_u8_i16
	.long	L_1+2564
	.long	LT__uLT_param_u8_i16
	.long	L_1+2580
	.long	LT__LT_var_u8_i16
	.long	L_1+2597
	.long	LT__uLT_var_u8_i16
	.long	L_1+2611
	.long	LT__LT_param_u8_i32
	.long	L_1+2626
	.long	LT__uLT_param_u8_i32
	.long	L_1+2642
	.long	LT__LT_var_u8_i32
	.long	L_1+2659
	.long	LT__uLT_var_u8_i32
	.long	L_1+2673
	.long	LT__LT_param_u8_LC
	.long	L_1+2688
	.long	LT__uLT_param_u8_LC
	.long	L_1+2703
	.long	LT__LT_var_u8_LC
	.long	L_1+2719
	.long	LT__uLT_var_u8_LC
	.long	L_1+2732
	.long	LT__LT_param_u8_u64
	.long	L_1+2746
	.long	LT__uLT_param_u8_u64
	.long	L_1+2762
	.long	LT__LT_var_u8_u64
	.long	L_1+2779
	.long	LT__uLT_var_u8_u64
	.long	L_1+2793
	.long	LT__LT_param_u8_u16
	.long	L_1+2808
	.long	LT__uLT_param_u8_u16
	.long	L_1+2824
	.long	LT__LT_var_u8_u16
	.long	L_1+2841
	.long	LT__uLT_var_u8_u16
	.long	L_1+2855
	.long	LT__LT_param_f64_f64
	.long	L_1+2870
	.long	LT__LT_var_f64_f64
	.long	L_1+2887
	.long	LT__LT_param_C_u32
	.long	L_1+2902
	.long	LT__uLT_param_C_u32
	.long	L_1+2917
	.long	LT__LT_var_C_u32
	.long	L_1+2933
	.long	LT__uLT_var_C_u32
	.long	L_1+2946
	.long	LT__LT_param_C_i8
	.long	L_1+2960
	.long	LT__uLT_param_C_i8
	.long	L_1+2974
	.long	LT__LT_var_C_i8
	.long	L_1+2989
	.long	LT__uLT_var_C_i8
	.long	L_1+3001
	.long	LT__LT_param_C_L
	.long	L_1+3014
	.long	LT__uLT_param_C_L
	.long	L_1+3027
	.long	LT__LT_var_C_L
	.long	L_1+3041
	.long	LT__uLT_var_C_L
	.long	L_1+3052
	.long	LT__LT_param_C_u8
	.long	L_1+3064
	.long	LT__uLT_param_C_u8
	.long	L_1+3078
	.long	LT__LT_var_C_u8
	.long	L_1+3093
	.long	LT__uLT_var_C_u8
	.long	L_1+3105
	.long	LT__LT_param_C_C
	.long	L_1+3118
	.long	LT__uLT_param_C_C
	.long	L_1+3131
	.long	LT__LT_var_C_C
	.long	L_1+3145
	.long	LT__uLT_var_C_C
	.long	L_1+3156
	.long	LT__LT_param_C_i64
	.long	L_1+3168
	.long	LT__uLT_param_C_i64
	.long	L_1+3183
	.long	LT__LT_var_C_i64
	.long	L_1+3199
	.long	LT__uLT_var_C_i64
	.long	L_1+3212
	.long	LT__LT_param_C_I
	.long	L_1+3226
	.long	LT__uLT_param_C_I
	.long	L_1+3239
	.long	LT__LT_var_C_I
	.long	L_1+3253
	.long	LT__uLT_var_C_I
	.long	L_1+3264
	.long	LT__LT_param_C_i16
	.long	L_1+3276
	.long	LT__uLT_param_C_i16
	.long	L_1+3291
	.long	LT__LT_var_C_i16
	.long	L_1+3307
	.long	LT__uLT_var_C_i16
	.long	L_1+3320
	.long	LT__LT_param_C_i32
	.long	L_1+3334
	.long	LT__uLT_param_C_i32
	.long	L_1+3349
	.long	LT__LT_var_C_i32
	.long	L_1+3365
	.long	LT__uLT_var_C_i32
	.long	L_1+3378
	.long	LT__LT_param_C_LC
	.long	L_1+3392
	.long	LT__uLT_param_C_LC
	.long	L_1+3406
	.long	LT__LT_var_C_LC
	.long	L_1+3421
	.long	LT__uLT_var_C_LC
	.long	L_1+3433
	.long	LT__LT_param_C_u64
	.long	L_1+3446
	.long	LT__uLT_param_C_u64
	.long	L_1+3461
	.long	LT__LT_var_C_u64
	.long	L_1+3477
	.long	LT__uLT_var_C_u64
	.long	L_1+3490
	.long	LT__LT_param_C_u16
	.long	L_1+3504
	.long	LT__uLT_param_C_u16
	.long	L_1+3519
	.long	LT__LT_var_C_u16
	.long	L_1+3535
	.long	LT__uLT_var_C_u16
	.long	L_1+3548
	.long	LT__LT_param_i64_u32
	.long	L_1+3562
	.long	LT__uLT_param_i64_u32
	.long	L_1+3579
	.long	LT__LT_var_i64_u32
	.long	L_1+3597
	.long	LT__uLT_var_i64_u32
	.long	L_1+3612
	.long	LT__LT_param_i64_i8
	.long	L_1+3628
	.long	LT__uLT_param_i64_i8
	.long	L_1+3644
	.long	LT__LT_var_i64_i8
	.long	L_1+3661
	.long	LT__uLT_var_i64_i8
	.long	L_1+3675
	.long	LT__LT_param_i64_L
	.long	L_1+3690
	.long	LT__uLT_param_i64_L
	.long	L_1+3705
	.long	LT__LT_var_i64_L
	.long	L_1+3721
	.long	LT__uLT_var_i64_L
	.long	L_1+3734
	.long	LT__LT_param_i64_u8
	.long	L_1+3748
	.long	LT__uLT_param_i64_u8
	.long	L_1+3764
	.long	LT__LT_var_i64_u8
	.long	L_1+3781
	.long	LT__uLT_var_i64_u8
	.long	L_1+3795
	.long	LT__LT_param_i64_C
	.long	L_1+3810
	.long	LT__uLT_param_i64_C
	.long	L_1+3825
	.long	LT__LT_var_i64_C
	.long	L_1+3841
	.long	LT__uLT_var_i64_C
	.long	L_1+3854
	.long	LT__LT_param_i64_i64
	.long	L_1+3868
	.long	LT__uLT_param_i64_i64
	.long	L_1+3885
	.long	LT__LT_var_i64_i64
	.long	L_1+3903
	.long	LT__uLT_var_i64_i64
	.long	L_1+3918
	.long	LT__LT_param_i64_I
	.long	L_1+3934
	.long	LT__uLT_param_i64_I
	.long	L_1+3949
	.long	LT__LT_var_i64_I
	.long	L_1+3965
	.long	LT__uLT_var_i64_I
	.long	L_1+3978
	.long	LT__LT_param_i64_i16
	.long	L_1+3992
	.long	LT__uLT_param_i64_i16
	.long	L_1+4009
	.long	LT__LT_var_i64_i16
	.long	L_1+4027
	.long	LT__uLT_var_i64_i16
	.long	L_1+4042
	.long	LT__LT_param_i64_i32
	.long	L_1+4058
	.long	LT__uLT_param_i64_i32
	.long	L_1+4075
	.long	LT__LT_var_i64_i32
	.long	L_1+4093
	.long	LT__uLT_var_i64_i32
	.long	L_1+4108
	.long	LT__LT_param_i64_LC
	.long	L_1+4124
	.long	LT__uLT_param_i64_LC
	.long	L_1+4140
	.long	LT__LT_var_i64_LC
	.long	L_1+4157
	.long	LT__uLT_var_i64_LC
	.long	L_1+4171
	.long	LT__LT_param_i64_u64
	.long	L_1+4186
	.long	LT__uLT_param_i64_u64
	.long	L_1+4203
	.long	LT__LT_var_i64_u64
	.long	L_1+4221
	.long	LT__uLT_var_i64_u64
	.long	L_1+4236
	.long	LT__LT_param_i64_u16
	.long	L_1+4252
	.long	LT__uLT_param_i64_u16
	.long	L_1+4269
	.long	LT__LT_var_i64_u16
	.long	L_1+4287
	.long	LT__uLT_var_i64_u16
	.long	L_1+4302
	.long	LT__LT_param_I_u32
	.long	L_1+4318
	.long	LT__uLT_param_I_u32
	.long	L_1+4333
	.long	LT__LT_var_I_u32
	.long	L_1+4349
	.long	LT__uLT_var_I_u32
	.long	L_1+4362
	.long	LT__LT_param_I_i8
	.long	L_1+4376
	.long	LT__uLT_param_I_i8
	.long	L_1+4390
	.long	LT__LT_var_I_i8
	.long	L_1+4405
	.long	LT__uLT_var_I_i8
	.long	L_1+4417
	.long	LT__LT_param_I_L
	.long	L_1+4430
	.long	LT__uLT_param_I_L
	.long	L_1+4443
	.long	LT__LT_var_I_L
	.long	L_1+4457
	.long	LT__uLT_var_I_L
	.long	L_1+4468
	.long	LT__LT_param_I_u8
	.long	L_1+4480
	.long	LT__uLT_param_I_u8
	.long	L_1+4494
	.long	LT__LT_var_I_u8
	.long	L_1+4509
	.long	LT__uLT_var_I_u8
	.long	L_1+4521
	.long	LT__LT_param_I_C
	.long	L_1+4534
	.long	LT__uLT_param_I_C
	.long	L_1+4547
	.long	LT__LT_var_I_C
	.long	L_1+4561
	.long	LT__uLT_var_I_C
	.long	L_1+4572
	.long	LT__LT_param_I_i64
	.long	L_1+4584
	.long	LT__uLT_param_I_i64
	.long	L_1+4599
	.long	LT__LT_var_I_i64
	.long	L_1+4615
	.long	LT__uLT_var_I_i64
	.long	L_1+4628
	.long	LT__LT_param_I_I
	.long	L_1+4642
	.long	LT__uLT_param_I_I
	.long	L_1+4655
	.long	LT__LT_var_I_I
	.long	L_1+4669
	.long	LT__uLT_var_I_I
	.long	L_1+4680
	.long	LT__LT_param_I_i16
	.long	L_1+4692
	.long	LT__uLT_param_I_i16
	.long	L_1+4707
	.long	LT__LT_var_I_i16
	.long	L_1+4723
	.long	LT__uLT_var_I_i16
	.long	L_1+4736
	.long	LT__LT_param_I_i32
	.long	L_1+4750
	.long	LT__uLT_param_I_i32
	.long	L_1+4765
	.long	LT__LT_var_I_i32
	.long	L_1+4781
	.long	LT__uLT_var_I_i32
	.long	L_1+4794
	.long	LT__LT_param_I_LC
	.long	L_1+4808
	.long	LT__uLT_param_I_LC
	.long	L_1+4822
	.long	LT__LT_var_I_LC
	.long	L_1+4837
	.long	LT__uLT_var_I_LC
	.long	L_1+4849
	.long	LT__LT_param_I_u64
	.long	L_1+4862
	.long	LT__uLT_param_I_u64
	.long	L_1+4877
	.long	LT__LT_var_I_u64
	.long	L_1+4893
	.long	LT__uLT_var_I_u64
	.long	L_1+4906
	.long	LT__LT_param_I_u16
	.long	L_1+4920
	.long	LT__uLT_param_I_u16
	.long	L_1+4935
	.long	LT__LT_var_I_u16
	.long	L_1+4951
	.long	LT__uLT_var_I_u16
	.long	L_1+4964
	.long	LT__LT_param_i16_u32
	.long	L_1+4978
	.long	LT__uLT_param_i16_u32
	.long	L_1+4995
	.long	LT__LT_var_i16_u32
	.long	L_1+5013
	.long	LT__uLT_var_i16_u32
	.long	L_1+5028
	.long	LT__LT_param_i16_i8
	.long	L_1+5044
	.long	LT__uLT_param_i16_i8
	.long	L_1+5060
	.long	LT__LT_var_i16_i8
	.long	L_1+5077
	.long	LT__uLT_var_i16_i8
	.long	L_1+5091
	.long	LT__LT_param_i16_L
	.long	L_1+5106
	.long	LT__uLT_param_i16_L
	.long	L_1+5121
	.long	LT__LT_var_i16_L
	.long	L_1+5137
	.long	LT__uLT_var_i16_L
	.long	L_1+5150
	.long	LT__LT_param_i16_u8
	.long	L_1+5164
	.long	LT__uLT_param_i16_u8
	.long	L_1+5180
	.long	LT__LT_var_i16_u8
	.long	L_1+5197
	.long	LT__uLT_var_i16_u8
	.long	L_1+5211
	.long	LT__LT_param_i16_C
	.long	L_1+5226
	.long	LT__uLT_param_i16_C
	.long	L_1+5241
	.long	LT__LT_var_i16_C
	.long	L_1+5257
	.long	LT__uLT_var_i16_C
	.long	L_1+5270
	.long	LT__LT_param_i16_i64
	.long	L_1+5284
	.long	LT__uLT_param_i16_i64
	.long	L_1+5301
	.long	LT__LT_var_i16_i64
	.long	L_1+5319
	.long	LT__uLT_var_i16_i64
	.long	L_1+5334
	.long	LT__LT_param_i16_I
	.long	L_1+5350
	.long	LT__uLT_param_i16_I
	.long	L_1+5365
	.long	LT__LT_var_i16_I
	.long	L_1+5381
	.long	LT__uLT_var_i16_I
	.long	L_1+5394
	.long	LT__LT_param_i16_i16
	.long	L_1+5408
	.long	LT__uLT_param_i16_i16
	.long	L_1+5425
	.long	LT__LT_var_i16_i16
	.long	L_1+5443
	.long	LT__uLT_var_i16_i16
	.long	L_1+5458
	.long	LT__LT_param_i16_i32
	.long	L_1+5474
	.long	LT__uLT_param_i16_i32
	.long	L_1+5491
	.long	LT__LT_var_i16_i32
	.long	L_1+5509
	.long	LT__uLT_var_i16_i32
	.long	L_1+5524
	.long	LT__LT_param_i16_LC
	.long	L_1+5540
	.long	LT__uLT_param_i16_LC
	.long	L_1+5556
	.long	LT__LT_var_i16_LC
	.long	L_1+5573
	.long	LT__uLT_var_i16_LC
	.long	L_1+5587
	.long	LT__LT_param_i16_u64
	.long	L_1+5602
	.long	LT__uLT_param_i16_u64
	.long	L_1+5619
	.long	LT__LT_var_i16_u64
	.long	L_1+5637
	.long	LT__uLT_var_i16_u64
	.long	L_1+5652
	.long	LT__LT_param_i16_u16
	.long	L_1+5668
	.long	LT__uLT_param_i16_u16
	.long	L_1+5685
	.long	LT__LT_var_i16_u16
	.long	L_1+5703
	.long	LT__uLT_var_i16_u16
	.long	L_1+5718
	.long	LT__LT_param_i32_u32
	.long	L_1+5734
	.long	LT__uLT_param_i32_u32
	.long	L_1+5751
	.long	LT__LT_var_i32_u32
	.long	L_1+5769
	.long	LT__uLT_var_i32_u32
	.long	L_1+5784
	.long	LT__LT_param_i32_i8
	.long	L_1+5800
	.long	LT__uLT_param_i32_i8
	.long	L_1+5816
	.long	LT__LT_var_i32_i8
	.long	L_1+5833
	.long	LT__uLT_var_i32_i8
	.long	L_1+5847
	.long	LT__LT_param_i32_L
	.long	L_1+5862
	.long	LT__uLT_param_i32_L
	.long	L_1+5877
	.long	LT__LT_var_i32_L
	.long	L_1+5893
	.long	LT__uLT_var_i32_L
	.long	L_1+5906
	.long	LT__LT_param_i32_u8
	.long	L_1+5920
	.long	LT__uLT_param_i32_u8
	.long	L_1+5936
	.long	LT__LT_var_i32_u8
	.long	L_1+5953
	.long	LT__uLT_var_i32_u8
	.long	L_1+5967
	.long	LT__LT_param_i32_C
	.long	L_1+5982
	.long	LT__uLT_param_i32_C
	.long	L_1+5997
	.long	LT__LT_var_i32_C
	.long	L_1+6013
	.long	LT__uLT_var_i32_C
	.long	L_1+6026
	.long	LT__LT_param_i32_i64
	.long	L_1+6040
	.long	LT__uLT_param_i32_i64
	.long	L_1+6057
	.long	LT__LT_var_i32_i64
	.long	L_1+6075
	.long	LT__uLT_var_i32_i64
	.long	L_1+6090
	.long	LT__LT_param_i32_I
	.long	L_1+6106
	.long	LT__uLT_param_i32_I
	.long	L_1+6121
	.long	LT__LT_var_i32_I
	.long	L_1+6137
	.long	LT__uLT_var_i32_I
	.long	L_1+6150
	.long	LT__LT_param_i32_i16
	.long	L_1+6164
	.long	LT__uLT_param_i32_i16
	.long	L_1+6181
	.long	LT__LT_var_i32_i16
	.long	L_1+6199
	.long	LT__uLT_var_i32_i16
	.long	L_1+6214
	.long	LT__LT_param_i32_i32
	.long	L_1+6230
	.long	LT__uLT_param_i32_i32
	.long	L_1+6247
	.long	LT__LT_var_i32_i32
	.long	L_1+6265
	.long	LT__uLT_var_i32_i32
	.long	L_1+6280
	.long	LT__LT_param_i32_LC
	.long	L_1+6296
	.long	LT__uLT_param_i32_LC
	.long	L_1+6312
	.long	LT__LT_var_i32_LC
	.long	L_1+6329
	.long	LT__uLT_var_i32_LC
	.long	L_1+6343
	.long	LT__LT_param_i32_u64
	.long	L_1+6358
	.long	LT__uLT_param_i32_u64
	.long	L_1+6375
	.long	LT__LT_var_i32_u64
	.long	L_1+6393
	.long	LT__uLT_var_i32_u64
	.long	L_1+6408
	.long	LT__LT_param_i32_u16
	.long	L_1+6424
	.long	LT__uLT_param_i32_u16
	.long	L_1+6441
	.long	LT__LT_var_i32_u16
	.long	L_1+6459
	.long	LT__uLT_var_i32_u16
	.long	L_1+6474
	.long	LT__LT_param_LC_u32
	.long	L_1+6490
	.long	LT__uLT_param_LC_u32
	.long	L_1+6506
	.long	LT__LT_var_LC_u32
	.long	L_1+6523
	.long	LT__uLT_var_LC_u32
	.long	L_1+6537
	.long	LT__LT_param_LC_i8
	.long	L_1+6552
	.long	LT__uLT_param_LC_i8
	.long	L_1+6567
	.long	LT__LT_var_LC_i8
	.long	L_1+6583
	.long	LT__uLT_var_LC_i8
	.long	L_1+6596
	.long	LT__LT_param_LC_L
	.long	L_1+6610
	.long	LT__uLT_param_LC_L
	.long	L_1+6624
	.long	LT__LT_var_LC_L
	.long	L_1+6639
	.long	LT__uLT_var_LC_L
	.long	L_1+6651
	.long	LT__LT_param_LC_u8
	.long	L_1+6664
	.long	LT__uLT_param_LC_u8
	.long	L_1+6679
	.long	LT__LT_var_LC_u8
	.long	L_1+6695
	.long	LT__uLT_var_LC_u8
	.long	L_1+6708
	.long	LT__LT_param_LC_C
	.long	L_1+6722
	.long	LT__uLT_param_LC_C
	.long	L_1+6736
	.long	LT__LT_var_LC_C
	.long	L_1+6751
	.long	LT__uLT_var_LC_C
	.long	L_1+6763
	.long	LT__LT_param_LC_i64
	.long	L_1+6776
	.long	LT__uLT_param_LC_i64
	.long	L_1+6792
	.long	LT__LT_var_LC_i64
	.long	L_1+6809
	.long	LT__uLT_var_LC_i64
	.long	L_1+6823
	.long	LT__LT_param_LC_I
	.long	L_1+6838
	.long	LT__uLT_param_LC_I
	.long	L_1+6852
	.long	LT__LT_var_LC_I
	.long	L_1+6867
	.long	LT__uLT_var_LC_I
	.long	L_1+6879
	.long	LT__LT_param_LC_i16
	.long	L_1+6892
	.long	LT__uLT_param_LC_i16
	.long	L_1+6908
	.long	LT__LT_var_LC_i16
	.long	L_1+6925
	.long	LT__uLT_var_LC_i16
	.long	L_1+6939
	.long	LT__LT_param_LC_i32
	.long	L_1+6954
	.long	LT__uLT_param_LC_i32
	.long	L_1+6970
	.long	LT__LT_var_LC_i32
	.long	L_1+6987
	.long	LT__uLT_var_LC_i32
	.long	L_1+7001
	.long	LT__LT_param_LC_LC
	.long	L_1+7016
	.long	LT__uLT_param_LC_LC
	.long	L_1+7031
	.long	LT__LT_var_LC_LC
	.long	L_1+7047
	.long	LT__uLT_var_LC_LC
	.long	L_1+7060
	.long	LT__LT_param_LC_u64
	.long	L_1+7074
	.long	LT__uLT_param_LC_u64
	.long	L_1+7090
	.long	LT__LT_var_LC_u64
	.long	L_1+7107
	.long	LT__uLT_var_LC_u64
	.long	L_1+7121
	.long	LT__LT_param_LC_u16
	.long	L_1+7136
	.long	LT__uLT_param_LC_u16
	.long	L_1+7152
	.long	LT__LT_var_LC_u16
	.long	L_1+7169
	.long	LT__uLT_var_LC_u16
	.long	L_1+7183
	.long	LT__LT_param_u64_u32
	.long	L_1+7198
	.long	LT__uLT_param_u64_u32
	.long	L_1+7215
	.long	LT__LT_var_u64_u32
	.long	L_1+7233
	.long	LT__uLT_var_u64_u32
	.long	L_1+7248
	.long	LT__LT_param_u64_i8
	.long	L_1+7264
	.long	LT__uLT_param_u64_i8
	.long	L_1+7280
	.long	LT__LT_var_u64_i8
	.long	L_1+7297
	.long	LT__uLT_var_u64_i8
	.long	L_1+7311
	.long	LT__LT_param_u64_L
	.long	L_1+7326
	.long	LT__uLT_param_u64_L
	.long	L_1+7341
	.long	LT__LT_var_u64_L
	.long	L_1+7357
	.long	LT__uLT_var_u64_L
	.long	L_1+7370
	.long	LT__LT_param_u64_u8
	.long	L_1+7384
	.long	LT__uLT_param_u64_u8
	.long	L_1+7400
	.long	LT__LT_var_u64_u8
	.long	L_1+7417
	.long	LT__uLT_var_u64_u8
	.long	L_1+7431
	.long	LT__LT_param_u64_C
	.long	L_1+7446
	.long	LT__uLT_param_u64_C
	.long	L_1+7461
	.long	LT__LT_var_u64_C
	.long	L_1+7477
	.long	LT__uLT_var_u64_C
	.long	L_1+7490
	.long	LT__LT_param_u64_i64
	.long	L_1+7504
	.long	LT__uLT_param_u64_i64
	.long	L_1+7521
	.long	LT__LT_var_u64_i64
	.long	L_1+7539
	.long	LT__uLT_var_u64_i64
	.long	L_1+7554
	.long	LT__LT_param_u64_I
	.long	L_1+7570
	.long	LT__uLT_param_u64_I
	.long	L_1+7585
	.long	LT__LT_var_u64_I
	.long	L_1+7601
	.long	LT__uLT_var_u64_I
	.long	L_1+7614
	.long	LT__LT_param_u64_i16
	.long	L_1+7628
	.long	LT__uLT_param_u64_i16
	.long	L_1+7645
	.long	LT__LT_var_u64_i16
	.long	L_1+7663
	.long	LT__uLT_var_u64_i16
	.long	L_1+7678
	.long	LT__LT_param_u64_i32
	.long	L_1+7694
	.long	LT__uLT_param_u64_i32
	.long	L_1+7711
	.long	LT__LT_var_u64_i32
	.long	L_1+7729
	.long	LT__uLT_var_u64_i32
	.long	L_1+7744
	.long	LT__LT_param_u64_LC
	.long	L_1+7760
	.long	LT__uLT_param_u64_LC
	.long	L_1+7776
	.long	LT__LT_var_u64_LC
	.long	L_1+7793
	.long	LT__uLT_var_u64_LC
	.long	L_1+7807
	.long	LT__LT_param_u64_u64
	.long	L_1+7822
	.long	LT__uLT_param_u64_u64
	.long	L_1+7839
	.long	LT__LT_var_u64_u64
	.long	L_1+7857
	.long	LT__uLT_var_u64_u64
	.long	L_1+7872
	.long	LT__LT_param_u64_u16
	.long	L_1+7888
	.long	LT__uLT_param_u64_u16
	.long	L_1+7905
	.long	LT__LT_var_u64_u16
	.long	L_1+7923
	.long	LT__uLT_var_u64_u16
	.long	L_1+7938
	.long	LT__LT_param_u16_u32
	.long	L_1+7954
	.long	LT__uLT_param_u16_u32
	.long	L_1+7971
	.long	LT__LT_var_u16_u32
	.long	L_1+7989
	.long	LT__uLT_var_u16_u32
	.long	L_1+8004
	.long	LT__LT_param_u16_i8
	.long	L_1+8020
	.long	LT__uLT_param_u16_i8
	.long	L_1+8036
	.long	LT__LT_var_u16_i8
	.long	L_1+8053
	.long	LT__uLT_var_u16_i8
	.long	L_1+8067
	.long	LT__LT_param_u16_L
	.long	L_1+8082
	.long	LT__uLT_param_u16_L
	.long	L_1+8097
	.long	LT__LT_var_u16_L
	.long	L_1+8113
	.long	LT__uLT_var_u16_L
	.long	L_1+8126
	.long	LT__LT_param_u16_u8
	.long	L_1+8140
	.long	LT__uLT_param_u16_u8
	.long	L_1+8156
	.long	LT__LT_var_u16_u8
	.long	L_1+8173
	.long	LT__uLT_var_u16_u8
	.long	L_1+8187
	.long	LT__LT_param_u16_C
	.long	L_1+8202
	.long	LT__uLT_param_u16_C
	.long	L_1+8217
	.long	LT__LT_var_u16_C
	.long	L_1+8233
	.long	LT__uLT_var_u16_C
	.long	L_1+8246
	.long	LT__LT_param_u16_i64
	.long	L_1+8260
	.long	LT__uLT_param_u16_i64
	.long	L_1+8277
	.long	LT__LT_var_u16_i64
	.long	L_1+8295
	.long	LT__uLT_var_u16_i64
	.long	L_1+8310
	.long	LT__LT_param_u16_I
	.long	L_1+8326
	.long	LT__uLT_param_u16_I
	.long	L_1+8341
	.long	LT__LT_var_u16_I
	.long	L_1+8357
	.long	LT__uLT_var_u16_I
	.long	L_1+8370
	.long	LT__LT_param_u16_i16
	.long	L_1+8384
	.long	LT__uLT_param_u16_i16
	.long	L_1+8401
	.long	LT__LT_var_u16_i16
	.long	L_1+8419
	.long	LT__uLT_var_u16_i16
	.long	L_1+8434
	.long	LT__LT_param_u16_i32
	.long	L_1+8450
	.long	LT__uLT_param_u16_i32
	.long	L_1+8467
	.long	LT__LT_var_u16_i32
	.long	L_1+8485
	.long	LT__uLT_var_u16_i32
	.long	L_1+8500
	.long	LT__LT_param_u16_LC
	.long	L_1+8516
	.long	LT__uLT_param_u16_LC
	.long	L_1+8532
	.long	LT__LT_var_u16_LC
	.long	L_1+8549
	.long	LT__uLT_var_u16_LC
	.long	L_1+8563
	.long	LT__LT_param_u16_u64
	.long	L_1+8578
	.long	LT__uLT_param_u16_u64
	.long	L_1+8595
	.long	LT__LT_var_u16_u64
	.long	L_1+8613
	.long	LT__uLT_var_u16_u64
	.long	L_1+8628
	.long	LT__LT_param_u16_u16
	.long	L_1+8644
	.long	LT__uLT_param_u16_u16
	.long	L_1+8661
	.long	LT__LT_var_u16_u16
	.long	L_1+8679
	.long	LT__uLT_var_u16_u16
	.long	L_1+8694
	.skip 4
	.ascii	"../SOLsun/LT.m3"
	.skip 5
	.align 8
	.type	MM_LT, #object
	.size	MM_LT, 208
MM_LT:
	.long	L_1+13364
	.skip 16
	.long	L_1+8712
	.skip 12
	.long	MM_LT+148
	.skip 4
	.long	LT_M3
	.long	3
	.half	305
	.skip 2
	.long	0
	.long	306
	.long	0
	.long	307
	.long	308
	.half	309
	.skip 2
	.long	310
	.skip 4
	.long	0
	.long	311
	.long	312
	.skip 4
	.long	1081316614
	.long	618475291
	.byte	59
	.skip 7
	.long	0
	.long	316
	.byte	61
	.skip 3
	.long	318
	.long	1134536950
	.skip 12
	.long	LT_I3
	.long	MM_LT+160
	.skip 4
	.long	Long_I3
	.long	MM_LT+172
	.skip 4
	.long	Word_I3
	.long	MM_LT+184
	.skip 4
	.long	Cstdint_I3
	.long	MM_LT+196
	.skip 4
	.long	RTHooks_I3
	.skip 4
	.ident	"GCC: (GNU) 4.5.1"
