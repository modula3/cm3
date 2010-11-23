	.file	"GE.mc"
	.section	".text"
	.align 4
	.type	GE__uGE_var_u16_u16, #function
	.proc	014
GE__uGE_var_u16_u16:
.LLFB0:
	save	%sp, -96, %sp
.LLCFI0:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE0:
	.size	GE__uGE_var_u16_u16, .-GE__uGE_var_u16_u16
	.align 4
	.type	GE__GE_var_u16_u16, #function
	.proc	014
GE__GE_var_u16_u16:
.LLFB1:
	save	%sp, -96, %sp
.LLCFI1:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1:
	.size	GE__GE_var_u16_u16, .-GE__GE_var_u16_u16
	.align 4
	.type	GE__uGE_param_u16_u16, #function
	.proc	014
GE__uGE_param_u16_u16:
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
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE2:
	.size	GE__uGE_param_u16_u16, .-GE__uGE_param_u16_u16
	.align 4
	.type	GE__GE_param_u16_u16, #function
	.proc	014
GE__GE_param_u16_u16:
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
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE3:
	.size	GE__GE_param_u16_u16, .-GE__GE_param_u16_u16
	.align 4
	.type	GE__uGE_var_u16_u64, #function
	.proc	014
GE__uGE_var_u16_u64:
.LLFB4:
	save	%sp, -96, %sp
.LLCFI4:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL7
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL6
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL7
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL6
	 nop
.LL7:
	mov	0, %g1
.LL6:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE4:
	.size	GE__uGE_var_u16_u64, .-GE__uGE_var_u16_u64
	.align 4
	.type	GE__GE_var_u16_u64, #function
	.proc	014
GE__GE_var_u16_u64:
.LLFB5:
	save	%sp, -96, %sp
.LLCFI5:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL10
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL9
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL10
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL9
	 nop
.LL10:
	mov	0, %g1
.LL9:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE5:
	.size	GE__GE_var_u16_u64, .-GE__GE_var_u16_u64
	.align 4
	.type	GE__uGE_param_u16_u64, #function
	.proc	014
GE__uGE_param_u16_u64:
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
	bgu	%icc, .LL13
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL12
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL13
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL12
	 nop
.LL13:
	mov	0, %g1
.LL12:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE6:
	.size	GE__uGE_param_u16_u64, .-GE__uGE_param_u16_u64
	.align 4
	.type	GE__GE_param_u16_u64, #function
	.proc	014
GE__GE_param_u16_u64:
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
	bg	%icc, .LL16
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL15
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL16
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL15
	 nop
.LL16:
	mov	0, %g1
.LL15:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE7:
	.size	GE__GE_param_u16_u64, .-GE__GE_param_u16_u64
	.align 4
	.type	GE__uGE_var_u16_LC, #function
	.proc	014
GE__uGE_var_u16_LC:
.LLFB8:
	save	%sp, -96, %sp
.LLCFI8:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL19
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL18
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL19
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL18
	 nop
.LL19:
	mov	0, %g1
.LL18:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE8:
	.size	GE__uGE_var_u16_LC, .-GE__uGE_var_u16_LC
	.align 4
	.type	GE__GE_var_u16_LC, #function
	.proc	014
GE__GE_var_u16_LC:
.LLFB9:
	save	%sp, -96, %sp
.LLCFI9:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL22
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL21
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL22
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL21
	 nop
.LL22:
	mov	0, %g1
.LL21:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE9:
	.size	GE__GE_var_u16_LC, .-GE__GE_var_u16_LC
	.align 4
	.type	GE__uGE_param_u16_LC, #function
	.proc	014
GE__uGE_param_u16_LC:
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
	bgu	%icc, .LL25
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL24
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL25
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL24
	 nop
.LL25:
	mov	0, %g1
.LL24:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE10:
	.size	GE__uGE_param_u16_LC, .-GE__uGE_param_u16_LC
	.align 4
	.type	GE__GE_param_u16_LC, #function
	.proc	014
GE__GE_param_u16_LC:
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
	bg	%icc, .LL28
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL27
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL28
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL27
	 nop
.LL28:
	mov	0, %g1
.LL27:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE11:
	.size	GE__GE_param_u16_LC, .-GE__GE_param_u16_LC
	.align 4
	.type	GE__uGE_var_u16_i32, #function
	.proc	014
GE__uGE_var_u16_i32:
.LLFB12:
	save	%sp, -96, %sp
.LLCFI12:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE12:
	.size	GE__uGE_var_u16_i32, .-GE__uGE_var_u16_i32
	.align 4
	.type	GE__GE_var_u16_i32, #function
	.proc	014
GE__GE_var_u16_i32:
.LLFB13:
	save	%sp, -96, %sp
.LLCFI13:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE13:
	.size	GE__GE_var_u16_i32, .-GE__GE_var_u16_i32
	.align 4
	.type	GE__uGE_param_u16_i32, #function
	.proc	014
GE__uGE_param_u16_i32:
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
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE14:
	.size	GE__uGE_param_u16_i32, .-GE__uGE_param_u16_i32
	.align 4
	.type	GE__GE_param_u16_i32, #function
	.proc	014
GE__GE_param_u16_i32:
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
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE15:
	.size	GE__GE_param_u16_i32, .-GE__GE_param_u16_i32
	.align 4
	.type	GE__uGE_var_u16_i16, #function
	.proc	014
GE__uGE_var_u16_i16:
.LLFB16:
	save	%sp, -96, %sp
.LLCFI16:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE16:
	.size	GE__uGE_var_u16_i16, .-GE__uGE_var_u16_i16
	.align 4
	.type	GE__GE_var_u16_i16, #function
	.proc	014
GE__GE_var_u16_i16:
.LLFB17:
	save	%sp, -96, %sp
.LLCFI17:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE17:
	.size	GE__GE_var_u16_i16, .-GE__GE_var_u16_i16
	.align 4
	.type	GE__uGE_param_u16_i16, #function
	.proc	014
GE__uGE_param_u16_i16:
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
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE18:
	.size	GE__uGE_param_u16_i16, .-GE__uGE_param_u16_i16
	.align 4
	.type	GE__GE_param_u16_i16, #function
	.proc	014
GE__GE_param_u16_i16:
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
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE19:
	.size	GE__GE_param_u16_i16, .-GE__GE_param_u16_i16
	.align 4
	.type	GE__uGE_var_u16_I, #function
	.proc	014
GE__uGE_var_u16_I:
.LLFB20:
	save	%sp, -96, %sp
.LLCFI20:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+80], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE20:
	.size	GE__uGE_var_u16_I, .-GE__uGE_var_u16_I
	.align 4
	.type	GE__GE_var_u16_I, #function
	.proc	014
GE__GE_var_u16_I:
.LLFB21:
	save	%sp, -96, %sp
.LLCFI21:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+80], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE21:
	.size	GE__GE_var_u16_I, .-GE__GE_var_u16_I
	.align 4
	.type	GE__uGE_param_u16_I, #function
	.proc	014
GE__uGE_param_u16_I:
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
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE22:
	.size	GE__uGE_param_u16_I, .-GE__uGE_param_u16_I
	.align 4
	.type	GE__GE_param_u16_I, #function
	.proc	014
GE__GE_param_u16_I:
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
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE23:
	.size	GE__GE_param_u16_I, .-GE__GE_param_u16_I
	.align 4
	.type	GE__uGE_var_u16_i64, #function
	.proc	014
GE__uGE_var_u16_i64:
.LLFB24:
	save	%sp, -96, %sp
.LLCFI24:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL43
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL42
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL43
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL42
	 nop
.LL43:
	mov	0, %g1
.LL42:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE24:
	.size	GE__uGE_var_u16_i64, .-GE__uGE_var_u16_i64
	.align 4
	.type	GE__GE_var_u16_i64, #function
	.proc	014
GE__GE_var_u16_i64:
.LLFB25:
	save	%sp, -96, %sp
.LLCFI25:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL46
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL45
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL46
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL45
	 nop
.LL46:
	mov	0, %g1
.LL45:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE25:
	.size	GE__GE_var_u16_i64, .-GE__GE_var_u16_i64
	.align 4
	.type	GE__uGE_param_u16_i64, #function
	.proc	014
GE__uGE_param_u16_i64:
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
	bgu	%icc, .LL49
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL48
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL49
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL48
	 nop
.LL49:
	mov	0, %g1
.LL48:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE26:
	.size	GE__uGE_param_u16_i64, .-GE__uGE_param_u16_i64
	.align 4
	.type	GE__GE_param_u16_i64, #function
	.proc	014
GE__GE_param_u16_i64:
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
	bg	%icc, .LL52
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL51
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL52
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL51
	 nop
.LL52:
	mov	0, %g1
.LL51:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE27:
	.size	GE__GE_param_u16_i64, .-GE__GE_param_u16_i64
	.align 4
	.type	GE__uGE_var_u16_C, #function
	.proc	014
GE__uGE_var_u16_C:
.LLFB28:
	save	%sp, -96, %sp
.LLCFI28:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+96], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE28:
	.size	GE__uGE_var_u16_C, .-GE__uGE_var_u16_C
	.align 4
	.type	GE__GE_var_u16_C, #function
	.proc	014
GE__GE_var_u16_C:
.LLFB29:
	save	%sp, -96, %sp
.LLCFI29:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+96], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE29:
	.size	GE__GE_var_u16_C, .-GE__GE_var_u16_C
	.align 4
	.type	GE__uGE_param_u16_C, #function
	.proc	014
GE__uGE_param_u16_C:
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
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE30:
	.size	GE__uGE_param_u16_C, .-GE__uGE_param_u16_C
	.align 4
	.type	GE__GE_param_u16_C, #function
	.proc	014
GE__GE_param_u16_C:
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
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE31:
	.size	GE__GE_param_u16_C, .-GE__GE_param_u16_C
	.align 4
	.type	GE__uGE_var_u16_u8, #function
	.proc	014
GE__uGE_var_u16_u8:
.LLFB32:
	save	%sp, -96, %sp
.LLCFI32:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	mov	%g1, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE32:
	.size	GE__uGE_var_u16_u8, .-GE__uGE_var_u16_u8
	.align 4
	.type	GE__GE_var_u16_u8, #function
	.proc	014
GE__GE_var_u16_u8:
.LLFB33:
	save	%sp, -96, %sp
.LLCFI33:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE33:
	.size	GE__GE_var_u16_u8, .-GE__GE_var_u16_u8
	.align 4
	.type	GE__uGE_param_u16_u8, #function
	.proc	014
GE__uGE_param_u16_u8:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE34:
	.size	GE__uGE_param_u16_u8, .-GE__uGE_param_u16_u8
	.align 4
	.type	GE__GE_param_u16_u8, #function
	.proc	014
GE__GE_param_u16_u8:
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
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE35:
	.size	GE__GE_param_u16_u8, .-GE__GE_param_u16_u8
	.align 4
	.type	GE__uGE_var_u16_L, #function
	.proc	014
GE__uGE_var_u16_L:
.LLFB36:
	save	%sp, -96, %sp
.LLCFI36:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL63
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL62
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL63
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL62
	 nop
.LL63:
	mov	0, %g1
.LL62:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE36:
	.size	GE__uGE_var_u16_L, .-GE__uGE_var_u16_L
	.align 4
	.type	GE__GE_var_u16_L, #function
	.proc	014
GE__GE_var_u16_L:
.LLFB37:
	save	%sp, -96, %sp
.LLCFI37:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL66
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL65
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL66
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL65
	 nop
.LL66:
	mov	0, %g1
.LL65:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE37:
	.size	GE__GE_var_u16_L, .-GE__GE_var_u16_L
	.align 4
	.type	GE__uGE_param_u16_L, #function
	.proc	014
GE__uGE_param_u16_L:
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
	bgu	%icc, .LL69
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL68
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL69
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL68
	 nop
.LL69:
	mov	0, %g1
.LL68:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE38:
	.size	GE__uGE_param_u16_L, .-GE__uGE_param_u16_L
	.align 4
	.type	GE__GE_param_u16_L, #function
	.proc	014
GE__GE_param_u16_L:
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
	bg	%icc, .LL72
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL71
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL72
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL71
	 nop
.LL72:
	mov	0, %g1
.LL71:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE39:
	.size	GE__GE_param_u16_L, .-GE__GE_param_u16_L
	.align 4
	.type	GE__uGE_var_u16_i8, #function
	.proc	014
GE__uGE_var_u16_i8:
.LLFB40:
	save	%sp, -96, %sp
.LLCFI40:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE40:
	.size	GE__uGE_var_u16_i8, .-GE__uGE_var_u16_i8
	.align 4
	.type	GE__GE_var_u16_i8, #function
	.proc	014
GE__GE_var_u16_i8:
.LLFB41:
	save	%sp, -96, %sp
.LLCFI41:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE41:
	.size	GE__GE_var_u16_i8, .-GE__GE_var_u16_i8
	.align 4
	.type	GE__uGE_param_u16_i8, #function
	.proc	014
GE__uGE_param_u16_i8:
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
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE42:
	.size	GE__uGE_param_u16_i8, .-GE__uGE_param_u16_i8
	.align 4
	.type	GE__GE_param_u16_i8, #function
	.proc	014
GE__GE_param_u16_i8:
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
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE43:
	.size	GE__GE_param_u16_i8, .-GE__GE_param_u16_i8
	.align 4
	.type	GE__uGE_var_u16_u32, #function
	.proc	014
GE__uGE_var_u16_u32:
.LLFB44:
	save	%sp, -96, %sp
.LLCFI44:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+132], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE44:
	.size	GE__uGE_var_u16_u32, .-GE__uGE_var_u16_u32
	.align 4
	.type	GE__GE_var_u16_u32, #function
	.proc	014
GE__GE_var_u16_u32:
.LLFB45:
	save	%sp, -96, %sp
.LLCFI45:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+132], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE45:
	.size	GE__GE_var_u16_u32, .-GE__GE_var_u16_u32
	.align 4
	.type	GE__uGE_param_u16_u32, #function
	.proc	014
GE__uGE_param_u16_u32:
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
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE46:
	.size	GE__uGE_param_u16_u32, .-GE__uGE_param_u16_u32
	.align 4
	.type	GE__GE_param_u16_u32, #function
	.proc	014
GE__GE_param_u16_u32:
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
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE47:
	.size	GE__GE_param_u16_u32, .-GE__GE_param_u16_u32
	.align 4
	.type	GE__uGE_var_u64_u16, #function
	.proc	014
GE__uGE_var_u64_u16:
.LLFB48:
	save	%sp, -96, %sp
.LLCFI48:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+56], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL83
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL82
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL83
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL82
	 nop
.LL83:
	mov	0, %g1
.LL82:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE48:
	.size	GE__uGE_var_u64_u16, .-GE__uGE_var_u64_u16
	.align 4
	.type	GE__GE_var_u64_u16, #function
	.proc	014
GE__GE_var_u64_u16:
.LLFB49:
	save	%sp, -96, %sp
.LLCFI49:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+56], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL86
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL85
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL86
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL85
	 nop
.LL86:
	mov	0, %g1
.LL85:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE49:
	.size	GE__GE_var_u64_u16, .-GE__GE_var_u64_u16
	.align 4
	.type	GE__uGE_param_u64_u16, #function
	.proc	014
GE__uGE_param_u64_u16:
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
	bgu	%icc, .LL89
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL88
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL89
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL88
	 nop
.LL89:
	mov	0, %g1
.LL88:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE50:
	.size	GE__uGE_param_u64_u16, .-GE__uGE_param_u64_u16
	.align 4
	.type	GE__GE_param_u64_u16, #function
	.proc	014
GE__GE_param_u64_u16:
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
	bl	%icc, .LL92
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL91
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL92
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL91
	 nop
.LL92:
	mov	0, %g1
.LL91:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE51:
	.size	GE__GE_param_u64_u16, .-GE__GE_param_u64_u16
	.align 4
	.type	GE__uGE_var_u64_u64, #function
	.proc	014
GE__uGE_var_u64_u64:
.LLFB52:
	save	%sp, -96, %sp
.LLCFI52:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE52:
	.size	GE__uGE_var_u64_u64, .-GE__uGE_var_u64_u64
	.align 4
	.type	GE__GE_var_u64_u64, #function
	.proc	014
GE__GE_var_u64_u64:
.LLFB53:
	save	%sp, -96, %sp
.LLCFI53:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE53:
	.size	GE__GE_var_u64_u64, .-GE__GE_var_u64_u64
	.align 4
	.type	GE__uGE_param_u64_u64, #function
	.proc	014
GE__uGE_param_u64_u64:
.LLFB54:
	save	%sp, -112, %sp
.LLCFI54:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	ldd	[%fp-16], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL97
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL96
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL97
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL96
	 nop
.LL97:
	mov	0, %g1
.LL96:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE54:
	.size	GE__uGE_param_u64_u64, .-GE__uGE_param_u64_u64
	.align 4
	.type	GE__GE_param_u64_u64, #function
	.proc	014
GE__GE_param_u64_u64:
.LLFB55:
	save	%sp, -112, %sp
.LLCFI55:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	mov	1, %g1
	ld	[%fp-16], %o4
	ld	[%fp-8], %o5
	cmp	%o4, %o5
	bg	%icc, .LL100
	 nop
	ld	[%fp-16], %o4
	ld	[%fp-8], %o5
	cmp	%o4, %o5
	bne	%icc, .LL99
	 nop
	ld	[%fp-12], %o4
	ld	[%fp-4], %o5
	cmp	%o4, %o5
	bgu	%icc, .LL100
	 nop
	ld	[%fp-12], %o4
	ld	[%fp-4], %o5
	cmp	%o4, %o5
	ba,pt	%xcc, .LL99
	 nop
.LL100:
	mov	0, %g1
.LL99:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE55:
	.size	GE__GE_param_u64_u64, .-GE__GE_param_u64_u64
	.align 4
	.type	GE__uGE_var_u64_LC, #function
	.proc	014
GE__uGE_var_u64_LC:
.LLFB56:
	save	%sp, -96, %sp
.LLCFI56:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+56], %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL103
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL102
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL103
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL102
	 nop
.LL103:
	mov	0, %g1
.LL102:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE56:
	.size	GE__uGE_var_u64_LC, .-GE__uGE_var_u64_LC
	.align 4
	.type	GE__GE_var_u64_LC, #function
	.proc	014
GE__GE_var_u64_LC:
.LLFB57:
	save	%sp, -96, %sp
.LLCFI57:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+56], %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL106
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL105
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL106
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL105
	 nop
.LL106:
	mov	0, %g1
.LL105:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE57:
	.size	GE__GE_var_u64_LC, .-GE__GE_var_u64_LC
	.align 4
	.type	GE__uGE_param_u64_LC, #function
	.proc	014
GE__uGE_param_u64_LC:
.LLFB58:
	save	%sp, -112, %sp
.LLCFI58:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	mov	1, %g1
	ld	[%fp-16], %o3
	cmp	%o3, %o4
	bgu	%icc, .LL109
	 nop
	ld	[%fp-16], %o3
	cmp	%o3, %o4
	bne	%icc, .LL108
	 nop
	ld	[%fp-12], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL109
	 nop
	ld	[%fp-12], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL108
	 nop
.LL109:
	mov	0, %g1
.LL108:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE58:
	.size	GE__uGE_param_u64_LC, .-GE__uGE_param_u64_LC
	.align 4
	.type	GE__GE_param_u64_LC, #function
	.proc	014
GE__GE_param_u64_LC:
.LLFB59:
	save	%sp, -112, %sp
.LLCFI59:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bl	%icc, .LL112
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL111
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL112
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL111
	 nop
.LL112:
	mov	0, %g1
.LL111:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE59:
	.size	GE__GE_param_u64_LC, .-GE__GE_param_u64_LC
	.align 4
	.type	GE__uGE_var_u64_i32, #function
	.proc	014
GE__uGE_var_u64_i32:
.LLFB60:
	save	%sp, -96, %sp
.LLCFI60:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL115
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL114
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL115
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL114
	 nop
.LL115:
	mov	0, %g1
.LL114:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE60:
	.size	GE__uGE_var_u64_i32, .-GE__uGE_var_u64_i32
	.align 4
	.type	GE__GE_var_u64_i32, #function
	.proc	014
GE__GE_var_u64_i32:
.LLFB61:
	save	%sp, -96, %sp
.LLCFI61:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL118
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL117
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL118
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL117
	 nop
.LL118:
	mov	0, %g1
.LL117:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE61:
	.size	GE__GE_var_u64_i32, .-GE__GE_var_u64_i32
	.align 4
	.type	GE__uGE_param_u64_i32, #function
	.proc	014
GE__uGE_param_u64_i32:
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
	bgu	%icc, .LL121
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL120
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL121
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL120
	 nop
.LL121:
	mov	0, %g1
.LL120:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE62:
	.size	GE__uGE_param_u64_i32, .-GE__uGE_param_u64_i32
	.align 4
	.type	GE__GE_param_u64_i32, #function
	.proc	014
GE__GE_param_u64_i32:
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
	bl	%icc, .LL124
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL123
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL124
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL123
	 nop
.LL124:
	mov	0, %g1
.LL123:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE63:
	.size	GE__GE_param_u64_i32, .-GE__GE_param_u64_i32
	.align 4
	.type	GE__uGE_var_u64_i16, #function
	.proc	014
GE__uGE_var_u64_i16:
.LLFB64:
	save	%sp, -96, %sp
.LLCFI64:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL127
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL126
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL127
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL126
	 nop
.LL127:
	mov	0, %g1
.LL126:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE64:
	.size	GE__uGE_var_u64_i16, .-GE__uGE_var_u64_i16
	.align 4
	.type	GE__GE_var_u64_i16, #function
	.proc	014
GE__GE_var_u64_i16:
.LLFB65:
	save	%sp, -96, %sp
.LLCFI65:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL130
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL129
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL130
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL129
	 nop
.LL130:
	mov	0, %g1
.LL129:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE65:
	.size	GE__GE_var_u64_i16, .-GE__GE_var_u64_i16
	.align 4
	.type	GE__uGE_param_u64_i16, #function
	.proc	014
GE__uGE_param_u64_i16:
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
	bgu	%icc, .LL133
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL132
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL133
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL132
	 nop
.LL133:
	mov	0, %g1
.LL132:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE66:
	.size	GE__uGE_param_u64_i16, .-GE__uGE_param_u64_i16
	.align 4
	.type	GE__GE_param_u64_i16, #function
	.proc	014
GE__GE_param_u64_i16:
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
	bl	%icc, .LL136
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL135
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL136
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL135
	 nop
.LL136:
	mov	0, %g1
.LL135:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE67:
	.size	GE__GE_param_u64_i16, .-GE__GE_param_u64_i16
	.align 4
	.type	GE__uGE_var_u64_I, #function
	.proc	014
GE__uGE_var_u64_I:
.LLFB68:
	save	%sp, -96, %sp
.LLCFI68:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL139
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL138
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL139
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL138
	 nop
.LL139:
	mov	0, %g1
.LL138:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE68:
	.size	GE__uGE_var_u64_I, .-GE__uGE_var_u64_I
	.align 4
	.type	GE__GE_var_u64_I, #function
	.proc	014
GE__GE_var_u64_I:
.LLFB69:
	save	%sp, -96, %sp
.LLCFI69:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL142
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL141
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL142
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL141
	 nop
.LL142:
	mov	0, %g1
.LL141:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE69:
	.size	GE__GE_var_u64_I, .-GE__GE_var_u64_I
	.align 4
	.type	GE__uGE_param_u64_I, #function
	.proc	014
GE__uGE_param_u64_I:
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
	bgu	%icc, .LL145
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL144
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL145
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL144
	 nop
.LL145:
	mov	0, %g1
.LL144:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE70:
	.size	GE__uGE_param_u64_I, .-GE__uGE_param_u64_I
	.align 4
	.type	GE__GE_param_u64_I, #function
	.proc	014
GE__GE_param_u64_I:
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
	bl	%icc, .LL148
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL147
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL148
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL147
	 nop
.LL148:
	mov	0, %g1
.LL147:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE71:
	.size	GE__GE_param_u64_I, .-GE__GE_param_u64_I
	.align 4
	.type	GE__uGE_var_u64_i64, #function
	.proc	014
GE__uGE_var_u64_i64:
.LLFB72:
	save	%sp, -96, %sp
.LLCFI72:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+56], %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL151
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL150
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL151
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL150
	 nop
.LL151:
	mov	0, %g1
.LL150:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE72:
	.size	GE__uGE_var_u64_i64, .-GE__uGE_var_u64_i64
	.align 4
	.type	GE__GE_var_u64_i64, #function
	.proc	014
GE__GE_var_u64_i64:
.LLFB73:
	save	%sp, -96, %sp
.LLCFI73:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+56], %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL154
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL153
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL154
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL153
	 nop
.LL154:
	mov	0, %g1
.LL153:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE73:
	.size	GE__GE_var_u64_i64, .-GE__GE_var_u64_i64
	.align 4
	.type	GE__uGE_param_u64_i64, #function
	.proc	014
GE__uGE_param_u64_i64:
.LLFB74:
	save	%sp, -112, %sp
.LLCFI74:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	ldd	[%fp-16], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL157
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL156
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL157
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL156
	 nop
.LL157:
	mov	0, %g1
.LL156:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE74:
	.size	GE__uGE_param_u64_i64, .-GE__uGE_param_u64_i64
	.align 4
	.type	GE__GE_param_u64_i64, #function
	.proc	014
GE__GE_param_u64_i64:
.LLFB75:
	save	%sp, -112, %sp
.LLCFI75:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	mov	1, %g1
	ld	[%fp-16], %o4
	ld	[%fp-8], %o5
	cmp	%o4, %o5
	bg	%icc, .LL160
	 nop
	ld	[%fp-16], %o4
	ld	[%fp-8], %o5
	cmp	%o4, %o5
	bne	%icc, .LL159
	 nop
	ld	[%fp-12], %o4
	ld	[%fp-4], %o5
	cmp	%o4, %o5
	bgu	%icc, .LL160
	 nop
	ld	[%fp-12], %o4
	ld	[%fp-4], %o5
	cmp	%o4, %o5
	ba,pt	%xcc, .LL159
	 nop
.LL160:
	mov	0, %g1
.LL159:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE75:
	.size	GE__GE_param_u64_i64, .-GE__GE_param_u64_i64
	.align 4
	.type	GE__uGE_var_u64_C, #function
	.proc	014
GE__uGE_var_u64_C:
.LLFB76:
	save	%sp, -96, %sp
.LLCFI76:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL163
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL162
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL163
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL162
	 nop
.LL163:
	mov	0, %g1
.LL162:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE76:
	.size	GE__uGE_var_u64_C, .-GE__uGE_var_u64_C
	.align 4
	.type	GE__GE_var_u64_C, #function
	.proc	014
GE__GE_var_u64_C:
.LLFB77:
	save	%sp, -96, %sp
.LLCFI77:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL166
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL165
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL166
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL165
	 nop
.LL166:
	mov	0, %g1
.LL165:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE77:
	.size	GE__GE_var_u64_C, .-GE__GE_var_u64_C
	.align 4
	.type	GE__uGE_param_u64_C, #function
	.proc	014
GE__uGE_param_u64_C:
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
	bgu	%icc, .LL169
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL168
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL169
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL168
	 nop
.LL169:
	mov	0, %g1
.LL168:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE78:
	.size	GE__uGE_param_u64_C, .-GE__uGE_param_u64_C
	.align 4
	.type	GE__GE_param_u64_C, #function
	.proc	014
GE__GE_param_u64_C:
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
	bl	%icc, .LL172
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL171
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL172
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL171
	 nop
.LL172:
	mov	0, %g1
.LL171:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE79:
	.size	GE__GE_param_u64_C, .-GE__GE_param_u64_C
	.align 4
	.type	GE__uGE_var_u64_u8, #function
	.proc	014
GE__uGE_var_u64_u8:
.LLFB80:
	save	%sp, -96, %sp
.LLCFI80:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+56], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL175
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL174
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL175
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL174
	 nop
.LL175:
	mov	0, %g1
.LL174:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE80:
	.size	GE__uGE_var_u64_u8, .-GE__uGE_var_u64_u8
	.align 4
	.type	GE__GE_var_u64_u8, #function
	.proc	014
GE__GE_var_u64_u8:
.LLFB81:
	save	%sp, -96, %sp
.LLCFI81:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+56], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL178
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL177
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL178
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL177
	 nop
.LL178:
	mov	0, %g1
.LL177:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE81:
	.size	GE__GE_var_u64_u8, .-GE__GE_var_u64_u8
	.align 4
	.type	GE__uGE_param_u64_u8, #function
	.proc	014
GE__uGE_param_u64_u8:
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
	bgu	%icc, .LL181
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL180
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL181
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL180
	 nop
.LL181:
	mov	0, %g1
.LL180:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE82:
	.size	GE__uGE_param_u64_u8, .-GE__uGE_param_u64_u8
	.align 4
	.type	GE__GE_param_u64_u8, #function
	.proc	014
GE__GE_param_u64_u8:
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
	bl	%icc, .LL184
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL183
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL184
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL183
	 nop
.LL184:
	mov	0, %g1
.LL183:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE83:
	.size	GE__GE_param_u64_u8, .-GE__GE_param_u64_u8
	.align 4
	.type	GE__uGE_var_u64_L, #function
	.proc	014
GE__uGE_var_u64_L:
.LLFB84:
	save	%sp, -96, %sp
.LLCFI84:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+56], %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL187
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL186
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL187
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL186
	 nop
.LL187:
	mov	0, %g1
.LL186:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE84:
	.size	GE__uGE_var_u64_L, .-GE__uGE_var_u64_L
	.align 4
	.type	GE__GE_var_u64_L, #function
	.proc	014
GE__GE_var_u64_L:
.LLFB85:
	save	%sp, -96, %sp
.LLCFI85:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+56], %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL190
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL189
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL190
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL189
	 nop
.LL190:
	mov	0, %g1
.LL189:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE85:
	.size	GE__GE_var_u64_L, .-GE__GE_var_u64_L
	.align 4
	.type	GE__uGE_param_u64_L, #function
	.proc	014
GE__uGE_param_u64_L:
.LLFB86:
	save	%sp, -112, %sp
.LLCFI86:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	ldd	[%fp-16], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL193
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL192
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL193
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL192
	 nop
.LL193:
	mov	0, %g1
.LL192:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE86:
	.size	GE__uGE_param_u64_L, .-GE__uGE_param_u64_L
	.align 4
	.type	GE__GE_param_u64_L, #function
	.proc	014
GE__GE_param_u64_L:
.LLFB87:
	save	%sp, -112, %sp
.LLCFI87:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	mov	1, %g1
	ld	[%fp-16], %o4
	ld	[%fp-8], %o5
	cmp	%o4, %o5
	bg	%icc, .LL196
	 nop
	ld	[%fp-16], %o4
	ld	[%fp-8], %o5
	cmp	%o4, %o5
	bne	%icc, .LL195
	 nop
	ld	[%fp-12], %o4
	ld	[%fp-4], %o5
	cmp	%o4, %o5
	bgu	%icc, .LL196
	 nop
	ld	[%fp-12], %o4
	ld	[%fp-4], %o5
	cmp	%o4, %o5
	ba,pt	%xcc, .LL195
	 nop
.LL196:
	mov	0, %g1
.LL195:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE87:
	.size	GE__GE_param_u64_L, .-GE__GE_param_u64_L
	.align 4
	.type	GE__uGE_var_u64_i8, #function
	.proc	014
GE__uGE_var_u64_i8:
.LLFB88:
	save	%sp, -96, %sp
.LLCFI88:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL199
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL198
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL199
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL198
	 nop
.LL199:
	mov	0, %g1
.LL198:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE88:
	.size	GE__uGE_var_u64_i8, .-GE__uGE_var_u64_i8
	.align 4
	.type	GE__GE_var_u64_i8, #function
	.proc	014
GE__GE_var_u64_i8:
.LLFB89:
	save	%sp, -96, %sp
.LLCFI89:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL202
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL201
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL202
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL201
	 nop
.LL202:
	mov	0, %g1
.LL201:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE89:
	.size	GE__GE_var_u64_i8, .-GE__GE_var_u64_i8
	.align 4
	.type	GE__uGE_param_u64_i8, #function
	.proc	014
GE__uGE_param_u64_i8:
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
	bgu	%icc, .LL205
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL204
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL205
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL204
	 nop
.LL205:
	mov	0, %g1
.LL204:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE90:
	.size	GE__uGE_param_u64_i8, .-GE__uGE_param_u64_i8
	.align 4
	.type	GE__GE_param_u64_i8, #function
	.proc	014
GE__GE_param_u64_i8:
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
	bl	%icc, .LL208
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL207
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL208
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL207
	 nop
.LL208:
	mov	0, %g1
.LL207:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE91:
	.size	GE__GE_param_u64_i8, .-GE__GE_param_u64_i8
	.align 4
	.type	GE__uGE_var_u64_u32, #function
	.proc	014
GE__uGE_var_u64_u32:
.LLFB92:
	save	%sp, -96, %sp
.LLCFI92:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL211
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL210
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL211
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL210
	 nop
.LL211:
	mov	0, %g1
.LL210:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE92:
	.size	GE__uGE_var_u64_u32, .-GE__uGE_var_u64_u32
	.align 4
	.type	GE__GE_var_u64_u32, #function
	.proc	014
GE__GE_var_u64_u32:
.LLFB93:
	save	%sp, -96, %sp
.LLCFI93:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL214
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL213
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL214
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL213
	 nop
.LL214:
	mov	0, %g1
.LL213:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE93:
	.size	GE__GE_var_u64_u32, .-GE__GE_var_u64_u32
	.align 4
	.type	GE__uGE_param_u64_u32, #function
	.proc	014
GE__uGE_param_u64_u32:
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
	bgu	%icc, .LL217
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL216
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL217
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL216
	 nop
.LL217:
	mov	0, %g1
.LL216:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE94:
	.size	GE__uGE_param_u64_u32, .-GE__uGE_param_u64_u32
	.align 4
	.type	GE__GE_param_u64_u32, #function
	.proc	014
GE__GE_param_u64_u32:
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
	bl	%icc, .LL220
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL219
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL220
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL219
	 nop
.LL220:
	mov	0, %g1
.LL219:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE95:
	.size	GE__GE_param_u64_u32, .-GE__GE_param_u64_u32
	.align 4
	.type	GE__uGE_var_LC_u16, #function
	.proc	014
GE__uGE_var_LC_u16:
.LLFB96:
	save	%sp, -96, %sp
.LLCFI96:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+64], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL223
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL222
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL223
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL222
	 nop
.LL223:
	mov	0, %g1
.LL222:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE96:
	.size	GE__uGE_var_LC_u16, .-GE__uGE_var_LC_u16
	.align 4
	.type	GE__GE_var_LC_u16, #function
	.proc	014
GE__GE_var_LC_u16:
.LLFB97:
	save	%sp, -96, %sp
.LLCFI97:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+64], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL226
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL225
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL226
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL225
	 nop
.LL226:
	mov	0, %g1
.LL225:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE97:
	.size	GE__GE_var_LC_u16, .-GE__GE_var_LC_u16
	.align 4
	.type	GE__uGE_param_LC_u16, #function
	.proc	014
GE__uGE_param_LC_u16:
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
	blu	%icc, .LL229
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL228
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL229
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL228
	 nop
.LL229:
	mov	0, %g1
.LL228:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE98:
	.size	GE__uGE_param_LC_u16, .-GE__uGE_param_LC_u16
	.align 4
	.type	GE__GE_param_LC_u16, #function
	.proc	014
GE__GE_param_LC_u16:
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
	bg	%icc, .LL232
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL231
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL232
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL231
	 nop
.LL232:
	mov	0, %g1
.LL231:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE99:
	.size	GE__GE_param_LC_u16, .-GE__GE_param_LC_u16
	.align 4
	.type	GE__uGE_var_LC_u64, #function
	.proc	014
GE__uGE_var_LC_u64:
.LLFB100:
	save	%sp, -96, %sp
.LLCFI100:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+64], %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL235
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL234
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL235
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL234
	 nop
.LL235:
	mov	0, %g1
.LL234:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE100:
	.size	GE__uGE_var_LC_u64, .-GE__uGE_var_LC_u64
	.align 4
	.type	GE__GE_var_LC_u64, #function
	.proc	014
GE__GE_var_LC_u64:
.LLFB101:
	save	%sp, -96, %sp
.LLCFI101:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+64], %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL238
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL237
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL238
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL237
	 nop
.LL238:
	mov	0, %g1
.LL237:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE101:
	.size	GE__GE_var_LC_u64, .-GE__GE_var_LC_u64
	.align 4
	.type	GE__uGE_param_LC_u64, #function
	.proc	014
GE__uGE_param_LC_u64:
.LLFB102:
	save	%sp, -112, %sp
.LLCFI102:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	blu	%icc, .LL241
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL240
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL241
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL240
	 nop
.LL241:
	mov	0, %g1
.LL240:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE102:
	.size	GE__uGE_param_LC_u64, .-GE__uGE_param_LC_u64
	.align 4
	.type	GE__GE_param_LC_u64, #function
	.proc	014
GE__GE_param_LC_u64:
.LLFB103:
	save	%sp, -112, %sp
.LLCFI103:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	mov	1, %g1
	ld	[%fp-16], %o3
	cmp	%o3, %o4
	bg	%icc, .LL244
	 nop
	ld	[%fp-16], %o3
	cmp	%o3, %o4
	bne	%icc, .LL243
	 nop
	ld	[%fp-12], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL244
	 nop
	ld	[%fp-12], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL243
	 nop
.LL244:
	mov	0, %g1
.LL243:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE103:
	.size	GE__GE_param_LC_u64, .-GE__GE_param_LC_u64
	.align 4
	.type	GE__uGE_var_LC_LC, #function
	.proc	014
GE__uGE_var_LC_LC:
.LLFB104:
	save	%sp, -96, %sp
.LLCFI104:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE104:
	.size	GE__uGE_var_LC_LC, .-GE__uGE_var_LC_LC
	.align 4
	.type	GE__GE_var_LC_LC, #function
	.proc	014
GE__GE_var_LC_LC:
.LLFB105:
	save	%sp, -96, %sp
.LLCFI105:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE105:
	.size	GE__GE_var_LC_LC, .-GE__GE_var_LC_LC
	.align 4
	.type	GE__uGE_param_LC_LC, #function
	.proc	014
GE__uGE_param_LC_LC:
.LLFB106:
	save	%sp, -112, %sp
.LLCFI106:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	mov	1, %g1
	ld	[%fp-16], %o4
	ld	[%fp-8], %o5
	cmp	%o4, %o5
	bgu	%icc, .LL249
	 nop
	ld	[%fp-16], %o4
	ld	[%fp-8], %o5
	cmp	%o4, %o5
	bne	%icc, .LL248
	 nop
	ld	[%fp-12], %o4
	ld	[%fp-4], %o5
	cmp	%o4, %o5
	bgu	%icc, .LL249
	 nop
	ld	[%fp-12], %o4
	ld	[%fp-4], %o5
	cmp	%o4, %o5
	ba,pt	%xcc, .LL248
	 nop
.LL249:
	mov	0, %g1
.LL248:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE106:
	.size	GE__uGE_param_LC_LC, .-GE__uGE_param_LC_LC
	.align 4
	.type	GE__GE_param_LC_LC, #function
	.proc	014
GE__GE_param_LC_LC:
.LLFB107:
	save	%sp, -112, %sp
.LLCFI107:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	ldd	[%fp-16], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL252
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL251
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL252
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL251
	 nop
.LL252:
	mov	0, %g1
.LL251:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE107:
	.size	GE__GE_param_LC_LC, .-GE__GE_param_LC_LC
	.align 4
	.type	GE__uGE_var_LC_i32, #function
	.proc	014
GE__uGE_var_LC_i32:
.LLFB108:
	save	%sp, -96, %sp
.LLCFI108:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL255
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL254
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL255
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL254
	 nop
.LL255:
	mov	0, %g1
.LL254:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE108:
	.size	GE__uGE_var_LC_i32, .-GE__uGE_var_LC_i32
	.align 4
	.type	GE__GE_var_LC_i32, #function
	.proc	014
GE__GE_var_LC_i32:
.LLFB109:
	save	%sp, -96, %sp
.LLCFI109:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL258
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL257
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL258
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL257
	 nop
.LL258:
	mov	0, %g1
.LL257:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE109:
	.size	GE__GE_var_LC_i32, .-GE__GE_var_LC_i32
	.align 4
	.type	GE__uGE_param_LC_i32, #function
	.proc	014
GE__uGE_param_LC_i32:
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
	blu	%icc, .LL261
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL260
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL261
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL260
	 nop
.LL261:
	mov	0, %g1
.LL260:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE110:
	.size	GE__uGE_param_LC_i32, .-GE__uGE_param_LC_i32
	.align 4
	.type	GE__GE_param_LC_i32, #function
	.proc	014
GE__GE_param_LC_i32:
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
	bg	%icc, .LL264
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL263
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL264
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL263
	 nop
.LL264:
	mov	0, %g1
.LL263:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE111:
	.size	GE__GE_param_LC_i32, .-GE__GE_param_LC_i32
	.align 4
	.type	GE__uGE_var_LC_i16, #function
	.proc	014
GE__uGE_var_LC_i16:
.LLFB112:
	save	%sp, -96, %sp
.LLCFI112:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL267
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL266
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL267
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL266
	 nop
.LL267:
	mov	0, %g1
.LL266:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE112:
	.size	GE__uGE_var_LC_i16, .-GE__uGE_var_LC_i16
	.align 4
	.type	GE__GE_var_LC_i16, #function
	.proc	014
GE__GE_var_LC_i16:
.LLFB113:
	save	%sp, -96, %sp
.LLCFI113:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL270
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL269
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL270
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL269
	 nop
.LL270:
	mov	0, %g1
.LL269:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE113:
	.size	GE__GE_var_LC_i16, .-GE__GE_var_LC_i16
	.align 4
	.type	GE__uGE_param_LC_i16, #function
	.proc	014
GE__uGE_param_LC_i16:
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
	blu	%icc, .LL273
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL272
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL273
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL272
	 nop
.LL273:
	mov	0, %g1
.LL272:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE114:
	.size	GE__uGE_param_LC_i16, .-GE__uGE_param_LC_i16
	.align 4
	.type	GE__GE_param_LC_i16, #function
	.proc	014
GE__GE_param_LC_i16:
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
	bg	%icc, .LL276
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL275
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL276
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL275
	 nop
.LL276:
	mov	0, %g1
.LL275:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE115:
	.size	GE__GE_param_LC_i16, .-GE__GE_param_LC_i16
	.align 4
	.type	GE__uGE_var_LC_I, #function
	.proc	014
GE__uGE_var_LC_I:
.LLFB116:
	save	%sp, -96, %sp
.LLCFI116:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL279
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL278
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL279
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL278
	 nop
.LL279:
	mov	0, %g1
.LL278:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE116:
	.size	GE__uGE_var_LC_I, .-GE__uGE_var_LC_I
	.align 4
	.type	GE__GE_var_LC_I, #function
	.proc	014
GE__GE_var_LC_I:
.LLFB117:
	save	%sp, -96, %sp
.LLCFI117:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL282
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL281
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL282
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL281
	 nop
.LL282:
	mov	0, %g1
.LL281:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE117:
	.size	GE__GE_var_LC_I, .-GE__GE_var_LC_I
	.align 4
	.type	GE__uGE_param_LC_I, #function
	.proc	014
GE__uGE_param_LC_I:
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
	blu	%icc, .LL285
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL284
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL285
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL284
	 nop
.LL285:
	mov	0, %g1
.LL284:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE118:
	.size	GE__uGE_param_LC_I, .-GE__uGE_param_LC_I
	.align 4
	.type	GE__GE_param_LC_I, #function
	.proc	014
GE__GE_param_LC_I:
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
	bg	%icc, .LL288
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL287
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL288
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL287
	 nop
.LL288:
	mov	0, %g1
.LL287:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE119:
	.size	GE__GE_param_LC_I, .-GE__GE_param_LC_I
	.align 4
	.type	GE__uGE_var_LC_i64, #function
	.proc	014
GE__uGE_var_LC_i64:
.LLFB120:
	save	%sp, -96, %sp
.LLCFI120:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+64], %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL291
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL290
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL291
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL290
	 nop
.LL291:
	mov	0, %g1
.LL290:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE120:
	.size	GE__uGE_var_LC_i64, .-GE__uGE_var_LC_i64
	.align 4
	.type	GE__GE_var_LC_i64, #function
	.proc	014
GE__GE_var_LC_i64:
.LLFB121:
	save	%sp, -96, %sp
.LLCFI121:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+64], %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL294
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL293
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL294
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL293
	 nop
.LL294:
	mov	0, %g1
.LL293:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE121:
	.size	GE__GE_var_LC_i64, .-GE__GE_var_LC_i64
	.align 4
	.type	GE__uGE_param_LC_i64, #function
	.proc	014
GE__uGE_param_LC_i64:
.LLFB122:
	save	%sp, -112, %sp
.LLCFI122:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	blu	%icc, .LL297
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL296
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL297
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL296
	 nop
.LL297:
	mov	0, %g1
.LL296:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE122:
	.size	GE__uGE_param_LC_i64, .-GE__uGE_param_LC_i64
	.align 4
	.type	GE__GE_param_LC_i64, #function
	.proc	014
GE__GE_param_LC_i64:
.LLFB123:
	save	%sp, -112, %sp
.LLCFI123:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	mov	1, %g1
	ld	[%fp-16], %o3
	cmp	%o3, %o4
	bg	%icc, .LL300
	 nop
	ld	[%fp-16], %o3
	cmp	%o3, %o4
	bne	%icc, .LL299
	 nop
	ld	[%fp-12], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL300
	 nop
	ld	[%fp-12], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL299
	 nop
.LL300:
	mov	0, %g1
.LL299:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE123:
	.size	GE__GE_param_LC_i64, .-GE__GE_param_LC_i64
	.align 4
	.type	GE__uGE_var_LC_C, #function
	.proc	014
GE__uGE_var_LC_C:
.LLFB124:
	save	%sp, -96, %sp
.LLCFI124:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL303
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL302
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL303
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL302
	 nop
.LL303:
	mov	0, %g1
.LL302:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE124:
	.size	GE__uGE_var_LC_C, .-GE__uGE_var_LC_C
	.align 4
	.type	GE__GE_var_LC_C, #function
	.proc	014
GE__GE_var_LC_C:
.LLFB125:
	save	%sp, -96, %sp
.LLCFI125:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL306
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL305
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL306
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL305
	 nop
.LL306:
	mov	0, %g1
.LL305:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE125:
	.size	GE__GE_var_LC_C, .-GE__GE_var_LC_C
	.align 4
	.type	GE__uGE_param_LC_C, #function
	.proc	014
GE__uGE_param_LC_C:
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
	blu	%icc, .LL309
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL308
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL309
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL308
	 nop
.LL309:
	mov	0, %g1
.LL308:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE126:
	.size	GE__uGE_param_LC_C, .-GE__uGE_param_LC_C
	.align 4
	.type	GE__GE_param_LC_C, #function
	.proc	014
GE__GE_param_LC_C:
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
	bg	%icc, .LL312
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL311
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL312
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL311
	 nop
.LL312:
	mov	0, %g1
.LL311:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE127:
	.size	GE__GE_param_LC_C, .-GE__GE_param_LC_C
	.align 4
	.type	GE__uGE_var_LC_u8, #function
	.proc	014
GE__uGE_var_LC_u8:
.LLFB128:
	save	%sp, -96, %sp
.LLCFI128:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+64], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL315
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL314
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL315
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL314
	 nop
.LL315:
	mov	0, %g1
.LL314:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE128:
	.size	GE__uGE_var_LC_u8, .-GE__uGE_var_LC_u8
	.align 4
	.type	GE__GE_var_LC_u8, #function
	.proc	014
GE__GE_var_LC_u8:
.LLFB129:
	save	%sp, -96, %sp
.LLCFI129:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+64], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL318
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL317
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL318
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL317
	 nop
.LL318:
	mov	0, %g1
.LL317:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE129:
	.size	GE__GE_var_LC_u8, .-GE__GE_var_LC_u8
	.align 4
	.type	GE__uGE_param_LC_u8, #function
	.proc	014
GE__uGE_param_LC_u8:
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
	blu	%icc, .LL321
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL320
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL321
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL320
	 nop
.LL321:
	mov	0, %g1
.LL320:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE130:
	.size	GE__uGE_param_LC_u8, .-GE__uGE_param_LC_u8
	.align 4
	.type	GE__GE_param_LC_u8, #function
	.proc	014
GE__GE_param_LC_u8:
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
	bg	%icc, .LL324
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL323
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL324
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL323
	 nop
.LL324:
	mov	0, %g1
.LL323:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE131:
	.size	GE__GE_param_LC_u8, .-GE__GE_param_LC_u8
	.align 4
	.type	GE__uGE_var_LC_L, #function
	.proc	014
GE__uGE_var_LC_L:
.LLFB132:
	save	%sp, -96, %sp
.LLCFI132:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+64], %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL327
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL326
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL327
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL326
	 nop
.LL327:
	mov	0, %g1
.LL326:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE132:
	.size	GE__uGE_var_LC_L, .-GE__uGE_var_LC_L
	.align 4
	.type	GE__GE_var_LC_L, #function
	.proc	014
GE__GE_var_LC_L:
.LLFB133:
	save	%sp, -96, %sp
.LLCFI133:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+64], %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL330
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL329
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL330
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL329
	 nop
.LL330:
	mov	0, %g1
.LL329:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE133:
	.size	GE__GE_var_LC_L, .-GE__GE_var_LC_L
	.align 4
	.type	GE__uGE_param_LC_L, #function
	.proc	014
GE__uGE_param_LC_L:
.LLFB134:
	save	%sp, -112, %sp
.LLCFI134:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	blu	%icc, .LL333
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL332
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL333
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL332
	 nop
.LL333:
	mov	0, %g1
.LL332:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE134:
	.size	GE__uGE_param_LC_L, .-GE__uGE_param_LC_L
	.align 4
	.type	GE__GE_param_LC_L, #function
	.proc	014
GE__GE_param_LC_L:
.LLFB135:
	save	%sp, -112, %sp
.LLCFI135:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	mov	1, %g1
	ld	[%fp-16], %o3
	cmp	%o3, %o4
	bg	%icc, .LL336
	 nop
	ld	[%fp-16], %o3
	cmp	%o3, %o4
	bne	%icc, .LL335
	 nop
	ld	[%fp-12], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL336
	 nop
	ld	[%fp-12], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL335
	 nop
.LL336:
	mov	0, %g1
.LL335:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE135:
	.size	GE__GE_param_LC_L, .-GE__GE_param_LC_L
	.align 4
	.type	GE__uGE_var_LC_i8, #function
	.proc	014
GE__uGE_var_LC_i8:
.LLFB136:
	save	%sp, -96, %sp
.LLCFI136:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL339
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL338
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL339
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL338
	 nop
.LL339:
	mov	0, %g1
.LL338:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE136:
	.size	GE__uGE_var_LC_i8, .-GE__uGE_var_LC_i8
	.align 4
	.type	GE__GE_var_LC_i8, #function
	.proc	014
GE__GE_var_LC_i8:
.LLFB137:
	save	%sp, -96, %sp
.LLCFI137:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL342
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL341
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL342
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL341
	 nop
.LL342:
	mov	0, %g1
.LL341:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE137:
	.size	GE__GE_var_LC_i8, .-GE__GE_var_LC_i8
	.align 4
	.type	GE__uGE_param_LC_i8, #function
	.proc	014
GE__uGE_param_LC_i8:
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
	blu	%icc, .LL345
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL344
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL345
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL344
	 nop
.LL345:
	mov	0, %g1
.LL344:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE138:
	.size	GE__uGE_param_LC_i8, .-GE__uGE_param_LC_i8
	.align 4
	.type	GE__GE_param_LC_i8, #function
	.proc	014
GE__GE_param_LC_i8:
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
	bg	%icc, .LL348
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL347
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL348
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL347
	 nop
.LL348:
	mov	0, %g1
.LL347:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE139:
	.size	GE__GE_param_LC_i8, .-GE__GE_param_LC_i8
	.align 4
	.type	GE__uGE_var_LC_u32, #function
	.proc	014
GE__uGE_var_LC_u32:
.LLFB140:
	save	%sp, -96, %sp
.LLCFI140:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL351
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL350
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL351
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL350
	 nop
.LL351:
	mov	0, %g1
.LL350:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE140:
	.size	GE__uGE_var_LC_u32, .-GE__uGE_var_LC_u32
	.align 4
	.type	GE__GE_var_LC_u32, #function
	.proc	014
GE__GE_var_LC_u32:
.LLFB141:
	save	%sp, -96, %sp
.LLCFI141:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL354
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL353
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL354
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL353
	 nop
.LL354:
	mov	0, %g1
.LL353:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE141:
	.size	GE__GE_var_LC_u32, .-GE__GE_var_LC_u32
	.align 4
	.type	GE__uGE_param_LC_u32, #function
	.proc	014
GE__uGE_param_LC_u32:
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
	blu	%icc, .LL357
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL356
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL357
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL356
	 nop
.LL357:
	mov	0, %g1
.LL356:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE142:
	.size	GE__uGE_param_LC_u32, .-GE__uGE_param_LC_u32
	.align 4
	.type	GE__GE_param_LC_u32, #function
	.proc	014
GE__GE_param_LC_u32:
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
	bg	%icc, .LL360
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL359
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL360
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL359
	 nop
.LL360:
	mov	0, %g1
.LL359:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE143:
	.size	GE__GE_param_LC_u32, .-GE__GE_param_LC_u32
	.align 4
	.type	GE__uGE_var_i32_u16, #function
	.proc	014
GE__uGE_var_i32_u16:
.LLFB144:
	save	%sp, -96, %sp
.LLCFI144:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE144:
	.size	GE__uGE_var_i32_u16, .-GE__uGE_var_i32_u16
	.align 4
	.type	GE__GE_var_i32_u16, #function
	.proc	014
GE__GE_var_i32_u16:
.LLFB145:
	save	%sp, -96, %sp
.LLCFI145:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE145:
	.size	GE__GE_var_i32_u16, .-GE__GE_var_i32_u16
	.align 4
	.type	GE__uGE_param_i32_u16, #function
	.proc	014
GE__uGE_param_i32_u16:
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
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE146:
	.size	GE__uGE_param_i32_u16, .-GE__uGE_param_i32_u16
	.align 4
	.type	GE__GE_param_i32_u16, #function
	.proc	014
GE__GE_param_i32_u16:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE147:
	.size	GE__GE_param_i32_u16, .-GE__GE_param_i32_u16
	.align 4
	.type	GE__uGE_var_i32_u64, #function
	.proc	014
GE__uGE_var_i32_u64:
.LLFB148:
	save	%sp, -96, %sp
.LLCFI148:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL367
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL366
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL367
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL366
	 nop
.LL367:
	mov	0, %g1
.LL366:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE148:
	.size	GE__uGE_var_i32_u64, .-GE__uGE_var_i32_u64
	.align 4
	.type	GE__GE_var_i32_u64, #function
	.proc	014
GE__GE_var_i32_u64:
.LLFB149:
	save	%sp, -96, %sp
.LLCFI149:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL370
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL369
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL370
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL369
	 nop
.LL370:
	mov	0, %g1
.LL369:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE149:
	.size	GE__GE_var_i32_u64, .-GE__GE_var_i32_u64
	.align 4
	.type	GE__uGE_param_i32_u64, #function
	.proc	014
GE__uGE_param_i32_u64:
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
	bgu	%icc, .LL373
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL372
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL373
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL372
	 nop
.LL373:
	mov	0, %g1
.LL372:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE150:
	.size	GE__uGE_param_i32_u64, .-GE__uGE_param_i32_u64
	.align 4
	.type	GE__GE_param_i32_u64, #function
	.proc	014
GE__GE_param_i32_u64:
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
	bg	%icc, .LL376
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL375
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL376
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL375
	 nop
.LL376:
	mov	0, %g1
.LL375:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE151:
	.size	GE__GE_param_i32_u64, .-GE__GE_param_i32_u64
	.align 4
	.type	GE__uGE_var_i32_LC, #function
	.proc	014
GE__uGE_var_i32_LC:
.LLFB152:
	save	%sp, -96, %sp
.LLCFI152:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL379
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL378
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL379
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL378
	 nop
.LL379:
	mov	0, %g1
.LL378:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE152:
	.size	GE__uGE_var_i32_LC, .-GE__uGE_var_i32_LC
	.align 4
	.type	GE__GE_var_i32_LC, #function
	.proc	014
GE__GE_var_i32_LC:
.LLFB153:
	save	%sp, -96, %sp
.LLCFI153:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL382
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL381
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL382
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL381
	 nop
.LL382:
	mov	0, %g1
.LL381:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE153:
	.size	GE__GE_var_i32_LC, .-GE__GE_var_i32_LC
	.align 4
	.type	GE__uGE_param_i32_LC, #function
	.proc	014
GE__uGE_param_i32_LC:
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
	bgu	%icc, .LL385
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL384
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL385
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL384
	 nop
.LL385:
	mov	0, %g1
.LL384:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE154:
	.size	GE__uGE_param_i32_LC, .-GE__uGE_param_i32_LC
	.align 4
	.type	GE__GE_param_i32_LC, #function
	.proc	014
GE__GE_param_i32_LC:
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
	bg	%icc, .LL388
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL387
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL388
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL387
	 nop
.LL388:
	mov	0, %g1
.LL387:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE155:
	.size	GE__GE_param_i32_LC, .-GE__GE_param_i32_LC
	.align 4
	.type	GE__uGE_var_i32_i32, #function
	.proc	014
GE__uGE_var_i32_i32:
.LLFB156:
	save	%sp, -96, %sp
.LLCFI156:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE156:
	.size	GE__uGE_var_i32_i32, .-GE__uGE_var_i32_i32
	.align 4
	.type	GE__GE_var_i32_i32, #function
	.proc	014
GE__GE_var_i32_i32:
.LLFB157:
	save	%sp, -96, %sp
.LLCFI157:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE157:
	.size	GE__GE_var_i32_i32, .-GE__GE_var_i32_i32
	.align 4
	.type	GE__uGE_param_i32_i32, #function
	.proc	014
GE__uGE_param_i32_i32:
.LLFB158:
	save	%sp, -96, %sp
.LLCFI158:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE158:
	.size	GE__uGE_param_i32_i32, .-GE__uGE_param_i32_i32
	.align 4
	.type	GE__GE_param_i32_i32, #function
	.proc	014
GE__GE_param_i32_i32:
.LLFB159:
	save	%sp, -96, %sp
.LLCFI159:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE159:
	.size	GE__GE_param_i32_i32, .-GE__GE_param_i32_i32
	.align 4
	.type	GE__uGE_var_i32_i16, #function
	.proc	014
GE__uGE_var_i32_i16:
.LLFB160:
	save	%sp, -96, %sp
.LLCFI160:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE160:
	.size	GE__uGE_var_i32_i16, .-GE__uGE_var_i32_i16
	.align 4
	.type	GE__GE_var_i32_i16, #function
	.proc	014
GE__GE_var_i32_i16:
.LLFB161:
	save	%sp, -96, %sp
.LLCFI161:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE161:
	.size	GE__GE_var_i32_i16, .-GE__GE_var_i32_i16
	.align 4
	.type	GE__uGE_param_i32_i16, #function
	.proc	014
GE__uGE_param_i32_i16:
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
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE162:
	.size	GE__uGE_param_i32_i16, .-GE__uGE_param_i32_i16
	.align 4
	.type	GE__GE_param_i32_i16, #function
	.proc	014
GE__GE_param_i32_i16:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE163:
	.size	GE__GE_param_i32_i16, .-GE__GE_param_i32_i16
	.align 4
	.type	GE__uGE_var_i32_I, #function
	.proc	014
GE__uGE_var_i32_I:
.LLFB164:
	save	%sp, -96, %sp
.LLCFI164:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+80], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE164:
	.size	GE__uGE_var_i32_I, .-GE__uGE_var_i32_I
	.align 4
	.type	GE__GE_var_i32_I, #function
	.proc	014
GE__GE_var_i32_I:
.LLFB165:
	save	%sp, -96, %sp
.LLCFI165:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+80], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE165:
	.size	GE__GE_var_i32_I, .-GE__GE_var_i32_I
	.align 4
	.type	GE__uGE_param_i32_I, #function
	.proc	014
GE__uGE_param_i32_I:
.LLFB166:
	save	%sp, -96, %sp
.LLCFI166:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE166:
	.size	GE__uGE_param_i32_I, .-GE__uGE_param_i32_I
	.align 4
	.type	GE__GE_param_i32_I, #function
	.proc	014
GE__GE_param_i32_I:
.LLFB167:
	save	%sp, -96, %sp
.LLCFI167:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE167:
	.size	GE__GE_param_i32_I, .-GE__GE_param_i32_I
	.align 4
	.type	GE__uGE_var_i32_i64, #function
	.proc	014
GE__uGE_var_i32_i64:
.LLFB168:
	save	%sp, -96, %sp
.LLCFI168:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL403
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL402
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL403
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL402
	 nop
.LL403:
	mov	0, %g1
.LL402:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE168:
	.size	GE__uGE_var_i32_i64, .-GE__uGE_var_i32_i64
	.align 4
	.type	GE__GE_var_i32_i64, #function
	.proc	014
GE__GE_var_i32_i64:
.LLFB169:
	save	%sp, -96, %sp
.LLCFI169:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL406
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL405
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL406
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL405
	 nop
.LL406:
	mov	0, %g1
.LL405:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE169:
	.size	GE__GE_var_i32_i64, .-GE__GE_var_i32_i64
	.align 4
	.type	GE__uGE_param_i32_i64, #function
	.proc	014
GE__uGE_param_i32_i64:
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
	bgu	%icc, .LL409
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL408
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL409
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL408
	 nop
.LL409:
	mov	0, %g1
.LL408:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE170:
	.size	GE__uGE_param_i32_i64, .-GE__uGE_param_i32_i64
	.align 4
	.type	GE__GE_param_i32_i64, #function
	.proc	014
GE__GE_param_i32_i64:
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
	bg	%icc, .LL412
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL411
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL412
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL411
	 nop
.LL412:
	mov	0, %g1
.LL411:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE171:
	.size	GE__GE_param_i32_i64, .-GE__GE_param_i32_i64
	.align 4
	.type	GE__uGE_var_i32_C, #function
	.proc	014
GE__uGE_var_i32_C:
.LLFB172:
	save	%sp, -96, %sp
.LLCFI172:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+96], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE172:
	.size	GE__uGE_var_i32_C, .-GE__uGE_var_i32_C
	.align 4
	.type	GE__GE_var_i32_C, #function
	.proc	014
GE__GE_var_i32_C:
.LLFB173:
	save	%sp, -96, %sp
.LLCFI173:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+96], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE173:
	.size	GE__GE_var_i32_C, .-GE__GE_var_i32_C
	.align 4
	.type	GE__uGE_param_i32_C, #function
	.proc	014
GE__uGE_param_i32_C:
.LLFB174:
	save	%sp, -96, %sp
.LLCFI174:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE174:
	.size	GE__uGE_param_i32_C, .-GE__uGE_param_i32_C
	.align 4
	.type	GE__GE_param_i32_C, #function
	.proc	014
GE__GE_param_i32_C:
.LLFB175:
	save	%sp, -96, %sp
.LLCFI175:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE175:
	.size	GE__GE_param_i32_C, .-GE__GE_param_i32_C
	.align 4
	.type	GE__uGE_var_i32_u8, #function
	.proc	014
GE__uGE_var_i32_u8:
.LLFB176:
	save	%sp, -96, %sp
.LLCFI176:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE176:
	.size	GE__uGE_var_i32_u8, .-GE__uGE_var_i32_u8
	.align 4
	.type	GE__GE_var_i32_u8, #function
	.proc	014
GE__GE_var_i32_u8:
.LLFB177:
	save	%sp, -96, %sp
.LLCFI177:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE177:
	.size	GE__GE_var_i32_u8, .-GE__GE_var_i32_u8
	.align 4
	.type	GE__uGE_param_i32_u8, #function
	.proc	014
GE__uGE_param_i32_u8:
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
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE178:
	.size	GE__uGE_param_i32_u8, .-GE__uGE_param_i32_u8
	.align 4
	.type	GE__GE_param_i32_u8, #function
	.proc	014
GE__GE_param_i32_u8:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE179:
	.size	GE__GE_param_i32_u8, .-GE__GE_param_i32_u8
	.align 4
	.type	GE__uGE_var_i32_L, #function
	.proc	014
GE__uGE_var_i32_L:
.LLFB180:
	save	%sp, -96, %sp
.LLCFI180:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL423
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL422
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL423
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL422
	 nop
.LL423:
	mov	0, %g1
.LL422:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE180:
	.size	GE__uGE_var_i32_L, .-GE__uGE_var_i32_L
	.align 4
	.type	GE__GE_var_i32_L, #function
	.proc	014
GE__GE_var_i32_L:
.LLFB181:
	save	%sp, -96, %sp
.LLCFI181:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL426
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL425
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL426
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL425
	 nop
.LL426:
	mov	0, %g1
.LL425:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE181:
	.size	GE__GE_var_i32_L, .-GE__GE_var_i32_L
	.align 4
	.type	GE__uGE_param_i32_L, #function
	.proc	014
GE__uGE_param_i32_L:
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
	bgu	%icc, .LL429
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL428
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL429
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL428
	 nop
.LL429:
	mov	0, %g1
.LL428:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE182:
	.size	GE__uGE_param_i32_L, .-GE__uGE_param_i32_L
	.align 4
	.type	GE__GE_param_i32_L, #function
	.proc	014
GE__GE_param_i32_L:
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
	bg	%icc, .LL432
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL431
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL432
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL431
	 nop
.LL432:
	mov	0, %g1
.LL431:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE183:
	.size	GE__GE_param_i32_L, .-GE__GE_param_i32_L
	.align 4
	.type	GE__uGE_var_i32_i8, #function
	.proc	014
GE__uGE_var_i32_i8:
.LLFB184:
	save	%sp, -96, %sp
.LLCFI184:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE184:
	.size	GE__uGE_var_i32_i8, .-GE__uGE_var_i32_i8
	.align 4
	.type	GE__GE_var_i32_i8, #function
	.proc	014
GE__GE_var_i32_i8:
.LLFB185:
	save	%sp, -96, %sp
.LLCFI185:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE185:
	.size	GE__GE_var_i32_i8, .-GE__GE_var_i32_i8
	.align 4
	.type	GE__uGE_param_i32_i8, #function
	.proc	014
GE__uGE_param_i32_i8:
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
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE186:
	.size	GE__uGE_param_i32_i8, .-GE__uGE_param_i32_i8
	.align 4
	.type	GE__GE_param_i32_i8, #function
	.proc	014
GE__GE_param_i32_i8:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE187:
	.size	GE__GE_param_i32_i8, .-GE__GE_param_i32_i8
	.align 4
	.type	GE__uGE_var_i32_u32, #function
	.proc	014
GE__uGE_var_i32_u32:
.LLFB188:
	save	%sp, -96, %sp
.LLCFI188:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+132], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE188:
	.size	GE__uGE_var_i32_u32, .-GE__uGE_var_i32_u32
	.align 4
	.type	GE__GE_var_i32_u32, #function
	.proc	014
GE__GE_var_i32_u32:
.LLFB189:
	save	%sp, -96, %sp
.LLCFI189:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+132], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE189:
	.size	GE__GE_var_i32_u32, .-GE__GE_var_i32_u32
	.align 4
	.type	GE__uGE_param_i32_u32, #function
	.proc	014
GE__uGE_param_i32_u32:
.LLFB190:
	save	%sp, -96, %sp
.LLCFI190:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE190:
	.size	GE__uGE_param_i32_u32, .-GE__uGE_param_i32_u32
	.align 4
	.type	GE__GE_param_i32_u32, #function
	.proc	014
GE__GE_param_i32_u32:
.LLFB191:
	save	%sp, -96, %sp
.LLCFI191:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE191:
	.size	GE__GE_param_i32_u32, .-GE__GE_param_i32_u32
	.align 4
	.type	GE__uGE_var_i16_u16, #function
	.proc	014
GE__uGE_var_i16_u16:
.LLFB192:
	save	%sp, -96, %sp
.LLCFI192:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE192:
	.size	GE__uGE_var_i16_u16, .-GE__uGE_var_i16_u16
	.align 4
	.type	GE__GE_var_i16_u16, #function
	.proc	014
GE__GE_var_i16_u16:
.LLFB193:
	save	%sp, -96, %sp
.LLCFI193:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE193:
	.size	GE__GE_var_i16_u16, .-GE__GE_var_i16_u16
	.align 4
	.type	GE__uGE_param_i16_u16, #function
	.proc	014
GE__uGE_param_i16_u16:
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
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE194:
	.size	GE__uGE_param_i16_u16, .-GE__uGE_param_i16_u16
	.align 4
	.type	GE__GE_param_i16_u16, #function
	.proc	014
GE__GE_param_i16_u16:
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
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE195:
	.size	GE__GE_param_i16_u16, .-GE__GE_param_i16_u16
	.align 4
	.type	GE__uGE_var_i16_u64, #function
	.proc	014
GE__uGE_var_i16_u64:
.LLFB196:
	save	%sp, -96, %sp
.LLCFI196:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL447
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL446
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL447
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL446
	 nop
.LL447:
	mov	0, %g1
.LL446:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE196:
	.size	GE__uGE_var_i16_u64, .-GE__uGE_var_i16_u64
	.align 4
	.type	GE__GE_var_i16_u64, #function
	.proc	014
GE__GE_var_i16_u64:
.LLFB197:
	save	%sp, -96, %sp
.LLCFI197:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL450
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL449
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL450
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL449
	 nop
.LL450:
	mov	0, %g1
.LL449:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE197:
	.size	GE__GE_var_i16_u64, .-GE__GE_var_i16_u64
	.align 4
	.type	GE__uGE_param_i16_u64, #function
	.proc	014
GE__uGE_param_i16_u64:
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
	bgu	%icc, .LL453
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL452
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL453
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL452
	 nop
.LL453:
	mov	0, %g1
.LL452:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE198:
	.size	GE__uGE_param_i16_u64, .-GE__uGE_param_i16_u64
	.align 4
	.type	GE__GE_param_i16_u64, #function
	.proc	014
GE__GE_param_i16_u64:
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
	bg	%icc, .LL456
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL455
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL456
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL455
	 nop
.LL456:
	mov	0, %g1
.LL455:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE199:
	.size	GE__GE_param_i16_u64, .-GE__GE_param_i16_u64
	.align 4
	.type	GE__uGE_var_i16_LC, #function
	.proc	014
GE__uGE_var_i16_LC:
.LLFB200:
	save	%sp, -96, %sp
.LLCFI200:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL459
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL458
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL459
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL458
	 nop
.LL459:
	mov	0, %g1
.LL458:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE200:
	.size	GE__uGE_var_i16_LC, .-GE__uGE_var_i16_LC
	.align 4
	.type	GE__GE_var_i16_LC, #function
	.proc	014
GE__GE_var_i16_LC:
.LLFB201:
	save	%sp, -96, %sp
.LLCFI201:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL462
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL461
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL462
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL461
	 nop
.LL462:
	mov	0, %g1
.LL461:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE201:
	.size	GE__GE_var_i16_LC, .-GE__GE_var_i16_LC
	.align 4
	.type	GE__uGE_param_i16_LC, #function
	.proc	014
GE__uGE_param_i16_LC:
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
	bgu	%icc, .LL465
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL464
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL465
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL464
	 nop
.LL465:
	mov	0, %g1
.LL464:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE202:
	.size	GE__uGE_param_i16_LC, .-GE__uGE_param_i16_LC
	.align 4
	.type	GE__GE_param_i16_LC, #function
	.proc	014
GE__GE_param_i16_LC:
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
	bg	%icc, .LL468
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL467
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL468
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL467
	 nop
.LL468:
	mov	0, %g1
.LL467:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE203:
	.size	GE__GE_param_i16_LC, .-GE__GE_param_i16_LC
	.align 4
	.type	GE__uGE_var_i16_i32, #function
	.proc	014
GE__uGE_var_i16_i32:
.LLFB204:
	save	%sp, -96, %sp
.LLCFI204:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE204:
	.size	GE__uGE_var_i16_i32, .-GE__uGE_var_i16_i32
	.align 4
	.type	GE__GE_var_i16_i32, #function
	.proc	014
GE__GE_var_i16_i32:
.LLFB205:
	save	%sp, -96, %sp
.LLCFI205:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE205:
	.size	GE__GE_var_i16_i32, .-GE__GE_var_i16_i32
	.align 4
	.type	GE__uGE_param_i16_i32, #function
	.proc	014
GE__uGE_param_i16_i32:
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
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE206:
	.size	GE__uGE_param_i16_i32, .-GE__uGE_param_i16_i32
	.align 4
	.type	GE__GE_param_i16_i32, #function
	.proc	014
GE__GE_param_i16_i32:
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
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE207:
	.size	GE__GE_param_i16_i32, .-GE__GE_param_i16_i32
	.align 4
	.type	GE__uGE_var_i16_i16, #function
	.proc	014
GE__uGE_var_i16_i16:
.LLFB208:
	save	%sp, -96, %sp
.LLCFI208:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE208:
	.size	GE__uGE_var_i16_i16, .-GE__uGE_var_i16_i16
	.align 4
	.type	GE__GE_var_i16_i16, #function
	.proc	014
GE__GE_var_i16_i16:
.LLFB209:
	save	%sp, -96, %sp
.LLCFI209:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE209:
	.size	GE__GE_var_i16_i16, .-GE__GE_var_i16_i16
	.align 4
	.type	GE__uGE_param_i16_i16, #function
	.proc	014
GE__uGE_param_i16_i16:
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
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE210:
	.size	GE__uGE_param_i16_i16, .-GE__uGE_param_i16_i16
	.align 4
	.type	GE__GE_param_i16_i16, #function
	.proc	014
GE__GE_param_i16_i16:
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
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE211:
	.size	GE__GE_param_i16_i16, .-GE__GE_param_i16_i16
	.align 4
	.type	GE__uGE_var_i16_I, #function
	.proc	014
GE__uGE_var_i16_I:
.LLFB212:
	save	%sp, -96, %sp
.LLCFI212:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+80], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE212:
	.size	GE__uGE_var_i16_I, .-GE__uGE_var_i16_I
	.align 4
	.type	GE__GE_var_i16_I, #function
	.proc	014
GE__GE_var_i16_I:
.LLFB213:
	save	%sp, -96, %sp
.LLCFI213:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+80], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE213:
	.size	GE__GE_var_i16_I, .-GE__GE_var_i16_I
	.align 4
	.type	GE__uGE_param_i16_I, #function
	.proc	014
GE__uGE_param_i16_I:
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
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE214:
	.size	GE__uGE_param_i16_I, .-GE__uGE_param_i16_I
	.align 4
	.type	GE__GE_param_i16_I, #function
	.proc	014
GE__GE_param_i16_I:
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
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE215:
	.size	GE__GE_param_i16_I, .-GE__GE_param_i16_I
	.align 4
	.type	GE__uGE_var_i16_i64, #function
	.proc	014
GE__uGE_var_i16_i64:
.LLFB216:
	save	%sp, -96, %sp
.LLCFI216:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL483
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL482
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL483
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL482
	 nop
.LL483:
	mov	0, %g1
.LL482:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE216:
	.size	GE__uGE_var_i16_i64, .-GE__uGE_var_i16_i64
	.align 4
	.type	GE__GE_var_i16_i64, #function
	.proc	014
GE__GE_var_i16_i64:
.LLFB217:
	save	%sp, -96, %sp
.LLCFI217:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL486
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL485
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL486
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL485
	 nop
.LL486:
	mov	0, %g1
.LL485:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE217:
	.size	GE__GE_var_i16_i64, .-GE__GE_var_i16_i64
	.align 4
	.type	GE__uGE_param_i16_i64, #function
	.proc	014
GE__uGE_param_i16_i64:
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
	bgu	%icc, .LL489
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL488
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL489
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL488
	 nop
.LL489:
	mov	0, %g1
.LL488:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE218:
	.size	GE__uGE_param_i16_i64, .-GE__uGE_param_i16_i64
	.align 4
	.type	GE__GE_param_i16_i64, #function
	.proc	014
GE__GE_param_i16_i64:
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
	bg	%icc, .LL492
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL491
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL492
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL491
	 nop
.LL492:
	mov	0, %g1
.LL491:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE219:
	.size	GE__GE_param_i16_i64, .-GE__GE_param_i16_i64
	.align 4
	.type	GE__uGE_var_i16_C, #function
	.proc	014
GE__uGE_var_i16_C:
.LLFB220:
	save	%sp, -96, %sp
.LLCFI220:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+96], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE220:
	.size	GE__uGE_var_i16_C, .-GE__uGE_var_i16_C
	.align 4
	.type	GE__GE_var_i16_C, #function
	.proc	014
GE__GE_var_i16_C:
.LLFB221:
	save	%sp, -96, %sp
.LLCFI221:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+96], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE221:
	.size	GE__GE_var_i16_C, .-GE__GE_var_i16_C
	.align 4
	.type	GE__uGE_param_i16_C, #function
	.proc	014
GE__uGE_param_i16_C:
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
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE222:
	.size	GE__uGE_param_i16_C, .-GE__uGE_param_i16_C
	.align 4
	.type	GE__GE_param_i16_C, #function
	.proc	014
GE__GE_param_i16_C:
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
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE223:
	.size	GE__GE_param_i16_C, .-GE__GE_param_i16_C
	.align 4
	.type	GE__uGE_var_i16_u8, #function
	.proc	014
GE__uGE_var_i16_u8:
.LLFB224:
	save	%sp, -96, %sp
.LLCFI224:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE224:
	.size	GE__uGE_var_i16_u8, .-GE__uGE_var_i16_u8
	.align 4
	.type	GE__GE_var_i16_u8, #function
	.proc	014
GE__GE_var_i16_u8:
.LLFB225:
	save	%sp, -96, %sp
.LLCFI225:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE225:
	.size	GE__GE_var_i16_u8, .-GE__GE_var_i16_u8
	.align 4
	.type	GE__uGE_param_i16_u8, #function
	.proc	014
GE__uGE_param_i16_u8:
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
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE226:
	.size	GE__uGE_param_i16_u8, .-GE__uGE_param_i16_u8
	.align 4
	.type	GE__GE_param_i16_u8, #function
	.proc	014
GE__GE_param_i16_u8:
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
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE227:
	.size	GE__GE_param_i16_u8, .-GE__GE_param_i16_u8
	.align 4
	.type	GE__uGE_var_i16_L, #function
	.proc	014
GE__uGE_var_i16_L:
.LLFB228:
	save	%sp, -96, %sp
.LLCFI228:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL503
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL502
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL503
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL502
	 nop
.LL503:
	mov	0, %g1
.LL502:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE228:
	.size	GE__uGE_var_i16_L, .-GE__uGE_var_i16_L
	.align 4
	.type	GE__GE_var_i16_L, #function
	.proc	014
GE__GE_var_i16_L:
.LLFB229:
	save	%sp, -96, %sp
.LLCFI229:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL506
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL505
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL506
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL505
	 nop
.LL506:
	mov	0, %g1
.LL505:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE229:
	.size	GE__GE_var_i16_L, .-GE__GE_var_i16_L
	.align 4
	.type	GE__uGE_param_i16_L, #function
	.proc	014
GE__uGE_param_i16_L:
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
	bgu	%icc, .LL509
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL508
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL509
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL508
	 nop
.LL509:
	mov	0, %g1
.LL508:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE230:
	.size	GE__uGE_param_i16_L, .-GE__uGE_param_i16_L
	.align 4
	.type	GE__GE_param_i16_L, #function
	.proc	014
GE__GE_param_i16_L:
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
	bg	%icc, .LL512
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL511
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL512
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL511
	 nop
.LL512:
	mov	0, %g1
.LL511:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE231:
	.size	GE__GE_param_i16_L, .-GE__GE_param_i16_L
	.align 4
	.type	GE__uGE_var_i16_i8, #function
	.proc	014
GE__uGE_var_i16_i8:
.LLFB232:
	save	%sp, -96, %sp
.LLCFI232:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE232:
	.size	GE__uGE_var_i16_i8, .-GE__uGE_var_i16_i8
	.align 4
	.type	GE__GE_var_i16_i8, #function
	.proc	014
GE__GE_var_i16_i8:
.LLFB233:
	save	%sp, -96, %sp
.LLCFI233:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
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
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE233:
	.size	GE__GE_var_i16_i8, .-GE__GE_var_i16_i8
	.align 4
	.type	GE__uGE_param_i16_i8, #function
	.proc	014
GE__uGE_param_i16_i8:
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
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE234:
	.size	GE__uGE_param_i16_i8, .-GE__uGE_param_i16_i8
	.align 4
	.type	GE__GE_param_i16_i8, #function
	.proc	014
GE__GE_param_i16_i8:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE235:
	.size	GE__GE_param_i16_i8, .-GE__GE_param_i16_i8
	.align 4
	.type	GE__uGE_var_i16_u32, #function
	.proc	014
GE__uGE_var_i16_u32:
.LLFB236:
	save	%sp, -96, %sp
.LLCFI236:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+132], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE236:
	.size	GE__uGE_var_i16_u32, .-GE__uGE_var_i16_u32
	.align 4
	.type	GE__GE_var_i16_u32, #function
	.proc	014
GE__GE_var_i16_u32:
.LLFB237:
	save	%sp, -96, %sp
.LLCFI237:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+132], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE237:
	.size	GE__GE_var_i16_u32, .-GE__GE_var_i16_u32
	.align 4
	.type	GE__uGE_param_i16_u32, #function
	.proc	014
GE__uGE_param_i16_u32:
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
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE238:
	.size	GE__uGE_param_i16_u32, .-GE__uGE_param_i16_u32
	.align 4
	.type	GE__GE_param_i16_u32, #function
	.proc	014
GE__GE_param_i16_u32:
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
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE239:
	.size	GE__GE_param_i16_u32, .-GE__GE_param_i16_u32
	.align 4
	.type	GE__uGE_var_I_u16, #function
	.proc	014
GE__uGE_var_I_u16:
.LLFB240:
	save	%sp, -96, %sp
.LLCFI240:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE240:
	.size	GE__uGE_var_I_u16, .-GE__uGE_var_I_u16
	.align 4
	.type	GE__GE_var_I_u16, #function
	.proc	014
GE__GE_var_I_u16:
.LLFB241:
	save	%sp, -96, %sp
.LLCFI241:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE241:
	.size	GE__GE_var_I_u16, .-GE__GE_var_I_u16
	.align 4
	.type	GE__uGE_param_I_u16, #function
	.proc	014
GE__uGE_param_I_u16:
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
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE242:
	.size	GE__uGE_param_I_u16, .-GE__uGE_param_I_u16
	.align 4
	.type	GE__GE_param_I_u16, #function
	.proc	014
GE__GE_param_I_u16:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE243:
	.size	GE__GE_param_I_u16, .-GE__GE_param_I_u16
	.align 4
	.type	GE__uGE_var_I_u64, #function
	.proc	014
GE__uGE_var_I_u64:
.LLFB244:
	save	%sp, -96, %sp
.LLCFI244:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL527
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL526
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL527
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL526
	 nop
.LL527:
	mov	0, %g1
.LL526:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE244:
	.size	GE__uGE_var_I_u64, .-GE__uGE_var_I_u64
	.align 4
	.type	GE__GE_var_I_u64, #function
	.proc	014
GE__GE_var_I_u64:
.LLFB245:
	save	%sp, -96, %sp
.LLCFI245:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL530
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL529
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL530
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL529
	 nop
.LL530:
	mov	0, %g1
.LL529:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE245:
	.size	GE__GE_var_I_u64, .-GE__GE_var_I_u64
	.align 4
	.type	GE__uGE_param_I_u64, #function
	.proc	014
GE__uGE_param_I_u64:
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
	bgu	%icc, .LL533
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL532
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL533
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL532
	 nop
.LL533:
	mov	0, %g1
.LL532:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE246:
	.size	GE__uGE_param_I_u64, .-GE__uGE_param_I_u64
	.align 4
	.type	GE__GE_param_I_u64, #function
	.proc	014
GE__GE_param_I_u64:
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
	bg	%icc, .LL536
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL535
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL536
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL535
	 nop
.LL536:
	mov	0, %g1
.LL535:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE247:
	.size	GE__GE_param_I_u64, .-GE__GE_param_I_u64
	.align 4
	.type	GE__uGE_var_I_LC, #function
	.proc	014
GE__uGE_var_I_LC:
.LLFB248:
	save	%sp, -96, %sp
.LLCFI248:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL539
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL538
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL539
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL538
	 nop
.LL539:
	mov	0, %g1
.LL538:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE248:
	.size	GE__uGE_var_I_LC, .-GE__uGE_var_I_LC
	.align 4
	.type	GE__GE_var_I_LC, #function
	.proc	014
GE__GE_var_I_LC:
.LLFB249:
	save	%sp, -96, %sp
.LLCFI249:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL542
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL541
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL542
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL541
	 nop
.LL542:
	mov	0, %g1
.LL541:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE249:
	.size	GE__GE_var_I_LC, .-GE__GE_var_I_LC
	.align 4
	.type	GE__uGE_param_I_LC, #function
	.proc	014
GE__uGE_param_I_LC:
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
	bgu	%icc, .LL545
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL544
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL545
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL544
	 nop
.LL545:
	mov	0, %g1
.LL544:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE250:
	.size	GE__uGE_param_I_LC, .-GE__uGE_param_I_LC
	.align 4
	.type	GE__GE_param_I_LC, #function
	.proc	014
GE__GE_param_I_LC:
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
	bg	%icc, .LL548
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL547
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL548
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL547
	 nop
.LL548:
	mov	0, %g1
.LL547:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE251:
	.size	GE__GE_param_I_LC, .-GE__GE_param_I_LC
	.align 4
	.type	GE__uGE_var_I_i32, #function
	.proc	014
GE__uGE_var_I_i32:
.LLFB252:
	save	%sp, -96, %sp
.LLCFI252:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE252:
	.size	GE__uGE_var_I_i32, .-GE__uGE_var_I_i32
	.align 4
	.type	GE__GE_var_I_i32, #function
	.proc	014
GE__GE_var_I_i32:
.LLFB253:
	save	%sp, -96, %sp
.LLCFI253:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE253:
	.size	GE__GE_var_I_i32, .-GE__GE_var_I_i32
	.align 4
	.type	GE__uGE_param_I_i32, #function
	.proc	014
GE__uGE_param_I_i32:
.LLFB254:
	save	%sp, -96, %sp
.LLCFI254:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE254:
	.size	GE__uGE_param_I_i32, .-GE__uGE_param_I_i32
	.align 4
	.type	GE__GE_param_I_i32, #function
	.proc	014
GE__GE_param_I_i32:
.LLFB255:
	save	%sp, -96, %sp
.LLCFI255:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE255:
	.size	GE__GE_param_I_i32, .-GE__GE_param_I_i32
	.align 4
	.type	GE__uGE_var_I_i16, #function
	.proc	014
GE__uGE_var_I_i16:
.LLFB256:
	save	%sp, -96, %sp
.LLCFI256:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE256:
	.size	GE__uGE_var_I_i16, .-GE__uGE_var_I_i16
	.align 4
	.type	GE__GE_var_I_i16, #function
	.proc	014
GE__GE_var_I_i16:
.LLFB257:
	save	%sp, -96, %sp
.LLCFI257:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE257:
	.size	GE__GE_var_I_i16, .-GE__GE_var_I_i16
	.align 4
	.type	GE__uGE_param_I_i16, #function
	.proc	014
GE__uGE_param_I_i16:
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
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE258:
	.size	GE__uGE_param_I_i16, .-GE__uGE_param_I_i16
	.align 4
	.type	GE__GE_param_I_i16, #function
	.proc	014
GE__GE_param_I_i16:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE259:
	.size	GE__GE_param_I_i16, .-GE__GE_param_I_i16
	.align 4
	.type	GE__uGE_var_I_I, #function
	.proc	014
GE__uGE_var_I_I:
.LLFB260:
	save	%sp, -96, %sp
.LLCFI260:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE260:
	.size	GE__uGE_var_I_I, .-GE__uGE_var_I_I
	.align 4
	.type	GE__GE_var_I_I, #function
	.proc	014
GE__GE_var_I_I:
.LLFB261:
	save	%sp, -96, %sp
.LLCFI261:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE261:
	.size	GE__GE_var_I_I, .-GE__GE_var_I_I
	.align 4
	.type	GE__uGE_param_I_I, #function
	.proc	014
GE__uGE_param_I_I:
.LLFB262:
	save	%sp, -96, %sp
.LLCFI262:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE262:
	.size	GE__uGE_param_I_I, .-GE__uGE_param_I_I
	.align 4
	.type	GE__GE_param_I_I, #function
	.proc	014
GE__GE_param_I_I:
.LLFB263:
	save	%sp, -96, %sp
.LLCFI263:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE263:
	.size	GE__GE_param_I_I, .-GE__GE_param_I_I
	.align 4
	.type	GE__uGE_var_I_i64, #function
	.proc	014
GE__uGE_var_I_i64:
.LLFB264:
	save	%sp, -96, %sp
.LLCFI264:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL563
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL562
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL563
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL562
	 nop
.LL563:
	mov	0, %g1
.LL562:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE264:
	.size	GE__uGE_var_I_i64, .-GE__uGE_var_I_i64
	.align 4
	.type	GE__GE_var_I_i64, #function
	.proc	014
GE__GE_var_I_i64:
.LLFB265:
	save	%sp, -96, %sp
.LLCFI265:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL566
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL565
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL566
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL565
	 nop
.LL566:
	mov	0, %g1
.LL565:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE265:
	.size	GE__GE_var_I_i64, .-GE__GE_var_I_i64
	.align 4
	.type	GE__uGE_param_I_i64, #function
	.proc	014
GE__uGE_param_I_i64:
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
	bgu	%icc, .LL569
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL568
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL569
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL568
	 nop
.LL569:
	mov	0, %g1
.LL568:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE266:
	.size	GE__uGE_param_I_i64, .-GE__uGE_param_I_i64
	.align 4
	.type	GE__GE_param_I_i64, #function
	.proc	014
GE__GE_param_I_i64:
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
	bg	%icc, .LL572
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL571
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL572
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL571
	 nop
.LL572:
	mov	0, %g1
.LL571:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE267:
	.size	GE__GE_param_I_i64, .-GE__GE_param_I_i64
	.align 4
	.type	GE__uGE_var_I_C, #function
	.proc	014
GE__uGE_var_I_C:
.LLFB268:
	save	%sp, -96, %sp
.LLCFI268:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+96], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE268:
	.size	GE__uGE_var_I_C, .-GE__uGE_var_I_C
	.align 4
	.type	GE__GE_var_I_C, #function
	.proc	014
GE__GE_var_I_C:
.LLFB269:
	save	%sp, -96, %sp
.LLCFI269:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+96], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE269:
	.size	GE__GE_var_I_C, .-GE__GE_var_I_C
	.align 4
	.type	GE__uGE_param_I_C, #function
	.proc	014
GE__uGE_param_I_C:
.LLFB270:
	save	%sp, -96, %sp
.LLCFI270:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE270:
	.size	GE__uGE_param_I_C, .-GE__uGE_param_I_C
	.align 4
	.type	GE__GE_param_I_C, #function
	.proc	014
GE__GE_param_I_C:
.LLFB271:
	save	%sp, -96, %sp
.LLCFI271:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE271:
	.size	GE__GE_param_I_C, .-GE__GE_param_I_C
	.align 4
	.type	GE__uGE_var_I_u8, #function
	.proc	014
GE__uGE_var_I_u8:
.LLFB272:
	save	%sp, -96, %sp
.LLCFI272:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE272:
	.size	GE__uGE_var_I_u8, .-GE__uGE_var_I_u8
	.align 4
	.type	GE__GE_var_I_u8, #function
	.proc	014
GE__GE_var_I_u8:
.LLFB273:
	save	%sp, -96, %sp
.LLCFI273:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE273:
	.size	GE__GE_var_I_u8, .-GE__GE_var_I_u8
	.align 4
	.type	GE__uGE_param_I_u8, #function
	.proc	014
GE__uGE_param_I_u8:
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
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE274:
	.size	GE__uGE_param_I_u8, .-GE__uGE_param_I_u8
	.align 4
	.type	GE__GE_param_I_u8, #function
	.proc	014
GE__GE_param_I_u8:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE275:
	.size	GE__GE_param_I_u8, .-GE__GE_param_I_u8
	.align 4
	.type	GE__uGE_var_I_L, #function
	.proc	014
GE__uGE_var_I_L:
.LLFB276:
	save	%sp, -96, %sp
.LLCFI276:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL583
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL582
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL583
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL582
	 nop
.LL583:
	mov	0, %g1
.LL582:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE276:
	.size	GE__uGE_var_I_L, .-GE__uGE_var_I_L
	.align 4
	.type	GE__GE_var_I_L, #function
	.proc	014
GE__GE_var_I_L:
.LLFB277:
	save	%sp, -96, %sp
.LLCFI277:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL586
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL585
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL586
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL585
	 nop
.LL586:
	mov	0, %g1
.LL585:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE277:
	.size	GE__GE_var_I_L, .-GE__GE_var_I_L
	.align 4
	.type	GE__uGE_param_I_L, #function
	.proc	014
GE__uGE_param_I_L:
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
	bgu	%icc, .LL589
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL588
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL589
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL588
	 nop
.LL589:
	mov	0, %g1
.LL588:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE278:
	.size	GE__uGE_param_I_L, .-GE__uGE_param_I_L
	.align 4
	.type	GE__GE_param_I_L, #function
	.proc	014
GE__GE_param_I_L:
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
	bg	%icc, .LL592
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL591
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL592
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL591
	 nop
.LL592:
	mov	0, %g1
.LL591:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE279:
	.size	GE__GE_param_I_L, .-GE__GE_param_I_L
	.align 4
	.type	GE__uGE_var_I_i8, #function
	.proc	014
GE__uGE_var_I_i8:
.LLFB280:
	save	%sp, -96, %sp
.LLCFI280:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE280:
	.size	GE__uGE_var_I_i8, .-GE__uGE_var_I_i8
	.align 4
	.type	GE__GE_var_I_i8, #function
	.proc	014
GE__GE_var_I_i8:
.LLFB281:
	save	%sp, -96, %sp
.LLCFI281:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE281:
	.size	GE__GE_var_I_i8, .-GE__GE_var_I_i8
	.align 4
	.type	GE__uGE_param_I_i8, #function
	.proc	014
GE__uGE_param_I_i8:
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
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE282:
	.size	GE__uGE_param_I_i8, .-GE__uGE_param_I_i8
	.align 4
	.type	GE__GE_param_I_i8, #function
	.proc	014
GE__GE_param_I_i8:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE283:
	.size	GE__GE_param_I_i8, .-GE__GE_param_I_i8
	.align 4
	.type	GE__uGE_var_I_u32, #function
	.proc	014
GE__uGE_var_I_u32:
.LLFB284:
	save	%sp, -96, %sp
.LLCFI284:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+132], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE284:
	.size	GE__uGE_var_I_u32, .-GE__uGE_var_I_u32
	.align 4
	.type	GE__GE_var_I_u32, #function
	.proc	014
GE__GE_var_I_u32:
.LLFB285:
	save	%sp, -96, %sp
.LLCFI285:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+132], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE285:
	.size	GE__GE_var_I_u32, .-GE__GE_var_I_u32
	.align 4
	.type	GE__uGE_param_I_u32, #function
	.proc	014
GE__uGE_param_I_u32:
.LLFB286:
	save	%sp, -96, %sp
.LLCFI286:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE286:
	.size	GE__uGE_param_I_u32, .-GE__uGE_param_I_u32
	.align 4
	.type	GE__GE_param_I_u32, #function
	.proc	014
GE__GE_param_I_u32:
.LLFB287:
	save	%sp, -96, %sp
.LLCFI287:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE287:
	.size	GE__GE_param_I_u32, .-GE__GE_param_I_u32
	.align 4
	.type	GE__uGE_var_i64_u16, #function
	.proc	014
GE__uGE_var_i64_u16:
.LLFB288:
	save	%sp, -96, %sp
.LLCFI288:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+88], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL603
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL602
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL603
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL602
	 nop
.LL603:
	mov	0, %g1
.LL602:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE288:
	.size	GE__uGE_var_i64_u16, .-GE__uGE_var_i64_u16
	.align 4
	.type	GE__GE_var_i64_u16, #function
	.proc	014
GE__GE_var_i64_u16:
.LLFB289:
	save	%sp, -96, %sp
.LLCFI289:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+88], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL606
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL605
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL606
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL605
	 nop
.LL606:
	mov	0, %g1
.LL605:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE289:
	.size	GE__GE_var_i64_u16, .-GE__GE_var_i64_u16
	.align 4
	.type	GE__uGE_param_i64_u16, #function
	.proc	014
GE__uGE_param_i64_u16:
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
	bgu	%icc, .LL609
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL608
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL609
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL608
	 nop
.LL609:
	mov	0, %g1
.LL608:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE290:
	.size	GE__uGE_param_i64_u16, .-GE__uGE_param_i64_u16
	.align 4
	.type	GE__GE_param_i64_u16, #function
	.proc	014
GE__GE_param_i64_u16:
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
	bl	%icc, .LL612
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL611
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL612
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL611
	 nop
.LL612:
	mov	0, %g1
.LL611:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE291:
	.size	GE__GE_param_i64_u16, .-GE__GE_param_i64_u16
	.align 4
	.type	GE__uGE_var_i64_u64, #function
	.proc	014
GE__uGE_var_i64_u64:
.LLFB292:
	save	%sp, -96, %sp
.LLCFI292:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+88], %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL615
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL614
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL615
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL614
	 nop
.LL615:
	mov	0, %g1
.LL614:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE292:
	.size	GE__uGE_var_i64_u64, .-GE__uGE_var_i64_u64
	.align 4
	.type	GE__GE_var_i64_u64, #function
	.proc	014
GE__GE_var_i64_u64:
.LLFB293:
	save	%sp, -96, %sp
.LLCFI293:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+88], %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL618
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL617
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL618
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL617
	 nop
.LL618:
	mov	0, %g1
.LL617:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE293:
	.size	GE__GE_var_i64_u64, .-GE__GE_var_i64_u64
	.align 4
	.type	GE__uGE_param_i64_u64, #function
	.proc	014
GE__uGE_param_i64_u64:
.LLFB294:
	save	%sp, -112, %sp
.LLCFI294:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	ldd	[%fp-16], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL621
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL620
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL621
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL620
	 nop
.LL621:
	mov	0, %g1
.LL620:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE294:
	.size	GE__uGE_param_i64_u64, .-GE__uGE_param_i64_u64
	.align 4
	.type	GE__GE_param_i64_u64, #function
	.proc	014
GE__GE_param_i64_u64:
.LLFB295:
	save	%sp, -112, %sp
.LLCFI295:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	mov	1, %g1
	ld	[%fp-16], %o4
	ld	[%fp-8], %o5
	cmp	%o4, %o5
	bg	%icc, .LL624
	 nop
	ld	[%fp-16], %o4
	ld	[%fp-8], %o5
	cmp	%o4, %o5
	bne	%icc, .LL623
	 nop
	ld	[%fp-12], %o4
	ld	[%fp-4], %o5
	cmp	%o4, %o5
	bgu	%icc, .LL624
	 nop
	ld	[%fp-12], %o4
	ld	[%fp-4], %o5
	cmp	%o4, %o5
	ba,pt	%xcc, .LL623
	 nop
.LL624:
	mov	0, %g1
.LL623:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE295:
	.size	GE__GE_param_i64_u64, .-GE__GE_param_i64_u64
	.align 4
	.type	GE__uGE_var_i64_LC, #function
	.proc	014
GE__uGE_var_i64_LC:
.LLFB296:
	save	%sp, -96, %sp
.LLCFI296:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+88], %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL627
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL626
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL627
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL626
	 nop
.LL627:
	mov	0, %g1
.LL626:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE296:
	.size	GE__uGE_var_i64_LC, .-GE__uGE_var_i64_LC
	.align 4
	.type	GE__GE_var_i64_LC, #function
	.proc	014
GE__GE_var_i64_LC:
.LLFB297:
	save	%sp, -96, %sp
.LLCFI297:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+88], %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL630
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL629
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL630
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL629
	 nop
.LL630:
	mov	0, %g1
.LL629:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE297:
	.size	GE__GE_var_i64_LC, .-GE__GE_var_i64_LC
	.align 4
	.type	GE__uGE_param_i64_LC, #function
	.proc	014
GE__uGE_param_i64_LC:
.LLFB298:
	save	%sp, -112, %sp
.LLCFI298:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	mov	1, %g1
	ld	[%fp-16], %o3
	cmp	%o3, %o4
	bgu	%icc, .LL633
	 nop
	ld	[%fp-16], %o3
	cmp	%o3, %o4
	bne	%icc, .LL632
	 nop
	ld	[%fp-12], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL633
	 nop
	ld	[%fp-12], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL632
	 nop
.LL633:
	mov	0, %g1
.LL632:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE298:
	.size	GE__uGE_param_i64_LC, .-GE__uGE_param_i64_LC
	.align 4
	.type	GE__GE_param_i64_LC, #function
	.proc	014
GE__GE_param_i64_LC:
.LLFB299:
	save	%sp, -112, %sp
.LLCFI299:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bl	%icc, .LL636
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL635
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL636
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL635
	 nop
.LL636:
	mov	0, %g1
.LL635:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE299:
	.size	GE__GE_param_i64_LC, .-GE__GE_param_i64_LC
	.align 4
	.type	GE__uGE_var_i64_i32, #function
	.proc	014
GE__uGE_var_i64_i32:
.LLFB300:
	save	%sp, -96, %sp
.LLCFI300:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL639
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL638
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL639
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL638
	 nop
.LL639:
	mov	0, %g1
.LL638:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE300:
	.size	GE__uGE_var_i64_i32, .-GE__uGE_var_i64_i32
	.align 4
	.type	GE__GE_var_i64_i32, #function
	.proc	014
GE__GE_var_i64_i32:
.LLFB301:
	save	%sp, -96, %sp
.LLCFI301:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL642
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL641
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL642
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL641
	 nop
.LL642:
	mov	0, %g1
.LL641:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE301:
	.size	GE__GE_var_i64_i32, .-GE__GE_var_i64_i32
	.align 4
	.type	GE__uGE_param_i64_i32, #function
	.proc	014
GE__uGE_param_i64_i32:
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
	bgu	%icc, .LL645
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL644
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL645
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL644
	 nop
.LL645:
	mov	0, %g1
.LL644:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE302:
	.size	GE__uGE_param_i64_i32, .-GE__uGE_param_i64_i32
	.align 4
	.type	GE__GE_param_i64_i32, #function
	.proc	014
GE__GE_param_i64_i32:
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
	bl	%icc, .LL648
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL647
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL648
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL647
	 nop
.LL648:
	mov	0, %g1
.LL647:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE303:
	.size	GE__GE_param_i64_i32, .-GE__GE_param_i64_i32
	.align 4
	.type	GE__uGE_var_i64_i16, #function
	.proc	014
GE__uGE_var_i64_i16:
.LLFB304:
	save	%sp, -96, %sp
.LLCFI304:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL651
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL650
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL651
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL650
	 nop
.LL651:
	mov	0, %g1
.LL650:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE304:
	.size	GE__uGE_var_i64_i16, .-GE__uGE_var_i64_i16
	.align 4
	.type	GE__GE_var_i64_i16, #function
	.proc	014
GE__GE_var_i64_i16:
.LLFB305:
	save	%sp, -96, %sp
.LLCFI305:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL654
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL653
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL654
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL653
	 nop
.LL654:
	mov	0, %g1
.LL653:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE305:
	.size	GE__GE_var_i64_i16, .-GE__GE_var_i64_i16
	.align 4
	.type	GE__uGE_param_i64_i16, #function
	.proc	014
GE__uGE_param_i64_i16:
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
	bgu	%icc, .LL657
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL656
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL657
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL656
	 nop
.LL657:
	mov	0, %g1
.LL656:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE306:
	.size	GE__uGE_param_i64_i16, .-GE__uGE_param_i64_i16
	.align 4
	.type	GE__GE_param_i64_i16, #function
	.proc	014
GE__GE_param_i64_i16:
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
	bl	%icc, .LL660
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL659
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL660
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL659
	 nop
.LL660:
	mov	0, %g1
.LL659:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE307:
	.size	GE__GE_param_i64_i16, .-GE__GE_param_i64_i16
	.align 4
	.type	GE__uGE_var_i64_I, #function
	.proc	014
GE__uGE_var_i64_I:
.LLFB308:
	save	%sp, -96, %sp
.LLCFI308:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL663
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL662
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL663
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL662
	 nop
.LL663:
	mov	0, %g1
.LL662:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE308:
	.size	GE__uGE_var_i64_I, .-GE__uGE_var_i64_I
	.align 4
	.type	GE__GE_var_i64_I, #function
	.proc	014
GE__GE_var_i64_I:
.LLFB309:
	save	%sp, -96, %sp
.LLCFI309:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL666
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL665
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL666
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL665
	 nop
.LL666:
	mov	0, %g1
.LL665:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE309:
	.size	GE__GE_var_i64_I, .-GE__GE_var_i64_I
	.align 4
	.type	GE__uGE_param_i64_I, #function
	.proc	014
GE__uGE_param_i64_I:
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
	bgu	%icc, .LL669
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL668
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL669
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL668
	 nop
.LL669:
	mov	0, %g1
.LL668:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE310:
	.size	GE__uGE_param_i64_I, .-GE__uGE_param_i64_I
	.align 4
	.type	GE__GE_param_i64_I, #function
	.proc	014
GE__GE_param_i64_I:
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
	bl	%icc, .LL672
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL671
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL672
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL671
	 nop
.LL672:
	mov	0, %g1
.LL671:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE311:
	.size	GE__GE_param_i64_I, .-GE__GE_param_i64_I
	.align 4
	.type	GE__uGE_var_i64_i64, #function
	.proc	014
GE__uGE_var_i64_i64:
.LLFB312:
	save	%sp, -96, %sp
.LLCFI312:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE312:
	.size	GE__uGE_var_i64_i64, .-GE__uGE_var_i64_i64
	.align 4
	.type	GE__GE_var_i64_i64, #function
	.proc	014
GE__GE_var_i64_i64:
.LLFB313:
	save	%sp, -96, %sp
.LLCFI313:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE313:
	.size	GE__GE_var_i64_i64, .-GE__GE_var_i64_i64
	.align 4
	.type	GE__uGE_param_i64_i64, #function
	.proc	014
GE__uGE_param_i64_i64:
.LLFB314:
	save	%sp, -112, %sp
.LLCFI314:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	ldd	[%fp-16], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL677
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL676
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL677
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL676
	 nop
.LL677:
	mov	0, %g1
.LL676:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE314:
	.size	GE__uGE_param_i64_i64, .-GE__uGE_param_i64_i64
	.align 4
	.type	GE__GE_param_i64_i64, #function
	.proc	014
GE__GE_param_i64_i64:
.LLFB315:
	save	%sp, -112, %sp
.LLCFI315:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	mov	1, %g1
	ld	[%fp-16], %o4
	ld	[%fp-8], %o5
	cmp	%o4, %o5
	bg	%icc, .LL680
	 nop
	ld	[%fp-16], %o4
	ld	[%fp-8], %o5
	cmp	%o4, %o5
	bne	%icc, .LL679
	 nop
	ld	[%fp-12], %o4
	ld	[%fp-4], %o5
	cmp	%o4, %o5
	bgu	%icc, .LL680
	 nop
	ld	[%fp-12], %o4
	ld	[%fp-4], %o5
	cmp	%o4, %o5
	ba,pt	%xcc, .LL679
	 nop
.LL680:
	mov	0, %g1
.LL679:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE315:
	.size	GE__GE_param_i64_i64, .-GE__GE_param_i64_i64
	.align 4
	.type	GE__uGE_var_i64_C, #function
	.proc	014
GE__uGE_var_i64_C:
.LLFB316:
	save	%sp, -96, %sp
.LLCFI316:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL683
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL682
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL683
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL682
	 nop
.LL683:
	mov	0, %g1
.LL682:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE316:
	.size	GE__uGE_var_i64_C, .-GE__uGE_var_i64_C
	.align 4
	.type	GE__GE_var_i64_C, #function
	.proc	014
GE__GE_var_i64_C:
.LLFB317:
	save	%sp, -96, %sp
.LLCFI317:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL686
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL685
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL686
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL685
	 nop
.LL686:
	mov	0, %g1
.LL685:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE317:
	.size	GE__GE_var_i64_C, .-GE__GE_var_i64_C
	.align 4
	.type	GE__uGE_param_i64_C, #function
	.proc	014
GE__uGE_param_i64_C:
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
	bgu	%icc, .LL689
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL688
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL689
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL688
	 nop
.LL689:
	mov	0, %g1
.LL688:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE318:
	.size	GE__uGE_param_i64_C, .-GE__uGE_param_i64_C
	.align 4
	.type	GE__GE_param_i64_C, #function
	.proc	014
GE__GE_param_i64_C:
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
	bl	%icc, .LL692
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL691
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL692
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL691
	 nop
.LL692:
	mov	0, %g1
.LL691:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE319:
	.size	GE__GE_param_i64_C, .-GE__GE_param_i64_C
	.align 4
	.type	GE__uGE_var_i64_u8, #function
	.proc	014
GE__uGE_var_i64_u8:
.LLFB320:
	save	%sp, -96, %sp
.LLCFI320:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+88], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL695
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL694
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL695
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL694
	 nop
.LL695:
	mov	0, %g1
.LL694:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE320:
	.size	GE__uGE_var_i64_u8, .-GE__uGE_var_i64_u8
	.align 4
	.type	GE__GE_var_i64_u8, #function
	.proc	014
GE__GE_var_i64_u8:
.LLFB321:
	save	%sp, -96, %sp
.LLCFI321:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+88], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL698
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL697
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL698
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL697
	 nop
.LL698:
	mov	0, %g1
.LL697:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE321:
	.size	GE__GE_var_i64_u8, .-GE__GE_var_i64_u8
	.align 4
	.type	GE__uGE_param_i64_u8, #function
	.proc	014
GE__uGE_param_i64_u8:
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
	bgu	%icc, .LL701
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL700
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL701
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL700
	 nop
.LL701:
	mov	0, %g1
.LL700:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE322:
	.size	GE__uGE_param_i64_u8, .-GE__uGE_param_i64_u8
	.align 4
	.type	GE__GE_param_i64_u8, #function
	.proc	014
GE__GE_param_i64_u8:
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
	bl	%icc, .LL704
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL703
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL704
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL703
	 nop
.LL704:
	mov	0, %g1
.LL703:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE323:
	.size	GE__GE_param_i64_u8, .-GE__GE_param_i64_u8
	.align 4
	.type	GE__uGE_var_i64_L, #function
	.proc	014
GE__uGE_var_i64_L:
.LLFB324:
	save	%sp, -96, %sp
.LLCFI324:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+88], %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL707
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL706
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL707
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL706
	 nop
.LL707:
	mov	0, %g1
.LL706:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE324:
	.size	GE__uGE_var_i64_L, .-GE__uGE_var_i64_L
	.align 4
	.type	GE__GE_var_i64_L, #function
	.proc	014
GE__GE_var_i64_L:
.LLFB325:
	save	%sp, -96, %sp
.LLCFI325:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+88], %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL710
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL709
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL710
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL709
	 nop
.LL710:
	mov	0, %g1
.LL709:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE325:
	.size	GE__GE_var_i64_L, .-GE__GE_var_i64_L
	.align 4
	.type	GE__uGE_param_i64_L, #function
	.proc	014
GE__uGE_param_i64_L:
.LLFB326:
	save	%sp, -112, %sp
.LLCFI326:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	ldd	[%fp-16], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL713
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL712
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL713
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL712
	 nop
.LL713:
	mov	0, %g1
.LL712:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE326:
	.size	GE__uGE_param_i64_L, .-GE__uGE_param_i64_L
	.align 4
	.type	GE__GE_param_i64_L, #function
	.proc	014
GE__GE_param_i64_L:
.LLFB327:
	save	%sp, -112, %sp
.LLCFI327:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	mov	1, %g1
	ld	[%fp-16], %o4
	ld	[%fp-8], %o5
	cmp	%o4, %o5
	bg	%icc, .LL716
	 nop
	ld	[%fp-16], %o4
	ld	[%fp-8], %o5
	cmp	%o4, %o5
	bne	%icc, .LL715
	 nop
	ld	[%fp-12], %o4
	ld	[%fp-4], %o5
	cmp	%o4, %o5
	bgu	%icc, .LL716
	 nop
	ld	[%fp-12], %o4
	ld	[%fp-4], %o5
	cmp	%o4, %o5
	ba,pt	%xcc, .LL715
	 nop
.LL716:
	mov	0, %g1
.LL715:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE327:
	.size	GE__GE_param_i64_L, .-GE__GE_param_i64_L
	.align 4
	.type	GE__uGE_var_i64_i8, #function
	.proc	014
GE__uGE_var_i64_i8:
.LLFB328:
	save	%sp, -96, %sp
.LLCFI328:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL719
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL718
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL719
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL718
	 nop
.LL719:
	mov	0, %g1
.LL718:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE328:
	.size	GE__uGE_var_i64_i8, .-GE__uGE_var_i64_i8
	.align 4
	.type	GE__GE_var_i64_i8, #function
	.proc	014
GE__GE_var_i64_i8:
.LLFB329:
	save	%sp, -96, %sp
.LLCFI329:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL722
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL721
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL722
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL721
	 nop
.LL722:
	mov	0, %g1
.LL721:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE329:
	.size	GE__GE_var_i64_i8, .-GE__GE_var_i64_i8
	.align 4
	.type	GE__uGE_param_i64_i8, #function
	.proc	014
GE__uGE_param_i64_i8:
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
	bgu	%icc, .LL725
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL724
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL725
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL724
	 nop
.LL725:
	mov	0, %g1
.LL724:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE330:
	.size	GE__uGE_param_i64_i8, .-GE__uGE_param_i64_i8
	.align 4
	.type	GE__GE_param_i64_i8, #function
	.proc	014
GE__GE_param_i64_i8:
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
	bl	%icc, .LL728
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL727
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL728
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL727
	 nop
.LL728:
	mov	0, %g1
.LL727:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE331:
	.size	GE__GE_param_i64_i8, .-GE__GE_param_i64_i8
	.align 4
	.type	GE__uGE_var_i64_u32, #function
	.proc	014
GE__uGE_var_i64_u32:
.LLFB332:
	save	%sp, -96, %sp
.LLCFI332:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL731
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL730
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL731
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL730
	 nop
.LL731:
	mov	0, %g1
.LL730:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE332:
	.size	GE__uGE_var_i64_u32, .-GE__uGE_var_i64_u32
	.align 4
	.type	GE__GE_var_i64_u32, #function
	.proc	014
GE__GE_var_i64_u32:
.LLFB333:
	save	%sp, -96, %sp
.LLCFI333:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL734
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL733
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL734
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL733
	 nop
.LL734:
	mov	0, %g1
.LL733:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE333:
	.size	GE__GE_var_i64_u32, .-GE__GE_var_i64_u32
	.align 4
	.type	GE__uGE_param_i64_u32, #function
	.proc	014
GE__uGE_param_i64_u32:
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
	bgu	%icc, .LL737
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL736
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL737
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL736
	 nop
.LL737:
	mov	0, %g1
.LL736:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE334:
	.size	GE__uGE_param_i64_u32, .-GE__uGE_param_i64_u32
	.align 4
	.type	GE__GE_param_i64_u32, #function
	.proc	014
GE__GE_param_i64_u32:
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
	bl	%icc, .LL740
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL739
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL740
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL739
	 nop
.LL740:
	mov	0, %g1
.LL739:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE335:
	.size	GE__GE_param_i64_u32, .-GE__GE_param_i64_u32
	.align 4
	.type	GE__uGE_var_C_u16, #function
	.proc	014
GE__uGE_var_C_u16:
.LLFB336:
	save	%sp, -96, %sp
.LLCFI336:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE336:
	.size	GE__uGE_var_C_u16, .-GE__uGE_var_C_u16
	.align 4
	.type	GE__GE_var_C_u16, #function
	.proc	014
GE__GE_var_C_u16:
.LLFB337:
	save	%sp, -96, %sp
.LLCFI337:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE337:
	.size	GE__GE_var_C_u16, .-GE__GE_var_C_u16
	.align 4
	.type	GE__uGE_param_C_u16, #function
	.proc	014
GE__uGE_param_C_u16:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE338:
	.size	GE__uGE_param_C_u16, .-GE__uGE_param_C_u16
	.align 4
	.type	GE__GE_param_C_u16, #function
	.proc	014
GE__GE_param_C_u16:
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
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE339:
	.size	GE__GE_param_C_u16, .-GE__GE_param_C_u16
	.align 4
	.type	GE__uGE_var_C_u64, #function
	.proc	014
GE__uGE_var_C_u64:
.LLFB340:
	save	%sp, -96, %sp
.LLCFI340:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL747
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL746
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL747
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL746
	 nop
.LL747:
	mov	0, %g1
.LL746:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE340:
	.size	GE__uGE_var_C_u64, .-GE__uGE_var_C_u64
	.align 4
	.type	GE__GE_var_C_u64, #function
	.proc	014
GE__GE_var_C_u64:
.LLFB341:
	save	%sp, -96, %sp
.LLCFI341:
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_GE), %g1
	or	%g1, %lo(MM_GE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL750
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL749
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL750
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL749
	 nop
.LL750:
	mov	0, %g1
.LL749:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE341:
	.size	GE__GE_var_C_u64, .-GE__GE_var_C_u64
	.align 4
	.type	GE__uGE_param_C_u64, #function
	.proc	014
GE__uGE_param_C_u64:
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
	bgu	%icc, .LL753
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL752
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL753
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL752
	 nop
.LL753:
	mov	0, %g1
.LL752:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE342:
	.size	GE__uGE_param_C_u64, .-GE__uGE_param_C_u64
	.align 4
	.type	GE__GE_param_C_u64, #function
	.proc	014
GE__GE_param_C_u64:
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
	bg	%icc, .LL756
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL755
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL756
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL755
	 nop
.LL756:
	mov	0, %g1
.LL755:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE343:
	.size	GE__GE_param_C_u64, .-GE__GE_param_C_u64
	.align 4
	.type	GE__uGE_var_C_LC,