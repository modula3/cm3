	.file	"LE.mc"
	.section	".text"
	.align 4
	.type	LE__uLE_var_u16_u16, #function
	.proc	014
LE__uLE_var_u16_u16:
.LLFB0:
	save	%sp, -96, %sp
.LLCFI0:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE0:
	.size	LE__uLE_var_u16_u16, .-LE__uLE_var_u16_u16
	.align 4
	.type	LE__LE_var_u16_u16, #function
	.proc	014
LE__LE_var_u16_u16:
.LLFB1:
	save	%sp, -96, %sp
.LLCFI1:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1:
	.size	LE__LE_var_u16_u16, .-LE__LE_var_u16_u16
	.align 4
	.type	LE__uLE_param_u16_u16, #function
	.proc	014
LE__uLE_param_u16_u16:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE2:
	.size	LE__uLE_param_u16_u16, .-LE__uLE_param_u16_u16
	.align 4
	.type	LE__LE_param_u16_u16, #function
	.proc	014
LE__LE_param_u16_u16:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE3:
	.size	LE__LE_param_u16_u16, .-LE__LE_param_u16_u16
	.align 4
	.type	LE__uLE_var_u16_u64, #function
	.proc	014
LE__uLE_var_u16_u64:
.LLFB4:
	save	%sp, -96, %sp
.LLCFI4:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o4
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
	.size	LE__uLE_var_u16_u64, .-LE__uLE_var_u16_u64
	.align 4
	.type	LE__LE_var_u16_u64, #function
	.proc	014
LE__LE_var_u16_u64:
.LLFB5:
	save	%sp, -96, %sp
.LLCFI5:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o4
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
	.size	LE__LE_var_u16_u64, .-LE__LE_var_u16_u64
	.align 4
	.type	LE__uLE_param_u16_u64, #function
	.proc	014
LE__uLE_param_u16_u64:
.LLFB6:
	save	%sp, -104, %sp
.LLCFI6:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
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
	.size	LE__uLE_param_u16_u64, .-LE__uLE_param_u16_u64
	.align 4
	.type	LE__LE_param_u16_u64, #function
	.proc	014
LE__LE_param_u16_u64:
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
	bl	%icc, .LL16
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL15
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL16
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
	.size	LE__LE_param_u16_u64, .-LE__LE_param_u16_u64
	.align 4
	.type	LE__uLE_var_u16_LC, #function
	.proc	014
LE__uLE_var_u16_LC:
.LLFB8:
	save	%sp, -96, %sp
.LLCFI8:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o4
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
	.size	LE__uLE_var_u16_LC, .-LE__uLE_var_u16_LC
	.align 4
	.type	LE__LE_var_u16_LC, #function
	.proc	014
LE__LE_var_u16_LC:
.LLFB9:
	save	%sp, -96, %sp
.LLCFI9:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o4
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
	.size	LE__LE_var_u16_LC, .-LE__LE_var_u16_LC
	.align 4
	.type	LE__uLE_param_u16_LC, #function
	.proc	014
LE__uLE_param_u16_LC:
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
	blu	%icc, .LL25
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL24
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL25
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
	.size	LE__uLE_param_u16_LC, .-LE__uLE_param_u16_LC
	.align 4
	.type	LE__LE_param_u16_LC, #function
	.proc	014
LE__LE_param_u16_LC:
.LLFB11:
	save	%sp, -104, %sp
.LLCFI11:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
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
	.size	LE__LE_param_u16_LC, .-LE__LE_param_u16_LC
	.align 4
	.type	LE__uLE_var_u16_i32, #function
	.proc	014
LE__uLE_var_u16_i32:
.LLFB12:
	save	%sp, -96, %sp
.LLCFI12:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE12:
	.size	LE__uLE_var_u16_i32, .-LE__uLE_var_u16_i32
	.align 4
	.type	LE__LE_var_u16_i32, #function
	.proc	014
LE__LE_var_u16_i32:
.LLFB13:
	save	%sp, -96, %sp
.LLCFI13:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE13:
	.size	LE__LE_var_u16_i32, .-LE__LE_var_u16_i32
	.align 4
	.type	LE__uLE_param_u16_i32, #function
	.proc	014
LE__uLE_param_u16_i32:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE14:
	.size	LE__uLE_param_u16_i32, .-LE__uLE_param_u16_i32
	.align 4
	.type	LE__LE_param_u16_i32, #function
	.proc	014
LE__LE_param_u16_i32:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE15:
	.size	LE__LE_param_u16_i32, .-LE__LE_param_u16_i32
	.align 4
	.type	LE__uLE_var_u16_i16, #function
	.proc	014
LE__uLE_var_u16_i16:
.LLFB16:
	save	%sp, -96, %sp
.LLCFI16:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE16:
	.size	LE__uLE_var_u16_i16, .-LE__uLE_var_u16_i16
	.align 4
	.type	LE__LE_var_u16_i16, #function
	.proc	014
LE__LE_var_u16_i16:
.LLFB17:
	save	%sp, -96, %sp
.LLCFI17:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE17:
	.size	LE__LE_var_u16_i16, .-LE__LE_var_u16_i16
	.align 4
	.type	LE__uLE_param_u16_i16, #function
	.proc	014
LE__uLE_param_u16_i16:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE18:
	.size	LE__uLE_param_u16_i16, .-LE__uLE_param_u16_i16
	.align 4
	.type	LE__LE_param_u16_i16, #function
	.proc	014
LE__LE_param_u16_i16:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE19:
	.size	LE__LE_param_u16_i16, .-LE__LE_param_u16_i16
	.align 4
	.type	LE__uLE_var_u16_I, #function
	.proc	014
LE__uLE_var_u16_I:
.LLFB20:
	save	%sp, -96, %sp
.LLCFI20:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE20:
	.size	LE__uLE_var_u16_I, .-LE__uLE_var_u16_I
	.align 4
	.type	LE__LE_var_u16_I, #function
	.proc	014
LE__LE_var_u16_I:
.LLFB21:
	save	%sp, -96, %sp
.LLCFI21:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE21:
	.size	LE__LE_var_u16_I, .-LE__LE_var_u16_I
	.align 4
	.type	LE__uLE_param_u16_I, #function
	.proc	014
LE__uLE_param_u16_I:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE22:
	.size	LE__uLE_param_u16_I, .-LE__uLE_param_u16_I
	.align 4
	.type	LE__LE_param_u16_I, #function
	.proc	014
LE__LE_param_u16_I:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE23:
	.size	LE__LE_param_u16_I, .-LE__LE_param_u16_I
	.align 4
	.type	LE__uLE_var_u16_i64, #function
	.proc	014
LE__uLE_var_u16_i64:
.LLFB24:
	save	%sp, -96, %sp
.LLCFI24:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o4
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
	.size	LE__uLE_var_u16_i64, .-LE__uLE_var_u16_i64
	.align 4
	.type	LE__LE_var_u16_i64, #function
	.proc	014
LE__LE_var_u16_i64:
.LLFB25:
	save	%sp, -96, %sp
.LLCFI25:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o4
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
	.size	LE__LE_var_u16_i64, .-LE__LE_var_u16_i64
	.align 4
	.type	LE__uLE_param_u16_i64, #function
	.proc	014
LE__uLE_param_u16_i64:
.LLFB26:
	save	%sp, -104, %sp
.LLCFI26:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
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
	.size	LE__uLE_param_u16_i64, .-LE__uLE_param_u16_i64
	.align 4
	.type	LE__LE_param_u16_i64, #function
	.proc	014
LE__LE_param_u16_i64:
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
	bl	%icc, .LL52
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL51
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL52
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
	.size	LE__LE_param_u16_i64, .-LE__LE_param_u16_i64
	.align 4
	.type	LE__uLE_var_u16_C, #function
	.proc	014
LE__uLE_var_u16_C:
.LLFB28:
	save	%sp, -96, %sp
.LLCFI28:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE28:
	.size	LE__uLE_var_u16_C, .-LE__uLE_var_u16_C
	.align 4
	.type	LE__LE_var_u16_C, #function
	.proc	014
LE__LE_var_u16_C:
.LLFB29:
	save	%sp, -96, %sp
.LLCFI29:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE29:
	.size	LE__LE_var_u16_C, .-LE__LE_var_u16_C
	.align 4
	.type	LE__uLE_param_u16_C, #function
	.proc	014
LE__uLE_param_u16_C:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE30:
	.size	LE__uLE_param_u16_C, .-LE__uLE_param_u16_C
	.align 4
	.type	LE__LE_param_u16_C, #function
	.proc	014
LE__LE_param_u16_C:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE31:
	.size	LE__LE_param_u16_C, .-LE__LE_param_u16_C
	.align 4
	.type	LE__uLE_var_u16_u8, #function
	.proc	014
LE__uLE_var_u16_u8:
.LLFB32:
	save	%sp, -96, %sp
.LLCFI32:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE32:
	.size	LE__uLE_var_u16_u8, .-LE__uLE_var_u16_u8
	.align 4
	.type	LE__LE_var_u16_u8, #function
	.proc	014
LE__LE_var_u16_u8:
.LLFB33:
	save	%sp, -96, %sp
.LLCFI33:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE33:
	.size	LE__LE_var_u16_u8, .-LE__LE_var_u16_u8
	.align 4
	.type	LE__uLE_param_u16_u8, #function
	.proc	014
LE__uLE_param_u16_u8:
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
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE34:
	.size	LE__uLE_param_u16_u8, .-LE__uLE_param_u16_u8
	.align 4
	.type	LE__LE_param_u16_u8, #function
	.proc	014
LE__LE_param_u16_u8:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE35:
	.size	LE__LE_param_u16_u8, .-LE__LE_param_u16_u8
	.align 4
	.type	LE__uLE_var_u16_L, #function
	.proc	014
LE__uLE_var_u16_L:
.LLFB36:
	save	%sp, -96, %sp
.LLCFI36:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o4
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
	.size	LE__uLE_var_u16_L, .-LE__uLE_var_u16_L
	.align 4
	.type	LE__LE_var_u16_L, #function
	.proc	014
LE__LE_var_u16_L:
.LLFB37:
	save	%sp, -96, %sp
.LLCFI37:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o4
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
	.size	LE__LE_var_u16_L, .-LE__LE_var_u16_L
	.align 4
	.type	LE__uLE_param_u16_L, #function
	.proc	014
LE__uLE_param_u16_L:
.LLFB38:
	save	%sp, -104, %sp
.LLCFI38:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
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
	.size	LE__uLE_param_u16_L, .-LE__uLE_param_u16_L
	.align 4
	.type	LE__LE_param_u16_L, #function
	.proc	014
LE__LE_param_u16_L:
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
	bl	%icc, .LL72
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL71
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL72
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
	.size	LE__LE_param_u16_L, .-LE__LE_param_u16_L
	.align 4
	.type	LE__uLE_var_u16_i8, #function
	.proc	014
LE__uLE_var_u16_i8:
.LLFB40:
	save	%sp, -96, %sp
.LLCFI40:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE40:
	.size	LE__uLE_var_u16_i8, .-LE__uLE_var_u16_i8
	.align 4
	.type	LE__LE_var_u16_i8, #function
	.proc	014
LE__LE_var_u16_i8:
.LLFB41:
	save	%sp, -96, %sp
.LLCFI41:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE41:
	.size	LE__LE_var_u16_i8, .-LE__LE_var_u16_i8
	.align 4
	.type	LE__uLE_param_u16_i8, #function
	.proc	014
LE__uLE_param_u16_i8:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE42:
	.size	LE__uLE_param_u16_i8, .-LE__uLE_param_u16_i8
	.align 4
	.type	LE__LE_param_u16_i8, #function
	.proc	014
LE__LE_param_u16_i8:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE43:
	.size	LE__LE_param_u16_i8, .-LE__LE_param_u16_i8
	.align 4
	.type	LE__uLE_var_u16_u32, #function
	.proc	014
LE__uLE_var_u16_u32:
.LLFB44:
	save	%sp, -96, %sp
.LLCFI44:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE44:
	.size	LE__uLE_var_u16_u32, .-LE__uLE_var_u16_u32
	.align 4
	.type	LE__LE_var_u16_u32, #function
	.proc	014
LE__LE_var_u16_u32:
.LLFB45:
	save	%sp, -96, %sp
.LLCFI45:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE45:
	.size	LE__LE_var_u16_u32, .-LE__LE_var_u16_u32
	.align 4
	.type	LE__uLE_param_u16_u32, #function
	.proc	014
LE__uLE_param_u16_u32:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE46:
	.size	LE__uLE_param_u16_u32, .-LE__uLE_param_u16_u32
	.align 4
	.type	LE__LE_param_u16_u32, #function
	.proc	014
LE__LE_param_u16_u32:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE47:
	.size	LE__LE_param_u16_u32, .-LE__LE_param_u16_u32
	.align 4
	.type	LE__uLE_var_u64_u16, #function
	.proc	014
LE__uLE_var_u64_u16:
.LLFB48:
	save	%sp, -96, %sp
.LLCFI48:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o2
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
	.size	LE__uLE_var_u64_u16, .-LE__uLE_var_u64_u16
	.align 4
	.type	LE__LE_var_u64_u16, #function
	.proc	014
LE__LE_var_u64_u16:
.LLFB49:
	save	%sp, -96, %sp
.LLCFI49:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o2
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
	.size	LE__LE_var_u64_u16, .-LE__LE_var_u64_u16
	.align 4
	.type	LE__uLE_param_u64_u16, #function
	.proc	014
LE__uLE_param_u64_u16:
.LLFB50:
	save	%sp, -104, %sp
.LLCFI50:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	ldd	[%fp-8], %o2
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
	.size	LE__uLE_param_u64_u16, .-LE__uLE_param_u64_u16
	.align 4
	.type	LE__LE_param_u64_u16, #function
	.proc	014
LE__LE_param_u64_u16:
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
	bg	%icc, .LL92
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL91
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL92
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
	.size	LE__LE_param_u64_u16, .-LE__LE_param_u64_u16
	.align 4
	.type	LE__uLE_var_u64_u64, #function
	.proc	014
LE__uLE_var_u64_u64:
.LLFB52:
	save	%sp, -96, %sp
.LLCFI52:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE52:
	.size	LE__uLE_var_u64_u64, .-LE__uLE_var_u64_u64
	.align 4
	.type	LE__LE_var_u64_u64, #function
	.proc	014
LE__LE_var_u64_u64:
.LLFB53:
	save	%sp, -96, %sp
.LLCFI53:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE53:
	.size	LE__LE_var_u64_u64, .-LE__LE_var_u64_u64
	.align 4
	.type	LE__uLE_param_u64_u64, #function
	.proc	014
LE__uLE_param_u64_u64:
.LLFB54:
	save	%sp, -112, %sp
.LLCFI54:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
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
	.size	LE__uLE_param_u64_u64, .-LE__uLE_param_u64_u64
	.align 4
	.type	LE__LE_param_u64_u64, #function
	.proc	014
LE__LE_param_u64_u64:
.LLFB55:
	save	%sp, -112, %sp
.LLCFI55:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	mov	1, %g1
	ld	[%fp-8], %o4
	ld	[%fp-16], %o5
	cmp	%o4, %o5
	bg	%icc, .LL100
	 nop
	ld	[%fp-8], %o4
	ld	[%fp-16], %o5
	cmp	%o4, %o5
	bne	%icc, .LL99
	 nop
	ld	[%fp-4], %o4
	ld	[%fp-12], %o5
	cmp	%o4, %o5
	bgu	%icc, .LL100
	 nop
	ld	[%fp-4], %o4
	ld	[%fp-12], %o5
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
	.size	LE__LE_param_u64_u64, .-LE__LE_param_u64_u64
	.align 4
	.type	LE__uLE_var_u64_LC, #function
	.proc	014
LE__uLE_var_u64_LC:
.LLFB56:
	save	%sp, -96, %sp
.LLCFI56:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o4
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
	.size	LE__uLE_var_u64_LC, .-LE__uLE_var_u64_LC
	.align 4
	.type	LE__LE_var_u64_LC, #function
	.proc	014
LE__LE_var_u64_LC:
.LLFB57:
	save	%sp, -96, %sp
.LLCFI57:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o2
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o4
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
	.size	LE__LE_var_u64_LC, .-LE__LE_var_u64_LC
	.align 4
	.type	LE__uLE_param_u64_LC, #function
	.proc	014
LE__uLE_param_u64_LC:
.LLFB58:
	save	%sp, -112, %sp
.LLCFI58:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	mov	1, %g1
	ld	[%fp-16], %o3
	cmp	%o3, %o4
	blu	%icc, .LL109
	 nop
	ld	[%fp-16], %o3
	cmp	%o3, %o4
	bne	%icc, .LL108
	 nop
	ld	[%fp-12], %o3
	cmp	%o3, %o5
	blu	%icc, .LL109
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
	.size	LE__uLE_param_u64_LC, .-LE__uLE_param_u64_LC
	.align 4
	.type	LE__LE_param_u64_LC, #function
	.proc	014
LE__LE_param_u64_LC:
.LLFB59:
	save	%sp, -112, %sp
.LLCFI59:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bg	%icc, .LL112
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL111
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL112
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
	.size	LE__LE_param_u64_LC, .-LE__LE_param_u64_LC
	.align 4
	.type	LE__uLE_var_u64_i32, #function
	.proc	014
LE__uLE_var_u64_i32:
.LLFB60:
	save	%sp, -96, %sp
.LLCFI60:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL115
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL114
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL115
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL114
	 nop
.LL115:
	mov	0, %g1
.LL114:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE60:
	.size	LE__uLE_var_u64_i32, .-LE__uLE_var_u64_i32
	.align 4
	.type	LE__LE_var_u64_i32, #function
	.proc	014
LE__LE_var_u64_i32:
.LLFB61:
	save	%sp, -96, %sp
.LLCFI61:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL118
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL117
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL118
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL117
	 nop
.LL118:
	mov	0, %g1
.LL117:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE61:
	.size	LE__LE_var_u64_i32, .-LE__LE_var_u64_i32
	.align 4
	.type	LE__uLE_param_u64_i32, #function
	.proc	014
LE__uLE_param_u64_i32:
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
	cmp	%o2, %o4
	bgu	%icc, .LL121
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL120
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL121
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL120
	 nop
.LL121:
	mov	0, %g1
.LL120:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE62:
	.size	LE__uLE_param_u64_i32, .-LE__uLE_param_u64_i32
	.align 4
	.type	LE__LE_param_u64_i32, #function
	.proc	014
LE__LE_param_u64_i32:
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
	bg	%icc, .LL124
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL123
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL124
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
	.size	LE__LE_param_u64_i32, .-LE__LE_param_u64_i32
	.align 4
	.type	LE__uLE_var_u64_i16, #function
	.proc	014
LE__uLE_var_u64_i16:
.LLFB64:
	save	%sp, -96, %sp
.LLCFI64:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL127
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL126
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL127
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL126
	 nop
.LL127:
	mov	0, %g1
.LL126:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE64:
	.size	LE__uLE_var_u64_i16, .-LE__uLE_var_u64_i16
	.align 4
	.type	LE__LE_var_u64_i16, #function
	.proc	014
LE__LE_var_u64_i16:
.LLFB65:
	save	%sp, -96, %sp
.LLCFI65:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL130
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL129
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL130
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL129
	 nop
.LL130:
	mov	0, %g1
.LL129:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE65:
	.size	LE__LE_var_u64_i16, .-LE__LE_var_u64_i16
	.align 4
	.type	LE__uLE_param_u64_i16, #function
	.proc	014
LE__uLE_param_u64_i16:
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
	cmp	%o2, %o4
	bgu	%icc, .LL133
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL132
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL133
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL132
	 nop
.LL133:
	mov	0, %g1
.LL132:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE66:
	.size	LE__uLE_param_u64_i16, .-LE__uLE_param_u64_i16
	.align 4
	.type	LE__LE_param_u64_i16, #function
	.proc	014
LE__LE_param_u64_i16:
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
	bg	%icc, .LL136
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL135
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL136
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
	.size	LE__LE_param_u64_i16, .-LE__LE_param_u64_i16
	.align 4
	.type	LE__uLE_var_u64_I, #function
	.proc	014
LE__uLE_var_u64_I:
.LLFB68:
	save	%sp, -96, %sp
.LLCFI68:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL139
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL138
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL139
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL138
	 nop
.LL139:
	mov	0, %g1
.LL138:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE68:
	.size	LE__uLE_var_u64_I, .-LE__uLE_var_u64_I
	.align 4
	.type	LE__LE_var_u64_I, #function
	.proc	014
LE__LE_var_u64_I:
.LLFB69:
	save	%sp, -96, %sp
.LLCFI69:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL142
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL141
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL142
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL141
	 nop
.LL142:
	mov	0, %g1
.LL141:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE69:
	.size	LE__LE_var_u64_I, .-LE__LE_var_u64_I
	.align 4
	.type	LE__uLE_param_u64_I, #function
	.proc	014
LE__uLE_param_u64_I:
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
	cmp	%o2, %o4
	bgu	%icc, .LL145
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL144
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL145
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL144
	 nop
.LL145:
	mov	0, %g1
.LL144:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE70:
	.size	LE__uLE_param_u64_I, .-LE__uLE_param_u64_I
	.align 4
	.type	LE__LE_param_u64_I, #function
	.proc	014
LE__LE_param_u64_I:
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
	bg	%icc, .LL148
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL147
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL148
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
	.size	LE__LE_param_u64_I, .-LE__LE_param_u64_I
	.align 4
	.type	LE__uLE_var_u64_i64, #function
	.proc	014
LE__uLE_var_u64_i64:
.LLFB72:
	save	%sp, -96, %sp
.LLCFI72:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o4
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
	.size	LE__uLE_var_u64_i64, .-LE__uLE_var_u64_i64
	.align 4
	.type	LE__LE_var_u64_i64, #function
	.proc	014
LE__LE_var_u64_i64:
.LLFB73:
	save	%sp, -96, %sp
.LLCFI73:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o2
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o4
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
	.size	LE__LE_var_u64_i64, .-LE__LE_var_u64_i64
	.align 4
	.type	LE__uLE_param_u64_i64, #function
	.proc	014
LE__uLE_param_u64_i64:
.LLFB74:
	save	%sp, -112, %sp
.LLCFI74:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
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
	.size	LE__uLE_param_u64_i64, .-LE__uLE_param_u64_i64
	.align 4
	.type	LE__LE_param_u64_i64, #function
	.proc	014
LE__LE_param_u64_i64:
.LLFB75:
	save	%sp, -112, %sp
.LLCFI75:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	mov	1, %g1
	ld	[%fp-8], %o4
	ld	[%fp-16], %o5
	cmp	%o4, %o5
	bg	%icc, .LL160
	 nop
	ld	[%fp-8], %o4
	ld	[%fp-16], %o5
	cmp	%o4, %o5
	bne	%icc, .LL159
	 nop
	ld	[%fp-4], %o4
	ld	[%fp-12], %o5
	cmp	%o4, %o5
	bgu	%icc, .LL160
	 nop
	ld	[%fp-4], %o4
	ld	[%fp-12], %o5
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
	.size	LE__LE_param_u64_i64, .-LE__LE_param_u64_i64
	.align 4
	.type	LE__uLE_var_u64_C, #function
	.proc	014
LE__uLE_var_u64_C:
.LLFB76:
	save	%sp, -96, %sp
.LLCFI76:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL163
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL162
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL163
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL162
	 nop
.LL163:
	mov	0, %g1
.LL162:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE76:
	.size	LE__uLE_var_u64_C, .-LE__uLE_var_u64_C
	.align 4
	.type	LE__LE_var_u64_C, #function
	.proc	014
LE__LE_var_u64_C:
.LLFB77:
	save	%sp, -96, %sp
.LLCFI77:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL166
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL165
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL166
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL165
	 nop
.LL166:
	mov	0, %g1
.LL165:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE77:
	.size	LE__LE_var_u64_C, .-LE__LE_var_u64_C
	.align 4
	.type	LE__uLE_param_u64_C, #function
	.proc	014
LE__uLE_param_u64_C:
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
	cmp	%o2, %o4
	bgu	%icc, .LL169
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL168
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL169
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL168
	 nop
.LL169:
	mov	0, %g1
.LL168:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE78:
	.size	LE__uLE_param_u64_C, .-LE__uLE_param_u64_C
	.align 4
	.type	LE__LE_param_u64_C, #function
	.proc	014
LE__LE_param_u64_C:
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
	bg	%icc, .LL172
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL171
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL172
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
	.size	LE__LE_param_u64_C, .-LE__LE_param_u64_C
	.align 4
	.type	LE__uLE_var_u64_u8, #function
	.proc	014
LE__uLE_var_u64_u8:
.LLFB80:
	save	%sp, -96, %sp
.LLCFI80:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o2
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
	.size	LE__uLE_var_u64_u8, .-LE__uLE_var_u64_u8
	.align 4
	.type	LE__LE_var_u64_u8, #function
	.proc	014
LE__LE_var_u64_u8:
.LLFB81:
	save	%sp, -96, %sp
.LLCFI81:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o2
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
	.size	LE__LE_var_u64_u8, .-LE__LE_var_u64_u8
	.align 4
	.type	LE__uLE_param_u64_u8, #function
	.proc	014
LE__uLE_param_u64_u8:
.LLFB82:
	save	%sp, -104, %sp
.LLCFI82:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	ldd	[%fp-8], %o2
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
	.size	LE__uLE_param_u64_u8, .-LE__uLE_param_u64_u8
	.align 4
	.type	LE__LE_param_u64_u8, #function
	.proc	014
LE__LE_param_u64_u8:
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
	bg	%icc, .LL184
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL183
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL184
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
	.size	LE__LE_param_u64_u8, .-LE__LE_param_u64_u8
	.align 4
	.type	LE__uLE_var_u64_L, #function
	.proc	014
LE__uLE_var_u64_L:
.LLFB84:
	save	%sp, -96, %sp
.LLCFI84:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o4
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
	.size	LE__uLE_var_u64_L, .-LE__uLE_var_u64_L
	.align 4
	.type	LE__LE_var_u64_L, #function
	.proc	014
LE__LE_var_u64_L:
.LLFB85:
	save	%sp, -96, %sp
.LLCFI85:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o2
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o4
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
	.size	LE__LE_var_u64_L, .-LE__LE_var_u64_L
	.align 4
	.type	LE__uLE_param_u64_L, #function
	.proc	014
LE__uLE_param_u64_L:
.LLFB86:
	save	%sp, -112, %sp
.LLCFI86:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
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
	.size	LE__uLE_param_u64_L, .-LE__uLE_param_u64_L
	.align 4
	.type	LE__LE_param_u64_L, #function
	.proc	014
LE__LE_param_u64_L:
.LLFB87:
	save	%sp, -112, %sp
.LLCFI87:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	mov	1, %g1
	ld	[%fp-8], %o4
	ld	[%fp-16], %o5
	cmp	%o4, %o5
	bg	%icc, .LL196
	 nop
	ld	[%fp-8], %o4
	ld	[%fp-16], %o5
	cmp	%o4, %o5
	bne	%icc, .LL195
	 nop
	ld	[%fp-4], %o4
	ld	[%fp-12], %o5
	cmp	%o4, %o5
	bgu	%icc, .LL196
	 nop
	ld	[%fp-4], %o4
	ld	[%fp-12], %o5
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
	.size	LE__LE_param_u64_L, .-LE__LE_param_u64_L
	.align 4
	.type	LE__uLE_var_u64_i8, #function
	.proc	014
LE__uLE_var_u64_i8:
.LLFB88:
	save	%sp, -96, %sp
.LLCFI88:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL199
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL198
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL199
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL198
	 nop
.LL199:
	mov	0, %g1
.LL198:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE88:
	.size	LE__uLE_var_u64_i8, .-LE__uLE_var_u64_i8
	.align 4
	.type	LE__LE_var_u64_i8, #function
	.proc	014
LE__LE_var_u64_i8:
.LLFB89:
	save	%sp, -96, %sp
.LLCFI89:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL202
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL201
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL202
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL201
	 nop
.LL202:
	mov	0, %g1
.LL201:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE89:
	.size	LE__LE_var_u64_i8, .-LE__LE_var_u64_i8
	.align 4
	.type	LE__uLE_param_u64_i8, #function
	.proc	014
LE__uLE_param_u64_i8:
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
	cmp	%o2, %o4
	bgu	%icc, .LL205
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL204
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL205
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL204
	 nop
.LL205:
	mov	0, %g1
.LL204:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE90:
	.size	LE__uLE_param_u64_i8, .-LE__uLE_param_u64_i8
	.align 4
	.type	LE__LE_param_u64_i8, #function
	.proc	014
LE__LE_param_u64_i8:
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
	bg	%icc, .LL208
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL207
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL208
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
	.size	LE__LE_param_u64_i8, .-LE__LE_param_u64_i8
	.align 4
	.type	LE__uLE_var_u64_u32, #function
	.proc	014
LE__uLE_var_u64_u32:
.LLFB92:
	save	%sp, -96, %sp
.LLCFI92:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL211
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL210
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL211
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL210
	 nop
.LL211:
	mov	0, %g1
.LL210:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE92:
	.size	LE__uLE_var_u64_u32, .-LE__uLE_var_u64_u32
	.align 4
	.type	LE__LE_var_u64_u32, #function
	.proc	014
LE__LE_var_u64_u32:
.LLFB93:
	save	%sp, -96, %sp
.LLCFI93:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL214
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL213
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL214
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL213
	 nop
.LL214:
	mov	0, %g1
.LL213:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE93:
	.size	LE__LE_var_u64_u32, .-LE__LE_var_u64_u32
	.align 4
	.type	LE__uLE_param_u64_u32, #function
	.proc	014
LE__uLE_param_u64_u32:
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
	cmp	%o2, %o4
	bgu	%icc, .LL217
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL216
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL217
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL216
	 nop
.LL217:
	mov	0, %g1
.LL216:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE94:
	.size	LE__uLE_param_u64_u32, .-LE__uLE_param_u64_u32
	.align 4
	.type	LE__LE_param_u64_u32, #function
	.proc	014
LE__LE_param_u64_u32:
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
	bg	%icc, .LL220
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL219
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL220
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
	.size	LE__LE_param_u64_u32, .-LE__LE_param_u64_u32
	.align 4
	.type	LE__uLE_var_LC_u16, #function
	.proc	014
LE__uLE_var_LC_u16:
.LLFB96:
	save	%sp, -96, %sp
.LLCFI96:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o2
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
	.size	LE__uLE_var_LC_u16, .-LE__uLE_var_LC_u16
	.align 4
	.type	LE__LE_var_LC_u16, #function
	.proc	014
LE__LE_var_LC_u16:
.LLFB97:
	save	%sp, -96, %sp
.LLCFI97:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o2
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
	.size	LE__LE_var_LC_u16, .-LE__LE_var_LC_u16
	.align 4
	.type	LE__uLE_param_LC_u16, #function
	.proc	014
LE__uLE_param_LC_u16:
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
	bgu	%icc, .LL229
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL228
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL229
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
	.size	LE__uLE_param_LC_u16, .-LE__uLE_param_LC_u16
	.align 4
	.type	LE__LE_param_LC_u16, #function
	.proc	014
LE__LE_param_LC_u16:
.LLFB99:
	save	%sp, -104, %sp
.LLCFI99:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	ldd	[%fp-8], %o2
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
	.size	LE__LE_param_LC_u16, .-LE__LE_param_LC_u16
	.align 4
	.type	LE__uLE_var_LC_u64, #function
	.proc	014
LE__uLE_var_LC_u64:
.LLFB100:
	save	%sp, -96, %sp
.LLCFI100:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o2
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o4
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
	.size	LE__uLE_var_LC_u64, .-LE__uLE_var_LC_u64
	.align 4
	.type	LE__LE_var_LC_u64, #function
	.proc	014
LE__LE_var_LC_u64:
.LLFB101:
	save	%sp, -96, %sp
.LLCFI101:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o4
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
	.size	LE__LE_var_LC_u64, .-LE__LE_var_LC_u64
	.align 4
	.type	LE__uLE_param_LC_u64, #function
	.proc	014
LE__uLE_param_LC_u64:
.LLFB102:
	save	%sp, -112, %sp
.LLCFI102:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bgu	%icc, .LL241
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL240
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL241
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
	.size	LE__uLE_param_LC_u64, .-LE__uLE_param_LC_u64
	.align 4
	.type	LE__LE_param_LC_u64, #function
	.proc	014
LE__LE_param_LC_u64:
.LLFB103:
	save	%sp, -112, %sp
.LLCFI103:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	mov	1, %g1
	ld	[%fp-16], %o3
	cmp	%o3, %o4
	bl	%icc, .LL244
	 nop
	ld	[%fp-16], %o3
	cmp	%o3, %o4
	bne	%icc, .LL243
	 nop
	ld	[%fp-12], %o3
	cmp	%o3, %o5
	blu	%icc, .LL244
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
	.size	LE__LE_param_LC_u64, .-LE__LE_param_LC_u64
	.align 4
	.type	LE__uLE_var_LC_LC, #function
	.proc	014
LE__uLE_var_LC_LC:
.LLFB104:
	save	%sp, -96, %sp
.LLCFI104:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE104:
	.size	LE__uLE_var_LC_LC, .-LE__uLE_var_LC_LC
	.align 4
	.type	LE__LE_var_LC_LC, #function
	.proc	014
LE__LE_var_LC_LC:
.LLFB105:
	save	%sp, -96, %sp
.LLCFI105:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE105:
	.size	LE__LE_var_LC_LC, .-LE__LE_var_LC_LC
	.align 4
	.type	LE__uLE_param_LC_LC, #function
	.proc	014
LE__uLE_param_LC_LC:
.LLFB106:
	save	%sp, -112, %sp
.LLCFI106:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	mov	1, %g1
	ld	[%fp-8], %o4
	ld	[%fp-16], %o5
	cmp	%o4, %o5
	bgu	%icc, .LL249
	 nop
	ld	[%fp-8], %o4
	ld	[%fp-16], %o5
	cmp	%o4, %o5
	bne	%icc, .LL248
	 nop
	ld	[%fp-4], %o4
	ld	[%fp-12], %o5
	cmp	%o4, %o5
	bgu	%icc, .LL249
	 nop
	ld	[%fp-4], %o4
	ld	[%fp-12], %o5
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
	.size	LE__uLE_param_LC_LC, .-LE__uLE_param_LC_LC
	.align 4
	.type	LE__LE_param_LC_LC, #function
	.proc	014
LE__LE_param_LC_LC:
.LLFB107:
	save	%sp, -112, %sp
.LLCFI107:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
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
	.size	LE__LE_param_LC_LC, .-LE__LE_param_LC_LC
	.align 4
	.type	LE__uLE_var_LC_i32, #function
	.proc	014
LE__uLE_var_LC_i32:
.LLFB108:
	save	%sp, -96, %sp
.LLCFI108:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL255
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL254
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL255
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL254
	 nop
.LL255:
	mov	0, %g1
.LL254:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE108:
	.size	LE__uLE_var_LC_i32, .-LE__uLE_var_LC_i32
	.align 4
	.type	LE__LE_var_LC_i32, #function
	.proc	014
LE__LE_var_LC_i32:
.LLFB109:
	save	%sp, -96, %sp
.LLCFI109:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL258
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL257
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL258
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL257
	 nop
.LL258:
	mov	0, %g1
.LL257:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE109:
	.size	LE__LE_var_LC_i32, .-LE__LE_var_LC_i32
	.align 4
	.type	LE__uLE_param_LC_i32, #function
	.proc	014
LE__uLE_param_LC_i32:
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
	bgu	%icc, .LL261
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL260
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL261
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
	.size	LE__uLE_param_LC_i32, .-LE__uLE_param_LC_i32
	.align 4
	.type	LE__LE_param_LC_i32, #function
	.proc	014
LE__LE_param_LC_i32:
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
	cmp	%o2, %o4
	bg	%icc, .LL264
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL263
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL264
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL263
	 nop
.LL264:
	mov	0, %g1
.LL263:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE111:
	.size	LE__LE_param_LC_i32, .-LE__LE_param_LC_i32
	.align 4
	.type	LE__uLE_var_LC_i16, #function
	.proc	014
LE__uLE_var_LC_i16:
.LLFB112:
	save	%sp, -96, %sp
.LLCFI112:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL267
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL266
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL267
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL266
	 nop
.LL267:
	mov	0, %g1
.LL266:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE112:
	.size	LE__uLE_var_LC_i16, .-LE__uLE_var_LC_i16
	.align 4
	.type	LE__LE_var_LC_i16, #function
	.proc	014
LE__LE_var_LC_i16:
.LLFB113:
	save	%sp, -96, %sp
.LLCFI113:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL270
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL269
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL270
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL269
	 nop
.LL270:
	mov	0, %g1
.LL269:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE113:
	.size	LE__LE_var_LC_i16, .-LE__LE_var_LC_i16
	.align 4
	.type	LE__uLE_param_LC_i16, #function
	.proc	014
LE__uLE_param_LC_i16:
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
	bgu	%icc, .LL273
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL272
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL273
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
	.size	LE__uLE_param_LC_i16, .-LE__uLE_param_LC_i16
	.align 4
	.type	LE__LE_param_LC_i16, #function
	.proc	014
LE__LE_param_LC_i16:
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
	cmp	%o2, %o4
	bg	%icc, .LL276
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL275
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL276
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL275
	 nop
.LL276:
	mov	0, %g1
.LL275:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE115:
	.size	LE__LE_param_LC_i16, .-LE__LE_param_LC_i16
	.align 4
	.type	LE__uLE_var_LC_I, #function
	.proc	014
LE__uLE_var_LC_I:
.LLFB116:
	save	%sp, -96, %sp
.LLCFI116:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL279
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL278
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL279
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL278
	 nop
.LL279:
	mov	0, %g1
.LL278:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE116:
	.size	LE__uLE_var_LC_I, .-LE__uLE_var_LC_I
	.align 4
	.type	LE__LE_var_LC_I, #function
	.proc	014
LE__LE_var_LC_I:
.LLFB117:
	save	%sp, -96, %sp
.LLCFI117:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL282
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL281
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL282
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL281
	 nop
.LL282:
	mov	0, %g1
.LL281:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE117:
	.size	LE__LE_var_LC_I, .-LE__LE_var_LC_I
	.align 4
	.type	LE__uLE_param_LC_I, #function
	.proc	014
LE__uLE_param_LC_I:
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
	bgu	%icc, .LL285
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL284
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL285
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
	.size	LE__uLE_param_LC_I, .-LE__uLE_param_LC_I
	.align 4
	.type	LE__LE_param_LC_I, #function
	.proc	014
LE__LE_param_LC_I:
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
	cmp	%o2, %o4
	bg	%icc, .LL288
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL287
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL288
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL287
	 nop
.LL288:
	mov	0, %g1
.LL287:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE119:
	.size	LE__LE_param_LC_I, .-LE__LE_param_LC_I
	.align 4
	.type	LE__uLE_var_LC_i64, #function
	.proc	014
LE__uLE_var_LC_i64:
.LLFB120:
	save	%sp, -96, %sp
.LLCFI120:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o2
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o4
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
	.size	LE__uLE_var_LC_i64, .-LE__uLE_var_LC_i64
	.align 4
	.type	LE__LE_var_LC_i64, #function
	.proc	014
LE__LE_var_LC_i64:
.LLFB121:
	save	%sp, -96, %sp
.LLCFI121:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o4
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
	.size	LE__LE_var_LC_i64, .-LE__LE_var_LC_i64
	.align 4
	.type	LE__uLE_param_LC_i64, #function
	.proc	014
LE__uLE_param_LC_i64:
.LLFB122:
	save	%sp, -112, %sp
.LLCFI122:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bgu	%icc, .LL297
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL296
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL297
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
	.size	LE__uLE_param_LC_i64, .-LE__uLE_param_LC_i64
	.align 4
	.type	LE__LE_param_LC_i64, #function
	.proc	014
LE__LE_param_LC_i64:
.LLFB123:
	save	%sp, -112, %sp
.LLCFI123:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	mov	1, %g1
	ld	[%fp-16], %o3
	cmp	%o3, %o4
	bl	%icc, .LL300
	 nop
	ld	[%fp-16], %o3
	cmp	%o3, %o4
	bne	%icc, .LL299
	 nop
	ld	[%fp-12], %o3
	cmp	%o3, %o5
	blu	%icc, .LL300
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
	.size	LE__LE_param_LC_i64, .-LE__LE_param_LC_i64
	.align 4
	.type	LE__uLE_var_LC_C, #function
	.proc	014
LE__uLE_var_LC_C:
.LLFB124:
	save	%sp, -96, %sp
.LLCFI124:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL303
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL302
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL303
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL302
	 nop
.LL303:
	mov	0, %g1
.LL302:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE124:
	.size	LE__uLE_var_LC_C, .-LE__uLE_var_LC_C
	.align 4
	.type	LE__LE_var_LC_C, #function
	.proc	014
LE__LE_var_LC_C:
.LLFB125:
	save	%sp, -96, %sp
.LLCFI125:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL306
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL305
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL306
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL305
	 nop
.LL306:
	mov	0, %g1
.LL305:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE125:
	.size	LE__LE_var_LC_C, .-LE__LE_var_LC_C
	.align 4
	.type	LE__uLE_param_LC_C, #function
	.proc	014
LE__uLE_param_LC_C:
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
	bgu	%icc, .LL309
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL308
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL309
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
	.size	LE__uLE_param_LC_C, .-LE__uLE_param_LC_C
	.align 4
	.type	LE__LE_param_LC_C, #function
	.proc	014
LE__LE_param_LC_C:
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
	cmp	%o2, %o4
	bg	%icc, .LL312
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL311
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL312
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL311
	 nop
.LL312:
	mov	0, %g1
.LL311:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE127:
	.size	LE__LE_param_LC_C, .-LE__LE_param_LC_C
	.align 4
	.type	LE__uLE_var_LC_u8, #function
	.proc	014
LE__uLE_var_LC_u8:
.LLFB128:
	save	%sp, -96, %sp
.LLCFI128:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o2
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
	.size	LE__uLE_var_LC_u8, .-LE__uLE_var_LC_u8
	.align 4
	.type	LE__LE_var_LC_u8, #function
	.proc	014
LE__LE_var_LC_u8:
.LLFB129:
	save	%sp, -96, %sp
.LLCFI129:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o2
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
	.size	LE__LE_var_LC_u8, .-LE__LE_var_LC_u8
	.align 4
	.type	LE__uLE_param_LC_u8, #function
	.proc	014
LE__uLE_param_LC_u8:
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
	bgu	%icc, .LL321
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL320
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL321
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
	.size	LE__uLE_param_LC_u8, .-LE__uLE_param_LC_u8
	.align 4
	.type	LE__LE_param_LC_u8, #function
	.proc	014
LE__LE_param_LC_u8:
.LLFB131:
	save	%sp, -104, %sp
.LLCFI131:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	ldd	[%fp-8], %o2
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
	.size	LE__LE_param_LC_u8, .-LE__LE_param_LC_u8
	.align 4
	.type	LE__uLE_var_LC_L, #function
	.proc	014
LE__uLE_var_LC_L:
.LLFB132:
	save	%sp, -96, %sp
.LLCFI132:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o2
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o4
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
	.size	LE__uLE_var_LC_L, .-LE__uLE_var_LC_L
	.align 4
	.type	LE__LE_var_LC_L, #function
	.proc	014
LE__LE_var_LC_L:
.LLFB133:
	save	%sp, -96, %sp
.LLCFI133:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o4
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
	.size	LE__LE_var_LC_L, .-LE__LE_var_LC_L
	.align 4
	.type	LE__uLE_param_LC_L, #function
	.proc	014
LE__uLE_param_LC_L:
.LLFB134:
	save	%sp, -112, %sp
.LLCFI134:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bgu	%icc, .LL333
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL332
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL333
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
	.size	LE__uLE_param_LC_L, .-LE__uLE_param_LC_L
	.align 4
	.type	LE__LE_param_LC_L, #function
	.proc	014
LE__LE_param_LC_L:
.LLFB135:
	save	%sp, -112, %sp
.LLCFI135:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	mov	1, %g1
	ld	[%fp-16], %o3
	cmp	%o3, %o4
	bl	%icc, .LL336
	 nop
	ld	[%fp-16], %o3
	cmp	%o3, %o4
	bne	%icc, .LL335
	 nop
	ld	[%fp-12], %o3
	cmp	%o3, %o5
	blu	%icc, .LL336
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
	.size	LE__LE_param_LC_L, .-LE__LE_param_LC_L
	.align 4
	.type	LE__uLE_var_LC_i8, #function
	.proc	014
LE__uLE_var_LC_i8:
.LLFB136:
	save	%sp, -96, %sp
.LLCFI136:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL339
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL338
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL339
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL338
	 nop
.LL339:
	mov	0, %g1
.LL338:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE136:
	.size	LE__uLE_var_LC_i8, .-LE__uLE_var_LC_i8
	.align 4
	.type	LE__LE_var_LC_i8, #function
	.proc	014
LE__LE_var_LC_i8:
.LLFB137:
	save	%sp, -96, %sp
.LLCFI137:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL342
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL341
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL342
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL341
	 nop
.LL342:
	mov	0, %g1
.LL341:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE137:
	.size	LE__LE_var_LC_i8, .-LE__LE_var_LC_i8
	.align 4
	.type	LE__uLE_param_LC_i8, #function
	.proc	014
LE__uLE_param_LC_i8:
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
	bgu	%icc, .LL345
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL344
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL345
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
	.size	LE__uLE_param_LC_i8, .-LE__uLE_param_LC_i8
	.align 4
	.type	LE__LE_param_LC_i8, #function
	.proc	014
LE__LE_param_LC_i8:
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
	cmp	%o2, %o4
	bg	%icc, .LL348
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL347
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL348
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL347
	 nop
.LL348:
	mov	0, %g1
.LL347:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE139:
	.size	LE__LE_param_LC_i8, .-LE__LE_param_LC_i8
	.align 4
	.type	LE__uLE_var_LC_u32, #function
	.proc	014
LE__uLE_var_LC_u32:
.LLFB140:
	save	%sp, -96, %sp
.LLCFI140:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL351
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL350
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL351
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL350
	 nop
.LL351:
	mov	0, %g1
.LL350:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE140:
	.size	LE__uLE_var_LC_u32, .-LE__uLE_var_LC_u32
	.align 4
	.type	LE__LE_var_LC_u32, #function
	.proc	014
LE__LE_var_LC_u32:
.LLFB141:
	save	%sp, -96, %sp
.LLCFI141:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL354
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL353
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL354
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL353
	 nop
.LL354:
	mov	0, %g1
.LL353:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE141:
	.size	LE__LE_var_LC_u32, .-LE__LE_var_LC_u32
	.align 4
	.type	LE__uLE_param_LC_u32, #function
	.proc	014
LE__uLE_param_LC_u32:
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
	bgu	%icc, .LL357
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL356
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL357
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
	.size	LE__uLE_param_LC_u32, .-LE__uLE_param_LC_u32
	.align 4
	.type	LE__LE_param_LC_u32, #function
	.proc	014
LE__LE_param_LC_u32:
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
	cmp	%o2, %o4
	bg	%icc, .LL360
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL359
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL360
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL359
	 nop
.LL360:
	mov	0, %g1
.LL359:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE143:
	.size	LE__LE_param_LC_u32, .-LE__LE_param_LC_u32
	.align 4
	.type	LE__uLE_var_i32_u16, #function
	.proc	014
LE__uLE_var_i32_u16:
.LLFB144:
	save	%sp, -96, %sp
.LLCFI144:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE144:
	.size	LE__uLE_var_i32_u16, .-LE__uLE_var_i32_u16
	.align 4
	.type	LE__LE_var_i32_u16, #function
	.proc	014
LE__LE_var_i32_u16:
.LLFB145:
	save	%sp, -96, %sp
.LLCFI145:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE145:
	.size	LE__LE_var_i32_u16, .-LE__LE_var_i32_u16
	.align 4
	.type	LE__uLE_param_i32_u16, #function
	.proc	014
LE__uLE_param_i32_u16:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE146:
	.size	LE__uLE_param_i32_u16, .-LE__uLE_param_i32_u16
	.align 4
	.type	LE__LE_param_i32_u16, #function
	.proc	014
LE__LE_param_i32_u16:
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
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE147:
	.size	LE__LE_param_i32_u16, .-LE__LE_param_i32_u16
	.align 4
	.type	LE__uLE_var_i32_u64, #function
	.proc	014
LE__uLE_var_i32_u64:
.LLFB148:
	save	%sp, -96, %sp
.LLCFI148:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL367
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL366
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL367
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL366
	 nop
.LL367:
	mov	0, %g1
.LL366:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE148:
	.size	LE__uLE_var_i32_u64, .-LE__uLE_var_i32_u64
	.align 4
	.type	LE__LE_var_i32_u64, #function
	.proc	014
LE__LE_var_i32_u64:
.LLFB149:
	save	%sp, -96, %sp
.LLCFI149:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL370
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL369
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL370
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL369
	 nop
.LL370:
	mov	0, %g1
.LL369:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE149:
	.size	LE__LE_var_i32_u64, .-LE__LE_var_i32_u64
	.align 4
	.type	LE__uLE_param_i32_u64, #function
	.proc	014
LE__uLE_param_i32_u64:
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
	cmp	%o4, %o2
	bgu	%icc, .LL373
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL372
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL373
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL372
	 nop
.LL373:
	mov	0, %g1
.LL372:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE150:
	.size	LE__uLE_param_i32_u64, .-LE__uLE_param_i32_u64
	.align 4
	.type	LE__LE_param_i32_u64, #function
	.proc	014
LE__LE_param_i32_u64:
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
	bl	%icc, .LL376
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL375
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL376
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
	.size	LE__LE_param_i32_u64, .-LE__LE_param_i32_u64
	.align 4
	.type	LE__uLE_var_i32_LC, #function
	.proc	014
LE__uLE_var_i32_LC:
.LLFB152:
	save	%sp, -96, %sp
.LLCFI152:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL379
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL378
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL379
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL378
	 nop
.LL379:
	mov	0, %g1
.LL378:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE152:
	.size	LE__uLE_var_i32_LC, .-LE__uLE_var_i32_LC
	.align 4
	.type	LE__LE_var_i32_LC, #function
	.proc	014
LE__LE_var_i32_LC:
.LLFB153:
	save	%sp, -96, %sp
.LLCFI153:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL382
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL381
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL382
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL381
	 nop
.LL382:
	mov	0, %g1
.LL381:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE153:
	.size	LE__LE_var_i32_LC, .-LE__LE_var_i32_LC
	.align 4
	.type	LE__uLE_param_i32_LC, #function
	.proc	014
LE__uLE_param_i32_LC:
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
	blu	%icc, .LL385
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL384
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL385
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
	.size	LE__uLE_param_i32_LC, .-LE__uLE_param_i32_LC
	.align 4
	.type	LE__LE_param_i32_LC, #function
	.proc	014
LE__LE_param_i32_LC:
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
	cmp	%o4, %o2
	bg	%icc, .LL388
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL387
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL388
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL387
	 nop
.LL388:
	mov	0, %g1
.LL387:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE155:
	.size	LE__LE_param_i32_LC, .-LE__LE_param_i32_LC
	.align 4
	.type	LE__uLE_var_i32_i32, #function
	.proc	014
LE__uLE_var_i32_i32:
.LLFB156:
	save	%sp, -96, %sp
.LLCFI156:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE156:
	.size	LE__uLE_var_i32_i32, .-LE__uLE_var_i32_i32
	.align 4
	.type	LE__LE_var_i32_i32, #function
	.proc	014
LE__LE_var_i32_i32:
.LLFB157:
	save	%sp, -96, %sp
.LLCFI157:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE157:
	.size	LE__LE_var_i32_i32, .-LE__LE_var_i32_i32
	.align 4
	.type	LE__uLE_param_i32_i32, #function
	.proc	014
LE__uLE_param_i32_i32:
.LLFB158:
	save	%sp, -96, %sp
.LLCFI158:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE158:
	.size	LE__uLE_param_i32_i32, .-LE__uLE_param_i32_i32
	.align 4
	.type	LE__LE_param_i32_i32, #function
	.proc	014
LE__LE_param_i32_i32:
.LLFB159:
	save	%sp, -96, %sp
.LLCFI159:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE159:
	.size	LE__LE_param_i32_i32, .-LE__LE_param_i32_i32
	.align 4
	.type	LE__uLE_var_i32_i16, #function
	.proc	014
LE__uLE_var_i32_i16:
.LLFB160:
	save	%sp, -96, %sp
.LLCFI160:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE160:
	.size	LE__uLE_var_i32_i16, .-LE__uLE_var_i32_i16
	.align 4
	.type	LE__LE_var_i32_i16, #function
	.proc	014
LE__LE_var_i32_i16:
.LLFB161:
	save	%sp, -96, %sp
.LLCFI161:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE161:
	.size	LE__LE_var_i32_i16, .-LE__LE_var_i32_i16
	.align 4
	.type	LE__uLE_param_i32_i16, #function
	.proc	014
LE__uLE_param_i32_i16:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE162:
	.size	LE__uLE_param_i32_i16, .-LE__uLE_param_i32_i16
	.align 4
	.type	LE__LE_param_i32_i16, #function
	.proc	014
LE__LE_param_i32_i16:
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
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE163:
	.size	LE__LE_param_i32_i16, .-LE__LE_param_i32_i16
	.align 4
	.type	LE__uLE_var_i32_I, #function
	.proc	014
LE__uLE_var_i32_I:
.LLFB164:
	save	%sp, -96, %sp
.LLCFI164:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE164:
	.size	LE__uLE_var_i32_I, .-LE__uLE_var_i32_I
	.align 4
	.type	LE__LE_var_i32_I, #function
	.proc	014
LE__LE_var_i32_I:
.LLFB165:
	save	%sp, -96, %sp
.LLCFI165:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE165:
	.size	LE__LE_var_i32_I, .-LE__LE_var_i32_I
	.align 4
	.type	LE__uLE_param_i32_I, #function
	.proc	014
LE__uLE_param_i32_I:
.LLFB166:
	save	%sp, -96, %sp
.LLCFI166:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE166:
	.size	LE__uLE_param_i32_I, .-LE__uLE_param_i32_I
	.align 4
	.type	LE__LE_param_i32_I, #function
	.proc	014
LE__LE_param_i32_I:
.LLFB167:
	save	%sp, -96, %sp
.LLCFI167:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE167:
	.size	LE__LE_param_i32_I, .-LE__LE_param_i32_I
	.align 4
	.type	LE__uLE_var_i32_i64, #function
	.proc	014
LE__uLE_var_i32_i64:
.LLFB168:
	save	%sp, -96, %sp
.LLCFI168:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL403
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL402
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL403
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL402
	 nop
.LL403:
	mov	0, %g1
.LL402:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE168:
	.size	LE__uLE_var_i32_i64, .-LE__uLE_var_i32_i64
	.align 4
	.type	LE__LE_var_i32_i64, #function
	.proc	014
LE__LE_var_i32_i64:
.LLFB169:
	save	%sp, -96, %sp
.LLCFI169:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL406
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL405
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL406
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL405
	 nop
.LL406:
	mov	0, %g1
.LL405:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE169:
	.size	LE__LE_var_i32_i64, .-LE__LE_var_i32_i64
	.align 4
	.type	LE__uLE_param_i32_i64, #function
	.proc	014
LE__uLE_param_i32_i64:
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
	cmp	%o4, %o2
	bgu	%icc, .LL409
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL408
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL409
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL408
	 nop
.LL409:
	mov	0, %g1
.LL408:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE170:
	.size	LE__uLE_param_i32_i64, .-LE__uLE_param_i32_i64
	.align 4
	.type	LE__LE_param_i32_i64, #function
	.proc	014
LE__LE_param_i32_i64:
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
	bl	%icc, .LL412
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL411
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL412
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
	.size	LE__LE_param_i32_i64, .-LE__LE_param_i32_i64
	.align 4
	.type	LE__uLE_var_i32_C, #function
	.proc	014
LE__uLE_var_i32_C:
.LLFB172:
	save	%sp, -96, %sp
.LLCFI172:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE172:
	.size	LE__uLE_var_i32_C, .-LE__uLE_var_i32_C
	.align 4
	.type	LE__LE_var_i32_C, #function
	.proc	014
LE__LE_var_i32_C:
.LLFB173:
	save	%sp, -96, %sp
.LLCFI173:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE173:
	.size	LE__LE_var_i32_C, .-LE__LE_var_i32_C
	.align 4
	.type	LE__uLE_param_i32_C, #function
	.proc	014
LE__uLE_param_i32_C:
.LLFB174:
	save	%sp, -96, %sp
.LLCFI174:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE174:
	.size	LE__uLE_param_i32_C, .-LE__uLE_param_i32_C
	.align 4
	.type	LE__LE_param_i32_C, #function
	.proc	014
LE__LE_param_i32_C:
.LLFB175:
	save	%sp, -96, %sp
.LLCFI175:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE175:
	.size	LE__LE_param_i32_C, .-LE__LE_param_i32_C
	.align 4
	.type	LE__uLE_var_i32_u8, #function
	.proc	014
LE__uLE_var_i32_u8:
.LLFB176:
	save	%sp, -96, %sp
.LLCFI176:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE176:
	.size	LE__uLE_var_i32_u8, .-LE__uLE_var_i32_u8
	.align 4
	.type	LE__LE_var_i32_u8, #function
	.proc	014
LE__LE_var_i32_u8:
.LLFB177:
	save	%sp, -96, %sp
.LLCFI177:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE177:
	.size	LE__LE_var_i32_u8, .-LE__LE_var_i32_u8
	.align 4
	.type	LE__uLE_param_i32_u8, #function
	.proc	014
LE__uLE_param_i32_u8:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE178:
	.size	LE__uLE_param_i32_u8, .-LE__uLE_param_i32_u8
	.align 4
	.type	LE__LE_param_i32_u8, #function
	.proc	014
LE__LE_param_i32_u8:
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
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE179:
	.size	LE__LE_param_i32_u8, .-LE__LE_param_i32_u8
	.align 4
	.type	LE__uLE_var_i32_L, #function
	.proc	014
LE__uLE_var_i32_L:
.LLFB180:
	save	%sp, -96, %sp
.LLCFI180:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL423
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL422
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL423
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL422
	 nop
.LL423:
	mov	0, %g1
.LL422:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE180:
	.size	LE__uLE_var_i32_L, .-LE__uLE_var_i32_L
	.align 4
	.type	LE__LE_var_i32_L, #function
	.proc	014
LE__LE_var_i32_L:
.LLFB181:
	save	%sp, -96, %sp
.LLCFI181:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL426
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL425
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL426
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL425
	 nop
.LL426:
	mov	0, %g1
.LL425:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE181:
	.size	LE__LE_var_i32_L, .-LE__LE_var_i32_L
	.align 4
	.type	LE__uLE_param_i32_L, #function
	.proc	014
LE__uLE_param_i32_L:
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
	cmp	%o4, %o2
	bgu	%icc, .LL429
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL428
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL429
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL428
	 nop
.LL429:
	mov	0, %g1
.LL428:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE182:
	.size	LE__uLE_param_i32_L, .-LE__uLE_param_i32_L
	.align 4
	.type	LE__LE_param_i32_L, #function
	.proc	014
LE__LE_param_i32_L:
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
	bl	%icc, .LL432
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL431
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL432
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
	.size	LE__LE_param_i32_L, .-LE__LE_param_i32_L
	.align 4
	.type	LE__uLE_var_i32_i8, #function
	.proc	014
LE__uLE_var_i32_i8:
.LLFB184:
	save	%sp, -96, %sp
.LLCFI184:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE184:
	.size	LE__uLE_var_i32_i8, .-LE__uLE_var_i32_i8
	.align 4
	.type	LE__LE_var_i32_i8, #function
	.proc	014
LE__LE_var_i32_i8:
.LLFB185:
	save	%sp, -96, %sp
.LLCFI185:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE185:
	.size	LE__LE_var_i32_i8, .-LE__LE_var_i32_i8
	.align 4
	.type	LE__uLE_param_i32_i8, #function
	.proc	014
LE__uLE_param_i32_i8:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE186:
	.size	LE__uLE_param_i32_i8, .-LE__uLE_param_i32_i8
	.align 4
	.type	LE__LE_param_i32_i8, #function
	.proc	014
LE__LE_param_i32_i8:
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
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE187:
	.size	LE__LE_param_i32_i8, .-LE__LE_param_i32_i8
	.align 4
	.type	LE__uLE_var_i32_u32, #function
	.proc	014
LE__uLE_var_i32_u32:
.LLFB188:
	save	%sp, -96, %sp
.LLCFI188:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE188:
	.size	LE__uLE_var_i32_u32, .-LE__uLE_var_i32_u32
	.align 4
	.type	LE__LE_var_i32_u32, #function
	.proc	014
LE__LE_var_i32_u32:
.LLFB189:
	save	%sp, -96, %sp
.LLCFI189:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE189:
	.size	LE__LE_var_i32_u32, .-LE__LE_var_i32_u32
	.align 4
	.type	LE__uLE_param_i32_u32, #function
	.proc	014
LE__uLE_param_i32_u32:
.LLFB190:
	save	%sp, -96, %sp
.LLCFI190:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE190:
	.size	LE__uLE_param_i32_u32, .-LE__uLE_param_i32_u32
	.align 4
	.type	LE__LE_param_i32_u32, #function
	.proc	014
LE__LE_param_i32_u32:
.LLFB191:
	save	%sp, -96, %sp
.LLCFI191:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE191:
	.size	LE__LE_param_i32_u32, .-LE__LE_param_i32_u32
	.align 4
	.type	LE__uLE_var_i16_u16, #function
	.proc	014
LE__uLE_var_i16_u16:
.LLFB192:
	save	%sp, -96, %sp
.LLCFI192:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE192:
	.size	LE__uLE_var_i16_u16, .-LE__uLE_var_i16_u16
	.align 4
	.type	LE__LE_var_i16_u16, #function
	.proc	014
LE__LE_var_i16_u16:
.LLFB193:
	save	%sp, -96, %sp
.LLCFI193:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE193:
	.size	LE__LE_var_i16_u16, .-LE__LE_var_i16_u16
	.align 4
	.type	LE__uLE_param_i16_u16, #function
	.proc	014
LE__uLE_param_i16_u16:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE194:
	.size	LE__uLE_param_i16_u16, .-LE__uLE_param_i16_u16
	.align 4
	.type	LE__LE_param_i16_u16, #function
	.proc	014
LE__LE_param_i16_u16:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE195:
	.size	LE__LE_param_i16_u16, .-LE__LE_param_i16_u16
	.align 4
	.type	LE__uLE_var_i16_u64, #function
	.proc	014
LE__uLE_var_i16_u64:
.LLFB196:
	save	%sp, -96, %sp
.LLCFI196:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL447
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL446
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL447
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL446
	 nop
.LL447:
	mov	0, %g1
.LL446:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE196:
	.size	LE__uLE_var_i16_u64, .-LE__uLE_var_i16_u64
	.align 4
	.type	LE__LE_var_i16_u64, #function
	.proc	014
LE__LE_var_i16_u64:
.LLFB197:
	save	%sp, -96, %sp
.LLCFI197:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL450
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL449
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL450
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL449
	 nop
.LL450:
	mov	0, %g1
.LL449:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE197:
	.size	LE__LE_var_i16_u64, .-LE__LE_var_i16_u64
	.align 4
	.type	LE__uLE_param_i16_u64, #function
	.proc	014
LE__uLE_param_i16_u64:
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
	cmp	%o4, %o2
	bgu	%icc, .LL453
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL452
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL453
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL452
	 nop
.LL453:
	mov	0, %g1
.LL452:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE198:
	.size	LE__uLE_param_i16_u64, .-LE__uLE_param_i16_u64
	.align 4
	.type	LE__LE_param_i16_u64, #function
	.proc	014
LE__LE_param_i16_u64:
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
	bl	%icc, .LL456
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL455
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL456
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
	.size	LE__LE_param_i16_u64, .-LE__LE_param_i16_u64
	.align 4
	.type	LE__uLE_var_i16_LC, #function
	.proc	014
LE__uLE_var_i16_LC:
.LLFB200:
	save	%sp, -96, %sp
.LLCFI200:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL459
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL458
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL459
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL458
	 nop
.LL459:
	mov	0, %g1
.LL458:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE200:
	.size	LE__uLE_var_i16_LC, .-LE__uLE_var_i16_LC
	.align 4
	.type	LE__LE_var_i16_LC, #function
	.proc	014
LE__LE_var_i16_LC:
.LLFB201:
	save	%sp, -96, %sp
.LLCFI201:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL462
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL461
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL462
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL461
	 nop
.LL462:
	mov	0, %g1
.LL461:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE201:
	.size	LE__LE_var_i16_LC, .-LE__LE_var_i16_LC
	.align 4
	.type	LE__uLE_param_i16_LC, #function
	.proc	014
LE__uLE_param_i16_LC:
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
	blu	%icc, .LL465
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL464
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL465
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
	.size	LE__uLE_param_i16_LC, .-LE__uLE_param_i16_LC
	.align 4
	.type	LE__LE_param_i16_LC, #function
	.proc	014
LE__LE_param_i16_LC:
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
	cmp	%o4, %o2
	bg	%icc, .LL468
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL467
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL468
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL467
	 nop
.LL468:
	mov	0, %g1
.LL467:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE203:
	.size	LE__LE_param_i16_LC, .-LE__LE_param_i16_LC
	.align 4
	.type	LE__uLE_var_i16_i32, #function
	.proc	014
LE__uLE_var_i16_i32:
.LLFB204:
	save	%sp, -96, %sp
.LLCFI204:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE204:
	.size	LE__uLE_var_i16_i32, .-LE__uLE_var_i16_i32
	.align 4
	.type	LE__LE_var_i16_i32, #function
	.proc	014
LE__LE_var_i16_i32:
.LLFB205:
	save	%sp, -96, %sp
.LLCFI205:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE205:
	.size	LE__LE_var_i16_i32, .-LE__LE_var_i16_i32
	.align 4
	.type	LE__uLE_param_i16_i32, #function
	.proc	014
LE__uLE_param_i16_i32:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE206:
	.size	LE__uLE_param_i16_i32, .-LE__uLE_param_i16_i32
	.align 4
	.type	LE__LE_param_i16_i32, #function
	.proc	014
LE__LE_param_i16_i32:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE207:
	.size	LE__LE_param_i16_i32, .-LE__LE_param_i16_i32
	.align 4
	.type	LE__uLE_var_i16_i16, #function
	.proc	014
LE__uLE_var_i16_i16:
.LLFB208:
	save	%sp, -96, %sp
.LLCFI208:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE208:
	.size	LE__uLE_var_i16_i16, .-LE__uLE_var_i16_i16
	.align 4
	.type	LE__LE_var_i16_i16, #function
	.proc	014
LE__LE_var_i16_i16:
.LLFB209:
	save	%sp, -96, %sp
.LLCFI209:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE209:
	.size	LE__LE_var_i16_i16, .-LE__LE_var_i16_i16
	.align 4
	.type	LE__uLE_param_i16_i16, #function
	.proc	014
LE__uLE_param_i16_i16:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE210:
	.size	LE__uLE_param_i16_i16, .-LE__uLE_param_i16_i16
	.align 4
	.type	LE__LE_param_i16_i16, #function
	.proc	014
LE__LE_param_i16_i16:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE211:
	.size	LE__LE_param_i16_i16, .-LE__LE_param_i16_i16
	.align 4
	.type	LE__uLE_var_i16_I, #function
	.proc	014
LE__uLE_var_i16_I:
.LLFB212:
	save	%sp, -96, %sp
.LLCFI212:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE212:
	.size	LE__uLE_var_i16_I, .-LE__uLE_var_i16_I
	.align 4
	.type	LE__LE_var_i16_I, #function
	.proc	014
LE__LE_var_i16_I:
.LLFB213:
	save	%sp, -96, %sp
.LLCFI213:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE213:
	.size	LE__LE_var_i16_I, .-LE__LE_var_i16_I
	.align 4
	.type	LE__uLE_param_i16_I, #function
	.proc	014
LE__uLE_param_i16_I:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE214:
	.size	LE__uLE_param_i16_I, .-LE__uLE_param_i16_I
	.align 4
	.type	LE__LE_param_i16_I, #function
	.proc	014
LE__LE_param_i16_I:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE215:
	.size	LE__LE_param_i16_I, .-LE__LE_param_i16_I
	.align 4
	.type	LE__uLE_var_i16_i64, #function
	.proc	014
LE__uLE_var_i16_i64:
.LLFB216:
	save	%sp, -96, %sp
.LLCFI216:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL483
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL482
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL483
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL482
	 nop
.LL483:
	mov	0, %g1
.LL482:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE216:
	.size	LE__uLE_var_i16_i64, .-LE__uLE_var_i16_i64
	.align 4
	.type	LE__LE_var_i16_i64, #function
	.proc	014
LE__LE_var_i16_i64:
.LLFB217:
	save	%sp, -96, %sp
.LLCFI217:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL486
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL485
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL486
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL485
	 nop
.LL486:
	mov	0, %g1
.LL485:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE217:
	.size	LE__LE_var_i16_i64, .-LE__LE_var_i16_i64
	.align 4
	.type	LE__uLE_param_i16_i64, #function
	.proc	014
LE__uLE_param_i16_i64:
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
	cmp	%o4, %o2
	bgu	%icc, .LL489
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL488
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL489
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL488
	 nop
.LL489:
	mov	0, %g1
.LL488:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE218:
	.size	LE__uLE_param_i16_i64, .-LE__uLE_param_i16_i64
	.align 4
	.type	LE__LE_param_i16_i64, #function
	.proc	014
LE__LE_param_i16_i64:
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
	bl	%icc, .LL492
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL491
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL492
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
	.size	LE__LE_param_i16_i64, .-LE__LE_param_i16_i64
	.align 4
	.type	LE__uLE_var_i16_C, #function
	.proc	014
LE__uLE_var_i16_C:
.LLFB220:
	save	%sp, -96, %sp
.LLCFI220:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE220:
	.size	LE__uLE_var_i16_C, .-LE__uLE_var_i16_C
	.align 4
	.type	LE__LE_var_i16_C, #function
	.proc	014
LE__LE_var_i16_C:
.LLFB221:
	save	%sp, -96, %sp
.LLCFI221:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE221:
	.size	LE__LE_var_i16_C, .-LE__LE_var_i16_C
	.align 4
	.type	LE__uLE_param_i16_C, #function
	.proc	014
LE__uLE_param_i16_C:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE222:
	.size	LE__uLE_param_i16_C, .-LE__uLE_param_i16_C
	.align 4
	.type	LE__LE_param_i16_C, #function
	.proc	014
LE__LE_param_i16_C:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE223:
	.size	LE__LE_param_i16_C, .-LE__LE_param_i16_C
	.align 4
	.type	LE__uLE_var_i16_u8, #function
	.proc	014
LE__uLE_var_i16_u8:
.LLFB224:
	save	%sp, -96, %sp
.LLCFI224:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE224:
	.size	LE__uLE_var_i16_u8, .-LE__uLE_var_i16_u8
	.align 4
	.type	LE__LE_var_i16_u8, #function
	.proc	014
LE__LE_var_i16_u8:
.LLFB225:
	save	%sp, -96, %sp
.LLCFI225:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE225:
	.size	LE__LE_var_i16_u8, .-LE__LE_var_i16_u8
	.align 4
	.type	LE__uLE_param_i16_u8, #function
	.proc	014
LE__uLE_param_i16_u8:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE226:
	.size	LE__uLE_param_i16_u8, .-LE__uLE_param_i16_u8
	.align 4
	.type	LE__LE_param_i16_u8, #function
	.proc	014
LE__LE_param_i16_u8:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE227:
	.size	LE__LE_param_i16_u8, .-LE__LE_param_i16_u8
	.align 4
	.type	LE__uLE_var_i16_L, #function
	.proc	014
LE__uLE_var_i16_L:
.LLFB228:
	save	%sp, -96, %sp
.LLCFI228:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL503
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL502
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL503
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL502
	 nop
.LL503:
	mov	0, %g1
.LL502:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE228:
	.size	LE__uLE_var_i16_L, .-LE__uLE_var_i16_L
	.align 4
	.type	LE__LE_var_i16_L, #function
	.proc	014
LE__LE_var_i16_L:
.LLFB229:
	save	%sp, -96, %sp
.LLCFI229:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL506
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL505
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL506
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL505
	 nop
.LL506:
	mov	0, %g1
.LL505:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE229:
	.size	LE__LE_var_i16_L, .-LE__LE_var_i16_L
	.align 4
	.type	LE__uLE_param_i16_L, #function
	.proc	014
LE__uLE_param_i16_L:
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
	cmp	%o4, %o2
	bgu	%icc, .LL509
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL508
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL509
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL508
	 nop
.LL509:
	mov	0, %g1
.LL508:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE230:
	.size	LE__uLE_param_i16_L, .-LE__uLE_param_i16_L
	.align 4
	.type	LE__LE_param_i16_L, #function
	.proc	014
LE__LE_param_i16_L:
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
	bl	%icc, .LL512
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL511
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL512
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
	.size	LE__LE_param_i16_L, .-LE__LE_param_i16_L
	.align 4
	.type	LE__uLE_var_i16_i8, #function
	.proc	014
LE__uLE_var_i16_i8:
.LLFB232:
	save	%sp, -96, %sp
.LLCFI232:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE232:
	.size	LE__uLE_var_i16_i8, .-LE__uLE_var_i16_i8
	.align 4
	.type	LE__LE_var_i16_i8, #function
	.proc	014
LE__LE_var_i16_i8:
.LLFB233:
	save	%sp, -96, %sp
.LLCFI233:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE233:
	.size	LE__LE_var_i16_i8, .-LE__LE_var_i16_i8
	.align 4
	.type	LE__uLE_param_i16_i8, #function
	.proc	014
LE__uLE_param_i16_i8:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE234:
	.size	LE__uLE_param_i16_i8, .-LE__uLE_param_i16_i8
	.align 4
	.type	LE__LE_param_i16_i8, #function
	.proc	014
LE__LE_param_i16_i8:
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
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE235:
	.size	LE__LE_param_i16_i8, .-LE__LE_param_i16_i8
	.align 4
	.type	LE__uLE_var_i16_u32, #function
	.proc	014
LE__uLE_var_i16_u32:
.LLFB236:
	save	%sp, -96, %sp
.LLCFI236:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE236:
	.size	LE__uLE_var_i16_u32, .-LE__uLE_var_i16_u32
	.align 4
	.type	LE__LE_var_i16_u32, #function
	.proc	014
LE__LE_var_i16_u32:
.LLFB237:
	save	%sp, -96, %sp
.LLCFI237:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE237:
	.size	LE__LE_var_i16_u32, .-LE__LE_var_i16_u32
	.align 4
	.type	LE__uLE_param_i16_u32, #function
	.proc	014
LE__uLE_param_i16_u32:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE238:
	.size	LE__uLE_param_i16_u32, .-LE__uLE_param_i16_u32
	.align 4
	.type	LE__LE_param_i16_u32, #function
	.proc	014
LE__LE_param_i16_u32:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE239:
	.size	LE__LE_param_i16_u32, .-LE__LE_param_i16_u32
	.align 4
	.type	LE__uLE_var_I_u16, #function
	.proc	014
LE__uLE_var_I_u16:
.LLFB240:
	save	%sp, -96, %sp
.LLCFI240:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE240:
	.size	LE__uLE_var_I_u16, .-LE__uLE_var_I_u16
	.align 4
	.type	LE__LE_var_I_u16, #function
	.proc	014
LE__LE_var_I_u16:
.LLFB241:
	save	%sp, -96, %sp
.LLCFI241:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE241:
	.size	LE__LE_var_I_u16, .-LE__LE_var_I_u16
	.align 4
	.type	LE__uLE_param_I_u16, #function
	.proc	014
LE__uLE_param_I_u16:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE242:
	.size	LE__uLE_param_I_u16, .-LE__uLE_param_I_u16
	.align 4
	.type	LE__LE_param_I_u16, #function
	.proc	014
LE__LE_param_I_u16:
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
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE243:
	.size	LE__LE_param_I_u16, .-LE__LE_param_I_u16
	.align 4
	.type	LE__uLE_var_I_u64, #function
	.proc	014
LE__uLE_var_I_u64:
.LLFB244:
	save	%sp, -96, %sp
.LLCFI244:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL527
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL526
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL527
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL526
	 nop
.LL527:
	mov	0, %g1
.LL526:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE244:
	.size	LE__uLE_var_I_u64, .-LE__uLE_var_I_u64
	.align 4
	.type	LE__LE_var_I_u64, #function
	.proc	014
LE__LE_var_I_u64:
.LLFB245:
	save	%sp, -96, %sp
.LLCFI245:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL530
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL529
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL530
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL529
	 nop
.LL530:
	mov	0, %g1
.LL529:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE245:
	.size	LE__LE_var_I_u64, .-LE__LE_var_I_u64
	.align 4
	.type	LE__uLE_param_I_u64, #function
	.proc	014
LE__uLE_param_I_u64:
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
	cmp	%o4, %o2
	bgu	%icc, .LL533
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL532
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL533
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL532
	 nop
.LL533:
	mov	0, %g1
.LL532:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE246:
	.size	LE__uLE_param_I_u64, .-LE__uLE_param_I_u64
	.align 4
	.type	LE__LE_param_I_u64, #function
	.proc	014
LE__LE_param_I_u64:
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
	bl	%icc, .LL536
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL535
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL536
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
	.size	LE__LE_param_I_u64, .-LE__LE_param_I_u64
	.align 4
	.type	LE__uLE_var_I_LC, #function
	.proc	014
LE__uLE_var_I_LC:
.LLFB248:
	save	%sp, -96, %sp
.LLCFI248:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL539
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL538
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL539
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL538
	 nop
.LL539:
	mov	0, %g1
.LL538:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE248:
	.size	LE__uLE_var_I_LC, .-LE__uLE_var_I_LC
	.align 4
	.type	LE__LE_var_I_LC, #function
	.proc	014
LE__LE_var_I_LC:
.LLFB249:
	save	%sp, -96, %sp
.LLCFI249:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL542
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL541
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL542
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL541
	 nop
.LL542:
	mov	0, %g1
.LL541:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE249:
	.size	LE__LE_var_I_LC, .-LE__LE_var_I_LC
	.align 4
	.type	LE__uLE_param_I_LC, #function
	.proc	014
LE__uLE_param_I_LC:
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
	blu	%icc, .LL545
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL544
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL545
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
	.size	LE__uLE_param_I_LC, .-LE__uLE_param_I_LC
	.align 4
	.type	LE__LE_param_I_LC, #function
	.proc	014
LE__LE_param_I_LC:
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
	cmp	%o4, %o2
	bg	%icc, .LL548
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL547
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL548
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL547
	 nop
.LL548:
	mov	0, %g1
.LL547:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE251:
	.size	LE__LE_param_I_LC, .-LE__LE_param_I_LC
	.align 4
	.type	LE__uLE_var_I_i32, #function
	.proc	014
LE__uLE_var_I_i32:
.LLFB252:
	save	%sp, -96, %sp
.LLCFI252:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE252:
	.size	LE__uLE_var_I_i32, .-LE__uLE_var_I_i32
	.align 4
	.type	LE__LE_var_I_i32, #function
	.proc	014
LE__LE_var_I_i32:
.LLFB253:
	save	%sp, -96, %sp
.LLCFI253:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE253:
	.size	LE__LE_var_I_i32, .-LE__LE_var_I_i32
	.align 4
	.type	LE__uLE_param_I_i32, #function
	.proc	014
LE__uLE_param_I_i32:
.LLFB254:
	save	%sp, -96, %sp
.LLCFI254:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE254:
	.size	LE__uLE_param_I_i32, .-LE__uLE_param_I_i32
	.align 4
	.type	LE__LE_param_I_i32, #function
	.proc	014
LE__LE_param_I_i32:
.LLFB255:
	save	%sp, -96, %sp
.LLCFI255:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE255:
	.size	LE__LE_param_I_i32, .-LE__LE_param_I_i32
	.align 4
	.type	LE__uLE_var_I_i16, #function
	.proc	014
LE__uLE_var_I_i16:
.LLFB256:
	save	%sp, -96, %sp
.LLCFI256:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE256:
	.size	LE__uLE_var_I_i16, .-LE__uLE_var_I_i16
	.align 4
	.type	LE__LE_var_I_i16, #function
	.proc	014
LE__LE_var_I_i16:
.LLFB257:
	save	%sp, -96, %sp
.LLCFI257:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE257:
	.size	LE__LE_var_I_i16, .-LE__LE_var_I_i16
	.align 4
	.type	LE__uLE_param_I_i16, #function
	.proc	014
LE__uLE_param_I_i16:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE258:
	.size	LE__uLE_param_I_i16, .-LE__uLE_param_I_i16
	.align 4
	.type	LE__LE_param_I_i16, #function
	.proc	014
LE__LE_param_I_i16:
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
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE259:
	.size	LE__LE_param_I_i16, .-LE__LE_param_I_i16
	.align 4
	.type	LE__uLE_var_I_I, #function
	.proc	014
LE__uLE_var_I_I:
.LLFB260:
	save	%sp, -96, %sp
.LLCFI260:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE260:
	.size	LE__uLE_var_I_I, .-LE__uLE_var_I_I
	.align 4
	.type	LE__LE_var_I_I, #function
	.proc	014
LE__LE_var_I_I:
.LLFB261:
	save	%sp, -96, %sp
.LLCFI261:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE261:
	.size	LE__LE_var_I_I, .-LE__LE_var_I_I
	.align 4
	.type	LE__uLE_param_I_I, #function
	.proc	014
LE__uLE_param_I_I:
.LLFB262:
	save	%sp, -96, %sp
.LLCFI262:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE262:
	.size	LE__uLE_param_I_I, .-LE__uLE_param_I_I
	.align 4
	.type	LE__LE_param_I_I, #function
	.proc	014
LE__LE_param_I_I:
.LLFB263:
	save	%sp, -96, %sp
.LLCFI263:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE263:
	.size	LE__LE_param_I_I, .-LE__LE_param_I_I
	.align 4
	.type	LE__uLE_var_I_i64, #function
	.proc	014
LE__uLE_var_I_i64:
.LLFB264:
	save	%sp, -96, %sp
.LLCFI264:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL563
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL562
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL563
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL562
	 nop
.LL563:
	mov	0, %g1
.LL562:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE264:
	.size	LE__uLE_var_I_i64, .-LE__uLE_var_I_i64
	.align 4
	.type	LE__LE_var_I_i64, #function
	.proc	014
LE__LE_var_I_i64:
.LLFB265:
	save	%sp, -96, %sp
.LLCFI265:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL566
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL565
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL566
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL565
	 nop
.LL566:
	mov	0, %g1
.LL565:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE265:
	.size	LE__LE_var_I_i64, .-LE__LE_var_I_i64
	.align 4
	.type	LE__uLE_param_I_i64, #function
	.proc	014
LE__uLE_param_I_i64:
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
	cmp	%o4, %o2
	bgu	%icc, .LL569
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL568
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL569
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL568
	 nop
.LL569:
	mov	0, %g1
.LL568:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE266:
	.size	LE__uLE_param_I_i64, .-LE__uLE_param_I_i64
	.align 4
	.type	LE__LE_param_I_i64, #function
	.proc	014
LE__LE_param_I_i64:
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
	bl	%icc, .LL572
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL571
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL572
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
	.size	LE__LE_param_I_i64, .-LE__LE_param_I_i64
	.align 4
	.type	LE__uLE_var_I_C, #function
	.proc	014
LE__uLE_var_I_C:
.LLFB268:
	save	%sp, -96, %sp
.LLCFI268:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE268:
	.size	LE__uLE_var_I_C, .-LE__uLE_var_I_C
	.align 4
	.type	LE__LE_var_I_C, #function
	.proc	014
LE__LE_var_I_C:
.LLFB269:
	save	%sp, -96, %sp
.LLCFI269:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE269:
	.size	LE__LE_var_I_C, .-LE__LE_var_I_C
	.align 4
	.type	LE__uLE_param_I_C, #function
	.proc	014
LE__uLE_param_I_C:
.LLFB270:
	save	%sp, -96, %sp
.LLCFI270:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE270:
	.size	LE__uLE_param_I_C, .-LE__uLE_param_I_C
	.align 4
	.type	LE__LE_param_I_C, #function
	.proc	014
LE__LE_param_I_C:
.LLFB271:
	save	%sp, -96, %sp
.LLCFI271:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE271:
	.size	LE__LE_param_I_C, .-LE__LE_param_I_C
	.align 4
	.type	LE__uLE_var_I_u8, #function
	.proc	014
LE__uLE_var_I_u8:
.LLFB272:
	save	%sp, -96, %sp
.LLCFI272:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE272:
	.size	LE__uLE_var_I_u8, .-LE__uLE_var_I_u8
	.align 4
	.type	LE__LE_var_I_u8, #function
	.proc	014
LE__LE_var_I_u8:
.LLFB273:
	save	%sp, -96, %sp
.LLCFI273:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE273:
	.size	LE__LE_var_I_u8, .-LE__LE_var_I_u8
	.align 4
	.type	LE__uLE_param_I_u8, #function
	.proc	014
LE__uLE_param_I_u8:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE274:
	.size	LE__uLE_param_I_u8, .-LE__uLE_param_I_u8
	.align 4
	.type	LE__LE_param_I_u8, #function
	.proc	014
LE__LE_param_I_u8:
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
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE275:
	.size	LE__LE_param_I_u8, .-LE__LE_param_I_u8
	.align 4
	.type	LE__uLE_var_I_L, #function
	.proc	014
LE__uLE_var_I_L:
.LLFB276:
	save	%sp, -96, %sp
.LLCFI276:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL583
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL582
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL583
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL582
	 nop
.LL583:
	mov	0, %g1
.LL582:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE276:
	.size	LE__uLE_var_I_L, .-LE__uLE_var_I_L
	.align 4
	.type	LE__LE_var_I_L, #function
	.proc	014
LE__LE_var_I_L:
.LLFB277:
	save	%sp, -96, %sp
.LLCFI277:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL586
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL585
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL586
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL585
	 nop
.LL586:
	mov	0, %g1
.LL585:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE277:
	.size	LE__LE_var_I_L, .-LE__LE_var_I_L
	.align 4
	.type	LE__uLE_param_I_L, #function
	.proc	014
LE__uLE_param_I_L:
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
	cmp	%o4, %o2
	bgu	%icc, .LL589
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL588
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL589
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL588
	 nop
.LL589:
	mov	0, %g1
.LL588:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE278:
	.size	LE__uLE_param_I_L, .-LE__uLE_param_I_L
	.align 4
	.type	LE__LE_param_I_L, #function
	.proc	014
LE__LE_param_I_L:
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
	bl	%icc, .LL592
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL591
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL592
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
	.size	LE__LE_param_I_L, .-LE__LE_param_I_L
	.align 4
	.type	LE__uLE_var_I_i8, #function
	.proc	014
LE__uLE_var_I_i8:
.LLFB280:
	save	%sp, -96, %sp
.LLCFI280:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE280:
	.size	LE__uLE_var_I_i8, .-LE__uLE_var_I_i8
	.align 4
	.type	LE__LE_var_I_i8, #function
	.proc	014
LE__LE_var_I_i8:
.LLFB281:
	save	%sp, -96, %sp
.LLCFI281:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE281:
	.size	LE__LE_var_I_i8, .-LE__LE_var_I_i8
	.align 4
	.type	LE__uLE_param_I_i8, #function
	.proc	014
LE__uLE_param_I_i8:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE282:
	.size	LE__uLE_param_I_i8, .-LE__uLE_param_I_i8
	.align 4
	.type	LE__LE_param_I_i8, #function
	.proc	014
LE__LE_param_I_i8:
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
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE283:
	.size	LE__LE_param_I_i8, .-LE__LE_param_I_i8
	.align 4
	.type	LE__uLE_var_I_u32, #function
	.proc	014
LE__uLE_var_I_u32:
.LLFB284:
	save	%sp, -96, %sp
.LLCFI284:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE284:
	.size	LE__uLE_var_I_u32, .-LE__uLE_var_I_u32
	.align 4
	.type	LE__LE_var_I_u32, #function
	.proc	014
LE__LE_var_I_u32:
.LLFB285:
	save	%sp, -96, %sp
.LLCFI285:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE285:
	.size	LE__LE_var_I_u32, .-LE__LE_var_I_u32
	.align 4
	.type	LE__uLE_param_I_u32, #function
	.proc	014
LE__uLE_param_I_u32:
.LLFB286:
	save	%sp, -96, %sp
.LLCFI286:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE286:
	.size	LE__uLE_param_I_u32, .-LE__uLE_param_I_u32
	.align 4
	.type	LE__LE_param_I_u32, #function
	.proc	014
LE__LE_param_I_u32:
.LLFB287:
	save	%sp, -96, %sp
.LLCFI287:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE287:
	.size	LE__LE_param_I_u32, .-LE__LE_param_I_u32
	.align 4
	.type	LE__uLE_var_i64_u16, #function
	.proc	014
LE__uLE_var_i64_u16:
.LLFB288:
	save	%sp, -96, %sp
.LLCFI288:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o2
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
	.size	LE__uLE_var_i64_u16, .-LE__uLE_var_i64_u16
	.align 4
	.type	LE__LE_var_i64_u16, #function
	.proc	014
LE__LE_var_i64_u16:
.LLFB289:
	save	%sp, -96, %sp
.LLCFI289:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o2
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
	.size	LE__LE_var_i64_u16, .-LE__LE_var_i64_u16
	.align 4
	.type	LE__uLE_param_i64_u16, #function
	.proc	014
LE__uLE_param_i64_u16:
.LLFB290:
	save	%sp, -104, %sp
.LLCFI290:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	ldd	[%fp-8], %o2
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
	.size	LE__uLE_param_i64_u16, .-LE__uLE_param_i64_u16
	.align 4
	.type	LE__LE_param_i64_u16, #function
	.proc	014
LE__LE_param_i64_u16:
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
	bg	%icc, .LL612
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL611
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL612
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
	.size	LE__LE_param_i64_u16, .-LE__LE_param_i64_u16
	.align 4
	.type	LE__uLE_var_i64_u64, #function
	.proc	014
LE__uLE_var_i64_u64:
.LLFB292:
	save	%sp, -96, %sp
.LLCFI292:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o4
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
	.size	LE__uLE_var_i64_u64, .-LE__uLE_var_i64_u64
	.align 4
	.type	LE__LE_var_i64_u64, #function
	.proc	014
LE__LE_var_i64_u64:
.LLFB293:
	save	%sp, -96, %sp
.LLCFI293:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o2
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o4
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
	.size	LE__LE_var_i64_u64, .-LE__LE_var_i64_u64
	.align 4
	.type	LE__uLE_param_i64_u64, #function
	.proc	014
LE__uLE_param_i64_u64:
.LLFB294:
	save	%sp, -112, %sp
.LLCFI294:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
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
	.size	LE__uLE_param_i64_u64, .-LE__uLE_param_i64_u64
	.align 4
	.type	LE__LE_param_i64_u64, #function
	.proc	014
LE__LE_param_i64_u64:
.LLFB295:
	save	%sp, -112, %sp
.LLCFI295:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	mov	1, %g1
	ld	[%fp-8], %o4
	ld	[%fp-16], %o5
	cmp	%o4, %o5
	bg	%icc, .LL624
	 nop
	ld	[%fp-8], %o4
	ld	[%fp-16], %o5
	cmp	%o4, %o5
	bne	%icc, .LL623
	 nop
	ld	[%fp-4], %o4
	ld	[%fp-12], %o5
	cmp	%o4, %o5
	bgu	%icc, .LL624
	 nop
	ld	[%fp-4], %o4
	ld	[%fp-12], %o5
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
	.size	LE__LE_param_i64_u64, .-LE__LE_param_i64_u64
	.align 4
	.type	LE__uLE_var_i64_LC, #function
	.proc	014
LE__uLE_var_i64_LC:
.LLFB296:
	save	%sp, -96, %sp
.LLCFI296:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o4
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
	.size	LE__uLE_var_i64_LC, .-LE__uLE_var_i64_LC
	.align 4
	.type	LE__LE_var_i64_LC, #function
	.proc	014
LE__LE_var_i64_LC:
.LLFB297:
	save	%sp, -96, %sp
.LLCFI297:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o2
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o4
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
	.size	LE__LE_var_i64_LC, .-LE__LE_var_i64_LC
	.align 4
	.type	LE__uLE_param_i64_LC, #function
	.proc	014
LE__uLE_param_i64_LC:
.LLFB298:
	save	%sp, -112, %sp
.LLCFI298:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	mov	1, %g1
	ld	[%fp-16], %o3
	cmp	%o3, %o4
	blu	%icc, .LL633
	 nop
	ld	[%fp-16], %o3
	cmp	%o3, %o4
	bne	%icc, .LL632
	 nop
	ld	[%fp-12], %o3
	cmp	%o3, %o5
	blu	%icc, .LL633
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
	.size	LE__uLE_param_i64_LC, .-LE__uLE_param_i64_LC
	.align 4
	.type	LE__LE_param_i64_LC, #function
	.proc	014
LE__LE_param_i64_LC:
.LLFB299:
	save	%sp, -112, %sp
.LLCFI299:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bg	%icc, .LL636
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL635
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL636
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
	.size	LE__LE_param_i64_LC, .-LE__LE_param_i64_LC
	.align 4
	.type	LE__uLE_var_i64_i32, #function
	.proc	014
LE__uLE_var_i64_i32:
.LLFB300:
	save	%sp, -96, %sp
.LLCFI300:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL639
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL638
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL639
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL638
	 nop
.LL639:
	mov	0, %g1
.LL638:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE300:
	.size	LE__uLE_var_i64_i32, .-LE__uLE_var_i64_i32
	.align 4
	.type	LE__LE_var_i64_i32, #function
	.proc	014
LE__LE_var_i64_i32:
.LLFB301:
	save	%sp, -96, %sp
.LLCFI301:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL642
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL641
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL642
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL641
	 nop
.LL642:
	mov	0, %g1
.LL641:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE301:
	.size	LE__LE_var_i64_i32, .-LE__LE_var_i64_i32
	.align 4
	.type	LE__uLE_param_i64_i32, #function
	.proc	014
LE__uLE_param_i64_i32:
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
	cmp	%o2, %o4
	bgu	%icc, .LL645
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL644
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL645
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL644
	 nop
.LL645:
	mov	0, %g1
.LL644:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE302:
	.size	LE__uLE_param_i64_i32, .-LE__uLE_param_i64_i32
	.align 4
	.type	LE__LE_param_i64_i32, #function
	.proc	014
LE__LE_param_i64_i32:
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
	bg	%icc, .LL648
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL647
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL648
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
	.size	LE__LE_param_i64_i32, .-LE__LE_param_i64_i32
	.align 4
	.type	LE__uLE_var_i64_i16, #function
	.proc	014
LE__uLE_var_i64_i16:
.LLFB304:
	save	%sp, -96, %sp
.LLCFI304:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL651
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL650
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL651
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL650
	 nop
.LL651:
	mov	0, %g1
.LL650:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE304:
	.size	LE__uLE_var_i64_i16, .-LE__uLE_var_i64_i16
	.align 4
	.type	LE__LE_var_i64_i16, #function
	.proc	014
LE__LE_var_i64_i16:
.LLFB305:
	save	%sp, -96, %sp
.LLCFI305:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL654
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL653
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL654
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL653
	 nop
.LL654:
	mov	0, %g1
.LL653:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE305:
	.size	LE__LE_var_i64_i16, .-LE__LE_var_i64_i16
	.align 4
	.type	LE__uLE_param_i64_i16, #function
	.proc	014
LE__uLE_param_i64_i16:
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
	cmp	%o2, %o4
	bgu	%icc, .LL657
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL656
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL657
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL656
	 nop
.LL657:
	mov	0, %g1
.LL656:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE306:
	.size	LE__uLE_param_i64_i16, .-LE__uLE_param_i64_i16
	.align 4
	.type	LE__LE_param_i64_i16, #function
	.proc	014
LE__LE_param_i64_i16:
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
	bg	%icc, .LL660
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL659
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL660
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
	.size	LE__LE_param_i64_i16, .-LE__LE_param_i64_i16
	.align 4
	.type	LE__uLE_var_i64_I, #function
	.proc	014
LE__uLE_var_i64_I:
.LLFB308:
	save	%sp, -96, %sp
.LLCFI308:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL663
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL662
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL663
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL662
	 nop
.LL663:
	mov	0, %g1
.LL662:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE308:
	.size	LE__uLE_var_i64_I, .-LE__uLE_var_i64_I
	.align 4
	.type	LE__LE_var_i64_I, #function
	.proc	014
LE__LE_var_i64_I:
.LLFB309:
	save	%sp, -96, %sp
.LLCFI309:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL666
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL665
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL666
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL665
	 nop
.LL666:
	mov	0, %g1
.LL665:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE309:
	.size	LE__LE_var_i64_I, .-LE__LE_var_i64_I
	.align 4
	.type	LE__uLE_param_i64_I, #function
	.proc	014
LE__uLE_param_i64_I:
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
	cmp	%o2, %o4
	bgu	%icc, .LL669
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL668
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL669
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL668
	 nop
.LL669:
	mov	0, %g1
.LL668:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE310:
	.size	LE__uLE_param_i64_I, .-LE__uLE_param_i64_I
	.align 4
	.type	LE__LE_param_i64_I, #function
	.proc	014
LE__LE_param_i64_I:
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
	bg	%icc, .LL672
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL671
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL672
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
	.size	LE__LE_param_i64_I, .-LE__LE_param_i64_I
	.align 4
	.type	LE__uLE_var_i64_i64, #function
	.proc	014
LE__uLE_var_i64_i64:
.LLFB312:
	save	%sp, -96, %sp
.LLCFI312:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE312:
	.size	LE__uLE_var_i64_i64, .-LE__uLE_var_i64_i64
	.align 4
	.type	LE__LE_var_i64_i64, #function
	.proc	014
LE__LE_var_i64_i64:
.LLFB313:
	save	%sp, -96, %sp
.LLCFI313:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE313:
	.size	LE__LE_var_i64_i64, .-LE__LE_var_i64_i64
	.align 4
	.type	LE__uLE_param_i64_i64, #function
	.proc	014
LE__uLE_param_i64_i64:
.LLFB314:
	save	%sp, -112, %sp
.LLCFI314:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
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
	.size	LE__uLE_param_i64_i64, .-LE__uLE_param_i64_i64
	.align 4
	.type	LE__LE_param_i64_i64, #function
	.proc	014
LE__LE_param_i64_i64:
.LLFB315:
	save	%sp, -112, %sp
.LLCFI315:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	mov	1, %g1
	ld	[%fp-8], %o4
	ld	[%fp-16], %o5
	cmp	%o4, %o5
	bg	%icc, .LL680
	 nop
	ld	[%fp-8], %o4
	ld	[%fp-16], %o5
	cmp	%o4, %o5
	bne	%icc, .LL679
	 nop
	ld	[%fp-4], %o4
	ld	[%fp-12], %o5
	cmp	%o4, %o5
	bgu	%icc, .LL680
	 nop
	ld	[%fp-4], %o4
	ld	[%fp-12], %o5
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
	.size	LE__LE_param_i64_i64, .-LE__LE_param_i64_i64
	.align 4
	.type	LE__uLE_var_i64_C, #function
	.proc	014
LE__uLE_var_i64_C:
.LLFB316:
	save	%sp, -96, %sp
.LLCFI316:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL683
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL682
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL683
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL682
	 nop
.LL683:
	mov	0, %g1
.LL682:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE316:
	.size	LE__uLE_var_i64_C, .-LE__uLE_var_i64_C
	.align 4
	.type	LE__LE_var_i64_C, #function
	.proc	014
LE__LE_var_i64_C:
.LLFB317:
	save	%sp, -96, %sp
.LLCFI317:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL686
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL685
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL686
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL685
	 nop
.LL686:
	mov	0, %g1
.LL685:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE317:
	.size	LE__LE_var_i64_C, .-LE__LE_var_i64_C
	.align 4
	.type	LE__uLE_param_i64_C, #function
	.proc	014
LE__uLE_param_i64_C:
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
	cmp	%o2, %o4
	bgu	%icc, .LL689
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL688
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL689
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL688
	 nop
.LL689:
	mov	0, %g1
.LL688:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE318:
	.size	LE__uLE_param_i64_C, .-LE__uLE_param_i64_C
	.align 4
	.type	LE__LE_param_i64_C, #function
	.proc	014
LE__LE_param_i64_C:
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
	bg	%icc, .LL692
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL691
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL692
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
	.size	LE__LE_param_i64_C, .-LE__LE_param_i64_C
	.align 4
	.type	LE__uLE_var_i64_u8, #function
	.proc	014
LE__uLE_var_i64_u8:
.LLFB320:
	save	%sp, -96, %sp
.LLCFI320:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o2
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
	.size	LE__uLE_var_i64_u8, .-LE__uLE_var_i64_u8
	.align 4
	.type	LE__LE_var_i64_u8, #function
	.proc	014
LE__LE_var_i64_u8:
.LLFB321:
	save	%sp, -96, %sp
.LLCFI321:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o2
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
	.size	LE__LE_var_i64_u8, .-LE__LE_var_i64_u8
	.align 4
	.type	LE__uLE_param_i64_u8, #function
	.proc	014
LE__uLE_param_i64_u8:
.LLFB322:
	save	%sp, -104, %sp
.LLCFI322:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	ldd	[%fp-8], %o2
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
	.size	LE__uLE_param_i64_u8, .-LE__uLE_param_i64_u8
	.align 4
	.type	LE__LE_param_i64_u8, #function
	.proc	014
LE__LE_param_i64_u8:
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
	bg	%icc, .LL704
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL703
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL704
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
	.size	LE__LE_param_i64_u8, .-LE__LE_param_i64_u8
	.align 4
	.type	LE__uLE_var_i64_L, #function
	.proc	014
LE__uLE_var_i64_L:
.LLFB324:
	save	%sp, -96, %sp
.LLCFI324:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o4
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
	.size	LE__uLE_var_i64_L, .-LE__uLE_var_i64_L
	.align 4
	.type	LE__LE_var_i64_L, #function
	.proc	014
LE__LE_var_i64_L:
.LLFB325:
	save	%sp, -96, %sp
.LLCFI325:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o2
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o4
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
	.size	LE__LE_var_i64_L, .-LE__LE_var_i64_L
	.align 4
	.type	LE__uLE_param_i64_L, #function
	.proc	014
LE__uLE_param_i64_L:
.LLFB326:
	save	%sp, -112, %sp
.LLCFI326:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
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
	.size	LE__uLE_param_i64_L, .-LE__uLE_param_i64_L
	.align 4
	.type	LE__LE_param_i64_L, #function
	.proc	014
LE__LE_param_i64_L:
.LLFB327:
	save	%sp, -112, %sp
.LLCFI327:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	mov	1, %g1
	ld	[%fp-8], %o4
	ld	[%fp-16], %o5
	cmp	%o4, %o5
	bg	%icc, .LL716
	 nop
	ld	[%fp-8], %o4
	ld	[%fp-16], %o5
	cmp	%o4, %o5
	bne	%icc, .LL715
	 nop
	ld	[%fp-4], %o4
	ld	[%fp-12], %o5
	cmp	%o4, %o5
	bgu	%icc, .LL716
	 nop
	ld	[%fp-4], %o4
	ld	[%fp-12], %o5
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
	.size	LE__LE_param_i64_L, .-LE__LE_param_i64_L
	.align 4
	.type	LE__uLE_var_i64_i8, #function
	.proc	014
LE__uLE_var_i64_i8:
.LLFB328:
	save	%sp, -96, %sp
.LLCFI328:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL719
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL718
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL719
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL718
	 nop
.LL719:
	mov	0, %g1
.LL718:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE328:
	.size	LE__uLE_var_i64_i8, .-LE__uLE_var_i64_i8
	.align 4
	.type	LE__LE_var_i64_i8, #function
	.proc	014
LE__LE_var_i64_i8:
.LLFB329:
	save	%sp, -96, %sp
.LLCFI329:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL722
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL721
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL722
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL721
	 nop
.LL722:
	mov	0, %g1
.LL721:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE329:
	.size	LE__LE_var_i64_i8, .-LE__LE_var_i64_i8
	.align 4
	.type	LE__uLE_param_i64_i8, #function
	.proc	014
LE__uLE_param_i64_i8:
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
	cmp	%o2, %o4
	bgu	%icc, .LL725
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL724
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL725
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL724
	 nop
.LL725:
	mov	0, %g1
.LL724:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE330:
	.size	LE__uLE_param_i64_i8, .-LE__uLE_param_i64_i8
	.align 4
	.type	LE__LE_param_i64_i8, #function
	.proc	014
LE__LE_param_i64_i8:
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
	bg	%icc, .LL728
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL727
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL728
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
	.size	LE__LE_param_i64_i8, .-LE__LE_param_i64_i8
	.align 4
	.type	LE__uLE_var_i64_u32, #function
	.proc	014
LE__uLE_var_i64_u32:
.LLFB332:
	save	%sp, -96, %sp
.LLCFI332:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL731
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL730
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL731
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL730
	 nop
.LL731:
	mov	0, %g1
.LL730:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE332:
	.size	LE__uLE_var_i64_u32, .-LE__uLE_var_i64_u32
	.align 4
	.type	LE__LE_var_i64_u32, #function
	.proc	014
LE__LE_var_i64_u32:
.LLFB333:
	save	%sp, -96, %sp
.LLCFI333:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL734
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL733
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL734
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL733
	 nop
.LL734:
	mov	0, %g1
.LL733:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE333:
	.size	LE__LE_var_i64_u32, .-LE__LE_var_i64_u32
	.align 4
	.type	LE__uLE_param_i64_u32, #function
	.proc	014
LE__uLE_param_i64_u32:
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
	cmp	%o2, %o4
	bgu	%icc, .LL737
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL736
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL737
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL736
	 nop
.LL737:
	mov	0, %g1
.LL736:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE334:
	.size	LE__uLE_param_i64_u32, .-LE__uLE_param_i64_u32
	.align 4
	.type	LE__LE_param_i64_u32, #function
	.proc	014
LE__LE_param_i64_u32:
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
	bg	%icc, .LL740
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL739
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL740
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
	.size	LE__LE_param_i64_u32, .-LE__LE_param_i64_u32
	.align 4
	.type	LE__uLE_var_C_u16, #function
	.proc	014
LE__uLE_var_C_u16:
.LLFB336:
	save	%sp, -96, %sp
.LLCFI336:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE336:
	.size	LE__uLE_var_C_u16, .-LE__uLE_var_C_u16
	.align 4
	.type	LE__LE_var_C_u16, #function
	.proc	014
LE__LE_var_C_u16:
.LLFB337:
	save	%sp, -96, %sp
.LLCFI337:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE337:
	.size	LE__LE_var_C_u16, .-LE__LE_var_C_u16
	.align 4
	.type	LE__uLE_param_C_u16, #function
	.proc	014
LE__uLE_param_C_u16:
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
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE338:
	.size	LE__uLE_param_C_u16, .-LE__uLE_param_C_u16
	.align 4
	.type	LE__LE_param_C_u16, #function
	.proc	014
LE__LE_param_C_u16:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE339:
	.size	LE__LE_param_C_u16, .-LE__LE_param_C_u16
	.align 4
	.type	LE__uLE_var_C_u64, #function
	.proc	014
LE__uLE_var_C_u64:
.LLFB340:
	save	%sp, -96, %sp
.LLCFI340:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL747
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL746
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL747
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL746
	 nop
.LL747:
	mov	0, %g1
.LL746:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE340:
	.size	LE__uLE_var_C_u64, .-LE__uLE_var_C_u64
	.align 4
	.type	LE__LE_var_C_u64, #function
	.proc	014
LE__LE_var_C_u64:
.LLFB341:
	save	%sp, -96, %sp
.LLCFI341:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL750
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL749
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL750
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL749
	 nop
.LL750:
	mov	0, %g1
.LL749:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE341:
	.size	LE__LE_var_C_u64, .-LE__LE_var_C_u64
	.align 4
	.type	LE__uLE_param_C_u64, #function
	.proc	014
LE__uLE_param_C_u64:
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
	cmp	%o4, %o2
	bgu	%icc, .LL753
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL752
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL753
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL752
	 nop
.LL753:
	mov	0, %g1
.LL752:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE342:
	.size	LE__uLE_param_C_u64, .-LE__uLE_param_C_u64
	.align 4
	.type	LE__LE_param_C_u64, #function
	.proc	014
LE__LE_param_C_u64:
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
	bl	%icc, .LL756
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL755
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL756
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
	.size	LE__LE_param_C_u64, .-LE__LE_param_C_u64
	.align 4
	.type	LE__uLE_var_C_LC, #function
	.proc	014
LE__uLE_var_C_LC:
.LLFB344:
	save	%sp, -96, %sp
.LLCFI344:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL759
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL758
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL759
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL758
	 nop
.LL759:
	mov	0, %g1
.LL758:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE344:
	.size	LE__uLE_var_C_LC, .-LE__uLE_var_C_LC
	.align 4
	.type	LE__LE_var_C_LC, #function
	.proc	014
LE__LE_var_C_LC:
.LLFB345:
	save	%sp, -96, %sp
.LLCFI345:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL762
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL761
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL762
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL761
	 nop
.LL762:
	mov	0, %g1
.LL761:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE345:
	.size	LE__LE_var_C_LC, .-LE__LE_var_C_LC
	.align 4
	.type	LE__uLE_param_C_LC, #function
	.proc	014
LE__uLE_param_C_LC:
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
	blu	%icc, .LL765
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL764
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL765
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL764
	 nop
.LL765:
	mov	0, %g1
.LL764:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE346:
	.size	LE__uLE_param_C_LC, .-LE__uLE_param_C_LC
	.align 4
	.type	LE__LE_param_C_LC, #function
	.proc	014
LE__LE_param_C_LC:
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
	cmp	%o4, %o2
	bg	%icc, .LL768
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL767
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL768
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL767
	 nop
.LL768:
	mov	0, %g1
.LL767:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE347:
	.size	LE__LE_param_C_LC, .-LE__LE_param_C_LC
	.align 4
	.type	LE__uLE_var_C_i32, #function
	.proc	014
LE__uLE_var_C_i32:
.LLFB348:
	save	%sp, -96, %sp
.LLCFI348:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE348:
	.size	LE__uLE_var_C_i32, .-LE__uLE_var_C_i32
	.align 4
	.type	LE__LE_var_C_i32, #function
	.proc	014
LE__LE_var_C_i32:
.LLFB349:
	save	%sp, -96, %sp
.LLCFI349:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE349:
	.size	LE__LE_var_C_i32, .-LE__LE_var_C_i32
	.align 4
	.type	LE__uLE_param_C_i32, #function
	.proc	014
LE__uLE_param_C_i32:
.LLFB350:
	save	%sp, -96, %sp
.LLCFI350:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE350:
	.size	LE__uLE_param_C_i32, .-LE__uLE_param_C_i32
	.align 4
	.type	LE__LE_param_C_i32, #function
	.proc	014
LE__LE_param_C_i32:
.LLFB351:
	save	%sp, -96, %sp
.LLCFI351:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE351:
	.size	LE__LE_param_C_i32, .-LE__LE_param_C_i32
	.align 4
	.type	LE__uLE_var_C_i16, #function
	.proc	014
LE__uLE_var_C_i16:
.LLFB352:
	save	%sp, -96, %sp
.LLCFI352:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE352:
	.size	LE__uLE_var_C_i16, .-LE__uLE_var_C_i16
	.align 4
	.type	LE__LE_var_C_i16, #function
	.proc	014
LE__LE_var_C_i16:
.LLFB353:
	save	%sp, -96, %sp
.LLCFI353:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE353:
	.size	LE__LE_var_C_i16, .-LE__LE_var_C_i16
	.align 4
	.type	LE__uLE_param_C_i16, #function
	.proc	014
LE__uLE_param_C_i16:
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
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE354:
	.size	LE__uLE_param_C_i16, .-LE__uLE_param_C_i16
	.align 4
	.type	LE__LE_param_C_i16, #function
	.proc	014
LE__LE_param_C_i16:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE355:
	.size	LE__LE_param_C_i16, .-LE__LE_param_C_i16
	.align 4
	.type	LE__uLE_var_C_I, #function
	.proc	014
LE__uLE_var_C_I:
.LLFB356:
	save	%sp, -96, %sp
.LLCFI356:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE356:
	.size	LE__uLE_var_C_I, .-LE__uLE_var_C_I
	.align 4
	.type	LE__LE_var_C_I, #function
	.proc	014
LE__LE_var_C_I:
.LLFB357:
	save	%sp, -96, %sp
.LLCFI357:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE357:
	.size	LE__LE_var_C_I, .-LE__LE_var_C_I
	.align 4
	.type	LE__uLE_param_C_I, #function
	.proc	014
LE__uLE_param_C_I:
.LLFB358:
	save	%sp, -96, %sp
.LLCFI358:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE358:
	.size	LE__uLE_param_C_I, .-LE__uLE_param_C_I
	.align 4
	.type	LE__LE_param_C_I, #function
	.proc	014
LE__LE_param_C_I:
.LLFB359:
	save	%sp, -96, %sp
.LLCFI359:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE359:
	.size	LE__LE_param_C_I, .-LE__LE_param_C_I
	.align 4
	.type	LE__uLE_var_C_i64, #function
	.proc	014
LE__uLE_var_C_i64:
.LLFB360:
	save	%sp, -96, %sp
.LLCFI360:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL783
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL782
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL783
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL782
	 nop
.LL783:
	mov	0, %g1
.LL782:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE360:
	.size	LE__uLE_var_C_i64, .-LE__uLE_var_C_i64
	.align 4
	.type	LE__LE_var_C_i64, #function
	.proc	014
LE__LE_var_C_i64:
.LLFB361:
	save	%sp, -96, %sp
.LLCFI361:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL786
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL785
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL786
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL785
	 nop
.LL786:
	mov	0, %g1
.LL785:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE361:
	.size	LE__LE_var_C_i64, .-LE__LE_var_C_i64
	.align 4
	.type	LE__uLE_param_C_i64, #function
	.proc	014
LE__uLE_param_C_i64:
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
	cmp	%o4, %o2
	bgu	%icc, .LL789
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL788
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL789
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL788
	 nop
.LL789:
	mov	0, %g1
.LL788:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE362:
	.size	LE__uLE_param_C_i64, .-LE__uLE_param_C_i64
	.align 4
	.type	LE__LE_param_C_i64, #function
	.proc	014
LE__LE_param_C_i64:
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
	bl	%icc, .LL792
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL791
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL792
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL791
	 nop
.LL792:
	mov	0, %g1
.LL791:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE363:
	.size	LE__LE_param_C_i64, .-LE__LE_param_C_i64
	.align 4
	.type	LE__uLE_var_C_C, #function
	.proc	014
LE__uLE_var_C_C:
.LLFB364:
	save	%sp, -96, %sp
.LLCFI364:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE364:
	.size	LE__uLE_var_C_C, .-LE__uLE_var_C_C
	.align 4
	.type	LE__LE_var_C_C, #function
	.proc	014
LE__LE_var_C_C:
.LLFB365:
	save	%sp, -96, %sp
.LLCFI365:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE365:
	.size	LE__LE_var_C_C, .-LE__LE_var_C_C
	.align 4
	.type	LE__uLE_param_C_C, #function
	.proc	014
LE__uLE_param_C_C:
.LLFB366:
	save	%sp, -96, %sp
.LLCFI366:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE366:
	.size	LE__uLE_param_C_C, .-LE__uLE_param_C_C
	.align 4
	.type	LE__LE_param_C_C, #function
	.proc	014
LE__LE_param_C_C:
.LLFB367:
	save	%sp, -96, %sp
.LLCFI367:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE367:
	.size	LE__LE_param_C_C, .-LE__LE_param_C_C
	.align 4
	.type	LE__uLE_var_C_u8, #function
	.proc	014
LE__uLE_var_C_u8:
.LLFB368:
	save	%sp, -96, %sp
.LLCFI368:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE368:
	.size	LE__uLE_var_C_u8, .-LE__uLE_var_C_u8
	.align 4
	.type	LE__LE_var_C_u8, #function
	.proc	014
LE__LE_var_C_u8:
.LLFB369:
	save	%sp, -96, %sp
.LLCFI369:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE369:
	.size	LE__LE_var_C_u8, .-LE__LE_var_C_u8
	.align 4
	.type	LE__uLE_param_C_u8, #function
	.proc	014
LE__uLE_param_C_u8:
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
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE370:
	.size	LE__uLE_param_C_u8, .-LE__uLE_param_C_u8
	.align 4
	.type	LE__LE_param_C_u8, #function
	.proc	014
LE__LE_param_C_u8:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE371:
	.size	LE__LE_param_C_u8, .-LE__LE_param_C_u8
	.align 4
	.type	LE__uLE_var_C_L, #function
	.proc	014
LE__uLE_var_C_L:
.LLFB372:
	save	%sp, -96, %sp
.LLCFI372:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL803
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL802
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL803
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL802
	 nop
.LL803:
	mov	0, %g1
.LL802:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE372:
	.size	LE__uLE_var_C_L, .-LE__uLE_var_C_L
	.align 4
	.type	LE__LE_var_C_L, #function
	.proc	014
LE__LE_var_C_L:
.LLFB373:
	save	%sp, -96, %sp
.LLCFI373:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL806
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL805
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL806
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL805
	 nop
.LL806:
	mov	0, %g1
.LL805:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE373:
	.size	LE__LE_var_C_L, .-LE__LE_var_C_L
	.align 4
	.type	LE__uLE_param_C_L, #function
	.proc	014
LE__uLE_param_C_L:
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
	cmp	%o4, %o2
	bgu	%icc, .LL809
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL808
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL809
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL808
	 nop
.LL809:
	mov	0, %g1
.LL808:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE374:
	.size	LE__uLE_param_C_L, .-LE__uLE_param_C_L
	.align 4
	.type	LE__LE_param_C_L, #function
	.proc	014
LE__LE_param_C_L:
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
	bl	%icc, .LL812
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL811
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL812
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL811
	 nop
.LL812:
	mov	0, %g1
.LL811:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE375:
	.size	LE__LE_param_C_L, .-LE__LE_param_C_L
	.align 4
	.type	LE__uLE_var_C_i8, #function
	.proc	014
LE__uLE_var_C_i8:
.LLFB376:
	save	%sp, -96, %sp
.LLCFI376:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE376:
	.size	LE__uLE_var_C_i8, .-LE__uLE_var_C_i8
	.align 4
	.type	LE__LE_var_C_i8, #function
	.proc	014
LE__LE_var_C_i8:
.LLFB377:
	save	%sp, -96, %sp
.LLCFI377:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE377:
	.size	LE__LE_var_C_i8, .-LE__LE_var_C_i8
	.align 4
	.type	LE__uLE_param_C_i8, #function
	.proc	014
LE__uLE_param_C_i8:
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
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE378:
	.size	LE__uLE_param_C_i8, .-LE__uLE_param_C_i8
	.align 4
	.type	LE__LE_param_C_i8, #function
	.proc	014
LE__LE_param_C_i8:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE379:
	.size	LE__LE_param_C_i8, .-LE__LE_param_C_i8
	.align 4
	.type	LE__uLE_var_C_u32, #function
	.proc	014
LE__uLE_var_C_u32:
.LLFB380:
	save	%sp, -96, %sp
.LLCFI380:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE380:
	.size	LE__uLE_var_C_u32, .-LE__uLE_var_C_u32
	.align 4
	.type	LE__LE_var_C_u32, #function
	.proc	014
LE__LE_var_C_u32:
.LLFB381:
	save	%sp, -96, %sp
.LLCFI381:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE381:
	.size	LE__LE_var_C_u32, .-LE__LE_var_C_u32
	.align 4
	.type	LE__uLE_param_C_u32, #function
	.proc	014
LE__uLE_param_C_u32:
.LLFB382:
	save	%sp, -96, %sp
.LLCFI382:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movgeu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE382:
	.size	LE__uLE_param_C_u32, .-LE__uLE_param_C_u32
	.align 4
	.type	LE__LE_param_C_u32, #function
	.proc	014
LE__LE_param_C_u32:
.LLFB383:
	save	%sp, -96, %sp
.LLCFI383:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE383:
	.size	LE__LE_param_C_u32, .-LE__LE_param_C_u32
	.align 4
	.type	LE__LE_var_f64_f64, #function
	.proc	014
LE__LE_var_f64_f64:
.LLFB384:
	save	%sp, -96, %sp
.LLCFI384:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	add	%g1, 104, %g1
	ldd	[%g1], %f10
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	add	%g1, 104, %g1
	ldd	[%g1], %f8
	fcmpd	%fcc0, %f10, %f8
	mov	0, %g1
	move	%fcc0, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE384:
	.size	LE__LE_var_f64_f64, .-LE__LE_var_f64_f64
	.align 4
	.type	LE__LE_param_f64_f64, #function
	.proc	014
LE__LE_param_f64_f64:
.LLFB385:
	save	%sp, -112, %sp
.LLCFI385:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %f10
	ldd	[%fp-16], %f8
	fcmped	%fcc1, %f10, %f8
	mov	0, %g1
	movle	%fcc1, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE385:
	.size	LE__LE_param_f64_f64, .-LE__LE_param_f64_f64
	.align 4
	.type	LE__uLE_var_u8_u16, #function
	.proc	014
LE__uLE_var_u8_u16:
.LLFB386:
	save	%sp, -96, %sp
.LLCFI386:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE386:
	.size	LE__uLE_var_u8_u16, .-LE__uLE_var_u8_u16
	.align 4
	.type	LE__LE_var_u8_u16, #function
	.proc	014
LE__LE_var_u8_u16:
.LLFB387:
	save	%sp, -96, %sp
.LLCFI387:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE387:
	.size	LE__LE_var_u8_u16, .-LE__LE_var_u8_u16
	.align 4
	.type	LE__uLE_param_u8_u16, #function
	.proc	014
LE__uLE_param_u8_u16:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE388:
	.size	LE__uLE_param_u8_u16, .-LE__uLE_param_u8_u16
	.align 4
	.type	LE__LE_param_u8_u16, #function
	.proc	014
LE__LE_param_u8_u16:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE389:
	.size	LE__LE_param_u8_u16, .-LE__LE_param_u8_u16
	.align 4
	.type	LE__uLE_var_u8_u64, #function
	.proc	014
LE__uLE_var_u8_u64:
.LLFB390:
	save	%sp, -96, %sp
.LLCFI390:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL829
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL828
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL829
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL828
	 nop
.LL829:
	mov	0, %g1
.LL828:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE390:
	.size	LE__uLE_var_u8_u64, .-LE__uLE_var_u8_u64
	.align 4
	.type	LE__LE_var_u8_u64, #function
	.proc	014
LE__LE_var_u8_u64:
.LLFB391:
	save	%sp, -96, %sp
.LLCFI391:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL832
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL831
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL832
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL831
	 nop
.LL832:
	mov	0, %g1
.LL831:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE391:
	.size	LE__LE_var_u8_u64, .-LE__LE_var_u8_u64
	.align 4
	.type	LE__uLE_param_u8_u64, #function
	.proc	014
LE__uLE_param_u8_u64:
.LLFB392:
	save	%sp, -104, %sp
.LLCFI392:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL835
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL834
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL835
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL834
	 nop
.LL835:
	mov	0, %g1
.LL834:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE392:
	.size	LE__uLE_param_u8_u64, .-LE__uLE_param_u8_u64
	.align 4
	.type	LE__LE_param_u8_u64, #function
	.proc	014
LE__LE_param_u8_u64:
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
	bl	%icc, .LL838
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL837
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL838
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL837
	 nop
.LL838:
	mov	0, %g1
.LL837:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE393:
	.size	LE__LE_param_u8_u64, .-LE__LE_param_u8_u64
	.align 4
	.type	LE__uLE_var_u8_LC, #function
	.proc	014
LE__uLE_var_u8_LC:
.LLFB394:
	save	%sp, -96, %sp
.LLCFI394:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL841
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL840
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL841
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL840
	 nop
.LL841:
	mov	0, %g1
.LL840:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE394:
	.size	LE__uLE_var_u8_LC, .-LE__uLE_var_u8_LC
	.align 4
	.type	LE__LE_var_u8_LC, #function
	.proc	014
LE__LE_var_u8_LC:
.LLFB395:
	save	%sp, -96, %sp
.LLCFI395:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL844
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL843
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL844
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL843
	 nop
.LL844:
	mov	0, %g1
.LL843:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE395:
	.size	LE__LE_var_u8_LC, .-LE__LE_var_u8_LC
	.align 4
	.type	LE__uLE_param_u8_LC, #function
	.proc	014
LE__uLE_param_u8_LC:
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
	blu	%icc, .LL847
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL846
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL847
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL846
	 nop
.LL847:
	mov	0, %g1
.LL846:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE396:
	.size	LE__uLE_param_u8_LC, .-LE__uLE_param_u8_LC
	.align 4
	.type	LE__LE_param_u8_LC, #function
	.proc	014
LE__LE_param_u8_LC:
.LLFB397:
	save	%sp, -104, %sp
.LLCFI397:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL850
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL849
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL850
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL849
	 nop
.LL850:
	mov	0, %g1
.LL849:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE397:
	.size	LE__LE_param_u8_LC, .-LE__LE_param_u8_LC
	.align 4
	.type	LE__uLE_var_u8_i32, #function
	.proc	014
LE__uLE_var_u8_i32:
.LLFB398:
	save	%sp, -96, %sp
.LLCFI398:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE398:
	.size	LE__uLE_var_u8_i32, .-LE__uLE_var_u8_i32
	.align 4
	.type	LE__LE_var_u8_i32, #function
	.proc	014
LE__LE_var_u8_i32:
.LLFB399:
	save	%sp, -96, %sp
.LLCFI399:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE399:
	.size	LE__LE_var_u8_i32, .-LE__LE_var_u8_i32
	.align 4
	.type	LE__uLE_param_u8_i32, #function
	.proc	014
LE__uLE_param_u8_i32:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE400:
	.size	LE__uLE_param_u8_i32, .-LE__uLE_param_u8_i32
	.align 4
	.type	LE__LE_param_u8_i32, #function
	.proc	014
LE__LE_param_u8_i32:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE401:
	.size	LE__LE_param_u8_i32, .-LE__LE_param_u8_i32
	.align 4
	.type	LE__uLE_var_u8_i16, #function
	.proc	014
LE__uLE_var_u8_i16:
.LLFB402:
	save	%sp, -96, %sp
.LLCFI402:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE402:
	.size	LE__uLE_var_u8_i16, .-LE__uLE_var_u8_i16
	.align 4
	.type	LE__LE_var_u8_i16, #function
	.proc	014
LE__LE_var_u8_i16:
.LLFB403:
	save	%sp, -96, %sp
.LLCFI403:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE403:
	.size	LE__LE_var_u8_i16, .-LE__LE_var_u8_i16
	.align 4
	.type	LE__uLE_param_u8_i16, #function
	.proc	014
LE__uLE_param_u8_i16:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE404:
	.size	LE__uLE_param_u8_i16, .-LE__uLE_param_u8_i16
	.align 4
	.type	LE__LE_param_u8_i16, #function
	.proc	014
LE__LE_param_u8_i16:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE405:
	.size	LE__LE_param_u8_i16, .-LE__LE_param_u8_i16
	.align 4
	.type	LE__uLE_var_u8_I, #function
	.proc	014
LE__uLE_var_u8_I:
.LLFB406:
	save	%sp, -96, %sp
.LLCFI406:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE406:
	.size	LE__uLE_var_u8_I, .-LE__uLE_var_u8_I
	.align 4
	.type	LE__LE_var_u8_I, #function
	.proc	014
LE__LE_var_u8_I:
.LLFB407:
	save	%sp, -96, %sp
.LLCFI407:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE407:
	.size	LE__LE_var_u8_I, .-LE__LE_var_u8_I
	.align 4
	.type	LE__uLE_param_u8_I, #function
	.proc	014
LE__uLE_param_u8_I:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE408:
	.size	LE__uLE_param_u8_I, .-LE__uLE_param_u8_I
	.align 4
	.type	LE__LE_param_u8_I, #function
	.proc	014
LE__LE_param_u8_I:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE409:
	.size	LE__LE_param_u8_I, .-LE__LE_param_u8_I
	.align 4
	.type	LE__uLE_var_u8_i64, #function
	.proc	014
LE__uLE_var_u8_i64:
.LLFB410:
	save	%sp, -96, %sp
.LLCFI410:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL865
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL864
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL865
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL864
	 nop
.LL865:
	mov	0, %g1
.LL864:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE410:
	.size	LE__uLE_var_u8_i64, .-LE__uLE_var_u8_i64
	.align 4
	.type	LE__LE_var_u8_i64, #function
	.proc	014
LE__LE_var_u8_i64:
.LLFB411:
	save	%sp, -96, %sp
.LLCFI411:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL868
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL867
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL868
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL867
	 nop
.LL868:
	mov	0, %g1
.LL867:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE411:
	.size	LE__LE_var_u8_i64, .-LE__LE_var_u8_i64
	.align 4
	.type	LE__uLE_param_u8_i64, #function
	.proc	014
LE__uLE_param_u8_i64:
.LLFB412:
	save	%sp, -104, %sp
.LLCFI412:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL871
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL870
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL871
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL870
	 nop
.LL871:
	mov	0, %g1
.LL870:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE412:
	.size	LE__uLE_param_u8_i64, .-LE__uLE_param_u8_i64
	.align 4
	.type	LE__LE_param_u8_i64, #function
	.proc	014
LE__LE_param_u8_i64:
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
	bl	%icc, .LL874
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL873
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL874
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL873
	 nop
.LL874:
	mov	0, %g1
.LL873:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE413:
	.size	LE__LE_param_u8_i64, .-LE__LE_param_u8_i64
	.align 4
	.type	LE__uLE_var_u8_C, #function
	.proc	014
LE__uLE_var_u8_C:
.LLFB414:
	save	%sp, -96, %sp
.LLCFI414:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE414:
	.size	LE__uLE_var_u8_C, .-LE__uLE_var_u8_C
	.align 4
	.type	LE__LE_var_u8_C, #function
	.proc	014
LE__LE_var_u8_C:
.LLFB415:
	save	%sp, -96, %sp
.LLCFI415:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE415:
	.size	LE__LE_var_u8_C, .-LE__LE_var_u8_C
	.align 4
	.type	LE__uLE_param_u8_C, #function
	.proc	014
LE__uLE_param_u8_C:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE416:
	.size	LE__uLE_param_u8_C, .-LE__uLE_param_u8_C
	.align 4
	.type	LE__LE_param_u8_C, #function
	.proc	014
LE__LE_param_u8_C:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE417:
	.size	LE__LE_param_u8_C, .-LE__LE_param_u8_C
	.align 4
	.type	LE__uLE_var_u8_u8, #function
	.proc	014
LE__uLE_var_u8_u8:
.LLFB418:
	save	%sp, -96, %sp
.LLCFI418:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE418:
	.size	LE__uLE_var_u8_u8, .-LE__uLE_var_u8_u8
	.align 4
	.type	LE__LE_var_u8_u8, #function
	.proc	014
LE__LE_var_u8_u8:
.LLFB419:
	save	%sp, -96, %sp
.LLCFI419:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE419:
	.size	LE__LE_var_u8_u8, .-LE__LE_var_u8_u8
	.align 4
	.type	LE__uLE_param_u8_u8, #function
	.proc	014
LE__uLE_param_u8_u8:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE420:
	.size	LE__uLE_param_u8_u8, .-LE__uLE_param_u8_u8
	.align 4
	.type	LE__LE_param_u8_u8, #function
	.proc	014
LE__LE_param_u8_u8:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE421:
	.size	LE__LE_param_u8_u8, .-LE__LE_param_u8_u8
	.align 4
	.type	LE__uLE_var_u8_L, #function
	.proc	014
LE__uLE_var_u8_L:
.LLFB422:
	save	%sp, -96, %sp
.LLCFI422:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL885
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL884
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL885
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL884
	 nop
.LL885:
	mov	0, %g1
.LL884:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE422:
	.size	LE__uLE_var_u8_L, .-LE__uLE_var_u8_L
	.align 4
	.type	LE__LE_var_u8_L, #function
	.proc	014
LE__LE_var_u8_L:
.LLFB423:
	save	%sp, -96, %sp
.LLCFI423:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL888
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL887
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL888
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL887
	 nop
.LL888:
	mov	0, %g1
.LL887:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE423:
	.size	LE__LE_var_u8_L, .-LE__LE_var_u8_L
	.align 4
	.type	LE__uLE_param_u8_L, #function
	.proc	014
LE__uLE_param_u8_L:
.LLFB424:
	save	%sp, -104, %sp
.LLCFI424:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL891
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL890
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL891
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL890
	 nop
.LL891:
	mov	0, %g1
.LL890:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE424:
	.size	LE__uLE_param_u8_L, .-LE__uLE_param_u8_L
	.align 4
	.type	LE__LE_param_u8_L, #function
	.proc	014
LE__LE_param_u8_L:
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
	bl	%icc, .LL894
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL893
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL894
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL893
	 nop
.LL894:
	mov	0, %g1
.LL893:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE425:
	.size	LE__LE_param_u8_L, .-LE__LE_param_u8_L
	.align 4
	.type	LE__uLE_var_u8_i8, #function
	.proc	014
LE__uLE_var_u8_i8:
.LLFB426:
	save	%sp, -96, %sp
.LLCFI426:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE426:
	.size	LE__uLE_var_u8_i8, .-LE__uLE_var_u8_i8
	.align 4
	.type	LE__LE_var_u8_i8, #function
	.proc	014
LE__LE_var_u8_i8:
.LLFB427:
	save	%sp, -96, %sp
.LLCFI427:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE427:
	.size	LE__LE_var_u8_i8, .-LE__LE_var_u8_i8
	.align 4
	.type	LE__uLE_param_u8_i8, #function
	.proc	014
LE__uLE_param_u8_i8:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE428:
	.size	LE__uLE_param_u8_i8, .-LE__uLE_param_u8_i8
	.align 4
	.type	LE__LE_param_u8_i8, #function
	.proc	014
LE__LE_param_u8_i8:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE429:
	.size	LE__LE_param_u8_i8, .-LE__LE_param_u8_i8
	.align 4
	.type	LE__uLE_var_u8_u32, #function
	.proc	014
LE__uLE_var_u8_u32:
.LLFB430:
	save	%sp, -96, %sp
.LLCFI430:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE430:
	.size	LE__uLE_var_u8_u32, .-LE__uLE_var_u8_u32
	.align 4
	.type	LE__LE_var_u8_u32, #function
	.proc	014
LE__LE_var_u8_u32:
.LLFB431:
	save	%sp, -96, %sp
.LLCFI431:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE431:
	.size	LE__LE_var_u8_u32, .-LE__LE_var_u8_u32
	.align 4
	.type	LE__uLE_param_u8_u32, #function
	.proc	014
LE__uLE_param_u8_u32:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE432:
	.size	LE__uLE_param_u8_u32, .-LE__uLE_param_u8_u32
	.align 4
	.type	LE__LE_param_u8_u32, #function
	.proc	014
LE__LE_param_u8_u32:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE433:
	.size	LE__LE_param_u8_u32, .-LE__LE_param_u8_u32
	.align 4
	.type	LE__uLE_var_L_u16, #function
	.proc	014
LE__uLE_var_L_u16:
.LLFB434:
	save	%sp, -96, %sp
.LLCFI434:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL905
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL904
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL905
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL904
	 nop
.LL905:
	mov	0, %g1
.LL904:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE434:
	.size	LE__uLE_var_L_u16, .-LE__uLE_var_L_u16
	.align 4
	.type	LE__LE_var_L_u16, #function
	.proc	014
LE__LE_var_L_u16:
.LLFB435:
	save	%sp, -96, %sp
.LLCFI435:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL908
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL907
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL908
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL907
	 nop
.LL908:
	mov	0, %g1
.LL907:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE435:
	.size	LE__LE_var_L_u16, .-LE__LE_var_L_u16
	.align 4
	.type	LE__uLE_param_L_u16, #function
	.proc	014
LE__uLE_param_L_u16:
.LLFB436:
	save	%sp, -104, %sp
.LLCFI436:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL911
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL910
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL911
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL910
	 nop
.LL911:
	mov	0, %g1
.LL910:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE436:
	.size	LE__uLE_param_L_u16, .-LE__uLE_param_L_u16
	.align 4
	.type	LE__LE_param_L_u16, #function
	.proc	014
LE__LE_param_L_u16:
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
	bg	%icc, .LL914
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL913
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL914
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL913
	 nop
.LL914:
	mov	0, %g1
.LL913:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE437:
	.size	LE__LE_param_L_u16, .-LE__LE_param_L_u16
	.align 4
	.type	LE__uLE_var_L_u64, #function
	.proc	014
LE__uLE_var_L_u64:
.LLFB438:
	save	%sp, -96, %sp
.LLCFI438:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL917
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL916
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL917
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL916
	 nop
.LL917:
	mov	0, %g1
.LL916:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE438:
	.size	LE__uLE_var_L_u64, .-LE__uLE_var_L_u64
	.align 4
	.type	LE__LE_var_L_u64, #function
	.proc	014
LE__LE_var_L_u64:
.LLFB439:
	save	%sp, -96, %sp
.LLCFI439:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o2
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL920
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL919
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL920
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL919
	 nop
.LL920:
	mov	0, %g1
.LL919:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE439:
	.size	LE__LE_var_L_u64, .-LE__LE_var_L_u64
	.align 4
	.type	LE__uLE_param_L_u64, #function
	.proc	014
LE__uLE_param_L_u64:
.LLFB440:
	save	%sp, -112, %sp
.LLCFI440:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL923
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL922
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL923
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL922
	 nop
.LL923:
	mov	0, %g1
.LL922:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE440:
	.size	LE__uLE_param_L_u64, .-LE__uLE_param_L_u64
	.align 4
	.type	LE__LE_param_L_u64, #function
	.proc	014
LE__LE_param_L_u64:
.LLFB441:
	save	%sp, -112, %sp
.LLCFI441:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	mov	1, %g1
	ld	[%fp-8], %o4
	ld	[%fp-16], %o5
	cmp	%o4, %o5
	bg	%icc, .LL926
	 nop
	ld	[%fp-8], %o4
	ld	[%fp-16], %o5
	cmp	%o4, %o5
	bne	%icc, .LL925
	 nop
	ld	[%fp-4], %o4
	ld	[%fp-12], %o5
	cmp	%o4, %o5
	bgu	%icc, .LL926
	 nop
	ld	[%fp-4], %o4
	ld	[%fp-12], %o5
	cmp	%o4, %o5
	ba,pt	%xcc, .LL925
	 nop
.LL926:
	mov	0, %g1
.LL925:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE441:
	.size	LE__LE_param_L_u64, .-LE__LE_param_L_u64
	.align 4
	.type	LE__uLE_var_L_LC, #function
	.proc	014
LE__uLE_var_L_LC:
.LLFB442:
	save	%sp, -96, %sp
.LLCFI442:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL929
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL928
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL929
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL928
	 nop
.LL929:
	mov	0, %g1
.LL928:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE442:
	.size	LE__uLE_var_L_LC, .-LE__uLE_var_L_LC
	.align 4
	.type	LE__LE_var_L_LC, #function
	.proc	014
LE__LE_var_L_LC:
.LLFB443:
	save	%sp, -96, %sp
.LLCFI443:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o2
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL932
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL931
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL932
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL931
	 nop
.LL932:
	mov	0, %g1
.LL931:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE443:
	.size	LE__LE_var_L_LC, .-LE__LE_var_L_LC
	.align 4
	.type	LE__uLE_param_L_LC, #function
	.proc	014
LE__uLE_param_L_LC:
.LLFB444:
	save	%sp, -112, %sp
.LLCFI444:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	mov	1, %g1
	ld	[%fp-16], %o3
	cmp	%o3, %o4
	blu	%icc, .LL935
	 nop
	ld	[%fp-16], %o3
	cmp	%o3, %o4
	bne	%icc, .LL934
	 nop
	ld	[%fp-12], %o3
	cmp	%o3, %o5
	blu	%icc, .LL935
	 nop
	ld	[%fp-12], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL934
	 nop
.LL935:
	mov	0, %g1
.LL934:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE444:
	.size	LE__uLE_param_L_LC, .-LE__uLE_param_L_LC
	.align 4
	.type	LE__LE_param_L_LC, #function
	.proc	014
LE__LE_param_L_LC:
.LLFB445:
	save	%sp, -112, %sp
.LLCFI445:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o4
	mov	1, %g1
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bg	%icc, .LL938
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL937
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL938
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL937
	 nop
.LL938:
	mov	0, %g1
.LL937:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE445:
	.size	LE__LE_param_L_LC, .-LE__LE_param_L_LC
	.align 4
	.type	LE__uLE_var_L_i32, #function
	.proc	014
LE__uLE_var_L_i32:
.LLFB446:
	save	%sp, -96, %sp
.LLCFI446:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL941
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL940
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL941
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL940
	 nop
.LL941:
	mov	0, %g1
.LL940:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE446:
	.size	LE__uLE_var_L_i32, .-LE__uLE_var_L_i32
	.align 4
	.type	LE__LE_var_L_i32, #function
	.proc	014
LE__LE_var_L_i32:
.LLFB447:
	save	%sp, -96, %sp
.LLCFI447:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL944
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL943
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL944
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL943
	 nop
.LL944:
	mov	0, %g1
.LL943:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE447:
	.size	LE__LE_var_L_i32, .-LE__LE_var_L_i32
	.align 4
	.type	LE__uLE_param_L_i32, #function
	.proc	014
LE__uLE_param_L_i32:
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
	cmp	%o2, %o4
	bgu	%icc, .LL947
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL946
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL947
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL946
	 nop
.LL947:
	mov	0, %g1
.LL946:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE448:
	.size	LE__uLE_param_L_i32, .-LE__uLE_param_L_i32
	.align 4
	.type	LE__LE_param_L_i32, #function
	.proc	014
LE__LE_param_L_i32:
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
	bg	%icc, .LL950
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL949
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL950
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL949
	 nop
.LL950:
	mov	0, %g1
.LL949:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE449:
	.size	LE__LE_param_L_i32, .-LE__LE_param_L_i32
	.align 4
	.type	LE__uLE_var_L_i16, #function
	.proc	014
LE__uLE_var_L_i16:
.LLFB450:
	save	%sp, -96, %sp
.LLCFI450:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL953
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL952
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL953
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL952
	 nop
.LL953:
	mov	0, %g1
.LL952:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE450:
	.size	LE__uLE_var_L_i16, .-LE__uLE_var_L_i16
	.align 4
	.type	LE__LE_var_L_i16, #function
	.proc	014
LE__LE_var_L_i16:
.LLFB451:
	save	%sp, -96, %sp
.LLCFI451:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL956
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL955
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL956
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL955
	 nop
.LL956:
	mov	0, %g1
.LL955:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE451:
	.size	LE__LE_var_L_i16, .-LE__LE_var_L_i16
	.align 4
	.type	LE__uLE_param_L_i16, #function
	.proc	014
LE__uLE_param_L_i16:
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
	cmp	%o2, %o4
	bgu	%icc, .LL959
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL958
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL959
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL958
	 nop
.LL959:
	mov	0, %g1
.LL958:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE452:
	.size	LE__uLE_param_L_i16, .-LE__uLE_param_L_i16
	.align 4
	.type	LE__LE_param_L_i16, #function
	.proc	014
LE__LE_param_L_i16:
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
	bg	%icc, .LL962
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL961
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL962
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL961
	 nop
.LL962:
	mov	0, %g1
.LL961:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE453:
	.size	LE__LE_param_L_i16, .-LE__LE_param_L_i16
	.align 4
	.type	LE__uLE_var_L_I, #function
	.proc	014
LE__uLE_var_L_I:
.LLFB454:
	save	%sp, -96, %sp
.LLCFI454:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL965
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL964
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL965
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL964
	 nop
.LL965:
	mov	0, %g1
.LL964:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE454:
	.size	LE__uLE_var_L_I, .-LE__uLE_var_L_I
	.align 4
	.type	LE__LE_var_L_I, #function
	.proc	014
LE__LE_var_L_I:
.LLFB455:
	save	%sp, -96, %sp
.LLCFI455:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL968
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL967
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL968
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL967
	 nop
.LL968:
	mov	0, %g1
.LL967:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE455:
	.size	LE__LE_var_L_I, .-LE__LE_var_L_I
	.align 4
	.type	LE__uLE_param_L_I, #function
	.proc	014
LE__uLE_param_L_I:
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
	cmp	%o2, %o4
	bgu	%icc, .LL971
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL970
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL971
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL970
	 nop
.LL971:
	mov	0, %g1
.LL970:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE456:
	.size	LE__uLE_param_L_I, .-LE__uLE_param_L_I
	.align 4
	.type	LE__LE_param_L_I, #function
	.proc	014
LE__LE_param_L_I:
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
	bg	%icc, .LL974
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL973
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL974
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL973
	 nop
.LL974:
	mov	0, %g1
.LL973:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE457:
	.size	LE__LE_param_L_I, .-LE__LE_param_L_I
	.align 4
	.type	LE__uLE_var_L_i64, #function
	.proc	014
LE__uLE_var_L_i64:
.LLFB458:
	save	%sp, -96, %sp
.LLCFI458:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL977
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL976
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL977
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL976
	 nop
.LL977:
	mov	0, %g1
.LL976:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE458:
	.size	LE__uLE_var_L_i64, .-LE__uLE_var_L_i64
	.align 4
	.type	LE__LE_var_L_i64, #function
	.proc	014
LE__LE_var_L_i64:
.LLFB459:
	save	%sp, -96, %sp
.LLCFI459:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o2
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL980
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL979
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL980
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL979
	 nop
.LL980:
	mov	0, %g1
.LL979:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE459:
	.size	LE__LE_var_L_i64, .-LE__LE_var_L_i64
	.align 4
	.type	LE__uLE_param_L_i64, #function
	.proc	014
LE__uLE_param_L_i64:
.LLFB460:
	save	%sp, -112, %sp
.LLCFI460:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL983
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL982
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL983
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL982
	 nop
.LL983:
	mov	0, %g1
.LL982:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE460:
	.size	LE__uLE_param_L_i64, .-LE__uLE_param_L_i64
	.align 4
	.type	LE__LE_param_L_i64, #function
	.proc	014
LE__LE_param_L_i64:
.LLFB461:
	save	%sp, -112, %sp
.LLCFI461:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	mov	1, %g1
	ld	[%fp-8], %o4
	ld	[%fp-16], %o5
	cmp	%o4, %o5
	bg	%icc, .LL986
	 nop
	ld	[%fp-8], %o4
	ld	[%fp-16], %o5
	cmp	%o4, %o5
	bne	%icc, .LL985
	 nop
	ld	[%fp-4], %o4
	ld	[%fp-12], %o5
	cmp	%o4, %o5
	bgu	%icc, .LL986
	 nop
	ld	[%fp-4], %o4
	ld	[%fp-12], %o5
	cmp	%o4, %o5
	ba,pt	%xcc, .LL985
	 nop
.LL986:
	mov	0, %g1
.LL985:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE461:
	.size	LE__LE_param_L_i64, .-LE__LE_param_L_i64
	.align 4
	.type	LE__uLE_var_L_C, #function
	.proc	014
LE__uLE_var_L_C:
.LLFB462:
	save	%sp, -96, %sp
.LLCFI462:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL989
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL988
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL989
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL988
	 nop
.LL989:
	mov	0, %g1
.LL988:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE462:
	.size	LE__uLE_var_L_C, .-LE__uLE_var_L_C
	.align 4
	.type	LE__LE_var_L_C, #function
	.proc	014
LE__LE_var_L_C:
.LLFB463:
	save	%sp, -96, %sp
.LLCFI463:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL992
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL991
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL992
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL991
	 nop
.LL992:
	mov	0, %g1
.LL991:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE463:
	.size	LE__LE_var_L_C, .-LE__LE_var_L_C
	.align 4
	.type	LE__uLE_param_L_C, #function
	.proc	014
LE__uLE_param_L_C:
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
	cmp	%o2, %o4
	bgu	%icc, .LL995
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL994
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL995
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL994
	 nop
.LL995:
	mov	0, %g1
.LL994:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE464:
	.size	LE__uLE_param_L_C, .-LE__uLE_param_L_C
	.align 4
	.type	LE__LE_param_L_C, #function
	.proc	014
LE__LE_param_L_C:
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
	bg	%icc, .LL998
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL997
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL998
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL997
	 nop
.LL998:
	mov	0, %g1
.LL997:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE465:
	.size	LE__LE_param_L_C, .-LE__LE_param_L_C
	.align 4
	.type	LE__uLE_var_L_u8, #function
	.proc	014
LE__uLE_var_L_u8:
.LLFB466:
	save	%sp, -96, %sp
.LLCFI466:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL1001
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL1000
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL1001
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL1000
	 nop
.LL1001:
	mov	0, %g1
.LL1000:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE466:
	.size	LE__uLE_var_L_u8, .-LE__uLE_var_L_u8
	.align 4
	.type	LE__LE_var_L_u8, #function
	.proc	014
LE__LE_var_L_u8:
.LLFB467:
	save	%sp, -96, %sp
.LLCFI467:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL1004
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL1003
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL1004
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL1003
	 nop
.LL1004:
	mov	0, %g1
.LL1003:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE467:
	.size	LE__LE_var_L_u8, .-LE__LE_var_L_u8
	.align 4
	.type	LE__uLE_param_L_u8, #function
	.proc	014
LE__uLE_param_L_u8:
.LLFB468:
	save	%sp, -104, %sp
.LLCFI468:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	ldd	[%fp-8], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL1007
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL1006
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL1007
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL1006
	 nop
.LL1007:
	mov	0, %g1
.LL1006:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE468:
	.size	LE__uLE_param_L_u8, .-LE__uLE_param_L_u8
	.align 4
	.type	LE__LE_param_L_u8, #function
	.proc	014
LE__LE_param_L_u8:
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
	bg	%icc, .LL1010
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL1009
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL1010
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL1009
	 nop
.LL1010:
	mov	0, %g1
.LL1009:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE469:
	.size	LE__LE_param_L_u8, .-LE__LE_param_L_u8
	.align 4
	.type	LE__uLE_var_L_L, #function
	.proc	014
LE__uLE_var_L_L:
.LLFB470:
	save	%sp, -96, %sp
.LLCFI470:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE470:
	.size	LE__uLE_var_L_L, .-LE__uLE_var_L_L
	.align 4
	.type	LE__LE_var_L_L, #function
	.proc	014
LE__LE_var_L_L:
.LLFB471:
	save	%sp, -96, %sp
.LLCFI471:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE471:
	.size	LE__LE_var_L_L, .-LE__LE_var_L_L
	.align 4
	.type	LE__uLE_param_L_L, #function
	.proc	014
LE__uLE_param_L_L:
.LLFB472:
	save	%sp, -112, %sp
.LLCFI472:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL1015
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL1014
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL1015
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL1014
	 nop
.LL1015:
	mov	0, %g1
.LL1014:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE472:
	.size	LE__uLE_param_L_L, .-LE__uLE_param_L_L
	.align 4
	.type	LE__LE_param_L_L, #function
	.proc	014
LE__LE_param_L_L:
.LLFB473:
	save	%sp, -112, %sp
.LLCFI473:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	mov	1, %g1
	ld	[%fp-8], %o4
	ld	[%fp-16], %o5
	cmp	%o4, %o5
	bg	%icc, .LL1018
	 nop
	ld	[%fp-8], %o4
	ld	[%fp-16], %o5
	cmp	%o4, %o5
	bne	%icc, .LL1017
	 nop
	ld	[%fp-4], %o4
	ld	[%fp-12], %o5
	cmp	%o4, %o5
	bgu	%icc, .LL1018
	 nop
	ld	[%fp-4], %o4
	ld	[%fp-12], %o5
	cmp	%o4, %o5
	ba,pt	%xcc, .LL1017
	 nop
.LL1018:
	mov	0, %g1
.LL1017:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE473:
	.size	LE__LE_param_L_L, .-LE__LE_param_L_L
	.align 4
	.type	LE__uLE_var_L_i8, #function
	.proc	014
LE__uLE_var_L_i8:
.LLFB474:
	save	%sp, -96, %sp
.LLCFI474:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL1021
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL1020
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL1021
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL1020
	 nop
.LL1021:
	mov	0, %g1
.LL1020:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE474:
	.size	LE__uLE_var_L_i8, .-LE__uLE_var_L_i8
	.align 4
	.type	LE__LE_var_L_i8, #function
	.proc	014
LE__LE_var_L_i8:
.LLFB475:
	save	%sp, -96, %sp
.LLCFI475:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL1024
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL1023
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL1024
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL1023
	 nop
.LL1024:
	mov	0, %g1
.LL1023:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE475:
	.size	LE__LE_var_L_i8, .-LE__LE_var_L_i8
	.align 4
	.type	LE__uLE_param_L_i8, #function
	.proc	014
LE__uLE_param_L_i8:
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
	cmp	%o2, %o4
	bgu	%icc, .LL1027
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL1026
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL1027
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL1026
	 nop
.LL1027:
	mov	0, %g1
.LL1026:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE476:
	.size	LE__uLE_param_L_i8, .-LE__uLE_param_L_i8
	.align 4
	.type	LE__LE_param_L_i8, #function
	.proc	014
LE__LE_param_L_i8:
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
	bg	%icc, .LL1030
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL1029
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL1030
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL1029
	 nop
.LL1030:
	mov	0, %g1
.LL1029:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE477:
	.size	LE__LE_param_L_i8, .-LE__LE_param_L_i8
	.align 4
	.type	LE__uLE_var_L_u32, #function
	.proc	014
LE__uLE_var_L_u32:
.LLFB478:
	save	%sp, -96, %sp
.LLCFI478:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bgu	%icc, .LL1033
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL1032
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL1033
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL1032
	 nop
.LL1033:
	mov	0, %g1
.LL1032:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE478:
	.size	LE__uLE_var_L_u32, .-LE__uLE_var_L_u32
	.align 4
	.type	LE__LE_var_L_u32, #function
	.proc	014
LE__LE_var_L_u32:
.LLFB479:
	save	%sp, -96, %sp
.LLCFI479:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o2, %o4
	bg	%icc, .LL1036
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL1035
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL1036
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL1035
	 nop
.LL1036:
	mov	0, %g1
.LL1035:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE479:
	.size	LE__LE_var_L_u32, .-LE__LE_var_L_u32
	.align 4
	.type	LE__uLE_param_L_u32, #function
	.proc	014
LE__uLE_param_L_u32:
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
	cmp	%o2, %o4
	bgu	%icc, .LL1039
	 nop
	cmp	%o2, %o4
	bne	%icc, .LL1038
	 nop
	cmp	%o3, %o5
	bgu	%icc, .LL1039
	 nop
	cmp	%o3, %o5
	ba,pt	%xcc, .LL1038
	 nop
.LL1039:
	mov	0, %g1
.LL1038:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE480:
	.size	LE__uLE_param_L_u32, .-LE__uLE_param_L_u32
	.align 4
	.type	LE__LE_param_L_u32, #function
	.proc	014
LE__LE_param_L_u32:
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
	bg	%icc, .LL1042
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL1041
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	bgu	%icc, .LL1042
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL1041
	 nop
.LL1042:
	mov	0, %g1
.LL1041:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE481:
	.size	LE__LE_param_L_u32, .-LE__LE_param_L_u32
	.align 4
	.type	LE__uLE_var_i8_u16, #function
	.proc	014
LE__uLE_var_i8_u16:
.LLFB482:
	save	%sp, -96, %sp
.LLCFI482:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE482:
	.size	LE__uLE_var_i8_u16, .-LE__uLE_var_i8_u16
	.align 4
	.type	LE__LE_var_i8_u16, #function
	.proc	014
LE__LE_var_i8_u16:
.LLFB483:
	save	%sp, -96, %sp
.LLCFI483:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE483:
	.size	LE__LE_var_i8_u16, .-LE__LE_var_i8_u16
	.align 4
	.type	LE__uLE_param_i8_u16, #function
	.proc	014
LE__uLE_param_i8_u16:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE484:
	.size	LE__uLE_param_i8_u16, .-LE__uLE_param_i8_u16
	.align 4
	.type	LE__LE_param_i8_u16, #function
	.proc	014
LE__LE_param_i8_u16:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE485:
	.size	LE__LE_param_i8_u16, .-LE__LE_param_i8_u16
	.align 4
	.type	LE__uLE_var_i8_u64, #function
	.proc	014
LE__uLE_var_i8_u64:
.LLFB486:
	save	%sp, -96, %sp
.LLCFI486:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL1049
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL1048
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL1049
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL1048
	 nop
.LL1049:
	mov	0, %g1
.LL1048:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE486:
	.size	LE__uLE_var_i8_u64, .-LE__uLE_var_i8_u64
	.align 4
	.type	LE__LE_var_i8_u64, #function
	.proc	014
LE__LE_var_i8_u64:
.LLFB487:
	save	%sp, -96, %sp
.LLCFI487:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL1052
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL1051
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL1052
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL1051
	 nop
.LL1052:
	mov	0, %g1
.LL1051:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE487:
	.size	LE__LE_var_i8_u64, .-LE__LE_var_i8_u64
	.align 4
	.type	LE__uLE_param_i8_u64, #function
	.proc	014
LE__uLE_param_i8_u64:
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
	cmp	%o4, %o2
	bgu	%icc, .LL1055
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL1054
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL1055
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL1054
	 nop
.LL1055:
	mov	0, %g1
.LL1054:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE488:
	.size	LE__uLE_param_i8_u64, .-LE__uLE_param_i8_u64
	.align 4
	.type	LE__LE_param_i8_u64, #function
	.proc	014
LE__LE_param_i8_u64:
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
	bl	%icc, .LL1058
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL1057
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL1058
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL1057
	 nop
.LL1058:
	mov	0, %g1
.LL1057:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE489:
	.size	LE__LE_param_i8_u64, .-LE__LE_param_i8_u64
	.align 4
	.type	LE__uLE_var_i8_LC, #function
	.proc	014
LE__uLE_var_i8_LC:
.LLFB490:
	save	%sp, -96, %sp
.LLCFI490:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL1061
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL1060
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL1061
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL1060
	 nop
.LL1061:
	mov	0, %g1
.LL1060:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE490:
	.size	LE__uLE_var_i8_LC, .-LE__uLE_var_i8_LC
	.align 4
	.type	LE__LE_var_i8_LC, #function
	.proc	014
LE__LE_var_i8_LC:
.LLFB491:
	save	%sp, -96, %sp
.LLCFI491:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL1064
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL1063
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL1064
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL1063
	 nop
.LL1064:
	mov	0, %g1
.LL1063:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE491:
	.size	LE__LE_var_i8_LC, .-LE__LE_var_i8_LC
	.align 4
	.type	LE__uLE_param_i8_LC, #function
	.proc	014
LE__uLE_param_i8_LC:
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
	blu	%icc, .LL1067
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL1066
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL1067
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL1066
	 nop
.LL1067:
	mov	0, %g1
.LL1066:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE492:
	.size	LE__uLE_param_i8_LC, .-LE__uLE_param_i8_LC
	.align 4
	.type	LE__LE_param_i8_LC, #function
	.proc	014
LE__LE_param_i8_LC:
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
	cmp	%o4, %o2
	bg	%icc, .LL1070
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL1069
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL1070
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL1069
	 nop
.LL1070:
	mov	0, %g1
.LL1069:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE493:
	.size	LE__LE_param_i8_LC, .-LE__LE_param_i8_LC
	.align 4
	.type	LE__uLE_var_i8_i32, #function
	.proc	014
LE__uLE_var_i8_i32:
.LLFB494:
	save	%sp, -96, %sp
.LLCFI494:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE494:
	.size	LE__uLE_var_i8_i32, .-LE__uLE_var_i8_i32
	.align 4
	.type	LE__LE_var_i8_i32, #function
	.proc	014
LE__LE_var_i8_i32:
.LLFB495:
	save	%sp, -96, %sp
.LLCFI495:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE495:
	.size	LE__LE_var_i8_i32, .-LE__LE_var_i8_i32
	.align 4
	.type	LE__uLE_param_i8_i32, #function
	.proc	014
LE__uLE_param_i8_i32:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE496:
	.size	LE__uLE_param_i8_i32, .-LE__uLE_param_i8_i32
	.align 4
	.type	LE__LE_param_i8_i32, #function
	.proc	014
LE__LE_param_i8_i32:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE497:
	.size	LE__LE_param_i8_i32, .-LE__LE_param_i8_i32
	.align 4
	.type	LE__uLE_var_i8_i16, #function
	.proc	014
LE__uLE_var_i8_i16:
.LLFB498:
	save	%sp, -96, %sp
.LLCFI498:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE498:
	.size	LE__uLE_var_i8_i16, .-LE__uLE_var_i8_i16
	.align 4
	.type	LE__LE_var_i8_i16, #function
	.proc	014
LE__LE_var_i8_i16:
.LLFB499:
	save	%sp, -96, %sp
.LLCFI499:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE499:
	.size	LE__LE_var_i8_i16, .-LE__LE_var_i8_i16
	.align 4
	.type	LE__uLE_param_i8_i16, #function
	.proc	014
LE__uLE_param_i8_i16:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE500:
	.size	LE__uLE_param_i8_i16, .-LE__uLE_param_i8_i16
	.align 4
	.type	LE__LE_param_i8_i16, #function
	.proc	014
LE__LE_param_i8_i16:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE501:
	.size	LE__LE_param_i8_i16, .-LE__LE_param_i8_i16
	.align 4
	.type	LE__uLE_var_i8_I, #function
	.proc	014
LE__uLE_var_i8_I:
.LLFB502:
	save	%sp, -96, %sp
.LLCFI502:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE502:
	.size	LE__uLE_var_i8_I, .-LE__uLE_var_i8_I
	.align 4
	.type	LE__LE_var_i8_I, #function
	.proc	014
LE__LE_var_i8_I:
.LLFB503:
	save	%sp, -96, %sp
.LLCFI503:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE503:
	.size	LE__LE_var_i8_I, .-LE__LE_var_i8_I
	.align 4
	.type	LE__uLE_param_i8_I, #function
	.proc	014
LE__uLE_param_i8_I:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE504:
	.size	LE__uLE_param_i8_I, .-LE__uLE_param_i8_I
	.align 4
	.type	LE__LE_param_i8_I, #function
	.proc	014
LE__LE_param_i8_I:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE505:
	.size	LE__LE_param_i8_I, .-LE__LE_param_i8_I
	.align 4
	.type	LE__uLE_var_i8_i64, #function
	.proc	014
LE__uLE_var_i8_i64:
.LLFB506:
	save	%sp, -96, %sp
.LLCFI506:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL1085
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL1084
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL1085
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL1084
	 nop
.LL1085:
	mov	0, %g1
.LL1084:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE506:
	.size	LE__uLE_var_i8_i64, .-LE__uLE_var_i8_i64
	.align 4
	.type	LE__LE_var_i8_i64, #function
	.proc	014
LE__LE_var_i8_i64:
.LLFB507:
	save	%sp, -96, %sp
.LLCFI507:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL1088
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL1087
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL1088
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL1087
	 nop
.LL1088:
	mov	0, %g1
.LL1087:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE507:
	.size	LE__LE_var_i8_i64, .-LE__LE_var_i8_i64
	.align 4
	.type	LE__uLE_param_i8_i64, #function
	.proc	014
LE__uLE_param_i8_i64:
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
	cmp	%o4, %o2
	bgu	%icc, .LL1091
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL1090
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL1091
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL1090
	 nop
.LL1091:
	mov	0, %g1
.LL1090:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE508:
	.size	LE__uLE_param_i8_i64, .-LE__uLE_param_i8_i64
	.align 4
	.type	LE__LE_param_i8_i64, #function
	.proc	014
LE__LE_param_i8_i64:
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
	bl	%icc, .LL1094
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL1093
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL1094
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL1093
	 nop
.LL1094:
	mov	0, %g1
.LL1093:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE509:
	.size	LE__LE_param_i8_i64, .-LE__LE_param_i8_i64
	.align 4
	.type	LE__uLE_var_i8_C, #function
	.proc	014
LE__uLE_var_i8_C:
.LLFB510:
	save	%sp, -96, %sp
.LLCFI510:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE510:
	.size	LE__uLE_var_i8_C, .-LE__uLE_var_i8_C
	.align 4
	.type	LE__LE_var_i8_C, #function
	.proc	014
LE__LE_var_i8_C:
.LLFB511:
	save	%sp, -96, %sp
.LLCFI511:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE511:
	.size	LE__LE_var_i8_C, .-LE__LE_var_i8_C
	.align 4
	.type	LE__uLE_param_i8_C, #function
	.proc	014
LE__uLE_param_i8_C:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE512:
	.size	LE__uLE_param_i8_C, .-LE__uLE_param_i8_C
	.align 4
	.type	LE__LE_param_i8_C, #function
	.proc	014
LE__LE_param_i8_C:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE513:
	.size	LE__LE_param_i8_C, .-LE__LE_param_i8_C
	.align 4
	.type	LE__uLE_var_i8_u8, #function
	.proc	014
LE__uLE_var_i8_u8:
.LLFB514:
	save	%sp, -96, %sp
.LLCFI514:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE514:
	.size	LE__uLE_var_i8_u8, .-LE__uLE_var_i8_u8
	.align 4
	.type	LE__LE_var_i8_u8, #function
	.proc	014
LE__LE_var_i8_u8:
.LLFB515:
	save	%sp, -96, %sp
.LLCFI515:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE515:
	.size	LE__LE_var_i8_u8, .-LE__LE_var_i8_u8
	.align 4
	.type	LE__uLE_param_i8_u8, #function
	.proc	014
LE__uLE_param_i8_u8:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE516:
	.size	LE__uLE_param_i8_u8, .-LE__uLE_param_i8_u8
	.align 4
	.type	LE__LE_param_i8_u8, #function
	.proc	014
LE__LE_param_i8_u8:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE517:
	.size	LE__LE_param_i8_u8, .-LE__LE_param_i8_u8
	.align 4
	.type	LE__uLE_var_i8_L, #function
	.proc	014
LE__uLE_var_i8_L:
.LLFB518:
	save	%sp, -96, %sp
.LLCFI518:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL1105
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL1104
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL1105
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL1104
	 nop
.LL1105:
	mov	0, %g1
.LL1104:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE518:
	.size	LE__uLE_var_i8_L, .-LE__uLE_var_i8_L
	.align 4
	.type	LE__LE_var_i8_L, #function
	.proc	014
LE__LE_var_i8_L:
.LLFB519:
	save	%sp, -96, %sp
.LLCFI519:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL1108
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL1107
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL1108
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL1107
	 nop
.LL1108:
	mov	0, %g1
.LL1107:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE519:
	.size	LE__LE_var_i8_L, .-LE__LE_var_i8_L
	.align 4
	.type	LE__uLE_param_i8_L, #function
	.proc	014
LE__uLE_param_i8_L:
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
	cmp	%o4, %o2
	bgu	%icc, .LL1111
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL1110
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL1111
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL1110
	 nop
.LL1111:
	mov	0, %g1
.LL1110:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE520:
	.size	LE__uLE_param_i8_L, .-LE__uLE_param_i8_L
	.align 4
	.type	LE__LE_param_i8_L, #function
	.proc	014
LE__LE_param_i8_L:
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
	bl	%icc, .LL1114
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL1113
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL1114
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL1113
	 nop
.LL1114:
	mov	0, %g1
.LL1113:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE521:
	.size	LE__LE_param_i8_L, .-LE__LE_param_i8_L
	.align 4
	.type	LE__uLE_var_i8_i8, #function
	.proc	014
LE__uLE_var_i8_i8:
.LLFB522:
	save	%sp, -96, %sp
.LLCFI522:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE522:
	.size	LE__uLE_var_i8_i8, .-LE__uLE_var_i8_i8
	.align 4
	.type	LE__LE_var_i8_i8, #function
	.proc	014
LE__LE_var_i8_i8:
.LLFB523:
	save	%sp, -96, %sp
.LLCFI523:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE523:
	.size	LE__LE_var_i8_i8, .-LE__LE_var_i8_i8
	.align 4
	.type	LE__uLE_param_i8_i8, #function
	.proc	014
LE__uLE_param_i8_i8:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE524:
	.size	LE__uLE_param_i8_i8, .-LE__uLE_param_i8_i8
	.align 4
	.type	LE__LE_param_i8_i8, #function
	.proc	014
LE__LE_param_i8_i8:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE525:
	.size	LE__LE_param_i8_i8, .-LE__LE_param_i8_i8
	.align 4
	.type	LE__uLE_var_i8_u32, #function
	.proc	014
LE__uLE_var_i8_u32:
.LLFB526:
	save	%sp, -96, %sp
.LLCFI526:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE526:
	.size	LE__uLE_var_i8_u32, .-LE__uLE_var_i8_u32
	.align 4
	.type	LE__LE_var_i8_u32, #function
	.proc	014
LE__LE_var_i8_u32:
.LLFB527:
	save	%sp, -96, %sp
.LLCFI527:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE527:
	.size	LE__LE_var_i8_u32, .-LE__LE_var_i8_u32
	.align 4
	.type	LE__uLE_param_i8_u32, #function
	.proc	014
LE__uLE_param_i8_u32:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE528:
	.size	LE__uLE_param_i8_u32, .-LE__uLE_param_i8_u32
	.align 4
	.type	LE__LE_param_i8_u32, #function
	.proc	014
LE__LE_param_i8_u32:
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
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE529:
	.size	LE__LE_param_i8_u32, .-LE__LE_param_i8_u32
	.align 4
	.type	LE__uLE_var_u32_u16, #function
	.proc	014
LE__uLE_var_u32_u16:
.LLFB530:
	save	%sp, -96, %sp
.LLCFI530:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE530:
	.size	LE__uLE_var_u32_u16, .-LE__uLE_var_u32_u16
	.align 4
	.type	LE__LE_var_u32_u16, #function
	.proc	014
LE__LE_var_u32_u16:
.LLFB531:
	save	%sp, -96, %sp
.LLCFI531:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE531:
	.size	LE__LE_var_u32_u16, .-LE__LE_var_u32_u16
	.align 4
	.type	LE__uLE_param_u32_u16, #function
	.proc	014
LE__uLE_param_u32_u16:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE532:
	.size	LE__uLE_param_u32_u16, .-LE__uLE_param_u32_u16
	.align 4
	.type	LE__LE_param_u32_u16, #function
	.proc	014
LE__LE_param_u32_u16:
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
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE533:
	.size	LE__LE_param_u32_u16, .-LE__LE_param_u32_u16
	.align 4
	.type	LE__uLE_var_u32_u64, #function
	.proc	014
LE__uLE_var_u32_u64:
.LLFB534:
	save	%sp, -96, %sp
.LLCFI534:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL1129
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL1128
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL1129
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL1128
	 nop
.LL1129:
	mov	0, %g1
.LL1128:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE534:
	.size	LE__uLE_var_u32_u64, .-LE__uLE_var_u32_u64
	.align 4
	.type	LE__LE_var_u32_u64, #function
	.proc	014
LE__LE_var_u32_u64:
.LLFB535:
	save	%sp, -96, %sp
.LLCFI535:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+56], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL1132
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL1131
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL1132
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL1131
	 nop
.LL1132:
	mov	0, %g1
.LL1131:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE535:
	.size	LE__LE_var_u32_u64, .-LE__LE_var_u32_u64
	.align 4
	.type	LE__uLE_param_u32_u64, #function
	.proc	014
LE__uLE_param_u32_u64:
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
	cmp	%o4, %o2
	bgu	%icc, .LL1135
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL1134
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL1135
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL1134
	 nop
.LL1135:
	mov	0, %g1
.LL1134:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE536:
	.size	LE__uLE_param_u32_u64, .-LE__uLE_param_u32_u64
	.align 4
	.type	LE__LE_param_u32_u64, #function
	.proc	014
LE__LE_param_u32_u64:
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
	bl	%icc, .LL1138
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL1137
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL1138
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL1137
	 nop
.LL1138:
	mov	0, %g1
.LL1137:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE537:
	.size	LE__LE_param_u32_u64, .-LE__LE_param_u32_u64
	.align 4
	.type	LE__uLE_var_u32_LC, #function
	.proc	014
LE__uLE_var_u32_LC:
.LLFB538:
	save	%sp, -96, %sp
.LLCFI538:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL1141
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL1140
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL1141
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL1140
	 nop
.LL1141:
	mov	0, %g1
.LL1140:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE538:
	.size	LE__uLE_var_u32_LC, .-LE__uLE_var_u32_LC
	.align 4
	.type	LE__LE_var_u32_LC, #function
	.proc	014
LE__LE_var_u32_LC:
.LLFB539:
	save	%sp, -96, %sp
.LLCFI539:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+64], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL1144
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL1143
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL1144
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL1143
	 nop
.LL1144:
	mov	0, %g1
.LL1143:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE539:
	.size	LE__LE_var_u32_LC, .-LE__LE_var_u32_LC
	.align 4
	.type	LE__uLE_param_u32_LC, #function
	.proc	014
LE__uLE_param_u32_LC:
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
	blu	%icc, .LL1147
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL1146
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL1147
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL1146
	 nop
.LL1147:
	mov	0, %g1
.LL1146:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE540:
	.size	LE__uLE_param_u32_LC, .-LE__uLE_param_u32_LC
	.align 4
	.type	LE__LE_param_u32_LC, #function
	.proc	014
LE__LE_param_u32_LC:
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
	cmp	%o4, %o2
	bg	%icc, .LL1150
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL1149
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL1150
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL1149
	 nop
.LL1150:
	mov	0, %g1
.LL1149:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE541:
	.size	LE__LE_param_u32_LC, .-LE__LE_param_u32_LC
	.align 4
	.type	LE__uLE_var_u32_i32, #function
	.proc	014
LE__uLE_var_u32_i32:
.LLFB542:
	save	%sp, -96, %sp
.LLCFI542:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE542:
	.size	LE__uLE_var_u32_i32, .-LE__uLE_var_u32_i32
	.align 4
	.type	LE__LE_var_u32_i32, #function
	.proc	014
LE__LE_var_u32_i32:
.LLFB543:
	save	%sp, -96, %sp
.LLCFI543:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE543:
	.size	LE__LE_var_u32_i32, .-LE__LE_var_u32_i32
	.align 4
	.type	LE__uLE_param_u32_i32, #function
	.proc	014
LE__uLE_param_u32_i32:
.LLFB544:
	save	%sp, -96, %sp
.LLCFI544:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE544:
	.size	LE__uLE_param_u32_i32, .-LE__uLE_param_u32_i32
	.align 4
	.type	LE__LE_param_u32_i32, #function
	.proc	014
LE__LE_param_u32_i32:
.LLFB545:
	save	%sp, -96, %sp
.LLCFI545:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE545:
	.size	LE__LE_param_u32_i32, .-LE__LE_param_u32_i32
	.align 4
	.type	LE__uLE_var_u32_i16, #function
	.proc	014
LE__uLE_var_u32_i16:
.LLFB546:
	save	%sp, -96, %sp
.LLCFI546:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE546:
	.size	LE__uLE_var_u32_i16, .-LE__uLE_var_u32_i16
	.align 4
	.type	LE__LE_var_u32_i16, #function
	.proc	014
LE__LE_var_u32_i16:
.LLFB547:
	save	%sp, -96, %sp
.LLCFI547:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE547:
	.size	LE__LE_var_u32_i16, .-LE__LE_var_u32_i16
	.align 4
	.type	LE__uLE_param_u32_i16, #function
	.proc	014
LE__uLE_param_u32_i16:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE548:
	.size	LE__uLE_param_u32_i16, .-LE__uLE_param_u32_i16
	.align 4
	.type	LE__LE_param_u32_i16, #function
	.proc	014
LE__LE_param_u32_i16:
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
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE549:
	.size	LE__LE_param_u32_i16, .-LE__LE_param_u32_i16
	.align 4
	.type	LE__uLE_var_u32_I, #function
	.proc	014
LE__uLE_var_u32_I:
.LLFB550:
	save	%sp, -96, %sp
.LLCFI550:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE550:
	.size	LE__uLE_var_u32_I, .-LE__uLE_var_u32_I
	.align 4
	.type	LE__LE_var_u32_I, #function
	.proc	014
LE__LE_var_u32_I:
.LLFB551:
	save	%sp, -96, %sp
.LLCFI551:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+80], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE551:
	.size	LE__LE_var_u32_I, .-LE__LE_var_u32_I
	.align 4
	.type	LE__uLE_param_u32_I, #function
	.proc	014
LE__uLE_param_u32_I:
.LLFB552:
	save	%sp, -96, %sp
.LLCFI552:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE552:
	.size	LE__uLE_param_u32_I, .-LE__uLE_param_u32_I
	.align 4
	.type	LE__LE_param_u32_I, #function
	.proc	014
LE__LE_param_u32_I:
.LLFB553:
	save	%sp, -96, %sp
.LLCFI553:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE553:
	.size	LE__LE_param_u32_I, .-LE__LE_param_u32_I
	.align 4
	.type	LE__uLE_var_u32_i64, #function
	.proc	014
LE__uLE_var_u32_i64:
.LLFB554:
	save	%sp, -96, %sp
.LLCFI554:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL1165
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL1164
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL1165
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL1164
	 nop
.LL1165:
	mov	0, %g1
.LL1164:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE554:
	.size	LE__uLE_var_u32_i64, .-LE__uLE_var_u32_i64
	.align 4
	.type	LE__LE_var_u32_i64, #function
	.proc	014
LE__LE_var_u32_i64:
.LLFB555:
	save	%sp, -96, %sp
.LLCFI555:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+88], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL1168
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL1167
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL1168
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL1167
	 nop
.LL1168:
	mov	0, %g1
.LL1167:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE555:
	.size	LE__LE_var_u32_i64, .-LE__LE_var_u32_i64
	.align 4
	.type	LE__uLE_param_u32_i64, #function
	.proc	014
LE__uLE_param_u32_i64:
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
	cmp	%o4, %o2
	bgu	%icc, .LL1171
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL1170
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL1171
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL1170
	 nop
.LL1171:
	mov	0, %g1
.LL1170:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE556:
	.size	LE__uLE_param_u32_i64, .-LE__uLE_param_u32_i64
	.align 4
	.type	LE__LE_param_u32_i64, #function
	.proc	014
LE__LE_param_u32_i64:
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
	bl	%icc, .LL1174
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL1173
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL1174
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL1173
	 nop
.LL1174:
	mov	0, %g1
.LL1173:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE557:
	.size	LE__LE_param_u32_i64, .-LE__LE_param_u32_i64
	.align 4
	.type	LE__uLE_var_u32_C, #function
	.proc	014
LE__uLE_var_u32_C:
.LLFB558:
	save	%sp, -96, %sp
.LLCFI558:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE558:
	.size	LE__uLE_var_u32_C, .-LE__uLE_var_u32_C
	.align 4
	.type	LE__LE_var_u32_C, #function
	.proc	014
LE__LE_var_u32_C:
.LLFB559:
	save	%sp, -96, %sp
.LLCFI559:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+96], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE559:
	.size	LE__LE_var_u32_C, .-LE__LE_var_u32_C
	.align 4
	.type	LE__uLE_param_u32_C, #function
	.proc	014
LE__uLE_param_u32_C:
.LLFB560:
	save	%sp, -96, %sp
.LLCFI560:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE560:
	.size	LE__uLE_param_u32_C, .-LE__uLE_param_u32_C
	.align 4
	.type	LE__LE_param_u32_C, #function
	.proc	014
LE__LE_param_u32_C:
.LLFB561:
	save	%sp, -96, %sp
.LLCFI561:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE561:
	.size	LE__LE_param_u32_C, .-LE__LE_param_u32_C
	.align 4
	.type	LE__uLE_var_u32_u8, #function
	.proc	014
LE__uLE_var_u32_u8:
.LLFB562:
	save	%sp, -96, %sp
.LLCFI562:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE562:
	.size	LE__uLE_var_u32_u8, .-LE__uLE_var_u32_u8
	.align 4
	.type	LE__LE_var_u32_u8, #function
	.proc	014
LE__LE_var_u32_u8:
.LLFB563:
	save	%sp, -96, %sp
.LLCFI563:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE563:
	.size	LE__LE_var_u32_u8, .-LE__LE_var_u32_u8
	.align 4
	.type	LE__uLE_param_u32_u8, #function
	.proc	014
LE__uLE_param_u32_u8:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE564:
	.size	LE__uLE_param_u32_u8, .-LE__uLE_param_u32_u8
	.align 4
	.type	LE__LE_param_u32_u8, #function
	.proc	014
LE__LE_param_u32_u8:
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
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE565:
	.size	LE__LE_param_u32_u8, .-LE__LE_param_u32_u8
	.align 4
	.type	LE__uLE_var_u32_L, #function
	.proc	014
LE__uLE_var_u32_L:
.LLFB566:
	save	%sp, -96, %sp
.LLCFI566:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bgu	%icc, .LL1185
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL1184
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL1185
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL1184
	 nop
.LL1185:
	mov	0, %g1
.LL1184:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE566:
	.size	LE__uLE_var_u32_L, .-LE__uLE_var_u32_L
	.align 4
	.type	LE__LE_var_u32_L, #function
	.proc	014
LE__LE_var_u32_L:
.LLFB567:
	save	%sp, -96, %sp
.LLCFI567:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ldd	[%g1+120], %o2
	mov	1, %g1
	cmp	%o4, %o2
	bg	%icc, .LL1188
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL1187
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL1188
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL1187
	 nop
.LL1188:
	mov	0, %g1
.LL1187:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE567:
	.size	LE__LE_var_u32_L, .-LE__LE_var_u32_L
	.align 4
	.type	LE__uLE_param_u32_L, #function
	.proc	014
LE__uLE_param_u32_L:
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
	cmp	%o4, %o2
	bgu	%icc, .LL1191
	 nop
	cmp	%o4, %o2
	bne	%icc, .LL1190
	 nop
	cmp	%o5, %o3
	bgu	%icc, .LL1191
	 nop
	cmp	%o5, %o3
	ba,pt	%xcc, .LL1190
	 nop
.LL1191:
	mov	0, %g1
.LL1190:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE568:
	.size	LE__uLE_param_u32_L, .-LE__uLE_param_u32_L
	.align 4
	.type	LE__LE_param_u32_L, #function
	.proc	014
LE__LE_param_u32_L:
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
	bl	%icc, .LL1194
	 nop
	ld	[%fp-8], %o3
	cmp	%o3, %o4
	bne	%icc, .LL1193
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	blu	%icc, .LL1194
	 nop
	ld	[%fp-4], %o3
	cmp	%o3, %o5
	ba,pt	%xcc, .LL1193
	 nop
.LL1194:
	mov	0, %g1
.LL1193:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE569:
	.size	LE__LE_param_u32_L, .-LE__LE_param_u32_L
	.align 4
	.type	LE__uLE_var_u32_i8, #function
	.proc	014
LE__uLE_var_u32_i8:
.LLFB570:
	save	%sp, -96, %sp
.LLCFI570:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE570:
	.size	LE__uLE_var_u32_i8, .-LE__uLE_var_u32_i8
	.align 4
	.type	LE__LE_var_u32_i8, #function
	.proc	014
LE__LE_var_u32_i8:
.LLFB571:
	save	%sp, -96, %sp
.LLCFI571:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+132], %o5
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE571:
	.size	LE__LE_var_u32_i8, .-LE__LE_var_u32_i8
	.align 4
	.type	LE__uLE_param_u32_i8, #function
	.proc	014
LE__uLE_param_u32_i8:
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
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE572:
	.size	LE__uLE_param_u32_i8, .-LE__uLE_param_u32_i8
	.align 4
	.type	LE__LE_param_u32_i8, #function
	.proc	014
LE__LE_param_u32_i8:
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
	movge	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE573:
	.size	LE__LE_param_u32_i8, .-LE__LE_param_u32_i8
	.align 4
	.type	LE__uLE_var_u32_u32, #function
	.proc	014
LE__uLE_var_u32_u32:
.LLFB574:
	save	%sp, -96, %sp
.LLCFI574:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE574:
	.size	LE__uLE_var_u32_u32, .-LE__uLE_var_u32_u32
	.align 4
	.type	LE__LE_var_u32_u32, #function
	.proc	014
LE__LE_var_u32_u32:
.LLFB575:
	save	%sp, -96, %sp
.LLCFI575:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE575:
	.size	LE__LE_var_u32_u32, .-LE__LE_var_u32_u32
	.align 4
	.type	LE__uLE_param_u32_u32, #function
	.proc	014
LE__uLE_param_u32_u32:
.LLFB576:
	save	%sp, -96, %sp
.LLCFI576:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movleu	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE576:
	.size	LE__uLE_param_u32_u32, .-LE__uLE_param_u32_u32
	.align 4
	.type	LE__LE_param_u32_u32, #function
	.proc	014
LE__LE_param_u32_u32:
.LLFB577:
	save	%sp, -96, %sp
.LLCFI577:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	cmp	%o5, %g1
	mov	0, %g1
	movle	%icc, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE577:
	.size	LE__LE_param_u32_u32, .-LE__LE_param_u32_u32
	.align 4
	.type	LE__LE_var_f32_f32, #function
	.proc	014
LE__LE_var_f32_f32:
.LLFB578:
	save	%sp, -96, %sp
.LLCFI578:
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	add	%g1, 136, %g1
	ld	[%g1], %f9
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	add	%g1, 136, %g1
	ld	[%g1], %f8
	fcmps	%fcc2, %f9, %f8
	mov	0, %g1
	move	%fcc2, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE578:
	.size	LE__LE_var_f32_f32, .-LE__LE_var_f32_f32
	.align 4
	.type	LE__LE_param_f32_f32, #function
	.proc	014
LE__LE_param_f32_f32:
.LLFB579:
	save	%sp, -96, %sp
.LLCFI579:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %f9
	ld	[%fp+72], %f8
	fcmpes	%fcc3, %f9, %f8
	mov	0, %g1
	movle	%fcc3, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE579:
	.size	LE__LE_param_f32_f32, .-LE__LE_param_f32_f32
	.align 4
	.global LE_M3
	.type	LE_M3, #function
	.proc	0120
LE_M3:
.LLFB580:
	save	%sp, -96, %sp
.LLCFI580:
	st	%i0, [%fp+68]
	sethi	%hi(MM_LE), %g1
	or	%g1, %lo(MM_LE), %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE580:
	.size	LE_M3, .-LE_M3
	.section	".data"
	.align 8
	.type	L_1, #object
	.size	L_1, 13384
L_1:
	.ascii	"LE_M3"
	.skip 1
	.ascii	"LE_param_f32_f32"
	.skip 1
	.ascii	"LE_var_f32_f32"
	.skip 1
	.ascii	"LE_param_u32_u32"
	.skip 1
	.ascii	"uLE_param_u32_u32"
	.skip 1
	.ascii	"LE_var_u32_u32"
	.skip 1
	.ascii	"uLE_var_u32_u32"
	.skip 1
	.ascii	"LE_param_u32_i8"
	.skip 1
	.ascii	"uLE_param_u32_i8"
	.skip 1
	.ascii	"LE_var_u32_i8"
	.skip 1
	.ascii	"uLE_var_u32_i8"
	.skip 1
	.ascii	"LE_param_u32_L"
	.skip 1
	.ascii	"uLE_param_u32_L"
	.skip 1
	.ascii	"LE_var_u32_L"
	.skip 1
	.ascii	"uLE_var_u32_L"
	.skip 1
	.ascii	"LE_param_u32_u8"
	.skip 1
	.ascii	"uLE_param_u32_u8"
	.skip 1
	.ascii	"LE_var_u32_u8"
	.skip 1
	.ascii	"uLE_var_u32_u8"
	.skip 1
	.ascii	"LE_param_u32_C"
	.skip 1
	.ascii	"uLE_param_u32_C"
	.skip 1
	.ascii	"LE_var_u32_C"
	.skip 1
	.ascii	"uLE_var_u32_C"
	.skip 1
	.ascii	"LE_param_u32_i64"
	.skip 1
	.ascii	"uLE_param_u32_i64"
	.skip 1
	.ascii	"LE_var_u32_i64"
	.skip 1
	.ascii	"uLE_var_u32_i64"
	.skip 1
	.ascii	"LE_param_u32_I"
	.skip 1
	.ascii	"uLE_param_u32_I"
	.skip 1
	.ascii	"LE_var_u32_I"
	.skip 1
	.ascii	"uLE_var_u32_I"
	.skip 1
	.ascii	"LE_param_u32_i16"
	.skip 1
	.ascii	"uLE_param_u32_i16"
	.skip 1
	.ascii	"LE_var_u32_i16"
	.skip 1
	.ascii	"uLE_var_u32_i16"
	.skip 1
	.ascii	"LE_param_u32_i32"
	.skip 1
	.ascii	"uLE_param_u32_i32"
	.skip 1
	.ascii	"LE_var_u32_i32"
	.skip 1
	.ascii	"uLE_var_u32_i32"
	.skip 1
	.ascii	"LE_param_u32_LC"
	.skip 1
	.ascii	"uLE_param_u32_LC"
	.skip 1
	.ascii	"LE_var_u32_LC"
	.skip 1
	.ascii	"uLE_var_u32_LC"
	.skip 1
	.ascii	"LE_param_u32_u64"
	.skip 1
	.ascii	"uLE_param_u32_u64"
	.skip 1
	.ascii	"LE_var_u32_u64"
	.skip 1
	.ascii	"uLE_var_u32_u64"
	.skip 1
	.ascii	"LE_param_u32_u16"
	.skip 1
	.ascii	"uLE_param_u32_u16"
	.skip 1
	.ascii	"LE_var_u32_u16"
	.skip 1
	.ascii	"uLE_var_u32_u16"
	.skip 1
	.ascii	"LE_param_i8_u32"
	.skip 1
	.ascii	"uLE_param_i8_u32"
	.skip 1
	.ascii	"LE_var_i8_u32"
	.skip 1
	.ascii	"uLE_var_i8_u32"
	.skip 1
	.ascii	"LE_param_i8_i8"
	.skip 1
	.ascii	"uLE_param_i8_i8"
	.skip 1
	.ascii	"LE_var_i8_i8"
	.skip 1
	.ascii	"uLE_var_i8_i8"
	.skip 1
	.ascii	"LE_param_i8_L"
	.skip 1
	.ascii	"uLE_param_i8_L"
	.skip 1
	.ascii	"LE_var_i8_L"
	.skip 1
	.ascii	"uLE_var_i8_L"
	.skip 1
	.ascii	"LE_param_i8_u8"
	.skip 1
	.ascii	"uLE_param_i8_u8"
	.skip 1
	.ascii	"LE_var_i8_u8"
	.skip 1
	.ascii	"uLE_var_i8_u8"
	.skip 1
	.ascii	"LE_param_i8_C"
	.skip 1
	.ascii	"uLE_param_i8_C"
	.skip 1
	.ascii	"LE_var_i8_C"
	.skip 1
	.ascii	"uLE_var_i8_C"
	.skip 1
	.ascii	"LE_param_i8_i64"
	.skip 1
	.ascii	"uLE_param_i8_i64"
	.skip 1
	.ascii	"LE_var_i8_i64"
	.skip 1
	.ascii	"uLE_var_i8_i64"
	.skip 1
	.ascii	"LE_param_i8_I"
	.skip 1
	.ascii	"uLE_param_i8_I"
	.skip 1
	.ascii	"LE_var_i8_I"
	.skip 1
	.ascii	"uLE_var_i8_I"
	.skip 1
	.ascii	"LE_param_i8_i16"
	.skip 1
	.ascii	"uLE_param_i8_i16"
	.skip 1
	.ascii	"LE_var_i8_i16"
	.skip 1
	.ascii	"uLE_var_i8_i16"
	.skip 1
	.ascii	"LE_param_i8_i32"
	.skip 1
	.ascii	"uLE_param_i8_i32"
	.skip 1
	.ascii	"LE_var_i8_i32"
	.skip 1
	.ascii	"uLE_var_i8_i32"
	.skip 1
	.ascii	"LE_param_i8_LC"
	.skip 1
	.ascii	"uLE_param_i8_LC"
	.skip 1
	.ascii	"LE_var_i8_LC"
	.skip 1
	.ascii	"uLE_var_i8_LC"
	.skip 1
	.ascii	"LE_param_i8_u64"
	.skip 1
	.ascii	"uLE_param_i8_u64"
	.skip 1
	.ascii	"LE_var_i8_u64"
	.skip 1
	.ascii	"uLE_var_i8_u64"
	.skip 1
	.ascii	"LE_param_i8_u16"
	.skip 1
	.ascii	"uLE_param_i8_u16"
	.skip 1
	.ascii	"LE_var_i8_u16"
	.skip 1
	.ascii	"uLE_var_i8_u16"
	.skip 1
	.ascii	"LE_param_L_u32"
	.skip 1
	.ascii	"uLE_param_L_u32"
	.skip 1
	.ascii	"LE_var_L_u32"
	.skip 1
	.ascii	"uLE_var_L_u32"
	.skip 1
	.ascii	"LE_param_L_i8"
	.skip 1
	.ascii	"uLE_param_L_i8"
	.skip 1
	.ascii	"LE_var_L_i8"
	.skip 1
	.ascii	"uLE_var_L_i8"
	.skip 1
	.ascii	"LE_param_L_L"
	.skip 1
	.ascii	"uLE_param_L_L"
	.skip 1
	.ascii	"LE_var_L_L"
	.skip 1
	.ascii	"uLE_var_L_L"
	.skip 1
	.ascii	"LE_param_L_u8"
	.skip 1
	.ascii	"uLE_param_L_u8"
	.skip 1
	.ascii	"LE_var_L_u8"
	.skip 1
	.ascii	"uLE_var_L_u8"
	.skip 1
	.ascii	"LE_param_L_C"
	.skip 1
	.ascii	"uLE_param_L_C"
	.skip 1
	.ascii	"LE_var_L_C"
	.skip 1
	.ascii	"uLE_var_L_C"
	.skip 1
	.ascii	"LE_param_L_i64"
	.skip 1
	.ascii	"uLE_param_L_i64"
	.skip 1
	.ascii	"LE_var_L_i64"
	.skip 1
	.ascii	"uLE_var_L_i64"
	.skip 1
	.ascii	"LE_param_L_I"
	.skip 1
	.ascii	"uLE_param_L_I"
	.skip 1
	.ascii	"LE_var_L_I"
	.skip 1
	.ascii	"uLE_var_L_I"
	.skip 1
	.ascii	"LE_param_L_i16"
	.skip 1
	.ascii	"uLE_param_L_i16"
	.skip 1
	.ascii	"LE_var_L_i16"
	.skip 1
	.ascii	"uLE_var_L_i16"
	.skip 1
	.ascii	"LE_param_L_i32"
	.skip 1
	.ascii	"uLE_param_L_i32"
	.skip 1
	.ascii	"LE_var_L_i32"
	.skip 1
	.ascii	"uLE_var_L_i32"
	.skip 1
	.ascii	"LE_param_L_LC"
	.skip 1
	.ascii	"uLE_param_L_LC"
	.skip 1
	.ascii	"LE_var_L_LC"
	.skip 1
	.ascii	"uLE_var_L_LC"
	.skip 1
	.ascii	"LE_param_L_u64"
	.skip 1
	.ascii	"uLE_param_L_u64"
	.skip 1
	.ascii	"LE_var_L_u64"
	.skip 1
	.ascii	"uLE_var_L_u64"
	.skip 1
	.ascii	"LE_param_L_u16"
	.skip 1
	.ascii	"uLE_param_L_u16"
	.skip 1
	.ascii	"LE_var_L_u16"
	.skip 1
	.ascii	"uLE_var_L_u16"
	.skip 1
	.ascii	"LE_param_u8_u32"
	.skip 1
	.ascii	"uLE_param_u8_u32"
	.skip 1
	.ascii	"LE_var_u8_u32"
	.skip 1
	.ascii	"uLE_var_u8_u32"
	.skip 1
	.ascii	"LE_param_u8_i8"
	.skip 1
	.ascii	"uLE_param_u8_i8"
	.skip 1
	.ascii	"LE_var_u8_i8"
	.skip 1
	.ascii	"uLE_var_u8_i8"
	.skip 1
	.ascii	"LE_param_u8_L"
	.skip 1
	.ascii	"uLE_param_u8_L"
	.skip 1
	.ascii	"LE_var_u8_L"
	.skip 1
	.ascii	"uLE_var_u8_L"
	.skip 1
	.ascii	"LE_param_u8_u8"
	.skip 1
	.ascii	"uLE_param_u8_u8"
	.skip 1
	.ascii	"LE_var_u8_u8"
	.skip 1
	.ascii	"uLE_var_u8_u8"
	.skip 1
	.ascii	"LE_param_u8_C"
	.skip 1
	.ascii	"uLE_param_u8_C"
	.skip 1
	.ascii	"LE_var_u8_C"
	.skip 1
	.ascii	"uLE_var_u8_C"
	.skip 1
	.ascii	"LE_param_u8_i64"
	.skip 1
	.ascii	"uLE_param_u8_i64"
	.skip 1
	.ascii	"LE_var_u8_i64"
	.skip 1
	.ascii	"uLE_var_u8_i64"
	.skip 1
	.ascii	"LE_param_u8_I"
	.skip 1
	.ascii	"uLE_param_u8_I"
	.skip 1
	.ascii	"LE_var_u8_I"
	.skip 1
	.ascii	"uLE_var_u8_I"
	.skip 1
	.ascii	"LE_param_u8_i16"
	.skip 1
	.ascii	"uLE_param_u8_i16"
	.skip 1
	.ascii	"LE_var_u8_i16"
	.skip 1
	.ascii	"uLE_var_u8_i16"
	.skip 1
	.ascii	"LE_param_u8_i32"
	.skip 1
	.ascii	"uLE_param_u8_i32"
	.skip 1
	.ascii	"LE_var_u8_i32"
	.skip 1
	.ascii	"uLE_var_u8_i32"
	.skip 1
	.ascii	"LE_param_u8_LC"
	.skip 1
	.ascii	"uLE_param_u8_LC"
	.skip 1
	.ascii	"LE_var_u8_LC"
	.skip 1
	.ascii	"uLE_var_u8_LC"
	.skip 1
	.ascii	"LE_param_u8_u64"
	.skip 1
	.ascii	"uLE_param_u8_u64"
	.skip 1
	.ascii	"LE_var_u8_u64"
	.skip 1
	.ascii	"uLE_var_u8_u64"
	.skip 1
	.ascii	"LE_param_u8_u16"
	.skip 1
	.ascii	"uLE_param_u8_u16"
	.skip 1
	.ascii	"LE_var_u8_u16"
	.skip 1
	.ascii	"uLE_var_u8_u16"
	.skip 1
	.ascii	"LE_param_f64_f64"
	.skip 1
	.ascii	"LE_var_f64_f64"
	.skip 1
	.ascii	"LE_param_C_u32"
	.skip 1
	.ascii	"uLE_param_C_u32"
	.skip 1
	.ascii	"LE_var_C_u32"
	.skip 1
	.ascii	"uLE_var_C_u32"
	.skip 1
	.ascii	"LE_param_C_i8"
	.skip 1
	.ascii	"uLE_param_C_i8"
	.skip 1
	.ascii	"LE_var_C_i8"
	.skip 1
	.ascii	"uLE_var_C_i8"
	.skip 1
	.ascii	"LE_param_C_L"
	.skip 1
	.ascii	"uLE_param_C_L"
	.skip 1
	.ascii	"LE_var_C_L"
	.skip 1
	.ascii	"uLE_var_C_L"
	.skip 1
	.ascii	"LE_param_C_u8"
	.skip 1
	.ascii	"uLE_param_C_u8"
	.skip 1
	.ascii	"LE_var_C_u8"
	.skip 1
	.ascii	"uLE_var_C_u8"
	.skip 1
	.ascii	"LE_param_C_C"
	.skip 1
	.ascii	"uLE_param_C_C"
	.skip 1
	.ascii	"LE_var_C_C"
	.skip 1
	.ascii	"uLE_var_C_C"
	.skip 1
	.ascii	"LE_param_C_i64"
	.skip 1
	.ascii	"uLE_param_C_i64"
	.skip 1
	.ascii	"LE_var_C_i64"
	.skip 1
	.ascii	"uLE_var_C_i64"
	.skip 1
	.ascii	"LE_param_C_I"
	.skip 1
	.ascii	"uLE_param_C_I"
	.skip 1
	.ascii	"LE_var_C_I"
	.skip 1
	.ascii	"uLE_var_C_I"
	.skip 1
	.ascii	"LE_param_C_i16"
	.skip 1
	.ascii	"uLE_param_C_i16"
	.skip 1
	.ascii	"LE_var_C_i16"
	.skip 1
	.ascii	"uLE_var_C_i16"
	.skip 1
	.ascii	"LE_param_C_i32"
	.skip 1
	.ascii	"uLE_param_C_i32"
	.skip 1
	.ascii	"LE_var_C_i32"
	.skip 1
	.ascii	"uLE_var_C_i32"
	.skip 1
	.ascii	"LE_param_C_LC"
	.skip 1
	.ascii	"uLE_param_C_LC"
	.skip 1
	.ascii	"LE_var_C_LC"
	.skip 1
	.ascii	"uLE_var_C_LC"
	.skip 1
	.ascii	"LE_param_C_u64"
	.skip 1
	.ascii	"uLE_param_C_u64"
	.skip 1
	.ascii	"LE_var_C_u64"
	.skip 1
	.ascii	"uLE_var_C_u64"
	.skip 1
	.ascii	"LE_param_C_u16"
	.skip 1
	.ascii	"uLE_param_C_u16"
	.skip 1
	.ascii	"LE_var_C_u16"
	.skip 1
	.ascii	"uLE_var_C_u16"
	.skip 1
	.ascii	"LE_param_i64_u32"
	.skip 1
	.ascii	"uLE_param_i64_u32"
	.skip 1
	.ascii	"LE_var_i64_u32"
	.skip 1
	.ascii	"uLE_var_i64_u32"
	.skip 1
	.ascii	"LE_param_i64_i8"
	.skip 1
	.ascii	"uLE_param_i64_i8"
	.skip 1
	.ascii	"LE_var_i64_i8"
	.skip 1
	.ascii	"uLE_var_i64_i8"
	.skip 1
	.ascii	"LE_param_i64_L"
	.skip 1
	.ascii	"uLE_param_i64_L"
	.skip 1
	.ascii	"LE_var_i64_L"
	.skip 1
	.ascii	"uLE_var_i64_L"
	.skip 1
	.ascii	"LE_param_i64_u8"
	.skip 1
	.ascii	"uLE_param_i64_u8"
	.skip 1
	.ascii	"LE_var_i64_u8"
	.skip 1
	.ascii	"uLE_var_i64_u8"
	.skip 1
	.ascii	"LE_param_i64_C"
	.skip 1
	.ascii	"uLE_param_i64_C"
	.skip 1
	.ascii	"LE_var_i64_C"
	.skip 1
	.ascii	"uLE_var_i64_C"
	.skip 1
	.ascii	"LE_param_i64_i64"
	.skip 1
	.ascii	"uLE_param_i64_i64"
	.skip 1
	.ascii	"LE_var_i64_i64"
	.skip 1
	.ascii	"uLE_var_i64_i64"
	.skip 1
	.ascii	"LE_param_i64_I"
	.skip 1
	.ascii	"uLE_param_i64_I"
	.skip 1
	.ascii	"LE_var_i64_I"
	.skip 1
	.ascii	"uLE_var_i64_I"
	.skip 1
	.ascii	"LE_param_i64_i16"
	.skip 1
	.ascii	"uLE_param_i64_i16"
	.skip 1
	.ascii	"LE_var_i64_i16"
	.skip 1
	.ascii	"uLE_var_i64_i16"
	.skip 1
	.ascii	"LE_param_i64_i32"
	.skip 1
	.ascii	"uLE_param_i64_i32"
	.skip 1
	.ascii	"LE_var_i64_i32"
	.skip 1
	.ascii	"uLE_var_i64_i32"
	.skip 1
	.ascii	"LE_param_i64_LC"
	.skip 1
	.ascii	"uLE_param_i64_LC"
	.skip 1
	.ascii	"LE_var_i64_LC"
	.skip 1
	.ascii	"uLE_var_i64_LC"
	.skip 1
	.ascii	"LE_param_i64_u64"
	.skip 1
	.ascii	"uLE_param_i64_u64"
	.skip 1
	.ascii	"LE_var_i64_u64"
	.skip 1
	.ascii	"uLE_var_i64_u64"
	.skip 1
	.ascii	"LE_param_i64_u16"
	.skip 1
	.ascii	"uLE_param_i64_u16"
	.skip 1
	.ascii	"LE_var_i64_u16"
	.skip 1
	.ascii	"uLE_var_i64_u16"
	.skip 1
	.ascii	"LE_param_I_u32"
	.skip 1
	.ascii	"uLE_param_I_u32"
	.skip 1
	.ascii	"LE_var_I_u32"
	.skip 1
	.ascii	"uLE_var_I_u32"
	.skip 1
	.ascii	"LE_param_I_i8"
	.skip 1
	.ascii	"uLE_param_I_i8"
	.skip 1
	.ascii	"LE_var_I_i8"
	.skip 1
	.ascii	"uLE_var_I_i8"
	.skip 1
	.ascii	"LE_param_I_L"
	.skip 1
	.ascii	"uLE_param_I_L"
	.skip 1
	.ascii	"LE_var_I_L"
	.skip 1
	.ascii	"uLE_var_I_L"
	.skip 1
	.ascii	"LE_param_I_u8"
	.skip 1
	.ascii	"uLE_param_I_u8"
	.skip 1
	.ascii	"LE_var_I_u8"
	.skip 1
	.ascii	"uLE_var_I_u8"
	.skip 1
	.ascii	"LE_param_I_C"
	.skip 1
	.ascii	"uLE_param_I_C"
	.skip 1
	.ascii	"LE_var_I_C"
	.skip 1
	.ascii	"uLE_var_I_C"
	.skip 1
	.ascii	"LE_param_I_i64"
	.skip 1
	.ascii	"uLE_param_I_i64"
	.skip 1
	.ascii	"LE_var_I_i64"
	.skip 1
	.ascii	"uLE_var_I_i64"
	.skip 1
	.ascii	"LE_param_I_I"
	.skip 1
	.ascii	"uLE_param_I_I"
	.skip 1
	.ascii	"LE_var_I_I"
	.skip 1
	.ascii	"uLE_var_I_I"
	.skip 1
	.ascii	"LE_param_I_i16"
	.skip 1
	.ascii	"uLE_param_I_i16"
	.skip 1
	.ascii	"LE_var_I_i16"
	.skip 1
	.ascii	"uLE_var_I_i16"
	.skip 1
	.ascii	"LE_param_I_i32"
	.skip 1
	.ascii	"uLE_param_I_i32"
	.skip 1
	.ascii	"LE_var_I_i32"
	.skip 1
	.ascii	"uLE_var_I_i32"
	.skip 1
	.ascii	"LE_param_I_LC"
	.skip 1
	.ascii	"uLE_param_I_LC"
	.skip 1
	.ascii	"LE_var_I_LC"
	.skip 1
	.ascii	"uLE_var_I_LC"
	.skip 1
	.ascii	"LE_param_I_u64"
	.skip 1
	.ascii	"uLE_param_I_u64"
	.skip 1
	.ascii	"LE_var_I_u64"
	.skip 1
	.ascii	"uLE_var_I_u64"
	.skip 1
	.ascii	"LE_param_I_u16"
	.skip 1
	.ascii	"uLE_param_I_u16"
	.skip 1
	.ascii	"LE_var_I_u16"
	.skip 1
	.ascii	"uLE_var_I_u16"
	.skip 1
	.ascii	"LE_param_i16_u32"
	.skip 1
	.ascii	"uLE_param_i16_u32"
	.skip 1
	.ascii	"LE_var_i16_u32"
	.skip 1
	.ascii	"uLE_var_i16_u32"
	.skip 1
	.ascii	"LE_param_i16_i8"
	.skip 1
	.ascii	"uLE_param_i16_i8"
	.skip 1
	.ascii	"LE_var_i16_i8"
	.skip 1
	.ascii	"uLE_var_i16_i8"
	.skip 1
	.ascii	"LE_param_i16_L"
	.skip 1
	.ascii	"uLE_param_i16_L"
	.skip 1
	.ascii	"LE_var_i16_L"
	.skip 1
	.ascii	"uLE_var_i16_L"
	.skip 1
	.ascii	"LE_param_i16_u8"
	.skip 1
	.ascii	"uLE_param_i16_u8"
	.skip 1
	.ascii	"LE_var_i16_u8"
	.skip 1
	.ascii	"uLE_var_i16_u8"
	.skip 1
	.ascii	"LE_param_i16_C"
	.skip 1
	.ascii	"uLE_param_i16_C"
	.skip 1
	.ascii	"LE_var_i16_C"
	.skip 1
	.ascii	"uLE_var_i16_C"
	.skip 1
	.ascii	"LE_param_i16_i64"
	.skip 1
	.ascii	"uLE_param_i16_i64"
	.skip 1
	.ascii	"LE_var_i16_i64"
	.skip 1
	.ascii	"uLE_var_i16_i64"
	.skip 1
	.ascii	"LE_param_i16_I"
	.skip 1
	.ascii	"uLE_param_i16_I"
	.skip 1
	.ascii	"LE_var_i16_I"
	.skip 1
	.ascii	"uLE_var_i16_I"
	.skip 1
	.ascii	"LE_param_i16_i16"
	.skip 1
	.ascii	"uLE_param_i16_i16"
	.skip 1
	.ascii	"LE_var_i16_i16"
	.skip 1
	.ascii	"uLE_var_i16_i16"
	.skip 1
	.ascii	"LE_param_i16_i32"
	.skip 1
	.ascii	"uLE_param_i16_i32"
	.skip 1
	.ascii	"LE_var_i16_i32"
	.skip 1
	.ascii	"uLE_var_i16_i32"
	.skip 1
	.ascii	"LE_param_i16_LC"
	.skip 1
	.ascii	"uLE_param_i16_LC"
	.skip 1
	.ascii	"LE_var_i16_LC"
	.skip 1
	.ascii	"uLE_var_i16_LC"
	.skip 1
	.ascii	"LE_param_i16_u64"
	.skip 1
	.ascii	"uLE_param_i16_u64"
	.skip 1
	.ascii	"LE_var_i16_u64"
	.skip 1
	.ascii	"uLE_var_i16_u64"
	.skip 1
	.ascii	"LE_param_i16_u16"
	.skip 1
	.ascii	"uLE_param_i16_u16"
	.skip 1
	.ascii	"LE_var_i16_u16"
	.skip 1
	.ascii	"uLE_var_i16_u16"
	.skip 1
	.ascii	"LE_param_i32_u32"
	.skip 1
	.ascii	"uLE_param_i32_u32"
	.skip 1
	.ascii	"LE_var_i32_u32"
	.skip 1
	.ascii	"uLE_var_i32_u32"
	.skip 1
	.ascii	"LE_param_i32_i8"
	.skip 1
	.ascii	"uLE_param_i32_i8"
	.skip 1
	.ascii	"LE_var_i32_i8"
	.skip 1
	.ascii	"uLE_var_i32_i8"
	.skip 1
	.ascii	"LE_param_i32_L"
	.skip 1
	.ascii	"uLE_param_i32_L"
	.skip 1
	.ascii	"LE_var_i32_L"
	.skip 1
	.ascii	"uLE_var_i32_L"
	.skip 1
	.ascii	"LE_param_i32_u8"
	.skip 1
	.ascii	"uLE_param_i32_u8"
	.skip 1
	.ascii	"LE_var_i32_u8"
	.skip 1
	.ascii	"uLE_var_i32_u8"
	.skip 1
	.ascii	"LE_param_i32_C"
	.skip 1
	.ascii	"uLE_param_i32_C"
	.skip 1
	.ascii	"LE_var_i32_C"
	.skip 1
	.ascii	"uLE_var_i32_C"
	.skip 1
	.ascii	"LE_param_i32_i64"
	.skip 1
	.ascii	"uLE_param_i32_i64"
	.skip 1
	.ascii	"LE_var_i32_i64"
	.skip 1
	.ascii	"uLE_var_i32_i64"
	.skip 1
	.ascii	"LE_param_i32_I"
	.skip 1
	.ascii	"uLE_param_i32_I"
	.skip 1
	.ascii	"LE_var_i32_I"
	.skip 1
	.ascii	"uLE_var_i32_I"
	.skip 1
	.ascii	"LE_param_i32_i16"
	.skip 1
	.ascii	"uLE_param_i32_i16"
	.skip 1
	.ascii	"LE_var_i32_i16"
	.skip 1
	.ascii	"uLE_var_i32_i16"
	.skip 1
	.ascii	"LE_param_i32_i32"
	.skip 1
	.ascii	"uLE_param_i32_i32"
	.skip 1
	.ascii	"LE_var_i32_i32"
	.skip 1
	.ascii	"uLE_var_i32_i32"
	.skip 1
	.ascii	"LE_param_i32_LC"
	.skip 1
	.ascii	"uLE_param_i32_LC"
	.skip 1
	.ascii	"LE_var_i32_LC"
	.skip 1
	.ascii	"uLE_var_i32_LC"
	.skip 1
	.ascii	"LE_param_i32_u64"
	.skip 1
	.ascii	"uLE_param_i32_u64"
	.skip 1
	.ascii	"LE_var_i32_u64"
	.skip 1
	.ascii	"uLE_var_i32_u64"
	.skip 1
	.ascii	"LE_param_i32_u16"
	.skip 1
	.ascii	"uLE_param_i32_u16"
	.skip 1
	.ascii	"LE_var_i32_u16"
	.skip 1
	.ascii	"uLE_var_i32_u16"
	.skip 1
	.ascii	"LE_param_LC_u32"
	.skip 1
	.ascii	"uLE_param_LC_u32"
	.skip 1
	.ascii	"LE_var_LC_u32"
	.skip 1
	.ascii	"uLE_var_LC_u32"
	.skip 1
	.ascii	"LE_param_LC_i8"
	.skip 1
	.ascii	"uLE_param_LC_i8"
	.skip 1
	.ascii	"LE_var_LC_i8"
	.skip 1
	.ascii	"uLE_var_LC_i8"
	.skip 1
	.ascii	"LE_param_LC_L"
	.skip 1
	.ascii	"uLE_param_LC_L"
	.skip 1
	.ascii	"LE_var_LC_L"
	.skip 1
	.ascii	"uLE_var_LC_L"
	.skip 1
	.ascii	"LE_param_LC_u8"
	.skip 1
	.ascii	"uLE_param_LC_u8"
	.skip 1
	.ascii	"LE_var_LC_u8"
	.skip 1
	.ascii	"uLE_var_LC_u8"
	.skip 1
	.ascii	"LE_param_LC_C"
	.skip 1
	.ascii	"uLE_param_LC_C"
	.skip 1
	.ascii	"LE_var_LC_C"
	.skip 1
	.ascii	"uLE_var_LC_C"
	.skip 1
	.ascii	"LE_param_LC_i64"
	.skip 1
	.ascii	"uLE_param_LC_i64"
	.skip 1
	.ascii	"LE_var_LC_i64"
	.skip 1
	.ascii	"uLE_var_LC_i64"
	.skip 1
	.ascii	"LE_param_LC_I"
	.skip 1
	.ascii	"uLE_param_LC_I"
	.skip 1
	.ascii	"LE_var_LC_I"
	.skip 1
	.ascii	"uLE_var_LC_I"
	.skip 1
	.ascii	"LE_param_LC_i16"
	.skip 1
	.ascii	"uLE_param_LC_i16"
	.skip 1
	.ascii	"LE_var_LC_i16"
	.skip 1
	.ascii	"uLE_var_LC_i16"
	.skip 1
	.ascii	"LE_param_LC_i32"
	.skip 1
	.ascii	"uLE_param_LC_i32"
	.skip 1
	.ascii	"LE_var_LC_i32"
	.skip 1
	.ascii	"uLE_var_LC_i32"
	.skip 1
	.ascii	"LE_param_LC_LC"
	.skip 1
	.ascii	"uLE_param_LC_LC"
	.skip 1
	.ascii	"LE_var_LC_LC"
	.skip 1
	.ascii	"uLE_var_LC_LC"
	.skip 1
	.ascii	"LE_param_LC_u64"
	.skip 1
	.ascii	"uLE_param_LC_u64"
	.skip 1
	.ascii	"LE_var_LC_u64"
	.skip 1
	.ascii	"uLE_var_LC_u64"
	.skip 1
	.ascii	"LE_param_LC_u16"
	.skip 1
	.ascii	"uLE_param_LC_u16"
	.skip 1
	.ascii	"LE_var_LC_u16"
	.skip 1
	.ascii	"uLE_var_LC_u16"
	.skip 1
	.ascii	"LE_param_u64_u32"
	.skip 1
	.ascii	"uLE_param_u64_u32"
	.skip 1
	.ascii	"LE_var_u64_u32"
	.skip 1
	.ascii	"uLE_var_u64_u32"
	.skip 1
	.ascii	"LE_param_u64_i8"
	.skip 1
	.ascii	"uLE_param_u64_i8"
	.skip 1
	.ascii	"LE_var_u64_i8"
	.skip 1
	.ascii	"uLE_var_u64_i8"
	.skip 1
	.ascii	"LE_param_u64_L"
	.skip 1
	.ascii	"uLE_param_u64_L"
	.skip 1
	.ascii	"LE_var_u64_L"
	.skip 1
	.ascii	"uLE_var_u64_L"
	.skip 1
	.ascii	"LE_param_u64_u8"
	.skip 1
	.ascii	"uLE_param_u64_u8"
	.skip 1
	.ascii	"LE_var_u64_u8"
	.skip 1
	.ascii	"uLE_var_u64_u8"
	.skip 1
	.ascii	"LE_param_u64_C"
	.skip 1
	.ascii	"uLE_param_u64_C"
	.skip 1
	.ascii	"LE_var_u64_C"
	.skip 1
	.ascii	"uLE_var_u64_C"
	.skip 1
	.ascii	"LE_param_u64_i64"
	.skip 1
	.ascii	"uLE_param_u64_i64"
	.skip 1
	.ascii	"LE_var_u64_i64"
	.skip 1
	.ascii	"uLE_var_u64_i64"
	.skip 1
	.ascii	"LE_param_u64_I"
	.skip 1
	.ascii	"uLE_param_u64_I"
	.skip 1
	.ascii	"LE_var_u64_I"
	.skip 1
	.ascii	"uLE_var_u64_I"
	.skip 1
	.ascii	"LE_param_u64_i16"
	.skip 1
	.ascii	"uLE_param_u64_i16"
	.skip 1
	.ascii	"LE_var_u64_i16"
	.skip 1
	.ascii	"uLE_var_u64_i16"
	.skip 1
	.ascii	"LE_param_u64_i32"
	.skip 1
	.ascii	"uLE_param_u64_i32"
	.skip 1
	.ascii	"LE_var_u64_i32"
	.skip 1
	.ascii	"uLE_var_u64_i32"
	.skip 1
	.ascii	"LE_param_u64_LC"
	.skip 1
	.ascii	"uLE_param_u64_LC"
	.skip 1
	.ascii	"LE_var_u64_LC"
	.skip 1
	.ascii	"uLE_var_u64_LC"
	.skip 1
	.ascii	"LE_param_u64_u64"
	.skip 1
	.ascii	"uLE_param_u64_u64"
	.skip 1
	.ascii	"LE_var_u64_u64"
	.skip 1
	.ascii	"uLE_var_u64_u64"
	.skip 1
	.ascii	"LE_param_u64_u16"
	.skip 1
	.ascii	"uLE_param_u64_u16"
	.skip 1
	.ascii	"LE_var_u64_u16"
	.skip 1
	.ascii	"uLE_var_u64_u16"
	.skip 1
	.ascii	"LE_param_u16_u32"
	.skip 1
	.ascii	"uLE_param_u16_u32"
	.skip 1
	.ascii	"LE_var_u16_u32"
	.skip 1
	.ascii	"uLE_var_u16_u32"
	.skip 1
	.ascii	"LE_param_u16_i8"
	.skip 1
	.ascii	"uLE_param_u16_i8"
	.skip 1
	.ascii	"LE_var_u16_i8"
	.skip 1
	.ascii	"uLE_var_u16_i8"
	.skip 1
	.ascii	"LE_param_u16_L"
	.skip 1
	.ascii	"uLE_param_u16_L"
	.skip 1
	.ascii	"LE_var_u16_L"
	.skip 1
	.ascii	"uLE_var_u16_L"
	.skip 1
	.ascii	"LE_param_u16_u8"
	.skip 1
	.ascii	"uLE_param_u16_u8"
	.skip 1
	.ascii	"LE_var_u16_u8"
	.skip 1
	.ascii	"uLE_var_u16_u8"
	.skip 1
	.ascii	"LE_param_u16_C"
	.skip 1
	.ascii	"uLE_param_u16_C"
	.skip 1
	.ascii	"LE_var_u16_C"
	.skip 1
	.ascii	"uLE_var_u16_C"
	.skip 1
	.ascii	"LE_param_u16_i64"
	.skip 1
	.ascii	"uLE_param_u16_i64"
	.skip 1
	.ascii	"LE_var_u16_i64"
	.skip 1
	.ascii	"uLE_var_u16_i64"
	.skip 1
	.ascii	"LE_param_u16_I"
	.skip 1
	.ascii	"uLE_param_u16_I"
	.skip 1
	.ascii	"LE_var_u16_I"
	.skip 1
	.ascii	"uLE_var_u16_I"
	.skip 1
	.ascii	"LE_param_u16_i16"
	.skip 1
	.ascii	"uLE_param_u16_i16"
	.skip 1
	.ascii	"LE_var_u16_i16"
	.skip 1
	.ascii	"uLE_var_u16_i16"
	.skip 1
	.ascii	"LE_param_u16_i32"
	.skip 1
	.ascii	"uLE_param_u16_i32"
	.skip 1
	.ascii	"LE_var_u16_i32"
	.skip 1
	.ascii	"uLE_var_u16_i32"
	.skip 1
	.ascii	"LE_param_u16_LC"
	.skip 1
	.ascii	"uLE_param_u16_LC"
	.skip 1
	.ascii	"LE_var_u16_LC"
	.skip 1
	.ascii	"uLE_var_u16_LC"
	.skip 1
	.ascii	"LE_param_u16_u64"
	.skip 1
	.ascii	"uLE_param_u16_u64"
	.skip 1
	.ascii	"LE_var_u16_u64"
	.skip 1
	.ascii	"uLE_var_u16_u64"
	.skip 1
	.ascii	"LE_param_u16_u16"
	.skip 1
	.ascii	"uLE_param_u16_u16"
	.skip 1
	.ascii	"LE_var_u16_u16"
	.skip 1
	.ascii	"uLE_var_u16_u16"
	.skip 3
	.long	LE_M3
	.long	L_1
	.long	LE__LE_param_f32_f32
	.long	L_1+6
	.long	LE__LE_var_f32_f32
	.long	L_1+23
	.long	LE__LE_param_u32_u32
	.long	L_1+38
	.long	LE__uLE_param_u32_u32
	.long	L_1+55
	.long	LE__LE_var_u32_u32
	.long	L_1+73
	.long	LE__uLE_var_u32_u32
	.long	L_1+88
	.long	LE__LE_param_u32_i8
	.long	L_1+104
	.long	LE__uLE_param_u32_i8
	.long	L_1+120
	.long	LE__LE_var_u32_i8
	.long	L_1+137
	.long	LE__uLE_var_u32_i8
	.long	L_1+151
	.long	LE__LE_param_u32_L
	.long	L_1+166
	.long	LE__uLE_param_u32_L
	.long	L_1+181
	.long	LE__LE_var_u32_L
	.long	L_1+197
	.long	LE__uLE_var_u32_L
	.long	L_1+210
	.long	LE__LE_param_u32_u8
	.long	L_1+224
	.long	LE__uLE_param_u32_u8
	.long	L_1+240
	.long	LE__LE_var_u32_u8
	.long	L_1+257
	.long	LE__uLE_var_u32_u8
	.long	L_1+271
	.long	LE__LE_param_u32_C
	.long	L_1+286
	.long	LE__uLE_param_u32_C
	.long	L_1+301
	.long	LE__LE_var_u32_C
	.long	L_1+317
	.long	LE__uLE_var_u32_C
	.long	L_1+330
	.long	LE__LE_param_u32_i64
	.long	L_1+344
	.long	LE__uLE_param_u32_i64
	.long	L_1+361
	.long	LE__LE_var_u32_i64
	.long	L_1+379
	.long	LE__uLE_var_u32_i64
	.long	L_1+394
	.long	LE__LE_param_u32_I
	.long	L_1+410
	.long	LE__uLE_param_u32_I
	.long	L_1+425
	.long	LE__LE_var_u32_I
	.long	L_1+441
	.long	LE__uLE_var_u32_I
	.long	L_1+454
	.long	LE__LE_param_u32_i16
	.long	L_1+468
	.long	LE__uLE_param_u32_i16
	.long	L_1+485
	.long	LE__LE_var_u32_i16
	.long	L_1+503
	.long	LE__uLE_var_u32_i16
	.long	L_1+518
	.long	LE__LE_param_u32_i32
	.long	L_1+534
	.long	LE__uLE_param_u32_i32
	.long	L_1+551
	.long	LE__LE_var_u32_i32
	.long	L_1+569
	.long	LE__uLE_var_u32_i32
	.long	L_1+584
	.long	LE__LE_param_u32_LC
	.long	L_1+600
	.long	LE__uLE_param_u32_LC
	.long	L_1+616
	.long	LE__LE_var_u32_LC
	.long	L_1+633
	.long	LE__uLE_var_u32_LC
	.long	L_1+647
	.long	LE__LE_param_u32_u64
	.long	L_1+662
	.long	LE__uLE_param_u32_u64
	.long	L_1+679
	.long	LE__LE_var_u32_u64
	.long	L_1+697
	.long	LE__uLE_var_u32_u64
	.long	L_1+712
	.long	LE__LE_param_u32_u16
	.long	L_1+728
	.long	LE__uLE_param_u32_u16
	.long	L_1+745
	.long	LE__LE_var_u32_u16
	.long	L_1+763
	.long	LE__uLE_var_u32_u16
	.long	L_1+778
	.long	LE__LE_param_i8_u32
	.long	L_1+794
	.long	LE__uLE_param_i8_u32
	.long	L_1+810
	.long	LE__LE_var_i8_u32
	.long	L_1+827
	.long	LE__uLE_var_i8_u32
	.long	L_1+841
	.long	LE__LE_param_i8_i8
	.long	L_1+856
	.long	LE__uLE_param_i8_i8
	.long	L_1+871
	.long	LE__LE_var_i8_i8
	.long	L_1+887
	.long	LE__uLE_var_i8_i8
	.long	L_1+900
	.long	LE__LE_param_i8_L
	.long	L_1+914
	.long	LE__uLE_param_i8_L
	.long	L_1+928
	.long	LE__LE_var_i8_L
	.long	L_1+943
	.long	LE__uLE_var_i8_L
	.long	L_1+955
	.long	LE__LE_param_i8_u8
	.long	L_1+968
	.long	LE__uLE_param_i8_u8
	.long	L_1+983
	.long	LE__LE_var_i8_u8
	.long	L_1+999
	.long	LE__uLE_var_i8_u8
	.long	L_1+1012
	.long	LE__LE_param_i8_C
	.long	L_1+1026
	.long	LE__uLE_param_i8_C
	.long	L_1+1040
	.long	LE__LE_var_i8_C
	.long	L_1+1055
	.long	LE__uLE_var_i8_C
	.long	L_1+1067
	.long	LE__LE_param_i8_i64
	.long	L_1+1080
	.long	LE__uLE_param_i8_i64
	.long	L_1+1096
	.long	LE__LE_var_i8_i64
	.long	L_1+1113
	.long	LE__uLE_var_i8_i64
	.long	L_1+1127
	.long	LE__LE_param_i8_I
	.long	L_1+1142
	.long	LE__uLE_param_i8_I
	.long	L_1+1156
	.long	LE__LE_var_i8_I
	.long	L_1+1171
	.long	LE__uLE_var_i8_I
	.long	L_1+1183
	.long	LE__LE_param_i8_i16
	.long	L_1+1196
	.long	LE__uLE_param_i8_i16
	.long	L_1+1212
	.long	LE__LE_var_i8_i16
	.long	L_1+1229
	.long	LE__uLE_var_i8_i16
	.long	L_1+1243
	.long	LE__LE_param_i8_i32
	.long	L_1+1258
	.long	LE__uLE_param_i8_i32
	.long	L_1+1274
	.long	LE__LE_var_i8_i32
	.long	L_1+1291
	.long	LE__uLE_var_i8_i32
	.long	L_1+1305
	.long	LE__LE_param_i8_LC
	.long	L_1+1320
	.long	LE__uLE_param_i8_LC
	.long	L_1+1335
	.long	LE__LE_var_i8_LC
	.long	L_1+1351
	.long	LE__uLE_var_i8_LC
	.long	L_1+1364
	.long	LE__LE_param_i8_u64
	.long	L_1+1378
	.long	LE__uLE_param_i8_u64
	.long	L_1+1394
	.long	LE__LE_var_i8_u64
	.long	L_1+1411
	.long	LE__uLE_var_i8_u64
	.long	L_1+1425
	.long	LE__LE_param_i8_u16
	.long	L_1+1440
	.long	LE__uLE_param_i8_u16
	.long	L_1+1456
	.long	LE__LE_var_i8_u16
	.long	L_1+1473
	.long	LE__uLE_var_i8_u16
	.long	L_1+1487
	.long	LE__LE_param_L_u32
	.long	L_1+1502
	.long	LE__uLE_param_L_u32
	.long	L_1+1517
	.long	LE__LE_var_L_u32
	.long	L_1+1533
	.long	LE__uLE_var_L_u32
	.long	L_1+1546
	.long	LE__LE_param_L_i8
	.long	L_1+1560
	.long	LE__uLE_param_L_i8
	.long	L_1+1574
	.long	LE__LE_var_L_i8
	.long	L_1+1589
	.long	LE__uLE_var_L_i8
	.long	L_1+1601
	.long	LE__LE_param_L_L
	.long	L_1+1614
	.long	LE__uLE_param_L_L
	.long	L_1+1627
	.long	LE__LE_var_L_L
	.long	L_1+1641
	.long	LE__uLE_var_L_L
	.long	L_1+1652
	.long	LE__LE_param_L_u8
	.long	L_1+1664
	.long	LE__uLE_param_L_u8
	.long	L_1+1678
	.long	LE__LE_var_L_u8
	.long	L_1+1693
	.long	LE__uLE_var_L_u8
	.long	L_1+1705
	.long	LE__LE_param_L_C
	.long	L_1+1718
	.long	LE__uLE_param_L_C
	.long	L_1+1731
	.long	LE__LE_var_L_C
	.long	L_1+1745
	.long	LE__uLE_var_L_C
	.long	L_1+1756
	.long	LE__LE_param_L_i64
	.long	L_1+1768
	.long	LE__uLE_param_L_i64
	.long	L_1+1783
	.long	LE__LE_var_L_i64
	.long	L_1+1799
	.long	LE__uLE_var_L_i64
	.long	L_1+1812
	.long	LE__LE_param_L_I
	.long	L_1+1826
	.long	LE__uLE_param_L_I
	.long	L_1+1839
	.long	LE__LE_var_L_I
	.long	L_1+1853
	.long	LE__uLE_var_L_I
	.long	L_1+1864
	.long	LE__LE_param_L_i16
	.long	L_1+1876
	.long	LE__uLE_param_L_i16
	.long	L_1+1891
	.long	LE__LE_var_L_i16
	.long	L_1+1907
	.long	LE__uLE_var_L_i16
	.long	L_1+1920
	.long	LE__LE_param_L_i32
	.long	L_1+1934
	.long	LE__uLE_param_L_i32
	.long	L_1+1949
	.long	LE__LE_var_L_i32
	.long	L_1+1965
	.long	LE__uLE_var_L_i32
	.long	L_1+1978
	.long	LE__LE_param_L_LC
	.long	L_1+1992
	.long	LE__uLE_param_L_LC
	.long	L_1+2006
	.long	LE__LE_var_L_LC
	.long	L_1+2021
	.long	LE__uLE_var_L_LC
	.long	L_1+2033
	.long	LE__LE_param_L_u64
	.long	L_1+2046
	.long	LE__uLE_param_L_u64
	.long	L_1+2061
	.long	LE__LE_var_L_u64
	.long	L_1+2077
	.long	LE__uLE_var_L_u64
	.long	L_1+2090
	.long	LE__LE_param_L_u16
	.long	L_1+2104
	.long	LE__uLE_param_L_u16
	.long	L_1+2119
	.long	LE__LE_var_L_u16
	.long	L_1+2135
	.long	LE__uLE_var_L_u16
	.long	L_1+2148
	.long	LE__LE_param_u8_u32
	.long	L_1+2162
	.long	LE__uLE_param_u8_u32
	.long	L_1+2178
	.long	LE__LE_var_u8_u32
	.long	L_1+2195
	.long	LE__uLE_var_u8_u32
	.long	L_1+2209
	.long	LE__LE_param_u8_i8
	.long	L_1+2224
	.long	LE__uLE_param_u8_i8
	.long	L_1+2239
	.long	LE__LE_var_u8_i8
	.long	L_1+2255
	.long	LE__uLE_var_u8_i8
	.long	L_1+2268
	.long	LE__LE_param_u8_L
	.long	L_1+2282
	.long	LE__uLE_param_u8_L
	.long	L_1+2296
	.long	LE__LE_var_u8_L
	.long	L_1+2311
	.long	LE__uLE_var_u8_L
	.long	L_1+2323
	.long	LE__LE_param_u8_u8
	.long	L_1+2336
	.long	LE__uLE_param_u8_u8
	.long	L_1+2351
	.long	LE__LE_var_u8_u8
	.long	L_1+2367
	.long	LE__uLE_var_u8_u8
	.long	L_1+2380
	.long	LE__LE_param_u8_C
	.long	L_1+2394
	.long	LE__uLE_param_u8_C
	.long	L_1+2408
	.long	LE__LE_var_u8_C
	.long	L_1+2423
	.long	LE__uLE_var_u8_C
	.long	L_1+2435
	.long	LE__LE_param_u8_i64
	.long	L_1+2448
	.long	LE__uLE_param_u8_i64
	.long	L_1+2464
	.long	LE__LE_var_u8_i64
	.long	L_1+2481
	.long	LE__uLE_var_u8_i64
	.long	L_1+2495
	.long	LE__LE_param_u8_I
	.long	L_1+2510
	.long	LE__uLE_param_u8_I
	.long	L_1+2524
	.long	LE__LE_var_u8_I
	.long	L_1+2539
	.long	LE__uLE_var_u8_I
	.long	L_1+2551
	.long	LE__LE_param_u8_i16
	.long	L_1+2564
	.long	LE__uLE_param_u8_i16
	.long	L_1+2580
	.long	LE__LE_var_u8_i16
	.long	L_1+2597
	.long	LE__uLE_var_u8_i16
	.long	L_1+2611
	.long	LE__LE_param_u8_i32
	.long	L_1+2626
	.long	LE__uLE_param_u8_i32
	.long	L_1+2642
	.long	LE__LE_var_u8_i32
	.long	L_1+2659
	.long	LE__uLE_var_u8_i32
	.long	L_1+2673
	.long	LE__LE_param_u8_LC
	.long	L_1+2688
	.long	LE__uLE_param_u8_LC
	.long	L_1+2703
	.long	LE__LE_var_u8_LC
	.long	L_1+2719
	.long	LE__uLE_var_u8_LC
	.long	L_1+2732
	.long	LE__LE_param_u8_u64
	.long	L_1+2746
	.long	LE__uLE_param_u8_u64
	.long	L_1+2762
	.long	LE__LE_var_u8_u64
	.long	L_1+2779
	.long	LE__uLE_var_u8_u64
	.long	L_1+2793
	.long	LE__LE_param_u8_u16
	.long	L_1+2808
	.long	LE__uLE_param_u8_u16
	.long	L_1+2824
	.long	LE__LE_var_u8_u16
	.long	L_1+2841
	.long	LE__uLE_var_u8_u16
	.long	L_1+2855
	.long	LE__LE_param_f64_f64
	.long	L_1+2870
	.long	LE__LE_var_f64_f64
	.long	L_1+2887
	.long	LE__LE_param_C_u32
	.long	L_1+2902
	.long	LE__uLE_param_C_u32
	.long	L_1+2917
	.long	LE__LE_var_C_u32
	.long	L_1+2933
	.long	LE__uLE_var_C_u32
	.long	L_1+2946
	.long	LE__LE_param_C_i8
	.long	L_1+2960
	.long	LE__uLE_param_C_i8
	.long	L_1+2974
	.long	LE__LE_var_C_i8
	.long	L_1+2989
	.long	LE__uLE_var_C_i8
	.long	L_1+3001
	.long	LE__LE_param_C_L
	.long	L_1+3014
	.long	LE__uLE_param_C_L
	.long	L_1+3027
	.long	LE__LE_var_C_L
	.long	L_1+3041
	.long	LE__uLE_var_C_L
	.long	L_1+3052
	.long	LE__LE_param_C_u8
	.long	L_1+3064
	.long	LE__uLE_param_C_u8
	.long	L_1+3078
	.long	LE__LE_var_C_u8
	.long	L_1+3093
	.long	LE__uLE_var_C_u8
	.long	L_1+3105
	.long	LE__LE_param_C_C
	.long	L_1+3118
	.long	LE__uLE_param_C_C
	.long	L_1+3131
	.long	LE__LE_var_C_C
	.long	L_1+3145
	.long	LE__uLE_var_C_C
	.long	L_1+3156
	.long	LE__LE_param_C_i64
	.long	L_1+3168
	.long	LE__uLE_param_C_i64
	.long	L_1+3183
	.long	LE__LE_var_C_i64
	.long	L_1+3199
	.long	LE__uLE_var_C_i64
	.long	L_1+3212
	.long	LE__LE_param_C_I
	.long	L_1+3226
	.long	LE__uLE_param_C_I
	.long	L_1+3239
	.long	LE__LE_var_C_I
	.long	L_1+3253
	.long	LE__uLE_var_C_I
	.long	L_1+3264
	.long	LE__LE_param_C_i16
	.long	L_1+3276
	.long	LE__uLE_param_C_i16
	.long	L_1+3291
	.long	LE__LE_var_C_i16
	.long	L_1+3307
	.long	LE__uLE_var_C_i16
	.long	L_1+3320
	.long	LE__LE_param_C_i32
	.long	L_1+3334
	.long	LE__uLE_param_C_i32
	.long	L_1+3349
	.long	LE__LE_var_C_i32
	.long	L_1+3365
	.long	LE__uLE_var_C_i32
	.long	L_1+3378
	.long	LE__LE_param_C_LC
	.long	L_1+3392
	.long	LE__uLE_param_C_LC
	.long	L_1+3406
	.long	LE__LE_var_C_LC
	.long	L_1+3421
	.long	LE__uLE_var_C_LC
	.long	L_1+3433
	.long	LE__LE_param_C_u64
	.long	L_1+3446
	.long	LE__uLE_param_C_u64
	.long	L_1+3461
	.long	LE__LE_var_C_u64
	.long	L_1+3477
	.long	LE__uLE_var_C_u64
	.long	L_1+3490
	.long	LE__LE_param_C_u16
	.long	L_1+3504
	.long	LE__uLE_param_C_u16
	.long	L_1+3519
	.long	LE__LE_var_C_u16
	.long	L_1+3535
	.long	LE__uLE_var_C_u16
	.long	L_1+3548
	.long	LE__LE_param_i64_u32
	.long	L_1+3562
	.long	LE__uLE_param_i64_u32
	.long	L_1+3579
	.long	LE__LE_var_i64_u32
	.long	L_1+3597
	.long	LE__uLE_var_i64_u32
	.long	L_1+3612
	.long	LE__LE_param_i64_i8
	.long	L_1+3628
	.long	LE__uLE_param_i64_i8
	.long	L_1+3644
	.long	LE__LE_var_i64_i8
	.long	L_1+3661
	.long	LE__uLE_var_i64_i8
	.long	L_1+3675
	.long	LE__LE_param_i64_L
	.long	L_1+3690
	.long	LE__uLE_param_i64_L
	.long	L_1+3705
	.long	LE__LE_var_i64_L
	.long	L_1+3721
	.long	LE__uLE_var_i64_L
	.long	L_1+3734
	.long	LE__LE_param_i64_u8
	.long	L_1+3748
	.long	LE__uLE_param_i64_u8
	.long	L_1+3764
	.long	LE__LE_var_i64_u8
	.long	L_1+3781
	.long	LE__uLE_var_i64_u8
	.long	L_1+3795
	.long	LE__LE_param_i64_C
	.long	L_1+3810
	.long	LE__uLE_param_i64_C
	.long	L_1+3825
	.long	LE__LE_var_i64_C
	.long	L_1+3841
	.long	LE__uLE_var_i64_C
	.long	L_1+3854
	.long	LE__LE_param_i64_i64
	.long	L_1+3868
	.long	LE__uLE_param_i64_i64
	.long	L_1+3885
	.long	LE__LE_var_i64_i64
	.long	L_1+3903
	.long	LE__uLE_var_i64_i64
	.long	L_1+3918
	.long	LE__LE_param_i64_I
	.long	L_1+3934
	.long	LE__uLE_param_i64_I
	.long	L_1+3949
	.long	LE__LE_var_i64_I
	.long	L_1+3965
	.long	LE__uLE_var_i64_I
	.long	L_1+3978
	.long	LE__LE_param_i64_i16
	.long	L_1+3992
	.long	LE__uLE_param_i64_i16
	.long	L_1+4009
	.long	LE__LE_var_i64_i16
	.long	L_1+4027
	.long	LE__uLE_var_i64_i16
	.long	L_1+4042
	.long	LE__LE_param_i64_i32
	.long	L_1+4058
	.long	LE__uLE_param_i64_i32
	.long	L_1+4075
	.long	LE__LE_var_i64_i32
	.long	L_1+4093
	.long	LE__uLE_var_i64_i32
	.long	L_1+4108
	.long	LE__LE_param_i64_LC
	.long	L_1+4124
	.long	LE__uLE_param_i64_LC
	.long	L_1+4140
	.long	LE__LE_var_i64_LC
	.long	L_1+4157
	.long	LE__uLE_var_i64_LC
	.long	L_1+4171
	.long	LE__LE_param_i64_u64
	.long	L_1+4186
	.long	LE__uLE_param_i64_u64
	.long	L_1+4203
	.long	LE__LE_var_i64_u64
	.long	L_1+4221
	.long	LE__uLE_var_i64_u64
	.long	L_1+4236
	.long	LE__LE_param_i64_u16
	.long	L_1+4252
	.long	LE__uLE_param_i64_u16
	.long	L_1+4269
	.long	LE__LE_var_i64_u16
	.long	L_1+4287
	.long	LE__uLE_var_i64_u16
	.long	L_1+4302
	.long	LE__LE_param_I_u32
	.long	L_1+4318
	.long	LE__uLE_param_I_u32
	.long	L_1+4333
	.long	LE__LE_var_I_u32
	.long	L_1+4349
	.long	LE__uLE_var_I_u32
	.long	L_1+4362
	.long	LE__LE_param_I_i8
	.long	L_1+4376
	.long	LE__uLE_param_I_i8
	.long	L_1+4390
	.long	LE__LE_var_I_i8
	.long	L_1+4405
	.long	LE__uLE_var_I_i8
	.long	L_1+4417
	.long	LE__LE_param_I_L
	.long	L_1+4430
	.long	LE__uLE_param_I_L
	.long	L_1+4443
	.long	LE__LE_var_I_L
	.long	L_1+4457
	.long	LE__uLE_var_I_L
	.long	L_1+4468
	.long	LE__LE_param_I_u8
	.long	L_1+4480
	.long	LE__uLE_param_I_u8
	.long	L_1+4494
	.long	LE__LE_var_I_u8
	.long	L_1+4509
	.long	LE__uLE_var_I_u8
	.long	L_1+4521
	.long	LE__LE_param_I_C
	.long	L_1+4534
	.long	LE__uLE_param_I_C
	.long	L_1+4547
	.long	LE__LE_var_I_C
	.long	L_1+4561
	.long	LE__uLE_var_I_C
	.long	L_1+4572
	.long	LE__LE_param_I_i64
	.long	L_1+4584
	.long	LE__uLE_param_I_i64
	.long	L_1+4599
	.long	LE__LE_var_I_i64
	.long	L_1+4615
	.long	LE__uLE_var_I_i64
	.long	L_1+4628
	.long	LE__LE_param_I_I
	.long	L_1+4642
	.long	LE__uLE_param_I_I
	.long	L_1+4655
	.long	LE__LE_var_I_I
	.long	L_1+4669
	.long	LE__uLE_var_I_I
	.long	L_1+4680
	.long	LE__LE_param_I_i16
	.long	L_1+4692
	.long	LE__uLE_param_I_i16
	.long	L_1+4707
	.long	LE__LE_var_I_i16
	.long	L_1+4723
	.long	LE__uLE_var_I_i16
	.long	L_1+4736
	.long	LE__LE_param_I_i32
	.long	L_1+4750
	.long	LE__uLE_param_I_i32
	.long	L_1+4765
	.long	LE__LE_var_I_i32
	.long	L_1+4781
	.long	LE__uLE_var_I_i32
	.long	L_1+4794
	.long	LE__LE_param_I_LC
	.long	L_1+4808
	.long	LE__uLE_param_I_LC
	.long	L_1+4822
	.long	LE__LE_var_I_LC
	.long	L_1+4837
	.long	LE__uLE_var_I_LC
	.long	L_1+4849
	.long	LE__LE_param_I_u64
	.long	L_1+4862
	.long	LE__uLE_param_I_u64
	.long	L_1+4877
	.long	LE__LE_var_I_u64
	.long	L_1+4893
	.long	LE__uLE_var_I_u64
	.long	L_1+4906
	.long	LE__LE_param_I_u16
	.long	L_1+4920
	.long	LE__uLE_param_I_u16
	.long	L_1+4935
	.long	LE__LE_var_I_u16
	.long	L_1+4951
	.long	LE__uLE_var_I_u16
	.long	L_1+4964
	.long	LE__LE_param_i16_u32
	.long	L_1+4978
	.long	LE__uLE_param_i16_u32
	.long	L_1+4995
	.long	LE__LE_var_i16_u32
	.long	L_1+5013
	.long	LE__uLE_var_i16_u32
	.long	L_1+5028
	.long	LE__LE_param_i16_i8
	.long	L_1+5044
	.long	LE__uLE_param_i16_i8
	.long	L_1+5060
	.long	LE__LE_var_i16_i8
	.long	L_1+5077
	.long	LE__uLE_var_i16_i8
	.long	L_1+5091
	.long	LE__LE_param_i16_L
	.long	L_1+5106
	.long	LE__uLE_param_i16_L
	.long	L_1+5121
	.long	LE__LE_var_i16_L
	.long	L_1+5137
	.long	LE__uLE_var_i16_L
	.long	L_1+5150
	.long	LE__LE_param_i16_u8
	.long	L_1+5164
	.long	LE__uLE_param_i16_u8
	.long	L_1+5180
	.long	LE__LE_var_i16_u8
	.long	L_1+5197
	.long	LE__uLE_var_i16_u8
	.long	L_1+5211
	.long	LE__LE_param_i16_C
	.long	L_1+5226
	.long	LE__uLE_param_i16_C
	.long	L_1+5241
	.long	LE__LE_var_i16_C
	.long	L_1+5257
	.long	LE__uLE_var_i16_C
	.long	L_1+5270
	.long	LE__LE_param_i16_i64
	.long	L_1+5284
	.long	LE__uLE_param_i16_i64
	.long	L_1+5301
	.long	LE__LE_var_i16_i64
	.long	L_1+5319
	.long	LE__uLE_var_i16_i64
	.long	L_1+5334
	.long	LE__LE_param_i16_I
	.long	L_1+5350
	.long	LE__uLE_param_i16_I
	.long	L_1+5365
	.long	LE__LE_var_i16_I
	.long	L_1+5381
	.long	LE__uLE_var_i16_I
	.long	L_1+5394
	.long	LE__LE_param_i16_i16
	.long	L_1+5408
	.long	LE__uLE_param_i16_i16
	.long	L_1+5425
	.long	LE__LE_var_i16_i16
	.long	L_1+5443
	.long	LE__uLE_var_i16_i16
	.long	L_1+5458
	.long	LE__LE_param_i16_i32
	.long	L_1+5474
	.long	LE__uLE_param_i16_i32
	.long	L_1+5491
	.long	LE__LE_var_i16_i32
	.long	L_1+5509
	.long	LE__uLE_var_i16_i32
	.long	L_1+5524
	.long	LE__LE_param_i16_LC
	.long	L_1+5540
	.long	LE__uLE_param_i16_LC
	.long	L_1+5556
	.long	LE__LE_var_i16_LC
	.long	L_1+5573
	.long	LE__uLE_var_i16_LC
	.long	L_1+5587
	.long	LE__LE_param_i16_u64
	.long	L_1+5602
	.long	LE__uLE_param_i16_u64
	.long	L_1+5619
	.long	LE__LE_var_i16_u64
	.long	L_1+5637
	.long	LE__uLE_var_i16_u64
	.long	L_1+5652
	.long	LE__LE_param_i16_u16
	.long	L_1+5668
	.long	LE__uLE_param_i16_u16
	.long	L_1+5685
	.long	LE__LE_var_i16_u16
	.long	L_1+5703
	.long	LE__uLE_var_i16_u16
	.long	L_1+5718
	.long	LE__LE_param_i32_u32
	.long	L_1+5734
	.long	LE__uLE_param_i32_u32
	.long	L_1+5751
	.long	LE__LE_var_i32_u32
	.long	L_1+5769
	.long	LE__uLE_var_i32_u32
	.long	L_1+5784
	.long	LE__LE_param_i32_i8
	.long	L_1+5800
	.long	LE__uLE_param_i32_i8
	.long	L_1+5816
	.long	LE__LE_var_i32_i8
	.long	L_1+5833
	.long	LE__uLE_var_i32_i8
	.long	L_1+5847
	.long	LE__LE_param_i32_L
	.long	L_1+5862
	.long	LE__uLE_param_i32_L
	.long	L_1+5877
	.long	LE__LE_var_i32_L
	.long	L_1+5893
	.long	LE__uLE_var_i32_L
	.long	L_1+5906
	.long	LE__LE_param_i32_u8
	.long	L_1+5920
	.long	LE__uLE_param_i32_u8
	.long	L_1+5936
	.long	LE__LE_var_i32_u8
	.long	L_1+5953
	.long	LE__uLE_var_i32_u8
	.long	L_1+5967
	.long	LE__LE_param_i32_C
	.long	L_1+5982
	.long	LE__uLE_param_i32_C
	.long	L_1+5997
	.long	LE__LE_var_i32_C
	.long	L_1+6013
	.long	LE__uLE_var_i32_C
	.long	L_1+6026
	.long	LE__LE_param_i32_i64
	.long	L_1+6040
	.long	LE__uLE_param_i32_i64
	.long	L_1+6057
	.long	LE__LE_var_i32_i64
	.long	L_1+6075
	.long	LE__uLE_var_i32_i64
	.long	L_1+6090
	.long	LE__LE_param_i32_I
	.long	L_1+6106
	.long	LE__uLE_param_i32_I
	.long	L_1+6121
	.long	LE__LE_var_i32_I
	.long	L_1+6137
	.long	LE__uLE_var_i32_I
	.long	L_1+6150
	.long	LE__LE_param_i32_i16
	.long	L_1+6164
	.long	LE__uLE_param_i32_i16
	.long	L_1+6181
	.long	LE__LE_var_i32_i16
	.long	L_1+6199
	.long	LE__uLE_var_i32_i16
	.long	L_1+6214
	.long	LE__LE_param_i32_i32
	.long	L_1+6230
	.long	LE__uLE_param_i32_i32
	.long	L_1+6247
	.long	LE__LE_var_i32_i32
	.long	L_1+6265
	.long	LE__uLE_var_i32_i32
	.long	L_1+6280
	.long	LE__LE_param_i32_LC
	.long	L_1+6296
	.long	LE__uLE_param_i32_LC
	.long	L_1+6312
	.long	LE__LE_var_i32_LC
	.long	L_1+6329
	.long	LE__uLE_var_i32_LC
	.long	L_1+6343
	.long	LE__LE_param_i32_u64
	.long	L_1+6358
	.long	LE__uLE_param_i32_u64
	.long	L_1+6375
	.long	LE__LE_var_i32_u64
	.long	L_1+6393
	.long	LE__uLE_var_i32_u64
	.long	L_1+6408
	.long	LE__LE_param_i32_u16
	.long	L_1+6424
	.long	LE__uLE_param_i32_u16
	.long	L_1+6441
	.long	LE__LE_var_i32_u16
	.long	L_1+6459
	.long	LE__uLE_var_i32_u16
	.long	L_1+6474
	.long	LE__LE_param_LC_u32
	.long	L_1+6490
	.long	LE__uLE_param_LC_u32
	.long	L_1+6506
	.long	LE__LE_var_LC_u32
	.long	L_1+6523
	.long	LE__uLE_var_LC_u32
	.long	L_1+6537
	.long	LE__LE_param_LC_i8
	.long	L_1+6552
	.long	LE__uLE_param_LC_i8
	.long	L_1+6567
	.long	LE__LE_var_LC_i8
	.long	L_1+6583
	.long	LE__uLE_var_LC_i8
	.long	L_1+6596
	.long	LE__LE_param_LC_L
	.long	L_1+6610
	.long	LE__uLE_param_LC_L
	.long	L_1+6624
	.long	LE__LE_var_LC_L
	.long	L_1+6639
	.long	LE__uLE_var_LC_L
	.long	L_1+6651
	.long	LE__LE_param_LC_u8
	.long	L_1+6664
	.long	LE__uLE_param_LC_u8
	.long	L_1+6679
	.long	LE__LE_var_LC_u8
	.long	L_1+6695
	.long	LE__uLE_var_LC_u8
	.long	L_1+6708
	.long	LE__LE_param_LC_C
	.long	L_1+6722
	.long	LE__uLE_param_LC_C
	.long	L_1+6736
	.long	LE__LE_var_LC_C
	.long	L_1+6751
	.long	LE__uLE_var_LC_C
	.long	L_1+6763
	.long	LE__LE_param_LC_i64
	.long	L_1+6776
	.long	LE__uLE_param_LC_i64
	.long	L_1+6792
	.long	LE__LE_var_LC_i64
	.long	L_1+6809
	.long	LE__uLE_var_LC_i64
	.long	L_1+6823
	.long	LE__LE_param_LC_I
	.long	L_1+6838
	.long	LE__uLE_param_LC_I
	.long	L_1+6852
	.long	LE__LE_var_LC_I
	.long	L_1+6867
	.long	LE__uLE_var_LC_I
	.long	L_1+6879
	.long	LE__LE_param_LC_i16
	.long	L_1+6892
	.long	LE__uLE_param_LC_i16
	.long	L_1+6908
	.long	LE__LE_var_LC_i16
	.long	L_1+6925
	.long	LE__uLE_var_LC_i16
	.long	L_1+6939
	.long	LE__LE_param_LC_i32
	.long	L_1+6954
	.long	LE__uLE_param_LC_i32
	.long	L_1+6970
	.long	LE__LE_var_LC_i32
	.long	L_1+6987
	.long	LE__uLE_var_LC_i32
	.long	L_1+7001
	.long	LE__LE_param_LC_LC
	.long	L_1+7016
	.long	LE__uLE_param_LC_LC
	.long	L_1+7031
	.long	LE__LE_var_LC_LC
	.long	L_1+7047
	.long	LE__uLE_var_LC_LC
	.long	L_1+7060
	.long	LE__LE_param_LC_u64
	.long	L_1+7074
	.long	LE__uLE_param_LC_u64
	.long	L_1+7090
	.long	LE__LE_var_LC_u64
	.long	L_1+7107
	.long	LE__uLE_var_LC_u64
	.long	L_1+7121
	.long	LE__LE_param_LC_u16
	.long	L_1+7136
	.long	LE__uLE_param_LC_u16
	.long	L_1+7152
	.long	LE__LE_var_LC_u16
	.long	L_1+7169
	.long	LE__uLE_var_LC_u16
	.long	L_1+7183
	.long	LE__LE_param_u64_u32
	.long	L_1+7198
	.long	LE__uLE_param_u64_u32
	.long	L_1+7215
	.long	LE__LE_var_u64_u32
	.long	L_1+7233
	.long	LE__uLE_var_u64_u32
	.long	L_1+7248
	.long	LE__LE_param_u64_i8
	.long	L_1+7264
	.long	LE__uLE_param_u64_i8
	.long	L_1+7280
	.long	LE__LE_var_u64_i8
	.long	L_1+7297
	.long	LE__uLE_var_u64_i8
	.long	L_1+7311
	.long	LE__LE_param_u64_L
	.long	L_1+7326
	.long	LE__uLE_param_u64_L
	.long	L_1+7341
	.long	LE__LE_var_u64_L
	.long	L_1+7357
	.long	LE__uLE_var_u64_L
	.long	L_1+7370
	.long	LE__LE_param_u64_u8
	.long	L_1+7384
	.long	LE__uLE_param_u64_u8
	.long	L_1+7400
	.long	LE__LE_var_u64_u8
	.long	L_1+7417
	.long	LE__uLE_var_u64_u8
	.long	L_1+7431
	.long	LE__LE_param_u64_C
	.long	L_1+7446
	.long	LE__uLE_param_u64_C
	.long	L_1+7461
	.long	LE__LE_var_u64_C
	.long	L_1+7477
	.long	LE__uLE_var_u64_C
	.long	L_1+7490
	.long	LE__LE_param_u64_i64
	.long	L_1+7504
	.long	LE__uLE_param_u64_i64
	.long	L_1+7521
	.long	LE__LE_var_u64_i64
	.long	L_1+7539
	.long	LE__uLE_var_u64_i64
	.long	L_1+7554
	.long	LE__LE_param_u64_I
	.long	L_1+7570
	.long	LE__uLE_param_u64_I
	.long	L_1+7585
	.long	LE__LE_var_u64_I
	.long	L_1+7601
	.long	LE__uLE_var_u64_I
	.long	L_1+7614
	.long	LE__LE_param_u64_i16
	.long	L_1+7628
	.long	LE__uLE_param_u64_i16
	.long	L_1+7645
	.long	LE__LE_var_u64_i16
	.long	L_1+7663
	.long	LE__uLE_var_u64_i16
	.long	L_1+7678
	.long	LE__LE_param_u64_i32
	.long	L_1+7694
	.long	LE__uLE_param_u64_i32
	.long	L_1+7711
	.long	LE__LE_var_u64_i32
	.long	L_1+7729
	.long	LE__uLE_var_u64_i32
	.long	L_1+7744
	.long	LE__LE_param_u64_LC
	.long	L_1+7760
	.long	LE__uLE_param_u64_LC
	.long	L_1+7776
	.long	LE__LE_var_u64_LC
	.long	L_1+7793
	.long	LE__uLE_var_u64_LC
	.long	L_1+7807
	.long	LE__LE_param_u64_u64
	.long	L_1+7822
	.long	LE__uLE_param_u64_u64
	.long	L_1+7839
	.long	LE__LE_var_u64_u64
	.long	L_1+7857
	.long	LE__uLE_var_u64_u64
	.long	L_1+7872
	.long	LE__LE_param_u64_u16
	.long	L_1+7888
	.long	LE__uLE_param_u64_u16
	.long	L_1+7905
	.long	LE__LE_var_u64_u16
	.long	L_1+7923
	.long	LE__uLE_var_u64_u16
	.long	L_1+7938
	.long	LE__LE_param_u16_u32
	.long	L_1+7954
	.long	LE__uLE_param_u16_u32
	.long	L_1+7971
	.long	LE__LE_var_u16_u32
	.long	L_1+7989
	.long	LE__uLE_var_u16_u32
	.long	L_1+8004
	.long	LE__LE_param_u16_i8
	.long	L_1+8020
	.long	LE__uLE_param_u16_i8
	.long	L_1+8036
	.long	LE__LE_var_u16_i8
	.long	L_1+8053
	.long	LE__uLE_var_u16_i8
	.long	L_1+8067
	.long	LE__LE_param_u16_L
	.long	L_1+8082
	.long	LE__uLE_param_u16_L
	.long	L_1+8097
	.long	LE__LE_var_u16_L
	.long	L_1+8113
	.long	LE__uLE_var_u16_L
	.long	L_1+8126
	.long	LE__LE_param_u16_u8
	.long	L_1+8140
	.long	LE__uLE_param_u16_u8
	.long	L_1+8156
	.long	LE__LE_var_u16_u8
	.long	L_1+8173
	.long	LE__uLE_var_u16_u8
	.long	L_1+8187
	.long	LE__LE_param_u16_C
	.long	L_1+8202
	.long	LE__uLE_param_u16_C
	.long	L_1+8217
	.long	LE__LE_var_u16_C
	.long	L_1+8233
	.long	LE__uLE_var_u16_C
	.long	L_1+8246
	.long	LE__LE_param_u16_i64
	.long	L_1+8260
	.long	LE__uLE_param_u16_i64
	.long	L_1+8277
	.long	LE__LE_var_u16_i64
	.long	L_1+8295
	.long	LE__uLE_var_u16_i64
	.long	L_1+8310
	.long	LE__LE_param_u16_I
	.long	L_1+8326
	.long	LE__uLE_param_u16_I
	.long	L_1+8341
	.long	LE__LE_var_u16_I
	.long	L_1+8357
	.long	LE__uLE_var_u16_I
	.long	L_1+8370
	.long	LE__LE_param_u16_i16
	.long	L_1+8384
	.long	LE__uLE_param_u16_i16
	.long	L_1+8401
	.long	LE__LE_var_u16_i16
	.long	L_1+8419
	.long	LE__uLE_var_u16_i16
	.long	L_1+8434
	.long	LE__LE_param_u16_i32
	.long	L_1+8450
	.long	LE__uLE_param_u16_i32
	.long	L_1+8467
	.long	LE__LE_var_u16_i32
	.long	L_1+8485
	.long	LE__uLE_var_u16_i32
	.long	L_1+8500
	.long	LE__LE_param_u16_LC
	.long	L_1+8516
	.long	LE__uLE_param_u16_LC
	.long	L_1+8532
	.long	LE__LE_var_u16_LC
	.long	L_1+8549
	.long	LE__uLE_var_u16_LC
	.long	L_1+8563
	.long	LE__LE_param_u16_u64
	.long	L_1+8578
	.long	LE__uLE_param_u16_u64
	.long	L_1+8595
	.long	LE__LE_var_u16_u64
	.long	L_1+8613
	.long	LE__uLE_var_u16_u64
	.long	L_1+8628
	.long	LE__LE_param_u16_u16
	.long	L_1+8644
	.long	LE__uLE_param_u16_u16
	.long	L_1+8661
	.long	LE__LE_var_u16_u16
	.long	L_1+8679
	.long	LE__uLE_var_u16_u16
	.long	L_1+8694
	.skip 4
	.ascii	"../SOLsun/LE.m3"
	.skip 5
	.align 8
	.type	MM_LE, #object
	.size	MM_LE, 208
MM_LE:
	.long	L_1+13364
	.skip 16
	.long	L_1+8712
	.skip 12
	.long	MM_LE+148
	.skip 4
	.long	LE_M3
	.long	3
	.half	337
	.skip 2
	.long	0
	.long	338
	.long	0
	.long	339
	.long	340
	.half	341
	.skip 2
	.long	342
	.skip 4
	.long	0
	.long	343
	.long	344
	.skip 4
	.long	1081447817
	.long	927712936
	.byte	91
	.skip 7
	.long	0
	.long	348
	.byte	93
	.skip 3
	.long	350
	.long	1135586574
	.skip 12
	.long	LE_I3
	.long	MM_LE+160
	.skip 4
	.long	Long_I3
	.long	MM_LE+172
	.skip 4
	.long	Word_I3
	.long	MM_LE+184
	.skip 4
	.long	Cstdint_I3
	.long	MM_LE+196
	.skip 4
	.long	RTHooks_I3
	.skip 4
	.ident	"GCC: (GNU) 4.5.1"
