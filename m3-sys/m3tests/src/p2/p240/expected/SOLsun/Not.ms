	.file	"Not.mc"
	.section	".text"
	.align 4
	.type	Not__uNot_var_u16_u16, #function
	.proc	04
Not__uNot_var_u16_u16:
.LLFB0:
	save	%sp, -96, %sp
.LLCFI0:
	sethi	%hi(MM_Not), %g1
	or	%g1, %lo(MM_Not), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	xnor	%g0, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE0:
	.size	Not__uNot_var_u16_u16, .-Not__uNot_var_u16_u16
	.align 4
	.type	Not__Not_var_u16_u16, #function
	.proc	015
Not__Not_var_u16_u16:
.LLFB1:
	save	%sp, -96, %sp
.LLCFI1:
	sethi	%hi(MM_Not), %g1
	or	%g1, %lo(MM_Not), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL3
	 nop
	sethi	%hi(MM_Not), %g1
	or	%g1, %lo(MM_Not), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sub	%g0, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL4
	 nop
.LL3:
	mov	1761, %o0
	call	_m3_fault, 0
	 nop
.LL4:
	sethi	%hi(MM_Not), %g1
	or	%g1, %lo(MM_Not), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sub	%g0, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1:
	.size	Not__Not_var_u16_u16, .-Not__Not_var_u16_u16
	.align 4
	.type	Not__uNot_param_u16_u16, #function
	.proc	04
Not__uNot_param_u16_u16:
.LLFB2:
	save	%sp, -96, %sp
.LLCFI2:
	sth	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	xnor	%g0, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE2:
	.size	Not__uNot_param_u16_u16, .-Not__uNot_param_u16_u16
	.align 4
	.type	Not__Not_param_u16_u16, #function
	.proc	015
Not__Not_param_u16_u16:
.LLFB3:
	save	%sp, -96, %sp
.LLCFI3:
	sth	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL7
	 nop
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sub	%g0, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL8
	 nop
.LL7:
	mov	1825, %o0
	call	_m3_fault, 0
	 nop
.LL8:
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sub	%g0, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE3:
	.size	Not__Not_param_u16_u16, .-Not__Not_param_u16_u16
	.align 4
	.type	Not__uNot_var_u64_u64, #function
	.proc	05
Not__uNot_var_u64_u64:
.LLFB4:
	save	%sp, -96, %sp
.LLCFI4:
	sethi	%hi(MM_Not), %g1
	or	%g1, %lo(MM_Not), %g1
	ldd	[%g1+56], %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE4:
	.size	Not__uNot_var_u64_u64, .-Not__uNot_var_u64_u64
	.align 4
	.type	Not__Not_var_u64_u64, #function
	.proc	05
Not__Not_var_u64_u64:
.LLFB5:
	save	%sp, -96, %sp
.LLCFI5:
	sethi	%hi(MM_Not), %g1
	or	%g1, %lo(MM_Not), %g1
	ldd	[%g1+56], %o4
	subcc	%g0, %o5, %o5
	subx	%g0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE5:
	.size	Not__Not_var_u64_u64, .-Not__Not_var_u64_u64
	.align 4
	.type	Not__uNot_param_u64_u64, #function
	.proc	05
Not__uNot_param_u64_u64:
.LLFB6:
	save	%sp, -112, %sp
.LLCFI6:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE6:
	.size	Not__uNot_param_u64_u64, .-Not__uNot_param_u64_u64
	.align 4
	.type	Not__Not_param_u64_u64, #function
	.proc	05
Not__Not_param_u64_u64:
.LLFB7:
	save	%sp, -112, %sp
.LLCFI7:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o4
	subcc	%g0, %o5, %o5
	subx	%g0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE7:
	.size	Not__Not_param_u64_u64, .-Not__Not_param_u64_u64
	.align 4
	.type	Not__uNot_var_LC_LC, #function
	.proc	05
Not__uNot_var_LC_LC:
.LLFB8:
	save	%sp, -96, %sp
.LLCFI8:
	sethi	%hi(MM_Not), %g1
	or	%g1, %lo(MM_Not), %g1
	ldd	[%g1+64], %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE8:
	.size	Not__uNot_var_LC_LC, .-Not__uNot_var_LC_LC
	.align 4
	.type	Not__Not_var_LC_LC, #function
	.proc	05
Not__Not_var_LC_LC:
.LLFB9:
	save	%sp, -96, %sp
.LLCFI9:
	sethi	%hi(MM_Not), %g1
	or	%g1, %lo(MM_Not), %g1
	ldd	[%g1+64], %o4
	subcc	%g0, %o5, %o5
	subx	%g0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE9:
	.size	Not__Not_var_LC_LC, .-Not__Not_var_LC_LC
	.align 4
	.type	Not__uNot_param_LC_LC, #function
	.proc	05
Not__uNot_param_LC_LC:
.LLFB10:
	save	%sp, -112, %sp
.LLCFI10:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE10:
	.size	Not__uNot_param_LC_LC, .-Not__uNot_param_LC_LC
	.align 4
	.type	Not__Not_param_LC_LC, #function
	.proc	05
Not__Not_param_LC_LC:
.LLFB11:
	save	%sp, -112, %sp
.LLCFI11:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o4
	subcc	%g0, %o5, %o5
	subx	%g0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE11:
	.size	Not__Not_param_LC_LC, .-Not__Not_param_LC_LC
	.align 4
	.type	Not__uNot_var_i32_i32, #function
	.proc	04
Not__uNot_var_i32_i32:
.LLFB12:
	save	%sp, -96, %sp
.LLCFI12:
	sethi	%hi(MM_Not), %g1
	or	%g1, %lo(MM_Not), %g1
	ld	[%g1+72], %g1
	xnor	%g0, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE12:
	.size	Not__uNot_var_i32_i32, .-Not__uNot_var_i32_i32
	.align 4
	.type	Not__Not_var_i32_i32, #function
	.proc	04
Not__Not_var_i32_i32:
.LLFB13:
	save	%sp, -96, %sp
.LLCFI13:
	sethi	%hi(MM_Not), %g1
	or	%g1, %lo(MM_Not), %g1
	ld	[%g1+72], %g1
	sub	%g0, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE13:
	.size	Not__Not_var_i32_i32, .-Not__Not_var_i32_i32
	.align 4
	.type	Not__uNot_param_i32_i32, #function
	.proc	04
Not__uNot_param_i32_i32:
.LLFB14:
	save	%sp, -96, %sp
.LLCFI14:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %g1
	xnor	%g0, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE14:
	.size	Not__uNot_param_i32_i32, .-Not__uNot_param_i32_i32
	.align 4
	.type	Not__Not_param_i32_i32, #function
	.proc	04
Not__Not_param_i32_i32:
.LLFB15:
	save	%sp, -96, %sp
.LLCFI15:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	sub	%g0, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE15:
	.size	Not__Not_param_i32_i32, .-Not__Not_param_i32_i32
	.align 4
	.type	Not__uNot_var_i16_i16, #function
	.proc	04
Not__uNot_var_i16_i16:
.LLFB16:
	save	%sp, -96, %sp
.LLCFI16:
	sethi	%hi(MM_Not), %g1
	or	%g1, %lo(MM_Not), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	xnor	%g0, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE16:
	.size	Not__uNot_var_i16_i16, .-Not__uNot_var_i16_i16
	.align 4
	.type	Not__Not_var_i16_i16, #function
	.proc	03
Not__Not_var_i16_i16:
.LLFB17:
	save	%sp, -96, %sp
.LLCFI17:
	sethi	%hi(MM_Not), %g1
	or	%g1, %lo(MM_Not), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sub	%g0, %g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL23
	 nop
	sethi	%hi(MM_Not), %g1
	or	%g1, %lo(MM_Not), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sub	%g0, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL24
	 nop
.LL23:
	mov	2273, %o0
	call	_m3_fault, 0
	 nop
.LL24:
	sethi	%hi(MM_Not), %g1
	or	%g1, %lo(MM_Not), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sub	%g0, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE17:
	.size	Not__Not_var_i16_i16, .-Not__Not_var_i16_i16
	.align 4
	.type	Not__uNot_param_i16_i16, #function
	.proc	04
Not__uNot_param_i16_i16:
.LLFB18:
	save	%sp, -96, %sp
.LLCFI18:
	sth	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	xnor	%g0, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE18:
	.size	Not__uNot_param_i16_i16, .-Not__uNot_param_i16_i16
	.align 4
	.type	Not__Not_param_i16_i16, #function
	.proc	03
Not__Not_param_i16_i16:
.LLFB19:
	save	%sp, -96, %sp
.LLCFI19:
	sth	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sub	%g0, %g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL27
	 nop
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sub	%g0, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL28
	 nop
.LL27:
	mov	2337, %o0
	call	_m3_fault, 0
	 nop
.LL28:
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sub	%g0, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE19:
	.size	Not__Not_param_i16_i16, .-Not__Not_param_i16_i16
	.align 4
	.type	Not__uNot_var_I_I, #function
	.proc	04
Not__uNot_var_I_I:
.LLFB20:
	save	%sp, -96, %sp
.LLCFI20:
	sethi	%hi(MM_Not), %g1
	or	%g1, %lo(MM_Not), %g1
	ld	[%g1+80], %g1
	xnor	%g0, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE20:
	.size	Not__uNot_var_I_I, .-Not__uNot_var_I_I
	.align 4
	.type	Not__Not_var_I_I, #function
	.proc	04
Not__Not_var_I_I:
.LLFB21:
	save	%sp, -96, %sp
.LLCFI21:
	sethi	%hi(MM_Not), %g1
	or	%g1, %lo(MM_Not), %g1
	ld	[%g1+80], %g1
	sub	%g0, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE21:
	.size	Not__Not_var_I_I, .-Not__Not_var_I_I
	.align 4
	.type	Not__uNot_param_I_I, #function
	.proc	04
Not__uNot_param_I_I:
.LLFB22:
	save	%sp, -96, %sp
.LLCFI22:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %g1
	xnor	%g0, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE22:
	.size	Not__uNot_param_I_I, .-Not__uNot_param_I_I
	.align 4
	.type	Not__Not_param_I_I, #function
	.proc	04
Not__Not_param_I_I:
.LLFB23:
	save	%sp, -96, %sp
.LLCFI23:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	sub	%g0, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE23:
	.size	Not__Not_param_I_I, .-Not__Not_param_I_I
	.align 4
	.type	Not__uNot_var_i64_i64, #function
	.proc	05
Not__uNot_var_i64_i64:
.LLFB24:
	save	%sp, -96, %sp
.LLCFI24:
	sethi	%hi(MM_Not), %g1
	or	%g1, %lo(MM_Not), %g1
	ldd	[%g1+88], %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE24:
	.size	Not__uNot_var_i64_i64, .-Not__uNot_var_i64_i64
	.align 4
	.type	Not__Not_var_i64_i64, #function
	.proc	05
Not__Not_var_i64_i64:
.LLFB25:
	save	%sp, -96, %sp
.LLCFI25:
	sethi	%hi(MM_Not), %g1
	or	%g1, %lo(MM_Not), %g1
	ldd	[%g1+88], %o4
	subcc	%g0, %o5, %o5
	subx	%g0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE25:
	.size	Not__Not_var_i64_i64, .-Not__Not_var_i64_i64
	.align 4
	.type	Not__uNot_param_i64_i64, #function
	.proc	05
Not__uNot_param_i64_i64:
.LLFB26:
	save	%sp, -112, %sp
.LLCFI26:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE26:
	.size	Not__uNot_param_i64_i64, .-Not__uNot_param_i64_i64
	.align 4
	.type	Not__Not_param_i64_i64, #function
	.proc	05
Not__Not_param_i64_i64:
.LLFB27:
	save	%sp, -112, %sp
.LLCFI27:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o4
	subcc	%g0, %o5, %o5
	subx	%g0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE27:
	.size	Not__Not_param_i64_i64, .-Not__Not_param_i64_i64
	.align 4
	.type	Not__uNot_var_C_C, #function
	.proc	04
Not__uNot_var_C_C:
.LLFB28:
	save	%sp, -96, %sp
.LLCFI28:
	sethi	%hi(MM_Not), %g1
	or	%g1, %lo(MM_Not), %g1
	ld	[%g1+96], %g1
	xnor	%g0, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE28:
	.size	Not__uNot_var_C_C, .-Not__uNot_var_C_C
	.align 4
	.type	Not__Not_var_C_C, #function
	.proc	016
Not__Not_var_C_C:
.LLFB29:
	save	%sp, -96, %sp
.LLCFI29:
	sethi	%hi(MM_Not), %g1
	or	%g1, %lo(MM_Not), %g1
	ld	[%g1+96], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL39
	 nop
	mov	2657, %o0
	call	_m3_fault, 0
	 nop
.LL39:
	sethi	%hi(MM_Not), %g1
	or	%g1, %lo(MM_Not), %g1
	ld	[%g1+96], %g1
	sub	%g0, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE29:
	.size	Not__Not_var_C_C, .-Not__Not_var_C_C
	.align 4
	.type	Not__uNot_param_C_C, #function
	.proc	04
Not__uNot_param_C_C:
.LLFB30:
	save	%sp, -96, %sp
.LLCFI30:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %g1
	xnor	%g0, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE30:
	.size	Not__uNot_param_C_C, .-Not__uNot_param_C_C
	.align 4
	.type	Not__Not_param_C_C, #function
	.proc	016
Not__Not_param_C_C:
.LLFB31:
	save	%sp, -96, %sp
.LLCFI31:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL42
	 nop
	mov	2721, %o0
	call	_m3_fault, 0
	 nop
.LL42:
	ld	[%fp+72], %g1
	sub	%g0, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE31:
	.size	Not__Not_param_C_C, .-Not__Not_param_C_C
	.align 4
	.type	Not__Not_var_f64_f64, #function
	.proc	07
Not__Not_var_f64_f64:
.LLFB32:
	save	%sp, -96, %sp
.LLCFI32:
	sethi	%hi(MM_Not), %g1
	or	%g1, %lo(MM_Not), %g1
	add	%g1, 104, %g1
	ldd	[%g1], %f8
	fnegd	%f8, %f8
	fmovd	%f8, %f0
	return	%i7+8
	 nop
.LLFE32:
	.size	Not__Not_var_f64_f64, .-Not__Not_var_f64_f64
	.align 4
	.type	Not__Not_param_f64_f64, #function
	.proc	07
Not__Not_param_f64_f64:
.LLFB33:
	save	%sp, -112, %sp
.LLCFI33:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %f8
	fnegd	%f8, %f8
	fmovd	%f8, %f0
	return	%i7+8
	 nop
.LLFE33:
	.size	Not__Not_param_f64_f64, .-Not__Not_param_f64_f64
	.align 4
	.type	Not__uNot_var_u8_u8, #function
	.proc	04
Not__uNot_var_u8_u8:
.LLFB34:
	save	%sp, -96, %sp
.LLCFI34:
	sethi	%hi(MM_Not), %g1
	or	%g1, %lo(MM_Not), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	xnor	%g0, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE34:
	.size	Not__uNot_var_u8_u8, .-Not__uNot_var_u8_u8
	.align 4
	.type	Not__Not_var_u8_u8, #function
	.proc	014
Not__Not_var_u8_u8:
.LLFB35:
	save	%sp, -96, %sp
.LLCFI35:
	sethi	%hi(MM_Not), %g1
	or	%g1, %lo(MM_Not), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL47
	 nop
	sethi	%hi(MM_Not), %g1
	or	%g1, %lo(MM_Not), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL48
	 nop
.LL47:
	mov	2849, %o0
	call	_m3_fault, 0
	 nop
.LL48:
	sethi	%hi(MM_Not), %g1
	or	%g1, %lo(MM_Not), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	sub	%g0, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE35:
	.size	Not__Not_var_u8_u8, .-Not__Not_var_u8_u8
	.align 4
	.type	Not__uNot_param_u8_u8, #function
	.proc	04
Not__uNot_param_u8_u8:
.LLFB36:
	save	%sp, -96, %sp
.LLCFI36:
	stb	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	xnor	%g0, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE36:
	.size	Not__uNot_param_u8_u8, .-Not__uNot_param_u8_u8
	.align 4
	.type	Not__Not_param_u8_u8, #function
	.proc	014
Not__Not_param_u8_u8:
.LLFB37:
	save	%sp, -96, %sp
.LLCFI37:
	stb	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL51
	 nop
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL52
	 nop
.LL51:
	mov	2913, %o0
	call	_m3_fault, 0
	 nop
.LL52:
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %g1
	sub	%g0, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE37:
	.size	Not__Not_param_u8_u8, .-Not__Not_param_u8_u8
	.align 4
	.type	Not__uNot_var_L_L, #function
	.proc	05
Not__uNot_var_L_L:
.LLFB38:
	save	%sp, -96, %sp
.LLCFI38:
	sethi	%hi(MM_Not), %g1
	or	%g1, %lo(MM_Not), %g1
	ldd	[%g1+120], %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE38:
	.size	Not__uNot_var_L_L, .-Not__uNot_var_L_L
	.align 4
	.type	Not__Not_var_L_L, #function
	.proc	05
Not__Not_var_L_L:
.LLFB39:
	save	%sp, -96, %sp
.LLCFI39:
	sethi	%hi(MM_Not), %g1
	or	%g1, %lo(MM_Not), %g1
	ldd	[%g1+120], %o4
	subcc	%g0, %o5, %o5
	subx	%g0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE39:
	.size	Not__Not_var_L_L, .-Not__Not_var_L_L
	.align 4
	.type	Not__uNot_param_L_L, #function
	.proc	05
Not__uNot_param_L_L:
.LLFB40:
	save	%sp, -112, %sp
.LLCFI40:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE40:
	.size	Not__uNot_param_L_L, .-Not__uNot_param_L_L
	.align 4
	.type	Not__Not_param_L_L, #function
	.proc	05
Not__Not_param_L_L:
.LLFB41:
	save	%sp, -112, %sp
.LLCFI41:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o4
	subcc	%g0, %o5, %o5
	subx	%g0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE41:
	.size	Not__Not_param_L_L, .-Not__Not_param_L_L
	.align 4
	.type	Not__uNot_var_i8_i8, #function
	.proc	04
Not__uNot_var_i8_i8:
.LLFB42:
	save	%sp, -96, %sp
.LLCFI42:
	sethi	%hi(MM_Not), %g1
	or	%g1, %lo(MM_Not), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	xnor	%g0, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE42:
	.size	Not__uNot_var_i8_i8, .-Not__uNot_var_i8_i8
	.align 4
	.type	Not__Not_var_i8_i8, #function
	.proc	02
Not__Not_var_i8_i8:
.LLFB43:
	save	%sp, -96, %sp
.LLCFI43:
	sethi	%hi(MM_Not), %g1
	or	%g1, %lo(MM_Not), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sub	%g0, %g1, %g1
	cmp	%g1, -128
	bl	%icc, .LL59
	 nop
	sethi	%hi(MM_Not), %g1
	or	%g1, %lo(MM_Not), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL60
	 nop
.LL59:
	mov	3105, %o0
	call	_m3_fault, 0
	 nop
.LL60:
	sethi	%hi(MM_Not), %g1
	or	%g1, %lo(MM_Not), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sub	%g0, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE43:
	.size	Not__Not_var_i8_i8, .-Not__Not_var_i8_i8
	.align 4
	.type	Not__uNot_param_i8_i8, #function
	.proc	04
Not__uNot_param_i8_i8:
.LLFB44:
	save	%sp, -96, %sp
.LLCFI44:
	stb	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	xnor	%g0, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE44:
	.size	Not__uNot_param_i8_i8, .-Not__uNot_param_i8_i8
	.align 4
	.type	Not__Not_param_i8_i8, #function
	.proc	02
Not__Not_param_i8_i8:
.LLFB45:
	save	%sp, -96, %sp
.LLCFI45:
	stb	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sub	%g0, %g1, %g1
	cmp	%g1, -128
	bl	%icc, .LL63
	 nop
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL64
	 nop
.LL63:
	mov	3169, %o0
	call	_m3_fault, 0
	 nop
.LL64:
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sub	%g0, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE45:
	.size	Not__Not_param_i8_i8, .-Not__Not_param_i8_i8
	.align 4
	.type	Not__uNot_var_u32_u32, #function
	.proc	04
Not__uNot_var_u32_u32:
.LLFB46:
	save	%sp, -96, %sp
.LLCFI46:
	sethi	%hi(MM_Not), %g1
	or	%g1, %lo(MM_Not), %g1
	ld	[%g1+132], %g1
	xnor	%g0, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE46:
	.size	Not__uNot_var_u32_u32, .-Not__uNot_var_u32_u32
	.align 4
	.type	Not__Not_var_u32_u32, #function
	.proc	04
Not__Not_var_u32_u32:
.LLFB47:
	save	%sp, -96, %sp
.LLCFI47:
	sethi	%hi(MM_Not), %g1
	or	%g1, %lo(MM_Not), %g1
	ld	[%g1+132], %g1
	sub	%g0, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE47:
	.size	Not__Not_var_u32_u32, .-Not__Not_var_u32_u32
	.align 4
	.type	Not__uNot_param_u32_u32, #function
	.proc	04
Not__uNot_param_u32_u32:
.LLFB48:
	save	%sp, -96, %sp
.LLCFI48:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %g1
	xnor	%g0, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE48:
	.size	Not__uNot_param_u32_u32, .-Not__uNot_param_u32_u32
	.align 4
	.type	Not__Not_param_u32_u32, #function
	.proc	04
Not__Not_param_u32_u32:
.LLFB49:
	save	%sp, -96, %sp
.LLCFI49:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	sub	%g0, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE49:
	.size	Not__Not_param_u32_u32, .-Not__Not_param_u32_u32
	.align 4
	.type	Not__Not_var_f32_f32, #function
	.proc	06
Not__Not_var_f32_f32:
.LLFB50:
	save	%sp, -96, %sp
.LLCFI50:
	sethi	%hi(MM_Not), %g1
	or	%g1, %lo(MM_Not), %g1
	add	%g1, 136, %g1
	ld	[%g1], %f8
	fnegs	%f8, %f8
	fmovs	%f8, %f0
	return	%i7+8
	 nop
.LLFE50:
	.size	Not__Not_var_f32_f32, .-Not__Not_var_f32_f32
	.align 4
	.type	Not__Not_param_f32_f32, #function
	.proc	06
Not__Not_param_f32_f32:
.LLFB51:
	save	%sp, -96, %sp
.LLCFI51:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %f8
	fnegs	%f8, %f8
	fmovs	%f8, %f0
	return	%i7+8
	 nop
.LLFE51:
	.size	Not__Not_param_f32_f32, .-Not__Not_param_f32_f32
	.align 4
	.global Not_M3
	.type	Not_M3, #function
	.proc	0120
Not_M3:
.LLFB52:
	save	%sp, -96, %sp
.LLCFI52:
	st	%i0, [%fp+68]
	sethi	%hi(MM_Not), %g1
	or	%g1, %lo(MM_Not), %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE52:
	.size	Not_M3, .-Not_M3
	.align 4
	.type	_m3_fault, #function
	.proc	020
_m3_fault:
.LLFB53:
	save	%sp, -96, %sp
.LLCFI53:
	st	%i0, [%fp+68]
	sethi	%hi(MM_Not), %g1
	or	%g1, %lo(MM_Not), %o0
	ld	[%fp+68], %o1
	call	RTHooks__ReportFault, 0
	 nop
	return	%i7+8
	 nop
.LLFE53:
	.size	_m3_fault, .-_m3_fault
	.section	".data"
	.align 8
	.type	L_1, #object
	.size	L_1, 1296
L_1:
	.ascii	"Not_M3"
	.skip 1
	.ascii	"Not_param_f32_f32"
	.skip 1
	.ascii	"Not_var_f32_f32"
	.skip 1
	.ascii	"Not_param_u32_u32"
	.skip 1
	.ascii	"uNot_param_u32_u32"
	.skip 1
	.ascii	"Not_var_u32_u32"
	.skip 1
	.ascii	"uNot_var_u32_u32"
	.skip 1
	.ascii	"Not_param_i8_i8"
	.skip 1
	.ascii	"uNot_param_i8_i8"
	.skip 1
	.ascii	"Not_var_i8_i8"
	.skip 1
	.ascii	"uNot_var_i8_i8"
	.skip 1
	.ascii	"Not_param_L_L"
	.skip 1
	.ascii	"uNot_param_L_L"
	.skip 1
	.ascii	"Not_var_L_L"
	.skip 1
	.ascii	"uNot_var_L_L"
	.skip 1
	.ascii	"Not_param_u8_u8"
	.skip 1
	.ascii	"uNot_param_u8_u8"
	.skip 1
	.ascii	"Not_var_u8_u8"
	.skip 1
	.ascii	"uNot_var_u8_u8"
	.skip 1
	.ascii	"Not_param_f64_f64"
	.skip 1
	.ascii	"Not_var_f64_f64"
	.skip 1
	.ascii	"Not_param_C_C"
	.skip 1
	.ascii	"uNot_param_C_C"
	.skip 1
	.ascii	"Not_var_C_C"
	.skip 1
	.ascii	"uNot_var_C_C"
	.skip 1
	.ascii	"Not_param_i64_i64"
	.skip 1
	.ascii	"uNot_param_i64_i64"
	.skip 1
	.ascii	"Not_var_i64_i64"
	.skip 1
	.ascii	"uNot_var_i64_i64"
	.skip 1
	.ascii	"Not_param_I_I"
	.skip 1
	.ascii	"uNot_param_I_I"
	.skip 1
	.ascii	"Not_var_I_I"
	.skip 1
	.ascii	"uNot_var_I_I"
	.skip 1
	.ascii	"Not_param_i16_i16"
	.skip 1
	.ascii	"uNot_param_i16_i16"
	.skip 1
	.ascii	"Not_var_i16_i16"
	.skip 1
	.ascii	"uNot_var_i16_i16"
	.skip 1
	.ascii	"Not_param_i32_i32"
	.skip 1
	.ascii	"uNot_param_i32_i32"
	.skip 1
	.ascii	"Not_var_i32_i32"
	.skip 1
	.ascii	"uNot_var_i32_i32"
	.skip 1
	.ascii	"Not_param_LC_LC"
	.skip 1
	.ascii	"uNot_param_LC_LC"
	.skip 1
	.ascii	"Not_var_LC_LC"
	.skip 1
	.ascii	"uNot_var_LC_LC"
	.skip 1
	.ascii	"Not_param_u64_u64"
	.skip 1
	.ascii	"uNot_param_u64_u64"
	.skip 1
	.ascii	"Not_var_u64_u64"
	.skip 1
	.ascii	"uNot_var_u64_u64"
	.skip 1
	.ascii	"Not_param_u16_u16"
	.skip 1
	.ascii	"uNot_param_u16_u16"
	.skip 1
	.ascii	"Not_var_u16_u16"
	.skip 1
	.ascii	"uNot_var_u16_u16"
	.skip 2
	.long	Not_M3
	.long	L_1
	.long	Not__Not_param_f32_f32
	.long	L_1+7
	.long	Not__Not_var_f32_f32
	.long	L_1+25
	.long	Not__Not_param_u32_u32
	.long	L_1+41
	.long	Not__uNot_param_u32_u32
	.long	L_1+59
	.long	Not__Not_var_u32_u32
	.long	L_1+78
	.long	Not__uNot_var_u32_u32
	.long	L_1+94
	.long	Not__Not_param_i8_i8
	.long	L_1+111
	.long	Not__uNot_param_i8_i8
	.long	L_1+127
	.long	Not__Not_var_i8_i8
	.long	L_1+144
	.long	Not__uNot_var_i8_i8
	.long	L_1+158
	.long	Not__Not_param_L_L
	.long	L_1+173
	.long	Not__uNot_param_L_L
	.long	L_1+187
	.long	Not__Not_var_L_L
	.long	L_1+202
	.long	Not__uNot_var_L_L
	.long	L_1+214
	.long	Not__Not_param_u8_u8
	.long	L_1+227
	.long	Not__uNot_param_u8_u8
	.long	L_1+243
	.long	Not__Not_var_u8_u8
	.long	L_1+260
	.long	Not__uNot_var_u8_u8
	.long	L_1+274
	.long	Not__Not_param_f64_f64
	.long	L_1+289
	.long	Not__Not_var_f64_f64
	.long	L_1+307
	.long	Not__Not_param_C_C
	.long	L_1+323
	.long	Not__uNot_param_C_C
	.long	L_1+337
	.long	Not__Not_var_C_C
	.long	L_1+352
	.long	Not__uNot_var_C_C
	.long	L_1+364
	.long	Not__Not_param_i64_i64
	.long	L_1+377
	.long	Not__uNot_param_i64_i64
	.long	L_1+395
	.long	Not__Not_var_i64_i64
	.long	L_1+414
	.long	Not__uNot_var_i64_i64
	.long	L_1+430
	.long	Not__Not_param_I_I
	.long	L_1+447
	.long	Not__uNot_param_I_I
	.long	L_1+461
	.long	Not__Not_var_I_I
	.long	L_1+476
	.long	Not__uNot_var_I_I
	.long	L_1+488
	.long	Not__Not_param_i16_i16
	.long	L_1+501
	.long	Not__uNot_param_i16_i16
	.long	L_1+519
	.long	Not__Not_var_i16_i16
	.long	L_1+538
	.long	Not__uNot_var_i16_i16
	.long	L_1+554
	.long	Not__Not_param_i32_i32
	.long	L_1+571
	.long	Not__uNot_param_i32_i32
	.long	L_1+589
	.long	Not__Not_var_i32_i32
	.long	L_1+608
	.long	Not__uNot_var_i32_i32
	.long	L_1+624
	.long	Not__Not_param_LC_LC
	.long	L_1+641
	.long	Not__uNot_param_LC_LC
	.long	L_1+657
	.long	Not__Not_var_LC_LC
	.long	L_1+674
	.long	Not__uNot_var_LC_LC
	.long	L_1+688
	.long	Not__Not_param_u64_u64
	.long	L_1+703
	.long	Not__uNot_param_u64_u64
	.long	L_1+721
	.long	Not__Not_var_u64_u64
	.long	L_1+740
	.long	Not__uNot_var_u64_u64
	.long	L_1+756
	.long	Not__Not_param_u16_u16
	.long	L_1+773
	.long	Not__uNot_param_u16_u16
	.long	L_1+791
	.long	Not__Not_var_u16_u16
	.long	L_1+810
	.long	Not__uNot_var_u16_u16
	.long	L_1+826
	.skip 4
	.ascii	"../SOLsun/Not.m3"
	.skip 8
	.align 8
	.type	MM_Not, #object
	.size	MM_Not, 208
MM_Not:
	.long	L_1+1272
	.skip 16
	.long	L_1+844
	.skip 12
	.long	MM_Not+148
	.skip 4
	.long	Not_M3
	.long	3
	.half	593
	.skip 2
	.long	0
	.long	594
	.long	0
	.long	595
	.long	596
	.half	597
	.skip 2
	.long	598
	.skip 4
	.long	0
	.long	599
	.long	600
	.skip 4
	.long	1082313936
	.long	-446676599
	.byte	91
	.skip 7
	.long	0
	.long	604
	.byte	93
	.skip 3
	.long	606
	.long	1142417129
	.skip 12
	.long	Not_I3
	.long	MM_Not+160
	.skip 4
	.long	Long_I3
	.long	MM_Not+172
	.skip 4
	.long	Word_I3
	.long	MM_Not+184
	.skip 4
	.long	Cstdint_I3
	.long	MM_Not+196
	.skip 4
	.long	RTHooks_I3
	.skip 4
	.section	".eh_frame",#alloc,#write
.LLframe1:
	.uaword	.LLECIE1-.LLSCIE1
.LLSCIE1:
	.uaword	0x0
	.byte	0x1
	.asciz	""
	.byte	0x1
	.byte	0x7c
	.byte	0xf
	.byte	0xc
	.byte	0xe
	.byte	0x0
	.align 4
.LLECIE1:
.LLSFDE1:
	.uaword	.LLEFDE1-.LLASFDE1
.LLASFDE1:
	.uaword	.LLASFDE1-.LLframe1
	.uaword	.LLFB1
	.uaword	.LLFE1-.LLFB1
	.byte	0x4
	.uaword	.LLCFI1-.LLFB1
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE1:
.LLSFDE3:
	.uaword	.LLEFDE3-.LLASFDE3
.LLASFDE3:
	.uaword	.LLASFDE3-.LLframe1
	.uaword	.LLFB3
	.uaword	.LLFE3-.LLFB3
	.byte	0x4
	.uaword	.LLCFI3-.LLFB3
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE3:
.LLSFDE5:
	.uaword	.LLEFDE5-.LLASFDE5
.LLASFDE5:
	.uaword	.LLASFDE5-.LLframe1
	.uaword	.LLFB17
	.uaword	.LLFE17-.LLFB17
	.byte	0x4
	.uaword	.LLCFI17-.LLFB17
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE5:
.LLSFDE7:
	.uaword	.LLEFDE7-.LLASFDE7
.LLASFDE7:
	.uaword	.LLASFDE7-.LLframe1
	.uaword	.LLFB19
	.uaword	.LLFE19-.LLFB19
	.byte	0x4
	.uaword	.LLCFI19-.LLFB19
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE7:
.LLSFDE9:
	.uaword	.LLEFDE9-.LLASFDE9
.LLASFDE9:
	.uaword	.LLASFDE9-.LLframe1
	.uaword	.LLFB29
	.uaword	.LLFE29-.LLFB29
	.byte	0x4
	.uaword	.LLCFI29-.LLFB29
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE9:
.LLSFDE11:
	.uaword	.LLEFDE11-.LLASFDE11
.LLASFDE11:
	.uaword	.LLASFDE11-.LLframe1
	.uaword	.LLFB31
	.uaword	.LLFE31-.LLFB31
	.byte	0x4
	.uaword	.LLCFI31-.LLFB31
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE11:
.LLSFDE13:
	.uaword	.LLEFDE13-.LLASFDE13
.LLASFDE13:
	.uaword	.LLASFDE13-.LLframe1
	.uaword	.LLFB35
	.uaword	.LLFE35-.LLFB35
	.byte	0x4
	.uaword	.LLCFI35-.LLFB35
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE13:
.LLSFDE15:
	.uaword	.LLEFDE15-.LLASFDE15
.LLASFDE15:
	.uaword	.LLASFDE15-.LLframe1
	.uaword	.LLFB37
	.uaword	.LLFE37-.LLFB37
	.byte	0x4
	.uaword	.LLCFI37-.LLFB37
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE15:
.LLSFDE17:
	.uaword	.LLEFDE17-.LLASFDE17
.LLASFDE17:
	.uaword	.LLASFDE17-.LLframe1
	.uaword	.LLFB43
	.uaword	.LLFE43-.LLFB43
	.byte	0x4
	.uaword	.LLCFI43-.LLFB43
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE17:
.LLSFDE19:
	.uaword	.LLEFDE19-.LLASFDE19
.LLASFDE19:
	.uaword	.LLASFDE19-.LLframe1
	.uaword	.LLFB45
	.uaword	.LLFE45-.LLFB45
	.byte	0x4
	.uaword	.LLCFI45-.LLFB45
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE19:
.LLSFDE21:
	.uaword	.LLEFDE21-.LLASFDE21
.LLASFDE21:
	.uaword	.LLASFDE21-.LLframe1
	.uaword	.LLFB53
	.uaword	.LLFE53-.LLFB53
	.byte	0x4
	.uaword	.LLCFI53-.LLFB53
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE21:
	.ident	"GCC: (GNU) 4.5.1"
