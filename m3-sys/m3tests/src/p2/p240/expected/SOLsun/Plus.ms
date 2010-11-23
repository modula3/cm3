	.file	"Plus.mc"
	.section	".text"
	.align 4
	.type	Plus__uPlus_var_u16_u16, #function
	.proc	04
Plus__uPlus_var_u16_u16:
.LLFB0:
	save	%sp, -96, %sp
.LLCFI0:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE0:
	.size	Plus__uPlus_var_u16_u16, .-Plus__uPlus_var_u16_u16
	.align 4
	.type	Plus__Plus_var_u16_u16, #function
	.proc	015
Plus__Plus_var_u16_u16:
.LLFB1:
	save	%sp, -96, %sp
.LLCFI1:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	add	%o5, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL3
	 nop
	mov	1761, %o0
	call	_m3_fault, 0
	 nop
.LL3:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1:
	.size	Plus__Plus_var_u16_u16, .-Plus__Plus_var_u16_u16
	.align 4
	.type	Plus__uPlus_param_u16_u16, #function
	.proc	04
Plus__uPlus_param_u16_u16:
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
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE2:
	.size	Plus__uPlus_param_u16_u16, .-Plus__uPlus_param_u16_u16
	.align 4
	.type	Plus__Plus_param_u16_u16, #function
	.proc	015
Plus__Plus_param_u16_u16:
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
	add	%o5, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL6
	 nop
	mov	1825, %o0
	call	_m3_fault, 0
	 nop
.LL6:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE3:
	.size	Plus__Plus_param_u16_u16, .-Plus__Plus_param_u16_u16
	.align 4
	.type	Plus__uPlus_var_u16_u64, #function
	.proc	05
Plus__uPlus_var_u16_u64:
.LLFB4:
	save	%sp, -96, %sp
.LLCFI4:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE4:
	.size	Plus__uPlus_var_u16_u64, .-Plus__uPlus_var_u16_u64
	.align 4
	.type	Plus__Plus_var_u16_u64, #function
	.proc	05
Plus__Plus_var_u16_u64:
.LLFB5:
	save	%sp, -96, %sp
.LLCFI5:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE5:
	.size	Plus__Plus_var_u16_u64, .-Plus__Plus_var_u16_u64
	.align 4
	.type	Plus__uPlus_param_u16_u64, #function
	.proc	05
Plus__uPlus_param_u16_u64:
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
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE6:
	.size	Plus__uPlus_param_u16_u64, .-Plus__uPlus_param_u16_u64
	.align 4
	.type	Plus__Plus_param_u16_u64, #function
	.proc	05
Plus__Plus_param_u16_u64:
.LLFB7:
	save	%sp, -104, %sp
.LLCFI7:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE7:
	.size	Plus__Plus_param_u16_u64, .-Plus__Plus_param_u16_u64
	.align 4
	.type	Plus__uPlus_var_u16_LC, #function
	.proc	05
Plus__uPlus_var_u16_LC:
.LLFB8:
	save	%sp, -96, %sp
.LLCFI8:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE8:
	.size	Plus__uPlus_var_u16_LC, .-Plus__uPlus_var_u16_LC
	.align 4
	.type	Plus__Plus_var_u16_LC, #function
	.proc	05
Plus__Plus_var_u16_LC:
.LLFB9:
	save	%sp, -96, %sp
.LLCFI9:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE9:
	.size	Plus__Plus_var_u16_LC, .-Plus__Plus_var_u16_LC
	.align 4
	.type	Plus__uPlus_param_u16_LC, #function
	.proc	05
Plus__uPlus_param_u16_LC:
.LLFB10:
	save	%sp, -104, %sp
.LLCFI10:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE10:
	.size	Plus__uPlus_param_u16_LC, .-Plus__uPlus_param_u16_LC
	.align 4
	.type	Plus__Plus_param_u16_LC, #function
	.proc	05
Plus__Plus_param_u16_LC:
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
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE11:
	.size	Plus__Plus_param_u16_LC, .-Plus__Plus_param_u16_LC
	.align 4
	.type	Plus__uPlus_var_u16_i32, #function
	.proc	04
Plus__uPlus_var_u16_i32:
.LLFB12:
	save	%sp, -96, %sp
.LLCFI12:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE12:
	.size	Plus__uPlus_var_u16_i32, .-Plus__uPlus_var_u16_i32
	.align 4
	.type	Plus__Plus_var_u16_i32, #function
	.proc	015
Plus__Plus_var_u16_i32:
.LLFB13:
	save	%sp, -96, %sp
.LLCFI13:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL17
	 nop
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	add	%o5, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL18
	 nop
.LL17:
	mov	2145, %o0
	call	_m3_fault, 0
	 nop
.LL18:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE13:
	.size	Plus__Plus_var_u16_i32, .-Plus__Plus_var_u16_i32
	.align 4
	.type	Plus__uPlus_param_u16_i32, #function
	.proc	04
Plus__uPlus_param_u16_i32:
.LLFB14:
	save	%sp, -96, %sp
.LLCFI14:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE14:
	.size	Plus__uPlus_param_u16_i32, .-Plus__uPlus_param_u16_i32
	.align 4
	.type	Plus__Plus_param_u16_i32, #function
	.proc	015
Plus__Plus_param_u16_i32:
.LLFB15:
	save	%sp, -96, %sp
.LLCFI15:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL21
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL22
	 nop
.LL21:
	mov	2209, %o0
	call	_m3_fault, 0
	 nop
.LL22:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE15:
	.size	Plus__Plus_param_u16_i32, .-Plus__Plus_param_u16_i32
	.align 4
	.type	Plus__uPlus_var_u16_i16, #function
	.proc	04
Plus__uPlus_var_u16_i16:
.LLFB16:
	save	%sp, -96, %sp
.LLCFI16:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE16:
	.size	Plus__uPlus_var_u16_i16, .-Plus__uPlus_var_u16_i16
	.align 4
	.type	Plus__Plus_var_u16_i16, #function
	.proc	015
Plus__Plus_var_u16_i16:
.LLFB17:
	save	%sp, -96, %sp
.LLCFI17:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	add	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL25
	 nop
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	add	%o5, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL26
	 nop
.LL25:
	mov	2273, %o0
	call	_m3_fault, 0
	 nop
.LL26:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE17:
	.size	Plus__Plus_var_u16_i16, .-Plus__Plus_var_u16_i16
	.align 4
	.type	Plus__uPlus_param_u16_i16, #function
	.proc	04
Plus__uPlus_param_u16_i16:
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
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE18:
	.size	Plus__uPlus_param_u16_i16, .-Plus__uPlus_param_u16_i16
	.align 4
	.type	Plus__Plus_param_u16_i16, #function
	.proc	015
Plus__Plus_param_u16_i16:
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
	add	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL29
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	add	%o5, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL30
	 nop
.LL29:
	mov	2337, %o0
	call	_m3_fault, 0
	 nop
.LL30:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE19:
	.size	Plus__Plus_param_u16_i16, .-Plus__Plus_param_u16_i16
	.align 4
	.type	Plus__uPlus_var_u16_I, #function
	.proc	04
Plus__uPlus_var_u16_I:
.LLFB20:
	save	%sp, -96, %sp
.LLCFI20:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE20:
	.size	Plus__uPlus_var_u16_I, .-Plus__uPlus_var_u16_I
	.align 4
	.type	Plus__Plus_var_u16_I, #function
	.proc	015
Plus__Plus_var_u16_I:
.LLFB21:
	save	%sp, -96, %sp
.LLCFI21:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL33
	 nop
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	add	%o5, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL34
	 nop
.LL33:
	mov	2401, %o0
	call	_m3_fault, 0
	 nop
.LL34:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE21:
	.size	Plus__Plus_var_u16_I, .-Plus__Plus_var_u16_I
	.align 4
	.type	Plus__uPlus_param_u16_I, #function
	.proc	04
Plus__uPlus_param_u16_I:
.LLFB22:
	save	%sp, -96, %sp
.LLCFI22:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE22:
	.size	Plus__uPlus_param_u16_I, .-Plus__uPlus_param_u16_I
	.align 4
	.type	Plus__Plus_param_u16_I, #function
	.proc	015
Plus__Plus_param_u16_I:
.LLFB23:
	save	%sp, -96, %sp
.LLCFI23:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL37
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL38
	 nop
.LL37:
	mov	2465, %o0
	call	_m3_fault, 0
	 nop
.LL38:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE23:
	.size	Plus__Plus_param_u16_I, .-Plus__Plus_param_u16_I
	.align 4
	.type	Plus__uPlus_var_u16_i64, #function
	.proc	05
Plus__uPlus_var_u16_i64:
.LLFB24:
	save	%sp, -96, %sp
.LLCFI24:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE24:
	.size	Plus__uPlus_var_u16_i64, .-Plus__uPlus_var_u16_i64
	.align 4
	.type	Plus__Plus_var_u16_i64, #function
	.proc	05
Plus__Plus_var_u16_i64:
.LLFB25:
	save	%sp, -96, %sp
.LLCFI25:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE25:
	.size	Plus__Plus_var_u16_i64, .-Plus__Plus_var_u16_i64
	.align 4
	.type	Plus__uPlus_param_u16_i64, #function
	.proc	05
Plus__uPlus_param_u16_i64:
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
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE26:
	.size	Plus__uPlus_param_u16_i64, .-Plus__uPlus_param_u16_i64
	.align 4
	.type	Plus__Plus_param_u16_i64, #function
	.proc	05
Plus__Plus_param_u16_i64:
.LLFB27:
	save	%sp, -104, %sp
.LLCFI27:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE27:
	.size	Plus__Plus_param_u16_i64, .-Plus__Plus_param_u16_i64
	.align 4
	.type	Plus__uPlus_var_u16_C, #function
	.proc	04
Plus__uPlus_var_u16_C:
.LLFB28:
	save	%sp, -96, %sp
.LLCFI28:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE28:
	.size	Plus__uPlus_var_u16_C, .-Plus__uPlus_var_u16_C
	.align 4
	.type	Plus__Plus_var_u16_C, #function
	.proc	015
Plus__Plus_var_u16_C:
.LLFB29:
	save	%sp, -96, %sp
.LLCFI29:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL45
	 nop
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	add	%o5, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL46
	 nop
.LL45:
	mov	2657, %o0
	call	_m3_fault, 0
	 nop
.LL46:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE29:
	.size	Plus__Plus_var_u16_C, .-Plus__Plus_var_u16_C
	.align 4
	.type	Plus__uPlus_param_u16_C, #function
	.proc	04
Plus__uPlus_param_u16_C:
.LLFB30:
	save	%sp, -96, %sp
.LLCFI30:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE30:
	.size	Plus__uPlus_param_u16_C, .-Plus__uPlus_param_u16_C
	.align 4
	.type	Plus__Plus_param_u16_C, #function
	.proc	015
Plus__Plus_param_u16_C:
.LLFB31:
	save	%sp, -96, %sp
.LLCFI31:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL49
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL50
	 nop
.LL49:
	mov	2721, %o0
	call	_m3_fault, 0
	 nop
.LL50:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE31:
	.size	Plus__Plus_param_u16_C, .-Plus__Plus_param_u16_C
	.align 4
	.type	Plus__uPlus_var_u16_u8, #function
	.proc	04
Plus__uPlus_var_u16_u8:
.LLFB32:
	save	%sp, -96, %sp
.LLCFI32:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE32:
	.size	Plus__uPlus_var_u16_u8, .-Plus__uPlus_var_u16_u8
	.align 4
	.type	Plus__Plus_var_u16_u8, #function
	.proc	015
Plus__Plus_var_u16_u8:
.LLFB33:
	save	%sp, -96, %sp
.LLCFI33:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	add	%o5, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL53
	 nop
	mov	2785, %o0
	call	_m3_fault, 0
	 nop
.LL53:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE33:
	.size	Plus__Plus_var_u16_u8, .-Plus__Plus_var_u16_u8
	.align 4
	.type	Plus__uPlus_param_u16_u8, #function
	.proc	04
Plus__uPlus_param_u16_u8:
.LLFB34:
	save	%sp, -96, %sp
.LLCFI34:
	sth	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE34:
	.size	Plus__uPlus_param_u16_u8, .-Plus__uPlus_param_u16_u8
	.align 4
	.type	Plus__Plus_param_u16_u8, #function
	.proc	015
Plus__Plus_param_u16_u8:
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
	add	%o5, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL56
	 nop
	mov	2849, %o0
	call	_m3_fault, 0
	 nop
.LL56:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE35:
	.size	Plus__Plus_param_u16_u8, .-Plus__Plus_param_u16_u8
	.align 4
	.type	Plus__uPlus_var_u16_L, #function
	.proc	05
Plus__uPlus_var_u16_L:
.LLFB36:
	save	%sp, -96, %sp
.LLCFI36:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE36:
	.size	Plus__uPlus_var_u16_L, .-Plus__uPlus_var_u16_L
	.align 4
	.type	Plus__Plus_var_u16_L, #function
	.proc	05
Plus__Plus_var_u16_L:
.LLFB37:
	save	%sp, -96, %sp
.LLCFI37:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE37:
	.size	Plus__Plus_var_u16_L, .-Plus__Plus_var_u16_L
	.align 4
	.type	Plus__uPlus_param_u16_L, #function
	.proc	05
Plus__uPlus_param_u16_L:
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
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE38:
	.size	Plus__uPlus_param_u16_L, .-Plus__uPlus_param_u16_L
	.align 4
	.type	Plus__Plus_param_u16_L, #function
	.proc	05
Plus__Plus_param_u16_L:
.LLFB39:
	save	%sp, -104, %sp
.LLCFI39:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE39:
	.size	Plus__Plus_param_u16_L, .-Plus__Plus_param_u16_L
	.align 4
	.type	Plus__uPlus_var_u16_i8, #function
	.proc	04
Plus__uPlus_var_u16_i8:
.LLFB40:
	save	%sp, -96, %sp
.LLCFI40:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE40:
	.size	Plus__uPlus_var_u16_i8, .-Plus__uPlus_var_u16_i8
	.align 4
	.type	Plus__Plus_var_u16_i8, #function
	.proc	015
Plus__Plus_var_u16_i8:
.LLFB41:
	save	%sp, -96, %sp
.LLCFI41:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	add	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL63
	 nop
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	add	%o5, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL64
	 nop
.LL63:
	mov	3041, %o0
	call	_m3_fault, 0
	 nop
.LL64:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE41:
	.size	Plus__Plus_var_u16_i8, .-Plus__Plus_var_u16_i8
	.align 4
	.type	Plus__uPlus_param_u16_i8, #function
	.proc	04
Plus__uPlus_param_u16_i8:
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
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE42:
	.size	Plus__uPlus_param_u16_i8, .-Plus__uPlus_param_u16_i8
	.align 4
	.type	Plus__Plus_param_u16_i8, #function
	.proc	015
Plus__Plus_param_u16_i8:
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
	add	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL67
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	add	%o5, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL68
	 nop
.LL67:
	mov	3105, %o0
	call	_m3_fault, 0
	 nop
.LL68:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE43:
	.size	Plus__Plus_param_u16_i8, .-Plus__Plus_param_u16_i8
	.align 4
	.type	Plus__uPlus_var_u16_u32, #function
	.proc	04
Plus__uPlus_var_u16_u32:
.LLFB44:
	save	%sp, -96, %sp
.LLCFI44:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE44:
	.size	Plus__uPlus_var_u16_u32, .-Plus__uPlus_var_u16_u32
	.align 4
	.type	Plus__Plus_var_u16_u32, #function
	.proc	015
Plus__Plus_var_u16_u32:
.LLFB45:
	save	%sp, -96, %sp
.LLCFI45:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL71
	 nop
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	add	%o5, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL72
	 nop
.LL71:
	mov	3169, %o0
	call	_m3_fault, 0
	 nop
.LL72:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE45:
	.size	Plus__Plus_var_u16_u32, .-Plus__Plus_var_u16_u32
	.align 4
	.type	Plus__uPlus_param_u16_u32, #function
	.proc	04
Plus__uPlus_param_u16_u32:
.LLFB46:
	save	%sp, -96, %sp
.LLCFI46:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE46:
	.size	Plus__uPlus_param_u16_u32, .-Plus__uPlus_param_u16_u32
	.align 4
	.type	Plus__Plus_param_u16_u32, #function
	.proc	015
Plus__Plus_param_u16_u32:
.LLFB47:
	save	%sp, -96, %sp
.LLCFI47:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL75
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL76
	 nop
.LL75:
	mov	3233, %o0
	call	_m3_fault, 0
	 nop
.LL76:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE47:
	.size	Plus__Plus_param_u16_u32, .-Plus__Plus_param_u16_u32
	.align 4
	.type	Plus__uPlus_var_u64_u16, #function
	.proc	05
Plus__uPlus_var_u64_u16:
.LLFB48:
	save	%sp, -96, %sp
.LLCFI48:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE48:
	.size	Plus__uPlus_var_u64_u16, .-Plus__uPlus_var_u64_u16
	.align 4
	.type	Plus__Plus_var_u64_u16, #function
	.proc	05
Plus__Plus_var_u64_u16:
.LLFB49:
	save	%sp, -96, %sp
.LLCFI49:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE49:
	.size	Plus__Plus_var_u64_u16, .-Plus__Plus_var_u64_u16
	.align 4
	.type	Plus__uPlus_param_u64_u16, #function
	.proc	05
Plus__uPlus_param_u64_u16:
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
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE50:
	.size	Plus__uPlus_param_u64_u16, .-Plus__uPlus_param_u64_u16
	.align 4
	.type	Plus__Plus_param_u64_u16, #function
	.proc	05
Plus__Plus_param_u64_u16:
.LLFB51:
	save	%sp, -104, %sp
.LLCFI51:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE51:
	.size	Plus__Plus_param_u64_u16, .-Plus__Plus_param_u64_u16
	.align 4
	.type	Plus__uPlus_var_u64_u64, #function
	.proc	05
Plus__uPlus_var_u64_u64:
.LLFB52:
	save	%sp, -96, %sp
.LLCFI52:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE52:
	.size	Plus__uPlus_var_u64_u64, .-Plus__uPlus_var_u64_u64
	.align 4
	.type	Plus__Plus_var_u64_u64, #function
	.proc	05
Plus__Plus_var_u64_u64:
.LLFB53:
	save	%sp, -96, %sp
.LLCFI53:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o2
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE53:
	.size	Plus__Plus_var_u64_u64, .-Plus__Plus_var_u64_u64
	.align 4
	.type	Plus__uPlus_param_u64_u64, #function
	.proc	05
Plus__uPlus_param_u64_u64:
.LLFB54:
	save	%sp, -112, %sp
.LLCFI54:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE54:
	.size	Plus__uPlus_param_u64_u64, .-Plus__uPlus_param_u64_u64
	.align 4
	.type	Plus__Plus_param_u64_u64, #function
	.proc	05
Plus__Plus_param_u64_u64:
.LLFB55:
	save	%sp, -112, %sp
.LLCFI55:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE55:
	.size	Plus__Plus_param_u64_u64, .-Plus__Plus_param_u64_u64
	.align 4
	.type	Plus__uPlus_var_u64_LC, #function
	.proc	05
Plus__uPlus_var_u64_LC:
.LLFB56:
	save	%sp, -96, %sp
.LLCFI56:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE56:
	.size	Plus__uPlus_var_u64_LC, .-Plus__uPlus_var_u64_LC
	.align 4
	.type	Plus__Plus_var_u64_LC, #function
	.proc	05
Plus__Plus_var_u64_LC:
.LLFB57:
	save	%sp, -96, %sp
.LLCFI57:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o2
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE57:
	.size	Plus__Plus_var_u64_LC, .-Plus__Plus_var_u64_LC
	.align 4
	.type	Plus__uPlus_param_u64_LC, #function
	.proc	05
Plus__uPlus_param_u64_LC:
.LLFB58:
	save	%sp, -112, %sp
.LLCFI58:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE58:
	.size	Plus__uPlus_param_u64_LC, .-Plus__uPlus_param_u64_LC
	.align 4
	.type	Plus__Plus_param_u64_LC, #function
	.proc	05
Plus__Plus_param_u64_LC:
.LLFB59:
	save	%sp, -112, %sp
.LLCFI59:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o2
	ldd	[%fp-8], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE59:
	.size	Plus__Plus_param_u64_LC, .-Plus__Plus_param_u64_LC
	.align 4
	.type	Plus__uPlus_var_u64_i32, #function
	.proc	05
Plus__uPlus_var_u64_i32:
.LLFB60:
	save	%sp, -96, %sp
.LLCFI60:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE60:
	.size	Plus__uPlus_var_u64_i32, .-Plus__uPlus_var_u64_i32
	.align 4
	.type	Plus__Plus_var_u64_i32, #function
	.proc	05
Plus__Plus_var_u64_i32:
.LLFB61:
	save	%sp, -96, %sp
.LLCFI61:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE61:
	.size	Plus__Plus_var_u64_i32, .-Plus__Plus_var_u64_i32
	.align 4
	.type	Plus__uPlus_param_u64_i32, #function
	.proc	05
Plus__uPlus_param_u64_i32:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE62:
	.size	Plus__uPlus_param_u64_i32, .-Plus__uPlus_param_u64_i32
	.align 4
	.type	Plus__Plus_param_u64_i32, #function
	.proc	05
Plus__Plus_param_u64_i32:
.LLFB63:
	save	%sp, -104, %sp
.LLCFI63:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE63:
	.size	Plus__Plus_param_u64_i32, .-Plus__Plus_param_u64_i32
	.align 4
	.type	Plus__uPlus_var_u64_i16, #function
	.proc	05
Plus__uPlus_var_u64_i16:
.LLFB64:
	save	%sp, -96, %sp
.LLCFI64:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE64:
	.size	Plus__uPlus_var_u64_i16, .-Plus__uPlus_var_u64_i16
	.align 4
	.type	Plus__Plus_var_u64_i16, #function
	.proc	05
Plus__Plus_var_u64_i16:
.LLFB65:
	save	%sp, -96, %sp
.LLCFI65:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE65:
	.size	Plus__Plus_var_u64_i16, .-Plus__Plus_var_u64_i16
	.align 4
	.type	Plus__uPlus_param_u64_i16, #function
	.proc	05
Plus__uPlus_param_u64_i16:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE66:
	.size	Plus__uPlus_param_u64_i16, .-Plus__uPlus_param_u64_i16
	.align 4
	.type	Plus__Plus_param_u64_i16, #function
	.proc	05
Plus__Plus_param_u64_i16:
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
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE67:
	.size	Plus__Plus_param_u64_i16, .-Plus__Plus_param_u64_i16
	.align 4
	.type	Plus__uPlus_var_u64_I, #function
	.proc	05
Plus__uPlus_var_u64_I:
.LLFB68:
	save	%sp, -96, %sp
.LLCFI68:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE68:
	.size	Plus__uPlus_var_u64_I, .-Plus__uPlus_var_u64_I
	.align 4
	.type	Plus__Plus_var_u64_I, #function
	.proc	05
Plus__Plus_var_u64_I:
.LLFB69:
	save	%sp, -96, %sp
.LLCFI69:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE69:
	.size	Plus__Plus_var_u64_I, .-Plus__Plus_var_u64_I
	.align 4
	.type	Plus__uPlus_param_u64_I, #function
	.proc	05
Plus__uPlus_param_u64_I:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE70:
	.size	Plus__uPlus_param_u64_I, .-Plus__uPlus_param_u64_I
	.align 4
	.type	Plus__Plus_param_u64_I, #function
	.proc	05
Plus__Plus_param_u64_I:
.LLFB71:
	save	%sp, -104, %sp
.LLCFI71:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE71:
	.size	Plus__Plus_param_u64_I, .-Plus__Plus_param_u64_I
	.align 4
	.type	Plus__uPlus_var_u64_i64, #function
	.proc	05
Plus__uPlus_var_u64_i64:
.LLFB72:
	save	%sp, -96, %sp
.LLCFI72:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE72:
	.size	Plus__uPlus_var_u64_i64, .-Plus__uPlus_var_u64_i64
	.align 4
	.type	Plus__Plus_var_u64_i64, #function
	.proc	05
Plus__Plus_var_u64_i64:
.LLFB73:
	save	%sp, -96, %sp
.LLCFI73:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o2
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE73:
	.size	Plus__Plus_var_u64_i64, .-Plus__Plus_var_u64_i64
	.align 4
	.type	Plus__uPlus_param_u64_i64, #function
	.proc	05
Plus__uPlus_param_u64_i64:
.LLFB74:
	save	%sp, -112, %sp
.LLCFI74:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE74:
	.size	Plus__uPlus_param_u64_i64, .-Plus__uPlus_param_u64_i64
	.align 4
	.type	Plus__Plus_param_u64_i64, #function
	.proc	05
Plus__Plus_param_u64_i64:
.LLFB75:
	save	%sp, -112, %sp
.LLCFI75:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE75:
	.size	Plus__Plus_param_u64_i64, .-Plus__Plus_param_u64_i64
	.align 4
	.type	Plus__uPlus_var_u64_C, #function
	.proc	05
Plus__uPlus_var_u64_C:
.LLFB76:
	save	%sp, -96, %sp
.LLCFI76:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE76:
	.size	Plus__uPlus_var_u64_C, .-Plus__uPlus_var_u64_C
	.align 4
	.type	Plus__Plus_var_u64_C, #function
	.proc	05
Plus__Plus_var_u64_C:
.LLFB77:
	save	%sp, -96, %sp
.LLCFI77:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE77:
	.size	Plus__Plus_var_u64_C, .-Plus__Plus_var_u64_C
	.align 4
	.type	Plus__uPlus_param_u64_C, #function
	.proc	05
Plus__uPlus_param_u64_C:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE78:
	.size	Plus__uPlus_param_u64_C, .-Plus__uPlus_param_u64_C
	.align 4
	.type	Plus__Plus_param_u64_C, #function
	.proc	05
Plus__Plus_param_u64_C:
.LLFB79:
	save	%sp, -104, %sp
.LLCFI79:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE79:
	.size	Plus__Plus_param_u64_C, .-Plus__Plus_param_u64_C
	.align 4
	.type	Plus__uPlus_var_u64_u8, #function
	.proc	05
Plus__uPlus_var_u64_u8:
.LLFB80:
	save	%sp, -96, %sp
.LLCFI80:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE80:
	.size	Plus__uPlus_var_u64_u8, .-Plus__uPlus_var_u64_u8
	.align 4
	.type	Plus__Plus_var_u64_u8, #function
	.proc	05
Plus__Plus_var_u64_u8:
.LLFB81:
	save	%sp, -96, %sp
.LLCFI81:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE81:
	.size	Plus__Plus_var_u64_u8, .-Plus__Plus_var_u64_u8
	.align 4
	.type	Plus__uPlus_param_u64_u8, #function
	.proc	05
Plus__uPlus_param_u64_u8:
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
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE82:
	.size	Plus__uPlus_param_u64_u8, .-Plus__uPlus_param_u64_u8
	.align 4
	.type	Plus__Plus_param_u64_u8, #function
	.proc	05
Plus__Plus_param_u64_u8:
.LLFB83:
	save	%sp, -104, %sp
.LLCFI83:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE83:
	.size	Plus__Plus_param_u64_u8, .-Plus__Plus_param_u64_u8
	.align 4
	.type	Plus__uPlus_var_u64_L, #function
	.proc	05
Plus__uPlus_var_u64_L:
.LLFB84:
	save	%sp, -96, %sp
.LLCFI84:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE84:
	.size	Plus__uPlus_var_u64_L, .-Plus__uPlus_var_u64_L
	.align 4
	.type	Plus__Plus_var_u64_L, #function
	.proc	05
Plus__Plus_var_u64_L:
.LLFB85:
	save	%sp, -96, %sp
.LLCFI85:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o2
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE85:
	.size	Plus__Plus_var_u64_L, .-Plus__Plus_var_u64_L
	.align 4
	.type	Plus__uPlus_param_u64_L, #function
	.proc	05
Plus__uPlus_param_u64_L:
.LLFB86:
	save	%sp, -112, %sp
.LLCFI86:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE86:
	.size	Plus__uPlus_param_u64_L, .-Plus__uPlus_param_u64_L
	.align 4
	.type	Plus__Plus_param_u64_L, #function
	.proc	05
Plus__Plus_param_u64_L:
.LLFB87:
	save	%sp, -112, %sp
.LLCFI87:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE87:
	.size	Plus__Plus_param_u64_L, .-Plus__Plus_param_u64_L
	.align 4
	.type	Plus__uPlus_var_u64_i8, #function
	.proc	05
Plus__uPlus_var_u64_i8:
.LLFB88:
	save	%sp, -96, %sp
.LLCFI88:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE88:
	.size	Plus__uPlus_var_u64_i8, .-Plus__uPlus_var_u64_i8
	.align 4
	.type	Plus__Plus_var_u64_i8, #function
	.proc	05
Plus__Plus_var_u64_i8:
.LLFB89:
	save	%sp, -96, %sp
.LLCFI89:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE89:
	.size	Plus__Plus_var_u64_i8, .-Plus__Plus_var_u64_i8
	.align 4
	.type	Plus__uPlus_param_u64_i8, #function
	.proc	05
Plus__uPlus_param_u64_i8:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE90:
	.size	Plus__uPlus_param_u64_i8, .-Plus__uPlus_param_u64_i8
	.align 4
	.type	Plus__Plus_param_u64_i8, #function
	.proc	05
Plus__Plus_param_u64_i8:
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
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE91:
	.size	Plus__Plus_param_u64_i8, .-Plus__Plus_param_u64_i8
	.align 4
	.type	Plus__uPlus_var_u64_u32, #function
	.proc	05
Plus__uPlus_var_u64_u32:
.LLFB92:
	save	%sp, -96, %sp
.LLCFI92:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE92:
	.size	Plus__uPlus_var_u64_u32, .-Plus__uPlus_var_u64_u32
	.align 4
	.type	Plus__Plus_var_u64_u32, #function
	.proc	05
Plus__Plus_var_u64_u32:
.LLFB93:
	save	%sp, -96, %sp
.LLCFI93:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE93:
	.size	Plus__Plus_var_u64_u32, .-Plus__Plus_var_u64_u32
	.align 4
	.type	Plus__uPlus_param_u64_u32, #function
	.proc	05
Plus__uPlus_param_u64_u32:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE94:
	.size	Plus__uPlus_param_u64_u32, .-Plus__uPlus_param_u64_u32
	.align 4
	.type	Plus__Plus_param_u64_u32, #function
	.proc	05
Plus__Plus_param_u64_u32:
.LLFB95:
	save	%sp, -104, %sp
.LLCFI95:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE95:
	.size	Plus__Plus_param_u64_u32, .-Plus__Plus_param_u64_u32
	.align 4
	.type	Plus__uPlus_var_LC_u16, #function
	.proc	05
Plus__uPlus_var_LC_u16:
.LLFB96:
	save	%sp, -96, %sp
.LLCFI96:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE96:
	.size	Plus__uPlus_var_LC_u16, .-Plus__uPlus_var_LC_u16
	.align 4
	.type	Plus__Plus_var_LC_u16, #function
	.proc	05
Plus__Plus_var_LC_u16:
.LLFB97:
	save	%sp, -96, %sp
.LLCFI97:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE97:
	.size	Plus__Plus_var_LC_u16, .-Plus__Plus_var_LC_u16
	.align 4
	.type	Plus__uPlus_param_LC_u16, #function
	.proc	05
Plus__uPlus_param_LC_u16:
.LLFB98:
	save	%sp, -104, %sp
.LLCFI98:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE98:
	.size	Plus__uPlus_param_LC_u16, .-Plus__uPlus_param_LC_u16
	.align 4
	.type	Plus__Plus_param_LC_u16, #function
	.proc	05
Plus__Plus_param_LC_u16:
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
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE99:
	.size	Plus__Plus_param_LC_u16, .-Plus__Plus_param_LC_u16
	.align 4
	.type	Plus__uPlus_var_LC_u64, #function
	.proc	05
Plus__uPlus_var_LC_u64:
.LLFB100:
	save	%sp, -96, %sp
.LLCFI100:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o2
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE100:
	.size	Plus__uPlus_var_LC_u64, .-Plus__uPlus_var_LC_u64
	.align 4
	.type	Plus__Plus_var_LC_u64, #function
	.proc	05
Plus__Plus_var_LC_u64:
.LLFB101:
	save	%sp, -96, %sp
.LLCFI101:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE101:
	.size	Plus__Plus_var_LC_u64, .-Plus__Plus_var_LC_u64
	.align 4
	.type	Plus__uPlus_param_LC_u64, #function
	.proc	05
Plus__uPlus_param_LC_u64:
.LLFB102:
	save	%sp, -112, %sp
.LLCFI102:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o2
	ldd	[%fp-8], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE102:
	.size	Plus__uPlus_param_LC_u64, .-Plus__uPlus_param_LC_u64
	.align 4
	.type	Plus__Plus_param_LC_u64, #function
	.proc	05
Plus__Plus_param_LC_u64:
.LLFB103:
	save	%sp, -112, %sp
.LLCFI103:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE103:
	.size	Plus__Plus_param_LC_u64, .-Plus__Plus_param_LC_u64
	.align 4
	.type	Plus__uPlus_var_LC_LC, #function
	.proc	05
Plus__uPlus_var_LC_LC:
.LLFB104:
	save	%sp, -96, %sp
.LLCFI104:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o2
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE104:
	.size	Plus__uPlus_var_LC_LC, .-Plus__uPlus_var_LC_LC
	.align 4
	.type	Plus__Plus_var_LC_LC, #function
	.proc	05
Plus__Plus_var_LC_LC:
.LLFB105:
	save	%sp, -96, %sp
.LLCFI105:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE105:
	.size	Plus__Plus_var_LC_LC, .-Plus__Plus_var_LC_LC
	.align 4
	.type	Plus__uPlus_param_LC_LC, #function
	.proc	05
Plus__uPlus_param_LC_LC:
.LLFB106:
	save	%sp, -112, %sp
.LLCFI106:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE106:
	.size	Plus__uPlus_param_LC_LC, .-Plus__uPlus_param_LC_LC
	.align 4
	.type	Plus__Plus_param_LC_LC, #function
	.proc	05
Plus__Plus_param_LC_LC:
.LLFB107:
	save	%sp, -112, %sp
.LLCFI107:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE107:
	.size	Plus__Plus_param_LC_LC, .-Plus__Plus_param_LC_LC
	.align 4
	.type	Plus__uPlus_var_LC_i32, #function
	.proc	05
Plus__uPlus_var_LC_i32:
.LLFB108:
	save	%sp, -96, %sp
.LLCFI108:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE108:
	.size	Plus__uPlus_var_LC_i32, .-Plus__uPlus_var_LC_i32
	.align 4
	.type	Plus__Plus_var_LC_i32, #function
	.proc	05
Plus__Plus_var_LC_i32:
.LLFB109:
	save	%sp, -96, %sp
.LLCFI109:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE109:
	.size	Plus__Plus_var_LC_i32, .-Plus__Plus_var_LC_i32
	.align 4
	.type	Plus__uPlus_param_LC_i32, #function
	.proc	05
Plus__uPlus_param_LC_i32:
.LLFB110:
	save	%sp, -104, %sp
.LLCFI110:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE110:
	.size	Plus__uPlus_param_LC_i32, .-Plus__uPlus_param_LC_i32
	.align 4
	.type	Plus__Plus_param_LC_i32, #function
	.proc	05
Plus__Plus_param_LC_i32:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE111:
	.size	Plus__Plus_param_LC_i32, .-Plus__Plus_param_LC_i32
	.align 4
	.type	Plus__uPlus_var_LC_i16, #function
	.proc	05
Plus__uPlus_var_LC_i16:
.LLFB112:
	save	%sp, -96, %sp
.LLCFI112:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE112:
	.size	Plus__uPlus_var_LC_i16, .-Plus__uPlus_var_LC_i16
	.align 4
	.type	Plus__Plus_var_LC_i16, #function
	.proc	05
Plus__Plus_var_LC_i16:
.LLFB113:
	save	%sp, -96, %sp
.LLCFI113:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE113:
	.size	Plus__Plus_var_LC_i16, .-Plus__Plus_var_LC_i16
	.align 4
	.type	Plus__uPlus_param_LC_i16, #function
	.proc	05
Plus__uPlus_param_LC_i16:
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
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE114:
	.size	Plus__uPlus_param_LC_i16, .-Plus__uPlus_param_LC_i16
	.align 4
	.type	Plus__Plus_param_LC_i16, #function
	.proc	05
Plus__Plus_param_LC_i16:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE115:
	.size	Plus__Plus_param_LC_i16, .-Plus__Plus_param_LC_i16
	.align 4
	.type	Plus__uPlus_var_LC_I, #function
	.proc	05
Plus__uPlus_var_LC_I:
.LLFB116:
	save	%sp, -96, %sp
.LLCFI116:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE116:
	.size	Plus__uPlus_var_LC_I, .-Plus__uPlus_var_LC_I
	.align 4
	.type	Plus__Plus_var_LC_I, #function
	.proc	05
Plus__Plus_var_LC_I:
.LLFB117:
	save	%sp, -96, %sp
.LLCFI117:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE117:
	.size	Plus__Plus_var_LC_I, .-Plus__Plus_var_LC_I
	.align 4
	.type	Plus__uPlus_param_LC_I, #function
	.proc	05
Plus__uPlus_param_LC_I:
.LLFB118:
	save	%sp, -104, %sp
.LLCFI118:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE118:
	.size	Plus__uPlus_param_LC_I, .-Plus__uPlus_param_LC_I
	.align 4
	.type	Plus__Plus_param_LC_I, #function
	.proc	05
Plus__Plus_param_LC_I:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE119:
	.size	Plus__Plus_param_LC_I, .-Plus__Plus_param_LC_I
	.align 4
	.type	Plus__uPlus_var_LC_i64, #function
	.proc	05
Plus__uPlus_var_LC_i64:
.LLFB120:
	save	%sp, -96, %sp
.LLCFI120:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o2
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE120:
	.size	Plus__uPlus_var_LC_i64, .-Plus__uPlus_var_LC_i64
	.align 4
	.type	Plus__Plus_var_LC_i64, #function
	.proc	05
Plus__Plus_var_LC_i64:
.LLFB121:
	save	%sp, -96, %sp
.LLCFI121:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE121:
	.size	Plus__Plus_var_LC_i64, .-Plus__Plus_var_LC_i64
	.align 4
	.type	Plus__uPlus_param_LC_i64, #function
	.proc	05
Plus__uPlus_param_LC_i64:
.LLFB122:
	save	%sp, -112, %sp
.LLCFI122:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o2
	ldd	[%fp-8], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE122:
	.size	Plus__uPlus_param_LC_i64, .-Plus__uPlus_param_LC_i64
	.align 4
	.type	Plus__Plus_param_LC_i64, #function
	.proc	05
Plus__Plus_param_LC_i64:
.LLFB123:
	save	%sp, -112, %sp
.LLCFI123:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE123:
	.size	Plus__Plus_param_LC_i64, .-Plus__Plus_param_LC_i64
	.align 4
	.type	Plus__uPlus_var_LC_C, #function
	.proc	05
Plus__uPlus_var_LC_C:
.LLFB124:
	save	%sp, -96, %sp
.LLCFI124:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE124:
	.size	Plus__uPlus_var_LC_C, .-Plus__uPlus_var_LC_C
	.align 4
	.type	Plus__Plus_var_LC_C, #function
	.proc	05
Plus__Plus_var_LC_C:
.LLFB125:
	save	%sp, -96, %sp
.LLCFI125:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE125:
	.size	Plus__Plus_var_LC_C, .-Plus__Plus_var_LC_C
	.align 4
	.type	Plus__uPlus_param_LC_C, #function
	.proc	05
Plus__uPlus_param_LC_C:
.LLFB126:
	save	%sp, -104, %sp
.LLCFI126:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE126:
	.size	Plus__uPlus_param_LC_C, .-Plus__uPlus_param_LC_C
	.align 4
	.type	Plus__Plus_param_LC_C, #function
	.proc	05
Plus__Plus_param_LC_C:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE127:
	.size	Plus__Plus_param_LC_C, .-Plus__Plus_param_LC_C
	.align 4
	.type	Plus__uPlus_var_LC_u8, #function
	.proc	05
Plus__uPlus_var_LC_u8:
.LLFB128:
	save	%sp, -96, %sp
.LLCFI128:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE128:
	.size	Plus__uPlus_var_LC_u8, .-Plus__uPlus_var_LC_u8
	.align 4
	.type	Plus__Plus_var_LC_u8, #function
	.proc	05
Plus__Plus_var_LC_u8:
.LLFB129:
	save	%sp, -96, %sp
.LLCFI129:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE129:
	.size	Plus__Plus_var_LC_u8, .-Plus__Plus_var_LC_u8
	.align 4
	.type	Plus__uPlus_param_LC_u8, #function
	.proc	05
Plus__uPlus_param_LC_u8:
.LLFB130:
	save	%sp, -104, %sp
.LLCFI130:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE130:
	.size	Plus__uPlus_param_LC_u8, .-Plus__uPlus_param_LC_u8
	.align 4
	.type	Plus__Plus_param_LC_u8, #function
	.proc	05
Plus__Plus_param_LC_u8:
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
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE131:
	.size	Plus__Plus_param_LC_u8, .-Plus__Plus_param_LC_u8
	.align 4
	.type	Plus__uPlus_var_LC_L, #function
	.proc	05
Plus__uPlus_var_LC_L:
.LLFB132:
	save	%sp, -96, %sp
.LLCFI132:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o2
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE132:
	.size	Plus__uPlus_var_LC_L, .-Plus__uPlus_var_LC_L
	.align 4
	.type	Plus__Plus_var_LC_L, #function
	.proc	05
Plus__Plus_var_LC_L:
.LLFB133:
	save	%sp, -96, %sp
.LLCFI133:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE133:
	.size	Plus__Plus_var_LC_L, .-Plus__Plus_var_LC_L
	.align 4
	.type	Plus__uPlus_param_LC_L, #function
	.proc	05
Plus__uPlus_param_LC_L:
.LLFB134:
	save	%sp, -112, %sp
.LLCFI134:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o2
	ldd	[%fp-8], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE134:
	.size	Plus__uPlus_param_LC_L, .-Plus__uPlus_param_LC_L
	.align 4
	.type	Plus__Plus_param_LC_L, #function
	.proc	05
Plus__Plus_param_LC_L:
.LLFB135:
	save	%sp, -112, %sp
.LLCFI135:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE135:
	.size	Plus__Plus_param_LC_L, .-Plus__Plus_param_LC_L
	.align 4
	.type	Plus__uPlus_var_LC_i8, #function
	.proc	05
Plus__uPlus_var_LC_i8:
.LLFB136:
	save	%sp, -96, %sp
.LLCFI136:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE136:
	.size	Plus__uPlus_var_LC_i8, .-Plus__uPlus_var_LC_i8
	.align 4
	.type	Plus__Plus_var_LC_i8, #function
	.proc	05
Plus__Plus_var_LC_i8:
.LLFB137:
	save	%sp, -96, %sp
.LLCFI137:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE137:
	.size	Plus__Plus_var_LC_i8, .-Plus__Plus_var_LC_i8
	.align 4
	.type	Plus__uPlus_param_LC_i8, #function
	.proc	05
Plus__uPlus_param_LC_i8:
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
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE138:
	.size	Plus__uPlus_param_LC_i8, .-Plus__uPlus_param_LC_i8
	.align 4
	.type	Plus__Plus_param_LC_i8, #function
	.proc	05
Plus__Plus_param_LC_i8:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE139:
	.size	Plus__Plus_param_LC_i8, .-Plus__Plus_param_LC_i8
	.align 4
	.type	Plus__uPlus_var_LC_u32, #function
	.proc	05
Plus__uPlus_var_LC_u32:
.LLFB140:
	save	%sp, -96, %sp
.LLCFI140:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE140:
	.size	Plus__uPlus_var_LC_u32, .-Plus__uPlus_var_LC_u32
	.align 4
	.type	Plus__Plus_var_LC_u32, #function
	.proc	05
Plus__Plus_var_LC_u32:
.LLFB141:
	save	%sp, -96, %sp
.LLCFI141:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE141:
	.size	Plus__Plus_var_LC_u32, .-Plus__Plus_var_LC_u32
	.align 4
	.type	Plus__uPlus_param_LC_u32, #function
	.proc	05
Plus__uPlus_param_LC_u32:
.LLFB142:
	save	%sp, -104, %sp
.LLCFI142:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE142:
	.size	Plus__uPlus_param_LC_u32, .-Plus__uPlus_param_LC_u32
	.align 4
	.type	Plus__Plus_param_LC_u32, #function
	.proc	05
Plus__Plus_param_LC_u32:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE143:
	.size	Plus__Plus_param_LC_u32, .-Plus__Plus_param_LC_u32
	.align 4
	.type	Plus__uPlus_var_i32_u16, #function
	.proc	04
Plus__uPlus_var_i32_u16:
.LLFB144:
	save	%sp, -96, %sp
.LLCFI144:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE144:
	.size	Plus__uPlus_var_i32_u16, .-Plus__uPlus_var_i32_u16
	.align 4
	.type	Plus__Plus_var_i32_u16, #function
	.proc	04
Plus__Plus_var_i32_u16:
.LLFB145:
	save	%sp, -96, %sp
.LLCFI145:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE145:
	.size	Plus__Plus_var_i32_u16, .-Plus__Plus_var_i32_u16
	.align 4
	.type	Plus__uPlus_param_i32_u16, #function
	.proc	04
Plus__uPlus_param_i32_u16:
.LLFB146:
	save	%sp, -96, %sp
.LLCFI146:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+68], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE146:
	.size	Plus__uPlus_param_i32_u16, .-Plus__uPlus_param_i32_u16
	.align 4
	.type	Plus__Plus_param_i32_u16, #function
	.proc	04
Plus__Plus_param_i32_u16:
.LLFB147:
	save	%sp, -96, %sp
.LLCFI147:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+68], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE147:
	.size	Plus__Plus_param_i32_u16, .-Plus__Plus_param_i32_u16
	.align 4
	.type	Plus__uPlus_var_i32_u64, #function
	.proc	05
Plus__uPlus_var_i32_u64:
.LLFB148:
	save	%sp, -96, %sp
.LLCFI148:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE148:
	.size	Plus__uPlus_var_i32_u64, .-Plus__uPlus_var_i32_u64
	.align 4
	.type	Plus__Plus_var_i32_u64, #function
	.proc	05
Plus__Plus_var_i32_u64:
.LLFB149:
	save	%sp, -96, %sp
.LLCFI149:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE149:
	.size	Plus__Plus_var_i32_u64, .-Plus__Plus_var_i32_u64
	.align 4
	.type	Plus__uPlus_param_i32_u64, #function
	.proc	05
Plus__uPlus_param_i32_u64:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE150:
	.size	Plus__uPlus_param_i32_u64, .-Plus__uPlus_param_i32_u64
	.align 4
	.type	Plus__Plus_param_i32_u64, #function
	.proc	05
Plus__Plus_param_i32_u64:
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
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE151:
	.size	Plus__Plus_param_i32_u64, .-Plus__Plus_param_i32_u64
	.align 4
	.type	Plus__uPlus_var_i32_LC, #function
	.proc	05
Plus__uPlus_var_i32_LC:
.LLFB152:
	save	%sp, -96, %sp
.LLCFI152:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE152:
	.size	Plus__uPlus_var_i32_LC, .-Plus__uPlus_var_i32_LC
	.align 4
	.type	Plus__Plus_var_i32_LC, #function
	.proc	05
Plus__Plus_var_i32_LC:
.LLFB153:
	save	%sp, -96, %sp
.LLCFI153:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE153:
	.size	Plus__Plus_var_i32_LC, .-Plus__Plus_var_i32_LC
	.align 4
	.type	Plus__uPlus_param_i32_LC, #function
	.proc	05
Plus__uPlus_param_i32_LC:
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
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE154:
	.size	Plus__uPlus_param_i32_LC, .-Plus__uPlus_param_i32_LC
	.align 4
	.type	Plus__Plus_param_i32_LC, #function
	.proc	05
Plus__Plus_param_i32_LC:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE155:
	.size	Plus__Plus_param_i32_LC, .-Plus__Plus_param_i32_LC
	.align 4
	.type	Plus__uPlus_var_i32_i32, #function
	.proc	04
Plus__uPlus_var_i32_i32:
.LLFB156:
	save	%sp, -96, %sp
.LLCFI156:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE156:
	.size	Plus__uPlus_var_i32_i32, .-Plus__uPlus_var_i32_i32
	.align 4
	.type	Plus__Plus_var_i32_i32, #function
	.proc	04
Plus__Plus_var_i32_i32:
.LLFB157:
	save	%sp, -96, %sp
.LLCFI157:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE157:
	.size	Plus__Plus_var_i32_i32, .-Plus__Plus_var_i32_i32
	.align 4
	.type	Plus__uPlus_param_i32_i32, #function
	.proc	04
Plus__uPlus_param_i32_i32:
.LLFB158:
	save	%sp, -96, %sp
.LLCFI158:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE158:
	.size	Plus__uPlus_param_i32_i32, .-Plus__uPlus_param_i32_i32
	.align 4
	.type	Plus__Plus_param_i32_i32, #function
	.proc	04
Plus__Plus_param_i32_i32:
.LLFB159:
	save	%sp, -96, %sp
.LLCFI159:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE159:
	.size	Plus__Plus_param_i32_i32, .-Plus__Plus_param_i32_i32
	.align 4
	.type	Plus__uPlus_var_i32_i16, #function
	.proc	04
Plus__uPlus_var_i32_i16:
.LLFB160:
	save	%sp, -96, %sp
.LLCFI160:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE160:
	.size	Plus__uPlus_var_i32_i16, .-Plus__uPlus_var_i32_i16
	.align 4
	.type	Plus__Plus_var_i32_i16, #function
	.proc	04
Plus__Plus_var_i32_i16:
.LLFB161:
	save	%sp, -96, %sp
.LLCFI161:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE161:
	.size	Plus__Plus_var_i32_i16, .-Plus__Plus_var_i32_i16
	.align 4
	.type	Plus__uPlus_param_i32_i16, #function
	.proc	04
Plus__uPlus_param_i32_i16:
.LLFB162:
	save	%sp, -96, %sp
.LLCFI162:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+68], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE162:
	.size	Plus__uPlus_param_i32_i16, .-Plus__uPlus_param_i32_i16
	.align 4
	.type	Plus__Plus_param_i32_i16, #function
	.proc	04
Plus__Plus_param_i32_i16:
.LLFB163:
	save	%sp, -96, %sp
.LLCFI163:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+68], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE163:
	.size	Plus__Plus_param_i32_i16, .-Plus__Plus_param_i32_i16
	.align 4
	.type	Plus__uPlus_var_i32_I, #function
	.proc	04
Plus__uPlus_var_i32_I:
.LLFB164:
	save	%sp, -96, %sp
.LLCFI164:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE164:
	.size	Plus__uPlus_var_i32_I, .-Plus__uPlus_var_i32_I
	.align 4
	.type	Plus__Plus_var_i32_I, #function
	.proc	04
Plus__Plus_var_i32_I:
.LLFB165:
	save	%sp, -96, %sp
.LLCFI165:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE165:
	.size	Plus__Plus_var_i32_I, .-Plus__Plus_var_i32_I
	.align 4
	.type	Plus__uPlus_param_i32_I, #function
	.proc	04
Plus__uPlus_param_i32_I:
.LLFB166:
	save	%sp, -96, %sp
.LLCFI166:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE166:
	.size	Plus__uPlus_param_i32_I, .-Plus__uPlus_param_i32_I
	.align 4
	.type	Plus__Plus_param_i32_I, #function
	.proc	04
Plus__Plus_param_i32_I:
.LLFB167:
	save	%sp, -96, %sp
.LLCFI167:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE167:
	.size	Plus__Plus_param_i32_I, .-Plus__Plus_param_i32_I
	.align 4
	.type	Plus__uPlus_var_i32_i64, #function
	.proc	05
Plus__uPlus_var_i32_i64:
.LLFB168:
	save	%sp, -96, %sp
.LLCFI168:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE168:
	.size	Plus__uPlus_var_i32_i64, .-Plus__uPlus_var_i32_i64
	.align 4
	.type	Plus__Plus_var_i32_i64, #function
	.proc	05
Plus__Plus_var_i32_i64:
.LLFB169:
	save	%sp, -96, %sp
.LLCFI169:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE169:
	.size	Plus__Plus_var_i32_i64, .-Plus__Plus_var_i32_i64
	.align 4
	.type	Plus__uPlus_param_i32_i64, #function
	.proc	05
Plus__uPlus_param_i32_i64:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE170:
	.size	Plus__uPlus_param_i32_i64, .-Plus__uPlus_param_i32_i64
	.align 4
	.type	Plus__Plus_param_i32_i64, #function
	.proc	05
Plus__Plus_param_i32_i64:
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
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE171:
	.size	Plus__Plus_param_i32_i64, .-Plus__Plus_param_i32_i64
	.align 4
	.type	Plus__uPlus_var_i32_C, #function
	.proc	04
Plus__uPlus_var_i32_C:
.LLFB172:
	save	%sp, -96, %sp
.LLCFI172:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE172:
	.size	Plus__uPlus_var_i32_C, .-Plus__uPlus_var_i32_C
	.align 4
	.type	Plus__Plus_var_i32_C, #function
	.proc	04
Plus__Plus_var_i32_C:
.LLFB173:
	save	%sp, -96, %sp
.LLCFI173:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE173:
	.size	Plus__Plus_var_i32_C, .-Plus__Plus_var_i32_C
	.align 4
	.type	Plus__uPlus_param_i32_C, #function
	.proc	04
Plus__uPlus_param_i32_C:
.LLFB174:
	save	%sp, -96, %sp
.LLCFI174:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE174:
	.size	Plus__uPlus_param_i32_C, .-Plus__uPlus_param_i32_C
	.align 4
	.type	Plus__Plus_param_i32_C, #function
	.proc	04
Plus__Plus_param_i32_C:
.LLFB175:
	save	%sp, -96, %sp
.LLCFI175:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE175:
	.size	Plus__Plus_param_i32_C, .-Plus__Plus_param_i32_C
	.align 4
	.type	Plus__uPlus_var_i32_u8, #function
	.proc	04
Plus__uPlus_var_i32_u8:
.LLFB176:
	save	%sp, -96, %sp
.LLCFI176:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE176:
	.size	Plus__uPlus_var_i32_u8, .-Plus__uPlus_var_i32_u8
	.align 4
	.type	Plus__Plus_var_i32_u8, #function
	.proc	04
Plus__Plus_var_i32_u8:
.LLFB177:
	save	%sp, -96, %sp
.LLCFI177:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE177:
	.size	Plus__Plus_var_i32_u8, .-Plus__Plus_var_i32_u8
	.align 4
	.type	Plus__uPlus_param_i32_u8, #function
	.proc	04
Plus__uPlus_param_i32_u8:
.LLFB178:
	save	%sp, -96, %sp
.LLCFI178:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+68], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE178:
	.size	Plus__uPlus_param_i32_u8, .-Plus__uPlus_param_i32_u8
	.align 4
	.type	Plus__Plus_param_i32_u8, #function
	.proc	04
Plus__Plus_param_i32_u8:
.LLFB179:
	save	%sp, -96, %sp
.LLCFI179:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+68], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE179:
	.size	Plus__Plus_param_i32_u8, .-Plus__Plus_param_i32_u8
	.align 4
	.type	Plus__uPlus_var_i32_L, #function
	.proc	05
Plus__uPlus_var_i32_L:
.LLFB180:
	save	%sp, -96, %sp
.LLCFI180:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE180:
	.size	Plus__uPlus_var_i32_L, .-Plus__uPlus_var_i32_L
	.align 4
	.type	Plus__Plus_var_i32_L, #function
	.proc	05
Plus__Plus_var_i32_L:
.LLFB181:
	save	%sp, -96, %sp
.LLCFI181:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE181:
	.size	Plus__Plus_var_i32_L, .-Plus__Plus_var_i32_L
	.align 4
	.type	Plus__uPlus_param_i32_L, #function
	.proc	05
Plus__uPlus_param_i32_L:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE182:
	.size	Plus__uPlus_param_i32_L, .-Plus__uPlus_param_i32_L
	.align 4
	.type	Plus__Plus_param_i32_L, #function
	.proc	05
Plus__Plus_param_i32_L:
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
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE183:
	.size	Plus__Plus_param_i32_L, .-Plus__Plus_param_i32_L
	.align 4
	.type	Plus__uPlus_var_i32_i8, #function
	.proc	04
Plus__uPlus_var_i32_i8:
.LLFB184:
	save	%sp, -96, %sp
.LLCFI184:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE184:
	.size	Plus__uPlus_var_i32_i8, .-Plus__uPlus_var_i32_i8
	.align 4
	.type	Plus__Plus_var_i32_i8, #function
	.proc	04
Plus__Plus_var_i32_i8:
.LLFB185:
	save	%sp, -96, %sp
.LLCFI185:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE185:
	.size	Plus__Plus_var_i32_i8, .-Plus__Plus_var_i32_i8
	.align 4
	.type	Plus__uPlus_param_i32_i8, #function
	.proc	04
Plus__uPlus_param_i32_i8:
.LLFB186:
	save	%sp, -96, %sp
.LLCFI186:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+68], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE186:
	.size	Plus__uPlus_param_i32_i8, .-Plus__uPlus_param_i32_i8
	.align 4
	.type	Plus__Plus_param_i32_i8, #function
	.proc	04
Plus__Plus_param_i32_i8:
.LLFB187:
	save	%sp, -96, %sp
.LLCFI187:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+68], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE187:
	.size	Plus__Plus_param_i32_i8, .-Plus__Plus_param_i32_i8
	.align 4
	.type	Plus__uPlus_var_i32_u32, #function
	.proc	04
Plus__uPlus_var_i32_u32:
.LLFB188:
	save	%sp, -96, %sp
.LLCFI188:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE188:
	.size	Plus__uPlus_var_i32_u32, .-Plus__uPlus_var_i32_u32
	.align 4
	.type	Plus__Plus_var_i32_u32, #function
	.proc	04
Plus__Plus_var_i32_u32:
.LLFB189:
	save	%sp, -96, %sp
.LLCFI189:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE189:
	.size	Plus__Plus_var_i32_u32, .-Plus__Plus_var_i32_u32
	.align 4
	.type	Plus__uPlus_param_i32_u32, #function
	.proc	04
Plus__uPlus_param_i32_u32:
.LLFB190:
	save	%sp, -96, %sp
.LLCFI190:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE190:
	.size	Plus__uPlus_param_i32_u32, .-Plus__uPlus_param_i32_u32
	.align 4
	.type	Plus__Plus_param_i32_u32, #function
	.proc	04
Plus__Plus_param_i32_u32:
.LLFB191:
	save	%sp, -96, %sp
.LLCFI191:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE191:
	.size	Plus__Plus_param_i32_u32, .-Plus__Plus_param_i32_u32
	.align 4
	.type	Plus__uPlus_var_i16_u16, #function
	.proc	04
Plus__uPlus_var_i16_u16:
.LLFB192:
	save	%sp, -96, %sp
.LLCFI192:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE192:
	.size	Plus__uPlus_var_i16_u16, .-Plus__uPlus_var_i16_u16
	.align 4
	.type	Plus__Plus_var_i16_u16, #function
	.proc	03
Plus__Plus_var_i16_u16:
.LLFB193:
	save	%sp, -96, %sp
.LLCFI193:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	add	%o5, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL223
	 nop
	sethi	%hi(7168), %g1
	or	%g1, 737, %o0
	call	_m3_fault, 0
	 nop
.LL223:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE193:
	.size	Plus__Plus_var_i16_u16, .-Plus__Plus_var_i16_u16
	.align 4
	.type	Plus__uPlus_param_i16_u16, #function
	.proc	04
Plus__uPlus_param_i16_u16:
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
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE194:
	.size	Plus__uPlus_param_i16_u16, .-Plus__uPlus_param_i16_u16
	.align 4
	.type	Plus__Plus_param_i16_u16, #function
	.proc	03
Plus__Plus_param_i16_u16:
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
	add	%o5, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL226
	 nop
	sethi	%hi(7168), %g1
	or	%g1, 801, %o0
	call	_m3_fault, 0
	 nop
.LL226:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE195:
	.size	Plus__Plus_param_i16_u16, .-Plus__Plus_param_i16_u16
	.align 4
	.type	Plus__uPlus_var_i16_u64, #function
	.proc	05
Plus__uPlus_var_i16_u64:
.LLFB196:
	save	%sp, -96, %sp
.LLCFI196:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE196:
	.size	Plus__uPlus_var_i16_u64, .-Plus__uPlus_var_i16_u64
	.align 4
	.type	Plus__Plus_var_i16_u64, #function
	.proc	05
Plus__Plus_var_i16_u64:
.LLFB197:
	save	%sp, -96, %sp
.LLCFI197:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE197:
	.size	Plus__Plus_var_i16_u64, .-Plus__Plus_var_i16_u64
	.align 4
	.type	Plus__uPlus_param_i16_u64, #function
	.proc	05
Plus__uPlus_param_i16_u64:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE198:
	.size	Plus__uPlus_param_i16_u64, .-Plus__uPlus_param_i16_u64
	.align 4
	.type	Plus__Plus_param_i16_u64, #function
	.proc	05
Plus__Plus_param_i16_u64:
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
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE199:
	.size	Plus__Plus_param_i16_u64, .-Plus__Plus_param_i16_u64
	.align 4
	.type	Plus__uPlus_var_i16_LC, #function
	.proc	05
Plus__uPlus_var_i16_LC:
.LLFB200:
	save	%sp, -96, %sp
.LLCFI200:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE200:
	.size	Plus__uPlus_var_i16_LC, .-Plus__uPlus_var_i16_LC
	.align 4
	.type	Plus__Plus_var_i16_LC, #function
	.proc	05
Plus__Plus_var_i16_LC:
.LLFB201:
	save	%sp, -96, %sp
.LLCFI201:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE201:
	.size	Plus__Plus_var_i16_LC, .-Plus__Plus_var_i16_LC
	.align 4
	.type	Plus__uPlus_param_i16_LC, #function
	.proc	05
Plus__uPlus_param_i16_LC:
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
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE202:
	.size	Plus__uPlus_param_i16_LC, .-Plus__uPlus_param_i16_LC
	.align 4
	.type	Plus__Plus_param_i16_LC, #function
	.proc	05
Plus__Plus_param_i16_LC:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE203:
	.size	Plus__Plus_param_i16_LC, .-Plus__Plus_param_i16_LC
	.align 4
	.type	Plus__uPlus_var_i16_i32, #function
	.proc	04
Plus__uPlus_var_i16_i32:
.LLFB204:
	save	%sp, -96, %sp
.LLCFI204:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE204:
	.size	Plus__uPlus_var_i16_i32, .-Plus__uPlus_var_i16_i32
	.align 4
	.type	Plus__Plus_var_i16_i32, #function
	.proc	03
Plus__Plus_var_i16_i32:
.LLFB205:
	save	%sp, -96, %sp
.LLCFI205:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	add	%o5, %g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL237
	 nop
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	add	%o5, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL238
	 nop
.LL237:
	sethi	%hi(8192), %g1
	or	%g1, 97, %o0
	call	_m3_fault, 0
	 nop
.LL238:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE205:
	.size	Plus__Plus_var_i16_i32, .-Plus__Plus_var_i16_i32
	.align 4
	.type	Plus__uPlus_param_i16_i32, #function
	.proc	04
Plus__uPlus_param_i16_i32:
.LLFB206:
	save	%sp, -96, %sp
.LLCFI206:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE206:
	.size	Plus__uPlus_param_i16_i32, .-Plus__uPlus_param_i16_i32
	.align 4
	.type	Plus__Plus_param_i16_i32, #function
	.proc	03
Plus__Plus_param_i16_i32:
.LLFB207:
	save	%sp, -96, %sp
.LLCFI207:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL241
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL242
	 nop
.LL241:
	sethi	%hi(8192), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL242:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE207:
	.size	Plus__Plus_param_i16_i32, .-Plus__Plus_param_i16_i32
	.align 4
	.type	Plus__uPlus_var_i16_i16, #function
	.proc	04
Plus__uPlus_var_i16_i16:
.LLFB208:
	save	%sp, -96, %sp
.LLCFI208:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE208:
	.size	Plus__uPlus_var_i16_i16, .-Plus__uPlus_var_i16_i16
	.align 4
	.type	Plus__Plus_var_i16_i16, #function
	.proc	03
Plus__Plus_var_i16_i16:
.LLFB209:
	save	%sp, -96, %sp
.LLCFI209:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	add	%o5, %g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL245
	 nop
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	add	%o5, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL246
	 nop
.LL245:
	sethi	%hi(8192), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL246:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE209:
	.size	Plus__Plus_var_i16_i16, .-Plus__Plus_var_i16_i16
	.align 4
	.type	Plus__uPlus_param_i16_i16, #function
	.proc	04
Plus__uPlus_param_i16_i16:
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
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE210:
	.size	Plus__uPlus_param_i16_i16, .-Plus__uPlus_param_i16_i16
	.align 4
	.type	Plus__Plus_param_i16_i16, #function
	.proc	03
Plus__Plus_param_i16_i16:
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
	add	%o5, %g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL249
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	add	%o5, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL250
	 nop
.LL249:
	sethi	%hi(8192), %g1
	or	%g1, 289, %o0
	call	_m3_fault, 0
	 nop
.LL250:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE211:
	.size	Plus__Plus_param_i16_i16, .-Plus__Plus_param_i16_i16
	.align 4
	.type	Plus__uPlus_var_i16_I, #function
	.proc	04
Plus__uPlus_var_i16_I:
.LLFB212:
	save	%sp, -96, %sp
.LLCFI212:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE212:
	.size	Plus__uPlus_var_i16_I, .-Plus__uPlus_var_i16_I
	.align 4
	.type	Plus__Plus_var_i16_I, #function
	.proc	03
Plus__Plus_var_i16_I:
.LLFB213:
	save	%sp, -96, %sp
.LLCFI213:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	add	%o5, %g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL253
	 nop
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	add	%o5, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL254
	 nop
.LL253:
	sethi	%hi(8192), %g1
	or	%g1, 353, %o0
	call	_m3_fault, 0
	 nop
.LL254:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE213:
	.size	Plus__Plus_var_i16_I, .-Plus__Plus_var_i16_I
	.align 4
	.type	Plus__uPlus_param_i16_I, #function
	.proc	04
Plus__uPlus_param_i16_I:
.LLFB214:
	save	%sp, -96, %sp
.LLCFI214:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE214:
	.size	Plus__uPlus_param_i16_I, .-Plus__uPlus_param_i16_I
	.align 4
	.type	Plus__Plus_param_i16_I, #function
	.proc	03
Plus__Plus_param_i16_I:
.LLFB215:
	save	%sp, -96, %sp
.LLCFI215:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL257
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL258
	 nop
.LL257:
	sethi	%hi(8192), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL258:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE215:
	.size	Plus__Plus_param_i16_I, .-Plus__Plus_param_i16_I
	.align 4
	.type	Plus__uPlus_var_i16_i64, #function
	.proc	05
Plus__uPlus_var_i16_i64:
.LLFB216:
	save	%sp, -96, %sp
.LLCFI216:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE216:
	.size	Plus__uPlus_var_i16_i64, .-Plus__uPlus_var_i16_i64
	.align 4
	.type	Plus__Plus_var_i16_i64, #function
	.proc	05
Plus__Plus_var_i16_i64:
.LLFB217:
	save	%sp, -96, %sp
.LLCFI217:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE217:
	.size	Plus__Plus_var_i16_i64, .-Plus__Plus_var_i16_i64
	.align 4
	.type	Plus__uPlus_param_i16_i64, #function
	.proc	05
Plus__uPlus_param_i16_i64:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE218:
	.size	Plus__uPlus_param_i16_i64, .-Plus__uPlus_param_i16_i64
	.align 4
	.type	Plus__Plus_param_i16_i64, #function
	.proc	05
Plus__Plus_param_i16_i64:
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
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE219:
	.size	Plus__Plus_param_i16_i64, .-Plus__Plus_param_i16_i64
	.align 4
	.type	Plus__uPlus_var_i16_C, #function
	.proc	04
Plus__uPlus_var_i16_C:
.LLFB220:
	save	%sp, -96, %sp
.LLCFI220:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE220:
	.size	Plus__uPlus_var_i16_C, .-Plus__uPlus_var_i16_C
	.align 4
	.type	Plus__Plus_var_i16_C, #function
	.proc	03
Plus__Plus_var_i16_C:
.LLFB221:
	save	%sp, -96, %sp
.LLCFI221:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	add	%o5, %g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL265
	 nop
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	add	%o5, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL266
	 nop
.LL265:
	sethi	%hi(8192), %g1
	or	%g1, 609, %o0
	call	_m3_fault, 0
	 nop
.LL266:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE221:
	.size	Plus__Plus_var_i16_C, .-Plus__Plus_var_i16_C
	.align 4
	.type	Plus__uPlus_param_i16_C, #function
	.proc	04
Plus__uPlus_param_i16_C:
.LLFB222:
	save	%sp, -96, %sp
.LLCFI222:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE222:
	.size	Plus__uPlus_param_i16_C, .-Plus__uPlus_param_i16_C
	.align 4
	.type	Plus__Plus_param_i16_C, #function
	.proc	03
Plus__Plus_param_i16_C:
.LLFB223:
	save	%sp, -96, %sp
.LLCFI223:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL269
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL270
	 nop
.LL269:
	sethi	%hi(8192), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL270:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE223:
	.size	Plus__Plus_param_i16_C, .-Plus__Plus_param_i16_C
	.align 4
	.type	Plus__uPlus_var_i16_u8, #function
	.proc	04
Plus__uPlus_var_i16_u8:
.LLFB224:
	save	%sp, -96, %sp
.LLCFI224:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE224:
	.size	Plus__uPlus_var_i16_u8, .-Plus__uPlus_var_i16_u8
	.align 4
	.type	Plus__Plus_var_i16_u8, #function
	.proc	03
Plus__Plus_var_i16_u8:
.LLFB225:
	save	%sp, -96, %sp
.LLCFI225:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	add	%o5, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL273
	 nop
	sethi	%hi(8192), %g1
	or	%g1, 737, %o0
	call	_m3_fault, 0
	 nop
.LL273:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE225:
	.size	Plus__Plus_var_i16_u8, .-Plus__Plus_var_i16_u8
	.align 4
	.type	Plus__uPlus_param_i16_u8, #function
	.proc	04
Plus__uPlus_param_i16_u8:
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
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE226:
	.size	Plus__uPlus_param_i16_u8, .-Plus__uPlus_param_i16_u8
	.align 4
	.type	Plus__Plus_param_i16_u8, #function
	.proc	03
Plus__Plus_param_i16_u8:
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
	add	%o5, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL276
	 nop
	sethi	%hi(8192), %g1
	or	%g1, 801, %o0
	call	_m3_fault, 0
	 nop
.LL276:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE227:
	.size	Plus__Plus_param_i16_u8, .-Plus__Plus_param_i16_u8
	.align 4
	.type	Plus__uPlus_var_i16_L, #function
	.proc	05
Plus__uPlus_var_i16_L:
.LLFB228:
	save	%sp, -96, %sp
.LLCFI228:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE228:
	.size	Plus__uPlus_var_i16_L, .-Plus__uPlus_var_i16_L
	.align 4
	.type	Plus__Plus_var_i16_L, #function
	.proc	05
Plus__Plus_var_i16_L:
.LLFB229:
	save	%sp, -96, %sp
.LLCFI229:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE229:
	.size	Plus__Plus_var_i16_L, .-Plus__Plus_var_i16_L
	.align 4
	.type	Plus__uPlus_param_i16_L, #function
	.proc	05
Plus__uPlus_param_i16_L:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE230:
	.size	Plus__uPlus_param_i16_L, .-Plus__uPlus_param_i16_L
	.align 4
	.type	Plus__Plus_param_i16_L, #function
	.proc	05
Plus__Plus_param_i16_L:
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
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE231:
	.size	Plus__Plus_param_i16_L, .-Plus__Plus_param_i16_L
	.align 4
	.type	Plus__uPlus_var_i16_i8, #function
	.proc	04
Plus__uPlus_var_i16_i8:
.LLFB232:
	save	%sp, -96, %sp
.LLCFI232:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE232:
	.size	Plus__uPlus_var_i16_i8, .-Plus__uPlus_var_i16_i8
	.align 4
	.type	Plus__Plus_var_i16_i8, #function
	.proc	03
Plus__Plus_var_i16_i8:
.LLFB233:
	save	%sp, -96, %sp
.LLCFI233:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	add	%o5, %g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL283
	 nop
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	add	%o5, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL284
	 nop
.LL283:
	sethi	%hi(8192), %g1
	or	%g1, 993, %o0
	call	_m3_fault, 0
	 nop
.LL284:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE233:
	.size	Plus__Plus_var_i16_i8, .-Plus__Plus_var_i16_i8
	.align 4
	.type	Plus__uPlus_param_i16_i8, #function
	.proc	04
Plus__uPlus_param_i16_i8:
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
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE234:
	.size	Plus__uPlus_param_i16_i8, .-Plus__uPlus_param_i16_i8
	.align 4
	.type	Plus__Plus_param_i16_i8, #function
	.proc	03
Plus__Plus_param_i16_i8:
.LLFB235:
	save	%sp, -96, %sp
.LLCFI235:
	sth	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	add	%o5, %g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL287
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	add	%o5, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL288
	 nop
.LL287:
	sethi	%hi(9216), %g1
	or	%g1, 33, %o0
	call	_m3_fault, 0
	 nop
.LL288:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE235:
	.size	Plus__Plus_param_i16_i8, .-Plus__Plus_param_i16_i8
	.align 4
	.type	Plus__uPlus_var_i16_u32, #function
	.proc	04
Plus__uPlus_var_i16_u32:
.LLFB236:
	save	%sp, -96, %sp
.LLCFI236:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE236:
	.size	Plus__uPlus_var_i16_u32, .-Plus__uPlus_var_i16_u32
	.align 4
	.type	Plus__Plus_var_i16_u32, #function
	.proc	03
Plus__Plus_var_i16_u32:
.LLFB237:
	save	%sp, -96, %sp
.LLCFI237:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	add	%o5, %g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL291
	 nop
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	add	%o5, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL292
	 nop
.LL291:
	sethi	%hi(9216), %g1
	or	%g1, 97, %o0
	call	_m3_fault, 0
	 nop
.LL292:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE237:
	.size	Plus__Plus_var_i16_u32, .-Plus__Plus_var_i16_u32
	.align 4
	.type	Plus__uPlus_param_i16_u32, #function
	.proc	04
Plus__uPlus_param_i16_u32:
.LLFB238:
	save	%sp, -96, %sp
.LLCFI238:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE238:
	.size	Plus__uPlus_param_i16_u32, .-Plus__uPlus_param_i16_u32
	.align 4
	.type	Plus__Plus_param_i16_u32, #function
	.proc	03
Plus__Plus_param_i16_u32:
.LLFB239:
	save	%sp, -96, %sp
.LLCFI239:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL295
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL296
	 nop
.LL295:
	sethi	%hi(9216), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL296:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE239:
	.size	Plus__Plus_param_i16_u32, .-Plus__Plus_param_i16_u32
	.align 4
	.type	Plus__uPlus_var_I_u16, #function
	.proc	04
Plus__uPlus_var_I_u16:
.LLFB240:
	save	%sp, -96, %sp
.LLCFI240:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE240:
	.size	Plus__uPlus_var_I_u16, .-Plus__uPlus_var_I_u16
	.align 4
	.type	Plus__Plus_var_I_u16, #function
	.proc	04
Plus__Plus_var_I_u16:
.LLFB241:
	save	%sp, -96, %sp
.LLCFI241:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE241:
	.size	Plus__Plus_var_I_u16, .-Plus__Plus_var_I_u16
	.align 4
	.type	Plus__uPlus_param_I_u16, #function
	.proc	04
Plus__uPlus_param_I_u16:
.LLFB242:
	save	%sp, -96, %sp
.LLCFI242:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+68], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE242:
	.size	Plus__uPlus_param_I_u16, .-Plus__uPlus_param_I_u16
	.align 4
	.type	Plus__Plus_param_I_u16, #function
	.proc	04
Plus__Plus_param_I_u16:
.LLFB243:
	save	%sp, -96, %sp
.LLCFI243:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+68], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE243:
	.size	Plus__Plus_param_I_u16, .-Plus__Plus_param_I_u16
	.align 4
	.type	Plus__uPlus_var_I_u64, #function
	.proc	05
Plus__uPlus_var_I_u64:
.LLFB244:
	save	%sp, -96, %sp
.LLCFI244:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE244:
	.size	Plus__uPlus_var_I_u64, .-Plus__uPlus_var_I_u64
	.align 4
	.type	Plus__Plus_var_I_u64, #function
	.proc	05
Plus__Plus_var_I_u64:
.LLFB245:
	save	%sp, -96, %sp
.LLCFI245:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE245:
	.size	Plus__Plus_var_I_u64, .-Plus__Plus_var_I_u64
	.align 4
	.type	Plus__uPlus_param_I_u64, #function
	.proc	05
Plus__uPlus_param_I_u64:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE246:
	.size	Plus__uPlus_param_I_u64, .-Plus__uPlus_param_I_u64
	.align 4
	.type	Plus__Plus_param_I_u64, #function
	.proc	05
Plus__Plus_param_I_u64:
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
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE247:
	.size	Plus__Plus_param_I_u64, .-Plus__Plus_param_I_u64
	.align 4
	.type	Plus__uPlus_var_I_LC, #function
	.proc	05
Plus__uPlus_var_I_LC:
.LLFB248:
	save	%sp, -96, %sp
.LLCFI248:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE248:
	.size	Plus__uPlus_var_I_LC, .-Plus__uPlus_var_I_LC
	.align 4
	.type	Plus__Plus_var_I_LC, #function
	.proc	05
Plus__Plus_var_I_LC:
.LLFB249:
	save	%sp, -96, %sp
.LLCFI249:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE249:
	.size	Plus__Plus_var_I_LC, .-Plus__Plus_var_I_LC
	.align 4
	.type	Plus__uPlus_param_I_LC, #function
	.proc	05
Plus__uPlus_param_I_LC:
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
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE250:
	.size	Plus__uPlus_param_I_LC, .-Plus__uPlus_param_I_LC
	.align 4
	.type	Plus__Plus_param_I_LC, #function
	.proc	05
Plus__Plus_param_I_LC:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE251:
	.size	Plus__Plus_param_I_LC, .-Plus__Plus_param_I_LC
	.align 4
	.type	Plus__uPlus_var_I_i32, #function
	.proc	04
Plus__uPlus_var_I_i32:
.LLFB252:
	save	%sp, -96, %sp
.LLCFI252:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE252:
	.size	Plus__uPlus_var_I_i32, .-Plus__uPlus_var_I_i32
	.align 4
	.type	Plus__Plus_var_I_i32, #function
	.proc	04
Plus__Plus_var_I_i32:
.LLFB253:
	save	%sp, -96, %sp
.LLCFI253:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE253:
	.size	Plus__Plus_var_I_i32, .-Plus__Plus_var_I_i32
	.align 4
	.type	Plus__uPlus_param_I_i32, #function
	.proc	04
Plus__uPlus_param_I_i32:
.LLFB254:
	save	%sp, -96, %sp
.LLCFI254:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE254:
	.size	Plus__uPlus_param_I_i32, .-Plus__uPlus_param_I_i32
	.align 4
	.type	Plus__Plus_param_I_i32, #function
	.proc	04
Plus__Plus_param_I_i32:
.LLFB255:
	save	%sp, -96, %sp
.LLCFI255:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE255:
	.size	Plus__Plus_param_I_i32, .-Plus__Plus_param_I_i32
	.align 4
	.type	Plus__uPlus_var_I_i16, #function
	.proc	04
Plus__uPlus_var_I_i16:
.LLFB256:
	save	%sp, -96, %sp
.LLCFI256:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE256:
	.size	Plus__uPlus_var_I_i16, .-Plus__uPlus_var_I_i16
	.align 4
	.type	Plus__Plus_var_I_i16, #function
	.proc	04
Plus__Plus_var_I_i16:
.LLFB257:
	save	%sp, -96, %sp
.LLCFI257:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE257:
	.size	Plus__Plus_var_I_i16, .-Plus__Plus_var_I_i16
	.align 4
	.type	Plus__uPlus_param_I_i16, #function
	.proc	04
Plus__uPlus_param_I_i16:
.LLFB258:
	save	%sp, -96, %sp
.LLCFI258:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+68], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE258:
	.size	Plus__uPlus_param_I_i16, .-Plus__uPlus_param_I_i16
	.align 4
	.type	Plus__Plus_param_I_i16, #function
	.proc	04
Plus__Plus_param_I_i16:
.LLFB259:
	save	%sp, -96, %sp
.LLCFI259:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+68], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE259:
	.size	Plus__Plus_param_I_i16, .-Plus__Plus_param_I_i16
	.align 4
	.type	Plus__uPlus_var_I_I, #function
	.proc	04
Plus__uPlus_var_I_I:
.LLFB260:
	save	%sp, -96, %sp
.LLCFI260:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE260:
	.size	Plus__uPlus_var_I_I, .-Plus__uPlus_var_I_I
	.align 4
	.type	Plus__Plus_var_I_I, #function
	.proc	04
Plus__Plus_var_I_I:
.LLFB261:
	save	%sp, -96, %sp
.LLCFI261:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE261:
	.size	Plus__Plus_var_I_I, .-Plus__Plus_var_I_I
	.align 4
	.type	Plus__uPlus_param_I_I, #function
	.proc	04
Plus__uPlus_param_I_I:
.LLFB262:
	save	%sp, -96, %sp
.LLCFI262:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE262:
	.size	Plus__uPlus_param_I_I, .-Plus__uPlus_param_I_I
	.align 4
	.type	Plus__Plus_param_I_I, #function
	.proc	04
Plus__Plus_param_I_I:
.LLFB263:
	save	%sp, -96, %sp
.LLCFI263:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE263:
	.size	Plus__Plus_param_I_I, .-Plus__Plus_param_I_I
	.align 4
	.type	Plus__uPlus_var_I_i64, #function
	.proc	05
Plus__uPlus_var_I_i64:
.LLFB264:
	save	%sp, -96, %sp
.LLCFI264:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE264:
	.size	Plus__uPlus_var_I_i64, .-Plus__uPlus_var_I_i64
	.align 4
	.type	Plus__Plus_var_I_i64, #function
	.proc	05
Plus__Plus_var_I_i64:
.LLFB265:
	save	%sp, -96, %sp
.LLCFI265:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE265:
	.size	Plus__Plus_var_I_i64, .-Plus__Plus_var_I_i64
	.align 4
	.type	Plus__uPlus_param_I_i64, #function
	.proc	05
Plus__uPlus_param_I_i64:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE266:
	.size	Plus__uPlus_param_I_i64, .-Plus__uPlus_param_I_i64
	.align 4
	.type	Plus__Plus_param_I_i64, #function
	.proc	05
Plus__Plus_param_I_i64:
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
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE267:
	.size	Plus__Plus_param_I_i64, .-Plus__Plus_param_I_i64
	.align 4
	.type	Plus__uPlus_var_I_C, #function
	.proc	04
Plus__uPlus_var_I_C:
.LLFB268:
	save	%sp, -96, %sp
.LLCFI268:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE268:
	.size	Plus__uPlus_var_I_C, .-Plus__uPlus_var_I_C
	.align 4
	.type	Plus__Plus_var_I_C, #function
	.proc	04
Plus__Plus_var_I_C:
.LLFB269:
	save	%sp, -96, %sp
.LLCFI269:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE269:
	.size	Plus__Plus_var_I_C, .-Plus__Plus_var_I_C
	.align 4
	.type	Plus__uPlus_param_I_C, #function
	.proc	04
Plus__uPlus_param_I_C:
.LLFB270:
	save	%sp, -96, %sp
.LLCFI270:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE270:
	.size	Plus__uPlus_param_I_C, .-Plus__uPlus_param_I_C
	.align 4
	.type	Plus__Plus_param_I_C, #function
	.proc	04
Plus__Plus_param_I_C:
.LLFB271:
	save	%sp, -96, %sp
.LLCFI271:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE271:
	.size	Plus__Plus_param_I_C, .-Plus__Plus_param_I_C
	.align 4
	.type	Plus__uPlus_var_I_u8, #function
	.proc	04
Plus__uPlus_var_I_u8:
.LLFB272:
	save	%sp, -96, %sp
.LLCFI272:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE272:
	.size	Plus__uPlus_var_I_u8, .-Plus__uPlus_var_I_u8
	.align 4
	.type	Plus__Plus_var_I_u8, #function
	.proc	04
Plus__Plus_var_I_u8:
.LLFB273:
	save	%sp, -96, %sp
.LLCFI273:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE273:
	.size	Plus__Plus_var_I_u8, .-Plus__Plus_var_I_u8
	.align 4
	.type	Plus__uPlus_param_I_u8, #function
	.proc	04
Plus__uPlus_param_I_u8:
.LLFB274:
	save	%sp, -96, %sp
.LLCFI274:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+68], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE274:
	.size	Plus__uPlus_param_I_u8, .-Plus__uPlus_param_I_u8
	.align 4
	.type	Plus__Plus_param_I_u8, #function
	.proc	04
Plus__Plus_param_I_u8:
.LLFB275:
	save	%sp, -96, %sp
.LLCFI275:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+68], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE275:
	.size	Plus__Plus_param_I_u8, .-Plus__Plus_param_I_u8
	.align 4
	.type	Plus__uPlus_var_I_L, #function
	.proc	05
Plus__uPlus_var_I_L:
.LLFB276:
	save	%sp, -96, %sp
.LLCFI276:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE276:
	.size	Plus__uPlus_var_I_L, .-Plus__uPlus_var_I_L
	.align 4
	.type	Plus__Plus_var_I_L, #function
	.proc	05
Plus__Plus_var_I_L:
.LLFB277:
	save	%sp, -96, %sp
.LLCFI277:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE277:
	.size	Plus__Plus_var_I_L, .-Plus__Plus_var_I_L
	.align 4
	.type	Plus__uPlus_param_I_L, #function
	.proc	05
Plus__uPlus_param_I_L:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE278:
	.size	Plus__uPlus_param_I_L, .-Plus__uPlus_param_I_L
	.align 4
	.type	Plus__Plus_param_I_L, #function
	.proc	05
Plus__Plus_param_I_L:
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
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE279:
	.size	Plus__Plus_param_I_L, .-Plus__Plus_param_I_L
	.align 4
	.type	Plus__uPlus_var_I_i8, #function
	.proc	04
Plus__uPlus_var_I_i8:
.LLFB280:
	save	%sp, -96, %sp
.LLCFI280:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE280:
	.size	Plus__uPlus_var_I_i8, .-Plus__uPlus_var_I_i8
	.align 4
	.type	Plus__Plus_var_I_i8, #function
	.proc	04
Plus__Plus_var_I_i8:
.LLFB281:
	save	%sp, -96, %sp
.LLCFI281:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE281:
	.size	Plus__Plus_var_I_i8, .-Plus__Plus_var_I_i8
	.align 4
	.type	Plus__uPlus_param_I_i8, #function
	.proc	04
Plus__uPlus_param_I_i8:
.LLFB282:
	save	%sp, -96, %sp
.LLCFI282:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+68], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE282:
	.size	Plus__uPlus_param_I_i8, .-Plus__uPlus_param_I_i8
	.align 4
	.type	Plus__Plus_param_I_i8, #function
	.proc	04
Plus__Plus_param_I_i8:
.LLFB283:
	save	%sp, -96, %sp
.LLCFI283:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+68], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE283:
	.size	Plus__Plus_param_I_i8, .-Plus__Plus_param_I_i8
	.align 4
	.type	Plus__uPlus_var_I_u32, #function
	.proc	04
Plus__uPlus_var_I_u32:
.LLFB284:
	save	%sp, -96, %sp
.LLCFI284:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE284:
	.size	Plus__uPlus_var_I_u32, .-Plus__uPlus_var_I_u32
	.align 4
	.type	Plus__Plus_var_I_u32, #function
	.proc	04
Plus__Plus_var_I_u32:
.LLFB285:
	save	%sp, -96, %sp
.LLCFI285:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE285:
	.size	Plus__Plus_var_I_u32, .-Plus__Plus_var_I_u32
	.align 4
	.type	Plus__uPlus_param_I_u32, #function
	.proc	04
Plus__uPlus_param_I_u32:
.LLFB286:
	save	%sp, -96, %sp
.LLCFI286:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE286:
	.size	Plus__uPlus_param_I_u32, .-Plus__uPlus_param_I_u32
	.align 4
	.type	Plus__Plus_param_I_u32, #function
	.proc	04
Plus__Plus_param_I_u32:
.LLFB287:
	save	%sp, -96, %sp
.LLCFI287:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE287:
	.size	Plus__Plus_param_I_u32, .-Plus__Plus_param_I_u32
	.align 4
	.type	Plus__uPlus_var_i64_u16, #function
	.proc	05
Plus__uPlus_var_i64_u16:
.LLFB288:
	save	%sp, -96, %sp
.LLCFI288:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE288:
	.size	Plus__uPlus_var_i64_u16, .-Plus__uPlus_var_i64_u16
	.align 4
	.type	Plus__Plus_var_i64_u16, #function
	.proc	05
Plus__Plus_var_i64_u16:
.LLFB289:
	save	%sp, -96, %sp
.LLCFI289:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE289:
	.size	Plus__Plus_var_i64_u16, .-Plus__Plus_var_i64_u16
	.align 4
	.type	Plus__uPlus_param_i64_u16, #function
	.proc	05
Plus__uPlus_param_i64_u16:
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
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE290:
	.size	Plus__uPlus_param_i64_u16, .-Plus__uPlus_param_i64_u16
	.align 4
	.type	Plus__Plus_param_i64_u16, #function
	.proc	05
Plus__Plus_param_i64_u16:
.LLFB291:
	save	%sp, -104, %sp
.LLCFI291:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE291:
	.size	Plus__Plus_param_i64_u16, .-Plus__Plus_param_i64_u16
	.align 4
	.type	Plus__uPlus_var_i64_u64, #function
	.proc	05
Plus__uPlus_var_i64_u64:
.LLFB292:
	save	%sp, -96, %sp
.LLCFI292:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE292:
	.size	Plus__uPlus_var_i64_u64, .-Plus__uPlus_var_i64_u64
	.align 4
	.type	Plus__Plus_var_i64_u64, #function
	.proc	05
Plus__Plus_var_i64_u64:
.LLFB293:
	save	%sp, -96, %sp
.LLCFI293:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o2
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE293:
	.size	Plus__Plus_var_i64_u64, .-Plus__Plus_var_i64_u64
	.align 4
	.type	Plus__uPlus_param_i64_u64, #function
	.proc	05
Plus__uPlus_param_i64_u64:
.LLFB294:
	save	%sp, -112, %sp
.LLCFI294:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE294:
	.size	Plus__uPlus_param_i64_u64, .-Plus__uPlus_param_i64_u64
	.align 4
	.type	Plus__Plus_param_i64_u64, #function
	.proc	05
Plus__Plus_param_i64_u64:
.LLFB295:
	save	%sp, -112, %sp
.LLCFI295:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE295:
	.size	Plus__Plus_param_i64_u64, .-Plus__Plus_param_i64_u64
	.align 4
	.type	Plus__uPlus_var_i64_LC, #function
	.proc	05
Plus__uPlus_var_i64_LC:
.LLFB296:
	save	%sp, -96, %sp
.LLCFI296:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE296:
	.size	Plus__uPlus_var_i64_LC, .-Plus__uPlus_var_i64_LC
	.align 4
	.type	Plus__Plus_var_i64_LC, #function
	.proc	05
Plus__Plus_var_i64_LC:
.LLFB297:
	save	%sp, -96, %sp
.LLCFI297:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o2
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE297:
	.size	Plus__Plus_var_i64_LC, .-Plus__Plus_var_i64_LC
	.align 4
	.type	Plus__uPlus_param_i64_LC, #function
	.proc	05
Plus__uPlus_param_i64_LC:
.LLFB298:
	save	%sp, -112, %sp
.LLCFI298:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE298:
	.size	Plus__uPlus_param_i64_LC, .-Plus__uPlus_param_i64_LC
	.align 4
	.type	Plus__Plus_param_i64_LC, #function
	.proc	05
Plus__Plus_param_i64_LC:
.LLFB299:
	save	%sp, -112, %sp
.LLCFI299:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o2
	ldd	[%fp-8], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE299:
	.size	Plus__Plus_param_i64_LC, .-Plus__Plus_param_i64_LC
	.align 4
	.type	Plus__uPlus_var_i64_i32, #function
	.proc	05
Plus__uPlus_var_i64_i32:
.LLFB300:
	save	%sp, -96, %sp
.LLCFI300:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE300:
	.size	Plus__uPlus_var_i64_i32, .-Plus__uPlus_var_i64_i32
	.align 4
	.type	Plus__Plus_var_i64_i32, #function
	.proc	05
Plus__Plus_var_i64_i32:
.LLFB301:
	save	%sp, -96, %sp
.LLCFI301:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE301:
	.size	Plus__Plus_var_i64_i32, .-Plus__Plus_var_i64_i32
	.align 4
	.type	Plus__uPlus_param_i64_i32, #function
	.proc	05
Plus__uPlus_param_i64_i32:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE302:
	.size	Plus__uPlus_param_i64_i32, .-Plus__uPlus_param_i64_i32
	.align 4
	.type	Plus__Plus_param_i64_i32, #function
	.proc	05
Plus__Plus_param_i64_i32:
.LLFB303:
	save	%sp, -104, %sp
.LLCFI303:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE303:
	.size	Plus__Plus_param_i64_i32, .-Plus__Plus_param_i64_i32
	.align 4
	.type	Plus__uPlus_var_i64_i16, #function
	.proc	05
Plus__uPlus_var_i64_i16:
.LLFB304:
	save	%sp, -96, %sp
.LLCFI304:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE304:
	.size	Plus__uPlus_var_i64_i16, .-Plus__uPlus_var_i64_i16
	.align 4
	.type	Plus__Plus_var_i64_i16, #function
	.proc	05
Plus__Plus_var_i64_i16:
.LLFB305:
	save	%sp, -96, %sp
.LLCFI305:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE305:
	.size	Plus__Plus_var_i64_i16, .-Plus__Plus_var_i64_i16
	.align 4
	.type	Plus__uPlus_param_i64_i16, #function
	.proc	05
Plus__uPlus_param_i64_i16:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE306:
	.size	Plus__uPlus_param_i64_i16, .-Plus__uPlus_param_i64_i16
	.align 4
	.type	Plus__Plus_param_i64_i16, #function
	.proc	05
Plus__Plus_param_i64_i16:
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
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE307:
	.size	Plus__Plus_param_i64_i16, .-Plus__Plus_param_i64_i16
	.align 4
	.type	Plus__uPlus_var_i64_I, #function
	.proc	05
Plus__uPlus_var_i64_I:
.LLFB308:
	save	%sp, -96, %sp
.LLCFI308:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE308:
	.size	Plus__uPlus_var_i64_I, .-Plus__uPlus_var_i64_I
	.align 4
	.type	Plus__Plus_var_i64_I, #function
	.proc	05
Plus__Plus_var_i64_I:
.LLFB309:
	save	%sp, -96, %sp
.LLCFI309:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE309:
	.size	Plus__Plus_var_i64_I, .-Plus__Plus_var_i64_I
	.align 4
	.type	Plus__uPlus_param_i64_I, #function
	.proc	05
Plus__uPlus_param_i64_I:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE310:
	.size	Plus__uPlus_param_i64_I, .-Plus__uPlus_param_i64_I
	.align 4
	.type	Plus__Plus_param_i64_I, #function
	.proc	05
Plus__Plus_param_i64_I:
.LLFB311:
	save	%sp, -104, %sp
.LLCFI311:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE311:
	.size	Plus__Plus_param_i64_I, .-Plus__Plus_param_i64_I
	.align 4
	.type	Plus__uPlus_var_i64_i64, #function
	.proc	05
Plus__uPlus_var_i64_i64:
.LLFB312:
	save	%sp, -96, %sp
.LLCFI312:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE312:
	.size	Plus__uPlus_var_i64_i64, .-Plus__uPlus_var_i64_i64
	.align 4
	.type	Plus__Plus_var_i64_i64, #function
	.proc	05
Plus__Plus_var_i64_i64:
.LLFB313:
	save	%sp, -96, %sp
.LLCFI313:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o2
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE313:
	.size	Plus__Plus_var_i64_i64, .-Plus__Plus_var_i64_i64
	.align 4
	.type	Plus__uPlus_param_i64_i64, #function
	.proc	05
Plus__uPlus_param_i64_i64:
.LLFB314:
	save	%sp, -112, %sp
.LLCFI314:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE314:
	.size	Plus__uPlus_param_i64_i64, .-Plus__uPlus_param_i64_i64
	.align 4
	.type	Plus__Plus_param_i64_i64, #function
	.proc	05
Plus__Plus_param_i64_i64:
.LLFB315:
	save	%sp, -112, %sp
.LLCFI315:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE315:
	.size	Plus__Plus_param_i64_i64, .-Plus__Plus_param_i64_i64
	.align 4
	.type	Plus__uPlus_var_i64_C, #function
	.proc	05
Plus__uPlus_var_i64_C:
.LLFB316:
	save	%sp, -96, %sp
.LLCFI316:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE316:
	.size	Plus__uPlus_var_i64_C, .-Plus__uPlus_var_i64_C
	.align 4
	.type	Plus__Plus_var_i64_C, #function
	.proc	05
Plus__Plus_var_i64_C:
.LLFB317:
	save	%sp, -96, %sp
.LLCFI317:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE317:
	.size	Plus__Plus_var_i64_C, .-Plus__Plus_var_i64_C
	.align 4
	.type	Plus__uPlus_param_i64_C, #function
	.proc	05
Plus__uPlus_param_i64_C:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE318:
	.size	Plus__uPlus_param_i64_C, .-Plus__uPlus_param_i64_C
	.align 4
	.type	Plus__Plus_param_i64_C, #function
	.proc	05
Plus__Plus_param_i64_C:
.LLFB319:
	save	%sp, -104, %sp
.LLCFI319:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE319:
	.size	Plus__Plus_param_i64_C, .-Plus__Plus_param_i64_C
	.align 4
	.type	Plus__uPlus_var_i64_u8, #function
	.proc	05
Plus__uPlus_var_i64_u8:
.LLFB320:
	save	%sp, -96, %sp
.LLCFI320:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE320:
	.size	Plus__uPlus_var_i64_u8, .-Plus__uPlus_var_i64_u8
	.align 4
	.type	Plus__Plus_var_i64_u8, #function
	.proc	05
Plus__Plus_var_i64_u8:
.LLFB321:
	save	%sp, -96, %sp
.LLCFI321:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE321:
	.size	Plus__Plus_var_i64_u8, .-Plus__Plus_var_i64_u8
	.align 4
	.type	Plus__uPlus_param_i64_u8, #function
	.proc	05
Plus__uPlus_param_i64_u8:
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
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE322:
	.size	Plus__uPlus_param_i64_u8, .-Plus__uPlus_param_i64_u8
	.align 4
	.type	Plus__Plus_param_i64_u8, #function
	.proc	05
Plus__Plus_param_i64_u8:
.LLFB323:
	save	%sp, -104, %sp
.LLCFI323:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE323:
	.size	Plus__Plus_param_i64_u8, .-Plus__Plus_param_i64_u8
	.align 4
	.type	Plus__uPlus_var_i64_L, #function
	.proc	05
Plus__uPlus_var_i64_L:
.LLFB324:
	save	%sp, -96, %sp
.LLCFI324:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE324:
	.size	Plus__uPlus_var_i64_L, .-Plus__uPlus_var_i64_L
	.align 4
	.type	Plus__Plus_var_i64_L, #function
	.proc	05
Plus__Plus_var_i64_L:
.LLFB325:
	save	%sp, -96, %sp
.LLCFI325:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o2
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE325:
	.size	Plus__Plus_var_i64_L, .-Plus__Plus_var_i64_L
	.align 4
	.type	Plus__uPlus_param_i64_L, #function
	.proc	05
Plus__uPlus_param_i64_L:
.LLFB326:
	save	%sp, -112, %sp
.LLCFI326:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE326:
	.size	Plus__uPlus_param_i64_L, .-Plus__uPlus_param_i64_L
	.align 4
	.type	Plus__Plus_param_i64_L, #function
	.proc	05
Plus__Plus_param_i64_L:
.LLFB327:
	save	%sp, -112, %sp
.LLCFI327:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE327:
	.size	Plus__Plus_param_i64_L, .-Plus__Plus_param_i64_L
	.align 4
	.type	Plus__uPlus_var_i64_i8, #function
	.proc	05
Plus__uPlus_var_i64_i8:
.LLFB328:
	save	%sp, -96, %sp
.LLCFI328:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE328:
	.size	Plus__uPlus_var_i64_i8, .-Plus__uPlus_var_i64_i8
	.align 4
	.type	Plus__Plus_var_i64_i8, #function
	.proc	05
Plus__Plus_var_i64_i8:
.LLFB329:
	save	%sp, -96, %sp
.LLCFI329:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE329:
	.size	Plus__Plus_var_i64_i8, .-Plus__Plus_var_i64_i8
	.align 4
	.type	Plus__uPlus_param_i64_i8, #function
	.proc	05
Plus__uPlus_param_i64_i8:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE330:
	.size	Plus__uPlus_param_i64_i8, .-Plus__uPlus_param_i64_i8
	.align 4
	.type	Plus__Plus_param_i64_i8, #function
	.proc	05
Plus__Plus_param_i64_i8:
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
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE331:
	.size	Plus__Plus_param_i64_i8, .-Plus__Plus_param_i64_i8
	.align 4
	.type	Plus__uPlus_var_i64_u32, #function
	.proc	05
Plus__uPlus_var_i64_u32:
.LLFB332:
	save	%sp, -96, %sp
.LLCFI332:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE332:
	.size	Plus__uPlus_var_i64_u32, .-Plus__uPlus_var_i64_u32
	.align 4
	.type	Plus__Plus_var_i64_u32, #function
	.proc	05
Plus__Plus_var_i64_u32:
.LLFB333:
	save	%sp, -96, %sp
.LLCFI333:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE333:
	.size	Plus__Plus_var_i64_u32, .-Plus__Plus_var_i64_u32
	.align 4
	.type	Plus__uPlus_param_i64_u32, #function
	.proc	05
Plus__uPlus_param_i64_u32:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE334:
	.size	Plus__uPlus_param_i64_u32, .-Plus__uPlus_param_i64_u32
	.align 4
	.type	Plus__Plus_param_i64_u32, #function
	.proc	05
Plus__Plus_param_i64_u32:
.LLFB335:
	save	%sp, -104, %sp
.LLCFI335:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE335:
	.size	Plus__Plus_param_i64_u32, .-Plus__Plus_param_i64_u32
	.align 4
	.type	Plus__uPlus_var_C_u16, #function
	.proc	04
Plus__uPlus_var_C_u16:
.LLFB336:
	save	%sp, -96, %sp
.LLCFI336:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE336:
	.size	Plus__uPlus_var_C_u16, .-Plus__uPlus_var_C_u16
	.align 4
	.type	Plus__Plus_var_C_u16, #function
	.proc	016
Plus__Plus_var_C_u16:
.LLFB337:
	save	%sp, -96, %sp
.LLCFI337:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	add	%o5, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL395
	 nop
	sethi	%hi(12288), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL395:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE337:
	.size	Plus__Plus_var_C_u16, .-Plus__Plus_var_C_u16
	.align 4
	.type	Plus__uPlus_param_C_u16, #function
	.proc	04
Plus__uPlus_param_C_u16:
.LLFB338:
	save	%sp, -96, %sp
.LLCFI338:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+68], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE338:
	.size	Plus__uPlus_param_C_u16, .-Plus__uPlus_param_C_u16
	.align 4
	.type	Plus__Plus_param_C_u16, #function
	.proc	016
Plus__Plus_param_C_u16:
.LLFB339:
	save	%sp, -96, %sp
.LLCFI339:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+68], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL398
	 nop
	sethi	%hi(12288), %g1
	or	%g1, 289, %o0
	call	_m3_fault, 0
	 nop
.LL398:
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+68], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE339:
	.size	Plus__Plus_param_C_u16, .-Plus__Plus_param_C_u16
	.align 4
	.type	Plus__uPlus_var_C_u64, #function
	.proc	05
Plus__uPlus_var_C_u64:
.LLFB340:
	save	%sp, -96, %sp
.LLCFI340:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE340:
	.size	Plus__uPlus_var_C_u64, .-Plus__uPlus_var_C_u64
	.align 4
	.type	Plus__Plus_var_C_u64, #function
	.proc	05
Plus__Plus_var_C_u64:
.LLFB341:
	save	%sp, -96, %sp
.LLCFI341:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE341:
	.size	Plus__Plus_var_C_u64, .-Plus__Plus_var_C_u64
	.align 4
	.type	Plus__uPlus_param_C_u64, #function
	.proc	05
Plus__uPlus_param_C_u64:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE342:
	.size	Plus__uPlus_param_C_u64, .-Plus__uPlus_param_C_u64
	.align 4
	.type	Plus__Plus_param_C_u64, #function
	.proc	05
Plus__Plus_param_C_u64:
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
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE343:
	.size	Plus__Plus_param_C_u64, .-Plus__Plus_param_C_u64
	.align 4
	.type	Plus__uPlus_var_C_LC, #function
	.proc	05
Plus__uPlus_var_C_LC:
.LLFB344:
	save	%sp, -96, %sp
.LLCFI344:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE344:
	.size	Plus__uPlus_var_C_LC, .-Plus__uPlus_var_C_LC
	.align 4
	.type	Plus__Plus_var_C_LC, #function
	.proc	05
Plus__Plus_var_C_LC:
.LLFB345:
	save	%sp, -96, %sp
.LLCFI345:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE345:
	.size	Plus__Plus_var_C_LC, .-Plus__Plus_var_C_LC
	.align 4
	.type	Plus__uPlus_param_C_LC, #function
	.proc	05
Plus__uPlus_param_C_LC:
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
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE346:
	.size	Plus__uPlus_param_C_LC, .-Plus__uPlus_param_C_LC
	.align 4
	.type	Plus__Plus_param_C_LC, #function
	.proc	05
Plus__Plus_param_C_LC:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE347:
	.size	Plus__Plus_param_C_LC, .-Plus__Plus_param_C_LC
	.align 4
	.type	Plus__uPlus_var_C_i32, #function
	.proc	04
Plus__uPlus_var_C_i32:
.LLFB348:
	save	%sp, -96, %sp
.LLCFI348:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE348:
	.size	Plus__uPlus_var_C_i32, .-Plus__uPlus_var_C_i32
	.align 4
	.type	Plus__Plus_var_C_i32, #function
	.proc	016
Plus__Plus_var_C_i32:
.LLFB349:
	save	%sp, -96, %sp
.LLCFI349:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL409
	 nop
	sethi	%hi(12288), %g1
	or	%g1, 609, %o0
	call	_m3_fault, 0
	 nop
.LL409:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE349:
	.size	Plus__Plus_var_C_i32, .-Plus__Plus_var_C_i32
	.align 4
	.type	Plus__uPlus_param_C_i32, #function
	.proc	04
Plus__uPlus_param_C_i32:
.LLFB350:
	save	%sp, -96, %sp
.LLCFI350:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE350:
	.size	Plus__uPlus_param_C_i32, .-Plus__uPlus_param_C_i32
	.align 4
	.type	Plus__Plus_param_C_i32, #function
	.proc	016
Plus__Plus_param_C_i32:
.LLFB351:
	save	%sp, -96, %sp
.LLCFI351:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL412
	 nop
	sethi	%hi(12288), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL412:
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE351:
	.size	Plus__Plus_param_C_i32, .-Plus__Plus_param_C_i32
	.align 4
	.type	Plus__uPlus_var_C_i16, #function
	.proc	04
Plus__uPlus_var_C_i16:
.LLFB352:
	save	%sp, -96, %sp
.LLCFI352:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE352:
	.size	Plus__uPlus_var_C_i16, .-Plus__uPlus_var_C_i16
	.align 4
	.type	Plus__Plus_var_C_i16, #function
	.proc	016
Plus__Plus_var_C_i16:
.LLFB353:
	save	%sp, -96, %sp
.LLCFI353:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	add	%o5, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL415
	 nop
	sethi	%hi(12288), %g1
	or	%g1, 737, %o0
	call	_m3_fault, 0
	 nop
.LL415:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE353:
	.size	Plus__Plus_var_C_i16, .-Plus__Plus_var_C_i16
	.align 4
	.type	Plus__uPlus_param_C_i16, #function
	.proc	04
Plus__uPlus_param_C_i16:
.LLFB354:
	save	%sp, -96, %sp
.LLCFI354:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+68], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE354:
	.size	Plus__uPlus_param_C_i16, .-Plus__uPlus_param_C_i16
	.align 4
	.type	Plus__Plus_param_C_i16, #function
	.proc	016
Plus__Plus_param_C_i16:
.LLFB355:
	save	%sp, -96, %sp
.LLCFI355:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+68], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL418
	 nop
	sethi	%hi(12288), %g1
	or	%g1, 801, %o0
	call	_m3_fault, 0
	 nop
.LL418:
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+68], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE355:
	.size	Plus__Plus_param_C_i16, .-Plus__Plus_param_C_i16
	.align 4
	.type	Plus__uPlus_var_C_I, #function
	.proc	04
Plus__uPlus_var_C_I:
.LLFB356:
	save	%sp, -96, %sp
.LLCFI356:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE356:
	.size	Plus__uPlus_var_C_I, .-Plus__uPlus_var_C_I
	.align 4
	.type	Plus__Plus_var_C_I, #function
	.proc	016
Plus__Plus_var_C_I:
.LLFB357:
	save	%sp, -96, %sp
.LLCFI357:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL421
	 nop
	sethi	%hi(12288), %g1
	or	%g1, 865, %o0
	call	_m3_fault, 0
	 nop
.LL421:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE357:
	.size	Plus__Plus_var_C_I, .-Plus__Plus_var_C_I
	.align 4
	.type	Plus__uPlus_param_C_I, #function
	.proc	04
Plus__uPlus_param_C_I:
.LLFB358:
	save	%sp, -96, %sp
.LLCFI358:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE358:
	.size	Plus__uPlus_param_C_I, .-Plus__uPlus_param_C_I
	.align 4
	.type	Plus__Plus_param_C_I, #function
	.proc	016
Plus__Plus_param_C_I:
.LLFB359:
	save	%sp, -96, %sp
.LLCFI359:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL424
	 nop
	sethi	%hi(12288), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL424:
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE359:
	.size	Plus__Plus_param_C_I, .-Plus__Plus_param_C_I
	.align 4
	.type	Plus__uPlus_var_C_i64, #function
	.proc	05
Plus__uPlus_var_C_i64:
.LLFB360:
	save	%sp, -96, %sp
.LLCFI360:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE360:
	.size	Plus__uPlus_var_C_i64, .-Plus__uPlus_var_C_i64
	.align 4
	.type	Plus__Plus_var_C_i64, #function
	.proc	05
Plus__Plus_var_C_i64:
.LLFB361:
	save	%sp, -96, %sp
.LLCFI361:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE361:
	.size	Plus__Plus_var_C_i64, .-Plus__Plus_var_C_i64
	.align 4
	.type	Plus__uPlus_param_C_i64, #function
	.proc	05
Plus__uPlus_param_C_i64:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE362:
	.size	Plus__uPlus_param_C_i64, .-Plus__uPlus_param_C_i64
	.align 4
	.type	Plus__Plus_param_C_i64, #function
	.proc	05
Plus__Plus_param_C_i64:
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
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE363:
	.size	Plus__Plus_param_C_i64, .-Plus__Plus_param_C_i64
	.align 4
	.type	Plus__uPlus_var_C_C, #function
	.proc	04
Plus__uPlus_var_C_C:
.LLFB364:
	save	%sp, -96, %sp
.LLCFI364:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE364:
	.size	Plus__uPlus_var_C_C, .-Plus__uPlus_var_C_C
	.align 4
	.type	Plus__Plus_var_C_C, #function
	.proc	016
Plus__Plus_var_C_C:
.LLFB365:
	save	%sp, -96, %sp
.LLCFI365:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL431
	 nop
	sethi	%hi(13312), %g1
	or	%g1, 97, %o0
	call	_m3_fault, 0
	 nop
.LL431:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE365:
	.size	Plus__Plus_var_C_C, .-Plus__Plus_var_C_C
	.align 4
	.type	Plus__uPlus_param_C_C, #function
	.proc	04
Plus__uPlus_param_C_C:
.LLFB366:
	save	%sp, -96, %sp
.LLCFI366:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE366:
	.size	Plus__uPlus_param_C_C, .-Plus__uPlus_param_C_C
	.align 4
	.type	Plus__Plus_param_C_C, #function
	.proc	016
Plus__Plus_param_C_C:
.LLFB367:
	save	%sp, -96, %sp
.LLCFI367:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL434
	 nop
	sethi	%hi(13312), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL434:
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE367:
	.size	Plus__Plus_param_C_C, .-Plus__Plus_param_C_C
	.align 4
	.type	Plus__uPlus_var_C_u8, #function
	.proc	04
Plus__uPlus_var_C_u8:
.LLFB368:
	save	%sp, -96, %sp
.LLCFI368:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE368:
	.size	Plus__uPlus_var_C_u8, .-Plus__uPlus_var_C_u8
	.align 4
	.type	Plus__Plus_var_C_u8, #function
	.proc	016
Plus__Plus_var_C_u8:
.LLFB369:
	save	%sp, -96, %sp
.LLCFI369:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	add	%o5, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL437
	 nop
	sethi	%hi(13312), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL437:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE369:
	.size	Plus__Plus_var_C_u8, .-Plus__Plus_var_C_u8
	.align 4
	.type	Plus__uPlus_param_C_u8, #function
	.proc	04
Plus__uPlus_param_C_u8:
.LLFB370:
	save	%sp, -96, %sp
.LLCFI370:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+68], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE370:
	.size	Plus__uPlus_param_C_u8, .-Plus__uPlus_param_C_u8
	.align 4
	.type	Plus__Plus_param_C_u8, #function
	.proc	016
Plus__Plus_param_C_u8:
.LLFB371:
	save	%sp, -96, %sp
.LLCFI371:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+68], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL440
	 nop
	sethi	%hi(13312), %g1
	or	%g1, 289, %o0
	call	_m3_fault, 0
	 nop
.LL440:
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+68], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE371:
	.size	Plus__Plus_param_C_u8, .-Plus__Plus_param_C_u8
	.align 4
	.type	Plus__uPlus_var_C_L, #function
	.proc	05
Plus__uPlus_var_C_L:
.LLFB372:
	save	%sp, -96, %sp
.LLCFI372:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE372:
	.size	Plus__uPlus_var_C_L, .-Plus__uPlus_var_C_L
	.align 4
	.type	Plus__Plus_var_C_L, #function
	.proc	05
Plus__Plus_var_C_L:
.LLFB373:
	save	%sp, -96, %sp
.LLCFI373:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE373:
	.size	Plus__Plus_var_C_L, .-Plus__Plus_var_C_L
	.align 4
	.type	Plus__uPlus_param_C_L, #function
	.proc	05
Plus__uPlus_param_C_L:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE374:
	.size	Plus__uPlus_param_C_L, .-Plus__uPlus_param_C_L
	.align 4
	.type	Plus__Plus_param_C_L, #function
	.proc	05
Plus__Plus_param_C_L:
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
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE375:
	.size	Plus__Plus_param_C_L, .-Plus__Plus_param_C_L
	.align 4
	.type	Plus__uPlus_var_C_i8, #function
	.proc	04
Plus__uPlus_var_C_i8:
.LLFB376:
	save	%sp, -96, %sp
.LLCFI376:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE376:
	.size	Plus__uPlus_var_C_i8, .-Plus__uPlus_var_C_i8
	.align 4
	.type	Plus__Plus_var_C_i8, #function
	.proc	016
Plus__Plus_var_C_i8:
.LLFB377:
	save	%sp, -96, %sp
.LLCFI377:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	add	%o5, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL447
	 nop
	sethi	%hi(13312), %g1
	or	%g1, 481, %o0
	call	_m3_fault, 0
	 nop
.LL447:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE377:
	.size	Plus__Plus_var_C_i8, .-Plus__Plus_var_C_i8
	.align 4
	.type	Plus__uPlus_param_C_i8, #function
	.proc	04
Plus__uPlus_param_C_i8:
.LLFB378:
	save	%sp, -96, %sp
.LLCFI378:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+68], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE378:
	.size	Plus__uPlus_param_C_i8, .-Plus__uPlus_param_C_i8
	.align 4
	.type	Plus__Plus_param_C_i8, #function
	.proc	016
Plus__Plus_param_C_i8:
.LLFB379:
	save	%sp, -96, %sp
.LLCFI379:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+68], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL450
	 nop
	sethi	%hi(13312), %g1
	or	%g1, 545, %o0
	call	_m3_fault, 0
	 nop
.LL450:
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+68], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE379:
	.size	Plus__Plus_param_C_i8, .-Plus__Plus_param_C_i8
	.align 4
	.type	Plus__uPlus_var_C_u32, #function
	.proc	04
Plus__uPlus_var_C_u32:
.LLFB380:
	save	%sp, -96, %sp
.LLCFI380:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE380:
	.size	Plus__uPlus_var_C_u32, .-Plus__uPlus_var_C_u32
	.align 4
	.type	Plus__Plus_var_C_u32, #function
	.proc	016
Plus__Plus_var_C_u32:
.LLFB381:
	save	%sp, -96, %sp
.LLCFI381:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL453
	 nop
	sethi	%hi(13312), %g1
	or	%g1, 609, %o0
	call	_m3_fault, 0
	 nop
.LL453:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE381:
	.size	Plus__Plus_var_C_u32, .-Plus__Plus_var_C_u32
	.align 4
	.type	Plus__uPlus_param_C_u32, #function
	.proc	04
Plus__uPlus_param_C_u32:
.LLFB382:
	save	%sp, -96, %sp
.LLCFI382:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE382:
	.size	Plus__uPlus_param_C_u32, .-Plus__uPlus_param_C_u32
	.align 4
	.type	Plus__Plus_param_C_u32, #function
	.proc	016
Plus__Plus_param_C_u32:
.LLFB383:
	save	%sp, -96, %sp
.LLCFI383:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL456
	 nop
	sethi	%hi(13312), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL456:
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE383:
	.size	Plus__Plus_param_C_u32, .-Plus__Plus_param_C_u32
	.align 4
	.type	Plus__Plus_var_f64_f64, #function
	.proc	07
Plus__Plus_var_f64_f64:
.LLFB384:
	save	%sp, -96, %sp
.LLCFI384:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	add	%g1, 104, %g1
	ldd	[%g1], %f8
	faddd	%f8, %f8, %f8
	fmovd	%f8, %f0
	return	%i7+8
	 nop
.LLFE384:
	.size	Plus__Plus_var_f64_f64, .-Plus__Plus_var_f64_f64
	.align 4
	.type	Plus__Plus_param_f64_f64, #function
	.proc	07
Plus__Plus_param_f64_f64:
.LLFB385:
	save	%sp, -112, %sp
.LLCFI385:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %f10
	ldd	[%fp-16], %f8
	faddd	%f10, %f8, %f8
	fmovd	%f8, %f0
	return	%i7+8
	 nop
.LLFE385:
	.size	Plus__Plus_param_f64_f64, .-Plus__Plus_param_f64_f64
	.align 4
	.type	Plus__uPlus_var_u8_u16, #function
	.proc	04
Plus__uPlus_var_u8_u16:
.LLFB386:
	save	%sp, -96, %sp
.LLCFI386:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE386:
	.size	Plus__uPlus_var_u8_u16, .-Plus__uPlus_var_u8_u16
	.align 4
	.type	Plus__Plus_var_u8_u16, #function
	.proc	014
Plus__Plus_var_u8_u16:
.LLFB387:
	save	%sp, -96, %sp
.LLCFI387:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	add	%o5, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL461
	 nop
	sethi	%hi(13312), %g1
	or	%g1, 801, %o0
	call	_m3_fault, 0
	 nop
.LL461:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE387:
	.size	Plus__Plus_var_u8_u16, .-Plus__Plus_var_u8_u16
	.align 4
	.type	Plus__uPlus_param_u8_u16, #function
	.proc	04
Plus__uPlus_param_u8_u16:
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
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE388:
	.size	Plus__uPlus_param_u8_u16, .-Plus__uPlus_param_u8_u16
	.align 4
	.type	Plus__Plus_param_u8_u16, #function
	.proc	014
Plus__Plus_param_u8_u16:
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
	add	%o5, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL464
	 nop
	sethi	%hi(13312), %g1
	or	%g1, 865, %o0
	call	_m3_fault, 0
	 nop
.LL464:
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE389:
	.size	Plus__Plus_param_u8_u16, .-Plus__Plus_param_u8_u16
	.align 4
	.type	Plus__uPlus_var_u8_u64, #function
	.proc	05
Plus__uPlus_var_u8_u64:
.LLFB390:
	save	%sp, -96, %sp
.LLCFI390:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE390:
	.size	Plus__uPlus_var_u8_u64, .-Plus__uPlus_var_u8_u64
	.align 4
	.type	Plus__Plus_var_u8_u64, #function
	.proc	05
Plus__Plus_var_u8_u64:
.LLFB391:
	save	%sp, -96, %sp
.LLCFI391:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE391:
	.size	Plus__Plus_var_u8_u64, .-Plus__Plus_var_u8_u64
	.align 4
	.type	Plus__uPlus_param_u8_u64, #function
	.proc	05
Plus__uPlus_param_u8_u64:
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
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE392:
	.size	Plus__uPlus_param_u8_u64, .-Plus__uPlus_param_u8_u64
	.align 4
	.type	Plus__Plus_param_u8_u64, #function
	.proc	05
Plus__Plus_param_u8_u64:
.LLFB393:
	save	%sp, -104, %sp
.LLCFI393:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE393:
	.size	Plus__Plus_param_u8_u64, .-Plus__Plus_param_u8_u64
	.align 4
	.type	Plus__uPlus_var_u8_LC, #function
	.proc	05
Plus__uPlus_var_u8_LC:
.LLFB394:
	save	%sp, -96, %sp
.LLCFI394:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE394:
	.size	Plus__uPlus_var_u8_LC, .-Plus__uPlus_var_u8_LC
	.align 4
	.type	Plus__Plus_var_u8_LC, #function
	.proc	05
Plus__Plus_var_u8_LC:
.LLFB395:
	save	%sp, -96, %sp
.LLCFI395:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE395:
	.size	Plus__Plus_var_u8_LC, .-Plus__Plus_var_u8_LC
	.align 4
	.type	Plus__uPlus_param_u8_LC, #function
	.proc	05
Plus__uPlus_param_u8_LC:
.LLFB396:
	save	%sp, -104, %sp
.LLCFI396:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE396:
	.size	Plus__uPlus_param_u8_LC, .-Plus__uPlus_param_u8_LC
	.align 4
	.type	Plus__Plus_param_u8_LC, #function
	.proc	05
Plus__Plus_param_u8_LC:
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
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE397:
	.size	Plus__Plus_param_u8_LC, .-Plus__Plus_param_u8_LC
	.align 4
	.type	Plus__uPlus_var_u8_i32, #function
	.proc	04
Plus__uPlus_var_u8_i32:
.LLFB398:
	save	%sp, -96, %sp
.LLCFI398:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE398:
	.size	Plus__uPlus_var_u8_i32, .-Plus__uPlus_var_u8_i32
	.align 4
	.type	Plus__Plus_var_u8_i32, #function
	.proc	014
Plus__Plus_var_u8_i32:
.LLFB399:
	save	%sp, -96, %sp
.LLCFI399:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL475
	 nop
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL476
	 nop
.LL475:
	sethi	%hi(14336), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL476:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE399:
	.size	Plus__Plus_var_u8_i32, .-Plus__Plus_var_u8_i32
	.align 4
	.type	Plus__uPlus_param_u8_i32, #function
	.proc	04
Plus__uPlus_param_u8_i32:
.LLFB400:
	save	%sp, -96, %sp
.LLCFI400:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE400:
	.size	Plus__uPlus_param_u8_i32, .-Plus__uPlus_param_u8_i32
	.align 4
	.type	Plus__Plus_param_u8_i32, #function
	.proc	014
Plus__Plus_param_u8_i32:
.LLFB401:
	save	%sp, -96, %sp
.LLCFI401:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL479
	 nop
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL480
	 nop
.LL479:
	sethi	%hi(14336), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL480:
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE401:
	.size	Plus__Plus_param_u8_i32, .-Plus__Plus_param_u8_i32
	.align 4
	.type	Plus__uPlus_var_u8_i16, #function
	.proc	04
Plus__uPlus_var_u8_i16:
.LLFB402:
	save	%sp, -96, %sp
.LLCFI402:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE402:
	.size	Plus__uPlus_var_u8_i16, .-Plus__uPlus_var_u8_i16
	.align 4
	.type	Plus__Plus_var_u8_i16, #function
	.proc	014
Plus__Plus_var_u8_i16:
.LLFB403:
	save	%sp, -96, %sp
.LLCFI403:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	add	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL483
	 nop
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	add	%o5, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL484
	 nop
.LL483:
	sethi	%hi(14336), %g1
	or	%g1, 289, %o0
	call	_m3_fault, 0
	 nop
.LL484:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE403:
	.size	Plus__Plus_var_u8_i16, .-Plus__Plus_var_u8_i16
	.align 4
	.type	Plus__uPlus_param_u8_i16, #function
	.proc	04
Plus__uPlus_param_u8_i16:
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
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE404:
	.size	Plus__uPlus_param_u8_i16, .-Plus__uPlus_param_u8_i16
	.align 4
	.type	Plus__Plus_param_u8_i16, #function
	.proc	014
Plus__Plus_param_u8_i16:
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
	add	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL487
	 nop
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	add	%o5, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL488
	 nop
.LL487:
	sethi	%hi(14336), %g1
	or	%g1, 353, %o0
	call	_m3_fault, 0
	 nop
.LL488:
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE405:
	.size	Plus__Plus_param_u8_i16, .-Plus__Plus_param_u8_i16
	.align 4
	.type	Plus__uPlus_var_u8_I, #function
	.proc	04
Plus__uPlus_var_u8_I:
.LLFB406:
	save	%sp, -96, %sp
.LLCFI406:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE406:
	.size	Plus__uPlus_var_u8_I, .-Plus__uPlus_var_u8_I
	.align 4
	.type	Plus__Plus_var_u8_I, #function
	.proc	014
Plus__Plus_var_u8_I:
.LLFB407:
	save	%sp, -96, %sp
.LLCFI407:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL491
	 nop
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL492
	 nop
.LL491:
	sethi	%hi(14336), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL492:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE407:
	.size	Plus__Plus_var_u8_I, .-Plus__Plus_var_u8_I
	.align 4
	.type	Plus__uPlus_param_u8_I, #function
	.proc	04
Plus__uPlus_param_u8_I:
.LLFB408:
	save	%sp, -96, %sp
.LLCFI408:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE408:
	.size	Plus__uPlus_param_u8_I, .-Plus__uPlus_param_u8_I
	.align 4
	.type	Plus__Plus_param_u8_I, #function
	.proc	014
Plus__Plus_param_u8_I:
.LLFB409:
	save	%sp, -96, %sp
.LLCFI409:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL495
	 nop
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL496
	 nop
.LL495:
	sethi	%hi(14336), %g1
	or	%g1, 481, %o0
	call	_m3_fault, 0
	 nop
.LL496:
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE409:
	.size	Plus__Plus_param_u8_I, .-Plus__Plus_param_u8_I
	.align 4
	.type	Plus__uPlus_var_u8_i64, #function
	.proc	05
Plus__uPlus_var_u8_i64:
.LLFB410:
	save	%sp, -96, %sp
.LLCFI410:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE410:
	.size	Plus__uPlus_var_u8_i64, .-Plus__uPlus_var_u8_i64
	.align 4
	.type	Plus__Plus_var_u8_i64, #function
	.proc	05
Plus__Plus_var_u8_i64:
.LLFB411:
	save	%sp, -96, %sp
.LLCFI411:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE411:
	.size	Plus__Plus_var_u8_i64, .-Plus__Plus_var_u8_i64
	.align 4
	.type	Plus__uPlus_param_u8_i64, #function
	.proc	05
Plus__uPlus_param_u8_i64:
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
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE412:
	.size	Plus__uPlus_param_u8_i64, .-Plus__uPlus_param_u8_i64
	.align 4
	.type	Plus__Plus_param_u8_i64, #function
	.proc	05
Plus__Plus_param_u8_i64:
.LLFB413:
	save	%sp, -104, %sp
.LLCFI413:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE413:
	.size	Plus__Plus_param_u8_i64, .-Plus__Plus_param_u8_i64
	.align 4
	.type	Plus__uPlus_var_u8_C, #function
	.proc	04
Plus__uPlus_var_u8_C:
.LLFB414:
	save	%sp, -96, %sp
.LLCFI414:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE414:
	.size	Plus__uPlus_var_u8_C, .-Plus__uPlus_var_u8_C
	.align 4
	.type	Plus__Plus_var_u8_C, #function
	.proc	014
Plus__Plus_var_u8_C:
.LLFB415:
	save	%sp, -96, %sp
.LLCFI415:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL503
	 nop
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL504
	 nop
.LL503:
	sethi	%hi(14336), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL504:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE415:
	.size	Plus__Plus_var_u8_C, .-Plus__Plus_var_u8_C
	.align 4
	.type	Plus__uPlus_param_u8_C, #function
	.proc	04
Plus__uPlus_param_u8_C:
.LLFB416:
	save	%sp, -96, %sp
.LLCFI416:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE416:
	.size	Plus__uPlus_param_u8_C, .-Plus__uPlus_param_u8_C
	.align 4
	.type	Plus__Plus_param_u8_C, #function
	.proc	014
Plus__Plus_param_u8_C:
.LLFB417:
	save	%sp, -96, %sp
.LLCFI417:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL507
	 nop
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL508
	 nop
.LL507:
	sethi	%hi(14336), %g1
	or	%g1, 737, %o0
	call	_m3_fault, 0
	 nop
.LL508:
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE417:
	.size	Plus__Plus_param_u8_C, .-Plus__Plus_param_u8_C
	.align 4
	.type	Plus__uPlus_var_u8_u8, #function
	.proc	04
Plus__uPlus_var_u8_u8:
.LLFB418:
	save	%sp, -96, %sp
.LLCFI418:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE418:
	.size	Plus__uPlus_var_u8_u8, .-Plus__uPlus_var_u8_u8
	.align 4
	.type	Plus__Plus_var_u8_u8, #function
	.proc	014
Plus__Plus_var_u8_u8:
.LLFB419:
	save	%sp, -96, %sp
.LLCFI419:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	add	%o5, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL511
	 nop
	sethi	%hi(14336), %g1
	or	%g1, 801, %o0
	call	_m3_fault, 0
	 nop
.LL511:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE419:
	.size	Plus__Plus_var_u8_u8, .-Plus__Plus_var_u8_u8
	.align 4
	.type	Plus__uPlus_param_u8_u8, #function
	.proc	04
Plus__uPlus_param_u8_u8:
.LLFB420:
	save	%sp, -96, %sp
.LLCFI420:
	stb	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE420:
	.size	Plus__uPlus_param_u8_u8, .-Plus__uPlus_param_u8_u8
	.align 4
	.type	Plus__Plus_param_u8_u8, #function
	.proc	014
Plus__Plus_param_u8_u8:
.LLFB421:
	save	%sp, -96, %sp
.LLCFI421:
	stb	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %g1
	add	%o5, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL514
	 nop
	sethi	%hi(14336), %g1
	or	%g1, 865, %o0
	call	_m3_fault, 0
	 nop
.LL514:
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE421:
	.size	Plus__Plus_param_u8_u8, .-Plus__Plus_param_u8_u8
	.align 4
	.type	Plus__uPlus_var_u8_L, #function
	.proc	05
Plus__uPlus_var_u8_L:
.LLFB422:
	save	%sp, -96, %sp
.LLCFI422:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE422:
	.size	Plus__uPlus_var_u8_L, .-Plus__uPlus_var_u8_L
	.align 4
	.type	Plus__Plus_var_u8_L, #function
	.proc	05
Plus__Plus_var_u8_L:
.LLFB423:
	save	%sp, -96, %sp
.LLCFI423:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE423:
	.size	Plus__Plus_var_u8_L, .-Plus__Plus_var_u8_L
	.align 4
	.type	Plus__uPlus_param_u8_L, #function
	.proc	05
Plus__uPlus_param_u8_L:
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
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE424:
	.size	Plus__uPlus_param_u8_L, .-Plus__uPlus_param_u8_L
	.align 4
	.type	Plus__Plus_param_u8_L, #function
	.proc	05
Plus__Plus_param_u8_L:
.LLFB425:
	save	%sp, -104, %sp
.LLCFI425:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE425:
	.size	Plus__Plus_param_u8_L, .-Plus__Plus_param_u8_L
	.align 4
	.type	Plus__uPlus_var_u8_i8, #function
	.proc	04
Plus__uPlus_var_u8_i8:
.LLFB426:
	save	%sp, -96, %sp
.LLCFI426:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE426:
	.size	Plus__uPlus_var_u8_i8, .-Plus__uPlus_var_u8_i8
	.align 4
	.type	Plus__Plus_var_u8_i8, #function
	.proc	014
Plus__Plus_var_u8_i8:
.LLFB427:
	save	%sp, -96, %sp
.LLCFI427:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	add	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL521
	 nop
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	add	%o5, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL522
	 nop
.LL521:
	sethi	%hi(15360), %g1
	or	%g1, 33, %o0
	call	_m3_fault, 0
	 nop
.LL522:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE427:
	.size	Plus__Plus_var_u8_i8, .-Plus__Plus_var_u8_i8
	.align 4
	.type	Plus__uPlus_param_u8_i8, #function
	.proc	04
Plus__uPlus_param_u8_i8:
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
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE428:
	.size	Plus__uPlus_param_u8_i8, .-Plus__uPlus_param_u8_i8
	.align 4
	.type	Plus__Plus_param_u8_i8, #function
	.proc	014
Plus__Plus_param_u8_i8:
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
	add	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL525
	 nop
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	add	%o5, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL526
	 nop
.LL525:
	sethi	%hi(15360), %g1
	or	%g1, 97, %o0
	call	_m3_fault, 0
	 nop
.LL526:
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE429:
	.size	Plus__Plus_param_u8_i8, .-Plus__Plus_param_u8_i8
	.align 4
	.type	Plus__uPlus_var_u8_u32, #function
	.proc	04
Plus__uPlus_var_u8_u32:
.LLFB430:
	save	%sp, -96, %sp
.LLCFI430:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE430:
	.size	Plus__uPlus_var_u8_u32, .-Plus__uPlus_var_u8_u32
	.align 4
	.type	Plus__Plus_var_u8_u32, #function
	.proc	014
Plus__Plus_var_u8_u32:
.LLFB431:
	save	%sp, -96, %sp
.LLCFI431:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL529
	 nop
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL530
	 nop
.LL529:
	sethi	%hi(15360), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL530:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE431:
	.size	Plus__Plus_var_u8_u32, .-Plus__Plus_var_u8_u32
	.align 4
	.type	Plus__uPlus_param_u8_u32, #function
	.proc	04
Plus__uPlus_param_u8_u32:
.LLFB432:
	save	%sp, -96, %sp
.LLCFI432:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE432:
	.size	Plus__uPlus_param_u8_u32, .-Plus__uPlus_param_u8_u32
	.align 4
	.type	Plus__Plus_param_u8_u32, #function
	.proc	014
Plus__Plus_param_u8_u32:
.LLFB433:
	save	%sp, -96, %sp
.LLCFI433:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL533
	 nop
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL534
	 nop
.LL533:
	sethi	%hi(15360), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL534:
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE433:
	.size	Plus__Plus_param_u8_u32, .-Plus__Plus_param_u8_u32
	.align 4
	.type	Plus__uPlus_var_L_u16, #function
	.proc	05
Plus__uPlus_var_L_u16:
.LLFB434:
	save	%sp, -96, %sp
.LLCFI434:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE434:
	.size	Plus__uPlus_var_L_u16, .-Plus__uPlus_var_L_u16
	.align 4
	.type	Plus__Plus_var_L_u16, #function
	.proc	05
Plus__Plus_var_L_u16:
.LLFB435:
	save	%sp, -96, %sp
.LLCFI435:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE435:
	.size	Plus__Plus_var_L_u16, .-Plus__Plus_var_L_u16
	.align 4
	.type	Plus__uPlus_param_L_u16, #function
	.proc	05
Plus__uPlus_param_L_u16:
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
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE436:
	.size	Plus__uPlus_param_L_u16, .-Plus__uPlus_param_L_u16
	.align 4
	.type	Plus__Plus_param_L_u16, #function
	.proc	05
Plus__Plus_param_L_u16:
.LLFB437:
	save	%sp, -104, %sp
.LLCFI437:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE437:
	.size	Plus__Plus_param_L_u16, .-Plus__Plus_param_L_u16
	.align 4
	.type	Plus__uPlus_var_L_u64, #function
	.proc	05
Plus__uPlus_var_L_u64:
.LLFB438:
	save	%sp, -96, %sp
.LLCFI438:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE438:
	.size	Plus__uPlus_var_L_u64, .-Plus__uPlus_var_L_u64
	.align 4
	.type	Plus__Plus_var_L_u64, #function
	.proc	05
Plus__Plus_var_L_u64:
.LLFB439:
	save	%sp, -96, %sp
.LLCFI439:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o2
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE439:
	.size	Plus__Plus_var_L_u64, .-Plus__Plus_var_L_u64
	.align 4
	.type	Plus__uPlus_param_L_u64, #function
	.proc	05
Plus__uPlus_param_L_u64:
.LLFB440:
	save	%sp, -112, %sp
.LLCFI440:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE440:
	.size	Plus__uPlus_param_L_u64, .-Plus__uPlus_param_L_u64
	.align 4
	.type	Plus__Plus_param_L_u64, #function
	.proc	05
Plus__Plus_param_L_u64:
.LLFB441:
	save	%sp, -112, %sp
.LLCFI441:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE441:
	.size	Plus__Plus_param_L_u64, .-Plus__Plus_param_L_u64
	.align 4
	.type	Plus__uPlus_var_L_LC, #function
	.proc	05
Plus__uPlus_var_L_LC:
.LLFB442:
	save	%sp, -96, %sp
.LLCFI442:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE442:
	.size	Plus__uPlus_var_L_LC, .-Plus__uPlus_var_L_LC
	.align 4
	.type	Plus__Plus_var_L_LC, #function
	.proc	05
Plus__Plus_var_L_LC:
.LLFB443:
	save	%sp, -96, %sp
.LLCFI443:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o2
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE443:
	.size	Plus__Plus_var_L_LC, .-Plus__Plus_var_L_LC
	.align 4
	.type	Plus__uPlus_param_L_LC, #function
	.proc	05
Plus__uPlus_param_L_LC:
.LLFB444:
	save	%sp, -112, %sp
.LLCFI444:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE444:
	.size	Plus__uPlus_param_L_LC, .-Plus__uPlus_param_L_LC
	.align 4
	.type	Plus__Plus_param_L_LC, #function
	.proc	05
Plus__Plus_param_L_LC:
.LLFB445:
	save	%sp, -112, %sp
.LLCFI445:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o2
	ldd	[%fp-8], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE445:
	.size	Plus__Plus_param_L_LC, .-Plus__Plus_param_L_LC
	.align 4
	.type	Plus__uPlus_var_L_i32, #function
	.proc	05
Plus__uPlus_var_L_i32:
.LLFB446:
	save	%sp, -96, %sp
.LLCFI446:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE446:
	.size	Plus__uPlus_var_L_i32, .-Plus__uPlus_var_L_i32
	.align 4
	.type	Plus__Plus_var_L_i32, #function
	.proc	05
Plus__Plus_var_L_i32:
.LLFB447:
	save	%sp, -96, %sp
.LLCFI447:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE447:
	.size	Plus__Plus_var_L_i32, .-Plus__Plus_var_L_i32
	.align 4
	.type	Plus__uPlus_param_L_i32, #function
	.proc	05
Plus__uPlus_param_L_i32:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE448:
	.size	Plus__uPlus_param_L_i32, .-Plus__uPlus_param_L_i32
	.align 4
	.type	Plus__Plus_param_L_i32, #function
	.proc	05
Plus__Plus_param_L_i32:
.LLFB449:
	save	%sp, -104, %sp
.LLCFI449:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE449:
	.size	Plus__Plus_param_L_i32, .-Plus__Plus_param_L_i32
	.align 4
	.type	Plus__uPlus_var_L_i16, #function
	.proc	05
Plus__uPlus_var_L_i16:
.LLFB450:
	save	%sp, -96, %sp
.LLCFI450:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE450:
	.size	Plus__uPlus_var_L_i16, .-Plus__uPlus_var_L_i16
	.align 4
	.type	Plus__Plus_var_L_i16, #function
	.proc	05
Plus__Plus_var_L_i16:
.LLFB451:
	save	%sp, -96, %sp
.LLCFI451:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE451:
	.size	Plus__Plus_var_L_i16, .-Plus__Plus_var_L_i16
	.align 4
	.type	Plus__uPlus_param_L_i16, #function
	.proc	05
Plus__uPlus_param_L_i16:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE452:
	.size	Plus__uPlus_param_L_i16, .-Plus__uPlus_param_L_i16
	.align 4
	.type	Plus__Plus_param_L_i16, #function
	.proc	05
Plus__Plus_param_L_i16:
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
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE453:
	.size	Plus__Plus_param_L_i16, .-Plus__Plus_param_L_i16
	.align 4
	.type	Plus__uPlus_var_L_I, #function
	.proc	05
Plus__uPlus_var_L_I:
.LLFB454:
	save	%sp, -96, %sp
.LLCFI454:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE454:
	.size	Plus__uPlus_var_L_I, .-Plus__uPlus_var_L_I
	.align 4
	.type	Plus__Plus_var_L_I, #function
	.proc	05
Plus__Plus_var_L_I:
.LLFB455:
	save	%sp, -96, %sp
.LLCFI455:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE455:
	.size	Plus__Plus_var_L_I, .-Plus__Plus_var_L_I
	.align 4
	.type	Plus__uPlus_param_L_I, #function
	.proc	05
Plus__uPlus_param_L_I:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE456:
	.size	Plus__uPlus_param_L_I, .-Plus__uPlus_param_L_I
	.align 4
	.type	Plus__Plus_param_L_I, #function
	.proc	05
Plus__Plus_param_L_I:
.LLFB457:
	save	%sp, -104, %sp
.LLCFI457:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE457:
	.size	Plus__Plus_param_L_I, .-Plus__Plus_param_L_I
	.align 4
	.type	Plus__uPlus_var_L_i64, #function
	.proc	05
Plus__uPlus_var_L_i64:
.LLFB458:
	save	%sp, -96, %sp
.LLCFI458:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE458:
	.size	Plus__uPlus_var_L_i64, .-Plus__uPlus_var_L_i64
	.align 4
	.type	Plus__Plus_var_L_i64, #function
	.proc	05
Plus__Plus_var_L_i64:
.LLFB459:
	save	%sp, -96, %sp
.LLCFI459:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o2
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE459:
	.size	Plus__Plus_var_L_i64, .-Plus__Plus_var_L_i64
	.align 4
	.type	Plus__uPlus_param_L_i64, #function
	.proc	05
Plus__uPlus_param_L_i64:
.LLFB460:
	save	%sp, -112, %sp
.LLCFI460:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE460:
	.size	Plus__uPlus_param_L_i64, .-Plus__uPlus_param_L_i64
	.align 4
	.type	Plus__Plus_param_L_i64, #function
	.proc	05
Plus__Plus_param_L_i64:
.LLFB461:
	save	%sp, -112, %sp
.LLCFI461:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE461:
	.size	Plus__Plus_param_L_i64, .-Plus__Plus_param_L_i64
	.align 4
	.type	Plus__uPlus_var_L_C, #function
	.proc	05
Plus__uPlus_var_L_C:
.LLFB462:
	save	%sp, -96, %sp
.LLCFI462:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE462:
	.size	Plus__uPlus_var_L_C, .-Plus__uPlus_var_L_C
	.align 4
	.type	Plus__Plus_var_L_C, #function
	.proc	05
Plus__Plus_var_L_C:
.LLFB463:
	save	%sp, -96, %sp
.LLCFI463:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE463:
	.size	Plus__Plus_var_L_C, .-Plus__Plus_var_L_C
	.align 4
	.type	Plus__uPlus_param_L_C, #function
	.proc	05
Plus__uPlus_param_L_C:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE464:
	.size	Plus__uPlus_param_L_C, .-Plus__uPlus_param_L_C
	.align 4
	.type	Plus__Plus_param_L_C, #function
	.proc	05
Plus__Plus_param_L_C:
.LLFB465:
	save	%sp, -104, %sp
.LLCFI465:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE465:
	.size	Plus__Plus_param_L_C, .-Plus__Plus_param_L_C
	.align 4
	.type	Plus__uPlus_var_L_u8, #function
	.proc	05
Plus__uPlus_var_L_u8:
.LLFB466:
	save	%sp, -96, %sp
.LLCFI466:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE466:
	.size	Plus__uPlus_var_L_u8, .-Plus__uPlus_var_L_u8
	.align 4
	.type	Plus__Plus_var_L_u8, #function
	.proc	05
Plus__Plus_var_L_u8:
.LLFB467:
	save	%sp, -96, %sp
.LLCFI467:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE467:
	.size	Plus__Plus_var_L_u8, .-Plus__Plus_var_L_u8
	.align 4
	.type	Plus__uPlus_param_L_u8, #function
	.proc	05
Plus__uPlus_param_L_u8:
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
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE468:
	.size	Plus__uPlus_param_L_u8, .-Plus__uPlus_param_L_u8
	.align 4
	.type	Plus__Plus_param_L_u8, #function
	.proc	05
Plus__Plus_param_L_u8:
.LLFB469:
	save	%sp, -104, %sp
.LLCFI469:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE469:
	.size	Plus__Plus_param_L_u8, .-Plus__Plus_param_L_u8
	.align 4
	.type	Plus__uPlus_var_L_L, #function
	.proc	05
Plus__uPlus_var_L_L:
.LLFB470:
	save	%sp, -96, %sp
.LLCFI470:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE470:
	.size	Plus__uPlus_var_L_L, .-Plus__uPlus_var_L_L
	.align 4
	.type	Plus__Plus_var_L_L, #function
	.proc	05
Plus__Plus_var_L_L:
.LLFB471:
	save	%sp, -96, %sp
.LLCFI471:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o2
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE471:
	.size	Plus__Plus_var_L_L, .-Plus__Plus_var_L_L
	.align 4
	.type	Plus__uPlus_param_L_L, #function
	.proc	05
Plus__uPlus_param_L_L:
.LLFB472:
	save	%sp, -112, %sp
.LLCFI472:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE472:
	.size	Plus__uPlus_param_L_L, .-Plus__uPlus_param_L_L
	.align 4
	.type	Plus__Plus_param_L_L, #function
	.proc	05
Plus__Plus_param_L_L:
.LLFB473:
	save	%sp, -112, %sp
.LLCFI473:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	addcc	%o3, %o5, %o5
	addx	%o2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE473:
	.size	Plus__Plus_param_L_L, .-Plus__Plus_param_L_L
	.align 4
	.type	Plus__uPlus_var_L_i8, #function
	.proc	05
Plus__uPlus_var_L_i8:
.LLFB474:
	save	%sp, -96, %sp
.LLCFI474:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE474:
	.size	Plus__uPlus_var_L_i8, .-Plus__uPlus_var_L_i8
	.align 4
	.type	Plus__Plus_var_L_i8, #function
	.proc	05
Plus__Plus_var_L_i8:
.LLFB475:
	save	%sp, -96, %sp
.LLCFI475:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE475:
	.size	Plus__Plus_var_L_i8, .-Plus__Plus_var_L_i8
	.align 4
	.type	Plus__uPlus_param_L_i8, #function
	.proc	05
Plus__uPlus_param_L_i8:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE476:
	.size	Plus__uPlus_param_L_i8, .-Plus__uPlus_param_L_i8
	.align 4
	.type	Plus__Plus_param_L_i8, #function
	.proc	05
Plus__Plus_param_L_i8:
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
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE477:
	.size	Plus__Plus_param_L_i8, .-Plus__Plus_param_L_i8
	.align 4
	.type	Plus__uPlus_var_L_u32, #function
	.proc	05
Plus__uPlus_var_L_u32:
.LLFB478:
	save	%sp, -96, %sp
.LLCFI478:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE478:
	.size	Plus__uPlus_var_L_u32, .-Plus__uPlus_var_L_u32
	.align 4
	.type	Plus__Plus_var_L_u32, #function
	.proc	05
Plus__Plus_var_L_u32:
.LLFB479:
	save	%sp, -96, %sp
.LLCFI479:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE479:
	.size	Plus__Plus_var_L_u32, .-Plus__Plus_var_L_u32
	.align 4
	.type	Plus__uPlus_param_L_u32, #function
	.proc	05
Plus__uPlus_param_L_u32:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE480:
	.size	Plus__uPlus_param_L_u32, .-Plus__uPlus_param_L_u32
	.align 4
	.type	Plus__Plus_param_L_u32, #function
	.proc	05
Plus__Plus_param_L_u32:
.LLFB481:
	save	%sp, -104, %sp
.LLCFI481:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE481:
	.size	Plus__Plus_param_L_u32, .-Plus__Plus_param_L_u32
	.align 4
	.type	Plus__uPlus_var_i8_u16, #function
	.proc	04
Plus__uPlus_var_i8_u16:
.LLFB482:
	save	%sp, -96, %sp
.LLCFI482:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE482:
	.size	Plus__uPlus_var_i8_u16, .-Plus__uPlus_var_i8_u16
	.align 4
	.type	Plus__Plus_var_i8_u16, #function
	.proc	02
Plus__Plus_var_i8_u16:
.LLFB483:
	save	%sp, -96, %sp
.LLCFI483:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	add	%o5, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL585
	 nop
	sethi	%hi(16384), %g1
	or	%g1, 801, %o0
	call	_m3_fault, 0
	 nop
.LL585:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE483:
	.size	Plus__Plus_var_i8_u16, .-Plus__Plus_var_i8_u16
	.align 4
	.type	Plus__uPlus_param_i8_u16, #function
	.proc	04
Plus__uPlus_param_i8_u16:
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
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE484:
	.size	Plus__uPlus_param_i8_u16, .-Plus__uPlus_param_i8_u16
	.align 4
	.type	Plus__Plus_param_i8_u16, #function
	.proc	02
Plus__Plus_param_i8_u16:
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
	add	%o5, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL588
	 nop
	sethi	%hi(16384), %g1
	or	%g1, 865, %o0
	call	_m3_fault, 0
	 nop
.LL588:
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE485:
	.size	Plus__Plus_param_i8_u16, .-Plus__Plus_param_i8_u16
	.align 4
	.type	Plus__uPlus_var_i8_u64, #function
	.proc	05
Plus__uPlus_var_i8_u64:
.LLFB486:
	save	%sp, -96, %sp
.LLCFI486:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE486:
	.size	Plus__uPlus_var_i8_u64, .-Plus__uPlus_var_i8_u64
	.align 4
	.type	Plus__Plus_var_i8_u64, #function
	.proc	05
Plus__Plus_var_i8_u64:
.LLFB487:
	save	%sp, -96, %sp
.LLCFI487:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE487:
	.size	Plus__Plus_var_i8_u64, .-Plus__Plus_var_i8_u64
	.align 4
	.type	Plus__uPlus_param_i8_u64, #function
	.proc	05
Plus__uPlus_param_i8_u64:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE488:
	.size	Plus__uPlus_param_i8_u64, .-Plus__uPlus_param_i8_u64
	.align 4
	.type	Plus__Plus_param_i8_u64, #function
	.proc	05
Plus__Plus_param_i8_u64:
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
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE489:
	.size	Plus__Plus_param_i8_u64, .-Plus__Plus_param_i8_u64
	.align 4
	.type	Plus__uPlus_var_i8_LC, #function
	.proc	05
Plus__uPlus_var_i8_LC:
.LLFB490:
	save	%sp, -96, %sp
.LLCFI490:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE490:
	.size	Plus__uPlus_var_i8_LC, .-Plus__uPlus_var_i8_LC
	.align 4
	.type	Plus__Plus_var_i8_LC, #function
	.proc	05
Plus__Plus_var_i8_LC:
.LLFB491:
	save	%sp, -96, %sp
.LLCFI491:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE491:
	.size	Plus__Plus_var_i8_LC, .-Plus__Plus_var_i8_LC
	.align 4
	.type	Plus__uPlus_param_i8_LC, #function
	.proc	05
Plus__uPlus_param_i8_LC:
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
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE492:
	.size	Plus__uPlus_param_i8_LC, .-Plus__uPlus_param_i8_LC
	.align 4
	.type	Plus__Plus_param_i8_LC, #function
	.proc	05
Plus__Plus_param_i8_LC:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE493:
	.size	Plus__Plus_param_i8_LC, .-Plus__Plus_param_i8_LC
	.align 4
	.type	Plus__uPlus_var_i8_i32, #function
	.proc	04
Plus__uPlus_var_i8_i32:
.LLFB494:
	save	%sp, -96, %sp
.LLCFI494:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE494:
	.size	Plus__uPlus_var_i8_i32, .-Plus__uPlus_var_i8_i32
	.align 4
	.type	Plus__Plus_var_i8_i32, #function
	.proc	02
Plus__Plus_var_i8_i32:
.LLFB495:
	save	%sp, -96, %sp
.LLCFI495:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	add	%o5, %g1, %g1
	cmp	%g1, -128
	bl	%icc, .LL599
	 nop
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL600
	 nop
.LL599:
	sethi	%hi(17408), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL600:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE495:
	.size	Plus__Plus_var_i8_i32, .-Plus__Plus_var_i8_i32
	.align 4
	.type	Plus__uPlus_param_i8_i32, #function
	.proc	04
Plus__uPlus_param_i8_i32:
.LLFB496:
	save	%sp, -96, %sp
.LLCFI496:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE496:
	.size	Plus__uPlus_param_i8_i32, .-Plus__uPlus_param_i8_i32
	.align 4
	.type	Plus__Plus_param_i8_i32, #function
	.proc	02
Plus__Plus_param_i8_i32:
.LLFB497:
	save	%sp, -96, %sp
.LLCFI497:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	cmp	%g1, -128
	bl	%icc, .LL603
	 nop
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL604
	 nop
.LL603:
	sethi	%hi(17408), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL604:
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE497:
	.size	Plus__Plus_param_i8_i32, .-Plus__Plus_param_i8_i32
	.align 4
	.type	Plus__uPlus_var_i8_i16, #function
	.proc	04
Plus__uPlus_var_i8_i16:
.LLFB498:
	save	%sp, -96, %sp
.LLCFI498:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE498:
	.size	Plus__uPlus_var_i8_i16, .-Plus__uPlus_var_i8_i16
	.align 4
	.type	Plus__Plus_var_i8_i16, #function
	.proc	02
Plus__Plus_var_i8_i16:
.LLFB499:
	save	%sp, -96, %sp
.LLCFI499:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	add	%o5, %g1, %g1
	cmp	%g1, -128
	bl	%icc, .LL607
	 nop
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	add	%o5, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL608
	 nop
.LL607:
	sethi	%hi(17408), %g1
	or	%g1, 289, %o0
	call	_m3_fault, 0
	 nop
.LL608:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE499:
	.size	Plus__Plus_var_i8_i16, .-Plus__Plus_var_i8_i16
	.align 4
	.type	Plus__uPlus_param_i8_i16, #function
	.proc	04
Plus__uPlus_param_i8_i16:
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
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE500:
	.size	Plus__uPlus_param_i8_i16, .-Plus__uPlus_param_i8_i16
	.align 4
	.type	Plus__Plus_param_i8_i16, #function
	.proc	02
Plus__Plus_param_i8_i16:
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
	add	%o5, %g1, %g1
	cmp	%g1, -128
	bl	%icc, .LL611
	 nop
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	add	%o5, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL612
	 nop
.LL611:
	sethi	%hi(17408), %g1
	or	%g1, 353, %o0
	call	_m3_fault, 0
	 nop
.LL612:
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE501:
	.size	Plus__Plus_param_i8_i16, .-Plus__Plus_param_i8_i16
	.align 4
	.type	Plus__uPlus_var_i8_I, #function
	.proc	04
Plus__uPlus_var_i8_I:
.LLFB502:
	save	%sp, -96, %sp
.LLCFI502:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE502:
	.size	Plus__uPlus_var_i8_I, .-Plus__uPlus_var_i8_I
	.align 4
	.type	Plus__Plus_var_i8_I, #function
	.proc	02
Plus__Plus_var_i8_I:
.LLFB503:
	save	%sp, -96, %sp
.LLCFI503:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	add	%o5, %g1, %g1
	cmp	%g1, -128
	bl	%icc, .LL615
	 nop
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL616
	 nop
.LL615:
	sethi	%hi(17408), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL616:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE503:
	.size	Plus__Plus_var_i8_I, .-Plus__Plus_var_i8_I
	.align 4
	.type	Plus__uPlus_param_i8_I, #function
	.proc	04
Plus__uPlus_param_i8_I:
.LLFB504:
	save	%sp, -96, %sp
.LLCFI504:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE504:
	.size	Plus__uPlus_param_i8_I, .-Plus__uPlus_param_i8_I
	.align 4
	.type	Plus__Plus_param_i8_I, #function
	.proc	02
Plus__Plus_param_i8_I:
.LLFB505:
	save	%sp, -96, %sp
.LLCFI505:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	cmp	%g1, -128
	bl	%icc, .LL619
	 nop
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL620
	 nop
.LL619:
	sethi	%hi(17408), %g1
	or	%g1, 481, %o0
	call	_m3_fault, 0
	 nop
.LL620:
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE505:
	.size	Plus__Plus_param_i8_I, .-Plus__Plus_param_i8_I
	.align 4
	.type	Plus__uPlus_var_i8_i64, #function
	.proc	05
Plus__uPlus_var_i8_i64:
.LLFB506:
	save	%sp, -96, %sp
.LLCFI506:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE506:
	.size	Plus__uPlus_var_i8_i64, .-Plus__uPlus_var_i8_i64
	.align 4
	.type	Plus__Plus_var_i8_i64, #function
	.proc	05
Plus__Plus_var_i8_i64:
.LLFB507:
	save	%sp, -96, %sp
.LLCFI507:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE507:
	.size	Plus__Plus_var_i8_i64, .-Plus__Plus_var_i8_i64
	.align 4
	.type	Plus__uPlus_param_i8_i64, #function
	.proc	05
Plus__uPlus_param_i8_i64:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE508:
	.size	Plus__uPlus_param_i8_i64, .-Plus__uPlus_param_i8_i64
	.align 4
	.type	Plus__Plus_param_i8_i64, #function
	.proc	05
Plus__Plus_param_i8_i64:
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
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE509:
	.size	Plus__Plus_param_i8_i64, .-Plus__Plus_param_i8_i64
	.align 4
	.type	Plus__uPlus_var_i8_C, #function
	.proc	04
Plus__uPlus_var_i8_C:
.LLFB510:
	save	%sp, -96, %sp
.LLCFI510:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE510:
	.size	Plus__uPlus_var_i8_C, .-Plus__uPlus_var_i8_C
	.align 4
	.type	Plus__Plus_var_i8_C, #function
	.proc	02
Plus__Plus_var_i8_C:
.LLFB511:
	save	%sp, -96, %sp
.LLCFI511:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	add	%o5, %g1, %g1
	cmp	%g1, -128
	bl	%icc, .LL627
	 nop
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL628
	 nop
.LL627:
	sethi	%hi(17408), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL628:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE511:
	.size	Plus__Plus_var_i8_C, .-Plus__Plus_var_i8_C
	.align 4
	.type	Plus__uPlus_param_i8_C, #function
	.proc	04
Plus__uPlus_param_i8_C:
.LLFB512:
	save	%sp, -96, %sp
.LLCFI512:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE512:
	.size	Plus__uPlus_param_i8_C, .-Plus__uPlus_param_i8_C
	.align 4
	.type	Plus__Plus_param_i8_C, #function
	.proc	02
Plus__Plus_param_i8_C:
.LLFB513:
	save	%sp, -96, %sp
.LLCFI513:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	cmp	%g1, -128
	bl	%icc, .LL631
	 nop
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL632
	 nop
.LL631:
	sethi	%hi(17408), %g1
	or	%g1, 737, %o0
	call	_m3_fault, 0
	 nop
.LL632:
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE513:
	.size	Plus__Plus_param_i8_C, .-Plus__Plus_param_i8_C
	.align 4
	.type	Plus__uPlus_var_i8_u8, #function
	.proc	04
Plus__uPlus_var_i8_u8:
.LLFB514:
	save	%sp, -96, %sp
.LLCFI514:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE514:
	.size	Plus__uPlus_var_i8_u8, .-Plus__uPlus_var_i8_u8
	.align 4
	.type	Plus__Plus_var_i8_u8, #function
	.proc	02
Plus__Plus_var_i8_u8:
.LLFB515:
	save	%sp, -96, %sp
.LLCFI515:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	add	%o5, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL635
	 nop
	sethi	%hi(17408), %g1
	or	%g1, 801, %o0
	call	_m3_fault, 0
	 nop
.LL635:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE515:
	.size	Plus__Plus_var_i8_u8, .-Plus__Plus_var_i8_u8
	.align 4
	.type	Plus__uPlus_param_i8_u8, #function
	.proc	04
Plus__uPlus_param_i8_u8:
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
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE516:
	.size	Plus__uPlus_param_i8_u8, .-Plus__uPlus_param_i8_u8
	.align 4
	.type	Plus__Plus_param_i8_u8, #function
	.proc	02
Plus__Plus_param_i8_u8:
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
	add	%o5, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL638
	 nop
	sethi	%hi(17408), %g1
	or	%g1, 865, %o0
	call	_m3_fault, 0
	 nop
.LL638:
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE517:
	.size	Plus__Plus_param_i8_u8, .-Plus__Plus_param_i8_u8
	.align 4
	.type	Plus__uPlus_var_i8_L, #function
	.proc	05
Plus__uPlus_var_i8_L:
.LLFB518:
	save	%sp, -96, %sp
.LLCFI518:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE518:
	.size	Plus__uPlus_var_i8_L, .-Plus__uPlus_var_i8_L
	.align 4
	.type	Plus__Plus_var_i8_L, #function
	.proc	05
Plus__Plus_var_i8_L:
.LLFB519:
	save	%sp, -96, %sp
.LLCFI519:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE519:
	.size	Plus__Plus_var_i8_L, .-Plus__Plus_var_i8_L
	.align 4
	.type	Plus__uPlus_param_i8_L, #function
	.proc	05
Plus__uPlus_param_i8_L:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE520:
	.size	Plus__uPlus_param_i8_L, .-Plus__uPlus_param_i8_L
	.align 4
	.type	Plus__Plus_param_i8_L, #function
	.proc	05
Plus__Plus_param_i8_L:
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
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE521:
	.size	Plus__Plus_param_i8_L, .-Plus__Plus_param_i8_L
	.align 4
	.type	Plus__uPlus_var_i8_i8, #function
	.proc	04
Plus__uPlus_var_i8_i8:
.LLFB522:
	save	%sp, -96, %sp
.LLCFI522:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE522:
	.size	Plus__uPlus_var_i8_i8, .-Plus__uPlus_var_i8_i8
	.align 4
	.type	Plus__Plus_var_i8_i8, #function
	.proc	02
Plus__Plus_var_i8_i8:
.LLFB523:
	save	%sp, -96, %sp
.LLCFI523:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	add	%o5, %g1, %g1
	cmp	%g1, -128
	bl	%icc, .LL645
	 nop
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	add	%o5, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL646
	 nop
.LL645:
	sethi	%hi(18432), %g1
	or	%g1, 33, %o0
	call	_m3_fault, 0
	 nop
.LL646:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE523:
	.size	Plus__Plus_var_i8_i8, .-Plus__Plus_var_i8_i8
	.align 4
	.type	Plus__uPlus_param_i8_i8, #function
	.proc	04
Plus__uPlus_param_i8_i8:
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
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE524:
	.size	Plus__uPlus_param_i8_i8, .-Plus__uPlus_param_i8_i8
	.align 4
	.type	Plus__Plus_param_i8_i8, #function
	.proc	02
Plus__Plus_param_i8_i8:
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
	add	%o5, %g1, %g1
	cmp	%g1, -128
	bl	%icc, .LL649
	 nop
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	add	%o5, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL650
	 nop
.LL649:
	sethi	%hi(18432), %g1
	or	%g1, 97, %o0
	call	_m3_fault, 0
	 nop
.LL650:
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE525:
	.size	Plus__Plus_param_i8_i8, .-Plus__Plus_param_i8_i8
	.align 4
	.type	Plus__uPlus_var_i8_u32, #function
	.proc	04
Plus__uPlus_var_i8_u32:
.LLFB526:
	save	%sp, -96, %sp
.LLCFI526:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE526:
	.size	Plus__uPlus_var_i8_u32, .-Plus__uPlus_var_i8_u32
	.align 4
	.type	Plus__Plus_var_i8_u32, #function
	.proc	02
Plus__Plus_var_i8_u32:
.LLFB527:
	save	%sp, -96, %sp
.LLCFI527:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	add	%o5, %g1, %g1
	cmp	%g1, -128
	bl	%icc, .LL653
	 nop
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL654
	 nop
.LL653:
	sethi	%hi(18432), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL654:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE527:
	.size	Plus__Plus_var_i8_u32, .-Plus__Plus_var_i8_u32
	.align 4
	.type	Plus__uPlus_param_i8_u32, #function
	.proc	04
Plus__uPlus_param_i8_u32:
.LLFB528:
	save	%sp, -96, %sp
.LLCFI528:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE528:
	.size	Plus__uPlus_param_i8_u32, .-Plus__uPlus_param_i8_u32
	.align 4
	.type	Plus__Plus_param_i8_u32, #function
	.proc	02
Plus__Plus_param_i8_u32:
.LLFB529:
	save	%sp, -96, %sp
.LLCFI529:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	cmp	%g1, -128
	bl	%icc, .LL657
	 nop
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL658
	 nop
.LL657:
	sethi	%hi(18432), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL658:
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE529:
	.size	Plus__Plus_param_i8_u32, .-Plus__Plus_param_i8_u32
	.align 4
	.type	Plus__uPlus_var_u32_u16, #function
	.proc	04
Plus__uPlus_var_u32_u16:
.LLFB530:
	save	%sp, -96, %sp
.LLCFI530:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE530:
	.size	Plus__uPlus_var_u32_u16, .-Plus__uPlus_var_u32_u16
	.align 4
	.type	Plus__Plus_var_u32_u16, #function
	.proc	04
Plus__Plus_var_u32_u16:
.LLFB531:
	save	%sp, -96, %sp
.LLCFI531:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE531:
	.size	Plus__Plus_var_u32_u16, .-Plus__Plus_var_u32_u16
	.align 4
	.type	Plus__uPlus_param_u32_u16, #function
	.proc	04
Plus__uPlus_param_u32_u16:
.LLFB532:
	save	%sp, -96, %sp
.LLCFI532:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+68], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE532:
	.size	Plus__uPlus_param_u32_u16, .-Plus__uPlus_param_u32_u16
	.align 4
	.type	Plus__Plus_param_u32_u16, #function
	.proc	04
Plus__Plus_param_u32_u16:
.LLFB533:
	save	%sp, -96, %sp
.LLCFI533:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+68], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE533:
	.size	Plus__Plus_param_u32_u16, .-Plus__Plus_param_u32_u16
	.align 4
	.type	Plus__uPlus_var_u32_u64, #function
	.proc	05
Plus__uPlus_var_u32_u64:
.LLFB534:
	save	%sp, -96, %sp
.LLCFI534:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE534:
	.size	Plus__uPlus_var_u32_u64, .-Plus__uPlus_var_u32_u64
	.align 4
	.type	Plus__Plus_var_u32_u64, #function
	.proc	05
Plus__Plus_var_u32_u64:
.LLFB535:
	save	%sp, -96, %sp
.LLCFI535:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+56], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE535:
	.size	Plus__Plus_var_u32_u64, .-Plus__Plus_var_u32_u64
	.align 4
	.type	Plus__uPlus_param_u32_u64, #function
	.proc	05
Plus__uPlus_param_u32_u64:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE536:
	.size	Plus__uPlus_param_u32_u64, .-Plus__uPlus_param_u32_u64
	.align 4
	.type	Plus__Plus_param_u32_u64, #function
	.proc	05
Plus__Plus_param_u32_u64:
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
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE537:
	.size	Plus__Plus_param_u32_u64, .-Plus__Plus_param_u32_u64
	.align 4
	.type	Plus__uPlus_var_u32_LC, #function
	.proc	05
Plus__uPlus_var_u32_LC:
.LLFB538:
	save	%sp, -96, %sp
.LLCFI538:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE538:
	.size	Plus__uPlus_var_u32_LC, .-Plus__uPlus_var_u32_LC
	.align 4
	.type	Plus__Plus_var_u32_LC, #function
	.proc	05
Plus__Plus_var_u32_LC:
.LLFB539:
	save	%sp, -96, %sp
.LLCFI539:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+64], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE539:
	.size	Plus__Plus_var_u32_LC, .-Plus__Plus_var_u32_LC
	.align 4
	.type	Plus__uPlus_param_u32_LC, #function
	.proc	05
Plus__uPlus_param_u32_LC:
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
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE540:
	.size	Plus__uPlus_param_u32_LC, .-Plus__uPlus_param_u32_LC
	.align 4
	.type	Plus__Plus_param_u32_LC, #function
	.proc	05
Plus__Plus_param_u32_LC:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE541:
	.size	Plus__Plus_param_u32_LC, .-Plus__Plus_param_u32_LC
	.align 4
	.type	Plus__uPlus_var_u32_i32, #function
	.proc	04
Plus__uPlus_var_u32_i32:
.LLFB542:
	save	%sp, -96, %sp
.LLCFI542:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE542:
	.size	Plus__uPlus_var_u32_i32, .-Plus__uPlus_var_u32_i32
	.align 4
	.type	Plus__Plus_var_u32_i32, #function
	.proc	04
Plus__Plus_var_u32_i32:
.LLFB543:
	save	%sp, -96, %sp
.LLCFI543:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE543:
	.size	Plus__Plus_var_u32_i32, .-Plus__Plus_var_u32_i32
	.align 4
	.type	Plus__uPlus_param_u32_i32, #function
	.proc	04
Plus__uPlus_param_u32_i32:
.LLFB544:
	save	%sp, -96, %sp
.LLCFI544:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE544:
	.size	Plus__uPlus_param_u32_i32, .-Plus__uPlus_param_u32_i32
	.align 4
	.type	Plus__Plus_param_u32_i32, #function
	.proc	04
Plus__Plus_param_u32_i32:
.LLFB545:
	save	%sp, -96, %sp
.LLCFI545:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE545:
	.size	Plus__Plus_param_u32_i32, .-Plus__Plus_param_u32_i32
	.align 4
	.type	Plus__uPlus_var_u32_i16, #function
	.proc	04
Plus__uPlus_var_u32_i16:
.LLFB546:
	save	%sp, -96, %sp
.LLCFI546:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE546:
	.size	Plus__uPlus_var_u32_i16, .-Plus__uPlus_var_u32_i16
	.align 4
	.type	Plus__Plus_var_u32_i16, #function
	.proc	04
Plus__Plus_var_u32_i16:
.LLFB547:
	save	%sp, -96, %sp
.LLCFI547:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE547:
	.size	Plus__Plus_var_u32_i16, .-Plus__Plus_var_u32_i16
	.align 4
	.type	Plus__uPlus_param_u32_i16, #function
	.proc	04
Plus__uPlus_param_u32_i16:
.LLFB548:
	save	%sp, -96, %sp
.LLCFI548:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+68], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE548:
	.size	Plus__uPlus_param_u32_i16, .-Plus__uPlus_param_u32_i16
	.align 4
	.type	Plus__Plus_param_u32_i16, #function
	.proc	04
Plus__Plus_param_u32_i16:
.LLFB549:
	save	%sp, -96, %sp
.LLCFI549:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+68], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE549:
	.size	Plus__Plus_param_u32_i16, .-Plus__Plus_param_u32_i16
	.align 4
	.type	Plus__uPlus_var_u32_I, #function
	.proc	04
Plus__uPlus_var_u32_I:
.LLFB550:
	save	%sp, -96, %sp
.LLCFI550:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE550:
	.size	Plus__uPlus_var_u32_I, .-Plus__uPlus_var_u32_I
	.align 4
	.type	Plus__Plus_var_u32_I, #function
	.proc	04
Plus__Plus_var_u32_I:
.LLFB551:
	save	%sp, -96, %sp
.LLCFI551:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+80], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE551:
	.size	Plus__Plus_var_u32_I, .-Plus__Plus_var_u32_I
	.align 4
	.type	Plus__uPlus_param_u32_I, #function
	.proc	04
Plus__uPlus_param_u32_I:
.LLFB552:
	save	%sp, -96, %sp
.LLCFI552:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE552:
	.size	Plus__uPlus_param_u32_I, .-Plus__uPlus_param_u32_I
	.align 4
	.type	Plus__Plus_param_u32_I, #function
	.proc	04
Plus__Plus_param_u32_I:
.LLFB553:
	save	%sp, -96, %sp
.LLCFI553:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE553:
	.size	Plus__Plus_param_u32_I, .-Plus__Plus_param_u32_I
	.align 4
	.type	Plus__uPlus_var_u32_i64, #function
	.proc	05
Plus__uPlus_var_u32_i64:
.LLFB554:
	save	%sp, -96, %sp
.LLCFI554:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE554:
	.size	Plus__uPlus_var_u32_i64, .-Plus__uPlus_var_u32_i64
	.align 4
	.type	Plus__Plus_var_u32_i64, #function
	.proc	05
Plus__Plus_var_u32_i64:
.LLFB555:
	save	%sp, -96, %sp
.LLCFI555:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+88], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE555:
	.size	Plus__Plus_var_u32_i64, .-Plus__Plus_var_u32_i64
	.align 4
	.type	Plus__uPlus_param_u32_i64, #function
	.proc	05
Plus__uPlus_param_u32_i64:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE556:
	.size	Plus__uPlus_param_u32_i64, .-Plus__uPlus_param_u32_i64
	.align 4
	.type	Plus__Plus_param_u32_i64, #function
	.proc	05
Plus__Plus_param_u32_i64:
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
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE557:
	.size	Plus__Plus_param_u32_i64, .-Plus__Plus_param_u32_i64
	.align 4
	.type	Plus__uPlus_var_u32_C, #function
	.proc	04
Plus__uPlus_var_u32_C:
.LLFB558:
	save	%sp, -96, %sp
.LLCFI558:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE558:
	.size	Plus__uPlus_var_u32_C, .-Plus__uPlus_var_u32_C
	.align 4
	.type	Plus__Plus_var_u32_C, #function
	.proc	04
Plus__Plus_var_u32_C:
.LLFB559:
	save	%sp, -96, %sp
.LLCFI559:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+96], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE559:
	.size	Plus__Plus_var_u32_C, .-Plus__Plus_var_u32_C
	.align 4
	.type	Plus__uPlus_param_u32_C, #function
	.proc	04
Plus__uPlus_param_u32_C:
.LLFB560:
	save	%sp, -96, %sp
.LLCFI560:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE560:
	.size	Plus__uPlus_param_u32_C, .-Plus__uPlus_param_u32_C
	.align 4
	.type	Plus__Plus_param_u32_C, #function
	.proc	04
Plus__Plus_param_u32_C:
.LLFB561:
	save	%sp, -96, %sp
.LLCFI561:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE561:
	.size	Plus__Plus_param_u32_C, .-Plus__Plus_param_u32_C
	.align 4
	.type	Plus__uPlus_var_u32_u8, #function
	.proc	04
Plus__uPlus_var_u32_u8:
.LLFB562:
	save	%sp, -96, %sp
.LLCFI562:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE562:
	.size	Plus__uPlus_var_u32_u8, .-Plus__uPlus_var_u32_u8
	.align 4
	.type	Plus__Plus_var_u32_u8, #function
	.proc	04
Plus__Plus_var_u32_u8:
.LLFB563:
	save	%sp, -96, %sp
.LLCFI563:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE563:
	.size	Plus__Plus_var_u32_u8, .-Plus__Plus_var_u32_u8
	.align 4
	.type	Plus__uPlus_param_u32_u8, #function
	.proc	04
Plus__uPlus_param_u32_u8:
.LLFB564:
	save	%sp, -96, %sp
.LLCFI564:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+68], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE564:
	.size	Plus__uPlus_param_u32_u8, .-Plus__uPlus_param_u32_u8
	.align 4
	.type	Plus__Plus_param_u32_u8, #function
	.proc	04
Plus__Plus_param_u32_u8:
.LLFB565:
	save	%sp, -96, %sp
.LLCFI565:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+68], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE565:
	.size	Plus__Plus_param_u32_u8, .-Plus__Plus_param_u32_u8
	.align 4
	.type	Plus__uPlus_var_u32_L, #function
	.proc	05
Plus__uPlus_var_u32_L:
.LLFB566:
	save	%sp, -96, %sp
.LLCFI566:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE566:
	.size	Plus__uPlus_var_u32_L, .-Plus__uPlus_var_u32_L
	.align 4
	.type	Plus__Plus_var_u32_L, #function
	.proc	05
Plus__Plus_var_u32_L:
.LLFB567:
	save	%sp, -96, %sp
.LLCFI567:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ldd	[%g1+120], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE567:
	.size	Plus__Plus_var_u32_L, .-Plus__Plus_var_u32_L
	.align 4
	.type	Plus__uPlus_param_u32_L, #function
	.proc	05
Plus__uPlus_param_u32_L:
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
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE568:
	.size	Plus__uPlus_param_u32_L, .-Plus__uPlus_param_u32_L
	.align 4
	.type	Plus__Plus_param_u32_L, #function
	.proc	05
Plus__Plus_param_u32_L:
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
	ldd	[%fp-8], %o2
	addcc	%o5, %o3, %o5
	addx	%o4, %o2, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE569:
	.size	Plus__Plus_param_u32_L, .-Plus__Plus_param_u32_L
	.align 4
	.type	Plus__uPlus_var_u32_i8, #function
	.proc	04
Plus__uPlus_var_u32_i8:
.LLFB570:
	save	%sp, -96, %sp
.LLCFI570:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE570:
	.size	Plus__uPlus_var_u32_i8, .-Plus__uPlus_var_u32_i8
	.align 4
	.type	Plus__Plus_var_u32_i8, #function
	.proc	04
Plus__Plus_var_u32_i8:
.LLFB571:
	save	%sp, -96, %sp
.LLCFI571:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE571:
	.size	Plus__Plus_var_u32_i8, .-Plus__Plus_var_u32_i8
	.align 4
	.type	Plus__uPlus_param_u32_i8, #function
	.proc	04
Plus__uPlus_param_u32_i8:
.LLFB572:
	save	%sp, -96, %sp
.LLCFI572:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+68], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE572:
	.size	Plus__uPlus_param_u32_i8, .-Plus__uPlus_param_u32_i8
	.align 4
	.type	Plus__Plus_param_u32_i8, #function
	.proc	04
Plus__Plus_param_u32_i8:
.LLFB573:
	save	%sp, -96, %sp
.LLCFI573:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+68], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE573:
	.size	Plus__Plus_param_u32_i8, .-Plus__Plus_param_u32_i8
	.align 4
	.type	Plus__uPlus_var_u32_u32, #function
	.proc	04
Plus__uPlus_var_u32_u32:
.LLFB574:
	save	%sp, -96, %sp
.LLCFI574:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE574:
	.size	Plus__uPlus_var_u32_u32, .-Plus__uPlus_var_u32_u32
	.align 4
	.type	Plus__Plus_var_u32_u32, #function
	.proc	04
Plus__Plus_var_u32_u32:
.LLFB575:
	save	%sp, -96, %sp
.LLCFI575:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %o5
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	ld	[%g1+132], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE575:
	.size	Plus__Plus_var_u32_u32, .-Plus__Plus_var_u32_u32
	.align 4
	.type	Plus__uPlus_param_u32_u32, #function
	.proc	04
Plus__uPlus_param_u32_u32:
.LLFB576:
	save	%sp, -96, %sp
.LLCFI576:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE576:
	.size	Plus__uPlus_param_u32_u32, .-Plus__uPlus_param_u32_u32
	.align 4
	.type	Plus__Plus_param_u32_u32, #function
	.proc	04
Plus__Plus_param_u32_u32:
.LLFB577:
	save	%sp, -96, %sp
.LLCFI577:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	add	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE577:
	.size	Plus__Plus_param_u32_u32, .-Plus__Plus_param_u32_u32
	.align 4
	.type	Plus__Plus_var_f32_f32, #function
	.proc	06
Plus__Plus_var_f32_f32:
.LLFB578:
	save	%sp, -96, %sp
.LLCFI578:
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	add	%g1, 136, %g1
	ld	[%g1], %f8
	fadds	%f8, %f8, %f8
	fmovs	%f8, %f0
	return	%i7+8
	 nop
.LLFE578:
	.size	Plus__Plus_var_f32_f32, .-Plus__Plus_var_f32_f32
	.align 4
	.type	Plus__Plus_param_f32_f32, #function
	.proc	06
Plus__Plus_param_f32_f32:
.LLFB579:
	save	%sp, -96, %sp
.LLCFI579:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %f9
	ld	[%fp+72], %f8
	fadds	%f9, %f8, %f8
	fmovs	%f8, %f0
	return	%i7+8
	 nop
.LLFE579:
	.size	Plus__Plus_param_f32_f32, .-Plus__Plus_param_f32_f32
	.align 4
	.global Plus_M3
	.type	Plus_M3, #function
	.proc	0120
Plus_M3:
.LLFB580:
	save	%sp, -96, %sp
.LLCFI580:
	st	%i0, [%fp+68]
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE580:
	.size	Plus_M3, .-Plus_M3
	.align 4
	.type	_m3_fault, #function
	.proc	020
_m3_fault:
.LLFB581:
	save	%sp, -96, %sp
.LLCFI581:
	st	%i0, [%fp+68]
	sethi	%hi(MM_Plus), %g1
	or	%g1, %lo(MM_Plus), %o0
	ld	[%fp+68], %o1
	call	RTHooks__ReportFault, 0
	 nop
	return	%i7+8
	 nop
.LLFE581:
	.size	_m3_fault, .-_m3_fault
	.section	".data"
	.align 8
	.type	L_1, #object
	.size	L_1, 14544
L_1:
	.ascii	"Plus_M3"
	.skip 1
	.ascii	"Plus_param_f32_f32"
	.skip 1
	.ascii	"Plus_var_f32_f32"
	.skip 1
	.ascii	"Plus_param_u32_u32"
	.skip 1
	.ascii	"uPlus_param_u32_u32"
	.skip 1
	.ascii	"Plus_var_u32_u32"
	.skip 1
	.ascii	"uPlus_var_u32_u32"
	.skip 1
	.ascii	"Plus_param_u32_i8"
	.skip 1
	.ascii	"uPlus_param_u32_i8"
	.skip 1
	.ascii	"Plus_var_u32_i8"
	.skip 1
	.ascii	"uPlus_var_u32_i8"
	.skip 1
	.ascii	"Plus_param_u32_L"
	.skip 1
	.ascii	"uPlus_param_u32_L"
	.skip 1
	.ascii	"Plus_var_u32_L"
	.skip 1
	.ascii	"uPlus_var_u32_L"
	.skip 1
	.ascii	"Plus_param_u32_u8"
	.skip 1
	.ascii	"uPlus_param_u32_u8"
	.skip 1
	.ascii	"Plus_var_u32_u8"
	.skip 1
	.ascii	"uPlus_var_u32_u8"
	.skip 1
	.ascii	"Plus_param_u32_C"
	.skip 1
	.ascii	"uPlus_param_u32_C"
	.skip 1
	.ascii	"Plus_var_u32_C"
	.skip 1
	.ascii	"uPlus_var_u32_C"
	.skip 1
	.ascii	"Plus_param_u32_i64"
	.skip 1
	.ascii	"uPlus_param_u32_i64"
	.skip 1
	.ascii	"Plus_var_u32_i64"
	.skip 1
	.ascii	"uPlus_var_u32_i64"
	.skip 1
	.ascii	"Plus_param_u32_I"
	.skip 1
	.ascii	"uPlus_param_u32_I"
	.skip 1
	.ascii	"Plus_var_u32_I"
	.skip 1
	.ascii	"uPlus_var_u32_I"
	.skip 1
	.ascii	"Plus_param_u32_i16"
	.skip 1
	.ascii	"uPlus_param_u32_i16"
	.skip 1
	.ascii	"Plus_var_u32_i16"
	.skip 1
	.ascii	"uPlus_var_u32_i16"
	.skip 1
	.ascii	"Plus_param_u32_i32"
	.skip 1
	.ascii	"uPlus_param_u32_i32"
	.skip 1
	.ascii	"Plus_var_u32_i32"
	.skip 1
	.ascii	"uPlus_var_u32_i32"
	.skip 1
	.ascii	"Plus_param_u32_LC"
	.skip 1
	.ascii	"uPlus_param_u32_LC"
	.skip 1
	.ascii	"Plus_var_u32_LC"
	.skip 1
	.ascii	"uPlus_var_u32_LC"
	.skip 1
	.ascii	"Plus_param_u32_u64"
	.skip 1
	.ascii	"uPlus_param_u32_u64"
	.skip 1
	.ascii	"Plus_var_u32_u64"
	.skip 1
	.ascii	"uPlus_var_u32_u64"
	.skip 1
	.ascii	"Plus_param_u32_u16"
	.skip 1
	.ascii	"uPlus_param_u32_u16"
	.skip 1
	.ascii	"Plus_var_u32_u16"
	.skip 1
	.ascii	"uPlus_var_u32_u16"
	.skip 1
	.ascii	"Plus_param_i8_u32"
	.skip 1
	.ascii	"uPlus_param_i8_u32"
	.skip 1
	.ascii	"Plus_var_i8_u32"
	.skip 1
	.ascii	"uPlus_var_i8_u32"
	.skip 1
	.ascii	"Plus_param_i8_i8"
	.skip 1
	.ascii	"uPlus_param_i8_i8"
	.skip 1
	.ascii	"Plus_var_i8_i8"
	.skip 1
	.ascii	"uPlus_var_i8_i8"
	.skip 1
	.ascii	"Plus_param_i8_L"
	.skip 1
	.ascii	"uPlus_param_i8_L"
	.skip 1
	.ascii	"Plus_var_i8_L"
	.skip 1
	.ascii	"uPlus_var_i8_L"
	.skip 1
	.ascii	"Plus_param_i8_u8"
	.skip 1
	.ascii	"uPlus_param_i8_u8"
	.skip 1
	.ascii	"Plus_var_i8_u8"
	.skip 1
	.ascii	"uPlus_var_i8_u8"
	.skip 1
	.ascii	"Plus_param_i8_C"
	.skip 1
	.ascii	"uPlus_param_i8_C"
	.skip 1
	.ascii	"Plus_var_i8_C"
	.skip 1
	.ascii	"uPlus_var_i8_C"
	.skip 1
	.ascii	"Plus_param_i8_i64"
	.skip 1
	.ascii	"uPlus_param_i8_i64"
	.skip 1
	.ascii	"Plus_var_i8_i64"
	.skip 1
	.ascii	"uPlus_var_i8_i64"
	.skip 1
	.ascii	"Plus_param_i8_I"
	.skip 1
	.ascii	"uPlus_param_i8_I"
	.skip 1
	.ascii	"Plus_var_i8_I"
	.skip 1
	.ascii	"uPlus_var_i8_I"
	.skip 1
	.ascii	"Plus_param_i8_i16"
	.skip 1
	.ascii	"uPlus_param_i8_i16"
	.skip 1
	.ascii	"Plus_var_i8_i16"
	.skip 1
	.ascii	"uPlus_var_i8_i16"
	.skip 1
	.ascii	"Plus_param_i8_i32"
	.skip 1
	.ascii	"uPlus_param_i8_i32"
	.skip 1
	.ascii	"Plus_var_i8_i32"
	.skip 1
	.ascii	"uPlus_var_i8_i32"
	.skip 1
	.ascii	"Plus_param_i8_LC"
	.skip 1
	.ascii	"uPlus_param_i8_LC"
	.skip 1
	.ascii	"Plus_var_i8_LC"
	.skip 1
	.ascii	"uPlus_var_i8_LC"
	.skip 1
	.ascii	"Plus_param_i8_u64"
	.skip 1
	.ascii	"uPlus_param_i8_u64"
	.skip 1
	.ascii	"Plus_var_i8_u64"
	.skip 1
	.ascii	"uPlus_var_i8_u64"
	.skip 1
	.ascii	"Plus_param_i8_u16"
	.skip 1
	.ascii	"uPlus_param_i8_u16"
	.skip 1
	.ascii	"Plus_var_i8_u16"
	.skip 1
	.ascii	"uPlus_var_i8_u16"
	.skip 1
	.ascii	"Plus_param_L_u32"
	.skip 1
	.ascii	"uPlus_param_L_u32"
	.skip 1
	.ascii	"Plus_var_L_u32"
	.skip 1
	.ascii	"uPlus_var_L_u32"
	.skip 1
	.ascii	"Plus_param_L_i8"
	.skip 1
	.ascii	"uPlus_param_L_i8"
	.skip 1
	.ascii	"Plus_var_L_i8"
	.skip 1
	.ascii	"uPlus_var_L_i8"
	.skip 1
	.ascii	"Plus_param_L_L"
	.skip 1
	.ascii	"uPlus_param_L_L"
	.skip 1
	.ascii	"Plus_var_L_L"
	.skip 1
	.ascii	"uPlus_var_L_L"
	.skip 1
	.ascii	"Plus_param_L_u8"
	.skip 1
	.ascii	"uPlus_param_L_u8"
	.skip 1
	.ascii	"Plus_var_L_u8"
	.skip 1
	.ascii	"uPlus_var_L_u8"
	.skip 1
	.ascii	"Plus_param_L_C"
	.skip 1
	.ascii	"uPlus_param_L_C"
	.skip 1
	.ascii	"Plus_var_L_C"
	.skip 1
	.ascii	"uPlus_var_L_C"
	.skip 1
	.ascii	"Plus_param_L_i64"
	.skip 1
	.ascii	"uPlus_param_L_i64"
	.skip 1
	.ascii	"Plus_var_L_i64"
	.skip 1
	.ascii	"uPlus_var_L_i64"
	.skip 1
	.ascii	"Plus_param_L_I"
	.skip 1
	.ascii	"uPlus_param_L_I"
	.skip 1
	.ascii	"Plus_var_L_I"
	.skip 1
	.ascii	"uPlus_var_L_I"
	.skip 1
	.ascii	"Plus_param_L_i16"
	.skip 1
	.ascii	"uPlus_param_L_i16"
	.skip 1
	.ascii	"Plus_var_L_i16"
	.skip 1
	.ascii	"uPlus_var_L_i16"
	.skip 1
	.ascii	"Plus_param_L_i32"
	.skip 1
	.ascii	"uPlus_param_L_i32"
	.skip 1
	.ascii	"Plus_var_L_i32"
	.skip 1
	.ascii	"uPlus_var_L_i32"
	.skip 1
	.ascii	"Plus_param_L_LC"
	.skip 1
	.ascii	"uPlus_param_L_LC"
	.skip 1
	.ascii	"Plus_var_L_LC"
	.skip 1
	.ascii	"uPlus_var_L_LC"
	.skip 1
	.ascii	"Plus_param_L_u64"
	.skip 1
	.ascii	"uPlus_param_L_u64"
	.skip 1
	.ascii	"Plus_var_L_u64"
	.skip 1
	.ascii	"uPlus_var_L_u64"
	.skip 1
	.ascii	"Plus_param_L_u16"
	.skip 1
	.ascii	"uPlus_param_L_u16"
	.skip 1
	.ascii	"Plus_var_L_u16"
	.skip 1
	.ascii	"uPlus_var_L_u16"
	.skip 1
	.ascii	"Plus_param_u8_u32"
	.skip 1
	.ascii	"uPlus_param_u8_u32"
	.skip 1
	.ascii	"Plus_var_u8_u32"
	.skip 1
	.ascii	"uPlus_var_u8_u32"
	.skip 1
	.ascii	"Plus_param_u8_i8"
	.skip 1
	.ascii	"uPlus_param_u8_i8"
	.skip 1
	.ascii	"Plus_var_u8_i8"
	.skip 1
	.ascii	"uPlus_var_u8_i8"
	.skip 1
	.ascii	"Plus_param_u8_L"
	.skip 1
	.ascii	"uPlus_param_u8_L"
	.skip 1
	.ascii	"Plus_var_u8_L"
	.skip 1
	.ascii	"uPlus_var_u8_L"
	.skip 1
	.ascii	"Plus_param_u8_u8"
	.skip 1
	.ascii	"uPlus_param_u8_u8"
	.skip 1
	.ascii	"Plus_var_u8_u8"
	.skip 1
	.ascii	"uPlus_var_u8_u8"
	.skip 1
	.ascii	"Plus_param_u8_C"
	.skip 1
	.ascii	"uPlus_param_u8_C"
	.skip 1
	.ascii	"Plus_var_u8_C"
	.skip 1
	.ascii	"uPlus_var_u8_C"
	.skip 1
	.ascii	"Plus_param_u8_i64"
	.skip 1
	.ascii	"uPlus_param_u8_i64"
	.skip 1
	.ascii	"Plus_var_u8_i64"
	.skip 1
	.ascii	"uPlus_var_u8_i64"
	.skip 1
	.ascii	"Plus_param_u8_I"
	.skip 1
	.ascii	"uPlus_param_u8_I"
	.skip 1
	.ascii	"Plus_var_u8_I"
	.skip 1
	.ascii	"uPlus_var_u8_I"
	.skip 1
	.ascii	"Plus_param_u8_i16"
	.skip 1
	.ascii	"uPlus_param_u8_i16"
	.skip 1
	.ascii	"Plus_var_u8_i16"
	.skip 1
	.ascii	"uPlus_var_u8_i16"
	.skip 1
	.ascii	"Plus_param_u8_i32"
	.skip 1
	.ascii	"uPlus_param_u8_i32"
	.skip 1
	.ascii	"Plus_var_u8_i32"
	.skip 1
	.ascii	"uPlus_var_u8_i32"
	.skip 1
	.ascii	"Plus_param_u8_LC"
	.skip 1
	.ascii	"uPlus_param_u8_LC"
	.skip 1
	.ascii	"Plus_var_u8_LC"
	.skip 1
	.ascii	"uPlus_var_u8_LC"
	.skip 1
	.ascii	"Plus_param_u8_u64"
	.skip 1
	.ascii	"uPlus_param_u8_u64"
	.skip 1
	.ascii	"Plus_var_u8_u64"
	.skip 1
	.ascii	"uPlus_var_u8_u64"
	.skip 1
	.ascii	"Plus_param_u8_u16"
	.skip 1
	.ascii	"uPlus_param_u8_u16"
	.skip 1
	.ascii	"Plus_var_u8_u16"
	.skip 1
	.ascii	"uPlus_var_u8_u16"
	.skip 1
	.ascii	"Plus_param_f64_f64"
	.skip 1
	.ascii	"Plus_var_f64_f64"
	.skip 1
	.ascii	"Plus_param_C_u32"
	.skip 1
	.ascii	"uPlus_param_C_u32"
	.skip 1
	.ascii	"Plus_var_C_u32"
	.skip 1
	.ascii	"uPlus_var_C_u32"
	.skip 1
	.ascii	"Plus_param_C_i8"
	.skip 1
	.ascii	"uPlus_param_C_i8"
	.skip 1
	.ascii	"Plus_var_C_i8"
	.skip 1
	.ascii	"uPlus_var_C_i8"
	.skip 1
	.ascii	"Plus_param_C_L"
	.skip 1
	.ascii	"uPlus_param_C_L"
	.skip 1
	.ascii	"Plus_var_C_L"
	.skip 1
	.ascii	"uPlus_var_C_L"
	.skip 1
	.ascii	"Plus_param_C_u8"
	.skip 1
	.ascii	"uPlus_param_C_u8"
	.skip 1
	.ascii	"Plus_var_C_u8"
	.skip 1
	.ascii	"uPlus_var_C_u8"
	.skip 1
	.ascii	"Plus_param_C_C"
	.skip 1
	.ascii	"uPlus_param_C_C"
	.skip 1
	.ascii	"Plus_var_C_C"
	.skip 1
	.ascii	"uPlus_var_C_C"
	.skip 1
	.ascii	"Plus_param_C_i64"
	.skip 1
	.ascii	"uPlus_param_C_i64"
	.skip 1
	.ascii	"Plus_var_C_i64"
	.skip 1
	.ascii	"uPlus_var_C_i64"
	.skip 1
	.ascii	"Plus_param_C_I"
	.skip 1
	.ascii	"uPlus_param_C_I"
	.skip 1
	.ascii	"Plus_var_C_I"
	.skip 1
	.ascii	"uPlus_var_C_I"
	.skip 1
	.ascii	"Plus_param_C_i16"
	.skip 1
	.ascii	"uPlus_param_C_i16"
	.skip 1
	.ascii	"Plus_var_C_i16"
	.skip 1
	.ascii	"uPlus_var_C_i16"
	.skip 1
	.ascii	"Plus_param_C_i32"
	.skip 1
	.ascii	"uPlus_param_C_i32"
	.skip 1
	.ascii	"Plus_var_C_i32"
	.skip 1
	.ascii	"uPlus_var_C_i32"
	.skip 1
	.ascii	"Plus_param_C_LC"
	.skip 1
	.ascii	"uPlus_param_C_LC"
	.skip 1
	.ascii	"Plus_var_C_LC"
	.skip 1
	.ascii	"uPlus_var_C_LC"
	.skip 1
	.ascii	"Plus_param_C_u64"
	.skip 1
	.ascii	"uPlus_param_C_u64"
	.skip 1
	.ascii	"Plus_var_C_u64"
	.skip 1
	.ascii	"uPlus_var_C_u64"
	.skip 1
	.ascii	"Plus_param_C_u16"
	.skip 1
	.ascii	"uPlus_param_C_u16"
	.skip 1
	.ascii	"Plus_var_C_u16"
	.skip 1
	.ascii	"uPlus_var_C_u16"
	.skip 1
	.ascii	"Plus_param_i64_u32"
	.skip 1
	.ascii	"uPlus_param_i64_u32"
	.skip 1
	.ascii	"Plus_var_i64_u32"
	.skip 1
	.ascii	"uPlus_var_i64_u32"
	.skip 1
	.ascii	"Plus_param_i64_i8"
	.skip 1
	.ascii	"uPlus_param_i64_i8"
	.skip 1
	.ascii	"Plus_var_i64_i8"
	.skip 1
	.ascii	"uPlus_var_i64_i8"
	.skip 1
	.ascii	"Plus_param_i64_L"
	.skip 1
	.ascii	"uPlus_param_i64_L"
	.skip 1
	.ascii	"Plus_var_i64_L"
	.skip 1
	.ascii	"uPlus_var_i64_L"
	.skip 1
	.ascii	"Plus_param_i64_u8"
	.skip 1
	.ascii	"uPlus_param_i64_u8"
	.skip 1
	.ascii	"Plus_var_i64_u8"
	.skip 1
	.ascii	"uPlus_var_i64_u8"
	.skip 1
	.ascii	"Plus_param_i64_C"
	.skip 1
	.ascii	"uPlus_param_i64_C"
	.skip 1
	.ascii	"Plus_var_i64_C"
	.skip 1
	.ascii	"uPlus_var_i64_C"
	.skip 1
	.ascii	"Plus_param_i64_i64"
	.skip 1
	.ascii	"uPlus_param_i64_i64"
	.skip 1
	.ascii	"Plus_var_i64_i64"
	.skip 1
	.ascii	"uPlus_var_i64_i64"
	.skip 1
	.ascii	"Plus_param_i64_I"
	.skip 1
	.ascii	"uPlus_param_i64_I"
	.skip 1
	.ascii	"Plus_var_i64_I"
	.skip 1
	.ascii	"uPlus_var_i64_I"
	.skip 1
	.ascii	"Plus_param_i64_i16"
	.skip 1
	.ascii	"uPlus_param_i64_i16"
	.skip 1
	.ascii	"Plus_var_i64_i16"
	.skip 1
	.ascii	"uPlus_var_i64_i16"
	.skip 1
	.ascii	"Plus_param_i64_i32"
	.skip 1
	.ascii	"uPlus_param_i64_i32"
	.skip 1
	.ascii	"Plus_var_i64_i32"
	.skip 1
	.ascii	"uPlus_var_i64_i32"
	.skip 1
	.ascii	"Plus_param_i64_LC"
	.skip 1
	.ascii	"uPlus_param_i64_LC"
	.skip 1
	.ascii	"Plus_var_i64_LC"
	.skip 1
	.ascii	"uPlus_var_i64_LC"
	.skip 1
	.ascii	"Plus_param_i64_u64"
	.skip 1
	.ascii	"uPlus_param_i64_u64"
	.skip 1
	.ascii	"Plus_var_i64_u64"
	.skip 1
	.ascii	"uPlus_var_i64_u64"
	.skip 1
	.ascii	"Plus_param_i64_u16"
	.skip 1
	.ascii	"uPlus_param_i64_u16"
	.skip 1
	.ascii	"Plus_var_i64_u16"
	.skip 1
	.ascii	"uPlus_var_i64_u16"
	.skip 1
	.ascii	"Plus_param_I_u32"
	.skip 1
	.ascii	"uPlus_param_I_u32"
	.skip 1
	.ascii	"Plus_var_I_u32"
	.skip 1
	.ascii	"uPlus_var_I_u32"
	.skip 1
	.ascii	"Plus_param_I_i8"
	.skip 1
	.ascii	"uPlus_param_I_i8"
	.skip 1
	.ascii	"Plus_var_I_i8"
	.skip 1
	.ascii	"uPlus_var_I_i8"
	.skip 1
	.ascii	"Plus_param_I_L"
	.skip 1
	.ascii	"uPlus_param_I_L"
	.skip 1
	.ascii	"Plus_var_I_L"
	.skip 1
	.ascii	"uPlus_var_I_L"
	.skip 1
	.ascii	"Plus_param_I_u8"
	.skip 1
	.ascii	"uPlus_param_I_u8"
	.skip 1
	.ascii	"Plus_var_I_u8"
	.skip 1
	.ascii	"uPlus_var_I_u8"
	.skip 1
	.ascii	"Plus_param_I_C"
	.skip 1
	.ascii	"uPlus_param_I_C"
	.skip 1
	.ascii	"Plus_var_I_C"
	.skip 1
	.ascii	"uPlus_var_I_C"
	.skip 1
	.ascii	"Plus_param_I_i64"
	.skip 1
	.ascii	"uPlus_param_I_i64"
	.skip 1
	.ascii	"Plus_var_I_i64"
	.skip 1
	.ascii	"uPlus_var_I_i64"
	.skip 1
	.ascii	"Plus_param_I_I"
	.skip 1
	.ascii	"uPlus_param_I_I"
	.skip 1
	.ascii	"Plus_var_I_I"
	.skip 1
	.ascii	"uPlus_var_I_I"
	.skip 1
	.ascii	"Plus_param_I_i16"
	.skip 1
	.ascii	"uPlus_param_I_i16"
	.skip 1
	.ascii	"Plus_var_I_i16"
	.skip 1
	.ascii	"uPlus_var_I_i16"
	.skip 1
	.ascii	"Plus_param_I_i32"
	.skip 1
	.ascii	"uPlus_param_I_i32"
	.skip 1
	.ascii	"Plus_var_I_i32"
	.skip 1
	.ascii	"uPlus_var_I_i32"
	.skip 1
	.ascii	"Plus_param_I_LC"
	.skip 1
	.ascii	"uPlus_param_I_LC"
	.skip 1
	.ascii	"Plus_var_I_LC"
	.skip 1
	.ascii	"uPlus_var_I_LC"
	.skip 1
	.ascii	"Plus_param_I_u64"
	.skip 1
	.ascii	"uPlus_param_I_u64"
	.skip 1
	.ascii	"Plus_var_I_u64"
	.skip 1
	.ascii	"uPlus_var_I_u64"
	.skip 1
	.ascii	"Plus_param_I_u16"
	.skip 1
	.ascii	"uPlus_param_I_u16"
	.skip 1
	.ascii	"Plus_var_I_u16"
	.skip 1
	.ascii	"uPlus_var_I_u16"
	.skip 1
	.ascii	"Plus_param_i16_u32"
	.skip 1
	.ascii	"uPlus_param_i16_u32"
	.skip 1
	.ascii	"Plus_var_i16_u32"
	.skip 1
	.ascii	"uPlus_var_i16_u32"
	.skip 1
	.ascii	"Plus_param_i16_i8"
	.skip 1
	.ascii	"uPlus_param_i16_i8"
	.skip 1
	.ascii	"Plus_var_i16_i8"
	.skip 1
	.ascii	"uPlus_var_i16_i8"
	.skip 1
	.ascii	"Plus_param_i16_L"
	.skip 1
	.ascii	"uPlus_param_i16_L"
	.skip 1
	.ascii	"Plus_var_i16_L"
	.skip 1
	.ascii	"uPlus_var_i16_L"
	.skip 1
	.ascii	"Plus_param_i16_u8"
	.skip 1
	.ascii	"uPlus_param_i16_u8"
	.skip 1
	.ascii	"Plus_var_i16_u8"
	.skip 1
	.ascii	"uPlus_var_i16_u8"
	.skip 1
	.ascii	"Plus_param_i16_C"
	.skip 1
	.ascii	"uPlus_param_i16_C"
	.skip 1
	.ascii	"Plus_var_i16_C"
	.skip 1
	.ascii	"uPlus_var_i16_C"
	.skip 1
	.ascii	"Plus_param_i16_i64"
	.skip 1
	.ascii	"uPlus_param_i16_i64"
	.skip 1
	.ascii	"Plus_var_i16_i64"
	.skip 1
	.ascii	"uPlus_var_i16_i64"
	.skip 1
	.ascii	"Plus_param_i16_I"
	.skip 1
	.ascii	"uPlus_param_i16_I"
	.skip 1
	.ascii	"Plus_var_i16_I"
	.skip 1
	.ascii	"uPlus_var_i16_I"
	.skip 1
	.ascii	"Plus_param_i16_i16"
	.skip 1
	.ascii	"uPlus_param_i16_i16"
	.skip 1
	.ascii	"Plus_var_i16_i16"
	.skip 1
	.ascii	"uPlus_var_i16_i16"
	.skip 1
	.ascii	"Plus_param_i16_i32"
	.skip 1
	.ascii	"uPlus_param_i16_i32"
	.skip 1
	.ascii	"Plus_var_i16_i32"
	.skip 1
	.ascii	"uPlus_var_i16_i32"
	.skip 1
	.ascii	"Plus_param_i16_LC"
	.skip 1
	.ascii	"uPlus_param_i16_LC"
	.skip 1
	.ascii	"Plus_var_i16_LC"
	.skip 1
	.ascii	"uPlus_var_i16_LC"
	.skip 1
	.ascii	"Plus_param_i16_u64"
	.skip 1
	.ascii	"uPlus_param_i16_u64"
	.skip 1
	.ascii	"Plus_var_i16_u64"
	.skip 1
	.ascii	"uPlus_var_i16_u64"
	.skip 1
	.ascii	"Plus_param_i16_u16"
	.skip 1
	.ascii	"uPlus_param_i16_u16"
	.skip 1
	.ascii	"Plus_var_i16_u16"
	.skip 1
	.ascii	"uPlus_var_i16_u16"
	.skip 1
	.ascii	"Plus_param_i32_u32"
	.skip 1
	.ascii	"uPlus_param_i32_u32"
	.skip 1
	.ascii	"Plus_var_i32_u32"
	.skip 1
	.ascii	"uPlus_var_i32_u32"
	.skip 1
	.ascii	"Plus_param_i32_i8"
	.skip 1
	.ascii	"uPlus_param_i32_i8"
	.skip 1
	.ascii	"Plus_var_i32_i8"
	.skip 1
	.ascii	"uPlus_var_i32_i8"
	.skip 1
	.ascii	"Plus_param_i32_L"
	.skip 1
	.ascii	"uPlus_param_i32_L"
	.skip 1
	.ascii	"Plus_var_i32_L"
	.skip 1
	.ascii	"uPlus_var_i32_L"
	.skip 1
	.ascii	"Plus_param_i32_u8"
	.skip 1
	.ascii	"uPlus_param_i32_u8"
	.skip 1
	.ascii	"Plus_var_i32_u8"
	.skip 1
	.ascii	"uPlus_var_i32_u8"
	.skip 1
	.ascii	"Plus_param_i32_C"
	.skip 1
	.ascii	"uPlus_param_i32_C"
	.skip 1
	.ascii	"Plus_var_i32_C"
	.skip 1
	.ascii	"uPlus_var_i32_C"
	.skip 1
	.ascii	"Plus_param_i32_i64"
	.skip 1
	.ascii	"uPlus_param_i32_i64"
	.skip 1
	.ascii	"Plus_var_i32_i64"
	.skip 1
	.ascii	"uPlus_var_i32_i64"
	.skip 1
	.ascii	"Plus_param_i32_I"
	.skip 1
	.ascii	"uPlus_param_i32_I"
	.skip 1
	.ascii	"Plus_var_i32_I"
	.skip 1
	.ascii	"uPlus_var_i32_I"
	.skip 1
	.ascii	"Plus_param_i32_i16"
	.skip 1
	.ascii	"uPlus_param_i32_i16"
	.skip 1
	.ascii	"Plus_var_i32_i16"
	.skip 1
	.ascii	"uPlus_var_i32_i16"
	.skip 1
	.ascii	"Plus_param_i32_i32"
	.skip 1
	.ascii	"uPlus_param_i32_i32"
	.skip 1
	.ascii	"Plus_var_i32_i32"
	.skip 1
	.ascii	"uPlus_var_i32_i32"
	.skip 1
	.ascii	"Plus_param_i32_LC"
	.skip 1
	.ascii	"uPlus_param_i32_LC"
	.skip 1
	.ascii	"Plus_var_i32_LC"
	.skip 1
	.ascii	"uPlus_var_i32_LC"
	.skip 1
	.ascii	"Plus_param_i32_u64"
	.skip 1
	.ascii	"uPlus_param_i32_u64"
	.skip 1
	.ascii	"Plus_var_i32_u64"
	.skip 1
	.ascii	"uPlus_var_i32_u64"
	.skip 1
	.ascii	"Plus_param_i32_u16"
	.skip 1
	.ascii	"uPlus_param_i32_u16"
	.skip 1
	.ascii	"Plus_var_i32_u16"
	.skip 1
	.ascii	"uPlus_var_i32_u16"
	.skip 1
	.ascii	"Plus_param_LC_u32"
	.skip 1
	.ascii	"uPlus_param_LC_u32"
	.skip 1
	.ascii	"Plus_var_LC_u32"
	.skip 1
	.ascii	"uPlus_var_LC_u32"
	.skip 1
	.ascii	"Plus_param_LC_i8"
	.skip 1
	.ascii	"uPlus_param_LC_i8"
	.skip 1
	.ascii	"Plus_var_LC_i8"
	.skip 1
	.ascii	"uPlus_var_LC_i8"
	.skip 1
	.ascii	"Plus_param_LC_L"
	.skip 1
	.ascii	"uPlus_param_LC_L"
	.skip 1
	.ascii	"Plus_var_LC_L"
	.skip 1
	.ascii	"uPlus_var_LC_L"
	.skip 1
	.ascii	"Plus_param_LC_u8"
	.skip 1
	.ascii	"uPlus_param_LC_u8"
	.skip 1
	.ascii	"Plus_var_LC_u8"
	.skip 1
	.ascii	"uPlus_var_LC_u8"
	.skip 1
	.ascii	"Plus_param_LC_C"
	.skip 1
	.ascii	"uPlus_param_LC_C"
	.skip 1
	.ascii	"Plus_var_LC_C"
	.skip 1
	.ascii	"uPlus_var_LC_C"
	.skip 1
	.ascii	"Plus_param_LC_i64"
	.skip 1
	.ascii	"uPlus_param_LC_i64"
	.skip 1
	.ascii	"Plus_var_LC_i64"
	.skip 1
	.ascii	"uPlus_var_LC_i64"
	.skip 1
	.ascii	"Plus_param_LC_I"
	.skip 1
	.ascii	"uPlus_param_LC_I"
	.skip 1
	.ascii	"Plus_var_LC_I"
	.skip 1
	.ascii	"uPlus_var_LC_I"
	.skip 1
	.ascii	"Plus_param_LC_i16"
	.skip 1
	.ascii	"uPlus_param_LC_i16"
	.skip 1
	.ascii	"Plus_var_LC_i16"
	.skip 1
	.ascii	"uPlus_var_LC_i16"
	.skip 1
	.ascii	"Plus_param_LC_i32"
	.skip 1
	.ascii	"uPlus_param_LC_i32"
	.skip 1
	.ascii	"Plus_var_LC_i32"
	.skip 1
	.ascii	"uPlus_var_LC_i32"
	.skip 1
	.ascii	"Plus_param_LC_LC"
	.skip 1
	.ascii	"uPlus_param_LC_LC"
	.skip 1
	.ascii	"Plus_var_LC_LC"
	.skip 1
	.ascii	"uPlus_var_LC_LC"
	.skip 1
	.ascii	"Plus_param_LC_u64"
	.skip 1
	.ascii	"uPlus_param_LC_u64"
	.skip 1
	.ascii	"Plus_var_LC_u64"
	.skip 1
	.ascii	"uPlus_var_LC_u64"
	.skip 1
	.ascii	"Plus_param_LC_u16"
	.skip 1
	.ascii	"uPlus_param_LC_u16"
	.skip 1
	.ascii	"Plus_var_LC_u16"
	.skip 1
	.ascii	"uPlus_var_LC_u16"
	.skip 1
	.ascii	"Plus_param_u64_u32"
	.skip 1
	.ascii	"uPlus_param_u64_u32"
	.skip 1
	.ascii	"Plus_var_u64_u32"
	.skip 1
	.ascii	"uPlus_var_u64_u32"
	.skip 1
	.ascii	"Plus_param_u64_i8"
	.skip 1
	.ascii	"uPlus_param_u64_i8"
	.skip 1
	.ascii	"Plus_var_u64_i8"
	.skip 1
	.ascii	"uPlus_var_u64_i8"
	.skip 1
	.ascii	"Plus_param_u64_L"
	.skip 1
	.ascii	"uPlus_param_u64_L"
	.skip 1
	.ascii	"Plus_var_u64_L"
	.skip 1
	.ascii	"uPlus_var_u64_L"
	.skip 1
	.ascii	"Plus_param_u64_u8"
	.skip 1
	.ascii	"uPlus_param_u64_u8"
	.skip 1
	.ascii	"Plus_var_u64_u8"
	.skip 1
	.ascii	"uPlus_var_u64_u8"
	.skip 1
	.ascii	"Plus_param_u64_C"
	.skip 1
	.ascii	"uPlus_param_u64_C"
	.skip 1
	.ascii	"Plus_var_u64_C"
	.skip 1
	.ascii	"uPlus_var_u64_C"
	.skip 1
	.ascii	"Plus_param_u64_i64"
	.skip 1
	.ascii	"uPlus_param_u64_i64"
	.skip 1
	.ascii	"Plus_var_u64_i64"
	.skip 1
	.ascii	"uPlus_var_u64_i64"
	.skip 1
	.ascii	"Plus_param_u64_I"
	.skip 1
	.ascii	"uPlus_param_u64_I"
	.skip 1
	.ascii	"Plus_var_u64_I"
	.skip 1
	.ascii	"uPlus_var_u64_I"
	.skip 1
	.ascii	"Plus_param_u64_i16"
	.skip 1
	.ascii	"uPlus_param_u64_i16"
	.skip 1
	.ascii	"Plus_var_u64_i16"
	.skip 1
	.ascii	"uPlus_var_u64_i16"
	.skip 1
	.ascii	"Plus_param_u64_i32"
	.skip 1
	.ascii	"uPlus_param_u64_i32"
	.skip 1
	.ascii	"Plus_var_u64_i32"
	.skip 1
	.ascii	"uPlus_var_u64_i32"
	.skip 1
	.ascii	"Plus_param_u64_LC"
	.skip 1
	.ascii	"uPlus_param_u64_LC"
	.skip 1
	.ascii	"Plus_var_u64_LC"
	.skip 1
	.ascii	"uPlus_var_u64_LC"
	.skip 1
	.ascii	"Plus_param_u64_u64"
	.skip 1
	.ascii	"uPlus_param_u64_u64"
	.skip 1
	.ascii	"Plus_var_u64_u64"
	.skip 1
	.ascii	"uPlus_var_u64_u64"
	.skip 1
	.ascii	"Plus_param_u64_u16"
	.skip 1
	.ascii	"uPlus_param_u64_u16"
	.skip 1
	.ascii	"Plus_var_u64_u16"
	.skip 1
	.ascii	"uPlus_var_u64_u16"
	.skip 1
	.ascii	"Plus_param_u16_u32"
	.skip 1
	.ascii	"uPlus_param_u16_u32"
	.skip 1
	.ascii	"Plus_var_u16_u32"
	.skip 1
	.ascii	"uPlus_var_u16_u32"
	.skip 1
	.ascii	"Plus_param_u16_i8"
	.skip 1
	.ascii	"uPlus_param_u16_i8"
	.skip 1
	.ascii	"Plus_var_u16_i8"
	.skip 1
	.ascii	"uPlus_var_u16_i8"
	.skip 1
	.ascii	"Plus_param_u16_L"
	.skip 1
	.ascii	"uPlus_param_u16_L"
	.skip 1
	.ascii	"Plus_var_u16_L"
	.skip 1
	.ascii	"uPlus_var_u16_L"
	.skip 1
	.ascii	"Plus_param_u16_u8"
	.skip 1
	.ascii	"uPlus_param_u16_u8"
	.skip 1
	.ascii	"Plus_var_u16_u8"
	.skip 1
	.ascii	"uPlus_var_u16_u8"
	.skip 1
	.ascii	"Plus_param_u16_C"
	.skip 1
	.ascii	"uPlus_param_u16_C"
	.skip 1
	.ascii	"Plus_var_u16_C"
	.skip 1
	.ascii	"uPlus_var_u16_C"
	.skip 1
	.ascii	"Plus_param_u16_i64"
	.skip 1
	.ascii	"uPlus_param_u16_i64"
	.skip 1
	.ascii	"Plus_var_u16_i64"
	.skip 1
	.ascii	"uPlus_var_u16_i64"
	.skip 1
	.ascii	"Plus_param_u16_I"
	.skip 1
	.ascii	"uPlus_param_u16_I"
	.skip 1
	.ascii	"Plus_var_u16_I"
	.skip 1
	.ascii	"uPlus_var_u16_I"
	.skip 1
	.ascii	"Plus_param_u16_i16"
	.skip 1
	.ascii	"uPlus_param_u16_i16"
	.skip 1
	.ascii	"Plus_var_u16_i16"
	.skip 1
	.ascii	"uPlus_var_u16_i16"
	.skip 1
	.ascii	"Plus_param_u16_i32"
	.skip 1
	.ascii	"uPlus_param_u16_i32"
	.skip 1
	.ascii	"Plus_var_u16_i32"
	.skip 1
	.ascii	"uPlus_var_u16_i32"
	.skip 1
	.ascii	"Plus_param_u16_LC"
	.skip 1
	.ascii	"uPlus_param_u16_LC"
	.skip 1
	.ascii	"Plus_var_u16_LC"
	.skip 1
	.ascii	"uPlus_var_u16_LC"
	.skip 1
	.ascii	"Plus_param_u16_u64"
	.skip 1
	.ascii	"uPlus_param_u16_u64"
	.skip 1
	.ascii	"Plus_var_u16_u64"
	.skip 1
	.ascii	"uPlus_var_u16_u64"
	.skip 1
	.ascii	"Plus_param_u16_u16"
	.skip 1
	.ascii	"uPlus_param_u16_u16"
	.skip 1
	.ascii	"Plus_var_u16_u16"
	.skip 1
	.ascii	"uPlus_var_u16_u16"
	.skip 1
	.long	Plus_M3
	.long	L_1
	.long	Plus__Plus_param_f32_f32
	.long	L_1+8
	.long	Plus__Plus_var_f32_f32
	.long	L_1+27
	.long	Plus__Plus_param_u32_u32
	.long	L_1+44
	.long	Plus__uPlus_param_u32_u32
	.long	L_1+63
	.long	Plus__Plus_var_u32_u32
	.long	L_1+83
	.long	Plus__uPlus_var_u32_u32
	.long	L_1+100
	.long	Plus__Plus_param_u32_i8
	.long	L_1+118
	.long	Plus__uPlus_param_u32_i8
	.long	L_1+136
	.long	Plus__Plus_var_u32_i8
	.long	L_1+155
	.long	Plus__uPlus_var_u32_i8
	.long	L_1+171
	.long	Plus__Plus_param_u32_L
	.long	L_1+188
	.long	Plus__uPlus_param_u32_L
	.long	L_1+205
	.long	Plus__Plus_var_u32_L
	.long	L_1+223
	.long	Plus__uPlus_var_u32_L
	.long	L_1+238
	.long	Plus__Plus_param_u32_u8
	.long	L_1+254
	.long	Plus__uPlus_param_u32_u8
	.long	L_1+272
	.long	Plus__Plus_var_u32_u8
	.long	L_1+291
	.long	Plus__uPlus_var_u32_u8
	.long	L_1+307
	.long	Plus__Plus_param_u32_C
	.long	L_1+324
	.long	Plus__uPlus_param_u32_C
	.long	L_1+341
	.long	Plus__Plus_var_u32_C
	.long	L_1+359
	.long	Plus__uPlus_var_u32_C
	.long	L_1+374
	.long	Plus__Plus_param_u32_i64
	.long	L_1+390
	.long	Plus__uPlus_param_u32_i64
	.long	L_1+409
	.long	Plus__Plus_var_u32_i64
	.long	L_1+429
	.long	Plus__uPlus_var_u32_i64
	.long	L_1+446
	.long	Plus__Plus_param_u32_I
	.long	L_1+464
	.long	Plus__uPlus_param_u32_I
	.long	L_1+481
	.long	Plus__Plus_var_u32_I
	.long	L_1+499
	.long	Plus__uPlus_var_u32_I
	.long	L_1+514
	.long	Plus__Plus_param_u32_i16
	.long	L_1+530
	.long	Plus__uPlus_param_u32_i16
	.long	L_1+549
	.long	Plus__Plus_var_u32_i16
	.long	L_1+569
	.long	Plus__uPlus_var_u32_i16
	.long	L_1+586
	.long	Plus__Plus_param_u32_i32
	.long	L_1+604
	.long	Plus__uPlus_param_u32_i32
	.long	L_1+623
	.long	Plus__Plus_var_u32_i32
	.long	L_1+643
	.long	Plus__uPlus_var_u32_i32
	.long	L_1+660
	.long	Plus__Plus_param_u32_LC
	.long	L_1+678
	.long	Plus__uPlus_param_u32_LC
	.long	L_1+696
	.long	Plus__Plus_var_u32_LC
	.long	L_1+715
	.long	Plus__uPlus_var_u32_LC
	.long	L_1+731
	.long	Plus__Plus_param_u32_u64
	.long	L_1+748
	.long	Plus__uPlus_param_u32_u64
	.long	L_1+767
	.long	Plus__Plus_var_u32_u64
	.long	L_1+787
	.long	Plus__uPlus_var_u32_u64
	.long	L_1+804
	.long	Plus__Plus_param_u32_u16
	.long	L_1+822
	.long	Plus__uPlus_param_u32_u16
	.long	L_1+841
	.long	Plus__Plus_var_u32_u16
	.long	L_1+861
	.long	Plus__uPlus_var_u32_u16
	.long	L_1+878
	.long	Plus__Plus_param_i8_u32
	.long	L_1+896
	.long	Plus__uPlus_param_i8_u32
	.long	L_1+914
	.long	Plus__Plus_var_i8_u32
	.long	L_1+933
	.long	Plus__uPlus_var_i8_u32
	.long	L_1+949
	.long	Plus__Plus_param_i8_i8
	.long	L_1+966
	.long	Plus__uPlus_param_i8_i8
	.long	L_1+983
	.long	Plus__Plus_var_i8_i8
	.long	L_1+1001
	.long	Plus__uPlus_var_i8_i8
	.long	L_1+1016
	.long	Plus__Plus_param_i8_L
	.long	L_1+1032
	.long	Plus__uPlus_param_i8_L
	.long	L_1+1048
	.long	Plus__Plus_var_i8_L
	.long	L_1+1065
	.long	Plus__uPlus_var_i8_L
	.long	L_1+1079
	.long	Plus__Plus_param_i8_u8
	.long	L_1+1094
	.long	Plus__uPlus_param_i8_u8
	.long	L_1+1111
	.long	Plus__Plus_var_i8_u8
	.long	L_1+1129
	.long	Plus__uPlus_var_i8_u8
	.long	L_1+1144
	.long	Plus__Plus_param_i8_C
	.long	L_1+1160
	.long	Plus__uPlus_param_i8_C
	.long	L_1+1176
	.long	Plus__Plus_var_i8_C
	.long	L_1+1193
	.long	Plus__uPlus_var_i8_C
	.long	L_1+1207
	.long	Plus__Plus_param_i8_i64
	.long	L_1+1222
	.long	Plus__uPlus_param_i8_i64
	.long	L_1+1240
	.long	Plus__Plus_var_i8_i64
	.long	L_1+1259
	.long	Plus__uPlus_var_i8_i64
	.long	L_1+1275
	.long	Plus__Plus_param_i8_I
	.long	L_1+1292
	.long	Plus__uPlus_param_i8_I
	.long	L_1+1308
	.long	Plus__Plus_var_i8_I
	.long	L_1+1325
	.long	Plus__uPlus_var_i8_I
	.long	L_1+1339
	.long	Plus__Plus_param_i8_i16
	.long	L_1+1354
	.long	Plus__uPlus_param_i8_i16
	.long	L_1+1372
	.long	Plus__Plus_var_i8_i16
	.long	L_1+1391
	.long	Plus__uPlus_var_i8_i16
	.long	L_1+1407
	.long	Plus__Plus_param_i8_i32
	.long	L_1+1424
	.long	Plus__uPlus_param_i8_i32
	.long	L_1+1442
	.long	Plus__Plus_var_i8_i32
	.long	L_1+1461
	.long	Plus__uPlus_var_i8_i32
	.long	L_1+1477
	.long	Plus__Plus_param_i8_LC
	.long	L_1+1494
	.long	Plus__uPlus_param_i8_LC
	.long	L_1+1511
	.long	Plus__Plus_var_i8_LC
	.long	L_1+1529
	.long	Plus__uPlus_var_i8_LC
	.long	L_1+1544
	.long	Plus__Plus_param_i8_u64
	.long	L_1+1560
	.long	Plus__uPlus_param_i8_u64
	.long	L_1+1578
	.long	Plus__Plus_var_i8_u64
	.long	L_1+1597
	.long	Plus__uPlus_var_i8_u64
	.long	L_1+1613
	.long	Plus__Plus_param_i8_u16
	.long	L_1+1630
	.long	Plus__uPlus_param_i8_u16
	.long	L_1+1648
	.long	Plus__Plus_var_i8_u16
	.long	L_1+1667
	.long	Plus__uPlus_var_i8_u16
	.long	L_1+1683
	.long	Plus__Plus_param_L_u32
	.long	L_1+1700
	.long	Plus__uPlus_param_L_u32
	.long	L_1+1717
	.long	Plus__Plus_var_L_u32
	.long	L_1+1735
	.long	Plus__uPlus_var_L_u32
	.long	L_1+1750
	.long	Plus__Plus_param_L_i8
	.long	L_1+1766
	.long	Plus__uPlus_param_L_i8
	.long	L_1+1782
	.long	Plus__Plus_var_L_i8
	.long	L_1+1799
	.long	Plus__uPlus_var_L_i8
	.long	L_1+1813
	.long	Plus__Plus_param_L_L
	.long	L_1+1828
	.long	Plus__uPlus_param_L_L
	.long	L_1+1843
	.long	Plus__Plus_var_L_L
	.long	L_1+1859
	.long	Plus__uPlus_var_L_L
	.long	L_1+1872
	.long	Plus__Plus_param_L_u8
	.long	L_1+1886
	.long	Plus__uPlus_param_L_u8
	.long	L_1+1902
	.long	Plus__Plus_var_L_u8
	.long	L_1+1919
	.long	Plus__uPlus_var_L_u8
	.long	L_1+1933
	.long	Plus__Plus_param_L_C
	.long	L_1+1948
	.long	Plus__uPlus_param_L_C
	.long	L_1+1963
	.long	Plus__Plus_var_L_C
	.long	L_1+1979
	.long	Plus__uPlus_var_L_C
	.long	L_1+1992
	.long	Plus__Plus_param_L_i64
	.long	L_1+2006
	.long	Plus__uPlus_param_L_i64
	.long	L_1+2023
	.long	Plus__Plus_var_L_i64
	.long	L_1+2041
	.long	Plus__uPlus_var_L_i64
	.long	L_1+2056
	.long	Plus__Plus_param_L_I
	.long	L_1+2072
	.long	Plus__uPlus_param_L_I
	.long	L_1+2087
	.long	Plus__Plus_var_L_I
	.long	L_1+2103
	.long	Plus__uPlus_var_L_I
	.long	L_1+2116
	.long	Plus__Plus_param_L_i16
	.long	L_1+2130
	.long	Plus__uPlus_param_L_i16
	.long	L_1+2147
	.long	Plus__Plus_var_L_i16
	.long	L_1+2165
	.long	Plus__uPlus_var_L_i16
	.long	L_1+2180
	.long	Plus__Plus_param_L_i32
	.long	L_1+2196
	.long	Plus__uPlus_param_L_i32
	.long	L_1+2213
	.long	Plus__Plus_var_L_i32
	.long	L_1+2231
	.long	Plus__uPlus_var_L_i32
	.long	L_1+2246
	.long	Plus__Plus_param_L_LC
	.long	L_1+2262
	.long	Plus__uPlus_param_L_LC
	.long	L_1+2278
	.long	Plus__Plus_var_L_LC
	.long	L_1+2295
	.long	Plus__uPlus_var_L_LC
	.long	L_1+2309
	.long	Plus__Plus_param_L_u64
	.long	L_1+2324
	.long	Plus__uPlus_param_L_u64
	.long	L_1+2341
	.long	Plus__Plus_var_L_u64
	.long	L_1+2359
	.long	Plus__uPlus_var_L_u64
	.long	L_1+2374
	.long	Plus__Plus_param_L_u16
	.long	L_1+2390
	.long	Plus__uPlus_param_L_u16
	.long	L_1+2407
	.long	Plus__Plus_var_L_u16
	.long	L_1+2425
	.long	Plus__uPlus_var_L_u16
	.long	L_1+2440
	.long	Plus__Plus_param_u8_u32
	.long	L_1+2456
	.long	Plus__uPlus_param_u8_u32
	.long	L_1+2474
	.long	Plus__Plus_var_u8_u32
	.long	L_1+2493
	.long	Plus__uPlus_var_u8_u32
	.long	L_1+2509
	.long	Plus__Plus_param_u8_i8
	.long	L_1+2526
	.long	Plus__uPlus_param_u8_i8
	.long	L_1+2543
	.long	Plus__Plus_var_u8_i8
	.long	L_1+2561
	.long	Plus__uPlus_var_u8_i8
	.long	L_1+2576
	.long	Plus__Plus_param_u8_L
	.long	L_1+2592
	.long	Plus__uPlus_param_u8_L
	.long	L_1+2608
	.long	Plus__Plus_var_u8_L
	.long	L_1+2625
	.long	Plus__uPlus_var_u8_L
	.long	L_1+2639
	.long	Plus__Plus_param_u8_u8
	.long	L_1+2654
	.long	Plus__uPlus_param_u8_u8
	.long	L_1+2671
	.long	Plus__Plus_var_u8_u8
	.long	L_1+2689
	.long	Plus__uPlus_var_u8_u8
	.long	L_1+2704
	.long	Plus__Plus_param_u8_C
	.long	L_1+2720
	.long	Plus__uPlus_param_u8_C
	.long	L_1+2736
	.long	Plus__Plus_var_u8_C
	.long	L_1+2753
	.long	Plus__uPlus_var_u8_C
	.long	L_1+2767
	.long	Plus__Plus_param_u8_i64
	.long	L_1+2782
	.long	Plus__uPlus_param_u8_i64
	.long	L_1+2800
	.long	Plus__Plus_var_u8_i64
	.long	L_1+2819
	.long	Plus__uPlus_var_u8_i64
	.long	L_1+2835
	.long	Plus__Plus_param_u8_I
	.long	L_1+2852
	.long	Plus__uPlus_param_u8_I
	.long	L_1+2868
	.long	Plus__Plus_var_u8_I
	.long	L_1+2885
	.long	Plus__uPlus_var_u8_I
	.long	L_1+2899
	.long	Plus__Plus_param_u8_i16
	.long	L_1+2914
	.long	Plus__uPlus_param_u8_i16
	.long	L_1+2932
	.long	Plus__Plus_var_u8_i16
	.long	L_1+2951
	.long	Plus__uPlus_var_u8_i16
	.long	L_1+2967
	.long	Plus__Plus_param_u8_i32
	.long	L_1+2984
	.long	Plus__uPlus_param_u8_i32
	.long	L_1+3002
	.long	Plus__Plus_var_u8_i32
	.long	L_1+3021
	.long	Plus__uPlus_var_u8_i32
	.long	L_1+3037
	.long	Plus__Plus_param_u8_LC
	.long	L_1+3054
	.long	Plus__uPlus_param_u8_LC
	.long	L_1+3071
	.long	Plus__Plus_var_u8_LC
	.long	L_1+3089
	.long	Plus__uPlus_var_u8_LC
	.long	L_1+3104
	.long	Plus__Plus_param_u8_u64
	.long	L_1+3120
	.long	Plus__uPlus_param_u8_u64
	.long	L_1+3138
	.long	Plus__Plus_var_u8_u64
	.long	L_1+3157
	.long	Plus__uPlus_var_u8_u64
	.long	L_1+3173
	.long	Plus__Plus_param_u8_u16
	.long	L_1+3190
	.long	Plus__uPlus_param_u8_u16
	.long	L_1+3208
	.long	Plus__Plus_var_u8_u16
	.long	L_1+3227
	.long	Plus__uPlus_var_u8_u16
	.long	L_1+3243
	.long	Plus__Plus_param_f64_f64
	.long	L_1+3260
	.long	Plus__Plus_var_f64_f64
	.long	L_1+3279
	.long	Plus__Plus_param_C_u32
	.long	L_1+3296
	.long	Plus__uPlus_param_C_u32
	.long	L_1+3313
	.long	Plus__Plus_var_C_u32
	.long	L_1+3331
	.long	Plus__uPlus_var_C_u32
	.long	L_1+3346
	.long	Plus__Plus_param_C_i8
	.long	L_1+3362
	.long	Plus__uPlus_param_C_i8
	.long	L_1+3378
	.long	Plus__Plus_var_C_i8
	.long	L_1+3395
	.long	Plus__uPlus_var_C_i8
	.long	L_1+3409
	.long	Plus__Plus_param_C_L
	.long	L_1+3424
	.long	Plus__uPlus_param_C_L
	.long	L_1+3439
	.long	Plus__Plus_var_C_L
	.long	L_1+3455
	.long	Plus__uPlus_var_C_L
	.long	L_1+3468
	.long	Plus__Plus_param_C_u8
	.long	L_1+3482
	.long	Plus__uPlus_param_C_u8
	.long	L_1+3498
	.long	Plus__Plus_var_C_u8
	.long	L_1+3515
	.long	Plus__uPlus_var_C_u8
	.long	L_1+3529
	.long	Plus__Plus_param_C_C
	.long	L_1+3544
	.long	Plus__uPlus_param_C_C
	.long	L_1+3559
	.long	Plus__Plus_var_C_C
	.long	L_1+3575
	.long	Plus__uPlus_var_C_C
	.long	L_1+3588
	.long	Plus__Plus_param_C_i64
	.long	L_1+3602
	.long	Plus__uPlus_param_C_i64
	.long	L_1+3619
	.long	Plus__Plus_var_C_i64
	.long	L_1+3637
	.long	Plus__uPlus_var_C_i64
	.long	L_1+3652
	.long	Plus__Plus_param_C_I
	.long	L_1+3668
	.long	Plus__uPlus_param_C_I
	.long	L_1+3683
	.long	Plus__Plus_var_C_I
	.long	L_1+3699
	.long	Plus__uPlus_var_C_I
	.long	L_1+3712
	.long	Plus__Plus_param_C_i16
	.long	L_1+3726
	.long	Plus__uPlus_param_C_i16
	.long	L_1+3743
	.long	Plus__Plus_var_C_i16
	.long	L_1+3761
	.long	Plus__uPlus_var_C_i16
	.long	L_1+3776
	.long	Plus__Plus_param_C_i32
	.long	L_1+3792
	.long	Plus__uPlus_param_C_i32
	.long	L_1+3809
	.long	Plus__Plus_var_C_i32
	.long	L_1+3827
	.long	Plus__uPlus_var_C_i32
	.long	L_1+3842
	.long	Plus__Plus_param_C_LC
	.long	L_1+3858
	.long	Plus__uPlus_param_C_LC
	.long	L_1+3874
	.long	Plus__Plus_var_C_LC
	.long	L_1+3891
	.long	Plus__uPlus_var_C_LC
	.long	L_1+3905
	.long	Plus__Plus_param_C_u64
	.long	L_1+3920
	.long	Plus__uPlus_param_C_u64
	.long	L_1+3937
	.long	Plus__Plus_var_C_u64
	.long	L_1+3955
	.long	Plus__uPlus_var_C_u64
	.long	L_1+3970
	.long	Plus__Plus_param_C_u16
	.long	L_1+3986
	.long	Plus__uPlus_param_C_u16
	.long	L_1+4003
	.long	Plus__Plus_var_C_u16
	.long	L_1+4021
	.long	Plus__uPlus_var_C_u16
	.long	L_1+4036
	.long	Plus__Plus_param_i64_u32
	.long	L_1+4052
	.long	Plus__uPlus_param_i64_u32
	.long	L_1+4071
	.long	Plus__Plus_var_i64_u32
	.long	L_1+4091
	.long	Plus__uPlus_var_i64_u32
	.long	L_1+4108
	.long	Plus__Plus_param_i64_i8
	.long	L_1+4126
	.long	Plus__uPlus_param_i64_i8
	.long	L_1+4144
	.long	Plus__Plus_var_i64_i8
	.long	L_1+4163
	.long	Plus__uPlus_var_i64_i8
	.long	L_1+4179
	.long	Plus__Plus_param_i64_L
	.long	L_1+4196
	.long	Plus__uPlus_param_i64_L
	.long	L_1+4213
	.long	Plus__Plus_var_i64_L
	.long	L_1+4231
	.long	Plus__uPlus_var_i64_L
	.long	L_1+4246
	.long	Plus__Plus_param_i64_u8
	.long	L_1+4262
	.long	Plus__uPlus_param_i64_u8
	.long	L_1+4280
	.long	Plus__Plus_var_i64_u8
	.long	L_1+4299
	.long	Plus__uPlus_var_i64_u8
	.long	L_1+4315
	.long	Plus__Plus_param_i64_C
	.long	L_1+4332
	.long	Plus__uPlus_param_i64_C
	.long	L_1+4349
	.long	Plus__Plus_var_i64_C
	.long	L_1+4367
	.long	Plus__uPlus_var_i64_C
	.long	L_1+4382
	.long	Plus__Plus_param_i64_i64
	.long	L_1+4398
	.long	Plus__uPlus_param_i64_i64
	.long	L_1+4417
	.long	Plus__Plus_var_i64_i64
	.long	L_1+4437
	.long	Plus__uPlus_var_i64_i64
	.long	L_1+4454
	.long	Plus__Plus_param_i64_I
	.long	L_1+4472
	.long	Plus__uPlus_param_i64_I
	.long	L_1+4489
	.long	Plus__Plus_var_i64_I
	.long	L_1+4507
	.long	Plus__uPlus_var_i64_I
	.long	L_1+4522
	.long	Plus__Plus_param_i64_i16
	.long	L_1+4538
	.long	Plus__uPlus_param_i64_i16
	.long	L_1+4557
	.long	Plus__Plus_var_i64_i16
	.long	L_1+4577
	.long	Plus__uPlus_var_i64_i16
	.long	L_1+4594
	.long	Plus__Plus_param_i64_i32
	.long	L_1+4612
	.long	Plus__uPlus_param_i64_i32
	.long	L_1+4631
	.long	Plus__Plus_var_i64_i32
	.long	L_1+4651
	.long	Plus__uPlus_var_i64_i32
	.long	L_1+4668
	.long	Plus__Plus_param_i64_LC
	.long	L_1+4686
	.long	Plus__uPlus_param_i64_LC
	.long	L_1+4704
	.long	Plus__Plus_var_i64_LC
	.long	L_1+4723
	.long	Plus__uPlus_var_i64_LC
	.long	L_1+4739
	.long	Plus__Plus_param_i64_u64
	.long	L_1+4756
	.long	Plus__uPlus_param_i64_u64
	.long	L_1+4775
	.long	Plus__Plus_var_i64_u64
	.long	L_1+4795
	.long	Plus__uPlus_var_i64_u64
	.long	L_1+4812
	.long	Plus__Plus_param_i64_u16
	.long	L_1+4830
	.long	Plus__uPlus_param_i64_u16
	.long	L_1+4849
	.long	Plus__Plus_var_i64_u16
	.long	L_1+4869
	.long	Plus__uPlus_var_i64_u16
	.long	L_1+4886
	.long	Plus__Plus_param_I_u32
	.long	L_1+4904
	.long	Plus__uPlus_param_I_u32
	.long	L_1+4921
	.long	Plus__Plus_var_I_u32
	.long	L_1+4939
	.long	Plus__uPlus_var_I_u32
	.long	L_1+4954
	.long	Plus__Plus_param_I_i8
	.long	L_1+4970
	.long	Plus__uPlus_param_I_i8
	.long	L_1+4986
	.long	Plus__Plus_var_I_i8
	.long	L_1+5003
	.long	Plus__uPlus_var_I_i8
	.long	L_1+5017
	.long	Plus__Plus_param_I_L
	.long	L_1+5032
	.long	Plus__uPlus_param_I_L
	.long	L_1+5047
	.long	Plus__Plus_var_I_L
	.long	L_1+5063
	.long	Plus__uPlus_var_I_L
	.long	L_1+5076
	.long	Plus__Plus_param_I_u8
	.long	L_1+5090
	.long	Plus__uPlus_param_I_u8
	.long	L_1+5106
	.long	Plus__Plus_var_I_u8
	.long	L_1+5123
	.long	Plus__uPlus_var_I_u8
	.long	L_1+5137
	.long	Plus__Plus_param_I_C
	.long	L_1+5152
	.long	Plus__uPlus_param_I_C
	.long	L_1+5167
	.long	Plus__Plus_var_I_C
	.long	L_1+5183
	.long	Plus__uPlus_var_I_C
	.long	L_1+5196
	.long	Plus__Plus_param_I_i64
	.long	L_1+5210
	.long	Plus__uPlus_param_I_i64
	.long	L_1+5227
	.long	Plus__Plus_var_I_i64
	.long	L_1+5245
	.long	Plus__uPlus_var_I_i64
	.long	L_1+5260
	.long	Plus__Plus_param_I_I
	.long	L_1+5276
	.long	Plus__uPlus_param_I_I
	.long	L_1+5291
	.long	Plus__Plus_var_I_I
	.long	L_1+5307
	.long	Plus__uPlus_var_I_I
	.long	L_1+5320
	.long	Plus__Plus_param_I_i16
	.long	L_1+5334
	.long	Plus__uPlus_param_I_i16
	.long	L_1+5351
	.long	Plus__Plus_var_I_i16
	.long	L_1+5369
	.long	Plus__uPlus_var_I_i16
	.long	L_1+5384
	.long	Plus__Plus_param_I_i32
	.long	L_1+5400
	.long	Plus__uPlus_param_I_i32
	.long	L_1+5417
	.long	Plus__Plus_var_I_i32
	.long	L_1+5435
	.long	Plus__uPlus_var_I_i32
	.long	L_1+5450
	.long	Plus__Plus_param_I_LC
	.long	L_1+5466
	.long	Plus__uPlus_param_I_LC
	.long	L_1+5482
	.long	Plus__Plus_var_I_LC
	.long	L_1+5499
	.long	Plus__uPlus_var_I_LC
	.long	L_1+5513
	.long	Plus__Plus_param_I_u64
	.long	L_1+5528
	.long	Plus__uPlus_param_I_u64
	.long	L_1+5545
	.long	Plus__Plus_var_I_u64
	.long	L_1+5563
	.long	Plus__uPlus_var_I_u64
	.long	L_1+5578
	.long	Plus__Plus_param_I_u16
	.long	L_1+5594
	.long	Plus__uPlus_param_I_u16
	.long	L_1+5611
	.long	Plus__Plus_var_I_u16
	.long	L_1+5629
	.long	Plus__uPlus_var_I_u16
	.long	L_1+5644
	.long	Plus__Plus_param_i16_u32
	.long	L_1+5660
	.long	Plus__uPlus_param_i16_u32
	.long	L_1+5679
	.long	Plus__Plus_var_i16_u32
	.long	L_1+5699
	.long	Plus__uPlus_var_i16_u32
	.long	L_1+5716
	.long	Plus__Plus_param_i16_i8
	.long	L_1+5734
	.long	Plus__uPlus_param_i16_i8
	.long	L_1+5752
	.long	Plus__Plus_var_i16_i8
	.long	L_1+5771
	.long	Plus__uPlus_var_i16_i8
	.long	L_1+5787
	.long	Plus__Plus_param_i16_L
	.long	L_1+5804
	.long	Plus__uPlus_param_i16_L
	.long	L_1+5821
	.long	Plus__Plus_var_i16_L
	.long	L_1+5839
	.long	Plus__uPlus_var_i16_L
	.long	L_1+5854
	.long	Plus__Plus_param_i16_u8
	.long	L_1+5870
	.long	Plus__uPlus_param_i16_u8
	.long	L_1+5888
	.long	Plus__Plus_var_i16_u8
	.long	L_1+5907
	.long	Plus__uPlus_var_i16_u8
	.long	L_1+5923
	.long	Plus__Plus_param_i16_C
	.long	L_1+5940
	.long	Plus__uPlus_param_i16_C
	.long	L_1+5957
	.long	Plus__Plus_var_i16_C
	.long	L_1+5975
	.long	Plus__uPlus_var_i16_C
	.long	L_1+5990
	.long	Plus__Plus_param_i16_i64
	.long	L_1+6006
	.long	Plus__uPlus_param_i16_i64
	.long	L_1+6025
	.long	Plus__Plus_var_i16_i64
	.long	L_1+6045
	.long	Plus__uPlus_var_i16_i64
	.long	L_1+6062
	.long	Plus__Plus_param_i16_I
	.long	L_1+6080
	.long	Plus__uPlus_param_i16_I
	.long	L_1+6097
	.long	Plus__Plus_var_i16_I
	.long	L_1+6115
	.long	Plus__uPlus_var_i16_I
	.long	L_1+6130
	.long	Plus__Plus_param_i16_i16
	.long	L_1+6146
	.long	Plus__uPlus_param_i16_i16
	.long	L_1+6165
	.long	Plus__Plus_var_i16_i16
	.long	L_1+6185
	.long	Plus__uPlus_var_i16_i16
	.long	L_1+6202
	.long	Plus__Plus_param_i16_i32
	.long	L_1+6220
	.long	Plus__uPlus_param_i16_i32
	.long	L_1+6239
	.long	Plus__Plus_var_i16_i32
	.long	L_1+6259
	.long	Plus__uPlus_var_i16_i32
	.long	L_1+6276
	.long	Plus__Plus_param_i16_LC
	.long	L_1+6294
	.long	Plus__uPlus_param_i16_LC
	.long	L_1+6312
	.long	Plus__Plus_var_i16_LC
	.long	L_1+6331
	.long	Plus__uPlus_var_i16_LC
	.long	L_1+6347
	.long	Plus__Plus_param_i16_u64
	.long	L_1+6364
	.long	Plus__uPlus_param_i16_u64
	.long	L_1+6383
	.long	Plus__Plus_var_i16_u64
	.long	L_1+6403
	.long	Plus__uPlus_var_i16_u64
	.long	L_1+6420
	.long	Plus__Plus_param_i16_u16
	.long	L_1+6438
	.long	Plus__uPlus_param_i16_u16
	.long	L_1+6457
	.long	Plus__Plus_var_i16_u16
	.long	L_1+6477
	.long	Plus__uPlus_var_i16_u16
	.long	L_1+6494
	.long	Plus__Plus_param_i32_u32
	.long	L_1+6512
	.long	Plus__uPlus_param_i32_u32
	.long	L_1+6531
	.long	Plus__Plus_var_i32_u32
	.long	L_1+6551
	.long	Plus__uPlus_var_i32_u32
	.long	L_1+6568
	.long	Plus__Plus_param_i32_i8
	.long	L_1+6586
	.long	Plus__uPlus_param_i32_i8
	.long	L_1+6604
	.long	Plus__Plus_var_i32_i8
	.long	L_1+6623
	.long	Plus__uPlus_var_i32_i8
	.long	L_1+6639
	.long	Plus__Plus_param_i32_L
	.long	L_1+6656
	.long	Plus__uPlus_param_i32_L
	.long	L_1+6673
	.long	Plus__Plus_var_i32_L
	.long	L_1+6691
	.long	Plus__uPlus_var_i32_L
	.long	L_1+6706
	.long	Plus__Plus_param_i32_u8
	.long	L_1+6722
	.long	Plus__uPlus_param_i32_u8
	.long	L_1+6740
	.long	Plus__Plus_var_i32_u8
	.long	L_1+6759
	.long	Plus__uPlus_var_i32_u8
	.long	L_1+6775
	.long	Plus__Plus_param_i32_C
	.long	L_1+6792
	.long	Plus__uPlus_param_i32_C
	.long	L_1+6809
	.long	Plus__Plus_var_i32_C
	.long	L_1+6827
	.long	Plus__uPlus_var_i32_C
	.long	L_1+6842
	.long	Plus__Plus_param_i32_i64
	.long	L_1+6858
	.long	Plus__uPlus_param_i32_i64
	.long	L_1+6877
	.long	Plus__Plus_var_i32_i64
	.long	L_1+6897
	.long	Plus__uPlus_var_i32_i64
	.long	L_1+6914
	.long	Plus__Plus_param_i32_I
	.long	L_1+6932
	.long	Plus__uPlus_param_i32_I
	.long	L_1+6949
	.long	Plus__Plus_var_i32_I
	.long	L_1+6967
	.long	Plus__uPlus_var_i32_I
	.long	L_1+6982
	.long	Plus__Plus_param_i32_i16
	.long	L_1+6998
	.long	Plus__uPlus_param_i32_i16
	.long	L_1+7017
	.long	Plus__Plus_var_i32_i16
	.long	L_1+7037
	.long	Plus__uPlus_var_i32_i16
	.long	L_1+7054
	.long	Plus__Plus_param_i32_i32
	.long	L_1+7072
	.long	Plus__uPlus_param_i32_i32
	.long	L_1+7091
	.long	Plus__Plus_var_i32_i32
	.long	L_1+7111
	.long	Plus__uPlus_var_i32_i32
	.long	L_1+7128
	.long	Plus__Plus_param_i32_LC
	.long	L_1+7146
	.long	Plus__uPlus_param_i32_LC
	.long	L_1+7164
	.long	Plus__Plus_var_i32_LC
	.long	L_1+7183
	.long	Plus__uPlus_var_i32_LC
	.long	L_1+7199
	.long	Plus__Plus_param_i32_u64
	.long	L_1+7216
	.long	Plus__uPlus_param_i32_u64
	.long	L_1+7235
	.long	Plus__Plus_var_i32_u64
	.long	L_1+7255
	.long	Plus__uPlus_var_i32_u64
	.long	L_1+7272
	.long	Plus__Plus_param_i32_u16
	.long	L_1+7290
	.long	Plus__uPlus_param_i32_u16
	.long	L_1+7309
	.long	Plus__Plus_var_i32_u16
	.long	L_1+7329
	.long	Plus__uPlus_var_i32_u16
	.long	L_1+7346
	.long	Plus__Plus_param_LC_u32
	.long	L_1+7364
	.long	Plus__uPlus_param_LC_u32
	.long	L_1+7382
	.long	Plus__Plus_var_LC_u32
	.long	L_1+7401
	.long	Plus__uPlus_var_LC_u32
	.long	L_1+7417
	.long	Plus__Plus_param_LC_i8
	.long	L_1+7434
	.long	Plus__uPlus_param_LC_i8
	.long	L_1+7451
	.long	Plus__Plus_var_LC_i8
	.long	L_1+7469
	.long	Plus__uPlus_var_LC_i8
	.long	L_1+7484
	.long	Plus__Plus_param_LC_L
	.long	L_1+7500
	.long	Plus__uPlus_param_LC_L
	.long	L_1+7516
	.long	Plus__Plus_var_LC_L
	.long	L_1+7533
	.long	Plus__uPlus_var_LC_L
	.long	L_1+7547
	.long	Plus__Plus_param_LC_u8
	.long	L_1+7562
	.long	Plus__uPlus_param_LC_u8
	.long	L_1+7579
	.long	Plus__Plus_var_LC_u8
	.long	L_1+7597
	.long	Plus__uPlus_var_LC_u8
	.long	L_1+7612
	.long	Plus__Plus_param_LC_C
	.long	L_1+7628
	.long	Plus__uPlus_param_LC_C
	.long	L_1+7644
	.long	Plus__Plus_var_LC_C
	.long	L_1+7661
	.long	Plus__uPlus_var_LC_C
	.long	L_1+7675
	.long	Plus__Plus_param_LC_i64
	.long	L_1+7690
	.long	Plus__uPlus_param_LC_i64
	.long	L_1+7708
	.long	Plus__Plus_var_LC_i64
	.long	L_1+7727
	.long	Plus__uPlus_var_LC_i64
	.long	L_1+7743
	.long	Plus__Plus_param_LC_I
	.long	L_1+7760
	.long	Plus__uPlus_param_LC_I
	.long	L_1+7776
	.long	Plus__Plus_var_LC_I
	.long	L_1+7793
	.long	Plus__uPlus_var_LC_I
	.long	L_1+7807
	.long	Plus__Plus_param_LC_i16
	.long	L_1+7822
	.long	Plus__uPlus_param_LC_i16
	.long	L_1+7840
	.long	Plus__Plus_var_LC_i16
	.long	L_1+7859
	.long	Plus__uPlus_var_LC_i16
	.long	L_1+7875
	.long	Plus__Plus_param_LC_i32
	.long	L_1+7892
	.long	Plus__uPlus_param_LC_i32
	.long	L_1+7910
	.long	Plus__Plus_var_LC_i32
	.long	L_1+7929
	.long	Plus__uPlus_var_LC_i32
	.long	L_1+7945
	.long	Plus__Plus_param_LC_LC
	.long	L_1+7962
	.long	Plus__uPlus_param_LC_LC
	.long	L_1+7979
	.long	Plus__Plus_var_LC_LC
	.long	L_1+7997
	.long	Plus__uPlus_var_LC_LC
	.long	L_1+8012
	.long	Plus__Plus_param_LC_u64
	.long	L_1+8028
	.long	Plus__uPlus_param_LC_u64
	.long	L_1+8046
	.long	Plus__Plus_var_LC_u64
	.long	L_1+8065
	.long	Plus__uPlus_var_LC_u64
	.long	L_1+8081
	.long	Plus__Plus_param_LC_u16
	.long	L_1+8098
	.long	Plus__uPlus_param_LC_u16
	.long	L_1+8116
	.long	Plus__Plus_var_LC_u16
	.long	L_1+8135
	.long	Plus__uPlus_var_LC_u16
	.long	L_1+8151
	.long	Plus__Plus_param_u64_u32
	.long	L_1+8168
	.long	Plus__uPlus_param_u64_u32
	.long	L_1+8187
	.long	Plus__Plus_var_u64_u32
	.long	L_1+8207
	.long	Plus__uPlus_var_u64_u32
	.long	L_1+8224
	.long	Plus__Plus_param_u64_i8
	.long	L_1+8242
	.long	Plus__uPlus_param_u64_i8
	.long	L_1+8260
	.long	Plus__Plus_var_u64_i8
	.long	L_1+8279
	.long	Plus__uPlus_var_u64_i8
	.long	L_1+8295
	.long	Plus__Plus_param_u64_L
	.long	L_1+8312
	.long	Plus__uPlus_param_u64_L
	.long	L_1+8329
	.long	Plus__Plus_var_u64_L
	.long	L_1+8347
	.long	Plus__uPlus_var_u64_L
	.long	L_1+8362
	.long	Plus__Plus_param_u64_u8
	.long	L_1+8378
	.long	Plus__uPlus_param_u64_u8
	.long	L_1+8396
	.long	Plus__Plus_var_u64_u8
	.long	L_1+8415
	.long	Plus__uPlus_var_u64_u8
	.long	L_1+8431
	.long	Plus__Plus_param_u64_C
	.long	L_1+8448
	.long	Plus__uPlus_param_u64_C
	.long	L_1+8465
	.long	Plus__Plus_var_u64_C
	.long	L_1+8483
	.long	Plus__uPlus_var_u64_C
	.long	L_1+8498
	.long	Plus__Plus_param_u64_i64
	.long	L_1+8514
	.long	Plus__uPlus_param_u64_i64
	.long	L_1+8533
	.long	Plus__Plus_var_u64_i64
	.long	L_1+8553
	.long	Plus__uPlus_var_u64_i64
	.long	L_1+8570
	.long	Plus__Plus_param_u64_I
	.long	L_1+8588
	.long	Plus__uPlus_param_u64_I
	.long	L_1+8605
	.long	Plus__Plus_var_u64_I
	.long	L_1+8623
	.long	Plus__uPlus_var_u64_I
	.long	L_1+8638
	.long	Plus__Plus_param_u64_i16
	.long	L_1+8654
	.long	Plus__uPlus_param_u64_i16
	.long	L_1+8673
	.long	Plus__Plus_var_u64_i16
	.long	L_1+8693
	.long	Plus__uPlus_var_u64_i16
	.long	L_1+8710
	.long	Plus__Plus_param_u64_i32
	.long	L_1+8728
	.long	Plus__uPlus_param_u64_i32
	.long	L_1+8747
	.long	Plus__Plus_var_u64_i32
	.long	L_1+8767
	.long	Plus__uPlus_var_u64_i32
	.long	L_1+8784
	.long	Plus__Plus_param_u64_LC
	.long	L_1+8802
	.long	Plus__uPlus_param_u64_LC
	.long	L_1+8820
	.long	Plus__Plus_var_u64_LC
	.long	L_1+8839
	.long	Plus__uPlus_var_u64_LC
	.long	L_1+8855
	.long	Plus__Plus_param_u64_u64
	.long	L_1+8872
	.long	Plus__uPlus_param_u64_u64
	.long	L_1+8891
	.long	Plus__Plus_var_u64_u64
	.long	L_1+8911
	.long	Plus__uPlus_var_u64_u64
	.long	L_1+8928
	.long	Plus__Plus_param_u64_u16
	.long	L_1+8946
	.long	Plus__uPlus_param_u64_u16
	.long	L_1+8965
	.long	Plus__Plus_var_u64_u16
	.long	L_1+8985
	.long	Plus__uPlus_var_u64_u16
	.long	L_1+9002
	.long	Plus__Plus_param_u16_u32
	.long	L_1+9020
	.long	Plus__uPlus_param_u16_u32
	.long	L_1+9039
	.long	Plus__Plus_var_u16_u32
	.long	L_1+9059
	.long	Plus__uPlus_var_u16_u32
	.long	L_1+9076
	.long	Plus__Plus_param_u16_i8
	.long	L_1+9094
	.long	Plus__uPlus_param_u16_i8
	.long	L_1+9112
	.long	Plus__Plus_var_u16_i8
	.long	L_1+9131
	.long	Plus__uPlus_var_u16_i8
	.long	L_1+9147
	.long	Plus__Plus_param_u16_L
	.long	L_1+9164
	.long	Plus__uPlus_param_u16_L
	.long	L_1+9181
	.long	Plus__Plus_var_u16_L
	.long	L_1+9199
	.long	Plus__uPlus_var_u16_L
	.long	L_1+9214
	.long	Plus__Plus_param_u16_u8
	.long	L_1+9230
	.long	Plus__uPlus_param_u16_u8
	.long	L_1+9248
	.long	Plus__Plus_var_u16_u8
	.long	L_1+9267
	.long	Plus__uPlus_var_u16_u8
	.long	L_1+9283
	.long	Plus__Plus_param_u16_C
	.long	L_1+9300
	.long	Plus__uPlus_param_u16_C
	.long	L_1+9317
	.long	Plus__Plus_var_u16_C
	.long	L_1+9335
	.long	Plus__uPlus_var_u16_C
	.long	L_1+9350
	.long	Plus__Plus_param_u16_i64
	.long	L_1+9366
	.long	Plus__uPlus_param_u16_i64
	.long	L_1+9385
	.long	Plus__Plus_var_u16_i64
	.long	L_1+9405
	.long	Plus__uPlus_var_u16_i64
	.long	L_1+9422
	.long	Plus__Plus_param_u16_I
	.long	L_1+9440
	.long	Plus__uPlus_param_u16_I
	.long	L_1+9457
	.long	Plus__Plus_var_u16_I
	.long	L_1+9475
	.long	Plus__uPlus_var_u16_I
	.long	L_1+9490
	.long	Plus__Plus_param_u16_i16
	.long	L_1+9506
	.long	Plus__uPlus_param_u16_i16
	.long	L_1+9525
	.long	Plus__Plus_var_u16_i16
	.long	L_1+9545
	.long	Plus__uPlus_var_u16_i16
	.long	L_1+9562
	.long	Plus__Plus_param_u16_i32
	.long	L_1+9580
	.long	Plus__uPlus_param_u16_i32
	.long	L_1+9599
	.long	Plus__Plus_var_u16_i32
	.long	L_1+9619
	.long	Plus__uPlus_var_u16_i32
	.long	L_1+9636
	.long	Plus__Plus_param_u16_LC
	.long	L_1+9654
	.long	Plus__uPlus_param_u16_LC
	.long	L_1+9672
	.long	Plus__Plus_var_u16_LC
	.long	L_1+9691
	.long	Plus__uPlus_var_u16_LC
	.long	L_1+9707
	.long	Plus__Plus_param_u16_u64
	.long	L_1+9724
	.long	Plus__uPlus_param_u16_u64
	.long	L_1+9743
	.long	Plus__Plus_var_u16_u64
	.long	L_1+9763
	.long	Plus__uPlus_var_u16_u64
	.long	L_1+9780
	.long	Plus__Plus_param_u16_u16
	.long	L_1+9798
	.long	Plus__uPlus_param_u16_u16
	.long	L_1+9817
	.long	Plus__Plus_var_u16_u16
	.long	L_1+9837
	.long	Plus__uPlus_var_u16_u16
	.long	L_1+9854
	.skip 4
	.ascii	"../SOLsun/Plus.m3"
	.skip 3
	.align 8
	.type	MM_Plus, #object
	.size	MM_Plus, 208
MM_Plus:
	.long	L_1+14524
	.skip 16
	.long	L_1+9872
	.skip 12
	.long	MM_Plus+148
	.skip 4
	.long	Plus_M3
	.long	3
	.half	145
	.skip 2
	.long	0
	.long	146
	.long	0
	.long	147
	.long	148
	.half	149
	.skip 2
	.long	150
	.skip 4
	.long	0
	.long	151
	.long	152
	.skip 4
	.long	1080239341
	.long	-1855425872
	.byte	-101
	.skip 7
	.long	0
	.long	156
	.byte	-99
	.skip 3
	.long	158
	.long	1126115574
	.skip 12
	.long	Plus_I3
	.long	MM_Plus+160
	.skip 4
	.long	Long_I3
	.long	MM_Plus+172
	.skip 4
	.long	Word_I3
	.long	MM_Plus+184
	.skip 4
	.long	Cstdint_I3
	.long	MM_Plus+196
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
	.uaword	.LLFB13
	.uaword	.LLFE13-.LLFB13
	.byte	0x4
	.uaword	.LLCFI13-.LLFB13
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
	.uaword	.LLFB15
	.uaword	.LLFE15-.LLFB15
	.byte	0x4
	.uaword	.LLCFI15-.LLFB15
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
.LLEFDE9:
.LLSFDE11:
	.uaword	.LLEFDE11-.LLASFDE11
.LLASFDE11:
	.uaword	.LLASFDE11-.LLframe1
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
.LLEFDE11:
.LLSFDE13:
	.uaword	.LLEFDE13-.LLASFDE13
.LLASFDE13:
	.uaword	.LLASFDE13-.LLframe1
	.uaword	.LLFB21
	.uaword	.LLFE21-.LLFB21
	.byte	0x4
	.uaword	.LLCFI21-.LLFB21
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
	.uaword	.LLFB23
	.uaword	.LLFE23-.LLFB23
	.byte	0x4
	.uaword	.LLCFI23-.LLFB23
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
.LLEFDE17:
.LLSFDE19:
	.uaword	.LLEFDE19-.LLASFDE19
.LLASFDE19:
	.uaword	.LLASFDE19-.LLframe1
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
.LLEFDE19:
.LLSFDE21:
	.uaword	.LLEFDE21-.LLASFDE21
.LLASFDE21:
	.uaword	.LLASFDE21-.LLframe1
	.uaword	.LLFB33
	.uaword	.LLFE33-.LLFB33
	.byte	0x4
	.uaword	.LLCFI33-.LLFB33
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE21:
.LLSFDE23:
	.uaword	.LLEFDE23-.LLASFDE23
.LLASFDE23:
	.uaword	.LLASFDE23-.LLframe1
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
.LLEFDE23:
.LLSFDE25:
	.uaword	.LLEFDE25-.LLASFDE25
.LLASFDE25:
	.uaword	.LLASFDE25-.LLframe1
	.uaword	.LLFB41
	.uaword	.LLFE41-.LLFB41
	.byte	0x4
	.uaword	.LLCFI41-.LLFB41
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE25:
.LLSFDE27:
	.uaword	.LLEFDE27-.LLASFDE27
.LLASFDE27:
	.uaword	.LLASFDE27-.LLframe1
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
.LLEFDE27:
.LLSFDE29:
	.uaword	.LLEFDE29-.LLASFDE29
.LLASFDE29:
	.uaword	.LLASFDE29-.LLframe1
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
.LLEFDE29:
.LLSFDE31:
	.uaword	.LLEFDE31-.LLASFDE31
.LLASFDE31:
	.uaword	.LLASFDE31-.LLframe1
	.uaword	.LLFB47
	.uaword	.LLFE47-.LLFB47
	.byte	0x4
	.uaword	.LLCFI47-.LLFB47
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE31:
.LLSFDE33:
	.uaword	.LLEFDE33-.LLASFDE33
.LLASFDE33:
	.uaword	.LLASFDE33-.LLframe1
	.uaword	.LLFB193
	.uaword	.LLFE193-.LLFB193
	.byte	0x4
	.uaword	.LLCFI193-.LLFB193
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE33:
.LLSFDE35:
	.uaword	.LLEFDE35-.LLASFDE35
.LLASFDE35:
	.uaword	.LLASFDE35-.LLframe1
	.uaword	.LLFB195
	.uaword	.LLFE195-.LLFB195
	.byte	0x4
	.uaword	.LLCFI195-.LLFB195
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE35:
.LLSFDE37:
	.uaword	.LLEFDE37-.LLASFDE37
.LLASFDE37:
	.uaword	.LLASFDE37-.LLframe1
	.uaword	.LLFB205
	.uaword	.LLFE205-.LLFB205
	.byte	0x4
	.uaword	.LLCFI205-.LLFB205
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE37:
.LLSFDE39:
	.uaword	.LLEFDE39-.LLASFDE39
.LLASFDE39:
	.uaword	.LLASFDE39-.LLframe1
	.uaword	.LLFB207
	.uaword	.LLFE207-.LLFB207
	.byte	0x4
	.uaword	.LLCFI207-.LLFB207
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE39:
.LLSFDE41:
	.uaword	.LLEFDE41-.LLASFDE41
.LLASFDE41:
	.uaword	.LLASFDE41-.LLframe1
	.uaword	.LLFB209
	.uaword	.LLFE209-.LLFB209
	.byte	0x4
	.uaword	.LLCFI209-.LLFB209
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE41:
.LLSFDE43:
	.uaword	.LLEFDE43-.LLASFDE43
.LLASFDE43:
	.uaword	.LLASFDE43-.LLframe1
	.uaword	.LLFB211
	.uaword	.LLFE211-.LLFB211
	.byte	0x4
	.uaword	.LLCFI211-.LLFB211
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE43:
.LLSFDE45:
	.uaword	.LLEFDE45-.LLASFDE45
.LLASFDE45:
	.uaword	.LLASFDE45-.LLframe1
	.uaword	.LLFB213
	.uaword	.LLFE213-.LLFB213
	.byte	0x4
	.uaword	.LLCFI213-.LLFB213
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE45:
.LLSFDE47:
	.uaword	.LLEFDE47-.LLASFDE47
.LLASFDE47:
	.uaword	.LLASFDE47-.LLframe1
	.uaword	.LLFB215
	.uaword	.LLFE215-.LLFB215
	.byte	0x4
	.uaword	.LLCFI215-.LLFB215
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE47:
.LLSFDE49:
	.uaword	.LLEFDE49-.LLASFDE49
.LLASFDE49:
	.uaword	.LLASFDE49-.LLframe1
	.uaword	.LLFB221
	.uaword	.LLFE221-.LLFB221
	.byte	0x4
	.uaword	.LLCFI221-.LLFB221
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE49:
.LLSFDE51:
	.uaword	.LLEFDE51-.LLASFDE51
.LLASFDE51:
	.uaword	.LLASFDE51-.LLframe1
	.uaword	.LLFB223
	.uaword	.LLFE223-.LLFB223
	.byte	0x4
	.uaword	.LLCFI223-.LLFB223
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE51:
.LLSFDE53:
	.uaword	.LLEFDE53-.LLASFDE53
.LLASFDE53:
	.uaword	.LLASFDE53-.LLframe1
	.uaword	.LLFB225
	.uaword	.LLFE225-.LLFB225
	.byte	0x4
	.uaword	.LLCFI225-.LLFB225
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE53:
.LLSFDE55:
	.uaword	.LLEFDE55-.LLASFDE55
.LLASFDE55:
	.uaword	.LLASFDE55-.LLframe1
	.uaword	.LLFB227
	.uaword	.LLFE227-.LLFB227
	.byte	0x4
	.uaword	.LLCFI227-.LLFB227
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE55:
.LLSFDE57:
	.uaword	.LLEFDE57-.LLASFDE57
.LLASFDE57:
	.uaword	.LLASFDE57-.LLframe1
	.uaword	.LLFB233
	.uaword	.LLFE233-.LLFB233
	.byte	0x4
	.uaword	.LLCFI233-.LLFB233
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE57:
.LLSFDE59:
	.uaword	.LLEFDE59-.LLASFDE59
.LLASFDE59:
	.uaword	.LLASFDE59-.LLframe1
	.uaword	.LLFB235
	.uaword	.LLFE235-.LLFB235
	.byte	0x4
	.uaword	.LLCFI235-.LLFB235
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE59:
.LLSFDE61:
	.uaword	.LLEFDE61-.LLASFDE61
.LLASFDE61:
	.uaword	.LLASFDE61-.LLframe1
	.uaword	.LLFB237
	.uaword	.LLFE237-.LLFB237
	.byte	0x4
	.uaword	.LLCFI237-.LLFB237
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE61:
.LLSFDE63:
	.uaword	.LLEFDE63-.LLASFDE63
.LLASFDE63:
	.uaword	.LLASFDE63-.LLframe1
	.uaword	.LLFB239
	.uaword	.LLFE239-.LLFB239
	.byte	0x4
	.uaword	.LLCFI239-.LLFB239
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE63:
.LLSFDE65:
	.uaword	.LLEFDE65-.LLASFDE65
.LLASFDE65:
	.uaword	.LLASFDE65-.LLframe1
	.uaword	.LLFB337
	.uaword	.LLFE337-.LLFB337
	.byte	0x4
	.uaword	.LLCFI337-.LLFB337
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE65:
.LLSFDE67:
	.uaword	.LLEFDE67-.LLASFDE67
.LLASFDE67:
	.uaword	.LLASFDE67-.LLframe1
	.uaword	.LLFB339
	.uaword	.LLFE339-.LLFB339
	.byte	0x4
	.uaword	.LLCFI339-.LLFB339
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE67:
.LLSFDE69:
	.uaword	.LLEFDE69-.LLASFDE69
.LLASFDE69:
	.uaword	.LLASFDE69-.LLframe1
	.uaword	.LLFB349
	.uaword	.LLFE349-.LLFB349
	.byte	0x4
	.uaword	.LLCFI349-.LLFB349
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE69:
.LLSFDE71:
	.uaword	.LLEFDE71-.LLASFDE71
.LLASFDE71:
	.uaword	.LLASFDE71-.LLframe1
	.uaword	.LLFB351
	.uaword	.LLFE351-.LLFB351
	.byte	0x4
	.uaword	.LLCFI351-.LLFB351
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE71:
.LLSFDE73:
	.uaword	.LLEFDE73-.LLASFDE73
.LLASFDE73:
	.uaword	.LLASFDE73-.LLframe1
	.uaword	.LLFB353
	.uaword	.LLFE353-.LLFB353
	.byte	0x4
	.uaword	.LLCFI353-.LLFB353
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE73:
.LLSFDE75:
	.uaword	.LLEFDE75-.LLASFDE75
.LLASFDE75:
	.uaword	.LLASFDE75-.LLframe1
	.uaword	.LLFB355
	.uaword	.LLFE355-.LLFB355
	.byte	0x4
	.uaword	.LLCFI355-.LLFB355
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE75:
.LLSFDE77:
	.uaword	.LLEFDE77-.LLASFDE77
.LLASFDE77:
	.uaword	.LLASFDE77-.LLframe1
	.uaword	.LLFB357
	.uaword	.LLFE357-.LLFB357
	.byte	0x4
	.uaword	.LLCFI357-.LLFB357
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE77:
.LLSFDE79:
	.uaword	.LLEFDE79-.LLASFDE79
.LLASFDE79:
	.uaword	.LLASFDE79-.LLframe1
	.uaword	.LLFB359
	.uaword	.LLFE359-.LLFB359
	.byte	0x4
	.uaword	.LLCFI359-.LLFB359
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE79:
.LLSFDE81:
	.uaword	.LLEFDE81-.LLASFDE81
.LLASFDE81:
	.uaword	.LLASFDE81-.LLframe1
	.uaword	.LLFB365
	.uaword	.LLFE365-.LLFB365
	.byte	0x4
	.uaword	.LLCFI365-.LLFB365
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE81:
.LLSFDE83:
	.uaword	.LLEFDE83-.LLASFDE83
.LLASFDE83:
	.uaword	.LLASFDE83-.LLframe1
	.uaword	.LLFB367
	.uaword	.LLFE367-.LLFB367
	.byte	0x4
	.uaword	.LLCFI367-.LLFB367
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE83:
.LLSFDE85:
	.uaword	.LLEFDE85-.LLASFDE85
.LLASFDE85:
	.uaword	.LLASFDE85-.LLframe1
	.uaword	.LLFB369
	.uaword	.LLFE369-.LLFB369
	.byte	0x4
	.uaword	.LLCFI369-.LLFB369
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE85:
.LLSFDE87:
	.uaword	.LLEFDE87-.LLASFDE87
.LLASFDE87:
	.uaword	.LLASFDE87-.LLframe1
	.uaword	.LLFB371
	.uaword	.LLFE371-.LLFB371
	.byte	0x4
	.uaword	.LLCFI371-.LLFB371
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE87:
.LLSFDE89:
	.uaword	.LLEFDE89-.LLASFDE89
.LLASFDE89:
	.uaword	.LLASFDE89-.LLframe1
	.uaword	.LLFB377
	.uaword	.LLFE377-.LLFB377
	.byte	0x4
	.uaword	.LLCFI377-.LLFB377
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE89:
.LLSFDE91:
	.uaword	.LLEFDE91-.LLASFDE91
.LLASFDE91:
	.uaword	.LLASFDE91-.LLframe1
	.uaword	.LLFB379
	.uaword	.LLFE379-.LLFB379
	.byte	0x4
	.uaword	.LLCFI379-.LLFB379
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE91:
.LLSFDE93:
	.uaword	.LLEFDE93-.LLASFDE93
.LLASFDE93:
	.uaword	.LLASFDE93-.LLframe1
	.uaword	.LLFB381
	.uaword	.LLFE381-.LLFB381
	.byte	0x4
	.uaword	.LLCFI381-.LLFB381
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE93:
.LLSFDE95:
	.uaword	.LLEFDE95-.LLASFDE95
.LLASFDE95:
	.uaword	.LLASFDE95-.LLframe1
	.uaword	.LLFB383
	.uaword	.LLFE383-.LLFB383
	.byte	0x4
	.uaword	.LLCFI383-.LLFB383
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE95:
.LLSFDE97:
	.uaword	.LLEFDE97-.LLASFDE97
.LLASFDE97:
	.uaword	.LLASFDE97-.LLframe1
	.uaword	.LLFB387
	.uaword	.LLFE387-.LLFB387
	.byte	0x4
	.uaword	.LLCFI387-.LLFB387
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE97:
.LLSFDE99:
	.uaword	.LLEFDE99-.LLASFDE99
.LLASFDE99:
	.uaword	.LLASFDE99-.LLframe1
	.uaword	.LLFB389
	.uaword	.LLFE389-.LLFB389
	.byte	0x4
	.uaword	.LLCFI389-.LLFB389
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE99:
.LLSFDE101:
	.uaword	.LLEFDE101-.LLASFDE101
.LLASFDE101:
	.uaword	.LLASFDE101-.LLframe1
	.uaword	.LLFB399
	.uaword	.LLFE399-.LLFB399
	.byte	0x4
	.uaword	.LLCFI399-.LLFB399
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE101:
.LLSFDE103:
	.uaword	.LLEFDE103-.LLASFDE103
.LLASFDE103:
	.uaword	.LLASFDE103-.LLframe1
	.uaword	.LLFB401
	.uaword	.LLFE401-.LLFB401
	.byte	0x4
	.uaword	.LLCFI401-.LLFB401
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE103:
.LLSFDE105:
	.uaword	.LLEFDE105-.LLASFDE105
.LLASFDE105:
	.uaword	.LLASFDE105-.LLframe1
	.uaword	.LLFB403
	.uaword	.LLFE403-.LLFB403
	.byte	0x4
	.uaword	.LLCFI403-.LLFB403
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE105:
.LLSFDE107:
	.uaword	.LLEFDE107-.LLASFDE107
.LLASFDE107:
	.uaword	.LLASFDE107-.LLframe1
	.uaword	.LLFB405
	.uaword	.LLFE405-.LLFB405
	.byte	0x4
	.uaword	.LLCFI405-.LLFB405
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE107:
.LLSFDE109:
	.uaword	.LLEFDE109-.LLASFDE109
.LLASFDE109:
	.uaword	.LLASFDE109-.LLframe1
	.uaword	.LLFB407
	.uaword	.LLFE407-.LLFB407
	.byte	0x4
	.uaword	.LLCFI407-.LLFB407
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE109:
.LLSFDE111:
	.uaword	.LLEFDE111-.LLASFDE111
.LLASFDE111:
	.uaword	.LLASFDE111-.LLframe1
	.uaword	.LLFB409
	.uaword	.LLFE409-.LLFB409
	.byte	0x4
	.uaword	.LLCFI409-.LLFB409
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE111:
.LLSFDE113:
	.uaword	.LLEFDE113-.LLASFDE113
.LLASFDE113:
	.uaword	.LLASFDE113-.LLframe1
	.uaword	.LLFB415
	.uaword	.LLFE415-.LLFB415
	.byte	0x4
	.uaword	.LLCFI415-.LLFB415
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE113:
.LLSFDE115:
	.uaword	.LLEFDE115-.LLASFDE115
.LLASFDE115:
	.uaword	.LLASFDE115-.LLframe1
	.uaword	.LLFB417
	.uaword	.LLFE417-.LLFB417
	.byte	0x4
	.uaword	.LLCFI417-.LLFB417
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE115:
.LLSFDE117:
	.uaword	.LLEFDE117-.LLASFDE117
.LLASFDE117:
	.uaword	.LLASFDE117-.LLframe1
	.uaword	.LLFB419
	.uaword	.LLFE419-.LLFB419
	.byte	0x4
	.uaword	.LLCFI419-.LLFB419
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE117:
.LLSFDE119:
	.uaword	.LLEFDE119-.LLASFDE119
.LLASFDE119:
	.uaword	.LLASFDE119-.LLframe1
	.uaword	.LLFB421
	.uaword	.LLFE421-.LLFB421
	.byte	0x4
	.uaword	.LLCFI421-.LLFB421
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE119:
.LLSFDE121:
	.uaword	.LLEFDE121-.LLASFDE121
.LLASFDE121:
	.uaword	.LLASFDE121-.LLframe1
	.uaword	.LLFB427
	.uaword	.LLFE427-.LLFB427
	.byte	0x4
	.uaword	.LLCFI427-.LLFB427
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE121:
.LLSFDE123:
	.uaword	.LLEFDE123-.LLASFDE123
.LLASFDE123:
	.uaword	.LLASFDE123-.LLframe1
	.uaword	.LLFB429
	.uaword	.LLFE429-.LLFB429
	.byte	0x4
	.uaword	.LLCFI429-.LLFB429
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE123:
.LLSFDE125:
	.uaword	.LLEFDE125-.LLASFDE125
.LLASFDE125:
	.uaword	.LLASFDE125-.LLframe1
	.uaword	.LLFB431
	.uaword	.LLFE431-.LLFB431
	.byte	0x4
	.uaword	.LLCFI431-.LLFB431
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE125:
.LLSFDE127:
	.uaword	.LLEFDE127-.LLASFDE127
.LLASFDE127:
	.uaword	.LLASFDE127-.LLframe1
	.uaword	.LLFB433
	.uaword	.LLFE433-.LLFB433
	.byte	0x4
	.uaword	.LLCFI433-.LLFB433
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE127:
.LLSFDE129:
	.uaword	.LLEFDE129-.LLASFDE129
.LLASFDE129:
	.uaword	.LLASFDE129-.LLframe1
	.uaword	.LLFB483
	.uaword	.LLFE483-.LLFB483
	.byte	0x4
	.uaword	.LLCFI483-.LLFB483
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE129:
.LLSFDE131:
	.uaword	.LLEFDE131-.LLASFDE131
.LLASFDE131:
	.uaword	.LLASFDE131-.LLframe1
	.uaword	.LLFB485
	.uaword	.LLFE485-.LLFB485
	.byte	0x4
	.uaword	.LLCFI485-.LLFB485
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE131:
.LLSFDE133:
	.uaword	.LLEFDE133-.LLASFDE133
.LLASFDE133:
	.uaword	.LLASFDE133-.LLframe1
	.uaword	.LLFB495
	.uaword	.LLFE495-.LLFB495
	.byte	0x4
	.uaword	.LLCFI495-.LLFB495
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE133:
.LLSFDE135:
	.uaword	.LLEFDE135-.LLASFDE135
.LLASFDE135:
	.uaword	.LLASFDE135-.LLframe1
	.uaword	.LLFB497
	.uaword	.LLFE497-.LLFB497
	.byte	0x4
	.uaword	.LLCFI497-.LLFB497
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE135:
.LLSFDE137:
	.uaword	.LLEFDE137-.LLASFDE137
.LLASFDE137:
	.uaword	.LLASFDE137-.LLframe1
	.uaword	.LLFB499
	.uaword	.LLFE499-.LLFB499
	.byte	0x4
	.uaword	.LLCFI499-.LLFB499
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE137:
.LLSFDE139:
	.uaword	.LLEFDE139-.LLASFDE139
.LLASFDE139:
	.uaword	.LLASFDE139-.LLframe1
	.uaword	.LLFB501
	.uaword	.LLFE501-.LLFB501
	.byte	0x4
	.uaword	.LLCFI501-.LLFB501
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE139:
.LLSFDE141:
	.uaword	.LLEFDE141-.LLASFDE141
.LLASFDE141:
	.uaword	.LLASFDE141-.LLframe1
	.uaword	.LLFB503
	.uaword	.LLFE503-.LLFB503
	.byte	0x4
	.uaword	.LLCFI503-.LLFB503
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE141:
.LLSFDE143:
	.uaword	.LLEFDE143-.LLASFDE143
.LLASFDE143:
	.uaword	.LLASFDE143-.LLframe1
	.uaword	.LLFB505
	.uaword	.LLFE505-.LLFB505
	.byte	0x4
	.uaword	.LLCFI505-.LLFB505
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE143:
.LLSFDE145:
	.uaword	.LLEFDE145-.LLASFDE145
.LLASFDE145:
	.uaword	.LLASFDE145-.LLframe1
	.uaword	.LLFB511
	.uaword	.LLFE511-.LLFB511
	.byte	0x4
	.uaword	.LLCFI511-.LLFB511
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE145:
.LLSFDE147:
	.uaword	.LLEFDE147-.LLASFDE147
.LLASFDE147:
	.uaword	.LLASFDE147-.LLframe1
	.uaword	.LLFB513
	.uaword	.LLFE513-.LLFB513
	.byte	0x4
	.uaword	.LLCFI513-.LLFB513
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE147:
.LLSFDE149:
	.uaword	.LLEFDE149-.LLASFDE149
.LLASFDE149:
	.uaword	.LLASFDE149-.LLframe1
	.uaword	.LLFB515
	.uaword	.LLFE515-.LLFB515
	.byte	0x4
	.uaword	.LLCFI515-.LLFB515
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE149:
.LLSFDE151:
	.uaword	.LLEFDE151-.LLASFDE151
.LLASFDE151:
	.uaword	.LLASFDE151-.LLframe1
	.uaword	.LLFB517
	.uaword	.LLFE517-.LLFB517
	.byte	0x4
	.uaword	.LLCFI517-.LLFB517
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE151:
.LLSFDE153:
	.uaword	.LLEFDE153-.LLASFDE153
.LLASFDE153:
	.uaword	.LLASFDE153-.LLframe1
	.uaword	.LLFB523
	.uaword	.LLFE523-.LLFB523
	.byte	0x4
	.uaword	.LLCFI523-.LLFB523
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE153:
.LLSFDE155:
	.uaword	.LLEFDE155-.LLASFDE155
.LLASFDE155:
	.uaword	.LLASFDE155-.LLframe1
	.uaword	.LLFB525
	.uaword	.LLFE525-.LLFB525
	.byte	0x4
	.uaword	.LLCFI525-.LLFB525
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE155:
.LLSFDE157:
	.uaword	.LLEFDE157-.LLASFDE157
.LLASFDE157:
	.uaword	.LLASFDE157-.LLframe1
	.uaword	.LLFB527
	.uaword	.LLFE527-.LLFB527
	.byte	0x4
	.uaword	.LLCFI527-.LLFB527
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE157:
.LLSFDE159:
	.uaword	.LLEFDE159-.LLASFDE159
.LLASFDE159:
	.uaword	.LLASFDE159-.LLframe1
	.uaword	.LLFB529
	.uaword	.LLFE529-.LLFB529
	.byte	0x4
	.uaword	.LLCFI529-.LLFB529
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE159:
.LLSFDE161:
	.uaword	.LLEFDE161-.LLASFDE161
.LLASFDE161:
	.uaword	.LLASFDE161-.LLframe1
	.uaword	.LLFB581
	.uaword	.LLFE581-.LLFB581
	.byte	0x4
	.uaword	.LLCFI581-.LLFB581
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE161:
	.ident	"GCC: (GNU) 4.5.1"
