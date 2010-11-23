	.file	"Times.mc"
	.section	".text"
	.align 4
	.type	Times__uTimes_var_u16_u16, #function
	.proc	04
Times__uTimes_var_u16_u16:
.LLFB0:
	save	%sp, -96, %sp
.LLCFI0:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE0:
	.size	Times__uTimes_var_u16_u16, .-Times__uTimes_var_u16_u16
	.align 4
	.type	Times__Times_var_u16_u16, #function
	.proc	015
Times__Times_var_u16_u16:
.LLFB1:
	save	%sp, -96, %sp
.LLCFI1:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL3
	 nop
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	smul	%o5, %g1, %o5
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
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1:
	.size	Times__Times_var_u16_u16, .-Times__Times_var_u16_u16
	.align 4
	.type	Times__uTimes_param_u16_u16, #function
	.proc	04
Times__uTimes_param_u16_u16:
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
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE2:
	.size	Times__uTimes_param_u16_u16, .-Times__uTimes_param_u16_u16
	.align 4
	.type	Times__Times_param_u16_u16, #function
	.proc	015
Times__Times_param_u16_u16:
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
	smul	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL7
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	smul	%o5, %g1, %o5
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
	lduh	[%fp+68], %o5
	lduh	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE3:
	.size	Times__Times_param_u16_u16, .-Times__Times_param_u16_u16
	.align 4
	.type	Times__uTimes_var_u16_u64, #function
	.proc	05
Times__uTimes_var_u16_u64:
.LLFB4:
	save	%sp, -96, %sp
.LLCFI4:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE4:
	.size	Times__uTimes_var_u16_u64, .-Times__uTimes_var_u16_u64
	.align 4
	.type	Times__Times_var_u16_u64, #function
	.proc	05
Times__Times_var_u16_u64:
.LLFB5:
	save	%sp, -96, %sp
.LLCFI5:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE5:
	.size	Times__Times_var_u16_u64, .-Times__Times_var_u16_u64
	.align 4
	.type	Times__uTimes_param_u16_u64, #function
	.proc	05
Times__uTimes_param_u16_u64:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE6:
	.size	Times__uTimes_param_u16_u64, .-Times__uTimes_param_u16_u64
	.align 4
	.type	Times__Times_param_u16_u64, #function
	.proc	05
Times__Times_param_u16_u64:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE7:
	.size	Times__Times_param_u16_u64, .-Times__Times_param_u16_u64
	.align 4
	.type	Times__uTimes_var_u16_LC, #function
	.proc	05
Times__uTimes_var_u16_LC:
.LLFB8:
	save	%sp, -96, %sp
.LLCFI8:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE8:
	.size	Times__uTimes_var_u16_LC, .-Times__uTimes_var_u16_LC
	.align 4
	.type	Times__Times_var_u16_LC, #function
	.proc	05
Times__Times_var_u16_LC:
.LLFB9:
	save	%sp, -96, %sp
.LLCFI9:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE9:
	.size	Times__Times_var_u16_LC, .-Times__Times_var_u16_LC
	.align 4
	.type	Times__uTimes_param_u16_LC, #function
	.proc	05
Times__uTimes_param_u16_LC:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE10:
	.size	Times__uTimes_param_u16_LC, .-Times__uTimes_param_u16_LC
	.align 4
	.type	Times__Times_param_u16_LC, #function
	.proc	05
Times__Times_param_u16_LC:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE11:
	.size	Times__Times_param_u16_LC, .-Times__Times_param_u16_LC
	.align 4
	.type	Times__uTimes_var_u16_i32, #function
	.proc	04
Times__uTimes_var_u16_i32:
.LLFB12:
	save	%sp, -96, %sp
.LLCFI12:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE12:
	.size	Times__uTimes_var_u16_i32, .-Times__uTimes_var_u16_i32
	.align 4
	.type	Times__Times_var_u16_i32, #function
	.proc	015
Times__Times_var_u16_i32:
.LLFB13:
	save	%sp, -96, %sp
.LLCFI13:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL19
	 nop
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	smul	%o5, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL20
	 nop
.LL19:
	mov	2145, %o0
	call	_m3_fault, 0
	 nop
.LL20:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE13:
	.size	Times__Times_var_u16_i32, .-Times__Times_var_u16_i32
	.align 4
	.type	Times__uTimes_param_u16_i32, #function
	.proc	04
Times__uTimes_param_u16_i32:
.LLFB14:
	save	%sp, -96, %sp
.LLCFI14:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE14:
	.size	Times__uTimes_param_u16_i32, .-Times__uTimes_param_u16_i32
	.align 4
	.type	Times__Times_param_u16_i32, #function
	.proc	015
Times__Times_param_u16_i32:
.LLFB15:
	save	%sp, -96, %sp
.LLCFI15:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL23
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL24
	 nop
.LL23:
	mov	2209, %o0
	call	_m3_fault, 0
	 nop
.LL24:
	ld	[%fp+72], %g1
	mov	%g1, %o5
	lduh	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE15:
	.size	Times__Times_param_u16_i32, .-Times__Times_param_u16_i32
	.align 4
	.type	Times__uTimes_var_u16_i16, #function
	.proc	04
Times__uTimes_var_u16_i16:
.LLFB16:
	save	%sp, -96, %sp
.LLCFI16:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE16:
	.size	Times__uTimes_var_u16_i16, .-Times__uTimes_var_u16_i16
	.align 4
	.type	Times__Times_var_u16_i16, #function
	.proc	015
Times__Times_var_u16_i16:
.LLFB17:
	save	%sp, -96, %sp
.LLCFI17:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL27
	 nop
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	smul	%o5, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL28
	 nop
.LL27:
	mov	2273, %o0
	call	_m3_fault, 0
	 nop
.LL28:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE17:
	.size	Times__Times_var_u16_i16, .-Times__Times_var_u16_i16
	.align 4
	.type	Times__uTimes_param_u16_i16, #function
	.proc	04
Times__uTimes_param_u16_i16:
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
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE18:
	.size	Times__uTimes_param_u16_i16, .-Times__uTimes_param_u16_i16
	.align 4
	.type	Times__Times_param_u16_i16, #function
	.proc	015
Times__Times_param_u16_i16:
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
	smul	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL31
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	smul	%o5, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL32
	 nop
.LL31:
	mov	2337, %o0
	call	_m3_fault, 0
	 nop
.LL32:
	lduh	[%fp+72], %g1
	lduh	[%fp+68], %o5
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE19:
	.size	Times__Times_param_u16_i16, .-Times__Times_param_u16_i16
	.align 4
	.type	Times__uTimes_var_u16_I, #function
	.proc	04
Times__uTimes_var_u16_I:
.LLFB20:
	save	%sp, -96, %sp
.LLCFI20:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE20:
	.size	Times__uTimes_var_u16_I, .-Times__uTimes_var_u16_I
	.align 4
	.type	Times__Times_var_u16_I, #function
	.proc	015
Times__Times_var_u16_I:
.LLFB21:
	save	%sp, -96, %sp
.LLCFI21:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL35
	 nop
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	smul	%o5, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL36
	 nop
.LL35:
	mov	2401, %o0
	call	_m3_fault, 0
	 nop
.LL36:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE21:
	.size	Times__Times_var_u16_I, .-Times__Times_var_u16_I
	.align 4
	.type	Times__uTimes_param_u16_I, #function
	.proc	04
Times__uTimes_param_u16_I:
.LLFB22:
	save	%sp, -96, %sp
.LLCFI22:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE22:
	.size	Times__uTimes_param_u16_I, .-Times__uTimes_param_u16_I
	.align 4
	.type	Times__Times_param_u16_I, #function
	.proc	015
Times__Times_param_u16_I:
.LLFB23:
	save	%sp, -96, %sp
.LLCFI23:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL39
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL40
	 nop
.LL39:
	mov	2465, %o0
	call	_m3_fault, 0
	 nop
.LL40:
	ld	[%fp+72], %g1
	mov	%g1, %o5
	lduh	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE23:
	.size	Times__Times_param_u16_I, .-Times__Times_param_u16_I
	.align 4
	.type	Times__uTimes_var_u16_i64, #function
	.proc	05
Times__uTimes_var_u16_i64:
.LLFB24:
	save	%sp, -96, %sp
.LLCFI24:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE24:
	.size	Times__uTimes_var_u16_i64, .-Times__uTimes_var_u16_i64
	.align 4
	.type	Times__Times_var_u16_i64, #function
	.proc	05
Times__Times_var_u16_i64:
.LLFB25:
	save	%sp, -96, %sp
.LLCFI25:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE25:
	.size	Times__Times_var_u16_i64, .-Times__Times_var_u16_i64
	.align 4
	.type	Times__uTimes_param_u16_i64, #function
	.proc	05
Times__uTimes_param_u16_i64:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE26:
	.size	Times__uTimes_param_u16_i64, .-Times__uTimes_param_u16_i64
	.align 4
	.type	Times__Times_param_u16_i64, #function
	.proc	05
Times__Times_param_u16_i64:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE27:
	.size	Times__Times_param_u16_i64, .-Times__Times_param_u16_i64
	.align 4
	.type	Times__uTimes_var_u16_C, #function
	.proc	04
Times__uTimes_var_u16_C:
.LLFB28:
	save	%sp, -96, %sp
.LLCFI28:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE28:
	.size	Times__uTimes_var_u16_C, .-Times__uTimes_var_u16_C
	.align 4
	.type	Times__Times_var_u16_C, #function
	.proc	015
Times__Times_var_u16_C:
.LLFB29:
	save	%sp, -96, %sp
.LLCFI29:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL47
	 nop
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	smul	%o5, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL48
	 nop
.LL47:
	mov	2657, %o0
	call	_m3_fault, 0
	 nop
.LL48:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE29:
	.size	Times__Times_var_u16_C, .-Times__Times_var_u16_C
	.align 4
	.type	Times__uTimes_param_u16_C, #function
	.proc	04
Times__uTimes_param_u16_C:
.LLFB30:
	save	%sp, -96, %sp
.LLCFI30:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE30:
	.size	Times__uTimes_param_u16_C, .-Times__uTimes_param_u16_C
	.align 4
	.type	Times__Times_param_u16_C, #function
	.proc	015
Times__Times_param_u16_C:
.LLFB31:
	save	%sp, -96, %sp
.LLCFI31:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL51
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL52
	 nop
.LL51:
	mov	2721, %o0
	call	_m3_fault, 0
	 nop
.LL52:
	ld	[%fp+72], %g1
	mov	%g1, %o5
	lduh	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE31:
	.size	Times__Times_param_u16_C, .-Times__Times_param_u16_C
	.align 4
	.type	Times__uTimes_var_u16_u8, #function
	.proc	04
Times__uTimes_var_u16_u8:
.LLFB32:
	save	%sp, -96, %sp
.LLCFI32:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE32:
	.size	Times__uTimes_var_u16_u8, .-Times__uTimes_var_u16_u8
	.align 4
	.type	Times__Times_var_u16_u8, #function
	.proc	015
Times__Times_var_u16_u8:
.LLFB33:
	save	%sp, -96, %sp
.LLCFI33:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	smul	%o5, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL55
	 nop
	mov	2785, %o0
	call	_m3_fault, 0
	 nop
.LL55:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE33:
	.size	Times__Times_var_u16_u8, .-Times__Times_var_u16_u8
	.align 4
	.type	Times__uTimes_param_u16_u8, #function
	.proc	04
Times__uTimes_param_u16_u8:
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
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE34:
	.size	Times__uTimes_param_u16_u8, .-Times__uTimes_param_u16_u8
	.align 4
	.type	Times__Times_param_u16_u8, #function
	.proc	015
Times__Times_param_u16_u8:
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
	smul	%o5, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL58
	 nop
	mov	2849, %o0
	call	_m3_fault, 0
	 nop
.LL58:
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	lduh	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE35:
	.size	Times__Times_param_u16_u8, .-Times__Times_param_u16_u8
	.align 4
	.type	Times__uTimes_var_u16_L, #function
	.proc	05
Times__uTimes_var_u16_L:
.LLFB36:
	save	%sp, -96, %sp
.LLCFI36:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE36:
	.size	Times__uTimes_var_u16_L, .-Times__uTimes_var_u16_L
	.align 4
	.type	Times__Times_var_u16_L, #function
	.proc	05
Times__Times_var_u16_L:
.LLFB37:
	save	%sp, -96, %sp
.LLCFI37:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE37:
	.size	Times__Times_var_u16_L, .-Times__Times_var_u16_L
	.align 4
	.type	Times__uTimes_param_u16_L, #function
	.proc	05
Times__uTimes_param_u16_L:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE38:
	.size	Times__uTimes_param_u16_L, .-Times__uTimes_param_u16_L
	.align 4
	.type	Times__Times_param_u16_L, #function
	.proc	05
Times__Times_param_u16_L:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE39:
	.size	Times__Times_param_u16_L, .-Times__Times_param_u16_L
	.align 4
	.type	Times__uTimes_var_u16_i8, #function
	.proc	04
Times__uTimes_var_u16_i8:
.LLFB40:
	save	%sp, -96, %sp
.LLCFI40:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE40:
	.size	Times__uTimes_var_u16_i8, .-Times__uTimes_var_u16_i8
	.align 4
	.type	Times__Times_var_u16_i8, #function
	.proc	015
Times__Times_var_u16_i8:
.LLFB41:
	save	%sp, -96, %sp
.LLCFI41:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL65
	 nop
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	smul	%o5, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL66
	 nop
.LL65:
	mov	3041, %o0
	call	_m3_fault, 0
	 nop
.LL66:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE41:
	.size	Times__Times_var_u16_i8, .-Times__Times_var_u16_i8
	.align 4
	.type	Times__uTimes_param_u16_i8, #function
	.proc	04
Times__uTimes_param_u16_i8:
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
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE42:
	.size	Times__uTimes_param_u16_i8, .-Times__uTimes_param_u16_i8
	.align 4
	.type	Times__Times_param_u16_i8, #function
	.proc	015
Times__Times_param_u16_i8:
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
	smul	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL69
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	smul	%o5, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL70
	 nop
.LL69:
	mov	3105, %o0
	call	_m3_fault, 0
	 nop
.LL70:
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	lduh	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE43:
	.size	Times__Times_param_u16_i8, .-Times__Times_param_u16_i8
	.align 4
	.type	Times__uTimes_var_u16_u32, #function
	.proc	04
Times__uTimes_var_u16_u32:
.LLFB44:
	save	%sp, -96, %sp
.LLCFI44:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE44:
	.size	Times__uTimes_var_u16_u32, .-Times__uTimes_var_u16_u32
	.align 4
	.type	Times__Times_var_u16_u32, #function
	.proc	015
Times__Times_var_u16_u32:
.LLFB45:
	save	%sp, -96, %sp
.LLCFI45:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL73
	 nop
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	smul	%o5, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL74
	 nop
.LL73:
	mov	3169, %o0
	call	_m3_fault, 0
	 nop
.LL74:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE45:
	.size	Times__Times_var_u16_u32, .-Times__Times_var_u16_u32
	.align 4
	.type	Times__uTimes_param_u16_u32, #function
	.proc	04
Times__uTimes_param_u16_u32:
.LLFB46:
	save	%sp, -96, %sp
.LLCFI46:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE46:
	.size	Times__uTimes_param_u16_u32, .-Times__uTimes_param_u16_u32
	.align 4
	.type	Times__Times_param_u16_u32, #function
	.proc	015
Times__Times_param_u16_u32:
.LLFB47:
	save	%sp, -96, %sp
.LLCFI47:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL77
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL78
	 nop
.LL77:
	mov	3233, %o0
	call	_m3_fault, 0
	 nop
.LL78:
	ld	[%fp+72], %g1
	mov	%g1, %o5
	lduh	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE47:
	.size	Times__Times_param_u16_u32, .-Times__Times_param_u16_u32
	.align 4
	.type	Times__uTimes_var_u64_u16, #function
	.proc	05
Times__uTimes_var_u64_u16:
.LLFB48:
	save	%sp, -96, %sp
.LLCFI48:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE48:
	.size	Times__uTimes_var_u64_u16, .-Times__uTimes_var_u64_u16
	.align 4
	.type	Times__Times_var_u64_u16, #function
	.proc	05
Times__Times_var_u64_u16:
.LLFB49:
	save	%sp, -96, %sp
.LLCFI49:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE49:
	.size	Times__Times_var_u64_u16, .-Times__Times_var_u64_u16
	.align 4
	.type	Times__uTimes_param_u64_u16, #function
	.proc	05
Times__uTimes_param_u64_u16:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE50:
	.size	Times__uTimes_param_u64_u16, .-Times__uTimes_param_u64_u16
	.align 4
	.type	Times__Times_param_u64_u16, #function
	.proc	05
Times__Times_param_u64_u16:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE51:
	.size	Times__Times_param_u64_u16, .-Times__Times_param_u64_u16
	.align 4
	.type	Times__uTimes_var_u64_u64, #function
	.proc	05
Times__uTimes_var_u64_u64:
.LLFB52:
	save	%sp, -96, %sp
.LLCFI52:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o4
	sllx	%o4, 32, %o4
	srl	%o3, 0, %o3
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE52:
	.size	Times__uTimes_var_u64_u64, .-Times__uTimes_var_u64_u64
	.align 4
	.type	Times__Times_var_u64_u64, #function
	.proc	05
Times__Times_var_u64_u64:
.LLFB53:
	save	%sp, -96, %sp
.LLCFI53:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o2
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE53:
	.size	Times__Times_var_u64_u64, .-Times__Times_var_u64_u64
	.align 4
	.type	Times__uTimes_param_u64_u64, #function
	.proc	05
Times__uTimes_param_u64_u64:
.LLFB54:
	save	%sp, -112, %sp
.LLCFI54:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE54:
	.size	Times__uTimes_param_u64_u64, .-Times__uTimes_param_u64_u64
	.align 4
	.type	Times__Times_param_u64_u64, #function
	.proc	05
Times__Times_param_u64_u64:
.LLFB55:
	save	%sp, -112, %sp
.LLCFI55:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE55:
	.size	Times__Times_param_u64_u64, .-Times__Times_param_u64_u64
	.align 4
	.type	Times__uTimes_var_u64_LC, #function
	.proc	05
Times__uTimes_var_u64_LC:
.LLFB56:
	save	%sp, -96, %sp
.LLCFI56:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o4
	sllx	%o4, 32, %o4
	srl	%o3, 0, %o3
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE56:
	.size	Times__uTimes_var_u64_LC, .-Times__uTimes_var_u64_LC
	.align 4
	.type	Times__Times_var_u64_LC, #function
	.proc	05
Times__Times_var_u64_LC:
.LLFB57:
	save	%sp, -96, %sp
.LLCFI57:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o2
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE57:
	.size	Times__Times_var_u64_LC, .-Times__Times_var_u64_LC
	.align 4
	.type	Times__uTimes_param_u64_LC, #function
	.proc	05
Times__uTimes_param_u64_LC:
.LLFB58:
	save	%sp, -112, %sp
.LLCFI58:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE58:
	.size	Times__uTimes_param_u64_LC, .-Times__uTimes_param_u64_LC
	.align 4
	.type	Times__Times_param_u64_LC, #function
	.proc	05
Times__Times_param_u64_LC:
.LLFB59:
	save	%sp, -112, %sp
.LLCFI59:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o2
	ldd	[%fp-8], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE59:
	.size	Times__Times_param_u64_LC, .-Times__Times_param_u64_LC
	.align 4
	.type	Times__uTimes_var_u64_i32, #function
	.proc	05
Times__uTimes_var_u64_i32:
.LLFB60:
	save	%sp, -96, %sp
.LLCFI60:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE60:
	.size	Times__uTimes_var_u64_i32, .-Times__uTimes_var_u64_i32
	.align 4
	.type	Times__Times_var_u64_i32, #function
	.proc	05
Times__Times_var_u64_i32:
.LLFB61:
	save	%sp, -96, %sp
.LLCFI61:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE61:
	.size	Times__Times_var_u64_i32, .-Times__Times_var_u64_i32
	.align 4
	.type	Times__uTimes_param_u64_i32, #function
	.proc	05
Times__uTimes_param_u64_i32:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE62:
	.size	Times__uTimes_param_u64_i32, .-Times__uTimes_param_u64_i32
	.align 4
	.type	Times__Times_param_u64_i32, #function
	.proc	05
Times__Times_param_u64_i32:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE63:
	.size	Times__Times_param_u64_i32, .-Times__Times_param_u64_i32
	.align 4
	.type	Times__uTimes_var_u64_i16, #function
	.proc	05
Times__uTimes_var_u64_i16:
.LLFB64:
	save	%sp, -96, %sp
.LLCFI64:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE64:
	.size	Times__uTimes_var_u64_i16, .-Times__uTimes_var_u64_i16
	.align 4
	.type	Times__Times_var_u64_i16, #function
	.proc	05
Times__Times_var_u64_i16:
.LLFB65:
	save	%sp, -96, %sp
.LLCFI65:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE65:
	.size	Times__Times_var_u64_i16, .-Times__Times_var_u64_i16
	.align 4
	.type	Times__uTimes_param_u64_i16, #function
	.proc	05
Times__uTimes_param_u64_i16:
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
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE66:
	.size	Times__uTimes_param_u64_i16, .-Times__uTimes_param_u64_i16
	.align 4
	.type	Times__Times_param_u64_i16, #function
	.proc	05
Times__Times_param_u64_i16:
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
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE67:
	.size	Times__Times_param_u64_i16, .-Times__Times_param_u64_i16
	.align 4
	.type	Times__uTimes_var_u64_I, #function
	.proc	05
Times__uTimes_var_u64_I:
.LLFB68:
	save	%sp, -96, %sp
.LLCFI68:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE68:
	.size	Times__uTimes_var_u64_I, .-Times__uTimes_var_u64_I
	.align 4
	.type	Times__Times_var_u64_I, #function
	.proc	05
Times__Times_var_u64_I:
.LLFB69:
	save	%sp, -96, %sp
.LLCFI69:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE69:
	.size	Times__Times_var_u64_I, .-Times__Times_var_u64_I
	.align 4
	.type	Times__uTimes_param_u64_I, #function
	.proc	05
Times__uTimes_param_u64_I:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE70:
	.size	Times__uTimes_param_u64_I, .-Times__uTimes_param_u64_I
	.align 4
	.type	Times__Times_param_u64_I, #function
	.proc	05
Times__Times_param_u64_I:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE71:
	.size	Times__Times_param_u64_I, .-Times__Times_param_u64_I
	.align 4
	.type	Times__uTimes_var_u64_i64, #function
	.proc	05
Times__uTimes_var_u64_i64:
.LLFB72:
	save	%sp, -96, %sp
.LLCFI72:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o4
	sllx	%o4, 32, %o4
	srl	%o3, 0, %o3
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE72:
	.size	Times__uTimes_var_u64_i64, .-Times__uTimes_var_u64_i64
	.align 4
	.type	Times__Times_var_u64_i64, #function
	.proc	05
Times__Times_var_u64_i64:
.LLFB73:
	save	%sp, -96, %sp
.LLCFI73:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o2
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE73:
	.size	Times__Times_var_u64_i64, .-Times__Times_var_u64_i64
	.align 4
	.type	Times__uTimes_param_u64_i64, #function
	.proc	05
Times__uTimes_param_u64_i64:
.LLFB74:
	save	%sp, -112, %sp
.LLCFI74:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE74:
	.size	Times__uTimes_param_u64_i64, .-Times__uTimes_param_u64_i64
	.align 4
	.type	Times__Times_param_u64_i64, #function
	.proc	05
Times__Times_param_u64_i64:
.LLFB75:
	save	%sp, -112, %sp
.LLCFI75:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE75:
	.size	Times__Times_param_u64_i64, .-Times__Times_param_u64_i64
	.align 4
	.type	Times__uTimes_var_u64_C, #function
	.proc	05
Times__uTimes_var_u64_C:
.LLFB76:
	save	%sp, -96, %sp
.LLCFI76:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE76:
	.size	Times__uTimes_var_u64_C, .-Times__uTimes_var_u64_C
	.align 4
	.type	Times__Times_var_u64_C, #function
	.proc	05
Times__Times_var_u64_C:
.LLFB77:
	save	%sp, -96, %sp
.LLCFI77:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE77:
	.size	Times__Times_var_u64_C, .-Times__Times_var_u64_C
	.align 4
	.type	Times__uTimes_param_u64_C, #function
	.proc	05
Times__uTimes_param_u64_C:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE78:
	.size	Times__uTimes_param_u64_C, .-Times__uTimes_param_u64_C
	.align 4
	.type	Times__Times_param_u64_C, #function
	.proc	05
Times__Times_param_u64_C:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE79:
	.size	Times__Times_param_u64_C, .-Times__Times_param_u64_C
	.align 4
	.type	Times__uTimes_var_u64_u8, #function
	.proc	05
Times__uTimes_var_u64_u8:
.LLFB80:
	save	%sp, -96, %sp
.LLCFI80:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE80:
	.size	Times__uTimes_var_u64_u8, .-Times__uTimes_var_u64_u8
	.align 4
	.type	Times__Times_var_u64_u8, #function
	.proc	05
Times__Times_var_u64_u8:
.LLFB81:
	save	%sp, -96, %sp
.LLCFI81:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE81:
	.size	Times__Times_var_u64_u8, .-Times__Times_var_u64_u8
	.align 4
	.type	Times__uTimes_param_u64_u8, #function
	.proc	05
Times__uTimes_param_u64_u8:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE82:
	.size	Times__uTimes_param_u64_u8, .-Times__uTimes_param_u64_u8
	.align 4
	.type	Times__Times_param_u64_u8, #function
	.proc	05
Times__Times_param_u64_u8:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE83:
	.size	Times__Times_param_u64_u8, .-Times__Times_param_u64_u8
	.align 4
	.type	Times__uTimes_var_u64_L, #function
	.proc	05
Times__uTimes_var_u64_L:
.LLFB84:
	save	%sp, -96, %sp
.LLCFI84:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o4
	sllx	%o4, 32, %o4
	srl	%o3, 0, %o3
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE84:
	.size	Times__uTimes_var_u64_L, .-Times__uTimes_var_u64_L
	.align 4
	.type	Times__Times_var_u64_L, #function
	.proc	05
Times__Times_var_u64_L:
.LLFB85:
	save	%sp, -96, %sp
.LLCFI85:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o2
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE85:
	.size	Times__Times_var_u64_L, .-Times__Times_var_u64_L
	.align 4
	.type	Times__uTimes_param_u64_L, #function
	.proc	05
Times__uTimes_param_u64_L:
.LLFB86:
	save	%sp, -112, %sp
.LLCFI86:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE86:
	.size	Times__uTimes_param_u64_L, .-Times__uTimes_param_u64_L
	.align 4
	.type	Times__Times_param_u64_L, #function
	.proc	05
Times__Times_param_u64_L:
.LLFB87:
	save	%sp, -112, %sp
.LLCFI87:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE87:
	.size	Times__Times_param_u64_L, .-Times__Times_param_u64_L
	.align 4
	.type	Times__uTimes_var_u64_i8, #function
	.proc	05
Times__uTimes_var_u64_i8:
.LLFB88:
	save	%sp, -96, %sp
.LLCFI88:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE88:
	.size	Times__uTimes_var_u64_i8, .-Times__uTimes_var_u64_i8
	.align 4
	.type	Times__Times_var_u64_i8, #function
	.proc	05
Times__Times_var_u64_i8:
.LLFB89:
	save	%sp, -96, %sp
.LLCFI89:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE89:
	.size	Times__Times_var_u64_i8, .-Times__Times_var_u64_i8
	.align 4
	.type	Times__uTimes_param_u64_i8, #function
	.proc	05
Times__uTimes_param_u64_i8:
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
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE90:
	.size	Times__uTimes_param_u64_i8, .-Times__uTimes_param_u64_i8
	.align 4
	.type	Times__Times_param_u64_i8, #function
	.proc	05
Times__Times_param_u64_i8:
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
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE91:
	.size	Times__Times_param_u64_i8, .-Times__Times_param_u64_i8
	.align 4
	.type	Times__uTimes_var_u64_u32, #function
	.proc	05
Times__uTimes_var_u64_u32:
.LLFB92:
	save	%sp, -96, %sp
.LLCFI92:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE92:
	.size	Times__uTimes_var_u64_u32, .-Times__uTimes_var_u64_u32
	.align 4
	.type	Times__Times_var_u64_u32, #function
	.proc	05
Times__Times_var_u64_u32:
.LLFB93:
	save	%sp, -96, %sp
.LLCFI93:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE93:
	.size	Times__Times_var_u64_u32, .-Times__Times_var_u64_u32
	.align 4
	.type	Times__uTimes_param_u64_u32, #function
	.proc	05
Times__uTimes_param_u64_u32:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE94:
	.size	Times__uTimes_param_u64_u32, .-Times__uTimes_param_u64_u32
	.align 4
	.type	Times__Times_param_u64_u32, #function
	.proc	05
Times__Times_param_u64_u32:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE95:
	.size	Times__Times_param_u64_u32, .-Times__Times_param_u64_u32
	.align 4
	.type	Times__uTimes_var_LC_u16, #function
	.proc	05
Times__uTimes_var_LC_u16:
.LLFB96:
	save	%sp, -96, %sp
.LLCFI96:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE96:
	.size	Times__uTimes_var_LC_u16, .-Times__uTimes_var_LC_u16
	.align 4
	.type	Times__Times_var_LC_u16, #function
	.proc	05
Times__Times_var_LC_u16:
.LLFB97:
	save	%sp, -96, %sp
.LLCFI97:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE97:
	.size	Times__Times_var_LC_u16, .-Times__Times_var_LC_u16
	.align 4
	.type	Times__uTimes_param_LC_u16, #function
	.proc	05
Times__uTimes_param_LC_u16:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE98:
	.size	Times__uTimes_param_LC_u16, .-Times__uTimes_param_LC_u16
	.align 4
	.type	Times__Times_param_LC_u16, #function
	.proc	05
Times__Times_param_LC_u16:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE99:
	.size	Times__Times_param_LC_u16, .-Times__Times_param_LC_u16
	.align 4
	.type	Times__uTimes_var_LC_u64, #function
	.proc	05
Times__uTimes_var_LC_u64:
.LLFB100:
	save	%sp, -96, %sp
.LLCFI100:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o2
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE100:
	.size	Times__uTimes_var_LC_u64, .-Times__uTimes_var_LC_u64
	.align 4
	.type	Times__Times_var_LC_u64, #function
	.proc	05
Times__Times_var_LC_u64:
.LLFB101:
	save	%sp, -96, %sp
.LLCFI101:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o4
	sllx	%o4, 32, %o4
	srl	%o3, 0, %o3
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE101:
	.size	Times__Times_var_LC_u64, .-Times__Times_var_LC_u64
	.align 4
	.type	Times__uTimes_param_LC_u64, #function
	.proc	05
Times__uTimes_param_LC_u64:
.LLFB102:
	save	%sp, -112, %sp
.LLCFI102:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o2
	ldd	[%fp-8], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE102:
	.size	Times__uTimes_param_LC_u64, .-Times__uTimes_param_LC_u64
	.align 4
	.type	Times__Times_param_LC_u64, #function
	.proc	05
Times__Times_param_LC_u64:
.LLFB103:
	save	%sp, -112, %sp
.LLCFI103:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE103:
	.size	Times__Times_param_LC_u64, .-Times__Times_param_LC_u64
	.align 4
	.type	Times__uTimes_var_LC_LC, #function
	.proc	05
Times__uTimes_var_LC_LC:
.LLFB104:
	save	%sp, -96, %sp
.LLCFI104:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o2
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE104:
	.size	Times__uTimes_var_LC_LC, .-Times__uTimes_var_LC_LC
	.align 4
	.type	Times__Times_var_LC_LC, #function
	.proc	05
Times__Times_var_LC_LC:
.LLFB105:
	save	%sp, -96, %sp
.LLCFI105:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o4
	sllx	%o4, 32, %o4
	srl	%o3, 0, %o3
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE105:
	.size	Times__Times_var_LC_LC, .-Times__Times_var_LC_LC
	.align 4
	.type	Times__uTimes_param_LC_LC, #function
	.proc	05
Times__uTimes_param_LC_LC:
.LLFB106:
	save	%sp, -112, %sp
.LLCFI106:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE106:
	.size	Times__uTimes_param_LC_LC, .-Times__uTimes_param_LC_LC
	.align 4
	.type	Times__Times_param_LC_LC, #function
	.proc	05
Times__Times_param_LC_LC:
.LLFB107:
	save	%sp, -112, %sp
.LLCFI107:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE107:
	.size	Times__Times_param_LC_LC, .-Times__Times_param_LC_LC
	.align 4
	.type	Times__uTimes_var_LC_i32, #function
	.proc	05
Times__uTimes_var_LC_i32:
.LLFB108:
	save	%sp, -96, %sp
.LLCFI108:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE108:
	.size	Times__uTimes_var_LC_i32, .-Times__uTimes_var_LC_i32
	.align 4
	.type	Times__Times_var_LC_i32, #function
	.proc	05
Times__Times_var_LC_i32:
.LLFB109:
	save	%sp, -96, %sp
.LLCFI109:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE109:
	.size	Times__Times_var_LC_i32, .-Times__Times_var_LC_i32
	.align 4
	.type	Times__uTimes_param_LC_i32, #function
	.proc	05
Times__uTimes_param_LC_i32:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE110:
	.size	Times__uTimes_param_LC_i32, .-Times__uTimes_param_LC_i32
	.align 4
	.type	Times__Times_param_LC_i32, #function
	.proc	05
Times__Times_param_LC_i32:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE111:
	.size	Times__Times_param_LC_i32, .-Times__Times_param_LC_i32
	.align 4
	.type	Times__uTimes_var_LC_i16, #function
	.proc	05
Times__uTimes_var_LC_i16:
.LLFB112:
	save	%sp, -96, %sp
.LLCFI112:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE112:
	.size	Times__uTimes_var_LC_i16, .-Times__uTimes_var_LC_i16
	.align 4
	.type	Times__Times_var_LC_i16, #function
	.proc	05
Times__Times_var_LC_i16:
.LLFB113:
	save	%sp, -96, %sp
.LLCFI113:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE113:
	.size	Times__Times_var_LC_i16, .-Times__Times_var_LC_i16
	.align 4
	.type	Times__uTimes_param_LC_i16, #function
	.proc	05
Times__uTimes_param_LC_i16:
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
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE114:
	.size	Times__uTimes_param_LC_i16, .-Times__uTimes_param_LC_i16
	.align 4
	.type	Times__Times_param_LC_i16, #function
	.proc	05
Times__Times_param_LC_i16:
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
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE115:
	.size	Times__Times_param_LC_i16, .-Times__Times_param_LC_i16
	.align 4
	.type	Times__uTimes_var_LC_I, #function
	.proc	05
Times__uTimes_var_LC_I:
.LLFB116:
	save	%sp, -96, %sp
.LLCFI116:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE116:
	.size	Times__uTimes_var_LC_I, .-Times__uTimes_var_LC_I
	.align 4
	.type	Times__Times_var_LC_I, #function
	.proc	05
Times__Times_var_LC_I:
.LLFB117:
	save	%sp, -96, %sp
.LLCFI117:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE117:
	.size	Times__Times_var_LC_I, .-Times__Times_var_LC_I
	.align 4
	.type	Times__uTimes_param_LC_I, #function
	.proc	05
Times__uTimes_param_LC_I:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE118:
	.size	Times__uTimes_param_LC_I, .-Times__uTimes_param_LC_I
	.align 4
	.type	Times__Times_param_LC_I, #function
	.proc	05
Times__Times_param_LC_I:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE119:
	.size	Times__Times_param_LC_I, .-Times__Times_param_LC_I
	.align 4
	.type	Times__uTimes_var_LC_i64, #function
	.proc	05
Times__uTimes_var_LC_i64:
.LLFB120:
	save	%sp, -96, %sp
.LLCFI120:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o2
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE120:
	.size	Times__uTimes_var_LC_i64, .-Times__uTimes_var_LC_i64
	.align 4
	.type	Times__Times_var_LC_i64, #function
	.proc	05
Times__Times_var_LC_i64:
.LLFB121:
	save	%sp, -96, %sp
.LLCFI121:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o4
	sllx	%o4, 32, %o4
	srl	%o3, 0, %o3
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE121:
	.size	Times__Times_var_LC_i64, .-Times__Times_var_LC_i64
	.align 4
	.type	Times__uTimes_param_LC_i64, #function
	.proc	05
Times__uTimes_param_LC_i64:
.LLFB122:
	save	%sp, -112, %sp
.LLCFI122:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o2
	ldd	[%fp-8], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE122:
	.size	Times__uTimes_param_LC_i64, .-Times__uTimes_param_LC_i64
	.align 4
	.type	Times__Times_param_LC_i64, #function
	.proc	05
Times__Times_param_LC_i64:
.LLFB123:
	save	%sp, -112, %sp
.LLCFI123:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE123:
	.size	Times__Times_param_LC_i64, .-Times__Times_param_LC_i64
	.align 4
	.type	Times__uTimes_var_LC_C, #function
	.proc	05
Times__uTimes_var_LC_C:
.LLFB124:
	save	%sp, -96, %sp
.LLCFI124:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE124:
	.size	Times__uTimes_var_LC_C, .-Times__uTimes_var_LC_C
	.align 4
	.type	Times__Times_var_LC_C, #function
	.proc	05
Times__Times_var_LC_C:
.LLFB125:
	save	%sp, -96, %sp
.LLCFI125:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE125:
	.size	Times__Times_var_LC_C, .-Times__Times_var_LC_C
	.align 4
	.type	Times__uTimes_param_LC_C, #function
	.proc	05
Times__uTimes_param_LC_C:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE126:
	.size	Times__uTimes_param_LC_C, .-Times__uTimes_param_LC_C
	.align 4
	.type	Times__Times_param_LC_C, #function
	.proc	05
Times__Times_param_LC_C:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE127:
	.size	Times__Times_param_LC_C, .-Times__Times_param_LC_C
	.align 4
	.type	Times__uTimes_var_LC_u8, #function
	.proc	05
Times__uTimes_var_LC_u8:
.LLFB128:
	save	%sp, -96, %sp
.LLCFI128:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE128:
	.size	Times__uTimes_var_LC_u8, .-Times__uTimes_var_LC_u8
	.align 4
	.type	Times__Times_var_LC_u8, #function
	.proc	05
Times__Times_var_LC_u8:
.LLFB129:
	save	%sp, -96, %sp
.LLCFI129:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE129:
	.size	Times__Times_var_LC_u8, .-Times__Times_var_LC_u8
	.align 4
	.type	Times__uTimes_param_LC_u8, #function
	.proc	05
Times__uTimes_param_LC_u8:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE130:
	.size	Times__uTimes_param_LC_u8, .-Times__uTimes_param_LC_u8
	.align 4
	.type	Times__Times_param_LC_u8, #function
	.proc	05
Times__Times_param_LC_u8:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE131:
	.size	Times__Times_param_LC_u8, .-Times__Times_param_LC_u8
	.align 4
	.type	Times__uTimes_var_LC_L, #function
	.proc	05
Times__uTimes_var_LC_L:
.LLFB132:
	save	%sp, -96, %sp
.LLCFI132:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o2
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE132:
	.size	Times__uTimes_var_LC_L, .-Times__uTimes_var_LC_L
	.align 4
	.type	Times__Times_var_LC_L, #function
	.proc	05
Times__Times_var_LC_L:
.LLFB133:
	save	%sp, -96, %sp
.LLCFI133:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o4
	sllx	%o4, 32, %o4
	srl	%o3, 0, %o3
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE133:
	.size	Times__Times_var_LC_L, .-Times__Times_var_LC_L
	.align 4
	.type	Times__uTimes_param_LC_L, #function
	.proc	05
Times__uTimes_param_LC_L:
.LLFB134:
	save	%sp, -112, %sp
.LLCFI134:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o2
	ldd	[%fp-8], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE134:
	.size	Times__uTimes_param_LC_L, .-Times__uTimes_param_LC_L
	.align 4
	.type	Times__Times_param_LC_L, #function
	.proc	05
Times__Times_param_LC_L:
.LLFB135:
	save	%sp, -112, %sp
.LLCFI135:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE135:
	.size	Times__Times_param_LC_L, .-Times__Times_param_LC_L
	.align 4
	.type	Times__uTimes_var_LC_i8, #function
	.proc	05
Times__uTimes_var_LC_i8:
.LLFB136:
	save	%sp, -96, %sp
.LLCFI136:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE136:
	.size	Times__uTimes_var_LC_i8, .-Times__uTimes_var_LC_i8
	.align 4
	.type	Times__Times_var_LC_i8, #function
	.proc	05
Times__Times_var_LC_i8:
.LLFB137:
	save	%sp, -96, %sp
.LLCFI137:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE137:
	.size	Times__Times_var_LC_i8, .-Times__Times_var_LC_i8
	.align 4
	.type	Times__uTimes_param_LC_i8, #function
	.proc	05
Times__uTimes_param_LC_i8:
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
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE138:
	.size	Times__uTimes_param_LC_i8, .-Times__uTimes_param_LC_i8
	.align 4
	.type	Times__Times_param_LC_i8, #function
	.proc	05
Times__Times_param_LC_i8:
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
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE139:
	.size	Times__Times_param_LC_i8, .-Times__Times_param_LC_i8
	.align 4
	.type	Times__uTimes_var_LC_u32, #function
	.proc	05
Times__uTimes_var_LC_u32:
.LLFB140:
	save	%sp, -96, %sp
.LLCFI140:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE140:
	.size	Times__uTimes_var_LC_u32, .-Times__uTimes_var_LC_u32
	.align 4
	.type	Times__Times_var_LC_u32, #function
	.proc	05
Times__Times_var_LC_u32:
.LLFB141:
	save	%sp, -96, %sp
.LLCFI141:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE141:
	.size	Times__Times_var_LC_u32, .-Times__Times_var_LC_u32
	.align 4
	.type	Times__uTimes_param_LC_u32, #function
	.proc	05
Times__uTimes_param_LC_u32:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE142:
	.size	Times__uTimes_param_LC_u32, .-Times__uTimes_param_LC_u32
	.align 4
	.type	Times__Times_param_LC_u32, #function
	.proc	05
Times__Times_param_LC_u32:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE143:
	.size	Times__Times_param_LC_u32, .-Times__Times_param_LC_u32
	.align 4
	.type	Times__uTimes_var_i32_u16, #function
	.proc	04
Times__uTimes_var_i32_u16:
.LLFB144:
	save	%sp, -96, %sp
.LLCFI144:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE144:
	.size	Times__uTimes_var_i32_u16, .-Times__uTimes_var_i32_u16
	.align 4
	.type	Times__Times_var_i32_u16, #function
	.proc	04
Times__Times_var_i32_u16:
.LLFB145:
	save	%sp, -96, %sp
.LLCFI145:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE145:
	.size	Times__Times_var_i32_u16, .-Times__Times_var_i32_u16
	.align 4
	.type	Times__uTimes_param_i32_u16, #function
	.proc	04
Times__uTimes_param_i32_u16:
.LLFB146:
	save	%sp, -96, %sp
.LLCFI146:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE146:
	.size	Times__uTimes_param_i32_u16, .-Times__uTimes_param_i32_u16
	.align 4
	.type	Times__Times_param_i32_u16, #function
	.proc	04
Times__Times_param_i32_u16:
.LLFB147:
	save	%sp, -96, %sp
.LLCFI147:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE147:
	.size	Times__Times_param_i32_u16, .-Times__Times_param_i32_u16
	.align 4
	.type	Times__uTimes_var_i32_u64, #function
	.proc	05
Times__uTimes_var_i32_u64:
.LLFB148:
	save	%sp, -96, %sp
.LLCFI148:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE148:
	.size	Times__uTimes_var_i32_u64, .-Times__uTimes_var_i32_u64
	.align 4
	.type	Times__Times_var_i32_u64, #function
	.proc	05
Times__Times_var_i32_u64:
.LLFB149:
	save	%sp, -96, %sp
.LLCFI149:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE149:
	.size	Times__Times_var_i32_u64, .-Times__Times_var_i32_u64
	.align 4
	.type	Times__uTimes_param_i32_u64, #function
	.proc	05
Times__uTimes_param_i32_u64:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE150:
	.size	Times__uTimes_param_i32_u64, .-Times__uTimes_param_i32_u64
	.align 4
	.type	Times__Times_param_i32_u64, #function
	.proc	05
Times__Times_param_i32_u64:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE151:
	.size	Times__Times_param_i32_u64, .-Times__Times_param_i32_u64
	.align 4
	.type	Times__uTimes_var_i32_LC, #function
	.proc	05
Times__uTimes_var_i32_LC:
.LLFB152:
	save	%sp, -96, %sp
.LLCFI152:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE152:
	.size	Times__uTimes_var_i32_LC, .-Times__uTimes_var_i32_LC
	.align 4
	.type	Times__Times_var_i32_LC, #function
	.proc	05
Times__Times_var_i32_LC:
.LLFB153:
	save	%sp, -96, %sp
.LLCFI153:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE153:
	.size	Times__Times_var_i32_LC, .-Times__Times_var_i32_LC
	.align 4
	.type	Times__uTimes_param_i32_LC, #function
	.proc	05
Times__uTimes_param_i32_LC:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE154:
	.size	Times__uTimes_param_i32_LC, .-Times__uTimes_param_i32_LC
	.align 4
	.type	Times__Times_param_i32_LC, #function
	.proc	05
Times__Times_param_i32_LC:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE155:
	.size	Times__Times_param_i32_LC, .-Times__Times_param_i32_LC
	.align 4
	.type	Times__uTimes_var_i32_i32, #function
	.proc	04
Times__uTimes_var_i32_i32:
.LLFB156:
	save	%sp, -96, %sp
.LLCFI156:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE156:
	.size	Times__uTimes_var_i32_i32, .-Times__uTimes_var_i32_i32
	.align 4
	.type	Times__Times_var_i32_i32, #function
	.proc	04
Times__Times_var_i32_i32:
.LLFB157:
	save	%sp, -96, %sp
.LLCFI157:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE157:
	.size	Times__Times_var_i32_i32, .-Times__Times_var_i32_i32
	.align 4
	.type	Times__uTimes_param_i32_i32, #function
	.proc	04
Times__uTimes_param_i32_i32:
.LLFB158:
	save	%sp, -96, %sp
.LLCFI158:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE158:
	.size	Times__uTimes_param_i32_i32, .-Times__uTimes_param_i32_i32
	.align 4
	.type	Times__Times_param_i32_i32, #function
	.proc	04
Times__Times_param_i32_i32:
.LLFB159:
	save	%sp, -96, %sp
.LLCFI159:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE159:
	.size	Times__Times_param_i32_i32, .-Times__Times_param_i32_i32
	.align 4
	.type	Times__uTimes_var_i32_i16, #function
	.proc	04
Times__uTimes_var_i32_i16:
.LLFB160:
	save	%sp, -96, %sp
.LLCFI160:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE160:
	.size	Times__uTimes_var_i32_i16, .-Times__uTimes_var_i32_i16
	.align 4
	.type	Times__Times_var_i32_i16, #function
	.proc	04
Times__Times_var_i32_i16:
.LLFB161:
	save	%sp, -96, %sp
.LLCFI161:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE161:
	.size	Times__Times_var_i32_i16, .-Times__Times_var_i32_i16
	.align 4
	.type	Times__uTimes_param_i32_i16, #function
	.proc	04
Times__uTimes_param_i32_i16:
.LLFB162:
	save	%sp, -96, %sp
.LLCFI162:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE162:
	.size	Times__uTimes_param_i32_i16, .-Times__uTimes_param_i32_i16
	.align 4
	.type	Times__Times_param_i32_i16, #function
	.proc	04
Times__Times_param_i32_i16:
.LLFB163:
	save	%sp, -96, %sp
.LLCFI163:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE163:
	.size	Times__Times_param_i32_i16, .-Times__Times_param_i32_i16
	.align 4
	.type	Times__uTimes_var_i32_I, #function
	.proc	04
Times__uTimes_var_i32_I:
.LLFB164:
	save	%sp, -96, %sp
.LLCFI164:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE164:
	.size	Times__uTimes_var_i32_I, .-Times__uTimes_var_i32_I
	.align 4
	.type	Times__Times_var_i32_I, #function
	.proc	04
Times__Times_var_i32_I:
.LLFB165:
	save	%sp, -96, %sp
.LLCFI165:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE165:
	.size	Times__Times_var_i32_I, .-Times__Times_var_i32_I
	.align 4
	.type	Times__uTimes_param_i32_I, #function
	.proc	04
Times__uTimes_param_i32_I:
.LLFB166:
	save	%sp, -96, %sp
.LLCFI166:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE166:
	.size	Times__uTimes_param_i32_I, .-Times__uTimes_param_i32_I
	.align 4
	.type	Times__Times_param_i32_I, #function
	.proc	04
Times__Times_param_i32_I:
.LLFB167:
	save	%sp, -96, %sp
.LLCFI167:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE167:
	.size	Times__Times_param_i32_I, .-Times__Times_param_i32_I
	.align 4
	.type	Times__uTimes_var_i32_i64, #function
	.proc	05
Times__uTimes_var_i32_i64:
.LLFB168:
	save	%sp, -96, %sp
.LLCFI168:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE168:
	.size	Times__uTimes_var_i32_i64, .-Times__uTimes_var_i32_i64
	.align 4
	.type	Times__Times_var_i32_i64, #function
	.proc	05
Times__Times_var_i32_i64:
.LLFB169:
	save	%sp, -96, %sp
.LLCFI169:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE169:
	.size	Times__Times_var_i32_i64, .-Times__Times_var_i32_i64
	.align 4
	.type	Times__uTimes_param_i32_i64, #function
	.proc	05
Times__uTimes_param_i32_i64:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE170:
	.size	Times__uTimes_param_i32_i64, .-Times__uTimes_param_i32_i64
	.align 4
	.type	Times__Times_param_i32_i64, #function
	.proc	05
Times__Times_param_i32_i64:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE171:
	.size	Times__Times_param_i32_i64, .-Times__Times_param_i32_i64
	.align 4
	.type	Times__uTimes_var_i32_C, #function
	.proc	04
Times__uTimes_var_i32_C:
.LLFB172:
	save	%sp, -96, %sp
.LLCFI172:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE172:
	.size	Times__uTimes_var_i32_C, .-Times__uTimes_var_i32_C
	.align 4
	.type	Times__Times_var_i32_C, #function
	.proc	04
Times__Times_var_i32_C:
.LLFB173:
	save	%sp, -96, %sp
.LLCFI173:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE173:
	.size	Times__Times_var_i32_C, .-Times__Times_var_i32_C
	.align 4
	.type	Times__uTimes_param_i32_C, #function
	.proc	04
Times__uTimes_param_i32_C:
.LLFB174:
	save	%sp, -96, %sp
.LLCFI174:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE174:
	.size	Times__uTimes_param_i32_C, .-Times__uTimes_param_i32_C
	.align 4
	.type	Times__Times_param_i32_C, #function
	.proc	04
Times__Times_param_i32_C:
.LLFB175:
	save	%sp, -96, %sp
.LLCFI175:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE175:
	.size	Times__Times_param_i32_C, .-Times__Times_param_i32_C
	.align 4
	.type	Times__uTimes_var_i32_u8, #function
	.proc	04
Times__uTimes_var_i32_u8:
.LLFB176:
	save	%sp, -96, %sp
.LLCFI176:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE176:
	.size	Times__uTimes_var_i32_u8, .-Times__uTimes_var_i32_u8
	.align 4
	.type	Times__Times_var_i32_u8, #function
	.proc	04
Times__Times_var_i32_u8:
.LLFB177:
	save	%sp, -96, %sp
.LLCFI177:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE177:
	.size	Times__Times_var_i32_u8, .-Times__Times_var_i32_u8
	.align 4
	.type	Times__uTimes_param_i32_u8, #function
	.proc	04
Times__uTimes_param_i32_u8:
.LLFB178:
	save	%sp, -96, %sp
.LLCFI178:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE178:
	.size	Times__uTimes_param_i32_u8, .-Times__uTimes_param_i32_u8
	.align 4
	.type	Times__Times_param_i32_u8, #function
	.proc	04
Times__Times_param_i32_u8:
.LLFB179:
	save	%sp, -96, %sp
.LLCFI179:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE179:
	.size	Times__Times_param_i32_u8, .-Times__Times_param_i32_u8
	.align 4
	.type	Times__uTimes_var_i32_L, #function
	.proc	05
Times__uTimes_var_i32_L:
.LLFB180:
	save	%sp, -96, %sp
.LLCFI180:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE180:
	.size	Times__uTimes_var_i32_L, .-Times__uTimes_var_i32_L
	.align 4
	.type	Times__Times_var_i32_L, #function
	.proc	05
Times__Times_var_i32_L:
.LLFB181:
	save	%sp, -96, %sp
.LLCFI181:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE181:
	.size	Times__Times_var_i32_L, .-Times__Times_var_i32_L
	.align 4
	.type	Times__uTimes_param_i32_L, #function
	.proc	05
Times__uTimes_param_i32_L:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE182:
	.size	Times__uTimes_param_i32_L, .-Times__uTimes_param_i32_L
	.align 4
	.type	Times__Times_param_i32_L, #function
	.proc	05
Times__Times_param_i32_L:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE183:
	.size	Times__Times_param_i32_L, .-Times__Times_param_i32_L
	.align 4
	.type	Times__uTimes_var_i32_i8, #function
	.proc	04
Times__uTimes_var_i32_i8:
.LLFB184:
	save	%sp, -96, %sp
.LLCFI184:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE184:
	.size	Times__uTimes_var_i32_i8, .-Times__uTimes_var_i32_i8
	.align 4
	.type	Times__Times_var_i32_i8, #function
	.proc	04
Times__Times_var_i32_i8:
.LLFB185:
	save	%sp, -96, %sp
.LLCFI185:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE185:
	.size	Times__Times_var_i32_i8, .-Times__Times_var_i32_i8
	.align 4
	.type	Times__uTimes_param_i32_i8, #function
	.proc	04
Times__uTimes_param_i32_i8:
.LLFB186:
	save	%sp, -96, %sp
.LLCFI186:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE186:
	.size	Times__uTimes_param_i32_i8, .-Times__uTimes_param_i32_i8
	.align 4
	.type	Times__Times_param_i32_i8, #function
	.proc	04
Times__Times_param_i32_i8:
.LLFB187:
	save	%sp, -96, %sp
.LLCFI187:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE187:
	.size	Times__Times_param_i32_i8, .-Times__Times_param_i32_i8
	.align 4
	.type	Times__uTimes_var_i32_u32, #function
	.proc	04
Times__uTimes_var_i32_u32:
.LLFB188:
	save	%sp, -96, %sp
.LLCFI188:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE188:
	.size	Times__uTimes_var_i32_u32, .-Times__uTimes_var_i32_u32
	.align 4
	.type	Times__Times_var_i32_u32, #function
	.proc	04
Times__Times_var_i32_u32:
.LLFB189:
	save	%sp, -96, %sp
.LLCFI189:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE189:
	.size	Times__Times_var_i32_u32, .-Times__Times_var_i32_u32
	.align 4
	.type	Times__uTimes_param_i32_u32, #function
	.proc	04
Times__uTimes_param_i32_u32:
.LLFB190:
	save	%sp, -96, %sp
.LLCFI190:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE190:
	.size	Times__uTimes_param_i32_u32, .-Times__uTimes_param_i32_u32
	.align 4
	.type	Times__Times_param_i32_u32, #function
	.proc	04
Times__Times_param_i32_u32:
.LLFB191:
	save	%sp, -96, %sp
.LLCFI191:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE191:
	.size	Times__Times_param_i32_u32, .-Times__Times_param_i32_u32
	.align 4
	.type	Times__uTimes_var_i16_u16, #function
	.proc	04
Times__uTimes_var_i16_u16:
.LLFB192:
	save	%sp, -96, %sp
.LLCFI192:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE192:
	.size	Times__uTimes_var_i16_u16, .-Times__uTimes_var_i16_u16
	.align 4
	.type	Times__Times_var_i16_u16, #function
	.proc	03
Times__Times_var_i16_u16:
.LLFB193:
	save	%sp, -96, %sp
.LLCFI193:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	smul	%o5, %g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL225
	 nop
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	smul	%o5, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL226
	 nop
.LL225:
	sethi	%hi(7168), %g1
	or	%g1, 737, %o0
	call	_m3_fault, 0
	 nop
.LL226:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE193:
	.size	Times__Times_var_i16_u16, .-Times__Times_var_i16_u16
	.align 4
	.type	Times__uTimes_param_i16_u16, #function
	.proc	04
Times__uTimes_param_i16_u16:
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
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE194:
	.size	Times__uTimes_param_i16_u16, .-Times__uTimes_param_i16_u16
	.align 4
	.type	Times__Times_param_i16_u16, #function
	.proc	03
Times__Times_param_i16_u16:
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
	smul	%o5, %g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL229
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	smul	%o5, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL230
	 nop
.LL229:
	sethi	%hi(7168), %g1
	or	%g1, 801, %o0
	call	_m3_fault, 0
	 nop
.LL230:
	lduh	[%fp+68], %o5
	lduh	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE195:
	.size	Times__Times_param_i16_u16, .-Times__Times_param_i16_u16
	.align 4
	.type	Times__uTimes_var_i16_u64, #function
	.proc	05
Times__uTimes_var_i16_u64:
.LLFB196:
	save	%sp, -96, %sp
.LLCFI196:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE196:
	.size	Times__uTimes_var_i16_u64, .-Times__uTimes_var_i16_u64
	.align 4
	.type	Times__Times_var_i16_u64, #function
	.proc	05
Times__Times_var_i16_u64:
.LLFB197:
	save	%sp, -96, %sp
.LLCFI197:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE197:
	.size	Times__Times_var_i16_u64, .-Times__Times_var_i16_u64
	.align 4
	.type	Times__uTimes_param_i16_u64, #function
	.proc	05
Times__uTimes_param_i16_u64:
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
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE198:
	.size	Times__uTimes_param_i16_u64, .-Times__uTimes_param_i16_u64
	.align 4
	.type	Times__Times_param_i16_u64, #function
	.proc	05
Times__Times_param_i16_u64:
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
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE199:
	.size	Times__Times_param_i16_u64, .-Times__Times_param_i16_u64
	.align 4
	.type	Times__uTimes_var_i16_LC, #function
	.proc	05
Times__uTimes_var_i16_LC:
.LLFB200:
	save	%sp, -96, %sp
.LLCFI200:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE200:
	.size	Times__uTimes_var_i16_LC, .-Times__uTimes_var_i16_LC
	.align 4
	.type	Times__Times_var_i16_LC, #function
	.proc	05
Times__Times_var_i16_LC:
.LLFB201:
	save	%sp, -96, %sp
.LLCFI201:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE201:
	.size	Times__Times_var_i16_LC, .-Times__Times_var_i16_LC
	.align 4
	.type	Times__uTimes_param_i16_LC, #function
	.proc	05
Times__uTimes_param_i16_LC:
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
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE202:
	.size	Times__uTimes_param_i16_LC, .-Times__uTimes_param_i16_LC
	.align 4
	.type	Times__Times_param_i16_LC, #function
	.proc	05
Times__Times_param_i16_LC:
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
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE203:
	.size	Times__Times_param_i16_LC, .-Times__Times_param_i16_LC
	.align 4
	.type	Times__uTimes_var_i16_i32, #function
	.proc	04
Times__uTimes_var_i16_i32:
.LLFB204:
	save	%sp, -96, %sp
.LLCFI204:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE204:
	.size	Times__uTimes_var_i16_i32, .-Times__uTimes_var_i16_i32
	.align 4
	.type	Times__Times_var_i16_i32, #function
	.proc	03
Times__Times_var_i16_i32:
.LLFB205:
	save	%sp, -96, %sp
.LLCFI205:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	smul	%o5, %g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL241
	 nop
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	smul	%o5, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL242
	 nop
.LL241:
	sethi	%hi(8192), %g1
	or	%g1, 97, %o0
	call	_m3_fault, 0
	 nop
.LL242:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE205:
	.size	Times__Times_var_i16_i32, .-Times__Times_var_i16_i32
	.align 4
	.type	Times__uTimes_param_i16_i32, #function
	.proc	04
Times__uTimes_param_i16_i32:
.LLFB206:
	save	%sp, -96, %sp
.LLCFI206:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE206:
	.size	Times__uTimes_param_i16_i32, .-Times__uTimes_param_i16_i32
	.align 4
	.type	Times__Times_param_i16_i32, #function
	.proc	03
Times__Times_param_i16_i32:
.LLFB207:
	save	%sp, -96, %sp
.LLCFI207:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL245
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL246
	 nop
.LL245:
	sethi	%hi(8192), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL246:
	ld	[%fp+72], %g1
	mov	%g1, %o5
	lduh	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE207:
	.size	Times__Times_param_i16_i32, .-Times__Times_param_i16_i32
	.align 4
	.type	Times__uTimes_var_i16_i16, #function
	.proc	04
Times__uTimes_var_i16_i16:
.LLFB208:
	save	%sp, -96, %sp
.LLCFI208:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE208:
	.size	Times__uTimes_var_i16_i16, .-Times__uTimes_var_i16_i16
	.align 4
	.type	Times__Times_var_i16_i16, #function
	.proc	03
Times__Times_var_i16_i16:
.LLFB209:
	save	%sp, -96, %sp
.LLCFI209:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	smul	%o5, %g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL249
	 nop
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	smul	%o5, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL250
	 nop
.LL249:
	sethi	%hi(8192), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL250:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE209:
	.size	Times__Times_var_i16_i16, .-Times__Times_var_i16_i16
	.align 4
	.type	Times__uTimes_param_i16_i16, #function
	.proc	04
Times__uTimes_param_i16_i16:
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
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE210:
	.size	Times__uTimes_param_i16_i16, .-Times__uTimes_param_i16_i16
	.align 4
	.type	Times__Times_param_i16_i16, #function
	.proc	03
Times__Times_param_i16_i16:
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
	smul	%o5, %g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL253
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	smul	%o5, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL254
	 nop
.LL253:
	sethi	%hi(8192), %g1
	or	%g1, 289, %o0
	call	_m3_fault, 0
	 nop
.LL254:
	lduh	[%fp+68], %o5
	lduh	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE211:
	.size	Times__Times_param_i16_i16, .-Times__Times_param_i16_i16
	.align 4
	.type	Times__uTimes_var_i16_I, #function
	.proc	04
Times__uTimes_var_i16_I:
.LLFB212:
	save	%sp, -96, %sp
.LLCFI212:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE212:
	.size	Times__uTimes_var_i16_I, .-Times__uTimes_var_i16_I
	.align 4
	.type	Times__Times_var_i16_I, #function
	.proc	03
Times__Times_var_i16_I:
.LLFB213:
	save	%sp, -96, %sp
.LLCFI213:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	smul	%o5, %g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL257
	 nop
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	smul	%o5, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL258
	 nop
.LL257:
	sethi	%hi(8192), %g1
	or	%g1, 353, %o0
	call	_m3_fault, 0
	 nop
.LL258:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE213:
	.size	Times__Times_var_i16_I, .-Times__Times_var_i16_I
	.align 4
	.type	Times__uTimes_param_i16_I, #function
	.proc	04
Times__uTimes_param_i16_I:
.LLFB214:
	save	%sp, -96, %sp
.LLCFI214:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE214:
	.size	Times__uTimes_param_i16_I, .-Times__uTimes_param_i16_I
	.align 4
	.type	Times__Times_param_i16_I, #function
	.proc	03
Times__Times_param_i16_I:
.LLFB215:
	save	%sp, -96, %sp
.LLCFI215:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL261
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL262
	 nop
.LL261:
	sethi	%hi(8192), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL262:
	ld	[%fp+72], %g1
	mov	%g1, %o5
	lduh	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE215:
	.size	Times__Times_param_i16_I, .-Times__Times_param_i16_I
	.align 4
	.type	Times__uTimes_var_i16_i64, #function
	.proc	05
Times__uTimes_var_i16_i64:
.LLFB216:
	save	%sp, -96, %sp
.LLCFI216:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE216:
	.size	Times__uTimes_var_i16_i64, .-Times__uTimes_var_i16_i64
	.align 4
	.type	Times__Times_var_i16_i64, #function
	.proc	05
Times__Times_var_i16_i64:
.LLFB217:
	save	%sp, -96, %sp
.LLCFI217:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE217:
	.size	Times__Times_var_i16_i64, .-Times__Times_var_i16_i64
	.align 4
	.type	Times__uTimes_param_i16_i64, #function
	.proc	05
Times__uTimes_param_i16_i64:
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
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE218:
	.size	Times__uTimes_param_i16_i64, .-Times__uTimes_param_i16_i64
	.align 4
	.type	Times__Times_param_i16_i64, #function
	.proc	05
Times__Times_param_i16_i64:
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
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE219:
	.size	Times__Times_param_i16_i64, .-Times__Times_param_i16_i64
	.align 4
	.type	Times__uTimes_var_i16_C, #function
	.proc	04
Times__uTimes_var_i16_C:
.LLFB220:
	save	%sp, -96, %sp
.LLCFI220:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE220:
	.size	Times__uTimes_var_i16_C, .-Times__uTimes_var_i16_C
	.align 4
	.type	Times__Times_var_i16_C, #function
	.proc	03
Times__Times_var_i16_C:
.LLFB221:
	save	%sp, -96, %sp
.LLCFI221:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	smul	%o5, %g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL269
	 nop
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	smul	%o5, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL270
	 nop
.LL269:
	sethi	%hi(8192), %g1
	or	%g1, 609, %o0
	call	_m3_fault, 0
	 nop
.LL270:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE221:
	.size	Times__Times_var_i16_C, .-Times__Times_var_i16_C
	.align 4
	.type	Times__uTimes_param_i16_C, #function
	.proc	04
Times__uTimes_param_i16_C:
.LLFB222:
	save	%sp, -96, %sp
.LLCFI222:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE222:
	.size	Times__uTimes_param_i16_C, .-Times__uTimes_param_i16_C
	.align 4
	.type	Times__Times_param_i16_C, #function
	.proc	03
Times__Times_param_i16_C:
.LLFB223:
	save	%sp, -96, %sp
.LLCFI223:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL273
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL274
	 nop
.LL273:
	sethi	%hi(8192), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL274:
	ld	[%fp+72], %g1
	mov	%g1, %o5
	lduh	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE223:
	.size	Times__Times_param_i16_C, .-Times__Times_param_i16_C
	.align 4
	.type	Times__uTimes_var_i16_u8, #function
	.proc	04
Times__uTimes_var_i16_u8:
.LLFB224:
	save	%sp, -96, %sp
.LLCFI224:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE224:
	.size	Times__uTimes_var_i16_u8, .-Times__uTimes_var_i16_u8
	.align 4
	.type	Times__Times_var_i16_u8, #function
	.proc	03
Times__Times_var_i16_u8:
.LLFB225:
	save	%sp, -96, %sp
.LLCFI225:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	smul	%o5, %g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL277
	 nop
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	smul	%o5, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL278
	 nop
.LL277:
	sethi	%hi(8192), %g1
	or	%g1, 737, %o0
	call	_m3_fault, 0
	 nop
.LL278:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE225:
	.size	Times__Times_var_i16_u8, .-Times__Times_var_i16_u8
	.align 4
	.type	Times__uTimes_param_i16_u8, #function
	.proc	04
Times__uTimes_param_i16_u8:
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
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE226:
	.size	Times__uTimes_param_i16_u8, .-Times__uTimes_param_i16_u8
	.align 4
	.type	Times__Times_param_i16_u8, #function
	.proc	03
Times__Times_param_i16_u8:
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
	smul	%o5, %g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL281
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %g1
	smul	%o5, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL282
	 nop
.LL281:
	sethi	%hi(8192), %g1
	or	%g1, 801, %o0
	call	_m3_fault, 0
	 nop
.LL282:
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	lduh	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE227:
	.size	Times__Times_param_i16_u8, .-Times__Times_param_i16_u8
	.align 4
	.type	Times__uTimes_var_i16_L, #function
	.proc	05
Times__uTimes_var_i16_L:
.LLFB228:
	save	%sp, -96, %sp
.LLCFI228:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE228:
	.size	Times__uTimes_var_i16_L, .-Times__uTimes_var_i16_L
	.align 4
	.type	Times__Times_var_i16_L, #function
	.proc	05
Times__Times_var_i16_L:
.LLFB229:
	save	%sp, -96, %sp
.LLCFI229:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE229:
	.size	Times__Times_var_i16_L, .-Times__Times_var_i16_L
	.align 4
	.type	Times__uTimes_param_i16_L, #function
	.proc	05
Times__uTimes_param_i16_L:
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
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE230:
	.size	Times__uTimes_param_i16_L, .-Times__uTimes_param_i16_L
	.align 4
	.type	Times__Times_param_i16_L, #function
	.proc	05
Times__Times_param_i16_L:
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
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE231:
	.size	Times__Times_param_i16_L, .-Times__Times_param_i16_L
	.align 4
	.type	Times__uTimes_var_i16_i8, #function
	.proc	04
Times__uTimes_var_i16_i8:
.LLFB232:
	save	%sp, -96, %sp
.LLCFI232:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE232:
	.size	Times__uTimes_var_i16_i8, .-Times__uTimes_var_i16_i8
	.align 4
	.type	Times__Times_var_i16_i8, #function
	.proc	03
Times__Times_var_i16_i8:
.LLFB233:
	save	%sp, -96, %sp
.LLCFI233:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	smul	%o5, %g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL289
	 nop
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	smul	%o5, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL290
	 nop
.LL289:
	sethi	%hi(8192), %g1
	or	%g1, 993, %o0
	call	_m3_fault, 0
	 nop
.LL290:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE233:
	.size	Times__Times_var_i16_i8, .-Times__Times_var_i16_i8
	.align 4
	.type	Times__uTimes_param_i16_i8, #function
	.proc	04
Times__uTimes_param_i16_i8:
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
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE234:
	.size	Times__uTimes_param_i16_i8, .-Times__uTimes_param_i16_i8
	.align 4
	.type	Times__Times_param_i16_i8, #function
	.proc	03
Times__Times_param_i16_i8:
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
	smul	%o5, %g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL293
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	smul	%o5, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL294
	 nop
.LL293:
	sethi	%hi(9216), %g1
	or	%g1, 33, %o0
	call	_m3_fault, 0
	 nop
.LL294:
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	lduh	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE235:
	.size	Times__Times_param_i16_i8, .-Times__Times_param_i16_i8
	.align 4
	.type	Times__uTimes_var_i16_u32, #function
	.proc	04
Times__uTimes_var_i16_u32:
.LLFB236:
	save	%sp, -96, %sp
.LLCFI236:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE236:
	.size	Times__uTimes_var_i16_u32, .-Times__uTimes_var_i16_u32
	.align 4
	.type	Times__Times_var_i16_u32, #function
	.proc	03
Times__Times_var_i16_u32:
.LLFB237:
	save	%sp, -96, %sp
.LLCFI237:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	smul	%o5, %g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL297
	 nop
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	smul	%o5, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL298
	 nop
.LL297:
	sethi	%hi(9216), %g1
	or	%g1, 97, %o0
	call	_m3_fault, 0
	 nop
.LL298:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE237:
	.size	Times__Times_var_i16_u32, .-Times__Times_var_i16_u32
	.align 4
	.type	Times__uTimes_param_i16_u32, #function
	.proc	04
Times__uTimes_param_i16_u32:
.LLFB238:
	save	%sp, -96, %sp
.LLCFI238:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE238:
	.size	Times__uTimes_param_i16_u32, .-Times__uTimes_param_i16_u32
	.align 4
	.type	Times__Times_param_i16_u32, #function
	.proc	03
Times__Times_param_i16_u32:
.LLFB239:
	save	%sp, -96, %sp
.LLCFI239:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL301
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL302
	 nop
.LL301:
	sethi	%hi(9216), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL302:
	ld	[%fp+72], %g1
	mov	%g1, %o5
	lduh	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE239:
	.size	Times__Times_param_i16_u32, .-Times__Times_param_i16_u32
	.align 4
	.type	Times__uTimes_var_I_u16, #function
	.proc	04
Times__uTimes_var_I_u16:
.LLFB240:
	save	%sp, -96, %sp
.LLCFI240:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE240:
	.size	Times__uTimes_var_I_u16, .-Times__uTimes_var_I_u16
	.align 4
	.type	Times__Times_var_I_u16, #function
	.proc	04
Times__Times_var_I_u16:
.LLFB241:
	save	%sp, -96, %sp
.LLCFI241:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE241:
	.size	Times__Times_var_I_u16, .-Times__Times_var_I_u16
	.align 4
	.type	Times__uTimes_param_I_u16, #function
	.proc	04
Times__uTimes_param_I_u16:
.LLFB242:
	save	%sp, -96, %sp
.LLCFI242:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE242:
	.size	Times__uTimes_param_I_u16, .-Times__uTimes_param_I_u16
	.align 4
	.type	Times__Times_param_I_u16, #function
	.proc	04
Times__Times_param_I_u16:
.LLFB243:
	save	%sp, -96, %sp
.LLCFI243:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE243:
	.size	Times__Times_param_I_u16, .-Times__Times_param_I_u16
	.align 4
	.type	Times__uTimes_var_I_u64, #function
	.proc	05
Times__uTimes_var_I_u64:
.LLFB244:
	save	%sp, -96, %sp
.LLCFI244:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE244:
	.size	Times__uTimes_var_I_u64, .-Times__uTimes_var_I_u64
	.align 4
	.type	Times__Times_var_I_u64, #function
	.proc	05
Times__Times_var_I_u64:
.LLFB245:
	save	%sp, -96, %sp
.LLCFI245:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE245:
	.size	Times__Times_var_I_u64, .-Times__Times_var_I_u64
	.align 4
	.type	Times__uTimes_param_I_u64, #function
	.proc	05
Times__uTimes_param_I_u64:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE246:
	.size	Times__uTimes_param_I_u64, .-Times__uTimes_param_I_u64
	.align 4
	.type	Times__Times_param_I_u64, #function
	.proc	05
Times__Times_param_I_u64:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE247:
	.size	Times__Times_param_I_u64, .-Times__Times_param_I_u64
	.align 4
	.type	Times__uTimes_var_I_LC, #function
	.proc	05
Times__uTimes_var_I_LC:
.LLFB248:
	save	%sp, -96, %sp
.LLCFI248:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE248:
	.size	Times__uTimes_var_I_LC, .-Times__uTimes_var_I_LC
	.align 4
	.type	Times__Times_var_I_LC, #function
	.proc	05
Times__Times_var_I_LC:
.LLFB249:
	save	%sp, -96, %sp
.LLCFI249:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE249:
	.size	Times__Times_var_I_LC, .-Times__Times_var_I_LC
	.align 4
	.type	Times__uTimes_param_I_LC, #function
	.proc	05
Times__uTimes_param_I_LC:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE250:
	.size	Times__uTimes_param_I_LC, .-Times__uTimes_param_I_LC
	.align 4
	.type	Times__Times_param_I_LC, #function
	.proc	05
Times__Times_param_I_LC:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE251:
	.size	Times__Times_param_I_LC, .-Times__Times_param_I_LC
	.align 4
	.type	Times__uTimes_var_I_i32, #function
	.proc	04
Times__uTimes_var_I_i32:
.LLFB252:
	save	%sp, -96, %sp
.LLCFI252:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE252:
	.size	Times__uTimes_var_I_i32, .-Times__uTimes_var_I_i32
	.align 4
	.type	Times__Times_var_I_i32, #function
	.proc	04
Times__Times_var_I_i32:
.LLFB253:
	save	%sp, -96, %sp
.LLCFI253:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE253:
	.size	Times__Times_var_I_i32, .-Times__Times_var_I_i32
	.align 4
	.type	Times__uTimes_param_I_i32, #function
	.proc	04
Times__uTimes_param_I_i32:
.LLFB254:
	save	%sp, -96, %sp
.LLCFI254:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE254:
	.size	Times__uTimes_param_I_i32, .-Times__uTimes_param_I_i32
	.align 4
	.type	Times__Times_param_I_i32, #function
	.proc	04
Times__Times_param_I_i32:
.LLFB255:
	save	%sp, -96, %sp
.LLCFI255:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE255:
	.size	Times__Times_param_I_i32, .-Times__Times_param_I_i32
	.align 4
	.type	Times__uTimes_var_I_i16, #function
	.proc	04
Times__uTimes_var_I_i16:
.LLFB256:
	save	%sp, -96, %sp
.LLCFI256:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE256:
	.size	Times__uTimes_var_I_i16, .-Times__uTimes_var_I_i16
	.align 4
	.type	Times__Times_var_I_i16, #function
	.proc	04
Times__Times_var_I_i16:
.LLFB257:
	save	%sp, -96, %sp
.LLCFI257:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE257:
	.size	Times__Times_var_I_i16, .-Times__Times_var_I_i16
	.align 4
	.type	Times__uTimes_param_I_i16, #function
	.proc	04
Times__uTimes_param_I_i16:
.LLFB258:
	save	%sp, -96, %sp
.LLCFI258:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE258:
	.size	Times__uTimes_param_I_i16, .-Times__uTimes_param_I_i16
	.align 4
	.type	Times__Times_param_I_i16, #function
	.proc	04
Times__Times_param_I_i16:
.LLFB259:
	save	%sp, -96, %sp
.LLCFI259:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE259:
	.size	Times__Times_param_I_i16, .-Times__Times_param_I_i16
	.align 4
	.type	Times__uTimes_var_I_I, #function
	.proc	04
Times__uTimes_var_I_I:
.LLFB260:
	save	%sp, -96, %sp
.LLCFI260:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE260:
	.size	Times__uTimes_var_I_I, .-Times__uTimes_var_I_I
	.align 4
	.type	Times__Times_var_I_I, #function
	.proc	04
Times__Times_var_I_I:
.LLFB261:
	save	%sp, -96, %sp
.LLCFI261:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE261:
	.size	Times__Times_var_I_I, .-Times__Times_var_I_I
	.align 4
	.type	Times__uTimes_param_I_I, #function
	.proc	04
Times__uTimes_param_I_I:
.LLFB262:
	save	%sp, -96, %sp
.LLCFI262:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE262:
	.size	Times__uTimes_param_I_I, .-Times__uTimes_param_I_I
	.align 4
	.type	Times__Times_param_I_I, #function
	.proc	04
Times__Times_param_I_I:
.LLFB263:
	save	%sp, -96, %sp
.LLCFI263:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE263:
	.size	Times__Times_param_I_I, .-Times__Times_param_I_I
	.align 4
	.type	Times__uTimes_var_I_i64, #function
	.proc	05
Times__uTimes_var_I_i64:
.LLFB264:
	save	%sp, -96, %sp
.LLCFI264:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE264:
	.size	Times__uTimes_var_I_i64, .-Times__uTimes_var_I_i64
	.align 4
	.type	Times__Times_var_I_i64, #function
	.proc	05
Times__Times_var_I_i64:
.LLFB265:
	save	%sp, -96, %sp
.LLCFI265:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE265:
	.size	Times__Times_var_I_i64, .-Times__Times_var_I_i64
	.align 4
	.type	Times__uTimes_param_I_i64, #function
	.proc	05
Times__uTimes_param_I_i64:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE266:
	.size	Times__uTimes_param_I_i64, .-Times__uTimes_param_I_i64
	.align 4
	.type	Times__Times_param_I_i64, #function
	.proc	05
Times__Times_param_I_i64:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE267:
	.size	Times__Times_param_I_i64, .-Times__Times_param_I_i64
	.align 4
	.type	Times__uTimes_var_I_C, #function
	.proc	04
Times__uTimes_var_I_C:
.LLFB268:
	save	%sp, -96, %sp
.LLCFI268:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE268:
	.size	Times__uTimes_var_I_C, .-Times__uTimes_var_I_C
	.align 4
	.type	Times__Times_var_I_C, #function
	.proc	04
Times__Times_var_I_C:
.LLFB269:
	save	%sp, -96, %sp
.LLCFI269:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE269:
	.size	Times__Times_var_I_C, .-Times__Times_var_I_C
	.align 4
	.type	Times__uTimes_param_I_C, #function
	.proc	04
Times__uTimes_param_I_C:
.LLFB270:
	save	%sp, -96, %sp
.LLCFI270:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE270:
	.size	Times__uTimes_param_I_C, .-Times__uTimes_param_I_C
	.align 4
	.type	Times__Times_param_I_C, #function
	.proc	04
Times__Times_param_I_C:
.LLFB271:
	save	%sp, -96, %sp
.LLCFI271:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE271:
	.size	Times__Times_param_I_C, .-Times__Times_param_I_C
	.align 4
	.type	Times__uTimes_var_I_u8, #function
	.proc	04
Times__uTimes_var_I_u8:
.LLFB272:
	save	%sp, -96, %sp
.LLCFI272:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE272:
	.size	Times__uTimes_var_I_u8, .-Times__uTimes_var_I_u8
	.align 4
	.type	Times__Times_var_I_u8, #function
	.proc	04
Times__Times_var_I_u8:
.LLFB273:
	save	%sp, -96, %sp
.LLCFI273:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE273:
	.size	Times__Times_var_I_u8, .-Times__Times_var_I_u8
	.align 4
	.type	Times__uTimes_param_I_u8, #function
	.proc	04
Times__uTimes_param_I_u8:
.LLFB274:
	save	%sp, -96, %sp
.LLCFI274:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE274:
	.size	Times__uTimes_param_I_u8, .-Times__uTimes_param_I_u8
	.align 4
	.type	Times__Times_param_I_u8, #function
	.proc	04
Times__Times_param_I_u8:
.LLFB275:
	save	%sp, -96, %sp
.LLCFI275:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE275:
	.size	Times__Times_param_I_u8, .-Times__Times_param_I_u8
	.align 4
	.type	Times__uTimes_var_I_L, #function
	.proc	05
Times__uTimes_var_I_L:
.LLFB276:
	save	%sp, -96, %sp
.LLCFI276:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE276:
	.size	Times__uTimes_var_I_L, .-Times__uTimes_var_I_L
	.align 4
	.type	Times__Times_var_I_L, #function
	.proc	05
Times__Times_var_I_L:
.LLFB277:
	save	%sp, -96, %sp
.LLCFI277:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE277:
	.size	Times__Times_var_I_L, .-Times__Times_var_I_L
	.align 4
	.type	Times__uTimes_param_I_L, #function
	.proc	05
Times__uTimes_param_I_L:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE278:
	.size	Times__uTimes_param_I_L, .-Times__uTimes_param_I_L
	.align 4
	.type	Times__Times_param_I_L, #function
	.proc	05
Times__Times_param_I_L:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE279:
	.size	Times__Times_param_I_L, .-Times__Times_param_I_L
	.align 4
	.type	Times__uTimes_var_I_i8, #function
	.proc	04
Times__uTimes_var_I_i8:
.LLFB280:
	save	%sp, -96, %sp
.LLCFI280:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE280:
	.size	Times__uTimes_var_I_i8, .-Times__uTimes_var_I_i8
	.align 4
	.type	Times__Times_var_I_i8, #function
	.proc	04
Times__Times_var_I_i8:
.LLFB281:
	save	%sp, -96, %sp
.LLCFI281:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE281:
	.size	Times__Times_var_I_i8, .-Times__Times_var_I_i8
	.align 4
	.type	Times__uTimes_param_I_i8, #function
	.proc	04
Times__uTimes_param_I_i8:
.LLFB282:
	save	%sp, -96, %sp
.LLCFI282:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE282:
	.size	Times__uTimes_param_I_i8, .-Times__uTimes_param_I_i8
	.align 4
	.type	Times__Times_param_I_i8, #function
	.proc	04
Times__Times_param_I_i8:
.LLFB283:
	save	%sp, -96, %sp
.LLCFI283:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE283:
	.size	Times__Times_param_I_i8, .-Times__Times_param_I_i8
	.align 4
	.type	Times__uTimes_var_I_u32, #function
	.proc	04
Times__uTimes_var_I_u32:
.LLFB284:
	save	%sp, -96, %sp
.LLCFI284:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE284:
	.size	Times__uTimes_var_I_u32, .-Times__uTimes_var_I_u32
	.align 4
	.type	Times__Times_var_I_u32, #function
	.proc	04
Times__Times_var_I_u32:
.LLFB285:
	save	%sp, -96, %sp
.LLCFI285:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE285:
	.size	Times__Times_var_I_u32, .-Times__Times_var_I_u32
	.align 4
	.type	Times__uTimes_param_I_u32, #function
	.proc	04
Times__uTimes_param_I_u32:
.LLFB286:
	save	%sp, -96, %sp
.LLCFI286:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE286:
	.size	Times__uTimes_param_I_u32, .-Times__uTimes_param_I_u32
	.align 4
	.type	Times__Times_param_I_u32, #function
	.proc	04
Times__Times_param_I_u32:
.LLFB287:
	save	%sp, -96, %sp
.LLCFI287:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE287:
	.size	Times__Times_param_I_u32, .-Times__Times_param_I_u32
	.align 4
	.type	Times__uTimes_var_i64_u16, #function
	.proc	05
Times__uTimes_var_i64_u16:
.LLFB288:
	save	%sp, -96, %sp
.LLCFI288:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE288:
	.size	Times__uTimes_var_i64_u16, .-Times__uTimes_var_i64_u16
	.align 4
	.type	Times__Times_var_i64_u16, #function
	.proc	05
Times__Times_var_i64_u16:
.LLFB289:
	save	%sp, -96, %sp
.LLCFI289:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE289:
	.size	Times__Times_var_i64_u16, .-Times__Times_var_i64_u16
	.align 4
	.type	Times__uTimes_param_i64_u16, #function
	.proc	05
Times__uTimes_param_i64_u16:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE290:
	.size	Times__uTimes_param_i64_u16, .-Times__uTimes_param_i64_u16
	.align 4
	.type	Times__Times_param_i64_u16, #function
	.proc	05
Times__Times_param_i64_u16:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE291:
	.size	Times__Times_param_i64_u16, .-Times__Times_param_i64_u16
	.align 4
	.type	Times__uTimes_var_i64_u64, #function
	.proc	05
Times__uTimes_var_i64_u64:
.LLFB292:
	save	%sp, -96, %sp
.LLCFI292:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o4
	sllx	%o4, 32, %o4
	srl	%o3, 0, %o3
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE292:
	.size	Times__uTimes_var_i64_u64, .-Times__uTimes_var_i64_u64
	.align 4
	.type	Times__Times_var_i64_u64, #function
	.proc	05
Times__Times_var_i64_u64:
.LLFB293:
	save	%sp, -96, %sp
.LLCFI293:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o2
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE293:
	.size	Times__Times_var_i64_u64, .-Times__Times_var_i64_u64
	.align 4
	.type	Times__uTimes_param_i64_u64, #function
	.proc	05
Times__uTimes_param_i64_u64:
.LLFB294:
	save	%sp, -112, %sp
.LLCFI294:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE294:
	.size	Times__uTimes_param_i64_u64, .-Times__uTimes_param_i64_u64
	.align 4
	.type	Times__Times_param_i64_u64, #function
	.proc	05
Times__Times_param_i64_u64:
.LLFB295:
	save	%sp, -112, %sp
.LLCFI295:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE295:
	.size	Times__Times_param_i64_u64, .-Times__Times_param_i64_u64
	.align 4
	.type	Times__uTimes_var_i64_LC, #function
	.proc	05
Times__uTimes_var_i64_LC:
.LLFB296:
	save	%sp, -96, %sp
.LLCFI296:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o4
	sllx	%o4, 32, %o4
	srl	%o3, 0, %o3
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE296:
	.size	Times__uTimes_var_i64_LC, .-Times__uTimes_var_i64_LC
	.align 4
	.type	Times__Times_var_i64_LC, #function
	.proc	05
Times__Times_var_i64_LC:
.LLFB297:
	save	%sp, -96, %sp
.LLCFI297:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o2
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE297:
	.size	Times__Times_var_i64_LC, .-Times__Times_var_i64_LC
	.align 4
	.type	Times__uTimes_param_i64_LC, #function
	.proc	05
Times__uTimes_param_i64_LC:
.LLFB298:
	save	%sp, -112, %sp
.LLCFI298:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE298:
	.size	Times__uTimes_param_i64_LC, .-Times__uTimes_param_i64_LC
	.align 4
	.type	Times__Times_param_i64_LC, #function
	.proc	05
Times__Times_param_i64_LC:
.LLFB299:
	save	%sp, -112, %sp
.LLCFI299:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o2
	ldd	[%fp-8], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE299:
	.size	Times__Times_param_i64_LC, .-Times__Times_param_i64_LC
	.align 4
	.type	Times__uTimes_var_i64_i32, #function
	.proc	05
Times__uTimes_var_i64_i32:
.LLFB300:
	save	%sp, -96, %sp
.LLCFI300:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE300:
	.size	Times__uTimes_var_i64_i32, .-Times__uTimes_var_i64_i32
	.align 4
	.type	Times__Times_var_i64_i32, #function
	.proc	05
Times__Times_var_i64_i32:
.LLFB301:
	save	%sp, -96, %sp
.LLCFI301:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE301:
	.size	Times__Times_var_i64_i32, .-Times__Times_var_i64_i32
	.align 4
	.type	Times__uTimes_param_i64_i32, #function
	.proc	05
Times__uTimes_param_i64_i32:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE302:
	.size	Times__uTimes_param_i64_i32, .-Times__uTimes_param_i64_i32
	.align 4
	.type	Times__Times_param_i64_i32, #function
	.proc	05
Times__Times_param_i64_i32:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE303:
	.size	Times__Times_param_i64_i32, .-Times__Times_param_i64_i32
	.align 4
	.type	Times__uTimes_var_i64_i16, #function
	.proc	05
Times__uTimes_var_i64_i16:
.LLFB304:
	save	%sp, -96, %sp
.LLCFI304:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE304:
	.size	Times__uTimes_var_i64_i16, .-Times__uTimes_var_i64_i16
	.align 4
	.type	Times__Times_var_i64_i16, #function
	.proc	05
Times__Times_var_i64_i16:
.LLFB305:
	save	%sp, -96, %sp
.LLCFI305:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE305:
	.size	Times__Times_var_i64_i16, .-Times__Times_var_i64_i16
	.align 4
	.type	Times__uTimes_param_i64_i16, #function
	.proc	05
Times__uTimes_param_i64_i16:
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
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE306:
	.size	Times__uTimes_param_i64_i16, .-Times__uTimes_param_i64_i16
	.align 4
	.type	Times__Times_param_i64_i16, #function
	.proc	05
Times__Times_param_i64_i16:
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
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE307:
	.size	Times__Times_param_i64_i16, .-Times__Times_param_i64_i16
	.align 4
	.type	Times__uTimes_var_i64_I, #function
	.proc	05
Times__uTimes_var_i64_I:
.LLFB308:
	save	%sp, -96, %sp
.LLCFI308:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE308:
	.size	Times__uTimes_var_i64_I, .-Times__uTimes_var_i64_I
	.align 4
	.type	Times__Times_var_i64_I, #function
	.proc	05
Times__Times_var_i64_I:
.LLFB309:
	save	%sp, -96, %sp
.LLCFI309:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE309:
	.size	Times__Times_var_i64_I, .-Times__Times_var_i64_I
	.align 4
	.type	Times__uTimes_param_i64_I, #function
	.proc	05
Times__uTimes_param_i64_I:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE310:
	.size	Times__uTimes_param_i64_I, .-Times__uTimes_param_i64_I
	.align 4
	.type	Times__Times_param_i64_I, #function
	.proc	05
Times__Times_param_i64_I:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE311:
	.size	Times__Times_param_i64_I, .-Times__Times_param_i64_I
	.align 4
	.type	Times__uTimes_var_i64_i64, #function
	.proc	05
Times__uTimes_var_i64_i64:
.LLFB312:
	save	%sp, -96, %sp
.LLCFI312:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o4
	sllx	%o4, 32, %o4
	srl	%o3, 0, %o3
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE312:
	.size	Times__uTimes_var_i64_i64, .-Times__uTimes_var_i64_i64
	.align 4
	.type	Times__Times_var_i64_i64, #function
	.proc	05
Times__Times_var_i64_i64:
.LLFB313:
	save	%sp, -96, %sp
.LLCFI313:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o2
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE313:
	.size	Times__Times_var_i64_i64, .-Times__Times_var_i64_i64
	.align 4
	.type	Times__uTimes_param_i64_i64, #function
	.proc	05
Times__uTimes_param_i64_i64:
.LLFB314:
	save	%sp, -112, %sp
.LLCFI314:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE314:
	.size	Times__uTimes_param_i64_i64, .-Times__uTimes_param_i64_i64
	.align 4
	.type	Times__Times_param_i64_i64, #function
	.proc	05
Times__Times_param_i64_i64:
.LLFB315:
	save	%sp, -112, %sp
.LLCFI315:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE315:
	.size	Times__Times_param_i64_i64, .-Times__Times_param_i64_i64
	.align 4
	.type	Times__uTimes_var_i64_C, #function
	.proc	05
Times__uTimes_var_i64_C:
.LLFB316:
	save	%sp, -96, %sp
.LLCFI316:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE316:
	.size	Times__uTimes_var_i64_C, .-Times__uTimes_var_i64_C
	.align 4
	.type	Times__Times_var_i64_C, #function
	.proc	05
Times__Times_var_i64_C:
.LLFB317:
	save	%sp, -96, %sp
.LLCFI317:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE317:
	.size	Times__Times_var_i64_C, .-Times__Times_var_i64_C
	.align 4
	.type	Times__uTimes_param_i64_C, #function
	.proc	05
Times__uTimes_param_i64_C:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE318:
	.size	Times__uTimes_param_i64_C, .-Times__uTimes_param_i64_C
	.align 4
	.type	Times__Times_param_i64_C, #function
	.proc	05
Times__Times_param_i64_C:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE319:
	.size	Times__Times_param_i64_C, .-Times__Times_param_i64_C
	.align 4
	.type	Times__uTimes_var_i64_u8, #function
	.proc	05
Times__uTimes_var_i64_u8:
.LLFB320:
	save	%sp, -96, %sp
.LLCFI320:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE320:
	.size	Times__uTimes_var_i64_u8, .-Times__uTimes_var_i64_u8
	.align 4
	.type	Times__Times_var_i64_u8, #function
	.proc	05
Times__Times_var_i64_u8:
.LLFB321:
	save	%sp, -96, %sp
.LLCFI321:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE321:
	.size	Times__Times_var_i64_u8, .-Times__Times_var_i64_u8
	.align 4
	.type	Times__uTimes_param_i64_u8, #function
	.proc	05
Times__uTimes_param_i64_u8:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE322:
	.size	Times__uTimes_param_i64_u8, .-Times__uTimes_param_i64_u8
	.align 4
	.type	Times__Times_param_i64_u8, #function
	.proc	05
Times__Times_param_i64_u8:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE323:
	.size	Times__Times_param_i64_u8, .-Times__Times_param_i64_u8
	.align 4
	.type	Times__uTimes_var_i64_L, #function
	.proc	05
Times__uTimes_var_i64_L:
.LLFB324:
	save	%sp, -96, %sp
.LLCFI324:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o4
	sllx	%o4, 32, %o4
	srl	%o3, 0, %o3
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE324:
	.size	Times__uTimes_var_i64_L, .-Times__uTimes_var_i64_L
	.align 4
	.type	Times__Times_var_i64_L, #function
	.proc	05
Times__Times_var_i64_L:
.LLFB325:
	save	%sp, -96, %sp
.LLCFI325:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o2
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE325:
	.size	Times__Times_var_i64_L, .-Times__Times_var_i64_L
	.align 4
	.type	Times__uTimes_param_i64_L, #function
	.proc	05
Times__uTimes_param_i64_L:
.LLFB326:
	save	%sp, -112, %sp
.LLCFI326:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE326:
	.size	Times__uTimes_param_i64_L, .-Times__uTimes_param_i64_L
	.align 4
	.type	Times__Times_param_i64_L, #function
	.proc	05
Times__Times_param_i64_L:
.LLFB327:
	save	%sp, -112, %sp
.LLCFI327:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE327:
	.size	Times__Times_param_i64_L, .-Times__Times_param_i64_L
	.align 4
	.type	Times__uTimes_var_i64_i8, #function
	.proc	05
Times__uTimes_var_i64_i8:
.LLFB328:
	save	%sp, -96, %sp
.LLCFI328:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE328:
	.size	Times__uTimes_var_i64_i8, .-Times__uTimes_var_i64_i8
	.align 4
	.type	Times__Times_var_i64_i8, #function
	.proc	05
Times__Times_var_i64_i8:
.LLFB329:
	save	%sp, -96, %sp
.LLCFI329:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE329:
	.size	Times__Times_var_i64_i8, .-Times__Times_var_i64_i8
	.align 4
	.type	Times__uTimes_param_i64_i8, #function
	.proc	05
Times__uTimes_param_i64_i8:
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
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE330:
	.size	Times__uTimes_param_i64_i8, .-Times__uTimes_param_i64_i8
	.align 4
	.type	Times__Times_param_i64_i8, #function
	.proc	05
Times__Times_param_i64_i8:
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
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE331:
	.size	Times__Times_param_i64_i8, .-Times__Times_param_i64_i8
	.align 4
	.type	Times__uTimes_var_i64_u32, #function
	.proc	05
Times__uTimes_var_i64_u32:
.LLFB332:
	save	%sp, -96, %sp
.LLCFI332:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE332:
	.size	Times__uTimes_var_i64_u32, .-Times__uTimes_var_i64_u32
	.align 4
	.type	Times__Times_var_i64_u32, #function
	.proc	05
Times__Times_var_i64_u32:
.LLFB333:
	save	%sp, -96, %sp
.LLCFI333:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE333:
	.size	Times__Times_var_i64_u32, .-Times__Times_var_i64_u32
	.align 4
	.type	Times__uTimes_param_i64_u32, #function
	.proc	05
Times__uTimes_param_i64_u32:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE334:
	.size	Times__uTimes_param_i64_u32, .-Times__uTimes_param_i64_u32
	.align 4
	.type	Times__Times_param_i64_u32, #function
	.proc	05
Times__Times_param_i64_u32:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE335:
	.size	Times__Times_param_i64_u32, .-Times__Times_param_i64_u32
	.align 4
	.type	Times__uTimes_var_C_u16, #function
	.proc	04
Times__uTimes_var_C_u16:
.LLFB336:
	save	%sp, -96, %sp
.LLCFI336:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE336:
	.size	Times__uTimes_var_C_u16, .-Times__uTimes_var_C_u16
	.align 4
	.type	Times__Times_var_C_u16, #function
	.proc	016
Times__Times_var_C_u16:
.LLFB337:
	save	%sp, -96, %sp
.LLCFI337:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL401
	 nop
	sethi	%hi(12288), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL401:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE337:
	.size	Times__Times_var_C_u16, .-Times__Times_var_C_u16
	.align 4
	.type	Times__uTimes_param_C_u16, #function
	.proc	04
Times__uTimes_param_C_u16:
.LLFB338:
	save	%sp, -96, %sp
.LLCFI338:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE338:
	.size	Times__uTimes_param_C_u16, .-Times__uTimes_param_C_u16
	.align 4
	.type	Times__Times_param_C_u16, #function
	.proc	016
Times__Times_param_C_u16:
.LLFB339:
	save	%sp, -96, %sp
.LLCFI339:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+68], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL404
	 nop
	sethi	%hi(12288), %g1
	or	%g1, 289, %o0
	call	_m3_fault, 0
	 nop
.LL404:
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE339:
	.size	Times__Times_param_C_u16, .-Times__Times_param_C_u16
	.align 4
	.type	Times__uTimes_var_C_u64, #function
	.proc	05
Times__uTimes_var_C_u64:
.LLFB340:
	save	%sp, -96, %sp
.LLCFI340:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE340:
	.size	Times__uTimes_var_C_u64, .-Times__uTimes_var_C_u64
	.align 4
	.type	Times__Times_var_C_u64, #function
	.proc	05
Times__Times_var_C_u64:
.LLFB341:
	save	%sp, -96, %sp
.LLCFI341:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE341:
	.size	Times__Times_var_C_u64, .-Times__Times_var_C_u64
	.align 4
	.type	Times__uTimes_param_C_u64, #function
	.proc	05
Times__uTimes_param_C_u64:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE342:
	.size	Times__uTimes_param_C_u64, .-Times__uTimes_param_C_u64
	.align 4
	.type	Times__Times_param_C_u64, #function
	.proc	05
Times__Times_param_C_u64:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE343:
	.size	Times__Times_param_C_u64, .-Times__Times_param_C_u64
	.align 4
	.type	Times__uTimes_var_C_LC, #function
	.proc	05
Times__uTimes_var_C_LC:
.LLFB344:
	save	%sp, -96, %sp
.LLCFI344:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE344:
	.size	Times__uTimes_var_C_LC, .-Times__uTimes_var_C_LC
	.align 4
	.type	Times__Times_var_C_LC, #function
	.proc	05
Times__Times_var_C_LC:
.LLFB345:
	save	%sp, -96, %sp
.LLCFI345:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE345:
	.size	Times__Times_var_C_LC, .-Times__Times_var_C_LC
	.align 4
	.type	Times__uTimes_param_C_LC, #function
	.proc	05
Times__uTimes_param_C_LC:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE346:
	.size	Times__uTimes_param_C_LC, .-Times__uTimes_param_C_LC
	.align 4
	.type	Times__Times_param_C_LC, #function
	.proc	05
Times__Times_param_C_LC:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE347:
	.size	Times__Times_param_C_LC, .-Times__Times_param_C_LC
	.align 4
	.type	Times__uTimes_var_C_i32, #function
	.proc	04
Times__uTimes_var_C_i32:
.LLFB348:
	save	%sp, -96, %sp
.LLCFI348:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE348:
	.size	Times__uTimes_var_C_i32, .-Times__uTimes_var_C_i32
	.align 4
	.type	Times__Times_var_C_i32, #function
	.proc	016
Times__Times_var_C_i32:
.LLFB349:
	save	%sp, -96, %sp
.LLCFI349:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL415
	 nop
	sethi	%hi(12288), %g1
	or	%g1, 609, %o0
	call	_m3_fault, 0
	 nop
.LL415:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE349:
	.size	Times__Times_var_C_i32, .-Times__Times_var_C_i32
	.align 4
	.type	Times__uTimes_param_C_i32, #function
	.proc	04
Times__uTimes_param_C_i32:
.LLFB350:
	save	%sp, -96, %sp
.LLCFI350:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE350:
	.size	Times__uTimes_param_C_i32, .-Times__uTimes_param_C_i32
	.align 4
	.type	Times__Times_param_C_i32, #function
	.proc	016
Times__Times_param_C_i32:
.LLFB351:
	save	%sp, -96, %sp
.LLCFI351:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL418
	 nop
	sethi	%hi(12288), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL418:
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE351:
	.size	Times__Times_param_C_i32, .-Times__Times_param_C_i32
	.align 4
	.type	Times__uTimes_var_C_i16, #function
	.proc	04
Times__uTimes_var_C_i16:
.LLFB352:
	save	%sp, -96, %sp
.LLCFI352:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE352:
	.size	Times__uTimes_var_C_i16, .-Times__uTimes_var_C_i16
	.align 4
	.type	Times__Times_var_C_i16, #function
	.proc	016
Times__Times_var_C_i16:
.LLFB353:
	save	%sp, -96, %sp
.LLCFI353:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL421
	 nop
	sethi	%hi(12288), %g1
	or	%g1, 737, %o0
	call	_m3_fault, 0
	 nop
.LL421:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE353:
	.size	Times__Times_var_C_i16, .-Times__Times_var_C_i16
	.align 4
	.type	Times__uTimes_param_C_i16, #function
	.proc	04
Times__uTimes_param_C_i16:
.LLFB354:
	save	%sp, -96, %sp
.LLCFI354:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE354:
	.size	Times__uTimes_param_C_i16, .-Times__uTimes_param_C_i16
	.align 4
	.type	Times__Times_param_C_i16, #function
	.proc	016
Times__Times_param_C_i16:
.LLFB355:
	save	%sp, -96, %sp
.LLCFI355:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+68], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL424
	 nop
	sethi	%hi(12288), %g1
	or	%g1, 801, %o0
	call	_m3_fault, 0
	 nop
.LL424:
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE355:
	.size	Times__Times_param_C_i16, .-Times__Times_param_C_i16
	.align 4
	.type	Times__uTimes_var_C_I, #function
	.proc	04
Times__uTimes_var_C_I:
.LLFB356:
	save	%sp, -96, %sp
.LLCFI356:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE356:
	.size	Times__uTimes_var_C_I, .-Times__uTimes_var_C_I
	.align 4
	.type	Times__Times_var_C_I, #function
	.proc	016
Times__Times_var_C_I:
.LLFB357:
	save	%sp, -96, %sp
.LLCFI357:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL427
	 nop
	sethi	%hi(12288), %g1
	or	%g1, 865, %o0
	call	_m3_fault, 0
	 nop
.LL427:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE357:
	.size	Times__Times_var_C_I, .-Times__Times_var_C_I
	.align 4
	.type	Times__uTimes_param_C_I, #function
	.proc	04
Times__uTimes_param_C_I:
.LLFB358:
	save	%sp, -96, %sp
.LLCFI358:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE358:
	.size	Times__uTimes_param_C_I, .-Times__uTimes_param_C_I
	.align 4
	.type	Times__Times_param_C_I, #function
	.proc	016
Times__Times_param_C_I:
.LLFB359:
	save	%sp, -96, %sp
.LLCFI359:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL430
	 nop
	sethi	%hi(12288), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL430:
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE359:
	.size	Times__Times_param_C_I, .-Times__Times_param_C_I
	.align 4
	.type	Times__uTimes_var_C_i64, #function
	.proc	05
Times__uTimes_var_C_i64:
.LLFB360:
	save	%sp, -96, %sp
.LLCFI360:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE360:
	.size	Times__uTimes_var_C_i64, .-Times__uTimes_var_C_i64
	.align 4
	.type	Times__Times_var_C_i64, #function
	.proc	05
Times__Times_var_C_i64:
.LLFB361:
	save	%sp, -96, %sp
.LLCFI361:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE361:
	.size	Times__Times_var_C_i64, .-Times__Times_var_C_i64
	.align 4
	.type	Times__uTimes_param_C_i64, #function
	.proc	05
Times__uTimes_param_C_i64:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE362:
	.size	Times__uTimes_param_C_i64, .-Times__uTimes_param_C_i64
	.align 4
	.type	Times__Times_param_C_i64, #function
	.proc	05
Times__Times_param_C_i64:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE363:
	.size	Times__Times_param_C_i64, .-Times__Times_param_C_i64
	.align 4
	.type	Times__uTimes_var_C_C, #function
	.proc	04
Times__uTimes_var_C_C:
.LLFB364:
	save	%sp, -96, %sp
.LLCFI364:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE364:
	.size	Times__uTimes_var_C_C, .-Times__uTimes_var_C_C
	.align 4
	.type	Times__Times_var_C_C, #function
	.proc	016
Times__Times_var_C_C:
.LLFB365:
	save	%sp, -96, %sp
.LLCFI365:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL437
	 nop
	sethi	%hi(13312), %g1
	or	%g1, 97, %o0
	call	_m3_fault, 0
	 nop
.LL437:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE365:
	.size	Times__Times_var_C_C, .-Times__Times_var_C_C
	.align 4
	.type	Times__uTimes_param_C_C, #function
	.proc	04
Times__uTimes_param_C_C:
.LLFB366:
	save	%sp, -96, %sp
.LLCFI366:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE366:
	.size	Times__uTimes_param_C_C, .-Times__uTimes_param_C_C
	.align 4
	.type	Times__Times_param_C_C, #function
	.proc	016
Times__Times_param_C_C:
.LLFB367:
	save	%sp, -96, %sp
.LLCFI367:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL440
	 nop
	sethi	%hi(13312), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL440:
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE367:
	.size	Times__Times_param_C_C, .-Times__Times_param_C_C
	.align 4
	.type	Times__uTimes_var_C_u8, #function
	.proc	04
Times__uTimes_var_C_u8:
.LLFB368:
	save	%sp, -96, %sp
.LLCFI368:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE368:
	.size	Times__uTimes_var_C_u8, .-Times__uTimes_var_C_u8
	.align 4
	.type	Times__Times_var_C_u8, #function
	.proc	016
Times__Times_var_C_u8:
.LLFB369:
	save	%sp, -96, %sp
.LLCFI369:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL443
	 nop
	sethi	%hi(13312), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL443:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE369:
	.size	Times__Times_var_C_u8, .-Times__Times_var_C_u8
	.align 4
	.type	Times__uTimes_param_C_u8, #function
	.proc	04
Times__uTimes_param_C_u8:
.LLFB370:
	save	%sp, -96, %sp
.LLCFI370:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE370:
	.size	Times__uTimes_param_C_u8, .-Times__uTimes_param_C_u8
	.align 4
	.type	Times__Times_param_C_u8, #function
	.proc	016
Times__Times_param_C_u8:
.LLFB371:
	save	%sp, -96, %sp
.LLCFI371:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+68], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL446
	 nop
	sethi	%hi(13312), %g1
	or	%g1, 289, %o0
	call	_m3_fault, 0
	 nop
.LL446:
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE371:
	.size	Times__Times_param_C_u8, .-Times__Times_param_C_u8
	.align 4
	.type	Times__uTimes_var_C_L, #function
	.proc	05
Times__uTimes_var_C_L:
.LLFB372:
	save	%sp, -96, %sp
.LLCFI372:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE372:
	.size	Times__uTimes_var_C_L, .-Times__uTimes_var_C_L
	.align 4
	.type	Times__Times_var_C_L, #function
	.proc	05
Times__Times_var_C_L:
.LLFB373:
	save	%sp, -96, %sp
.LLCFI373:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE373:
	.size	Times__Times_var_C_L, .-Times__Times_var_C_L
	.align 4
	.type	Times__uTimes_param_C_L, #function
	.proc	05
Times__uTimes_param_C_L:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE374:
	.size	Times__uTimes_param_C_L, .-Times__uTimes_param_C_L
	.align 4
	.type	Times__Times_param_C_L, #function
	.proc	05
Times__Times_param_C_L:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE375:
	.size	Times__Times_param_C_L, .-Times__Times_param_C_L
	.align 4
	.type	Times__uTimes_var_C_i8, #function
	.proc	04
Times__uTimes_var_C_i8:
.LLFB376:
	save	%sp, -96, %sp
.LLCFI376:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE376:
	.size	Times__uTimes_var_C_i8, .-Times__uTimes_var_C_i8
	.align 4
	.type	Times__Times_var_C_i8, #function
	.proc	016
Times__Times_var_C_i8:
.LLFB377:
	save	%sp, -96, %sp
.LLCFI377:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL453
	 nop
	sethi	%hi(13312), %g1
	or	%g1, 481, %o0
	call	_m3_fault, 0
	 nop
.LL453:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE377:
	.size	Times__Times_var_C_i8, .-Times__Times_var_C_i8
	.align 4
	.type	Times__uTimes_param_C_i8, #function
	.proc	04
Times__uTimes_param_C_i8:
.LLFB378:
	save	%sp, -96, %sp
.LLCFI378:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE378:
	.size	Times__uTimes_param_C_i8, .-Times__uTimes_param_C_i8
	.align 4
	.type	Times__Times_param_C_i8, #function
	.proc	016
Times__Times_param_C_i8:
.LLFB379:
	save	%sp, -96, %sp
.LLCFI379:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+68], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL456
	 nop
	sethi	%hi(13312), %g1
	or	%g1, 545, %o0
	call	_m3_fault, 0
	 nop
.LL456:
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE379:
	.size	Times__Times_param_C_i8, .-Times__Times_param_C_i8
	.align 4
	.type	Times__uTimes_var_C_u32, #function
	.proc	04
Times__uTimes_var_C_u32:
.LLFB380:
	save	%sp, -96, %sp
.LLCFI380:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE380:
	.size	Times__uTimes_var_C_u32, .-Times__uTimes_var_C_u32
	.align 4
	.type	Times__Times_var_C_u32, #function
	.proc	016
Times__Times_var_C_u32:
.LLFB381:
	save	%sp, -96, %sp
.LLCFI381:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL459
	 nop
	sethi	%hi(13312), %g1
	or	%g1, 609, %o0
	call	_m3_fault, 0
	 nop
.LL459:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE381:
	.size	Times__Times_var_C_u32, .-Times__Times_var_C_u32
	.align 4
	.type	Times__uTimes_param_C_u32, #function
	.proc	04
Times__uTimes_param_C_u32:
.LLFB382:
	save	%sp, -96, %sp
.LLCFI382:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE382:
	.size	Times__uTimes_param_C_u32, .-Times__uTimes_param_C_u32
	.align 4
	.type	Times__Times_param_C_u32, #function
	.proc	016
Times__Times_param_C_u32:
.LLFB383:
	save	%sp, -96, %sp
.LLCFI383:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL462
	 nop
	sethi	%hi(13312), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL462:
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE383:
	.size	Times__Times_param_C_u32, .-Times__Times_param_C_u32
	.align 4
	.type	Times__Times_var_f64_f64, #function
	.proc	07
Times__Times_var_f64_f64:
.LLFB384:
	save	%sp, -96, %sp
.LLCFI384:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	add	%g1, 104, %g1
	ldd	[%g1], %f10
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	add	%g1, 104, %g1
	ldd	[%g1], %f8
	fmuld	%f10, %f8, %f8
	fmovd	%f8, %f0
	return	%i7+8
	 nop
.LLFE384:
	.size	Times__Times_var_f64_f64, .-Times__Times_var_f64_f64
	.align 4
	.type	Times__Times_param_f64_f64, #function
	.proc	07
Times__Times_param_f64_f64:
.LLFB385:
	save	%sp, -112, %sp
.LLCFI385:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %f10
	ldd	[%fp-16], %f8
	fmuld	%f10, %f8, %f8
	fmovd	%f8, %f0
	return	%i7+8
	 nop
.LLFE385:
	.size	Times__Times_param_f64_f64, .-Times__Times_param_f64_f64
	.align 4
	.type	Times__uTimes_var_u8_u16, #function
	.proc	04
Times__uTimes_var_u8_u16:
.LLFB386:
	save	%sp, -96, %sp
.LLCFI386:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE386:
	.size	Times__uTimes_var_u8_u16, .-Times__uTimes_var_u8_u16
	.align 4
	.type	Times__Times_var_u8_u16, #function
	.proc	014
Times__Times_var_u8_u16:
.LLFB387:
	save	%sp, -96, %sp
.LLCFI387:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL467
	 nop
	sethi	%hi(13312), %g1
	or	%g1, 801, %o0
	call	_m3_fault, 0
	 nop
.LL467:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE387:
	.size	Times__Times_var_u8_u16, .-Times__Times_var_u8_u16
	.align 4
	.type	Times__uTimes_param_u8_u16, #function
	.proc	04
Times__uTimes_param_u8_u16:
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
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE388:
	.size	Times__uTimes_param_u8_u16, .-Times__uTimes_param_u8_u16
	.align 4
	.type	Times__Times_param_u8_u16, #function
	.proc	014
Times__Times_param_u8_u16:
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
	smul	%o5, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL470
	 nop
	sethi	%hi(13312), %g1
	or	%g1, 865, %o0
	call	_m3_fault, 0
	 nop
.LL470:
	lduh	[%fp+72], %g1
	mov	%g1, %o5
	ldub	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE389:
	.size	Times__Times_param_u8_u16, .-Times__Times_param_u8_u16
	.align 4
	.type	Times__uTimes_var_u8_u64, #function
	.proc	05
Times__uTimes_var_u8_u64:
.LLFB390:
	save	%sp, -96, %sp
.LLCFI390:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE390:
	.size	Times__uTimes_var_u8_u64, .-Times__uTimes_var_u8_u64
	.align 4
	.type	Times__Times_var_u8_u64, #function
	.proc	05
Times__Times_var_u8_u64:
.LLFB391:
	save	%sp, -96, %sp
.LLCFI391:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE391:
	.size	Times__Times_var_u8_u64, .-Times__Times_var_u8_u64
	.align 4
	.type	Times__uTimes_param_u8_u64, #function
	.proc	05
Times__uTimes_param_u8_u64:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE392:
	.size	Times__uTimes_param_u8_u64, .-Times__uTimes_param_u8_u64
	.align 4
	.type	Times__Times_param_u8_u64, #function
	.proc	05
Times__Times_param_u8_u64:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE393:
	.size	Times__Times_param_u8_u64, .-Times__Times_param_u8_u64
	.align 4
	.type	Times__uTimes_var_u8_LC, #function
	.proc	05
Times__uTimes_var_u8_LC:
.LLFB394:
	save	%sp, -96, %sp
.LLCFI394:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE394:
	.size	Times__uTimes_var_u8_LC, .-Times__uTimes_var_u8_LC
	.align 4
	.type	Times__Times_var_u8_LC, #function
	.proc	05
Times__Times_var_u8_LC:
.LLFB395:
	save	%sp, -96, %sp
.LLCFI395:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE395:
	.size	Times__Times_var_u8_LC, .-Times__Times_var_u8_LC
	.align 4
	.type	Times__uTimes_param_u8_LC, #function
	.proc	05
Times__uTimes_param_u8_LC:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE396:
	.size	Times__uTimes_param_u8_LC, .-Times__uTimes_param_u8_LC
	.align 4
	.type	Times__Times_param_u8_LC, #function
	.proc	05
Times__Times_param_u8_LC:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE397:
	.size	Times__Times_param_u8_LC, .-Times__Times_param_u8_LC
	.align 4
	.type	Times__uTimes_var_u8_i32, #function
	.proc	04
Times__uTimes_var_u8_i32:
.LLFB398:
	save	%sp, -96, %sp
.LLCFI398:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE398:
	.size	Times__uTimes_var_u8_i32, .-Times__uTimes_var_u8_i32
	.align 4
	.type	Times__Times_var_u8_i32, #function
	.proc	014
Times__Times_var_u8_i32:
.LLFB399:
	save	%sp, -96, %sp
.LLCFI399:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL481
	 nop
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL482
	 nop
.LL481:
	sethi	%hi(14336), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL482:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE399:
	.size	Times__Times_var_u8_i32, .-Times__Times_var_u8_i32
	.align 4
	.type	Times__uTimes_param_u8_i32, #function
	.proc	04
Times__uTimes_param_u8_i32:
.LLFB400:
	save	%sp, -96, %sp
.LLCFI400:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE400:
	.size	Times__uTimes_param_u8_i32, .-Times__uTimes_param_u8_i32
	.align 4
	.type	Times__Times_param_u8_i32, #function
	.proc	014
Times__Times_param_u8_i32:
.LLFB401:
	save	%sp, -96, %sp
.LLCFI401:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL485
	 nop
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL486
	 nop
.LL485:
	sethi	%hi(14336), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL486:
	ld	[%fp+72], %g1
	mov	%g1, %o5
	ldub	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE401:
	.size	Times__Times_param_u8_i32, .-Times__Times_param_u8_i32
	.align 4
	.type	Times__uTimes_var_u8_i16, #function
	.proc	04
Times__uTimes_var_u8_i16:
.LLFB402:
	save	%sp, -96, %sp
.LLCFI402:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE402:
	.size	Times__uTimes_var_u8_i16, .-Times__uTimes_var_u8_i16
	.align 4
	.type	Times__Times_var_u8_i16, #function
	.proc	014
Times__Times_var_u8_i16:
.LLFB403:
	save	%sp, -96, %sp
.LLCFI403:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL489
	 nop
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL490
	 nop
.LL489:
	sethi	%hi(14336), %g1
	or	%g1, 289, %o0
	call	_m3_fault, 0
	 nop
.LL490:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE403:
	.size	Times__Times_var_u8_i16, .-Times__Times_var_u8_i16
	.align 4
	.type	Times__uTimes_param_u8_i16, #function
	.proc	04
Times__uTimes_param_u8_i16:
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
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE404:
	.size	Times__uTimes_param_u8_i16, .-Times__uTimes_param_u8_i16
	.align 4
	.type	Times__Times_param_u8_i16, #function
	.proc	014
Times__Times_param_u8_i16:
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
	smul	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL493
	 nop
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL494
	 nop
.LL493:
	sethi	%hi(14336), %g1
	or	%g1, 353, %o0
	call	_m3_fault, 0
	 nop
.LL494:
	lduh	[%fp+72], %g1
	mov	%g1, %o5
	ldub	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE405:
	.size	Times__Times_param_u8_i16, .-Times__Times_param_u8_i16
	.align 4
	.type	Times__uTimes_var_u8_I, #function
	.proc	04
Times__uTimes_var_u8_I:
.LLFB406:
	save	%sp, -96, %sp
.LLCFI406:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE406:
	.size	Times__uTimes_var_u8_I, .-Times__uTimes_var_u8_I
	.align 4
	.type	Times__Times_var_u8_I, #function
	.proc	014
Times__Times_var_u8_I:
.LLFB407:
	save	%sp, -96, %sp
.LLCFI407:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL497
	 nop
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL498
	 nop
.LL497:
	sethi	%hi(14336), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL498:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE407:
	.size	Times__Times_var_u8_I, .-Times__Times_var_u8_I
	.align 4
	.type	Times__uTimes_param_u8_I, #function
	.proc	04
Times__uTimes_param_u8_I:
.LLFB408:
	save	%sp, -96, %sp
.LLCFI408:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE408:
	.size	Times__uTimes_param_u8_I, .-Times__uTimes_param_u8_I
	.align 4
	.type	Times__Times_param_u8_I, #function
	.proc	014
Times__Times_param_u8_I:
.LLFB409:
	save	%sp, -96, %sp
.LLCFI409:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL501
	 nop
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL502
	 nop
.LL501:
	sethi	%hi(14336), %g1
	or	%g1, 481, %o0
	call	_m3_fault, 0
	 nop
.LL502:
	ld	[%fp+72], %g1
	mov	%g1, %o5
	ldub	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE409:
	.size	Times__Times_param_u8_I, .-Times__Times_param_u8_I
	.align 4
	.type	Times__uTimes_var_u8_i64, #function
	.proc	05
Times__uTimes_var_u8_i64:
.LLFB410:
	save	%sp, -96, %sp
.LLCFI410:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE410:
	.size	Times__uTimes_var_u8_i64, .-Times__uTimes_var_u8_i64
	.align 4
	.type	Times__Times_var_u8_i64, #function
	.proc	05
Times__Times_var_u8_i64:
.LLFB411:
	save	%sp, -96, %sp
.LLCFI411:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE411:
	.size	Times__Times_var_u8_i64, .-Times__Times_var_u8_i64
	.align 4
	.type	Times__uTimes_param_u8_i64, #function
	.proc	05
Times__uTimes_param_u8_i64:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE412:
	.size	Times__uTimes_param_u8_i64, .-Times__uTimes_param_u8_i64
	.align 4
	.type	Times__Times_param_u8_i64, #function
	.proc	05
Times__Times_param_u8_i64:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE413:
	.size	Times__Times_param_u8_i64, .-Times__Times_param_u8_i64
	.align 4
	.type	Times__uTimes_var_u8_C, #function
	.proc	04
Times__uTimes_var_u8_C:
.LLFB414:
	save	%sp, -96, %sp
.LLCFI414:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE414:
	.size	Times__uTimes_var_u8_C, .-Times__uTimes_var_u8_C
	.align 4
	.type	Times__Times_var_u8_C, #function
	.proc	014
Times__Times_var_u8_C:
.LLFB415:
	save	%sp, -96, %sp
.LLCFI415:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL509
	 nop
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL510
	 nop
.LL509:
	sethi	%hi(14336), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL510:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE415:
	.size	Times__Times_var_u8_C, .-Times__Times_var_u8_C
	.align 4
	.type	Times__uTimes_param_u8_C, #function
	.proc	04
Times__uTimes_param_u8_C:
.LLFB416:
	save	%sp, -96, %sp
.LLCFI416:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE416:
	.size	Times__uTimes_param_u8_C, .-Times__uTimes_param_u8_C
	.align 4
	.type	Times__Times_param_u8_C, #function
	.proc	014
Times__Times_param_u8_C:
.LLFB417:
	save	%sp, -96, %sp
.LLCFI417:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL513
	 nop
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL514
	 nop
.LL513:
	sethi	%hi(14336), %g1
	or	%g1, 737, %o0
	call	_m3_fault, 0
	 nop
.LL514:
	ld	[%fp+72], %g1
	mov	%g1, %o5
	ldub	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE417:
	.size	Times__Times_param_u8_C, .-Times__Times_param_u8_C
	.align 4
	.type	Times__uTimes_var_u8_u8, #function
	.proc	04
Times__uTimes_var_u8_u8:
.LLFB418:
	save	%sp, -96, %sp
.LLCFI418:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE418:
	.size	Times__uTimes_var_u8_u8, .-Times__uTimes_var_u8_u8
	.align 4
	.type	Times__Times_var_u8_u8, #function
	.proc	014
Times__Times_var_u8_u8:
.LLFB419:
	save	%sp, -96, %sp
.LLCFI419:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL517
	 nop
	sethi	%hi(14336), %g1
	or	%g1, 801, %o0
	call	_m3_fault, 0
	 nop
.LL517:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE419:
	.size	Times__Times_var_u8_u8, .-Times__Times_var_u8_u8
	.align 4
	.type	Times__uTimes_param_u8_u8, #function
	.proc	04
Times__uTimes_param_u8_u8:
.LLFB420:
	save	%sp, -96, %sp
.LLCFI420:
	stb	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE420:
	.size	Times__uTimes_param_u8_u8, .-Times__uTimes_param_u8_u8
	.align 4
	.type	Times__Times_param_u8_u8, #function
	.proc	014
Times__Times_param_u8_u8:
.LLFB421:
	save	%sp, -96, %sp
.LLCFI421:
	stb	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL520
	 nop
	sethi	%hi(14336), %g1
	or	%g1, 865, %o0
	call	_m3_fault, 0
	 nop
.LL520:
	ldub	[%fp+68], %o5
	ldub	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE421:
	.size	Times__Times_param_u8_u8, .-Times__Times_param_u8_u8
	.align 4
	.type	Times__uTimes_var_u8_L, #function
	.proc	05
Times__uTimes_var_u8_L:
.LLFB422:
	save	%sp, -96, %sp
.LLCFI422:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE422:
	.size	Times__uTimes_var_u8_L, .-Times__uTimes_var_u8_L
	.align 4
	.type	Times__Times_var_u8_L, #function
	.proc	05
Times__Times_var_u8_L:
.LLFB423:
	save	%sp, -96, %sp
.LLCFI423:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE423:
	.size	Times__Times_var_u8_L, .-Times__Times_var_u8_L
	.align 4
	.type	Times__uTimes_param_u8_L, #function
	.proc	05
Times__uTimes_param_u8_L:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE424:
	.size	Times__uTimes_param_u8_L, .-Times__uTimes_param_u8_L
	.align 4
	.type	Times__Times_param_u8_L, #function
	.proc	05
Times__Times_param_u8_L:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE425:
	.size	Times__Times_param_u8_L, .-Times__Times_param_u8_L
	.align 4
	.type	Times__uTimes_var_u8_i8, #function
	.proc	04
Times__uTimes_var_u8_i8:
.LLFB426:
	save	%sp, -96, %sp
.LLCFI426:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE426:
	.size	Times__uTimes_var_u8_i8, .-Times__uTimes_var_u8_i8
	.align 4
	.type	Times__Times_var_u8_i8, #function
	.proc	014
Times__Times_var_u8_i8:
.LLFB427:
	save	%sp, -96, %sp
.LLCFI427:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL527
	 nop
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL528
	 nop
.LL527:
	sethi	%hi(15360), %g1
	or	%g1, 33, %o0
	call	_m3_fault, 0
	 nop
.LL528:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE427:
	.size	Times__Times_var_u8_i8, .-Times__Times_var_u8_i8
	.align 4
	.type	Times__uTimes_param_u8_i8, #function
	.proc	04
Times__uTimes_param_u8_i8:
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
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE428:
	.size	Times__uTimes_param_u8_i8, .-Times__uTimes_param_u8_i8
	.align 4
	.type	Times__Times_param_u8_i8, #function
	.proc	014
Times__Times_param_u8_i8:
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
	smul	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL531
	 nop
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL532
	 nop
.LL531:
	sethi	%hi(15360), %g1
	or	%g1, 97, %o0
	call	_m3_fault, 0
	 nop
.LL532:
	ldub	[%fp+72], %g1
	ldub	[%fp+68], %o5
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE429:
	.size	Times__Times_param_u8_i8, .-Times__Times_param_u8_i8
	.align 4
	.type	Times__uTimes_var_u8_u32, #function
	.proc	04
Times__uTimes_var_u8_u32:
.LLFB430:
	save	%sp, -96, %sp
.LLCFI430:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE430:
	.size	Times__uTimes_var_u8_u32, .-Times__uTimes_var_u8_u32
	.align 4
	.type	Times__Times_var_u8_u32, #function
	.proc	014
Times__Times_var_u8_u32:
.LLFB431:
	save	%sp, -96, %sp
.LLCFI431:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL535
	 nop
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL536
	 nop
.LL535:
	sethi	%hi(15360), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL536:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE431:
	.size	Times__Times_var_u8_u32, .-Times__Times_var_u8_u32
	.align 4
	.type	Times__uTimes_param_u8_u32, #function
	.proc	04
Times__uTimes_param_u8_u32:
.LLFB432:
	save	%sp, -96, %sp
.LLCFI432:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE432:
	.size	Times__uTimes_param_u8_u32, .-Times__uTimes_param_u8_u32
	.align 4
	.type	Times__Times_param_u8_u32, #function
	.proc	014
Times__Times_param_u8_u32:
.LLFB433:
	save	%sp, -96, %sp
.LLCFI433:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 0
	bl	%icc, .LL539
	 nop
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL540
	 nop
.LL539:
	sethi	%hi(15360), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL540:
	ld	[%fp+72], %g1
	mov	%g1, %o5
	ldub	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE433:
	.size	Times__Times_param_u8_u32, .-Times__Times_param_u8_u32
	.align 4
	.type	Times__uTimes_var_L_u16, #function
	.proc	05
Times__uTimes_var_L_u16:
.LLFB434:
	save	%sp, -96, %sp
.LLCFI434:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE434:
	.size	Times__uTimes_var_L_u16, .-Times__uTimes_var_L_u16
	.align 4
	.type	Times__Times_var_L_u16, #function
	.proc	05
Times__Times_var_L_u16:
.LLFB435:
	save	%sp, -96, %sp
.LLCFI435:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE435:
	.size	Times__Times_var_L_u16, .-Times__Times_var_L_u16
	.align 4
	.type	Times__uTimes_param_L_u16, #function
	.proc	05
Times__uTimes_param_L_u16:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE436:
	.size	Times__uTimes_param_L_u16, .-Times__uTimes_param_L_u16
	.align 4
	.type	Times__Times_param_L_u16, #function
	.proc	05
Times__Times_param_L_u16:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE437:
	.size	Times__Times_param_L_u16, .-Times__Times_param_L_u16
	.align 4
	.type	Times__uTimes_var_L_u64, #function
	.proc	05
Times__uTimes_var_L_u64:
.LLFB438:
	save	%sp, -96, %sp
.LLCFI438:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o4
	sllx	%o4, 32, %o4
	srl	%o3, 0, %o3
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE438:
	.size	Times__uTimes_var_L_u64, .-Times__uTimes_var_L_u64
	.align 4
	.type	Times__Times_var_L_u64, #function
	.proc	05
Times__Times_var_L_u64:
.LLFB439:
	save	%sp, -96, %sp
.LLCFI439:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o2
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE439:
	.size	Times__Times_var_L_u64, .-Times__Times_var_L_u64
	.align 4
	.type	Times__uTimes_param_L_u64, #function
	.proc	05
Times__uTimes_param_L_u64:
.LLFB440:
	save	%sp, -112, %sp
.LLCFI440:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE440:
	.size	Times__uTimes_param_L_u64, .-Times__uTimes_param_L_u64
	.align 4
	.type	Times__Times_param_L_u64, #function
	.proc	05
Times__Times_param_L_u64:
.LLFB441:
	save	%sp, -112, %sp
.LLCFI441:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE441:
	.size	Times__Times_param_L_u64, .-Times__Times_param_L_u64
	.align 4
	.type	Times__uTimes_var_L_LC, #function
	.proc	05
Times__uTimes_var_L_LC:
.LLFB442:
	save	%sp, -96, %sp
.LLCFI442:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o4
	sllx	%o4, 32, %o4
	srl	%o3, 0, %o3
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE442:
	.size	Times__uTimes_var_L_LC, .-Times__uTimes_var_L_LC
	.align 4
	.type	Times__Times_var_L_LC, #function
	.proc	05
Times__Times_var_L_LC:
.LLFB443:
	save	%sp, -96, %sp
.LLCFI443:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o2
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE443:
	.size	Times__Times_var_L_LC, .-Times__Times_var_L_LC
	.align 4
	.type	Times__uTimes_param_L_LC, #function
	.proc	05
Times__uTimes_param_L_LC:
.LLFB444:
	save	%sp, -112, %sp
.LLCFI444:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE444:
	.size	Times__uTimes_param_L_LC, .-Times__uTimes_param_L_LC
	.align 4
	.type	Times__Times_param_L_LC, #function
	.proc	05
Times__Times_param_L_LC:
.LLFB445:
	save	%sp, -112, %sp
.LLCFI445:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o2
	ldd	[%fp-8], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE445:
	.size	Times__Times_param_L_LC, .-Times__Times_param_L_LC
	.align 4
	.type	Times__uTimes_var_L_i32, #function
	.proc	05
Times__uTimes_var_L_i32:
.LLFB446:
	save	%sp, -96, %sp
.LLCFI446:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE446:
	.size	Times__uTimes_var_L_i32, .-Times__uTimes_var_L_i32
	.align 4
	.type	Times__Times_var_L_i32, #function
	.proc	05
Times__Times_var_L_i32:
.LLFB447:
	save	%sp, -96, %sp
.LLCFI447:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE447:
	.size	Times__Times_var_L_i32, .-Times__Times_var_L_i32
	.align 4
	.type	Times__uTimes_param_L_i32, #function
	.proc	05
Times__uTimes_param_L_i32:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE448:
	.size	Times__uTimes_param_L_i32, .-Times__uTimes_param_L_i32
	.align 4
	.type	Times__Times_param_L_i32, #function
	.proc	05
Times__Times_param_L_i32:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE449:
	.size	Times__Times_param_L_i32, .-Times__Times_param_L_i32
	.align 4
	.type	Times__uTimes_var_L_i16, #function
	.proc	05
Times__uTimes_var_L_i16:
.LLFB450:
	save	%sp, -96, %sp
.LLCFI450:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE450:
	.size	Times__uTimes_var_L_i16, .-Times__uTimes_var_L_i16
	.align 4
	.type	Times__Times_var_L_i16, #function
	.proc	05
Times__Times_var_L_i16:
.LLFB451:
	save	%sp, -96, %sp
.LLCFI451:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE451:
	.size	Times__Times_var_L_i16, .-Times__Times_var_L_i16
	.align 4
	.type	Times__uTimes_param_L_i16, #function
	.proc	05
Times__uTimes_param_L_i16:
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
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE452:
	.size	Times__uTimes_param_L_i16, .-Times__uTimes_param_L_i16
	.align 4
	.type	Times__Times_param_L_i16, #function
	.proc	05
Times__Times_param_L_i16:
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
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE453:
	.size	Times__Times_param_L_i16, .-Times__Times_param_L_i16
	.align 4
	.type	Times__uTimes_var_L_I, #function
	.proc	05
Times__uTimes_var_L_I:
.LLFB454:
	save	%sp, -96, %sp
.LLCFI454:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE454:
	.size	Times__uTimes_var_L_I, .-Times__uTimes_var_L_I
	.align 4
	.type	Times__Times_var_L_I, #function
	.proc	05
Times__Times_var_L_I:
.LLFB455:
	save	%sp, -96, %sp
.LLCFI455:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE455:
	.size	Times__Times_var_L_I, .-Times__Times_var_L_I
	.align 4
	.type	Times__uTimes_param_L_I, #function
	.proc	05
Times__uTimes_param_L_I:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE456:
	.size	Times__uTimes_param_L_I, .-Times__uTimes_param_L_I
	.align 4
	.type	Times__Times_param_L_I, #function
	.proc	05
Times__Times_param_L_I:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE457:
	.size	Times__Times_param_L_I, .-Times__Times_param_L_I
	.align 4
	.type	Times__uTimes_var_L_i64, #function
	.proc	05
Times__uTimes_var_L_i64:
.LLFB458:
	save	%sp, -96, %sp
.LLCFI458:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o4
	sllx	%o4, 32, %o4
	srl	%o3, 0, %o3
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE458:
	.size	Times__uTimes_var_L_i64, .-Times__uTimes_var_L_i64
	.align 4
	.type	Times__Times_var_L_i64, #function
	.proc	05
Times__Times_var_L_i64:
.LLFB459:
	save	%sp, -96, %sp
.LLCFI459:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o2
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE459:
	.size	Times__Times_var_L_i64, .-Times__Times_var_L_i64
	.align 4
	.type	Times__uTimes_param_L_i64, #function
	.proc	05
Times__uTimes_param_L_i64:
.LLFB460:
	save	%sp, -112, %sp
.LLCFI460:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE460:
	.size	Times__uTimes_param_L_i64, .-Times__uTimes_param_L_i64
	.align 4
	.type	Times__Times_param_L_i64, #function
	.proc	05
Times__Times_param_L_i64:
.LLFB461:
	save	%sp, -112, %sp
.LLCFI461:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE461:
	.size	Times__Times_param_L_i64, .-Times__Times_param_L_i64
	.align 4
	.type	Times__uTimes_var_L_C, #function
	.proc	05
Times__uTimes_var_L_C:
.LLFB462:
	save	%sp, -96, %sp
.LLCFI462:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE462:
	.size	Times__uTimes_var_L_C, .-Times__uTimes_var_L_C
	.align 4
	.type	Times__Times_var_L_C, #function
	.proc	05
Times__Times_var_L_C:
.LLFB463:
	save	%sp, -96, %sp
.LLCFI463:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE463:
	.size	Times__Times_var_L_C, .-Times__Times_var_L_C
	.align 4
	.type	Times__uTimes_param_L_C, #function
	.proc	05
Times__uTimes_param_L_C:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE464:
	.size	Times__uTimes_param_L_C, .-Times__uTimes_param_L_C
	.align 4
	.type	Times__Times_param_L_C, #function
	.proc	05
Times__Times_param_L_C:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE465:
	.size	Times__Times_param_L_C, .-Times__Times_param_L_C
	.align 4
	.type	Times__uTimes_var_L_u8, #function
	.proc	05
Times__uTimes_var_L_u8:
.LLFB466:
	save	%sp, -96, %sp
.LLCFI466:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE466:
	.size	Times__uTimes_var_L_u8, .-Times__uTimes_var_L_u8
	.align 4
	.type	Times__Times_var_L_u8, #function
	.proc	05
Times__Times_var_L_u8:
.LLFB467:
	save	%sp, -96, %sp
.LLCFI467:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE467:
	.size	Times__Times_var_L_u8, .-Times__Times_var_L_u8
	.align 4
	.type	Times__uTimes_param_L_u8, #function
	.proc	05
Times__uTimes_param_L_u8:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE468:
	.size	Times__uTimes_param_L_u8, .-Times__uTimes_param_L_u8
	.align 4
	.type	Times__Times_param_L_u8, #function
	.proc	05
Times__Times_param_L_u8:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE469:
	.size	Times__Times_param_L_u8, .-Times__Times_param_L_u8
	.align 4
	.type	Times__uTimes_var_L_L, #function
	.proc	05
Times__uTimes_var_L_L:
.LLFB470:
	save	%sp, -96, %sp
.LLCFI470:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o4
	sllx	%o4, 32, %o4
	srl	%o3, 0, %o3
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE470:
	.size	Times__uTimes_var_L_L, .-Times__uTimes_var_L_L
	.align 4
	.type	Times__Times_var_L_L, #function
	.proc	05
Times__Times_var_L_L:
.LLFB471:
	save	%sp, -96, %sp
.LLCFI471:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o2
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE471:
	.size	Times__Times_var_L_L, .-Times__Times_var_L_L
	.align 4
	.type	Times__uTimes_param_L_L, #function
	.proc	05
Times__uTimes_param_L_L:
.LLFB472:
	save	%sp, -112, %sp
.LLCFI472:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE472:
	.size	Times__uTimes_param_L_L, .-Times__uTimes_param_L_L
	.align 4
	.type	Times__Times_param_L_L, #function
	.proc	05
Times__Times_param_L_L:
.LLFB473:
	save	%sp, -112, %sp
.LLCFI473:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE473:
	.size	Times__Times_param_L_L, .-Times__Times_param_L_L
	.align 4
	.type	Times__uTimes_var_L_i8, #function
	.proc	05
Times__uTimes_var_L_i8:
.LLFB474:
	save	%sp, -96, %sp
.LLCFI474:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE474:
	.size	Times__uTimes_var_L_i8, .-Times__uTimes_var_L_i8
	.align 4
	.type	Times__Times_var_L_i8, #function
	.proc	05
Times__Times_var_L_i8:
.LLFB475:
	save	%sp, -96, %sp
.LLCFI475:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE475:
	.size	Times__Times_var_L_i8, .-Times__Times_var_L_i8
	.align 4
	.type	Times__uTimes_param_L_i8, #function
	.proc	05
Times__uTimes_param_L_i8:
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
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE476:
	.size	Times__uTimes_param_L_i8, .-Times__uTimes_param_L_i8
	.align 4
	.type	Times__Times_param_L_i8, #function
	.proc	05
Times__Times_param_L_i8:
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
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE477:
	.size	Times__Times_param_L_i8, .-Times__Times_param_L_i8
	.align 4
	.type	Times__uTimes_var_L_u32, #function
	.proc	05
Times__uTimes_var_L_u32:
.LLFB478:
	save	%sp, -96, %sp
.LLCFI478:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE478:
	.size	Times__uTimes_var_L_u32, .-Times__uTimes_var_L_u32
	.align 4
	.type	Times__Times_var_L_u32, #function
	.proc	05
Times__Times_var_L_u32:
.LLFB479:
	save	%sp, -96, %sp
.LLCFI479:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE479:
	.size	Times__Times_var_L_u32, .-Times__Times_var_L_u32
	.align 4
	.type	Times__uTimes_param_L_u32, #function
	.proc	05
Times__uTimes_param_L_u32:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE480:
	.size	Times__uTimes_param_L_u32, .-Times__uTimes_param_L_u32
	.align 4
	.type	Times__Times_param_L_u32, #function
	.proc	05
Times__Times_param_L_u32:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE481:
	.size	Times__Times_param_L_u32, .-Times__Times_param_L_u32
	.align 4
	.type	Times__uTimes_var_i8_u16, #function
	.proc	04
Times__uTimes_var_i8_u16:
.LLFB482:
	save	%sp, -96, %sp
.LLCFI482:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE482:
	.size	Times__uTimes_var_i8_u16, .-Times__uTimes_var_i8_u16
	.align 4
	.type	Times__Times_var_i8_u16, #function
	.proc	02
Times__Times_var_i8_u16:
.LLFB483:
	save	%sp, -96, %sp
.LLCFI483:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	smul	%o5, %g1, %g1
	cmp	%g1, -128
	bl	%icc, .LL591
	 nop
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL592
	 nop
.LL591:
	sethi	%hi(16384), %g1
	or	%g1, 801, %o0
	call	_m3_fault, 0
	 nop
.LL592:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE483:
	.size	Times__Times_var_i8_u16, .-Times__Times_var_i8_u16
	.align 4
	.type	Times__uTimes_param_i8_u16, #function
	.proc	04
Times__uTimes_param_i8_u16:
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
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE484:
	.size	Times__uTimes_param_i8_u16, .-Times__uTimes_param_i8_u16
	.align 4
	.type	Times__Times_param_i8_u16, #function
	.proc	02
Times__Times_param_i8_u16:
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
	smul	%o5, %g1, %g1
	cmp	%g1, -128
	bl	%icc, .LL595
	 nop
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL596
	 nop
.LL595:
	sethi	%hi(16384), %g1
	or	%g1, 865, %o0
	call	_m3_fault, 0
	 nop
.LL596:
	lduh	[%fp+72], %g1
	mov	%g1, %o5
	ldub	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE485:
	.size	Times__Times_param_i8_u16, .-Times__Times_param_i8_u16
	.align 4
	.type	Times__uTimes_var_i8_u64, #function
	.proc	05
Times__uTimes_var_i8_u64:
.LLFB486:
	save	%sp, -96, %sp
.LLCFI486:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE486:
	.size	Times__uTimes_var_i8_u64, .-Times__uTimes_var_i8_u64
	.align 4
	.type	Times__Times_var_i8_u64, #function
	.proc	05
Times__Times_var_i8_u64:
.LLFB487:
	save	%sp, -96, %sp
.LLCFI487:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE487:
	.size	Times__Times_var_i8_u64, .-Times__Times_var_i8_u64
	.align 4
	.type	Times__uTimes_param_i8_u64, #function
	.proc	05
Times__uTimes_param_i8_u64:
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
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE488:
	.size	Times__uTimes_param_i8_u64, .-Times__uTimes_param_i8_u64
	.align 4
	.type	Times__Times_param_i8_u64, #function
	.proc	05
Times__Times_param_i8_u64:
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
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE489:
	.size	Times__Times_param_i8_u64, .-Times__Times_param_i8_u64
	.align 4
	.type	Times__uTimes_var_i8_LC, #function
	.proc	05
Times__uTimes_var_i8_LC:
.LLFB490:
	save	%sp, -96, %sp
.LLCFI490:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE490:
	.size	Times__uTimes_var_i8_LC, .-Times__uTimes_var_i8_LC
	.align 4
	.type	Times__Times_var_i8_LC, #function
	.proc	05
Times__Times_var_i8_LC:
.LLFB491:
	save	%sp, -96, %sp
.LLCFI491:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE491:
	.size	Times__Times_var_i8_LC, .-Times__Times_var_i8_LC
	.align 4
	.type	Times__uTimes_param_i8_LC, #function
	.proc	05
Times__uTimes_param_i8_LC:
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
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE492:
	.size	Times__uTimes_param_i8_LC, .-Times__uTimes_param_i8_LC
	.align 4
	.type	Times__Times_param_i8_LC, #function
	.proc	05
Times__Times_param_i8_LC:
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
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE493:
	.size	Times__Times_param_i8_LC, .-Times__Times_param_i8_LC
	.align 4
	.type	Times__uTimes_var_i8_i32, #function
	.proc	04
Times__uTimes_var_i8_i32:
.LLFB494:
	save	%sp, -96, %sp
.LLCFI494:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE494:
	.size	Times__uTimes_var_i8_i32, .-Times__uTimes_var_i8_i32
	.align 4
	.type	Times__Times_var_i8_i32, #function
	.proc	02
Times__Times_var_i8_i32:
.LLFB495:
	save	%sp, -96, %sp
.LLCFI495:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, -128
	bl	%icc, .LL607
	 nop
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL608
	 nop
.LL607:
	sethi	%hi(17408), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL608:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE495:
	.size	Times__Times_var_i8_i32, .-Times__Times_var_i8_i32
	.align 4
	.type	Times__uTimes_param_i8_i32, #function
	.proc	04
Times__uTimes_param_i8_i32:
.LLFB496:
	save	%sp, -96, %sp
.LLCFI496:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE496:
	.size	Times__uTimes_param_i8_i32, .-Times__uTimes_param_i8_i32
	.align 4
	.type	Times__Times_param_i8_i32, #function
	.proc	02
Times__Times_param_i8_i32:
.LLFB497:
	save	%sp, -96, %sp
.LLCFI497:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, -128
	bl	%icc, .LL611
	 nop
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL612
	 nop
.LL611:
	sethi	%hi(17408), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL612:
	ld	[%fp+72], %g1
	mov	%g1, %o5
	ldub	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE497:
	.size	Times__Times_param_i8_i32, .-Times__Times_param_i8_i32
	.align 4
	.type	Times__uTimes_var_i8_i16, #function
	.proc	04
Times__uTimes_var_i8_i16:
.LLFB498:
	save	%sp, -96, %sp
.LLCFI498:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE498:
	.size	Times__uTimes_var_i8_i16, .-Times__uTimes_var_i8_i16
	.align 4
	.type	Times__Times_var_i8_i16, #function
	.proc	02
Times__Times_var_i8_i16:
.LLFB499:
	save	%sp, -96, %sp
.LLCFI499:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	smul	%o5, %g1, %g1
	cmp	%g1, -128
	bl	%icc, .LL615
	 nop
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL616
	 nop
.LL615:
	sethi	%hi(17408), %g1
	or	%g1, 289, %o0
	call	_m3_fault, 0
	 nop
.LL616:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE499:
	.size	Times__Times_var_i8_i16, .-Times__Times_var_i8_i16
	.align 4
	.type	Times__uTimes_param_i8_i16, #function
	.proc	04
Times__uTimes_param_i8_i16:
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
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE500:
	.size	Times__uTimes_param_i8_i16, .-Times__uTimes_param_i8_i16
	.align 4
	.type	Times__Times_param_i8_i16, #function
	.proc	02
Times__Times_param_i8_i16:
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
	smul	%o5, %g1, %g1
	cmp	%g1, -128
	bl	%icc, .LL619
	 nop
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL620
	 nop
.LL619:
	sethi	%hi(17408), %g1
	or	%g1, 353, %o0
	call	_m3_fault, 0
	 nop
.LL620:
	lduh	[%fp+72], %g1
	mov	%g1, %o5
	ldub	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE501:
	.size	Times__Times_param_i8_i16, .-Times__Times_param_i8_i16
	.align 4
	.type	Times__uTimes_var_i8_I, #function
	.proc	04
Times__uTimes_var_i8_I:
.LLFB502:
	save	%sp, -96, %sp
.LLCFI502:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE502:
	.size	Times__uTimes_var_i8_I, .-Times__uTimes_var_i8_I
	.align 4
	.type	Times__Times_var_i8_I, #function
	.proc	02
Times__Times_var_i8_I:
.LLFB503:
	save	%sp, -96, %sp
.LLCFI503:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, -128
	bl	%icc, .LL623
	 nop
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL624
	 nop
.LL623:
	sethi	%hi(17408), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL624:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE503:
	.size	Times__Times_var_i8_I, .-Times__Times_var_i8_I
	.align 4
	.type	Times__uTimes_param_i8_I, #function
	.proc	04
Times__uTimes_param_i8_I:
.LLFB504:
	save	%sp, -96, %sp
.LLCFI504:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE504:
	.size	Times__uTimes_param_i8_I, .-Times__uTimes_param_i8_I
	.align 4
	.type	Times__Times_param_i8_I, #function
	.proc	02
Times__Times_param_i8_I:
.LLFB505:
	save	%sp, -96, %sp
.LLCFI505:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, -128
	bl	%icc, .LL627
	 nop
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL628
	 nop
.LL627:
	sethi	%hi(17408), %g1
	or	%g1, 481, %o0
	call	_m3_fault, 0
	 nop
.LL628:
	ld	[%fp+72], %g1
	mov	%g1, %o5
	ldub	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE505:
	.size	Times__Times_param_i8_I, .-Times__Times_param_i8_I
	.align 4
	.type	Times__uTimes_var_i8_i64, #function
	.proc	05
Times__uTimes_var_i8_i64:
.LLFB506:
	save	%sp, -96, %sp
.LLCFI506:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE506:
	.size	Times__uTimes_var_i8_i64, .-Times__uTimes_var_i8_i64
	.align 4
	.type	Times__Times_var_i8_i64, #function
	.proc	05
Times__Times_var_i8_i64:
.LLFB507:
	save	%sp, -96, %sp
.LLCFI507:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE507:
	.size	Times__Times_var_i8_i64, .-Times__Times_var_i8_i64
	.align 4
	.type	Times__uTimes_param_i8_i64, #function
	.proc	05
Times__uTimes_param_i8_i64:
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
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE508:
	.size	Times__uTimes_param_i8_i64, .-Times__uTimes_param_i8_i64
	.align 4
	.type	Times__Times_param_i8_i64, #function
	.proc	05
Times__Times_param_i8_i64:
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
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE509:
	.size	Times__Times_param_i8_i64, .-Times__Times_param_i8_i64
	.align 4
	.type	Times__uTimes_var_i8_C, #function
	.proc	04
Times__uTimes_var_i8_C:
.LLFB510:
	save	%sp, -96, %sp
.LLCFI510:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE510:
	.size	Times__uTimes_var_i8_C, .-Times__uTimes_var_i8_C
	.align 4
	.type	Times__Times_var_i8_C, #function
	.proc	02
Times__Times_var_i8_C:
.LLFB511:
	save	%sp, -96, %sp
.LLCFI511:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, -128
	bl	%icc, .LL635
	 nop
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL636
	 nop
.LL635:
	sethi	%hi(17408), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL636:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE511:
	.size	Times__Times_var_i8_C, .-Times__Times_var_i8_C
	.align 4
	.type	Times__uTimes_param_i8_C, #function
	.proc	04
Times__uTimes_param_i8_C:
.LLFB512:
	save	%sp, -96, %sp
.LLCFI512:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE512:
	.size	Times__uTimes_param_i8_C, .-Times__uTimes_param_i8_C
	.align 4
	.type	Times__Times_param_i8_C, #function
	.proc	02
Times__Times_param_i8_C:
.LLFB513:
	save	%sp, -96, %sp
.LLCFI513:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, -128
	bl	%icc, .LL639
	 nop
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL640
	 nop
.LL639:
	sethi	%hi(17408), %g1
	or	%g1, 737, %o0
	call	_m3_fault, 0
	 nop
.LL640:
	ld	[%fp+72], %g1
	mov	%g1, %o5
	ldub	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE513:
	.size	Times__Times_param_i8_C, .-Times__Times_param_i8_C
	.align 4
	.type	Times__uTimes_var_i8_u8, #function
	.proc	04
Times__uTimes_var_i8_u8:
.LLFB514:
	save	%sp, -96, %sp
.LLCFI514:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE514:
	.size	Times__uTimes_var_i8_u8, .-Times__uTimes_var_i8_u8
	.align 4
	.type	Times__Times_var_i8_u8, #function
	.proc	02
Times__Times_var_i8_u8:
.LLFB515:
	save	%sp, -96, %sp
.LLCFI515:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	smul	%o5, %g1, %g1
	cmp	%g1, -128
	bl	%icc, .LL643
	 nop
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL644
	 nop
.LL643:
	sethi	%hi(17408), %g1
	or	%g1, 801, %o0
	call	_m3_fault, 0
	 nop
.LL644:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE515:
	.size	Times__Times_var_i8_u8, .-Times__Times_var_i8_u8
	.align 4
	.type	Times__uTimes_param_i8_u8, #function
	.proc	04
Times__uTimes_param_i8_u8:
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
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE516:
	.size	Times__uTimes_param_i8_u8, .-Times__uTimes_param_i8_u8
	.align 4
	.type	Times__Times_param_i8_u8, #function
	.proc	02
Times__Times_param_i8_u8:
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
	smul	%o5, %g1, %g1
	cmp	%g1, -128
	bl	%icc, .LL647
	 nop
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL648
	 nop
.LL647:
	sethi	%hi(17408), %g1
	or	%g1, 865, %o0
	call	_m3_fault, 0
	 nop
.LL648:
	ldub	[%fp+68], %o5
	ldub	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE517:
	.size	Times__Times_param_i8_u8, .-Times__Times_param_i8_u8
	.align 4
	.type	Times__uTimes_var_i8_L, #function
	.proc	05
Times__uTimes_var_i8_L:
.LLFB518:
	save	%sp, -96, %sp
.LLCFI518:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE518:
	.size	Times__uTimes_var_i8_L, .-Times__uTimes_var_i8_L
	.align 4
	.type	Times__Times_var_i8_L, #function
	.proc	05
Times__Times_var_i8_L:
.LLFB519:
	save	%sp, -96, %sp
.LLCFI519:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE519:
	.size	Times__Times_var_i8_L, .-Times__Times_var_i8_L
	.align 4
	.type	Times__uTimes_param_i8_L, #function
	.proc	05
Times__uTimes_param_i8_L:
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
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE520:
	.size	Times__uTimes_param_i8_L, .-Times__uTimes_param_i8_L
	.align 4
	.type	Times__Times_param_i8_L, #function
	.proc	05
Times__Times_param_i8_L:
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
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE521:
	.size	Times__Times_param_i8_L, .-Times__Times_param_i8_L
	.align 4
	.type	Times__uTimes_var_i8_i8, #function
	.proc	04
Times__uTimes_var_i8_i8:
.LLFB522:
	save	%sp, -96, %sp
.LLCFI522:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE522:
	.size	Times__uTimes_var_i8_i8, .-Times__uTimes_var_i8_i8
	.align 4
	.type	Times__Times_var_i8_i8, #function
	.proc	02
Times__Times_var_i8_i8:
.LLFB523:
	save	%sp, -96, %sp
.LLCFI523:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	smul	%o5, %g1, %g1
	cmp	%g1, -128
	bl	%icc, .LL655
	 nop
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL656
	 nop
.LL655:
	sethi	%hi(18432), %g1
	or	%g1, 33, %o0
	call	_m3_fault, 0
	 nop
.LL656:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE523:
	.size	Times__Times_var_i8_i8, .-Times__Times_var_i8_i8
	.align 4
	.type	Times__uTimes_param_i8_i8, #function
	.proc	04
Times__uTimes_param_i8_i8:
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
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE524:
	.size	Times__uTimes_param_i8_i8, .-Times__uTimes_param_i8_i8
	.align 4
	.type	Times__Times_param_i8_i8, #function
	.proc	02
Times__Times_param_i8_i8:
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
	smul	%o5, %g1, %g1
	cmp	%g1, -128
	bl	%icc, .LL659
	 nop
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL660
	 nop
.LL659:
	sethi	%hi(18432), %g1
	or	%g1, 97, %o0
	call	_m3_fault, 0
	 nop
.LL660:
	ldub	[%fp+68], %o5
	ldub	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE525:
	.size	Times__Times_param_i8_i8, .-Times__Times_param_i8_i8
	.align 4
	.type	Times__uTimes_var_i8_u32, #function
	.proc	04
Times__uTimes_var_i8_u32:
.LLFB526:
	save	%sp, -96, %sp
.LLCFI526:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE526:
	.size	Times__uTimes_var_i8_u32, .-Times__uTimes_var_i8_u32
	.align 4
	.type	Times__Times_var_i8_u32, #function
	.proc	02
Times__Times_var_i8_u32:
.LLFB527:
	save	%sp, -96, %sp
.LLCFI527:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, -128
	bl	%icc, .LL663
	 nop
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL664
	 nop
.LL663:
	sethi	%hi(18432), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL664:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE527:
	.size	Times__Times_var_i8_u32, .-Times__Times_var_i8_u32
	.align 4
	.type	Times__uTimes_param_i8_u32, #function
	.proc	04
Times__uTimes_param_i8_u32:
.LLFB528:
	save	%sp, -96, %sp
.LLCFI528:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE528:
	.size	Times__uTimes_param_i8_u32, .-Times__uTimes_param_i8_u32
	.align 4
	.type	Times__Times_param_i8_u32, #function
	.proc	02
Times__Times_param_i8_u32:
.LLFB529:
	save	%sp, -96, %sp
.LLCFI529:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, -128
	bl	%icc, .LL667
	 nop
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL668
	 nop
.LL667:
	sethi	%hi(18432), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL668:
	ld	[%fp+72], %g1
	mov	%g1, %o5
	ldub	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE529:
	.size	Times__Times_param_i8_u32, .-Times__Times_param_i8_u32
	.align 4
	.type	Times__uTimes_var_u32_u16, #function
	.proc	04
Times__uTimes_var_u32_u16:
.LLFB530:
	save	%sp, -96, %sp
.LLCFI530:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE530:
	.size	Times__uTimes_var_u32_u16, .-Times__uTimes_var_u32_u16
	.align 4
	.type	Times__Times_var_u32_u16, #function
	.proc	04
Times__Times_var_u32_u16:
.LLFB531:
	save	%sp, -96, %sp
.LLCFI531:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE531:
	.size	Times__Times_var_u32_u16, .-Times__Times_var_u32_u16
	.align 4
	.type	Times__uTimes_param_u32_u16, #function
	.proc	04
Times__uTimes_param_u32_u16:
.LLFB532:
	save	%sp, -96, %sp
.LLCFI532:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE532:
	.size	Times__uTimes_param_u32_u16, .-Times__uTimes_param_u32_u16
	.align 4
	.type	Times__Times_param_u32_u16, #function
	.proc	04
Times__Times_param_u32_u16:
.LLFB533:
	save	%sp, -96, %sp
.LLCFI533:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE533:
	.size	Times__Times_param_u32_u16, .-Times__Times_param_u32_u16
	.align 4
	.type	Times__uTimes_var_u32_u64, #function
	.proc	05
Times__uTimes_var_u32_u64:
.LLFB534:
	save	%sp, -96, %sp
.LLCFI534:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE534:
	.size	Times__uTimes_var_u32_u64, .-Times__uTimes_var_u32_u64
	.align 4
	.type	Times__Times_var_u32_u64, #function
	.proc	05
Times__Times_var_u32_u64:
.LLFB535:
	save	%sp, -96, %sp
.LLCFI535:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+56], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE535:
	.size	Times__Times_var_u32_u64, .-Times__Times_var_u32_u64
	.align 4
	.type	Times__uTimes_param_u32_u64, #function
	.proc	05
Times__uTimes_param_u32_u64:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE536:
	.size	Times__uTimes_param_u32_u64, .-Times__uTimes_param_u32_u64
	.align 4
	.type	Times__Times_param_u32_u64, #function
	.proc	05
Times__Times_param_u32_u64:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE537:
	.size	Times__Times_param_u32_u64, .-Times__Times_param_u32_u64
	.align 4
	.type	Times__uTimes_var_u32_LC, #function
	.proc	05
Times__uTimes_var_u32_LC:
.LLFB538:
	save	%sp, -96, %sp
.LLCFI538:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE538:
	.size	Times__uTimes_var_u32_LC, .-Times__uTimes_var_u32_LC
	.align 4
	.type	Times__Times_var_u32_LC, #function
	.proc	05
Times__Times_var_u32_LC:
.LLFB539:
	save	%sp, -96, %sp
.LLCFI539:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+64], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE539:
	.size	Times__Times_var_u32_LC, .-Times__Times_var_u32_LC
	.align 4
	.type	Times__uTimes_param_u32_LC, #function
	.proc	05
Times__uTimes_param_u32_LC:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE540:
	.size	Times__uTimes_param_u32_LC, .-Times__uTimes_param_u32_LC
	.align 4
	.type	Times__Times_param_u32_LC, #function
	.proc	05
Times__Times_param_u32_LC:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE541:
	.size	Times__Times_param_u32_LC, .-Times__Times_param_u32_LC
	.align 4
	.type	Times__uTimes_var_u32_i32, #function
	.proc	04
Times__uTimes_var_u32_i32:
.LLFB542:
	save	%sp, -96, %sp
.LLCFI542:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE542:
	.size	Times__uTimes_var_u32_i32, .-Times__uTimes_var_u32_i32
	.align 4
	.type	Times__Times_var_u32_i32, #function
	.proc	04
Times__Times_var_u32_i32:
.LLFB543:
	save	%sp, -96, %sp
.LLCFI543:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE543:
	.size	Times__Times_var_u32_i32, .-Times__Times_var_u32_i32
	.align 4
	.type	Times__uTimes_param_u32_i32, #function
	.proc	04
Times__uTimes_param_u32_i32:
.LLFB544:
	save	%sp, -96, %sp
.LLCFI544:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE544:
	.size	Times__uTimes_param_u32_i32, .-Times__uTimes_param_u32_i32
	.align 4
	.type	Times__Times_param_u32_i32, #function
	.proc	04
Times__Times_param_u32_i32:
.LLFB545:
	save	%sp, -96, %sp
.LLCFI545:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE545:
	.size	Times__Times_param_u32_i32, .-Times__Times_param_u32_i32
	.align 4
	.type	Times__uTimes_var_u32_i16, #function
	.proc	04
Times__uTimes_var_u32_i16:
.LLFB546:
	save	%sp, -96, %sp
.LLCFI546:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE546:
	.size	Times__uTimes_var_u32_i16, .-Times__uTimes_var_u32_i16
	.align 4
	.type	Times__Times_var_u32_i16, #function
	.proc	04
Times__Times_var_u32_i16:
.LLFB547:
	save	%sp, -96, %sp
.LLCFI547:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE547:
	.size	Times__Times_var_u32_i16, .-Times__Times_var_u32_i16
	.align 4
	.type	Times__uTimes_param_u32_i16, #function
	.proc	04
Times__uTimes_param_u32_i16:
.LLFB548:
	save	%sp, -96, %sp
.LLCFI548:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE548:
	.size	Times__uTimes_param_u32_i16, .-Times__uTimes_param_u32_i16
	.align 4
	.type	Times__Times_param_u32_i16, #function
	.proc	04
Times__Times_param_u32_i16:
.LLFB549:
	save	%sp, -96, %sp
.LLCFI549:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE549:
	.size	Times__Times_param_u32_i16, .-Times__Times_param_u32_i16
	.align 4
	.type	Times__uTimes_var_u32_I, #function
	.proc	04
Times__uTimes_var_u32_I:
.LLFB550:
	save	%sp, -96, %sp
.LLCFI550:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE550:
	.size	Times__uTimes_var_u32_I, .-Times__uTimes_var_u32_I
	.align 4
	.type	Times__Times_var_u32_I, #function
	.proc	04
Times__Times_var_u32_I:
.LLFB551:
	save	%sp, -96, %sp
.LLCFI551:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+80], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE551:
	.size	Times__Times_var_u32_I, .-Times__Times_var_u32_I
	.align 4
	.type	Times__uTimes_param_u32_I, #function
	.proc	04
Times__uTimes_param_u32_I:
.LLFB552:
	save	%sp, -96, %sp
.LLCFI552:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE552:
	.size	Times__uTimes_param_u32_I, .-Times__uTimes_param_u32_I
	.align 4
	.type	Times__Times_param_u32_I, #function
	.proc	04
Times__Times_param_u32_I:
.LLFB553:
	save	%sp, -96, %sp
.LLCFI553:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE553:
	.size	Times__Times_param_u32_I, .-Times__Times_param_u32_I
	.align 4
	.type	Times__uTimes_var_u32_i64, #function
	.proc	05
Times__uTimes_var_u32_i64:
.LLFB554:
	save	%sp, -96, %sp
.LLCFI554:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE554:
	.size	Times__uTimes_var_u32_i64, .-Times__uTimes_var_u32_i64
	.align 4
	.type	Times__Times_var_u32_i64, #function
	.proc	05
Times__Times_var_u32_i64:
.LLFB555:
	save	%sp, -96, %sp
.LLCFI555:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+88], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE555:
	.size	Times__Times_var_u32_i64, .-Times__Times_var_u32_i64
	.align 4
	.type	Times__uTimes_param_u32_i64, #function
	.proc	05
Times__uTimes_param_u32_i64:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE556:
	.size	Times__uTimes_param_u32_i64, .-Times__uTimes_param_u32_i64
	.align 4
	.type	Times__Times_param_u32_i64, #function
	.proc	05
Times__Times_param_u32_i64:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE557:
	.size	Times__Times_param_u32_i64, .-Times__Times_param_u32_i64
	.align 4
	.type	Times__uTimes_var_u32_C, #function
	.proc	04
Times__uTimes_var_u32_C:
.LLFB558:
	save	%sp, -96, %sp
.LLCFI558:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE558:
	.size	Times__uTimes_var_u32_C, .-Times__uTimes_var_u32_C
	.align 4
	.type	Times__Times_var_u32_C, #function
	.proc	04
Times__Times_var_u32_C:
.LLFB559:
	save	%sp, -96, %sp
.LLCFI559:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+96], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE559:
	.size	Times__Times_var_u32_C, .-Times__Times_var_u32_C
	.align 4
	.type	Times__uTimes_param_u32_C, #function
	.proc	04
Times__uTimes_param_u32_C:
.LLFB560:
	save	%sp, -96, %sp
.LLCFI560:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE560:
	.size	Times__uTimes_param_u32_C, .-Times__uTimes_param_u32_C
	.align 4
	.type	Times__Times_param_u32_C, #function
	.proc	04
Times__Times_param_u32_C:
.LLFB561:
	save	%sp, -96, %sp
.LLCFI561:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE561:
	.size	Times__Times_param_u32_C, .-Times__Times_param_u32_C
	.align 4
	.type	Times__uTimes_var_u32_u8, #function
	.proc	04
Times__uTimes_var_u32_u8:
.LLFB562:
	save	%sp, -96, %sp
.LLCFI562:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE562:
	.size	Times__uTimes_var_u32_u8, .-Times__uTimes_var_u32_u8
	.align 4
	.type	Times__Times_var_u32_u8, #function
	.proc	04
Times__Times_var_u32_u8:
.LLFB563:
	save	%sp, -96, %sp
.LLCFI563:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE563:
	.size	Times__Times_var_u32_u8, .-Times__Times_var_u32_u8
	.align 4
	.type	Times__uTimes_param_u32_u8, #function
	.proc	04
Times__uTimes_param_u32_u8:
.LLFB564:
	save	%sp, -96, %sp
.LLCFI564:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE564:
	.size	Times__uTimes_param_u32_u8, .-Times__uTimes_param_u32_u8
	.align 4
	.type	Times__Times_param_u32_u8, #function
	.proc	04
Times__Times_param_u32_u8:
.LLFB565:
	save	%sp, -96, %sp
.LLCFI565:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE565:
	.size	Times__Times_param_u32_u8, .-Times__Times_param_u32_u8
	.align 4
	.type	Times__uTimes_var_u32_L, #function
	.proc	05
Times__uTimes_var_u32_L:
.LLFB566:
	save	%sp, -96, %sp
.LLCFI566:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE566:
	.size	Times__uTimes_var_u32_L, .-Times__uTimes_var_u32_L
	.align 4
	.type	Times__Times_var_u32_L, #function
	.proc	05
Times__Times_var_u32_L:
.LLFB567:
	save	%sp, -96, %sp
.LLCFI567:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ldd	[%g1+120], %o2
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE567:
	.size	Times__Times_var_u32_L, .-Times__Times_var_u32_L
	.align 4
	.type	Times__uTimes_param_u32_L, #function
	.proc	05
Times__uTimes_param_u32_L:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE568:
	.size	Times__uTimes_param_u32_L, .-Times__uTimes_param_u32_L
	.align 4
	.type	Times__Times_param_u32_L, #function
	.proc	05
Times__Times_param_u32_L:
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
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE569:
	.size	Times__Times_param_u32_L, .-Times__Times_param_u32_L
	.align 4
	.type	Times__uTimes_var_u32_i8, #function
	.proc	04
Times__uTimes_var_u32_i8:
.LLFB570:
	save	%sp, -96, %sp
.LLCFI570:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE570:
	.size	Times__uTimes_var_u32_i8, .-Times__uTimes_var_u32_i8
	.align 4
	.type	Times__Times_var_u32_i8, #function
	.proc	04
Times__Times_var_u32_i8:
.LLFB571:
	save	%sp, -96, %sp
.LLCFI571:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE571:
	.size	Times__Times_var_u32_i8, .-Times__Times_var_u32_i8
	.align 4
	.type	Times__uTimes_param_u32_i8, #function
	.proc	04
Times__uTimes_param_u32_i8:
.LLFB572:
	save	%sp, -96, %sp
.LLCFI572:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE572:
	.size	Times__uTimes_param_u32_i8, .-Times__uTimes_param_u32_i8
	.align 4
	.type	Times__Times_param_u32_i8, #function
	.proc	04
Times__Times_param_u32_i8:
.LLFB573:
	save	%sp, -96, %sp
.LLCFI573:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+68], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE573:
	.size	Times__Times_param_u32_i8, .-Times__Times_param_u32_i8
	.align 4
	.type	Times__uTimes_var_u32_u32, #function
	.proc	04
Times__uTimes_var_u32_u32:
.LLFB574:
	save	%sp, -96, %sp
.LLCFI574:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE574:
	.size	Times__uTimes_var_u32_u32, .-Times__uTimes_var_u32_u32
	.align 4
	.type	Times__Times_var_u32_u32, #function
	.proc	04
Times__Times_var_u32_u32:
.LLFB575:
	save	%sp, -96, %sp
.LLCFI575:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %o5
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	ld	[%g1+132], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE575:
	.size	Times__Times_var_u32_u32, .-Times__Times_var_u32_u32
	.align 4
	.type	Times__uTimes_param_u32_u32, #function
	.proc	04
Times__uTimes_param_u32_u32:
.LLFB576:
	save	%sp, -96, %sp
.LLCFI576:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE576:
	.size	Times__uTimes_param_u32_u32, .-Times__uTimes_param_u32_u32
	.align 4
	.type	Times__Times_param_u32_u32, #function
	.proc	04
Times__Times_param_u32_u32:
.LLFB577:
	save	%sp, -96, %sp
.LLCFI577:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	smul	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE577:
	.size	Times__Times_param_u32_u32, .-Times__Times_param_u32_u32
	.align 4
	.type	Times__Times_var_f32_f32, #function
	.proc	06
Times__Times_var_f32_f32:
.LLFB578:
	save	%sp, -96, %sp
.LLCFI578:
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	add	%g1, 136, %g1
	ld	[%g1], %f9
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	add	%g1, 136, %g1
	ld	[%g1], %f8
	fmuls	%f9, %f8, %f8
	fmovs	%f8, %f0
	return	%i7+8
	 nop
.LLFE578:
	.size	Times__Times_var_f32_f32, .-Times__Times_var_f32_f32
	.align 4
	.type	Times__Times_param_f32_f32, #function
	.proc	06
Times__Times_param_f32_f32:
.LLFB579:
	save	%sp, -96, %sp
.LLCFI579:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %f9
	ld	[%fp+72], %f8
	fmuls	%f9, %f8, %f8
	fmovs	%f8, %f0
	return	%i7+8
	 nop
.LLFE579:
	.size	Times__Times_param_f32_f32, .-Times__Times_param_f32_f32
	.align 4
	.global Times_M3
	.type	Times_M3, #function
	.proc	0120
Times_M3:
.LLFB580:
	save	%sp, -96, %sp
.LLCFI580:
	st	%i0, [%fp+68]
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE580:
	.size	Times_M3, .-Times_M3
	.align 4
	.type	_m3_fault, #function
	.proc	020
_m3_fault:
.LLFB581:
	save	%sp, -96, %sp
.LLCFI581:
	st	%i0, [%fp+68]
	sethi	%hi(MM_Times), %g1
	or	%g1, %lo(MM_Times), %o0
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
	.size	L_1, 15128
L_1:
	.ascii	"Times_M3"
	.skip 1
	.ascii	"Times_param_f32_f32"
	.skip 1
	.ascii	"Times_var_f32_f32"
	.skip 1
	.ascii	"Times_param_u32_u32"
	.skip 1
	.ascii	"uTimes_param_u32_u32"
	.skip 1
	.ascii	"Times_var_u32_u32"
	.skip 1
	.ascii	"uTimes_var_u32_u32"
	.skip 1
	.ascii	"Times_param_u32_i8"
	.skip 1
	.ascii	"uTimes_param_u32_i8"
	.skip 1
	.ascii	"Times_var_u32_i8"
	.skip 1
	.ascii	"uTimes_var_u32_i8"
	.skip 1
	.ascii	"Times_param_u32_L"
	.skip 1
	.ascii	"uTimes_param_u32_L"
	.skip 1
	.ascii	"Times_var_u32_L"
	.skip 1
	.ascii	"uTimes_var_u32_L"
	.skip 1
	.ascii	"Times_param_u32_u8"
	.skip 1
	.ascii	"uTimes_param_u32_u8"
	.skip 1
	.ascii	"Times_var_u32_u8"
	.skip 1
	.ascii	"uTimes_var_u32_u8"
	.skip 1
	.ascii	"Times_param_u32_C"
	.skip 1
	.ascii	"uTimes_param_u32_C"
	.skip 1
	.ascii	"Times_var_u32_C"
	.skip 1
	.ascii	"uTimes_var_u32_C"
	.skip 1
	.ascii	"Times_param_u32_i64"
	.skip 1
	.ascii	"uTimes_param_u32_i64"
	.skip 1
	.ascii	"Times_var_u32_i64"
	.skip 1
	.ascii	"uTimes_var_u32_i64"
	.skip 1
	.ascii	"Times_param_u32_I"
	.skip 1
	.ascii	"uTimes_param_u32_I"
	.skip 1
	.ascii	"Times_var_u32_I"
	.skip 1
	.ascii	"uTimes_var_u32_I"
	.skip 1
	.ascii	"Times_param_u32_i16"
	.skip 1
	.ascii	"uTimes_param_u32_i16"
	.skip 1
	.ascii	"Times_var_u32_i16"
	.skip 1
	.ascii	"uTimes_var_u32_i16"
	.skip 1
	.ascii	"Times_param_u32_i32"
	.skip 1
	.ascii	"uTimes_param_u32_i32"
	.skip 1
	.ascii	"Times_var_u32_i32"
	.skip 1
	.ascii	"uTimes_var_u32_i32"
	.skip 1
	.ascii	"Times_param_u32_LC"
	.skip 1
	.ascii	"uTimes_param_u32_LC"
	.skip 1
	.ascii	"Times_var_u32_LC"
	.skip 1
	.ascii	"uTimes_var_u32_LC"
	.skip 1
	.ascii	"Times_param_u32_u64"
	.skip 1
	.ascii	"uTimes_param_u32_u64"
	.skip 1
	.ascii	"Times_var_u32_u64"
	.skip 1
	.ascii	"uTimes_var_u32_u64"
	.skip 1
	.ascii	"Times_param_u32_u16"
	.skip 1
	.ascii	"uTimes_param_u32_u16"
	.skip 1
	.ascii	"Times_var_u32_u16"
	.skip 1
	.ascii	"uTimes_var_u32_u16"
	.skip 1
	.ascii	"Times_param_i8_u32"
	.skip 1
	.ascii	"uTimes_param_i8_u32"
	.skip 1
	.ascii	"Times_var_i8_u32"
	.skip 1
	.ascii	"uTimes_var_i8_u32"
	.skip 1
	.ascii	"Times_param_i8_i8"
	.skip 1
	.ascii	"uTimes_param_i8_i8"
	.skip 1
	.ascii	"Times_var_i8_i8"
	.skip 1
	.ascii	"uTimes_var_i8_i8"
	.skip 1
	.ascii	"Times_param_i8_L"
	.skip 1
	.ascii	"uTimes_param_i8_L"
	.skip 1
	.ascii	"Times_var_i8_L"
	.skip 1
	.ascii	"uTimes_var_i8_L"
	.skip 1
	.ascii	"Times_param_i8_u8"
	.skip 1
	.ascii	"uTimes_param_i8_u8"
	.skip 1
	.ascii	"Times_var_i8_u8"
	.skip 1
	.ascii	"uTimes_var_i8_u8"
	.skip 1
	.ascii	"Times_param_i8_C"
	.skip 1
	.ascii	"uTimes_param_i8_C"
	.skip 1
	.ascii	"Times_var_i8_C"
	.skip 1
	.ascii	"uTimes_var_i8_C"
	.skip 1
	.ascii	"Times_param_i8_i64"
	.skip 1
	.ascii	"uTimes_param_i8_i64"
	.skip 1
	.ascii	"Times_var_i8_i64"
	.skip 1
	.ascii	"uTimes_var_i8_i64"
	.skip 1
	.ascii	"Times_param_i8_I"
	.skip 1
	.ascii	"uTimes_param_i8_I"
	.skip 1
	.ascii	"Times_var_i8_I"
	.skip 1
	.ascii	"uTimes_var_i8_I"
	.skip 1
	.ascii	"Times_param_i8_i16"
	.skip 1
	.ascii	"uTimes_param_i8_i16"
	.skip 1
	.ascii	"Times_var_i8_i16"
	.skip 1
	.ascii	"uTimes_var_i8_i16"
	.skip 1
	.ascii	"Times_param_i8_i32"
	.skip 1
	.ascii	"uTimes_param_i8_i32"
	.skip 1
	.ascii	"Times_var_i8_i32"
	.skip 1
	.ascii	"uTimes_var_i8_i32"
	.skip 1
	.ascii	"Times_param_i8_LC"
	.skip 1
	.ascii	"uTimes_param_i8_LC"
	.skip 1
	.ascii	"Times_var_i8_LC"
	.skip 1
	.ascii	"uTimes_var_i8_LC"
	.skip 1
	.ascii	"Times_param_i8_u64"
	.skip 1
	.ascii	"uTimes_param_i8_u64"
	.skip 1
	.ascii	"Times_var_i8_u64"
	.skip 1
	.ascii	"uTimes_var_i8_u64"
	.skip 1
	.ascii	"Times_param_i8_u16"
	.skip 1
	.ascii	"uTimes_param_i8_u16"
	.skip 1
	.ascii	"Times_var_i8_u16"
	.skip 1
	.ascii	"uTimes_var_i8_u16"
	.skip 1
	.ascii	"Times_param_L_u32"
	.skip 1
	.ascii	"uTimes_param_L_u32"
	.skip 1
	.ascii	"Times_var_L_u32"
	.skip 1
	.ascii	"uTimes_var_L_u32"
	.skip 1
	.ascii	"Times_param_L_i8"
	.skip 1
	.ascii	"uTimes_param_L_i8"
	.skip 1
	.ascii	"Times_var_L_i8"
	.skip 1
	.ascii	"uTimes_var_L_i8"
	.skip 1
	.ascii	"Times_param_L_L"
	.skip 1
	.ascii	"uTimes_param_L_L"
	.skip 1
	.ascii	"Times_var_L_L"
	.skip 1
	.ascii	"uTimes_var_L_L"
	.skip 1
	.ascii	"Times_param_L_u8"
	.skip 1
	.ascii	"uTimes_param_L_u8"
	.skip 1
	.ascii	"Times_var_L_u8"
	.skip 1
	.ascii	"uTimes_var_L_u8"
	.skip 1
	.ascii	"Times_param_L_C"
	.skip 1
	.ascii	"uTimes_param_L_C"
	.skip 1
	.ascii	"Times_var_L_C"
	.skip 1
	.ascii	"uTimes_var_L_C"
	.skip 1
	.ascii	"Times_param_L_i64"
	.skip 1
	.ascii	"uTimes_param_L_i64"
	.skip 1
	.ascii	"Times_var_L_i64"
	.skip 1
	.ascii	"uTimes_var_L_i64"
	.skip 1
	.ascii	"Times_param_L_I"
	.skip 1
	.ascii	"uTimes_param_L_I"
	.skip 1
	.ascii	"Times_var_L_I"
	.skip 1
	.ascii	"uTimes_var_L_I"
	.skip 1
	.ascii	"Times_param_L_i16"
	.skip 1
	.ascii	"uTimes_param_L_i16"
	.skip 1
	.ascii	"Times_var_L_i16"
	.skip 1
	.ascii	"uTimes_var_L_i16"
	.skip 1
	.ascii	"Times_param_L_i32"
	.skip 1
	.ascii	"uTimes_param_L_i32"
	.skip 1
	.ascii	"Times_var_L_i32"
	.skip 1
	.ascii	"uTimes_var_L_i32"
	.skip 1
	.ascii	"Times_param_L_LC"
	.skip 1
	.ascii	"uTimes_param_L_LC"
	.skip 1
	.ascii	"Times_var_L_LC"
	.skip 1
	.ascii	"uTimes_var_L_LC"
	.skip 1
	.ascii	"Times_param_L_u64"
	.skip 1
	.ascii	"uTimes_param_L_u64"
	.skip 1
	.ascii	"Times_var_L_u64"
	.skip 1
	.ascii	"uTimes_var_L_u64"
	.skip 1
	.ascii	"Times_param_L_u16"
	.skip 1
	.ascii	"uTimes_param_L_u16"
	.skip 1
	.ascii	"Times_var_L_u16"
	.skip 1
	.ascii	"uTimes_var_L_u16"
	.skip 1
	.ascii	"Times_param_u8_u32"
	.skip 1
	.ascii	"uTimes_param_u8_u32"
	.skip 1
	.ascii	"Times_var_u8_u32"
	.skip 1
	.ascii	"uTimes_var_u8_u32"
	.skip 1
	.ascii	"Times_param_u8_i8"
	.skip 1
	.ascii	"uTimes_param_u8_i8"
	.skip 1
	.ascii	"Times_var_u8_i8"
	.skip 1
	.ascii	"uTimes_var_u8_i8"
	.skip 1
	.ascii	"Times_param_u8_L"
	.skip 1
	.ascii	"uTimes_param_u8_L"
	.skip 1
	.ascii	"Times_var_u8_L"
	.skip 1
	.ascii	"uTimes_var_u8_L"
	.skip 1
	.ascii	"Times_param_u8_u8"
	.skip 1
	.ascii	"uTimes_param_u8_u8"
	.skip 1
	.ascii	"Times_var_u8_u8"
	.skip 1
	.ascii	"uTimes_var_u8_u8"
	.skip 1
	.ascii	"Times_param_u8_C"
	.skip 1
	.ascii	"uTimes_param_u8_C"
	.skip 1
	.ascii	"Times_var_u8_C"
	.skip 1
	.ascii	"uTimes_var_u8_C"
	.skip 1
	.ascii	"Times_param_u8_i64"
	.skip 1
	.ascii	"uTimes_param_u8_i64"
	.skip 1
	.ascii	"Times_var_u8_i64"
	.skip 1
	.ascii	"uTimes_var_u8_i64"
	.skip 1
	.ascii	"Times_param_u8_I"
	.skip 1
	.ascii	"uTimes_param_u8_I"
	.skip 1
	.ascii	"Times_var_u8_I"
	.skip 1
	.ascii	"uTimes_var_u8_I"
	.skip 1
	.ascii	"Times_param_u8_i16"
	.skip 1
	.ascii	"uTimes_param_u8_i16"
	.skip 1
	.ascii	"Times_var_u8_i16"
	.skip 1
	.ascii	"uTimes_var_u8_i16"
	.skip 1
	.ascii	"Times_param_u8_i32"
	.skip 1
	.ascii	"uTimes_param_u8_i32"
	.skip 1
	.ascii	"Times_var_u8_i32"
	.skip 1
	.ascii	"uTimes_var_u8_i32"
	.skip 1
	.ascii	"Times_param_u8_LC"
	.skip 1
	.ascii	"uTimes_param_u8_LC"
	.skip 1
	.ascii	"Times_var_u8_LC"
	.skip 1
	.ascii	"uTimes_var_u8_LC"
	.skip 1
	.ascii	"Times_param_u8_u64"
	.skip 1
	.ascii	"uTimes_param_u8_u64"
	.skip 1
	.ascii	"Times_var_u8_u64"
	.skip 1
	.ascii	"uTimes_var_u8_u64"
	.skip 1
	.ascii	"Times_param_u8_u16"
	.skip 1
	.ascii	"uTimes_param_u8_u16"
	.skip 1
	.ascii	"Times_var_u8_u16"
	.skip 1
	.ascii	"uTimes_var_u8_u16"
	.skip 1
	.ascii	"Times_param_f64_f64"
	.skip 1
	.ascii	"Times_var_f64_f64"
	.skip 1
	.ascii	"Times_param_C_u32"
	.skip 1
	.ascii	"uTimes_param_C_u32"
	.skip 1
	.ascii	"Times_var_C_u32"
	.skip 1
	.ascii	"uTimes_var_C_u32"
	.skip 1
	.ascii	"Times_param_C_i8"
	.skip 1
	.ascii	"uTimes_param_C_i8"
	.skip 1
	.ascii	"Times_var_C_i8"
	.skip 1
	.ascii	"uTimes_var_C_i8"
	.skip 1
	.ascii	"Times_param_C_L"
	.skip 1
	.ascii	"uTimes_param_C_L"
	.skip 1
	.ascii	"Times_var_C_L"
	.skip 1
	.ascii	"uTimes_var_C_L"
	.skip 1
	.ascii	"Times_param_C_u8"
	.skip 1
	.ascii	"uTimes_param_C_u8"
	.skip 1
	.ascii	"Times_var_C_u8"
	.skip 1
	.ascii	"uTimes_var_C_u8"
	.skip 1
	.ascii	"Times_param_C_C"
	.skip 1
	.ascii	"uTimes_param_C_C"
	.skip 1
	.ascii	"Times_var_C_C"
	.skip 1
	.ascii	"uTimes_var_C_C"
	.skip 1
	.ascii	"Times_param_C_i64"
	.skip 1
	.ascii	"uTimes_param_C_i64"
	.skip 1
	.ascii	"Times_var_C_i64"
	.skip 1
	.ascii	"uTimes_var_C_i64"
	.skip 1
	.ascii	"Times_param_C_I"
	.skip 1
	.ascii	"uTimes_param_C_I"
	.skip 1
	.ascii	"Times_var_C_I"
	.skip 1
	.ascii	"uTimes_var_C_I"
	.skip 1
	.ascii	"Times_param_C_i16"
	.skip 1
	.ascii	"uTimes_param_C_i16"
	.skip 1
	.ascii	"Times_var_C_i16"
	.skip 1
	.ascii	"uTimes_var_C_i16"
	.skip 1
	.ascii	"Times_param_C_i32"
	.skip 1
	.ascii	"uTimes_param_C_i32"
	.skip 1
	.ascii	"Times_var_C_i32"
	.skip 1
	.ascii	"uTimes_var_C_i32"
	.skip 1
	.ascii	"Times_param_C_LC"
	.skip 1
	.ascii	"uTimes_param_C_LC"
	.skip 1
	.ascii	"Times_var_C_LC"
	.skip 1
	.ascii	"uTimes_var_C_LC"
	.skip 1
	.ascii	"Times_param_C_u64"
	.skip 1
	.ascii	"uTimes_param_C_u64"
	.skip 1
	.ascii	"Times_var_C_u64"
	.skip 1
	.ascii	"uTimes_var_C_u64"
	.skip 1
	.ascii	"Times_param_C_u16"
	.skip 1
	.ascii	"uTimes_param_C_u16"
	.skip 1
	.ascii	"Times_var_C_u16"
	.skip 1
	.ascii	"uTimes_var_C_u16"
	.skip 1
	.ascii	"Times_param_i64_u32"
	.skip 1
	.ascii	"uTimes_param_i64_u32"
	.skip 1
	.ascii	"Times_var_i64_u32"
	.skip 1
	.ascii	"uTimes_var_i64_u32"
	.skip 1
	.ascii	"Times_param_i64_i8"
	.skip 1
	.ascii	"uTimes_param_i64_i8"
	.skip 1
	.ascii	"Times_var_i64_i8"
	.skip 1
	.ascii	"uTimes_var_i64_i8"
	.skip 1
	.ascii	"Times_param_i64_L"
	.skip 1
	.ascii	"uTimes_param_i64_L"
	.skip 1
	.ascii	"Times_var_i64_L"
	.skip 1
	.ascii	"uTimes_var_i64_L"
	.skip 1
	.ascii	"Times_param_i64_u8"
	.skip 1
	.ascii	"uTimes_param_i64_u8"
	.skip 1
	.ascii	"Times_var_i64_u8"
	.skip 1
	.ascii	"uTimes_var_i64_u8"
	.skip 1
	.ascii	"Times_param_i64_C"
	.skip 1
	.ascii	"uTimes_param_i64_C"
	.skip 1
	.ascii	"Times_var_i64_C"
	.skip 1
	.ascii	"uTimes_var_i64_C"
	.skip 1
	.ascii	"Times_param_i64_i64"
	.skip 1
	.ascii	"uTimes_param_i64_i64"
	.skip 1
	.ascii	"Times_var_i64_i64"
	.skip 1
	.ascii	"uTimes_var_i64_i64"
	.skip 1
	.ascii	"Times_param_i64_I"
	.skip 1
	.ascii	"uTimes_param_i64_I"
	.skip 1
	.ascii	"Times_var_i64_I"
	.skip 1
	.ascii	"uTimes_var_i64_I"
	.skip 1
	.ascii	"Times_param_i64_i16"
	.skip 1
	.ascii	"uTimes_param_i64_i16"
	.skip 1
	.ascii	"Times_var_i64_i16"
	.skip 1
	.ascii	"uTimes_var_i64_i16"
	.skip 1
	.ascii	"Times_param_i64_i32"
	.skip 1
	.ascii	"uTimes_param_i64_i32"
	.skip 1
	.ascii	"Times_var_i64_i32"
	.skip 1
	.ascii	"uTimes_var_i64_i32"
	.skip 1
	.ascii	"Times_param_i64_LC"
	.skip 1
	.ascii	"uTimes_param_i64_LC"
	.skip 1
	.ascii	"Times_var_i64_LC"
	.skip 1
	.ascii	"uTimes_var_i64_LC"
	.skip 1
	.ascii	"Times_param_i64_u64"
	.skip 1
	.ascii	"uTimes_param_i64_u64"
	.skip 1
	.ascii	"Times_var_i64_u64"
	.skip 1
	.ascii	"uTimes_var_i64_u64"
	.skip 1
	.ascii	"Times_param_i64_u16"
	.skip 1
	.ascii	"uTimes_param_i64_u16"
	.skip 1
	.ascii	"Times_var_i64_u16"
	.skip 1
	.ascii	"uTimes_var_i64_u16"
	.skip 1
	.ascii	"Times_param_I_u32"
	.skip 1
	.ascii	"uTimes_param_I_u32"
	.skip 1
	.ascii	"Times_var_I_u32"
	.skip 1
	.ascii	"uTimes_var_I_u32"
	.skip 1
	.ascii	"Times_param_I_i8"
	.skip 1
	.ascii	"uTimes_param_I_i8"
	.skip 1
	.ascii	"Times_var_I_i8"
	.skip 1
	.ascii	"uTimes_var_I_i8"
	.skip 1
	.ascii	"Times_param_I_L"
	.skip 1
	.ascii	"uTimes_param_I_L"
	.skip 1
	.ascii	"Times_var_I_L"
	.skip 1
	.ascii	"uTimes_var_I_L"
	.skip 1
	.ascii	"Times_param_I_u8"
	.skip 1
	.ascii	"uTimes_param_I_u8"
	.skip 1
	.ascii	"Times_var_I_u8"
	.skip 1
	.ascii	"uTimes_var_I_u8"
	.skip 1
	.ascii	"Times_param_I_C"
	.skip 1
	.ascii	"uTimes_param_I_C"
	.skip 1
	.ascii	"Times_var_I_C"
	.skip 1
	.ascii	"uTimes_var_I_C"
	.skip 1
	.ascii	"Times_param_I_i64"
	.skip 1
	.ascii	"uTimes_param_I_i64"
	.skip 1
	.ascii	"Times_var_I_i64"
	.skip 1
	.ascii	"uTimes_var_I_i64"
	.skip 1
	.ascii	"Times_param_I_I"
	.skip 1
	.ascii	"uTimes_param_I_I"
	.skip 1
	.ascii	"Times_var_I_I"
	.skip 1
	.ascii	"uTimes_var_I_I"
	.skip 1
	.ascii	"Times_param_I_i16"
	.skip 1
	.ascii	"uTimes_param_I_i16"
	.skip 1
	.ascii	"Times_var_I_i16"
	.skip 1
	.ascii	"uTimes_var_I_i16"
	.skip 1
	.ascii	"Times_param_I_i32"
	.skip 1
	.ascii	"uTimes_param_I_i32"
	.skip 1
	.ascii	"Times_var_I_i32"
	.skip 1
	.ascii	"uTimes_var_I_i32"
	.skip 1
	.ascii	"Times_param_I_LC"
	.skip 1
	.ascii	"uTimes_param_I_LC"
	.skip 1
	.ascii	"Times_var_I_LC"
	.skip 1
	.ascii	"uTimes_var_I_LC"
	.skip 1
	.ascii	"Times_param_I_u64"
	.skip 1
	.ascii	"uTimes_param_I_u64"
	.skip 1
	.ascii	"Times_var_I_u64"
	.skip 1
	.ascii	"uTimes_var_I_u64"
	.skip 1
	.ascii	"Times_param_I_u16"
	.skip 1
	.ascii	"uTimes_param_I_u16"
	.skip 1
	.ascii	"Times_var_I_u16"
	.skip 1
	.ascii	"uTimes_var_I_u16"
	.skip 1
	.ascii	"Times_param_i16_u32"
	.skip 1
	.ascii	"uTimes_param_i16_u32"
	.skip 1
	.ascii	"Times_var_i16_u32"
	.skip 1
	.ascii	"uTimes_var_i16_u32"
	.skip 1
	.ascii	"Times_param_i16_i8"
	.skip 1
	.ascii	"uTimes_param_i16_i8"
	.skip 1
	.ascii	"Times_var_i16_i8"
	.skip 1
	.ascii	"uTimes_var_i16_i8"
	.skip 1
	.ascii	"Times_param_i16_L"
	.skip 1
	.ascii	"uTimes_param_i16_L"
	.skip 1
	.ascii	"Times_var_i16_L"
	.skip 1
	.ascii	"uTimes_var_i16_L"
	.skip 1
	.ascii	"Times_param_i16_u8"
	.skip 1
	.ascii	"uTimes_param_i16_u8"
	.skip 1
	.ascii	"Times_var_i16_u8"
	.skip 1
	.ascii	"uTimes_var_i16_u8"
	.skip 1
	.ascii	"Times_param_i16_C"
	.skip 1
	.ascii	"uTimes_param_i16_C"
	.skip 1
	.ascii	"Times_var_i16_C"
	.skip 1
	.ascii	"uTimes_var_i16_C"
	.skip 1
	.ascii	"Times_param_i16_i64"
	.skip 1
	.ascii	"uTimes_param_i16_i64"
	.skip 1
	.ascii	"Times_var_i16_i64"
	.skip 1
	.ascii	"uTimes_var_i16_i64"
	.skip 1
	.ascii	"Times_param_i16_I"
	.skip 1
	.ascii	"uTimes_param_i16_I"
	.skip 1
	.ascii	"Times_var_i16_I"
	.skip 1
	.ascii	"uTimes_var_i16_I"
	.skip 1
	.ascii	"Times_param_i16_i16"
	.skip 1
	.ascii	"uTimes_param_i16_i16"
	.skip 1
	.ascii	"Times_var_i16_i16"
	.skip 1
	.ascii	"uTimes_var_i16_i16"
	.skip 1
	.ascii	"Times_param_i16_i32"
	.skip 1
	.ascii	"uTimes_param_i16_i32"
	.skip 1
	.ascii	"Times_var_i16_i32"
	.skip 1
	.ascii	"uTimes_var_i16_i32"
	.skip 1
	.ascii	"Times_param_i16_LC"
	.skip 1
	.ascii	"uTimes_param_i16_LC"
	.skip 1
	.ascii	"Times_var_i16_LC"
	.skip 1
	.ascii	"uTimes_var_i16_LC"
	.skip 1
	.ascii	"Times_param_i16_u64"
	.skip 1
	.ascii	"uTimes_param_i16_u64"
	.skip 1
	.ascii	"Times_var_i16_u64"
	.skip 1
	.ascii	"uTimes_var_i16_u64"
	.skip 1
	.ascii	"Times_param_i16_u16"
	.skip 1
	.ascii	"uTimes_param_i16_u16"
	.skip 1
	.ascii	"Times_var_i16_u16"
	.skip 1
	.ascii	"uTimes_var_i16_u16"
	.skip 1
	.ascii	"Times_param_i32_u32"
	.skip 1
	.ascii	"uTimes_param_i32_u32"
	.skip 1
	.ascii	"Times_var_i32_u32"
	.skip 1
	.ascii	"uTimes_var_i32_u32"
	.skip 1
	.ascii	"Times_param_i32_i8"
	.skip 1
	.ascii	"uTimes_param_i32_i8"
	.skip 1
	.ascii	"Times_var_i32_i8"
	.skip 1
	.ascii	"uTimes_var_i32_i8"
	.skip 1
	.ascii	"Times_param_i32_L"
	.skip 1
	.ascii	"uTimes_param_i32_L"
	.skip 1
	.ascii	"Times_var_i32_L"
	.skip 1
	.ascii	"uTimes_var_i32_L"
	.skip 1
	.ascii	"Times_param_i32_u8"
	.skip 1
	.ascii	"uTimes_param_i32_u8"
	.skip 1
	.ascii	"Times_var_i32_u8"
	.skip 1
	.ascii	"uTimes_var_i32_u8"
	.skip 1
	.ascii	"Times_param_i32_C"
	.skip 1
	.ascii	"uTimes_param_i32_C"
	.skip 1
	.ascii	"Times_var_i32_C"
	.skip 1
	.ascii	"uTimes_var_i32_C"
	.skip 1
	.ascii	"Times_param_i32_i64"
	.skip 1
	.ascii	"uTimes_param_i32_i64"
	.skip 1
	.ascii	"Times_var_i32_i64"
	.skip 1
	.ascii	"uTimes_var_i32_i64"
	.skip 1
	.ascii	"Times_param_i32_I"
	.skip 1
	.ascii	"uTimes_param_i32_I"
	.skip 1
	.ascii	"Times_var_i32_I"
	.skip 1
	.ascii	"uTimes_var_i32_I"
	.skip 1
	.ascii	"Times_param_i32_i16"
	.skip 1
	.ascii	"uTimes_param_i32_i16"
	.skip 1
	.ascii	"Times_var_i32_i16"
	.skip 1
	.ascii	"uTimes_var_i32_i16"
	.skip 1
	.ascii	"Times_param_i32_i32"
	.skip 1
	.ascii	"uTimes_param_i32_i32"
	.skip 1
	.ascii	"Times_var_i32_i32"
	.skip 1
	.ascii	"uTimes_var_i32_i32"
	.skip 1
	.ascii	"Times_param_i32_LC"
	.skip 1
	.ascii	"uTimes_param_i32_LC"
	.skip 1
	.ascii	"Times_var_i32_LC"
	.skip 1
	.ascii	"uTimes_var_i32_LC"
	.skip 1
	.ascii	"Times_param_i32_u64"
	.skip 1
	.ascii	"uTimes_param_i32_u64"
	.skip 1
	.ascii	"Times_var_i32_u64"
	.skip 1
	.ascii	"uTimes_var_i32_u64"
	.skip 1
	.ascii	"Times_param_i32_u16"
	.skip 1
	.ascii	"uTimes_param_i32_u16"
	.skip 1
	.ascii	"Times_var_i32_u16"
	.skip 1
	.ascii	"uTimes_var_i32_u16"
	.skip 1
	.ascii	"Times_param_LC_u32"
	.skip 1
	.ascii	"uTimes_param_LC_u32"
	.skip 1
	.ascii	"Times_var_LC_u32"
	.skip 1
	.ascii	"uTimes_var_LC_u32"
	.skip 1
	.ascii	"Times_param_LC_i8"
	.skip 1
	.ascii	"uTimes_param_LC_i8"
	.skip 1
	.ascii	"Times_var_LC_i8"
	.skip 1
	.ascii	"uTimes_var_LC_i8"
	.skip 1
	.ascii	"Times_param_LC_L"
	.skip 1
	.ascii	"uTimes_param_LC_L"
	.skip 1
	.ascii	"Times_var_LC_L"
	.skip 1
	.ascii	"uTimes_var_LC_L"
	.skip 1
	.ascii	"Times_param_LC_u8"
	.skip 1
	.ascii	"uTimes_param_LC_u8"
	.skip 1
	.ascii	"Times_var_LC_u8"
	.skip 1
	.ascii	"uTimes_var_LC_u8"
	.skip 1
	.ascii	"Times_param_LC_C"
	.skip 1
	.ascii	"uTimes_param_LC_C"
	.skip 1
	.ascii	"Times_var_LC_C"
	.skip 1
	.ascii	"uTimes_var_LC_C"
	.skip 1
	.ascii	"Times_param_LC_i64"
	.skip 1
	.ascii	"uTimes_param_LC_i64"
	.skip 1
	.ascii	"Times_var_LC_i64"
	.skip 1
	.ascii	"uTimes_var_LC_i64"
	.skip 1
	.ascii	"Times_param_LC_I"
	.skip 1
	.ascii	"uTimes_param_LC_I"
	.skip 1
	.ascii	"Times_var_LC_I"
	.skip 1
	.ascii	"uTimes_var_LC_I"
	.skip 1
	.ascii	"Times_param_LC_i16"
	.skip 1
	.ascii	"uTimes_param_LC_i16"
	.skip 1
	.ascii	"Times_var_LC_i16"
	.skip 1
	.ascii	"uTimes_var_LC_i16"
	.skip 1
	.ascii	"Times_param_LC_i32"
	.skip 1
	.ascii	"uTimes_param_LC_i32"
	.skip 1
	.ascii	"Times_var_LC_i32"
	.skip 1
	.ascii	"uTimes_var_LC_i32"
	.skip 1
	.ascii	"Times_param_LC_LC"
	.skip 1
	.ascii	"uTimes_param_LC_LC"
	.skip 1
	.ascii	"Times_var_LC_LC"
	.skip 1
	.ascii	"uTimes_var_LC_LC"
	.skip 1
	.ascii	"Times_param_LC_u64"
	.skip 1
	.ascii	"uTimes_param_LC_u64"
	.skip 1
	.ascii	"Times_var_LC_u64"
	.skip 1
	.ascii	"uTimes_var_LC_u64"
	.skip 1
	.ascii	"Times_param_LC_u16"
	.skip 1
	.ascii	"uTimes_param_LC_u16"
	.skip 1
	.ascii	"Times_var_LC_u16"
	.skip 1
	.ascii	"uTimes_var_LC_u16"
	.skip 1
	.ascii	"Times_param_u64_u32"
	.skip 1
	.ascii	"uTimes_param_u64_u32"
	.skip 1
	.ascii	"Times_var_u64_u32"
	.skip 1
	.ascii	"uTimes_var_u64_u32"
	.skip 1
	.ascii	"Times_param_u64_i8"
	.skip 1
	.ascii	"uTimes_param_u64_i8"
	.skip 1
	.ascii	"Times_var_u64_i8"
	.skip 1
	.ascii	"uTimes_var_u64_i8"
	.skip 1
	.ascii	"Times_param_u64_L"
	.skip 1
	.ascii	"uTimes_param_u64_L"
	.skip 1
	.ascii	"Times_var_u64_L"
	.skip 1
	.ascii	"uTimes_var_u64_L"
	.skip 1
	.ascii	"Times_param_u64_u8"
	.skip 1
	.ascii	"uTimes_param_u64_u8"
	.skip 1
	.ascii	"Times_var_u64_u8"
	.skip 1
	.ascii	"uTimes_var_u64_u8"
	.skip 1
	.ascii	"Times_param_u64_C"
	.skip 1
	.ascii	"uTimes_param_u64_C"
	.skip 1
	.ascii	"Times_var_u64_C"
	.skip 1
	.ascii	"uTimes_var_u64_C"
	.skip 1
	.ascii	"Times_param_u64_i64"
	.skip 1
	.ascii	"uTimes_param_u64_i64"
	.skip 1
	.ascii	"Times_var_u64_i64"
	.skip 1
	.ascii	"uTimes_var_u64_i64"
	.skip 1
	.ascii	"Times_param_u64_I"
	.skip 1
	.ascii	"uTimes_param_u64_I"
	.skip 1
	.ascii	"Times_var_u64_I"
	.skip 1
	.ascii	"uTimes_var_u64_I"
	.skip 1
	.ascii	"Times_param_u64_i16"
	.skip 1
	.ascii	"uTimes_param_u64_i16"
	.skip 1
	.ascii	"Times_var_u64_i16"
	.skip 1
	.ascii	"uTimes_var_u64_i16"
	.skip 1
	.ascii	"Times_param_u64_i32"
	.skip 1
	.ascii	"uTimes_param_u64_i32"
	.skip 1
	.ascii	"Times_var_u64_i32"
	.skip 1
	.ascii	"uTimes_var_u64_i32"
	.skip 1
	.ascii	"Times_param_u64_LC"
	.skip 1
	.ascii	"uTimes_param_u64_LC"
	.skip 1
	.ascii	"Times_var_u64_LC"
	.skip 1
	.ascii	"uTimes_var_u64_LC"
	.skip 1
	.ascii	"Times_param_u64_u64"
	.skip 1
	.ascii	"uTimes_param_u64_u64"
	.skip 1
	.ascii	"Times_var_u64_u64"
	.skip 1
	.ascii	"uTimes_var_u64_u64"
	.skip 1
	.ascii	"Times_param_u64_u16"
	.skip 1
	.ascii	"uTimes_param_u64_u16"
	.skip 1
	.ascii	"Times_var_u64_u16"
	.skip 1
	.ascii	"uTimes_var_u64_u16"
	.skip 1
	.ascii	"Times_param_u16_u32"
	.skip 1
	.ascii	"uTimes_param_u16_u32"
	.skip 1
	.ascii	"Times_var_u16_u32"
	.skip 1
	.ascii	"uTimes_var_u16_u32"
	.skip 1
	.ascii	"Times_param_u16_i8"
	.skip 1
	.ascii	"uTimes_param_u16_i8"
	.skip 1
	.ascii	"Times_var_u16_i8"
	.skip 1
	.ascii	"uTimes_var_u16_i8"
	.skip 1
	.ascii	"Times_param_u16_L"
	.skip 1
	.ascii	"uTimes_param_u16_L"
	.skip 1
	.ascii	"Times_var_u16_L"
	.skip 1
	.ascii	"uTimes_var_u16_L"
	.skip 1
	.ascii	"Times_param_u16_u8"
	.skip 1
	.ascii	"uTimes_param_u16_u8"
	.skip 1
	.ascii	"Times_var_u16_u8"
	.skip 1
	.ascii	"uTimes_var_u16_u8"
	.skip 1
	.ascii	"Times_param_u16_C"
	.skip 1
	.ascii	"uTimes_param_u16_C"
	.skip 1
	.ascii	"Times_var_u16_C"
	.skip 1
	.ascii	"uTimes_var_u16_C"
	.skip 1
	.ascii	"Times_param_u16_i64"
	.skip 1
	.ascii	"uTimes_param_u16_i64"
	.skip 1
	.ascii	"Times_var_u16_i64"
	.skip 1
	.ascii	"uTimes_var_u16_i64"
	.skip 1
	.ascii	"Times_param_u16_I"
	.skip 1
	.ascii	"uTimes_param_u16_I"
	.skip 1
	.ascii	"Times_var_u16_I"
	.skip 1
	.ascii	"uTimes_var_u16_I"
	.skip 1
	.ascii	"Times_param_u16_i16"
	.skip 1
	.ascii	"uTimes_param_u16_i16"
	.skip 1
	.ascii	"Times_var_u16_i16"
	.skip 1
	.ascii	"uTimes_var_u16_i16"
	.skip 1
	.ascii	"Times_param_u16_i32"
	.skip 1
	.ascii	"uTimes_param_u16_i32"
	.skip 1
	.ascii	"Times_var_u16_i32"
	.skip 1
	.ascii	"uTimes_var_u16_i32"
	.skip 1
	.ascii	"Times_param_u16_LC"
	.skip 1
	.ascii	"uTimes_param_u16_LC"
	.skip 1
	.ascii	"Times_var_u16_LC"
	.skip 1
	.ascii	"uTimes_var_u16_LC"
	.skip 1
	.ascii	"Times_param_u16_u64"
	.skip 1
	.ascii	"uTimes_param_u16_u64"
	.skip 1
	.ascii	"Times_var_u16_u64"
	.skip 1
	.ascii	"uTimes_var_u16_u64"
	.skip 1
	.ascii	"Times_param_u16_u16"
	.skip 1
	.ascii	"uTimes_param_u16_u16"
	.skip 1
	.ascii	"Times_var_u16_u16"
	.skip 1
	.ascii	"uTimes_var_u16_u16"
	.skip 4
	.long	Times_M3
	.long	L_1
	.long	Times__Times_param_f32_f32
	.long	L_1+9
	.long	Times__Times_var_f32_f32
	.long	L_1+29
	.long	Times__Times_param_u32_u32
	.long	L_1+47
	.long	Times__uTimes_param_u32_u32
	.long	L_1+67
	.long	Times__Times_var_u32_u32
	.long	L_1+88
	.long	Times__uTimes_var_u32_u32
	.long	L_1+106
	.long	Times__Times_param_u32_i8
	.long	L_1+125
	.long	Times__uTimes_param_u32_i8
	.long	L_1+144
	.long	Times__Times_var_u32_i8
	.long	L_1+164
	.long	Times__uTimes_var_u32_i8
	.long	L_1+181
	.long	Times__Times_param_u32_L
	.long	L_1+199
	.long	Times__uTimes_param_u32_L
	.long	L_1+217
	.long	Times__Times_var_u32_L
	.long	L_1+236
	.long	Times__uTimes_var_u32_L
	.long	L_1+252
	.long	Times__Times_param_u32_u8
	.long	L_1+269
	.long	Times__uTimes_param_u32_u8
	.long	L_1+288
	.long	Times__Times_var_u32_u8
	.long	L_1+308
	.long	Times__uTimes_var_u32_u8
	.long	L_1+325
	.long	Times__Times_param_u32_C
	.long	L_1+343
	.long	Times__uTimes_param_u32_C
	.long	L_1+361
	.long	Times__Times_var_u32_C
	.long	L_1+380
	.long	Times__uTimes_var_u32_C
	.long	L_1+396
	.long	Times__Times_param_u32_i64
	.long	L_1+413
	.long	Times__uTimes_param_u32_i64
	.long	L_1+433
	.long	Times__Times_var_u32_i64
	.long	L_1+454
	.long	Times__uTimes_var_u32_i64
	.long	L_1+472
	.long	Times__Times_param_u32_I
	.long	L_1+491
	.long	Times__uTimes_param_u32_I
	.long	L_1+509
	.long	Times__Times_var_u32_I
	.long	L_1+528
	.long	Times__uTimes_var_u32_I
	.long	L_1+544
	.long	Times__Times_param_u32_i16
	.long	L_1+561
	.long	Times__uTimes_param_u32_i16
	.long	L_1+581
	.long	Times__Times_var_u32_i16
	.long	L_1+602
	.long	Times__uTimes_var_u32_i16
	.long	L_1+620
	.long	Times__Times_param_u32_i32
	.long	L_1+639
	.long	Times__uTimes_param_u32_i32
	.long	L_1+659
	.long	Times__Times_var_u32_i32
	.long	L_1+680
	.long	Times__uTimes_var_u32_i32
	.long	L_1+698
	.long	Times__Times_param_u32_LC
	.long	L_1+717
	.long	Times__uTimes_param_u32_LC
	.long	L_1+736
	.long	Times__Times_var_u32_LC
	.long	L_1+756
	.long	Times__uTimes_var_u32_LC
	.long	L_1+773
	.long	Times__Times_param_u32_u64
	.long	L_1+791
	.long	Times__uTimes_param_u32_u64
	.long	L_1+811
	.long	Times__Times_var_u32_u64
	.long	L_1+832
	.long	Times__uTimes_var_u32_u64
	.long	L_1+850
	.long	Times__Times_param_u32_u16
	.long	L_1+869
	.long	Times__uTimes_param_u32_u16
	.long	L_1+889
	.long	Times__Times_var_u32_u16
	.long	L_1+910
	.long	Times__uTimes_var_u32_u16
	.long	L_1+928
	.long	Times__Times_param_i8_u32
	.long	L_1+947
	.long	Times__uTimes_param_i8_u32
	.long	L_1+966
	.long	Times__Times_var_i8_u32
	.long	L_1+986
	.long	Times__uTimes_var_i8_u32
	.long	L_1+1003
	.long	Times__Times_param_i8_i8
	.long	L_1+1021
	.long	Times__uTimes_param_i8_i8
	.long	L_1+1039
	.long	Times__Times_var_i8_i8
	.long	L_1+1058
	.long	Times__uTimes_var_i8_i8
	.long	L_1+1074
	.long	Times__Times_param_i8_L
	.long	L_1+1091
	.long	Times__uTimes_param_i8_L
	.long	L_1+1108
	.long	Times__Times_var_i8_L
	.long	L_1+1126
	.long	Times__uTimes_var_i8_L
	.long	L_1+1141
	.long	Times__Times_param_i8_u8
	.long	L_1+1157
	.long	Times__uTimes_param_i8_u8
	.long	L_1+1175
	.long	Times__Times_var_i8_u8
	.long	L_1+1194
	.long	Times__uTimes_var_i8_u8
	.long	L_1+1210
	.long	Times__Times_param_i8_C
	.long	L_1+1227
	.long	Times__uTimes_param_i8_C
	.long	L_1+1244
	.long	Times__Times_var_i8_C
	.long	L_1+1262
	.long	Times__uTimes_var_i8_C
	.long	L_1+1277
	.long	Times__Times_param_i8_i64
	.long	L_1+1293
	.long	Times__uTimes_param_i8_i64
	.long	L_1+1312
	.long	Times__Times_var_i8_i64
	.long	L_1+1332
	.long	Times__uTimes_var_i8_i64
	.long	L_1+1349
	.long	Times__Times_param_i8_I
	.long	L_1+1367
	.long	Times__uTimes_param_i8_I
	.long	L_1+1384
	.long	Times__Times_var_i8_I
	.long	L_1+1402
	.long	Times__uTimes_var_i8_I
	.long	L_1+1417
	.long	Times__Times_param_i8_i16
	.long	L_1+1433
	.long	Times__uTimes_param_i8_i16
	.long	L_1+1452
	.long	Times__Times_var_i8_i16
	.long	L_1+1472
	.long	Times__uTimes_var_i8_i16
	.long	L_1+1489
	.long	Times__Times_param_i8_i32
	.long	L_1+1507
	.long	Times__uTimes_param_i8_i32
	.long	L_1+1526
	.long	Times__Times_var_i8_i32
	.long	L_1+1546
	.long	Times__uTimes_var_i8_i32
	.long	L_1+1563
	.long	Times__Times_param_i8_LC
	.long	L_1+1581
	.long	Times__uTimes_param_i8_LC
	.long	L_1+1599
	.long	Times__Times_var_i8_LC
	.long	L_1+1618
	.long	Times__uTimes_var_i8_LC
	.long	L_1+1634
	.long	Times__Times_param_i8_u64
	.long	L_1+1651
	.long	Times__uTimes_param_i8_u64
	.long	L_1+1670
	.long	Times__Times_var_i8_u64
	.long	L_1+1690
	.long	Times__uTimes_var_i8_u64
	.long	L_1+1707
	.long	Times__Times_param_i8_u16
	.long	L_1+1725
	.long	Times__uTimes_param_i8_u16
	.long	L_1+1744
	.long	Times__Times_var_i8_u16
	.long	L_1+1764
	.long	Times__uTimes_var_i8_u16
	.long	L_1+1781
	.long	Times__Times_param_L_u32
	.long	L_1+1799
	.long	Times__uTimes_param_L_u32
	.long	L_1+1817
	.long	Times__Times_var_L_u32
	.long	L_1+1836
	.long	Times__uTimes_var_L_u32
	.long	L_1+1852
	.long	Times__Times_param_L_i8
	.long	L_1+1869
	.long	Times__uTimes_param_L_i8
	.long	L_1+1886
	.long	Times__Times_var_L_i8
	.long	L_1+1904
	.long	Times__uTimes_var_L_i8
	.long	L_1+1919
	.long	Times__Times_param_L_L
	.long	L_1+1935
	.long	Times__uTimes_param_L_L
	.long	L_1+1951
	.long	Times__Times_var_L_L
	.long	L_1+1968
	.long	Times__uTimes_var_L_L
	.long	L_1+1982
	.long	Times__Times_param_L_u8
	.long	L_1+1997
	.long	Times__uTimes_param_L_u8
	.long	L_1+2014
	.long	Times__Times_var_L_u8
	.long	L_1+2032
	.long	Times__uTimes_var_L_u8
	.long	L_1+2047
	.long	Times__Times_param_L_C
	.long	L_1+2063
	.long	Times__uTimes_param_L_C
	.long	L_1+2079
	.long	Times__Times_var_L_C
	.long	L_1+2096
	.long	Times__uTimes_var_L_C
	.long	L_1+2110
	.long	Times__Times_param_L_i64
	.long	L_1+2125
	.long	Times__uTimes_param_L_i64
	.long	L_1+2143
	.long	Times__Times_var_L_i64
	.long	L_1+2162
	.long	Times__uTimes_var_L_i64
	.long	L_1+2178
	.long	Times__Times_param_L_I
	.long	L_1+2195
	.long	Times__uTimes_param_L_I
	.long	L_1+2211
	.long	Times__Times_var_L_I
	.long	L_1+2228
	.long	Times__uTimes_var_L_I
	.long	L_1+2242
	.long	Times__Times_param_L_i16
	.long	L_1+2257
	.long	Times__uTimes_param_L_i16
	.long	L_1+2275
	.long	Times__Times_var_L_i16
	.long	L_1+2294
	.long	Times__uTimes_var_L_i16
	.long	L_1+2310
	.long	Times__Times_param_L_i32
	.long	L_1+2327
	.long	Times__uTimes_param_L_i32
	.long	L_1+2345
	.long	Times__Times_var_L_i32
	.long	L_1+2364
	.long	Times__uTimes_var_L_i32
	.long	L_1+2380
	.long	Times__Times_param_L_LC
	.long	L_1+2397
	.long	Times__uTimes_param_L_LC
	.long	L_1+2414
	.long	Times__Times_var_L_LC
	.long	L_1+2432
	.long	Times__uTimes_var_L_LC
	.long	L_1+2447
	.long	Times__Times_param_L_u64
	.long	L_1+2463
	.long	Times__uTimes_param_L_u64
	.long	L_1+2481
	.long	Times__Times_var_L_u64
	.long	L_1+2500
	.long	Times__uTimes_var_L_u64
	.long	L_1+2516
	.long	Times__Times_param_L_u16
	.long	L_1+2533
	.long	Times__uTimes_param_L_u16
	.long	L_1+2551
	.long	Times__Times_var_L_u16
	.long	L_1+2570
	.long	Times__uTimes_var_L_u16
	.long	L_1+2586
	.long	Times__Times_param_u8_u32
	.long	L_1+2603
	.long	Times__uTimes_param_u8_u32
	.long	L_1+2622
	.long	Times__Times_var_u8_u32
	.long	L_1+2642
	.long	Times__uTimes_var_u8_u32
	.long	L_1+2659
	.long	Times__Times_param_u8_i8
	.long	L_1+2677
	.long	Times__uTimes_param_u8_i8
	.long	L_1+2695
	.long	Times__Times_var_u8_i8
	.long	L_1+2714
	.long	Times__uTimes_var_u8_i8
	.long	L_1+2730
	.long	Times__Times_param_u8_L
	.long	L_1+2747
	.long	Times__uTimes_param_u8_L
	.long	L_1+2764
	.long	Times__Times_var_u8_L
	.long	L_1+2782
	.long	Times__uTimes_var_u8_L
	.long	L_1+2797
	.long	Times__Times_param_u8_u8
	.long	L_1+2813
	.long	Times__uTimes_param_u8_u8
	.long	L_1+2831
	.long	Times__Times_var_u8_u8
	.long	L_1+2850
	.long	Times__uTimes_var_u8_u8
	.long	L_1+2866
	.long	Times__Times_param_u8_C
	.long	L_1+2883
	.long	Times__uTimes_param_u8_C
	.long	L_1+2900
	.long	Times__Times_var_u8_C
	.long	L_1+2918
	.long	Times__uTimes_var_u8_C
	.long	L_1+2933
	.long	Times__Times_param_u8_i64
	.long	L_1+2949
	.long	Times__uTimes_param_u8_i64
	.long	L_1+2968
	.long	Times__Times_var_u8_i64
	.long	L_1+2988
	.long	Times__uTimes_var_u8_i64
	.long	L_1+3005
	.long	Times__Times_param_u8_I
	.long	L_1+3023
	.long	Times__uTimes_param_u8_I
	.long	L_1+3040
	.long	Times__Times_var_u8_I
	.long	L_1+3058
	.long	Times__uTimes_var_u8_I
	.long	L_1+3073
	.long	Times__Times_param_u8_i16
	.long	L_1+3089
	.long	Times__uTimes_param_u8_i16
	.long	L_1+3108
	.long	Times__Times_var_u8_i16
	.long	L_1+3128
	.long	Times__uTimes_var_u8_i16
	.long	L_1+3145
	.long	Times__Times_param_u8_i32
	.long	L_1+3163
	.long	Times__uTimes_param_u8_i32
	.long	L_1+3182
	.long	Times__Times_var_u8_i32
	.long	L_1+3202
	.long	Times__uTimes_var_u8_i32
	.long	L_1+3219
	.long	Times__Times_param_u8_LC
	.long	L_1+3237
	.long	Times__uTimes_param_u8_LC
	.long	L_1+3255
	.long	Times__Times_var_u8_LC
	.long	L_1+3274
	.long	Times__uTimes_var_u8_LC
	.long	L_1+3290
	.long	Times__Times_param_u8_u64
	.long	L_1+3307
	.long	Times__uTimes_param_u8_u64
	.long	L_1+3326
	.long	Times__Times_var_u8_u64
	.long	L_1+3346
	.long	Times__uTimes_var_u8_u64
	.long	L_1+3363
	.long	Times__Times_param_u8_u16
	.long	L_1+3381
	.long	Times__uTimes_param_u8_u16
	.long	L_1+3400
	.long	Times__Times_var_u8_u16
	.long	L_1+3420
	.long	Times__uTimes_var_u8_u16
	.long	L_1+3437
	.long	Times__Times_param_f64_f64
	.long	L_1+3455
	.long	Times__Times_var_f64_f64
	.long	L_1+3475
	.long	Times__Times_param_C_u32
	.long	L_1+3493
	.long	Times__uTimes_param_C_u32
	.long	L_1+3511
	.long	Times__Times_var_C_u32
	.long	L_1+3530
	.long	Times__uTimes_var_C_u32
	.long	L_1+3546
	.long	Times__Times_param_C_i8
	.long	L_1+3563
	.long	Times__uTimes_param_C_i8
	.long	L_1+3580
	.long	Times__Times_var_C_i8
	.long	L_1+3598
	.long	Times__uTimes_var_C_i8
	.long	L_1+3613
	.long	Times__Times_param_C_L
	.long	L_1+3629
	.long	Times__uTimes_param_C_L
	.long	L_1+3645
	.long	Times__Times_var_C_L
	.long	L_1+3662
	.long	Times__uTimes_var_C_L
	.long	L_1+3676
	.long	Times__Times_param_C_u8
	.long	L_1+3691
	.long	Times__uTimes_param_C_u8
	.long	L_1+3708
	.long	Times__Times_var_C_u8
	.long	L_1+3726
	.long	Times__uTimes_var_C_u8
	.long	L_1+3741
	.long	Times__Times_param_C_C
	.long	L_1+3757
	.long	Times__uTimes_param_C_C
	.long	L_1+3773
	.long	Times__Times_var_C_C
	.long	L_1+3790
	.long	Times__uTimes_var_C_C
	.long	L_1+3804
	.long	Times__Times_param_C_i64
	.long	L_1+3819
	.long	Times__uTimes_param_C_i64
	.long	L_1+3837
	.long	Times__Times_var_C_i64
	.long	L_1+3856
	.long	Times__uTimes_var_C_i64
	.long	L_1+3872
	.long	Times__Times_param_C_I
	.long	L_1+3889
	.long	Times__uTimes_param_C_I
	.long	L_1+3905
	.long	Times__Times_var_C_I
	.long	L_1+3922
	.long	Times__uTimes_var_C_I
	.long	L_1+3936
	.long	Times__Times_param_C_i16
	.long	L_1+3951
	.long	Times__uTimes_param_C_i16
	.long	L_1+3969
	.long	Times__Times_var_C_i16
	.long	L_1+3988
	.long	Times__uTimes_var_C_i16
	.long	L_1+4004
	.long	Times__Times_param_C_i32
	.long	L_1+4021
	.long	Times__uTimes_param_C_i32
	.long	L_1+4039
	.long	Times__Times_var_C_i32
	.long	L_1+4058
	.long	Times__uTimes_var_C_i32
	.long	L_1+4074
	.long	Times__Times_param_C_LC
	.long	L_1+4091
	.long	Times__uTimes_param_C_LC
	.long	L_1+4108
	.long	Times__Times_var_C_LC
	.long	L_1+4126
	.long	Times__uTimes_var_C_LC
	.long	L_1+4141
	.long	Times__Times_param_C_u64
	.long	L_1+4157
	.long	Times__uTimes_param_C_u64
	.long	L_1+4175
	.long	Times__Times_var_C_u64
	.long	L_1+4194
	.long	Times__uTimes_var_C_u64
	.long	L_1+4210
	.long	Times__Times_param_C_u16
	.long	L_1+4227
	.long	Times__uTimes_param_C_u16
	.long	L_1+4245
	.long	Times__Times_var_C_u16
	.long	L_1+4264
	.long	Times__uTimes_var_C_u16
	.long	L_1+4280
	.long	Times__Times_param_i64_u32
	.long	L_1+4297
	.long	Times__uTimes_param_i64_u32
	.long	L_1+4317
	.long	Times__Times_var_i64_u32
	.long	L_1+4338
	.long	Times__uTimes_var_i64_u32
	.long	L_1+4356
	.long	Times__Times_param_i64_i8
	.long	L_1+4375
	.long	Times__uTimes_param_i64_i8
	.long	L_1+4394
	.long	Times__Times_var_i64_i8
	.long	L_1+4414
	.long	Times__uTimes_var_i64_i8
	.long	L_1+4431
	.long	Times__Times_param_i64_L
	.long	L_1+4449
	.long	Times__uTimes_param_i64_L
	.long	L_1+4467
	.long	Times__Times_var_i64_L
	.long	L_1+4486
	.long	Times__uTimes_var_i64_L
	.long	L_1+4502
	.long	Times__Times_param_i64_u8
	.long	L_1+4519
	.long	Times__uTimes_param_i64_u8
	.long	L_1+4538
	.long	Times__Times_var_i64_u8
	.long	L_1+4558
	.long	Times__uTimes_var_i64_u8
	.long	L_1+4575
	.long	Times__Times_param_i64_C
	.long	L_1+4593
	.long	Times__uTimes_param_i64_C
	.long	L_1+4611
	.long	Times__Times_var_i64_C
	.long	L_1+4630
	.long	Times__uTimes_var_i64_C
	.long	L_1+4646
	.long	Times__Times_param_i64_i64
	.long	L_1+4663
	.long	Times__uTimes_param_i64_i64
	.long	L_1+4683
	.long	Times__Times_var_i64_i64
	.long	L_1+4704
	.long	Times__uTimes_var_i64_i64
	.long	L_1+4722
	.long	Times__Times_param_i64_I
	.long	L_1+4741
	.long	Times__uTimes_param_i64_I
	.long	L_1+4759
	.long	Times__Times_var_i64_I
	.long	L_1+4778
	.long	Times__uTimes_var_i64_I
	.long	L_1+4794
	.long	Times__Times_param_i64_i16
	.long	L_1+4811
	.long	Times__uTimes_param_i64_i16
	.long	L_1+4831
	.long	Times__Times_var_i64_i16
	.long	L_1+4852
	.long	Times__uTimes_var_i64_i16
	.long	L_1+4870
	.long	Times__Times_param_i64_i32
	.long	L_1+4889
	.long	Times__uTimes_param_i64_i32
	.long	L_1+4909
	.long	Times__Times_var_i64_i32
	.long	L_1+4930
	.long	Times__uTimes_var_i64_i32
	.long	L_1+4948
	.long	Times__Times_param_i64_LC
	.long	L_1+4967
	.long	Times__uTimes_param_i64_LC
	.long	L_1+4986
	.long	Times__Times_var_i64_LC
	.long	L_1+5006
	.long	Times__uTimes_var_i64_LC
	.long	L_1+5023
	.long	Times__Times_param_i64_u64
	.long	L_1+5041
	.long	Times__uTimes_param_i64_u64
	.long	L_1+5061
	.long	Times__Times_var_i64_u64
	.long	L_1+5082
	.long	Times__uTimes_var_i64_u64
	.long	L_1+5100
	.long	Times__Times_param_i64_u16
	.long	L_1+5119
	.long	Times__uTimes_param_i64_u16
	.long	L_1+5139
	.long	Times__Times_var_i64_u16
	.long	L_1+5160
	.long	Times__uTimes_var_i64_u16
	.long	L_1+5178
	.long	Times__Times_param_I_u32
	.long	L_1+5197
	.long	Times__uTimes_param_I_u32
	.long	L_1+5215
	.long	Times__Times_var_I_u32
	.long	L_1+5234
	.long	Times__uTimes_var_I_u32
	.long	L_1+5250
	.long	Times__Times_param_I_i8
	.long	L_1+5267
	.long	Times__uTimes_param_I_i8
	.long	L_1+5284
	.long	Times__Times_var_I_i8
	.long	L_1+5302
	.long	Times__uTimes_var_I_i8
	.long	L_1+5317
	.long	Times__Times_param_I_L
	.long	L_1+5333
	.long	Times__uTimes_param_I_L
	.long	L_1+5349
	.long	Times__Times_var_I_L
	.long	L_1+5366
	.long	Times__uTimes_var_I_L
	.long	L_1+5380
	.long	Times__Times_param_I_u8
	.long	L_1+5395
	.long	Times__uTimes_param_I_u8
	.long	L_1+5412
	.long	Times__Times_var_I_u8
	.long	L_1+5430
	.long	Times__uTimes_var_I_u8
	.long	L_1+5445
	.long	Times__Times_param_I_C
	.long	L_1+5461
	.long	Times__uTimes_param_I_C
	.long	L_1+5477
	.long	Times__Times_var_I_C
	.long	L_1+5494
	.long	Times__uTimes_var_I_C
	.long	L_1+5508
	.long	Times__Times_param_I_i64
	.long	L_1+5523
	.long	Times__uTimes_param_I_i64
	.long	L_1+5541
	.long	Times__Times_var_I_i64
	.long	L_1+5560
	.long	Times__uTimes_var_I_i64
	.long	L_1+5576
	.long	Times__Times_param_I_I
	.long	L_1+5593
	.long	Times__uTimes_param_I_I
	.long	L_1+5609
	.long	Times__Times_var_I_I
	.long	L_1+5626
	.long	Times__uTimes_var_I_I
	.long	L_1+5640
	.long	Times__Times_param_I_i16
	.long	L_1+5655
	.long	Times__uTimes_param_I_i16
	.long	L_1+5673
	.long	Times__Times_var_I_i16
	.long	L_1+5692
	.long	Times__uTimes_var_I_i16
	.long	L_1+5708
	.long	Times__Times_param_I_i32
	.long	L_1+5725
	.long	Times__uTimes_param_I_i32
	.long	L_1+5743
	.long	Times__Times_var_I_i32
	.long	L_1+5762
	.long	Times__uTimes_var_I_i32
	.long	L_1+5778
	.long	Times__Times_param_I_LC
	.long	L_1+5795
	.long	Times__uTimes_param_I_LC
	.long	L_1+5812
	.long	Times__Times_var_I_LC
	.long	L_1+5830
	.long	Times__uTimes_var_I_LC
	.long	L_1+5845
	.long	Times__Times_param_I_u64
	.long	L_1+5861
	.long	Times__uTimes_param_I_u64
	.long	L_1+5879
	.long	Times__Times_var_I_u64
	.long	L_1+5898
	.long	Times__uTimes_var_I_u64
	.long	L_1+5914
	.long	Times__Times_param_I_u16
	.long	L_1+5931
	.long	Times__uTimes_param_I_u16
	.long	L_1+5949
	.long	Times__Times_var_I_u16
	.long	L_1+5968
	.long	Times__uTimes_var_I_u16
	.long	L_1+5984
	.long	Times__Times_param_i16_u32
	.long	L_1+6001
	.long	Times__uTimes_param_i16_u32
	.long	L_1+6021
	.long	Times__Times_var_i16_u32
	.long	L_1+6042
	.long	Times__uTimes_var_i16_u32
	.long	L_1+6060
	.long	Times__Times_param_i16_i8
	.long	L_1+6079
	.long	Times__uTimes_param_i16_i8
	.long	L_1+6098
	.long	Times__Times_var_i16_i8
	.long	L_1+6118
	.long	Times__uTimes_var_i16_i8
	.long	L_1+6135
	.long	Times__Times_param_i16_L
	.long	L_1+6153
	.long	Times__uTimes_param_i16_L
	.long	L_1+6171
	.long	Times__Times_var_i16_L
	.long	L_1+6190
	.long	Times__uTimes_var_i16_L
	.long	L_1+6206
	.long	Times__Times_param_i16_u8
	.long	L_1+6223
	.long	Times__uTimes_param_i16_u8
	.long	L_1+6242
	.long	Times__Times_var_i16_u8
	.long	L_1+6262
	.long	Times__uTimes_var_i16_u8
	.long	L_1+6279
	.long	Times__Times_param_i16_C
	.long	L_1+6297
	.long	Times__uTimes_param_i16_C
	.long	L_1+6315
	.long	Times__Times_var_i16_C
	.long	L_1+6334
	.long	Times__uTimes_var_i16_C
	.long	L_1+6350
	.long	Times__Times_param_i16_i64
	.long	L_1+6367
	.long	Times__uTimes_param_i16_i64
	.long	L_1+6387
	.long	Times__Times_var_i16_i64
	.long	L_1+6408
	.long	Times__uTimes_var_i16_i64
	.long	L_1+6426
	.long	Times__Times_param_i16_I
	.long	L_1+6445
	.long	Times__uTimes_param_i16_I
	.long	L_1+6463
	.long	Times__Times_var_i16_I
	.long	L_1+6482
	.long	Times__uTimes_var_i16_I
	.long	L_1+6498
	.long	Times__Times_param_i16_i16
	.long	L_1+6515
	.long	Times__uTimes_param_i16_i16
	.long	L_1+6535
	.long	Times__Times_var_i16_i16
	.long	L_1+6556
	.long	Times__uTimes_var_i16_i16
	.long	L_1+6574
	.long	Times__Times_param_i16_i32
	.long	L_1+6593
	.long	Times__uTimes_param_i16_i32
	.long	L_1+6613
	.long	Times__Times_var_i16_i32
	.long	L_1+6634
	.long	Times__uTimes_var_i16_i32
	.long	L_1+6652
	.long	Times__Times_param_i16_LC
	.long	L_1+6671
	.long	Times__uTimes_param_i16_LC
	.long	L_1+6690
	.long	Times__Times_var_i16_LC
	.long	L_1+6710
	.long	Times__uTimes_var_i16_LC
	.long	L_1+6727
	.long	Times__Times_param_i16_u64
	.long	L_1+6745
	.long	Times__uTimes_param_i16_u64
	.long	L_1+6765
	.long	Times__Times_var_i16_u64
	.long	L_1+6786
	.long	Times__uTimes_var_i16_u64
	.long	L_1+6804
	.long	Times__Times_param_i16_u16
	.long	L_1+6823
	.long	Times__uTimes_param_i16_u16
	.long	L_1+6843
	.long	Times__Times_var_i16_u16
	.long	L_1+6864
	.long	Times__uTimes_var_i16_u16
	.long	L_1+6882
	.long	Times__Times_param_i32_u32
	.long	L_1+6901
	.long	Times__uTimes_param_i32_u32
	.long	L_1+6921
	.long	Times__Times_var_i32_u32
	.long	L_1+6942
	.long	Times__uTimes_var_i32_u32
	.long	L_1+6960
	.long	Times__Times_param_i32_i8
	.long	L_1+6979
	.long	Times__uTimes_param_i32_i8
	.long	L_1+6998
	.long	Times__Times_var_i32_i8
	.long	L_1+7018
	.long	Times__uTimes_var_i32_i8
	.long	L_1+7035
	.long	Times__Times_param_i32_L
	.long	L_1+7053
	.long	Times__uTimes_param_i32_L
	.long	L_1+7071
	.long	Times__Times_var_i32_L
	.long	L_1+7090
	.long	Times__uTimes_var_i32_L
	.long	L_1+7106
	.long	Times__Times_param_i32_u8
	.long	L_1+7123
	.long	Times__uTimes_param_i32_u8
	.long	L_1+7142
	.long	Times__Times_var_i32_u8
	.long	L_1+7162
	.long	Times__uTimes_var_i32_u8
	.long	L_1+7179
	.long	Times__Times_param_i32_C
	.long	L_1+7197
	.long	Times__uTimes_param_i32_C
	.long	L_1+7215
	.long	Times__Times_var_i32_C
	.long	L_1+7234
	.long	Times__uTimes_var_i32_C
	.long	L_1+7250
	.long	Times__Times_param_i32_i64
	.long	L_1+7267
	.long	Times__uTimes_param_i32_i64
	.long	L_1+7287
	.long	Times__Times_var_i32_i64
	.long	L_1+7308
	.long	Times__uTimes_var_i32_i64
	.long	L_1+7326
	.long	Times__Times_param_i32_I
	.long	L_1+7345
	.long	Times__uTimes_param_i32_I
	.long	L_1+7363
	.long	Times__Times_var_i32_I
	.long	L_1+7382
	.long	Times__uTimes_var_i32_I
	.long	L_1+7398
	.long	Times__Times_param_i32_i16
	.long	L_1+7415
	.long	Times__uTimes_param_i32_i16
	.long	L_1+7435
	.long	Times__Times_var_i32_i16
	.long	L_1+7456
	.long	Times__uTimes_var_i32_i16
	.long	L_1+7474
	.long	Times__Times_param_i32_i32
	.long	L_1+7493
	.long	Times__uTimes_param_i32_i32
	.long	L_1+7513
	.long	Times__Times_var_i32_i32
	.long	L_1+7534
	.long	Times__uTimes_var_i32_i32
	.long	L_1+7552
	.long	Times__Times_param_i32_LC
	.long	L_1+7571
	.long	Times__uTimes_param_i32_LC
	.long	L_1+7590
	.long	Times__Times_var_i32_LC
	.long	L_1+7610
	.long	Times__uTimes_var_i32_LC
	.long	L_1+7627
	.long	Times__Times_param_i32_u64
	.long	L_1+7645
	.long	Times__uTimes_param_i32_u64
	.long	L_1+7665
	.long	Times__Times_var_i32_u64
	.long	L_1+7686
	.long	Times__uTimes_var_i32_u64
	.long	L_1+7704
	.long	Times__Times_param_i32_u16
	.long	L_1+7723
	.long	Times__uTimes_param_i32_u16
	.long	L_1+7743
	.long	Times__Times_var_i32_u16
	.long	L_1+7764
	.long	Times__uTimes_var_i32_u16
	.long	L_1+7782
	.long	Times__Times_param_LC_u32
	.long	L_1+7801
	.long	Times__uTimes_param_LC_u32
	.long	L_1+7820
	.long	Times__Times_var_LC_u32
	.long	L_1+7840
	.long	Times__uTimes_var_LC_u32
	.long	L_1+7857
	.long	Times__Times_param_LC_i8
	.long	L_1+7875
	.long	Times__uTimes_param_LC_i8
	.long	L_1+7893
	.long	Times__Times_var_LC_i8
	.long	L_1+7912
	.long	Times__uTimes_var_LC_i8
	.long	L_1+7928
	.long	Times__Times_param_LC_L
	.long	L_1+7945
	.long	Times__uTimes_param_LC_L
	.long	L_1+7962
	.long	Times__Times_var_LC_L
	.long	L_1+7980
	.long	Times__uTimes_var_LC_L
	.long	L_1+7995
	.long	Times__Times_param_LC_u8
	.long	L_1+8011
	.long	Times__uTimes_param_LC_u8
	.long	L_1+8029
	.long	Times__Times_var_LC_u8
	.long	L_1+8048
	.long	Times__uTimes_var_LC_u8
	.long	L_1+8064
	.long	Times__Times_param_LC_C
	.long	L_1+8081
	.long	Times__uTimes_param_LC_C
	.long	L_1+8098
	.long	Times__Times_var_LC_C
	.long	L_1+8116
	.long	Times__uTimes_var_LC_C
	.long	L_1+8131
	.long	Times__Times_param_LC_i64
	.long	L_1+8147
	.long	Times__uTimes_param_LC_i64
	.long	L_1+8166
	.long	Times__Times_var_LC_i64
	.long	L_1+8186
	.long	Times__uTimes_var_LC_i64
	.long	L_1+8203
	.long	Times__Times_param_LC_I
	.long	L_1+8221
	.long	Times__uTimes_param_LC_I
	.long	L_1+8238
	.long	Times__Times_var_LC_I
	.long	L_1+8256
	.long	Times__uTimes_var_LC_I
	.long	L_1+8271
	.long	Times__Times_param_LC_i16
	.long	L_1+8287
	.long	Times__uTimes_param_LC_i16
	.long	L_1+8306
	.long	Times__Times_var_LC_i16
	.long	L_1+8326
	.long	Times__uTimes_var_LC_i16
	.long	L_1+8343
	.long	Times__Times_param_LC_i32
	.long	L_1+8361
	.long	Times__uTimes_param_LC_i32
	.long	L_1+8380
	.long	Times__Times_var_LC_i32
	.long	L_1+8400
	.long	Times__uTimes_var_LC_i32
	.long	L_1+8417
	.long	Times__Times_param_LC_LC
	.long	L_1+8435
	.long	Times__uTimes_param_LC_LC
	.long	L_1+8453
	.long	Times__Times_var_LC_LC
	.long	L_1+8472
	.long	Times__uTimes_var_LC_LC
	.long	L_1+8488
	.long	Times__Times_param_LC_u64
	.long	L_1+8505
	.long	Times__uTimes_param_LC_u64
	.long	L_1+8524
	.long	Times__Times_var_LC_u64
	.long	L_1+8544
	.long	Times__uTimes_var_LC_u64
	.long	L_1+8561
	.long	Times__Times_param_LC_u16
	.long	L_1+8579
	.long	Times__uTimes_param_LC_u16
	.long	L_1+8598
	.long	Times__Times_var_LC_u16
	.long	L_1+8618
	.long	Times__uTimes_var_LC_u16
	.long	L_1+8635
	.long	Times__Times_param_u64_u32
	.long	L_1+8653
	.long	Times__uTimes_param_u64_u32
	.long	L_1+8673
	.long	Times__Times_var_u64_u32
	.long	L_1+8694
	.long	Times__uTimes_var_u64_u32
	.long	L_1+8712
	.long	Times__Times_param_u64_i8
	.long	L_1+8731
	.long	Times__uTimes_param_u64_i8
	.long	L_1+8750
	.long	Times__Times_var_u64_i8
	.long	L_1+8770
	.long	Times__uTimes_var_u64_i8
	.long	L_1+8787
	.long	Times__Times_param_u64_L
	.long	L_1+8805
	.long	Times__uTimes_param_u64_L
	.long	L_1+8823
	.long	Times__Times_var_u64_L
	.long	L_1+8842
	.long	Times__uTimes_var_u64_L
	.long	L_1+8858
	.long	Times__Times_param_u64_u8
	.long	L_1+8875
	.long	Times__uTimes_param_u64_u8
	.long	L_1+8894
	.long	Times__Times_var_u64_u8
	.long	L_1+8914
	.long	Times__uTimes_var_u64_u8
	.long	L_1+8931
	.long	Times__Times_param_u64_C
	.long	L_1+8949
	.long	Times__uTimes_param_u64_C
	.long	L_1+8967
	.long	Times__Times_var_u64_C
	.long	L_1+8986
	.long	Times__uTimes_var_u64_C
	.long	L_1+9002
	.long	Times__Times_param_u64_i64
	.long	L_1+9019
	.long	Times__uTimes_param_u64_i64
	.long	L_1+9039
	.long	Times__Times_var_u64_i64
	.long	L_1+9060
	.long	Times__uTimes_var_u64_i64
	.long	L_1+9078
	.long	Times__Times_param_u64_I
	.long	L_1+9097
	.long	Times__uTimes_param_u64_I
	.long	L_1+9115
	.long	Times__Times_var_u64_I
	.long	L_1+9134
	.long	Times__uTimes_var_u64_I
	.long	L_1+9150
	.long	Times__Times_param_u64_i16
	.long	L_1+9167
	.long	Times__uTimes_param_u64_i16
	.long	L_1+9187
	.long	Times__Times_var_u64_i16
	.long	L_1+9208
	.long	Times__uTimes_var_u64_i16
	.long	L_1+9226
	.long	Times__Times_param_u64_i32
	.long	L_1+9245
	.long	Times__uTimes_param_u64_i32
	.long	L_1+9265
	.long	Times__Times_var_u64_i32
	.long	L_1+9286
	.long	Times__uTimes_var_u64_i32
	.long	L_1+9304
	.long	Times__Times_param_u64_LC
	.long	L_1+9323
	.long	Times__uTimes_param_u64_LC
	.long	L_1+9342
	.long	Times__Times_var_u64_LC
	.long	L_1+9362
	.long	Times__uTimes_var_u64_LC
	.long	L_1+9379
	.long	Times__Times_param_u64_u64
	.long	L_1+9397
	.long	Times__uTimes_param_u64_u64
	.long	L_1+9417
	.long	Times__Times_var_u64_u64
	.long	L_1+9438
	.long	Times__uTimes_var_u64_u64
	.long	L_1+9456
	.long	Times__Times_param_u64_u16
	.long	L_1+9475
	.long	Times__uTimes_param_u64_u16
	.long	L_1+9495
	.long	Times__Times_var_u64_u16
	.long	L_1+9516
	.long	Times__uTimes_var_u64_u16
	.long	L_1+9534
	.long	Times__Times_param_u16_u32
	.long	L_1+9553
	.long	Times__uTimes_param_u16_u32
	.long	L_1+9573
	.long	Times__Times_var_u16_u32
	.long	L_1+9594
	.long	Times__uTimes_var_u16_u32
	.long	L_1+9612
	.long	Times__Times_param_u16_i8
	.long	L_1+9631
	.long	Times__uTimes_param_u16_i8
	.long	L_1+9650
	.long	Times__Times_var_u16_i8
	.long	L_1+9670
	.long	Times__uTimes_var_u16_i8
	.long	L_1+9687
	.long	Times__Times_param_u16_L
	.long	L_1+9705
	.long	Times__uTimes_param_u16_L
	.long	L_1+9723
	.long	Times__Times_var_u16_L
	.long	L_1+9742
	.long	Times__uTimes_var_u16_L
	.long	L_1+9758
	.long	Times__Times_param_u16_u8
	.long	L_1+9775
	.long	Times__uTimes_param_u16_u8
	.long	L_1+9794
	.long	Times__Times_var_u16_u8
	.long	L_1+9814
	.long	Times__uTimes_var_u16_u8
	.long	L_1+9831
	.long	Times__Times_param_u16_C
	.long	L_1+9849
	.long	Times__uTimes_param_u16_C
	.long	L_1+9867
	.long	Times__Times_var_u16_C
	.long	L_1+9886
	.long	Times__uTimes_var_u16_C
	.long	L_1+9902
	.long	Times__Times_param_u16_i64
	.long	L_1+9919
	.long	Times__uTimes_param_u16_i64
	.long	L_1+9939
	.long	Times__Times_var_u16_i64
	.long	L_1+9960
	.long	Times__uTimes_var_u16_i64
	.long	L_1+9978
	.long	Times__Times_param_u16_I
	.long	L_1+9997
	.long	Times__uTimes_param_u16_I
	.long	L_1+10015
	.long	Times__Times_var_u16_I
	.long	L_1+10034
	.long	Times__uTimes_var_u16_I
	.long	L_1+10050
	.long	Times__Times_param_u16_i16
	.long	L_1+10067
	.long	Times__uTimes_param_u16_i16
	.long	L_1+10087
	.long	Times__Times_var_u16_i16
	.long	L_1+10108
	.long	Times__uTimes_var_u16_i16
	.long	L_1+10126
	.long	Times__Times_param_u16_i32
	.long	L_1+10145
	.long	Times__uTimes_param_u16_i32
	.long	L_1+10165
	.long	Times__Times_var_u16_i32
	.long	L_1+10186
	.long	Times__uTimes_var_u16_i32
	.long	L_1+10204
	.long	Times__Times_param_u16_LC
	.long	L_1+10223
	.long	Times__uTimes_param_u16_LC
	.long	L_1+10242
	.long	Times__Times_var_u16_LC
	.long	L_1+10262
	.long	Times__uTimes_var_u16_LC
	.long	L_1+10279
	.long	Times__Times_param_u16_u64
	.long	L_1+10297
	.long	Times__uTimes_param_u16_u64
	.long	L_1+10317
	.long	Times__Times_var_u16_u64
	.long	L_1+10338
	.long	Times__uTimes_var_u16_u64
	.long	L_1+10356
	.long	Times__Times_param_u16_u16
	.long	L_1+10375
	.long	Times__uTimes_param_u16_u16
	.long	L_1+10395
	.long	Times__Times_var_u16_u16
	.long	L_1+10416
	.long	Times__uTimes_var_u16_u16
	.long	L_1+10434
	.skip 4
	.ascii	"../SOLsun/Times.m3"
	.skip 2
	.align 8
	.type	MM_Times, #object
	.size	MM_Times, 208
MM_Times:
	.long	L_1+15108
	.skip 16
	.long	L_1+10456
	.skip 12
	.long	MM_Times+148
	.skip 4
	.long	Times_M3
	.long	3
	.half	177
	.skip 2
	.long	0
	.long	178
	.long	0
	.long	179
	.long	180
	.half	181
	.skip 2
	.long	182
	.skip 4
	.long	0
	.long	183
	.long	184
	.skip 4
	.long	1080501747
	.long	-1236950581
	.byte	-69
	.skip 7
	.long	0
	.long	188
	.byte	-67
	.skip 3
	.long	190
	.long	1128214823
	.skip 12
	.long	Times_I3
	.long	MM_Times+160
	.skip 4
	.long	Long_I3
	.long	MM_Times+172
	.skip 4
	.long	Word_I3
	.long	MM_Times+184
	.skip 4
	.long	Cstdint_I3
	.long	MM_Times+196
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
