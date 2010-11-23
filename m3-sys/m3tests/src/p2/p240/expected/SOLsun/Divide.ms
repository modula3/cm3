	.file	"Divide.mc"
	.section	".text"
	.align 4
	.type	Divide__uDivide_var_u16_u16, #function
	.proc	04
Divide__uDivide_var_u16_u16:
.LLFB0:
	save	%sp, -96, %sp
.LLCFI0:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE0:
	.size	Divide__uDivide_var_u16_u16, .-Divide__uDivide_var_u16_u16
	.align 4
	.type	Divide__Divide_var_u16_u16, #function
	.proc	015
Divide__Divide_var_u16_u16:
.LLFB1:
	save	%sp, -96, %sp
.LLCFI1:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+52], %o5
	srl	%o5, 16, %o5
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL4
	 nop
	cmp	%g1, 0
	bl	%icc, .LL3
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL7
	 nop
.LL3:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL6
	 nop
.LL4:
	cmp	%g1, 0
	bg	%icc, .LL5
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL7
	 nop
.LL5:
	add	%g1, -1, %g1
.LL6:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL7:
	mov	%g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL8
	 nop
	mov	1761, %o0
	call	_m3_fault, 0
	 nop
.LL8:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+52], %o5
	srl	%o5, 16, %o5
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL10
	 nop
	cmp	%g1, 0
	bl	%icc, .LL9
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL13
	 nop
.LL9:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL12
	 nop
.LL10:
	cmp	%g1, 0
	bg	%icc, .LL11
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL13
	 nop
.LL11:
	add	%g1, -1, %g1
.LL12:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL13:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1:
	.size	Divide__Divide_var_u16_u16, .-Divide__Divide_var_u16_u16
	.align 4
	.type	Divide__uDivide_param_u16_u16, #function
	.proc	04
Divide__uDivide_param_u16_u16:
.LLFB2:
	save	%sp, -96, %sp
.LLCFI2:
	sth	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o4
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE2:
	.size	Divide__uDivide_param_u16_u16, .-Divide__uDivide_param_u16_u16
	.align 4
	.type	Divide__Divide_param_u16_u16, #function
	.proc	015
Divide__Divide_param_u16_u16:
.LLFB3:
	save	%sp, -96, %sp
.LLCFI3:
	sth	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	lduh	[%fp+72], %o5
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL17
	 nop
	cmp	%g1, 0
	bl	%icc, .LL16
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL20
	 nop
.LL16:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL19
	 nop
.LL17:
	cmp	%g1, 0
	bg	%icc, .LL18
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL20
	 nop
.LL18:
	add	%g1, -1, %g1
.LL19:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL20:
	mov	%g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL21
	 nop
	mov	1825, %o0
	call	_m3_fault, 0
	 nop
.LL21:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	lduh	[%fp+72], %o5
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL23
	 nop
	cmp	%g1, 0
	bl	%icc, .LL22
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL26
	 nop
.LL22:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL25
	 nop
.LL23:
	cmp	%g1, 0
	bg	%icc, .LL24
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL26
	 nop
.LL24:
	add	%g1, -1, %g1
.LL25:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL26:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE3:
	.size	Divide__Divide_param_u16_u16, .-Divide__Divide_param_u16_u16
	.global __udivdi3
	.align 4
	.type	Divide__uDivide_var_u16_u64, #function
	.proc	05
Divide__uDivide_var_u16_u64:
.LLFB4:
	save	%sp, -96, %sp
.LLCFI4:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE4:
	.size	Divide__uDivide_var_u16_u64, .-Divide__uDivide_var_u16_u64
	.global __divdi3
	.align 4
	.type	Divide__Divide_var_u16_u64, #function
	.proc	05
Divide__Divide_var_u16_u64:
.LLFB5:
	save	%sp, -96, %sp
.LLCFI5:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o2
	cmp	%o2, 0
	bl	%icc, .LL30
	 nop
	cmp	%o2, 0
	bne	%icc, .LL34
	 nop
	cmp	%o3, 0
.LL34:
	cmp	%o4, 0
	bl	%icc, .LL29
	 nop
	cmp	%o4, 0
	bne	%icc, .LL35
	 nop
	cmp	%o5, 0
.LL35:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL33
	 nop
.LL29:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL32
	 nop
.LL30:
	cmp	%o4, 0
	bg	%icc, .LL31
	 nop
	cmp	%o4, 0
	bne	%icc, .LL36
	 nop
	cmp	%o5, 0
	bne	%icc, .LL31
	 nop
	cmp	%o5, 0
.LL36:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL33
	 nop
.LL31:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL32:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL33:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE5:
	.size	Divide__Divide_var_u16_u64, .-Divide__Divide_var_u16_u64
	.align 4
	.type	Divide__uDivide_param_u16_u64, #function
	.proc	05
Divide__uDivide_param_u16_u64:
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
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE6:
	.size	Divide__uDivide_param_u16_u64, .-Divide__uDivide_param_u16_u64
	.align 4
	.type	Divide__Divide_param_u16_u64, #function
	.proc	05
Divide__Divide_param_u16_u64:
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
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL40
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL44
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL44:
	cmp	%o4, 0
	bl	%icc, .LL39
	 nop
	cmp	%o4, 0
	bne	%icc, .LL45
	 nop
	cmp	%o5, 0
.LL45:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL43
	 nop
.LL39:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL42
	 nop
.LL40:
	cmp	%o4, 0
	bg	%icc, .LL41
	 nop
	cmp	%o4, 0
	bne	%icc, .LL46
	 nop
	cmp	%o5, 0
	bne	%icc, .LL41
	 nop
	cmp	%o5, 0
.LL46:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL43
	 nop
.LL41:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL42:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL43:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE7:
	.size	Divide__Divide_param_u16_u64, .-Divide__Divide_param_u16_u64
	.align 4
	.type	Divide__uDivide_var_u16_LC, #function
	.proc	05
Divide__uDivide_var_u16_LC:
.LLFB8:
	save	%sp, -96, %sp
.LLCFI8:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE8:
	.size	Divide__uDivide_var_u16_LC, .-Divide__uDivide_var_u16_LC
	.align 4
	.type	Divide__Divide_var_u16_LC, #function
	.proc	05
Divide__Divide_var_u16_LC:
.LLFB9:
	save	%sp, -96, %sp
.LLCFI9:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o2
	cmp	%o2, 0
	bl	%icc, .LL50
	 nop
	cmp	%o2, 0
	bne	%icc, .LL54
	 nop
	cmp	%o3, 0
.LL54:
	cmp	%o4, 0
	bl	%icc, .LL49
	 nop
	cmp	%o4, 0
	bne	%icc, .LL55
	 nop
	cmp	%o5, 0
.LL55:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL53
	 nop
.LL49:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL52
	 nop
.LL50:
	cmp	%o4, 0
	bg	%icc, .LL51
	 nop
	cmp	%o4, 0
	bne	%icc, .LL56
	 nop
	cmp	%o5, 0
	bne	%icc, .LL51
	 nop
	cmp	%o5, 0
.LL56:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL53
	 nop
.LL51:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL52:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL53:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE9:
	.size	Divide__Divide_var_u16_LC, .-Divide__Divide_var_u16_LC
	.align 4
	.type	Divide__uDivide_param_u16_LC, #function
	.proc	05
Divide__uDivide_param_u16_LC:
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
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE10:
	.size	Divide__uDivide_param_u16_LC, .-Divide__uDivide_param_u16_LC
	.align 4
	.type	Divide__Divide_param_u16_LC, #function
	.proc	05
Divide__Divide_param_u16_LC:
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
	cmp	%o2, 0
	bl	%icc, .LL60
	 nop
	cmp	%o2, 0
	bne	%icc, .LL64
	 nop
	cmp	%o3, 0
.LL64:
	cmp	%o4, 0
	bl	%icc, .LL59
	 nop
	cmp	%o4, 0
	bne	%icc, .LL65
	 nop
	cmp	%o5, 0
.LL65:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL63
	 nop
.LL59:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL62
	 nop
.LL60:
	cmp	%o4, 0
	bg	%icc, .LL61
	 nop
	cmp	%o4, 0
	bne	%icc, .LL66
	 nop
	cmp	%o5, 0
	bne	%icc, .LL61
	 nop
	cmp	%o5, 0
.LL66:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL63
	 nop
.LL61:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL62:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL63:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE11:
	.size	Divide__Divide_param_u16_LC, .-Divide__Divide_param_u16_LC
	.align 4
	.type	Divide__uDivide_var_u16_i32, #function
	.proc	04
Divide__uDivide_var_u16_i32:
.LLFB12:
	save	%sp, -96, %sp
.LLCFI12:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE12:
	.size	Divide__uDivide_var_u16_i32, .-Divide__uDivide_var_u16_i32
	.align 4
	.type	Divide__Divide_var_u16_i32, #function
	.proc	015
Divide__Divide_var_u16_i32:
.LLFB13:
	save	%sp, -96, %sp
.LLCFI13:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL70
	 nop
	cmp	%g1, 0
	bl	%icc, .LL69
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL73
	 nop
.LL69:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL72
	 nop
.LL70:
	cmp	%g1, 0
	bg	%icc, .LL71
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL73
	 nop
.LL71:
	add	%g1, -1, %g1
.LL72:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL73:
	cmp	%g1, 0
	bl	%icc, .LL74
	 nop
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL76
	 nop
	cmp	%g1, 0
	bl	%icc, .LL75
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL79
	 nop
.LL75:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL78
	 nop
.LL76:
	cmp	%g1, 0
	bg	%icc, .LL77
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL79
	 nop
.LL77:
	add	%g1, -1, %g1
.LL78:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL79:
	mov	%g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL80
	 nop
.LL74:
	mov	2145, %o0
	call	_m3_fault, 0
	 nop
.LL80:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL82
	 nop
	cmp	%g1, 0
	bl	%icc, .LL81
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL85
	 nop
.LL81:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL84
	 nop
.LL82:
	cmp	%g1, 0
	bg	%icc, .LL83
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL85
	 nop
.LL83:
	add	%g1, -1, %g1
.LL84:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL85:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE13:
	.size	Divide__Divide_var_u16_i32, .-Divide__Divide_var_u16_i32
	.align 4
	.type	Divide__uDivide_param_u16_i32, #function
	.proc	04
Divide__uDivide_param_u16_i32:
.LLFB14:
	save	%sp, -96, %sp
.LLCFI14:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o4
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE14:
	.size	Divide__uDivide_param_u16_i32, .-Divide__uDivide_param_u16_i32
	.align 4
	.type	Divide__Divide_param_u16_i32, #function
	.proc	015
Divide__Divide_param_u16_i32:
.LLFB15:
	save	%sp, -96, %sp
.LLCFI15:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL89
	 nop
	cmp	%g1, 0
	bl	%icc, .LL88
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL92
	 nop
.LL88:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL91
	 nop
.LL89:
	cmp	%g1, 0
	bg	%icc, .LL90
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL92
	 nop
.LL90:
	add	%g1, -1, %g1
.LL91:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL92:
	cmp	%g1, 0
	bl	%icc, .LL93
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL95
	 nop
	cmp	%g1, 0
	bl	%icc, .LL94
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL98
	 nop
.LL94:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL97
	 nop
.LL95:
	cmp	%g1, 0
	bg	%icc, .LL96
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL98
	 nop
.LL96:
	add	%g1, -1, %g1
.LL97:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL98:
	mov	%g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL99
	 nop
.LL93:
	mov	2209, %o0
	call	_m3_fault, 0
	 nop
.LL99:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL101
	 nop
	cmp	%g1, 0
	bl	%icc, .LL100
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL104
	 nop
.LL100:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL103
	 nop
.LL101:
	cmp	%g1, 0
	bg	%icc, .LL102
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL104
	 nop
.LL102:
	add	%g1, -1, %g1
.LL103:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL104:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE15:
	.size	Divide__Divide_param_u16_i32, .-Divide__Divide_param_u16_i32
	.align 4
	.type	Divide__uDivide_var_u16_i16, #function
	.proc	04
Divide__uDivide_var_u16_i16:
.LLFB16:
	save	%sp, -96, %sp
.LLCFI16:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE16:
	.size	Divide__uDivide_var_u16_i16, .-Divide__uDivide_var_u16_i16
	.align 4
	.type	Divide__Divide_var_u16_i16, #function
	.proc	015
Divide__Divide_var_u16_i16:
.LLFB17:
	save	%sp, -96, %sp
.LLCFI17:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+76], %o5
	sra	%o5, 16, %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL108
	 nop
	cmp	%g1, 0
	bl	%icc, .LL107
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL111
	 nop
.LL107:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL110
	 nop
.LL108:
	cmp	%g1, 0
	bg	%icc, .LL109
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL111
	 nop
.LL109:
	add	%g1, -1, %g1
.LL110:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL111:
	cmp	%g1, 0
	bl	%icc, .LL112
	 nop
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+76], %o5
	sra	%o5, 16, %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL114
	 nop
	cmp	%g1, 0
	bl	%icc, .LL113
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL117
	 nop
.LL113:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL116
	 nop
.LL114:
	cmp	%g1, 0
	bg	%icc, .LL115
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL117
	 nop
.LL115:
	add	%g1, -1, %g1
.LL116:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL117:
	mov	%g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL118
	 nop
.LL112:
	mov	2273, %o0
	call	_m3_fault, 0
	 nop
.LL118:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+76], %o5
	sra	%o5, 16, %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL120
	 nop
	cmp	%g1, 0
	bl	%icc, .LL119
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL123
	 nop
.LL119:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL122
	 nop
.LL120:
	cmp	%g1, 0
	bg	%icc, .LL121
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL123
	 nop
.LL121:
	add	%g1, -1, %g1
.LL122:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL123:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE17:
	.size	Divide__Divide_var_u16_i16, .-Divide__Divide_var_u16_i16
	.align 4
	.type	Divide__uDivide_param_u16_i16, #function
	.proc	04
Divide__uDivide_param_u16_i16:
.LLFB18:
	save	%sp, -96, %sp
.LLCFI18:
	sth	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o4
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE18:
	.size	Divide__uDivide_param_u16_i16, .-Divide__uDivide_param_u16_i16
	.align 4
	.type	Divide__Divide_param_u16_i16, #function
	.proc	015
Divide__Divide_param_u16_i16:
.LLFB19:
	save	%sp, -96, %sp
.LLCFI19:
	sth	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	lduh	[%fp+72], %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL127
	 nop
	cmp	%g1, 0
	bl	%icc, .LL126
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL130
	 nop
.LL126:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL129
	 nop
.LL127:
	cmp	%g1, 0
	bg	%icc, .LL128
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL130
	 nop
.LL128:
	add	%g1, -1, %g1
.LL129:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL130:
	cmp	%g1, 0
	bl	%icc, .LL131
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	lduh	[%fp+72], %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL133
	 nop
	cmp	%g1, 0
	bl	%icc, .LL132
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL136
	 nop
.LL132:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL135
	 nop
.LL133:
	cmp	%g1, 0
	bg	%icc, .LL134
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL136
	 nop
.LL134:
	add	%g1, -1, %g1
.LL135:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL136:
	mov	%g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL137
	 nop
.LL131:
	mov	2337, %o0
	call	_m3_fault, 0
	 nop
.LL137:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	lduh	[%fp+72], %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL139
	 nop
	cmp	%g1, 0
	bl	%icc, .LL138
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL142
	 nop
.LL138:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL141
	 nop
.LL139:
	cmp	%g1, 0
	bg	%icc, .LL140
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL142
	 nop
.LL140:
	add	%g1, -1, %g1
.LL141:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL142:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE19:
	.size	Divide__Divide_param_u16_i16, .-Divide__Divide_param_u16_i16
	.align 4
	.type	Divide__uDivide_var_u16_I, #function
	.proc	04
Divide__uDivide_var_u16_I:
.LLFB20:
	save	%sp, -96, %sp
.LLCFI20:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE20:
	.size	Divide__uDivide_var_u16_I, .-Divide__uDivide_var_u16_I
	.align 4
	.type	Divide__Divide_var_u16_I, #function
	.proc	015
Divide__Divide_var_u16_I:
.LLFB21:
	save	%sp, -96, %sp
.LLCFI21:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+80], %o5
	cmp	%o5, 0
	bl	%icc, .LL146
	 nop
	cmp	%g1, 0
	bl	%icc, .LL145
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL149
	 nop
.LL145:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL148
	 nop
.LL146:
	cmp	%g1, 0
	bg	%icc, .LL147
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL149
	 nop
.LL147:
	add	%g1, -1, %g1
.LL148:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL149:
	cmp	%g1, 0
	bl	%icc, .LL150
	 nop
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+80], %o5
	cmp	%o5, 0
	bl	%icc, .LL152
	 nop
	cmp	%g1, 0
	bl	%icc, .LL151
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL155
	 nop
.LL151:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL154
	 nop
.LL152:
	cmp	%g1, 0
	bg	%icc, .LL153
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL155
	 nop
.LL153:
	add	%g1, -1, %g1
.LL154:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL155:
	mov	%g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL156
	 nop
.LL150:
	mov	2401, %o0
	call	_m3_fault, 0
	 nop
.LL156:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+80], %o5
	cmp	%o5, 0
	bl	%icc, .LL158
	 nop
	cmp	%g1, 0
	bl	%icc, .LL157
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL161
	 nop
.LL157:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL160
	 nop
.LL158:
	cmp	%g1, 0
	bg	%icc, .LL159
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL161
	 nop
.LL159:
	add	%g1, -1, %g1
.LL160:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL161:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE21:
	.size	Divide__Divide_var_u16_I, .-Divide__Divide_var_u16_I
	.align 4
	.type	Divide__uDivide_param_u16_I, #function
	.proc	04
Divide__uDivide_param_u16_I:
.LLFB22:
	save	%sp, -96, %sp
.LLCFI22:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o4
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE22:
	.size	Divide__uDivide_param_u16_I, .-Divide__uDivide_param_u16_I
	.align 4
	.type	Divide__Divide_param_u16_I, #function
	.proc	015
Divide__Divide_param_u16_I:
.LLFB23:
	save	%sp, -96, %sp
.LLCFI23:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL165
	 nop
	cmp	%g1, 0
	bl	%icc, .LL164
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL168
	 nop
.LL164:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL167
	 nop
.LL165:
	cmp	%g1, 0
	bg	%icc, .LL166
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL168
	 nop
.LL166:
	add	%g1, -1, %g1
.LL167:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL168:
	cmp	%g1, 0
	bl	%icc, .LL169
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL171
	 nop
	cmp	%g1, 0
	bl	%icc, .LL170
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL174
	 nop
.LL170:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL173
	 nop
.LL171:
	cmp	%g1, 0
	bg	%icc, .LL172
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL174
	 nop
.LL172:
	add	%g1, -1, %g1
.LL173:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL174:
	mov	%g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL175
	 nop
.LL169:
	mov	2465, %o0
	call	_m3_fault, 0
	 nop
.LL175:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL177
	 nop
	cmp	%g1, 0
	bl	%icc, .LL176
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL180
	 nop
.LL176:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL179
	 nop
.LL177:
	cmp	%g1, 0
	bg	%icc, .LL178
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL180
	 nop
.LL178:
	add	%g1, -1, %g1
.LL179:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL180:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE23:
	.size	Divide__Divide_param_u16_I, .-Divide__Divide_param_u16_I
	.align 4
	.type	Divide__uDivide_var_u16_i64, #function
	.proc	05
Divide__uDivide_var_u16_i64:
.LLFB24:
	save	%sp, -96, %sp
.LLCFI24:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE24:
	.size	Divide__uDivide_var_u16_i64, .-Divide__uDivide_var_u16_i64
	.align 4
	.type	Divide__Divide_var_u16_i64, #function
	.proc	05
Divide__Divide_var_u16_i64:
.LLFB25:
	save	%sp, -96, %sp
.LLCFI25:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o2
	cmp	%o2, 0
	bl	%icc, .LL184
	 nop
	cmp	%o2, 0
	bne	%icc, .LL188
	 nop
	cmp	%o3, 0
.LL188:
	cmp	%o4, 0
	bl	%icc, .LL183
	 nop
	cmp	%o4, 0
	bne	%icc, .LL189
	 nop
	cmp	%o5, 0
.LL189:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL187
	 nop
.LL183:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL186
	 nop
.LL184:
	cmp	%o4, 0
	bg	%icc, .LL185
	 nop
	cmp	%o4, 0
	bne	%icc, .LL190
	 nop
	cmp	%o5, 0
	bne	%icc, .LL185
	 nop
	cmp	%o5, 0
.LL190:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL187
	 nop
.LL185:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL186:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL187:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE25:
	.size	Divide__Divide_var_u16_i64, .-Divide__Divide_var_u16_i64
	.align 4
	.type	Divide__uDivide_param_u16_i64, #function
	.proc	05
Divide__uDivide_param_u16_i64:
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
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE26:
	.size	Divide__uDivide_param_u16_i64, .-Divide__uDivide_param_u16_i64
	.align 4
	.type	Divide__Divide_param_u16_i64, #function
	.proc	05
Divide__Divide_param_u16_i64:
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
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL194
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL198
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL198:
	cmp	%o4, 0
	bl	%icc, .LL193
	 nop
	cmp	%o4, 0
	bne	%icc, .LL199
	 nop
	cmp	%o5, 0
.LL199:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL197
	 nop
.LL193:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL196
	 nop
.LL194:
	cmp	%o4, 0
	bg	%icc, .LL195
	 nop
	cmp	%o4, 0
	bne	%icc, .LL200
	 nop
	cmp	%o5, 0
	bne	%icc, .LL195
	 nop
	cmp	%o5, 0
.LL200:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL197
	 nop
.LL195:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL196:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL197:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE27:
	.size	Divide__Divide_param_u16_i64, .-Divide__Divide_param_u16_i64
	.align 4
	.type	Divide__uDivide_var_u16_C, #function
	.proc	04
Divide__uDivide_var_u16_C:
.LLFB28:
	save	%sp, -96, %sp
.LLCFI28:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE28:
	.size	Divide__uDivide_var_u16_C, .-Divide__uDivide_var_u16_C
	.align 4
	.type	Divide__Divide_var_u16_C, #function
	.proc	015
Divide__Divide_var_u16_C:
.LLFB29:
	save	%sp, -96, %sp
.LLCFI29:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+96], %o5
	cmp	%o5, 0
	bl	%icc, .LL204
	 nop
	cmp	%g1, 0
	bl	%icc, .LL203
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL207
	 nop
.LL203:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL206
	 nop
.LL204:
	cmp	%g1, 0
	bg	%icc, .LL205
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL207
	 nop
.LL205:
	add	%g1, -1, %g1
.LL206:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL207:
	cmp	%g1, 0
	bl	%icc, .LL208
	 nop
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+96], %o5
	cmp	%o5, 0
	bl	%icc, .LL210
	 nop
	cmp	%g1, 0
	bl	%icc, .LL209
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL213
	 nop
.LL209:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL212
	 nop
.LL210:
	cmp	%g1, 0
	bg	%icc, .LL211
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL213
	 nop
.LL211:
	add	%g1, -1, %g1
.LL212:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL213:
	mov	%g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL214
	 nop
.LL208:
	mov	2657, %o0
	call	_m3_fault, 0
	 nop
.LL214:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+96], %o5
	cmp	%o5, 0
	bl	%icc, .LL216
	 nop
	cmp	%g1, 0
	bl	%icc, .LL215
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL219
	 nop
.LL215:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL218
	 nop
.LL216:
	cmp	%g1, 0
	bg	%icc, .LL217
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL219
	 nop
.LL217:
	add	%g1, -1, %g1
.LL218:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL219:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE29:
	.size	Divide__Divide_var_u16_C, .-Divide__Divide_var_u16_C
	.align 4
	.type	Divide__uDivide_param_u16_C, #function
	.proc	04
Divide__uDivide_param_u16_C:
.LLFB30:
	save	%sp, -96, %sp
.LLCFI30:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	wr	%g0, 0, %y
	ld	[%fp+72], %g1
	udiv	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE30:
	.size	Divide__uDivide_param_u16_C, .-Divide__uDivide_param_u16_C
	.align 4
	.type	Divide__Divide_param_u16_C, #function
	.proc	015
Divide__Divide_param_u16_C:
.LLFB31:
	save	%sp, -96, %sp
.LLCFI31:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL223
	 nop
	cmp	%g1, 0
	bl	%icc, .LL222
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL226
	 nop
.LL222:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL225
	 nop
.LL223:
	cmp	%g1, 0
	bg	%icc, .LL224
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL226
	 nop
.LL224:
	add	%g1, -1, %g1
.LL225:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL226:
	cmp	%g1, 0
	bl	%icc, .LL227
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL229
	 nop
	cmp	%g1, 0
	bl	%icc, .LL228
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL232
	 nop
.LL228:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL231
	 nop
.LL229:
	cmp	%g1, 0
	bg	%icc, .LL230
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL232
	 nop
.LL230:
	add	%g1, -1, %g1
.LL231:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL232:
	mov	%g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL233
	 nop
.LL227:
	mov	2721, %o0
	call	_m3_fault, 0
	 nop
.LL233:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL235
	 nop
	cmp	%g1, 0
	bl	%icc, .LL234
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL238
	 nop
.LL234:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL237
	 nop
.LL235:
	cmp	%g1, 0
	bg	%icc, .LL236
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL238
	 nop
.LL236:
	add	%g1, -1, %g1
.LL237:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL238:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE31:
	.size	Divide__Divide_param_u16_C, .-Divide__Divide_param_u16_C
	.align 4
	.type	Divide__uDivide_var_u16_u8, #function
	.proc	04
Divide__uDivide_var_u16_u8:
.LLFB32:
	save	%sp, -96, %sp
.LLCFI32:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE32:
	.size	Divide__uDivide_var_u16_u8, .-Divide__uDivide_var_u16_u8
	.align 4
	.type	Divide__Divide_var_u16_u8, #function
	.proc	015
Divide__Divide_var_u16_u8:
.LLFB33:
	save	%sp, -96, %sp
.LLCFI33:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+112], %o5
	srl	%o5, 24, %o5
	and	%o5, 0xff, %o5
	cmp	%o5, 0
	bl	%icc, .LL242
	 nop
	cmp	%g1, 0
	bl	%icc, .LL241
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL245
	 nop
.LL241:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL244
	 nop
.LL242:
	cmp	%g1, 0
	bg	%icc, .LL243
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL245
	 nop
.LL243:
	add	%g1, -1, %g1
.LL244:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL245:
	mov	%g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL246
	 nop
	mov	2785, %o0
	call	_m3_fault, 0
	 nop
.LL246:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+112], %o5
	srl	%o5, 24, %o5
	and	%o5, 0xff, %o5
	cmp	%o5, 0
	bl	%icc, .LL248
	 nop
	cmp	%g1, 0
	bl	%icc, .LL247
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL251
	 nop
.LL247:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL250
	 nop
.LL248:
	cmp	%g1, 0
	bg	%icc, .LL249
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL251
	 nop
.LL249:
	add	%g1, -1, %g1
.LL250:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL251:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE33:
	.size	Divide__Divide_var_u16_u8, .-Divide__Divide_var_u16_u8
	.align 4
	.type	Divide__uDivide_param_u16_u8, #function
	.proc	04
Divide__uDivide_param_u16_u8:
.LLFB34:
	save	%sp, -96, %sp
.LLCFI34:
	sth	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o4
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE34:
	.size	Divide__uDivide_param_u16_u8, .-Divide__uDivide_param_u16_u8
	.align 4
	.type	Divide__Divide_param_u16_u8, #function
	.proc	015
Divide__Divide_param_u16_u8:
.LLFB35:
	save	%sp, -96, %sp
.LLCFI35:
	sth	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	ldub	[%fp+72], %o5
	and	%o5, 0xff, %o5
	cmp	%o5, 0
	bl	%icc, .LL255
	 nop
	cmp	%g1, 0
	bl	%icc, .LL254
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL258
	 nop
.LL254:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL257
	 nop
.LL255:
	cmp	%g1, 0
	bg	%icc, .LL256
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL258
	 nop
.LL256:
	add	%g1, -1, %g1
.LL257:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL258:
	mov	%g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL259
	 nop
	mov	2849, %o0
	call	_m3_fault, 0
	 nop
.LL259:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	ldub	[%fp+72], %o5
	and	%o5, 0xff, %o5
	cmp	%o5, 0
	bl	%icc, .LL261
	 nop
	cmp	%g1, 0
	bl	%icc, .LL260
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL264
	 nop
.LL260:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL263
	 nop
.LL261:
	cmp	%g1, 0
	bg	%icc, .LL262
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL264
	 nop
.LL262:
	add	%g1, -1, %g1
.LL263:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL264:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE35:
	.size	Divide__Divide_param_u16_u8, .-Divide__Divide_param_u16_u8
	.align 4
	.type	Divide__uDivide_var_u16_L, #function
	.proc	05
Divide__uDivide_var_u16_L:
.LLFB36:
	save	%sp, -96, %sp
.LLCFI36:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE36:
	.size	Divide__uDivide_var_u16_L, .-Divide__uDivide_var_u16_L
	.align 4
	.type	Divide__Divide_var_u16_L, #function
	.proc	05
Divide__Divide_var_u16_L:
.LLFB37:
	save	%sp, -96, %sp
.LLCFI37:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o2
	cmp	%o2, 0
	bl	%icc, .LL268
	 nop
	cmp	%o2, 0
	bne	%icc, .LL272
	 nop
	cmp	%o3, 0
.LL272:
	cmp	%o4, 0
	bl	%icc, .LL267
	 nop
	cmp	%o4, 0
	bne	%icc, .LL273
	 nop
	cmp	%o5, 0
.LL273:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL271
	 nop
.LL267:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL270
	 nop
.LL268:
	cmp	%o4, 0
	bg	%icc, .LL269
	 nop
	cmp	%o4, 0
	bne	%icc, .LL274
	 nop
	cmp	%o5, 0
	bne	%icc, .LL269
	 nop
	cmp	%o5, 0
.LL274:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL271
	 nop
.LL269:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL270:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL271:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE37:
	.size	Divide__Divide_var_u16_L, .-Divide__Divide_var_u16_L
	.align 4
	.type	Divide__uDivide_param_u16_L, #function
	.proc	05
Divide__uDivide_param_u16_L:
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
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE38:
	.size	Divide__uDivide_param_u16_L, .-Divide__uDivide_param_u16_L
	.align 4
	.type	Divide__Divide_param_u16_L, #function
	.proc	05
Divide__Divide_param_u16_L:
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
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL278
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL282
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL282:
	cmp	%o4, 0
	bl	%icc, .LL277
	 nop
	cmp	%o4, 0
	bne	%icc, .LL283
	 nop
	cmp	%o5, 0
.LL283:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL281
	 nop
.LL277:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL280
	 nop
.LL278:
	cmp	%o4, 0
	bg	%icc, .LL279
	 nop
	cmp	%o4, 0
	bne	%icc, .LL284
	 nop
	cmp	%o5, 0
	bne	%icc, .LL279
	 nop
	cmp	%o5, 0
.LL284:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL281
	 nop
.LL279:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL280:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL281:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE39:
	.size	Divide__Divide_param_u16_L, .-Divide__Divide_param_u16_L
	.align 4
	.type	Divide__uDivide_var_u16_i8, #function
	.proc	04
Divide__uDivide_var_u16_i8:
.LLFB40:
	save	%sp, -96, %sp
.LLCFI40:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE40:
	.size	Divide__uDivide_var_u16_i8, .-Divide__uDivide_var_u16_i8
	.align 4
	.type	Divide__Divide_var_u16_i8, #function
	.proc	015
Divide__Divide_var_u16_i8:
.LLFB41:
	save	%sp, -96, %sp
.LLCFI41:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+128], %o5
	sra	%o5, 24, %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	cmp	%o5, 0
	bl	%icc, .LL288
	 nop
	cmp	%g1, 0
	bl	%icc, .LL287
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL291
	 nop
.LL287:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL290
	 nop
.LL288:
	cmp	%g1, 0
	bg	%icc, .LL289
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL291
	 nop
.LL289:
	add	%g1, -1, %g1
.LL290:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL291:
	cmp	%g1, 0
	bl	%icc, .LL292
	 nop
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+128], %o5
	sra	%o5, 24, %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	cmp	%o5, 0
	bl	%icc, .LL294
	 nop
	cmp	%g1, 0
	bl	%icc, .LL293
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL297
	 nop
.LL293:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL296
	 nop
.LL294:
	cmp	%g1, 0
	bg	%icc, .LL295
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL297
	 nop
.LL295:
	add	%g1, -1, %g1
.LL296:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL297:
	mov	%g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL298
	 nop
.LL292:
	mov	3041, %o0
	call	_m3_fault, 0
	 nop
.LL298:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+128], %o5
	sra	%o5, 24, %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	cmp	%o5, 0
	bl	%icc, .LL300
	 nop
	cmp	%g1, 0
	bl	%icc, .LL299
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL303
	 nop
.LL299:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL302
	 nop
.LL300:
	cmp	%g1, 0
	bg	%icc, .LL301
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL303
	 nop
.LL301:
	add	%g1, -1, %g1
.LL302:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL303:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE41:
	.size	Divide__Divide_var_u16_i8, .-Divide__Divide_var_u16_i8
	.align 4
	.type	Divide__uDivide_param_u16_i8, #function
	.proc	04
Divide__uDivide_param_u16_i8:
.LLFB42:
	save	%sp, -96, %sp
.LLCFI42:
	sth	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o4
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE42:
	.size	Divide__uDivide_param_u16_i8, .-Divide__uDivide_param_u16_i8
	.align 4
	.type	Divide__Divide_param_u16_i8, #function
	.proc	015
Divide__Divide_param_u16_i8:
.LLFB43:
	save	%sp, -96, %sp
.LLCFI43:
	sth	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	ldub	[%fp+72], %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	cmp	%o5, 0
	bl	%icc, .LL307
	 nop
	cmp	%g1, 0
	bl	%icc, .LL306
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL310
	 nop
.LL306:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL309
	 nop
.LL307:
	cmp	%g1, 0
	bg	%icc, .LL308
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL310
	 nop
.LL308:
	add	%g1, -1, %g1
.LL309:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL310:
	cmp	%g1, 0
	bl	%icc, .LL311
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	ldub	[%fp+72], %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	cmp	%o5, 0
	bl	%icc, .LL313
	 nop
	cmp	%g1, 0
	bl	%icc, .LL312
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL316
	 nop
.LL312:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL315
	 nop
.LL313:
	cmp	%g1, 0
	bg	%icc, .LL314
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL316
	 nop
.LL314:
	add	%g1, -1, %g1
.LL315:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL316:
	mov	%g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL317
	 nop
.LL311:
	mov	3105, %o0
	call	_m3_fault, 0
	 nop
.LL317:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	ldub	[%fp+72], %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	cmp	%o5, 0
	bl	%icc, .LL319
	 nop
	cmp	%g1, 0
	bl	%icc, .LL318
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL322
	 nop
.LL318:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL321
	 nop
.LL319:
	cmp	%g1, 0
	bg	%icc, .LL320
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL322
	 nop
.LL320:
	add	%g1, -1, %g1
.LL321:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL322:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE43:
	.size	Divide__Divide_param_u16_i8, .-Divide__Divide_param_u16_i8
	.align 4
	.type	Divide__uDivide_var_u16_u32, #function
	.proc	04
Divide__uDivide_var_u16_u32:
.LLFB44:
	save	%sp, -96, %sp
.LLCFI44:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE44:
	.size	Divide__uDivide_var_u16_u32, .-Divide__uDivide_var_u16_u32
	.align 4
	.type	Divide__Divide_var_u16_u32, #function
	.proc	015
Divide__Divide_var_u16_u32:
.LLFB45:
	save	%sp, -96, %sp
.LLCFI45:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+132], %o5
	cmp	%o5, 0
	bl	%icc, .LL326
	 nop
	cmp	%g1, 0
	bl	%icc, .LL325
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL329
	 nop
.LL325:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL328
	 nop
.LL326:
	cmp	%g1, 0
	bg	%icc, .LL327
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL329
	 nop
.LL327:
	add	%g1, -1, %g1
.LL328:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL329:
	cmp	%g1, 0
	bl	%icc, .LL330
	 nop
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+132], %o5
	cmp	%o5, 0
	bl	%icc, .LL332
	 nop
	cmp	%g1, 0
	bl	%icc, .LL331
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL335
	 nop
.LL331:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL334
	 nop
.LL332:
	cmp	%g1, 0
	bg	%icc, .LL333
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL335
	 nop
.LL333:
	add	%g1, -1, %g1
.LL334:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL335:
	mov	%g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL336
	 nop
.LL330:
	mov	3169, %o0
	call	_m3_fault, 0
	 nop
.LL336:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+132], %o5
	cmp	%o5, 0
	bl	%icc, .LL338
	 nop
	cmp	%g1, 0
	bl	%icc, .LL337
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL341
	 nop
.LL337:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL340
	 nop
.LL338:
	cmp	%g1, 0
	bg	%icc, .LL339
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL341
	 nop
.LL339:
	add	%g1, -1, %g1
.LL340:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL341:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE45:
	.size	Divide__Divide_var_u16_u32, .-Divide__Divide_var_u16_u32
	.align 4
	.type	Divide__uDivide_param_u16_u32, #function
	.proc	04
Divide__uDivide_param_u16_u32:
.LLFB46:
	save	%sp, -96, %sp
.LLCFI46:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o4
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE46:
	.size	Divide__uDivide_param_u16_u32, .-Divide__uDivide_param_u16_u32
	.align 4
	.type	Divide__Divide_param_u16_u32, #function
	.proc	015
Divide__Divide_param_u16_u32:
.LLFB47:
	save	%sp, -96, %sp
.LLCFI47:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL345
	 nop
	cmp	%g1, 0
	bl	%icc, .LL344
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL348
	 nop
.LL344:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL347
	 nop
.LL345:
	cmp	%g1, 0
	bg	%icc, .LL346
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL348
	 nop
.LL346:
	add	%g1, -1, %g1
.LL347:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL348:
	cmp	%g1, 0
	bl	%icc, .LL349
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL351
	 nop
	cmp	%g1, 0
	bl	%icc, .LL350
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL354
	 nop
.LL350:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL353
	 nop
.LL351:
	cmp	%g1, 0
	bg	%icc, .LL352
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL354
	 nop
.LL352:
	add	%g1, -1, %g1
.LL353:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL354:
	mov	%g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL355
	 nop
.LL349:
	mov	3233, %o0
	call	_m3_fault, 0
	 nop
.LL355:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL357
	 nop
	cmp	%g1, 0
	bl	%icc, .LL356
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL360
	 nop
.LL356:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL359
	 nop
.LL357:
	cmp	%g1, 0
	bg	%icc, .LL358
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL360
	 nop
.LL358:
	add	%g1, -1, %g1
.LL359:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL360:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE47:
	.size	Divide__Divide_param_u16_u32, .-Divide__Divide_param_u16_u32
	.align 4
	.type	Divide__uDivide_var_u64_u16, #function
	.proc	05
Divide__uDivide_var_u64_u16:
.LLFB48:
	save	%sp, -96, %sp
.LLCFI48:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE48:
	.size	Divide__uDivide_var_u64_u16, .-Divide__uDivide_var_u64_u16
	.align 4
	.type	Divide__Divide_var_u64_u16, #function
	.proc	05
Divide__Divide_var_u64_u16:
.LLFB49:
	save	%sp, -96, %sp
.LLCFI49:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	cmp	%o2, 0
	bl	%icc, .LL364
	 nop
	cmp	%o2, 0
	bne	%icc, .LL368
	 nop
	cmp	%o3, 0
.LL368:
	cmp	%o4, 0
	bl	%icc, .LL363
	 nop
	cmp	%o4, 0
	bne	%icc, .LL369
	 nop
	cmp	%o5, 0
.LL369:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL367
	 nop
.LL363:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL366
	 nop
.LL364:
	cmp	%o4, 0
	bg	%icc, .LL365
	 nop
	cmp	%o4, 0
	bne	%icc, .LL370
	 nop
	cmp	%o5, 0
	bne	%icc, .LL365
	 nop
	cmp	%o5, 0
.LL370:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL367
	 nop
.LL365:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL366:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL367:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE49:
	.size	Divide__Divide_var_u64_u16, .-Divide__Divide_var_u64_u16
	.align 4
	.type	Divide__uDivide_param_u64_u16, #function
	.proc	05
Divide__uDivide_param_u64_u16:
.LLFB50:
	save	%sp, -104, %sp
.LLCFI50:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE50:
	.size	Divide__uDivide_param_u64_u16, .-Divide__uDivide_param_u64_u16
	.align 4
	.type	Divide__Divide_param_u64_u16, #function
	.proc	05
Divide__Divide_param_u64_u16:
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
	cmp	%o2, 0
	bl	%icc, .LL374
	 nop
	cmp	%o2, 0
	bne	%icc, .LL378
	 nop
	cmp	%o3, 0
.LL378:
	cmp	%o4, 0
	bl	%icc, .LL373
	 nop
	cmp	%o4, 0
	bne	%icc, .LL379
	 nop
	cmp	%o5, 0
.LL379:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL377
	 nop
.LL373:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL376
	 nop
.LL374:
	cmp	%o4, 0
	bg	%icc, .LL375
	 nop
	cmp	%o4, 0
	bne	%icc, .LL380
	 nop
	cmp	%o5, 0
	bne	%icc, .LL375
	 nop
	cmp	%o5, 0
.LL380:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL377
	 nop
.LL375:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL376:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL377:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE51:
	.size	Divide__Divide_param_u64_u16, .-Divide__Divide_param_u64_u16
	.align 4
	.type	Divide__uDivide_var_u64_u64, #function
	.proc	05
Divide__uDivide_var_u64_u64:
.LLFB52:
	save	%sp, -96, %sp
.LLCFI52:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE52:
	.size	Divide__uDivide_var_u64_u64, .-Divide__uDivide_var_u64_u64
	.align 4
	.type	Divide__Divide_var_u64_u64, #function
	.proc	05
Divide__Divide_var_u64_u64:
.LLFB53:
	save	%sp, -96, %sp
.LLCFI53:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o2
	cmp	%o2, 0
	bl	%icc, .LL384
	 nop
	cmp	%o2, 0
	bne	%icc, .LL388
	 nop
	cmp	%o3, 0
.LL388:
	cmp	%o4, 0
	bl	%icc, .LL383
	 nop
	cmp	%o4, 0
	bne	%icc, .LL389
	 nop
	cmp	%o5, 0
.LL389:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL387
	 nop
.LL383:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL386
	 nop
.LL384:
	cmp	%o4, 0
	bg	%icc, .LL385
	 nop
	cmp	%o4, 0
	bne	%icc, .LL390
	 nop
	cmp	%o5, 0
	bne	%icc, .LL385
	 nop
	cmp	%o5, 0
.LL390:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL387
	 nop
.LL385:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL386:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL387:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE53:
	.size	Divide__Divide_var_u64_u64, .-Divide__Divide_var_u64_u64
	.align 4
	.type	Divide__uDivide_param_u64_u64, #function
	.proc	05
Divide__uDivide_param_u64_u64:
.LLFB54:
	save	%sp, -112, %sp
.LLCFI54:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE54:
	.size	Divide__uDivide_param_u64_u64, .-Divide__uDivide_param_u64_u64
	.align 4
	.type	Divide__Divide_param_u64_u64, #function
	.proc	05
Divide__Divide_param_u64_u64:
.LLFB55:
	save	%sp, -112, %sp
.LLCFI55:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bl	%icc, .LL394
	 nop
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bne	%icc, .LL398
	 nop
	ld	[%fp-12], %g1
	cmp	%g1, 0
.LL398:
	cmp	%o4, 0
	bl	%icc, .LL393
	 nop
	cmp	%o4, 0
	bne	%icc, .LL399
	 nop
	cmp	%o5, 0
.LL399:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL397
	 nop
.LL393:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL396
	 nop
.LL394:
	cmp	%o4, 0
	bg	%icc, .LL395
	 nop
	cmp	%o4, 0
	bne	%icc, .LL400
	 nop
	cmp	%o5, 0
	bne	%icc, .LL395
	 nop
	cmp	%o5, 0
.LL400:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL397
	 nop
.LL395:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL396:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL397:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE55:
	.size	Divide__Divide_param_u64_u64, .-Divide__Divide_param_u64_u64
	.align 4
	.type	Divide__uDivide_var_u64_LC, #function
	.proc	05
Divide__uDivide_var_u64_LC:
.LLFB56:
	save	%sp, -96, %sp
.LLCFI56:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE56:
	.size	Divide__uDivide_var_u64_LC, .-Divide__uDivide_var_u64_LC
	.align 4
	.type	Divide__Divide_var_u64_LC, #function
	.proc	05
Divide__Divide_var_u64_LC:
.LLFB57:
	save	%sp, -96, %sp
.LLCFI57:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o2
	cmp	%o2, 0
	bl	%icc, .LL404
	 nop
	cmp	%o2, 0
	bne	%icc, .LL408
	 nop
	cmp	%o3, 0
.LL408:
	cmp	%o4, 0
	bl	%icc, .LL403
	 nop
	cmp	%o4, 0
	bne	%icc, .LL409
	 nop
	cmp	%o5, 0
.LL409:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL407
	 nop
.LL403:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL406
	 nop
.LL404:
	cmp	%o4, 0
	bg	%icc, .LL405
	 nop
	cmp	%o4, 0
	bne	%icc, .LL410
	 nop
	cmp	%o5, 0
	bne	%icc, .LL405
	 nop
	cmp	%o5, 0
.LL410:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL407
	 nop
.LL405:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL406:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL407:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE57:
	.size	Divide__Divide_var_u64_LC, .-Divide__Divide_var_u64_LC
	.align 4
	.type	Divide__uDivide_param_u64_LC, #function
	.proc	05
Divide__uDivide_param_u64_LC:
.LLFB58:
	save	%sp, -112, %sp
.LLCFI58:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE58:
	.size	Divide__uDivide_param_u64_LC, .-Divide__uDivide_param_u64_LC
	.align 4
	.type	Divide__Divide_param_u64_LC, #function
	.proc	05
Divide__Divide_param_u64_LC:
.LLFB59:
	save	%sp, -112, %sp
.LLCFI59:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o2
	ldd	[%fp-8], %o4
	cmp	%o2, 0
	bl	%icc, .LL414
	 nop
	cmp	%o2, 0
	bne	%icc, .LL418
	 nop
	cmp	%o3, 0
.LL418:
	cmp	%o4, 0
	bl	%icc, .LL413
	 nop
	cmp	%o4, 0
	bne	%icc, .LL419
	 nop
	cmp	%o5, 0
.LL419:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL417
	 nop
.LL413:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL416
	 nop
.LL414:
	cmp	%o4, 0
	bg	%icc, .LL415
	 nop
	cmp	%o4, 0
	bne	%icc, .LL420
	 nop
	cmp	%o5, 0
	bne	%icc, .LL415
	 nop
	cmp	%o5, 0
.LL420:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL417
	 nop
.LL415:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL416:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL417:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE59:
	.size	Divide__Divide_param_u64_LC, .-Divide__Divide_param_u64_LC
	.align 4
	.type	Divide__uDivide_var_u64_i32, #function
	.proc	05
Divide__uDivide_var_u64_i32:
.LLFB60:
	save	%sp, -96, %sp
.LLCFI60:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o2
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE60:
	.size	Divide__uDivide_var_u64_i32, .-Divide__uDivide_var_u64_i32
	.align 4
	.type	Divide__Divide_var_u64_i32, #function
	.proc	05
Divide__Divide_var_u64_i32:
.LLFB61:
	save	%sp, -96, %sp
.LLCFI61:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o3
	sra	%g1, 31, %g1
	mov	%g1, %o2
	cmp	%o2, 0
	bl	%icc, .LL424
	 nop
	cmp	%o2, 0
	bne	%icc, .LL428
	 nop
	cmp	%o3, 0
.LL428:
	cmp	%o4, 0
	bl	%icc, .LL423
	 nop
	cmp	%o4, 0
	bne	%icc, .LL429
	 nop
	cmp	%o5, 0
.LL429:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL427
	 nop
.LL423:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL426
	 nop
.LL424:
	cmp	%o4, 0
	bg	%icc, .LL425
	 nop
	cmp	%o4, 0
	bne	%icc, .LL430
	 nop
	cmp	%o5, 0
	bne	%icc, .LL425
	 nop
	cmp	%o5, 0
.LL430:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL427
	 nop
.LL425:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL426:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL427:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE61:
	.size	Divide__Divide_var_u64_i32, .-Divide__Divide_var_u64_i32
	.align 4
	.type	Divide__uDivide_param_u64_i32, #function
	.proc	05
Divide__uDivide_param_u64_i32:
.LLFB62:
	save	%sp, -104, %sp
.LLCFI62:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE62:
	.size	Divide__uDivide_param_u64_i32, .-Divide__uDivide_param_u64_i32
	.align 4
	.type	Divide__Divide_param_u64_i32, #function
	.proc	05
Divide__Divide_param_u64_i32:
.LLFB63:
	save	%sp, -104, %sp
.LLCFI63:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o3
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o2
	ldd	[%fp-8], %o4
	cmp	%o2, 0
	bl	%icc, .LL434
	 nop
	cmp	%o2, 0
	bne	%icc, .LL438
	 nop
	cmp	%o3, 0
.LL438:
	cmp	%o4, 0
	bl	%icc, .LL433
	 nop
	cmp	%o4, 0
	bne	%icc, .LL439
	 nop
	cmp	%o5, 0
.LL439:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL437
	 nop
.LL433:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL436
	 nop
.LL434:
	cmp	%o4, 0
	bg	%icc, .LL435
	 nop
	cmp	%o4, 0
	bne	%icc, .LL440
	 nop
	cmp	%o5, 0
	bne	%icc, .LL435
	 nop
	cmp	%o5, 0
.LL440:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL437
	 nop
.LL435:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL436:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL437:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE63:
	.size	Divide__Divide_param_u64_i32, .-Divide__Divide_param_u64_i32
	.align 4
	.type	Divide__uDivide_var_u64_i16, #function
	.proc	05
Divide__uDivide_var_u64_i16:
.LLFB64:
	save	%sp, -96, %sp
.LLCFI64:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o2
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE64:
	.size	Divide__uDivide_var_u64_i16, .-Divide__uDivide_var_u64_i16
	.align 4
	.type	Divide__Divide_var_u64_i16, #function
	.proc	05
Divide__Divide_var_u64_i16:
.LLFB65:
	save	%sp, -96, %sp
.LLCFI65:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o3
	sra	%g1, 31, %g1
	mov	%g1, %o2
	cmp	%o2, 0
	bl	%icc, .LL444
	 nop
	cmp	%o2, 0
	bne	%icc, .LL448
	 nop
	cmp	%o3, 0
.LL448:
	cmp	%o4, 0
	bl	%icc, .LL443
	 nop
	cmp	%o4, 0
	bne	%icc, .LL449
	 nop
	cmp	%o5, 0
.LL449:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL447
	 nop
.LL443:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL446
	 nop
.LL444:
	cmp	%o4, 0
	bg	%icc, .LL445
	 nop
	cmp	%o4, 0
	bne	%icc, .LL450
	 nop
	cmp	%o5, 0
	bne	%icc, .LL445
	 nop
	cmp	%o5, 0
.LL450:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL447
	 nop
.LL445:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL446:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL447:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE65:
	.size	Divide__Divide_var_u64_i16, .-Divide__Divide_var_u64_i16
	.align 4
	.type	Divide__uDivide_param_u64_i16, #function
	.proc	05
Divide__uDivide_param_u64_i16:
.LLFB66:
	save	%sp, -104, %sp
.LLCFI66:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE66:
	.size	Divide__uDivide_param_u64_i16, .-Divide__uDivide_param_u64_i16
	.align 4
	.type	Divide__Divide_param_u64_i16, #function
	.proc	05
Divide__Divide_param_u64_i16:
.LLFB67:
	save	%sp, -104, %sp
.LLCFI67:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o3
	sra	%g1, 31, %g1
	mov	%g1, %o2
	ldd	[%fp-8], %o4
	cmp	%o2, 0
	bl	%icc, .LL454
	 nop
	cmp	%o2, 0
	bne	%icc, .LL458
	 nop
	cmp	%o3, 0
.LL458:
	cmp	%o4, 0
	bl	%icc, .LL453
	 nop
	cmp	%o4, 0
	bne	%icc, .LL459
	 nop
	cmp	%o5, 0
.LL459:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL457
	 nop
.LL453:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL456
	 nop
.LL454:
	cmp	%o4, 0
	bg	%icc, .LL455
	 nop
	cmp	%o4, 0
	bne	%icc, .LL460
	 nop
	cmp	%o5, 0
	bne	%icc, .LL455
	 nop
	cmp	%o5, 0
.LL460:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL457
	 nop
.LL455:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL456:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL457:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE67:
	.size	Divide__Divide_param_u64_i16, .-Divide__Divide_param_u64_i16
	.align 4
	.type	Divide__uDivide_var_u64_I, #function
	.proc	05
Divide__uDivide_var_u64_I:
.LLFB68:
	save	%sp, -96, %sp
.LLCFI68:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o2
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE68:
	.size	Divide__uDivide_var_u64_I, .-Divide__uDivide_var_u64_I
	.align 4
	.type	Divide__Divide_var_u64_I, #function
	.proc	05
Divide__Divide_var_u64_I:
.LLFB69:
	save	%sp, -96, %sp
.LLCFI69:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o3
	sra	%g1, 31, %g1
	mov	%g1, %o2
	cmp	%o2, 0
	bl	%icc, .LL464
	 nop
	cmp	%o2, 0
	bne	%icc, .LL468
	 nop
	cmp	%o3, 0
.LL468:
	cmp	%o4, 0
	bl	%icc, .LL463
	 nop
	cmp	%o4, 0
	bne	%icc, .LL469
	 nop
	cmp	%o5, 0
.LL469:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL467
	 nop
.LL463:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL466
	 nop
.LL464:
	cmp	%o4, 0
	bg	%icc, .LL465
	 nop
	cmp	%o4, 0
	bne	%icc, .LL470
	 nop
	cmp	%o5, 0
	bne	%icc, .LL465
	 nop
	cmp	%o5, 0
.LL470:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL467
	 nop
.LL465:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL466:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL467:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE69:
	.size	Divide__Divide_var_u64_I, .-Divide__Divide_var_u64_I
	.align 4
	.type	Divide__uDivide_param_u64_I, #function
	.proc	05
Divide__uDivide_param_u64_I:
.LLFB70:
	save	%sp, -104, %sp
.LLCFI70:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE70:
	.size	Divide__uDivide_param_u64_I, .-Divide__uDivide_param_u64_I
	.align 4
	.type	Divide__Divide_param_u64_I, #function
	.proc	05
Divide__Divide_param_u64_I:
.LLFB71:
	save	%sp, -104, %sp
.LLCFI71:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o3
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o2
	ldd	[%fp-8], %o4
	cmp	%o2, 0
	bl	%icc, .LL474
	 nop
	cmp	%o2, 0
	bne	%icc, .LL478
	 nop
	cmp	%o3, 0
.LL478:
	cmp	%o4, 0
	bl	%icc, .LL473
	 nop
	cmp	%o4, 0
	bne	%icc, .LL479
	 nop
	cmp	%o5, 0
.LL479:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL477
	 nop
.LL473:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL476
	 nop
.LL474:
	cmp	%o4, 0
	bg	%icc, .LL475
	 nop
	cmp	%o4, 0
	bne	%icc, .LL480
	 nop
	cmp	%o5, 0
	bne	%icc, .LL475
	 nop
	cmp	%o5, 0
.LL480:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL477
	 nop
.LL475:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL476:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL477:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE71:
	.size	Divide__Divide_param_u64_I, .-Divide__Divide_param_u64_I
	.align 4
	.type	Divide__uDivide_var_u64_i64, #function
	.proc	05
Divide__uDivide_var_u64_i64:
.LLFB72:
	save	%sp, -96, %sp
.LLCFI72:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE72:
	.size	Divide__uDivide_var_u64_i64, .-Divide__uDivide_var_u64_i64
	.align 4
	.type	Divide__Divide_var_u64_i64, #function
	.proc	05
Divide__Divide_var_u64_i64:
.LLFB73:
	save	%sp, -96, %sp
.LLCFI73:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o2
	cmp	%o2, 0
	bl	%icc, .LL484
	 nop
	cmp	%o2, 0
	bne	%icc, .LL488
	 nop
	cmp	%o3, 0
.LL488:
	cmp	%o4, 0
	bl	%icc, .LL483
	 nop
	cmp	%o4, 0
	bne	%icc, .LL489
	 nop
	cmp	%o5, 0
.LL489:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL487
	 nop
.LL483:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL486
	 nop
.LL484:
	cmp	%o4, 0
	bg	%icc, .LL485
	 nop
	cmp	%o4, 0
	bne	%icc, .LL490
	 nop
	cmp	%o5, 0
	bne	%icc, .LL485
	 nop
	cmp	%o5, 0
.LL490:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL487
	 nop
.LL485:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL486:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL487:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE73:
	.size	Divide__Divide_var_u64_i64, .-Divide__Divide_var_u64_i64
	.align 4
	.type	Divide__uDivide_param_u64_i64, #function
	.proc	05
Divide__uDivide_param_u64_i64:
.LLFB74:
	save	%sp, -112, %sp
.LLCFI74:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE74:
	.size	Divide__uDivide_param_u64_i64, .-Divide__uDivide_param_u64_i64
	.align 4
	.type	Divide__Divide_param_u64_i64, #function
	.proc	05
Divide__Divide_param_u64_i64:
.LLFB75:
	save	%sp, -112, %sp
.LLCFI75:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bl	%icc, .LL494
	 nop
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bne	%icc, .LL498
	 nop
	ld	[%fp-12], %g1
	cmp	%g1, 0
.LL498:
	cmp	%o4, 0
	bl	%icc, .LL493
	 nop
	cmp	%o4, 0
	bne	%icc, .LL499
	 nop
	cmp	%o5, 0
.LL499:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL497
	 nop
.LL493:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL496
	 nop
.LL494:
	cmp	%o4, 0
	bg	%icc, .LL495
	 nop
	cmp	%o4, 0
	bne	%icc, .LL500
	 nop
	cmp	%o5, 0
	bne	%icc, .LL495
	 nop
	cmp	%o5, 0
.LL500:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL497
	 nop
.LL495:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL496:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL497:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE75:
	.size	Divide__Divide_param_u64_i64, .-Divide__Divide_param_u64_i64
	.align 4
	.type	Divide__uDivide_var_u64_C, #function
	.proc	05
Divide__uDivide_var_u64_C:
.LLFB76:
	save	%sp, -96, %sp
.LLCFI76:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o2
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE76:
	.size	Divide__uDivide_var_u64_C, .-Divide__uDivide_var_u64_C
	.align 4
	.type	Divide__Divide_var_u64_C, #function
	.proc	05
Divide__Divide_var_u64_C:
.LLFB77:
	save	%sp, -96, %sp
.LLCFI77:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o3
	sra	%g1, 31, %g1
	mov	%g1, %o2
	cmp	%o2, 0
	bl	%icc, .LL504
	 nop
	cmp	%o2, 0
	bne	%icc, .LL508
	 nop
	cmp	%o3, 0
.LL508:
	cmp	%o4, 0
	bl	%icc, .LL503
	 nop
	cmp	%o4, 0
	bne	%icc, .LL509
	 nop
	cmp	%o5, 0
.LL509:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL507
	 nop
.LL503:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL506
	 nop
.LL504:
	cmp	%o4, 0
	bg	%icc, .LL505
	 nop
	cmp	%o4, 0
	bne	%icc, .LL510
	 nop
	cmp	%o5, 0
	bne	%icc, .LL505
	 nop
	cmp	%o5, 0
.LL510:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL507
	 nop
.LL505:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL506:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL507:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE77:
	.size	Divide__Divide_var_u64_C, .-Divide__Divide_var_u64_C
	.align 4
	.type	Divide__uDivide_param_u64_C, #function
	.proc	05
Divide__uDivide_param_u64_C:
.LLFB78:
	save	%sp, -104, %sp
.LLCFI78:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	ld	[%fp+76], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE78:
	.size	Divide__uDivide_param_u64_C, .-Divide__uDivide_param_u64_C
	.align 4
	.type	Divide__Divide_param_u64_C, #function
	.proc	05
Divide__Divide_param_u64_C:
.LLFB79:
	save	%sp, -104, %sp
.LLCFI79:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	mov	%g1, %o3
	sra	%g1, 31, %g1
	mov	%g1, %o2
	ldd	[%fp-8], %o4
	cmp	%o2, 0
	bl	%icc, .LL514
	 nop
	cmp	%o2, 0
	bne	%icc, .LL518
	 nop
	cmp	%o3, 0
.LL518:
	cmp	%o4, 0
	bl	%icc, .LL513
	 nop
	cmp	%o4, 0
	bne	%icc, .LL519
	 nop
	cmp	%o5, 0
.LL519:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL517
	 nop
.LL513:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL516
	 nop
.LL514:
	cmp	%o4, 0
	bg	%icc, .LL515
	 nop
	cmp	%o4, 0
	bne	%icc, .LL520
	 nop
	cmp	%o5, 0
	bne	%icc, .LL515
	 nop
	cmp	%o5, 0
.LL520:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL517
	 nop
.LL515:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL516:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL517:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE79:
	.size	Divide__Divide_param_u64_C, .-Divide__Divide_param_u64_C
	.align 4
	.type	Divide__uDivide_var_u64_u8, #function
	.proc	05
Divide__uDivide_var_u64_u8:
.LLFB80:
	save	%sp, -96, %sp
.LLCFI80:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE80:
	.size	Divide__uDivide_var_u64_u8, .-Divide__uDivide_var_u64_u8
	.align 4
	.type	Divide__Divide_var_u64_u8, #function
	.proc	05
Divide__Divide_var_u64_u8:
.LLFB81:
	save	%sp, -96, %sp
.LLCFI81:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	cmp	%o2, 0
	bl	%icc, .LL524
	 nop
	cmp	%o2, 0
	bne	%icc, .LL528
	 nop
	cmp	%o3, 0
.LL528:
	cmp	%o4, 0
	bl	%icc, .LL523
	 nop
	cmp	%o4, 0
	bne	%icc, .LL529
	 nop
	cmp	%o5, 0
.LL529:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL527
	 nop
.LL523:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL526
	 nop
.LL524:
	cmp	%o4, 0
	bg	%icc, .LL525
	 nop
	cmp	%o4, 0
	bne	%icc, .LL530
	 nop
	cmp	%o5, 0
	bne	%icc, .LL525
	 nop
	cmp	%o5, 0
.LL530:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL527
	 nop
.LL525:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL526:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL527:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE81:
	.size	Divide__Divide_var_u64_u8, .-Divide__Divide_var_u64_u8
	.align 4
	.type	Divide__uDivide_param_u64_u8, #function
	.proc	05
Divide__uDivide_param_u64_u8:
.LLFB82:
	save	%sp, -104, %sp
.LLCFI82:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	ldub	[%fp+76], %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE82:
	.size	Divide__uDivide_param_u64_u8, .-Divide__uDivide_param_u64_u8
	.align 4
	.type	Divide__Divide_param_u64_u8, #function
	.proc	05
Divide__Divide_param_u64_u8:
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
	cmp	%o2, 0
	bl	%icc, .LL534
	 nop
	cmp	%o2, 0
	bne	%icc, .LL538
	 nop
	cmp	%o3, 0
.LL538:
	cmp	%o4, 0
	bl	%icc, .LL533
	 nop
	cmp	%o4, 0
	bne	%icc, .LL539
	 nop
	cmp	%o5, 0
.LL539:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL537
	 nop
.LL533:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL536
	 nop
.LL534:
	cmp	%o4, 0
	bg	%icc, .LL535
	 nop
	cmp	%o4, 0
	bne	%icc, .LL540
	 nop
	cmp	%o5, 0
	bne	%icc, .LL535
	 nop
	cmp	%o5, 0
.LL540:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL537
	 nop
.LL535:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL536:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL537:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE83:
	.size	Divide__Divide_param_u64_u8, .-Divide__Divide_param_u64_u8
	.align 4
	.type	Divide__uDivide_var_u64_L, #function
	.proc	05
Divide__uDivide_var_u64_L:
.LLFB84:
	save	%sp, -96, %sp
.LLCFI84:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE84:
	.size	Divide__uDivide_var_u64_L, .-Divide__uDivide_var_u64_L
	.align 4
	.type	Divide__Divide_var_u64_L, #function
	.proc	05
Divide__Divide_var_u64_L:
.LLFB85:
	save	%sp, -96, %sp
.LLCFI85:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o2
	cmp	%o2, 0
	bl	%icc, .LL544
	 nop
	cmp	%o2, 0
	bne	%icc, .LL548
	 nop
	cmp	%o3, 0
.LL548:
	cmp	%o4, 0
	bl	%icc, .LL543
	 nop
	cmp	%o4, 0
	bne	%icc, .LL549
	 nop
	cmp	%o5, 0
.LL549:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL547
	 nop
.LL543:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL546
	 nop
.LL544:
	cmp	%o4, 0
	bg	%icc, .LL545
	 nop
	cmp	%o4, 0
	bne	%icc, .LL550
	 nop
	cmp	%o5, 0
	bne	%icc, .LL545
	 nop
	cmp	%o5, 0
.LL550:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL547
	 nop
.LL545:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL546:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL547:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE85:
	.size	Divide__Divide_var_u64_L, .-Divide__Divide_var_u64_L
	.align 4
	.type	Divide__uDivide_param_u64_L, #function
	.proc	05
Divide__uDivide_param_u64_L:
.LLFB86:
	save	%sp, -112, %sp
.LLCFI86:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE86:
	.size	Divide__uDivide_param_u64_L, .-Divide__uDivide_param_u64_L
	.align 4
	.type	Divide__Divide_param_u64_L, #function
	.proc	05
Divide__Divide_param_u64_L:
.LLFB87:
	save	%sp, -112, %sp
.LLCFI87:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bl	%icc, .LL554
	 nop
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bne	%icc, .LL558
	 nop
	ld	[%fp-12], %g1
	cmp	%g1, 0
.LL558:
	cmp	%o4, 0
	bl	%icc, .LL553
	 nop
	cmp	%o4, 0
	bne	%icc, .LL559
	 nop
	cmp	%o5, 0
.LL559:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL557
	 nop
.LL553:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL556
	 nop
.LL554:
	cmp	%o4, 0
	bg	%icc, .LL555
	 nop
	cmp	%o4, 0
	bne	%icc, .LL560
	 nop
	cmp	%o5, 0
	bne	%icc, .LL555
	 nop
	cmp	%o5, 0
.LL560:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL557
	 nop
.LL555:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL556:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL557:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE87:
	.size	Divide__Divide_param_u64_L, .-Divide__Divide_param_u64_L
	.align 4
	.type	Divide__uDivide_var_u64_i8, #function
	.proc	05
Divide__uDivide_var_u64_i8:
.LLFB88:
	save	%sp, -96, %sp
.LLCFI88:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o2
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE88:
	.size	Divide__uDivide_var_u64_i8, .-Divide__uDivide_var_u64_i8
	.align 4
	.type	Divide__Divide_var_u64_i8, #function
	.proc	05
Divide__Divide_var_u64_i8:
.LLFB89:
	save	%sp, -96, %sp
.LLCFI89:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o3
	sra	%g1, 31, %g1
	mov	%g1, %o2
	cmp	%o2, 0
	bl	%icc, .LL564
	 nop
	cmp	%o2, 0
	bne	%icc, .LL568
	 nop
	cmp	%o3, 0
.LL568:
	cmp	%o4, 0
	bl	%icc, .LL563
	 nop
	cmp	%o4, 0
	bne	%icc, .LL569
	 nop
	cmp	%o5, 0
.LL569:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL567
	 nop
.LL563:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL566
	 nop
.LL564:
	cmp	%o4, 0
	bg	%icc, .LL565
	 nop
	cmp	%o4, 0
	bne	%icc, .LL570
	 nop
	cmp	%o5, 0
	bne	%icc, .LL565
	 nop
	cmp	%o5, 0
.LL570:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL567
	 nop
.LL565:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL566:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL567:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE89:
	.size	Divide__Divide_var_u64_i8, .-Divide__Divide_var_u64_i8
	.align 4
	.type	Divide__uDivide_param_u64_i8, #function
	.proc	05
Divide__uDivide_param_u64_i8:
.LLFB90:
	save	%sp, -104, %sp
.LLCFI90:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	ldub	[%fp+76], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE90:
	.size	Divide__uDivide_param_u64_i8, .-Divide__uDivide_param_u64_i8
	.align 4
	.type	Divide__Divide_param_u64_i8, #function
	.proc	05
Divide__Divide_param_u64_i8:
.LLFB91:
	save	%sp, -104, %sp
.LLCFI91:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o3
	sra	%g1, 31, %g1
	mov	%g1, %o2
	ldd	[%fp-8], %o4
	cmp	%o2, 0
	bl	%icc, .LL574
	 nop
	cmp	%o2, 0
	bne	%icc, .LL578
	 nop
	cmp	%o3, 0
.LL578:
	cmp	%o4, 0
	bl	%icc, .LL573
	 nop
	cmp	%o4, 0
	bne	%icc, .LL579
	 nop
	cmp	%o5, 0
.LL579:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL577
	 nop
.LL573:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL576
	 nop
.LL574:
	cmp	%o4, 0
	bg	%icc, .LL575
	 nop
	cmp	%o4, 0
	bne	%icc, .LL580
	 nop
	cmp	%o5, 0
	bne	%icc, .LL575
	 nop
	cmp	%o5, 0
.LL580:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL577
	 nop
.LL575:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL576:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL577:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE91:
	.size	Divide__Divide_param_u64_i8, .-Divide__Divide_param_u64_i8
	.align 4
	.type	Divide__uDivide_var_u64_u32, #function
	.proc	05
Divide__uDivide_var_u64_u32:
.LLFB92:
	save	%sp, -96, %sp
.LLCFI92:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o2
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE92:
	.size	Divide__uDivide_var_u64_u32, .-Divide__uDivide_var_u64_u32
	.align 4
	.type	Divide__Divide_var_u64_u32, #function
	.proc	05
Divide__Divide_var_u64_u32:
.LLFB93:
	save	%sp, -96, %sp
.LLCFI93:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o3
	sra	%g1, 31, %g1
	mov	%g1, %o2
	cmp	%o2, 0
	bl	%icc, .LL584
	 nop
	cmp	%o2, 0
	bne	%icc, .LL588
	 nop
	cmp	%o3, 0
.LL588:
	cmp	%o4, 0
	bl	%icc, .LL583
	 nop
	cmp	%o4, 0
	bne	%icc, .LL589
	 nop
	cmp	%o5, 0
.LL589:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL587
	 nop
.LL583:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL586
	 nop
.LL584:
	cmp	%o4, 0
	bg	%icc, .LL585
	 nop
	cmp	%o4, 0
	bne	%icc, .LL590
	 nop
	cmp	%o5, 0
	bne	%icc, .LL585
	 nop
	cmp	%o5, 0
.LL590:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL587
	 nop
.LL585:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL586:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL587:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE93:
	.size	Divide__Divide_var_u64_u32, .-Divide__Divide_var_u64_u32
	.align 4
	.type	Divide__uDivide_param_u64_u32, #function
	.proc	05
Divide__uDivide_param_u64_u32:
.LLFB94:
	save	%sp, -104, %sp
.LLCFI94:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE94:
	.size	Divide__uDivide_param_u64_u32, .-Divide__uDivide_param_u64_u32
	.align 4
	.type	Divide__Divide_param_u64_u32, #function
	.proc	05
Divide__Divide_param_u64_u32:
.LLFB95:
	save	%sp, -104, %sp
.LLCFI95:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o3
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o2
	ldd	[%fp-8], %o4
	cmp	%o2, 0
	bl	%icc, .LL594
	 nop
	cmp	%o2, 0
	bne	%icc, .LL598
	 nop
	cmp	%o3, 0
.LL598:
	cmp	%o4, 0
	bl	%icc, .LL593
	 nop
	cmp	%o4, 0
	bne	%icc, .LL599
	 nop
	cmp	%o5, 0
.LL599:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL597
	 nop
.LL593:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL596
	 nop
.LL594:
	cmp	%o4, 0
	bg	%icc, .LL595
	 nop
	cmp	%o4, 0
	bne	%icc, .LL600
	 nop
	cmp	%o5, 0
	bne	%icc, .LL595
	 nop
	cmp	%o5, 0
.LL600:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL597
	 nop
.LL595:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL596:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL597:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE95:
	.size	Divide__Divide_param_u64_u32, .-Divide__Divide_param_u64_u32
	.align 4
	.type	Divide__uDivide_var_LC_u16, #function
	.proc	05
Divide__uDivide_var_LC_u16:
.LLFB96:
	save	%sp, -96, %sp
.LLCFI96:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o2
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE96:
	.size	Divide__uDivide_var_LC_u16, .-Divide__uDivide_var_LC_u16
	.align 4
	.type	Divide__Divide_var_LC_u16, #function
	.proc	05
Divide__Divide_var_LC_u16:
.LLFB97:
	save	%sp, -96, %sp
.LLCFI97:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	cmp	%o2, 0
	bl	%icc, .LL604
	 nop
	cmp	%o2, 0
	bne	%icc, .LL608
	 nop
	cmp	%o3, 0
.LL608:
	cmp	%o4, 0
	bl	%icc, .LL603
	 nop
	cmp	%o4, 0
	bne	%icc, .LL609
	 nop
	cmp	%o5, 0
.LL609:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL607
	 nop
.LL603:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL606
	 nop
.LL604:
	cmp	%o4, 0
	bg	%icc, .LL605
	 nop
	cmp	%o4, 0
	bne	%icc, .LL610
	 nop
	cmp	%o5, 0
	bne	%icc, .LL605
	 nop
	cmp	%o5, 0
.LL610:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL607
	 nop
.LL605:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL606:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL607:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE97:
	.size	Divide__Divide_var_LC_u16, .-Divide__Divide_var_LC_u16
	.align 4
	.type	Divide__uDivide_param_LC_u16, #function
	.proc	05
Divide__uDivide_param_LC_u16:
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
	ldd	[%fp-8], %o0
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE98:
	.size	Divide__uDivide_param_LC_u16, .-Divide__uDivide_param_LC_u16
	.align 4
	.type	Divide__Divide_param_LC_u16, #function
	.proc	05
Divide__Divide_param_LC_u16:
.LLFB99:
	save	%sp, -104, %sp
.LLCFI99:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	ldd	[%fp-8], %o4
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	cmp	%o2, 0
	bl	%icc, .LL614
	 nop
	cmp	%o2, 0
	bne	%icc, .LL618
	 nop
	cmp	%o3, 0
.LL618:
	cmp	%o4, 0
	bl	%icc, .LL613
	 nop
	cmp	%o4, 0
	bne	%icc, .LL619
	 nop
	cmp	%o5, 0
.LL619:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL617
	 nop
.LL613:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL616
	 nop
.LL614:
	cmp	%o4, 0
	bg	%icc, .LL615
	 nop
	cmp	%o4, 0
	bne	%icc, .LL620
	 nop
	cmp	%o5, 0
	bne	%icc, .LL615
	 nop
	cmp	%o5, 0
.LL620:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL617
	 nop
.LL615:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL616:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL617:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE99:
	.size	Divide__Divide_param_LC_u16, .-Divide__Divide_param_LC_u16
	.align 4
	.type	Divide__uDivide_var_LC_u64, #function
	.proc	05
Divide__uDivide_var_LC_u64:
.LLFB100:
	save	%sp, -96, %sp
.LLCFI100:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o2
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE100:
	.size	Divide__uDivide_var_LC_u64, .-Divide__uDivide_var_LC_u64
	.align 4
	.type	Divide__Divide_var_LC_u64, #function
	.proc	05
Divide__Divide_var_LC_u64:
.LLFB101:
	save	%sp, -96, %sp
.LLCFI101:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o2
	cmp	%o2, 0
	bl	%icc, .LL624
	 nop
	cmp	%o2, 0
	bne	%icc, .LL628
	 nop
	cmp	%o3, 0
.LL628:
	cmp	%o4, 0
	bl	%icc, .LL623
	 nop
	cmp	%o4, 0
	bne	%icc, .LL629
	 nop
	cmp	%o5, 0
.LL629:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL627
	 nop
.LL623:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL626
	 nop
.LL624:
	cmp	%o4, 0
	bg	%icc, .LL625
	 nop
	cmp	%o4, 0
	bne	%icc, .LL630
	 nop
	cmp	%o5, 0
	bne	%icc, .LL625
	 nop
	cmp	%o5, 0
.LL630:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL627
	 nop
.LL625:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL626:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL627:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE101:
	.size	Divide__Divide_var_LC_u64, .-Divide__Divide_var_LC_u64
	.align 4
	.type	Divide__uDivide_param_LC_u64, #function
	.proc	05
Divide__uDivide_param_LC_u64:
.LLFB102:
	save	%sp, -112, %sp
.LLCFI102:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o4
	ldd	[%fp-8], %o0
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE102:
	.size	Divide__uDivide_param_LC_u64, .-Divide__uDivide_param_LC_u64
	.align 4
	.type	Divide__Divide_param_LC_u64, #function
	.proc	05
Divide__Divide_param_LC_u64:
.LLFB103:
	save	%sp, -112, %sp
.LLCFI103:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bl	%icc, .LL634
	 nop
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bne	%icc, .LL638
	 nop
	ld	[%fp-12], %g1
	cmp	%g1, 0
.LL638:
	cmp	%o4, 0
	bl	%icc, .LL633
	 nop
	cmp	%o4, 0
	bne	%icc, .LL639
	 nop
	cmp	%o5, 0
.LL639:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL637
	 nop
.LL633:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL636
	 nop
.LL634:
	cmp	%o4, 0
	bg	%icc, .LL635
	 nop
	cmp	%o4, 0
	bne	%icc, .LL640
	 nop
	cmp	%o5, 0
	bne	%icc, .LL635
	 nop
	cmp	%o5, 0
.LL640:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL637
	 nop
.LL635:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL636:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL637:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE103:
	.size	Divide__Divide_param_LC_u64, .-Divide__Divide_param_LC_u64
	.align 4
	.type	Divide__uDivide_var_LC_LC, #function
	.proc	05
Divide__uDivide_var_LC_LC:
.LLFB104:
	save	%sp, -96, %sp
.LLCFI104:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o2
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE104:
	.size	Divide__uDivide_var_LC_LC, .-Divide__uDivide_var_LC_LC
	.align 4
	.type	Divide__Divide_var_LC_LC, #function
	.proc	05
Divide__Divide_var_LC_LC:
.LLFB105:
	save	%sp, -96, %sp
.LLCFI105:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o2
	cmp	%o2, 0
	bl	%icc, .LL644
	 nop
	cmp	%o2, 0
	bne	%icc, .LL648
	 nop
	cmp	%o3, 0
.LL648:
	cmp	%o4, 0
	bl	%icc, .LL643
	 nop
	cmp	%o4, 0
	bne	%icc, .LL649
	 nop
	cmp	%o5, 0
.LL649:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL647
	 nop
.LL643:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL646
	 nop
.LL644:
	cmp	%o4, 0
	bg	%icc, .LL645
	 nop
	cmp	%o4, 0
	bne	%icc, .LL650
	 nop
	cmp	%o5, 0
	bne	%icc, .LL645
	 nop
	cmp	%o5, 0
.LL650:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL647
	 nop
.LL645:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL646:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL647:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE105:
	.size	Divide__Divide_var_LC_LC, .-Divide__Divide_var_LC_LC
	.align 4
	.type	Divide__uDivide_param_LC_LC, #function
	.proc	05
Divide__uDivide_param_LC_LC:
.LLFB106:
	save	%sp, -112, %sp
.LLCFI106:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o0
	ldd	[%fp-16], %o2
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE106:
	.size	Divide__uDivide_param_LC_LC, .-Divide__uDivide_param_LC_LC
	.align 4
	.type	Divide__Divide_param_LC_LC, #function
	.proc	05
Divide__Divide_param_LC_LC:
.LLFB107:
	save	%sp, -112, %sp
.LLCFI107:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	ldd	[%fp-16], %o2
	cmp	%o2, 0
	bl	%icc, .LL654
	 nop
	cmp	%o2, 0
	bne	%icc, .LL658
	 nop
	cmp	%o3, 0
.LL658:
	cmp	%o4, 0
	bl	%icc, .LL653
	 nop
	cmp	%o4, 0
	bne	%icc, .LL659
	 nop
	cmp	%o5, 0
.LL659:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL657
	 nop
.LL653:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL656
	 nop
.LL654:
	cmp	%o4, 0
	bg	%icc, .LL655
	 nop
	cmp	%o4, 0
	bne	%icc, .LL660
	 nop
	cmp	%o5, 0
	bne	%icc, .LL655
	 nop
	cmp	%o5, 0
.LL660:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL657
	 nop
.LL655:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL656:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL657:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE107:
	.size	Divide__Divide_param_LC_LC, .-Divide__Divide_param_LC_LC
	.align 4
	.type	Divide__uDivide_var_LC_i32, #function
	.proc	05
Divide__uDivide_var_LC_i32:
.LLFB108:
	save	%sp, -96, %sp
.LLCFI108:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o2
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE108:
	.size	Divide__uDivide_var_LC_i32, .-Divide__uDivide_var_LC_i32
	.align 4
	.type	Divide__Divide_var_LC_i32, #function
	.proc	05
Divide__Divide_var_LC_i32:
.LLFB109:
	save	%sp, -96, %sp
.LLCFI109:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o3
	sra	%g1, 31, %g1
	mov	%g1, %o2
	cmp	%o2, 0
	bl	%icc, .LL664
	 nop
	cmp	%o2, 0
	bne	%icc, .LL668
	 nop
	cmp	%o3, 0
.LL668:
	cmp	%o4, 0
	bl	%icc, .LL663
	 nop
	cmp	%o4, 0
	bne	%icc, .LL669
	 nop
	cmp	%o5, 0
.LL669:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL667
	 nop
.LL663:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL666
	 nop
.LL664:
	cmp	%o4, 0
	bg	%icc, .LL665
	 nop
	cmp	%o4, 0
	bne	%icc, .LL670
	 nop
	cmp	%o5, 0
	bne	%icc, .LL665
	 nop
	cmp	%o5, 0
.LL670:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL667
	 nop
.LL665:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL666:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL667:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE109:
	.size	Divide__Divide_var_LC_i32, .-Divide__Divide_var_LC_i32
	.align 4
	.type	Divide__uDivide_param_LC_i32, #function
	.proc	05
Divide__uDivide_param_LC_i32:
.LLFB110:
	save	%sp, -104, %sp
.LLCFI110:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o0
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE110:
	.size	Divide__uDivide_param_LC_i32, .-Divide__uDivide_param_LC_i32
	.align 4
	.type	Divide__Divide_param_LC_i32, #function
	.proc	05
Divide__Divide_param_LC_i32:
.LLFB111:
	save	%sp, -104, %sp
.LLCFI111:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ldd	[%fp-8], %o4
	ld	[%fp+76], %o3
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o2
	cmp	%o2, 0
	bl	%icc, .LL674
	 nop
	cmp	%o2, 0
	bne	%icc, .LL678
	 nop
	cmp	%o3, 0
.LL678:
	cmp	%o4, 0
	bl	%icc, .LL673
	 nop
	cmp	%o4, 0
	bne	%icc, .LL679
	 nop
	cmp	%o5, 0
.LL679:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL677
	 nop
.LL673:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL676
	 nop
.LL674:
	cmp	%o4, 0
	bg	%icc, .LL675
	 nop
	cmp	%o4, 0
	bne	%icc, .LL680
	 nop
	cmp	%o5, 0
	bne	%icc, .LL675
	 nop
	cmp	%o5, 0
.LL680:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL677
	 nop
.LL675:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL676:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL677:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE111:
	.size	Divide__Divide_param_LC_i32, .-Divide__Divide_param_LC_i32
	.align 4
	.type	Divide__uDivide_var_LC_i16, #function
	.proc	05
Divide__uDivide_var_LC_i16:
.LLFB112:
	save	%sp, -96, %sp
.LLCFI112:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o2
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE112:
	.size	Divide__uDivide_var_LC_i16, .-Divide__uDivide_var_LC_i16
	.align 4
	.type	Divide__Divide_var_LC_i16, #function
	.proc	05
Divide__Divide_var_LC_i16:
.LLFB113:
	save	%sp, -96, %sp
.LLCFI113:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o3
	sra	%g1, 31, %g1
	mov	%g1, %o2
	cmp	%o2, 0
	bl	%icc, .LL684
	 nop
	cmp	%o2, 0
	bne	%icc, .LL688
	 nop
	cmp	%o3, 0
.LL688:
	cmp	%o4, 0
	bl	%icc, .LL683
	 nop
	cmp	%o4, 0
	bne	%icc, .LL689
	 nop
	cmp	%o5, 0
.LL689:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL687
	 nop
.LL683:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL686
	 nop
.LL684:
	cmp	%o4, 0
	bg	%icc, .LL685
	 nop
	cmp	%o4, 0
	bne	%icc, .LL690
	 nop
	cmp	%o5, 0
	bne	%icc, .LL685
	 nop
	cmp	%o5, 0
.LL690:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL687
	 nop
.LL685:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL686:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL687:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE113:
	.size	Divide__Divide_var_LC_i16, .-Divide__Divide_var_LC_i16
	.align 4
	.type	Divide__uDivide_param_LC_i16, #function
	.proc	05
Divide__uDivide_param_LC_i16:
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
	ldd	[%fp-8], %o0
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE114:
	.size	Divide__uDivide_param_LC_i16, .-Divide__uDivide_param_LC_i16
	.align 4
	.type	Divide__Divide_param_LC_i16, #function
	.proc	05
Divide__Divide_param_LC_i16:
.LLFB115:
	save	%sp, -104, %sp
.LLCFI115:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	ldd	[%fp-8], %o4
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o3
	sra	%g1, 31, %g1
	mov	%g1, %o2
	cmp	%o2, 0
	bl	%icc, .LL694
	 nop
	cmp	%o2, 0
	bne	%icc, .LL698
	 nop
	cmp	%o3, 0
.LL698:
	cmp	%o4, 0
	bl	%icc, .LL693
	 nop
	cmp	%o4, 0
	bne	%icc, .LL699
	 nop
	cmp	%o5, 0
.LL699:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL697
	 nop
.LL693:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL696
	 nop
.LL694:
	cmp	%o4, 0
	bg	%icc, .LL695
	 nop
	cmp	%o4, 0
	bne	%icc, .LL700
	 nop
	cmp	%o5, 0
	bne	%icc, .LL695
	 nop
	cmp	%o5, 0
.LL700:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL697
	 nop
.LL695:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL696:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL697:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE115:
	.size	Divide__Divide_param_LC_i16, .-Divide__Divide_param_LC_i16
	.align 4
	.type	Divide__uDivide_var_LC_I, #function
	.proc	05
Divide__uDivide_var_LC_I:
.LLFB116:
	save	%sp, -96, %sp
.LLCFI116:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o2
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE116:
	.size	Divide__uDivide_var_LC_I, .-Divide__uDivide_var_LC_I
	.align 4
	.type	Divide__Divide_var_LC_I, #function
	.proc	05
Divide__Divide_var_LC_I:
.LLFB117:
	save	%sp, -96, %sp
.LLCFI117:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o3
	sra	%g1, 31, %g1
	mov	%g1, %o2
	cmp	%o2, 0
	bl	%icc, .LL704
	 nop
	cmp	%o2, 0
	bne	%icc, .LL708
	 nop
	cmp	%o3, 0
.LL708:
	cmp	%o4, 0
	bl	%icc, .LL703
	 nop
	cmp	%o4, 0
	bne	%icc, .LL709
	 nop
	cmp	%o5, 0
.LL709:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL707
	 nop
.LL703:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL706
	 nop
.LL704:
	cmp	%o4, 0
	bg	%icc, .LL705
	 nop
	cmp	%o4, 0
	bne	%icc, .LL710
	 nop
	cmp	%o5, 0
	bne	%icc, .LL705
	 nop
	cmp	%o5, 0
.LL710:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL707
	 nop
.LL705:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL706:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL707:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE117:
	.size	Divide__Divide_var_LC_I, .-Divide__Divide_var_LC_I
	.align 4
	.type	Divide__uDivide_param_LC_I, #function
	.proc	05
Divide__uDivide_param_LC_I:
.LLFB118:
	save	%sp, -104, %sp
.LLCFI118:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o0
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE118:
	.size	Divide__uDivide_param_LC_I, .-Divide__uDivide_param_LC_I
	.align 4
	.type	Divide__Divide_param_LC_I, #function
	.proc	05
Divide__Divide_param_LC_I:
.LLFB119:
	save	%sp, -104, %sp
.LLCFI119:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ldd	[%fp-8], %o4
	ld	[%fp+76], %o3
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o2
	cmp	%o2, 0
	bl	%icc, .LL714
	 nop
	cmp	%o2, 0
	bne	%icc, .LL718
	 nop
	cmp	%o3, 0
.LL718:
	cmp	%o4, 0
	bl	%icc, .LL713
	 nop
	cmp	%o4, 0
	bne	%icc, .LL719
	 nop
	cmp	%o5, 0
.LL719:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL717
	 nop
.LL713:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL716
	 nop
.LL714:
	cmp	%o4, 0
	bg	%icc, .LL715
	 nop
	cmp	%o4, 0
	bne	%icc, .LL720
	 nop
	cmp	%o5, 0
	bne	%icc, .LL715
	 nop
	cmp	%o5, 0
.LL720:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL717
	 nop
.LL715:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL716:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL717:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE119:
	.size	Divide__Divide_param_LC_I, .-Divide__Divide_param_LC_I
	.align 4
	.type	Divide__uDivide_var_LC_i64, #function
	.proc	05
Divide__uDivide_var_LC_i64:
.LLFB120:
	save	%sp, -96, %sp
.LLCFI120:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o2
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE120:
	.size	Divide__uDivide_var_LC_i64, .-Divide__uDivide_var_LC_i64
	.align 4
	.type	Divide__Divide_var_LC_i64, #function
	.proc	05
Divide__Divide_var_LC_i64:
.LLFB121:
	save	%sp, -96, %sp
.LLCFI121:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o2
	cmp	%o2, 0
	bl	%icc, .LL724
	 nop
	cmp	%o2, 0
	bne	%icc, .LL728
	 nop
	cmp	%o3, 0
.LL728:
	cmp	%o4, 0
	bl	%icc, .LL723
	 nop
	cmp	%o4, 0
	bne	%icc, .LL729
	 nop
	cmp	%o5, 0
.LL729:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL727
	 nop
.LL723:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL726
	 nop
.LL724:
	cmp	%o4, 0
	bg	%icc, .LL725
	 nop
	cmp	%o4, 0
	bne	%icc, .LL730
	 nop
	cmp	%o5, 0
	bne	%icc, .LL725
	 nop
	cmp	%o5, 0
.LL730:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL727
	 nop
.LL725:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL726:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL727:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE121:
	.size	Divide__Divide_var_LC_i64, .-Divide__Divide_var_LC_i64
	.align 4
	.type	Divide__uDivide_param_LC_i64, #function
	.proc	05
Divide__uDivide_param_LC_i64:
.LLFB122:
	save	%sp, -112, %sp
.LLCFI122:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o4
	ldd	[%fp-8], %o0
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE122:
	.size	Divide__uDivide_param_LC_i64, .-Divide__uDivide_param_LC_i64
	.align 4
	.type	Divide__Divide_param_LC_i64, #function
	.proc	05
Divide__Divide_param_LC_i64:
.LLFB123:
	save	%sp, -112, %sp
.LLCFI123:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bl	%icc, .LL734
	 nop
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bne	%icc, .LL738
	 nop
	ld	[%fp-12], %g1
	cmp	%g1, 0
.LL738:
	cmp	%o4, 0
	bl	%icc, .LL733
	 nop
	cmp	%o4, 0
	bne	%icc, .LL739
	 nop
	cmp	%o5, 0
.LL739:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL737
	 nop
.LL733:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL736
	 nop
.LL734:
	cmp	%o4, 0
	bg	%icc, .LL735
	 nop
	cmp	%o4, 0
	bne	%icc, .LL740
	 nop
	cmp	%o5, 0
	bne	%icc, .LL735
	 nop
	cmp	%o5, 0
.LL740:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL737
	 nop
.LL735:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL736:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL737:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE123:
	.size	Divide__Divide_param_LC_i64, .-Divide__Divide_param_LC_i64
	.align 4
	.type	Divide__uDivide_var_LC_C, #function
	.proc	05
Divide__uDivide_var_LC_C:
.LLFB124:
	save	%sp, -96, %sp
.LLCFI124:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o2
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE124:
	.size	Divide__uDivide_var_LC_C, .-Divide__uDivide_var_LC_C
	.align 4
	.type	Divide__Divide_var_LC_C, #function
	.proc	05
Divide__Divide_var_LC_C:
.LLFB125:
	save	%sp, -96, %sp
.LLCFI125:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o3
	sra	%g1, 31, %g1
	mov	%g1, %o2
	cmp	%o2, 0
	bl	%icc, .LL744
	 nop
	cmp	%o2, 0
	bne	%icc, .LL748
	 nop
	cmp	%o3, 0
.LL748:
	cmp	%o4, 0
	bl	%icc, .LL743
	 nop
	cmp	%o4, 0
	bne	%icc, .LL749
	 nop
	cmp	%o5, 0
.LL749:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL747
	 nop
.LL743:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL746
	 nop
.LL744:
	cmp	%o4, 0
	bg	%icc, .LL745
	 nop
	cmp	%o4, 0
	bne	%icc, .LL750
	 nop
	cmp	%o5, 0
	bne	%icc, .LL745
	 nop
	cmp	%o5, 0
.LL750:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL747
	 nop
.LL745:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL746:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL747:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE125:
	.size	Divide__Divide_var_LC_C, .-Divide__Divide_var_LC_C
	.align 4
	.type	Divide__uDivide_param_LC_C, #function
	.proc	05
Divide__uDivide_param_LC_C:
.LLFB126:
	save	%sp, -104, %sp
.LLCFI126:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o0
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE126:
	.size	Divide__uDivide_param_LC_C, .-Divide__uDivide_param_LC_C
	.align 4
	.type	Divide__Divide_param_LC_C, #function
	.proc	05
Divide__Divide_param_LC_C:
.LLFB127:
	save	%sp, -104, %sp
.LLCFI127:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ldd	[%fp-8], %o4
	ld	[%fp+76], %g1
	mov	%g1, %o3
	sra	%g1, 31, %g1
	mov	%g1, %o2
	cmp	%o2, 0
	bl	%icc, .LL754
	 nop
	cmp	%o2, 0
	bne	%icc, .LL758
	 nop
	cmp	%o3, 0
.LL758:
	cmp	%o4, 0
	bl	%icc, .LL753
	 nop
	cmp	%o4, 0
	bne	%icc, .LL759
	 nop
	cmp	%o5, 0
.LL759:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL757
	 nop
.LL753:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL756
	 nop
.LL754:
	cmp	%o4, 0
	bg	%icc, .LL755
	 nop
	cmp	%o4, 0
	bne	%icc, .LL760
	 nop
	cmp	%o5, 0
	bne	%icc, .LL755
	 nop
	cmp	%o5, 0
.LL760:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL757
	 nop
.LL755:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL756:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL757:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE127:
	.size	Divide__Divide_param_LC_C, .-Divide__Divide_param_LC_C
	.align 4
	.type	Divide__uDivide_var_LC_u8, #function
	.proc	05
Divide__uDivide_var_LC_u8:
.LLFB128:
	save	%sp, -96, %sp
.LLCFI128:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o2
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE128:
	.size	Divide__uDivide_var_LC_u8, .-Divide__uDivide_var_LC_u8
	.align 4
	.type	Divide__Divide_var_LC_u8, #function
	.proc	05
Divide__Divide_var_LC_u8:
.LLFB129:
	save	%sp, -96, %sp
.LLCFI129:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	cmp	%o2, 0
	bl	%icc, .LL764
	 nop
	cmp	%o2, 0
	bne	%icc, .LL768
	 nop
	cmp	%o3, 0
.LL768:
	cmp	%o4, 0
	bl	%icc, .LL763
	 nop
	cmp	%o4, 0
	bne	%icc, .LL769
	 nop
	cmp	%o5, 0
.LL769:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL767
	 nop
.LL763:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL766
	 nop
.LL764:
	cmp	%o4, 0
	bg	%icc, .LL765
	 nop
	cmp	%o4, 0
	bne	%icc, .LL770
	 nop
	cmp	%o5, 0
	bne	%icc, .LL765
	 nop
	cmp	%o5, 0
.LL770:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL767
	 nop
.LL765:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL766:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL767:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE129:
	.size	Divide__Divide_var_LC_u8, .-Divide__Divide_var_LC_u8
	.align 4
	.type	Divide__uDivide_param_LC_u8, #function
	.proc	05
Divide__uDivide_param_LC_u8:
.LLFB130:
	save	%sp, -104, %sp
.LLCFI130:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	ldd	[%fp-8], %o0
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE130:
	.size	Divide__uDivide_param_LC_u8, .-Divide__uDivide_param_LC_u8
	.align 4
	.type	Divide__Divide_param_LC_u8, #function
	.proc	05
Divide__Divide_param_LC_u8:
.LLFB131:
	save	%sp, -104, %sp
.LLCFI131:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldd	[%fp-8], %o4
	ldub	[%fp+76], %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	cmp	%o2, 0
	bl	%icc, .LL774
	 nop
	cmp	%o2, 0
	bne	%icc, .LL778
	 nop
	cmp	%o3, 0
.LL778:
	cmp	%o4, 0
	bl	%icc, .LL773
	 nop
	cmp	%o4, 0
	bne	%icc, .LL779
	 nop
	cmp	%o5, 0
.LL779:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL777
	 nop
.LL773:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL776
	 nop
.LL774:
	cmp	%o4, 0
	bg	%icc, .LL775
	 nop
	cmp	%o4, 0
	bne	%icc, .LL780
	 nop
	cmp	%o5, 0
	bne	%icc, .LL775
	 nop
	cmp	%o5, 0
.LL780:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL777
	 nop
.LL775:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL776:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL777:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE131:
	.size	Divide__Divide_param_LC_u8, .-Divide__Divide_param_LC_u8
	.align 4
	.type	Divide__uDivide_var_LC_L, #function
	.proc	05
Divide__uDivide_var_LC_L:
.LLFB132:
	save	%sp, -96, %sp
.LLCFI132:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o2
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE132:
	.size	Divide__uDivide_var_LC_L, .-Divide__uDivide_var_LC_L
	.align 4
	.type	Divide__Divide_var_LC_L, #function
	.proc	05
Divide__Divide_var_LC_L:
.LLFB133:
	save	%sp, -96, %sp
.LLCFI133:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o2
	cmp	%o2, 0
	bl	%icc, .LL784
	 nop
	cmp	%o2, 0
	bne	%icc, .LL788
	 nop
	cmp	%o3, 0
.LL788:
	cmp	%o4, 0
	bl	%icc, .LL783
	 nop
	cmp	%o4, 0
	bne	%icc, .LL789
	 nop
	cmp	%o5, 0
.LL789:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL787
	 nop
.LL783:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL786
	 nop
.LL784:
	cmp	%o4, 0
	bg	%icc, .LL785
	 nop
	cmp	%o4, 0
	bne	%icc, .LL790
	 nop
	cmp	%o5, 0
	bne	%icc, .LL785
	 nop
	cmp	%o5, 0
.LL790:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL787
	 nop
.LL785:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL786:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL787:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE133:
	.size	Divide__Divide_var_LC_L, .-Divide__Divide_var_LC_L
	.align 4
	.type	Divide__uDivide_param_LC_L, #function
	.proc	05
Divide__uDivide_param_LC_L:
.LLFB134:
	save	%sp, -112, %sp
.LLCFI134:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o4
	ldd	[%fp-8], %o0
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE134:
	.size	Divide__uDivide_param_LC_L, .-Divide__uDivide_param_LC_L
	.align 4
	.type	Divide__Divide_param_LC_L, #function
	.proc	05
Divide__Divide_param_LC_L:
.LLFB135:
	save	%sp, -112, %sp
.LLCFI135:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bl	%icc, .LL794
	 nop
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bne	%icc, .LL798
	 nop
	ld	[%fp-12], %g1
	cmp	%g1, 0
.LL798:
	cmp	%o4, 0
	bl	%icc, .LL793
	 nop
	cmp	%o4, 0
	bne	%icc, .LL799
	 nop
	cmp	%o5, 0
.LL799:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL797
	 nop
.LL793:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL796
	 nop
.LL794:
	cmp	%o4, 0
	bg	%icc, .LL795
	 nop
	cmp	%o4, 0
	bne	%icc, .LL800
	 nop
	cmp	%o5, 0
	bne	%icc, .LL795
	 nop
	cmp	%o5, 0
.LL800:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL797
	 nop
.LL795:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL796:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL797:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE135:
	.size	Divide__Divide_param_LC_L, .-Divide__Divide_param_LC_L
	.align 4
	.type	Divide__uDivide_var_LC_i8, #function
	.proc	05
Divide__uDivide_var_LC_i8:
.LLFB136:
	save	%sp, -96, %sp
.LLCFI136:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o2
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE136:
	.size	Divide__uDivide_var_LC_i8, .-Divide__uDivide_var_LC_i8
	.align 4
	.type	Divide__Divide_var_LC_i8, #function
	.proc	05
Divide__Divide_var_LC_i8:
.LLFB137:
	save	%sp, -96, %sp
.LLCFI137:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o3
	sra	%g1, 31, %g1
	mov	%g1, %o2
	cmp	%o2, 0
	bl	%icc, .LL804
	 nop
	cmp	%o2, 0
	bne	%icc, .LL808
	 nop
	cmp	%o3, 0
.LL808:
	cmp	%o4, 0
	bl	%icc, .LL803
	 nop
	cmp	%o4, 0
	bne	%icc, .LL809
	 nop
	cmp	%o5, 0
.LL809:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL807
	 nop
.LL803:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL806
	 nop
.LL804:
	cmp	%o4, 0
	bg	%icc, .LL805
	 nop
	cmp	%o4, 0
	bne	%icc, .LL810
	 nop
	cmp	%o5, 0
	bne	%icc, .LL805
	 nop
	cmp	%o5, 0
.LL810:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL807
	 nop
.LL805:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL806:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL807:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE137:
	.size	Divide__Divide_var_LC_i8, .-Divide__Divide_var_LC_i8
	.align 4
	.type	Divide__uDivide_param_LC_i8, #function
	.proc	05
Divide__uDivide_param_LC_i8:
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
	ldd	[%fp-8], %o0
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE138:
	.size	Divide__uDivide_param_LC_i8, .-Divide__uDivide_param_LC_i8
	.align 4
	.type	Divide__Divide_param_LC_i8, #function
	.proc	05
Divide__Divide_param_LC_i8:
.LLFB139:
	save	%sp, -104, %sp
.LLCFI139:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldd	[%fp-8], %o4
	ldub	[%fp+76], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o3
	sra	%g1, 31, %g1
	mov	%g1, %o2
	cmp	%o2, 0
	bl	%icc, .LL814
	 nop
	cmp	%o2, 0
	bne	%icc, .LL818
	 nop
	cmp	%o3, 0
.LL818:
	cmp	%o4, 0
	bl	%icc, .LL813
	 nop
	cmp	%o4, 0
	bne	%icc, .LL819
	 nop
	cmp	%o5, 0
.LL819:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL817
	 nop
.LL813:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL816
	 nop
.LL814:
	cmp	%o4, 0
	bg	%icc, .LL815
	 nop
	cmp	%o4, 0
	bne	%icc, .LL820
	 nop
	cmp	%o5, 0
	bne	%icc, .LL815
	 nop
	cmp	%o5, 0
.LL820:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL817
	 nop
.LL815:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL816:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL817:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE139:
	.size	Divide__Divide_param_LC_i8, .-Divide__Divide_param_LC_i8
	.align 4
	.type	Divide__uDivide_var_LC_u32, #function
	.proc	05
Divide__uDivide_var_LC_u32:
.LLFB140:
	save	%sp, -96, %sp
.LLCFI140:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o2
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE140:
	.size	Divide__uDivide_var_LC_u32, .-Divide__uDivide_var_LC_u32
	.align 4
	.type	Divide__Divide_var_LC_u32, #function
	.proc	05
Divide__Divide_var_LC_u32:
.LLFB141:
	save	%sp, -96, %sp
.LLCFI141:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o3
	sra	%g1, 31, %g1
	mov	%g1, %o2
	cmp	%o2, 0
	bl	%icc, .LL824
	 nop
	cmp	%o2, 0
	bne	%icc, .LL828
	 nop
	cmp	%o3, 0
.LL828:
	cmp	%o4, 0
	bl	%icc, .LL823
	 nop
	cmp	%o4, 0
	bne	%icc, .LL829
	 nop
	cmp	%o5, 0
.LL829:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL827
	 nop
.LL823:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL826
	 nop
.LL824:
	cmp	%o4, 0
	bg	%icc, .LL825
	 nop
	cmp	%o4, 0
	bne	%icc, .LL830
	 nop
	cmp	%o5, 0
	bne	%icc, .LL825
	 nop
	cmp	%o5, 0
.LL830:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL827
	 nop
.LL825:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL826:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL827:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE141:
	.size	Divide__Divide_var_LC_u32, .-Divide__Divide_var_LC_u32
	.align 4
	.type	Divide__uDivide_param_LC_u32, #function
	.proc	05
Divide__uDivide_param_LC_u32:
.LLFB142:
	save	%sp, -104, %sp
.LLCFI142:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o0
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE142:
	.size	Divide__uDivide_param_LC_u32, .-Divide__uDivide_param_LC_u32
	.align 4
	.type	Divide__Divide_param_LC_u32, #function
	.proc	05
Divide__Divide_param_LC_u32:
.LLFB143:
	save	%sp, -104, %sp
.LLCFI143:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ldd	[%fp-8], %o4
	ld	[%fp+76], %o3
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o2
	cmp	%o2, 0
	bl	%icc, .LL834
	 nop
	cmp	%o2, 0
	bne	%icc, .LL838
	 nop
	cmp	%o3, 0
.LL838:
	cmp	%o4, 0
	bl	%icc, .LL833
	 nop
	cmp	%o4, 0
	bne	%icc, .LL839
	 nop
	cmp	%o5, 0
.LL839:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL837
	 nop
.LL833:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL836
	 nop
.LL834:
	cmp	%o4, 0
	bg	%icc, .LL835
	 nop
	cmp	%o4, 0
	bne	%icc, .LL840
	 nop
	cmp	%o5, 0
	bne	%icc, .LL835
	 nop
	cmp	%o5, 0
.LL840:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL837
	 nop
.LL835:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL836:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL837:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE143:
	.size	Divide__Divide_param_LC_u32, .-Divide__Divide_param_LC_u32
	.align 4
	.type	Divide__uDivide_var_i32_u16, #function
	.proc	04
Divide__uDivide_var_i32_u16:
.LLFB144:
	save	%sp, -96, %sp
.LLCFI144:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE144:
	.size	Divide__uDivide_var_i32_u16, .-Divide__uDivide_var_i32_u16
	.align 4
	.type	Divide__Divide_var_i32_u16, #function
	.proc	04
Divide__Divide_var_i32_u16:
.LLFB145:
	save	%sp, -96, %sp
.LLCFI145:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+72], %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+52], %o5
	srl	%o5, 16, %o5
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL844
	 nop
	cmp	%g1, 0
	bl	%icc, .LL843
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL847
	 nop
.LL843:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL846
	 nop
.LL844:
	cmp	%g1, 0
	bg	%icc, .LL845
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL847
	 nop
.LL845:
	add	%g1, -1, %g1
.LL846:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL847:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE145:
	.size	Divide__Divide_var_i32_u16, .-Divide__Divide_var_i32_u16
	.align 4
	.type	Divide__uDivide_param_i32_u16, #function
	.proc	04
Divide__uDivide_param_i32_u16:
.LLFB146:
	save	%sp, -96, %sp
.LLCFI146:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ld	[%fp+68], %o4
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE146:
	.size	Divide__uDivide_param_i32_u16, .-Divide__uDivide_param_i32_u16
	.align 4
	.type	Divide__Divide_param_i32_u16, #function
	.proc	04
Divide__Divide_param_i32_u16:
.LLFB147:
	save	%sp, -96, %sp
.LLCFI147:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+68], %g1
	cmp	%o5, 0
	bl	%icc, .LL851
	 nop
	cmp	%g1, 0
	bl	%icc, .LL850
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL854
	 nop
.LL850:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL853
	 nop
.LL851:
	cmp	%g1, 0
	bg	%icc, .LL852
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL854
	 nop
.LL852:
	add	%g1, -1, %g1
.LL853:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL854:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE147:
	.size	Divide__Divide_param_i32_u16, .-Divide__Divide_param_i32_u16
	.align 4
	.type	Divide__uDivide_var_i32_u64, #function
	.proc	05
Divide__uDivide_var_i32_u64:
.LLFB148:
	save	%sp, -96, %sp
.LLCFI148:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE148:
	.size	Divide__uDivide_var_i32_u64, .-Divide__uDivide_var_i32_u64
	.align 4
	.type	Divide__Divide_var_i32_u64, #function
	.proc	05
Divide__Divide_var_i32_u64:
.LLFB149:
	save	%sp, -96, %sp
.LLCFI149:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o2
	cmp	%o2, 0
	bl	%icc, .LL858
	 nop
	cmp	%o2, 0
	bne	%icc, .LL862
	 nop
	cmp	%o3, 0
.LL862:
	cmp	%o4, 0
	bl	%icc, .LL857
	 nop
	cmp	%o4, 0
	bne	%icc, .LL863
	 nop
	cmp	%o5, 0
.LL863:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL861
	 nop
.LL857:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL860
	 nop
.LL858:
	cmp	%o4, 0
	bg	%icc, .LL859
	 nop
	cmp	%o4, 0
	bne	%icc, .LL864
	 nop
	cmp	%o5, 0
	bne	%icc, .LL859
	 nop
	cmp	%o5, 0
.LL864:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL861
	 nop
.LL859:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL860:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL861:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE149:
	.size	Divide__Divide_var_i32_u64, .-Divide__Divide_var_i32_u64
	.align 4
	.type	Divide__uDivide_param_i32_u64, #function
	.proc	05
Divide__uDivide_param_i32_u64:
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
	mov	%o4, %o0
	mov	%o5, %o1
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE150:
	.size	Divide__uDivide_param_i32_u64, .-Divide__uDivide_param_i32_u64
	.align 4
	.type	Divide__Divide_param_i32_u64, #function
	.proc	05
Divide__Divide_param_i32_u64:
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
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL868
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL872
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL872:
	cmp	%o4, 0
	bl	%icc, .LL867
	 nop
	cmp	%o4, 0
	bne	%icc, .LL873
	 nop
	cmp	%o5, 0
.LL873:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL871
	 nop
.LL867:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL870
	 nop
.LL868:
	cmp	%o4, 0
	bg	%icc, .LL869
	 nop
	cmp	%o4, 0
	bne	%icc, .LL874
	 nop
	cmp	%o5, 0
	bne	%icc, .LL869
	 nop
	cmp	%o5, 0
.LL874:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL871
	 nop
.LL869:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL870:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL871:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE151:
	.size	Divide__Divide_param_i32_u64, .-Divide__Divide_param_i32_u64
	.align 4
	.type	Divide__uDivide_var_i32_LC, #function
	.proc	05
Divide__uDivide_var_i32_LC:
.LLFB152:
	save	%sp, -96, %sp
.LLCFI152:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE152:
	.size	Divide__uDivide_var_i32_LC, .-Divide__uDivide_var_i32_LC
	.align 4
	.type	Divide__Divide_var_i32_LC, #function
	.proc	05
Divide__Divide_var_i32_LC:
.LLFB153:
	save	%sp, -96, %sp
.LLCFI153:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o2
	cmp	%o2, 0
	bl	%icc, .LL878
	 nop
	cmp	%o2, 0
	bne	%icc, .LL882
	 nop
	cmp	%o3, 0
.LL882:
	cmp	%o4, 0
	bl	%icc, .LL877
	 nop
	cmp	%o4, 0
	bne	%icc, .LL883
	 nop
	cmp	%o5, 0
.LL883:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL881
	 nop
.LL877:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL880
	 nop
.LL878:
	cmp	%o4, 0
	bg	%icc, .LL879
	 nop
	cmp	%o4, 0
	bne	%icc, .LL884
	 nop
	cmp	%o5, 0
	bne	%icc, .LL879
	 nop
	cmp	%o5, 0
.LL884:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL881
	 nop
.LL879:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL880:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL881:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE153:
	.size	Divide__Divide_var_i32_LC, .-Divide__Divide_var_i32_LC
	.align 4
	.type	Divide__uDivide_param_i32_LC, #function
	.proc	05
Divide__uDivide_param_i32_LC:
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
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE154:
	.size	Divide__uDivide_param_i32_LC, .-Divide__uDivide_param_i32_LC
	.align 4
	.type	Divide__Divide_param_i32_LC, #function
	.proc	05
Divide__Divide_param_i32_LC:
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
	cmp	%o2, 0
	bl	%icc, .LL888
	 nop
	cmp	%o2, 0
	bne	%icc, .LL892
	 nop
	cmp	%o3, 0
.LL892:
	cmp	%o4, 0
	bl	%icc, .LL887
	 nop
	cmp	%o4, 0
	bne	%icc, .LL893
	 nop
	cmp	%o5, 0
.LL893:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL891
	 nop
.LL887:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL890
	 nop
.LL888:
	cmp	%o4, 0
	bg	%icc, .LL889
	 nop
	cmp	%o4, 0
	bne	%icc, .LL894
	 nop
	cmp	%o5, 0
	bne	%icc, .LL889
	 nop
	cmp	%o5, 0
.LL894:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL891
	 nop
.LL889:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL890:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL891:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE155:
	.size	Divide__Divide_param_i32_LC, .-Divide__Divide_param_i32_LC
	.align 4
	.type	Divide__uDivide_var_i32_i32, #function
	.proc	04
Divide__uDivide_var_i32_i32:
.LLFB156:
	save	%sp, -96, %sp
.LLCFI156:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE156:
	.size	Divide__uDivide_var_i32_i32, .-Divide__uDivide_var_i32_i32
	.align 4
	.type	Divide__Divide_var_i32_i32, #function
	.proc	04
Divide__Divide_var_i32_i32:
.LLFB157:
	save	%sp, -96, %sp
.LLCFI157:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+72], %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL898
	 nop
	cmp	%g1, 0
	bl	%icc, .LL897
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL901
	 nop
.LL897:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL900
	 nop
.LL898:
	cmp	%g1, 0
	bg	%icc, .LL899
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL901
	 nop
.LL899:
	add	%g1, -1, %g1
.LL900:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL901:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE157:
	.size	Divide__Divide_var_i32_i32, .-Divide__Divide_var_i32_i32
	.align 4
	.type	Divide__uDivide_param_i32_i32, #function
	.proc	04
Divide__uDivide_param_i32_i32:
.LLFB158:
	save	%sp, -96, %sp
.LLCFI158:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o4
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE158:
	.size	Divide__uDivide_param_i32_i32, .-Divide__uDivide_param_i32_i32
	.align 4
	.type	Divide__Divide_param_i32_i32, #function
	.proc	04
Divide__Divide_param_i32_i32:
.LLFB159:
	save	%sp, -96, %sp
.LLCFI159:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL905
	 nop
	cmp	%g1, 0
	bl	%icc, .LL904
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL908
	 nop
.LL904:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL907
	 nop
.LL905:
	cmp	%g1, 0
	bg	%icc, .LL906
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL908
	 nop
.LL906:
	add	%g1, -1, %g1
.LL907:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL908:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE159:
	.size	Divide__Divide_param_i32_i32, .-Divide__Divide_param_i32_i32
	.align 4
	.type	Divide__uDivide_var_i32_i16, #function
	.proc	04
Divide__uDivide_var_i32_i16:
.LLFB160:
	save	%sp, -96, %sp
.LLCFI160:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE160:
	.size	Divide__uDivide_var_i32_i16, .-Divide__uDivide_var_i32_i16
	.align 4
	.type	Divide__Divide_var_i32_i16, #function
	.proc	04
Divide__Divide_var_i32_i16:
.LLFB161:
	save	%sp, -96, %sp
.LLCFI161:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+72], %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+76], %o5
	sra	%o5, 16, %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL912
	 nop
	cmp	%g1, 0
	bl	%icc, .LL911
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL915
	 nop
.LL911:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL914
	 nop
.LL912:
	cmp	%g1, 0
	bg	%icc, .LL913
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL915
	 nop
.LL913:
	add	%g1, -1, %g1
.LL914:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL915:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE161:
	.size	Divide__Divide_var_i32_i16, .-Divide__Divide_var_i32_i16
	.align 4
	.type	Divide__uDivide_param_i32_i16, #function
	.proc	04
Divide__uDivide_param_i32_i16:
.LLFB162:
	save	%sp, -96, %sp
.LLCFI162:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ld	[%fp+68], %o4
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE162:
	.size	Divide__uDivide_param_i32_i16, .-Divide__uDivide_param_i32_i16
	.align 4
	.type	Divide__Divide_param_i32_i16, #function
	.proc	04
Divide__Divide_param_i32_i16:
.LLFB163:
	save	%sp, -96, %sp
.LLCFI163:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+68], %g1
	cmp	%o5, 0
	bl	%icc, .LL919
	 nop
	cmp	%g1, 0
	bl	%icc, .LL918
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL922
	 nop
.LL918:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL921
	 nop
.LL919:
	cmp	%g1, 0
	bg	%icc, .LL920
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL922
	 nop
.LL920:
	add	%g1, -1, %g1
.LL921:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL922:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE163:
	.size	Divide__Divide_param_i32_i16, .-Divide__Divide_param_i32_i16
	.align 4
	.type	Divide__uDivide_var_i32_I, #function
	.proc	04
Divide__uDivide_var_i32_I:
.LLFB164:
	save	%sp, -96, %sp
.LLCFI164:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE164:
	.size	Divide__uDivide_var_i32_I, .-Divide__uDivide_var_i32_I
	.align 4
	.type	Divide__Divide_var_i32_I, #function
	.proc	04
Divide__Divide_var_i32_I:
.LLFB165:
	save	%sp, -96, %sp
.LLCFI165:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+72], %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+80], %o5
	cmp	%o5, 0
	bl	%icc, .LL926
	 nop
	cmp	%g1, 0
	bl	%icc, .LL925
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL929
	 nop
.LL925:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL928
	 nop
.LL926:
	cmp	%g1, 0
	bg	%icc, .LL927
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL929
	 nop
.LL927:
	add	%g1, -1, %g1
.LL928:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL929:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE165:
	.size	Divide__Divide_var_i32_I, .-Divide__Divide_var_i32_I
	.align 4
	.type	Divide__uDivide_param_i32_I, #function
	.proc	04
Divide__uDivide_param_i32_I:
.LLFB166:
	save	%sp, -96, %sp
.LLCFI166:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o4
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE166:
	.size	Divide__uDivide_param_i32_I, .-Divide__uDivide_param_i32_I
	.align 4
	.type	Divide__Divide_param_i32_I, #function
	.proc	04
Divide__Divide_param_i32_I:
.LLFB167:
	save	%sp, -96, %sp
.LLCFI167:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL933
	 nop
	cmp	%g1, 0
	bl	%icc, .LL932
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL936
	 nop
.LL932:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL935
	 nop
.LL933:
	cmp	%g1, 0
	bg	%icc, .LL934
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL936
	 nop
.LL934:
	add	%g1, -1, %g1
.LL935:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL936:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE167:
	.size	Divide__Divide_param_i32_I, .-Divide__Divide_param_i32_I
	.align 4
	.type	Divide__uDivide_var_i32_i64, #function
	.proc	05
Divide__uDivide_var_i32_i64:
.LLFB168:
	save	%sp, -96, %sp
.LLCFI168:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE168:
	.size	Divide__uDivide_var_i32_i64, .-Divide__uDivide_var_i32_i64
	.align 4
	.type	Divide__Divide_var_i32_i64, #function
	.proc	05
Divide__Divide_var_i32_i64:
.LLFB169:
	save	%sp, -96, %sp
.LLCFI169:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o2
	cmp	%o2, 0
	bl	%icc, .LL940
	 nop
	cmp	%o2, 0
	bne	%icc, .LL944
	 nop
	cmp	%o3, 0
.LL944:
	cmp	%o4, 0
	bl	%icc, .LL939
	 nop
	cmp	%o4, 0
	bne	%icc, .LL945
	 nop
	cmp	%o5, 0
.LL945:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL943
	 nop
.LL939:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL942
	 nop
.LL940:
	cmp	%o4, 0
	bg	%icc, .LL941
	 nop
	cmp	%o4, 0
	bne	%icc, .LL946
	 nop
	cmp	%o5, 0
	bne	%icc, .LL941
	 nop
	cmp	%o5, 0
.LL946:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL943
	 nop
.LL941:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL942:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL943:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE169:
	.size	Divide__Divide_var_i32_i64, .-Divide__Divide_var_i32_i64
	.align 4
	.type	Divide__uDivide_param_i32_i64, #function
	.proc	05
Divide__uDivide_param_i32_i64:
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
	mov	%o4, %o0
	mov	%o5, %o1
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE170:
	.size	Divide__uDivide_param_i32_i64, .-Divide__uDivide_param_i32_i64
	.align 4
	.type	Divide__Divide_param_i32_i64, #function
	.proc	05
Divide__Divide_param_i32_i64:
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
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL950
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL954
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL954:
	cmp	%o4, 0
	bl	%icc, .LL949
	 nop
	cmp	%o4, 0
	bne	%icc, .LL955
	 nop
	cmp	%o5, 0
.LL955:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL953
	 nop
.LL949:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL952
	 nop
.LL950:
	cmp	%o4, 0
	bg	%icc, .LL951
	 nop
	cmp	%o4, 0
	bne	%icc, .LL956
	 nop
	cmp	%o5, 0
	bne	%icc, .LL951
	 nop
	cmp	%o5, 0
.LL956:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL953
	 nop
.LL951:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL952:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL953:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE171:
	.size	Divide__Divide_param_i32_i64, .-Divide__Divide_param_i32_i64
	.align 4
	.type	Divide__uDivide_var_i32_C, #function
	.proc	04
Divide__uDivide_var_i32_C:
.LLFB172:
	save	%sp, -96, %sp
.LLCFI172:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE172:
	.size	Divide__uDivide_var_i32_C, .-Divide__uDivide_var_i32_C
	.align 4
	.type	Divide__Divide_var_i32_C, #function
	.proc	04
Divide__Divide_var_i32_C:
.LLFB173:
	save	%sp, -96, %sp
.LLCFI173:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+72], %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+96], %o5
	cmp	%o5, 0
	bl	%icc, .LL960
	 nop
	cmp	%g1, 0
	bl	%icc, .LL959
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL963
	 nop
.LL959:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL962
	 nop
.LL960:
	cmp	%g1, 0
	bg	%icc, .LL961
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL963
	 nop
.LL961:
	add	%g1, -1, %g1
.LL962:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL963:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE173:
	.size	Divide__Divide_var_i32_C, .-Divide__Divide_var_i32_C
	.align 4
	.type	Divide__uDivide_param_i32_C, #function
	.proc	04
Divide__uDivide_param_i32_C:
.LLFB174:
	save	%sp, -96, %sp
.LLCFI174:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	wr	%g0, 0, %y
	ld	[%fp+72], %g1
	udiv	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE174:
	.size	Divide__uDivide_param_i32_C, .-Divide__uDivide_param_i32_C
	.align 4
	.type	Divide__Divide_param_i32_C, #function
	.proc	04
Divide__Divide_param_i32_C:
.LLFB175:
	save	%sp, -96, %sp
.LLCFI175:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %g1
	cmp	%o5, 0
	bl	%icc, .LL967
	 nop
	cmp	%g1, 0
	bl	%icc, .LL966
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL970
	 nop
.LL966:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL969
	 nop
.LL967:
	cmp	%g1, 0
	bg	%icc, .LL968
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL970
	 nop
.LL968:
	add	%g1, -1, %g1
.LL969:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL970:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE175:
	.size	Divide__Divide_param_i32_C, .-Divide__Divide_param_i32_C
	.align 4
	.type	Divide__uDivide_var_i32_u8, #function
	.proc	04
Divide__uDivide_var_i32_u8:
.LLFB176:
	save	%sp, -96, %sp
.LLCFI176:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE176:
	.size	Divide__uDivide_var_i32_u8, .-Divide__uDivide_var_i32_u8
	.align 4
	.type	Divide__Divide_var_i32_u8, #function
	.proc	04
Divide__Divide_var_i32_u8:
.LLFB177:
	save	%sp, -96, %sp
.LLCFI177:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+72], %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+112], %o5
	srl	%o5, 24, %o5
	and	%o5, 0xff, %o5
	cmp	%o5, 0
	bl	%icc, .LL974
	 nop
	cmp	%g1, 0
	bl	%icc, .LL973
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL977
	 nop
.LL973:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL976
	 nop
.LL974:
	cmp	%g1, 0
	bg	%icc, .LL975
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL977
	 nop
.LL975:
	add	%g1, -1, %g1
.LL976:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL977:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE177:
	.size	Divide__Divide_var_i32_u8, .-Divide__Divide_var_i32_u8
	.align 4
	.type	Divide__uDivide_param_i32_u8, #function
	.proc	04
Divide__uDivide_param_i32_u8:
.LLFB178:
	save	%sp, -96, %sp
.LLCFI178:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ld	[%fp+68], %o4
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE178:
	.size	Divide__uDivide_param_i32_u8, .-Divide__uDivide_param_i32_u8
	.align 4
	.type	Divide__Divide_param_i32_u8, #function
	.proc	04
Divide__Divide_param_i32_u8:
.LLFB179:
	save	%sp, -96, %sp
.LLCFI179:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+68], %g1
	cmp	%o5, 0
	bl	%icc, .LL981
	 nop
	cmp	%g1, 0
	bl	%icc, .LL980
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL984
	 nop
.LL980:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL983
	 nop
.LL981:
	cmp	%g1, 0
	bg	%icc, .LL982
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL984
	 nop
.LL982:
	add	%g1, -1, %g1
.LL983:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL984:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE179:
	.size	Divide__Divide_param_i32_u8, .-Divide__Divide_param_i32_u8
	.align 4
	.type	Divide__uDivide_var_i32_L, #function
	.proc	05
Divide__uDivide_var_i32_L:
.LLFB180:
	save	%sp, -96, %sp
.LLCFI180:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE180:
	.size	Divide__uDivide_var_i32_L, .-Divide__uDivide_var_i32_L
	.align 4
	.type	Divide__Divide_var_i32_L, #function
	.proc	05
Divide__Divide_var_i32_L:
.LLFB181:
	save	%sp, -96, %sp
.LLCFI181:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o2
	cmp	%o2, 0
	bl	%icc, .LL988
	 nop
	cmp	%o2, 0
	bne	%icc, .LL992
	 nop
	cmp	%o3, 0
.LL992:
	cmp	%o4, 0
	bl	%icc, .LL987
	 nop
	cmp	%o4, 0
	bne	%icc, .LL993
	 nop
	cmp	%o5, 0
.LL993:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL991
	 nop
.LL987:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL990
	 nop
.LL988:
	cmp	%o4, 0
	bg	%icc, .LL989
	 nop
	cmp	%o4, 0
	bne	%icc, .LL994
	 nop
	cmp	%o5, 0
	bne	%icc, .LL989
	 nop
	cmp	%o5, 0
.LL994:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL991
	 nop
.LL989:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL990:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL991:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE181:
	.size	Divide__Divide_var_i32_L, .-Divide__Divide_var_i32_L
	.align 4
	.type	Divide__uDivide_param_i32_L, #function
	.proc	05
Divide__uDivide_param_i32_L:
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
	mov	%o4, %o0
	mov	%o5, %o1
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE182:
	.size	Divide__uDivide_param_i32_L, .-Divide__uDivide_param_i32_L
	.align 4
	.type	Divide__Divide_param_i32_L, #function
	.proc	05
Divide__Divide_param_i32_L:
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
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL998
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL1002
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL1002:
	cmp	%o4, 0
	bl	%icc, .LL997
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1003
	 nop
	cmp	%o5, 0
.LL1003:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1001
	 nop
.LL997:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1000
	 nop
.LL998:
	cmp	%o4, 0
	bg	%icc, .LL999
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1004
	 nop
	cmp	%o5, 0
	bne	%icc, .LL999
	 nop
	cmp	%o5, 0
.LL1004:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1001
	 nop
.LL999:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1000:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1001:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE183:
	.size	Divide__Divide_param_i32_L, .-Divide__Divide_param_i32_L
	.align 4
	.type	Divide__uDivide_var_i32_i8, #function
	.proc	04
Divide__uDivide_var_i32_i8:
.LLFB184:
	save	%sp, -96, %sp
.LLCFI184:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE184:
	.size	Divide__uDivide_var_i32_i8, .-Divide__uDivide_var_i32_i8
	.align 4
	.type	Divide__Divide_var_i32_i8, #function
	.proc	04
Divide__Divide_var_i32_i8:
.LLFB185:
	save	%sp, -96, %sp
.LLCFI185:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+72], %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+128], %o5
	sra	%o5, 24, %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	cmp	%o5, 0
	bl	%icc, .LL1008
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1007
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1011
	 nop
.LL1007:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1010
	 nop
.LL1008:
	cmp	%g1, 0
	bg	%icc, .LL1009
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1011
	 nop
.LL1009:
	add	%g1, -1, %g1
.LL1010:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1011:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE185:
	.size	Divide__Divide_var_i32_i8, .-Divide__Divide_var_i32_i8
	.align 4
	.type	Divide__uDivide_param_i32_i8, #function
	.proc	04
Divide__uDivide_param_i32_i8:
.LLFB186:
	save	%sp, -96, %sp
.LLCFI186:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ld	[%fp+68], %o4
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE186:
	.size	Divide__uDivide_param_i32_i8, .-Divide__uDivide_param_i32_i8
	.align 4
	.type	Divide__Divide_param_i32_i8, #function
	.proc	04
Divide__Divide_param_i32_i8:
.LLFB187:
	save	%sp, -96, %sp
.LLCFI187:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+68], %g1
	cmp	%o5, 0
	bl	%icc, .LL1015
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1014
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1018
	 nop
.LL1014:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1017
	 nop
.LL1015:
	cmp	%g1, 0
	bg	%icc, .LL1016
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1018
	 nop
.LL1016:
	add	%g1, -1, %g1
.LL1017:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1018:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE187:
	.size	Divide__Divide_param_i32_i8, .-Divide__Divide_param_i32_i8
	.align 4
	.type	Divide__uDivide_var_i32_u32, #function
	.proc	04
Divide__uDivide_var_i32_u32:
.LLFB188:
	save	%sp, -96, %sp
.LLCFI188:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE188:
	.size	Divide__uDivide_var_i32_u32, .-Divide__uDivide_var_i32_u32
	.align 4
	.type	Divide__Divide_var_i32_u32, #function
	.proc	04
Divide__Divide_var_i32_u32:
.LLFB189:
	save	%sp, -96, %sp
.LLCFI189:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+72], %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+132], %o5
	cmp	%o5, 0
	bl	%icc, .LL1022
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1021
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1025
	 nop
.LL1021:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1024
	 nop
.LL1022:
	cmp	%g1, 0
	bg	%icc, .LL1023
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1025
	 nop
.LL1023:
	add	%g1, -1, %g1
.LL1024:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1025:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE189:
	.size	Divide__Divide_var_i32_u32, .-Divide__Divide_var_i32_u32
	.align 4
	.type	Divide__uDivide_param_i32_u32, #function
	.proc	04
Divide__uDivide_param_i32_u32:
.LLFB190:
	save	%sp, -96, %sp
.LLCFI190:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o4
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE190:
	.size	Divide__uDivide_param_i32_u32, .-Divide__uDivide_param_i32_u32
	.align 4
	.type	Divide__Divide_param_i32_u32, #function
	.proc	04
Divide__Divide_param_i32_u32:
.LLFB191:
	save	%sp, -96, %sp
.LLCFI191:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL1029
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1028
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1032
	 nop
.LL1028:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1031
	 nop
.LL1029:
	cmp	%g1, 0
	bg	%icc, .LL1030
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1032
	 nop
.LL1030:
	add	%g1, -1, %g1
.LL1031:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1032:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE191:
	.size	Divide__Divide_param_i32_u32, .-Divide__Divide_param_i32_u32
	.align 4
	.type	Divide__uDivide_var_i16_u16, #function
	.proc	04
Divide__uDivide_var_i16_u16:
.LLFB192:
	save	%sp, -96, %sp
.LLCFI192:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE192:
	.size	Divide__uDivide_var_i16_u16, .-Divide__uDivide_var_i16_u16
	.align 4
	.type	Divide__Divide_var_i16_u16, #function
	.proc	03
Divide__Divide_var_i16_u16:
.LLFB193:
	save	%sp, -96, %sp
.LLCFI193:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+52], %o5
	srl	%o5, 16, %o5
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL1036
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1035
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1039
	 nop
.LL1035:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1038
	 nop
.LL1036:
	cmp	%g1, 0
	bg	%icc, .LL1037
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1039
	 nop
.LL1037:
	add	%g1, -1, %g1
.LL1038:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1039:
	mov	%g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL1040
	 nop
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+52], %o5
	srl	%o5, 16, %o5
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL1042
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1041
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1045
	 nop
.LL1041:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1044
	 nop
.LL1042:
	cmp	%g1, 0
	bg	%icc, .LL1043
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1045
	 nop
.LL1043:
	add	%g1, -1, %g1
.LL1044:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1045:
	mov	%g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1046
	 nop
.LL1040:
	sethi	%hi(7168), %g1
	or	%g1, 737, %o0
	call	_m3_fault, 0
	 nop
.LL1046:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+52], %o5
	srl	%o5, 16, %o5
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL1048
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1047
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1051
	 nop
.LL1047:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1050
	 nop
.LL1048:
	cmp	%g1, 0
	bg	%icc, .LL1049
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1051
	 nop
.LL1049:
	add	%g1, -1, %g1
.LL1050:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1051:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE193:
	.size	Divide__Divide_var_i16_u16, .-Divide__Divide_var_i16_u16
	.align 4
	.type	Divide__uDivide_param_i16_u16, #function
	.proc	04
Divide__uDivide_param_i16_u16:
.LLFB194:
	save	%sp, -96, %sp
.LLCFI194:
	sth	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE194:
	.size	Divide__uDivide_param_i16_u16, .-Divide__uDivide_param_i16_u16
	.align 4
	.type	Divide__Divide_param_i16_u16, #function
	.proc	03
Divide__Divide_param_i16_u16:
.LLFB195:
	save	%sp, -96, %sp
.LLCFI195:
	sth	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	lduh	[%fp+72], %o5
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL1055
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1054
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1058
	 nop
.LL1054:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1057
	 nop
.LL1055:
	cmp	%g1, 0
	bg	%icc, .LL1056
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1058
	 nop
.LL1056:
	add	%g1, -1, %g1
.LL1057:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1058:
	mov	%g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL1059
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	lduh	[%fp+72], %o5
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL1061
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1060
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1064
	 nop
.LL1060:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1063
	 nop
.LL1061:
	cmp	%g1, 0
	bg	%icc, .LL1062
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1064
	 nop
.LL1062:
	add	%g1, -1, %g1
.LL1063:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1064:
	mov	%g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1065
	 nop
.LL1059:
	sethi	%hi(7168), %g1
	or	%g1, 801, %o0
	call	_m3_fault, 0
	 nop
.LL1065:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	lduh	[%fp+72], %o5
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL1067
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1066
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1070
	 nop
.LL1066:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1069
	 nop
.LL1067:
	cmp	%g1, 0
	bg	%icc, .LL1068
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1070
	 nop
.LL1068:
	add	%g1, -1, %g1
.LL1069:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1070:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE195:
	.size	Divide__Divide_param_i16_u16, .-Divide__Divide_param_i16_u16
	.align 4
	.type	Divide__uDivide_var_i16_u64, #function
	.proc	05
Divide__uDivide_var_i16_u64:
.LLFB196:
	save	%sp, -96, %sp
.LLCFI196:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE196:
	.size	Divide__uDivide_var_i16_u64, .-Divide__uDivide_var_i16_u64
	.align 4
	.type	Divide__Divide_var_i16_u64, #function
	.proc	05
Divide__Divide_var_i16_u64:
.LLFB197:
	save	%sp, -96, %sp
.LLCFI197:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o2
	cmp	%o2, 0
	bl	%icc, .LL1074
	 nop
	cmp	%o2, 0
	bne	%icc, .LL1078
	 nop
	cmp	%o3, 0
.LL1078:
	cmp	%o4, 0
	bl	%icc, .LL1073
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1079
	 nop
	cmp	%o5, 0
.LL1079:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1077
	 nop
.LL1073:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1076
	 nop
.LL1074:
	cmp	%o4, 0
	bg	%icc, .LL1075
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1080
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1075
	 nop
	cmp	%o5, 0
.LL1080:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1077
	 nop
.LL1075:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1076:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1077:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE197:
	.size	Divide__Divide_var_i16_u64, .-Divide__Divide_var_i16_u64
	.align 4
	.type	Divide__uDivide_param_i16_u64, #function
	.proc	05
Divide__uDivide_param_i16_u64:
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
	mov	%o4, %o0
	mov	%o5, %o1
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE198:
	.size	Divide__uDivide_param_i16_u64, .-Divide__uDivide_param_i16_u64
	.align 4
	.type	Divide__Divide_param_i16_u64, #function
	.proc	05
Divide__Divide_param_i16_u64:
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
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL1084
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL1088
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL1088:
	cmp	%o4, 0
	bl	%icc, .LL1083
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1089
	 nop
	cmp	%o5, 0
.LL1089:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1087
	 nop
.LL1083:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1086
	 nop
.LL1084:
	cmp	%o4, 0
	bg	%icc, .LL1085
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1090
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1085
	 nop
	cmp	%o5, 0
.LL1090:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1087
	 nop
.LL1085:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1086:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1087:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE199:
	.size	Divide__Divide_param_i16_u64, .-Divide__Divide_param_i16_u64
	.align 4
	.type	Divide__uDivide_var_i16_LC, #function
	.proc	05
Divide__uDivide_var_i16_LC:
.LLFB200:
	save	%sp, -96, %sp
.LLCFI200:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE200:
	.size	Divide__uDivide_var_i16_LC, .-Divide__uDivide_var_i16_LC
	.align 4
	.type	Divide__Divide_var_i16_LC, #function
	.proc	05
Divide__Divide_var_i16_LC:
.LLFB201:
	save	%sp, -96, %sp
.LLCFI201:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o2
	cmp	%o2, 0
	bl	%icc, .LL1094
	 nop
	cmp	%o2, 0
	bne	%icc, .LL1098
	 nop
	cmp	%o3, 0
.LL1098:
	cmp	%o4, 0
	bl	%icc, .LL1093
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1099
	 nop
	cmp	%o5, 0
.LL1099:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1097
	 nop
.LL1093:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1096
	 nop
.LL1094:
	cmp	%o4, 0
	bg	%icc, .LL1095
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1100
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1095
	 nop
	cmp	%o5, 0
.LL1100:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1097
	 nop
.LL1095:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1096:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1097:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE201:
	.size	Divide__Divide_var_i16_LC, .-Divide__Divide_var_i16_LC
	.align 4
	.type	Divide__uDivide_param_i16_LC, #function
	.proc	05
Divide__uDivide_param_i16_LC:
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
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE202:
	.size	Divide__uDivide_param_i16_LC, .-Divide__uDivide_param_i16_LC
	.align 4
	.type	Divide__Divide_param_i16_LC, #function
	.proc	05
Divide__Divide_param_i16_LC:
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
	cmp	%o2, 0
	bl	%icc, .LL1104
	 nop
	cmp	%o2, 0
	bne	%icc, .LL1108
	 nop
	cmp	%o3, 0
.LL1108:
	cmp	%o4, 0
	bl	%icc, .LL1103
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1109
	 nop
	cmp	%o5, 0
.LL1109:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1107
	 nop
.LL1103:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1106
	 nop
.LL1104:
	cmp	%o4, 0
	bg	%icc, .LL1105
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1110
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1105
	 nop
	cmp	%o5, 0
.LL1110:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1107
	 nop
.LL1105:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1106:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1107:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE203:
	.size	Divide__Divide_param_i16_LC, .-Divide__Divide_param_i16_LC
	.align 4
	.type	Divide__uDivide_var_i16_i32, #function
	.proc	04
Divide__uDivide_var_i16_i32:
.LLFB204:
	save	%sp, -96, %sp
.LLCFI204:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE204:
	.size	Divide__uDivide_var_i16_i32, .-Divide__uDivide_var_i16_i32
	.align 4
	.type	Divide__Divide_var_i16_i32, #function
	.proc	03
Divide__Divide_var_i16_i32:
.LLFB205:
	save	%sp, -96, %sp
.LLCFI205:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL1114
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1113
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1117
	 nop
.LL1113:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1116
	 nop
.LL1114:
	cmp	%g1, 0
	bg	%icc, .LL1115
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1117
	 nop
.LL1115:
	add	%g1, -1, %g1
.LL1116:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1117:
	mov	%g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL1118
	 nop
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL1120
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1119
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1123
	 nop
.LL1119:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1122
	 nop
.LL1120:
	cmp	%g1, 0
	bg	%icc, .LL1121
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1123
	 nop
.LL1121:
	add	%g1, -1, %g1
.LL1122:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1123:
	mov	%g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1124
	 nop
.LL1118:
	sethi	%hi(8192), %g1
	or	%g1, 97, %o0
	call	_m3_fault, 0
	 nop
.LL1124:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL1126
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1125
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1129
	 nop
.LL1125:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1128
	 nop
.LL1126:
	cmp	%g1, 0
	bg	%icc, .LL1127
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1129
	 nop
.LL1127:
	add	%g1, -1, %g1
.LL1128:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1129:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE205:
	.size	Divide__Divide_var_i16_i32, .-Divide__Divide_var_i16_i32
	.align 4
	.type	Divide__uDivide_param_i16_i32, #function
	.proc	04
Divide__uDivide_param_i16_i32:
.LLFB206:
	save	%sp, -96, %sp
.LLCFI206:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE206:
	.size	Divide__uDivide_param_i16_i32, .-Divide__uDivide_param_i16_i32
	.align 4
	.type	Divide__Divide_param_i16_i32, #function
	.proc	03
Divide__Divide_param_i16_i32:
.LLFB207:
	save	%sp, -96, %sp
.LLCFI207:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL1133
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1132
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1136
	 nop
.LL1132:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1135
	 nop
.LL1133:
	cmp	%g1, 0
	bg	%icc, .LL1134
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1136
	 nop
.LL1134:
	add	%g1, -1, %g1
.LL1135:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1136:
	mov	%g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL1137
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL1139
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1138
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1142
	 nop
.LL1138:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1141
	 nop
.LL1139:
	cmp	%g1, 0
	bg	%icc, .LL1140
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1142
	 nop
.LL1140:
	add	%g1, -1, %g1
.LL1141:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1142:
	mov	%g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1143
	 nop
.LL1137:
	sethi	%hi(8192), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1143:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL1145
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1144
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1148
	 nop
.LL1144:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1147
	 nop
.LL1145:
	cmp	%g1, 0
	bg	%icc, .LL1146
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1148
	 nop
.LL1146:
	add	%g1, -1, %g1
.LL1147:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1148:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE207:
	.size	Divide__Divide_param_i16_i32, .-Divide__Divide_param_i16_i32
	.align 4
	.type	Divide__uDivide_var_i16_i16, #function
	.proc	04
Divide__uDivide_var_i16_i16:
.LLFB208:
	save	%sp, -96, %sp
.LLCFI208:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE208:
	.size	Divide__uDivide_var_i16_i16, .-Divide__uDivide_var_i16_i16
	.align 4
	.type	Divide__Divide_var_i16_i16, #function
	.proc	03
Divide__Divide_var_i16_i16:
.LLFB209:
	save	%sp, -96, %sp
.LLCFI209:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+76], %o5
	sra	%o5, 16, %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL1152
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1151
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1155
	 nop
.LL1151:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1154
	 nop
.LL1152:
	cmp	%g1, 0
	bg	%icc, .LL1153
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1155
	 nop
.LL1153:
	add	%g1, -1, %g1
.LL1154:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1155:
	mov	%g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL1156
	 nop
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+76], %o5
	sra	%o5, 16, %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL1158
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1157
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1161
	 nop
.LL1157:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1160
	 nop
.LL1158:
	cmp	%g1, 0
	bg	%icc, .LL1159
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1161
	 nop
.LL1159:
	add	%g1, -1, %g1
.LL1160:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1161:
	mov	%g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1162
	 nop
.LL1156:
	sethi	%hi(8192), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL1162:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+76], %o5
	sra	%o5, 16, %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL1164
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1163
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1167
	 nop
.LL1163:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1166
	 nop
.LL1164:
	cmp	%g1, 0
	bg	%icc, .LL1165
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1167
	 nop
.LL1165:
	add	%g1, -1, %g1
.LL1166:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1167:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE209:
	.size	Divide__Divide_var_i16_i16, .-Divide__Divide_var_i16_i16
	.align 4
	.type	Divide__uDivide_param_i16_i16, #function
	.proc	04
Divide__uDivide_param_i16_i16:
.LLFB210:
	save	%sp, -96, %sp
.LLCFI210:
	sth	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE210:
	.size	Divide__uDivide_param_i16_i16, .-Divide__uDivide_param_i16_i16
	.align 4
	.type	Divide__Divide_param_i16_i16, #function
	.proc	03
Divide__Divide_param_i16_i16:
.LLFB211:
	save	%sp, -96, %sp
.LLCFI211:
	sth	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	lduh	[%fp+72], %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL1171
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1170
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1174
	 nop
.LL1170:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1173
	 nop
.LL1171:
	cmp	%g1, 0
	bg	%icc, .LL1172
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1174
	 nop
.LL1172:
	add	%g1, -1, %g1
.LL1173:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1174:
	mov	%g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL1175
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	lduh	[%fp+72], %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL1177
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1176
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1180
	 nop
.LL1176:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1179
	 nop
.LL1177:
	cmp	%g1, 0
	bg	%icc, .LL1178
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1180
	 nop
.LL1178:
	add	%g1, -1, %g1
.LL1179:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1180:
	mov	%g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1181
	 nop
.LL1175:
	sethi	%hi(8192), %g1
	or	%g1, 289, %o0
	call	_m3_fault, 0
	 nop
.LL1181:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	lduh	[%fp+72], %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL1183
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1182
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1186
	 nop
.LL1182:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1185
	 nop
.LL1183:
	cmp	%g1, 0
	bg	%icc, .LL1184
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1186
	 nop
.LL1184:
	add	%g1, -1, %g1
.LL1185:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1186:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE211:
	.size	Divide__Divide_param_i16_i16, .-Divide__Divide_param_i16_i16
	.align 4
	.type	Divide__uDivide_var_i16_I, #function
	.proc	04
Divide__uDivide_var_i16_I:
.LLFB212:
	save	%sp, -96, %sp
.LLCFI212:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE212:
	.size	Divide__uDivide_var_i16_I, .-Divide__uDivide_var_i16_I
	.align 4
	.type	Divide__Divide_var_i16_I, #function
	.proc	03
Divide__Divide_var_i16_I:
.LLFB213:
	save	%sp, -96, %sp
.LLCFI213:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+80], %o5
	cmp	%o5, 0
	bl	%icc, .LL1190
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1189
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1193
	 nop
.LL1189:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1192
	 nop
.LL1190:
	cmp	%g1, 0
	bg	%icc, .LL1191
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1193
	 nop
.LL1191:
	add	%g1, -1, %g1
.LL1192:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1193:
	mov	%g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL1194
	 nop
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+80], %o5
	cmp	%o5, 0
	bl	%icc, .LL1196
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1195
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1199
	 nop
.LL1195:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1198
	 nop
.LL1196:
	cmp	%g1, 0
	bg	%icc, .LL1197
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1199
	 nop
.LL1197:
	add	%g1, -1, %g1
.LL1198:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1199:
	mov	%g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1200
	 nop
.LL1194:
	sethi	%hi(8192), %g1
	or	%g1, 353, %o0
	call	_m3_fault, 0
	 nop
.LL1200:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+80], %o5
	cmp	%o5, 0
	bl	%icc, .LL1202
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1201
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1205
	 nop
.LL1201:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1204
	 nop
.LL1202:
	cmp	%g1, 0
	bg	%icc, .LL1203
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1205
	 nop
.LL1203:
	add	%g1, -1, %g1
.LL1204:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1205:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE213:
	.size	Divide__Divide_var_i16_I, .-Divide__Divide_var_i16_I
	.align 4
	.type	Divide__uDivide_param_i16_I, #function
	.proc	04
Divide__uDivide_param_i16_I:
.LLFB214:
	save	%sp, -96, %sp
.LLCFI214:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE214:
	.size	Divide__uDivide_param_i16_I, .-Divide__uDivide_param_i16_I
	.align 4
	.type	Divide__Divide_param_i16_I, #function
	.proc	03
Divide__Divide_param_i16_I:
.LLFB215:
	save	%sp, -96, %sp
.LLCFI215:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL1209
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1208
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1212
	 nop
.LL1208:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1211
	 nop
.LL1209:
	cmp	%g1, 0
	bg	%icc, .LL1210
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1212
	 nop
.LL1210:
	add	%g1, -1, %g1
.LL1211:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1212:
	mov	%g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL1213
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL1215
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1214
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1218
	 nop
.LL1214:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1217
	 nop
.LL1215:
	cmp	%g1, 0
	bg	%icc, .LL1216
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1218
	 nop
.LL1216:
	add	%g1, -1, %g1
.LL1217:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1218:
	mov	%g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1219
	 nop
.LL1213:
	sethi	%hi(8192), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1219:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL1221
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1220
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1224
	 nop
.LL1220:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1223
	 nop
.LL1221:
	cmp	%g1, 0
	bg	%icc, .LL1222
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1224
	 nop
.LL1222:
	add	%g1, -1, %g1
.LL1223:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1224:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE215:
	.size	Divide__Divide_param_i16_I, .-Divide__Divide_param_i16_I
	.align 4
	.type	Divide__uDivide_var_i16_i64, #function
	.proc	05
Divide__uDivide_var_i16_i64:
.LLFB216:
	save	%sp, -96, %sp
.LLCFI216:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE216:
	.size	Divide__uDivide_var_i16_i64, .-Divide__uDivide_var_i16_i64
	.align 4
	.type	Divide__Divide_var_i16_i64, #function
	.proc	05
Divide__Divide_var_i16_i64:
.LLFB217:
	save	%sp, -96, %sp
.LLCFI217:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o2
	cmp	%o2, 0
	bl	%icc, .LL1228
	 nop
	cmp	%o2, 0
	bne	%icc, .LL1232
	 nop
	cmp	%o3, 0
.LL1232:
	cmp	%o4, 0
	bl	%icc, .LL1227
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1233
	 nop
	cmp	%o5, 0
.LL1233:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1231
	 nop
.LL1227:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1230
	 nop
.LL1228:
	cmp	%o4, 0
	bg	%icc, .LL1229
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1234
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1229
	 nop
	cmp	%o5, 0
.LL1234:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1231
	 nop
.LL1229:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1230:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1231:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE217:
	.size	Divide__Divide_var_i16_i64, .-Divide__Divide_var_i16_i64
	.align 4
	.type	Divide__uDivide_param_i16_i64, #function
	.proc	05
Divide__uDivide_param_i16_i64:
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
	mov	%o4, %o0
	mov	%o5, %o1
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE218:
	.size	Divide__uDivide_param_i16_i64, .-Divide__uDivide_param_i16_i64
	.align 4
	.type	Divide__Divide_param_i16_i64, #function
	.proc	05
Divide__Divide_param_i16_i64:
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
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL1238
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL1242
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL1242:
	cmp	%o4, 0
	bl	%icc, .LL1237
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1243
	 nop
	cmp	%o5, 0
.LL1243:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1241
	 nop
.LL1237:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1240
	 nop
.LL1238:
	cmp	%o4, 0
	bg	%icc, .LL1239
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1244
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1239
	 nop
	cmp	%o5, 0
.LL1244:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1241
	 nop
.LL1239:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1240:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1241:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE219:
	.size	Divide__Divide_param_i16_i64, .-Divide__Divide_param_i16_i64
	.align 4
	.type	Divide__uDivide_var_i16_C, #function
	.proc	04
Divide__uDivide_var_i16_C:
.LLFB220:
	save	%sp, -96, %sp
.LLCFI220:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE220:
	.size	Divide__uDivide_var_i16_C, .-Divide__uDivide_var_i16_C
	.align 4
	.type	Divide__Divide_var_i16_C, #function
	.proc	03
Divide__Divide_var_i16_C:
.LLFB221:
	save	%sp, -96, %sp
.LLCFI221:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+96], %o5
	cmp	%o5, 0
	bl	%icc, .LL1248
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1247
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1251
	 nop
.LL1247:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1250
	 nop
.LL1248:
	cmp	%g1, 0
	bg	%icc, .LL1249
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1251
	 nop
.LL1249:
	add	%g1, -1, %g1
.LL1250:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1251:
	mov	%g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL1252
	 nop
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+96], %o5
	cmp	%o5, 0
	bl	%icc, .LL1254
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1253
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1257
	 nop
.LL1253:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1256
	 nop
.LL1254:
	cmp	%g1, 0
	bg	%icc, .LL1255
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1257
	 nop
.LL1255:
	add	%g1, -1, %g1
.LL1256:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1257:
	mov	%g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1258
	 nop
.LL1252:
	sethi	%hi(8192), %g1
	or	%g1, 609, %o0
	call	_m3_fault, 0
	 nop
.LL1258:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+96], %o5
	cmp	%o5, 0
	bl	%icc, .LL1260
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1259
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1263
	 nop
.LL1259:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1262
	 nop
.LL1260:
	cmp	%g1, 0
	bg	%icc, .LL1261
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1263
	 nop
.LL1261:
	add	%g1, -1, %g1
.LL1262:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1263:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE221:
	.size	Divide__Divide_var_i16_C, .-Divide__Divide_var_i16_C
	.align 4
	.type	Divide__uDivide_param_i16_C, #function
	.proc	04
Divide__uDivide_param_i16_C:
.LLFB222:
	save	%sp, -96, %sp
.LLCFI222:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	wr	%g0, 0, %y
	ld	[%fp+72], %g1
	udiv	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE222:
	.size	Divide__uDivide_param_i16_C, .-Divide__uDivide_param_i16_C
	.align 4
	.type	Divide__Divide_param_i16_C, #function
	.proc	03
Divide__Divide_param_i16_C:
.LLFB223:
	save	%sp, -96, %sp
.LLCFI223:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL1267
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1266
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1270
	 nop
.LL1266:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1269
	 nop
.LL1267:
	cmp	%g1, 0
	bg	%icc, .LL1268
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1270
	 nop
.LL1268:
	add	%g1, -1, %g1
.LL1269:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1270:
	mov	%g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL1271
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL1273
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1272
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1276
	 nop
.LL1272:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1275
	 nop
.LL1273:
	cmp	%g1, 0
	bg	%icc, .LL1274
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1276
	 nop
.LL1274:
	add	%g1, -1, %g1
.LL1275:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1276:
	mov	%g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1277
	 nop
.LL1271:
	sethi	%hi(8192), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1277:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL1279
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1278
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1282
	 nop
.LL1278:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1281
	 nop
.LL1279:
	cmp	%g1, 0
	bg	%icc, .LL1280
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1282
	 nop
.LL1280:
	add	%g1, -1, %g1
.LL1281:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1282:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE223:
	.size	Divide__Divide_param_i16_C, .-Divide__Divide_param_i16_C
	.align 4
	.type	Divide__uDivide_var_i16_u8, #function
	.proc	04
Divide__uDivide_var_i16_u8:
.LLFB224:
	save	%sp, -96, %sp
.LLCFI224:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE224:
	.size	Divide__uDivide_var_i16_u8, .-Divide__uDivide_var_i16_u8
	.align 4
	.type	Divide__Divide_var_i16_u8, #function
	.proc	03
Divide__Divide_var_i16_u8:
.LLFB225:
	save	%sp, -96, %sp
.LLCFI225:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+112], %o5
	srl	%o5, 24, %o5
	and	%o5, 0xff, %o5
	cmp	%o5, 0
	bl	%icc, .LL1286
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1285
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1289
	 nop
.LL1285:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1288
	 nop
.LL1286:
	cmp	%g1, 0
	bg	%icc, .LL1287
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1289
	 nop
.LL1287:
	add	%g1, -1, %g1
.LL1288:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1289:
	mov	%g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL1290
	 nop
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+112], %o5
	srl	%o5, 24, %o5
	and	%o5, 0xff, %o5
	cmp	%o5, 0
	bl	%icc, .LL1292
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1291
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1295
	 nop
.LL1291:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1294
	 nop
.LL1292:
	cmp	%g1, 0
	bg	%icc, .LL1293
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1295
	 nop
.LL1293:
	add	%g1, -1, %g1
.LL1294:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1295:
	mov	%g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1296
	 nop
.LL1290:
	sethi	%hi(8192), %g1
	or	%g1, 737, %o0
	call	_m3_fault, 0
	 nop
.LL1296:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+112], %o5
	srl	%o5, 24, %o5
	and	%o5, 0xff, %o5
	cmp	%o5, 0
	bl	%icc, .LL1298
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1297
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1301
	 nop
.LL1297:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1300
	 nop
.LL1298:
	cmp	%g1, 0
	bg	%icc, .LL1299
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1301
	 nop
.LL1299:
	add	%g1, -1, %g1
.LL1300:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1301:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE225:
	.size	Divide__Divide_var_i16_u8, .-Divide__Divide_var_i16_u8
	.align 4
	.type	Divide__uDivide_param_i16_u8, #function
	.proc	04
Divide__uDivide_param_i16_u8:
.LLFB226:
	save	%sp, -96, %sp
.LLCFI226:
	sth	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE226:
	.size	Divide__uDivide_param_i16_u8, .-Divide__uDivide_param_i16_u8
	.align 4
	.type	Divide__Divide_param_i16_u8, #function
	.proc	03
Divide__Divide_param_i16_u8:
.LLFB227:
	save	%sp, -96, %sp
.LLCFI227:
	sth	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	ldub	[%fp+72], %o5
	and	%o5, 0xff, %o5
	cmp	%o5, 0
	bl	%icc, .LL1305
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1304
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1308
	 nop
.LL1304:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1307
	 nop
.LL1305:
	cmp	%g1, 0
	bg	%icc, .LL1306
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1308
	 nop
.LL1306:
	add	%g1, -1, %g1
.LL1307:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1308:
	mov	%g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL1309
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	ldub	[%fp+72], %o5
	and	%o5, 0xff, %o5
	cmp	%o5, 0
	bl	%icc, .LL1311
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1310
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1314
	 nop
.LL1310:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1313
	 nop
.LL1311:
	cmp	%g1, 0
	bg	%icc, .LL1312
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1314
	 nop
.LL1312:
	add	%g1, -1, %g1
.LL1313:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1314:
	mov	%g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1315
	 nop
.LL1309:
	sethi	%hi(8192), %g1
	or	%g1, 801, %o0
	call	_m3_fault, 0
	 nop
.LL1315:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	ldub	[%fp+72], %o5
	and	%o5, 0xff, %o5
	cmp	%o5, 0
	bl	%icc, .LL1317
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1316
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1320
	 nop
.LL1316:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1319
	 nop
.LL1317:
	cmp	%g1, 0
	bg	%icc, .LL1318
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1320
	 nop
.LL1318:
	add	%g1, -1, %g1
.LL1319:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1320:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE227:
	.size	Divide__Divide_param_i16_u8, .-Divide__Divide_param_i16_u8
	.align 4
	.type	Divide__uDivide_var_i16_L, #function
	.proc	05
Divide__uDivide_var_i16_L:
.LLFB228:
	save	%sp, -96, %sp
.LLCFI228:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE228:
	.size	Divide__uDivide_var_i16_L, .-Divide__uDivide_var_i16_L
	.align 4
	.type	Divide__Divide_var_i16_L, #function
	.proc	05
Divide__Divide_var_i16_L:
.LLFB229:
	save	%sp, -96, %sp
.LLCFI229:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o2
	cmp	%o2, 0
	bl	%icc, .LL1324
	 nop
	cmp	%o2, 0
	bne	%icc, .LL1328
	 nop
	cmp	%o3, 0
.LL1328:
	cmp	%o4, 0
	bl	%icc, .LL1323
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1329
	 nop
	cmp	%o5, 0
.LL1329:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1327
	 nop
.LL1323:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1326
	 nop
.LL1324:
	cmp	%o4, 0
	bg	%icc, .LL1325
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1330
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1325
	 nop
	cmp	%o5, 0
.LL1330:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1327
	 nop
.LL1325:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1326:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1327:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE229:
	.size	Divide__Divide_var_i16_L, .-Divide__Divide_var_i16_L
	.align 4
	.type	Divide__uDivide_param_i16_L, #function
	.proc	05
Divide__uDivide_param_i16_L:
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
	mov	%o4, %o0
	mov	%o5, %o1
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE230:
	.size	Divide__uDivide_param_i16_L, .-Divide__uDivide_param_i16_L
	.align 4
	.type	Divide__Divide_param_i16_L, #function
	.proc	05
Divide__Divide_param_i16_L:
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
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL1334
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL1338
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL1338:
	cmp	%o4, 0
	bl	%icc, .LL1333
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1339
	 nop
	cmp	%o5, 0
.LL1339:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1337
	 nop
.LL1333:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1336
	 nop
.LL1334:
	cmp	%o4, 0
	bg	%icc, .LL1335
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1340
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1335
	 nop
	cmp	%o5, 0
.LL1340:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1337
	 nop
.LL1335:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1336:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1337:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE231:
	.size	Divide__Divide_param_i16_L, .-Divide__Divide_param_i16_L
	.align 4
	.type	Divide__uDivide_var_i16_i8, #function
	.proc	04
Divide__uDivide_var_i16_i8:
.LLFB232:
	save	%sp, -96, %sp
.LLCFI232:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE232:
	.size	Divide__uDivide_var_i16_i8, .-Divide__uDivide_var_i16_i8
	.align 4
	.type	Divide__Divide_var_i16_i8, #function
	.proc	03
Divide__Divide_var_i16_i8:
.LLFB233:
	save	%sp, -96, %sp
.LLCFI233:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+128], %o5
	sra	%o5, 24, %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	cmp	%o5, 0
	bl	%icc, .LL1344
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1343
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1347
	 nop
.LL1343:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1346
	 nop
.LL1344:
	cmp	%g1, 0
	bg	%icc, .LL1345
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1347
	 nop
.LL1345:
	add	%g1, -1, %g1
.LL1346:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1347:
	mov	%g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL1348
	 nop
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+128], %o5
	sra	%o5, 24, %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	cmp	%o5, 0
	bl	%icc, .LL1350
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1349
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1353
	 nop
.LL1349:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1352
	 nop
.LL1350:
	cmp	%g1, 0
	bg	%icc, .LL1351
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1353
	 nop
.LL1351:
	add	%g1, -1, %g1
.LL1352:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1353:
	mov	%g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1354
	 nop
.LL1348:
	sethi	%hi(8192), %g1
	or	%g1, 993, %o0
	call	_m3_fault, 0
	 nop
.LL1354:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+128], %o5
	sra	%o5, 24, %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	cmp	%o5, 0
	bl	%icc, .LL1356
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1355
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1359
	 nop
.LL1355:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1358
	 nop
.LL1356:
	cmp	%g1, 0
	bg	%icc, .LL1357
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1359
	 nop
.LL1357:
	add	%g1, -1, %g1
.LL1358:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1359:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE233:
	.size	Divide__Divide_var_i16_i8, .-Divide__Divide_var_i16_i8
	.align 4
	.type	Divide__uDivide_param_i16_i8, #function
	.proc	04
Divide__uDivide_param_i16_i8:
.LLFB234:
	save	%sp, -96, %sp
.LLCFI234:
	sth	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE234:
	.size	Divide__uDivide_param_i16_i8, .-Divide__uDivide_param_i16_i8
	.align 4
	.type	Divide__Divide_param_i16_i8, #function
	.proc	03
Divide__Divide_param_i16_i8:
.LLFB235:
	save	%sp, -96, %sp
.LLCFI235:
	sth	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	ldub	[%fp+72], %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	cmp	%o5, 0
	bl	%icc, .LL1363
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1362
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1366
	 nop
.LL1362:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1365
	 nop
.LL1363:
	cmp	%g1, 0
	bg	%icc, .LL1364
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1366
	 nop
.LL1364:
	add	%g1, -1, %g1
.LL1365:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1366:
	mov	%g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL1367
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	ldub	[%fp+72], %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	cmp	%o5, 0
	bl	%icc, .LL1369
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1368
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1372
	 nop
.LL1368:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1371
	 nop
.LL1369:
	cmp	%g1, 0
	bg	%icc, .LL1370
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1372
	 nop
.LL1370:
	add	%g1, -1, %g1
.LL1371:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1372:
	mov	%g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1373
	 nop
.LL1367:
	sethi	%hi(9216), %g1
	or	%g1, 33, %o0
	call	_m3_fault, 0
	 nop
.LL1373:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	ldub	[%fp+72], %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	cmp	%o5, 0
	bl	%icc, .LL1375
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1374
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1378
	 nop
.LL1374:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1377
	 nop
.LL1375:
	cmp	%g1, 0
	bg	%icc, .LL1376
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1378
	 nop
.LL1376:
	add	%g1, -1, %g1
.LL1377:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1378:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE235:
	.size	Divide__Divide_param_i16_i8, .-Divide__Divide_param_i16_i8
	.align 4
	.type	Divide__uDivide_var_i16_u32, #function
	.proc	04
Divide__uDivide_var_i16_u32:
.LLFB236:
	save	%sp, -96, %sp
.LLCFI236:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE236:
	.size	Divide__uDivide_var_i16_u32, .-Divide__uDivide_var_i16_u32
	.align 4
	.type	Divide__Divide_var_i16_u32, #function
	.proc	03
Divide__Divide_var_i16_u32:
.LLFB237:
	save	%sp, -96, %sp
.LLCFI237:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+132], %o5
	cmp	%o5, 0
	bl	%icc, .LL1382
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1381
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1385
	 nop
.LL1381:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1384
	 nop
.LL1382:
	cmp	%g1, 0
	bg	%icc, .LL1383
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1385
	 nop
.LL1383:
	add	%g1, -1, %g1
.LL1384:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1385:
	mov	%g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL1386
	 nop
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+132], %o5
	cmp	%o5, 0
	bl	%icc, .LL1388
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1387
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1391
	 nop
.LL1387:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1390
	 nop
.LL1388:
	cmp	%g1, 0
	bg	%icc, .LL1389
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1391
	 nop
.LL1389:
	add	%g1, -1, %g1
.LL1390:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1391:
	mov	%g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1392
	 nop
.LL1386:
	sethi	%hi(9216), %g1
	or	%g1, 97, %o0
	call	_m3_fault, 0
	 nop
.LL1392:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+132], %o5
	cmp	%o5, 0
	bl	%icc, .LL1394
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1393
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1397
	 nop
.LL1393:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1396
	 nop
.LL1394:
	cmp	%g1, 0
	bg	%icc, .LL1395
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1397
	 nop
.LL1395:
	add	%g1, -1, %g1
.LL1396:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1397:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE237:
	.size	Divide__Divide_var_i16_u32, .-Divide__Divide_var_i16_u32
	.align 4
	.type	Divide__uDivide_param_i16_u32, #function
	.proc	04
Divide__uDivide_param_i16_u32:
.LLFB238:
	save	%sp, -96, %sp
.LLCFI238:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE238:
	.size	Divide__uDivide_param_i16_u32, .-Divide__uDivide_param_i16_u32
	.align 4
	.type	Divide__Divide_param_i16_u32, #function
	.proc	03
Divide__Divide_param_i16_u32:
.LLFB239:
	save	%sp, -96, %sp
.LLCFI239:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL1401
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1400
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1404
	 nop
.LL1400:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1403
	 nop
.LL1401:
	cmp	%g1, 0
	bg	%icc, .LL1402
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1404
	 nop
.LL1402:
	add	%g1, -1, %g1
.LL1403:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1404:
	mov	%g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL1405
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL1407
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1406
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1410
	 nop
.LL1406:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1409
	 nop
.LL1407:
	cmp	%g1, 0
	bg	%icc, .LL1408
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1410
	 nop
.LL1408:
	add	%g1, -1, %g1
.LL1409:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1410:
	mov	%g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1411
	 nop
.LL1405:
	sethi	%hi(9216), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1411:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL1413
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1412
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1416
	 nop
.LL1412:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1415
	 nop
.LL1413:
	cmp	%g1, 0
	bg	%icc, .LL1414
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1416
	 nop
.LL1414:
	add	%g1, -1, %g1
.LL1415:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1416:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE239:
	.size	Divide__Divide_param_i16_u32, .-Divide__Divide_param_i16_u32
	.align 4
	.type	Divide__uDivide_var_I_u16, #function
	.proc	04
Divide__uDivide_var_I_u16:
.LLFB240:
	save	%sp, -96, %sp
.LLCFI240:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE240:
	.size	Divide__uDivide_var_I_u16, .-Divide__uDivide_var_I_u16
	.align 4
	.type	Divide__Divide_var_I_u16, #function
	.proc	04
Divide__Divide_var_I_u16:
.LLFB241:
	save	%sp, -96, %sp
.LLCFI241:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+80], %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+52], %o5
	srl	%o5, 16, %o5
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL1420
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1419
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1423
	 nop
.LL1419:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1422
	 nop
.LL1420:
	cmp	%g1, 0
	bg	%icc, .LL1421
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1423
	 nop
.LL1421:
	add	%g1, -1, %g1
.LL1422:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1423:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE241:
	.size	Divide__Divide_var_I_u16, .-Divide__Divide_var_I_u16
	.align 4
	.type	Divide__uDivide_param_I_u16, #function
	.proc	04
Divide__uDivide_param_I_u16:
.LLFB242:
	save	%sp, -96, %sp
.LLCFI242:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ld	[%fp+68], %o4
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE242:
	.size	Divide__uDivide_param_I_u16, .-Divide__uDivide_param_I_u16
	.align 4
	.type	Divide__Divide_param_I_u16, #function
	.proc	04
Divide__Divide_param_I_u16:
.LLFB243:
	save	%sp, -96, %sp
.LLCFI243:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+68], %g1
	cmp	%o5, 0
	bl	%icc, .LL1427
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1426
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1430
	 nop
.LL1426:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1429
	 nop
.LL1427:
	cmp	%g1, 0
	bg	%icc, .LL1428
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1430
	 nop
.LL1428:
	add	%g1, -1, %g1
.LL1429:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1430:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE243:
	.size	Divide__Divide_param_I_u16, .-Divide__Divide_param_I_u16
	.align 4
	.type	Divide__uDivide_var_I_u64, #function
	.proc	05
Divide__uDivide_var_I_u64:
.LLFB244:
	save	%sp, -96, %sp
.LLCFI244:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE244:
	.size	Divide__uDivide_var_I_u64, .-Divide__uDivide_var_I_u64
	.align 4
	.type	Divide__Divide_var_I_u64, #function
	.proc	05
Divide__Divide_var_I_u64:
.LLFB245:
	save	%sp, -96, %sp
.LLCFI245:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o2
	cmp	%o2, 0
	bl	%icc, .LL1434
	 nop
	cmp	%o2, 0
	bne	%icc, .LL1438
	 nop
	cmp	%o3, 0
.LL1438:
	cmp	%o4, 0
	bl	%icc, .LL1433
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1439
	 nop
	cmp	%o5, 0
.LL1439:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1437
	 nop
.LL1433:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1436
	 nop
.LL1434:
	cmp	%o4, 0
	bg	%icc, .LL1435
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1440
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1435
	 nop
	cmp	%o5, 0
.LL1440:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1437
	 nop
.LL1435:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1436:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1437:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE245:
	.size	Divide__Divide_var_I_u64, .-Divide__Divide_var_I_u64
	.align 4
	.type	Divide__uDivide_param_I_u64, #function
	.proc	05
Divide__uDivide_param_I_u64:
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
	mov	%o4, %o0
	mov	%o5, %o1
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE246:
	.size	Divide__uDivide_param_I_u64, .-Divide__uDivide_param_I_u64
	.align 4
	.type	Divide__Divide_param_I_u64, #function
	.proc	05
Divide__Divide_param_I_u64:
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
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL1444
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL1448
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL1448:
	cmp	%o4, 0
	bl	%icc, .LL1443
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1449
	 nop
	cmp	%o5, 0
.LL1449:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1447
	 nop
.LL1443:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1446
	 nop
.LL1444:
	cmp	%o4, 0
	bg	%icc, .LL1445
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1450
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1445
	 nop
	cmp	%o5, 0
.LL1450:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1447
	 nop
.LL1445:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1446:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1447:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE247:
	.size	Divide__Divide_param_I_u64, .-Divide__Divide_param_I_u64
	.align 4
	.type	Divide__uDivide_var_I_LC, #function
	.proc	05
Divide__uDivide_var_I_LC:
.LLFB248:
	save	%sp, -96, %sp
.LLCFI248:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE248:
	.size	Divide__uDivide_var_I_LC, .-Divide__uDivide_var_I_LC
	.align 4
	.type	Divide__Divide_var_I_LC, #function
	.proc	05
Divide__Divide_var_I_LC:
.LLFB249:
	save	%sp, -96, %sp
.LLCFI249:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o2
	cmp	%o2, 0
	bl	%icc, .LL1454
	 nop
	cmp	%o2, 0
	bne	%icc, .LL1458
	 nop
	cmp	%o3, 0
.LL1458:
	cmp	%o4, 0
	bl	%icc, .LL1453
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1459
	 nop
	cmp	%o5, 0
.LL1459:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1457
	 nop
.LL1453:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1456
	 nop
.LL1454:
	cmp	%o4, 0
	bg	%icc, .LL1455
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1460
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1455
	 nop
	cmp	%o5, 0
.LL1460:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1457
	 nop
.LL1455:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1456:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1457:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE249:
	.size	Divide__Divide_var_I_LC, .-Divide__Divide_var_I_LC
	.align 4
	.type	Divide__uDivide_param_I_LC, #function
	.proc	05
Divide__uDivide_param_I_LC:
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
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE250:
	.size	Divide__uDivide_param_I_LC, .-Divide__uDivide_param_I_LC
	.align 4
	.type	Divide__Divide_param_I_LC, #function
	.proc	05
Divide__Divide_param_I_LC:
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
	cmp	%o2, 0
	bl	%icc, .LL1464
	 nop
	cmp	%o2, 0
	bne	%icc, .LL1468
	 nop
	cmp	%o3, 0
.LL1468:
	cmp	%o4, 0
	bl	%icc, .LL1463
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1469
	 nop
	cmp	%o5, 0
.LL1469:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1467
	 nop
.LL1463:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1466
	 nop
.LL1464:
	cmp	%o4, 0
	bg	%icc, .LL1465
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1470
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1465
	 nop
	cmp	%o5, 0
.LL1470:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1467
	 nop
.LL1465:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1466:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1467:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE251:
	.size	Divide__Divide_param_I_LC, .-Divide__Divide_param_I_LC
	.align 4
	.type	Divide__uDivide_var_I_i32, #function
	.proc	04
Divide__uDivide_var_I_i32:
.LLFB252:
	save	%sp, -96, %sp
.LLCFI252:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE252:
	.size	Divide__uDivide_var_I_i32, .-Divide__uDivide_var_I_i32
	.align 4
	.type	Divide__Divide_var_I_i32, #function
	.proc	04
Divide__Divide_var_I_i32:
.LLFB253:
	save	%sp, -96, %sp
.LLCFI253:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+80], %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL1474
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1473
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1477
	 nop
.LL1473:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1476
	 nop
.LL1474:
	cmp	%g1, 0
	bg	%icc, .LL1475
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1477
	 nop
.LL1475:
	add	%g1, -1, %g1
.LL1476:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1477:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE253:
	.size	Divide__Divide_var_I_i32, .-Divide__Divide_var_I_i32
	.align 4
	.type	Divide__uDivide_param_I_i32, #function
	.proc	04
Divide__uDivide_param_I_i32:
.LLFB254:
	save	%sp, -96, %sp
.LLCFI254:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o4
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE254:
	.size	Divide__uDivide_param_I_i32, .-Divide__uDivide_param_I_i32
	.align 4
	.type	Divide__Divide_param_I_i32, #function
	.proc	04
Divide__Divide_param_I_i32:
.LLFB255:
	save	%sp, -96, %sp
.LLCFI255:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL1481
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1480
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1484
	 nop
.LL1480:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1483
	 nop
.LL1481:
	cmp	%g1, 0
	bg	%icc, .LL1482
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1484
	 nop
.LL1482:
	add	%g1, -1, %g1
.LL1483:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1484:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE255:
	.size	Divide__Divide_param_I_i32, .-Divide__Divide_param_I_i32
	.align 4
	.type	Divide__uDivide_var_I_i16, #function
	.proc	04
Divide__uDivide_var_I_i16:
.LLFB256:
	save	%sp, -96, %sp
.LLCFI256:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE256:
	.size	Divide__uDivide_var_I_i16, .-Divide__uDivide_var_I_i16
	.align 4
	.type	Divide__Divide_var_I_i16, #function
	.proc	04
Divide__Divide_var_I_i16:
.LLFB257:
	save	%sp, -96, %sp
.LLCFI257:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+80], %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+76], %o5
	sra	%o5, 16, %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL1488
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1487
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1491
	 nop
.LL1487:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1490
	 nop
.LL1488:
	cmp	%g1, 0
	bg	%icc, .LL1489
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1491
	 nop
.LL1489:
	add	%g1, -1, %g1
.LL1490:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1491:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE257:
	.size	Divide__Divide_var_I_i16, .-Divide__Divide_var_I_i16
	.align 4
	.type	Divide__uDivide_param_I_i16, #function
	.proc	04
Divide__uDivide_param_I_i16:
.LLFB258:
	save	%sp, -96, %sp
.LLCFI258:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ld	[%fp+68], %o4
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE258:
	.size	Divide__uDivide_param_I_i16, .-Divide__uDivide_param_I_i16
	.align 4
	.type	Divide__Divide_param_I_i16, #function
	.proc	04
Divide__Divide_param_I_i16:
.LLFB259:
	save	%sp, -96, %sp
.LLCFI259:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+68], %g1
	cmp	%o5, 0
	bl	%icc, .LL1495
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1494
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1498
	 nop
.LL1494:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1497
	 nop
.LL1495:
	cmp	%g1, 0
	bg	%icc, .LL1496
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1498
	 nop
.LL1496:
	add	%g1, -1, %g1
.LL1497:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1498:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE259:
	.size	Divide__Divide_param_I_i16, .-Divide__Divide_param_I_i16
	.align 4
	.type	Divide__uDivide_var_I_I, #function
	.proc	04
Divide__uDivide_var_I_I:
.LLFB260:
	save	%sp, -96, %sp
.LLCFI260:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE260:
	.size	Divide__uDivide_var_I_I, .-Divide__uDivide_var_I_I
	.align 4
	.type	Divide__Divide_var_I_I, #function
	.proc	04
Divide__Divide_var_I_I:
.LLFB261:
	save	%sp, -96, %sp
.LLCFI261:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+80], %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+80], %o5
	cmp	%o5, 0
	bl	%icc, .LL1502
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1501
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1505
	 nop
.LL1501:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1504
	 nop
.LL1502:
	cmp	%g1, 0
	bg	%icc, .LL1503
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1505
	 nop
.LL1503:
	add	%g1, -1, %g1
.LL1504:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1505:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE261:
	.size	Divide__Divide_var_I_I, .-Divide__Divide_var_I_I
	.align 4
	.type	Divide__uDivide_param_I_I, #function
	.proc	04
Divide__uDivide_param_I_I:
.LLFB262:
	save	%sp, -96, %sp
.LLCFI262:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o4
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE262:
	.size	Divide__uDivide_param_I_I, .-Divide__uDivide_param_I_I
	.align 4
	.type	Divide__Divide_param_I_I, #function
	.proc	04
Divide__Divide_param_I_I:
.LLFB263:
	save	%sp, -96, %sp
.LLCFI263:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL1509
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1508
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1512
	 nop
.LL1508:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1511
	 nop
.LL1509:
	cmp	%g1, 0
	bg	%icc, .LL1510
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1512
	 nop
.LL1510:
	add	%g1, -1, %g1
.LL1511:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1512:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE263:
	.size	Divide__Divide_param_I_I, .-Divide__Divide_param_I_I
	.align 4
	.type	Divide__uDivide_var_I_i64, #function
	.proc	05
Divide__uDivide_var_I_i64:
.LLFB264:
	save	%sp, -96, %sp
.LLCFI264:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE264:
	.size	Divide__uDivide_var_I_i64, .-Divide__uDivide_var_I_i64
	.align 4
	.type	Divide__Divide_var_I_i64, #function
	.proc	05
Divide__Divide_var_I_i64:
.LLFB265:
	save	%sp, -96, %sp
.LLCFI265:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o2
	cmp	%o2, 0
	bl	%icc, .LL1516
	 nop
	cmp	%o2, 0
	bne	%icc, .LL1520
	 nop
	cmp	%o3, 0
.LL1520:
	cmp	%o4, 0
	bl	%icc, .LL1515
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1521
	 nop
	cmp	%o5, 0
.LL1521:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1519
	 nop
.LL1515:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1518
	 nop
.LL1516:
	cmp	%o4, 0
	bg	%icc, .LL1517
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1522
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1517
	 nop
	cmp	%o5, 0
.LL1522:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1519
	 nop
.LL1517:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1518:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1519:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE265:
	.size	Divide__Divide_var_I_i64, .-Divide__Divide_var_I_i64
	.align 4
	.type	Divide__uDivide_param_I_i64, #function
	.proc	05
Divide__uDivide_param_I_i64:
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
	mov	%o4, %o0
	mov	%o5, %o1
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE266:
	.size	Divide__uDivide_param_I_i64, .-Divide__uDivide_param_I_i64
	.align 4
	.type	Divide__Divide_param_I_i64, #function
	.proc	05
Divide__Divide_param_I_i64:
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
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL1526
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL1530
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL1530:
	cmp	%o4, 0
	bl	%icc, .LL1525
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1531
	 nop
	cmp	%o5, 0
.LL1531:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1529
	 nop
.LL1525:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1528
	 nop
.LL1526:
	cmp	%o4, 0
	bg	%icc, .LL1527
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1532
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1527
	 nop
	cmp	%o5, 0
.LL1532:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1529
	 nop
.LL1527:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1528:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1529:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE267:
	.size	Divide__Divide_param_I_i64, .-Divide__Divide_param_I_i64
	.align 4
	.type	Divide__uDivide_var_I_C, #function
	.proc	04
Divide__uDivide_var_I_C:
.LLFB268:
	save	%sp, -96, %sp
.LLCFI268:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE268:
	.size	Divide__uDivide_var_I_C, .-Divide__uDivide_var_I_C
	.align 4
	.type	Divide__Divide_var_I_C, #function
	.proc	04
Divide__Divide_var_I_C:
.LLFB269:
	save	%sp, -96, %sp
.LLCFI269:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+80], %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+96], %o5
	cmp	%o5, 0
	bl	%icc, .LL1536
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1535
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1539
	 nop
.LL1535:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1538
	 nop
.LL1536:
	cmp	%g1, 0
	bg	%icc, .LL1537
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1539
	 nop
.LL1537:
	add	%g1, -1, %g1
.LL1538:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1539:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE269:
	.size	Divide__Divide_var_I_C, .-Divide__Divide_var_I_C
	.align 4
	.type	Divide__uDivide_param_I_C, #function
	.proc	04
Divide__uDivide_param_I_C:
.LLFB270:
	save	%sp, -96, %sp
.LLCFI270:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	wr	%g0, 0, %y
	ld	[%fp+72], %g1
	udiv	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE270:
	.size	Divide__uDivide_param_I_C, .-Divide__uDivide_param_I_C
	.align 4
	.type	Divide__Divide_param_I_C, #function
	.proc	04
Divide__Divide_param_I_C:
.LLFB271:
	save	%sp, -96, %sp
.LLCFI271:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %g1
	cmp	%o5, 0
	bl	%icc, .LL1543
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1542
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1546
	 nop
.LL1542:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1545
	 nop
.LL1543:
	cmp	%g1, 0
	bg	%icc, .LL1544
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1546
	 nop
.LL1544:
	add	%g1, -1, %g1
.LL1545:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1546:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE271:
	.size	Divide__Divide_param_I_C, .-Divide__Divide_param_I_C
	.align 4
	.type	Divide__uDivide_var_I_u8, #function
	.proc	04
Divide__uDivide_var_I_u8:
.LLFB272:
	save	%sp, -96, %sp
.LLCFI272:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE272:
	.size	Divide__uDivide_var_I_u8, .-Divide__uDivide_var_I_u8
	.align 4
	.type	Divide__Divide_var_I_u8, #function
	.proc	04
Divide__Divide_var_I_u8:
.LLFB273:
	save	%sp, -96, %sp
.LLCFI273:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+80], %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+112], %o5
	srl	%o5, 24, %o5
	and	%o5, 0xff, %o5
	cmp	%o5, 0
	bl	%icc, .LL1550
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1549
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1553
	 nop
.LL1549:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1552
	 nop
.LL1550:
	cmp	%g1, 0
	bg	%icc, .LL1551
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1553
	 nop
.LL1551:
	add	%g1, -1, %g1
.LL1552:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1553:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE273:
	.size	Divide__Divide_var_I_u8, .-Divide__Divide_var_I_u8
	.align 4
	.type	Divide__uDivide_param_I_u8, #function
	.proc	04
Divide__uDivide_param_I_u8:
.LLFB274:
	save	%sp, -96, %sp
.LLCFI274:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ld	[%fp+68], %o4
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE274:
	.size	Divide__uDivide_param_I_u8, .-Divide__uDivide_param_I_u8
	.align 4
	.type	Divide__Divide_param_I_u8, #function
	.proc	04
Divide__Divide_param_I_u8:
.LLFB275:
	save	%sp, -96, %sp
.LLCFI275:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+68], %g1
	cmp	%o5, 0
	bl	%icc, .LL1557
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1556
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1560
	 nop
.LL1556:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1559
	 nop
.LL1557:
	cmp	%g1, 0
	bg	%icc, .LL1558
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1560
	 nop
.LL1558:
	add	%g1, -1, %g1
.LL1559:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1560:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE275:
	.size	Divide__Divide_param_I_u8, .-Divide__Divide_param_I_u8
	.align 4
	.type	Divide__uDivide_var_I_L, #function
	.proc	05
Divide__uDivide_var_I_L:
.LLFB276:
	save	%sp, -96, %sp
.LLCFI276:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE276:
	.size	Divide__uDivide_var_I_L, .-Divide__uDivide_var_I_L
	.align 4
	.type	Divide__Divide_var_I_L, #function
	.proc	05
Divide__Divide_var_I_L:
.LLFB277:
	save	%sp, -96, %sp
.LLCFI277:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o2
	cmp	%o2, 0
	bl	%icc, .LL1564
	 nop
	cmp	%o2, 0
	bne	%icc, .LL1568
	 nop
	cmp	%o3, 0
.LL1568:
	cmp	%o4, 0
	bl	%icc, .LL1563
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1569
	 nop
	cmp	%o5, 0
.LL1569:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1567
	 nop
.LL1563:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1566
	 nop
.LL1564:
	cmp	%o4, 0
	bg	%icc, .LL1565
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1570
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1565
	 nop
	cmp	%o5, 0
.LL1570:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1567
	 nop
.LL1565:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1566:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1567:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE277:
	.size	Divide__Divide_var_I_L, .-Divide__Divide_var_I_L
	.align 4
	.type	Divide__uDivide_param_I_L, #function
	.proc	05
Divide__uDivide_param_I_L:
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
	mov	%o4, %o0
	mov	%o5, %o1
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE278:
	.size	Divide__uDivide_param_I_L, .-Divide__uDivide_param_I_L
	.align 4
	.type	Divide__Divide_param_I_L, #function
	.proc	05
Divide__Divide_param_I_L:
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
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL1574
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL1578
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL1578:
	cmp	%o4, 0
	bl	%icc, .LL1573
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1579
	 nop
	cmp	%o5, 0
.LL1579:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1577
	 nop
.LL1573:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1576
	 nop
.LL1574:
	cmp	%o4, 0
	bg	%icc, .LL1575
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1580
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1575
	 nop
	cmp	%o5, 0
.LL1580:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1577
	 nop
.LL1575:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1576:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1577:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE279:
	.size	Divide__Divide_param_I_L, .-Divide__Divide_param_I_L
	.align 4
	.type	Divide__uDivide_var_I_i8, #function
	.proc	04
Divide__uDivide_var_I_i8:
.LLFB280:
	save	%sp, -96, %sp
.LLCFI280:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE280:
	.size	Divide__uDivide_var_I_i8, .-Divide__uDivide_var_I_i8
	.align 4
	.type	Divide__Divide_var_I_i8, #function
	.proc	04
Divide__Divide_var_I_i8:
.LLFB281:
	save	%sp, -96, %sp
.LLCFI281:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+80], %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+128], %o5
	sra	%o5, 24, %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	cmp	%o5, 0
	bl	%icc, .LL1584
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1583
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1587
	 nop
.LL1583:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1586
	 nop
.LL1584:
	cmp	%g1, 0
	bg	%icc, .LL1585
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1587
	 nop
.LL1585:
	add	%g1, -1, %g1
.LL1586:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1587:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE281:
	.size	Divide__Divide_var_I_i8, .-Divide__Divide_var_I_i8
	.align 4
	.type	Divide__uDivide_param_I_i8, #function
	.proc	04
Divide__uDivide_param_I_i8:
.LLFB282:
	save	%sp, -96, %sp
.LLCFI282:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ld	[%fp+68], %o4
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE282:
	.size	Divide__uDivide_param_I_i8, .-Divide__uDivide_param_I_i8
	.align 4
	.type	Divide__Divide_param_I_i8, #function
	.proc	04
Divide__Divide_param_I_i8:
.LLFB283:
	save	%sp, -96, %sp
.LLCFI283:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+68], %g1
	cmp	%o5, 0
	bl	%icc, .LL1591
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1590
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1594
	 nop
.LL1590:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1593
	 nop
.LL1591:
	cmp	%g1, 0
	bg	%icc, .LL1592
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1594
	 nop
.LL1592:
	add	%g1, -1, %g1
.LL1593:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1594:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE283:
	.size	Divide__Divide_param_I_i8, .-Divide__Divide_param_I_i8
	.align 4
	.type	Divide__uDivide_var_I_u32, #function
	.proc	04
Divide__uDivide_var_I_u32:
.LLFB284:
	save	%sp, -96, %sp
.LLCFI284:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE284:
	.size	Divide__uDivide_var_I_u32, .-Divide__uDivide_var_I_u32
	.align 4
	.type	Divide__Divide_var_I_u32, #function
	.proc	04
Divide__Divide_var_I_u32:
.LLFB285:
	save	%sp, -96, %sp
.LLCFI285:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+80], %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+132], %o5
	cmp	%o5, 0
	bl	%icc, .LL1598
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1597
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1601
	 nop
.LL1597:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1600
	 nop
.LL1598:
	cmp	%g1, 0
	bg	%icc, .LL1599
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1601
	 nop
.LL1599:
	add	%g1, -1, %g1
.LL1600:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1601:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE285:
	.size	Divide__Divide_var_I_u32, .-Divide__Divide_var_I_u32
	.align 4
	.type	Divide__uDivide_param_I_u32, #function
	.proc	04
Divide__uDivide_param_I_u32:
.LLFB286:
	save	%sp, -96, %sp
.LLCFI286:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o4
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE286:
	.size	Divide__uDivide_param_I_u32, .-Divide__uDivide_param_I_u32
	.align 4
	.type	Divide__Divide_param_I_u32, #function
	.proc	04
Divide__Divide_param_I_u32:
.LLFB287:
	save	%sp, -96, %sp
.LLCFI287:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL1605
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1604
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1608
	 nop
.LL1604:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1607
	 nop
.LL1605:
	cmp	%g1, 0
	bg	%icc, .LL1606
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1608
	 nop
.LL1606:
	add	%g1, -1, %g1
.LL1607:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1608:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE287:
	.size	Divide__Divide_param_I_u32, .-Divide__Divide_param_I_u32
	.align 4
	.type	Divide__uDivide_var_i64_u16, #function
	.proc	05
Divide__uDivide_var_i64_u16:
.LLFB288:
	save	%sp, -96, %sp
.LLCFI288:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE288:
	.size	Divide__uDivide_var_i64_u16, .-Divide__uDivide_var_i64_u16
	.align 4
	.type	Divide__Divide_var_i64_u16, #function
	.proc	05
Divide__Divide_var_i64_u16:
.LLFB289:
	save	%sp, -96, %sp
.LLCFI289:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	cmp	%o2, 0
	bl	%icc, .LL1612
	 nop
	cmp	%o2, 0
	bne	%icc, .LL1616
	 nop
	cmp	%o3, 0
.LL1616:
	cmp	%o4, 0
	bl	%icc, .LL1611
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1617
	 nop
	cmp	%o5, 0
.LL1617:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1615
	 nop
.LL1611:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1614
	 nop
.LL1612:
	cmp	%o4, 0
	bg	%icc, .LL1613
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1618
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1613
	 nop
	cmp	%o5, 0
.LL1618:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1615
	 nop
.LL1613:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1614:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1615:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE289:
	.size	Divide__Divide_var_i64_u16, .-Divide__Divide_var_i64_u16
	.align 4
	.type	Divide__uDivide_param_i64_u16, #function
	.proc	05
Divide__uDivide_param_i64_u16:
.LLFB290:
	save	%sp, -104, %sp
.LLCFI290:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE290:
	.size	Divide__uDivide_param_i64_u16, .-Divide__uDivide_param_i64_u16
	.align 4
	.type	Divide__Divide_param_i64_u16, #function
	.proc	05
Divide__Divide_param_i64_u16:
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
	cmp	%o2, 0
	bl	%icc, .LL1622
	 nop
	cmp	%o2, 0
	bne	%icc, .LL1626
	 nop
	cmp	%o3, 0
.LL1626:
	cmp	%o4, 0
	bl	%icc, .LL1621
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1627
	 nop
	cmp	%o5, 0
.LL1627:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1625
	 nop
.LL1621:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1624
	 nop
.LL1622:
	cmp	%o4, 0
	bg	%icc, .LL1623
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1628
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1623
	 nop
	cmp	%o5, 0
.LL1628:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1625
	 nop
.LL1623:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1624:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1625:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE291:
	.size	Divide__Divide_param_i64_u16, .-Divide__Divide_param_i64_u16
	.align 4
	.type	Divide__uDivide_var_i64_u64, #function
	.proc	05
Divide__uDivide_var_i64_u64:
.LLFB292:
	save	%sp, -96, %sp
.LLCFI292:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE292:
	.size	Divide__uDivide_var_i64_u64, .-Divide__uDivide_var_i64_u64
	.align 4
	.type	Divide__Divide_var_i64_u64, #function
	.proc	05
Divide__Divide_var_i64_u64:
.LLFB293:
	save	%sp, -96, %sp
.LLCFI293:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o2
	cmp	%o2, 0
	bl	%icc, .LL1632
	 nop
	cmp	%o2, 0
	bne	%icc, .LL1636
	 nop
	cmp	%o3, 0
.LL1636:
	cmp	%o4, 0
	bl	%icc, .LL1631
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1637
	 nop
	cmp	%o5, 0
.LL1637:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1635
	 nop
.LL1631:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1634
	 nop
.LL1632:
	cmp	%o4, 0
	bg	%icc, .LL1633
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1638
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1633
	 nop
	cmp	%o5, 0
.LL1638:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1635
	 nop
.LL1633:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1634:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1635:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE293:
	.size	Divide__Divide_var_i64_u64, .-Divide__Divide_var_i64_u64
	.align 4
	.type	Divide__uDivide_param_i64_u64, #function
	.proc	05
Divide__uDivide_param_i64_u64:
.LLFB294:
	save	%sp, -112, %sp
.LLCFI294:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE294:
	.size	Divide__uDivide_param_i64_u64, .-Divide__uDivide_param_i64_u64
	.align 4
	.type	Divide__Divide_param_i64_u64, #function
	.proc	05
Divide__Divide_param_i64_u64:
.LLFB295:
	save	%sp, -112, %sp
.LLCFI295:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bl	%icc, .LL1642
	 nop
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bne	%icc, .LL1646
	 nop
	ld	[%fp-12], %g1
	cmp	%g1, 0
.LL1646:
	cmp	%o4, 0
	bl	%icc, .LL1641
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1647
	 nop
	cmp	%o5, 0
.LL1647:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1645
	 nop
.LL1641:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1644
	 nop
.LL1642:
	cmp	%o4, 0
	bg	%icc, .LL1643
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1648
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1643
	 nop
	cmp	%o5, 0
.LL1648:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1645
	 nop
.LL1643:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1644:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1645:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE295:
	.size	Divide__Divide_param_i64_u64, .-Divide__Divide_param_i64_u64
	.align 4
	.type	Divide__uDivide_var_i64_LC, #function
	.proc	05
Divide__uDivide_var_i64_LC:
.LLFB296:
	save	%sp, -96, %sp
.LLCFI296:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE296:
	.size	Divide__uDivide_var_i64_LC, .-Divide__uDivide_var_i64_LC
	.align 4
	.type	Divide__Divide_var_i64_LC, #function
	.proc	05
Divide__Divide_var_i64_LC:
.LLFB297:
	save	%sp, -96, %sp
.LLCFI297:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o2
	cmp	%o2, 0
	bl	%icc, .LL1652
	 nop
	cmp	%o2, 0
	bne	%icc, .LL1656
	 nop
	cmp	%o3, 0
.LL1656:
	cmp	%o4, 0
	bl	%icc, .LL1651
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1657
	 nop
	cmp	%o5, 0
.LL1657:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1655
	 nop
.LL1651:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1654
	 nop
.LL1652:
	cmp	%o4, 0
	bg	%icc, .LL1653
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1658
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1653
	 nop
	cmp	%o5, 0
.LL1658:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1655
	 nop
.LL1653:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1654:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1655:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE297:
	.size	Divide__Divide_var_i64_LC, .-Divide__Divide_var_i64_LC
	.align 4
	.type	Divide__uDivide_param_i64_LC, #function
	.proc	05
Divide__uDivide_param_i64_LC:
.LLFB298:
	save	%sp, -112, %sp
.LLCFI298:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE298:
	.size	Divide__uDivide_param_i64_LC, .-Divide__uDivide_param_i64_LC
	.align 4
	.type	Divide__Divide_param_i64_LC, #function
	.proc	05
Divide__Divide_param_i64_LC:
.LLFB299:
	save	%sp, -112, %sp
.LLCFI299:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o2
	ldd	[%fp-8], %o4
	cmp	%o2, 0
	bl	%icc, .LL1662
	 nop
	cmp	%o2, 0
	bne	%icc, .LL1666
	 nop
	cmp	%o3, 0
.LL1666:
	cmp	%o4, 0
	bl	%icc, .LL1661
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1667
	 nop
	cmp	%o5, 0
.LL1667:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1665
	 nop
.LL1661:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1664
	 nop
.LL1662:
	cmp	%o4, 0
	bg	%icc, .LL1663
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1668
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1663
	 nop
	cmp	%o5, 0
.LL1668:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1665
	 nop
.LL1663:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1664:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1665:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE299:
	.size	Divide__Divide_param_i64_LC, .-Divide__Divide_param_i64_LC
	.align 4
	.type	Divide__uDivide_var_i64_i32, #function
	.proc	05
Divide__uDivide_var_i64_i32:
.LLFB300:
	save	%sp, -96, %sp
.LLCFI300:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o2
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE300:
	.size	Divide__uDivide_var_i64_i32, .-Divide__uDivide_var_i64_i32
	.align 4
	.type	Divide__Divide_var_i64_i32, #function
	.proc	05
Divide__Divide_var_i64_i32:
.LLFB301:
	save	%sp, -96, %sp
.LLCFI301:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o3
	sra	%g1, 31, %g1
	mov	%g1, %o2
	cmp	%o2, 0
	bl	%icc, .LL1672
	 nop
	cmp	%o2, 0
	bne	%icc, .LL1676
	 nop
	cmp	%o3, 0
.LL1676:
	cmp	%o4, 0
	bl	%icc, .LL1671
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1677
	 nop
	cmp	%o5, 0
.LL1677:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1675
	 nop
.LL1671:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1674
	 nop
.LL1672:
	cmp	%o4, 0
	bg	%icc, .LL1673
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1678
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1673
	 nop
	cmp	%o5, 0
.LL1678:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1675
	 nop
.LL1673:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1674:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1675:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE301:
	.size	Divide__Divide_var_i64_i32, .-Divide__Divide_var_i64_i32
	.align 4
	.type	Divide__uDivide_param_i64_i32, #function
	.proc	05
Divide__uDivide_param_i64_i32:
.LLFB302:
	save	%sp, -104, %sp
.LLCFI302:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE302:
	.size	Divide__uDivide_param_i64_i32, .-Divide__uDivide_param_i64_i32
	.align 4
	.type	Divide__Divide_param_i64_i32, #function
	.proc	05
Divide__Divide_param_i64_i32:
.LLFB303:
	save	%sp, -104, %sp
.LLCFI303:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o3
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o2
	ldd	[%fp-8], %o4
	cmp	%o2, 0
	bl	%icc, .LL1682
	 nop
	cmp	%o2, 0
	bne	%icc, .LL1686
	 nop
	cmp	%o3, 0
.LL1686:
	cmp	%o4, 0
	bl	%icc, .LL1681
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1687
	 nop
	cmp	%o5, 0
.LL1687:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1685
	 nop
.LL1681:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1684
	 nop
.LL1682:
	cmp	%o4, 0
	bg	%icc, .LL1683
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1688
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1683
	 nop
	cmp	%o5, 0
.LL1688:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1685
	 nop
.LL1683:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1684:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1685:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE303:
	.size	Divide__Divide_param_i64_i32, .-Divide__Divide_param_i64_i32
	.align 4
	.type	Divide__uDivide_var_i64_i16, #function
	.proc	05
Divide__uDivide_var_i64_i16:
.LLFB304:
	save	%sp, -96, %sp
.LLCFI304:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o2
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE304:
	.size	Divide__uDivide_var_i64_i16, .-Divide__uDivide_var_i64_i16
	.align 4
	.type	Divide__Divide_var_i64_i16, #function
	.proc	05
Divide__Divide_var_i64_i16:
.LLFB305:
	save	%sp, -96, %sp
.LLCFI305:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o3
	sra	%g1, 31, %g1
	mov	%g1, %o2
	cmp	%o2, 0
	bl	%icc, .LL1692
	 nop
	cmp	%o2, 0
	bne	%icc, .LL1696
	 nop
	cmp	%o3, 0
.LL1696:
	cmp	%o4, 0
	bl	%icc, .LL1691
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1697
	 nop
	cmp	%o5, 0
.LL1697:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1695
	 nop
.LL1691:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1694
	 nop
.LL1692:
	cmp	%o4, 0
	bg	%icc, .LL1693
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1698
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1693
	 nop
	cmp	%o5, 0
.LL1698:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1695
	 nop
.LL1693:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1694:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1695:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE305:
	.size	Divide__Divide_var_i64_i16, .-Divide__Divide_var_i64_i16
	.align 4
	.type	Divide__uDivide_param_i64_i16, #function
	.proc	05
Divide__uDivide_param_i64_i16:
.LLFB306:
	save	%sp, -104, %sp
.LLCFI306:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE306:
	.size	Divide__uDivide_param_i64_i16, .-Divide__uDivide_param_i64_i16
	.align 4
	.type	Divide__Divide_param_i64_i16, #function
	.proc	05
Divide__Divide_param_i64_i16:
.LLFB307:
	save	%sp, -104, %sp
.LLCFI307:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o3
	sra	%g1, 31, %g1
	mov	%g1, %o2
	ldd	[%fp-8], %o4
	cmp	%o2, 0
	bl	%icc, .LL1702
	 nop
	cmp	%o2, 0
	bne	%icc, .LL1706
	 nop
	cmp	%o3, 0
.LL1706:
	cmp	%o4, 0
	bl	%icc, .LL1701
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1707
	 nop
	cmp	%o5, 0
.LL1707:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1705
	 nop
.LL1701:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1704
	 nop
.LL1702:
	cmp	%o4, 0
	bg	%icc, .LL1703
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1708
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1703
	 nop
	cmp	%o5, 0
.LL1708:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1705
	 nop
.LL1703:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1704:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1705:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE307:
	.size	Divide__Divide_param_i64_i16, .-Divide__Divide_param_i64_i16
	.align 4
	.type	Divide__uDivide_var_i64_I, #function
	.proc	05
Divide__uDivide_var_i64_I:
.LLFB308:
	save	%sp, -96, %sp
.LLCFI308:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o2
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE308:
	.size	Divide__uDivide_var_i64_I, .-Divide__uDivide_var_i64_I
	.align 4
	.type	Divide__Divide_var_i64_I, #function
	.proc	05
Divide__Divide_var_i64_I:
.LLFB309:
	save	%sp, -96, %sp
.LLCFI309:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o3
	sra	%g1, 31, %g1
	mov	%g1, %o2
	cmp	%o2, 0
	bl	%icc, .LL1712
	 nop
	cmp	%o2, 0
	bne	%icc, .LL1716
	 nop
	cmp	%o3, 0
.LL1716:
	cmp	%o4, 0
	bl	%icc, .LL1711
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1717
	 nop
	cmp	%o5, 0
.LL1717:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1715
	 nop
.LL1711:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1714
	 nop
.LL1712:
	cmp	%o4, 0
	bg	%icc, .LL1713
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1718
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1713
	 nop
	cmp	%o5, 0
.LL1718:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1715
	 nop
.LL1713:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1714:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1715:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE309:
	.size	Divide__Divide_var_i64_I, .-Divide__Divide_var_i64_I
	.align 4
	.type	Divide__uDivide_param_i64_I, #function
	.proc	05
Divide__uDivide_param_i64_I:
.LLFB310:
	save	%sp, -104, %sp
.LLCFI310:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE310:
	.size	Divide__uDivide_param_i64_I, .-Divide__uDivide_param_i64_I
	.align 4
	.type	Divide__Divide_param_i64_I, #function
	.proc	05
Divide__Divide_param_i64_I:
.LLFB311:
	save	%sp, -104, %sp
.LLCFI311:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o3
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o2
	ldd	[%fp-8], %o4
	cmp	%o2, 0
	bl	%icc, .LL1722
	 nop
	cmp	%o2, 0
	bne	%icc, .LL1726
	 nop
	cmp	%o3, 0
.LL1726:
	cmp	%o4, 0
	bl	%icc, .LL1721
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1727
	 nop
	cmp	%o5, 0
.LL1727:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1725
	 nop
.LL1721:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1724
	 nop
.LL1722:
	cmp	%o4, 0
	bg	%icc, .LL1723
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1728
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1723
	 nop
	cmp	%o5, 0
.LL1728:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1725
	 nop
.LL1723:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1724:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1725:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE311:
	.size	Divide__Divide_param_i64_I, .-Divide__Divide_param_i64_I
	.align 4
	.type	Divide__uDivide_var_i64_i64, #function
	.proc	05
Divide__uDivide_var_i64_i64:
.LLFB312:
	save	%sp, -96, %sp
.LLCFI312:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE312:
	.size	Divide__uDivide_var_i64_i64, .-Divide__uDivide_var_i64_i64
	.align 4
	.type	Divide__Divide_var_i64_i64, #function
	.proc	05
Divide__Divide_var_i64_i64:
.LLFB313:
	save	%sp, -96, %sp
.LLCFI313:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o2
	cmp	%o2, 0
	bl	%icc, .LL1732
	 nop
	cmp	%o2, 0
	bne	%icc, .LL1736
	 nop
	cmp	%o3, 0
.LL1736:
	cmp	%o4, 0
	bl	%icc, .LL1731
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1737
	 nop
	cmp	%o5, 0
.LL1737:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1735
	 nop
.LL1731:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1734
	 nop
.LL1732:
	cmp	%o4, 0
	bg	%icc, .LL1733
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1738
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1733
	 nop
	cmp	%o5, 0
.LL1738:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1735
	 nop
.LL1733:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1734:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1735:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE313:
	.size	Divide__Divide_var_i64_i64, .-Divide__Divide_var_i64_i64
	.align 4
	.type	Divide__uDivide_param_i64_i64, #function
	.proc	05
Divide__uDivide_param_i64_i64:
.LLFB314:
	save	%sp, -112, %sp
.LLCFI314:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE314:
	.size	Divide__uDivide_param_i64_i64, .-Divide__uDivide_param_i64_i64
	.align 4
	.type	Divide__Divide_param_i64_i64, #function
	.proc	05
Divide__Divide_param_i64_i64:
.LLFB315:
	save	%sp, -112, %sp
.LLCFI315:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bl	%icc, .LL1742
	 nop
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bne	%icc, .LL1746
	 nop
	ld	[%fp-12], %g1
	cmp	%g1, 0
.LL1746:
	cmp	%o4, 0
	bl	%icc, .LL1741
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1747
	 nop
	cmp	%o5, 0
.LL1747:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1745
	 nop
.LL1741:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1744
	 nop
.LL1742:
	cmp	%o4, 0
	bg	%icc, .LL1743
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1748
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1743
	 nop
	cmp	%o5, 0
.LL1748:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1745
	 nop
.LL1743:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1744:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1745:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE315:
	.size	Divide__Divide_param_i64_i64, .-Divide__Divide_param_i64_i64
	.align 4
	.type	Divide__uDivide_var_i64_C, #function
	.proc	05
Divide__uDivide_var_i64_C:
.LLFB316:
	save	%sp, -96, %sp
.LLCFI316:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o2
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE316:
	.size	Divide__uDivide_var_i64_C, .-Divide__uDivide_var_i64_C
	.align 4
	.type	Divide__Divide_var_i64_C, #function
	.proc	05
Divide__Divide_var_i64_C:
.LLFB317:
	save	%sp, -96, %sp
.LLCFI317:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o3
	sra	%g1, 31, %g1
	mov	%g1, %o2
	cmp	%o2, 0
	bl	%icc, .LL1752
	 nop
	cmp	%o2, 0
	bne	%icc, .LL1756
	 nop
	cmp	%o3, 0
.LL1756:
	cmp	%o4, 0
	bl	%icc, .LL1751
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1757
	 nop
	cmp	%o5, 0
.LL1757:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1755
	 nop
.LL1751:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1754
	 nop
.LL1752:
	cmp	%o4, 0
	bg	%icc, .LL1753
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1758
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1753
	 nop
	cmp	%o5, 0
.LL1758:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1755
	 nop
.LL1753:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1754:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1755:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE317:
	.size	Divide__Divide_var_i64_C, .-Divide__Divide_var_i64_C
	.align 4
	.type	Divide__uDivide_param_i64_C, #function
	.proc	05
Divide__uDivide_param_i64_C:
.LLFB318:
	save	%sp, -104, %sp
.LLCFI318:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	ld	[%fp+76], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE318:
	.size	Divide__uDivide_param_i64_C, .-Divide__uDivide_param_i64_C
	.align 4
	.type	Divide__Divide_param_i64_C, #function
	.proc	05
Divide__Divide_param_i64_C:
.LLFB319:
	save	%sp, -104, %sp
.LLCFI319:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	mov	%g1, %o3
	sra	%g1, 31, %g1
	mov	%g1, %o2
	ldd	[%fp-8], %o4
	cmp	%o2, 0
	bl	%icc, .LL1762
	 nop
	cmp	%o2, 0
	bne	%icc, .LL1766
	 nop
	cmp	%o3, 0
.LL1766:
	cmp	%o4, 0
	bl	%icc, .LL1761
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1767
	 nop
	cmp	%o5, 0
.LL1767:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1765
	 nop
.LL1761:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1764
	 nop
.LL1762:
	cmp	%o4, 0
	bg	%icc, .LL1763
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1768
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1763
	 nop
	cmp	%o5, 0
.LL1768:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1765
	 nop
.LL1763:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1764:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1765:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE319:
	.size	Divide__Divide_param_i64_C, .-Divide__Divide_param_i64_C
	.align 4
	.type	Divide__uDivide_var_i64_u8, #function
	.proc	05
Divide__uDivide_var_i64_u8:
.LLFB320:
	save	%sp, -96, %sp
.LLCFI320:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE320:
	.size	Divide__uDivide_var_i64_u8, .-Divide__uDivide_var_i64_u8
	.align 4
	.type	Divide__Divide_var_i64_u8, #function
	.proc	05
Divide__Divide_var_i64_u8:
.LLFB321:
	save	%sp, -96, %sp
.LLCFI321:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	cmp	%o2, 0
	bl	%icc, .LL1772
	 nop
	cmp	%o2, 0
	bne	%icc, .LL1776
	 nop
	cmp	%o3, 0
.LL1776:
	cmp	%o4, 0
	bl	%icc, .LL1771
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1777
	 nop
	cmp	%o5, 0
.LL1777:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1775
	 nop
.LL1771:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1774
	 nop
.LL1772:
	cmp	%o4, 0
	bg	%icc, .LL1773
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1778
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1773
	 nop
	cmp	%o5, 0
.LL1778:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1775
	 nop
.LL1773:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1774:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1775:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE321:
	.size	Divide__Divide_var_i64_u8, .-Divide__Divide_var_i64_u8
	.align 4
	.type	Divide__uDivide_param_i64_u8, #function
	.proc	05
Divide__uDivide_param_i64_u8:
.LLFB322:
	save	%sp, -104, %sp
.LLCFI322:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	ldub	[%fp+76], %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE322:
	.size	Divide__uDivide_param_i64_u8, .-Divide__uDivide_param_i64_u8
	.align 4
	.type	Divide__Divide_param_i64_u8, #function
	.proc	05
Divide__Divide_param_i64_u8:
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
	cmp	%o2, 0
	bl	%icc, .LL1782
	 nop
	cmp	%o2, 0
	bne	%icc, .LL1786
	 nop
	cmp	%o3, 0
.LL1786:
	cmp	%o4, 0
	bl	%icc, .LL1781
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1787
	 nop
	cmp	%o5, 0
.LL1787:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1785
	 nop
.LL1781:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1784
	 nop
.LL1782:
	cmp	%o4, 0
	bg	%icc, .LL1783
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1788
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1783
	 nop
	cmp	%o5, 0
.LL1788:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1785
	 nop
.LL1783:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1784:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1785:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE323:
	.size	Divide__Divide_param_i64_u8, .-Divide__Divide_param_i64_u8
	.align 4
	.type	Divide__uDivide_var_i64_L, #function
	.proc	05
Divide__uDivide_var_i64_L:
.LLFB324:
	save	%sp, -96, %sp
.LLCFI324:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE324:
	.size	Divide__uDivide_var_i64_L, .-Divide__uDivide_var_i64_L
	.align 4
	.type	Divide__Divide_var_i64_L, #function
	.proc	05
Divide__Divide_var_i64_L:
.LLFB325:
	save	%sp, -96, %sp
.LLCFI325:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o2
	cmp	%o2, 0
	bl	%icc, .LL1792
	 nop
	cmp	%o2, 0
	bne	%icc, .LL1796
	 nop
	cmp	%o3, 0
.LL1796:
	cmp	%o4, 0
	bl	%icc, .LL1791
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1797
	 nop
	cmp	%o5, 0
.LL1797:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1795
	 nop
.LL1791:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1794
	 nop
.LL1792:
	cmp	%o4, 0
	bg	%icc, .LL1793
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1798
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1793
	 nop
	cmp	%o5, 0
.LL1798:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1795
	 nop
.LL1793:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1794:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1795:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE325:
	.size	Divide__Divide_var_i64_L, .-Divide__Divide_var_i64_L
	.align 4
	.type	Divide__uDivide_param_i64_L, #function
	.proc	05
Divide__uDivide_param_i64_L:
.LLFB326:
	save	%sp, -112, %sp
.LLCFI326:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE326:
	.size	Divide__uDivide_param_i64_L, .-Divide__uDivide_param_i64_L
	.align 4
	.type	Divide__Divide_param_i64_L, #function
	.proc	05
Divide__Divide_param_i64_L:
.LLFB327:
	save	%sp, -112, %sp
.LLCFI327:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bl	%icc, .LL1802
	 nop
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bne	%icc, .LL1806
	 nop
	ld	[%fp-12], %g1
	cmp	%g1, 0
.LL1806:
	cmp	%o4, 0
	bl	%icc, .LL1801
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1807
	 nop
	cmp	%o5, 0
.LL1807:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1805
	 nop
.LL1801:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1804
	 nop
.LL1802:
	cmp	%o4, 0
	bg	%icc, .LL1803
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1808
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1803
	 nop
	cmp	%o5, 0
.LL1808:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1805
	 nop
.LL1803:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1804:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1805:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE327:
	.size	Divide__Divide_param_i64_L, .-Divide__Divide_param_i64_L
	.align 4
	.type	Divide__uDivide_var_i64_i8, #function
	.proc	05
Divide__uDivide_var_i64_i8:
.LLFB328:
	save	%sp, -96, %sp
.LLCFI328:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o2
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE328:
	.size	Divide__uDivide_var_i64_i8, .-Divide__uDivide_var_i64_i8
	.align 4
	.type	Divide__Divide_var_i64_i8, #function
	.proc	05
Divide__Divide_var_i64_i8:
.LLFB329:
	save	%sp, -96, %sp
.LLCFI329:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o3
	sra	%g1, 31, %g1
	mov	%g1, %o2
	cmp	%o2, 0
	bl	%icc, .LL1812
	 nop
	cmp	%o2, 0
	bne	%icc, .LL1816
	 nop
	cmp	%o3, 0
.LL1816:
	cmp	%o4, 0
	bl	%icc, .LL1811
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1817
	 nop
	cmp	%o5, 0
.LL1817:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1815
	 nop
.LL1811:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1814
	 nop
.LL1812:
	cmp	%o4, 0
	bg	%icc, .LL1813
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1818
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1813
	 nop
	cmp	%o5, 0
.LL1818:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1815
	 nop
.LL1813:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1814:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1815:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE329:
	.size	Divide__Divide_var_i64_i8, .-Divide__Divide_var_i64_i8
	.align 4
	.type	Divide__uDivide_param_i64_i8, #function
	.proc	05
Divide__uDivide_param_i64_i8:
.LLFB330:
	save	%sp, -104, %sp
.LLCFI330:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	ldub	[%fp+76], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE330:
	.size	Divide__uDivide_param_i64_i8, .-Divide__uDivide_param_i64_i8
	.align 4
	.type	Divide__Divide_param_i64_i8, #function
	.proc	05
Divide__Divide_param_i64_i8:
.LLFB331:
	save	%sp, -104, %sp
.LLCFI331:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o3
	sra	%g1, 31, %g1
	mov	%g1, %o2
	ldd	[%fp-8], %o4
	cmp	%o2, 0
	bl	%icc, .LL1822
	 nop
	cmp	%o2, 0
	bne	%icc, .LL1826
	 nop
	cmp	%o3, 0
.LL1826:
	cmp	%o4, 0
	bl	%icc, .LL1821
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1827
	 nop
	cmp	%o5, 0
.LL1827:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1825
	 nop
.LL1821:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1824
	 nop
.LL1822:
	cmp	%o4, 0
	bg	%icc, .LL1823
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1828
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1823
	 nop
	cmp	%o5, 0
.LL1828:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1825
	 nop
.LL1823:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1824:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1825:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE331:
	.size	Divide__Divide_param_i64_i8, .-Divide__Divide_param_i64_i8
	.align 4
	.type	Divide__uDivide_var_i64_u32, #function
	.proc	05
Divide__uDivide_var_i64_u32:
.LLFB332:
	save	%sp, -96, %sp
.LLCFI332:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o2
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE332:
	.size	Divide__uDivide_var_i64_u32, .-Divide__uDivide_var_i64_u32
	.align 4
	.type	Divide__Divide_var_i64_u32, #function
	.proc	05
Divide__Divide_var_i64_u32:
.LLFB333:
	save	%sp, -96, %sp
.LLCFI333:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o3
	sra	%g1, 31, %g1
	mov	%g1, %o2
	cmp	%o2, 0
	bl	%icc, .LL1832
	 nop
	cmp	%o2, 0
	bne	%icc, .LL1836
	 nop
	cmp	%o3, 0
.LL1836:
	cmp	%o4, 0
	bl	%icc, .LL1831
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1837
	 nop
	cmp	%o5, 0
.LL1837:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1835
	 nop
.LL1831:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1834
	 nop
.LL1832:
	cmp	%o4, 0
	bg	%icc, .LL1833
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1838
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1833
	 nop
	cmp	%o5, 0
.LL1838:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1835
	 nop
.LL1833:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1834:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1835:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE333:
	.size	Divide__Divide_var_i64_u32, .-Divide__Divide_var_i64_u32
	.align 4
	.type	Divide__uDivide_param_i64_u32, #function
	.proc	05
Divide__uDivide_param_i64_u32:
.LLFB334:
	save	%sp, -104, %sp
.LLCFI334:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE334:
	.size	Divide__uDivide_param_i64_u32, .-Divide__uDivide_param_i64_u32
	.align 4
	.type	Divide__Divide_param_i64_u32, #function
	.proc	05
Divide__Divide_param_i64_u32:
.LLFB335:
	save	%sp, -104, %sp
.LLCFI335:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o3
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o2
	ldd	[%fp-8], %o4
	cmp	%o2, 0
	bl	%icc, .LL1842
	 nop
	cmp	%o2, 0
	bne	%icc, .LL1846
	 nop
	cmp	%o3, 0
.LL1846:
	cmp	%o4, 0
	bl	%icc, .LL1841
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1847
	 nop
	cmp	%o5, 0
.LL1847:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1845
	 nop
.LL1841:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1844
	 nop
.LL1842:
	cmp	%o4, 0
	bg	%icc, .LL1843
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1848
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1843
	 nop
	cmp	%o5, 0
.LL1848:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1845
	 nop
.LL1843:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1844:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1845:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE335:
	.size	Divide__Divide_param_i64_u32, .-Divide__Divide_param_i64_u32
	.align 4
	.type	Divide__uDivide_var_C_u16, #function
	.proc	04
Divide__uDivide_var_C_u16:
.LLFB336:
	save	%sp, -96, %sp
.LLCFI336:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE336:
	.size	Divide__uDivide_var_C_u16, .-Divide__uDivide_var_C_u16
	.align 4
	.type	Divide__Divide_var_C_u16, #function
	.proc	016
Divide__Divide_var_C_u16:
.LLFB337:
	save	%sp, -96, %sp
.LLCFI337:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+52], %o5
	srl	%o5, 16, %o5
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL1852
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1851
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1855
	 nop
.LL1851:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1854
	 nop
.LL1852:
	cmp	%g1, 0
	bg	%icc, .LL1853
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1855
	 nop
.LL1853:
	add	%g1, -1, %g1
.LL1854:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1855:
	cmp	%g1, 0
	bge	%icc, .LL1856
	 nop
	sethi	%hi(12288), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL1856:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+52], %o5
	srl	%o5, 16, %o5
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL1858
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1857
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1861
	 nop
.LL1857:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1860
	 nop
.LL1858:
	cmp	%g1, 0
	bg	%icc, .LL1859
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1861
	 nop
.LL1859:
	add	%g1, -1, %g1
.LL1860:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1861:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE337:
	.size	Divide__Divide_var_C_u16, .-Divide__Divide_var_C_u16
	.align 4
	.type	Divide__uDivide_param_C_u16, #function
	.proc	04
Divide__uDivide_param_C_u16:
.LLFB338:
	save	%sp, -96, %sp
.LLCFI338:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	wr	%g0, 0, %y
	ld	[%fp+68], %g1
	udiv	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE338:
	.size	Divide__uDivide_param_C_u16, .-Divide__uDivide_param_C_u16
	.align 4
	.type	Divide__Divide_param_C_u16, #function
	.proc	016
Divide__Divide_param_C_u16:
.LLFB339:
	save	%sp, -96, %sp
.LLCFI339:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ld	[%fp+68], %g1
	lduh	[%fp+72], %o5
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL1865
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1864
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1868
	 nop
.LL1864:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1867
	 nop
.LL1865:
	cmp	%g1, 0
	bg	%icc, .LL1866
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1868
	 nop
.LL1866:
	add	%g1, -1, %g1
.LL1867:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1868:
	cmp	%g1, 0
	bge	%icc, .LL1869
	 nop
	sethi	%hi(12288), %g1
	or	%g1, 289, %o0
	call	_m3_fault, 0
	 nop
.LL1869:
	ld	[%fp+68], %g1
	lduh	[%fp+72], %o5
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL1871
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1870
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1874
	 nop
.LL1870:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1873
	 nop
.LL1871:
	cmp	%g1, 0
	bg	%icc, .LL1872
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1874
	 nop
.LL1872:
	add	%g1, -1, %g1
.LL1873:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1874:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE339:
	.size	Divide__Divide_param_C_u16, .-Divide__Divide_param_C_u16
	.align 4
	.type	Divide__uDivide_var_C_u64, #function
	.proc	05
Divide__uDivide_var_C_u64:
.LLFB340:
	save	%sp, -96, %sp
.LLCFI340:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE340:
	.size	Divide__uDivide_var_C_u64, .-Divide__uDivide_var_C_u64
	.align 4
	.type	Divide__Divide_var_C_u64, #function
	.proc	05
Divide__Divide_var_C_u64:
.LLFB341:
	save	%sp, -96, %sp
.LLCFI341:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o2
	cmp	%o2, 0
	bl	%icc, .LL1878
	 nop
	cmp	%o2, 0
	bne	%icc, .LL1882
	 nop
	cmp	%o3, 0
.LL1882:
	cmp	%o4, 0
	bl	%icc, .LL1877
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1883
	 nop
	cmp	%o5, 0
.LL1883:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1881
	 nop
.LL1877:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1880
	 nop
.LL1878:
	cmp	%o4, 0
	bg	%icc, .LL1879
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1884
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1879
	 nop
	cmp	%o5, 0
.LL1884:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1881
	 nop
.LL1879:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1880:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1881:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE341:
	.size	Divide__Divide_var_C_u64, .-Divide__Divide_var_C_u64
	.align 4
	.type	Divide__uDivide_param_C_u64, #function
	.proc	05
Divide__uDivide_param_C_u64:
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
	mov	%o4, %o0
	mov	%o5, %o1
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE342:
	.size	Divide__uDivide_param_C_u64, .-Divide__uDivide_param_C_u64
	.align 4
	.type	Divide__Divide_param_C_u64, #function
	.proc	05
Divide__Divide_param_C_u64:
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
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL1888
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL1892
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL1892:
	cmp	%o4, 0
	bl	%icc, .LL1887
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1893
	 nop
	cmp	%o5, 0
.LL1893:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1891
	 nop
.LL1887:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1890
	 nop
.LL1888:
	cmp	%o4, 0
	bg	%icc, .LL1889
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1894
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1889
	 nop
	cmp	%o5, 0
.LL1894:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1891
	 nop
.LL1889:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1890:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1891:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE343:
	.size	Divide__Divide_param_C_u64, .-Divide__Divide_param_C_u64
	.align 4
	.type	Divide__uDivide_var_C_LC, #function
	.proc	05
Divide__uDivide_var_C_LC:
.LLFB344:
	save	%sp, -96, %sp
.LLCFI344:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE344:
	.size	Divide__uDivide_var_C_LC, .-Divide__uDivide_var_C_LC
	.align 4
	.type	Divide__Divide_var_C_LC, #function
	.proc	05
Divide__Divide_var_C_LC:
.LLFB345:
	save	%sp, -96, %sp
.LLCFI345:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o2
	cmp	%o2, 0
	bl	%icc, .LL1898
	 nop
	cmp	%o2, 0
	bne	%icc, .LL1902
	 nop
	cmp	%o3, 0
.LL1902:
	cmp	%o4, 0
	bl	%icc, .LL1897
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1903
	 nop
	cmp	%o5, 0
.LL1903:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1901
	 nop
.LL1897:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1900
	 nop
.LL1898:
	cmp	%o4, 0
	bg	%icc, .LL1899
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1904
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1899
	 nop
	cmp	%o5, 0
.LL1904:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1901
	 nop
.LL1899:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1900:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1901:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE345:
	.size	Divide__Divide_var_C_LC, .-Divide__Divide_var_C_LC
	.align 4
	.type	Divide__uDivide_param_C_LC, #function
	.proc	05
Divide__uDivide_param_C_LC:
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
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE346:
	.size	Divide__uDivide_param_C_LC, .-Divide__uDivide_param_C_LC
	.align 4
	.type	Divide__Divide_param_C_LC, #function
	.proc	05
Divide__Divide_param_C_LC:
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
	cmp	%o2, 0
	bl	%icc, .LL1908
	 nop
	cmp	%o2, 0
	bne	%icc, .LL1912
	 nop
	cmp	%o3, 0
.LL1912:
	cmp	%o4, 0
	bl	%icc, .LL1907
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1913
	 nop
	cmp	%o5, 0
.LL1913:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1911
	 nop
.LL1907:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1910
	 nop
.LL1908:
	cmp	%o4, 0
	bg	%icc, .LL1909
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1914
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1909
	 nop
	cmp	%o5, 0
.LL1914:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1911
	 nop
.LL1909:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1910:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1911:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE347:
	.size	Divide__Divide_param_C_LC, .-Divide__Divide_param_C_LC
	.align 4
	.type	Divide__uDivide_var_C_i32, #function
	.proc	04
Divide__uDivide_var_C_i32:
.LLFB348:
	save	%sp, -96, %sp
.LLCFI348:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE348:
	.size	Divide__uDivide_var_C_i32, .-Divide__uDivide_var_C_i32
	.align 4
	.type	Divide__Divide_var_C_i32, #function
	.proc	016
Divide__Divide_var_C_i32:
.LLFB349:
	save	%sp, -96, %sp
.LLCFI349:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL1918
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1917
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1921
	 nop
.LL1917:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1920
	 nop
.LL1918:
	cmp	%g1, 0
	bg	%icc, .LL1919
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1921
	 nop
.LL1919:
	add	%g1, -1, %g1
.LL1920:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1921:
	cmp	%g1, 0
	bge	%icc, .LL1922
	 nop
	sethi	%hi(12288), %g1
	or	%g1, 609, %o0
	call	_m3_fault, 0
	 nop
.LL1922:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL1924
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1923
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1927
	 nop
.LL1923:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1926
	 nop
.LL1924:
	cmp	%g1, 0
	bg	%icc, .LL1925
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1927
	 nop
.LL1925:
	add	%g1, -1, %g1
.LL1926:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1927:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE349:
	.size	Divide__Divide_var_C_i32, .-Divide__Divide_var_C_i32
	.align 4
	.type	Divide__uDivide_param_C_i32, #function
	.proc	04
Divide__uDivide_param_C_i32:
.LLFB350:
	save	%sp, -96, %sp
.LLCFI350:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	ld	[%fp+68], %g1
	udiv	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE350:
	.size	Divide__uDivide_param_C_i32, .-Divide__uDivide_param_C_i32
	.align 4
	.type	Divide__Divide_param_C_i32, #function
	.proc	016
Divide__Divide_param_C_i32:
.LLFB351:
	save	%sp, -96, %sp
.LLCFI351:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL1931
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1930
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1934
	 nop
.LL1930:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1933
	 nop
.LL1931:
	cmp	%g1, 0
	bg	%icc, .LL1932
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1934
	 nop
.LL1932:
	add	%g1, -1, %g1
.LL1933:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1934:
	cmp	%g1, 0
	bge	%icc, .LL1935
	 nop
	sethi	%hi(12288), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1935:
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL1937
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1936
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1940
	 nop
.LL1936:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1939
	 nop
.LL1937:
	cmp	%g1, 0
	bg	%icc, .LL1938
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1940
	 nop
.LL1938:
	add	%g1, -1, %g1
.LL1939:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1940:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE351:
	.size	Divide__Divide_param_C_i32, .-Divide__Divide_param_C_i32
	.align 4
	.type	Divide__uDivide_var_C_i16, #function
	.proc	04
Divide__uDivide_var_C_i16:
.LLFB352:
	save	%sp, -96, %sp
.LLCFI352:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE352:
	.size	Divide__uDivide_var_C_i16, .-Divide__uDivide_var_C_i16
	.align 4
	.type	Divide__Divide_var_C_i16, #function
	.proc	016
Divide__Divide_var_C_i16:
.LLFB353:
	save	%sp, -96, %sp
.LLCFI353:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+76], %o5
	sra	%o5, 16, %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL1944
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1943
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1947
	 nop
.LL1943:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1946
	 nop
.LL1944:
	cmp	%g1, 0
	bg	%icc, .LL1945
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1947
	 nop
.LL1945:
	add	%g1, -1, %g1
.LL1946:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1947:
	cmp	%g1, 0
	bge	%icc, .LL1948
	 nop
	sethi	%hi(12288), %g1
	or	%g1, 737, %o0
	call	_m3_fault, 0
	 nop
.LL1948:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+76], %o5
	sra	%o5, 16, %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL1950
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1949
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1953
	 nop
.LL1949:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1952
	 nop
.LL1950:
	cmp	%g1, 0
	bg	%icc, .LL1951
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1953
	 nop
.LL1951:
	add	%g1, -1, %g1
.LL1952:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1953:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE353:
	.size	Divide__Divide_var_C_i16, .-Divide__Divide_var_C_i16
	.align 4
	.type	Divide__uDivide_param_C_i16, #function
	.proc	04
Divide__uDivide_param_C_i16:
.LLFB354:
	save	%sp, -96, %sp
.LLCFI354:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	wr	%g0, 0, %y
	ld	[%fp+68], %g1
	udiv	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE354:
	.size	Divide__uDivide_param_C_i16, .-Divide__uDivide_param_C_i16
	.align 4
	.type	Divide__Divide_param_C_i16, #function
	.proc	016
Divide__Divide_param_C_i16:
.LLFB355:
	save	%sp, -96, %sp
.LLCFI355:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ld	[%fp+68], %g1
	lduh	[%fp+72], %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL1957
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1956
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1960
	 nop
.LL1956:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1959
	 nop
.LL1957:
	cmp	%g1, 0
	bg	%icc, .LL1958
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1960
	 nop
.LL1958:
	add	%g1, -1, %g1
.LL1959:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1960:
	cmp	%g1, 0
	bge	%icc, .LL1961
	 nop
	sethi	%hi(12288), %g1
	or	%g1, 801, %o0
	call	_m3_fault, 0
	 nop
.LL1961:
	ld	[%fp+68], %g1
	lduh	[%fp+72], %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL1963
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1962
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1966
	 nop
.LL1962:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1965
	 nop
.LL1963:
	cmp	%g1, 0
	bg	%icc, .LL1964
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1966
	 nop
.LL1964:
	add	%g1, -1, %g1
.LL1965:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1966:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE355:
	.size	Divide__Divide_param_C_i16, .-Divide__Divide_param_C_i16
	.align 4
	.type	Divide__uDivide_var_C_I, #function
	.proc	04
Divide__uDivide_var_C_I:
.LLFB356:
	save	%sp, -96, %sp
.LLCFI356:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE356:
	.size	Divide__uDivide_var_C_I, .-Divide__uDivide_var_C_I
	.align 4
	.type	Divide__Divide_var_C_I, #function
	.proc	016
Divide__Divide_var_C_I:
.LLFB357:
	save	%sp, -96, %sp
.LLCFI357:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+80], %o5
	cmp	%o5, 0
	bl	%icc, .LL1970
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1969
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1973
	 nop
.LL1969:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1972
	 nop
.LL1970:
	cmp	%g1, 0
	bg	%icc, .LL1971
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1973
	 nop
.LL1971:
	add	%g1, -1, %g1
.LL1972:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1973:
	cmp	%g1, 0
	bge	%icc, .LL1974
	 nop
	sethi	%hi(12288), %g1
	or	%g1, 865, %o0
	call	_m3_fault, 0
	 nop
.LL1974:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+80], %o5
	cmp	%o5, 0
	bl	%icc, .LL1976
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1975
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1979
	 nop
.LL1975:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1978
	 nop
.LL1976:
	cmp	%g1, 0
	bg	%icc, .LL1977
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1979
	 nop
.LL1977:
	add	%g1, -1, %g1
.LL1978:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1979:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE357:
	.size	Divide__Divide_var_C_I, .-Divide__Divide_var_C_I
	.align 4
	.type	Divide__uDivide_param_C_I, #function
	.proc	04
Divide__uDivide_param_C_I:
.LLFB358:
	save	%sp, -96, %sp
.LLCFI358:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	ld	[%fp+68], %g1
	udiv	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE358:
	.size	Divide__uDivide_param_C_I, .-Divide__uDivide_param_C_I
	.align 4
	.type	Divide__Divide_param_C_I, #function
	.proc	016
Divide__Divide_param_C_I:
.LLFB359:
	save	%sp, -96, %sp
.LLCFI359:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL1983
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1982
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1986
	 nop
.LL1982:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1985
	 nop
.LL1983:
	cmp	%g1, 0
	bg	%icc, .LL1984
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1986
	 nop
.LL1984:
	add	%g1, -1, %g1
.LL1985:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1986:
	cmp	%g1, 0
	bge	%icc, .LL1987
	 nop
	sethi	%hi(12288), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1987:
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL1989
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1988
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1992
	 nop
.LL1988:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1991
	 nop
.LL1989:
	cmp	%g1, 0
	bg	%icc, .LL1990
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1992
	 nop
.LL1990:
	add	%g1, -1, %g1
.LL1991:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1992:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE359:
	.size	Divide__Divide_param_C_I, .-Divide__Divide_param_C_I
	.align 4
	.type	Divide__uDivide_var_C_i64, #function
	.proc	05
Divide__uDivide_var_C_i64:
.LLFB360:
	save	%sp, -96, %sp
.LLCFI360:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE360:
	.size	Divide__uDivide_var_C_i64, .-Divide__uDivide_var_C_i64
	.align 4
	.type	Divide__Divide_var_C_i64, #function
	.proc	05
Divide__Divide_var_C_i64:
.LLFB361:
	save	%sp, -96, %sp
.LLCFI361:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o2
	cmp	%o2, 0
	bl	%icc, .LL1996
	 nop
	cmp	%o2, 0
	bne	%icc, .LL2000
	 nop
	cmp	%o3, 0
.LL2000:
	cmp	%o4, 0
	bl	%icc, .LL1995
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2001
	 nop
	cmp	%o5, 0
.LL2001:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1999
	 nop
.LL1995:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1998
	 nop
.LL1996:
	cmp	%o4, 0
	bg	%icc, .LL1997
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2002
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1997
	 nop
	cmp	%o5, 0
.LL2002:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1999
	 nop
.LL1997:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1998:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1999:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE361:
	.size	Divide__Divide_var_C_i64, .-Divide__Divide_var_C_i64
	.align 4
	.type	Divide__uDivide_param_C_i64, #function
	.proc	05
Divide__uDivide_param_C_i64:
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
	mov	%o4, %o0
	mov	%o5, %o1
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE362:
	.size	Divide__uDivide_param_C_i64, .-Divide__uDivide_param_C_i64
	.align 4
	.type	Divide__Divide_param_C_i64, #function
	.proc	05
Divide__Divide_param_C_i64:
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
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL2006
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL2010
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL2010:
	cmp	%o4, 0
	bl	%icc, .LL2005
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2011
	 nop
	cmp	%o5, 0
.LL2011:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2009
	 nop
.LL2005:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2008
	 nop
.LL2006:
	cmp	%o4, 0
	bg	%icc, .LL2007
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2012
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2007
	 nop
	cmp	%o5, 0
.LL2012:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2009
	 nop
.LL2007:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2008:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2009:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE363:
	.size	Divide__Divide_param_C_i64, .-Divide__Divide_param_C_i64
	.align 4
	.type	Divide__uDivide_var_C_C, #function
	.proc	04
Divide__uDivide_var_C_C:
.LLFB364:
	save	%sp, -96, %sp
.LLCFI364:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE364:
	.size	Divide__uDivide_var_C_C, .-Divide__uDivide_var_C_C
	.align 4
	.type	Divide__Divide_var_C_C, #function
	.proc	016
Divide__Divide_var_C_C:
.LLFB365:
	save	%sp, -96, %sp
.LLCFI365:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+96], %o5
	cmp	%o5, 0
	bl	%icc, .LL2016
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2015
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2019
	 nop
.LL2015:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2018
	 nop
.LL2016:
	cmp	%g1, 0
	bg	%icc, .LL2017
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2019
	 nop
.LL2017:
	add	%g1, -1, %g1
.LL2018:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2019:
	cmp	%g1, 0
	bge	%icc, .LL2020
	 nop
	sethi	%hi(13312), %g1
	or	%g1, 97, %o0
	call	_m3_fault, 0
	 nop
.LL2020:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+96], %o5
	cmp	%o5, 0
	bl	%icc, .LL2022
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2021
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2025
	 nop
.LL2021:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2024
	 nop
.LL2022:
	cmp	%g1, 0
	bg	%icc, .LL2023
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2025
	 nop
.LL2023:
	add	%g1, -1, %g1
.LL2024:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2025:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE365:
	.size	Divide__Divide_var_C_C, .-Divide__Divide_var_C_C
	.align 4
	.type	Divide__uDivide_param_C_C, #function
	.proc	04
Divide__uDivide_param_C_C:
.LLFB366:
	save	%sp, -96, %sp
.LLCFI366:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	wr	%g0, 0, %y
	ld	[%fp+72], %g1
	udiv	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE366:
	.size	Divide__uDivide_param_C_C, .-Divide__uDivide_param_C_C
	.align 4
	.type	Divide__Divide_param_C_C, #function
	.proc	016
Divide__Divide_param_C_C:
.LLFB367:
	save	%sp, -96, %sp
.LLCFI367:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL2029
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2028
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2032
	 nop
.LL2028:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2031
	 nop
.LL2029:
	cmp	%g1, 0
	bg	%icc, .LL2030
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2032
	 nop
.LL2030:
	add	%g1, -1, %g1
.LL2031:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2032:
	cmp	%g1, 0
	bge	%icc, .LL2033
	 nop
	sethi	%hi(13312), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2033:
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL2035
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2034
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2038
	 nop
.LL2034:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2037
	 nop
.LL2035:
	cmp	%g1, 0
	bg	%icc, .LL2036
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2038
	 nop
.LL2036:
	add	%g1, -1, %g1
.LL2037:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2038:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE367:
	.size	Divide__Divide_param_C_C, .-Divide__Divide_param_C_C
	.align 4
	.type	Divide__uDivide_var_C_u8, #function
	.proc	04
Divide__uDivide_var_C_u8:
.LLFB368:
	save	%sp, -96, %sp
.LLCFI368:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE368:
	.size	Divide__uDivide_var_C_u8, .-Divide__uDivide_var_C_u8
	.align 4
	.type	Divide__Divide_var_C_u8, #function
	.proc	016
Divide__Divide_var_C_u8:
.LLFB369:
	save	%sp, -96, %sp
.LLCFI369:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+112], %o5
	srl	%o5, 24, %o5
	and	%o5, 0xff, %o5
	cmp	%o5, 0
	bl	%icc, .LL2042
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2041
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2045
	 nop
.LL2041:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2044
	 nop
.LL2042:
	cmp	%g1, 0
	bg	%icc, .LL2043
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2045
	 nop
.LL2043:
	add	%g1, -1, %g1
.LL2044:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2045:
	cmp	%g1, 0
	bge	%icc, .LL2046
	 nop
	sethi	%hi(13312), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL2046:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+112], %o5
	srl	%o5, 24, %o5
	and	%o5, 0xff, %o5
	cmp	%o5, 0
	bl	%icc, .LL2048
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2047
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2051
	 nop
.LL2047:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2050
	 nop
.LL2048:
	cmp	%g1, 0
	bg	%icc, .LL2049
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2051
	 nop
.LL2049:
	add	%g1, -1, %g1
.LL2050:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2051:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE369:
	.size	Divide__Divide_var_C_u8, .-Divide__Divide_var_C_u8
	.align 4
	.type	Divide__uDivide_param_C_u8, #function
	.proc	04
Divide__uDivide_param_C_u8:
.LLFB370:
	save	%sp, -96, %sp
.LLCFI370:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	wr	%g0, 0, %y
	ld	[%fp+68], %g1
	udiv	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE370:
	.size	Divide__uDivide_param_C_u8, .-Divide__uDivide_param_C_u8
	.align 4
	.type	Divide__Divide_param_C_u8, #function
	.proc	016
Divide__Divide_param_C_u8:
.LLFB371:
	save	%sp, -96, %sp
.LLCFI371:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ld	[%fp+68], %g1
	ldub	[%fp+72], %o5
	and	%o5, 0xff, %o5
	cmp	%o5, 0
	bl	%icc, .LL2055
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2054
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2058
	 nop
.LL2054:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2057
	 nop
.LL2055:
	cmp	%g1, 0
	bg	%icc, .LL2056
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2058
	 nop
.LL2056:
	add	%g1, -1, %g1
.LL2057:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2058:
	cmp	%g1, 0
	bge	%icc, .LL2059
	 nop
	sethi	%hi(13312), %g1
	or	%g1, 289, %o0
	call	_m3_fault, 0
	 nop
.LL2059:
	ld	[%fp+68], %g1
	ldub	[%fp+72], %o5
	and	%o5, 0xff, %o5
	cmp	%o5, 0
	bl	%icc, .LL2061
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2060
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2064
	 nop
.LL2060:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2063
	 nop
.LL2061:
	cmp	%g1, 0
	bg	%icc, .LL2062
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2064
	 nop
.LL2062:
	add	%g1, -1, %g1
.LL2063:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2064:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE371:
	.size	Divide__Divide_param_C_u8, .-Divide__Divide_param_C_u8
	.align 4
	.type	Divide__uDivide_var_C_L, #function
	.proc	05
Divide__uDivide_var_C_L:
.LLFB372:
	save	%sp, -96, %sp
.LLCFI372:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE372:
	.size	Divide__uDivide_var_C_L, .-Divide__uDivide_var_C_L
	.align 4
	.type	Divide__Divide_var_C_L, #function
	.proc	05
Divide__Divide_var_C_L:
.LLFB373:
	save	%sp, -96, %sp
.LLCFI373:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o2
	cmp	%o2, 0
	bl	%icc, .LL2068
	 nop
	cmp	%o2, 0
	bne	%icc, .LL2072
	 nop
	cmp	%o3, 0
.LL2072:
	cmp	%o4, 0
	bl	%icc, .LL2067
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2073
	 nop
	cmp	%o5, 0
.LL2073:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2071
	 nop
.LL2067:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2070
	 nop
.LL2068:
	cmp	%o4, 0
	bg	%icc, .LL2069
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2074
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2069
	 nop
	cmp	%o5, 0
.LL2074:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2071
	 nop
.LL2069:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2070:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2071:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE373:
	.size	Divide__Divide_var_C_L, .-Divide__Divide_var_C_L
	.align 4
	.type	Divide__uDivide_param_C_L, #function
	.proc	05
Divide__uDivide_param_C_L:
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
	mov	%o4, %o0
	mov	%o5, %o1
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE374:
	.size	Divide__uDivide_param_C_L, .-Divide__uDivide_param_C_L
	.align 4
	.type	Divide__Divide_param_C_L, #function
	.proc	05
Divide__Divide_param_C_L:
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
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL2078
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL2082
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL2082:
	cmp	%o4, 0
	bl	%icc, .LL2077
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2083
	 nop
	cmp	%o5, 0
.LL2083:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2081
	 nop
.LL2077:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2080
	 nop
.LL2078:
	cmp	%o4, 0
	bg	%icc, .LL2079
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2084
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2079
	 nop
	cmp	%o5, 0
.LL2084:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2081
	 nop
.LL2079:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2080:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2081:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE375:
	.size	Divide__Divide_param_C_L, .-Divide__Divide_param_C_L
	.align 4
	.type	Divide__uDivide_var_C_i8, #function
	.proc	04
Divide__uDivide_var_C_i8:
.LLFB376:
	save	%sp, -96, %sp
.LLCFI376:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE376:
	.size	Divide__uDivide_var_C_i8, .-Divide__uDivide_var_C_i8
	.align 4
	.type	Divide__Divide_var_C_i8, #function
	.proc	016
Divide__Divide_var_C_i8:
.LLFB377:
	save	%sp, -96, %sp
.LLCFI377:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+128], %o5
	sra	%o5, 24, %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	cmp	%o5, 0
	bl	%icc, .LL2088
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2087
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2091
	 nop
.LL2087:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2090
	 nop
.LL2088:
	cmp	%g1, 0
	bg	%icc, .LL2089
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2091
	 nop
.LL2089:
	add	%g1, -1, %g1
.LL2090:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2091:
	cmp	%g1, 0
	bge	%icc, .LL2092
	 nop
	sethi	%hi(13312), %g1
	or	%g1, 481, %o0
	call	_m3_fault, 0
	 nop
.LL2092:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+128], %o5
	sra	%o5, 24, %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	cmp	%o5, 0
	bl	%icc, .LL2094
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2093
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2097
	 nop
.LL2093:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2096
	 nop
.LL2094:
	cmp	%g1, 0
	bg	%icc, .LL2095
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2097
	 nop
.LL2095:
	add	%g1, -1, %g1
.LL2096:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2097:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE377:
	.size	Divide__Divide_var_C_i8, .-Divide__Divide_var_C_i8
	.align 4
	.type	Divide__uDivide_param_C_i8, #function
	.proc	04
Divide__uDivide_param_C_i8:
.LLFB378:
	save	%sp, -96, %sp
.LLCFI378:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	wr	%g0, 0, %y
	ld	[%fp+68], %g1
	udiv	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE378:
	.size	Divide__uDivide_param_C_i8, .-Divide__uDivide_param_C_i8
	.align 4
	.type	Divide__Divide_param_C_i8, #function
	.proc	016
Divide__Divide_param_C_i8:
.LLFB379:
	save	%sp, -96, %sp
.LLCFI379:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ld	[%fp+68], %g1
	ldub	[%fp+72], %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	cmp	%o5, 0
	bl	%icc, .LL2101
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2100
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2104
	 nop
.LL2100:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2103
	 nop
.LL2101:
	cmp	%g1, 0
	bg	%icc, .LL2102
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2104
	 nop
.LL2102:
	add	%g1, -1, %g1
.LL2103:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2104:
	cmp	%g1, 0
	bge	%icc, .LL2105
	 nop
	sethi	%hi(13312), %g1
	or	%g1, 545, %o0
	call	_m3_fault, 0
	 nop
.LL2105:
	ld	[%fp+68], %g1
	ldub	[%fp+72], %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	cmp	%o5, 0
	bl	%icc, .LL2107
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2106
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2110
	 nop
.LL2106:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2109
	 nop
.LL2107:
	cmp	%g1, 0
	bg	%icc, .LL2108
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2110
	 nop
.LL2108:
	add	%g1, -1, %g1
.LL2109:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2110:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE379:
	.size	Divide__Divide_param_C_i8, .-Divide__Divide_param_C_i8
	.align 4
	.type	Divide__uDivide_var_C_u32, #function
	.proc	04
Divide__uDivide_var_C_u32:
.LLFB380:
	save	%sp, -96, %sp
.LLCFI380:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE380:
	.size	Divide__uDivide_var_C_u32, .-Divide__uDivide_var_C_u32
	.align 4
	.type	Divide__Divide_var_C_u32, #function
	.proc	016
Divide__Divide_var_C_u32:
.LLFB381:
	save	%sp, -96, %sp
.LLCFI381:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+132], %o5
	cmp	%o5, 0
	bl	%icc, .LL2114
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2113
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2117
	 nop
.LL2113:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2116
	 nop
.LL2114:
	cmp	%g1, 0
	bg	%icc, .LL2115
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2117
	 nop
.LL2115:
	add	%g1, -1, %g1
.LL2116:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2117:
	cmp	%g1, 0
	bge	%icc, .LL2118
	 nop
	sethi	%hi(13312), %g1
	or	%g1, 609, %o0
	call	_m3_fault, 0
	 nop
.LL2118:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+132], %o5
	cmp	%o5, 0
	bl	%icc, .LL2120
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2119
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2123
	 nop
.LL2119:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2122
	 nop
.LL2120:
	cmp	%g1, 0
	bg	%icc, .LL2121
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2123
	 nop
.LL2121:
	add	%g1, -1, %g1
.LL2122:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2123:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE381:
	.size	Divide__Divide_var_C_u32, .-Divide__Divide_var_C_u32
	.align 4
	.type	Divide__uDivide_param_C_u32, #function
	.proc	04
Divide__uDivide_param_C_u32:
.LLFB382:
	save	%sp, -96, %sp
.LLCFI382:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	ld	[%fp+68], %g1
	udiv	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE382:
	.size	Divide__uDivide_param_C_u32, .-Divide__uDivide_param_C_u32
	.align 4
	.type	Divide__Divide_param_C_u32, #function
	.proc	016
Divide__Divide_param_C_u32:
.LLFB383:
	save	%sp, -96, %sp
.LLCFI383:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL2127
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2126
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2130
	 nop
.LL2126:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2129
	 nop
.LL2127:
	cmp	%g1, 0
	bg	%icc, .LL2128
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2130
	 nop
.LL2128:
	add	%g1, -1, %g1
.LL2129:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2130:
	cmp	%g1, 0
	bge	%icc, .LL2131
	 nop
	sethi	%hi(13312), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2131:
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL2133
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2132
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2136
	 nop
.LL2132:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2135
	 nop
.LL2133:
	cmp	%g1, 0
	bg	%icc, .LL2134
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2136
	 nop
.LL2134:
	add	%g1, -1, %g1
.LL2135:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2136:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE383:
	.size	Divide__Divide_param_C_u32, .-Divide__Divide_param_C_u32
	.align 4
	.type	Divide__Divide_var_f64_f64, #function
	.proc	07
Divide__Divide_var_f64_f64:
.LLFB384:
	save	%sp, -96, %sp
.LLCFI384:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	add	%g1, 104, %g1
	ldd	[%g1], %f10
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	add	%g1, 104, %g1
	ldd	[%g1], %f8
	fdivd	%f10, %f8, %f8
	fmovd	%f8, %f0
	return	%i7+8
	 nop
.LLFE384:
	.size	Divide__Divide_var_f64_f64, .-Divide__Divide_var_f64_f64
	.align 4
	.type	Divide__Divide_param_f64_f64, #function
	.proc	07
Divide__Divide_param_f64_f64:
.LLFB385:
	save	%sp, -112, %sp
.LLCFI385:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %f10
	ldd	[%fp-16], %f8
	fdivd	%f10, %f8, %f8
	fmovd	%f8, %f0
	return	%i7+8
	 nop
.LLFE385:
	.size	Divide__Divide_param_f64_f64, .-Divide__Divide_param_f64_f64
	.align 4
	.type	Divide__uDivide_var_u8_u16, #function
	.proc	04
Divide__uDivide_var_u8_u16:
.LLFB386:
	save	%sp, -96, %sp
.LLCFI386:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE386:
	.size	Divide__uDivide_var_u8_u16, .-Divide__uDivide_var_u8_u16
	.align 4
	.type	Divide__Divide_var_u8_u16, #function
	.proc	014
Divide__Divide_var_u8_u16:
.LLFB387:
	save	%sp, -96, %sp
.LLCFI387:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+52], %o5
	srl	%o5, 16, %o5
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL2142
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2141
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2145
	 nop
.LL2141:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2144
	 nop
.LL2142:
	cmp	%g1, 0
	bg	%icc, .LL2143
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2145
	 nop
.LL2143:
	add	%g1, -1, %g1
.LL2144:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2145:
	cmp	%g1, 255
	ble	%icc, .LL2146
	 nop
	sethi	%hi(13312), %g1
	or	%g1, 801, %o0
	call	_m3_fault, 0
	 nop
.LL2146:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+52], %o5
	srl	%o5, 16, %o5
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL2148
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2147
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2151
	 nop
.LL2147:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2150
	 nop
.LL2148:
	cmp	%g1, 0
	bg	%icc, .LL2149
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2151
	 nop
.LL2149:
	add	%g1, -1, %g1
.LL2150:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2151:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE387:
	.size	Divide__Divide_var_u8_u16, .-Divide__Divide_var_u8_u16
	.align 4
	.type	Divide__uDivide_param_u8_u16, #function
	.proc	04
Divide__uDivide_param_u8_u16:
.LLFB388:
	save	%sp, -96, %sp
.LLCFI388:
	stb	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o4
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE388:
	.size	Divide__uDivide_param_u8_u16, .-Divide__uDivide_param_u8_u16
	.align 4
	.type	Divide__Divide_param_u8_u16, #function
	.proc	014
Divide__Divide_param_u8_u16:
.LLFB389:
	save	%sp, -96, %sp
.LLCFI389:
	stb	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	lduh	[%fp+72], %o5
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL2155
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2154
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2158
	 nop
.LL2154:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2157
	 nop
.LL2155:
	cmp	%g1, 0
	bg	%icc, .LL2156
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2158
	 nop
.LL2156:
	add	%g1, -1, %g1
.LL2157:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2158:
	cmp	%g1, 255
	ble	%icc, .LL2159
	 nop
	sethi	%hi(13312), %g1
	or	%g1, 865, %o0
	call	_m3_fault, 0
	 nop
.LL2159:
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	lduh	[%fp+72], %o5
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL2161
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2160
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2164
	 nop
.LL2160:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2163
	 nop
.LL2161:
	cmp	%g1, 0
	bg	%icc, .LL2162
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2164
	 nop
.LL2162:
	add	%g1, -1, %g1
.LL2163:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2164:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE389:
	.size	Divide__Divide_param_u8_u16, .-Divide__Divide_param_u8_u16
	.align 4
	.type	Divide__uDivide_var_u8_u64, #function
	.proc	05
Divide__uDivide_var_u8_u64:
.LLFB390:
	save	%sp, -96, %sp
.LLCFI390:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE390:
	.size	Divide__uDivide_var_u8_u64, .-Divide__uDivide_var_u8_u64
	.align 4
	.type	Divide__Divide_var_u8_u64, #function
	.proc	05
Divide__Divide_var_u8_u64:
.LLFB391:
	save	%sp, -96, %sp
.LLCFI391:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o2
	cmp	%o2, 0
	bl	%icc, .LL2168
	 nop
	cmp	%o2, 0
	bne	%icc, .LL2172
	 nop
	cmp	%o3, 0
.LL2172:
	cmp	%o4, 0
	bl	%icc, .LL2167
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2173
	 nop
	cmp	%o5, 0
.LL2173:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2171
	 nop
.LL2167:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2170
	 nop
.LL2168:
	cmp	%o4, 0
	bg	%icc, .LL2169
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2174
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2169
	 nop
	cmp	%o5, 0
.LL2174:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2171
	 nop
.LL2169:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2170:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2171:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE391:
	.size	Divide__Divide_var_u8_u64, .-Divide__Divide_var_u8_u64
	.align 4
	.type	Divide__uDivide_param_u8_u64, #function
	.proc	05
Divide__uDivide_param_u8_u64:
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
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE392:
	.size	Divide__uDivide_param_u8_u64, .-Divide__uDivide_param_u8_u64
	.align 4
	.type	Divide__Divide_param_u8_u64, #function
	.proc	05
Divide__Divide_param_u8_u64:
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
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL2178
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL2182
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL2182:
	cmp	%o4, 0
	bl	%icc, .LL2177
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2183
	 nop
	cmp	%o5, 0
.LL2183:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2181
	 nop
.LL2177:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2180
	 nop
.LL2178:
	cmp	%o4, 0
	bg	%icc, .LL2179
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2184
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2179
	 nop
	cmp	%o5, 0
.LL2184:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2181
	 nop
.LL2179:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2180:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2181:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE393:
	.size	Divide__Divide_param_u8_u64, .-Divide__Divide_param_u8_u64
	.align 4
	.type	Divide__uDivide_var_u8_LC, #function
	.proc	05
Divide__uDivide_var_u8_LC:
.LLFB394:
	save	%sp, -96, %sp
.LLCFI394:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE394:
	.size	Divide__uDivide_var_u8_LC, .-Divide__uDivide_var_u8_LC
	.align 4
	.type	Divide__Divide_var_u8_LC, #function
	.proc	05
Divide__Divide_var_u8_LC:
.LLFB395:
	save	%sp, -96, %sp
.LLCFI395:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o2
	cmp	%o2, 0
	bl	%icc, .LL2188
	 nop
	cmp	%o2, 0
	bne	%icc, .LL2192
	 nop
	cmp	%o3, 0
.LL2192:
	cmp	%o4, 0
	bl	%icc, .LL2187
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2193
	 nop
	cmp	%o5, 0
.LL2193:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2191
	 nop
.LL2187:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2190
	 nop
.LL2188:
	cmp	%o4, 0
	bg	%icc, .LL2189
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2194
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2189
	 nop
	cmp	%o5, 0
.LL2194:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2191
	 nop
.LL2189:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2190:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2191:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE395:
	.size	Divide__Divide_var_u8_LC, .-Divide__Divide_var_u8_LC
	.align 4
	.type	Divide__uDivide_param_u8_LC, #function
	.proc	05
Divide__uDivide_param_u8_LC:
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
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE396:
	.size	Divide__uDivide_param_u8_LC, .-Divide__uDivide_param_u8_LC
	.align 4
	.type	Divide__Divide_param_u8_LC, #function
	.proc	05
Divide__Divide_param_u8_LC:
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
	cmp	%o2, 0
	bl	%icc, .LL2198
	 nop
	cmp	%o2, 0
	bne	%icc, .LL2202
	 nop
	cmp	%o3, 0
.LL2202:
	cmp	%o4, 0
	bl	%icc, .LL2197
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2203
	 nop
	cmp	%o5, 0
.LL2203:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2201
	 nop
.LL2197:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2200
	 nop
.LL2198:
	cmp	%o4, 0
	bg	%icc, .LL2199
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2204
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2199
	 nop
	cmp	%o5, 0
.LL2204:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2201
	 nop
.LL2199:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2200:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2201:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE397:
	.size	Divide__Divide_param_u8_LC, .-Divide__Divide_param_u8_LC
	.align 4
	.type	Divide__uDivide_var_u8_i32, #function
	.proc	04
Divide__uDivide_var_u8_i32:
.LLFB398:
	save	%sp, -96, %sp
.LLCFI398:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE398:
	.size	Divide__uDivide_var_u8_i32, .-Divide__uDivide_var_u8_i32
	.align 4
	.type	Divide__Divide_var_u8_i32, #function
	.proc	014
Divide__Divide_var_u8_i32:
.LLFB399:
	save	%sp, -96, %sp
.LLCFI399:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL2208
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2207
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2211
	 nop
.LL2207:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2210
	 nop
.LL2208:
	cmp	%g1, 0
	bg	%icc, .LL2209
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2211
	 nop
.LL2209:
	add	%g1, -1, %g1
.LL2210:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2211:
	cmp	%g1, 0
	bl	%icc, .LL2212
	 nop
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL2214
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2213
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2217
	 nop
.LL2213:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2216
	 nop
.LL2214:
	cmp	%g1, 0
	bg	%icc, .LL2215
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2217
	 nop
.LL2215:
	add	%g1, -1, %g1
.LL2216:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2217:
	cmp	%g1, 255
	ble	%icc, .LL2218
	 nop
.LL2212:
	sethi	%hi(14336), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2218:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL2220
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2219
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2223
	 nop
.LL2219:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2222
	 nop
.LL2220:
	cmp	%g1, 0
	bg	%icc, .LL2221
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2223
	 nop
.LL2221:
	add	%g1, -1, %g1
.LL2222:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2223:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE399:
	.size	Divide__Divide_var_u8_i32, .-Divide__Divide_var_u8_i32
	.align 4
	.type	Divide__uDivide_param_u8_i32, #function
	.proc	04
Divide__uDivide_param_u8_i32:
.LLFB400:
	save	%sp, -96, %sp
.LLCFI400:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o4
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE400:
	.size	Divide__uDivide_param_u8_i32, .-Divide__uDivide_param_u8_i32
	.align 4
	.type	Divide__Divide_param_u8_i32, #function
	.proc	014
Divide__Divide_param_u8_i32:
.LLFB401:
	save	%sp, -96, %sp
.LLCFI401:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL2227
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2226
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2230
	 nop
.LL2226:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2229
	 nop
.LL2227:
	cmp	%g1, 0
	bg	%icc, .LL2228
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2230
	 nop
.LL2228:
	add	%g1, -1, %g1
.LL2229:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2230:
	cmp	%g1, 0
	bl	%icc, .LL2231
	 nop
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL2233
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2232
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2236
	 nop
.LL2232:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2235
	 nop
.LL2233:
	cmp	%g1, 0
	bg	%icc, .LL2234
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2236
	 nop
.LL2234:
	add	%g1, -1, %g1
.LL2235:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2236:
	cmp	%g1, 255
	ble	%icc, .LL2237
	 nop
.LL2231:
	sethi	%hi(14336), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL2237:
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL2239
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2238
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2242
	 nop
.LL2238:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2241
	 nop
.LL2239:
	cmp	%g1, 0
	bg	%icc, .LL2240
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2242
	 nop
.LL2240:
	add	%g1, -1, %g1
.LL2241:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2242:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE401:
	.size	Divide__Divide_param_u8_i32, .-Divide__Divide_param_u8_i32
	.align 4
	.type	Divide__uDivide_var_u8_i16, #function
	.proc	04
Divide__uDivide_var_u8_i16:
.LLFB402:
	save	%sp, -96, %sp
.LLCFI402:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE402:
	.size	Divide__uDivide_var_u8_i16, .-Divide__uDivide_var_u8_i16
	.align 4
	.type	Divide__Divide_var_u8_i16, #function
	.proc	014
Divide__Divide_var_u8_i16:
.LLFB403:
	save	%sp, -96, %sp
.LLCFI403:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+76], %o5
	sra	%o5, 16, %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL2246
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2245
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2249
	 nop
.LL2245:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2248
	 nop
.LL2246:
	cmp	%g1, 0
	bg	%icc, .LL2247
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2249
	 nop
.LL2247:
	add	%g1, -1, %g1
.LL2248:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2249:
	cmp	%g1, 0
	bl	%icc, .LL2250
	 nop
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+76], %o5
	sra	%o5, 16, %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL2252
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2251
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2255
	 nop
.LL2251:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2254
	 nop
.LL2252:
	cmp	%g1, 0
	bg	%icc, .LL2253
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2255
	 nop
.LL2253:
	add	%g1, -1, %g1
.LL2254:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2255:
	cmp	%g1, 255
	ble	%icc, .LL2256
	 nop
.LL2250:
	sethi	%hi(14336), %g1
	or	%g1, 289, %o0
	call	_m3_fault, 0
	 nop
.LL2256:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+76], %o5
	sra	%o5, 16, %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL2258
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2257
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2261
	 nop
.LL2257:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2260
	 nop
.LL2258:
	cmp	%g1, 0
	bg	%icc, .LL2259
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2261
	 nop
.LL2259:
	add	%g1, -1, %g1
.LL2260:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2261:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE403:
	.size	Divide__Divide_var_u8_i16, .-Divide__Divide_var_u8_i16
	.align 4
	.type	Divide__uDivide_param_u8_i16, #function
	.proc	04
Divide__uDivide_param_u8_i16:
.LLFB404:
	save	%sp, -96, %sp
.LLCFI404:
	stb	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o4
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE404:
	.size	Divide__uDivide_param_u8_i16, .-Divide__uDivide_param_u8_i16
	.align 4
	.type	Divide__Divide_param_u8_i16, #function
	.proc	014
Divide__Divide_param_u8_i16:
.LLFB405:
	save	%sp, -96, %sp
.LLCFI405:
	stb	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	lduh	[%fp+72], %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL2265
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2264
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2268
	 nop
.LL2264:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2267
	 nop
.LL2265:
	cmp	%g1, 0
	bg	%icc, .LL2266
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2268
	 nop
.LL2266:
	add	%g1, -1, %g1
.LL2267:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2268:
	cmp	%g1, 0
	bl	%icc, .LL2269
	 nop
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	lduh	[%fp+72], %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL2271
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2270
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2274
	 nop
.LL2270:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2273
	 nop
.LL2271:
	cmp	%g1, 0
	bg	%icc, .LL2272
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2274
	 nop
.LL2272:
	add	%g1, -1, %g1
.LL2273:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2274:
	cmp	%g1, 255
	ble	%icc, .LL2275
	 nop
.LL2269:
	sethi	%hi(14336), %g1
	or	%g1, 353, %o0
	call	_m3_fault, 0
	 nop
.LL2275:
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	lduh	[%fp+72], %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL2277
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2276
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2280
	 nop
.LL2276:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2279
	 nop
.LL2277:
	cmp	%g1, 0
	bg	%icc, .LL2278
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2280
	 nop
.LL2278:
	add	%g1, -1, %g1
.LL2279:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2280:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE405:
	.size	Divide__Divide_param_u8_i16, .-Divide__Divide_param_u8_i16
	.align 4
	.type	Divide__uDivide_var_u8_I, #function
	.proc	04
Divide__uDivide_var_u8_I:
.LLFB406:
	save	%sp, -96, %sp
.LLCFI406:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE406:
	.size	Divide__uDivide_var_u8_I, .-Divide__uDivide_var_u8_I
	.align 4
	.type	Divide__Divide_var_u8_I, #function
	.proc	014
Divide__Divide_var_u8_I:
.LLFB407:
	save	%sp, -96, %sp
.LLCFI407:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+80], %o5
	cmp	%o5, 0
	bl	%icc, .LL2284
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2283
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2287
	 nop
.LL2283:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2286
	 nop
.LL2284:
	cmp	%g1, 0
	bg	%icc, .LL2285
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2287
	 nop
.LL2285:
	add	%g1, -1, %g1
.LL2286:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2287:
	cmp	%g1, 0
	bl	%icc, .LL2288
	 nop
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+80], %o5
	cmp	%o5, 0
	bl	%icc, .LL2290
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2289
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2293
	 nop
.LL2289:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2292
	 nop
.LL2290:
	cmp	%g1, 0
	bg	%icc, .LL2291
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2293
	 nop
.LL2291:
	add	%g1, -1, %g1
.LL2292:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2293:
	cmp	%g1, 255
	ble	%icc, .LL2294
	 nop
.LL2288:
	sethi	%hi(14336), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2294:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+80], %o5
	cmp	%o5, 0
	bl	%icc, .LL2296
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2295
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2299
	 nop
.LL2295:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2298
	 nop
.LL2296:
	cmp	%g1, 0
	bg	%icc, .LL2297
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2299
	 nop
.LL2297:
	add	%g1, -1, %g1
.LL2298:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2299:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE407:
	.size	Divide__Divide_var_u8_I, .-Divide__Divide_var_u8_I
	.align 4
	.type	Divide__uDivide_param_u8_I, #function
	.proc	04
Divide__uDivide_param_u8_I:
.LLFB408:
	save	%sp, -96, %sp
.LLCFI408:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o4
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE408:
	.size	Divide__uDivide_param_u8_I, .-Divide__uDivide_param_u8_I
	.align 4
	.type	Divide__Divide_param_u8_I, #function
	.proc	014
Divide__Divide_param_u8_I:
.LLFB409:
	save	%sp, -96, %sp
.LLCFI409:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL2303
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2302
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2306
	 nop
.LL2302:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2305
	 nop
.LL2303:
	cmp	%g1, 0
	bg	%icc, .LL2304
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2306
	 nop
.LL2304:
	add	%g1, -1, %g1
.LL2305:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2306:
	cmp	%g1, 0
	bl	%icc, .LL2307
	 nop
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL2309
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2308
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2312
	 nop
.LL2308:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2311
	 nop
.LL2309:
	cmp	%g1, 0
	bg	%icc, .LL2310
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2312
	 nop
.LL2310:
	add	%g1, -1, %g1
.LL2311:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2312:
	cmp	%g1, 255
	ble	%icc, .LL2313
	 nop
.LL2307:
	sethi	%hi(14336), %g1
	or	%g1, 481, %o0
	call	_m3_fault, 0
	 nop
.LL2313:
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL2315
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2314
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2318
	 nop
.LL2314:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2317
	 nop
.LL2315:
	cmp	%g1, 0
	bg	%icc, .LL2316
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2318
	 nop
.LL2316:
	add	%g1, -1, %g1
.LL2317:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2318:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE409:
	.size	Divide__Divide_param_u8_I, .-Divide__Divide_param_u8_I
	.align 4
	.type	Divide__uDivide_var_u8_i64, #function
	.proc	05
Divide__uDivide_var_u8_i64:
.LLFB410:
	save	%sp, -96, %sp
.LLCFI410:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE410:
	.size	Divide__uDivide_var_u8_i64, .-Divide__uDivide_var_u8_i64
	.align 4
	.type	Divide__Divide_var_u8_i64, #function
	.proc	05
Divide__Divide_var_u8_i64:
.LLFB411:
	save	%sp, -96, %sp
.LLCFI411:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o2
	cmp	%o2, 0
	bl	%icc, .LL2322
	 nop
	cmp	%o2, 0
	bne	%icc, .LL2326
	 nop
	cmp	%o3, 0
.LL2326:
	cmp	%o4, 0
	bl	%icc, .LL2321
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2327
	 nop
	cmp	%o5, 0
.LL2327:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2325
	 nop
.LL2321:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2324
	 nop
.LL2322:
	cmp	%o4, 0
	bg	%icc, .LL2323
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2328
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2323
	 nop
	cmp	%o5, 0
.LL2328:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2325
	 nop
.LL2323:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2324:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2325:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE411:
	.size	Divide__Divide_var_u8_i64, .-Divide__Divide_var_u8_i64
	.align 4
	.type	Divide__uDivide_param_u8_i64, #function
	.proc	05
Divide__uDivide_param_u8_i64:
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
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE412:
	.size	Divide__uDivide_param_u8_i64, .-Divide__uDivide_param_u8_i64
	.align 4
	.type	Divide__Divide_param_u8_i64, #function
	.proc	05
Divide__Divide_param_u8_i64:
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
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL2332
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL2336
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL2336:
	cmp	%o4, 0
	bl	%icc, .LL2331
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2337
	 nop
	cmp	%o5, 0
.LL2337:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2335
	 nop
.LL2331:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2334
	 nop
.LL2332:
	cmp	%o4, 0
	bg	%icc, .LL2333
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2338
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2333
	 nop
	cmp	%o5, 0
.LL2338:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2335
	 nop
.LL2333:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2334:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2335:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE413:
	.size	Divide__Divide_param_u8_i64, .-Divide__Divide_param_u8_i64
	.align 4
	.type	Divide__uDivide_var_u8_C, #function
	.proc	04
Divide__uDivide_var_u8_C:
.LLFB414:
	save	%sp, -96, %sp
.LLCFI414:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE414:
	.size	Divide__uDivide_var_u8_C, .-Divide__uDivide_var_u8_C
	.align 4
	.type	Divide__Divide_var_u8_C, #function
	.proc	014
Divide__Divide_var_u8_C:
.LLFB415:
	save	%sp, -96, %sp
.LLCFI415:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+96], %o5
	cmp	%o5, 0
	bl	%icc, .LL2342
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2341
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2345
	 nop
.LL2341:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2344
	 nop
.LL2342:
	cmp	%g1, 0
	bg	%icc, .LL2343
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2345
	 nop
.LL2343:
	add	%g1, -1, %g1
.LL2344:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2345:
	cmp	%g1, 0
	bl	%icc, .LL2346
	 nop
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+96], %o5
	cmp	%o5, 0
	bl	%icc, .LL2348
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2347
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2351
	 nop
.LL2347:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2350
	 nop
.LL2348:
	cmp	%g1, 0
	bg	%icc, .LL2349
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2351
	 nop
.LL2349:
	add	%g1, -1, %g1
.LL2350:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2351:
	cmp	%g1, 255
	ble	%icc, .LL2352
	 nop
.LL2346:
	sethi	%hi(14336), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2352:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+96], %o5
	cmp	%o5, 0
	bl	%icc, .LL2354
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2353
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2357
	 nop
.LL2353:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2356
	 nop
.LL2354:
	cmp	%g1, 0
	bg	%icc, .LL2355
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2357
	 nop
.LL2355:
	add	%g1, -1, %g1
.LL2356:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2357:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE415:
	.size	Divide__Divide_var_u8_C, .-Divide__Divide_var_u8_C
	.align 4
	.type	Divide__uDivide_param_u8_C, #function
	.proc	04
Divide__uDivide_param_u8_C:
.LLFB416:
	save	%sp, -96, %sp
.LLCFI416:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	wr	%g0, 0, %y
	ld	[%fp+72], %g1
	udiv	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE416:
	.size	Divide__uDivide_param_u8_C, .-Divide__uDivide_param_u8_C
	.align 4
	.type	Divide__Divide_param_u8_C, #function
	.proc	014
Divide__Divide_param_u8_C:
.LLFB417:
	save	%sp, -96, %sp
.LLCFI417:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL2361
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2360
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2364
	 nop
.LL2360:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2363
	 nop
.LL2361:
	cmp	%g1, 0
	bg	%icc, .LL2362
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2364
	 nop
.LL2362:
	add	%g1, -1, %g1
.LL2363:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2364:
	cmp	%g1, 0
	bl	%icc, .LL2365
	 nop
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL2367
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2366
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2370
	 nop
.LL2366:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2369
	 nop
.LL2367:
	cmp	%g1, 0
	bg	%icc, .LL2368
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2370
	 nop
.LL2368:
	add	%g1, -1, %g1
.LL2369:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2370:
	cmp	%g1, 255
	ble	%icc, .LL2371
	 nop
.LL2365:
	sethi	%hi(14336), %g1
	or	%g1, 737, %o0
	call	_m3_fault, 0
	 nop
.LL2371:
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL2373
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2372
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2376
	 nop
.LL2372:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2375
	 nop
.LL2373:
	cmp	%g1, 0
	bg	%icc, .LL2374
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2376
	 nop
.LL2374:
	add	%g1, -1, %g1
.LL2375:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2376:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE417:
	.size	Divide__Divide_param_u8_C, .-Divide__Divide_param_u8_C
	.align 4
	.type	Divide__uDivide_var_u8_u8, #function
	.proc	04
Divide__uDivide_var_u8_u8:
.LLFB418:
	save	%sp, -96, %sp
.LLCFI418:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE418:
	.size	Divide__uDivide_var_u8_u8, .-Divide__uDivide_var_u8_u8
	.align 4
	.type	Divide__Divide_var_u8_u8, #function
	.proc	014
Divide__Divide_var_u8_u8:
.LLFB419:
	save	%sp, -96, %sp
.LLCFI419:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+112], %o5
	srl	%o5, 24, %o5
	and	%o5, 0xff, %o5
	cmp	%o5, 0
	bl	%icc, .LL2380
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2379
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2383
	 nop
.LL2379:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2382
	 nop
.LL2380:
	cmp	%g1, 0
	bg	%icc, .LL2381
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2383
	 nop
.LL2381:
	add	%g1, -1, %g1
.LL2382:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2383:
	cmp	%g1, 255
	ble	%icc, .LL2384
	 nop
	sethi	%hi(14336), %g1
	or	%g1, 801, %o0
	call	_m3_fault, 0
	 nop
.LL2384:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+112], %o5
	srl	%o5, 24, %o5
	and	%o5, 0xff, %o5
	cmp	%o5, 0
	bl	%icc, .LL2386
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2385
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2389
	 nop
.LL2385:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2388
	 nop
.LL2386:
	cmp	%g1, 0
	bg	%icc, .LL2387
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2389
	 nop
.LL2387:
	add	%g1, -1, %g1
.LL2388:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2389:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE419:
	.size	Divide__Divide_var_u8_u8, .-Divide__Divide_var_u8_u8
	.align 4
	.type	Divide__uDivide_param_u8_u8, #function
	.proc	04
Divide__uDivide_param_u8_u8:
.LLFB420:
	save	%sp, -96, %sp
.LLCFI420:
	stb	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o4
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE420:
	.size	Divide__uDivide_param_u8_u8, .-Divide__uDivide_param_u8_u8
	.align 4
	.type	Divide__Divide_param_u8_u8, #function
	.proc	014
Divide__Divide_param_u8_u8:
.LLFB421:
	save	%sp, -96, %sp
.LLCFI421:
	stb	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	ldub	[%fp+72], %o5
	and	%o5, 0xff, %o5
	cmp	%o5, 0
	bl	%icc, .LL2393
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2392
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2396
	 nop
.LL2392:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2395
	 nop
.LL2393:
	cmp	%g1, 0
	bg	%icc, .LL2394
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2396
	 nop
.LL2394:
	add	%g1, -1, %g1
.LL2395:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2396:
	cmp	%g1, 255
	ble	%icc, .LL2397
	 nop
	sethi	%hi(14336), %g1
	or	%g1, 865, %o0
	call	_m3_fault, 0
	 nop
.LL2397:
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	ldub	[%fp+72], %o5
	and	%o5, 0xff, %o5
	cmp	%o5, 0
	bl	%icc, .LL2399
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2398
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2402
	 nop
.LL2398:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2401
	 nop
.LL2399:
	cmp	%g1, 0
	bg	%icc, .LL2400
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2402
	 nop
.LL2400:
	add	%g1, -1, %g1
.LL2401:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2402:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE421:
	.size	Divide__Divide_param_u8_u8, .-Divide__Divide_param_u8_u8
	.align 4
	.type	Divide__uDivide_var_u8_L, #function
	.proc	05
Divide__uDivide_var_u8_L:
.LLFB422:
	save	%sp, -96, %sp
.LLCFI422:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE422:
	.size	Divide__uDivide_var_u8_L, .-Divide__uDivide_var_u8_L
	.align 4
	.type	Divide__Divide_var_u8_L, #function
	.proc	05
Divide__Divide_var_u8_L:
.LLFB423:
	save	%sp, -96, %sp
.LLCFI423:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o2
	cmp	%o2, 0
	bl	%icc, .LL2406
	 nop
	cmp	%o2, 0
	bne	%icc, .LL2410
	 nop
	cmp	%o3, 0
.LL2410:
	cmp	%o4, 0
	bl	%icc, .LL2405
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2411
	 nop
	cmp	%o5, 0
.LL2411:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2409
	 nop
.LL2405:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2408
	 nop
.LL2406:
	cmp	%o4, 0
	bg	%icc, .LL2407
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2412
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2407
	 nop
	cmp	%o5, 0
.LL2412:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2409
	 nop
.LL2407:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2408:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2409:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE423:
	.size	Divide__Divide_var_u8_L, .-Divide__Divide_var_u8_L
	.align 4
	.type	Divide__uDivide_param_u8_L, #function
	.proc	05
Divide__uDivide_param_u8_L:
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
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE424:
	.size	Divide__uDivide_param_u8_L, .-Divide__uDivide_param_u8_L
	.align 4
	.type	Divide__Divide_param_u8_L, #function
	.proc	05
Divide__Divide_param_u8_L:
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
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL2416
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL2420
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL2420:
	cmp	%o4, 0
	bl	%icc, .LL2415
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2421
	 nop
	cmp	%o5, 0
.LL2421:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2419
	 nop
.LL2415:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2418
	 nop
.LL2416:
	cmp	%o4, 0
	bg	%icc, .LL2417
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2422
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2417
	 nop
	cmp	%o5, 0
.LL2422:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2419
	 nop
.LL2417:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2418:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2419:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE425:
	.size	Divide__Divide_param_u8_L, .-Divide__Divide_param_u8_L
	.align 4
	.type	Divide__uDivide_var_u8_i8, #function
	.proc	04
Divide__uDivide_var_u8_i8:
.LLFB426:
	save	%sp, -96, %sp
.LLCFI426:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE426:
	.size	Divide__uDivide_var_u8_i8, .-Divide__uDivide_var_u8_i8
	.align 4
	.type	Divide__Divide_var_u8_i8, #function
	.proc	014
Divide__Divide_var_u8_i8:
.LLFB427:
	save	%sp, -96, %sp
.LLCFI427:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+128], %o5
	sra	%o5, 24, %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	cmp	%o5, 0
	bl	%icc, .LL2426
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2425
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2429
	 nop
.LL2425:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2428
	 nop
.LL2426:
	cmp	%g1, 0
	bg	%icc, .LL2427
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2429
	 nop
.LL2427:
	add	%g1, -1, %g1
.LL2428:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2429:
	cmp	%g1, 0
	bl	%icc, .LL2430
	 nop
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+128], %o5
	sra	%o5, 24, %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	cmp	%o5, 0
	bl	%icc, .LL2432
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2431
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2435
	 nop
.LL2431:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2434
	 nop
.LL2432:
	cmp	%g1, 0
	bg	%icc, .LL2433
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2435
	 nop
.LL2433:
	add	%g1, -1, %g1
.LL2434:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2435:
	cmp	%g1, 255
	ble	%icc, .LL2436
	 nop
.LL2430:
	sethi	%hi(15360), %g1
	or	%g1, 33, %o0
	call	_m3_fault, 0
	 nop
.LL2436:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+128], %o5
	sra	%o5, 24, %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	cmp	%o5, 0
	bl	%icc, .LL2438
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2437
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2441
	 nop
.LL2437:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2440
	 nop
.LL2438:
	cmp	%g1, 0
	bg	%icc, .LL2439
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2441
	 nop
.LL2439:
	add	%g1, -1, %g1
.LL2440:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2441:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE427:
	.size	Divide__Divide_var_u8_i8, .-Divide__Divide_var_u8_i8
	.align 4
	.type	Divide__uDivide_param_u8_i8, #function
	.proc	04
Divide__uDivide_param_u8_i8:
.LLFB428:
	save	%sp, -96, %sp
.LLCFI428:
	stb	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o4
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE428:
	.size	Divide__uDivide_param_u8_i8, .-Divide__uDivide_param_u8_i8
	.align 4
	.type	Divide__Divide_param_u8_i8, #function
	.proc	014
Divide__Divide_param_u8_i8:
.LLFB429:
	save	%sp, -96, %sp
.LLCFI429:
	stb	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	ldub	[%fp+72], %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	cmp	%o5, 0
	bl	%icc, .LL2445
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2444
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2448
	 nop
.LL2444:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2447
	 nop
.LL2445:
	cmp	%g1, 0
	bg	%icc, .LL2446
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2448
	 nop
.LL2446:
	add	%g1, -1, %g1
.LL2447:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2448:
	cmp	%g1, 0
	bl	%icc, .LL2449
	 nop
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	ldub	[%fp+72], %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	cmp	%o5, 0
	bl	%icc, .LL2451
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2450
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2454
	 nop
.LL2450:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2453
	 nop
.LL2451:
	cmp	%g1, 0
	bg	%icc, .LL2452
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2454
	 nop
.LL2452:
	add	%g1, -1, %g1
.LL2453:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2454:
	cmp	%g1, 255
	ble	%icc, .LL2455
	 nop
.LL2449:
	sethi	%hi(15360), %g1
	or	%g1, 97, %o0
	call	_m3_fault, 0
	 nop
.LL2455:
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	ldub	[%fp+72], %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	cmp	%o5, 0
	bl	%icc, .LL2457
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2456
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2460
	 nop
.LL2456:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2459
	 nop
.LL2457:
	cmp	%g1, 0
	bg	%icc, .LL2458
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2460
	 nop
.LL2458:
	add	%g1, -1, %g1
.LL2459:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2460:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE429:
	.size	Divide__Divide_param_u8_i8, .-Divide__Divide_param_u8_i8
	.align 4
	.type	Divide__uDivide_var_u8_u32, #function
	.proc	04
Divide__uDivide_var_u8_u32:
.LLFB430:
	save	%sp, -96, %sp
.LLCFI430:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE430:
	.size	Divide__uDivide_var_u8_u32, .-Divide__uDivide_var_u8_u32
	.align 4
	.type	Divide__Divide_var_u8_u32, #function
	.proc	014
Divide__Divide_var_u8_u32:
.LLFB431:
	save	%sp, -96, %sp
.LLCFI431:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+132], %o5
	cmp	%o5, 0
	bl	%icc, .LL2464
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2463
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2467
	 nop
.LL2463:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2466
	 nop
.LL2464:
	cmp	%g1, 0
	bg	%icc, .LL2465
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2467
	 nop
.LL2465:
	add	%g1, -1, %g1
.LL2466:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2467:
	cmp	%g1, 0
	bl	%icc, .LL2468
	 nop
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+132], %o5
	cmp	%o5, 0
	bl	%icc, .LL2470
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2469
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2473
	 nop
.LL2469:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2472
	 nop
.LL2470:
	cmp	%g1, 0
	bg	%icc, .LL2471
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2473
	 nop
.LL2471:
	add	%g1, -1, %g1
.LL2472:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2473:
	cmp	%g1, 255
	ble	%icc, .LL2474
	 nop
.LL2468:
	sethi	%hi(15360), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2474:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+132], %o5
	cmp	%o5, 0
	bl	%icc, .LL2476
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2475
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2479
	 nop
.LL2475:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2478
	 nop
.LL2476:
	cmp	%g1, 0
	bg	%icc, .LL2477
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2479
	 nop
.LL2477:
	add	%g1, -1, %g1
.LL2478:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2479:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE431:
	.size	Divide__Divide_var_u8_u32, .-Divide__Divide_var_u8_u32
	.align 4
	.type	Divide__uDivide_param_u8_u32, #function
	.proc	04
Divide__uDivide_param_u8_u32:
.LLFB432:
	save	%sp, -96, %sp
.LLCFI432:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o4
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE432:
	.size	Divide__uDivide_param_u8_u32, .-Divide__uDivide_param_u8_u32
	.align 4
	.type	Divide__Divide_param_u8_u32, #function
	.proc	014
Divide__Divide_param_u8_u32:
.LLFB433:
	save	%sp, -96, %sp
.LLCFI433:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL2483
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2482
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2486
	 nop
.LL2482:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2485
	 nop
.LL2483:
	cmp	%g1, 0
	bg	%icc, .LL2484
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2486
	 nop
.LL2484:
	add	%g1, -1, %g1
.LL2485:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2486:
	cmp	%g1, 0
	bl	%icc, .LL2487
	 nop
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL2489
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2488
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2492
	 nop
.LL2488:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2491
	 nop
.LL2489:
	cmp	%g1, 0
	bg	%icc, .LL2490
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2492
	 nop
.LL2490:
	add	%g1, -1, %g1
.LL2491:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2492:
	cmp	%g1, 255
	ble	%icc, .LL2493
	 nop
.LL2487:
	sethi	%hi(15360), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL2493:
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL2495
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2494
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2498
	 nop
.LL2494:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2497
	 nop
.LL2495:
	cmp	%g1, 0
	bg	%icc, .LL2496
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2498
	 nop
.LL2496:
	add	%g1, -1, %g1
.LL2497:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2498:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE433:
	.size	Divide__Divide_param_u8_u32, .-Divide__Divide_param_u8_u32
	.align 4
	.type	Divide__uDivide_var_L_u16, #function
	.proc	05
Divide__uDivide_var_L_u16:
.LLFB434:
	save	%sp, -96, %sp
.LLCFI434:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE434:
	.size	Divide__uDivide_var_L_u16, .-Divide__uDivide_var_L_u16
	.align 4
	.type	Divide__Divide_var_L_u16, #function
	.proc	05
Divide__Divide_var_L_u16:
.LLFB435:
	save	%sp, -96, %sp
.LLCFI435:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	cmp	%o2, 0
	bl	%icc, .LL2502
	 nop
	cmp	%o2, 0
	bne	%icc, .LL2506
	 nop
	cmp	%o3, 0
.LL2506:
	cmp	%o4, 0
	bl	%icc, .LL2501
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2507
	 nop
	cmp	%o5, 0
.LL2507:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2505
	 nop
.LL2501:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2504
	 nop
.LL2502:
	cmp	%o4, 0
	bg	%icc, .LL2503
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2508
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2503
	 nop
	cmp	%o5, 0
.LL2508:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2505
	 nop
.LL2503:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2504:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2505:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE435:
	.size	Divide__Divide_var_L_u16, .-Divide__Divide_var_L_u16
	.align 4
	.type	Divide__uDivide_param_L_u16, #function
	.proc	05
Divide__uDivide_param_L_u16:
.LLFB436:
	save	%sp, -104, %sp
.LLCFI436:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE436:
	.size	Divide__uDivide_param_L_u16, .-Divide__uDivide_param_L_u16
	.align 4
	.type	Divide__Divide_param_L_u16, #function
	.proc	05
Divide__Divide_param_L_u16:
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
	cmp	%o2, 0
	bl	%icc, .LL2512
	 nop
	cmp	%o2, 0
	bne	%icc, .LL2516
	 nop
	cmp	%o3, 0
.LL2516:
	cmp	%o4, 0
	bl	%icc, .LL2511
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2517
	 nop
	cmp	%o5, 0
.LL2517:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2515
	 nop
.LL2511:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2514
	 nop
.LL2512:
	cmp	%o4, 0
	bg	%icc, .LL2513
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2518
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2513
	 nop
	cmp	%o5, 0
.LL2518:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2515
	 nop
.LL2513:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2514:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2515:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE437:
	.size	Divide__Divide_param_L_u16, .-Divide__Divide_param_L_u16
	.align 4
	.type	Divide__uDivide_var_L_u64, #function
	.proc	05
Divide__uDivide_var_L_u64:
.LLFB438:
	save	%sp, -96, %sp
.LLCFI438:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE438:
	.size	Divide__uDivide_var_L_u64, .-Divide__uDivide_var_L_u64
	.align 4
	.type	Divide__Divide_var_L_u64, #function
	.proc	05
Divide__Divide_var_L_u64:
.LLFB439:
	save	%sp, -96, %sp
.LLCFI439:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o2
	cmp	%o2, 0
	bl	%icc, .LL2522
	 nop
	cmp	%o2, 0
	bne	%icc, .LL2526
	 nop
	cmp	%o3, 0
.LL2526:
	cmp	%o4, 0
	bl	%icc, .LL2521
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2527
	 nop
	cmp	%o5, 0
.LL2527:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2525
	 nop
.LL2521:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2524
	 nop
.LL2522:
	cmp	%o4, 0
	bg	%icc, .LL2523
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2528
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2523
	 nop
	cmp	%o5, 0
.LL2528:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2525
	 nop
.LL2523:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2524:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2525:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE439:
	.size	Divide__Divide_var_L_u64, .-Divide__Divide_var_L_u64
	.align 4
	.type	Divide__uDivide_param_L_u64, #function
	.proc	05
Divide__uDivide_param_L_u64:
.LLFB440:
	save	%sp, -112, %sp
.LLCFI440:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE440:
	.size	Divide__uDivide_param_L_u64, .-Divide__uDivide_param_L_u64
	.align 4
	.type	Divide__Divide_param_L_u64, #function
	.proc	05
Divide__Divide_param_L_u64:
.LLFB441:
	save	%sp, -112, %sp
.LLCFI441:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bl	%icc, .LL2532
	 nop
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bne	%icc, .LL2536
	 nop
	ld	[%fp-12], %g1
	cmp	%g1, 0
.LL2536:
	cmp	%o4, 0
	bl	%icc, .LL2531
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2537
	 nop
	cmp	%o5, 0
.LL2537:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2535
	 nop
.LL2531:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2534
	 nop
.LL2532:
	cmp	%o4, 0
	bg	%icc, .LL2533
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2538
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2533
	 nop
	cmp	%o5, 0
.LL2538:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2535
	 nop
.LL2533:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2534:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2535:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE441:
	.size	Divide__Divide_param_L_u64, .-Divide__Divide_param_L_u64
	.align 4
	.type	Divide__uDivide_var_L_LC, #function
	.proc	05
Divide__uDivide_var_L_LC:
.LLFB442:
	save	%sp, -96, %sp
.LLCFI442:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE442:
	.size	Divide__uDivide_var_L_LC, .-Divide__uDivide_var_L_LC
	.align 4
	.type	Divide__Divide_var_L_LC, #function
	.proc	05
Divide__Divide_var_L_LC:
.LLFB443:
	save	%sp, -96, %sp
.LLCFI443:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o2
	cmp	%o2, 0
	bl	%icc, .LL2542
	 nop
	cmp	%o2, 0
	bne	%icc, .LL2546
	 nop
	cmp	%o3, 0
.LL2546:
	cmp	%o4, 0
	bl	%icc, .LL2541
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2547
	 nop
	cmp	%o5, 0
.LL2547:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2545
	 nop
.LL2541:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2544
	 nop
.LL2542:
	cmp	%o4, 0
	bg	%icc, .LL2543
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2548
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2543
	 nop
	cmp	%o5, 0
.LL2548:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2545
	 nop
.LL2543:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2544:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2545:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE443:
	.size	Divide__Divide_var_L_LC, .-Divide__Divide_var_L_LC
	.align 4
	.type	Divide__uDivide_param_L_LC, #function
	.proc	05
Divide__uDivide_param_L_LC:
.LLFB444:
	save	%sp, -112, %sp
.LLCFI444:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE444:
	.size	Divide__uDivide_param_L_LC, .-Divide__uDivide_param_L_LC
	.align 4
	.type	Divide__Divide_param_L_LC, #function
	.proc	05
Divide__Divide_param_L_LC:
.LLFB445:
	save	%sp, -112, %sp
.LLCFI445:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o2
	ldd	[%fp-8], %o4
	cmp	%o2, 0
	bl	%icc, .LL2552
	 nop
	cmp	%o2, 0
	bne	%icc, .LL2556
	 nop
	cmp	%o3, 0
.LL2556:
	cmp	%o4, 0
	bl	%icc, .LL2551
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2557
	 nop
	cmp	%o5, 0
.LL2557:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2555
	 nop
.LL2551:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2554
	 nop
.LL2552:
	cmp	%o4, 0
	bg	%icc, .LL2553
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2558
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2553
	 nop
	cmp	%o5, 0
.LL2558:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2555
	 nop
.LL2553:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2554:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2555:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE445:
	.size	Divide__Divide_param_L_LC, .-Divide__Divide_param_L_LC
	.align 4
	.type	Divide__uDivide_var_L_i32, #function
	.proc	05
Divide__uDivide_var_L_i32:
.LLFB446:
	save	%sp, -96, %sp
.LLCFI446:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o2
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE446:
	.size	Divide__uDivide_var_L_i32, .-Divide__uDivide_var_L_i32
	.align 4
	.type	Divide__Divide_var_L_i32, #function
	.proc	05
Divide__Divide_var_L_i32:
.LLFB447:
	save	%sp, -96, %sp
.LLCFI447:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o3
	sra	%g1, 31, %g1
	mov	%g1, %o2
	cmp	%o2, 0
	bl	%icc, .LL2562
	 nop
	cmp	%o2, 0
	bne	%icc, .LL2566
	 nop
	cmp	%o3, 0
.LL2566:
	cmp	%o4, 0
	bl	%icc, .LL2561
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2567
	 nop
	cmp	%o5, 0
.LL2567:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2565
	 nop
.LL2561:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2564
	 nop
.LL2562:
	cmp	%o4, 0
	bg	%icc, .LL2563
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2568
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2563
	 nop
	cmp	%o5, 0
.LL2568:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2565
	 nop
.LL2563:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2564:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2565:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE447:
	.size	Divide__Divide_var_L_i32, .-Divide__Divide_var_L_i32
	.align 4
	.type	Divide__uDivide_param_L_i32, #function
	.proc	05
Divide__uDivide_param_L_i32:
.LLFB448:
	save	%sp, -104, %sp
.LLCFI448:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE448:
	.size	Divide__uDivide_param_L_i32, .-Divide__uDivide_param_L_i32
	.align 4
	.type	Divide__Divide_param_L_i32, #function
	.proc	05
Divide__Divide_param_L_i32:
.LLFB449:
	save	%sp, -104, %sp
.LLCFI449:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o3
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o2
	ldd	[%fp-8], %o4
	cmp	%o2, 0
	bl	%icc, .LL2572
	 nop
	cmp	%o2, 0
	bne	%icc, .LL2576
	 nop
	cmp	%o3, 0
.LL2576:
	cmp	%o4, 0
	bl	%icc, .LL2571
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2577
	 nop
	cmp	%o5, 0
.LL2577:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2575
	 nop
.LL2571:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2574
	 nop
.LL2572:
	cmp	%o4, 0
	bg	%icc, .LL2573
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2578
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2573
	 nop
	cmp	%o5, 0
.LL2578:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2575
	 nop
.LL2573:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2574:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2575:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE449:
	.size	Divide__Divide_param_L_i32, .-Divide__Divide_param_L_i32
	.align 4
	.type	Divide__uDivide_var_L_i16, #function
	.proc	05
Divide__uDivide_var_L_i16:
.LLFB450:
	save	%sp, -96, %sp
.LLCFI450:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o2
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE450:
	.size	Divide__uDivide_var_L_i16, .-Divide__uDivide_var_L_i16
	.align 4
	.type	Divide__Divide_var_L_i16, #function
	.proc	05
Divide__Divide_var_L_i16:
.LLFB451:
	save	%sp, -96, %sp
.LLCFI451:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o3
	sra	%g1, 31, %g1
	mov	%g1, %o2
	cmp	%o2, 0
	bl	%icc, .LL2582
	 nop
	cmp	%o2, 0
	bne	%icc, .LL2586
	 nop
	cmp	%o3, 0
.LL2586:
	cmp	%o4, 0
	bl	%icc, .LL2581
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2587
	 nop
	cmp	%o5, 0
.LL2587:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2585
	 nop
.LL2581:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2584
	 nop
.LL2582:
	cmp	%o4, 0
	bg	%icc, .LL2583
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2588
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2583
	 nop
	cmp	%o5, 0
.LL2588:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2585
	 nop
.LL2583:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2584:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2585:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE451:
	.size	Divide__Divide_var_L_i16, .-Divide__Divide_var_L_i16
	.align 4
	.type	Divide__uDivide_param_L_i16, #function
	.proc	05
Divide__uDivide_param_L_i16:
.LLFB452:
	save	%sp, -104, %sp
.LLCFI452:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE452:
	.size	Divide__uDivide_param_L_i16, .-Divide__uDivide_param_L_i16
	.align 4
	.type	Divide__Divide_param_L_i16, #function
	.proc	05
Divide__Divide_param_L_i16:
.LLFB453:
	save	%sp, -104, %sp
.LLCFI453:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o3
	sra	%g1, 31, %g1
	mov	%g1, %o2
	ldd	[%fp-8], %o4
	cmp	%o2, 0
	bl	%icc, .LL2592
	 nop
	cmp	%o2, 0
	bne	%icc, .LL2596
	 nop
	cmp	%o3, 0
.LL2596:
	cmp	%o4, 0
	bl	%icc, .LL2591
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2597
	 nop
	cmp	%o5, 0
.LL2597:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2595
	 nop
.LL2591:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2594
	 nop
.LL2592:
	cmp	%o4, 0
	bg	%icc, .LL2593
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2598
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2593
	 nop
	cmp	%o5, 0
.LL2598:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2595
	 nop
.LL2593:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2594:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2595:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE453:
	.size	Divide__Divide_param_L_i16, .-Divide__Divide_param_L_i16
	.align 4
	.type	Divide__uDivide_var_L_I, #function
	.proc	05
Divide__uDivide_var_L_I:
.LLFB454:
	save	%sp, -96, %sp
.LLCFI454:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o2
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE454:
	.size	Divide__uDivide_var_L_I, .-Divide__uDivide_var_L_I
	.align 4
	.type	Divide__Divide_var_L_I, #function
	.proc	05
Divide__Divide_var_L_I:
.LLFB455:
	save	%sp, -96, %sp
.LLCFI455:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o3
	sra	%g1, 31, %g1
	mov	%g1, %o2
	cmp	%o2, 0
	bl	%icc, .LL2602
	 nop
	cmp	%o2, 0
	bne	%icc, .LL2606
	 nop
	cmp	%o3, 0
.LL2606:
	cmp	%o4, 0
	bl	%icc, .LL2601
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2607
	 nop
	cmp	%o5, 0
.LL2607:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2605
	 nop
.LL2601:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2604
	 nop
.LL2602:
	cmp	%o4, 0
	bg	%icc, .LL2603
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2608
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2603
	 nop
	cmp	%o5, 0
.LL2608:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2605
	 nop
.LL2603:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2604:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2605:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE455:
	.size	Divide__Divide_var_L_I, .-Divide__Divide_var_L_I
	.align 4
	.type	Divide__uDivide_param_L_I, #function
	.proc	05
Divide__uDivide_param_L_I:
.LLFB456:
	save	%sp, -104, %sp
.LLCFI456:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE456:
	.size	Divide__uDivide_param_L_I, .-Divide__uDivide_param_L_I
	.align 4
	.type	Divide__Divide_param_L_I, #function
	.proc	05
Divide__Divide_param_L_I:
.LLFB457:
	save	%sp, -104, %sp
.LLCFI457:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o3
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o2
	ldd	[%fp-8], %o4
	cmp	%o2, 0
	bl	%icc, .LL2612
	 nop
	cmp	%o2, 0
	bne	%icc, .LL2616
	 nop
	cmp	%o3, 0
.LL2616:
	cmp	%o4, 0
	bl	%icc, .LL2611
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2617
	 nop
	cmp	%o5, 0
.LL2617:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2615
	 nop
.LL2611:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2614
	 nop
.LL2612:
	cmp	%o4, 0
	bg	%icc, .LL2613
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2618
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2613
	 nop
	cmp	%o5, 0
.LL2618:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2615
	 nop
.LL2613:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2614:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2615:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE457:
	.size	Divide__Divide_param_L_I, .-Divide__Divide_param_L_I
	.align 4
	.type	Divide__uDivide_var_L_i64, #function
	.proc	05
Divide__uDivide_var_L_i64:
.LLFB458:
	save	%sp, -96, %sp
.LLCFI458:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE458:
	.size	Divide__uDivide_var_L_i64, .-Divide__uDivide_var_L_i64
	.align 4
	.type	Divide__Divide_var_L_i64, #function
	.proc	05
Divide__Divide_var_L_i64:
.LLFB459:
	save	%sp, -96, %sp
.LLCFI459:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o2
	cmp	%o2, 0
	bl	%icc, .LL2622
	 nop
	cmp	%o2, 0
	bne	%icc, .LL2626
	 nop
	cmp	%o3, 0
.LL2626:
	cmp	%o4, 0
	bl	%icc, .LL2621
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2627
	 nop
	cmp	%o5, 0
.LL2627:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2625
	 nop
.LL2621:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2624
	 nop
.LL2622:
	cmp	%o4, 0
	bg	%icc, .LL2623
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2628
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2623
	 nop
	cmp	%o5, 0
.LL2628:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2625
	 nop
.LL2623:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2624:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2625:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE459:
	.size	Divide__Divide_var_L_i64, .-Divide__Divide_var_L_i64
	.align 4
	.type	Divide__uDivide_param_L_i64, #function
	.proc	05
Divide__uDivide_param_L_i64:
.LLFB460:
	save	%sp, -112, %sp
.LLCFI460:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE460:
	.size	Divide__uDivide_param_L_i64, .-Divide__uDivide_param_L_i64
	.align 4
	.type	Divide__Divide_param_L_i64, #function
	.proc	05
Divide__Divide_param_L_i64:
.LLFB461:
	save	%sp, -112, %sp
.LLCFI461:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bl	%icc, .LL2632
	 nop
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bne	%icc, .LL2636
	 nop
	ld	[%fp-12], %g1
	cmp	%g1, 0
.LL2636:
	cmp	%o4, 0
	bl	%icc, .LL2631
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2637
	 nop
	cmp	%o5, 0
.LL2637:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2635
	 nop
.LL2631:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2634
	 nop
.LL2632:
	cmp	%o4, 0
	bg	%icc, .LL2633
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2638
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2633
	 nop
	cmp	%o5, 0
.LL2638:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2635
	 nop
.LL2633:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2634:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2635:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE461:
	.size	Divide__Divide_param_L_i64, .-Divide__Divide_param_L_i64
	.align 4
	.type	Divide__uDivide_var_L_C, #function
	.proc	05
Divide__uDivide_var_L_C:
.LLFB462:
	save	%sp, -96, %sp
.LLCFI462:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o2
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE462:
	.size	Divide__uDivide_var_L_C, .-Divide__uDivide_var_L_C
	.align 4
	.type	Divide__Divide_var_L_C, #function
	.proc	05
Divide__Divide_var_L_C:
.LLFB463:
	save	%sp, -96, %sp
.LLCFI463:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o3
	sra	%g1, 31, %g1
	mov	%g1, %o2
	cmp	%o2, 0
	bl	%icc, .LL2642
	 nop
	cmp	%o2, 0
	bne	%icc, .LL2646
	 nop
	cmp	%o3, 0
.LL2646:
	cmp	%o4, 0
	bl	%icc, .LL2641
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2647
	 nop
	cmp	%o5, 0
.LL2647:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2645
	 nop
.LL2641:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2644
	 nop
.LL2642:
	cmp	%o4, 0
	bg	%icc, .LL2643
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2648
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2643
	 nop
	cmp	%o5, 0
.LL2648:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2645
	 nop
.LL2643:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2644:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2645:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE463:
	.size	Divide__Divide_var_L_C, .-Divide__Divide_var_L_C
	.align 4
	.type	Divide__uDivide_param_L_C, #function
	.proc	05
Divide__uDivide_param_L_C:
.LLFB464:
	save	%sp, -104, %sp
.LLCFI464:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	ld	[%fp+76], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE464:
	.size	Divide__uDivide_param_L_C, .-Divide__uDivide_param_L_C
	.align 4
	.type	Divide__Divide_param_L_C, #function
	.proc	05
Divide__Divide_param_L_C:
.LLFB465:
	save	%sp, -104, %sp
.LLCFI465:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	mov	%g1, %o3
	sra	%g1, 31, %g1
	mov	%g1, %o2
	ldd	[%fp-8], %o4
	cmp	%o2, 0
	bl	%icc, .LL2652
	 nop
	cmp	%o2, 0
	bne	%icc, .LL2656
	 nop
	cmp	%o3, 0
.LL2656:
	cmp	%o4, 0
	bl	%icc, .LL2651
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2657
	 nop
	cmp	%o5, 0
.LL2657:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2655
	 nop
.LL2651:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2654
	 nop
.LL2652:
	cmp	%o4, 0
	bg	%icc, .LL2653
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2658
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2653
	 nop
	cmp	%o5, 0
.LL2658:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2655
	 nop
.LL2653:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2654:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2655:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE465:
	.size	Divide__Divide_param_L_C, .-Divide__Divide_param_L_C
	.align 4
	.type	Divide__uDivide_var_L_u8, #function
	.proc	05
Divide__uDivide_var_L_u8:
.LLFB466:
	save	%sp, -96, %sp
.LLCFI466:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE466:
	.size	Divide__uDivide_var_L_u8, .-Divide__uDivide_var_L_u8
	.align 4
	.type	Divide__Divide_var_L_u8, #function
	.proc	05
Divide__Divide_var_L_u8:
.LLFB467:
	save	%sp, -96, %sp
.LLCFI467:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	cmp	%o2, 0
	bl	%icc, .LL2662
	 nop
	cmp	%o2, 0
	bne	%icc, .LL2666
	 nop
	cmp	%o3, 0
.LL2666:
	cmp	%o4, 0
	bl	%icc, .LL2661
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2667
	 nop
	cmp	%o5, 0
.LL2667:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2665
	 nop
.LL2661:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2664
	 nop
.LL2662:
	cmp	%o4, 0
	bg	%icc, .LL2663
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2668
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2663
	 nop
	cmp	%o5, 0
.LL2668:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2665
	 nop
.LL2663:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2664:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2665:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE467:
	.size	Divide__Divide_var_L_u8, .-Divide__Divide_var_L_u8
	.align 4
	.type	Divide__uDivide_param_L_u8, #function
	.proc	05
Divide__uDivide_param_L_u8:
.LLFB468:
	save	%sp, -104, %sp
.LLCFI468:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	ldub	[%fp+76], %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE468:
	.size	Divide__uDivide_param_L_u8, .-Divide__uDivide_param_L_u8
	.align 4
	.type	Divide__Divide_param_L_u8, #function
	.proc	05
Divide__Divide_param_L_u8:
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
	cmp	%o2, 0
	bl	%icc, .LL2672
	 nop
	cmp	%o2, 0
	bne	%icc, .LL2676
	 nop
	cmp	%o3, 0
.LL2676:
	cmp	%o4, 0
	bl	%icc, .LL2671
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2677
	 nop
	cmp	%o5, 0
.LL2677:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2675
	 nop
.LL2671:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2674
	 nop
.LL2672:
	cmp	%o4, 0
	bg	%icc, .LL2673
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2678
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2673
	 nop
	cmp	%o5, 0
.LL2678:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2675
	 nop
.LL2673:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2674:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2675:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE469:
	.size	Divide__Divide_param_L_u8, .-Divide__Divide_param_L_u8
	.align 4
	.type	Divide__uDivide_var_L_L, #function
	.proc	05
Divide__uDivide_var_L_L:
.LLFB470:
	save	%sp, -96, %sp
.LLCFI470:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE470:
	.size	Divide__uDivide_var_L_L, .-Divide__uDivide_var_L_L
	.align 4
	.type	Divide__Divide_var_L_L, #function
	.proc	05
Divide__Divide_var_L_L:
.LLFB471:
	save	%sp, -96, %sp
.LLCFI471:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o2
	cmp	%o2, 0
	bl	%icc, .LL2682
	 nop
	cmp	%o2, 0
	bne	%icc, .LL2686
	 nop
	cmp	%o3, 0
.LL2686:
	cmp	%o4, 0
	bl	%icc, .LL2681
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2687
	 nop
	cmp	%o5, 0
.LL2687:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2685
	 nop
.LL2681:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2684
	 nop
.LL2682:
	cmp	%o4, 0
	bg	%icc, .LL2683
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2688
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2683
	 nop
	cmp	%o5, 0
.LL2688:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2685
	 nop
.LL2683:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2684:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2685:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE471:
	.size	Divide__Divide_var_L_L, .-Divide__Divide_var_L_L
	.align 4
	.type	Divide__uDivide_param_L_L, #function
	.proc	05
Divide__uDivide_param_L_L:
.LLFB472:
	save	%sp, -112, %sp
.LLCFI472:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE472:
	.size	Divide__uDivide_param_L_L, .-Divide__uDivide_param_L_L
	.align 4
	.type	Divide__Divide_param_L_L, #function
	.proc	05
Divide__Divide_param_L_L:
.LLFB473:
	save	%sp, -112, %sp
.LLCFI473:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bl	%icc, .LL2692
	 nop
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bne	%icc, .LL2696
	 nop
	ld	[%fp-12], %g1
	cmp	%g1, 0
.LL2696:
	cmp	%o4, 0
	bl	%icc, .LL2691
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2697
	 nop
	cmp	%o5, 0
.LL2697:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2695
	 nop
.LL2691:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2694
	 nop
.LL2692:
	cmp	%o4, 0
	bg	%icc, .LL2693
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2698
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2693
	 nop
	cmp	%o5, 0
.LL2698:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2695
	 nop
.LL2693:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2694:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2695:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE473:
	.size	Divide__Divide_param_L_L, .-Divide__Divide_param_L_L
	.align 4
	.type	Divide__uDivide_var_L_i8, #function
	.proc	05
Divide__uDivide_var_L_i8:
.LLFB474:
	save	%sp, -96, %sp
.LLCFI474:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o2
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE474:
	.size	Divide__uDivide_var_L_i8, .-Divide__uDivide_var_L_i8
	.align 4
	.type	Divide__Divide_var_L_i8, #function
	.proc	05
Divide__Divide_var_L_i8:
.LLFB475:
	save	%sp, -96, %sp
.LLCFI475:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o3
	sra	%g1, 31, %g1
	mov	%g1, %o2
	cmp	%o2, 0
	bl	%icc, .LL2702
	 nop
	cmp	%o2, 0
	bne	%icc, .LL2706
	 nop
	cmp	%o3, 0
.LL2706:
	cmp	%o4, 0
	bl	%icc, .LL2701
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2707
	 nop
	cmp	%o5, 0
.LL2707:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2705
	 nop
.LL2701:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2704
	 nop
.LL2702:
	cmp	%o4, 0
	bg	%icc, .LL2703
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2708
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2703
	 nop
	cmp	%o5, 0
.LL2708:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2705
	 nop
.LL2703:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2704:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2705:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE475:
	.size	Divide__Divide_var_L_i8, .-Divide__Divide_var_L_i8
	.align 4
	.type	Divide__uDivide_param_L_i8, #function
	.proc	05
Divide__uDivide_param_L_i8:
.LLFB476:
	save	%sp, -104, %sp
.LLCFI476:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	ldub	[%fp+76], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE476:
	.size	Divide__uDivide_param_L_i8, .-Divide__uDivide_param_L_i8
	.align 4
	.type	Divide__Divide_param_L_i8, #function
	.proc	05
Divide__Divide_param_L_i8:
.LLFB477:
	save	%sp, -104, %sp
.LLCFI477:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o3
	sra	%g1, 31, %g1
	mov	%g1, %o2
	ldd	[%fp-8], %o4
	cmp	%o2, 0
	bl	%icc, .LL2712
	 nop
	cmp	%o2, 0
	bne	%icc, .LL2716
	 nop
	cmp	%o3, 0
.LL2716:
	cmp	%o4, 0
	bl	%icc, .LL2711
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2717
	 nop
	cmp	%o5, 0
.LL2717:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2715
	 nop
.LL2711:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2714
	 nop
.LL2712:
	cmp	%o4, 0
	bg	%icc, .LL2713
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2718
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2713
	 nop
	cmp	%o5, 0
.LL2718:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2715
	 nop
.LL2713:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2714:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2715:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE477:
	.size	Divide__Divide_param_L_i8, .-Divide__Divide_param_L_i8
	.align 4
	.type	Divide__uDivide_var_L_u32, #function
	.proc	05
Divide__uDivide_var_L_u32:
.LLFB478:
	save	%sp, -96, %sp
.LLCFI478:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o2
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE478:
	.size	Divide__uDivide_var_L_u32, .-Divide__uDivide_var_L_u32
	.align 4
	.type	Divide__Divide_var_L_u32, #function
	.proc	05
Divide__Divide_var_L_u32:
.LLFB479:
	save	%sp, -96, %sp
.LLCFI479:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o3
	sra	%g1, 31, %g1
	mov	%g1, %o2
	cmp	%o2, 0
	bl	%icc, .LL2722
	 nop
	cmp	%o2, 0
	bne	%icc, .LL2726
	 nop
	cmp	%o3, 0
.LL2726:
	cmp	%o4, 0
	bl	%icc, .LL2721
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2727
	 nop
	cmp	%o5, 0
.LL2727:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2725
	 nop
.LL2721:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2724
	 nop
.LL2722:
	cmp	%o4, 0
	bg	%icc, .LL2723
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2728
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2723
	 nop
	cmp	%o5, 0
.LL2728:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2725
	 nop
.LL2723:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2724:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2725:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE479:
	.size	Divide__Divide_var_L_u32, .-Divide__Divide_var_L_u32
	.align 4
	.type	Divide__uDivide_param_L_u32, #function
	.proc	05
Divide__uDivide_param_L_u32:
.LLFB480:
	save	%sp, -104, %sp
.LLCFI480:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE480:
	.size	Divide__uDivide_param_L_u32, .-Divide__uDivide_param_L_u32
	.align 4
	.type	Divide__Divide_param_L_u32, #function
	.proc	05
Divide__Divide_param_L_u32:
.LLFB481:
	save	%sp, -104, %sp
.LLCFI481:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o3
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o2
	ldd	[%fp-8], %o4
	cmp	%o2, 0
	bl	%icc, .LL2732
	 nop
	cmp	%o2, 0
	bne	%icc, .LL2736
	 nop
	cmp	%o3, 0
.LL2736:
	cmp	%o4, 0
	bl	%icc, .LL2731
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2737
	 nop
	cmp	%o5, 0
.LL2737:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2735
	 nop
.LL2731:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2734
	 nop
.LL2732:
	cmp	%o4, 0
	bg	%icc, .LL2733
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2738
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2733
	 nop
	cmp	%o5, 0
.LL2738:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2735
	 nop
.LL2733:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2734:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2735:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE481:
	.size	Divide__Divide_param_L_u32, .-Divide__Divide_param_L_u32
	.align 4
	.type	Divide__uDivide_var_i8_u16, #function
	.proc	04
Divide__uDivide_var_i8_u16:
.LLFB482:
	save	%sp, -96, %sp
.LLCFI482:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE482:
	.size	Divide__uDivide_var_i8_u16, .-Divide__uDivide_var_i8_u16
	.align 4
	.type	Divide__Divide_var_i8_u16, #function
	.proc	02
Divide__Divide_var_i8_u16:
.LLFB483:
	save	%sp, -96, %sp
.LLCFI483:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+52], %o5
	srl	%o5, 16, %o5
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL2742
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2741
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2745
	 nop
.LL2741:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2744
	 nop
.LL2742:
	cmp	%g1, 0
	bg	%icc, .LL2743
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2745
	 nop
.LL2743:
	add	%g1, -1, %g1
.LL2744:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2745:
	cmp	%g1, -128
	bl	%icc, .LL2746
	 nop
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+52], %o5
	srl	%o5, 16, %o5
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL2748
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2747
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2751
	 nop
.LL2747:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2750
	 nop
.LL2748:
	cmp	%g1, 0
	bg	%icc, .LL2749
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2751
	 nop
.LL2749:
	add	%g1, -1, %g1
.LL2750:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2751:
	cmp	%g1, 127
	ble	%icc, .LL2752
	 nop
.LL2746:
	sethi	%hi(16384), %g1
	or	%g1, 801, %o0
	call	_m3_fault, 0
	 nop
.LL2752:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+52], %o5
	srl	%o5, 16, %o5
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL2754
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2753
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2757
	 nop
.LL2753:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2756
	 nop
.LL2754:
	cmp	%g1, 0
	bg	%icc, .LL2755
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2757
	 nop
.LL2755:
	add	%g1, -1, %g1
.LL2756:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2757:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE483:
	.size	Divide__Divide_var_i8_u16, .-Divide__Divide_var_i8_u16
	.align 4
	.type	Divide__uDivide_param_i8_u16, #function
	.proc	04
Divide__uDivide_param_i8_u16:
.LLFB484:
	save	%sp, -96, %sp
.LLCFI484:
	stb	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE484:
	.size	Divide__uDivide_param_i8_u16, .-Divide__uDivide_param_i8_u16
	.align 4
	.type	Divide__Divide_param_i8_u16, #function
	.proc	02
Divide__Divide_param_i8_u16:
.LLFB485:
	save	%sp, -96, %sp
.LLCFI485:
	stb	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	lduh	[%fp+72], %o5
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL2761
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2760
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2764
	 nop
.LL2760:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2763
	 nop
.LL2761:
	cmp	%g1, 0
	bg	%icc, .LL2762
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2764
	 nop
.LL2762:
	add	%g1, -1, %g1
.LL2763:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2764:
	cmp	%g1, -128
	bl	%icc, .LL2765
	 nop
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	lduh	[%fp+72], %o5
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL2767
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2766
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2770
	 nop
.LL2766:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2769
	 nop
.LL2767:
	cmp	%g1, 0
	bg	%icc, .LL2768
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2770
	 nop
.LL2768:
	add	%g1, -1, %g1
.LL2769:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2770:
	cmp	%g1, 127
	ble	%icc, .LL2771
	 nop
.LL2765:
	sethi	%hi(16384), %g1
	or	%g1, 865, %o0
	call	_m3_fault, 0
	 nop
.LL2771:
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	lduh	[%fp+72], %o5
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL2773
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2772
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2776
	 nop
.LL2772:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2775
	 nop
.LL2773:
	cmp	%g1, 0
	bg	%icc, .LL2774
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2776
	 nop
.LL2774:
	add	%g1, -1, %g1
.LL2775:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2776:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE485:
	.size	Divide__Divide_param_i8_u16, .-Divide__Divide_param_i8_u16
	.align 4
	.type	Divide__uDivide_var_i8_u64, #function
	.proc	05
Divide__uDivide_var_i8_u64:
.LLFB486:
	save	%sp, -96, %sp
.LLCFI486:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE486:
	.size	Divide__uDivide_var_i8_u64, .-Divide__uDivide_var_i8_u64
	.align 4
	.type	Divide__Divide_var_i8_u64, #function
	.proc	05
Divide__Divide_var_i8_u64:
.LLFB487:
	save	%sp, -96, %sp
.LLCFI487:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o2
	cmp	%o2, 0
	bl	%icc, .LL2780
	 nop
	cmp	%o2, 0
	bne	%icc, .LL2784
	 nop
	cmp	%o3, 0
.LL2784:
	cmp	%o4, 0
	bl	%icc, .LL2779
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2785
	 nop
	cmp	%o5, 0
.LL2785:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2783
	 nop
.LL2779:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2782
	 nop
.LL2780:
	cmp	%o4, 0
	bg	%icc, .LL2781
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2786
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2781
	 nop
	cmp	%o5, 0
.LL2786:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2783
	 nop
.LL2781:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2782:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2783:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE487:
	.size	Divide__Divide_var_i8_u64, .-Divide__Divide_var_i8_u64
	.align 4
	.type	Divide__uDivide_param_i8_u64, #function
	.proc	05
Divide__uDivide_param_i8_u64:
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
	mov	%o4, %o0
	mov	%o5, %o1
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE488:
	.size	Divide__uDivide_param_i8_u64, .-Divide__uDivide_param_i8_u64
	.align 4
	.type	Divide__Divide_param_i8_u64, #function
	.proc	05
Divide__Divide_param_i8_u64:
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
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL2790
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL2794
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL2794:
	cmp	%o4, 0
	bl	%icc, .LL2789
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2795
	 nop
	cmp	%o5, 0
.LL2795:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2793
	 nop
.LL2789:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2792
	 nop
.LL2790:
	cmp	%o4, 0
	bg	%icc, .LL2791
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2796
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2791
	 nop
	cmp	%o5, 0
.LL2796:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2793
	 nop
.LL2791:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2792:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2793:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE489:
	.size	Divide__Divide_param_i8_u64, .-Divide__Divide_param_i8_u64
	.align 4
	.type	Divide__uDivide_var_i8_LC, #function
	.proc	05
Divide__uDivide_var_i8_LC:
.LLFB490:
	save	%sp, -96, %sp
.LLCFI490:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE490:
	.size	Divide__uDivide_var_i8_LC, .-Divide__uDivide_var_i8_LC
	.align 4
	.type	Divide__Divide_var_i8_LC, #function
	.proc	05
Divide__Divide_var_i8_LC:
.LLFB491:
	save	%sp, -96, %sp
.LLCFI491:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o2
	cmp	%o2, 0
	bl	%icc, .LL2800
	 nop
	cmp	%o2, 0
	bne	%icc, .LL2804
	 nop
	cmp	%o3, 0
.LL2804:
	cmp	%o4, 0
	bl	%icc, .LL2799
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2805
	 nop
	cmp	%o5, 0
.LL2805:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2803
	 nop
.LL2799:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2802
	 nop
.LL2800:
	cmp	%o4, 0
	bg	%icc, .LL2801
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2806
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2801
	 nop
	cmp	%o5, 0
.LL2806:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2803
	 nop
.LL2801:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2802:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2803:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE491:
	.size	Divide__Divide_var_i8_LC, .-Divide__Divide_var_i8_LC
	.align 4
	.type	Divide__uDivide_param_i8_LC, #function
	.proc	05
Divide__uDivide_param_i8_LC:
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
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE492:
	.size	Divide__uDivide_param_i8_LC, .-Divide__uDivide_param_i8_LC
	.align 4
	.type	Divide__Divide_param_i8_LC, #function
	.proc	05
Divide__Divide_param_i8_LC:
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
	cmp	%o2, 0
	bl	%icc, .LL2810
	 nop
	cmp	%o2, 0
	bne	%icc, .LL2814
	 nop
	cmp	%o3, 0
.LL2814:
	cmp	%o4, 0
	bl	%icc, .LL2809
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2815
	 nop
	cmp	%o5, 0
.LL2815:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2813
	 nop
.LL2809:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2812
	 nop
.LL2810:
	cmp	%o4, 0
	bg	%icc, .LL2811
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2816
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2811
	 nop
	cmp	%o5, 0
.LL2816:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2813
	 nop
.LL2811:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2812:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2813:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE493:
	.size	Divide__Divide_param_i8_LC, .-Divide__Divide_param_i8_LC
	.align 4
	.type	Divide__uDivide_var_i8_i32, #function
	.proc	04
Divide__uDivide_var_i8_i32:
.LLFB494:
	save	%sp, -96, %sp
.LLCFI494:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE494:
	.size	Divide__uDivide_var_i8_i32, .-Divide__uDivide_var_i8_i32
	.align 4
	.type	Divide__Divide_var_i8_i32, #function
	.proc	02
Divide__Divide_var_i8_i32:
.LLFB495:
	save	%sp, -96, %sp
.LLCFI495:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL2820
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2819
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2823
	 nop
.LL2819:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2822
	 nop
.LL2820:
	cmp	%g1, 0
	bg	%icc, .LL2821
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2823
	 nop
.LL2821:
	add	%g1, -1, %g1
.LL2822:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2823:
	cmp	%g1, -128
	bl	%icc, .LL2824
	 nop
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL2826
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2825
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2829
	 nop
.LL2825:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2828
	 nop
.LL2826:
	cmp	%g1, 0
	bg	%icc, .LL2827
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2829
	 nop
.LL2827:
	add	%g1, -1, %g1
.LL2828:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2829:
	cmp	%g1, 127
	ble	%icc, .LL2830
	 nop
.LL2824:
	sethi	%hi(17408), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2830:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL2832
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2831
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2835
	 nop
.LL2831:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2834
	 nop
.LL2832:
	cmp	%g1, 0
	bg	%icc, .LL2833
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2835
	 nop
.LL2833:
	add	%g1, -1, %g1
.LL2834:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2835:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE495:
	.size	Divide__Divide_var_i8_i32, .-Divide__Divide_var_i8_i32
	.align 4
	.type	Divide__uDivide_param_i8_i32, #function
	.proc	04
Divide__uDivide_param_i8_i32:
.LLFB496:
	save	%sp, -96, %sp
.LLCFI496:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE496:
	.size	Divide__uDivide_param_i8_i32, .-Divide__uDivide_param_i8_i32
	.align 4
	.type	Divide__Divide_param_i8_i32, #function
	.proc	02
Divide__Divide_param_i8_i32:
.LLFB497:
	save	%sp, -96, %sp
.LLCFI497:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL2839
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2838
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2842
	 nop
.LL2838:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2841
	 nop
.LL2839:
	cmp	%g1, 0
	bg	%icc, .LL2840
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2842
	 nop
.LL2840:
	add	%g1, -1, %g1
.LL2841:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2842:
	cmp	%g1, -128
	bl	%icc, .LL2843
	 nop
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL2845
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2844
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2848
	 nop
.LL2844:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2847
	 nop
.LL2845:
	cmp	%g1, 0
	bg	%icc, .LL2846
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2848
	 nop
.LL2846:
	add	%g1, -1, %g1
.LL2847:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2848:
	cmp	%g1, 127
	ble	%icc, .LL2849
	 nop
.LL2843:
	sethi	%hi(17408), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL2849:
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL2851
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2850
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2854
	 nop
.LL2850:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2853
	 nop
.LL2851:
	cmp	%g1, 0
	bg	%icc, .LL2852
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2854
	 nop
.LL2852:
	add	%g1, -1, %g1
.LL2853:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2854:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE497:
	.size	Divide__Divide_param_i8_i32, .-Divide__Divide_param_i8_i32
	.align 4
	.type	Divide__uDivide_var_i8_i16, #function
	.proc	04
Divide__uDivide_var_i8_i16:
.LLFB498:
	save	%sp, -96, %sp
.LLCFI498:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE498:
	.size	Divide__uDivide_var_i8_i16, .-Divide__uDivide_var_i8_i16
	.align 4
	.type	Divide__Divide_var_i8_i16, #function
	.proc	02
Divide__Divide_var_i8_i16:
.LLFB499:
	save	%sp, -96, %sp
.LLCFI499:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+76], %o5
	sra	%o5, 16, %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL2858
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2857
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2861
	 nop
.LL2857:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2860
	 nop
.LL2858:
	cmp	%g1, 0
	bg	%icc, .LL2859
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2861
	 nop
.LL2859:
	add	%g1, -1, %g1
.LL2860:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2861:
	cmp	%g1, -128
	bl	%icc, .LL2862
	 nop
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+76], %o5
	sra	%o5, 16, %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL2864
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2863
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2867
	 nop
.LL2863:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2866
	 nop
.LL2864:
	cmp	%g1, 0
	bg	%icc, .LL2865
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2867
	 nop
.LL2865:
	add	%g1, -1, %g1
.LL2866:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2867:
	cmp	%g1, 127
	ble	%icc, .LL2868
	 nop
.LL2862:
	sethi	%hi(17408), %g1
	or	%g1, 289, %o0
	call	_m3_fault, 0
	 nop
.LL2868:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+76], %o5
	sra	%o5, 16, %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL2870
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2869
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2873
	 nop
.LL2869:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2872
	 nop
.LL2870:
	cmp	%g1, 0
	bg	%icc, .LL2871
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2873
	 nop
.LL2871:
	add	%g1, -1, %g1
.LL2872:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2873:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE499:
	.size	Divide__Divide_var_i8_i16, .-Divide__Divide_var_i8_i16
	.align 4
	.type	Divide__uDivide_param_i8_i16, #function
	.proc	04
Divide__uDivide_param_i8_i16:
.LLFB500:
	save	%sp, -96, %sp
.LLCFI500:
	stb	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE500:
	.size	Divide__uDivide_param_i8_i16, .-Divide__uDivide_param_i8_i16
	.align 4
	.type	Divide__Divide_param_i8_i16, #function
	.proc	02
Divide__Divide_param_i8_i16:
.LLFB501:
	save	%sp, -96, %sp
.LLCFI501:
	stb	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	lduh	[%fp+72], %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL2877
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2876
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2880
	 nop
.LL2876:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2879
	 nop
.LL2877:
	cmp	%g1, 0
	bg	%icc, .LL2878
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2880
	 nop
.LL2878:
	add	%g1, -1, %g1
.LL2879:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2880:
	cmp	%g1, -128
	bl	%icc, .LL2881
	 nop
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	lduh	[%fp+72], %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL2883
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2882
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2886
	 nop
.LL2882:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2885
	 nop
.LL2883:
	cmp	%g1, 0
	bg	%icc, .LL2884
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2886
	 nop
.LL2884:
	add	%g1, -1, %g1
.LL2885:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2886:
	cmp	%g1, 127
	ble	%icc, .LL2887
	 nop
.LL2881:
	sethi	%hi(17408), %g1
	or	%g1, 353, %o0
	call	_m3_fault, 0
	 nop
.LL2887:
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	lduh	[%fp+72], %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL2889
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2888
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2892
	 nop
.LL2888:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2891
	 nop
.LL2889:
	cmp	%g1, 0
	bg	%icc, .LL2890
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2892
	 nop
.LL2890:
	add	%g1, -1, %g1
.LL2891:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2892:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE501:
	.size	Divide__Divide_param_i8_i16, .-Divide__Divide_param_i8_i16
	.align 4
	.type	Divide__uDivide_var_i8_I, #function
	.proc	04
Divide__uDivide_var_i8_I:
.LLFB502:
	save	%sp, -96, %sp
.LLCFI502:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE502:
	.size	Divide__uDivide_var_i8_I, .-Divide__uDivide_var_i8_I
	.align 4
	.type	Divide__Divide_var_i8_I, #function
	.proc	02
Divide__Divide_var_i8_I:
.LLFB503:
	save	%sp, -96, %sp
.LLCFI503:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+80], %o5
	cmp	%o5, 0
	bl	%icc, .LL2896
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2895
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2899
	 nop
.LL2895:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2898
	 nop
.LL2896:
	cmp	%g1, 0
	bg	%icc, .LL2897
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2899
	 nop
.LL2897:
	add	%g1, -1, %g1
.LL2898:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2899:
	cmp	%g1, -128
	bl	%icc, .LL2900
	 nop
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+80], %o5
	cmp	%o5, 0
	bl	%icc, .LL2902
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2901
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2905
	 nop
.LL2901:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2904
	 nop
.LL2902:
	cmp	%g1, 0
	bg	%icc, .LL2903
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2905
	 nop
.LL2903:
	add	%g1, -1, %g1
.LL2904:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2905:
	cmp	%g1, 127
	ble	%icc, .LL2906
	 nop
.LL2900:
	sethi	%hi(17408), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2906:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+80], %o5
	cmp	%o5, 0
	bl	%icc, .LL2908
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2907
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2911
	 nop
.LL2907:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2910
	 nop
.LL2908:
	cmp	%g1, 0
	bg	%icc, .LL2909
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2911
	 nop
.LL2909:
	add	%g1, -1, %g1
.LL2910:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2911:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE503:
	.size	Divide__Divide_var_i8_I, .-Divide__Divide_var_i8_I
	.align 4
	.type	Divide__uDivide_param_i8_I, #function
	.proc	04
Divide__uDivide_param_i8_I:
.LLFB504:
	save	%sp, -96, %sp
.LLCFI504:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE504:
	.size	Divide__uDivide_param_i8_I, .-Divide__uDivide_param_i8_I
	.align 4
	.type	Divide__Divide_param_i8_I, #function
	.proc	02
Divide__Divide_param_i8_I:
.LLFB505:
	save	%sp, -96, %sp
.LLCFI505:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL2915
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2914
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2918
	 nop
.LL2914:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2917
	 nop
.LL2915:
	cmp	%g1, 0
	bg	%icc, .LL2916
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2918
	 nop
.LL2916:
	add	%g1, -1, %g1
.LL2917:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2918:
	cmp	%g1, -128
	bl	%icc, .LL2919
	 nop
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL2921
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2920
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2924
	 nop
.LL2920:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2923
	 nop
.LL2921:
	cmp	%g1, 0
	bg	%icc, .LL2922
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2924
	 nop
.LL2922:
	add	%g1, -1, %g1
.LL2923:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2924:
	cmp	%g1, 127
	ble	%icc, .LL2925
	 nop
.LL2919:
	sethi	%hi(17408), %g1
	or	%g1, 481, %o0
	call	_m3_fault, 0
	 nop
.LL2925:
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL2927
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2926
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2930
	 nop
.LL2926:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2929
	 nop
.LL2927:
	cmp	%g1, 0
	bg	%icc, .LL2928
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2930
	 nop
.LL2928:
	add	%g1, -1, %g1
.LL2929:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2930:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE505:
	.size	Divide__Divide_param_i8_I, .-Divide__Divide_param_i8_I
	.align 4
	.type	Divide__uDivide_var_i8_i64, #function
	.proc	05
Divide__uDivide_var_i8_i64:
.LLFB506:
	save	%sp, -96, %sp
.LLCFI506:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE506:
	.size	Divide__uDivide_var_i8_i64, .-Divide__uDivide_var_i8_i64
	.align 4
	.type	Divide__Divide_var_i8_i64, #function
	.proc	05
Divide__Divide_var_i8_i64:
.LLFB507:
	save	%sp, -96, %sp
.LLCFI507:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o2
	cmp	%o2, 0
	bl	%icc, .LL2934
	 nop
	cmp	%o2, 0
	bne	%icc, .LL2938
	 nop
	cmp	%o3, 0
.LL2938:
	cmp	%o4, 0
	bl	%icc, .LL2933
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2939
	 nop
	cmp	%o5, 0
.LL2939:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2937
	 nop
.LL2933:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2936
	 nop
.LL2934:
	cmp	%o4, 0
	bg	%icc, .LL2935
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2940
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2935
	 nop
	cmp	%o5, 0
.LL2940:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2937
	 nop
.LL2935:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2936:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2937:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE507:
	.size	Divide__Divide_var_i8_i64, .-Divide__Divide_var_i8_i64
	.align 4
	.type	Divide__uDivide_param_i8_i64, #function
	.proc	05
Divide__uDivide_param_i8_i64:
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
	mov	%o4, %o0
	mov	%o5, %o1
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE508:
	.size	Divide__uDivide_param_i8_i64, .-Divide__uDivide_param_i8_i64
	.align 4
	.type	Divide__Divide_param_i8_i64, #function
	.proc	05
Divide__Divide_param_i8_i64:
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
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL2944
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL2948
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL2948:
	cmp	%o4, 0
	bl	%icc, .LL2943
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2949
	 nop
	cmp	%o5, 0
.LL2949:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2947
	 nop
.LL2943:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2946
	 nop
.LL2944:
	cmp	%o4, 0
	bg	%icc, .LL2945
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2950
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2945
	 nop
	cmp	%o5, 0
.LL2950:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2947
	 nop
.LL2945:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2946:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2947:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE509:
	.size	Divide__Divide_param_i8_i64, .-Divide__Divide_param_i8_i64
	.align 4
	.type	Divide__uDivide_var_i8_C, #function
	.proc	04
Divide__uDivide_var_i8_C:
.LLFB510:
	save	%sp, -96, %sp
.LLCFI510:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE510:
	.size	Divide__uDivide_var_i8_C, .-Divide__uDivide_var_i8_C
	.align 4
	.type	Divide__Divide_var_i8_C, #function
	.proc	02
Divide__Divide_var_i8_C:
.LLFB511:
	save	%sp, -96, %sp
.LLCFI511:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+96], %o5
	cmp	%o5, 0
	bl	%icc, .LL2954
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2953
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2957
	 nop
.LL2953:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2956
	 nop
.LL2954:
	cmp	%g1, 0
	bg	%icc, .LL2955
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2957
	 nop
.LL2955:
	add	%g1, -1, %g1
.LL2956:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2957:
	cmp	%g1, -128
	bl	%icc, .LL2958
	 nop
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+96], %o5
	cmp	%o5, 0
	bl	%icc, .LL2960
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2959
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2963
	 nop
.LL2959:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2962
	 nop
.LL2960:
	cmp	%g1, 0
	bg	%icc, .LL2961
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2963
	 nop
.LL2961:
	add	%g1, -1, %g1
.LL2962:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2963:
	cmp	%g1, 127
	ble	%icc, .LL2964
	 nop
.LL2958:
	sethi	%hi(17408), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2964:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+96], %o5
	cmp	%o5, 0
	bl	%icc, .LL2966
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2965
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2969
	 nop
.LL2965:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2968
	 nop
.LL2966:
	cmp	%g1, 0
	bg	%icc, .LL2967
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2969
	 nop
.LL2967:
	add	%g1, -1, %g1
.LL2968:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2969:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE511:
	.size	Divide__Divide_var_i8_C, .-Divide__Divide_var_i8_C
	.align 4
	.type	Divide__uDivide_param_i8_C, #function
	.proc	04
Divide__uDivide_param_i8_C:
.LLFB512:
	save	%sp, -96, %sp
.LLCFI512:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	wr	%g0, 0, %y
	ld	[%fp+72], %g1
	udiv	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE512:
	.size	Divide__uDivide_param_i8_C, .-Divide__uDivide_param_i8_C
	.align 4
	.type	Divide__Divide_param_i8_C, #function
	.proc	02
Divide__Divide_param_i8_C:
.LLFB513:
	save	%sp, -96, %sp
.LLCFI513:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL2973
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2972
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2976
	 nop
.LL2972:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2975
	 nop
.LL2973:
	cmp	%g1, 0
	bg	%icc, .LL2974
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2976
	 nop
.LL2974:
	add	%g1, -1, %g1
.LL2975:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2976:
	cmp	%g1, -128
	bl	%icc, .LL2977
	 nop
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL2979
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2978
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2982
	 nop
.LL2978:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2981
	 nop
.LL2979:
	cmp	%g1, 0
	bg	%icc, .LL2980
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2982
	 nop
.LL2980:
	add	%g1, -1, %g1
.LL2981:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2982:
	cmp	%g1, 127
	ble	%icc, .LL2983
	 nop
.LL2977:
	sethi	%hi(17408), %g1
	or	%g1, 737, %o0
	call	_m3_fault, 0
	 nop
.LL2983:
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL2985
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2984
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2988
	 nop
.LL2984:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2987
	 nop
.LL2985:
	cmp	%g1, 0
	bg	%icc, .LL2986
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2988
	 nop
.LL2986:
	add	%g1, -1, %g1
.LL2987:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2988:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE513:
	.size	Divide__Divide_param_i8_C, .-Divide__Divide_param_i8_C
	.align 4
	.type	Divide__uDivide_var_i8_u8, #function
	.proc	04
Divide__uDivide_var_i8_u8:
.LLFB514:
	save	%sp, -96, %sp
.LLCFI514:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE514:
	.size	Divide__uDivide_var_i8_u8, .-Divide__uDivide_var_i8_u8
	.align 4
	.type	Divide__Divide_var_i8_u8, #function
	.proc	02
Divide__Divide_var_i8_u8:
.LLFB515:
	save	%sp, -96, %sp
.LLCFI515:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+112], %o5
	srl	%o5, 24, %o5
	and	%o5, 0xff, %o5
	cmp	%o5, 0
	bl	%icc, .LL2992
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2991
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2995
	 nop
.LL2991:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2994
	 nop
.LL2992:
	cmp	%g1, 0
	bg	%icc, .LL2993
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2995
	 nop
.LL2993:
	add	%g1, -1, %g1
.LL2994:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2995:
	cmp	%g1, -128
	bl	%icc, .LL2996
	 nop
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+112], %o5
	srl	%o5, 24, %o5
	and	%o5, 0xff, %o5
	cmp	%o5, 0
	bl	%icc, .LL2998
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2997
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3001
	 nop
.LL2997:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL3000
	 nop
.LL2998:
	cmp	%g1, 0
	bg	%icc, .LL2999
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3001
	 nop
.LL2999:
	add	%g1, -1, %g1
.LL3000:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL3001:
	cmp	%g1, 127
	ble	%icc, .LL3002
	 nop
.LL2996:
	sethi	%hi(17408), %g1
	or	%g1, 801, %o0
	call	_m3_fault, 0
	 nop
.LL3002:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+112], %o5
	srl	%o5, 24, %o5
	and	%o5, 0xff, %o5
	cmp	%o5, 0
	bl	%icc, .LL3004
	 nop
	cmp	%g1, 0
	bl	%icc, .LL3003
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3007
	 nop
.LL3003:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL3006
	 nop
.LL3004:
	cmp	%g1, 0
	bg	%icc, .LL3005
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3007
	 nop
.LL3005:
	add	%g1, -1, %g1
.LL3006:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL3007:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE515:
	.size	Divide__Divide_var_i8_u8, .-Divide__Divide_var_i8_u8
	.align 4
	.type	Divide__uDivide_param_i8_u8, #function
	.proc	04
Divide__uDivide_param_i8_u8:
.LLFB516:
	save	%sp, -96, %sp
.LLCFI516:
	stb	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE516:
	.size	Divide__uDivide_param_i8_u8, .-Divide__uDivide_param_i8_u8
	.align 4
	.type	Divide__Divide_param_i8_u8, #function
	.proc	02
Divide__Divide_param_i8_u8:
.LLFB517:
	save	%sp, -96, %sp
.LLCFI517:
	stb	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	ldub	[%fp+72], %o5
	and	%o5, 0xff, %o5
	cmp	%o5, 0
	bl	%icc, .LL3011
	 nop
	cmp	%g1, 0
	bl	%icc, .LL3010
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3014
	 nop
.LL3010:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL3013
	 nop
.LL3011:
	cmp	%g1, 0
	bg	%icc, .LL3012
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3014
	 nop
.LL3012:
	add	%g1, -1, %g1
.LL3013:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL3014:
	cmp	%g1, -128
	bl	%icc, .LL3015
	 nop
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	ldub	[%fp+72], %o5
	and	%o5, 0xff, %o5
	cmp	%o5, 0
	bl	%icc, .LL3017
	 nop
	cmp	%g1, 0
	bl	%icc, .LL3016
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3020
	 nop
.LL3016:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL3019
	 nop
.LL3017:
	cmp	%g1, 0
	bg	%icc, .LL3018
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3020
	 nop
.LL3018:
	add	%g1, -1, %g1
.LL3019:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL3020:
	cmp	%g1, 127
	ble	%icc, .LL3021
	 nop
.LL3015:
	sethi	%hi(17408), %g1
	or	%g1, 865, %o0
	call	_m3_fault, 0
	 nop
.LL3021:
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	ldub	[%fp+72], %o5
	and	%o5, 0xff, %o5
	cmp	%o5, 0
	bl	%icc, .LL3023
	 nop
	cmp	%g1, 0
	bl	%icc, .LL3022
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3026
	 nop
.LL3022:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL3025
	 nop
.LL3023:
	cmp	%g1, 0
	bg	%icc, .LL3024
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3026
	 nop
.LL3024:
	add	%g1, -1, %g1
.LL3025:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL3026:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE517:
	.size	Divide__Divide_param_i8_u8, .-Divide__Divide_param_i8_u8
	.align 4
	.type	Divide__uDivide_var_i8_L, #function
	.proc	05
Divide__uDivide_var_i8_L:
.LLFB518:
	save	%sp, -96, %sp
.LLCFI518:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE518:
	.size	Divide__uDivide_var_i8_L, .-Divide__uDivide_var_i8_L
	.align 4
	.type	Divide__Divide_var_i8_L, #function
	.proc	05
Divide__Divide_var_i8_L:
.LLFB519:
	save	%sp, -96, %sp
.LLCFI519:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o2
	cmp	%o2, 0
	bl	%icc, .LL3030
	 nop
	cmp	%o2, 0
	bne	%icc, .LL3034
	 nop
	cmp	%o3, 0
.LL3034:
	cmp	%o4, 0
	bl	%icc, .LL3029
	 nop
	cmp	%o4, 0
	bne	%icc, .LL3035
	 nop
	cmp	%o5, 0
.LL3035:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL3033
	 nop
.LL3029:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL3032
	 nop
.LL3030:
	cmp	%o4, 0
	bg	%icc, .LL3031
	 nop
	cmp	%o4, 0
	bne	%icc, .LL3036
	 nop
	cmp	%o5, 0
	bne	%icc, .LL3031
	 nop
	cmp	%o5, 0
.LL3036:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL3033
	 nop
.LL3031:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL3032:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL3033:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE519:
	.size	Divide__Divide_var_i8_L, .-Divide__Divide_var_i8_L
	.align 4
	.type	Divide__uDivide_param_i8_L, #function
	.proc	05
Divide__uDivide_param_i8_L:
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
	mov	%o4, %o0
	mov	%o5, %o1
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE520:
	.size	Divide__uDivide_param_i8_L, .-Divide__uDivide_param_i8_L
	.align 4
	.type	Divide__Divide_param_i8_L, #function
	.proc	05
Divide__Divide_param_i8_L:
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
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL3040
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL3044
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL3044:
	cmp	%o4, 0
	bl	%icc, .LL3039
	 nop
	cmp	%o4, 0
	bne	%icc, .LL3045
	 nop
	cmp	%o5, 0
.LL3045:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL3043
	 nop
.LL3039:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL3042
	 nop
.LL3040:
	cmp	%o4, 0
	bg	%icc, .LL3041
	 nop
	cmp	%o4, 0
	bne	%icc, .LL3046
	 nop
	cmp	%o5, 0
	bne	%icc, .LL3041
	 nop
	cmp	%o5, 0
.LL3046:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL3043
	 nop
.LL3041:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL3042:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL3043:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE521:
	.size	Divide__Divide_param_i8_L, .-Divide__Divide_param_i8_L
	.align 4
	.type	Divide__uDivide_var_i8_i8, #function
	.proc	04
Divide__uDivide_var_i8_i8:
.LLFB522:
	save	%sp, -96, %sp
.LLCFI522:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE522:
	.size	Divide__uDivide_var_i8_i8, .-Divide__uDivide_var_i8_i8
	.align 4
	.type	Divide__Divide_var_i8_i8, #function
	.proc	02
Divide__Divide_var_i8_i8:
.LLFB523:
	save	%sp, -96, %sp
.LLCFI523:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+128], %o5
	sra	%o5, 24, %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	cmp	%o5, 0
	bl	%icc, .LL3050
	 nop
	cmp	%g1, 0
	bl	%icc, .LL3049
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3053
	 nop
.LL3049:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL3052
	 nop
.LL3050:
	cmp	%g1, 0
	bg	%icc, .LL3051
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3053
	 nop
.LL3051:
	add	%g1, -1, %g1
.LL3052:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL3053:
	cmp	%g1, -128
	bl	%icc, .LL3054
	 nop
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+128], %o5
	sra	%o5, 24, %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	cmp	%o5, 0
	bl	%icc, .LL3056
	 nop
	cmp	%g1, 0
	bl	%icc, .LL3055
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3059
	 nop
.LL3055:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL3058
	 nop
.LL3056:
	cmp	%g1, 0
	bg	%icc, .LL3057
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3059
	 nop
.LL3057:
	add	%g1, -1, %g1
.LL3058:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL3059:
	cmp	%g1, 127
	ble	%icc, .LL3060
	 nop
.LL3054:
	sethi	%hi(18432), %g1
	or	%g1, 33, %o0
	call	_m3_fault, 0
	 nop
.LL3060:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+128], %o5
	sra	%o5, 24, %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	cmp	%o5, 0
	bl	%icc, .LL3062
	 nop
	cmp	%g1, 0
	bl	%icc, .LL3061
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3065
	 nop
.LL3061:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL3064
	 nop
.LL3062:
	cmp	%g1, 0
	bg	%icc, .LL3063
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3065
	 nop
.LL3063:
	add	%g1, -1, %g1
.LL3064:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL3065:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE523:
	.size	Divide__Divide_var_i8_i8, .-Divide__Divide_var_i8_i8
	.align 4
	.type	Divide__uDivide_param_i8_i8, #function
	.proc	04
Divide__uDivide_param_i8_i8:
.LLFB524:
	save	%sp, -96, %sp
.LLCFI524:
	stb	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE524:
	.size	Divide__uDivide_param_i8_i8, .-Divide__uDivide_param_i8_i8
	.align 4
	.type	Divide__Divide_param_i8_i8, #function
	.proc	02
Divide__Divide_param_i8_i8:
.LLFB525:
	save	%sp, -96, %sp
.LLCFI525:
	stb	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	ldub	[%fp+72], %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	cmp	%o5, 0
	bl	%icc, .LL3069
	 nop
	cmp	%g1, 0
	bl	%icc, .LL3068
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3072
	 nop
.LL3068:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL3071
	 nop
.LL3069:
	cmp	%g1, 0
	bg	%icc, .LL3070
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3072
	 nop
.LL3070:
	add	%g1, -1, %g1
.LL3071:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL3072:
	cmp	%g1, -128
	bl	%icc, .LL3073
	 nop
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	ldub	[%fp+72], %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	cmp	%o5, 0
	bl	%icc, .LL3075
	 nop
	cmp	%g1, 0
	bl	%icc, .LL3074
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3078
	 nop
.LL3074:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL3077
	 nop
.LL3075:
	cmp	%g1, 0
	bg	%icc, .LL3076
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3078
	 nop
.LL3076:
	add	%g1, -1, %g1
.LL3077:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL3078:
	cmp	%g1, 127
	ble	%icc, .LL3079
	 nop
.LL3073:
	sethi	%hi(18432), %g1
	or	%g1, 97, %o0
	call	_m3_fault, 0
	 nop
.LL3079:
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	ldub	[%fp+72], %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	cmp	%o5, 0
	bl	%icc, .LL3081
	 nop
	cmp	%g1, 0
	bl	%icc, .LL3080
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3084
	 nop
.LL3080:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL3083
	 nop
.LL3081:
	cmp	%g1, 0
	bg	%icc, .LL3082
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3084
	 nop
.LL3082:
	add	%g1, -1, %g1
.LL3083:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL3084:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE525:
	.size	Divide__Divide_param_i8_i8, .-Divide__Divide_param_i8_i8
	.align 4
	.type	Divide__uDivide_var_i8_u32, #function
	.proc	04
Divide__uDivide_var_i8_u32:
.LLFB526:
	save	%sp, -96, %sp
.LLCFI526:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE526:
	.size	Divide__uDivide_var_i8_u32, .-Divide__uDivide_var_i8_u32
	.align 4
	.type	Divide__Divide_var_i8_u32, #function
	.proc	02
Divide__Divide_var_i8_u32:
.LLFB527:
	save	%sp, -96, %sp
.LLCFI527:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+132], %o5
	cmp	%o5, 0
	bl	%icc, .LL3088
	 nop
	cmp	%g1, 0
	bl	%icc, .LL3087
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3091
	 nop
.LL3087:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL3090
	 nop
.LL3088:
	cmp	%g1, 0
	bg	%icc, .LL3089
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3091
	 nop
.LL3089:
	add	%g1, -1, %g1
.LL3090:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL3091:
	cmp	%g1, -128
	bl	%icc, .LL3092
	 nop
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+132], %o5
	cmp	%o5, 0
	bl	%icc, .LL3094
	 nop
	cmp	%g1, 0
	bl	%icc, .LL3093
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3097
	 nop
.LL3093:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL3096
	 nop
.LL3094:
	cmp	%g1, 0
	bg	%icc, .LL3095
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3097
	 nop
.LL3095:
	add	%g1, -1, %g1
.LL3096:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL3097:
	cmp	%g1, 127
	ble	%icc, .LL3098
	 nop
.LL3092:
	sethi	%hi(18432), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL3098:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+132], %o5
	cmp	%o5, 0
	bl	%icc, .LL3100
	 nop
	cmp	%g1, 0
	bl	%icc, .LL3099
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3103
	 nop
.LL3099:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL3102
	 nop
.LL3100:
	cmp	%g1, 0
	bg	%icc, .LL3101
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3103
	 nop
.LL3101:
	add	%g1, -1, %g1
.LL3102:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL3103:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE527:
	.size	Divide__Divide_var_i8_u32, .-Divide__Divide_var_i8_u32
	.align 4
	.type	Divide__uDivide_param_i8_u32, #function
	.proc	04
Divide__uDivide_param_i8_u32:
.LLFB528:
	save	%sp, -96, %sp
.LLCFI528:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE528:
	.size	Divide__uDivide_param_i8_u32, .-Divide__uDivide_param_i8_u32
	.align 4
	.type	Divide__Divide_param_i8_u32, #function
	.proc	02
Divide__Divide_param_i8_u32:
.LLFB529:
	save	%sp, -96, %sp
.LLCFI529:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL3107
	 nop
	cmp	%g1, 0
	bl	%icc, .LL3106
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3110
	 nop
.LL3106:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL3109
	 nop
.LL3107:
	cmp	%g1, 0
	bg	%icc, .LL3108
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3110
	 nop
.LL3108:
	add	%g1, -1, %g1
.LL3109:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL3110:
	cmp	%g1, -128
	bl	%icc, .LL3111
	 nop
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL3113
	 nop
	cmp	%g1, 0
	bl	%icc, .LL3112
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3116
	 nop
.LL3112:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL3115
	 nop
.LL3113:
	cmp	%g1, 0
	bg	%icc, .LL3114
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3116
	 nop
.LL3114:
	add	%g1, -1, %g1
.LL3115:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL3116:
	cmp	%g1, 127
	ble	%icc, .LL3117
	 nop
.LL3111:
	sethi	%hi(18432), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL3117:
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL3119
	 nop
	cmp	%g1, 0
	bl	%icc, .LL3118
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3122
	 nop
.LL3118:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL3121
	 nop
.LL3119:
	cmp	%g1, 0
	bg	%icc, .LL3120
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3122
	 nop
.LL3120:
	add	%g1, -1, %g1
.LL3121:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL3122:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE529:
	.size	Divide__Divide_param_i8_u32, .-Divide__Divide_param_i8_u32
	.align 4
	.type	Divide__uDivide_var_u32_u16, #function
	.proc	04
Divide__uDivide_var_u32_u16:
.LLFB530:
	save	%sp, -96, %sp
.LLCFI530:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE530:
	.size	Divide__uDivide_var_u32_u16, .-Divide__uDivide_var_u32_u16
	.align 4
	.type	Divide__Divide_var_u32_u16, #function
	.proc	04
Divide__Divide_var_u32_u16:
.LLFB531:
	save	%sp, -96, %sp
.LLCFI531:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+132], %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+52], %o5
	srl	%o5, 16, %o5
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL3126
	 nop
	cmp	%g1, 0
	bl	%icc, .LL3125
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3129
	 nop
.LL3125:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL3128
	 nop
.LL3126:
	cmp	%g1, 0
	bg	%icc, .LL3127
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3129
	 nop
.LL3127:
	add	%g1, -1, %g1
.LL3128:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL3129:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE531:
	.size	Divide__Divide_var_u32_u16, .-Divide__Divide_var_u32_u16
	.align 4
	.type	Divide__uDivide_param_u32_u16, #function
	.proc	04
Divide__uDivide_param_u32_u16:
.LLFB532:
	save	%sp, -96, %sp
.LLCFI532:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ld	[%fp+68], %o4
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE532:
	.size	Divide__uDivide_param_u32_u16, .-Divide__uDivide_param_u32_u16
	.align 4
	.type	Divide__Divide_param_u32_u16, #function
	.proc	04
Divide__Divide_param_u32_u16:
.LLFB533:
	save	%sp, -96, %sp
.LLCFI533:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+68], %g1
	cmp	%o5, 0
	bl	%icc, .LL3133
	 nop
	cmp	%g1, 0
	bl	%icc, .LL3132
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3136
	 nop
.LL3132:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL3135
	 nop
.LL3133:
	cmp	%g1, 0
	bg	%icc, .LL3134
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3136
	 nop
.LL3134:
	add	%g1, -1, %g1
.LL3135:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL3136:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE533:
	.size	Divide__Divide_param_u32_u16, .-Divide__Divide_param_u32_u16
	.align 4
	.type	Divide__uDivide_var_u32_u64, #function
	.proc	05
Divide__uDivide_var_u32_u64:
.LLFB534:
	save	%sp, -96, %sp
.LLCFI534:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE534:
	.size	Divide__uDivide_var_u32_u64, .-Divide__uDivide_var_u32_u64
	.align 4
	.type	Divide__Divide_var_u32_u64, #function
	.proc	05
Divide__Divide_var_u32_u64:
.LLFB535:
	save	%sp, -96, %sp
.LLCFI535:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+56], %o2
	cmp	%o2, 0
	bl	%icc, .LL3140
	 nop
	cmp	%o2, 0
	bne	%icc, .LL3144
	 nop
	cmp	%o3, 0
.LL3144:
	cmp	%o4, 0
	bl	%icc, .LL3139
	 nop
	cmp	%o4, 0
	bne	%icc, .LL3145
	 nop
	cmp	%o5, 0
.LL3145:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL3143
	 nop
.LL3139:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL3142
	 nop
.LL3140:
	cmp	%o4, 0
	bg	%icc, .LL3141
	 nop
	cmp	%o4, 0
	bne	%icc, .LL3146
	 nop
	cmp	%o5, 0
	bne	%icc, .LL3141
	 nop
	cmp	%o5, 0
.LL3146:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL3143
	 nop
.LL3141:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL3142:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL3143:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE535:
	.size	Divide__Divide_var_u32_u64, .-Divide__Divide_var_u32_u64
	.align 4
	.type	Divide__uDivide_param_u32_u64, #function
	.proc	05
Divide__uDivide_param_u32_u64:
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
	mov	%o4, %o0
	mov	%o5, %o1
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE536:
	.size	Divide__uDivide_param_u32_u64, .-Divide__uDivide_param_u32_u64
	.align 4
	.type	Divide__Divide_param_u32_u64, #function
	.proc	05
Divide__Divide_param_u32_u64:
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
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL3150
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL3154
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL3154:
	cmp	%o4, 0
	bl	%icc, .LL3149
	 nop
	cmp	%o4, 0
	bne	%icc, .LL3155
	 nop
	cmp	%o5, 0
.LL3155:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL3153
	 nop
.LL3149:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL3152
	 nop
.LL3150:
	cmp	%o4, 0
	bg	%icc, .LL3151
	 nop
	cmp	%o4, 0
	bne	%icc, .LL3156
	 nop
	cmp	%o5, 0
	bne	%icc, .LL3151
	 nop
	cmp	%o5, 0
.LL3156:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL3153
	 nop
.LL3151:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL3152:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL3153:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE537:
	.size	Divide__Divide_param_u32_u64, .-Divide__Divide_param_u32_u64
	.align 4
	.type	Divide__uDivide_var_u32_LC, #function
	.proc	05
Divide__uDivide_var_u32_LC:
.LLFB538:
	save	%sp, -96, %sp
.LLCFI538:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE538:
	.size	Divide__uDivide_var_u32_LC, .-Divide__uDivide_var_u32_LC
	.align 4
	.type	Divide__Divide_var_u32_LC, #function
	.proc	05
Divide__Divide_var_u32_LC:
.LLFB539:
	save	%sp, -96, %sp
.LLCFI539:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+64], %o2
	cmp	%o2, 0
	bl	%icc, .LL3160
	 nop
	cmp	%o2, 0
	bne	%icc, .LL3164
	 nop
	cmp	%o3, 0
.LL3164:
	cmp	%o4, 0
	bl	%icc, .LL3159
	 nop
	cmp	%o4, 0
	bne	%icc, .LL3165
	 nop
	cmp	%o5, 0
.LL3165:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL3163
	 nop
.LL3159:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL3162
	 nop
.LL3160:
	cmp	%o4, 0
	bg	%icc, .LL3161
	 nop
	cmp	%o4, 0
	bne	%icc, .LL3166
	 nop
	cmp	%o5, 0
	bne	%icc, .LL3161
	 nop
	cmp	%o5, 0
.LL3166:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL3163
	 nop
.LL3161:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL3162:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL3163:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE539:
	.size	Divide__Divide_var_u32_LC, .-Divide__Divide_var_u32_LC
	.align 4
	.type	Divide__uDivide_param_u32_LC, #function
	.proc	05
Divide__uDivide_param_u32_LC:
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
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE540:
	.size	Divide__uDivide_param_u32_LC, .-Divide__uDivide_param_u32_LC
	.align 4
	.type	Divide__Divide_param_u32_LC, #function
	.proc	05
Divide__Divide_param_u32_LC:
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
	cmp	%o2, 0
	bl	%icc, .LL3170
	 nop
	cmp	%o2, 0
	bne	%icc, .LL3174
	 nop
	cmp	%o3, 0
.LL3174:
	cmp	%o4, 0
	bl	%icc, .LL3169
	 nop
	cmp	%o4, 0
	bne	%icc, .LL3175
	 nop
	cmp	%o5, 0
.LL3175:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL3173
	 nop
.LL3169:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL3172
	 nop
.LL3170:
	cmp	%o4, 0
	bg	%icc, .LL3171
	 nop
	cmp	%o4, 0
	bne	%icc, .LL3176
	 nop
	cmp	%o5, 0
	bne	%icc, .LL3171
	 nop
	cmp	%o5, 0
.LL3176:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL3173
	 nop
.LL3171:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL3172:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL3173:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE541:
	.size	Divide__Divide_param_u32_LC, .-Divide__Divide_param_u32_LC
	.align 4
	.type	Divide__uDivide_var_u32_i32, #function
	.proc	04
Divide__uDivide_var_u32_i32:
.LLFB542:
	save	%sp, -96, %sp
.LLCFI542:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE542:
	.size	Divide__uDivide_var_u32_i32, .-Divide__uDivide_var_u32_i32
	.align 4
	.type	Divide__Divide_var_u32_i32, #function
	.proc	04
Divide__Divide_var_u32_i32:
.LLFB543:
	save	%sp, -96, %sp
.LLCFI543:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+132], %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL3180
	 nop
	cmp	%g1, 0
	bl	%icc, .LL3179
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3183
	 nop
.LL3179:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL3182
	 nop
.LL3180:
	cmp	%g1, 0
	bg	%icc, .LL3181
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3183
	 nop
.LL3181:
	add	%g1, -1, %g1
.LL3182:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL3183:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE543:
	.size	Divide__Divide_var_u32_i32, .-Divide__Divide_var_u32_i32
	.align 4
	.type	Divide__uDivide_param_u32_i32, #function
	.proc	04
Divide__uDivide_param_u32_i32:
.LLFB544:
	save	%sp, -96, %sp
.LLCFI544:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o4
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE544:
	.size	Divide__uDivide_param_u32_i32, .-Divide__uDivide_param_u32_i32
	.align 4
	.type	Divide__Divide_param_u32_i32, #function
	.proc	04
Divide__Divide_param_u32_i32:
.LLFB545:
	save	%sp, -96, %sp
.LLCFI545:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL3187
	 nop
	cmp	%g1, 0
	bl	%icc, .LL3186
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3190
	 nop
.LL3186:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL3189
	 nop
.LL3187:
	cmp	%g1, 0
	bg	%icc, .LL3188
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3190
	 nop
.LL3188:
	add	%g1, -1, %g1
.LL3189:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL3190:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE545:
	.size	Divide__Divide_param_u32_i32, .-Divide__Divide_param_u32_i32
	.align 4
	.type	Divide__uDivide_var_u32_i16, #function
	.proc	04
Divide__uDivide_var_u32_i16:
.LLFB546:
	save	%sp, -96, %sp
.LLCFI546:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE546:
	.size	Divide__uDivide_var_u32_i16, .-Divide__uDivide_var_u32_i16
	.align 4
	.type	Divide__Divide_var_u32_i16, #function
	.proc	04
Divide__Divide_var_u32_i16:
.LLFB547:
	save	%sp, -96, %sp
.LLCFI547:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+132], %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+76], %o5
	sra	%o5, 16, %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	cmp	%o5, 0
	bl	%icc, .LL3194
	 nop
	cmp	%g1, 0
	bl	%icc, .LL3193
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3197
	 nop
.LL3193:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL3196
	 nop
.LL3194:
	cmp	%g1, 0
	bg	%icc, .LL3195
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3197
	 nop
.LL3195:
	add	%g1, -1, %g1
.LL3196:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL3197:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE547:
	.size	Divide__Divide_var_u32_i16, .-Divide__Divide_var_u32_i16
	.align 4
	.type	Divide__uDivide_param_u32_i16, #function
	.proc	04
Divide__uDivide_param_u32_i16:
.LLFB548:
	save	%sp, -96, %sp
.LLCFI548:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ld	[%fp+68], %o4
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE548:
	.size	Divide__uDivide_param_u32_i16, .-Divide__uDivide_param_u32_i16
	.align 4
	.type	Divide__Divide_param_u32_i16, #function
	.proc	04
Divide__Divide_param_u32_i16:
.LLFB549:
	save	%sp, -96, %sp
.LLCFI549:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+68], %g1
	cmp	%o5, 0
	bl	%icc, .LL3201
	 nop
	cmp	%g1, 0
	bl	%icc, .LL3200
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3204
	 nop
.LL3200:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL3203
	 nop
.LL3201:
	cmp	%g1, 0
	bg	%icc, .LL3202
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3204
	 nop
.LL3202:
	add	%g1, -1, %g1
.LL3203:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL3204:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE549:
	.size	Divide__Divide_param_u32_i16, .-Divide__Divide_param_u32_i16
	.align 4
	.type	Divide__uDivide_var_u32_I, #function
	.proc	04
Divide__uDivide_var_u32_I:
.LLFB550:
	save	%sp, -96, %sp
.LLCFI550:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE550:
	.size	Divide__uDivide_var_u32_I, .-Divide__uDivide_var_u32_I
	.align 4
	.type	Divide__Divide_var_u32_I, #function
	.proc	04
Divide__Divide_var_u32_I:
.LLFB551:
	save	%sp, -96, %sp
.LLCFI551:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+132], %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+80], %o5
	cmp	%o5, 0
	bl	%icc, .LL3208
	 nop
	cmp	%g1, 0
	bl	%icc, .LL3207
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3211
	 nop
.LL3207:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL3210
	 nop
.LL3208:
	cmp	%g1, 0
	bg	%icc, .LL3209
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3211
	 nop
.LL3209:
	add	%g1, -1, %g1
.LL3210:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL3211:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE551:
	.size	Divide__Divide_var_u32_I, .-Divide__Divide_var_u32_I
	.align 4
	.type	Divide__uDivide_param_u32_I, #function
	.proc	04
Divide__uDivide_param_u32_I:
.LLFB552:
	save	%sp, -96, %sp
.LLCFI552:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o4
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE552:
	.size	Divide__uDivide_param_u32_I, .-Divide__uDivide_param_u32_I
	.align 4
	.type	Divide__Divide_param_u32_I, #function
	.proc	04
Divide__Divide_param_u32_I:
.LLFB553:
	save	%sp, -96, %sp
.LLCFI553:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL3215
	 nop
	cmp	%g1, 0
	bl	%icc, .LL3214
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3218
	 nop
.LL3214:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL3217
	 nop
.LL3215:
	cmp	%g1, 0
	bg	%icc, .LL3216
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3218
	 nop
.LL3216:
	add	%g1, -1, %g1
.LL3217:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL3218:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE553:
	.size	Divide__Divide_param_u32_I, .-Divide__Divide_param_u32_I
	.align 4
	.type	Divide__uDivide_var_u32_i64, #function
	.proc	05
Divide__uDivide_var_u32_i64:
.LLFB554:
	save	%sp, -96, %sp
.LLCFI554:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE554:
	.size	Divide__uDivide_var_u32_i64, .-Divide__uDivide_var_u32_i64
	.align 4
	.type	Divide__Divide_var_u32_i64, #function
	.proc	05
Divide__Divide_var_u32_i64:
.LLFB555:
	save	%sp, -96, %sp
.LLCFI555:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+88], %o2
	cmp	%o2, 0
	bl	%icc, .LL3222
	 nop
	cmp	%o2, 0
	bne	%icc, .LL3226
	 nop
	cmp	%o3, 0
.LL3226:
	cmp	%o4, 0
	bl	%icc, .LL3221
	 nop
	cmp	%o4, 0
	bne	%icc, .LL3227
	 nop
	cmp	%o5, 0
.LL3227:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL3225
	 nop
.LL3221:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL3224
	 nop
.LL3222:
	cmp	%o4, 0
	bg	%icc, .LL3223
	 nop
	cmp	%o4, 0
	bne	%icc, .LL3228
	 nop
	cmp	%o5, 0
	bne	%icc, .LL3223
	 nop
	cmp	%o5, 0
.LL3228:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL3225
	 nop
.LL3223:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL3224:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL3225:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE555:
	.size	Divide__Divide_var_u32_i64, .-Divide__Divide_var_u32_i64
	.align 4
	.type	Divide__uDivide_param_u32_i64, #function
	.proc	05
Divide__uDivide_param_u32_i64:
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
	mov	%o4, %o0
	mov	%o5, %o1
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE556:
	.size	Divide__uDivide_param_u32_i64, .-Divide__uDivide_param_u32_i64
	.align 4
	.type	Divide__Divide_param_u32_i64, #function
	.proc	05
Divide__Divide_param_u32_i64:
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
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL3232
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL3236
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL3236:
	cmp	%o4, 0
	bl	%icc, .LL3231
	 nop
	cmp	%o4, 0
	bne	%icc, .LL3237
	 nop
	cmp	%o5, 0
.LL3237:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL3235
	 nop
.LL3231:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL3234
	 nop
.LL3232:
	cmp	%o4, 0
	bg	%icc, .LL3233
	 nop
	cmp	%o4, 0
	bne	%icc, .LL3238
	 nop
	cmp	%o5, 0
	bne	%icc, .LL3233
	 nop
	cmp	%o5, 0
.LL3238:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL3235
	 nop
.LL3233:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL3234:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL3235:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE557:
	.size	Divide__Divide_param_u32_i64, .-Divide__Divide_param_u32_i64
	.align 4
	.type	Divide__uDivide_var_u32_C, #function
	.proc	04
Divide__uDivide_var_u32_C:
.LLFB558:
	save	%sp, -96, %sp
.LLCFI558:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+96], %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE558:
	.size	Divide__uDivide_var_u32_C, .-Divide__uDivide_var_u32_C
	.align 4
	.type	Divide__Divide_var_u32_C, #function
	.proc	04
Divide__Divide_var_u32_C:
.LLFB559:
	save	%sp, -96, %sp
.LLCFI559:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+132], %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+96], %o5
	cmp	%o5, 0
	bl	%icc, .LL3242
	 nop
	cmp	%g1, 0
	bl	%icc, .LL3241
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3245
	 nop
.LL3241:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL3244
	 nop
.LL3242:
	cmp	%g1, 0
	bg	%icc, .LL3243
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3245
	 nop
.LL3243:
	add	%g1, -1, %g1
.LL3244:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL3245:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE559:
	.size	Divide__Divide_var_u32_C, .-Divide__Divide_var_u32_C
	.align 4
	.type	Divide__uDivide_param_u32_C, #function
	.proc	04
Divide__uDivide_param_u32_C:
.LLFB560:
	save	%sp, -96, %sp
.LLCFI560:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	wr	%g0, 0, %y
	ld	[%fp+72], %g1
	udiv	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE560:
	.size	Divide__uDivide_param_u32_C, .-Divide__uDivide_param_u32_C
	.align 4
	.type	Divide__Divide_param_u32_C, #function
	.proc	04
Divide__Divide_param_u32_C:
.LLFB561:
	save	%sp, -96, %sp
.LLCFI561:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %g1
	cmp	%o5, 0
	bl	%icc, .LL3249
	 nop
	cmp	%g1, 0
	bl	%icc, .LL3248
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3252
	 nop
.LL3248:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL3251
	 nop
.LL3249:
	cmp	%g1, 0
	bg	%icc, .LL3250
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3252
	 nop
.LL3250:
	add	%g1, -1, %g1
.LL3251:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL3252:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE561:
	.size	Divide__Divide_param_u32_C, .-Divide__Divide_param_u32_C
	.align 4
	.type	Divide__uDivide_var_u32_u8, #function
	.proc	04
Divide__uDivide_var_u32_u8:
.LLFB562:
	save	%sp, -96, %sp
.LLCFI562:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE562:
	.size	Divide__uDivide_var_u32_u8, .-Divide__uDivide_var_u32_u8
	.align 4
	.type	Divide__Divide_var_u32_u8, #function
	.proc	04
Divide__Divide_var_u32_u8:
.LLFB563:
	save	%sp, -96, %sp
.LLCFI563:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+132], %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+112], %o5
	srl	%o5, 24, %o5
	and	%o5, 0xff, %o5
	cmp	%o5, 0
	bl	%icc, .LL3256
	 nop
	cmp	%g1, 0
	bl	%icc, .LL3255
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3259
	 nop
.LL3255:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL3258
	 nop
.LL3256:
	cmp	%g1, 0
	bg	%icc, .LL3257
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3259
	 nop
.LL3257:
	add	%g1, -1, %g1
.LL3258:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL3259:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE563:
	.size	Divide__Divide_var_u32_u8, .-Divide__Divide_var_u32_u8
	.align 4
	.type	Divide__uDivide_param_u32_u8, #function
	.proc	04
Divide__uDivide_param_u32_u8:
.LLFB564:
	save	%sp, -96, %sp
.LLCFI564:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ld	[%fp+68], %o4
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE564:
	.size	Divide__uDivide_param_u32_u8, .-Divide__uDivide_param_u32_u8
	.align 4
	.type	Divide__Divide_param_u32_u8, #function
	.proc	04
Divide__Divide_param_u32_u8:
.LLFB565:
	save	%sp, -96, %sp
.LLCFI565:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+68], %g1
	cmp	%o5, 0
	bl	%icc, .LL3263
	 nop
	cmp	%g1, 0
	bl	%icc, .LL3262
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3266
	 nop
.LL3262:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL3265
	 nop
.LL3263:
	cmp	%g1, 0
	bg	%icc, .LL3264
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3266
	 nop
.LL3264:
	add	%g1, -1, %g1
.LL3265:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL3266:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE565:
	.size	Divide__Divide_param_u32_u8, .-Divide__Divide_param_u32_u8
	.align 4
	.type	Divide__uDivide_var_u32_L, #function
	.proc	05
Divide__uDivide_var_u32_L:
.LLFB566:
	save	%sp, -96, %sp
.LLCFI566:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE566:
	.size	Divide__uDivide_var_u32_L, .-Divide__uDivide_var_u32_L
	.align 4
	.type	Divide__Divide_var_u32_L, #function
	.proc	05
Divide__Divide_var_u32_L:
.LLFB567:
	save	%sp, -96, %sp
.LLCFI567:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ldd	[%g1+120], %o2
	cmp	%o2, 0
	bl	%icc, .LL3270
	 nop
	cmp	%o2, 0
	bne	%icc, .LL3274
	 nop
	cmp	%o3, 0
.LL3274:
	cmp	%o4, 0
	bl	%icc, .LL3269
	 nop
	cmp	%o4, 0
	bne	%icc, .LL3275
	 nop
	cmp	%o5, 0
.LL3275:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL3273
	 nop
.LL3269:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL3272
	 nop
.LL3270:
	cmp	%o4, 0
	bg	%icc, .LL3271
	 nop
	cmp	%o4, 0
	bne	%icc, .LL3276
	 nop
	cmp	%o5, 0
	bne	%icc, .LL3271
	 nop
	cmp	%o5, 0
.LL3276:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL3273
	 nop
.LL3271:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL3272:
	mov	%o4, %o0
	mov	%o5, %o1
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL3273:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE567:
	.size	Divide__Divide_var_u32_L, .-Divide__Divide_var_u32_L
	.align 4
	.type	Divide__uDivide_param_u32_L, #function
	.proc	05
Divide__uDivide_param_u32_L:
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
	mov	%o4, %o0
	mov	%o5, %o1
	call	__udivdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE568:
	.size	Divide__uDivide_param_u32_L, .-Divide__uDivide_param_u32_L
	.align 4
	.type	Divide__Divide_param_u32_L, #function
	.proc	05
Divide__Divide_param_u32_L:
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
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL3280
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL3284
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL3284:
	cmp	%o4, 0
	bl	%icc, .LL3279
	 nop
	cmp	%o4, 0
	bne	%icc, .LL3285
	 nop
	cmp	%o5, 0
.LL3285:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL3283
	 nop
.LL3279:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL3282
	 nop
.LL3280:
	cmp	%o4, 0
	bg	%icc, .LL3281
	 nop
	cmp	%o4, 0
	bne	%icc, .LL3286
	 nop
	cmp	%o5, 0
	bne	%icc, .LL3281
	 nop
	cmp	%o5, 0
.LL3286:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL3283
	 nop
.LL3281:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL3282:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL3283:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE569:
	.size	Divide__Divide_param_u32_L, .-Divide__Divide_param_u32_L
	.align 4
	.type	Divide__uDivide_var_u32_i8, #function
	.proc	04
Divide__uDivide_var_u32_i8:
.LLFB570:
	save	%sp, -96, %sp
.LLCFI570:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE570:
	.size	Divide__uDivide_var_u32_i8, .-Divide__uDivide_var_u32_i8
	.align 4
	.type	Divide__Divide_var_u32_i8, #function
	.proc	04
Divide__Divide_var_u32_i8:
.LLFB571:
	save	%sp, -96, %sp
.LLCFI571:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+132], %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+128], %o5
	sra	%o5, 24, %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	cmp	%o5, 0
	bl	%icc, .LL3290
	 nop
	cmp	%g1, 0
	bl	%icc, .LL3289
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3293
	 nop
.LL3289:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL3292
	 nop
.LL3290:
	cmp	%g1, 0
	bg	%icc, .LL3291
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3293
	 nop
.LL3291:
	add	%g1, -1, %g1
.LL3292:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL3293:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE571:
	.size	Divide__Divide_var_u32_i8, .-Divide__Divide_var_u32_i8
	.align 4
	.type	Divide__uDivide_param_u32_i8, #function
	.proc	04
Divide__uDivide_param_u32_i8:
.LLFB572:
	save	%sp, -96, %sp
.LLCFI572:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ld	[%fp+68], %o4
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE572:
	.size	Divide__uDivide_param_u32_i8, .-Divide__uDivide_param_u32_i8
	.align 4
	.type	Divide__Divide_param_u32_i8, #function
	.proc	04
Divide__Divide_param_u32_i8:
.LLFB573:
	save	%sp, -96, %sp
.LLCFI573:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+68], %g1
	cmp	%o5, 0
	bl	%icc, .LL3297
	 nop
	cmp	%g1, 0
	bl	%icc, .LL3296
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3300
	 nop
.LL3296:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL3299
	 nop
.LL3297:
	cmp	%g1, 0
	bg	%icc, .LL3298
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3300
	 nop
.LL3298:
	add	%g1, -1, %g1
.LL3299:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL3300:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE573:
	.size	Divide__Divide_param_u32_i8, .-Divide__Divide_param_u32_i8
	.align 4
	.type	Divide__uDivide_var_u32_u32, #function
	.proc	04
Divide__uDivide_var_u32_u32:
.LLFB574:
	save	%sp, -96, %sp
.LLCFI574:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o4
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE574:
	.size	Divide__uDivide_var_u32_u32, .-Divide__uDivide_var_u32_u32
	.align 4
	.type	Divide__Divide_var_u32_u32, #function
	.proc	04
Divide__Divide_var_u32_u32:
.LLFB575:
	save	%sp, -96, %sp
.LLCFI575:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	ld	[%g1+132], %g1
	sethi	%hi(MM_Divide), %o5
	or	%o5, %lo(MM_Divide), %o5
	ld	[%o5+132], %o5
	cmp	%o5, 0
	bl	%icc, .LL3304
	 nop
	cmp	%g1, 0
	bl	%icc, .LL3303
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3307
	 nop
.LL3303:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL3306
	 nop
.LL3304:
	cmp	%g1, 0
	bg	%icc, .LL3305
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3307
	 nop
.LL3305:
	add	%g1, -1, %g1
.LL3306:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL3307:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE575:
	.size	Divide__Divide_var_u32_u32, .-Divide__Divide_var_u32_u32
	.align 4
	.type	Divide__uDivide_param_u32_u32, #function
	.proc	04
Divide__uDivide_param_u32_u32:
.LLFB576:
	save	%sp, -96, %sp
.LLCFI576:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o4
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	udiv	%o4, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE576:
	.size	Divide__uDivide_param_u32_u32, .-Divide__uDivide_param_u32_u32
	.align 4
	.type	Divide__Divide_param_u32_u32, #function
	.proc	04
Divide__Divide_param_u32_u32:
.LLFB577:
	save	%sp, -96, %sp
.LLCFI577:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	cmp	%o5, 0
	bl	%icc, .LL3311
	 nop
	cmp	%g1, 0
	bl	%icc, .LL3310
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3314
	 nop
.LL3310:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL3313
	 nop
.LL3311:
	cmp	%g1, 0
	bg	%icc, .LL3312
	 nop
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3314
	 nop
.LL3312:
	add	%g1, -1, %g1
.LL3313:
	sra	%g1, 31, %o5
	wr	%o5, 0, %y
	ld	[%fp+72], %o5
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL3314:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE577:
	.size	Divide__Divide_param_u32_u32, .-Divide__Divide_param_u32_u32
	.align 4
	.type	Divide__Divide_var_f32_f32, #function
	.proc	06
Divide__Divide_var_f32_f32:
.LLFB578:
	save	%sp, -96, %sp
.LLCFI578:
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	add	%g1, 136, %g1
	ld	[%g1], %f9
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	add	%g1, 136, %g1
	ld	[%g1], %f8
	fdivs	%f9, %f8, %f8
	fmovs	%f8, %f0
	return	%i7+8
	 nop
.LLFE578:
	.size	Divide__Divide_var_f32_f32, .-Divide__Divide_var_f32_f32
	.align 4
	.type	Divide__Divide_param_f32_f32, #function
	.proc	06
Divide__Divide_param_f32_f32:
.LLFB579:
	save	%sp, -96, %sp
.LLCFI579:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %f9
	ld	[%fp+72], %f8
	fdivs	%f9, %f8, %f8
	fmovs	%f8, %f0
	return	%i7+8
	 nop
.LLFE579:
	.size	Divide__Divide_param_f32_f32, .-Divide__Divide_param_f32_f32
	.align 4
	.global Divide_M3
	.type	Divide_M3, #function
	.proc	0120
Divide_M3:
.LLFB580:
	save	%sp, -96, %sp
.LLCFI580:
	st	%i0, [%fp+68]
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE580:
	.size	Divide_M3, .-Divide_M3
	.align 4
	.type	_m3_fault, #function
	.proc	020
_m3_fault:
.LLFB581:
	save	%sp, -96, %sp
.LLCFI581:
	st	%i0, [%fp+68]
	sethi	%hi(MM_Divide), %g1
	or	%g1, %lo(MM_Divide), %o0
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
	.size	L_1, 15712
L_1:
	.ascii	"Divide_M3"
	.skip 1
	.ascii	"Divide_param_f32_f32"
	.skip 1
	.ascii	"Divide_var_f32_f32"
	.skip 1
	.ascii	"Divide_param_u32_u32"
	.skip 1
	.ascii	"uDivide_param_u32_u32"
	.skip 1
	.ascii	"Divide_var_u32_u32"
	.skip 1
	.ascii	"uDivide_var_u32_u32"
	.skip 1
	.ascii	"Divide_param_u32_i8"
	.skip 1
	.ascii	"uDivide_param_u32_i8"
	.skip 1
	.ascii	"Divide_var_u32_i8"
	.skip 1
	.ascii	"uDivide_var_u32_i8"
	.skip 1
	.ascii	"Divide_param_u32_L"
	.skip 1
	.ascii	"uDivide_param_u32_L"
	.skip 1
	.ascii	"Divide_var_u32_L"
	.skip 1
	.ascii	"uDivide_var_u32_L"
	.skip 1
	.ascii	"Divide_param_u32_u8"
	.skip 1
	.ascii	"uDivide_param_u32_u8"
	.skip 1
	.ascii	"Divide_var_u32_u8"
	.skip 1
	.ascii	"uDivide_var_u32_u8"
	.skip 1
	.ascii	"Divide_param_u32_C"
	.skip 1
	.ascii	"uDivide_param_u32_C"
	.skip 1
	.ascii	"Divide_var_u32_C"
	.skip 1
	.ascii	"uDivide_var_u32_C"
	.skip 1
	.ascii	"Divide_param_u32_i64"
	.skip 1
	.ascii	"uDivide_param_u32_i64"
	.skip 1
	.ascii	"Divide_var_u32_i64"
	.skip 1
	.ascii	"uDivide_var_u32_i64"
	.skip 1
	.ascii	"Divide_param_u32_I"
	.skip 1
	.ascii	"uDivide_param_u32_I"
	.skip 1
	.ascii	"Divide_var_u32_I"
	.skip 1
	.ascii	"uDivide_var_u32_I"
	.skip 1
	.ascii	"Divide_param_u32_i16"
	.skip 1
	.ascii	"uDivide_param_u32_i16"
	.skip 1
	.ascii	"Divide_var_u32_i16"
	.skip 1
	.ascii	"uDivide_var_u32_i16"
	.skip 1
	.ascii	"Divide_param_u32_i32"
	.skip 1
	.ascii	"uDivide_param_u32_i32"
	.skip 1
	.ascii	"Divide_var_u32_i32"
	.skip 1
	.ascii	"uDivide_var_u32_i32"
	.skip 1
	.ascii	"Divide_param_u32_LC"
	.skip 1
	.ascii	"uDivide_param_u32_LC"
	.skip 1
	.ascii	"Divide_var_u32_LC"
	.skip 1
	.ascii	"uDivide_var_u32_LC"
	.skip 1
	.ascii	"Divide_param_u32_u64"
	.skip 1
	.ascii	"uDivide_param_u32_u64"
	.skip 1
	.ascii	"Divide_var_u32_u64"
	.skip 1
	.ascii	"uDivide_var_u32_u64"
	.skip 1
	.ascii	"Divide_param_u32_u16"
	.skip 1
	.ascii	"uDivide_param_u32_u16"
	.skip 1
	.ascii	"Divide_var_u32_u16"
	.skip 1
	.ascii	"uDivide_var_u32_u16"
	.skip 1
	.ascii	"Divide_param_i8_u32"
	.skip 1
	.ascii	"uDivide_param_i8_u32"
	.skip 1
	.ascii	"Divide_var_i8_u32"
	.skip 1
	.ascii	"uDivide_var_i8_u32"
	.skip 1
	.ascii	"Divide_param_i8_i8"
	.skip 1
	.ascii	"uDivide_param_i8_i8"
	.skip 1
	.ascii	"Divide_var_i8_i8"
	.skip 1
	.ascii	"uDivide_var_i8_i8"
	.skip 1
	.ascii	"Divide_param_i8_L"
	.skip 1
	.ascii	"uDivide_param_i8_L"
	.skip 1
	.ascii	"Divide_var_i8_L"
	.skip 1
	.ascii	"uDivide_var_i8_L"
	.skip 1
	.ascii	"Divide_param_i8_u8"
	.skip 1
	.ascii	"uDivide_param_i8_u8"
	.skip 1
	.ascii	"Divide_var_i8_u8"
	.skip 1
	.ascii	"uDivide_var_i8_u8"
	.skip 1
	.ascii	"Divide_param_i8_C"
	.skip 1
	.ascii	"uDivide_param_i8_C"
	.skip 1
	.ascii	"Divide_var_i8_C"
	.skip 1
	.ascii	"uDivide_var_i8_C"
	.skip 1
	.ascii	"Divide_param_i8_i64"
	.skip 1
	.ascii	"uDivide_param_i8_i64"
	.skip 1
	.ascii	"Divide_var_i8_i64"
	.skip 1
	.ascii	"uDivide_var_i8_i64"
	.skip 1
	.ascii	"Divide_param_i8_I"
	.skip 1
	.ascii	"uDivide_param_i8_I"
	.skip 1
	.ascii	"Divide_var_i8_I"
	.skip 1
	.ascii	"uDivide_var_i8_I"
	.skip 1
	.ascii	"Divide_param_i8_i16"
	.skip 1
	.ascii	"uDivide_param_i8_i16"
	.skip 1
	.ascii	"Divide_var_i8_i16"
	.skip 1
	.ascii	"uDivide_var_i8_i16"
	.skip 1
	.ascii	"Divide_param_i8_i32"
	.skip 1
	.ascii	"uDivide_param_i8_i32"
	.skip 1
	.ascii	"Divide_var_i8_i32"
	.skip 1
	.ascii	"uDivide_var_i8_i32"
	.skip 1
	.ascii	"Divide_param_i8_LC"
	.skip 1
	.ascii	"uDivide_param_i8_LC"
	.skip 1
	.ascii	"Divide_var_i8_LC"
	.skip 1
	.ascii	"uDivide_var_i8_LC"
	.skip 1
	.ascii	"Divide_param_i8_u64"
	.skip 1
	.ascii	"uDivide_param_i8_u64"
	.skip 1
	.ascii	"Divide_var_i8_u64"
	.skip 1
	.ascii	"uDivide_var_i8_u64"
	.skip 1
	.ascii	"Divide_param_i8_u16"
	.skip 1
	.ascii	"uDivide_param_i8_u16"
	.skip 1
	.ascii	"Divide_var_i8_u16"
	.skip 1
	.ascii	"uDivide_var_i8_u16"
	.skip 1
	.ascii	"Divide_param_L_u32"
	.skip 1
	.ascii	"uDivide_param_L_u32"
	.skip 1
	.ascii	"Divide_var_L_u32"
	.skip 1
	.ascii	"uDivide_var_L_u32"
	.skip 1
	.ascii	"Divide_param_L_i8"
	.skip 1
	.ascii	"uDivide_param_L_i8"
	.skip 1
	.ascii	"Divide_var_L_i8"
	.skip 1
	.ascii	"uDivide_var_L_i8"
	.skip 1
	.ascii	"Divide_param_L_L"
	.skip 1
	.ascii	"uDivide_param_L_L"
	.skip 1
	.ascii	"Divide_var_L_L"
	.skip 1
	.ascii	"uDivide_var_L_L"
	.skip 1
	.ascii	"Divide_param_L_u8"
	.skip 1
	.ascii	"uDivide_param_L_u8"
	.skip 1
	.ascii	"Divide_var_L_u8"
	.skip 1
	.ascii	"uDivide_var_L_u8"
	.skip 1
	.ascii	"Divide_param_L_C"
	.skip 1
	.ascii	"uDivide_param_L_C"
	.skip 1
	.ascii	"Divide_var_L_C"
	.skip 1
	.ascii	"uDivide_var_L_C"
	.skip 1
	.ascii	"Divide_param_L_i64"
	.skip 1
	.ascii	"uDivide_param_L_i64"
	.skip 1
	.ascii	"Divide_var_L_i64"
	.skip 1
	.ascii	"uDivide_var_L_i64"
	.skip 1
	.ascii	"Divide_param_L_I"
	.skip 1
	.ascii	"uDivide_param_L_I"
	.skip 1
	.ascii	"Divide_var_L_I"
	.skip 1
	.ascii	"uDivide_var_L_I"
	.skip 1
	.ascii	"Divide_param_L_i16"
	.skip 1
	.ascii	"uDivide_param_L_i16"
	.skip 1
	.ascii	"Divide_var_L_i16"
	.skip 1
	.ascii	"uDivide_var_L_i16"
	.skip 1
	.ascii	"Divide_param_L_i32"
	.skip 1
	.ascii	"uDivide_param_L_i32"
	.skip 1
	.ascii	"Divide_var_L_i32"
	.skip 1
	.ascii	"uDivide_var_L_i32"
	.skip 1
	.ascii	"Divide_param_L_LC"
	.skip 1
	.ascii	"uDivide_param_L_LC"
	.skip 1
	.ascii	"Divide_var_L_LC"
	.skip 1
	.ascii	"uDivide_var_L_LC"
	.skip 1
	.ascii	"Divide_param_L_u64"
	.skip 1
	.ascii	"uDivide_param_L_u64"
	.skip 1
	.ascii	"Divide_var_L_u64"
	.skip 1
	.ascii	"uDivide_var_L_u64"
	.skip 1
	.ascii	"Divide_param_L_u16"
	.skip 1
	.ascii	"uDivide_param_L_u16"
	.skip 1
	.ascii	"Divide_var_L_u16"
	.skip 1
	.ascii	"uDivide_var_L_u16"
	.skip 1
	.ascii	"Divide_param_u8_u32"
	.skip 1
	.ascii	"uDivide_param_u8_u32"
	.skip 1
	.ascii	"Divide_var_u8_u32"
	.skip 1
	.ascii	"uDivide_var_u8_u32"
	.skip 1
	.ascii	"Divide_param_u8_i8"
	.skip 1
	.ascii	"uDivide_param_u8_i8"
	.skip 1
	.ascii	"Divide_var_u8_i8"
	.skip 1
	.ascii	"uDivide_var_u8_i8"
	.skip 1
	.ascii	"Divide_param_u8_L"
	.skip 1
	.ascii	"uDivide_param_u8_L"
	.skip 1
	.ascii	"Divide_var_u8_L"
	.skip 1
	.ascii	"uDivide_var_u8_L"
	.skip 1
	.ascii	"Divide_param_u8_u8"
	.skip 1
	.ascii	"uDivide_param_u8_u8"
	.skip 1
	.ascii	"Divide_var_u8_u8"
	.skip 1
	.ascii	"uDivide_var_u8_u8"
	.skip 1
	.ascii	"Divide_param_u8_C"
	.skip 1
	.ascii	"uDivide_param_u8_C"
	.skip 1
	.ascii	"Divide_var_u8_C"
	.skip 1
	.ascii	"uDivide_var_u8_C"
	.skip 1
	.ascii	"Divide_param_u8_i64"
	.skip 1
	.ascii	"uDivide_param_u8_i64"
	.skip 1
	.ascii	"Divide_var_u8_i64"
	.skip 1
	.ascii	"uDivide_var_u8_i64"
	.skip 1
	.ascii	"Divide_param_u8_I"
	.skip 1
	.ascii	"uDivide_param_u8_I"
	.skip 1
	.ascii	"Divide_var_u8_I"
	.skip 1
	.ascii	"uDivide_var_u8_I"
	.skip 1
	.ascii	"Divide_param_u8_i16"
	.skip 1
	.ascii	"uDivide_param_u8_i16"
	.skip 1
	.ascii	"Divide_var_u8_i16"
	.skip 1
	.ascii	"uDivide_var_u8_i16"
	.skip 1
	.ascii	"Divide_param_u8_i32"
	.skip 1
	.ascii	"uDivide_param_u8_i32"
	.skip 1
	.ascii	"Divide_var_u8_i32"
	.skip 1
	.ascii	"uDivide_var_u8_i32"
	.skip 1
	.ascii	"Divide_param_u8_LC"
	.skip 1
	.ascii	"uDivide_param_u8_LC"
	.skip 1
	.ascii	"Divide_var_u8_LC"
	.skip 1
	.ascii	"uDivide_var_u8_LC"
	.skip 1
	.ascii	"Divide_param_u8_u64"
	.skip 1
	.ascii	"uDivide_param_u8_u64"
	.skip 1
	.ascii	"Divide_var_u8_u64"
	.skip 1
	.ascii	"uDivide_var_u8_u64"
	.skip 1
	.ascii	"Divide_param_u8_u16"
	.skip 1
	.ascii	"uDivide_param_u8_u16"
	.skip 1
	.ascii	"Divide_var_u8_u16"
	.skip 1
	.ascii	"uDivide_var_u8_u16"
	.skip 1
	.ascii	"Divide_param_f64_f64"
	.skip 1
	.ascii	"Divide_var_f64_f64"
	.skip 1
	.ascii	"Divide_param_C_u32"
	.skip 1
	.ascii	"uDivide_param_C_u32"
	.skip 1
	.ascii	"Divide_var_C_u32"
	.skip 1
	.ascii	"uDivide_var_C_u32"
	.skip 1
	.ascii	"Divide_param_C_i8"
	.skip 1
	.ascii	"uDivide_param_C_i8"
	.skip 1
	.ascii	"Divide_var_C_i8"
	.skip 1
	.ascii	"uDivide_var_C_i8"
	.skip 1
	.ascii	"Divide_param_C_L"
	.skip 1
	.ascii	"uDivide_param_C_L"
	.skip 1
	.ascii	"Divide_var_C_L"
	.skip 1
	.ascii	"uDivide_var_C_L"
	.skip 1
	.ascii	"Divide_param_C_u8"
	.skip 1
	.ascii	"uDivide_param_C_u8"
	.skip 1
	.ascii	"Divide_var_C_u8"
	.skip 1
	.ascii	"uDivide_var_C_u8"
	.skip 1
	.ascii	"Divide_param_C_C"
	.skip 1
	.ascii	"uDivide_param_C_C"
	.skip 1
	.ascii	"Divide_var_C_C"
	.skip 1
	.ascii	"uDivide_var_C_C"
	.skip 1
	.ascii	"Divide_param_C_i64"
	.skip 1
	.ascii	"uDivide_param_C_i64"
	.skip 1
	.ascii	"Divide_var_C_i64"
	.skip 1
	.ascii	"uDivide_var_C_i64"
	.skip 1
	.ascii	"Divide_param_C_I"
	.skip 1
	.ascii	"uDivide_param_C_I"
	.skip 1
	.ascii	"Divide_var_C_I"
	.skip 1
	.ascii	"uDivide_var_C_I"
	.skip 1
	.ascii	"Divide_param_C_i16"
	.skip 1
	.ascii	"uDivide_param_C_i16"
	.skip 1
	.ascii	"Divide_var_C_i16"
	.skip 1
	.ascii	"uDivide_var_C_i16"
	.skip 1
	.ascii	"Divide_param_C_i32"
	.skip 1
	.ascii	"uDivide_param_C_i32"
	.skip 1
	.ascii	"Divide_var_C_i32"
	.skip 1
	.ascii	"uDivide_var_C_i32"
	.skip 1
	.ascii	"Divide_param_C_LC"
	.skip 1
	.ascii	"uDivide_param_C_LC"
	.skip 1
	.ascii	"Divide_var_C_LC"
	.skip 1
	.ascii	"uDivide_var_C_LC"
	.skip 1
	.ascii	"Divide_param_C_u64"
	.skip 1
	.ascii	"uDivide_param_C_u64"
	.skip 1
	.ascii	"Divide_var_C_u64"
	.skip 1
	.ascii	"uDivide_var_C_u64"
	.skip 1
	.ascii	"Divide_param_C_u16"
	.skip 1
	.ascii	"uDivide_param_C_u16"
	.skip 1
	.ascii	"Divide_var_C_u16"
	.skip 1
	.ascii	"uDivide_var_C_u16"
	.skip 1
	.ascii	"Divide_param_i64_u32"
	.skip 1
	.ascii	"uDivide_param_i64_u32"
	.skip 1
	.ascii	"Divide_var_i64_u32"
	.skip 1
	.ascii	"uDivide_var_i64_u32"
	.skip 1
	.ascii	"Divide_param_i64_i8"
	.skip 1
	.ascii	"uDivide_param_i64_i8"
	.skip 1
	.ascii	"Divide_var_i64_i8"
	.skip 1
	.ascii	"uDivide_var_i64_i8"
	.skip 1
	.ascii	"Divide_param_i64_L"
	.skip 1
	.ascii	"uDivide_param_i64_L"
	.skip 1
	.ascii	"Divide_var_i64_L"
	.skip 1
	.ascii	"uDivide_var_i64_L"
	.skip 1
	.ascii	"Divide_param_i64_u8"
	.skip 1
	.ascii	"uDivide_param_i64_u8"
	.skip 1
	.ascii	"Divide_var_i64_u8"
	.skip 1
	.ascii	"uDivide_var_i64_u8"
	.skip 1
	.ascii	"Divide_param_i64_C"
	.skip 1
	.ascii	"uDivide_param_i64_C"
	.skip 1
	.ascii	"Divide_var_i64_C"
	.skip 1
	.ascii	"uDivide_var_i64_C"
	.skip 1
	.ascii	"Divide_param_i64_i64"
	.skip 1
	.ascii	"uDivide_param_i64_i64"
	.skip 1
	.ascii	"Divide_var_i64_i64"
	.skip 1
	.ascii	"uDivide_var_i64_i64"
	.skip 1
	.ascii	"Divide_param_i64_I"
	.skip 1
	.ascii	"uDivide_param_i64_I"
	.skip 1
	.ascii	"Divide_var_i64_I"
	.skip 1
	.ascii	"uDivide_var_i64_I"
	.skip 1
	.ascii	"Divide_param_i64_i16"
	.skip 1
	.ascii	"uDivide_param_i64_i16"
	.skip 1
	.ascii	"Divide_var_i64_i16"
	.skip 1
	.ascii	"uDivide_var_i64_i16"
	.skip 1
	.ascii	"Divide_param_i64_i32"
	.skip 1
	.ascii	"uDivide_param_i64_i32"
	.skip 1
	.ascii	"Divide_var_i64_i32"
	.skip 1
	.ascii	"uDivide_var_i64_i32"
	.skip 1
	.ascii	"Divide_param_i64_LC"
	.skip 1
	.ascii	"uDivide_param_i64_LC"
	.skip 1
	.ascii	"Divide_var_i64_LC"
	.skip 1
	.ascii	"uDivide_var_i64_LC"
	.skip 1
	.ascii	"Divide_param_i64_u64"
	.skip 1
	.ascii	"uDivide_param_i64_u64"
	.skip 1
	.ascii	"Divide_var_i64_u64"
	.skip 1
	.ascii	"uDivide_var_i64_u64"
	.skip 1
	.ascii	"Divide_param_i64_u16"
	.skip 1
	.ascii	"uDivide_param_i64_u16"
	.skip 1
	.ascii	"Divide_var_i64_u16"
	.skip 1
	.ascii	"uDivide_var_i64_u16"
	.skip 1
	.ascii	"Divide_param_I_u32"
	.skip 1
	.ascii	"uDivide_param_I_u32"
	.skip 1
	.ascii	"Divide_var_I_u32"
	.skip 1
	.ascii	"uDivide_var_I_u32"
	.skip 1
	.ascii	"Divide_param_I_i8"
	.skip 1
	.ascii	"uDivide_param_I_i8"
	.skip 1
	.ascii	"Divide_var_I_i8"
	.skip 1
	.ascii	"uDivide_var_I_i8"
	.skip 1
	.ascii	"Divide_param_I_L"
	.skip 1
	.ascii	"uDivide_param_I_L"
	.skip 1
	.ascii	"Divide_var_I_L"
	.skip 1
	.ascii	"uDivide_var_I_L"
	.skip 1
	.ascii	"Divide_param_I_u8"
	.skip 1
	.ascii	"uDivide_param_I_u8"
	.skip 1
	.ascii	"Divide_var_I_u8"
	.skip 1
	.ascii	"uDivide_var_I_u8"
	.skip 1
	.ascii	"Divide_param_I_C"
	.skip 1
	.ascii	"uDivide_param_I_C"
	.skip 1
	.ascii	"Divide_var_I_C"
	.skip 1
	.ascii	"uDivide_var_I_C"
	.skip 1
	.ascii	"Divide_param_I_i64"
	.skip 1
	.ascii	"uDivide_param_I_i64"
	.skip 1
	.ascii	"Divide_var_I_i64"
	.skip 1
	.ascii	"uDivide_var_I_i64"
	.skip 1
	.ascii	"Divide_param_I_I"
	.skip 1
	.ascii	"uDivide_param_I_I"
	.skip 1
	.ascii	"Divide_var_I_I"
	.skip 1
	.ascii	"uDivide_var_I_I"
	.skip 1
	.ascii	"Divide_param_I_i16"
	.skip 1
	.ascii	"uDivide_param_I_i16"
	.skip 1
	.ascii	"Divide_var_I_i16"
	.skip 1
	.ascii	"uDivide_var_I_i16"
	.skip 1
	.ascii	"Divide_param_I_i32"
	.skip 1
	.ascii	"uDivide_param_I_i32"
	.skip 1
	.ascii	"Divide_var_I_i32"
	.skip 1
	.ascii	"uDivide_var_I_i32"
	.skip 1
	.ascii	"Divide_param_I_LC"
	.skip 1
	.ascii	"uDivide_param_I_LC"
	.skip 1
	.ascii	"Divide_var_I_LC"
	.skip 1
	.ascii	"uDivide_var_I_LC"
	.skip 1
	.ascii	"Divide_param_I_u64"
	.skip 1
	.ascii	"uDivide_param_I_u64"
	.skip 1
	.ascii	"Divide_var_I_u64"
	.skip 1
	.ascii	"uDivide_var_I_u64"
	.skip 1
	.ascii	"Divide_param_I_u16"
	.skip 1
	.ascii	"uDivide_param_I_u16"
	.skip 1
	.ascii	"Divide_var_I_u16"
	.skip 1
	.ascii	"uDivide_var_I_u16"
	.skip 1
	.ascii	"Divide_param_i16_u32"
	.skip 1
	.ascii	"uDivide_param_i16_u32"
	.skip 1
	.ascii	"Divide_var_i16_u32"
	.skip 1
	.ascii	"uDivide_var_i16_u32"
	.skip 1
	.ascii	"Divide_param_i16_i8"
	.skip 1
	.ascii	"uDivide_param_i16_i8"
	.skip 1
	.ascii	"Divide_var_i16_i8"
	.skip 1
	.ascii	"uDivide_var_i16_i8"
	.skip 1
	.ascii	"Divide_param_i16_L"
	.skip 1
	.ascii	"uDivide_param_i16_L"
	.skip 1
	.ascii	"Divide_var_i16_L"
	.skip 1
	.ascii	"uDivide_var_i16_L"
	.skip 1
	.ascii	"Divide_param_i16_u8"
	.skip 1
	.ascii	"uDivide_param_i16_u8"
	.skip 1
	.ascii	"Divide_var_i16_u8"
	.skip 1
	.ascii	"uDivide_var_i16_u8"
	.skip 1
	.ascii	"Divide_param_i16_C"
	.skip 1
	.ascii	"uDivide_param_i16_C"
	.skip 1
	.ascii	"Divide_var_i16_C"
	.skip 1
	.ascii	"uDivide_var_i16_C"
	.skip 1
	.ascii	"Divide_param_i16_i64"
	.skip 1
	.ascii	"uDivide_param_i16_i64"
	.skip 1
	.ascii	"Divide_var_i16_i64"
	.skip 1
	.ascii	"uDivide_var_i16_i64"
	.skip 1
	.ascii	"Divide_param_i16_I"
	.skip 1
	.ascii	"uDivide_param_i16_I"
	.skip 1
	.ascii	"Divide_var_i16_I"
	.skip 1
	.ascii	"uDivide_var_i16_I"
	.skip 1
	.ascii	"Divide_param_i16_i16"
	.skip 1
	.ascii	"uDivide_param_i16_i16"
	.skip 1
	.ascii	"Divide_var_i16_i16"
	.skip 1
	.ascii	"uDivide_var_i16_i16"
	.skip 1
	.ascii	"Divide_param_i16_i32"
	.skip 1
	.ascii	"uDivide_param_i16_i32"
	.skip 1
	.ascii	"Divide_var_i16_i32"
	.skip 1
	.ascii	"uDivide_var_i16_i32"
	.skip 1
	.ascii	"Divide_param_i16_LC"
	.skip 1
	.ascii	"uDivide_param_i16_LC"
	.skip 1
	.ascii	"Divide_var_i16_LC"
	.skip 1
	.ascii	"uDivide_var_i16_LC"
	.skip 1
	.ascii	"Divide_param_i16_u64"
	.skip 1
	.ascii	"uDivide_param_i16_u64"
	.skip 1
	.ascii	"Divide_var_i16_u64"
	.skip 1
	.ascii	"uDivide_var_i16_u64"
	.skip 1
	.ascii	"Divide_param_i16_u16"
	.skip 1
	.ascii	"uDivide_param_i16_u16"
	.skip 1
	.ascii	"Divide_var_i16_u16"
	.skip 1
	.ascii	"uDivide_var_i16_u16"
	.skip 1
	.ascii	"Divide_param_i32_u32"
	.skip 1
	.ascii	"uDivide_param_i32_u32"
	.skip 1
	.ascii	"Divide_var_i32_u32"
	.skip 1
	.ascii	"uDivide_var_i32_u32"
	.skip 1
	.ascii	"Divide_param_i32_i8"
	.skip 1
	.ascii	"uDivide_param_i32_i8"
	.skip 1
	.ascii	"Divide_var_i32_i8"
	.skip 1
	.ascii	"uDivide_var_i32_i8"
	.skip 1
	.ascii	"Divide_param_i32_L"
	.skip 1
	.ascii	"uDivide_param_i32_L"
	.skip 1
	.ascii	"Divide_var_i32_L"
	.skip 1
	.ascii	"uDivide_var_i32_L"
	.skip 1
	.ascii	"Divide_param_i32_u8"
	.skip 1
	.ascii	"uDivide_param_i32_u8"
	.skip 1
	.ascii	"Divide_var_i32_u8"
	.skip 1
	.ascii	"uDivide_var_i32_u8"
	.skip 1
	.ascii	"Divide_param_i32_C"
	.skip 1
	.ascii	"uDivide_param_i32_C"
	.skip 1
	.ascii	"Divide_var_i32_C"
	.skip 1
	.ascii	"uDivide_var_i32_C"
	.skip 1
	.ascii	"Divide_param_i32_i64"
	.skip 1
	.ascii	"uDivide_param_i32_i64"
	.skip 1
	.ascii	"Divide_var_i32_i64"
	.skip 1
	.ascii	"uDivide_var_i32_i64"
	.skip 1
	.ascii	"Divide_param_i32_I"
	.skip 1
	.ascii	"uDivide_param_i32_I"
	.skip 1
	.ascii	"Divide_var_i32_I"
	.skip 1
	.ascii	"uDivide_var_i32_I"
	.skip 1
	.ascii	"Divide_param_i32_i16"
	.skip 1
	.ascii	"uDivide_param_i32_i16"
	.skip 1
	.ascii	"Divide_var_i32_i16"
	.skip 1
	.ascii	"uDivide_var_i32_i16"
	.skip 1
	.ascii	"Divide_param_i32_i32"
	.skip 1
	.ascii	"uDivide_param_i32_i32"
	.skip 1
	.ascii	"Divide_var_i32_i32"
	.skip 1
	.ascii	"uDivide_var_i32_i32"
	.skip 1
	.ascii	"Divide_param_i32_LC"
	.skip 1
	.ascii	"uDivide_param_i32_LC"
	.skip 1
	.ascii	"Divide_var_i32_LC"
	.skip 1
	.ascii	"uDivide_var_i32_LC"
	.skip 1
	.ascii	"Divide_param_i32_u64"
	.skip 1
	.ascii	"uDivide_param_i32_u64"
	.skip 1
	.ascii	"Divide_var_i32_u64"
	.skip 1
	.ascii	"uDivide_var_i32_u64"
	.skip 1
	.ascii	"Divide_param_i32_u16"
	.skip 1
	.ascii	"uDivide_param_i32_u16"
	.skip 1
	.ascii	"Divide_var_i32_u16"
	.skip 1
	.ascii	"uDivide_var_i32_u16"
	.skip 1
	.ascii	"Divide_param_LC_u32"
	.skip 1
	.ascii	"uDivide_param_LC_u32"
	.skip 1
	.ascii	"Divide_var_LC_u32"
	.skip 1
	.ascii	"uDivide_var_LC_u32"
	.skip 1
	.ascii	"Divide_param_LC_i8"
	.skip 1
	.ascii	"uDivide_param_LC_i8"
	.skip 1
	.ascii	"Divide_var_LC_i8"
	.skip 1
	.ascii	"uDivide_var_LC_i8"
	.skip 1
	.ascii	"Divide_param_LC_L"
	.skip 1
	.ascii	"uDivide_param_LC_L"
	.skip 1
	.ascii	"Divide_var_LC_L"
	.skip 1
	.ascii	"uDivide_var_LC_L"
	.skip 1
	.ascii	"Divide_param_LC_u8"
	.skip 1
	.ascii	"uDivide_param_LC_u8"
	.skip 1
	.ascii	"Divide_var_LC_u8"
	.skip 1
	.ascii	"uDivide_var_LC_u8"
	.skip 1
	.ascii	"Divide_param_LC_C"
	.skip 1
	.ascii	"uDivide_param_LC_C"
	.skip 1
	.ascii	"Divide_var_LC_C"
	.skip 1
	.ascii	"uDivide_var_LC_C"
	.skip 1
	.ascii	"Divide_param_LC_i64"
	.skip 1
	.ascii	"uDivide_param_LC_i64"
	.skip 1
	.ascii	"Divide_var_LC_i64"
	.skip 1
	.ascii	"uDivide_var_LC_i64"
	.skip 1
	.ascii	"Divide_param_LC_I"
	.skip 1
	.ascii	"uDivide_param_LC_I"
	.skip 1
	.ascii	"Divide_var_LC_I"
	.skip 1
	.ascii	"uDivide_var_LC_I"
	.skip 1
	.ascii	"Divide_param_LC_i16"
	.skip 1
	.ascii	"uDivide_param_LC_i16"
	.skip 1
	.ascii	"Divide_var_LC_i16"
	.skip 1
	.ascii	"uDivide_var_LC_i16"
	.skip 1
	.ascii	"Divide_param_LC_i32"
	.skip 1
	.ascii	"uDivide_param_LC_i32"
	.skip 1
	.ascii	"Divide_var_LC_i32"
	.skip 1
	.ascii	"uDivide_var_LC_i32"
	.skip 1
	.ascii	"Divide_param_LC_LC"
	.skip 1
	.ascii	"uDivide_param_LC_LC"
	.skip 1
	.ascii	"Divide_var_LC_LC"
	.skip 1
	.ascii	"uDivide_var_LC_LC"
	.skip 1
	.ascii	"Divide_param_LC_u64"
	.skip 1
	.ascii	"uDivide_param_LC_u64"
	.skip 1
	.ascii	"Divide_var_LC_u64"
	.skip 1
	.ascii	"uDivide_var_LC_u64"
	.skip 1
	.ascii	"Divide_param_LC_u16"
	.skip 1
	.ascii	"uDivide_param_LC_u16"
	.skip 1
	.ascii	"Divide_var_LC_u16"
	.skip 1
	.ascii	"uDivide_var_LC_u16"
	.skip 1
	.ascii	"Divide_param_u64_u32"
	.skip 1
	.ascii	"uDivide_param_u64_u32"
	.skip 1
	.ascii	"Divide_var_u64_u32"
	.skip 1
	.ascii	"uDivide_var_u64_u32"
	.skip 1
	.ascii	"Divide_param_u64_i8"
	.skip 1
	.ascii	"uDivide_param_u64_i8"
	.skip 1
	.ascii	"Divide_var_u64_i8"
	.skip 1
	.ascii	"uDivide_var_u64_i8"
	.skip 1
	.ascii	"Divide_param_u64_L"
	.skip 1
	.ascii	"uDivide_param_u64_L"
	.skip 1
	.ascii	"Divide_var_u64_L"
	.skip 1
	.ascii	"uDivide_var_u64_L"
	.skip 1
	.ascii	"Divide_param_u64_u8"
	.skip 1
	.ascii	"uDivide_param_u64_u8"
	.skip 1
	.ascii	"Divide_var_u64_u8"
	.skip 1
	.ascii	"uDivide_var_u64_u8"
	.skip 1
	.ascii	"Divide_param_u64_C"
	.skip 1
	.ascii	"uDivide_param_u64_C"
	.skip 1
	.ascii	"Divide_var_u64_C"
	.skip 1
	.ascii	"uDivide_var_u64_C"
	.skip 1
	.ascii	"Divide_param_u64_i64"
	.skip 1
	.ascii	"uDivide_param_u64_i64"
	.skip 1
	.ascii	"Divide_var_u64_i64"
	.skip 1
	.ascii	"uDivide_var_u64_i64"
	.skip 1
	.ascii	"Divide_param_u64_I"
	.skip 1
	.ascii	"uDivide_param_u64_I"
	.skip 1
	.ascii	"Divide_var_u64_I"
	.skip 1
	.ascii	"uDivide_var_u64_I"
	.skip 1
	.ascii	"Divide_param_u64_i16"
	.skip 1
	.ascii	"uDivide_param_u64_i16"
	.skip 1
	.ascii	"Divide_var_u64_i16"
	.skip 1
	.ascii	"uDivide_var_u64_i16"
	.skip 1
	.ascii	"Divide_param_u64_i32"
	.skip 1
	.ascii	"uDivide_param_u64_i32"
	.skip 1
	.ascii	"Divide_var_u64_i32"
	.skip 1
	.ascii	"uDivide_var_u64_i32"
	.skip 1
	.ascii	"Divide_param_u64_LC"
	.skip 1
	.ascii	"uDivide_param_u64_LC"
	.skip 1
	.ascii	"Divide_var_u64_LC"
	.skip 1
	.ascii	"uDivide_var_u64_LC"
	.skip 1
	.ascii	"Divide_param_u64_u64"
	.skip 1
	.ascii	"uDivide_param_u64_u64"
	.skip 1
	.ascii	"Divide_var_u64_u64"
	.skip 1
	.ascii	"uDivide_var_u64_u64"
	.skip 1
	.ascii	"Divide_param_u64_u16"
	.skip 1
	.ascii	"uDivide_param_u64_u16"
	.skip 1
	.ascii	"Divide_var_u64_u16"
	.skip 1
	.ascii	"uDivide_var_u64_u16"
	.skip 1
	.ascii	"Divide_param_u16_u32"
	.skip 1
	.ascii	"uDivide_param_u16_u32"
	.skip 1
	.ascii	"Divide_var_u16_u32"
	.skip 1
	.ascii	"uDivide_var_u16_u32"
	.skip 1
	.ascii	"Divide_param_u16_i8"
	.skip 1
	.ascii	"uDivide_param_u16_i8"
	.skip 1
	.ascii	"Divide_var_u16_i8"
	.skip 1
	.ascii	"uDivide_var_u16_i8"
	.skip 1
	.ascii	"Divide_param_u16_L"
	.skip 1
	.ascii	"uDivide_param_u16_L"
	.skip 1
	.ascii	"Divide_var_u16_L"
	.skip 1
	.ascii	"uDivide_var_u16_L"
	.skip 1
	.ascii	"Divide_param_u16_u8"
	.skip 1
	.ascii	"uDivide_param_u16_u8"
	.skip 1
	.ascii	"Divide_var_u16_u8"
	.skip 1
	.ascii	"uDivide_var_u16_u8"
	.skip 1
	.ascii	"Divide_param_u16_C"
	.skip 1
	.ascii	"uDivide_param_u16_C"
	.skip 1
	.ascii	"Divide_var_u16_C"
	.skip 1
	.ascii	"uDivide_var_u16_C"
	.skip 1
	.ascii	"Divide_param_u16_i64"
	.skip 1
	.ascii	"uDivide_param_u16_i64"
	.skip 1
	.ascii	"Divide_var_u16_i64"
	.skip 1
	.ascii	"uDivide_var_u16_i64"
	.skip 1
	.ascii	"Divide_param_u16_I"
	.skip 1
	.ascii	"uDivide_param_u16_I"
	.skip 1
	.ascii	"Divide_var_u16_I"
	.skip 1
	.ascii	"uDivide_var_u16_I"
	.skip 1
	.ascii	"Divide_param_u16_i16"
	.skip 1
	.ascii	"uDivide_param_u16_i16"
	.skip 1
	.ascii	"Divide_var_u16_i16"
	.skip 1
	.ascii	"uDivide_var_u16_i16"
	.skip 1
	.ascii	"Divide_param_u16_i32"
	.skip 1
	.ascii	"uDivide_param_u16_i32"
	.skip 1
	.ascii	"Divide_var_u16_i32"
	.skip 1
	.ascii	"uDivide_var_u16_i32"
	.skip 1
	.ascii	"Divide_param_u16_LC"
	.skip 1
	.ascii	"uDivide_param_u16_LC"
	.skip 1
	.ascii	"Divide_var_u16_LC"
	.skip 1
	.ascii	"uDivide_var_u16_LC"
	.skip 1
	.ascii	"Divide_param_u16_u64"
	.skip 1
	.ascii	"uDivide_param_u16_u64"
	.skip 1
	.ascii	"Divide_var_u16_u64"
	.skip 1
	.ascii	"uDivide_var_u16_u64"
	.skip 1
	.ascii	"Divide_param_u16_u16"
	.skip 1
	.ascii	"uDivide_param_u16_u16"
	.skip 1
	.ascii	"Divide_var_u16_u16"
	.skip 1
	.ascii	"uDivide_var_u16_u16"
	.skip 3
	.long	Divide_M3
	.long	L_1
	.long	Divide__Divide_param_f32_f32
	.long	L_1+10
	.long	Divide__Divide_var_f32_f32
	.long	L_1+31
	.long	Divide__Divide_param_u32_u32
	.long	L_1+50
	.long	Divide__uDivide_param_u32_u32
	.long	L_1+71
	.long	Divide__Divide_var_u32_u32
	.long	L_1+93
	.long	Divide__uDivide_var_u32_u32
	.long	L_1+112
	.long	Divide__Divide_param_u32_i8
	.long	L_1+132
	.long	Divide__uDivide_param_u32_i8
	.long	L_1+152
	.long	Divide__Divide_var_u32_i8
	.long	L_1+173
	.long	Divide__uDivide_var_u32_i8
	.long	L_1+191
	.long	Divide__Divide_param_u32_L
	.long	L_1+210
	.long	Divide__uDivide_param_u32_L
	.long	L_1+229
	.long	Divide__Divide_var_u32_L
	.long	L_1+249
	.long	Divide__uDivide_var_u32_L
	.long	L_1+266
	.long	Divide__Divide_param_u32_u8
	.long	L_1+284
	.long	Divide__uDivide_param_u32_u8
	.long	L_1+304
	.long	Divide__Divide_var_u32_u8
	.long	L_1+325
	.long	Divide__uDivide_var_u32_u8
	.long	L_1+343
	.long	Divide__Divide_param_u32_C
	.long	L_1+362
	.long	Divide__uDivide_param_u32_C
	.long	L_1+381
	.long	Divide__Divide_var_u32_C
	.long	L_1+401
	.long	Divide__uDivide_var_u32_C
	.long	L_1+418
	.long	Divide__Divide_param_u32_i64
	.long	L_1+436
	.long	Divide__uDivide_param_u32_i64
	.long	L_1+457
	.long	Divide__Divide_var_u32_i64
	.long	L_1+479
	.long	Divide__uDivide_var_u32_i64
	.long	L_1+498
	.long	Divide__Divide_param_u32_I
	.long	L_1+518
	.long	Divide__uDivide_param_u32_I
	.long	L_1+537
	.long	Divide__Divide_var_u32_I
	.long	L_1+557
	.long	Divide__uDivide_var_u32_I
	.long	L_1+574
	.long	Divide__Divide_param_u32_i16
	.long	L_1+592
	.long	Divide__uDivide_param_u32_i16
	.long	L_1+613
	.long	Divide__Divide_var_u32_i16
	.long	L_1+635
	.long	Divide__uDivide_var_u32_i16
	.long	L_1+654
	.long	Divide__Divide_param_u32_i32
	.long	L_1+674
	.long	Divide__uDivide_param_u32_i32
	.long	L_1+695
	.long	Divide__Divide_var_u32_i32
	.long	L_1+717
	.long	Divide__uDivide_var_u32_i32
	.long	L_1+736
	.long	Divide__Divide_param_u32_LC
	.long	L_1+756
	.long	Divide__uDivide_param_u32_LC
	.long	L_1+776
	.long	Divide__Divide_var_u32_LC
	.long	L_1+797
	.long	Divide__uDivide_var_u32_LC
	.long	L_1+815
	.long	Divide__Divide_param_u32_u64
	.long	L_1+834
	.long	Divide__uDivide_param_u32_u64
	.long	L_1+855
	.long	Divide__Divide_var_u32_u64
	.long	L_1+877
	.long	Divide__uDivide_var_u32_u64
	.long	L_1+896
	.long	Divide__Divide_param_u32_u16
	.long	L_1+916
	.long	Divide__uDivide_param_u32_u16
	.long	L_1+937
	.long	Divide__Divide_var_u32_u16
	.long	L_1+959
	.long	Divide__uDivide_var_u32_u16
	.long	L_1+978
	.long	Divide__Divide_param_i8_u32
	.long	L_1+998
	.long	Divide__uDivide_param_i8_u32
	.long	L_1+1018
	.long	Divide__Divide_var_i8_u32
	.long	L_1+1039
	.long	Divide__uDivide_var_i8_u32
	.long	L_1+1057
	.long	Divide__Divide_param_i8_i8
	.long	L_1+1076
	.long	Divide__uDivide_param_i8_i8
	.long	L_1+1095
	.long	Divide__Divide_var_i8_i8
	.long	L_1+1115
	.long	Divide__uDivide_var_i8_i8
	.long	L_1+1132
	.long	Divide__Divide_param_i8_L
	.long	L_1+1150
	.long	Divide__uDivide_param_i8_L
	.long	L_1+1168
	.long	Divide__Divide_var_i8_L
	.long	L_1+1187
	.long	Divide__uDivide_var_i8_L
	.long	L_1+1203
	.long	Divide__Divide_param_i8_u8
	.long	L_1+1220
	.long	Divide__uDivide_param_i8_u8
	.long	L_1+1239
	.long	Divide__Divide_var_i8_u8
	.long	L_1+1259
	.long	Divide__uDivide_var_i8_u8
	.long	L_1+1276
	.long	Divide__Divide_param_i8_C
	.long	L_1+1294
	.long	Divide__uDivide_param_i8_C
	.long	L_1+1312
	.long	Divide__Divide_var_i8_C
	.long	L_1+1331
	.long	Divide__uDivide_var_i8_C
	.long	L_1+1347
	.long	Divide__Divide_param_i8_i64
	.long	L_1+1364
	.long	Divide__uDivide_param_i8_i64
	.long	L_1+1384
	.long	Divide__Divide_var_i8_i64
	.long	L_1+1405
	.long	Divide__uDivide_var_i8_i64
	.long	L_1+1423
	.long	Divide__Divide_param_i8_I
	.long	L_1+1442
	.long	Divide__uDivide_param_i8_I
	.long	L_1+1460
	.long	Divide__Divide_var_i8_I
	.long	L_1+1479
	.long	Divide__uDivide_var_i8_I
	.long	L_1+1495
	.long	Divide__Divide_param_i8_i16
	.long	L_1+1512
	.long	Divide__uDivide_param_i8_i16
	.long	L_1+1532
	.long	Divide__Divide_var_i8_i16
	.long	L_1+1553
	.long	Divide__uDivide_var_i8_i16
	.long	L_1+1571
	.long	Divide__Divide_param_i8_i32
	.long	L_1+1590
	.long	Divide__uDivide_param_i8_i32
	.long	L_1+1610
	.long	Divide__Divide_var_i8_i32
	.long	L_1+1631
	.long	Divide__uDivide_var_i8_i32
	.long	L_1+1649
	.long	Divide__Divide_param_i8_LC
	.long	L_1+1668
	.long	Divide__uDivide_param_i8_LC
	.long	L_1+1687
	.long	Divide__Divide_var_i8_LC
	.long	L_1+1707
	.long	Divide__uDivide_var_i8_LC
	.long	L_1+1724
	.long	Divide__Divide_param_i8_u64
	.long	L_1+1742
	.long	Divide__uDivide_param_i8_u64
	.long	L_1+1762
	.long	Divide__Divide_var_i8_u64
	.long	L_1+1783
	.long	Divide__uDivide_var_i8_u64
	.long	L_1+1801
	.long	Divide__Divide_param_i8_u16
	.long	L_1+1820
	.long	Divide__uDivide_param_i8_u16
	.long	L_1+1840
	.long	Divide__Divide_var_i8_u16
	.long	L_1+1861
	.long	Divide__uDivide_var_i8_u16
	.long	L_1+1879
	.long	Divide__Divide_param_L_u32
	.long	L_1+1898
	.long	Divide__uDivide_param_L_u32
	.long	L_1+1917
	.long	Divide__Divide_var_L_u32
	.long	L_1+1937
	.long	Divide__uDivide_var_L_u32
	.long	L_1+1954
	.long	Divide__Divide_param_L_i8
	.long	L_1+1972
	.long	Divide__uDivide_param_L_i8
	.long	L_1+1990
	.long	Divide__Divide_var_L_i8
	.long	L_1+2009
	.long	Divide__uDivide_var_L_i8
	.long	L_1+2025
	.long	Divide__Divide_param_L_L
	.long	L_1+2042
	.long	Divide__uDivide_param_L_L
	.long	L_1+2059
	.long	Divide__Divide_var_L_L
	.long	L_1+2077
	.long	Divide__uDivide_var_L_L
	.long	L_1+2092
	.long	Divide__Divide_param_L_u8
	.long	L_1+2108
	.long	Divide__uDivide_param_L_u8
	.long	L_1+2126
	.long	Divide__Divide_var_L_u8
	.long	L_1+2145
	.long	Divide__uDivide_var_L_u8
	.long	L_1+2161
	.long	Divide__Divide_param_L_C
	.long	L_1+2178
	.long	Divide__uDivide_param_L_C
	.long	L_1+2195
	.long	Divide__Divide_var_L_C
	.long	L_1+2213
	.long	Divide__uDivide_var_L_C
	.long	L_1+2228
	.long	Divide__Divide_param_L_i64
	.long	L_1+2244
	.long	Divide__uDivide_param_L_i64
	.long	L_1+2263
	.long	Divide__Divide_var_L_i64
	.long	L_1+2283
	.long	Divide__uDivide_var_L_i64
	.long	L_1+2300
	.long	Divide__Divide_param_L_I
	.long	L_1+2318
	.long	Divide__uDivide_param_L_I
	.long	L_1+2335
	.long	Divide__Divide_var_L_I
	.long	L_1+2353
	.long	Divide__uDivide_var_L_I
	.long	L_1+2368
	.long	Divide__Divide_param_L_i16
	.long	L_1+2384
	.long	Divide__uDivide_param_L_i16
	.long	L_1+2403
	.long	Divide__Divide_var_L_i16
	.long	L_1+2423
	.long	Divide__uDivide_var_L_i16
	.long	L_1+2440
	.long	Divide__Divide_param_L_i32
	.long	L_1+2458
	.long	Divide__uDivide_param_L_i32
	.long	L_1+2477
	.long	Divide__Divide_var_L_i32
	.long	L_1+2497
	.long	Divide__uDivide_var_L_i32
	.long	L_1+2514
	.long	Divide__Divide_param_L_LC
	.long	L_1+2532
	.long	Divide__uDivide_param_L_LC
	.long	L_1+2550
	.long	Divide__Divide_var_L_LC
	.long	L_1+2569
	.long	Divide__uDivide_var_L_LC
	.long	L_1+2585
	.long	Divide__Divide_param_L_u64
	.long	L_1+2602
	.long	Divide__uDivide_param_L_u64
	.long	L_1+2621
	.long	Divide__Divide_var_L_u64
	.long	L_1+2641
	.long	Divide__uDivide_var_L_u64
	.long	L_1+2658
	.long	Divide__Divide_param_L_u16
	.long	L_1+2676
	.long	Divide__uDivide_param_L_u16
	.long	L_1+2695
	.long	Divide__Divide_var_L_u16
	.long	L_1+2715
	.long	Divide__uDivide_var_L_u16
	.long	L_1+2732
	.long	Divide__Divide_param_u8_u32
	.long	L_1+2750
	.long	Divide__uDivide_param_u8_u32
	.long	L_1+2770
	.long	Divide__Divide_var_u8_u32
	.long	L_1+2791
	.long	Divide__uDivide_var_u8_u32
	.long	L_1+2809
	.long	Divide__Divide_param_u8_i8
	.long	L_1+2828
	.long	Divide__uDivide_param_u8_i8
	.long	L_1+2847
	.long	Divide__Divide_var_u8_i8
	.long	L_1+2867
	.long	Divide__uDivide_var_u8_i8
	.long	L_1+2884
	.long	Divide__Divide_param_u8_L
	.long	L_1+2902
	.long	Divide__uDivide_param_u8_L
	.long	L_1+2920
	.long	Divide__Divide_var_u8_L
	.long	L_1+2939
	.long	Divide__uDivide_var_u8_L
	.long	L_1+2955
	.long	Divide__Divide_param_u8_u8
	.long	L_1+2972
	.long	Divide__uDivide_param_u8_u8
	.long	L_1+2991
	.long	Divide__Divide_var_u8_u8
	.long	L_1+3011
	.long	Divide__uDivide_var_u8_u8
	.long	L_1+3028
	.long	Divide__Divide_param_u8_C
	.long	L_1+3046
	.long	Divide__uDivide_param_u8_C
	.long	L_1+3064
	.long	Divide__Divide_var_u8_C
	.long	L_1+3083
	.long	Divide__uDivide_var_u8_C
	.long	L_1+3099
	.long	Divide__Divide_param_u8_i64
	.long	L_1+3116
	.long	Divide__uDivide_param_u8_i64
	.long	L_1+3136
	.long	Divide__Divide_var_u8_i64
	.long	L_1+3157
	.long	Divide__uDivide_var_u8_i64
	.long	L_1+3175
	.long	Divide__Divide_param_u8_I
	.long	L_1+3194
	.long	Divide__uDivide_param_u8_I
	.long	L_1+3212
	.long	Divide__Divide_var_u8_I
	.long	L_1+3231
	.long	Divide__uDivide_var_u8_I
	.long	L_1+3247
	.long	Divide__Divide_param_u8_i16
	.long	L_1+3264
	.long	Divide__uDivide_param_u8_i16
	.long	L_1+3284
	.long	Divide__Divide_var_u8_i16
	.long	L_1+3305
	.long	Divide__uDivide_var_u8_i16
	.long	L_1+3323
	.long	Divide__Divide_param_u8_i32
	.long	L_1+3342
	.long	Divide__uDivide_param_u8_i32
	.long	L_1+3362
	.long	Divide__Divide_var_u8_i32
	.long	L_1+3383
	.long	Divide__uDivide_var_u8_i32
	.long	L_1+3401
	.long	Divide__Divide_param_u8_LC
	.long	L_1+3420
	.long	Divide__uDivide_param_u8_LC
	.long	L_1+3439
	.long	Divide__Divide_var_u8_LC
	.long	L_1+3459
	.long	Divide__uDivide_var_u8_LC
	.long	L_1+3476
	.long	Divide__Divide_param_u8_u64
	.long	L_1+3494
	.long	Divide__uDivide_param_u8_u64
	.long	L_1+3514
	.long	Divide__Divide_var_u8_u64
	.long	L_1+3535
	.long	Divide__uDivide_var_u8_u64
	.long	L_1+3553
	.long	Divide__Divide_param_u8_u16
	.long	L_1+3572
	.long	Divide__uDivide_param_u8_u16
	.long	L_1+3592
	.long	Divide__Divide_var_u8_u16
	.long	L_1+3613
	.long	Divide__uDivide_var_u8_u16
	.long	L_1+3631
	.long	Divide__Divide_param_f64_f64
	.long	L_1+3650
	.long	Divide__Divide_var_f64_f64
	.long	L_1+3671
	.long	Divide__Divide_param_C_u32
	.long	L_1+3690
	.long	Divide__uDivide_param_C_u32
	.long	L_1+3709
	.long	Divide__Divide_var_C_u32
	.long	L_1+3729
	.long	Divide__uDivide_var_C_u32
	.long	L_1+3746
	.long	Divide__Divide_param_C_i8
	.long	L_1+3764
	.long	Divide__uDivide_param_C_i8
	.long	L_1+3782
	.long	Divide__Divide_var_C_i8
	.long	L_1+3801
	.long	Divide__uDivide_var_C_i8
	.long	L_1+3817
	.long	Divide__Divide_param_C_L
	.long	L_1+3834
	.long	Divide__uDivide_param_C_L
	.long	L_1+3851
	.long	Divide__Divide_var_C_L
	.long	L_1+3869
	.long	Divide__uDivide_var_C_L
	.long	L_1+3884
	.long	Divide__Divide_param_C_u8
	.long	L_1+3900
	.long	Divide__uDivide_param_C_u8
	.long	L_1+3918
	.long	Divide__Divide_var_C_u8
	.long	L_1+3937
	.long	Divide__uDivide_var_C_u8
	.long	L_1+3953
	.long	Divide__Divide_param_C_C
	.long	L_1+3970
	.long	Divide__uDivide_param_C_C
	.long	L_1+3987
	.long	Divide__Divide_var_C_C
	.long	L_1+4005
	.long	Divide__uDivide_var_C_C
	.long	L_1+4020
	.long	Divide__Divide_param_C_i64
	.long	L_1+4036
	.long	Divide__uDivide_param_C_i64
	.long	L_1+4055
	.long	Divide__Divide_var_C_i64
	.long	L_1+4075
	.long	Divide__uDivide_var_C_i64
	.long	L_1+4092
	.long	Divide__Divide_param_C_I
	.long	L_1+4110
	.long	Divide__uDivide_param_C_I
	.long	L_1+4127
	.long	Divide__Divide_var_C_I
	.long	L_1+4145
	.long	Divide__uDivide_var_C_I
	.long	L_1+4160
	.long	Divide__Divide_param_C_i16
	.long	L_1+4176
	.long	Divide__uDivide_param_C_i16
	.long	L_1+4195
	.long	Divide__Divide_var_C_i16
	.long	L_1+4215
	.long	Divide__uDivide_var_C_i16
	.long	L_1+4232
	.long	Divide__Divide_param_C_i32
	.long	L_1+4250
	.long	Divide__uDivide_param_C_i32
	.long	L_1+4269
	.long	Divide__Divide_var_C_i32
	.long	L_1+4289
	.long	Divide__uDivide_var_C_i32
	.long	L_1+4306
	.long	Divide__Divide_param_C_LC
	.long	L_1+4324
	.long	Divide__uDivide_param_C_LC
	.long	L_1+4342
	.long	Divide__Divide_var_C_LC
	.long	L_1+4361
	.long	Divide__uDivide_var_C_LC
	.long	L_1+4377
	.long	Divide__Divide_param_C_u64
	.long	L_1+4394
	.long	Divide__uDivide_param_C_u64
	.long	L_1+4413
	.long	Divide__Divide_var_C_u64
	.long	L_1+4433
	.long	Divide__uDivide_var_C_u64
	.long	L_1+4450
	.long	Divide__Divide_param_C_u16
	.long	L_1+4468
	.long	Divide__uDivide_param_C_u16
	.long	L_1+4487
	.long	Divide__Divide_var_C_u16
	.long	L_1+4507
	.long	Divide__uDivide_var_C_u16
	.long	L_1+4524
	.long	Divide__Divide_param_i64_u32
	.long	L_1+4542
	.long	Divide__uDivide_param_i64_u32
	.long	L_1+4563
	.long	Divide__Divide_var_i64_u32
	.long	L_1+4585
	.long	Divide__uDivide_var_i64_u32
	.long	L_1+4604
	.long	Divide__Divide_param_i64_i8
	.long	L_1+4624
	.long	Divide__uDivide_param_i64_i8
	.long	L_1+4644
	.long	Divide__Divide_var_i64_i8
	.long	L_1+4665
	.long	Divide__uDivide_var_i64_i8
	.long	L_1+4683
	.long	Divide__Divide_param_i64_L
	.long	L_1+4702
	.long	Divide__uDivide_param_i64_L
	.long	L_1+4721
	.long	Divide__Divide_var_i64_L
	.long	L_1+4741
	.long	Divide__uDivide_var_i64_L
	.long	L_1+4758
	.long	Divide__Divide_param_i64_u8
	.long	L_1+4776
	.long	Divide__uDivide_param_i64_u8
	.long	L_1+4796
	.long	Divide__Divide_var_i64_u8
	.long	L_1+4817
	.long	Divide__uDivide_var_i64_u8
	.long	L_1+4835
	.long	Divide__Divide_param_i64_C
	.long	L_1+4854
	.long	Divide__uDivide_param_i64_C
	.long	L_1+4873
	.long	Divide__Divide_var_i64_C
	.long	L_1+4893
	.long	Divide__uDivide_var_i64_C
	.long	L_1+4910
	.long	Divide__Divide_param_i64_i64
	.long	L_1+4928
	.long	Divide__uDivide_param_i64_i64
	.long	L_1+4949
	.long	Divide__Divide_var_i64_i64
	.long	L_1+4971
	.long	Divide__uDivide_var_i64_i64
	.long	L_1+4990
	.long	Divide__Divide_param_i64_I
	.long	L_1+5010
	.long	Divide__uDivide_param_i64_I
	.long	L_1+5029
	.long	Divide__Divide_var_i64_I
	.long	L_1+5049
	.long	Divide__uDivide_var_i64_I
	.long	L_1+5066
	.long	Divide__Divide_param_i64_i16
	.long	L_1+5084
	.long	Divide__uDivide_param_i64_i16
	.long	L_1+5105
	.long	Divide__Divide_var_i64_i16
	.long	L_1+5127
	.long	Divide__uDivide_var_i64_i16
	.long	L_1+5146
	.long	Divide__Divide_param_i64_i32
	.long	L_1+5166
	.long	Divide__uDivide_param_i64_i32
	.long	L_1+5187
	.long	Divide__Divide_var_i64_i32
	.long	L_1+5209
	.long	Divide__uDivide_var_i64_i32
	.long	L_1+5228
	.long	Divide__Divide_param_i64_LC
	.long	L_1+5248
	.long	Divide__uDivide_param_i64_LC
	.long	L_1+5268
	.long	Divide__Divide_var_i64_LC
	.long	L_1+5289
	.long	Divide__uDivide_var_i64_LC
	.long	L_1+5307
	.long	Divide__Divide_param_i64_u64
	.long	L_1+5326
	.long	Divide__uDivide_param_i64_u64
	.long	L_1+5347
	.long	Divide__Divide_var_i64_u64
	.long	L_1+5369
	.long	Divide__uDivide_var_i64_u64
	.long	L_1+5388
	.long	Divide__Divide_param_i64_u16
	.long	L_1+5408
	.long	Divide__uDivide_param_i64_u16
	.long	L_1+5429
	.long	Divide__Divide_var_i64_u16
	.long	L_1+5451
	.long	Divide__uDivide_var_i64_u16
	.long	L_1+5470
	.long	Divide__Divide_param_I_u32
	.long	L_1+5490
	.long	Divide__uDivide_param_I_u32
	.long	L_1+5509
	.long	Divide__Divide_var_I_u32
	.long	L_1+5529
	.long	Divide__uDivide_var_I_u32
	.long	L_1+5546
	.long	Divide__Divide_param_I_i8
	.long	L_1+5564
	.long	Divide__uDivide_param_I_i8
	.long	L_1+5582
	.long	Divide__Divide_var_I_i8
	.long	L_1+5601
	.long	Divide__uDivide_var_I_i8
	.long	L_1+5617
	.long	Divide__Divide_param_I_L
	.long	L_1+5634
	.long	Divide__uDivide_param_I_L
	.long	L_1+5651
	.long	Divide__Divide_var_I_L
	.long	L_1+5669
	.long	Divide__uDivide_var_I_L
	.long	L_1+5684
	.long	Divide__Divide_param_I_u8
	.long	L_1+5700
	.long	Divide__uDivide_param_I_u8
	.long	L_1+5718
	.long	Divide__Divide_var_I_u8
	.long	L_1+5737
	.long	Divide__uDivide_var_I_u8
	.long	L_1+5753
	.long	Divide__Divide_param_I_C
	.long	L_1+5770
	.long	Divide__uDivide_param_I_C
	.long	L_1+5787
	.long	Divide__Divide_var_I_C
	.long	L_1+5805
	.long	Divide__uDivide_var_I_C
	.long	L_1+5820
	.long	Divide__Divide_param_I_i64
	.long	L_1+5836
	.long	Divide__uDivide_param_I_i64
	.long	L_1+5855
	.long	Divide__Divide_var_I_i64
	.long	L_1+5875
	.long	Divide__uDivide_var_I_i64
	.long	L_1+5892
	.long	Divide__Divide_param_I_I
	.long	L_1+5910
	.long	Divide__uDivide_param_I_I
	.long	L_1+5927
	.long	Divide__Divide_var_I_I
	.long	L_1+5945
	.long	Divide__uDivide_var_I_I
	.long	L_1+5960
	.long	Divide__Divide_param_I_i16
	.long	L_1+5976
	.long	Divide__uDivide_param_I_i16
	.long	L_1+5995
	.long	Divide__Divide_var_I_i16
	.long	L_1+6015
	.long	Divide__uDivide_var_I_i16
	.long	L_1+6032
	.long	Divide__Divide_param_I_i32
	.long	L_1+6050
	.long	Divide__uDivide_param_I_i32
	.long	L_1+6069
	.long	Divide__Divide_var_I_i32
	.long	L_1+6089
	.long	Divide__uDivide_var_I_i32
	.long	L_1+6106
	.long	Divide__Divide_param_I_LC
	.long	L_1+6124
	.long	Divide__uDivide_param_I_LC
	.long	L_1+6142
	.long	Divide__Divide_var_I_LC
	.long	L_1+6161
	.long	Divide__uDivide_var_I_LC
	.long	L_1+6177
	.long	Divide__Divide_param_I_u64
	.long	L_1+6194
	.long	Divide__uDivide_param_I_u64
	.long	L_1+6213
	.long	Divide__Divide_var_I_u64
	.long	L_1+6233
	.long	Divide__uDivide_var_I_u64
	.long	L_1+6250
	.long	Divide__Divide_param_I_u16
	.long	L_1+6268
	.long	Divide__uDivide_param_I_u16
	.long	L_1+6287
	.long	Divide__Divide_var_I_u16
	.long	L_1+6307
	.long	Divide__uDivide_var_I_u16
	.long	L_1+6324
	.long	Divide__Divide_param_i16_u32
	.long	L_1+6342
	.long	Divide__uDivide_param_i16_u32
	.long	L_1+6363
	.long	Divide__Divide_var_i16_u32
	.long	L_1+6385
	.long	Divide__uDivide_var_i16_u32
	.long	L_1+6404
	.long	Divide__Divide_param_i16_i8
	.long	L_1+6424
	.long	Divide__uDivide_param_i16_i8
	.long	L_1+6444
	.long	Divide__Divide_var_i16_i8
	.long	L_1+6465
	.long	Divide__uDivide_var_i16_i8
	.long	L_1+6483
	.long	Divide__Divide_param_i16_L
	.long	L_1+6502
	.long	Divide__uDivide_param_i16_L
	.long	L_1+6521
	.long	Divide__Divide_var_i16_L
	.long	L_1+6541
	.long	Divide__uDivide_var_i16_L
	.long	L_1+6558
	.long	Divide__Divide_param_i16_u8
	.long	L_1+6576
	.long	Divide__uDivide_param_i16_u8
	.long	L_1+6596
	.long	Divide__Divide_var_i16_u8
	.long	L_1+6617
	.long	Divide__uDivide_var_i16_u8
	.long	L_1+6635
	.long	Divide__Divide_param_i16_C
	.long	L_1+6654
	.long	Divide__uDivide_param_i16_C
	.long	L_1+6673
	.long	Divide__Divide_var_i16_C
	.long	L_1+6693
	.long	Divide__uDivide_var_i16_C
	.long	L_1+6710
	.long	Divide__Divide_param_i16_i64
	.long	L_1+6728
	.long	Divide__uDivide_param_i16_i64
	.long	L_1+6749
	.long	Divide__Divide_var_i16_i64
	.long	L_1+6771
	.long	Divide__uDivide_var_i16_i64
	.long	L_1+6790
	.long	Divide__Divide_param_i16_I
	.long	L_1+6810
	.long	Divide__uDivide_param_i16_I
	.long	L_1+6829
	.long	Divide__Divide_var_i16_I
	.long	L_1+6849
	.long	Divide__uDivide_var_i16_I
	.long	L_1+6866
	.long	Divide__Divide_param_i16_i16
	.long	L_1+6884
	.long	Divide__uDivide_param_i16_i16
	.long	L_1+6905
	.long	Divide__Divide_var_i16_i16
	.long	L_1+6927
	.long	Divide__uDivide_var_i16_i16
	.long	L_1+6946
	.long	Divide__Divide_param_i16_i32
	.long	L_1+6966
	.long	Divide__uDivide_param_i16_i32
	.long	L_1+6987
	.long	Divide__Divide_var_i16_i32
	.long	L_1+7009
	.long	Divide__uDivide_var_i16_i32
	.long	L_1+7028
	.long	Divide__Divide_param_i16_LC
	.long	L_1+7048
	.long	Divide__uDivide_param_i16_LC
	.long	L_1+7068
	.long	Divide__Divide_var_i16_LC
	.long	L_1+7089
	.long	Divide__uDivide_var_i16_LC
	.long	L_1+7107
	.long	Divide__Divide_param_i16_u64
	.long	L_1+7126
	.long	Divide__uDivide_param_i16_u64
	.long	L_1+7147
	.long	Divide__Divide_var_i16_u64
	.long	L_1+7169
	.long	Divide__uDivide_var_i16_u64
	.long	L_1+7188
	.long	Divide__Divide_param_i16_u16
	.long	L_1+7208
	.long	Divide__uDivide_param_i16_u16
	.long	L_1+7229
	.long	Divide__Divide_var_i16_u16
	.long	L_1+7251
	.long	Divide__uDivide_var_i16_u16
	.long	L_1+7270
	.long	Divide__Divide_param_i32_u32
	.long	L_1+7290
	.long	Divide__uDivide_param_i32_u32
	.long	L_1+7311
	.long	Divide__Divide_var_i32_u32
	.long	L_1+7333
	.long	Divide__uDivide_var_i32_u32
	.long	L_1+7352
	.long	Divide__Divide_param_i32_i8
	.long	L_1+7372
	.long	Divide__uDivide_param_i32_i8
	.long	L_1+7392
	.long	Divide__Divide_var_i32_i8
	.long	L_1+7413
	.long	Divide__uDivide_var_i32_i8
	.long	L_1+7431
	.long	Divide__Divide_param_i32_L
	.long	L_1+7450
	.long	Divide__uDivide_param_i32_L
	.long	L_1+7469
	.long	Divide__Divide_var_i32_L
	.long	L_1+7489
	.long	Divide__uDivide_var_i32_L
	.long	L_1+7506
	.long	Divide__Divide_param_i32_u8
	.long	L_1+7524
	.long	Divide__uDivide_param_i32_u8
	.long	L_1+7544
	.long	Divide__Divide_var_i32_u8
	.long	L_1+7565
	.long	Divide__uDivide_var_i32_u8
	.long	L_1+7583
	.long	Divide__Divide_param_i32_C
	.long	L_1+7602
	.long	Divide__uDivide_param_i32_C
	.long	L_1+7621
	.long	Divide__Divide_var_i32_C
	.long	L_1+7641
	.long	Divide__uDivide_var_i32_C
	.long	L_1+7658
	.long	Divide__Divide_param_i32_i64
	.long	L_1+7676
	.long	Divide__uDivide_param_i32_i64
	.long	L_1+7697
	.long	Divide__Divide_var_i32_i64
	.long	L_1+7719
	.long	Divide__uDivide_var_i32_i64
	.long	L_1+7738
	.long	Divide__Divide_param_i32_I
	.long	L_1+7758
	.long	Divide__uDivide_param_i32_I
	.long	L_1+7777
	.long	Divide__Divide_var_i32_I
	.long	L_1+7797
	.long	Divide__uDivide_var_i32_I
	.long	L_1+7814
	.long	Divide__Divide_param_i32_i16
	.long	L_1+7832
	.long	Divide__uDivide_param_i32_i16
	.long	L_1+7853
	.long	Divide__Divide_var_i32_i16
	.long	L_1+7875
	.long	Divide__uDivide_var_i32_i16
	.long	L_1+7894
	.long	Divide__Divide_param_i32_i32
	.long	L_1+7914
	.long	Divide__uDivide_param_i32_i32
	.long	L_1+7935
	.long	Divide__Divide_var_i32_i32
	.long	L_1+7957
	.long	Divide__uDivide_var_i32_i32
	.long	L_1+7976
	.long	Divide__Divide_param_i32_LC
	.long	L_1+7996
	.long	Divide__uDivide_param_i32_LC
	.long	L_1+8016
	.long	Divide__Divide_var_i32_LC
	.long	L_1+8037
	.long	Divide__uDivide_var_i32_LC
	.long	L_1+8055
	.long	Divide__Divide_param_i32_u64
	.long	L_1+8074
	.long	Divide__uDivide_param_i32_u64
	.long	L_1+8095
	.long	Divide__Divide_var_i32_u64
	.long	L_1+8117
	.long	Divide__uDivide_var_i32_u64
	.long	L_1+8136
	.long	Divide__Divide_param_i32_u16
	.long	L_1+8156
	.long	Divide__uDivide_param_i32_u16
	.long	L_1+8177
	.long	Divide__Divide_var_i32_u16
	.long	L_1+8199
	.long	Divide__uDivide_var_i32_u16
	.long	L_1+8218
	.long	Divide__Divide_param_LC_u32
	.long	L_1+8238
	.long	Divide__uDivide_param_LC_u32
	.long	L_1+8258
	.long	Divide__Divide_var_LC_u32
	.long	L_1+8279
	.long	Divide__uDivide_var_LC_u32
	.long	L_1+8297
	.long	Divide__Divide_param_LC_i8
	.long	L_1+8316
	.long	Divide__uDivide_param_LC_i8
	.long	L_1+8335
	.long	Divide__Divide_var_LC_i8
	.long	L_1+8355
	.long	Divide__uDivide_var_LC_i8
	.long	L_1+8372
	.long	Divide__Divide_param_LC_L
	.long	L_1+8390
	.long	Divide__uDivide_param_LC_L
	.long	L_1+8408
	.long	Divide__Divide_var_LC_L
	.long	L_1+8427
	.long	Divide__uDivide_var_LC_L
	.long	L_1+8443
	.long	Divide__Divide_param_LC_u8
	.long	L_1+8460
	.long	Divide__uDivide_param_LC_u8
	.long	L_1+8479
	.long	Divide__Divide_var_LC_u8
	.long	L_1+8499
	.long	Divide__uDivide_var_LC_u8
	.long	L_1+8516
	.long	Divide__Divide_param_LC_C
	.long	L_1+8534
	.long	Divide__uDivide_param_LC_C
	.long	L_1+8552
	.long	Divide__Divide_var_LC_C
	.long	L_1+8571
	.long	Divide__uDivide_var_LC_C
	.long	L_1+8587
	.long	Divide__Divide_param_LC_i64
	.long	L_1+8604
	.long	Divide__uDivide_param_LC_i64
	.long	L_1+8624
	.long	Divide__Divide_var_LC_i64
	.long	L_1+8645
	.long	Divide__uDivide_var_LC_i64
	.long	L_1+8663
	.long	Divide__Divide_param_LC_I
	.long	L_1+8682
	.long	Divide__uDivide_param_LC_I
	.long	L_1+8700
	.long	Divide__Divide_var_LC_I
	.long	L_1+8719
	.long	Divide__uDivide_var_LC_I
	.long	L_1+8735
	.long	Divide__Divide_param_LC_i16
	.long	L_1+8752
	.long	Divide__uDivide_param_LC_i16
	.long	L_1+8772
	.long	Divide__Divide_var_LC_i16
	.long	L_1+8793
	.long	Divide__uDivide_var_LC_i16
	.long	L_1+8811
	.long	Divide__Divide_param_LC_i32
	.long	L_1+8830
	.long	Divide__uDivide_param_LC_i32
	.long	L_1+8850
	.long	Divide__Divide_var_LC_i32
	.long	L_1+8871
	.long	Divide__uDivide_var_LC_i32
	.long	L_1+8889
	.long	Divide__Divide_param_LC_LC
	.long	L_1+8908
	.long	Divide__uDivide_param_LC_LC
	.long	L_1+8927
	.long	Divide__Divide_var_LC_LC
	.long	L_1+8947
	.long	Divide__uDivide_var_LC_LC
	.long	L_1+8964
	.long	Divide__Divide_param_LC_u64
	.long	L_1+8982
	.long	Divide__uDivide_param_LC_u64
	.long	L_1+9002
	.long	Divide__Divide_var_LC_u64
	.long	L_1+9023
	.long	Divide__uDivide_var_LC_u64
	.long	L_1+9041
	.long	Divide__Divide_param_LC_u16
	.long	L_1+9060
	.long	Divide__uDivide_param_LC_u16
	.long	L_1+9080
	.long	Divide__Divide_var_LC_u16
	.long	L_1+9101
	.long	Divide__uDivide_var_LC_u16
	.long	L_1+9119
	.long	Divide__Divide_param_u64_u32
	.long	L_1+9138
	.long	Divide__uDivide_param_u64_u32
	.long	L_1+9159
	.long	Divide__Divide_var_u64_u32
	.long	L_1+9181
	.long	Divide__uDivide_var_u64_u32
	.long	L_1+9200
	.long	Divide__Divide_param_u64_i8
	.long	L_1+9220
	.long	Divide__uDivide_param_u64_i8
	.long	L_1+9240
	.long	Divide__Divide_var_u64_i8
	.long	L_1+9261
	.long	Divide__uDivide_var_u64_i8
	.long	L_1+9279
	.long	Divide__Divide_param_u64_L
	.long	L_1+9298
	.long	Divide__uDivide_param_u64_L
	.long	L_1+9317
	.long	Divide__Divide_var_u64_L
	.long	L_1+9337
	.long	Divide__uDivide_var_u64_L
	.long	L_1+9354
	.long	Divide__Divide_param_u64_u8
	.long	L_1+9372
	.long	Divide__uDivide_param_u64_u8
	.long	L_1+9392
	.long	Divide__Divide_var_u64_u8
	.long	L_1+9413
	.long	Divide__uDivide_var_u64_u8
	.long	L_1+9431
	.long	Divide__Divide_param_u64_C
	.long	L_1+9450
	.long	Divide__uDivide_param_u64_C
	.long	L_1+9469
	.long	Divide__Divide_var_u64_C
	.long	L_1+9489
	.long	Divide__uDivide_var_u64_C
	.long	L_1+9506
	.long	Divide__Divide_param_u64_i64
	.long	L_1+9524
	.long	Divide__uDivide_param_u64_i64
	.long	L_1+9545
	.long	Divide__Divide_var_u64_i64
	.long	L_1+9567
	.long	Divide__uDivide_var_u64_i64
	.long	L_1+9586
	.long	Divide__Divide_param_u64_I
	.long	L_1+9606
	.long	Divide__uDivide_param_u64_I
	.long	L_1+9625
	.long	Divide__Divide_var_u64_I
	.long	L_1+9645
	.long	Divide__uDivide_var_u64_I
	.long	L_1+9662
	.long	Divide__Divide_param_u64_i16
	.long	L_1+9680
	.long	Divide__uDivide_param_u64_i16
	.long	L_1+9701
	.long	Divide__Divide_var_u64_i16
	.long	L_1+9723
	.long	Divide__uDivide_var_u64_i16
	.long	L_1+9742
	.long	Divide__Divide_param_u64_i32
	.long	L_1+9762
	.long	Divide__uDivide_param_u64_i32
	.long	L_1+9783
	.long	Divide__Divide_var_u64_i32
	.long	L_1+9805
	.long	Divide__uDivide_var_u64_i32
	.long	L_1+9824
	.long	Divide__Divide_param_u64_LC
	.long	L_1+9844
	.long	Divide__uDivide_param_u64_LC
	.long	L_1+9864
	.long	Divide__Divide_var_u64_LC
	.long	L_1+9885
	.long	Divide__uDivide_var_u64_LC
	.long	L_1+9903
	.long	Divide__Divide_param_u64_u64
	.long	L_1+9922
	.long	Divide__uDivide_param_u64_u64
	.long	L_1+9943
	.long	Divide__Divide_var_u64_u64
	.long	L_1+9965
	.long	Divide__uDivide_var_u64_u64
	.long	L_1+9984
	.long	Divide__Divide_param_u64_u16
	.long	L_1+10004
	.long	Divide__uDivide_param_u64_u16
	.long	L_1+10025
	.long	Divide__Divide_var_u64_u16
	.long	L_1+10047
	.long	Divide__uDivide_var_u64_u16
	.long	L_1+10066
	.long	Divide__Divide_param_u16_u32
	.long	L_1+10086
	.long	Divide__uDivide_param_u16_u32
	.long	L_1+10107
	.long	Divide__Divide_var_u16_u32
	.long	L_1+10129
	.long	Divide__uDivide_var_u16_u32
	.long	L_1+10148
	.long	Divide__Divide_param_u16_i8
	.long	L_1+10168
	.long	Divide__uDivide_param_u16_i8
	.long	L_1+10188
	.long	Divide__Divide_var_u16_i8
	.long	L_1+10209
	.long	Divide__uDivide_var_u16_i8
	.long	L_1+10227
	.long	Divide__Divide_param_u16_L
	.long	L_1+10246
	.long	Divide__uDivide_param_u16_L
	.long	L_1+10265
	.long	Divide__Divide_var_u16_L
	.long	L_1+10285
	.long	Divide__uDivide_var_u16_L
	.long	L_1+10302
	.long	Divide__Divide_param_u16_u8
	.long	L_1+10320
	.long	Divide__uDivide_param_u16_u8
	.long	L_1+10340
	.long	Divide__Divide_var_u16_u8
	.long	L_1+10361
	.long	Divide__uDivide_var_u16_u8
	.long	L_1+10379
	.long	Divide__Divide_param_u16_C
	.long	L_1+10398
	.long	Divide__uDivide_param_u16_C
	.long	L_1+10417
	.long	Divide__Divide_var_u16_C
	.long	L_1+10437
	.long	Divide__uDivide_var_u16_C
	.long	L_1+10454
	.long	Divide__Divide_param_u16_i64
	.long	L_1+10472
	.long	Divide__uDivide_param_u16_i64
	.long	L_1+10493
	.long	Divide__Divide_var_u16_i64
	.long	L_1+10515
	.long	Divide__uDivide_var_u16_i64
	.long	L_1+10534
	.long	Divide__Divide_param_u16_I
	.long	L_1+10554
	.long	Divide__uDivide_param_u16_I
	.long	L_1+10573
	.long	Divide__Divide_var_u16_I
	.long	L_1+10593
	.long	Divide__uDivide_var_u16_I
	.long	L_1+10610
	.long	Divide__Divide_param_u16_i16
	.long	L_1+10628
	.long	Divide__uDivide_param_u16_i16
	.long	L_1+10649
	.long	Divide__Divide_var_u16_i16
	.long	L_1+10671
	.long	Divide__uDivide_var_u16_i16
	.long	L_1+10690
	.long	Divide__Divide_param_u16_i32
	.long	L_1+10710
	.long	Divide__uDivide_param_u16_i32
	.long	L_1+10731
	.long	Divide__Divide_var_u16_i32
	.long	L_1+10753
	.long	Divide__uDivide_var_u16_i32
	.long	L_1+10772
	.long	Divide__Divide_param_u16_LC
	.long	L_1+10792
	.long	Divide__uDivide_param_u16_LC
	.long	L_1+10812
	.long	Divide__Divide_var_u16_LC
	.long	L_1+10833
	.long	Divide__uDivide_var_u16_LC
	.long	L_1+10851
	.long	Divide__Divide_param_u16_u64
	.long	L_1+10870
	.long	Divide__uDivide_param_u16_u64
	.long	L_1+10891
	.long	Divide__Divide_var_u16_u64
	.long	L_1+10913
	.long	Divide__uDivide_var_u16_u64
	.long	L_1+10932
	.long	Divide__Divide_param_u16_u16
	.long	L_1+10952
	.long	Divide__uDivide_param_u16_u16
	.long	L_1+10973
	.long	Divide__Divide_var_u16_u16
	.long	L_1+10995
	.long	Divide__uDivide_var_u16_u16
	.long	L_1+11014
	.skip 4
	.ascii	"../SOLsun/Divide.m3"
	.skip 5
	.align 8
	.type	MM_Divide, #object
	.size	MM_Divide, 208
MM_Divide:
	.long	L_1+15688
	.skip 16
	.long	L_1+11036
	.skip 12
	.long	MM_Divide+148
	.skip 4
	.long	Divide_M3
	.long	3
	.half	241
	.skip 2
	.long	0
	.long	242
	.long	0
	.long	243
	.long	244
	.half	245
	.skip 2
	.long	246
	.skip 4
	.long	0
	.long	247
	.long	248
	.skip 4
	.long	1081026560
	.long	0
	.byte	-5
	.skip 7
	.long	0
	.long	252
	.byte	-3
	.skip 3
	.long	254
	.long	1132413321
	.skip 12
	.long	Divide_I3
	.long	MM_Divide+160
	.skip 4
	.long	Long_I3
	.long	MM_Divide+172
	.skip 4
	.long	Word_I3
	.long	MM_Divide+184
	.skip 4
	.long	Cstdint_I3
	.long	MM_Divide+196
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
