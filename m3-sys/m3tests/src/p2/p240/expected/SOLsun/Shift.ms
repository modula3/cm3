	.file	"Shift.mc"
	.section	".text"
	.align 4
	.type	Shift__uShift_var_u16_I, #function
	.proc	04
Shift__uShift_var_u16_I:
.LLFB0:
	save	%sp, -96, %sp
.LLCFI0:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL2
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 31
	bg	%icc, .LL3
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	sll	%o5, %g1, %g1
	ba,pt	%xcc, .LL4
	 nop
.LL3:
	mov	0, %g1
.LL4:
	ba,pt	%xcc, .LL5
	 nop
.LL2:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 31
	bg	%icc, .LL6
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	sub	%g0, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL7
	 nop
.LL6:
	mov	0, %g1
.LL7:
.LL5:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE0:
	.size	Shift__uShift_var_u16_I, .-Shift__uShift_var_u16_I
	.align 4
	.type	Shift__uShift_param_u16_I, #function
	.proc	04
Shift__uShift_param_u16_I:
.LLFB1:
	save	%sp, -96, %sp
.LLCFI1:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL9
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	bg	%icc, .LL10
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	sll	%o5, %g1, %g1
	ba,pt	%xcc, .LL11
	 nop
.LL10:
	mov	0, %g1
.LL11:
	ba,pt	%xcc, .LL12
	 nop
.LL9:
	ld	[%fp+72], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 31
	bg	%icc, .LL13
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	sub	%g0, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL14
	 nop
.LL13:
	mov	0, %g1
.LL14:
.LL12:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1:
	.size	Shift__uShift_param_u16_I, .-Shift__uShift_param_u16_I
	.align 4
	.type	Shift__uShift_var_u16_C, #function
	.proc	04
Shift__uShift_var_u16_C:
.LLFB2:
	save	%sp, -96, %sp
.LLCFI2:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 0
	bl	%icc, .LL16
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 31
	bg	%icc, .LL17
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	sll	%o5, %g1, %g1
	ba,pt	%xcc, .LL18
	 nop
.LL17:
	mov	0, %g1
.LL18:
	ba,pt	%xcc, .LL19
	 nop
.LL16:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 31
	bg	%icc, .LL20
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	sub	%g0, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL21
	 nop
.LL20:
	mov	0, %g1
.LL21:
.LL19:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE2:
	.size	Shift__uShift_var_u16_C, .-Shift__uShift_var_u16_C
	.align 4
	.type	Shift__uShift_param_u16_C, #function
	.proc	04
Shift__uShift_param_u16_C:
.LLFB3:
	save	%sp, -96, %sp
.LLCFI3:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL23
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	bg	%icc, .LL24
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	sll	%o5, %g1, %g1
	ba,pt	%xcc, .LL25
	 nop
.LL24:
	mov	0, %g1
.LL25:
	ba,pt	%xcc, .LL26
	 nop
.LL23:
	ld	[%fp+72], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 31
	bg	%icc, .LL27
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	sub	%g0, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL28
	 nop
.LL27:
	mov	0, %g1
.LL28:
.LL26:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE3:
	.size	Shift__uShift_param_u16_C, .-Shift__uShift_param_u16_C
	.align 4
	.type	Shift__uShift_var_u64_I, #function
	.proc	05
Shift__uShift_var_u64_I:
.LLFB4:
	save	%sp, -96, %sp
.LLCFI4:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL30
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 63
	bg	%icc, .LL31
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ldd	[%g1+56], %o4
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL32
	 nop
.LL31:
	mov	0, %o4
	mov	0, %o5
.LL32:
	ba,pt	%xcc, .LL33
	 nop
.LL30:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 63
	bg	%icc, .LL34
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ldd	[%g1+56], %o4
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	sub	%g0, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL35
	 nop
.LL34:
	mov	0, %o4
	mov	0, %o5
.LL35:
.LL33:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE4:
	.size	Shift__uShift_var_u64_I, .-Shift__uShift_var_u64_I
	.align 4
	.type	Shift__uShift_param_u64_I, #function
	.proc	05
Shift__uShift_param_u64_I:
.LLFB5:
	save	%sp, -104, %sp
.LLCFI5:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	cmp	%g1, 0
	bl	%icc, .LL37
	 nop
	ld	[%fp+76], %g1
	cmp	%g1, 63
	bg	%icc, .LL38
	 nop
	ldd	[%fp-8], %o4
	ld	[%fp+76], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL39
	 nop
.LL38:
	mov	0, %o4
	mov	0, %o5
.LL39:
	ba,pt	%xcc, .LL40
	 nop
.LL37:
	ld	[%fp+76], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 63
	bg	%icc, .LL41
	 nop
	ldd	[%fp-8], %o4
	ld	[%fp+76], %g1
	sub	%g0, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL42
	 nop
.LL41:
	mov	0, %o4
	mov	0, %o5
.LL42:
.LL40:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE5:
	.size	Shift__uShift_param_u64_I, .-Shift__uShift_param_u64_I
	.align 4
	.type	Shift__uShift_var_u64_C, #function
	.proc	05
Shift__uShift_var_u64_C:
.LLFB6:
	save	%sp, -96, %sp
.LLCFI6:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 0
	bl	%icc, .LL44
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 63
	bg	%icc, .LL45
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ldd	[%g1+56], %o4
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL46
	 nop
.LL45:
	mov	0, %o4
	mov	0, %o5
.LL46:
	ba,pt	%xcc, .LL47
	 nop
.LL44:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 63
	bg	%icc, .LL48
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ldd	[%g1+56], %o4
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	sub	%g0, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL49
	 nop
.LL48:
	mov	0, %o4
	mov	0, %o5
.LL49:
.LL47:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE6:
	.size	Shift__uShift_var_u64_C, .-Shift__uShift_var_u64_C
	.align 4
	.type	Shift__uShift_param_u64_C, #function
	.proc	05
Shift__uShift_param_u64_C:
.LLFB7:
	save	%sp, -104, %sp
.LLCFI7:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	cmp	%g1, 0
	bl	%icc, .LL51
	 nop
	ld	[%fp+76], %g1
	cmp	%g1, 63
	bg	%icc, .LL52
	 nop
	ldd	[%fp-8], %o4
	ld	[%fp+76], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL53
	 nop
.LL52:
	mov	0, %o4
	mov	0, %o5
.LL53:
	ba,pt	%xcc, .LL54
	 nop
.LL51:
	ld	[%fp+76], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 63
	bg	%icc, .LL55
	 nop
	ldd	[%fp-8], %o4
	ld	[%fp+76], %g1
	sub	%g0, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL56
	 nop
.LL55:
	mov	0, %o4
	mov	0, %o5
.LL56:
.LL54:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE7:
	.size	Shift__uShift_param_u64_C, .-Shift__uShift_param_u64_C
	.align 4
	.type	Shift__uShift_var_LC_I, #function
	.proc	05
Shift__uShift_var_LC_I:
.LLFB8:
	save	%sp, -96, %sp
.LLCFI8:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL58
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 63
	bg	%icc, .LL59
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ldd	[%g1+64], %o4
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL60
	 nop
.LL59:
	mov	0, %o4
	mov	0, %o5
.LL60:
	ba,pt	%xcc, .LL61
	 nop
.LL58:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 63
	bg	%icc, .LL62
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ldd	[%g1+64], %o4
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	sub	%g0, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL63
	 nop
.LL62:
	mov	0, %o4
	mov	0, %o5
.LL63:
.LL61:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE8:
	.size	Shift__uShift_var_LC_I, .-Shift__uShift_var_LC_I
	.align 4
	.type	Shift__uShift_param_LC_I, #function
	.proc	05
Shift__uShift_param_LC_I:
.LLFB9:
	save	%sp, -104, %sp
.LLCFI9:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	cmp	%g1, 0
	bl	%icc, .LL65
	 nop
	ld	[%fp+76], %g1
	cmp	%g1, 63
	bg	%icc, .LL66
	 nop
	ldd	[%fp-8], %o4
	ld	[%fp+76], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL67
	 nop
.LL66:
	mov	0, %o4
	mov	0, %o5
.LL67:
	ba,pt	%xcc, .LL68
	 nop
.LL65:
	ld	[%fp+76], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 63
	bg	%icc, .LL69
	 nop
	ld	[%fp+76], %g1
	sub	%g0, %g1, %g1
	ldd	[%fp-8], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL70
	 nop
.LL69:
	mov	0, %o4
	mov	0, %o5
.LL70:
.LL68:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE9:
	.size	Shift__uShift_param_LC_I, .-Shift__uShift_param_LC_I
	.align 4
	.type	Shift__uShift_var_LC_C, #function
	.proc	05
Shift__uShift_var_LC_C:
.LLFB10:
	save	%sp, -96, %sp
.LLCFI10:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 0
	bl	%icc, .LL72
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 63
	bg	%icc, .LL73
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ldd	[%g1+64], %o4
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL74
	 nop
.LL73:
	mov	0, %o4
	mov	0, %o5
.LL74:
	ba,pt	%xcc, .LL75
	 nop
.LL72:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 63
	bg	%icc, .LL76
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ldd	[%g1+64], %o4
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	sub	%g0, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL77
	 nop
.LL76:
	mov	0, %o4
	mov	0, %o5
.LL77:
.LL75:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE10:
	.size	Shift__uShift_var_LC_C, .-Shift__uShift_var_LC_C
	.align 4
	.type	Shift__uShift_param_LC_C, #function
	.proc	05
Shift__uShift_param_LC_C:
.LLFB11:
	save	%sp, -104, %sp
.LLCFI11:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	cmp	%g1, 0
	bl	%icc, .LL79
	 nop
	ld	[%fp+76], %g1
	cmp	%g1, 63
	bg	%icc, .LL80
	 nop
	ld	[%fp+76], %g1
	ldd	[%fp-8], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL81
	 nop
.LL80:
	mov	0, %o4
	mov	0, %o5
.LL81:
	ba,pt	%xcc, .LL82
	 nop
.LL79:
	ld	[%fp+76], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 63
	bg	%icc, .LL83
	 nop
	ld	[%fp+76], %g1
	sub	%g0, %g1, %g1
	ldd	[%fp-8], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL84
	 nop
.LL83:
	mov	0, %o4
	mov	0, %o5
.LL84:
.LL82:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE11:
	.size	Shift__uShift_param_LC_C, .-Shift__uShift_param_LC_C
	.align 4
	.type	Shift__uShift_var_i32_I, #function
	.proc	04
Shift__uShift_var_i32_I:
.LLFB12:
	save	%sp, -96, %sp
.LLCFI12:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL86
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 31
	bg	%icc, .LL87
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	sll	%o5, %g1, %g1
	ba,pt	%xcc, .LL88
	 nop
.LL87:
	mov	0, %g1
.LL88:
	ba,pt	%xcc, .LL89
	 nop
.LL86:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 31
	bg	%icc, .LL90
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	sub	%g0, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL91
	 nop
.LL90:
	mov	0, %g1
.LL91:
.LL89:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE12:
	.size	Shift__uShift_var_i32_I, .-Shift__uShift_var_i32_I
	.align 4
	.type	Shift__uShift_param_i32_I, #function
	.proc	04
Shift__uShift_param_i32_I:
.LLFB13:
	save	%sp, -96, %sp
.LLCFI13:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL93
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	bg	%icc, .LL94
	 nop
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	sll	%o5, %g1, %g1
	ba,pt	%xcc, .LL95
	 nop
.LL94:
	mov	0, %g1
.LL95:
	ba,pt	%xcc, .LL96
	 nop
.LL93:
	ld	[%fp+72], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 31
	bg	%icc, .LL97
	 nop
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	sub	%g0, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL98
	 nop
.LL97:
	mov	0, %g1
.LL98:
.LL96:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE13:
	.size	Shift__uShift_param_i32_I, .-Shift__uShift_param_i32_I
	.align 4
	.type	Shift__uShift_var_i32_C, #function
	.proc	04
Shift__uShift_var_i32_C:
.LLFB14:
	save	%sp, -96, %sp
.LLCFI14:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 0
	bl	%icc, .LL100
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 31
	bg	%icc, .LL101
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	sll	%o5, %g1, %g1
	ba,pt	%xcc, .LL102
	 nop
.LL101:
	mov	0, %g1
.LL102:
	ba,pt	%xcc, .LL103
	 nop
.LL100:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 31
	bg	%icc, .LL104
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	sub	%g0, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL105
	 nop
.LL104:
	mov	0, %g1
.LL105:
.LL103:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE14:
	.size	Shift__uShift_var_i32_C, .-Shift__uShift_var_i32_C
	.align 4
	.type	Shift__uShift_param_i32_C, #function
	.proc	04
Shift__uShift_param_i32_C:
.LLFB15:
	save	%sp, -96, %sp
.LLCFI15:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL107
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	bg	%icc, .LL108
	 nop
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	sll	%o5, %g1, %g1
	ba,pt	%xcc, .LL109
	 nop
.LL108:
	mov	0, %g1
.LL109:
	ba,pt	%xcc, .LL110
	 nop
.LL107:
	ld	[%fp+72], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 31
	bg	%icc, .LL111
	 nop
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	sub	%g0, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL112
	 nop
.LL111:
	mov	0, %g1
.LL112:
.LL110:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE15:
	.size	Shift__uShift_param_i32_C, .-Shift__uShift_param_i32_C
	.align 4
	.type	Shift__uShift_var_i16_I, #function
	.proc	04
Shift__uShift_var_i16_I:
.LLFB16:
	save	%sp, -96, %sp
.LLCFI16:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL114
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 31
	bg	%icc, .LL115
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	sll	%o5, %g1, %g1
	ba,pt	%xcc, .LL116
	 nop
.LL115:
	mov	0, %g1
.LL116:
	ba,pt	%xcc, .LL117
	 nop
.LL114:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 31
	bg	%icc, .LL118
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	sub	%g0, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL119
	 nop
.LL118:
	mov	0, %g1
.LL119:
.LL117:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE16:
	.size	Shift__uShift_var_i16_I, .-Shift__uShift_var_i16_I
	.align 4
	.type	Shift__uShift_param_i16_I, #function
	.proc	04
Shift__uShift_param_i16_I:
.LLFB17:
	save	%sp, -96, %sp
.LLCFI17:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL121
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	bg	%icc, .LL122
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	sll	%o5, %g1, %g1
	ba,pt	%xcc, .LL123
	 nop
.LL122:
	mov	0, %g1
.LL123:
	ba,pt	%xcc, .LL124
	 nop
.LL121:
	ld	[%fp+72], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 31
	bg	%icc, .LL125
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	sub	%g0, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL126
	 nop
.LL125:
	mov	0, %g1
.LL126:
.LL124:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE17:
	.size	Shift__uShift_param_i16_I, .-Shift__uShift_param_i16_I
	.align 4
	.type	Shift__uShift_var_i16_C, #function
	.proc	04
Shift__uShift_var_i16_C:
.LLFB18:
	save	%sp, -96, %sp
.LLCFI18:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 0
	bl	%icc, .LL128
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 31
	bg	%icc, .LL129
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	sll	%o5, %g1, %g1
	ba,pt	%xcc, .LL130
	 nop
.LL129:
	mov	0, %g1
.LL130:
	ba,pt	%xcc, .LL131
	 nop
.LL128:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 31
	bg	%icc, .LL132
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	sub	%g0, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL133
	 nop
.LL132:
	mov	0, %g1
.LL133:
.LL131:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE18:
	.size	Shift__uShift_var_i16_C, .-Shift__uShift_var_i16_C
	.align 4
	.type	Shift__uShift_param_i16_C, #function
	.proc	04
Shift__uShift_param_i16_C:
.LLFB19:
	save	%sp, -96, %sp
.LLCFI19:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL135
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	bg	%icc, .LL136
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	sll	%o5, %g1, %g1
	ba,pt	%xcc, .LL137
	 nop
.LL136:
	mov	0, %g1
.LL137:
	ba,pt	%xcc, .LL138
	 nop
.LL135:
	ld	[%fp+72], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 31
	bg	%icc, .LL139
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	sub	%g0, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL140
	 nop
.LL139:
	mov	0, %g1
.LL140:
.LL138:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE19:
	.size	Shift__uShift_param_i16_C, .-Shift__uShift_param_i16_C
	.align 4
	.type	Shift__uShift_var_I_I, #function
	.proc	04
Shift__uShift_var_I_I:
.LLFB20:
	save	%sp, -96, %sp
.LLCFI20:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL142
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 31
	bg	%icc, .LL143
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	sll	%o5, %g1, %g1
	ba,pt	%xcc, .LL144
	 nop
.LL143:
	mov	0, %g1
.LL144:
	ba,pt	%xcc, .LL145
	 nop
.LL142:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 31
	bg	%icc, .LL146
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	sub	%g0, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL147
	 nop
.LL146:
	mov	0, %g1
.LL147:
.LL145:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE20:
	.size	Shift__uShift_var_I_I, .-Shift__uShift_var_I_I
	.align 4
	.type	Shift__uShift_param_I_I, #function
	.proc	04
Shift__uShift_param_I_I:
.LLFB21:
	save	%sp, -96, %sp
.LLCFI21:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL149
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	bg	%icc, .LL150
	 nop
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	sll	%o5, %g1, %g1
	ba,pt	%xcc, .LL151
	 nop
.LL150:
	mov	0, %g1
.LL151:
	ba,pt	%xcc, .LL152
	 nop
.LL149:
	ld	[%fp+72], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 31
	bg	%icc, .LL153
	 nop
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	sub	%g0, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL154
	 nop
.LL153:
	mov	0, %g1
.LL154:
.LL152:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE21:
	.size	Shift__uShift_param_I_I, .-Shift__uShift_param_I_I
	.align 4
	.type	Shift__uShift_var_I_C, #function
	.proc	04
Shift__uShift_var_I_C:
.LLFB22:
	save	%sp, -96, %sp
.LLCFI22:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 0
	bl	%icc, .LL156
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 31
	bg	%icc, .LL157
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	sll	%o5, %g1, %g1
	ba,pt	%xcc, .LL158
	 nop
.LL157:
	mov	0, %g1
.LL158:
	ba,pt	%xcc, .LL159
	 nop
.LL156:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 31
	bg	%icc, .LL160
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	sub	%g0, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL161
	 nop
.LL160:
	mov	0, %g1
.LL161:
.LL159:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE22:
	.size	Shift__uShift_var_I_C, .-Shift__uShift_var_I_C
	.align 4
	.type	Shift__uShift_param_I_C, #function
	.proc	04
Shift__uShift_param_I_C:
.LLFB23:
	save	%sp, -96, %sp
.LLCFI23:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL163
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	bg	%icc, .LL164
	 nop
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	sll	%o5, %g1, %g1
	ba,pt	%xcc, .LL165
	 nop
.LL164:
	mov	0, %g1
.LL165:
	ba,pt	%xcc, .LL166
	 nop
.LL163:
	ld	[%fp+72], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 31
	bg	%icc, .LL167
	 nop
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	sub	%g0, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL168
	 nop
.LL167:
	mov	0, %g1
.LL168:
.LL166:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE23:
	.size	Shift__uShift_param_I_C, .-Shift__uShift_param_I_C
	.align 4
	.type	Shift__uShift_var_i64_I, #function
	.proc	05
Shift__uShift_var_i64_I:
.LLFB24:
	save	%sp, -96, %sp
.LLCFI24:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL170
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 63
	bg	%icc, .LL171
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ldd	[%g1+88], %o4
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL172
	 nop
.LL171:
	mov	0, %o4
	mov	0, %o5
.LL172:
	ba,pt	%xcc, .LL173
	 nop
.LL170:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 63
	bg	%icc, .LL174
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ldd	[%g1+88], %o4
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	sub	%g0, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL175
	 nop
.LL174:
	mov	0, %o4
	mov	0, %o5
.LL175:
.LL173:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE24:
	.size	Shift__uShift_var_i64_I, .-Shift__uShift_var_i64_I
	.align 4
	.type	Shift__uShift_param_i64_I, #function
	.proc	05
Shift__uShift_param_i64_I:
.LLFB25:
	save	%sp, -104, %sp
.LLCFI25:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	cmp	%g1, 0
	bl	%icc, .LL177
	 nop
	ld	[%fp+76], %g1
	cmp	%g1, 63
	bg	%icc, .LL178
	 nop
	ldd	[%fp-8], %o4
	ld	[%fp+76], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL179
	 nop
.LL178:
	mov	0, %o4
	mov	0, %o5
.LL179:
	ba,pt	%xcc, .LL180
	 nop
.LL177:
	ld	[%fp+76], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 63
	bg	%icc, .LL181
	 nop
	ldd	[%fp-8], %o4
	ld	[%fp+76], %g1
	sub	%g0, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL182
	 nop
.LL181:
	mov	0, %o4
	mov	0, %o5
.LL182:
.LL180:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE25:
	.size	Shift__uShift_param_i64_I, .-Shift__uShift_param_i64_I
	.align 4
	.type	Shift__uShift_var_i64_C, #function
	.proc	05
Shift__uShift_var_i64_C:
.LLFB26:
	save	%sp, -96, %sp
.LLCFI26:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 0
	bl	%icc, .LL184
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 63
	bg	%icc, .LL185
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ldd	[%g1+88], %o4
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL186
	 nop
.LL185:
	mov	0, %o4
	mov	0, %o5
.LL186:
	ba,pt	%xcc, .LL187
	 nop
.LL184:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 63
	bg	%icc, .LL188
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ldd	[%g1+88], %o4
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	sub	%g0, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL189
	 nop
.LL188:
	mov	0, %o4
	mov	0, %o5
.LL189:
.LL187:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE26:
	.size	Shift__uShift_var_i64_C, .-Shift__uShift_var_i64_C
	.align 4
	.type	Shift__uShift_param_i64_C, #function
	.proc	05
Shift__uShift_param_i64_C:
.LLFB27:
	save	%sp, -104, %sp
.LLCFI27:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	cmp	%g1, 0
	bl	%icc, .LL191
	 nop
	ld	[%fp+76], %g1
	cmp	%g1, 63
	bg	%icc, .LL192
	 nop
	ldd	[%fp-8], %o4
	ld	[%fp+76], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL193
	 nop
.LL192:
	mov	0, %o4
	mov	0, %o5
.LL193:
	ba,pt	%xcc, .LL194
	 nop
.LL191:
	ld	[%fp+76], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 63
	bg	%icc, .LL195
	 nop
	ldd	[%fp-8], %o4
	ld	[%fp+76], %g1
	sub	%g0, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL196
	 nop
.LL195:
	mov	0, %o4
	mov	0, %o5
.LL196:
.LL194:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE27:
	.size	Shift__uShift_param_i64_C, .-Shift__uShift_param_i64_C
	.align 4
	.type	Shift__uShift_var_C_I, #function
	.proc	04
Shift__uShift_var_C_I:
.LLFB28:
	save	%sp, -96, %sp
.LLCFI28:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL198
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 31
	bg	%icc, .LL199
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	sll	%o5, %g1, %g1
	ba,pt	%xcc, .LL200
	 nop
.LL199:
	mov	0, %g1
.LL200:
	ba,pt	%xcc, .LL201
	 nop
.LL198:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 31
	bg	%icc, .LL202
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	sub	%g0, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL203
	 nop
.LL202:
	mov	0, %g1
.LL203:
.LL201:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE28:
	.size	Shift__uShift_var_C_I, .-Shift__uShift_var_C_I
	.align 4
	.type	Shift__uShift_param_C_I, #function
	.proc	04
Shift__uShift_param_C_I:
.LLFB29:
	save	%sp, -96, %sp
.LLCFI29:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL205
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	bg	%icc, .LL206
	 nop
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	sll	%o5, %g1, %g1
	ba,pt	%xcc, .LL207
	 nop
.LL206:
	mov	0, %g1
.LL207:
	ba,pt	%xcc, .LL208
	 nop
.LL205:
	ld	[%fp+72], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 31
	bg	%icc, .LL209
	 nop
	ld	[%fp+72], %g1
	sub	%g0, %g1, %g1
	ld	[%fp+68], %o5
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL210
	 nop
.LL209:
	mov	0, %g1
.LL210:
.LL208:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE29:
	.size	Shift__uShift_param_C_I, .-Shift__uShift_param_C_I
	.align 4
	.type	Shift__uShift_var_C_C, #function
	.proc	04
Shift__uShift_var_C_C:
.LLFB30:
	save	%sp, -96, %sp
.LLCFI30:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 0
	bl	%icc, .LL212
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 31
	bg	%icc, .LL213
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	sll	%o5, %g1, %g1
	ba,pt	%xcc, .LL214
	 nop
.LL213:
	mov	0, %g1
.LL214:
	ba,pt	%xcc, .LL215
	 nop
.LL212:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 31
	bg	%icc, .LL216
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	sub	%g0, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL217
	 nop
.LL216:
	mov	0, %g1
.LL217:
.LL215:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE30:
	.size	Shift__uShift_var_C_C, .-Shift__uShift_var_C_C
	.align 4
	.type	Shift__uShift_param_C_C, #function
	.proc	04
Shift__uShift_param_C_C:
.LLFB31:
	save	%sp, -96, %sp
.LLCFI31:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL219
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	bg	%icc, .LL220
	 nop
	ld	[%fp+72], %g1
	ld	[%fp+68], %o5
	sll	%o5, %g1, %g1
	ba,pt	%xcc, .LL221
	 nop
.LL220:
	mov	0, %g1
.LL221:
	ba,pt	%xcc, .LL222
	 nop
.LL219:
	ld	[%fp+72], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 31
	bg	%icc, .LL223
	 nop
	ld	[%fp+72], %g1
	sub	%g0, %g1, %g1
	ld	[%fp+68], %o5
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL224
	 nop
.LL223:
	mov	0, %g1
.LL224:
.LL222:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE31:
	.size	Shift__uShift_param_C_C, .-Shift__uShift_param_C_C
	.align 4
	.type	Shift__uShift_var_u8_I, #function
	.proc	04
Shift__uShift_var_u8_I:
.LLFB32:
	save	%sp, -96, %sp
.LLCFI32:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL226
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 31
	bg	%icc, .LL227
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	sll	%o5, %g1, %g1
	ba,pt	%xcc, .LL228
	 nop
.LL227:
	mov	0, %g1
.LL228:
	ba,pt	%xcc, .LL229
	 nop
.LL226:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 31
	bg	%icc, .LL230
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	sub	%g0, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL231
	 nop
.LL230:
	mov	0, %g1
.LL231:
.LL229:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE32:
	.size	Shift__uShift_var_u8_I, .-Shift__uShift_var_u8_I
	.align 4
	.type	Shift__uShift_param_u8_I, #function
	.proc	04
Shift__uShift_param_u8_I:
.LLFB33:
	save	%sp, -96, %sp
.LLCFI33:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL233
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	bg	%icc, .LL234
	 nop
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	sll	%o5, %g1, %g1
	ba,pt	%xcc, .LL235
	 nop
.LL234:
	mov	0, %g1
.LL235:
	ba,pt	%xcc, .LL236
	 nop
.LL233:
	ld	[%fp+72], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 31
	bg	%icc, .LL237
	 nop
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	sub	%g0, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL238
	 nop
.LL237:
	mov	0, %g1
.LL238:
.LL236:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE33:
	.size	Shift__uShift_param_u8_I, .-Shift__uShift_param_u8_I
	.align 4
	.type	Shift__uShift_var_u8_C, #function
	.proc	04
Shift__uShift_var_u8_C:
.LLFB34:
	save	%sp, -96, %sp
.LLCFI34:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 0
	bl	%icc, .LL240
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 31
	bg	%icc, .LL241
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	sll	%o5, %g1, %g1
	ba,pt	%xcc, .LL242
	 nop
.LL241:
	mov	0, %g1
.LL242:
	ba,pt	%xcc, .LL243
	 nop
.LL240:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 31
	bg	%icc, .LL244
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	sub	%g0, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL245
	 nop
.LL244:
	mov	0, %g1
.LL245:
.LL243:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE34:
	.size	Shift__uShift_var_u8_C, .-Shift__uShift_var_u8_C
	.align 4
	.type	Shift__uShift_param_u8_C, #function
	.proc	04
Shift__uShift_param_u8_C:
.LLFB35:
	save	%sp, -96, %sp
.LLCFI35:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL247
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	bg	%icc, .LL248
	 nop
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	sll	%o5, %g1, %g1
	ba,pt	%xcc, .LL249
	 nop
.LL248:
	mov	0, %g1
.LL249:
	ba,pt	%xcc, .LL250
	 nop
.LL247:
	ld	[%fp+72], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 31
	bg	%icc, .LL251
	 nop
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	sub	%g0, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL252
	 nop
.LL251:
	mov	0, %g1
.LL252:
.LL250:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE35:
	.size	Shift__uShift_param_u8_C, .-Shift__uShift_param_u8_C
	.align 4
	.type	Shift__uShift_var_L_I, #function
	.proc	05
Shift__uShift_var_L_I:
.LLFB36:
	save	%sp, -96, %sp
.LLCFI36:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL254
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 63
	bg	%icc, .LL255
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ldd	[%g1+120], %o4
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL256
	 nop
.LL255:
	mov	0, %o4
	mov	0, %o5
.LL256:
	ba,pt	%xcc, .LL257
	 nop
.LL254:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 63
	bg	%icc, .LL258
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ldd	[%g1+120], %o4
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	sub	%g0, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL259
	 nop
.LL258:
	mov	0, %o4
	mov	0, %o5
.LL259:
.LL257:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE36:
	.size	Shift__uShift_var_L_I, .-Shift__uShift_var_L_I
	.align 4
	.type	Shift__uShift_param_L_I, #function
	.proc	05
Shift__uShift_param_L_I:
.LLFB37:
	save	%sp, -104, %sp
.LLCFI37:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	cmp	%g1, 0
	bl	%icc, .LL261
	 nop
	ld	[%fp+76], %g1
	cmp	%g1, 63
	bg	%icc, .LL262
	 nop
	ldd	[%fp-8], %o4
	ld	[%fp+76], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL263
	 nop
.LL262:
	mov	0, %o4
	mov	0, %o5
.LL263:
	ba,pt	%xcc, .LL264
	 nop
.LL261:
	ld	[%fp+76], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 63
	bg	%icc, .LL265
	 nop
	ldd	[%fp-8], %o4
	ld	[%fp+76], %g1
	sub	%g0, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL266
	 nop
.LL265:
	mov	0, %o4
	mov	0, %o5
.LL266:
.LL264:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE37:
	.size	Shift__uShift_param_L_I, .-Shift__uShift_param_L_I
	.align 4
	.type	Shift__uShift_var_L_C, #function
	.proc	05
Shift__uShift_var_L_C:
.LLFB38:
	save	%sp, -96, %sp
.LLCFI38:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 0
	bl	%icc, .LL268
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 63
	bg	%icc, .LL269
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ldd	[%g1+120], %o4
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL270
	 nop
.LL269:
	mov	0, %o4
	mov	0, %o5
.LL270:
	ba,pt	%xcc, .LL271
	 nop
.LL268:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 63
	bg	%icc, .LL272
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ldd	[%g1+120], %o4
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	sub	%g0, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL273
	 nop
.LL272:
	mov	0, %o4
	mov	0, %o5
.LL273:
.LL271:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE38:
	.size	Shift__uShift_var_L_C, .-Shift__uShift_var_L_C
	.align 4
	.type	Shift__uShift_param_L_C, #function
	.proc	05
Shift__uShift_param_L_C:
.LLFB39:
	save	%sp, -104, %sp
.LLCFI39:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	cmp	%g1, 0
	bl	%icc, .LL275
	 nop
	ld	[%fp+76], %g1
	cmp	%g1, 63
	bg	%icc, .LL276
	 nop
	ldd	[%fp-8], %o4
	ld	[%fp+76], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL277
	 nop
.LL276:
	mov	0, %o4
	mov	0, %o5
.LL277:
	ba,pt	%xcc, .LL278
	 nop
.LL275:
	ld	[%fp+76], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 63
	bg	%icc, .LL279
	 nop
	ldd	[%fp-8], %o4
	ld	[%fp+76], %g1
	sub	%g0, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL280
	 nop
.LL279:
	mov	0, %o4
	mov	0, %o5
.LL280:
.LL278:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE39:
	.size	Shift__uShift_param_L_C, .-Shift__uShift_param_L_C
	.align 4
	.type	Shift__uShift_var_i8_I, #function
	.proc	04
Shift__uShift_var_i8_I:
.LLFB40:
	save	%sp, -96, %sp
.LLCFI40:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL282
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 31
	bg	%icc, .LL283
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	sll	%o5, %g1, %g1
	ba,pt	%xcc, .LL284
	 nop
.LL283:
	mov	0, %g1
.LL284:
	ba,pt	%xcc, .LL285
	 nop
.LL282:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 31
	bg	%icc, .LL286
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	sub	%g0, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL287
	 nop
.LL286:
	mov	0, %g1
.LL287:
.LL285:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE40:
	.size	Shift__uShift_var_i8_I, .-Shift__uShift_var_i8_I
	.align 4
	.type	Shift__uShift_param_i8_I, #function
	.proc	04
Shift__uShift_param_i8_I:
.LLFB41:
	save	%sp, -96, %sp
.LLCFI41:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL289
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	bg	%icc, .LL290
	 nop
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	sll	%o5, %g1, %g1
	ba,pt	%xcc, .LL291
	 nop
.LL290:
	mov	0, %g1
.LL291:
	ba,pt	%xcc, .LL292
	 nop
.LL289:
	ld	[%fp+72], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 31
	bg	%icc, .LL293
	 nop
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	sub	%g0, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL294
	 nop
.LL293:
	mov	0, %g1
.LL294:
.LL292:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE41:
	.size	Shift__uShift_param_i8_I, .-Shift__uShift_param_i8_I
	.align 4
	.type	Shift__uShift_var_i8_C, #function
	.proc	04
Shift__uShift_var_i8_C:
.LLFB42:
	save	%sp, -96, %sp
.LLCFI42:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 0
	bl	%icc, .LL296
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 31
	bg	%icc, .LL297
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	sll	%o5, %g1, %g1
	ba,pt	%xcc, .LL298
	 nop
.LL297:
	mov	0, %g1
.LL298:
	ba,pt	%xcc, .LL299
	 nop
.LL296:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 31
	bg	%icc, .LL300
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	sub	%g0, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL301
	 nop
.LL300:
	mov	0, %g1
.LL301:
.LL299:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE42:
	.size	Shift__uShift_var_i8_C, .-Shift__uShift_var_i8_C
	.align 4
	.type	Shift__uShift_param_i8_C, #function
	.proc	04
Shift__uShift_param_i8_C:
.LLFB43:
	save	%sp, -96, %sp
.LLCFI43:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL303
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	bg	%icc, .LL304
	 nop
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	sll	%o5, %g1, %g1
	ba,pt	%xcc, .LL305
	 nop
.LL304:
	mov	0, %g1
.LL305:
	ba,pt	%xcc, .LL306
	 nop
.LL303:
	ld	[%fp+72], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 31
	bg	%icc, .LL307
	 nop
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	sub	%g0, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL308
	 nop
.LL307:
	mov	0, %g1
.LL308:
.LL306:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE43:
	.size	Shift__uShift_param_i8_C, .-Shift__uShift_param_i8_C
	.align 4
	.type	Shift__uShift_var_u32_I, #function
	.proc	04
Shift__uShift_var_u32_I:
.LLFB44:
	save	%sp, -96, %sp
.LLCFI44:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL310
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 31
	bg	%icc, .LL311
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	sll	%o5, %g1, %g1
	ba,pt	%xcc, .LL312
	 nop
.LL311:
	mov	0, %g1
.LL312:
	ba,pt	%xcc, .LL313
	 nop
.LL310:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 31
	bg	%icc, .LL314
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+80], %g1
	sub	%g0, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL315
	 nop
.LL314:
	mov	0, %g1
.LL315:
.LL313:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE44:
	.size	Shift__uShift_var_u32_I, .-Shift__uShift_var_u32_I
	.align 4
	.type	Shift__uShift_param_u32_I, #function
	.proc	04
Shift__uShift_param_u32_I:
.LLFB45:
	save	%sp, -96, %sp
.LLCFI45:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL317
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	bg	%icc, .LL318
	 nop
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	sll	%o5, %g1, %g1
	ba,pt	%xcc, .LL319
	 nop
.LL318:
	mov	0, %g1
.LL319:
	ba,pt	%xcc, .LL320
	 nop
.LL317:
	ld	[%fp+72], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 31
	bg	%icc, .LL321
	 nop
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	sub	%g0, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL322
	 nop
.LL321:
	mov	0, %g1
.LL322:
.LL320:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE45:
	.size	Shift__uShift_param_u32_I, .-Shift__uShift_param_u32_I
	.align 4
	.type	Shift__uShift_var_u32_C, #function
	.proc	04
Shift__uShift_var_u32_C:
.LLFB46:
	save	%sp, -96, %sp
.LLCFI46:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 0
	bl	%icc, .LL324
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 31
	bg	%icc, .LL325
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	sll	%o5, %g1, %g1
	ba,pt	%xcc, .LL326
	 nop
.LL325:
	mov	0, %g1
.LL326:
	ba,pt	%xcc, .LL327
	 nop
.LL324:
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 31
	bg	%icc, .LL328
	 nop
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	ld	[%g1+96], %g1
	sub	%g0, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL329
	 nop
.LL328:
	mov	0, %g1
.LL329:
.LL327:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE46:
	.size	Shift__uShift_var_u32_C, .-Shift__uShift_var_u32_C
	.align 4
	.type	Shift__uShift_param_u32_C, #function
	.proc	04
Shift__uShift_param_u32_C:
.LLFB47:
	save	%sp, -96, %sp
.LLCFI47:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL331
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	bg	%icc, .LL332
	 nop
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	sll	%o5, %g1, %g1
	ba,pt	%xcc, .LL333
	 nop
.LL332:
	mov	0, %g1
.LL333:
	ba,pt	%xcc, .LL334
	 nop
.LL331:
	ld	[%fp+72], %g1
	sub	%g0, %g1, %g1
	cmp	%g1, 31
	bg	%icc, .LL335
	 nop
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	sub	%g0, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL336
	 nop
.LL335:
	mov	0, %g1
.LL336:
.LL334:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE47:
	.size	Shift__uShift_param_u32_C, .-Shift__uShift_param_u32_C
	.align 4
	.global Shift_M3
	.type	Shift_M3, #function
	.proc	0120
Shift_M3:
.LLFB48:
	save	%sp, -96, %sp
.LLCFI48:
	st	%i0, [%fp+68]
	sethi	%hi(MM_Shift), %g1
	or	%g1, %lo(MM_Shift), %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE48:
	.size	Shift_M3, .-Shift_M3
	.section	".data"
	.align 8
	.type	L_1, #object
	.size	L_1, 1256
L_1:
	.ascii	"Shift_M3"
	.skip 1
	.ascii	"uShift_param_u32_C"
	.skip 1
	.ascii	"uShift_var_u32_C"
	.skip 1
	.ascii	"uShift_param_u32_I"
	.skip 1
	.ascii	"uShift_var_u32_I"
	.skip 1
	.ascii	"uShift_param_i8_C"
	.skip 1
	.ascii	"uShift_var_i8_C"
	.skip 1
	.ascii	"uShift_param_i8_I"
	.skip 1
	.ascii	"uShift_var_i8_I"
	.skip 1
	.ascii	"uShift_param_L_C"
	.skip 1
	.ascii	"uShift_var_L_C"
	.skip 1
	.ascii	"uShift_param_L_I"
	.skip 1
	.ascii	"uShift_var_L_I"
	.skip 1
	.ascii	"uShift_param_u8_C"
	.skip 1
	.ascii	"uShift_var_u8_C"
	.skip 1
	.ascii	"uShift_param_u8_I"
	.skip 1
	.ascii	"uShift_var_u8_I"
	.skip 1
	.ascii	"uShift_param_C_C"
	.skip 1
	.ascii	"uShift_var_C_C"
	.skip 1
	.ascii	"uShift_param_C_I"
	.skip 1
	.ascii	"uShift_var_C_I"
	.skip 1
	.ascii	"uShift_param_i64_C"
	.skip 1
	.ascii	"uShift_var_i64_C"
	.skip 1
	.ascii	"uShift_param_i64_I"
	.skip 1
	.ascii	"uShift_var_i64_I"
	.skip 1
	.ascii	"uShift_param_I_C"
	.skip 1
	.ascii	"uShift_var_I_C"
	.skip 1
	.ascii	"uShift_param_I_I"
	.skip 1
	.ascii	"uShift_var_I_I"
	.skip 1
	.ascii	"uShift_param_i16_C"
	.skip 1
	.ascii	"uShift_var_i16_C"
	.skip 1
	.ascii	"uShift_param_i16_I"
	.skip 1
	.ascii	"uShift_var_i16_I"
	.skip 1
	.ascii	"uShift_param_i32_C"
	.skip 1
	.ascii	"uShift_var_i32_C"
	.skip 1
	.ascii	"uShift_param_i32_I"
	.skip 1
	.ascii	"uShift_var_i32_I"
	.skip 1
	.ascii	"uShift_param_LC_C"
	.skip 1
	.ascii	"uShift_var_LC_C"
	.skip 1
	.ascii	"uShift_param_LC_I"
	.skip 1
	.ascii	"uShift_var_LC_I"
	.skip 1
	.ascii	"uShift_param_u64_C"
	.skip 1
	.ascii	"uShift_var_u64_C"
	.skip 1
	.ascii	"uShift_param_u64_I"
	.skip 1
	.ascii	"uShift_var_u64_I"
	.skip 1
	.ascii	"uShift_param_u16_C"
	.skip 1
	.ascii	"uShift_var_u16_C"
	.skip 1
	.ascii	"uShift_param_u16_I"
	.skip 1
	.ascii	"uShift_var_u16_I"
	.skip 4
	.long	Shift_M3
	.long	L_1
	.long	Shift__uShift_param_u32_C
	.long	L_1+9
	.long	Shift__uShift_var_u32_C
	.long	L_1+28
	.long	Shift__uShift_param_u32_I
	.long	L_1+45
	.long	Shift__uShift_var_u32_I
	.long	L_1+64
	.long	Shift__uShift_param_i8_C
	.long	L_1+81
	.long	Shift__uShift_var_i8_C
	.long	L_1+99
	.long	Shift__uShift_param_i8_I
	.long	L_1+115
	.long	Shift__uShift_var_i8_I
	.long	L_1+133
	.long	Shift__uShift_param_L_C
	.long	L_1+149
	.long	Shift__uShift_var_L_C
	.long	L_1+166
	.long	Shift__uShift_param_L_I
	.long	L_1+181
	.long	Shift__uShift_var_L_I
	.long	L_1+198
	.long	Shift__uShift_param_u8_C
	.long	L_1+213
	.long	Shift__uShift_var_u8_C
	.long	L_1+231
	.long	Shift__uShift_param_u8_I
	.long	L_1+247
	.long	Shift__uShift_var_u8_I
	.long	L_1+265
	.long	Shift__uShift_param_C_C
	.long	L_1+281
	.long	Shift__uShift_var_C_C
	.long	L_1+298
	.long	Shift__uShift_param_C_I
	.long	L_1+313
	.long	Shift__uShift_var_C_I
	.long	L_1+330
	.long	Shift__uShift_param_i64_C
	.long	L_1+345
	.long	Shift__uShift_var_i64_C
	.long	L_1+364
	.long	Shift__uShift_param_i64_I
	.long	L_1+381
	.long	Shift__uShift_var_i64_I
	.long	L_1+400
	.long	Shift__uShift_param_I_C
	.long	L_1+417
	.long	Shift__uShift_var_I_C
	.long	L_1+434
	.long	Shift__uShift_param_I_I
	.long	L_1+449
	.long	Shift__uShift_var_I_I
	.long	L_1+466
	.long	Shift__uShift_param_i16_C
	.long	L_1+481
	.long	Shift__uShift_var_i16_C
	.long	L_1+500
	.long	Shift__uShift_param_i16_I
	.long	L_1+517
	.long	Shift__uShift_var_i16_I
	.long	L_1+536
	.long	Shift__uShift_param_i32_C
	.long	L_1+553
	.long	Shift__uShift_var_i32_C
	.long	L_1+572
	.long	Shift__uShift_param_i32_I
	.long	L_1+589
	.long	Shift__uShift_var_i32_I
	.long	L_1+608
	.long	Shift__uShift_param_LC_C
	.long	L_1+625
	.long	Shift__uShift_var_LC_C
	.long	L_1+643
	.long	Shift__uShift_param_LC_I
	.long	L_1+659
	.long	Shift__uShift_var_LC_I
	.long	L_1+677
	.long	Shift__uShift_param_u64_C
	.long	L_1+693
	.long	Shift__uShift_var_u64_C
	.long	L_1+712
	.long	Shift__uShift_param_u64_I
	.long	L_1+729
	.long	Shift__uShift_var_u64_I
	.long	L_1+748
	.long	Shift__uShift_param_u16_C
	.long	L_1+765
	.long	Shift__uShift_var_u16_C
	.long	L_1+784
	.long	Shift__uShift_param_u16_I
	.long	L_1+801
	.long	Shift__uShift_var_u16_I
	.long	L_1+820
	.skip 4
	.ascii	"../SOLsun/Shift.m3"
	.skip 2
	.align 8
	.type	MM_Shift, #object
	.size	MM_Shift, 208
MM_Shift:
	.long	L_1+1236
	.skip 16
	.long	L_1+840
	.skip 12
	.long	MM_Shift+148
	.skip 4
	.long	Shift_M3
	.long	3
	.half	689
	.skip 2
	.long	0
	.long	690
	.long	0
	.long	691
	.long	692
	.half	693
	.skip 2
	.long	694
	.skip 4
	.long	0
	.long	695
	.long	696
	.skip 4
	.long	1082510741
	.long	-2130303779
	.byte	-69
	.skip 7
	.long	0
	.long	700
	.byte	-67
	.skip 3
	.long	702
	.long	1143991566
	.skip 12
	.long	Shift_I3
	.long	MM_Shift+160
	.skip 4
	.long	Long_I3
	.long	MM_Shift+172
	.skip 4
	.long	Word_I3
	.long	MM_Shift+184
	.skip 4
	.long	Cstdint_I3
	.long	MM_Shift+196
	.skip 4
	.long	RTHooks_I3
	.skip 4
	.ident	"GCC: (GNU) 4.5.1"
