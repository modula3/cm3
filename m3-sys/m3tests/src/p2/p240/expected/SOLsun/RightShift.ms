	.file	"RightShift.mc"
	.section	".text"
	.align 4
	.type	RightShift__uRightShift_var_u16_I, #function
	.proc	04
RightShift__uRightShift_var_u16_I:
.LLFB0:
	save	%sp, -96, %sp
.LLCFI0:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL2
	 nop
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 31
	ble	%icc, .LL3
	 nop
.LL2:
	mov	1729, %o0
	call	_m3_fault, 0
	 nop
.LL3:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 31
	bg	%icc, .LL4
	 nop
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL5
	 nop
.LL4:
	mov	0, %g1
.LL5:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE0:
	.size	RightShift__uRightShift_var_u16_I, .-RightShift__uRightShift_var_u16_I
	.align 4
	.type	RightShift__uRightShift_param_u16_I, #function
	.proc	04
RightShift__uRightShift_param_u16_I:
.LLFB1:
	save	%sp, -96, %sp
.LLCFI1:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL7
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL8
	 nop
.LL7:
	mov	1761, %o0
	call	_m3_fault, 0
	 nop
.LL8:
	ld	[%fp+72], %g1
	cmp	%g1, 31
	bg	%icc, .LL9
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL10
	 nop
.LL9:
	mov	0, %g1
.LL10:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1:
	.size	RightShift__uRightShift_param_u16_I, .-RightShift__uRightShift_param_u16_I
	.align 4
	.type	RightShift__uRightShift_var_u16_C, #function
	.proc	04
RightShift__uRightShift_var_u16_C:
.LLFB2:
	save	%sp, -96, %sp
.LLCFI2:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 31
	ble	%icc, .LL12
	 nop
	mov	1793, %o0
	call	_m3_fault, 0
	 nop
.LL12:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 31
	bg	%icc, .LL13
	 nop
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+96], %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL14
	 nop
.LL13:
	mov	0, %g1
.LL14:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE2:
	.size	RightShift__uRightShift_var_u16_C, .-RightShift__uRightShift_var_u16_C
	.align 4
	.type	RightShift__uRightShift_param_u16_C, #function
	.proc	04
RightShift__uRightShift_param_u16_C:
.LLFB3:
	save	%sp, -96, %sp
.LLCFI3:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL16
	 nop
	mov	1825, %o0
	call	_m3_fault, 0
	 nop
.LL16:
	ld	[%fp+72], %g1
	cmp	%g1, 31
	bg	%icc, .LL17
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL18
	 nop
.LL17:
	mov	0, %g1
.LL18:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE3:
	.size	RightShift__uRightShift_param_u16_C, .-RightShift__uRightShift_param_u16_C
	.align 4
	.type	RightShift__uRightShift_var_u64_I, #function
	.proc	05
RightShift__uRightShift_var_u64_I:
.LLFB4:
	save	%sp, -96, %sp
.LLCFI4:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL20
	 nop
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 63
	ble	%icc, .LL21
	 nop
.LL20:
	mov	1857, %o0
	call	_m3_fault, 0
	 nop
.LL21:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 63
	bg	%icc, .LL22
	 nop
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ldd	[%g1+56], %o4
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL23
	 nop
.LL22:
	mov	0, %o4
	mov	0, %o5
.LL23:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE4:
	.size	RightShift__uRightShift_var_u64_I, .-RightShift__uRightShift_var_u64_I
	.align 4
	.type	RightShift__uRightShift_param_u64_I, #function
	.proc	05
RightShift__uRightShift_param_u64_I:
.LLFB5:
	save	%sp, -104, %sp
.LLCFI5:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	cmp	%g1, 0
	bl	%icc, .LL25
	 nop
	ld	[%fp+76], %g1
	cmp	%g1, 63
	ble	%icc, .LL26
	 nop
.LL25:
	mov	1889, %o0
	call	_m3_fault, 0
	 nop
.LL26:
	ld	[%fp+76], %g1
	cmp	%g1, 63
	bg	%icc, .LL27
	 nop
	ldd	[%fp-8], %o4
	ld	[%fp+76], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL28
	 nop
.LL27:
	mov	0, %o4
	mov	0, %o5
.LL28:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE5:
	.size	RightShift__uRightShift_param_u64_I, .-RightShift__uRightShift_param_u64_I
	.align 4
	.type	RightShift__uRightShift_var_u64_C, #function
	.proc	05
RightShift__uRightShift_var_u64_C:
.LLFB6:
	save	%sp, -96, %sp
.LLCFI6:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 63
	ble	%icc, .LL30
	 nop
	mov	1921, %o0
	call	_m3_fault, 0
	 nop
.LL30:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 63
	bg	%icc, .LL31
	 nop
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ldd	[%g1+56], %o4
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+96], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL32
	 nop
.LL31:
	mov	0, %o4
	mov	0, %o5
.LL32:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE6:
	.size	RightShift__uRightShift_var_u64_C, .-RightShift__uRightShift_var_u64_C
	.align 4
	.type	RightShift__uRightShift_param_u64_C, #function
	.proc	05
RightShift__uRightShift_param_u64_C:
.LLFB7:
	save	%sp, -104, %sp
.LLCFI7:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	cmp	%g1, 63
	ble	%icc, .LL34
	 nop
	mov	1953, %o0
	call	_m3_fault, 0
	 nop
.LL34:
	ld	[%fp+76], %g1
	cmp	%g1, 63
	bg	%icc, .LL35
	 nop
	ldd	[%fp-8], %o4
	ld	[%fp+76], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL36
	 nop
.LL35:
	mov	0, %o4
	mov	0, %o5
.LL36:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE7:
	.size	RightShift__uRightShift_param_u64_C, .-RightShift__uRightShift_param_u64_C
	.align 4
	.type	RightShift__uRightShift_var_LC_I, #function
	.proc	05
RightShift__uRightShift_var_LC_I:
.LLFB8:
	save	%sp, -96, %sp
.LLCFI8:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL38
	 nop
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 63
	ble	%icc, .LL39
	 nop
.LL38:
	mov	1985, %o0
	call	_m3_fault, 0
	 nop
.LL39:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 63
	bg	%icc, .LL40
	 nop
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ldd	[%g1+64], %o4
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL41
	 nop
.LL40:
	mov	0, %o4
	mov	0, %o5
.LL41:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE8:
	.size	RightShift__uRightShift_var_LC_I, .-RightShift__uRightShift_var_LC_I
	.align 4
	.type	RightShift__uRightShift_param_LC_I, #function
	.proc	05
RightShift__uRightShift_param_LC_I:
.LLFB9:
	save	%sp, -104, %sp
.LLCFI9:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	cmp	%g1, 0
	bl	%icc, .LL43
	 nop
	ld	[%fp+76], %g1
	cmp	%g1, 63
	ble	%icc, .LL44
	 nop
.LL43:
	mov	2017, %o0
	call	_m3_fault, 0
	 nop
.LL44:
	ld	[%fp+76], %g1
	cmp	%g1, 63
	bg	%icc, .LL45
	 nop
	ldd	[%fp-8], %o4
	ld	[%fp+76], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL46
	 nop
.LL45:
	mov	0, %o4
	mov	0, %o5
.LL46:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE9:
	.size	RightShift__uRightShift_param_LC_I, .-RightShift__uRightShift_param_LC_I
	.align 4
	.type	RightShift__uRightShift_var_LC_C, #function
	.proc	05
RightShift__uRightShift_var_LC_C:
.LLFB10:
	save	%sp, -96, %sp
.LLCFI10:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 63
	ble	%icc, .LL48
	 nop
	mov	2049, %o0
	call	_m3_fault, 0
	 nop
.LL48:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 63
	bg	%icc, .LL49
	 nop
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ldd	[%g1+64], %o4
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+96], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL50
	 nop
.LL49:
	mov	0, %o4
	mov	0, %o5
.LL50:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE10:
	.size	RightShift__uRightShift_var_LC_C, .-RightShift__uRightShift_var_LC_C
	.align 4
	.type	RightShift__uRightShift_param_LC_C, #function
	.proc	05
RightShift__uRightShift_param_LC_C:
.LLFB11:
	save	%sp, -104, %sp
.LLCFI11:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	cmp	%g1, 63
	ble	%icc, .LL52
	 nop
	mov	2081, %o0
	call	_m3_fault, 0
	 nop
.LL52:
	ld	[%fp+76], %g1
	cmp	%g1, 63
	bg	%icc, .LL53
	 nop
	ld	[%fp+76], %g1
	ldd	[%fp-8], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL54
	 nop
.LL53:
	mov	0, %o4
	mov	0, %o5
.LL54:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE11:
	.size	RightShift__uRightShift_param_LC_C, .-RightShift__uRightShift_param_LC_C
	.align 4
	.type	RightShift__uRightShift_var_i32_I, #function
	.proc	04
RightShift__uRightShift_var_i32_I:
.LLFB12:
	save	%sp, -96, %sp
.LLCFI12:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL56
	 nop
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 31
	ble	%icc, .LL57
	 nop
.LL56:
	mov	2113, %o0
	call	_m3_fault, 0
	 nop
.LL57:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 31
	bg	%icc, .LL58
	 nop
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL59
	 nop
.LL58:
	mov	0, %g1
.LL59:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE12:
	.size	RightShift__uRightShift_var_i32_I, .-RightShift__uRightShift_var_i32_I
	.align 4
	.type	RightShift__uRightShift_param_i32_I, #function
	.proc	04
RightShift__uRightShift_param_i32_I:
.LLFB13:
	save	%sp, -96, %sp
.LLCFI13:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL61
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL62
	 nop
.LL61:
	mov	2145, %o0
	call	_m3_fault, 0
	 nop
.LL62:
	ld	[%fp+72], %g1
	cmp	%g1, 31
	bg	%icc, .LL63
	 nop
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL64
	 nop
.LL63:
	mov	0, %g1
.LL64:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE13:
	.size	RightShift__uRightShift_param_i32_I, .-RightShift__uRightShift_param_i32_I
	.align 4
	.type	RightShift__uRightShift_var_i32_C, #function
	.proc	04
RightShift__uRightShift_var_i32_C:
.LLFB14:
	save	%sp, -96, %sp
.LLCFI14:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 31
	ble	%icc, .LL66
	 nop
	mov	2177, %o0
	call	_m3_fault, 0
	 nop
.LL66:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 31
	bg	%icc, .LL67
	 nop
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+96], %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL68
	 nop
.LL67:
	mov	0, %g1
.LL68:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE14:
	.size	RightShift__uRightShift_var_i32_C, .-RightShift__uRightShift_var_i32_C
	.align 4
	.type	RightShift__uRightShift_param_i32_C, #function
	.proc	04
RightShift__uRightShift_param_i32_C:
.LLFB15:
	save	%sp, -96, %sp
.LLCFI15:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL70
	 nop
	mov	2209, %o0
	call	_m3_fault, 0
	 nop
.LL70:
	ld	[%fp+72], %g1
	cmp	%g1, 31
	bg	%icc, .LL71
	 nop
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL72
	 nop
.LL71:
	mov	0, %g1
.LL72:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE15:
	.size	RightShift__uRightShift_param_i32_C, .-RightShift__uRightShift_param_i32_C
	.align 4
	.type	RightShift__uRightShift_var_i16_I, #function
	.proc	04
RightShift__uRightShift_var_i16_I:
.LLFB16:
	save	%sp, -96, %sp
.LLCFI16:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL74
	 nop
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 31
	ble	%icc, .LL75
	 nop
.LL74:
	mov	2241, %o0
	call	_m3_fault, 0
	 nop
.LL75:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 31
	bg	%icc, .LL76
	 nop
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL77
	 nop
.LL76:
	mov	0, %g1
.LL77:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE16:
	.size	RightShift__uRightShift_var_i16_I, .-RightShift__uRightShift_var_i16_I
	.align 4
	.type	RightShift__uRightShift_param_i16_I, #function
	.proc	04
RightShift__uRightShift_param_i16_I:
.LLFB17:
	save	%sp, -96, %sp
.LLCFI17:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL79
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL80
	 nop
.LL79:
	mov	2273, %o0
	call	_m3_fault, 0
	 nop
.LL80:
	ld	[%fp+72], %g1
	cmp	%g1, 31
	bg	%icc, .LL81
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL82
	 nop
.LL81:
	mov	0, %g1
.LL82:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE17:
	.size	RightShift__uRightShift_param_i16_I, .-RightShift__uRightShift_param_i16_I
	.align 4
	.type	RightShift__uRightShift_var_i16_C, #function
	.proc	04
RightShift__uRightShift_var_i16_C:
.LLFB18:
	save	%sp, -96, %sp
.LLCFI18:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 31
	ble	%icc, .LL84
	 nop
	mov	2305, %o0
	call	_m3_fault, 0
	 nop
.LL84:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 31
	bg	%icc, .LL85
	 nop
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+96], %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL86
	 nop
.LL85:
	mov	0, %g1
.LL86:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE18:
	.size	RightShift__uRightShift_var_i16_C, .-RightShift__uRightShift_var_i16_C
	.align 4
	.type	RightShift__uRightShift_param_i16_C, #function
	.proc	04
RightShift__uRightShift_param_i16_C:
.LLFB19:
	save	%sp, -96, %sp
.LLCFI19:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL88
	 nop
	mov	2337, %o0
	call	_m3_fault, 0
	 nop
.LL88:
	ld	[%fp+72], %g1
	cmp	%g1, 31
	bg	%icc, .LL89
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL90
	 nop
.LL89:
	mov	0, %g1
.LL90:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE19:
	.size	RightShift__uRightShift_param_i16_C, .-RightShift__uRightShift_param_i16_C
	.align 4
	.type	RightShift__uRightShift_var_I_I, #function
	.proc	04
RightShift__uRightShift_var_I_I:
.LLFB20:
	save	%sp, -96, %sp
.LLCFI20:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL92
	 nop
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 31
	ble	%icc, .LL93
	 nop
.LL92:
	mov	2369, %o0
	call	_m3_fault, 0
	 nop
.LL93:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 31
	bg	%icc, .LL94
	 nop
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL95
	 nop
.LL94:
	mov	0, %g1
.LL95:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE20:
	.size	RightShift__uRightShift_var_I_I, .-RightShift__uRightShift_var_I_I
	.align 4
	.type	RightShift__uRightShift_param_I_I, #function
	.proc	04
RightShift__uRightShift_param_I_I:
.LLFB21:
	save	%sp, -96, %sp
.LLCFI21:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL97
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL98
	 nop
.LL97:
	mov	2401, %o0
	call	_m3_fault, 0
	 nop
.LL98:
	ld	[%fp+72], %g1
	cmp	%g1, 31
	bg	%icc, .LL99
	 nop
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL100
	 nop
.LL99:
	mov	0, %g1
.LL100:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE21:
	.size	RightShift__uRightShift_param_I_I, .-RightShift__uRightShift_param_I_I
	.align 4
	.type	RightShift__uRightShift_var_I_C, #function
	.proc	04
RightShift__uRightShift_var_I_C:
.LLFB22:
	save	%sp, -96, %sp
.LLCFI22:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 31
	ble	%icc, .LL102
	 nop
	mov	2433, %o0
	call	_m3_fault, 0
	 nop
.LL102:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 31
	bg	%icc, .LL103
	 nop
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+96], %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL104
	 nop
.LL103:
	mov	0, %g1
.LL104:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE22:
	.size	RightShift__uRightShift_var_I_C, .-RightShift__uRightShift_var_I_C
	.align 4
	.type	RightShift__uRightShift_param_I_C, #function
	.proc	04
RightShift__uRightShift_param_I_C:
.LLFB23:
	save	%sp, -96, %sp
.LLCFI23:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL106
	 nop
	mov	2465, %o0
	call	_m3_fault, 0
	 nop
.LL106:
	ld	[%fp+72], %g1
	cmp	%g1, 31
	bg	%icc, .LL107
	 nop
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL108
	 nop
.LL107:
	mov	0, %g1
.LL108:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE23:
	.size	RightShift__uRightShift_param_I_C, .-RightShift__uRightShift_param_I_C
	.align 4
	.type	RightShift__uRightShift_var_i64_I, #function
	.proc	05
RightShift__uRightShift_var_i64_I:
.LLFB24:
	save	%sp, -96, %sp
.LLCFI24:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL110
	 nop
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 63
	ble	%icc, .LL111
	 nop
.LL110:
	mov	2497, %o0
	call	_m3_fault, 0
	 nop
.LL111:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 63
	bg	%icc, .LL112
	 nop
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ldd	[%g1+88], %o4
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL113
	 nop
.LL112:
	mov	0, %o4
	mov	0, %o5
.LL113:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE24:
	.size	RightShift__uRightShift_var_i64_I, .-RightShift__uRightShift_var_i64_I
	.align 4
	.type	RightShift__uRightShift_param_i64_I, #function
	.proc	05
RightShift__uRightShift_param_i64_I:
.LLFB25:
	save	%sp, -104, %sp
.LLCFI25:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	cmp	%g1, 0
	bl	%icc, .LL115
	 nop
	ld	[%fp+76], %g1
	cmp	%g1, 63
	ble	%icc, .LL116
	 nop
.LL115:
	mov	2529, %o0
	call	_m3_fault, 0
	 nop
.LL116:
	ld	[%fp+76], %g1
	cmp	%g1, 63
	bg	%icc, .LL117
	 nop
	ldd	[%fp-8], %o4
	ld	[%fp+76], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL118
	 nop
.LL117:
	mov	0, %o4
	mov	0, %o5
.LL118:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE25:
	.size	RightShift__uRightShift_param_i64_I, .-RightShift__uRightShift_param_i64_I
	.align 4
	.type	RightShift__uRightShift_var_i64_C, #function
	.proc	05
RightShift__uRightShift_var_i64_C:
.LLFB26:
	save	%sp, -96, %sp
.LLCFI26:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 63
	ble	%icc, .LL120
	 nop
	mov	2561, %o0
	call	_m3_fault, 0
	 nop
.LL120:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 63
	bg	%icc, .LL121
	 nop
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ldd	[%g1+88], %o4
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+96], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL122
	 nop
.LL121:
	mov	0, %o4
	mov	0, %o5
.LL122:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE26:
	.size	RightShift__uRightShift_var_i64_C, .-RightShift__uRightShift_var_i64_C
	.align 4
	.type	RightShift__uRightShift_param_i64_C, #function
	.proc	05
RightShift__uRightShift_param_i64_C:
.LLFB27:
	save	%sp, -104, %sp
.LLCFI27:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	cmp	%g1, 63
	ble	%icc, .LL124
	 nop
	mov	2593, %o0
	call	_m3_fault, 0
	 nop
.LL124:
	ld	[%fp+76], %g1
	cmp	%g1, 63
	bg	%icc, .LL125
	 nop
	ldd	[%fp-8], %o4
	ld	[%fp+76], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL126
	 nop
.LL125:
	mov	0, %o4
	mov	0, %o5
.LL126:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE27:
	.size	RightShift__uRightShift_param_i64_C, .-RightShift__uRightShift_param_i64_C
	.align 4
	.type	RightShift__uRightShift_var_C_I, #function
	.proc	04
RightShift__uRightShift_var_C_I:
.LLFB28:
	save	%sp, -96, %sp
.LLCFI28:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL128
	 nop
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 31
	ble	%icc, .LL129
	 nop
.LL128:
	mov	2625, %o0
	call	_m3_fault, 0
	 nop
.LL129:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 31
	bg	%icc, .LL130
	 nop
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL131
	 nop
.LL130:
	mov	0, %g1
.LL131:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE28:
	.size	RightShift__uRightShift_var_C_I, .-RightShift__uRightShift_var_C_I
	.align 4
	.type	RightShift__uRightShift_param_C_I, #function
	.proc	04
RightShift__uRightShift_param_C_I:
.LLFB29:
	save	%sp, -96, %sp
.LLCFI29:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL133
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL134
	 nop
.LL133:
	mov	2657, %o0
	call	_m3_fault, 0
	 nop
.LL134:
	ld	[%fp+72], %g1
	cmp	%g1, 31
	bg	%icc, .LL135
	 nop
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL136
	 nop
.LL135:
	mov	0, %g1
.LL136:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE29:
	.size	RightShift__uRightShift_param_C_I, .-RightShift__uRightShift_param_C_I
	.align 4
	.type	RightShift__uRightShift_var_C_C, #function
	.proc	04
RightShift__uRightShift_var_C_C:
.LLFB30:
	save	%sp, -96, %sp
.LLCFI30:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 31
	ble	%icc, .LL138
	 nop
	mov	2689, %o0
	call	_m3_fault, 0
	 nop
.LL138:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 31
	bg	%icc, .LL139
	 nop
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+96], %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL140
	 nop
.LL139:
	mov	0, %g1
.LL140:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE30:
	.size	RightShift__uRightShift_var_C_C, .-RightShift__uRightShift_var_C_C
	.align 4
	.type	RightShift__uRightShift_param_C_C, #function
	.proc	04
RightShift__uRightShift_param_C_C:
.LLFB31:
	save	%sp, -96, %sp
.LLCFI31:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL142
	 nop
	mov	2721, %o0
	call	_m3_fault, 0
	 nop
.LL142:
	ld	[%fp+72], %g1
	cmp	%g1, 31
	bg	%icc, .LL143
	 nop
	ld	[%fp+72], %g1
	ld	[%fp+68], %o5
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL144
	 nop
.LL143:
	mov	0, %g1
.LL144:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE31:
	.size	RightShift__uRightShift_param_C_C, .-RightShift__uRightShift_param_C_C
	.align 4
	.type	RightShift__uRightShift_var_u8_I, #function
	.proc	04
RightShift__uRightShift_var_u8_I:
.LLFB32:
	save	%sp, -96, %sp
.LLCFI32:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL146
	 nop
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 31
	ble	%icc, .LL147
	 nop
.LL146:
	mov	2753, %o0
	call	_m3_fault, 0
	 nop
.LL147:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 31
	bg	%icc, .LL148
	 nop
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL149
	 nop
.LL148:
	mov	0, %g1
.LL149:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE32:
	.size	RightShift__uRightShift_var_u8_I, .-RightShift__uRightShift_var_u8_I
	.align 4
	.type	RightShift__uRightShift_param_u8_I, #function
	.proc	04
RightShift__uRightShift_param_u8_I:
.LLFB33:
	save	%sp, -96, %sp
.LLCFI33:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL151
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL152
	 nop
.LL151:
	mov	2785, %o0
	call	_m3_fault, 0
	 nop
.LL152:
	ld	[%fp+72], %g1
	cmp	%g1, 31
	bg	%icc, .LL153
	 nop
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL154
	 nop
.LL153:
	mov	0, %g1
.LL154:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE33:
	.size	RightShift__uRightShift_param_u8_I, .-RightShift__uRightShift_param_u8_I
	.align 4
	.type	RightShift__uRightShift_var_u8_C, #function
	.proc	04
RightShift__uRightShift_var_u8_C:
.LLFB34:
	save	%sp, -96, %sp
.LLCFI34:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 31
	ble	%icc, .LL156
	 nop
	mov	2817, %o0
	call	_m3_fault, 0
	 nop
.LL156:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 31
	bg	%icc, .LL157
	 nop
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+96], %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL158
	 nop
.LL157:
	mov	0, %g1
.LL158:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE34:
	.size	RightShift__uRightShift_var_u8_C, .-RightShift__uRightShift_var_u8_C
	.align 4
	.type	RightShift__uRightShift_param_u8_C, #function
	.proc	04
RightShift__uRightShift_param_u8_C:
.LLFB35:
	save	%sp, -96, %sp
.LLCFI35:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL160
	 nop
	mov	2849, %o0
	call	_m3_fault, 0
	 nop
.LL160:
	ld	[%fp+72], %g1
	cmp	%g1, 31
	bg	%icc, .LL161
	 nop
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL162
	 nop
.LL161:
	mov	0, %g1
.LL162:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE35:
	.size	RightShift__uRightShift_param_u8_C, .-RightShift__uRightShift_param_u8_C
	.align 4
	.type	RightShift__uRightShift_var_L_I, #function
	.proc	05
RightShift__uRightShift_var_L_I:
.LLFB36:
	save	%sp, -96, %sp
.LLCFI36:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL164
	 nop
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 63
	ble	%icc, .LL165
	 nop
.LL164:
	mov	2881, %o0
	call	_m3_fault, 0
	 nop
.LL165:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 63
	bg	%icc, .LL166
	 nop
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ldd	[%g1+120], %o4
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL167
	 nop
.LL166:
	mov	0, %o4
	mov	0, %o5
.LL167:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE36:
	.size	RightShift__uRightShift_var_L_I, .-RightShift__uRightShift_var_L_I
	.align 4
	.type	RightShift__uRightShift_param_L_I, #function
	.proc	05
RightShift__uRightShift_param_L_I:
.LLFB37:
	save	%sp, -104, %sp
.LLCFI37:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	cmp	%g1, 0
	bl	%icc, .LL169
	 nop
	ld	[%fp+76], %g1
	cmp	%g1, 63
	ble	%icc, .LL170
	 nop
.LL169:
	mov	2913, %o0
	call	_m3_fault, 0
	 nop
.LL170:
	ld	[%fp+76], %g1
	cmp	%g1, 63
	bg	%icc, .LL171
	 nop
	ldd	[%fp-8], %o4
	ld	[%fp+76], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL172
	 nop
.LL171:
	mov	0, %o4
	mov	0, %o5
.LL172:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE37:
	.size	RightShift__uRightShift_param_L_I, .-RightShift__uRightShift_param_L_I
	.align 4
	.type	RightShift__uRightShift_var_L_C, #function
	.proc	05
RightShift__uRightShift_var_L_C:
.LLFB38:
	save	%sp, -96, %sp
.LLCFI38:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 63
	ble	%icc, .LL174
	 nop
	mov	2945, %o0
	call	_m3_fault, 0
	 nop
.LL174:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 63
	bg	%icc, .LL175
	 nop
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ldd	[%g1+120], %o4
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+96], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL176
	 nop
.LL175:
	mov	0, %o4
	mov	0, %o5
.LL176:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE38:
	.size	RightShift__uRightShift_var_L_C, .-RightShift__uRightShift_var_L_C
	.align 4
	.type	RightShift__uRightShift_param_L_C, #function
	.proc	05
RightShift__uRightShift_param_L_C:
.LLFB39:
	save	%sp, -104, %sp
.LLCFI39:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	cmp	%g1, 63
	ble	%icc, .LL178
	 nop
	mov	2977, %o0
	call	_m3_fault, 0
	 nop
.LL178:
	ld	[%fp+76], %g1
	cmp	%g1, 63
	bg	%icc, .LL179
	 nop
	ldd	[%fp-8], %o4
	ld	[%fp+76], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL180
	 nop
.LL179:
	mov	0, %o4
	mov	0, %o5
.LL180:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE39:
	.size	RightShift__uRightShift_param_L_C, .-RightShift__uRightShift_param_L_C
	.align 4
	.type	RightShift__uRightShift_var_i8_I, #function
	.proc	04
RightShift__uRightShift_var_i8_I:
.LLFB40:
	save	%sp, -96, %sp
.LLCFI40:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL182
	 nop
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 31
	ble	%icc, .LL183
	 nop
.LL182:
	mov	3009, %o0
	call	_m3_fault, 0
	 nop
.LL183:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 31
	bg	%icc, .LL184
	 nop
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL185
	 nop
.LL184:
	mov	0, %g1
.LL185:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE40:
	.size	RightShift__uRightShift_var_i8_I, .-RightShift__uRightShift_var_i8_I
	.align 4
	.type	RightShift__uRightShift_param_i8_I, #function
	.proc	04
RightShift__uRightShift_param_i8_I:
.LLFB41:
	save	%sp, -96, %sp
.LLCFI41:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL187
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL188
	 nop
.LL187:
	mov	3041, %o0
	call	_m3_fault, 0
	 nop
.LL188:
	ld	[%fp+72], %g1
	cmp	%g1, 31
	bg	%icc, .LL189
	 nop
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL190
	 nop
.LL189:
	mov	0, %g1
.LL190:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE41:
	.size	RightShift__uRightShift_param_i8_I, .-RightShift__uRightShift_param_i8_I
	.align 4
	.type	RightShift__uRightShift_var_i8_C, #function
	.proc	04
RightShift__uRightShift_var_i8_C:
.LLFB42:
	save	%sp, -96, %sp
.LLCFI42:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 31
	ble	%icc, .LL192
	 nop
	mov	3073, %o0
	call	_m3_fault, 0
	 nop
.LL192:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 31
	bg	%icc, .LL193
	 nop
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+96], %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL194
	 nop
.LL193:
	mov	0, %g1
.LL194:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE42:
	.size	RightShift__uRightShift_var_i8_C, .-RightShift__uRightShift_var_i8_C
	.align 4
	.type	RightShift__uRightShift_param_i8_C, #function
	.proc	04
RightShift__uRightShift_param_i8_C:
.LLFB43:
	save	%sp, -96, %sp
.LLCFI43:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL196
	 nop
	mov	3105, %o0
	call	_m3_fault, 0
	 nop
.LL196:
	ld	[%fp+72], %g1
	cmp	%g1, 31
	bg	%icc, .LL197
	 nop
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL198
	 nop
.LL197:
	mov	0, %g1
.LL198:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE43:
	.size	RightShift__uRightShift_param_i8_C, .-RightShift__uRightShift_param_i8_C
	.align 4
	.type	RightShift__uRightShift_var_u32_I, #function
	.proc	04
RightShift__uRightShift_var_u32_I:
.LLFB44:
	save	%sp, -96, %sp
.LLCFI44:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL200
	 nop
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 31
	ble	%icc, .LL201
	 nop
.LL200:
	mov	3137, %o0
	call	_m3_fault, 0
	 nop
.LL201:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 31
	bg	%icc, .LL202
	 nop
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+80], %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL203
	 nop
.LL202:
	mov	0, %g1
.LL203:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE44:
	.size	RightShift__uRightShift_var_u32_I, .-RightShift__uRightShift_var_u32_I
	.align 4
	.type	RightShift__uRightShift_param_u32_I, #function
	.proc	04
RightShift__uRightShift_param_u32_I:
.LLFB45:
	save	%sp, -96, %sp
.LLCFI45:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL205
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL206
	 nop
.LL205:
	mov	3169, %o0
	call	_m3_fault, 0
	 nop
.LL206:
	ld	[%fp+72], %g1
	cmp	%g1, 31
	bg	%icc, .LL207
	 nop
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL208
	 nop
.LL207:
	mov	0, %g1
.LL208:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE45:
	.size	RightShift__uRightShift_param_u32_I, .-RightShift__uRightShift_param_u32_I
	.align 4
	.type	RightShift__uRightShift_var_u32_C, #function
	.proc	04
RightShift__uRightShift_var_u32_C:
.LLFB46:
	save	%sp, -96, %sp
.LLCFI46:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 31
	ble	%icc, .LL210
	 nop
	mov	3201, %o0
	call	_m3_fault, 0
	 nop
.LL210:
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 31
	bg	%icc, .LL211
	 nop
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	ld	[%g1+96], %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL212
	 nop
.LL211:
	mov	0, %g1
.LL212:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE46:
	.size	RightShift__uRightShift_var_u32_C, .-RightShift__uRightShift_var_u32_C
	.align 4
	.type	RightShift__uRightShift_param_u32_C, #function
	.proc	04
RightShift__uRightShift_param_u32_C:
.LLFB47:
	save	%sp, -96, %sp
.LLCFI47:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL214
	 nop
	mov	3233, %o0
	call	_m3_fault, 0
	 nop
.LL214:
	ld	[%fp+72], %g1
	cmp	%g1, 31
	bg	%icc, .LL215
	 nop
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL216
	 nop
.LL215:
	mov	0, %g1
.LL216:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE47:
	.size	RightShift__uRightShift_param_u32_C, .-RightShift__uRightShift_param_u32_C
	.align 4
	.global RightShift_M3
	.type	RightShift_M3, #function
	.proc	0120
RightShift_M3:
.LLFB48:
	save	%sp, -96, %sp
.LLCFI48:
	st	%i0, [%fp+68]
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE48:
	.size	RightShift_M3, .-RightShift_M3
	.align 4
	.type	_m3_fault, #function
	.proc	020
_m3_fault:
.LLFB49:
	save	%sp, -96, %sp
.LLCFI49:
	st	%i0, [%fp+68]
	sethi	%hi(MM_RightShift), %g1
	or	%g1, %lo(MM_RightShift), %o0
	ld	[%fp+68], %o1
	call	RTHooks__ReportFault, 0
	 nop
	return	%i7+8
	 nop
.LLFE49:
	.size	_m3_fault, .-_m3_fault
	.section	".data"
	.align 8
	.type	L_1, #object
	.size	L_1, 1504
L_1:
	.ascii	"RightShift_M3"
	.skip 1
	.ascii	"uRightShift_param_u32_C"
	.skip 1
	.ascii	"uRightShift_var_u32_C"
	.skip 1
	.ascii	"uRightShift_param_u32_I"
	.skip 1
	.ascii	"uRightShift_var_u32_I"
	.skip 1
	.ascii	"uRightShift_param_i8_C"
	.skip 1
	.ascii	"uRightShift_var_i8_C"
	.skip 1
	.ascii	"uRightShift_param_i8_I"
	.skip 1
	.ascii	"uRightShift_var_i8_I"
	.skip 1
	.ascii	"uRightShift_param_L_C"
	.skip 1
	.ascii	"uRightShift_var_L_C"
	.skip 1
	.ascii	"uRightShift_param_L_I"
	.skip 1
	.ascii	"uRightShift_var_L_I"
	.skip 1
	.ascii	"uRightShift_param_u8_C"
	.skip 1
	.ascii	"uRightShift_var_u8_C"
	.skip 1
	.ascii	"uRightShift_param_u8_I"
	.skip 1
	.ascii	"uRightShift_var_u8_I"
	.skip 1
	.ascii	"uRightShift_param_C_C"
	.skip 1
	.ascii	"uRightShift_var_C_C"
	.skip 1
	.ascii	"uRightShift_param_C_I"
	.skip 1
	.ascii	"uRightShift_var_C_I"
	.skip 1
	.ascii	"uRightShift_param_i64_C"
	.skip 1
	.ascii	"uRightShift_var_i64_C"
	.skip 1
	.ascii	"uRightShift_param_i64_I"
	.skip 1
	.ascii	"uRightShift_var_i64_I"
	.skip 1
	.ascii	"uRightShift_param_I_C"
	.skip 1
	.ascii	"uRightShift_var_I_C"
	.skip 1
	.ascii	"uRightShift_param_I_I"
	.skip 1
	.ascii	"uRightShift_var_I_I"
	.skip 1
	.ascii	"uRightShift_param_i16_C"
	.skip 1
	.ascii	"uRightShift_var_i16_C"
	.skip 1
	.ascii	"uRightShift_param_i16_I"
	.skip 1
	.ascii	"uRightShift_var_i16_I"
	.skip 1
	.ascii	"uRightShift_param_i32_C"
	.skip 1
	.ascii	"uRightShift_var_i32_C"
	.skip 1
	.ascii	"uRightShift_param_i32_I"
	.skip 1
	.ascii	"uRightShift_var_i32_I"
	.skip 1
	.ascii	"uRightShift_param_LC_C"
	.skip 1
	.ascii	"uRightShift_var_LC_C"
	.skip 1
	.ascii	"uRightShift_param_LC_I"
	.skip 1
	.ascii	"uRightShift_var_LC_I"
	.skip 1
	.ascii	"uRightShift_param_u64_C"
	.skip 1
	.ascii	"uRightShift_var_u64_C"
	.skip 1
	.ascii	"uRightShift_param_u64_I"
	.skip 1
	.ascii	"uRightShift_var_u64_I"
	.skip 1
	.ascii	"uRightShift_param_u16_C"
	.skip 1
	.ascii	"uRightShift_var_u16_C"
	.skip 1
	.ascii	"uRightShift_param_u16_I"
	.skip 1
	.ascii	"uRightShift_var_u16_I"
	.skip 3
	.long	RightShift_M3
	.long	L_1
	.long	RightShift__uRightShift_param_u32_C
	.long	L_1+14
	.long	RightShift__uRightShift_var_u32_C
	.long	L_1+38
	.long	RightShift__uRightShift_param_u32_I
	.long	L_1+60
	.long	RightShift__uRightShift_var_u32_I
	.long	L_1+84
	.long	RightShift__uRightShift_param_i8_C
	.long	L_1+106
	.long	RightShift__uRightShift_var_i8_C
	.long	L_1+129
	.long	RightShift__uRightShift_param_i8_I
	.long	L_1+150
	.long	RightShift__uRightShift_var_i8_I
	.long	L_1+173
	.long	RightShift__uRightShift_param_L_C
	.long	L_1+194
	.long	RightShift__uRightShift_var_L_C
	.long	L_1+216
	.long	RightShift__uRightShift_param_L_I
	.long	L_1+236
	.long	RightShift__uRightShift_var_L_I
	.long	L_1+258
	.long	RightShift__uRightShift_param_u8_C
	.long	L_1+278
	.long	RightShift__uRightShift_var_u8_C
	.long	L_1+301
	.long	RightShift__uRightShift_param_u8_I
	.long	L_1+322
	.long	RightShift__uRightShift_var_u8_I
	.long	L_1+345
	.long	RightShift__uRightShift_param_C_C
	.long	L_1+366
	.long	RightShift__uRightShift_var_C_C
	.long	L_1+388
	.long	RightShift__uRightShift_param_C_I
	.long	L_1+408
	.long	RightShift__uRightShift_var_C_I
	.long	L_1+430
	.long	RightShift__uRightShift_param_i64_C
	.long	L_1+450
	.long	RightShift__uRightShift_var_i64_C
	.long	L_1+474
	.long	RightShift__uRightShift_param_i64_I
	.long	L_1+496
	.long	RightShift__uRightShift_var_i64_I
	.long	L_1+520
	.long	RightShift__uRightShift_param_I_C
	.long	L_1+542
	.long	RightShift__uRightShift_var_I_C
	.long	L_1+564
	.long	RightShift__uRightShift_param_I_I
	.long	L_1+584
	.long	RightShift__uRightShift_var_I_I
	.long	L_1+606
	.long	RightShift__uRightShift_param_i16_C
	.long	L_1+626
	.long	RightShift__uRightShift_var_i16_C
	.long	L_1+650
	.long	RightShift__uRightShift_param_i16_I
	.long	L_1+672
	.long	RightShift__uRightShift_var_i16_I
	.long	L_1+696
	.long	RightShift__uRightShift_param_i32_C
	.long	L_1+718
	.long	RightShift__uRightShift_var_i32_C
	.long	L_1+742
	.long	RightShift__uRightShift_param_i32_I
	.long	L_1+764
	.long	RightShift__uRightShift_var_i32_I
	.long	L_1+788
	.long	RightShift__uRightShift_param_LC_C
	.long	L_1+810
	.long	RightShift__uRightShift_var_LC_C
	.long	L_1+833
	.long	RightShift__uRightShift_param_LC_I
	.long	L_1+854
	.long	RightShift__uRightShift_var_LC_I
	.long	L_1+877
	.long	RightShift__uRightShift_param_u64_C
	.long	L_1+898
	.long	RightShift__uRightShift_var_u64_C
	.long	L_1+922
	.long	RightShift__uRightShift_param_u64_I
	.long	L_1+944
	.long	RightShift__uRightShift_var_u64_I
	.long	L_1+968
	.long	RightShift__uRightShift_param_u16_C
	.long	L_1+990
	.long	RightShift__uRightShift_var_u16_C
	.long	L_1+1014
	.long	RightShift__uRightShift_param_u16_I
	.long	L_1+1036
	.long	RightShift__uRightShift_var_u16_I
	.long	L_1+1060
	.skip 4
	.ascii	"../SOLsun/RightShift.m3"
	.skip 1
	.align 8
	.type	MM_RightShift, #object
	.size	MM_RightShift, 208
MM_RightShift:
	.long	L_1+1480
	.skip 16
	.long	L_1+1084
	.skip 12
	.long	MM_RightShift+148
	.skip 4
	.long	RightShift_M3
	.long	3
	.half	657
	.skip 2
	.long	0
	.long	658
	.long	0
	.long	659
	.long	660
	.half	661
	.skip 2
	.long	662
	.skip 4
	.long	0
	.long	663
	.long	664
	.skip 4
	.long	1082445139
	.long	-137438953
	.byte	-101
	.skip 7
	.long	0
	.long	668
	.byte	-99
	.skip 3
	.long	670
	.long	1143466754
	.skip 12
	.long	RightShift_I3
	.long	MM_RightShift+160
	.skip 4
	.long	Long_I3
	.long	MM_RightShift+172
	.skip 4
	.long	Word_I3
	.long	MM_RightShift+184
	.skip 4
	.long	Cstdint_I3
	.long	MM_RightShift+196
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
	.uaword	.LLFB0
	.uaword	.LLFE0-.LLFB0
	.byte	0x4
	.uaword	.LLCFI0-.LLFB0
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
.LLEFDE3:
.LLSFDE5:
	.uaword	.LLEFDE5-.LLASFDE5
.LLASFDE5:
	.uaword	.LLASFDE5-.LLframe1
	.uaword	.LLFB2
	.uaword	.LLFE2-.LLFB2
	.byte	0x4
	.uaword	.LLCFI2-.LLFB2
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
.LLEFDE7:
.LLSFDE9:
	.uaword	.LLEFDE9-.LLASFDE9
.LLASFDE9:
	.uaword	.LLASFDE9-.LLframe1
	.uaword	.LLFB4
	.uaword	.LLFE4-.LLFB4
	.byte	0x4
	.uaword	.LLCFI4-.LLFB4
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
	.uaword	.LLFB5
	.uaword	.LLFE5-.LLFB5
	.byte	0x4
	.uaword	.LLCFI5-.LLFB5
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
	.uaword	.LLFB6
	.uaword	.LLFE6-.LLFB6
	.byte	0x4
	.uaword	.LLCFI6-.LLFB6
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
	.uaword	.LLFB7
	.uaword	.LLFE7-.LLFB7
	.byte	0x4
	.uaword	.LLCFI7-.LLFB7
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
	.uaword	.LLFB8
	.uaword	.LLFE8-.LLFB8
	.byte	0x4
	.uaword	.LLCFI8-.LLFB8
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
	.uaword	.LLFB9
	.uaword	.LLFE9-.LLFB9
	.byte	0x4
	.uaword	.LLCFI9-.LLFB9
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
	.uaword	.LLFB10
	.uaword	.LLFE10-.LLFB10
	.byte	0x4
	.uaword	.LLCFI10-.LLFB10
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
	.uaword	.LLFB11
	.uaword	.LLFE11-.LLFB11
	.byte	0x4
	.uaword	.LLCFI11-.LLFB11
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
	.uaword	.LLFB12
	.uaword	.LLFE12-.LLFB12
	.byte	0x4
	.uaword	.LLCFI12-.LLFB12
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
.LLEFDE27:
.LLSFDE29:
	.uaword	.LLEFDE29-.LLASFDE29
.LLASFDE29:
	.uaword	.LLASFDE29-.LLframe1
	.uaword	.LLFB14
	.uaword	.LLFE14-.LLFB14
	.byte	0x4
	.uaword	.LLCFI14-.LLFB14
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
.LLEFDE31:
.LLSFDE33:
	.uaword	.LLEFDE33-.LLASFDE33
.LLASFDE33:
	.uaword	.LLASFDE33-.LLframe1
	.uaword	.LLFB16
	.uaword	.LLFE16-.LLFB16
	.byte	0x4
	.uaword	.LLCFI16-.LLFB16
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
.LLEFDE35:
.LLSFDE37:
	.uaword	.LLEFDE37-.LLASFDE37
.LLASFDE37:
	.uaword	.LLASFDE37-.LLframe1
	.uaword	.LLFB18
	.uaword	.LLFE18-.LLFB18
	.byte	0x4
	.uaword	.LLCFI18-.LLFB18
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
.LLEFDE39:
.LLSFDE41:
	.uaword	.LLEFDE41-.LLASFDE41
.LLASFDE41:
	.uaword	.LLASFDE41-.LLframe1
	.uaword	.LLFB20
	.uaword	.LLFE20-.LLFB20
	.byte	0x4
	.uaword	.LLCFI20-.LLFB20
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
.LLEFDE43:
.LLSFDE45:
	.uaword	.LLEFDE45-.LLASFDE45
.LLASFDE45:
	.uaword	.LLASFDE45-.LLframe1
	.uaword	.LLFB22
	.uaword	.LLFE22-.LLFB22
	.byte	0x4
	.uaword	.LLCFI22-.LLFB22
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
.LLEFDE47:
.LLSFDE49:
	.uaword	.LLEFDE49-.LLASFDE49
.LLASFDE49:
	.uaword	.LLASFDE49-.LLframe1
	.uaword	.LLFB24
	.uaword	.LLFE24-.LLFB24
	.byte	0x4
	.uaword	.LLCFI24-.LLFB24
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
	.uaword	.LLFB25
	.uaword	.LLFE25-.LLFB25
	.byte	0x4
	.uaword	.LLCFI25-.LLFB25
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
	.uaword	.LLFB26
	.uaword	.LLFE26-.LLFB26
	.byte	0x4
	.uaword	.LLCFI26-.LLFB26
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
	.uaword	.LLFB27
	.uaword	.LLFE27-.LLFB27
	.byte	0x4
	.uaword	.LLCFI27-.LLFB27
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
	.uaword	.LLFB28
	.uaword	.LLFE28-.LLFB28
	.byte	0x4
	.uaword	.LLCFI28-.LLFB28
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
.LLEFDE59:
.LLSFDE61:
	.uaword	.LLEFDE61-.LLASFDE61
.LLASFDE61:
	.uaword	.LLASFDE61-.LLframe1
	.uaword	.LLFB30
	.uaword	.LLFE30-.LLFB30
	.byte	0x4
	.uaword	.LLCFI30-.LLFB30
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
.LLEFDE63:
.LLSFDE65:
	.uaword	.LLEFDE65-.LLASFDE65
.LLASFDE65:
	.uaword	.LLASFDE65-.LLframe1
	.uaword	.LLFB32
	.uaword	.LLFE32-.LLFB32
	.byte	0x4
	.uaword	.LLCFI32-.LLFB32
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
.LLEFDE67:
.LLSFDE69:
	.uaword	.LLEFDE69-.LLASFDE69
.LLASFDE69:
	.uaword	.LLASFDE69-.LLframe1
	.uaword	.LLFB34
	.uaword	.LLFE34-.LLFB34
	.byte	0x4
	.uaword	.LLCFI34-.LLFB34
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
.LLEFDE71:
.LLSFDE73:
	.uaword	.LLEFDE73-.LLASFDE73
.LLASFDE73:
	.uaword	.LLASFDE73-.LLframe1
	.uaword	.LLFB36
	.uaword	.LLFE36-.LLFB36
	.byte	0x4
	.uaword	.LLCFI36-.LLFB36
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
.LLEFDE75:
.LLSFDE77:
	.uaword	.LLEFDE77-.LLASFDE77
.LLASFDE77:
	.uaword	.LLASFDE77-.LLframe1
	.uaword	.LLFB38
	.uaword	.LLFE38-.LLFB38
	.byte	0x4
	.uaword	.LLCFI38-.LLFB38
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
	.uaword	.LLFB39
	.uaword	.LLFE39-.LLFB39
	.byte	0x4
	.uaword	.LLCFI39-.LLFB39
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
	.uaword	.LLFB40
	.uaword	.LLFE40-.LLFB40
	.byte	0x4
	.uaword	.LLCFI40-.LLFB40
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
.LLEFDE83:
.LLSFDE85:
	.uaword	.LLEFDE85-.LLASFDE85
.LLASFDE85:
	.uaword	.LLASFDE85-.LLframe1
	.uaword	.LLFB42
	.uaword	.LLFE42-.LLFB42
	.byte	0x4
	.uaword	.LLCFI42-.LLFB42
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
.LLEFDE87:
.LLSFDE89:
	.uaword	.LLEFDE89-.LLASFDE89
.LLASFDE89:
	.uaword	.LLASFDE89-.LLframe1
	.uaword	.LLFB44
	.uaword	.LLFE44-.LLFB44
	.byte	0x4
	.uaword	.LLCFI44-.LLFB44
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
.LLEFDE91:
.LLSFDE93:
	.uaword	.LLEFDE93-.LLASFDE93
.LLASFDE93:
	.uaword	.LLASFDE93-.LLframe1
	.uaword	.LLFB46
	.uaword	.LLFE46-.LLFB46
	.byte	0x4
	.uaword	.LLCFI46-.LLFB46
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
.LLEFDE95:
.LLSFDE97:
	.uaword	.LLEFDE97-.LLASFDE97
.LLASFDE97:
	.uaword	.LLASFDE97-.LLframe1
	.uaword	.LLFB49
	.uaword	.LLFE49-.LLFB49
	.byte	0x4
	.uaword	.LLCFI49-.LLFB49
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE97:
	.ident	"GCC: (GNU) 4.5.1"
