	.file	"extract.mc"
	.section	".text"
	.align 4
	.type	extract__uExtract_var_u16_u16, #function
	.proc	04
extract__uExtract_var_u16_u16:
.LLFB0:
	save	%sp, -96, %sp
.LLCFI0:
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+140], %g1
	mov	%g1, %o5
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 32
	ble	%icc, .LL2
	 nop
	mov	1729, %o0
	call	_m3_fault, 0
	 nop
.LL2:
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL3
	 nop
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	mov	32, %o4
	sub	%o4, %g1, %o4
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+140], %g1
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	mov	32, %o4
	sub	%o4, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL4
	 nop
.LL3:
	mov	0, %g1
.LL4:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE0:
	.size	extract__uExtract_var_u16_u16, .-extract__uExtract_var_u16_u16
	.align 4
	.type	extract__uExtract_param_u16_u16, #function
	.proc	04
extract__uExtract_param_u16_u16:
.LLFB1:
	save	%sp, -96, %sp
.LLCFI1:
	sth	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	st	%i2, [%fp+76]
	st	%i3, [%fp+80]
	ld	[%fp+76], %o5
	ld	[%fp+80], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 32
	ble	%icc, .LL6
	 nop
	mov	1761, %o0
	call	_m3_fault, 0
	 nop
.LL6:
	ld	[%fp+80], %g1
	cmp	%g1, 0
	be	%icc, .LL7
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+80], %g1
	mov	32, %o4
	sub	%o4, %g1, %o4
	ld	[%fp+76], %g1
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	ld	[%fp+80], %g1
	mov	32, %o4
	sub	%o4, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL8
	 nop
.LL7:
	mov	0, %g1
.LL8:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1:
	.size	extract__uExtract_param_u16_u16, .-extract__uExtract_param_u16_u16
	.align 4
	.type	extract__uExtract_var_u64_u64, #function
	.proc	05
extract__uExtract_var_u64_u64:
.LLFB2:
	save	%sp, -96, %sp
.LLCFI2:
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+140], %g1
	mov	%g1, %o5
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 64
	ble	%icc, .LL10
	 nop
	mov	1793, %o0
	call	_m3_fault, 0
	 nop
.LL10:
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL11
	 nop
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ldd	[%g1+56], %o4
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %o3
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+140], %g1
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL12
	 nop
.LL11:
	mov	0, %o4
	mov	0, %o5
.LL12:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE2:
	.size	extract__uExtract_var_u64_u64, .-extract__uExtract_var_u64_u64
	.align 4
	.type	extract__uExtract_param_u64_u64, #function
	.proc	05
extract__uExtract_param_u64_u64:
.LLFB3:
	save	%sp, -112, %sp
.LLCFI3:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	st	%i4, [%fp+84]
	st	%i5, [%fp+88]
	ld	[%fp+84], %o5
	ld	[%fp+88], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 64
	ble	%icc, .LL14
	 nop
	mov	1825, %o0
	call	_m3_fault, 0
	 nop
.LL14:
	ld	[%fp+88], %g1
	cmp	%g1, 0
	be	%icc, .LL15
	 nop
	ldd	[%fp-8], %o4
	ld	[%fp+88], %g1
	mov	64, %o3
	sub	%o3, %g1, %o3
	ld	[%fp+84], %g1
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ld	[%fp+88], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL16
	 nop
.LL15:
	mov	0, %o4
	mov	0, %o5
.LL16:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE3:
	.size	extract__uExtract_param_u64_u64, .-extract__uExtract_param_u64_u64
	.align 4
	.type	extract__uExtract_var_LC_LC, #function
	.proc	05
extract__uExtract_var_LC_LC:
.LLFB4:
	save	%sp, -96, %sp
.LLCFI4:
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+140], %g1
	mov	%g1, %o5
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 64
	ble	%icc, .LL18
	 nop
	mov	1857, %o0
	call	_m3_fault, 0
	 nop
.LL18:
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL19
	 nop
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ldd	[%g1+64], %o4
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %o3
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+140], %g1
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL20
	 nop
.LL19:
	mov	0, %o4
	mov	0, %o5
.LL20:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE4:
	.size	extract__uExtract_var_LC_LC, .-extract__uExtract_var_LC_LC
	.align 4
	.type	extract__uExtract_param_LC_LC, #function
	.proc	05
extract__uExtract_param_LC_LC:
.LLFB5:
	save	%sp, -112, %sp
.LLCFI5:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	st	%i4, [%fp+84]
	st	%i5, [%fp+88]
	ld	[%fp+84], %o5
	ld	[%fp+88], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 64
	ble	%icc, .LL22
	 nop
	mov	1889, %o0
	call	_m3_fault, 0
	 nop
.LL22:
	ld	[%fp+88], %g1
	cmp	%g1, 0
	be	%icc, .LL23
	 nop
	ld	[%fp+88], %g1
	mov	64, %o5
	sub	%o5, %g1, %o5
	ld	[%fp+84], %g1
	sub	%o5, %g1, %g1
	ldd	[%fp-8], %o4
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ld	[%fp+88], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL24
	 nop
.LL23:
	mov	0, %o4
	mov	0, %o5
.LL24:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE5:
	.size	extract__uExtract_param_LC_LC, .-extract__uExtract_param_LC_LC
	.align 4
	.type	extract__uExtract_var_i32_i32, #function
	.proc	04
extract__uExtract_var_i32_i32:
.LLFB6:
	save	%sp, -96, %sp
.LLCFI6:
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+140], %g1
	mov	%g1, %o5
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 32
	ble	%icc, .LL26
	 nop
	mov	1921, %o0
	call	_m3_fault, 0
	 nop
.LL26:
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL27
	 nop
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	mov	32, %o4
	sub	%o4, %g1, %o4
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+140], %g1
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	mov	32, %o4
	sub	%o4, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL28
	 nop
.LL27:
	mov	0, %g1
.LL28:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE6:
	.size	extract__uExtract_var_i32_i32, .-extract__uExtract_var_i32_i32
	.align 4
	.type	extract__uExtract_param_i32_i32, #function
	.proc	04
extract__uExtract_param_i32_i32:
.LLFB7:
	save	%sp, -96, %sp
.LLCFI7:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	st	%i3, [%fp+80]
	ld	[%fp+76], %o5
	ld	[%fp+80], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 32
	ble	%icc, .LL30
	 nop
	mov	1953, %o0
	call	_m3_fault, 0
	 nop
.LL30:
	ld	[%fp+80], %g1
	cmp	%g1, 0
	be	%icc, .LL31
	 nop
	ld	[%fp+68], %o5
	ld	[%fp+80], %g1
	mov	32, %o4
	sub	%o4, %g1, %o4
	ld	[%fp+76], %g1
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	ld	[%fp+80], %g1
	mov	32, %o4
	sub	%o4, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL32
	 nop
.LL31:
	mov	0, %g1
.LL32:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE7:
	.size	extract__uExtract_param_i32_i32, .-extract__uExtract_param_i32_i32
	.align 4
	.type	extract__uExtract_var_i16_i16, #function
	.proc	04
extract__uExtract_var_i16_i16:
.LLFB8:
	save	%sp, -96, %sp
.LLCFI8:
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+140], %g1
	mov	%g1, %o5
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 32
	ble	%icc, .LL34
	 nop
	mov	1985, %o0
	call	_m3_fault, 0
	 nop
.LL34:
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL35
	 nop
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	mov	32, %o4
	sub	%o4, %g1, %o4
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+140], %g1
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	mov	32, %o4
	sub	%o4, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL36
	 nop
.LL35:
	mov	0, %g1
.LL36:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE8:
	.size	extract__uExtract_var_i16_i16, .-extract__uExtract_var_i16_i16
	.align 4
	.type	extract__uExtract_param_i16_i16, #function
	.proc	04
extract__uExtract_param_i16_i16:
.LLFB9:
	save	%sp, -96, %sp
.LLCFI9:
	sth	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	st	%i2, [%fp+76]
	st	%i3, [%fp+80]
	ld	[%fp+76], %o5
	ld	[%fp+80], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 32
	ble	%icc, .LL38
	 nop
	mov	2017, %o0
	call	_m3_fault, 0
	 nop
.LL38:
	ld	[%fp+80], %g1
	cmp	%g1, 0
	be	%icc, .LL39
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+80], %g1
	mov	32, %o4
	sub	%o4, %g1, %o4
	ld	[%fp+76], %g1
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	ld	[%fp+80], %g1
	mov	32, %o4
	sub	%o4, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL40
	 nop
.LL39:
	mov	0, %g1
.LL40:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE9:
	.size	extract__uExtract_param_i16_i16, .-extract__uExtract_param_i16_i16
	.align 4
	.type	extract__uExtract_var_I_I, #function
	.proc	04
extract__uExtract_var_I_I:
.LLFB10:
	save	%sp, -96, %sp
.LLCFI10:
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+140], %g1
	mov	%g1, %o5
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 32
	ble	%icc, .LL42
	 nop
	mov	2049, %o0
	call	_m3_fault, 0
	 nop
.LL42:
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL43
	 nop
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	mov	32, %o4
	sub	%o4, %g1, %o4
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+140], %g1
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	mov	32, %o4
	sub	%o4, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL44
	 nop
.LL43:
	mov	0, %g1
.LL44:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE10:
	.size	extract__uExtract_var_I_I, .-extract__uExtract_var_I_I
	.align 4
	.type	extract__uExtract_param_I_I, #function
	.proc	04
extract__uExtract_param_I_I:
.LLFB11:
	save	%sp, -96, %sp
.LLCFI11:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	st	%i3, [%fp+80]
	ld	[%fp+76], %o5
	ld	[%fp+80], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 32
	ble	%icc, .LL46
	 nop
	mov	2081, %o0
	call	_m3_fault, 0
	 nop
.LL46:
	ld	[%fp+80], %g1
	cmp	%g1, 0
	be	%icc, .LL47
	 nop
	ld	[%fp+68], %o5
	ld	[%fp+80], %g1
	mov	32, %o4
	sub	%o4, %g1, %o4
	ld	[%fp+76], %g1
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	ld	[%fp+80], %g1
	mov	32, %o4
	sub	%o4, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL48
	 nop
.LL47:
	mov	0, %g1
.LL48:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE11:
	.size	extract__uExtract_param_I_I, .-extract__uExtract_param_I_I
	.align 4
	.type	extract__uExtract_var_i64_i64, #function
	.proc	05
extract__uExtract_var_i64_i64:
.LLFB12:
	save	%sp, -96, %sp
.LLCFI12:
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+140], %g1
	mov	%g1, %o5
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 64
	ble	%icc, .LL50
	 nop
	mov	2113, %o0
	call	_m3_fault, 0
	 nop
.LL50:
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL51
	 nop
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ldd	[%g1+88], %o4
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %o3
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+140], %g1
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL52
	 nop
.LL51:
	mov	0, %o4
	mov	0, %o5
.LL52:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE12:
	.size	extract__uExtract_var_i64_i64, .-extract__uExtract_var_i64_i64
	.align 4
	.type	extract__uExtract_param_i64_i64, #function
	.proc	05
extract__uExtract_param_i64_i64:
.LLFB13:
	save	%sp, -112, %sp
.LLCFI13:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	st	%i4, [%fp+84]
	st	%i5, [%fp+88]
	ld	[%fp+84], %o5
	ld	[%fp+88], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 64
	ble	%icc, .LL54
	 nop
	mov	2145, %o0
	call	_m3_fault, 0
	 nop
.LL54:
	ld	[%fp+88], %g1
	cmp	%g1, 0
	be	%icc, .LL55
	 nop
	ldd	[%fp-8], %o4
	ld	[%fp+88], %g1
	mov	64, %o3
	sub	%o3, %g1, %o3
	ld	[%fp+84], %g1
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ld	[%fp+88], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL56
	 nop
.LL55:
	mov	0, %o4
	mov	0, %o5
.LL56:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE13:
	.size	extract__uExtract_param_i64_i64, .-extract__uExtract_param_i64_i64
	.align 4
	.type	extract__uExtract_var_C_C, #function
	.proc	04
extract__uExtract_var_C_C:
.LLFB14:
	save	%sp, -96, %sp
.LLCFI14:
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+140], %g1
	mov	%g1, %o5
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 32
	ble	%icc, .LL58
	 nop
	mov	2177, %o0
	call	_m3_fault, 0
	 nop
.LL58:
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL59
	 nop
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	mov	32, %o4
	sub	%o4, %g1, %o4
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+140], %g1
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	mov	32, %o4
	sub	%o4, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL60
	 nop
.LL59:
	mov	0, %g1
.LL60:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE14:
	.size	extract__uExtract_var_C_C, .-extract__uExtract_var_C_C
	.align 4
	.type	extract__uExtract_param_C_C, #function
	.proc	04
extract__uExtract_param_C_C:
.LLFB15:
	save	%sp, -96, %sp
.LLCFI15:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	st	%i3, [%fp+80]
	ld	[%fp+76], %o5
	ld	[%fp+80], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 32
	ble	%icc, .LL62
	 nop
	mov	2209, %o0
	call	_m3_fault, 0
	 nop
.LL62:
	ld	[%fp+80], %g1
	cmp	%g1, 0
	be	%icc, .LL63
	 nop
	ld	[%fp+80], %g1
	mov	32, %o5
	sub	%o5, %g1, %o5
	ld	[%fp+76], %g1
	sub	%o5, %g1, %g1
	ld	[%fp+68], %o5
	sll	%o5, %g1, %o5
	ld	[%fp+80], %g1
	mov	32, %o4
	sub	%o4, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL64
	 nop
.LL63:
	mov	0, %g1
.LL64:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE15:
	.size	extract__uExtract_param_C_C, .-extract__uExtract_param_C_C
	.align 4
	.type	extract__uExtract_var_u8_u8, #function
	.proc	04
extract__uExtract_var_u8_u8:
.LLFB16:
	save	%sp, -96, %sp
.LLCFI16:
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+140], %g1
	mov	%g1, %o5
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 32
	ble	%icc, .LL66
	 nop
	mov	2241, %o0
	call	_m3_fault, 0
	 nop
.LL66:
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL67
	 nop
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	mov	32, %o4
	sub	%o4, %g1, %o4
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+140], %g1
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	mov	32, %o4
	sub	%o4, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL68
	 nop
.LL67:
	mov	0, %g1
.LL68:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE16:
	.size	extract__uExtract_var_u8_u8, .-extract__uExtract_var_u8_u8
	.align 4
	.type	extract__uExtract_param_u8_u8, #function
	.proc	04
extract__uExtract_param_u8_u8:
.LLFB17:
	save	%sp, -96, %sp
.LLCFI17:
	stb	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	st	%i2, [%fp+76]
	st	%i3, [%fp+80]
	ld	[%fp+76], %o5
	ld	[%fp+80], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 32
	ble	%icc, .LL70
	 nop
	mov	2273, %o0
	call	_m3_fault, 0
	 nop
.LL70:
	ld	[%fp+80], %g1
	cmp	%g1, 0
	be	%icc, .LL71
	 nop
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+80], %g1
	mov	32, %o4
	sub	%o4, %g1, %o4
	ld	[%fp+76], %g1
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	ld	[%fp+80], %g1
	mov	32, %o4
	sub	%o4, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL72
	 nop
.LL71:
	mov	0, %g1
.LL72:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE17:
	.size	extract__uExtract_param_u8_u8, .-extract__uExtract_param_u8_u8
	.align 4
	.type	extract__uExtract_var_L_L, #function
	.proc	05
extract__uExtract_var_L_L:
.LLFB18:
	save	%sp, -96, %sp
.LLCFI18:
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+140], %g1
	mov	%g1, %o5
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 64
	ble	%icc, .LL74
	 nop
	mov	2305, %o0
	call	_m3_fault, 0
	 nop
.LL74:
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL75
	 nop
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ldd	[%g1+120], %o4
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %o3
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+140], %g1
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL76
	 nop
.LL75:
	mov	0, %o4
	mov	0, %o5
.LL76:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE18:
	.size	extract__uExtract_var_L_L, .-extract__uExtract_var_L_L
	.align 4
	.type	extract__uExtract_param_L_L, #function
	.proc	05
extract__uExtract_param_L_L:
.LLFB19:
	save	%sp, -112, %sp
.LLCFI19:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	st	%i4, [%fp+84]
	st	%i5, [%fp+88]
	ld	[%fp+84], %o5
	ld	[%fp+88], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 64
	ble	%icc, .LL78
	 nop
	mov	2337, %o0
	call	_m3_fault, 0
	 nop
.LL78:
	ld	[%fp+88], %g1
	cmp	%g1, 0
	be	%icc, .LL79
	 nop
	ldd	[%fp-8], %o4
	ld	[%fp+88], %g1
	mov	64, %o3
	sub	%o3, %g1, %o3
	ld	[%fp+84], %g1
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ld	[%fp+88], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL80
	 nop
.LL79:
	mov	0, %o4
	mov	0, %o5
.LL80:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE19:
	.size	extract__uExtract_param_L_L, .-extract__uExtract_param_L_L
	.align 4
	.type	extract__uExtract_var_i8_i8, #function
	.proc	04
extract__uExtract_var_i8_i8:
.LLFB20:
	save	%sp, -96, %sp
.LLCFI20:
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+140], %g1
	mov	%g1, %o5
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 32
	ble	%icc, .LL82
	 nop
	mov	2369, %o0
	call	_m3_fault, 0
	 nop
.LL82:
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL83
	 nop
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	mov	32, %o4
	sub	%o4, %g1, %o4
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+140], %g1
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	mov	32, %o4
	sub	%o4, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL84
	 nop
.LL83:
	mov	0, %g1
.LL84:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE20:
	.size	extract__uExtract_var_i8_i8, .-extract__uExtract_var_i8_i8
	.align 4
	.type	extract__uExtract_param_i8_i8, #function
	.proc	04
extract__uExtract_param_i8_i8:
.LLFB21:
	save	%sp, -96, %sp
.LLCFI21:
	stb	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	st	%i2, [%fp+76]
	st	%i3, [%fp+80]
	ld	[%fp+76], %o5
	ld	[%fp+80], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 32
	ble	%icc, .LL86
	 nop
	mov	2401, %o0
	call	_m3_fault, 0
	 nop
.LL86:
	ld	[%fp+80], %g1
	cmp	%g1, 0
	be	%icc, .LL87
	 nop
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+80], %g1
	mov	32, %o4
	sub	%o4, %g1, %o4
	ld	[%fp+76], %g1
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	ld	[%fp+80], %g1
	mov	32, %o4
	sub	%o4, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL88
	 nop
.LL87:
	mov	0, %g1
.LL88:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE21:
	.size	extract__uExtract_param_i8_i8, .-extract__uExtract_param_i8_i8
	.align 4
	.type	extract__uExtract_var_u32_u32, #function
	.proc	04
extract__uExtract_var_u32_u32:
.LLFB22:
	save	%sp, -96, %sp
.LLCFI22:
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+140], %g1
	mov	%g1, %o5
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 32
	ble	%icc, .LL90
	 nop
	mov	2433, %o0
	call	_m3_fault, 0
	 nop
.LL90:
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL91
	 nop
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	mov	32, %o4
	sub	%o4, %g1, %o4
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+140], %g1
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	ld	[%g1+144], %g1
	mov	32, %o4
	sub	%o4, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL92
	 nop
.LL91:
	mov	0, %g1
.LL92:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE22:
	.size	extract__uExtract_var_u32_u32, .-extract__uExtract_var_u32_u32
	.align 4
	.type	extract__uExtract_param_u32_u32, #function
	.proc	04
extract__uExtract_param_u32_u32:
.LLFB23:
	save	%sp, -96, %sp
.LLCFI23:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	st	%i3, [%fp+80]
	ld	[%fp+76], %o5
	ld	[%fp+80], %g1
	add	%o5, %g1, %g1
	cmp	%g1, 32
	ble	%icc, .LL94
	 nop
	mov	2465, %o0
	call	_m3_fault, 0
	 nop
.LL94:
	ld	[%fp+80], %g1
	cmp	%g1, 0
	be	%icc, .LL95
	 nop
	ld	[%fp+68], %o5
	ld	[%fp+80], %g1
	mov	32, %o4
	sub	%o4, %g1, %o4
	ld	[%fp+76], %g1
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	ld	[%fp+80], %g1
	mov	32, %o4
	sub	%o4, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL96
	 nop
.LL95:
	mov	0, %g1
.LL96:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE23:
	.size	extract__uExtract_param_u32_u32, .-extract__uExtract_param_u32_u32
	.align 4
	.global extract_M3
	.type	extract_M3, #function
	.proc	0120
extract_M3:
.LLFB24:
	save	%sp, -96, %sp
.LLCFI24:
	st	%i0, [%fp+68]
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE24:
	.size	extract_M3, .-extract_M3
	.align 4
	.type	_m3_fault, #function
	.proc	020
_m3_fault:
.LLFB25:
	save	%sp, -96, %sp
.LLCFI25:
	st	%i0, [%fp+68]
	sethi	%hi(MM_extract), %g1
	or	%g1, %lo(MM_extract), %o0
	ld	[%fp+68], %o1
	call	RTHooks__ReportFault, 0
	 nop
	return	%i7+8
	 nop
.LLFE25:
	.size	_m3_fault, .-_m3_fault
	.section	".data"
	.align 8
	.type	L_1, #object
	.size	L_1, 736
L_1:
	.ascii	"extract_M3"
	.skip 1
	.ascii	"uExtract_param_u32_u32"
	.skip 1
	.ascii	"uExtract_var_u32_u32"
	.skip 1
	.ascii	"uExtract_param_i8_i8"
	.skip 1
	.ascii	"uExtract_var_i8_i8"
	.skip 1
	.ascii	"uExtract_param_L_L"
	.skip 1
	.ascii	"uExtract_var_L_L"
	.skip 1
	.ascii	"uExtract_param_u8_u8"
	.skip 1
	.ascii	"uExtract_var_u8_u8"
	.skip 1
	.ascii	"uExtract_param_C_C"
	.skip 1
	.ascii	"uExtract_var_C_C"
	.skip 1
	.ascii	"uExtract_param_i64_i64"
	.skip 1
	.ascii	"uExtract_var_i64_i64"
	.skip 1
	.ascii	"uExtract_param_I_I"
	.skip 1
	.ascii	"uExtract_var_I_I"
	.skip 1
	.ascii	"uExtract_param_i16_i16"
	.skip 1
	.ascii	"uExtract_var_i16_i16"
	.skip 1
	.ascii	"uExtract_param_i32_i32"
	.skip 1
	.ascii	"uExtract_var_i32_i32"
	.skip 1
	.ascii	"uExtract_param_LC_LC"
	.skip 1
	.ascii	"uExtract_var_LC_LC"
	.skip 1
	.ascii	"uExtract_param_u64_u64"
	.skip 1
	.ascii	"uExtract_var_u64_u64"
	.skip 1
	.ascii	"uExtract_param_u16_u16"
	.skip 1
	.ascii	"uExtract_var_u16_u16"
	.skip 2
	.long	extract_M3
	.long	L_1
	.long	extract__uExtract_param_u32_u32
	.long	L_1+11
	.long	extract__uExtract_var_u32_u32
	.long	L_1+34
	.long	extract__uExtract_param_i8_i8
	.long	L_1+55
	.long	extract__uExtract_var_i8_i8
	.long	L_1+76
	.long	extract__uExtract_param_L_L
	.long	L_1+95
	.long	extract__uExtract_var_L_L
	.long	L_1+114
	.long	extract__uExtract_param_u8_u8
	.long	L_1+131
	.long	extract__uExtract_var_u8_u8
	.long	L_1+152
	.long	extract__uExtract_param_C_C
	.long	L_1+171
	.long	extract__uExtract_var_C_C
	.long	L_1+190
	.long	extract__uExtract_param_i64_i64
	.long	L_1+207
	.long	extract__uExtract_var_i64_i64
	.long	L_1+230
	.long	extract__uExtract_param_I_I
	.long	L_1+251
	.long	extract__uExtract_var_I_I
	.long	L_1+270
	.long	extract__uExtract_param_i16_i16
	.long	L_1+287
	.long	extract__uExtract_var_i16_i16
	.long	L_1+310
	.long	extract__uExtract_param_i32_i32
	.long	L_1+331
	.long	extract__uExtract_var_i32_i32
	.long	L_1+354
	.long	extract__uExtract_param_LC_LC
	.long	L_1+375
	.long	extract__uExtract_var_LC_LC
	.long	L_1+396
	.long	extract__uExtract_param_u64_u64
	.long	L_1+415
	.long	extract__uExtract_var_u64_u64
	.long	L_1+438
	.long	extract__uExtract_param_u16_u16
	.long	L_1+459
	.long	extract__uExtract_var_u16_u16
	.long	L_1+482
	.skip 4
	.ascii	"../SOLsun/extract.m3"
	.skip 8
	.align 8
	.type	MM_extract, #object
	.size	MM_extract, 208
MM_extract:
	.long	L_1+708
	.skip 16
	.long	L_1+504
	.skip 12
	.long	MM_extract+148
	.skip 4
	.long	extract_M3
	.long	3
	.half	849
	.skip 2
	.long	0
	.long	850
	.long	0
	.long	851
	.long	852
	.half	853
	.skip 2
	.long	854
	.skip 4
	.long	0
	.long	855
	.long	856
	.skip 4
	.long	1082838749
	.long	790273982
	.byte	91
	.skip 7
	.long	0
	.long	860
	.byte	93
	.skip 3
	.long	862
	.long	1146615628
	.skip 12
	.long	extract_I3
	.long	MM_extract+160
	.skip 4
	.long	Long_I3
	.long	MM_extract+172
	.skip 4
	.long	Word_I3
	.long	MM_extract+184
	.skip 4
	.long	Cstdint_I3
	.long	MM_extract+196
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
.LLEFDE49:
	.ident	"GCC: (GNU) 4.5.1"
