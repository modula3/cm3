	.file	"LeftRotate.mc"
	.section	".text"
	.align 4
	.type	LeftRotate__uLeftRotate_var_u16_I, #function
	.proc	04
LeftRotate__uLeftRotate_var_u16_I:
.LLFB0:
	save	%sp, -96, %sp
.LLCFI0:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL2
	 nop
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 31
	ble	%icc, .LL3
	 nop
.LL2:
	mov	1729, %o0
	call	_m3_fault, 0
	 nop
.LL3:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sethi	%hi(MM_LeftRotate), %o5
	or	%o5, %lo(MM_LeftRotate), %o5
	ld	[%o5+80], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE0:
	.size	LeftRotate__uLeftRotate_var_u16_I, .-LeftRotate__uLeftRotate_var_u16_I
	.align 4
	.type	LeftRotate__uLeftRotate_param_u16_I, #function
	.proc	04
LeftRotate__uLeftRotate_param_u16_I:
.LLFB1:
	save	%sp, -96, %sp
.LLCFI1:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL5
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL6
	 nop
.LL5:
	mov	1761, %o0
	call	_m3_fault, 0
	 nop
.LL6:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	ld	[%fp+72], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1:
	.size	LeftRotate__uLeftRotate_param_u16_I, .-LeftRotate__uLeftRotate_param_u16_I
	.align 4
	.type	LeftRotate__uLeftRotate_var_u16_C, #function
	.proc	04
LeftRotate__uLeftRotate_var_u16_C:
.LLFB2:
	save	%sp, -96, %sp
.LLCFI2:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 31
	ble	%icc, .LL8
	 nop
	mov	1793, %o0
	call	_m3_fault, 0
	 nop
.LL8:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sethi	%hi(MM_LeftRotate), %o5
	or	%o5, %lo(MM_LeftRotate), %o5
	ld	[%o5+96], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE2:
	.size	LeftRotate__uLeftRotate_var_u16_C, .-LeftRotate__uLeftRotate_var_u16_C
	.align 4
	.type	LeftRotate__uLeftRotate_param_u16_C, #function
	.proc	04
LeftRotate__uLeftRotate_param_u16_C:
.LLFB3:
	save	%sp, -96, %sp
.LLCFI3:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL10
	 nop
	mov	1825, %o0
	call	_m3_fault, 0
	 nop
.LL10:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	ld	[%fp+72], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE3:
	.size	LeftRotate__uLeftRotate_param_u16_C, .-LeftRotate__uLeftRotate_param_u16_C
	.align 4
	.type	LeftRotate__uLeftRotate_var_u64_I, #function
	.proc	05
LeftRotate__uLeftRotate_var_u64_I:
.LLFB4:
	save	%sp, -96, %sp
.LLCFI4:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL12
	 nop
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 63
	ble	%icc, .LL13
	 nop
.LL12:
	mov	1857, %o0
	call	_m3_fault, 0
	 nop
.LL13:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ldd	[%g1+56], %o4
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+80], %g1
	and	%g1, 63, %g1
	sllx	%o4, 32, %o2
	or	%o5, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	mov	64, %o1
	sub	%o1, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o4, %o2, %o4
	or	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE4:
	.size	LeftRotate__uLeftRotate_var_u64_I, .-LeftRotate__uLeftRotate_var_u64_I
	.align 4
	.type	LeftRotate__uLeftRotate_param_u64_I, #function
	.proc	05
LeftRotate__uLeftRotate_param_u64_I:
.LLFB5:
	save	%sp, -104, %sp
.LLCFI5:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	cmp	%g1, 0
	bl	%icc, .LL15
	 nop
	ld	[%fp+76], %g1
	cmp	%g1, 63
	ble	%icc, .LL16
	 nop
.LL15:
	mov	1889, %o0
	call	_m3_fault, 0
	 nop
.LL16:
	ldd	[%fp-8], %o4
	ld	[%fp+76], %g1
	and	%g1, 63, %g1
	sllx	%o4, 32, %o2
	or	%o5, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	mov	64, %o1
	sub	%o1, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o4, %o2, %o4
	or	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE5:
	.size	LeftRotate__uLeftRotate_param_u64_I, .-LeftRotate__uLeftRotate_param_u64_I
	.align 4
	.type	LeftRotate__uLeftRotate_var_u64_C, #function
	.proc	05
LeftRotate__uLeftRotate_var_u64_C:
.LLFB6:
	save	%sp, -96, %sp
.LLCFI6:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 63
	ble	%icc, .LL18
	 nop
	mov	1921, %o0
	call	_m3_fault, 0
	 nop
.LL18:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ldd	[%g1+56], %o4
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+96], %g1
	and	%g1, 63, %g1
	sllx	%o4, 32, %o2
	or	%o5, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	mov	64, %o1
	sub	%o1, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o4, %o2, %o4
	or	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE6:
	.size	LeftRotate__uLeftRotate_var_u64_C, .-LeftRotate__uLeftRotate_var_u64_C
	.align 4
	.type	LeftRotate__uLeftRotate_param_u64_C, #function
	.proc	05
LeftRotate__uLeftRotate_param_u64_C:
.LLFB7:
	save	%sp, -104, %sp
.LLCFI7:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	cmp	%g1, 63
	ble	%icc, .LL20
	 nop
	mov	1953, %o0
	call	_m3_fault, 0
	 nop
.LL20:
	ldd	[%fp-8], %o4
	ld	[%fp+76], %g1
	and	%g1, 63, %g1
	sllx	%o4, 32, %o2
	or	%o5, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	mov	64, %o1
	sub	%o1, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o4, %o2, %o4
	or	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE7:
	.size	LeftRotate__uLeftRotate_param_u64_C, .-LeftRotate__uLeftRotate_param_u64_C
	.align 4
	.type	LeftRotate__uLeftRotate_var_LC_I, #function
	.proc	05
LeftRotate__uLeftRotate_var_LC_I:
.LLFB8:
	save	%sp, -96, %sp
.LLCFI8:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL22
	 nop
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 63
	ble	%icc, .LL23
	 nop
.LL22:
	mov	1985, %o0
	call	_m3_fault, 0
	 nop
.LL23:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ldd	[%g1+64], %o4
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+80], %g1
	and	%g1, 63, %g1
	sllx	%o4, 32, %o2
	or	%o5, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	mov	64, %o1
	sub	%o1, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o4, %o2, %o4
	or	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE8:
	.size	LeftRotate__uLeftRotate_var_LC_I, .-LeftRotate__uLeftRotate_var_LC_I
	.align 4
	.type	LeftRotate__uLeftRotate_param_LC_I, #function
	.proc	05
LeftRotate__uLeftRotate_param_LC_I:
.LLFB9:
	save	%sp, -104, %sp
.LLCFI9:
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
	mov	2017, %o0
	call	_m3_fault, 0
	 nop
.LL26:
	ld	[%fp+76], %g1
	and	%g1, 63, %g1
	ldd	[%fp-8], %o4
	sllx	%o4, 32, %o2
	or	%o5, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	mov	64, %o1
	sub	%o1, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o4, %o2, %o4
	or	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE9:
	.size	LeftRotate__uLeftRotate_param_LC_I, .-LeftRotate__uLeftRotate_param_LC_I
	.align 4
	.type	LeftRotate__uLeftRotate_var_LC_C, #function
	.proc	05
LeftRotate__uLeftRotate_var_LC_C:
.LLFB10:
	save	%sp, -96, %sp
.LLCFI10:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 63
	ble	%icc, .LL28
	 nop
	mov	2049, %o0
	call	_m3_fault, 0
	 nop
.LL28:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ldd	[%g1+64], %o4
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+96], %g1
	and	%g1, 63, %g1
	sllx	%o4, 32, %o2
	or	%o5, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	mov	64, %o1
	sub	%o1, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o4, %o2, %o4
	or	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE10:
	.size	LeftRotate__uLeftRotate_var_LC_C, .-LeftRotate__uLeftRotate_var_LC_C
	.align 4
	.type	LeftRotate__uLeftRotate_param_LC_C, #function
	.proc	05
LeftRotate__uLeftRotate_param_LC_C:
.LLFB11:
	save	%sp, -104, %sp
.LLCFI11:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	cmp	%g1, 63
	ble	%icc, .LL30
	 nop
	mov	2081, %o0
	call	_m3_fault, 0
	 nop
.LL30:
	ld	[%fp+76], %g1
	and	%g1, 63, %g1
	ldd	[%fp-8], %o4
	sllx	%o4, 32, %o2
	or	%o5, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	mov	64, %o1
	sub	%o1, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o4, %o2, %o4
	or	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE11:
	.size	LeftRotate__uLeftRotate_param_LC_C, .-LeftRotate__uLeftRotate_param_LC_C
	.align 4
	.type	LeftRotate__uLeftRotate_var_i32_I, #function
	.proc	04
LeftRotate__uLeftRotate_var_i32_I:
.LLFB12:
	save	%sp, -96, %sp
.LLCFI12:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL32
	 nop
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 31
	ble	%icc, .LL33
	 nop
.LL32:
	mov	2113, %o0
	call	_m3_fault, 0
	 nop
.LL33:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+72], %g1
	sethi	%hi(MM_LeftRotate), %o5
	or	%o5, %lo(MM_LeftRotate), %o5
	ld	[%o5+80], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE12:
	.size	LeftRotate__uLeftRotate_var_i32_I, .-LeftRotate__uLeftRotate_var_i32_I
	.align 4
	.type	LeftRotate__uLeftRotate_param_i32_I, #function
	.proc	04
LeftRotate__uLeftRotate_param_i32_I:
.LLFB13:
	save	%sp, -96, %sp
.LLCFI13:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL35
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL36
	 nop
.LL35:
	mov	2145, %o0
	call	_m3_fault, 0
	 nop
.LL36:
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE13:
	.size	LeftRotate__uLeftRotate_param_i32_I, .-LeftRotate__uLeftRotate_param_i32_I
	.align 4
	.type	LeftRotate__uLeftRotate_var_i32_C, #function
	.proc	04
LeftRotate__uLeftRotate_var_i32_C:
.LLFB14:
	save	%sp, -96, %sp
.LLCFI14:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 31
	ble	%icc, .LL38
	 nop
	mov	2177, %o0
	call	_m3_fault, 0
	 nop
.LL38:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+72], %g1
	sethi	%hi(MM_LeftRotate), %o5
	or	%o5, %lo(MM_LeftRotate), %o5
	ld	[%o5+96], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE14:
	.size	LeftRotate__uLeftRotate_var_i32_C, .-LeftRotate__uLeftRotate_var_i32_C
	.align 4
	.type	LeftRotate__uLeftRotate_param_i32_C, #function
	.proc	04
LeftRotate__uLeftRotate_param_i32_C:
.LLFB15:
	save	%sp, -96, %sp
.LLCFI15:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL40
	 nop
	mov	2209, %o0
	call	_m3_fault, 0
	 nop
.LL40:
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE15:
	.size	LeftRotate__uLeftRotate_param_i32_C, .-LeftRotate__uLeftRotate_param_i32_C
	.align 4
	.type	LeftRotate__uLeftRotate_var_i16_I, #function
	.proc	04
LeftRotate__uLeftRotate_var_i16_I:
.LLFB16:
	save	%sp, -96, %sp
.LLCFI16:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL42
	 nop
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 31
	ble	%icc, .LL43
	 nop
.LL42:
	mov	2241, %o0
	call	_m3_fault, 0
	 nop
.LL43:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sethi	%hi(MM_LeftRotate), %o5
	or	%o5, %lo(MM_LeftRotate), %o5
	ld	[%o5+80], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE16:
	.size	LeftRotate__uLeftRotate_var_i16_I, .-LeftRotate__uLeftRotate_var_i16_I
	.align 4
	.type	LeftRotate__uLeftRotate_param_i16_I, #function
	.proc	04
LeftRotate__uLeftRotate_param_i16_I:
.LLFB17:
	save	%sp, -96, %sp
.LLCFI17:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL45
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL46
	 nop
.LL45:
	mov	2273, %o0
	call	_m3_fault, 0
	 nop
.LL46:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	ld	[%fp+72], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE17:
	.size	LeftRotate__uLeftRotate_param_i16_I, .-LeftRotate__uLeftRotate_param_i16_I
	.align 4
	.type	LeftRotate__uLeftRotate_var_i16_C, #function
	.proc	04
LeftRotate__uLeftRotate_var_i16_C:
.LLFB18:
	save	%sp, -96, %sp
.LLCFI18:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 31
	ble	%icc, .LL48
	 nop
	mov	2305, %o0
	call	_m3_fault, 0
	 nop
.LL48:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sethi	%hi(MM_LeftRotate), %o5
	or	%o5, %lo(MM_LeftRotate), %o5
	ld	[%o5+96], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE18:
	.size	LeftRotate__uLeftRotate_var_i16_C, .-LeftRotate__uLeftRotate_var_i16_C
	.align 4
	.type	LeftRotate__uLeftRotate_param_i16_C, #function
	.proc	04
LeftRotate__uLeftRotate_param_i16_C:
.LLFB19:
	save	%sp, -96, %sp
.LLCFI19:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL50
	 nop
	mov	2337, %o0
	call	_m3_fault, 0
	 nop
.LL50:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	ld	[%fp+72], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE19:
	.size	LeftRotate__uLeftRotate_param_i16_C, .-LeftRotate__uLeftRotate_param_i16_C
	.align 4
	.type	LeftRotate__uLeftRotate_var_I_I, #function
	.proc	04
LeftRotate__uLeftRotate_var_I_I:
.LLFB20:
	save	%sp, -96, %sp
.LLCFI20:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL52
	 nop
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 31
	ble	%icc, .LL53
	 nop
.LL52:
	mov	2369, %o0
	call	_m3_fault, 0
	 nop
.LL53:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+80], %g1
	sethi	%hi(MM_LeftRotate), %o5
	or	%o5, %lo(MM_LeftRotate), %o5
	ld	[%o5+80], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE20:
	.size	LeftRotate__uLeftRotate_var_I_I, .-LeftRotate__uLeftRotate_var_I_I
	.align 4
	.type	LeftRotate__uLeftRotate_param_I_I, #function
	.proc	04
LeftRotate__uLeftRotate_param_I_I:
.LLFB21:
	save	%sp, -96, %sp
.LLCFI21:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL55
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL56
	 nop
.LL55:
	mov	2401, %o0
	call	_m3_fault, 0
	 nop
.LL56:
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE21:
	.size	LeftRotate__uLeftRotate_param_I_I, .-LeftRotate__uLeftRotate_param_I_I
	.align 4
	.type	LeftRotate__uLeftRotate_var_I_C, #function
	.proc	04
LeftRotate__uLeftRotate_var_I_C:
.LLFB22:
	save	%sp, -96, %sp
.LLCFI22:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 31
	ble	%icc, .LL58
	 nop
	mov	2433, %o0
	call	_m3_fault, 0
	 nop
.LL58:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+80], %g1
	sethi	%hi(MM_LeftRotate), %o5
	or	%o5, %lo(MM_LeftRotate), %o5
	ld	[%o5+96], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE22:
	.size	LeftRotate__uLeftRotate_var_I_C, .-LeftRotate__uLeftRotate_var_I_C
	.align 4
	.type	LeftRotate__uLeftRotate_param_I_C, #function
	.proc	04
LeftRotate__uLeftRotate_param_I_C:
.LLFB23:
	save	%sp, -96, %sp
.LLCFI23:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL60
	 nop
	mov	2465, %o0
	call	_m3_fault, 0
	 nop
.LL60:
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE23:
	.size	LeftRotate__uLeftRotate_param_I_C, .-LeftRotate__uLeftRotate_param_I_C
	.align 4
	.type	LeftRotate__uLeftRotate_var_i64_I, #function
	.proc	05
LeftRotate__uLeftRotate_var_i64_I:
.LLFB24:
	save	%sp, -96, %sp
.LLCFI24:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL62
	 nop
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 63
	ble	%icc, .LL63
	 nop
.LL62:
	mov	2497, %o0
	call	_m3_fault, 0
	 nop
.LL63:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ldd	[%g1+88], %o4
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+80], %g1
	and	%g1, 63, %g1
	sllx	%o4, 32, %o2
	or	%o5, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	mov	64, %o1
	sub	%o1, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o4, %o2, %o4
	or	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE24:
	.size	LeftRotate__uLeftRotate_var_i64_I, .-LeftRotate__uLeftRotate_var_i64_I
	.align 4
	.type	LeftRotate__uLeftRotate_param_i64_I, #function
	.proc	05
LeftRotate__uLeftRotate_param_i64_I:
.LLFB25:
	save	%sp, -104, %sp
.LLCFI25:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	cmp	%g1, 0
	bl	%icc, .LL65
	 nop
	ld	[%fp+76], %g1
	cmp	%g1, 63
	ble	%icc, .LL66
	 nop
.LL65:
	mov	2529, %o0
	call	_m3_fault, 0
	 nop
.LL66:
	ldd	[%fp-8], %o4
	ld	[%fp+76], %g1
	and	%g1, 63, %g1
	sllx	%o4, 32, %o2
	or	%o5, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	mov	64, %o1
	sub	%o1, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o4, %o2, %o4
	or	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE25:
	.size	LeftRotate__uLeftRotate_param_i64_I, .-LeftRotate__uLeftRotate_param_i64_I
	.align 4
	.type	LeftRotate__uLeftRotate_var_i64_C, #function
	.proc	05
LeftRotate__uLeftRotate_var_i64_C:
.LLFB26:
	save	%sp, -96, %sp
.LLCFI26:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 63
	ble	%icc, .LL68
	 nop
	mov	2561, %o0
	call	_m3_fault, 0
	 nop
.LL68:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ldd	[%g1+88], %o4
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+96], %g1
	and	%g1, 63, %g1
	sllx	%o4, 32, %o2
	or	%o5, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	mov	64, %o1
	sub	%o1, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o4, %o2, %o4
	or	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE26:
	.size	LeftRotate__uLeftRotate_var_i64_C, .-LeftRotate__uLeftRotate_var_i64_C
	.align 4
	.type	LeftRotate__uLeftRotate_param_i64_C, #function
	.proc	05
LeftRotate__uLeftRotate_param_i64_C:
.LLFB27:
	save	%sp, -104, %sp
.LLCFI27:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	cmp	%g1, 63
	ble	%icc, .LL70
	 nop
	mov	2593, %o0
	call	_m3_fault, 0
	 nop
.LL70:
	ldd	[%fp-8], %o4
	ld	[%fp+76], %g1
	and	%g1, 63, %g1
	sllx	%o4, 32, %o2
	or	%o5, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	mov	64, %o1
	sub	%o1, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o4, %o2, %o4
	or	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE27:
	.size	LeftRotate__uLeftRotate_param_i64_C, .-LeftRotate__uLeftRotate_param_i64_C
	.align 4
	.type	LeftRotate__uLeftRotate_var_C_I, #function
	.proc	04
LeftRotate__uLeftRotate_var_C_I:
.LLFB28:
	save	%sp, -96, %sp
.LLCFI28:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL72
	 nop
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 31
	ble	%icc, .LL73
	 nop
.LL72:
	mov	2625, %o0
	call	_m3_fault, 0
	 nop
.LL73:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+96], %g1
	sethi	%hi(MM_LeftRotate), %o5
	or	%o5, %lo(MM_LeftRotate), %o5
	ld	[%o5+80], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE28:
	.size	LeftRotate__uLeftRotate_var_C_I, .-LeftRotate__uLeftRotate_var_C_I
	.align 4
	.type	LeftRotate__uLeftRotate_param_C_I, #function
	.proc	04
LeftRotate__uLeftRotate_param_C_I:
.LLFB29:
	save	%sp, -96, %sp
.LLCFI29:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL75
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL76
	 nop
.LL75:
	mov	2657, %o0
	call	_m3_fault, 0
	 nop
.LL76:
	ld	[%fp+72], %g1
	and	%g1, 31, %o5
	ld	[%fp+68], %g1
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE29:
	.size	LeftRotate__uLeftRotate_param_C_I, .-LeftRotate__uLeftRotate_param_C_I
	.align 4
	.type	LeftRotate__uLeftRotate_var_C_C, #function
	.proc	04
LeftRotate__uLeftRotate_var_C_C:
.LLFB30:
	save	%sp, -96, %sp
.LLCFI30:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 31
	ble	%icc, .LL78
	 nop
	mov	2689, %o0
	call	_m3_fault, 0
	 nop
.LL78:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+96], %g1
	sethi	%hi(MM_LeftRotate), %o5
	or	%o5, %lo(MM_LeftRotate), %o5
	ld	[%o5+96], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE30:
	.size	LeftRotate__uLeftRotate_var_C_C, .-LeftRotate__uLeftRotate_var_C_C
	.align 4
	.type	LeftRotate__uLeftRotate_param_C_C, #function
	.proc	04
LeftRotate__uLeftRotate_param_C_C:
.LLFB31:
	save	%sp, -96, %sp
.LLCFI31:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL80
	 nop
	mov	2721, %o0
	call	_m3_fault, 0
	 nop
.LL80:
	ld	[%fp+72], %g1
	and	%g1, 31, %o5
	ld	[%fp+68], %g1
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE31:
	.size	LeftRotate__uLeftRotate_param_C_C, .-LeftRotate__uLeftRotate_param_C_C
	.align 4
	.type	LeftRotate__uLeftRotate_var_u8_I, #function
	.proc	04
LeftRotate__uLeftRotate_var_u8_I:
.LLFB32:
	save	%sp, -96, %sp
.LLCFI32:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL82
	 nop
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 31
	ble	%icc, .LL83
	 nop
.LL82:
	mov	2753, %o0
	call	_m3_fault, 0
	 nop
.LL83:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	sethi	%hi(MM_LeftRotate), %o5
	or	%o5, %lo(MM_LeftRotate), %o5
	ld	[%o5+80], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE32:
	.size	LeftRotate__uLeftRotate_var_u8_I, .-LeftRotate__uLeftRotate_var_u8_I
	.align 4
	.type	LeftRotate__uLeftRotate_param_u8_I, #function
	.proc	04
LeftRotate__uLeftRotate_param_u8_I:
.LLFB33:
	save	%sp, -96, %sp
.LLCFI33:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL85
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL86
	 nop
.LL85:
	mov	2785, %o0
	call	_m3_fault, 0
	 nop
.LL86:
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	ld	[%fp+72], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE33:
	.size	LeftRotate__uLeftRotate_param_u8_I, .-LeftRotate__uLeftRotate_param_u8_I
	.align 4
	.type	LeftRotate__uLeftRotate_var_u8_C, #function
	.proc	04
LeftRotate__uLeftRotate_var_u8_C:
.LLFB34:
	save	%sp, -96, %sp
.LLCFI34:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 31
	ble	%icc, .LL88
	 nop
	mov	2817, %o0
	call	_m3_fault, 0
	 nop
.LL88:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	sethi	%hi(MM_LeftRotate), %o5
	or	%o5, %lo(MM_LeftRotate), %o5
	ld	[%o5+96], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE34:
	.size	LeftRotate__uLeftRotate_var_u8_C, .-LeftRotate__uLeftRotate_var_u8_C
	.align 4
	.type	LeftRotate__uLeftRotate_param_u8_C, #function
	.proc	04
LeftRotate__uLeftRotate_param_u8_C:
.LLFB35:
	save	%sp, -96, %sp
.LLCFI35:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL90
	 nop
	mov	2849, %o0
	call	_m3_fault, 0
	 nop
.LL90:
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	ld	[%fp+72], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE35:
	.size	LeftRotate__uLeftRotate_param_u8_C, .-LeftRotate__uLeftRotate_param_u8_C
	.align 4
	.type	LeftRotate__uLeftRotate_var_L_I, #function
	.proc	05
LeftRotate__uLeftRotate_var_L_I:
.LLFB36:
	save	%sp, -96, %sp
.LLCFI36:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL92
	 nop
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 63
	ble	%icc, .LL93
	 nop
.LL92:
	mov	2881, %o0
	call	_m3_fault, 0
	 nop
.LL93:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ldd	[%g1+120], %o4
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+80], %g1
	and	%g1, 63, %g1
	sllx	%o4, 32, %o2
	or	%o5, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	mov	64, %o1
	sub	%o1, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o4, %o2, %o4
	or	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE36:
	.size	LeftRotate__uLeftRotate_var_L_I, .-LeftRotate__uLeftRotate_var_L_I
	.align 4
	.type	LeftRotate__uLeftRotate_param_L_I, #function
	.proc	05
LeftRotate__uLeftRotate_param_L_I:
.LLFB37:
	save	%sp, -104, %sp
.LLCFI37:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	cmp	%g1, 0
	bl	%icc, .LL95
	 nop
	ld	[%fp+76], %g1
	cmp	%g1, 63
	ble	%icc, .LL96
	 nop
.LL95:
	mov	2913, %o0
	call	_m3_fault, 0
	 nop
.LL96:
	ldd	[%fp-8], %o4
	ld	[%fp+76], %g1
	and	%g1, 63, %g1
	sllx	%o4, 32, %o2
	or	%o5, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	mov	64, %o1
	sub	%o1, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o4, %o2, %o4
	or	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE37:
	.size	LeftRotate__uLeftRotate_param_L_I, .-LeftRotate__uLeftRotate_param_L_I
	.align 4
	.type	LeftRotate__uLeftRotate_var_L_C, #function
	.proc	05
LeftRotate__uLeftRotate_var_L_C:
.LLFB38:
	save	%sp, -96, %sp
.LLCFI38:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 63
	ble	%icc, .LL98
	 nop
	mov	2945, %o0
	call	_m3_fault, 0
	 nop
.LL98:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ldd	[%g1+120], %o4
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+96], %g1
	and	%g1, 63, %g1
	sllx	%o4, 32, %o2
	or	%o5, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	mov	64, %o1
	sub	%o1, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o4, %o2, %o4
	or	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE38:
	.size	LeftRotate__uLeftRotate_var_L_C, .-LeftRotate__uLeftRotate_var_L_C
	.align 4
	.type	LeftRotate__uLeftRotate_param_L_C, #function
	.proc	05
LeftRotate__uLeftRotate_param_L_C:
.LLFB39:
	save	%sp, -104, %sp
.LLCFI39:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	cmp	%g1, 63
	ble	%icc, .LL100
	 nop
	mov	2977, %o0
	call	_m3_fault, 0
	 nop
.LL100:
	ldd	[%fp-8], %o4
	ld	[%fp+76], %g1
	and	%g1, 63, %g1
	sllx	%o4, 32, %o2
	or	%o5, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	mov	64, %o1
	sub	%o1, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o4, %o2, %o4
	or	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE39:
	.size	LeftRotate__uLeftRotate_param_L_C, .-LeftRotate__uLeftRotate_param_L_C
	.align 4
	.type	LeftRotate__uLeftRotate_var_i8_I, #function
	.proc	04
LeftRotate__uLeftRotate_var_i8_I:
.LLFB40:
	save	%sp, -96, %sp
.LLCFI40:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL102
	 nop
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 31
	ble	%icc, .LL103
	 nop
.LL102:
	mov	3009, %o0
	call	_m3_fault, 0
	 nop
.LL103:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sethi	%hi(MM_LeftRotate), %o5
	or	%o5, %lo(MM_LeftRotate), %o5
	ld	[%o5+80], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE40:
	.size	LeftRotate__uLeftRotate_var_i8_I, .-LeftRotate__uLeftRotate_var_i8_I
	.align 4
	.type	LeftRotate__uLeftRotate_param_i8_I, #function
	.proc	04
LeftRotate__uLeftRotate_param_i8_I:
.LLFB41:
	save	%sp, -96, %sp
.LLCFI41:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL105
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL106
	 nop
.LL105:
	mov	3041, %o0
	call	_m3_fault, 0
	 nop
.LL106:
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	ld	[%fp+72], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE41:
	.size	LeftRotate__uLeftRotate_param_i8_I, .-LeftRotate__uLeftRotate_param_i8_I
	.align 4
	.type	LeftRotate__uLeftRotate_var_i8_C, #function
	.proc	04
LeftRotate__uLeftRotate_var_i8_C:
.LLFB42:
	save	%sp, -96, %sp
.LLCFI42:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 31
	ble	%icc, .LL108
	 nop
	mov	3073, %o0
	call	_m3_fault, 0
	 nop
.LL108:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sethi	%hi(MM_LeftRotate), %o5
	or	%o5, %lo(MM_LeftRotate), %o5
	ld	[%o5+96], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE42:
	.size	LeftRotate__uLeftRotate_var_i8_C, .-LeftRotate__uLeftRotate_var_i8_C
	.align 4
	.type	LeftRotate__uLeftRotate_param_i8_C, #function
	.proc	04
LeftRotate__uLeftRotate_param_i8_C:
.LLFB43:
	save	%sp, -96, %sp
.LLCFI43:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL110
	 nop
	mov	3105, %o0
	call	_m3_fault, 0
	 nop
.LL110:
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	ld	[%fp+72], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE43:
	.size	LeftRotate__uLeftRotate_param_i8_C, .-LeftRotate__uLeftRotate_param_i8_C
	.align 4
	.type	LeftRotate__uLeftRotate_var_u32_I, #function
	.proc	04
LeftRotate__uLeftRotate_var_u32_I:
.LLFB44:
	save	%sp, -96, %sp
.LLCFI44:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL112
	 nop
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 31
	ble	%icc, .LL113
	 nop
.LL112:
	mov	3137, %o0
	call	_m3_fault, 0
	 nop
.LL113:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+132], %g1
	sethi	%hi(MM_LeftRotate), %o5
	or	%o5, %lo(MM_LeftRotate), %o5
	ld	[%o5+80], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE44:
	.size	LeftRotate__uLeftRotate_var_u32_I, .-LeftRotate__uLeftRotate_var_u32_I
	.align 4
	.type	LeftRotate__uLeftRotate_param_u32_I, #function
	.proc	04
LeftRotate__uLeftRotate_param_u32_I:
.LLFB45:
	save	%sp, -96, %sp
.LLCFI45:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL115
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL116
	 nop
.LL115:
	mov	3169, %o0
	call	_m3_fault, 0
	 nop
.LL116:
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE45:
	.size	LeftRotate__uLeftRotate_param_u32_I, .-LeftRotate__uLeftRotate_param_u32_I
	.align 4
	.type	LeftRotate__uLeftRotate_var_u32_C, #function
	.proc	04
LeftRotate__uLeftRotate_var_u32_C:
.LLFB46:
	save	%sp, -96, %sp
.LLCFI46:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 31
	ble	%icc, .LL118
	 nop
	mov	3201, %o0
	call	_m3_fault, 0
	 nop
.LL118:
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	ld	[%g1+132], %g1
	sethi	%hi(MM_LeftRotate), %o5
	or	%o5, %lo(MM_LeftRotate), %o5
	ld	[%o5+96], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE46:
	.size	LeftRotate__uLeftRotate_var_u32_C, .-LeftRotate__uLeftRotate_var_u32_C
	.align 4
	.type	LeftRotate__uLeftRotate_param_u32_C, #function
	.proc	04
LeftRotate__uLeftRotate_param_u32_C:
.LLFB47:
	save	%sp, -96, %sp
.LLCFI47:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL120
	 nop
	mov	3233, %o0
	call	_m3_fault, 0
	 nop
.LL120:
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE47:
	.size	LeftRotate__uLeftRotate_param_u32_C, .-LeftRotate__uLeftRotate_param_u32_C
	.align 4
	.global LeftRotate_M3
	.type	LeftRotate_M3, #function
	.proc	0120
LeftRotate_M3:
.LLFB48:
	save	%sp, -96, %sp
.LLCFI48:
	st	%i0, [%fp+68]
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE48:
	.size	LeftRotate_M3, .-LeftRotate_M3
	.align 4
	.type	_m3_fault, #function
	.proc	020
_m3_fault:
.LLFB49:
	save	%sp, -96, %sp
.LLCFI49:
	st	%i0, [%fp+68]
	sethi	%hi(MM_LeftRotate), %g1
	or	%g1, %lo(MM_LeftRotate), %o0
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
	.ascii	"LeftRotate_M3"
	.skip 1
	.ascii	"uLeftRotate_param_u32_C"
	.skip 1
	.ascii	"uLeftRotate_var_u32_C"
	.skip 1
	.ascii	"uLeftRotate_param_u32_I"
	.skip 1
	.ascii	"uLeftRotate_var_u32_I"
	.skip 1
	.ascii	"uLeftRotate_param_i8_C"
	.skip 1
	.ascii	"uLeftRotate_var_i8_C"
	.skip 1
	.ascii	"uLeftRotate_param_i8_I"
	.skip 1
	.ascii	"uLeftRotate_var_i8_I"
	.skip 1
	.ascii	"uLeftRotate_param_L_C"
	.skip 1
	.ascii	"uLeftRotate_var_L_C"
	.skip 1
	.ascii	"uLeftRotate_param_L_I"
	.skip 1
	.ascii	"uLeftRotate_var_L_I"
	.skip 1
	.ascii	"uLeftRotate_param_u8_C"
	.skip 1
	.ascii	"uLeftRotate_var_u8_C"
	.skip 1
	.ascii	"uLeftRotate_param_u8_I"
	.skip 1
	.ascii	"uLeftRotate_var_u8_I"
	.skip 1
	.ascii	"uLeftRotate_param_C_C"
	.skip 1
	.ascii	"uLeftRotate_var_C_C"
	.skip 1
	.ascii	"uLeftRotate_param_C_I"
	.skip 1
	.ascii	"uLeftRotate_var_C_I"
	.skip 1
	.ascii	"uLeftRotate_param_i64_C"
	.skip 1
	.ascii	"uLeftRotate_var_i64_C"
	.skip 1
	.ascii	"uLeftRotate_param_i64_I"
	.skip 1
	.ascii	"uLeftRotate_var_i64_I"
	.skip 1
	.ascii	"uLeftRotate_param_I_C"
	.skip 1
	.ascii	"uLeftRotate_var_I_C"
	.skip 1
	.ascii	"uLeftRotate_param_I_I"
	.skip 1
	.ascii	"uLeftRotate_var_I_I"
	.skip 1
	.ascii	"uLeftRotate_param_i16_C"
	.skip 1
	.ascii	"uLeftRotate_var_i16_C"
	.skip 1
	.ascii	"uLeftRotate_param_i16_I"
	.skip 1
	.ascii	"uLeftRotate_var_i16_I"
	.skip 1
	.ascii	"uLeftRotate_param_i32_C"
	.skip 1
	.ascii	"uLeftRotate_var_i32_C"
	.skip 1
	.ascii	"uLeftRotate_param_i32_I"
	.skip 1
	.ascii	"uLeftRotate_var_i32_I"
	.skip 1
	.ascii	"uLeftRotate_param_LC_C"
	.skip 1
	.ascii	"uLeftRotate_var_LC_C"
	.skip 1
	.ascii	"uLeftRotate_param_LC_I"
	.skip 1
	.ascii	"uLeftRotate_var_LC_I"
	.skip 1
	.ascii	"uLeftRotate_param_u64_C"
	.skip 1
	.ascii	"uLeftRotate_var_u64_C"
	.skip 1
	.ascii	"uLeftRotate_param_u64_I"
	.skip 1
	.ascii	"uLeftRotate_var_u64_I"
	.skip 1
	.ascii	"uLeftRotate_param_u16_C"
	.skip 1
	.ascii	"uLeftRotate_var_u16_C"
	.skip 1
	.ascii	"uLeftRotate_param_u16_I"
	.skip 1
	.ascii	"uLeftRotate_var_u16_I"
	.skip 3
	.long	LeftRotate_M3
	.long	L_1
	.long	LeftRotate__uLeftRotate_param_u32_C
	.long	L_1+14
	.long	LeftRotate__uLeftRotate_var_u32_C
	.long	L_1+38
	.long	LeftRotate__uLeftRotate_param_u32_I
	.long	L_1+60
	.long	LeftRotate__uLeftRotate_var_u32_I
	.long	L_1+84
	.long	LeftRotate__uLeftRotate_param_i8_C
	.long	L_1+106
	.long	LeftRotate__uLeftRotate_var_i8_C
	.long	L_1+129
	.long	LeftRotate__uLeftRotate_param_i8_I
	.long	L_1+150
	.long	LeftRotate__uLeftRotate_var_i8_I
	.long	L_1+173
	.long	LeftRotate__uLeftRotate_param_L_C
	.long	L_1+194
	.long	LeftRotate__uLeftRotate_var_L_C
	.long	L_1+216
	.long	LeftRotate__uLeftRotate_param_L_I
	.long	L_1+236
	.long	LeftRotate__uLeftRotate_var_L_I
	.long	L_1+258
	.long	LeftRotate__uLeftRotate_param_u8_C
	.long	L_1+278
	.long	LeftRotate__uLeftRotate_var_u8_C
	.long	L_1+301
	.long	LeftRotate__uLeftRotate_param_u8_I
	.long	L_1+322
	.long	LeftRotate__uLeftRotate_var_u8_I
	.long	L_1+345
	.long	LeftRotate__uLeftRotate_param_C_C
	.long	L_1+366
	.long	LeftRotate__uLeftRotate_var_C_C
	.long	L_1+388
	.long	LeftRotate__uLeftRotate_param_C_I
	.long	L_1+408
	.long	LeftRotate__uLeftRotate_var_C_I
	.long	L_1+430
	.long	LeftRotate__uLeftRotate_param_i64_C
	.long	L_1+450
	.long	LeftRotate__uLeftRotate_var_i64_C
	.long	L_1+474
	.long	LeftRotate__uLeftRotate_param_i64_I
	.long	L_1+496
	.long	LeftRotate__uLeftRotate_var_i64_I
	.long	L_1+520
	.long	LeftRotate__uLeftRotate_param_I_C
	.long	L_1+542
	.long	LeftRotate__uLeftRotate_var_I_C
	.long	L_1+564
	.long	LeftRotate__uLeftRotate_param_I_I
	.long	L_1+584
	.long	LeftRotate__uLeftRotate_var_I_I
	.long	L_1+606
	.long	LeftRotate__uLeftRotate_param_i16_C
	.long	L_1+626
	.long	LeftRotate__uLeftRotate_var_i16_C
	.long	L_1+650
	.long	LeftRotate__uLeftRotate_param_i16_I
	.long	L_1+672
	.long	LeftRotate__uLeftRotate_var_i16_I
	.long	L_1+696
	.long	LeftRotate__uLeftRotate_param_i32_C
	.long	L_1+718
	.long	LeftRotate__uLeftRotate_var_i32_C
	.long	L_1+742
	.long	LeftRotate__uLeftRotate_param_i32_I
	.long	L_1+764
	.long	LeftRotate__uLeftRotate_var_i32_I
	.long	L_1+788
	.long	LeftRotate__uLeftRotate_param_LC_C
	.long	L_1+810
	.long	LeftRotate__uLeftRotate_var_LC_C
	.long	L_1+833
	.long	LeftRotate__uLeftRotate_param_LC_I
	.long	L_1+854
	.long	LeftRotate__uLeftRotate_var_LC_I
	.long	L_1+877
	.long	LeftRotate__uLeftRotate_param_u64_C
	.long	L_1+898
	.long	LeftRotate__uLeftRotate_var_u64_C
	.long	L_1+922
	.long	LeftRotate__uLeftRotate_param_u64_I
	.long	L_1+944
	.long	LeftRotate__uLeftRotate_var_u64_I
	.long	L_1+968
	.long	LeftRotate__uLeftRotate_param_u16_C
	.long	L_1+990
	.long	LeftRotate__uLeftRotate_var_u16_C
	.long	L_1+1014
	.long	LeftRotate__uLeftRotate_param_u16_I
	.long	L_1+1036
	.long	LeftRotate__uLeftRotate_var_u16_I
	.long	L_1+1060
	.skip 4
	.ascii	"../SOLsun/LeftRotate.m3"
	.skip 1
	.align 8
	.type	MM_LeftRotate, #object
	.size	MM_LeftRotate, 208
MM_LeftRotate:
	.long	L_1+1480
	.skip 16
	.long	L_1+1084
	.skip 12
	.long	MM_LeftRotate+148
	.skip 4
	.long	LeftRotate_M3
	.long	3
	.half	721
	.skip 2
	.long	0
	.long	722
	.long	0
	.long	723
	.long	724
	.half	725
	.skip 2
	.long	726
	.skip 4
	.long	0
	.long	727
	.long	728
	.skip 4
	.long	1082576343
	.long	171798692
	.byte	-37
	.skip 7
	.long	0
	.long	732
	.byte	-35
	.skip 3
	.long	734
	.long	1144516379
	.skip 12
	.long	LeftRotate_I3
	.long	MM_LeftRotate+160
	.skip 4
	.long	Long_I3
	.long	MM_LeftRotate+172
	.skip 4
	.long	Word_I3
	.long	MM_LeftRotate+184
	.skip 4
	.long	Cstdint_I3
	.long	MM_LeftRotate+196
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
