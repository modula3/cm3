	.file	"Rotate.mc"
	.section	".text"
	.align 4
	.type	Rotate__uRotate_var_u16_I, #function
	.proc	04
Rotate__uRotate_var_u16_I:
.LLFB0:
	save	%sp, -96, %sp
.LLCFI0:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL2
	 nop
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sethi	%hi(MM_Rotate), %o5
	or	%o5, %lo(MM_Rotate), %o5
	ld	[%o5+80], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	ba,pt	%xcc, .LL3
	 nop
.LL2:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sethi	%hi(MM_Rotate), %o5
	or	%o5, %lo(MM_Rotate), %o5
	ld	[%o5+80], %o5
	sub	%g0, %o5, %o5
	and	%o5, 31, %o5
	srl	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	sll	%g1, %o5, %g1
	or	%g1, %o4, %g1
.LL3:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE0:
	.size	Rotate__uRotate_var_u16_I, .-Rotate__uRotate_var_u16_I
	.align 4
	.type	Rotate__uRotate_param_u16_I, #function
	.proc	04
Rotate__uRotate_param_u16_I:
.LLFB1:
	save	%sp, -96, %sp
.LLCFI1:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL5
	 nop
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
	ba,pt	%xcc, .LL6
	 nop
.LL5:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	ld	[%fp+72], %o5
	sub	%g0, %o5, %o5
	and	%o5, 31, %o5
	srl	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	sll	%g1, %o5, %g1
	or	%g1, %o4, %g1
.LL6:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1:
	.size	Rotate__uRotate_param_u16_I, .-Rotate__uRotate_param_u16_I
	.align 4
	.type	Rotate__uRotate_var_u16_C, #function
	.proc	04
Rotate__uRotate_var_u16_C:
.LLFB2:
	save	%sp, -96, %sp
.LLCFI2:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 0
	bl	%icc, .LL8
	 nop
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sethi	%hi(MM_Rotate), %o5
	or	%o5, %lo(MM_Rotate), %o5
	ld	[%o5+96], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	ba,pt	%xcc, .LL9
	 nop
.LL8:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sethi	%hi(MM_Rotate), %o5
	or	%o5, %lo(MM_Rotate), %o5
	ld	[%o5+96], %o5
	sub	%g0, %o5, %o5
	and	%o5, 31, %o5
	srl	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	sll	%g1, %o5, %g1
	or	%g1, %o4, %g1
.LL9:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE2:
	.size	Rotate__uRotate_var_u16_C, .-Rotate__uRotate_var_u16_C
	.align 4
	.type	Rotate__uRotate_param_u16_C, #function
	.proc	04
Rotate__uRotate_param_u16_C:
.LLFB3:
	save	%sp, -96, %sp
.LLCFI3:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL11
	 nop
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
	ba,pt	%xcc, .LL12
	 nop
.LL11:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	ld	[%fp+72], %o5
	sub	%g0, %o5, %o5
	and	%o5, 31, %o5
	srl	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	sll	%g1, %o5, %g1
	or	%g1, %o4, %g1
.LL12:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE3:
	.size	Rotate__uRotate_param_u16_C, .-Rotate__uRotate_param_u16_C
	.align 4
	.type	Rotate__uRotate_var_u64_I, #function
	.proc	05
Rotate__uRotate_var_u64_I:
.LLFB4:
	save	%sp, -96, %sp
.LLCFI4:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL14
	 nop
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ldd	[%g1+56], %o4
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
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
	ba,pt	%xcc, .LL15
	 nop
.LL14:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ldd	[%g1+56], %o4
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+80], %g1
	sub	%g0, %g1, %g1
	and	%g1, 63, %g1
	sllx	%o4, 32, %o2
	or	%o5, %o2, %o2
	srlx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	mov	64, %o1
	sub	%o1, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o4, %o2, %o4
	or	%o5, %o3, %o5
.LL15:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE4:
	.size	Rotate__uRotate_var_u64_I, .-Rotate__uRotate_var_u64_I
	.align 4
	.type	Rotate__uRotate_param_u64_I, #function
	.proc	05
Rotate__uRotate_param_u64_I:
.LLFB5:
	save	%sp, -104, %sp
.LLCFI5:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	cmp	%g1, 0
	bl	%icc, .LL17
	 nop
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
	ba,pt	%xcc, .LL18
	 nop
.LL17:
	ldd	[%fp-8], %o4
	ld	[%fp+76], %g1
	sub	%g0, %g1, %g1
	and	%g1, 63, %g1
	sllx	%o4, 32, %o2
	or	%o5, %o2, %o2
	srlx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	mov	64, %o1
	sub	%o1, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o4, %o2, %o4
	or	%o5, %o3, %o5
.LL18:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE5:
	.size	Rotate__uRotate_param_u64_I, .-Rotate__uRotate_param_u64_I
	.align 4
	.type	Rotate__uRotate_var_u64_C, #function
	.proc	05
Rotate__uRotate_var_u64_C:
.LLFB6:
	save	%sp, -96, %sp
.LLCFI6:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 0
	bl	%icc, .LL20
	 nop
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ldd	[%g1+56], %o4
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
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
	ba,pt	%xcc, .LL21
	 nop
.LL20:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ldd	[%g1+56], %o4
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+96], %g1
	sub	%g0, %g1, %g1
	and	%g1, 63, %g1
	sllx	%o4, 32, %o2
	or	%o5, %o2, %o2
	srlx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	mov	64, %o1
	sub	%o1, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o4, %o2, %o4
	or	%o5, %o3, %o5
.LL21:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE6:
	.size	Rotate__uRotate_var_u64_C, .-Rotate__uRotate_var_u64_C
	.align 4
	.type	Rotate__uRotate_param_u64_C, #function
	.proc	05
Rotate__uRotate_param_u64_C:
.LLFB7:
	save	%sp, -104, %sp
.LLCFI7:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	cmp	%g1, 0
	bl	%icc, .LL23
	 nop
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
	ba,pt	%xcc, .LL24
	 nop
.LL23:
	ldd	[%fp-8], %o4
	ld	[%fp+76], %g1
	sub	%g0, %g1, %g1
	and	%g1, 63, %g1
	sllx	%o4, 32, %o2
	or	%o5, %o2, %o2
	srlx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	mov	64, %o1
	sub	%o1, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o4, %o2, %o4
	or	%o5, %o3, %o5
.LL24:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE7:
	.size	Rotate__uRotate_param_u64_C, .-Rotate__uRotate_param_u64_C
	.align 4
	.type	Rotate__uRotate_var_LC_I, #function
	.proc	05
Rotate__uRotate_var_LC_I:
.LLFB8:
	save	%sp, -96, %sp
.LLCFI8:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL26
	 nop
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ldd	[%g1+64], %o4
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
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
	ba,pt	%xcc, .LL27
	 nop
.LL26:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ldd	[%g1+64], %o4
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+80], %g1
	sub	%g0, %g1, %g1
	and	%g1, 63, %g1
	sllx	%o4, 32, %o2
	or	%o5, %o2, %o2
	srlx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	mov	64, %o1
	sub	%o1, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o4, %o2, %o4
	or	%o5, %o3, %o5
.LL27:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE8:
	.size	Rotate__uRotate_var_LC_I, .-Rotate__uRotate_var_LC_I
	.align 4
	.type	Rotate__uRotate_param_LC_I, #function
	.proc	05
Rotate__uRotate_param_LC_I:
.LLFB9:
	save	%sp, -104, %sp
.LLCFI9:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	cmp	%g1, 0
	bl	%icc, .LL29
	 nop
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
	ba,pt	%xcc, .LL30
	 nop
.LL29:
	ld	[%fp+76], %g1
	sub	%g0, %g1, %g1
	and	%g1, 63, %g1
	ldd	[%fp-8], %o4
	sllx	%o4, 32, %o2
	or	%o5, %o2, %o2
	srlx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	mov	64, %o1
	sub	%o1, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o4, %o2, %o4
	or	%o5, %o3, %o5
.LL30:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE9:
	.size	Rotate__uRotate_param_LC_I, .-Rotate__uRotate_param_LC_I
	.align 4
	.type	Rotate__uRotate_var_LC_C, #function
	.proc	05
Rotate__uRotate_var_LC_C:
.LLFB10:
	save	%sp, -96, %sp
.LLCFI10:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 0
	bl	%icc, .LL32
	 nop
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ldd	[%g1+64], %o4
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
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
	ba,pt	%xcc, .LL33
	 nop
.LL32:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ldd	[%g1+64], %o4
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+96], %g1
	sub	%g0, %g1, %g1
	and	%g1, 63, %g1
	sllx	%o4, 32, %o2
	or	%o5, %o2, %o2
	srlx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	mov	64, %o1
	sub	%o1, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o4, %o2, %o4
	or	%o5, %o3, %o5
.LL33:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE10:
	.size	Rotate__uRotate_var_LC_C, .-Rotate__uRotate_var_LC_C
	.align 4
	.type	Rotate__uRotate_param_LC_C, #function
	.proc	05
Rotate__uRotate_param_LC_C:
.LLFB11:
	save	%sp, -104, %sp
.LLCFI11:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	cmp	%g1, 0
	bl	%icc, .LL35
	 nop
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
	ba,pt	%xcc, .LL36
	 nop
.LL35:
	ld	[%fp+76], %g1
	sub	%g0, %g1, %g1
	and	%g1, 63, %g1
	ldd	[%fp-8], %o4
	sllx	%o4, 32, %o2
	or	%o5, %o2, %o2
	srlx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	mov	64, %o1
	sub	%o1, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o4, %o2, %o4
	or	%o5, %o3, %o5
.LL36:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE11:
	.size	Rotate__uRotate_param_LC_C, .-Rotate__uRotate_param_LC_C
	.align 4
	.type	Rotate__uRotate_var_i32_I, #function
	.proc	04
Rotate__uRotate_var_i32_I:
.LLFB12:
	save	%sp, -96, %sp
.LLCFI12:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL38
	 nop
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+72], %g1
	sethi	%hi(MM_Rotate), %o5
	or	%o5, %lo(MM_Rotate), %o5
	ld	[%o5+80], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	ba,pt	%xcc, .LL39
	 nop
.LL38:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+72], %g1
	sethi	%hi(MM_Rotate), %o5
	or	%o5, %lo(MM_Rotate), %o5
	ld	[%o5+80], %o5
	sub	%g0, %o5, %o5
	and	%o5, 31, %o5
	srl	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	sll	%g1, %o5, %g1
	or	%g1, %o4, %g1
.LL39:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE12:
	.size	Rotate__uRotate_var_i32_I, .-Rotate__uRotate_var_i32_I
	.align 4
	.type	Rotate__uRotate_param_i32_I, #function
	.proc	04
Rotate__uRotate_param_i32_I:
.LLFB13:
	save	%sp, -96, %sp
.LLCFI13:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL41
	 nop
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	ba,pt	%xcc, .LL42
	 nop
.LL41:
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	sub	%g0, %o5, %o5
	and	%o5, 31, %o5
	srl	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	sll	%g1, %o5, %g1
	or	%g1, %o4, %g1
.LL42:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE13:
	.size	Rotate__uRotate_param_i32_I, .-Rotate__uRotate_param_i32_I
	.align 4
	.type	Rotate__uRotate_var_i32_C, #function
	.proc	04
Rotate__uRotate_var_i32_C:
.LLFB14:
	save	%sp, -96, %sp
.LLCFI14:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 0
	bl	%icc, .LL44
	 nop
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+72], %g1
	sethi	%hi(MM_Rotate), %o5
	or	%o5, %lo(MM_Rotate), %o5
	ld	[%o5+96], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	ba,pt	%xcc, .LL45
	 nop
.LL44:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+72], %g1
	sethi	%hi(MM_Rotate), %o5
	or	%o5, %lo(MM_Rotate), %o5
	ld	[%o5+96], %o5
	sub	%g0, %o5, %o5
	and	%o5, 31, %o5
	srl	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	sll	%g1, %o5, %g1
	or	%g1, %o4, %g1
.LL45:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE14:
	.size	Rotate__uRotate_var_i32_C, .-Rotate__uRotate_var_i32_C
	.align 4
	.type	Rotate__uRotate_param_i32_C, #function
	.proc	04
Rotate__uRotate_param_i32_C:
.LLFB15:
	save	%sp, -96, %sp
.LLCFI15:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL47
	 nop
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	ba,pt	%xcc, .LL48
	 nop
.LL47:
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	sub	%g0, %o5, %o5
	and	%o5, 31, %o5
	srl	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	sll	%g1, %o5, %g1
	or	%g1, %o4, %g1
.LL48:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE15:
	.size	Rotate__uRotate_param_i32_C, .-Rotate__uRotate_param_i32_C
	.align 4
	.type	Rotate__uRotate_var_i16_I, #function
	.proc	04
Rotate__uRotate_var_i16_I:
.LLFB16:
	save	%sp, -96, %sp
.LLCFI16:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL50
	 nop
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sethi	%hi(MM_Rotate), %o5
	or	%o5, %lo(MM_Rotate), %o5
	ld	[%o5+80], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	ba,pt	%xcc, .LL51
	 nop
.LL50:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sethi	%hi(MM_Rotate), %o5
	or	%o5, %lo(MM_Rotate), %o5
	ld	[%o5+80], %o5
	sub	%g0, %o5, %o5
	and	%o5, 31, %o5
	srl	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	sll	%g1, %o5, %g1
	or	%g1, %o4, %g1
.LL51:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE16:
	.size	Rotate__uRotate_var_i16_I, .-Rotate__uRotate_var_i16_I
	.align 4
	.type	Rotate__uRotate_param_i16_I, #function
	.proc	04
Rotate__uRotate_param_i16_I:
.LLFB17:
	save	%sp, -96, %sp
.LLCFI17:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL53
	 nop
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
	ba,pt	%xcc, .LL54
	 nop
.LL53:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	ld	[%fp+72], %o5
	sub	%g0, %o5, %o5
	and	%o5, 31, %o5
	srl	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	sll	%g1, %o5, %g1
	or	%g1, %o4, %g1
.LL54:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE17:
	.size	Rotate__uRotate_param_i16_I, .-Rotate__uRotate_param_i16_I
	.align 4
	.type	Rotate__uRotate_var_i16_C, #function
	.proc	04
Rotate__uRotate_var_i16_C:
.LLFB18:
	save	%sp, -96, %sp
.LLCFI18:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 0
	bl	%icc, .LL56
	 nop
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sethi	%hi(MM_Rotate), %o5
	or	%o5, %lo(MM_Rotate), %o5
	ld	[%o5+96], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	ba,pt	%xcc, .LL57
	 nop
.LL56:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sethi	%hi(MM_Rotate), %o5
	or	%o5, %lo(MM_Rotate), %o5
	ld	[%o5+96], %o5
	sub	%g0, %o5, %o5
	and	%o5, 31, %o5
	srl	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	sll	%g1, %o5, %g1
	or	%g1, %o4, %g1
.LL57:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE18:
	.size	Rotate__uRotate_var_i16_C, .-Rotate__uRotate_var_i16_C
	.align 4
	.type	Rotate__uRotate_param_i16_C, #function
	.proc	04
Rotate__uRotate_param_i16_C:
.LLFB19:
	save	%sp, -96, %sp
.LLCFI19:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL59
	 nop
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
	ba,pt	%xcc, .LL60
	 nop
.LL59:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	ld	[%fp+72], %o5
	sub	%g0, %o5, %o5
	and	%o5, 31, %o5
	srl	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	sll	%g1, %o5, %g1
	or	%g1, %o4, %g1
.LL60:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE19:
	.size	Rotate__uRotate_param_i16_C, .-Rotate__uRotate_param_i16_C
	.align 4
	.type	Rotate__uRotate_var_I_I, #function
	.proc	04
Rotate__uRotate_var_I_I:
.LLFB20:
	save	%sp, -96, %sp
.LLCFI20:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL62
	 nop
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+80], %g1
	sethi	%hi(MM_Rotate), %o5
	or	%o5, %lo(MM_Rotate), %o5
	ld	[%o5+80], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	ba,pt	%xcc, .LL63
	 nop
.LL62:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+80], %g1
	sethi	%hi(MM_Rotate), %o5
	or	%o5, %lo(MM_Rotate), %o5
	ld	[%o5+80], %o5
	sub	%g0, %o5, %o5
	and	%o5, 31, %o5
	srl	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	sll	%g1, %o5, %g1
	or	%g1, %o4, %g1
.LL63:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE20:
	.size	Rotate__uRotate_var_I_I, .-Rotate__uRotate_var_I_I
	.align 4
	.type	Rotate__uRotate_param_I_I, #function
	.proc	04
Rotate__uRotate_param_I_I:
.LLFB21:
	save	%sp, -96, %sp
.LLCFI21:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL65
	 nop
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	ba,pt	%xcc, .LL66
	 nop
.LL65:
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	sub	%g0, %o5, %o5
	and	%o5, 31, %o5
	srl	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	sll	%g1, %o5, %g1
	or	%g1, %o4, %g1
.LL66:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE21:
	.size	Rotate__uRotate_param_I_I, .-Rotate__uRotate_param_I_I
	.align 4
	.type	Rotate__uRotate_var_I_C, #function
	.proc	04
Rotate__uRotate_var_I_C:
.LLFB22:
	save	%sp, -96, %sp
.LLCFI22:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 0
	bl	%icc, .LL68
	 nop
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+80], %g1
	sethi	%hi(MM_Rotate), %o5
	or	%o5, %lo(MM_Rotate), %o5
	ld	[%o5+96], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	ba,pt	%xcc, .LL69
	 nop
.LL68:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+80], %g1
	sethi	%hi(MM_Rotate), %o5
	or	%o5, %lo(MM_Rotate), %o5
	ld	[%o5+96], %o5
	sub	%g0, %o5, %o5
	and	%o5, 31, %o5
	srl	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	sll	%g1, %o5, %g1
	or	%g1, %o4, %g1
.LL69:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE22:
	.size	Rotate__uRotate_var_I_C, .-Rotate__uRotate_var_I_C
	.align 4
	.type	Rotate__uRotate_param_I_C, #function
	.proc	04
Rotate__uRotate_param_I_C:
.LLFB23:
	save	%sp, -96, %sp
.LLCFI23:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL71
	 nop
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	ba,pt	%xcc, .LL72
	 nop
.LL71:
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	sub	%g0, %o5, %o5
	and	%o5, 31, %o5
	srl	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	sll	%g1, %o5, %g1
	or	%g1, %o4, %g1
.LL72:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE23:
	.size	Rotate__uRotate_param_I_C, .-Rotate__uRotate_param_I_C
	.align 4
	.type	Rotate__uRotate_var_i64_I, #function
	.proc	05
Rotate__uRotate_var_i64_I:
.LLFB24:
	save	%sp, -96, %sp
.LLCFI24:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL74
	 nop
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ldd	[%g1+88], %o4
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
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
	ba,pt	%xcc, .LL75
	 nop
.LL74:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ldd	[%g1+88], %o4
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+80], %g1
	sub	%g0, %g1, %g1
	and	%g1, 63, %g1
	sllx	%o4, 32, %o2
	or	%o5, %o2, %o2
	srlx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	mov	64, %o1
	sub	%o1, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o4, %o2, %o4
	or	%o5, %o3, %o5
.LL75:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE24:
	.size	Rotate__uRotate_var_i64_I, .-Rotate__uRotate_var_i64_I
	.align 4
	.type	Rotate__uRotate_param_i64_I, #function
	.proc	05
Rotate__uRotate_param_i64_I:
.LLFB25:
	save	%sp, -104, %sp
.LLCFI25:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	cmp	%g1, 0
	bl	%icc, .LL77
	 nop
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
	ba,pt	%xcc, .LL78
	 nop
.LL77:
	ldd	[%fp-8], %o4
	ld	[%fp+76], %g1
	sub	%g0, %g1, %g1
	and	%g1, 63, %g1
	sllx	%o4, 32, %o2
	or	%o5, %o2, %o2
	srlx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	mov	64, %o1
	sub	%o1, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o4, %o2, %o4
	or	%o5, %o3, %o5
.LL78:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE25:
	.size	Rotate__uRotate_param_i64_I, .-Rotate__uRotate_param_i64_I
	.align 4
	.type	Rotate__uRotate_var_i64_C, #function
	.proc	05
Rotate__uRotate_var_i64_C:
.LLFB26:
	save	%sp, -96, %sp
.LLCFI26:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 0
	bl	%icc, .LL80
	 nop
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ldd	[%g1+88], %o4
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
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
	ba,pt	%xcc, .LL81
	 nop
.LL80:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ldd	[%g1+88], %o4
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+96], %g1
	sub	%g0, %g1, %g1
	and	%g1, 63, %g1
	sllx	%o4, 32, %o2
	or	%o5, %o2, %o2
	srlx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	mov	64, %o1
	sub	%o1, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o4, %o2, %o4
	or	%o5, %o3, %o5
.LL81:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE26:
	.size	Rotate__uRotate_var_i64_C, .-Rotate__uRotate_var_i64_C
	.align 4
	.type	Rotate__uRotate_param_i64_C, #function
	.proc	05
Rotate__uRotate_param_i64_C:
.LLFB27:
	save	%sp, -104, %sp
.LLCFI27:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	cmp	%g1, 0
	bl	%icc, .LL83
	 nop
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
	ba,pt	%xcc, .LL84
	 nop
.LL83:
	ldd	[%fp-8], %o4
	ld	[%fp+76], %g1
	sub	%g0, %g1, %g1
	and	%g1, 63, %g1
	sllx	%o4, 32, %o2
	or	%o5, %o2, %o2
	srlx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	mov	64, %o1
	sub	%o1, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o4, %o2, %o4
	or	%o5, %o3, %o5
.LL84:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE27:
	.size	Rotate__uRotate_param_i64_C, .-Rotate__uRotate_param_i64_C
	.align 4
	.type	Rotate__uRotate_var_C_I, #function
	.proc	04
Rotate__uRotate_var_C_I:
.LLFB28:
	save	%sp, -96, %sp
.LLCFI28:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL86
	 nop
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+96], %g1
	sethi	%hi(MM_Rotate), %o5
	or	%o5, %lo(MM_Rotate), %o5
	ld	[%o5+80], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	ba,pt	%xcc, .LL87
	 nop
.LL86:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+96], %g1
	sethi	%hi(MM_Rotate), %o5
	or	%o5, %lo(MM_Rotate), %o5
	ld	[%o5+80], %o5
	sub	%g0, %o5, %o5
	and	%o5, 31, %o5
	srl	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	sll	%g1, %o5, %g1
	or	%g1, %o4, %g1
.LL87:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE28:
	.size	Rotate__uRotate_var_C_I, .-Rotate__uRotate_var_C_I
	.align 4
	.type	Rotate__uRotate_param_C_I, #function
	.proc	04
Rotate__uRotate_param_C_I:
.LLFB29:
	save	%sp, -96, %sp
.LLCFI29:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL89
	 nop
	ld	[%fp+72], %g1
	and	%g1, 31, %o5
	ld	[%fp+68], %g1
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	ba,pt	%xcc, .LL90
	 nop
.LL89:
	ld	[%fp+72], %g1
	sub	%g0, %g1, %g1
	and	%g1, 31, %o5
	ld	[%fp+68], %g1
	srl	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	sll	%g1, %o5, %g1
	or	%g1, %o4, %g1
.LL90:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE29:
	.size	Rotate__uRotate_param_C_I, .-Rotate__uRotate_param_C_I
	.align 4
	.type	Rotate__uRotate_var_C_C, #function
	.proc	04
Rotate__uRotate_var_C_C:
.LLFB30:
	save	%sp, -96, %sp
.LLCFI30:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 0
	bl	%icc, .LL92
	 nop
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+96], %g1
	sethi	%hi(MM_Rotate), %o5
	or	%o5, %lo(MM_Rotate), %o5
	ld	[%o5+96], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	ba,pt	%xcc, .LL93
	 nop
.LL92:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+96], %g1
	sethi	%hi(MM_Rotate), %o5
	or	%o5, %lo(MM_Rotate), %o5
	ld	[%o5+96], %o5
	sub	%g0, %o5, %o5
	and	%o5, 31, %o5
	srl	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	sll	%g1, %o5, %g1
	or	%g1, %o4, %g1
.LL93:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE30:
	.size	Rotate__uRotate_var_C_C, .-Rotate__uRotate_var_C_C
	.align 4
	.type	Rotate__uRotate_param_C_C, #function
	.proc	04
Rotate__uRotate_param_C_C:
.LLFB31:
	save	%sp, -96, %sp
.LLCFI31:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL95
	 nop
	ld	[%fp+72], %g1
	and	%g1, 31, %o5
	ld	[%fp+68], %g1
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	ba,pt	%xcc, .LL96
	 nop
.LL95:
	ld	[%fp+72], %g1
	sub	%g0, %g1, %g1
	and	%g1, 31, %o5
	ld	[%fp+68], %g1
	srl	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	sll	%g1, %o5, %g1
	or	%g1, %o4, %g1
.LL96:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE31:
	.size	Rotate__uRotate_param_C_C, .-Rotate__uRotate_param_C_C
	.align 4
	.type	Rotate__uRotate_var_u8_I, #function
	.proc	04
Rotate__uRotate_var_u8_I:
.LLFB32:
	save	%sp, -96, %sp
.LLCFI32:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL98
	 nop
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	sethi	%hi(MM_Rotate), %o5
	or	%o5, %lo(MM_Rotate), %o5
	ld	[%o5+80], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	ba,pt	%xcc, .LL99
	 nop
.LL98:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	sethi	%hi(MM_Rotate), %o5
	or	%o5, %lo(MM_Rotate), %o5
	ld	[%o5+80], %o5
	sub	%g0, %o5, %o5
	and	%o5, 31, %o5
	srl	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	sll	%g1, %o5, %g1
	or	%g1, %o4, %g1
.LL99:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE32:
	.size	Rotate__uRotate_var_u8_I, .-Rotate__uRotate_var_u8_I
	.align 4
	.type	Rotate__uRotate_param_u8_I, #function
	.proc	04
Rotate__uRotate_param_u8_I:
.LLFB33:
	save	%sp, -96, %sp
.LLCFI33:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL101
	 nop
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	ld	[%fp+72], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	ba,pt	%xcc, .LL102
	 nop
.LL101:
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	ld	[%fp+72], %o5
	sub	%g0, %o5, %o5
	and	%o5, 31, %o5
	srl	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	sll	%g1, %o5, %g1
	or	%g1, %o4, %g1
.LL102:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE33:
	.size	Rotate__uRotate_param_u8_I, .-Rotate__uRotate_param_u8_I
	.align 4
	.type	Rotate__uRotate_var_u8_C, #function
	.proc	04
Rotate__uRotate_var_u8_C:
.LLFB34:
	save	%sp, -96, %sp
.LLCFI34:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 0
	bl	%icc, .LL104
	 nop
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	sethi	%hi(MM_Rotate), %o5
	or	%o5, %lo(MM_Rotate), %o5
	ld	[%o5+96], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	ba,pt	%xcc, .LL105
	 nop
.LL104:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	sethi	%hi(MM_Rotate), %o5
	or	%o5, %lo(MM_Rotate), %o5
	ld	[%o5+96], %o5
	sub	%g0, %o5, %o5
	and	%o5, 31, %o5
	srl	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	sll	%g1, %o5, %g1
	or	%g1, %o4, %g1
.LL105:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE34:
	.size	Rotate__uRotate_var_u8_C, .-Rotate__uRotate_var_u8_C
	.align 4
	.type	Rotate__uRotate_param_u8_C, #function
	.proc	04
Rotate__uRotate_param_u8_C:
.LLFB35:
	save	%sp, -96, %sp
.LLCFI35:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL107
	 nop
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	ld	[%fp+72], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	ba,pt	%xcc, .LL108
	 nop
.LL107:
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	ld	[%fp+72], %o5
	sub	%g0, %o5, %o5
	and	%o5, 31, %o5
	srl	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	sll	%g1, %o5, %g1
	or	%g1, %o4, %g1
.LL108:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE35:
	.size	Rotate__uRotate_param_u8_C, .-Rotate__uRotate_param_u8_C
	.align 4
	.type	Rotate__uRotate_var_L_I, #function
	.proc	05
Rotate__uRotate_var_L_I:
.LLFB36:
	save	%sp, -96, %sp
.LLCFI36:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL110
	 nop
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ldd	[%g1+120], %o4
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
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
	ba,pt	%xcc, .LL111
	 nop
.LL110:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ldd	[%g1+120], %o4
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+80], %g1
	sub	%g0, %g1, %g1
	and	%g1, 63, %g1
	sllx	%o4, 32, %o2
	or	%o5, %o2, %o2
	srlx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	mov	64, %o1
	sub	%o1, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o4, %o2, %o4
	or	%o5, %o3, %o5
.LL111:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE36:
	.size	Rotate__uRotate_var_L_I, .-Rotate__uRotate_var_L_I
	.align 4
	.type	Rotate__uRotate_param_L_I, #function
	.proc	05
Rotate__uRotate_param_L_I:
.LLFB37:
	save	%sp, -104, %sp
.LLCFI37:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	cmp	%g1, 0
	bl	%icc, .LL113
	 nop
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
	ba,pt	%xcc, .LL114
	 nop
.LL113:
	ldd	[%fp-8], %o4
	ld	[%fp+76], %g1
	sub	%g0, %g1, %g1
	and	%g1, 63, %g1
	sllx	%o4, 32, %o2
	or	%o5, %o2, %o2
	srlx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	mov	64, %o1
	sub	%o1, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o4, %o2, %o4
	or	%o5, %o3, %o5
.LL114:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE37:
	.size	Rotate__uRotate_param_L_I, .-Rotate__uRotate_param_L_I
	.align 4
	.type	Rotate__uRotate_var_L_C, #function
	.proc	05
Rotate__uRotate_var_L_C:
.LLFB38:
	save	%sp, -96, %sp
.LLCFI38:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 0
	bl	%icc, .LL116
	 nop
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ldd	[%g1+120], %o4
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
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
	ba,pt	%xcc, .LL117
	 nop
.LL116:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ldd	[%g1+120], %o4
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+96], %g1
	sub	%g0, %g1, %g1
	and	%g1, 63, %g1
	sllx	%o4, 32, %o2
	or	%o5, %o2, %o2
	srlx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	mov	64, %o1
	sub	%o1, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o4, %o2, %o4
	or	%o5, %o3, %o5
.LL117:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE38:
	.size	Rotate__uRotate_var_L_C, .-Rotate__uRotate_var_L_C
	.align 4
	.type	Rotate__uRotate_param_L_C, #function
	.proc	05
Rotate__uRotate_param_L_C:
.LLFB39:
	save	%sp, -104, %sp
.LLCFI39:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	cmp	%g1, 0
	bl	%icc, .LL119
	 nop
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
	ba,pt	%xcc, .LL120
	 nop
.LL119:
	ldd	[%fp-8], %o4
	ld	[%fp+76], %g1
	sub	%g0, %g1, %g1
	and	%g1, 63, %g1
	sllx	%o4, 32, %o2
	or	%o5, %o2, %o2
	srlx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	mov	64, %o1
	sub	%o1, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o4, %o2, %o4
	or	%o5, %o3, %o5
.LL120:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE39:
	.size	Rotate__uRotate_param_L_C, .-Rotate__uRotate_param_L_C
	.align 4
	.type	Rotate__uRotate_var_i8_I, #function
	.proc	04
Rotate__uRotate_var_i8_I:
.LLFB40:
	save	%sp, -96, %sp
.LLCFI40:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL122
	 nop
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sethi	%hi(MM_Rotate), %o5
	or	%o5, %lo(MM_Rotate), %o5
	ld	[%o5+80], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	ba,pt	%xcc, .LL123
	 nop
.LL122:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sethi	%hi(MM_Rotate), %o5
	or	%o5, %lo(MM_Rotate), %o5
	ld	[%o5+80], %o5
	sub	%g0, %o5, %o5
	and	%o5, 31, %o5
	srl	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	sll	%g1, %o5, %g1
	or	%g1, %o4, %g1
.LL123:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE40:
	.size	Rotate__uRotate_var_i8_I, .-Rotate__uRotate_var_i8_I
	.align 4
	.type	Rotate__uRotate_param_i8_I, #function
	.proc	04
Rotate__uRotate_param_i8_I:
.LLFB41:
	save	%sp, -96, %sp
.LLCFI41:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL125
	 nop
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
	ba,pt	%xcc, .LL126
	 nop
.LL125:
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	ld	[%fp+72], %o5
	sub	%g0, %o5, %o5
	and	%o5, 31, %o5
	srl	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	sll	%g1, %o5, %g1
	or	%g1, %o4, %g1
.LL126:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE41:
	.size	Rotate__uRotate_param_i8_I, .-Rotate__uRotate_param_i8_I
	.align 4
	.type	Rotate__uRotate_var_i8_C, #function
	.proc	04
Rotate__uRotate_var_i8_C:
.LLFB42:
	save	%sp, -96, %sp
.LLCFI42:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 0
	bl	%icc, .LL128
	 nop
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sethi	%hi(MM_Rotate), %o5
	or	%o5, %lo(MM_Rotate), %o5
	ld	[%o5+96], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	ba,pt	%xcc, .LL129
	 nop
.LL128:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sethi	%hi(MM_Rotate), %o5
	or	%o5, %lo(MM_Rotate), %o5
	ld	[%o5+96], %o5
	sub	%g0, %o5, %o5
	and	%o5, 31, %o5
	srl	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	sll	%g1, %o5, %g1
	or	%g1, %o4, %g1
.LL129:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE42:
	.size	Rotate__uRotate_var_i8_C, .-Rotate__uRotate_var_i8_C
	.align 4
	.type	Rotate__uRotate_param_i8_C, #function
	.proc	04
Rotate__uRotate_param_i8_C:
.LLFB43:
	save	%sp, -96, %sp
.LLCFI43:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL131
	 nop
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
	ba,pt	%xcc, .LL132
	 nop
.LL131:
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	ld	[%fp+72], %o5
	sub	%g0, %o5, %o5
	and	%o5, 31, %o5
	srl	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	sll	%g1, %o5, %g1
	or	%g1, %o4, %g1
.LL132:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE43:
	.size	Rotate__uRotate_param_i8_C, .-Rotate__uRotate_param_i8_C
	.align 4
	.type	Rotate__uRotate_var_u32_I, #function
	.proc	04
Rotate__uRotate_var_u32_I:
.LLFB44:
	save	%sp, -96, %sp
.LLCFI44:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+80], %g1
	cmp	%g1, 0
	bl	%icc, .LL134
	 nop
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+132], %g1
	sethi	%hi(MM_Rotate), %o5
	or	%o5, %lo(MM_Rotate), %o5
	ld	[%o5+80], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	ba,pt	%xcc, .LL135
	 nop
.LL134:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+132], %g1
	sethi	%hi(MM_Rotate), %o5
	or	%o5, %lo(MM_Rotate), %o5
	ld	[%o5+80], %o5
	sub	%g0, %o5, %o5
	and	%o5, 31, %o5
	srl	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	sll	%g1, %o5, %g1
	or	%g1, %o4, %g1
.LL135:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE44:
	.size	Rotate__uRotate_var_u32_I, .-Rotate__uRotate_var_u32_I
	.align 4
	.type	Rotate__uRotate_param_u32_I, #function
	.proc	04
Rotate__uRotate_param_u32_I:
.LLFB45:
	save	%sp, -96, %sp
.LLCFI45:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL137
	 nop
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	ba,pt	%xcc, .LL138
	 nop
.LL137:
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	sub	%g0, %o5, %o5
	and	%o5, 31, %o5
	srl	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	sll	%g1, %o5, %g1
	or	%g1, %o4, %g1
.LL138:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE45:
	.size	Rotate__uRotate_param_u32_I, .-Rotate__uRotate_param_u32_I
	.align 4
	.type	Rotate__uRotate_var_u32_C, #function
	.proc	04
Rotate__uRotate_var_u32_C:
.LLFB46:
	save	%sp, -96, %sp
.LLCFI46:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+96], %g1
	cmp	%g1, 0
	bl	%icc, .LL140
	 nop
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+132], %g1
	sethi	%hi(MM_Rotate), %o5
	or	%o5, %lo(MM_Rotate), %o5
	ld	[%o5+96], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	ba,pt	%xcc, .LL141
	 nop
.LL140:
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	ld	[%g1+132], %g1
	sethi	%hi(MM_Rotate), %o5
	or	%o5, %lo(MM_Rotate), %o5
	ld	[%o5+96], %o5
	sub	%g0, %o5, %o5
	and	%o5, 31, %o5
	srl	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	sll	%g1, %o5, %g1
	or	%g1, %o4, %g1
.LL141:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE46:
	.size	Rotate__uRotate_var_u32_C, .-Rotate__uRotate_var_u32_C
	.align 4
	.type	Rotate__uRotate_param_u32_C, #function
	.proc	04
Rotate__uRotate_param_u32_C:
.LLFB47:
	save	%sp, -96, %sp
.LLCFI47:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL143
	 nop
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	and	%o5, 31, %o5
	sll	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	srl	%g1, %o5, %g1
	or	%g1, %o4, %g1
	ba,pt	%xcc, .LL144
	 nop
.LL143:
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	sub	%g0, %o5, %o5
	and	%o5, 31, %o5
	srl	%g1, %o5, %o4
	mov	32, %o3
	sub	%o3, %o5, %o5
	sll	%g1, %o5, %g1
	or	%g1, %o4, %g1
.LL144:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE47:
	.size	Rotate__uRotate_param_u32_C, .-Rotate__uRotate_param_u32_C
	.align 4
	.global Rotate_M3
	.type	Rotate_M3, #function
	.proc	0120
Rotate_M3:
.LLFB48:
	save	%sp, -96, %sp
.LLCFI48:
	st	%i0, [%fp+68]
	sethi	%hi(MM_Rotate), %g1
	or	%g1, %lo(MM_Rotate), %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE48:
	.size	Rotate_M3, .-Rotate_M3
	.section	".data"
	.align 8
	.type	L_1, #object
	.size	L_1, 1304
L_1:
	.ascii	"Rotate_M3"
	.skip 1
	.ascii	"uRotate_param_u32_C"
	.skip 1
	.ascii	"uRotate_var_u32_C"
	.skip 1
	.ascii	"uRotate_param_u32_I"
	.skip 1
	.ascii	"uRotate_var_u32_I"
	.skip 1
	.ascii	"uRotate_param_i8_C"
	.skip 1
	.ascii	"uRotate_var_i8_C"
	.skip 1
	.ascii	"uRotate_param_i8_I"
	.skip 1
	.ascii	"uRotate_var_i8_I"
	.skip 1
	.ascii	"uRotate_param_L_C"
	.skip 1
	.ascii	"uRotate_var_L_C"
	.skip 1
	.ascii	"uRotate_param_L_I"
	.skip 1
	.ascii	"uRotate_var_L_I"
	.skip 1
	.ascii	"uRotate_param_u8_C"
	.skip 1
	.ascii	"uRotate_var_u8_C"
	.skip 1
	.ascii	"uRotate_param_u8_I"
	.skip 1
	.ascii	"uRotate_var_u8_I"
	.skip 1
	.ascii	"uRotate_param_C_C"
	.skip 1
	.ascii	"uRotate_var_C_C"
	.skip 1
	.ascii	"uRotate_param_C_I"
	.skip 1
	.ascii	"uRotate_var_C_I"
	.skip 1
	.ascii	"uRotate_param_i64_C"
	.skip 1
	.ascii	"uRotate_var_i64_C"
	.skip 1
	.ascii	"uRotate_param_i64_I"
	.skip 1
	.ascii	"uRotate_var_i64_I"
	.skip 1
	.ascii	"uRotate_param_I_C"
	.skip 1
	.ascii	"uRotate_var_I_C"
	.skip 1
	.ascii	"uRotate_param_I_I"
	.skip 1
	.ascii	"uRotate_var_I_I"
	.skip 1
	.ascii	"uRotate_param_i16_C"
	.skip 1
	.ascii	"uRotate_var_i16_C"
	.skip 1
	.ascii	"uRotate_param_i16_I"
	.skip 1
	.ascii	"uRotate_var_i16_I"
	.skip 1
	.ascii	"uRotate_param_i32_C"
	.skip 1
	.ascii	"uRotate_var_i32_C"
	.skip 1
	.ascii	"uRotate_param_i32_I"
	.skip 1
	.ascii	"uRotate_var_i32_I"
	.skip 1
	.ascii	"uRotate_param_LC_C"
	.skip 1
	.ascii	"uRotate_var_LC_C"
	.skip 1
	.ascii	"uRotate_param_LC_I"
	.skip 1
	.ascii	"uRotate_var_LC_I"
	.skip 1
	.ascii	"uRotate_param_u64_C"
	.skip 1
	.ascii	"uRotate_var_u64_C"
	.skip 1
	.ascii	"uRotate_param_u64_I"
	.skip 1
	.ascii	"uRotate_var_u64_I"
	.skip 1
	.ascii	"uRotate_param_u16_C"
	.skip 1
	.ascii	"uRotate_var_u16_C"
	.skip 1
	.ascii	"uRotate_param_u16_I"
	.skip 1
	.ascii	"uRotate_var_u16_I"
	.skip 3
	.long	Rotate_M3
	.long	L_1
	.long	Rotate__uRotate_param_u32_C
	.long	L_1+10
	.long	Rotate__uRotate_var_u32_C
	.long	L_1+30
	.long	Rotate__uRotate_param_u32_I
	.long	L_1+48
	.long	Rotate__uRotate_var_u32_I
	.long	L_1+68
	.long	Rotate__uRotate_param_i8_C
	.long	L_1+86
	.long	Rotate__uRotate_var_i8_C
	.long	L_1+105
	.long	Rotate__uRotate_param_i8_I
	.long	L_1+122
	.long	Rotate__uRotate_var_i8_I
	.long	L_1+141
	.long	Rotate__uRotate_param_L_C
	.long	L_1+158
	.long	Rotate__uRotate_var_L_C
	.long	L_1+176
	.long	Rotate__uRotate_param_L_I
	.long	L_1+192
	.long	Rotate__uRotate_var_L_I
	.long	L_1+210
	.long	Rotate__uRotate_param_u8_C
	.long	L_1+226
	.long	Rotate__uRotate_var_u8_C
	.long	L_1+245
	.long	Rotate__uRotate_param_u8_I
	.long	L_1+262
	.long	Rotate__uRotate_var_u8_I
	.long	L_1+281
	.long	Rotate__uRotate_param_C_C
	.long	L_1+298
	.long	Rotate__uRotate_var_C_C
	.long	L_1+316
	.long	Rotate__uRotate_param_C_I
	.long	L_1+332
	.long	Rotate__uRotate_var_C_I
	.long	L_1+350
	.long	Rotate__uRotate_param_i64_C
	.long	L_1+366
	.long	Rotate__uRotate_var_i64_C
	.long	L_1+386
	.long	Rotate__uRotate_param_i64_I
	.long	L_1+404
	.long	Rotate__uRotate_var_i64_I
	.long	L_1+424
	.long	Rotate__uRotate_param_I_C
	.long	L_1+442
	.long	Rotate__uRotate_var_I_C
	.long	L_1+460
	.long	Rotate__uRotate_param_I_I
	.long	L_1+476
	.long	Rotate__uRotate_var_I_I
	.long	L_1+494
	.long	Rotate__uRotate_param_i16_C
	.long	L_1+510
	.long	Rotate__uRotate_var_i16_C
	.long	L_1+530
	.long	Rotate__uRotate_param_i16_I
	.long	L_1+548
	.long	Rotate__uRotate_var_i16_I
	.long	L_1+568
	.long	Rotate__uRotate_param_i32_C
	.long	L_1+586
	.long	Rotate__uRotate_var_i32_C
	.long	L_1+606
	.long	Rotate__uRotate_param_i32_I
	.long	L_1+624
	.long	Rotate__uRotate_var_i32_I
	.long	L_1+644
	.long	Rotate__uRotate_param_LC_C
	.long	L_1+662
	.long	Rotate__uRotate_var_LC_C
	.long	L_1+681
	.long	Rotate__uRotate_param_LC_I
	.long	L_1+698
	.long	Rotate__uRotate_var_LC_I
	.long	L_1+717
	.long	Rotate__uRotate_param_u64_C
	.long	L_1+734
	.long	Rotate__uRotate_var_u64_C
	.long	L_1+754
	.long	Rotate__uRotate_param_u64_I
	.long	L_1+772
	.long	Rotate__uRotate_var_u64_I
	.long	L_1+792
	.long	Rotate__uRotate_param_u16_C
	.long	L_1+810
	.long	Rotate__uRotate_var_u16_C
	.long	L_1+830
	.long	Rotate__uRotate_param_u16_I
	.long	L_1+848
	.long	Rotate__uRotate_var_u16_I
	.long	L_1+868
	.skip 4
	.ascii	"../SOLsun/Rotate.m3"
	.skip 1
	.align 8
	.type	MM_Rotate, #object
	.size	MM_Rotate, 208
MM_Rotate:
	.long	L_1+1284
	.skip 16
	.long	L_1+888
	.skip 12
	.long	MM_Rotate+148
	.skip 4
	.long	Rotate_M3
	.long	3
	.half	785
	.skip 2
	.long	0
	.long	786
	.long	0
	.long	787
	.long	788
	.half	789
	.skip 2
	.long	790
	.skip 4
	.long	0
	.long	791
	.long	792
	.skip 4
	.long	1082707546
	.long	481036337
	.byte	27
	.skip 7
	.long	0
	.long	796
	.byte	29
	.skip 3
	.long	798
	.long	1145566003
	.skip 12
	.long	Rotate_I3
	.long	MM_Rotate+160
	.skip 4
	.long	Long_I3
	.long	MM_Rotate+172
	.skip 4
	.long	Word_I3
	.long	MM_Rotate+184
	.skip 4
	.long	Cstdint_I3
	.long	MM_Rotate+196
	.skip 4
	.long	RTHooks_I3
	.skip 4
	.ident	"GCC: (GNU) 4.5.1"
