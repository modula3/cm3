	.file	"And.mc"
	.section	".text"
	.align 4
	.type	And__uAnd_var_u16_u16, #function
	.proc	04
And__uAnd_var_u16_u16:
.LLFB0:
	save	%sp, -96, %sp
.LLCFI0:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE0:
	.size	And__uAnd_var_u16_u16, .-And__uAnd_var_u16_u16
	.align 4
	.type	And__uAnd_param_u16_u16, #function
	.proc	04
And__uAnd_param_u16_u16:
.LLFB1:
	save	%sp, -96, %sp
.LLCFI1:
	sth	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1:
	.size	And__uAnd_param_u16_u16, .-And__uAnd_param_u16_u16
	.align 4
	.type	And__uAnd_var_u16_u64, #function
	.proc	05
And__uAnd_var_u16_u64:
.LLFB2:
	save	%sp, -96, %sp
.LLCFI2:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+56], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE2:
	.size	And__uAnd_var_u16_u64, .-And__uAnd_var_u16_u64
	.align 4
	.type	And__uAnd_param_u16_u64, #function
	.proc	05
And__uAnd_param_u16_u64:
.LLFB3:
	save	%sp, -104, %sp
.LLCFI3:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE3:
	.size	And__uAnd_param_u16_u64, .-And__uAnd_param_u16_u64
	.align 4
	.type	And__uAnd_var_u16_LC, #function
	.proc	05
And__uAnd_var_u16_LC:
.LLFB4:
	save	%sp, -96, %sp
.LLCFI4:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+64], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE4:
	.size	And__uAnd_var_u16_LC, .-And__uAnd_var_u16_LC
	.align 4
	.type	And__uAnd_param_u16_LC, #function
	.proc	05
And__uAnd_param_u16_LC:
.LLFB5:
	save	%sp, -104, %sp
.LLCFI5:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE5:
	.size	And__uAnd_param_u16_LC, .-And__uAnd_param_u16_LC
	.align 4
	.type	And__uAnd_var_u16_i32, #function
	.proc	04
And__uAnd_var_u16_i32:
.LLFB6:
	save	%sp, -96, %sp
.LLCFI6:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+72], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE6:
	.size	And__uAnd_var_u16_i32, .-And__uAnd_var_u16_i32
	.align 4
	.type	And__uAnd_param_u16_i32, #function
	.proc	04
And__uAnd_param_u16_i32:
.LLFB7:
	save	%sp, -96, %sp
.LLCFI7:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE7:
	.size	And__uAnd_param_u16_i32, .-And__uAnd_param_u16_i32
	.align 4
	.type	And__uAnd_var_u16_i16, #function
	.proc	04
And__uAnd_var_u16_i16:
.LLFB8:
	save	%sp, -96, %sp
.LLCFI8:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE8:
	.size	And__uAnd_var_u16_i16, .-And__uAnd_var_u16_i16
	.align 4
	.type	And__uAnd_param_u16_i16, #function
	.proc	04
And__uAnd_param_u16_i16:
.LLFB9:
	save	%sp, -96, %sp
.LLCFI9:
	sth	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE9:
	.size	And__uAnd_param_u16_i16, .-And__uAnd_param_u16_i16
	.align 4
	.type	And__uAnd_var_u16_I, #function
	.proc	04
And__uAnd_var_u16_I:
.LLFB10:
	save	%sp, -96, %sp
.LLCFI10:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+80], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE10:
	.size	And__uAnd_var_u16_I, .-And__uAnd_var_u16_I
	.align 4
	.type	And__uAnd_param_u16_I, #function
	.proc	04
And__uAnd_param_u16_I:
.LLFB11:
	save	%sp, -96, %sp
.LLCFI11:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE11:
	.size	And__uAnd_param_u16_I, .-And__uAnd_param_u16_I
	.align 4
	.type	And__uAnd_var_u16_i64, #function
	.proc	05
And__uAnd_var_u16_i64:
.LLFB12:
	save	%sp, -96, %sp
.LLCFI12:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+88], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE12:
	.size	And__uAnd_var_u16_i64, .-And__uAnd_var_u16_i64
	.align 4
	.type	And__uAnd_param_u16_i64, #function
	.proc	05
And__uAnd_param_u16_i64:
.LLFB13:
	save	%sp, -104, %sp
.LLCFI13:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE13:
	.size	And__uAnd_param_u16_i64, .-And__uAnd_param_u16_i64
	.align 4
	.type	And__uAnd_var_u16_C, #function
	.proc	04
And__uAnd_var_u16_C:
.LLFB14:
	save	%sp, -96, %sp
.LLCFI14:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+96], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE14:
	.size	And__uAnd_var_u16_C, .-And__uAnd_var_u16_C
	.align 4
	.type	And__uAnd_param_u16_C, #function
	.proc	04
And__uAnd_param_u16_C:
.LLFB15:
	save	%sp, -96, %sp
.LLCFI15:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE15:
	.size	And__uAnd_param_u16_C, .-And__uAnd_param_u16_C
	.align 4
	.type	And__uAnd_var_u16_u8, #function
	.proc	04
And__uAnd_var_u16_u8:
.LLFB16:
	save	%sp, -96, %sp
.LLCFI16:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE16:
	.size	And__uAnd_var_u16_u8, .-And__uAnd_var_u16_u8
	.align 4
	.type	And__uAnd_param_u16_u8, #function
	.proc	04
And__uAnd_param_u16_u8:
.LLFB17:
	save	%sp, -96, %sp
.LLCFI17:
	sth	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE17:
	.size	And__uAnd_param_u16_u8, .-And__uAnd_param_u16_u8
	.align 4
	.type	And__uAnd_var_u16_L, #function
	.proc	05
And__uAnd_var_u16_L:
.LLFB18:
	save	%sp, -96, %sp
.LLCFI18:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+120], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE18:
	.size	And__uAnd_var_u16_L, .-And__uAnd_var_u16_L
	.align 4
	.type	And__uAnd_param_u16_L, #function
	.proc	05
And__uAnd_param_u16_L:
.LLFB19:
	save	%sp, -104, %sp
.LLCFI19:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE19:
	.size	And__uAnd_param_u16_L, .-And__uAnd_param_u16_L
	.align 4
	.type	And__uAnd_var_u16_i8, #function
	.proc	04
And__uAnd_var_u16_i8:
.LLFB20:
	save	%sp, -96, %sp
.LLCFI20:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE20:
	.size	And__uAnd_var_u16_i8, .-And__uAnd_var_u16_i8
	.align 4
	.type	And__uAnd_param_u16_i8, #function
	.proc	04
And__uAnd_param_u16_i8:
.LLFB21:
	save	%sp, -96, %sp
.LLCFI21:
	sth	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE21:
	.size	And__uAnd_param_u16_i8, .-And__uAnd_param_u16_i8
	.align 4
	.type	And__uAnd_var_u16_u32, #function
	.proc	04
And__uAnd_var_u16_u32:
.LLFB22:
	save	%sp, -96, %sp
.LLCFI22:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+132], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE22:
	.size	And__uAnd_var_u16_u32, .-And__uAnd_var_u16_u32
	.align 4
	.type	And__uAnd_param_u16_u32, #function
	.proc	04
And__uAnd_param_u16_u32:
.LLFB23:
	save	%sp, -96, %sp
.LLCFI23:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE23:
	.size	And__uAnd_param_u16_u32, .-And__uAnd_param_u16_u32
	.align 4
	.type	And__uAnd_var_u64_u16, #function
	.proc	05
And__uAnd_var_u64_u16:
.LLFB24:
	save	%sp, -96, %sp
.LLCFI24:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+56], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE24:
	.size	And__uAnd_var_u64_u16, .-And__uAnd_var_u64_u16
	.align 4
	.type	And__uAnd_param_u64_u16, #function
	.proc	05
And__uAnd_param_u64_u16:
.LLFB25:
	save	%sp, -104, %sp
.LLCFI25:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE25:
	.size	And__uAnd_param_u64_u16, .-And__uAnd_param_u64_u16
	.align 4
	.type	And__uAnd_var_u64_u64, #function
	.proc	05
And__uAnd_var_u64_u64:
.LLFB26:
	save	%sp, -96, %sp
.LLCFI26:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+56], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+56], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE26:
	.size	And__uAnd_var_u64_u64, .-And__uAnd_var_u64_u64
	.align 4
	.type	And__uAnd_param_u64_u64, #function
	.proc	05
And__uAnd_param_u64_u64:
.LLFB27:
	save	%sp, -112, %sp
.LLCFI27:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE27:
	.size	And__uAnd_param_u64_u64, .-And__uAnd_param_u64_u64
	.align 4
	.type	And__uAnd_var_u64_LC, #function
	.proc	05
And__uAnd_var_u64_LC:
.LLFB28:
	save	%sp, -96, %sp
.LLCFI28:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+56], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+64], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE28:
	.size	And__uAnd_var_u64_LC, .-And__uAnd_var_u64_LC
	.align 4
	.type	And__uAnd_param_u64_LC, #function
	.proc	05
And__uAnd_param_u64_LC:
.LLFB29:
	save	%sp, -112, %sp
.LLCFI29:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE29:
	.size	And__uAnd_param_u64_LC, .-And__uAnd_param_u64_LC
	.align 4
	.type	And__uAnd_var_u64_i32, #function
	.proc	05
And__uAnd_var_u64_i32:
.LLFB30:
	save	%sp, -96, %sp
.LLCFI30:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+56], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE30:
	.size	And__uAnd_var_u64_i32, .-And__uAnd_var_u64_i32
	.align 4
	.type	And__uAnd_param_u64_i32, #function
	.proc	05
And__uAnd_param_u64_i32:
.LLFB31:
	save	%sp, -104, %sp
.LLCFI31:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE31:
	.size	And__uAnd_param_u64_i32, .-And__uAnd_param_u64_i32
	.align 4
	.type	And__uAnd_var_u64_i16, #function
	.proc	05
And__uAnd_var_u64_i16:
.LLFB32:
	save	%sp, -96, %sp
.LLCFI32:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+56], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE32:
	.size	And__uAnd_var_u64_i16, .-And__uAnd_var_u64_i16
	.align 4
	.type	And__uAnd_param_u64_i16, #function
	.proc	05
And__uAnd_param_u64_i16:
.LLFB33:
	save	%sp, -104, %sp
.LLCFI33:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE33:
	.size	And__uAnd_param_u64_i16, .-And__uAnd_param_u64_i16
	.align 4
	.type	And__uAnd_var_u64_I, #function
	.proc	05
And__uAnd_var_u64_I:
.LLFB34:
	save	%sp, -96, %sp
.LLCFI34:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+56], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE34:
	.size	And__uAnd_var_u64_I, .-And__uAnd_var_u64_I
	.align 4
	.type	And__uAnd_param_u64_I, #function
	.proc	05
And__uAnd_param_u64_I:
.LLFB35:
	save	%sp, -104, %sp
.LLCFI35:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE35:
	.size	And__uAnd_param_u64_I, .-And__uAnd_param_u64_I
	.align 4
	.type	And__uAnd_var_u64_i64, #function
	.proc	05
And__uAnd_var_u64_i64:
.LLFB36:
	save	%sp, -96, %sp
.LLCFI36:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+56], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+88], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE36:
	.size	And__uAnd_var_u64_i64, .-And__uAnd_var_u64_i64
	.align 4
	.type	And__uAnd_param_u64_i64, #function
	.proc	05
And__uAnd_param_u64_i64:
.LLFB37:
	save	%sp, -112, %sp
.LLCFI37:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE37:
	.size	And__uAnd_param_u64_i64, .-And__uAnd_param_u64_i64
	.align 4
	.type	And__uAnd_var_u64_C, #function
	.proc	05
And__uAnd_var_u64_C:
.LLFB38:
	save	%sp, -96, %sp
.LLCFI38:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+56], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE38:
	.size	And__uAnd_var_u64_C, .-And__uAnd_var_u64_C
	.align 4
	.type	And__uAnd_param_u64_C, #function
	.proc	05
And__uAnd_param_u64_C:
.LLFB39:
	save	%sp, -104, %sp
.LLCFI39:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE39:
	.size	And__uAnd_param_u64_C, .-And__uAnd_param_u64_C
	.align 4
	.type	And__uAnd_var_u64_u8, #function
	.proc	05
And__uAnd_var_u64_u8:
.LLFB40:
	save	%sp, -96, %sp
.LLCFI40:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+56], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE40:
	.size	And__uAnd_var_u64_u8, .-And__uAnd_var_u64_u8
	.align 4
	.type	And__uAnd_param_u64_u8, #function
	.proc	05
And__uAnd_param_u64_u8:
.LLFB41:
	save	%sp, -104, %sp
.LLCFI41:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE41:
	.size	And__uAnd_param_u64_u8, .-And__uAnd_param_u64_u8
	.align 4
	.type	And__uAnd_var_u64_L, #function
	.proc	05
And__uAnd_var_u64_L:
.LLFB42:
	save	%sp, -96, %sp
.LLCFI42:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+56], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+120], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE42:
	.size	And__uAnd_var_u64_L, .-And__uAnd_var_u64_L
	.align 4
	.type	And__uAnd_param_u64_L, #function
	.proc	05
And__uAnd_param_u64_L:
.LLFB43:
	save	%sp, -112, %sp
.LLCFI43:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE43:
	.size	And__uAnd_param_u64_L, .-And__uAnd_param_u64_L
	.align 4
	.type	And__uAnd_var_u64_i8, #function
	.proc	05
And__uAnd_var_u64_i8:
.LLFB44:
	save	%sp, -96, %sp
.LLCFI44:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+56], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE44:
	.size	And__uAnd_var_u64_i8, .-And__uAnd_var_u64_i8
	.align 4
	.type	And__uAnd_param_u64_i8, #function
	.proc	05
And__uAnd_param_u64_i8:
.LLFB45:
	save	%sp, -104, %sp
.LLCFI45:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE45:
	.size	And__uAnd_param_u64_i8, .-And__uAnd_param_u64_i8
	.align 4
	.type	And__uAnd_var_u64_u32, #function
	.proc	05
And__uAnd_var_u64_u32:
.LLFB46:
	save	%sp, -96, %sp
.LLCFI46:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+56], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE46:
	.size	And__uAnd_var_u64_u32, .-And__uAnd_var_u64_u32
	.align 4
	.type	And__uAnd_param_u64_u32, #function
	.proc	05
And__uAnd_param_u64_u32:
.LLFB47:
	save	%sp, -104, %sp
.LLCFI47:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE47:
	.size	And__uAnd_param_u64_u32, .-And__uAnd_param_u64_u32
	.align 4
	.type	And__uAnd_var_LC_u16, #function
	.proc	05
And__uAnd_var_LC_u16:
.LLFB48:
	save	%sp, -96, %sp
.LLCFI48:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+64], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE48:
	.size	And__uAnd_var_LC_u16, .-And__uAnd_var_LC_u16
	.align 4
	.type	And__uAnd_param_LC_u16, #function
	.proc	05
And__uAnd_param_LC_u16:
.LLFB49:
	save	%sp, -104, %sp
.LLCFI49:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE49:
	.size	And__uAnd_param_LC_u16, .-And__uAnd_param_LC_u16
	.align 4
	.type	And__uAnd_var_LC_u64, #function
	.proc	05
And__uAnd_var_LC_u64:
.LLFB50:
	save	%sp, -96, %sp
.LLCFI50:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+64], %o2
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+56], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE50:
	.size	And__uAnd_var_LC_u64, .-And__uAnd_var_LC_u64
	.align 4
	.type	And__uAnd_param_LC_u64, #function
	.proc	05
And__uAnd_param_LC_u64:
.LLFB51:
	save	%sp, -112, %sp
.LLCFI51:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o2
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE51:
	.size	And__uAnd_param_LC_u64, .-And__uAnd_param_LC_u64
	.align 4
	.type	And__uAnd_var_LC_LC, #function
	.proc	05
And__uAnd_var_LC_LC:
.LLFB52:
	save	%sp, -96, %sp
.LLCFI52:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+64], %o2
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+64], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE52:
	.size	And__uAnd_var_LC_LC, .-And__uAnd_var_LC_LC
	.align 4
	.type	And__uAnd_param_LC_LC, #function
	.proc	05
And__uAnd_param_LC_LC:
.LLFB53:
	save	%sp, -112, %sp
.LLCFI53:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE53:
	.size	And__uAnd_param_LC_LC, .-And__uAnd_param_LC_LC
	.align 4
	.type	And__uAnd_var_LC_i32, #function
	.proc	05
And__uAnd_var_LC_i32:
.LLFB54:
	save	%sp, -96, %sp
.LLCFI54:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+64], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE54:
	.size	And__uAnd_var_LC_i32, .-And__uAnd_var_LC_i32
	.align 4
	.type	And__uAnd_param_LC_i32, #function
	.proc	05
And__uAnd_param_LC_i32:
.LLFB55:
	save	%sp, -104, %sp
.LLCFI55:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE55:
	.size	And__uAnd_param_LC_i32, .-And__uAnd_param_LC_i32
	.align 4
	.type	And__uAnd_var_LC_i16, #function
	.proc	05
And__uAnd_var_LC_i16:
.LLFB56:
	save	%sp, -96, %sp
.LLCFI56:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+64], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE56:
	.size	And__uAnd_var_LC_i16, .-And__uAnd_var_LC_i16
	.align 4
	.type	And__uAnd_param_LC_i16, #function
	.proc	05
And__uAnd_param_LC_i16:
.LLFB57:
	save	%sp, -104, %sp
.LLCFI57:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE57:
	.size	And__uAnd_param_LC_i16, .-And__uAnd_param_LC_i16
	.align 4
	.type	And__uAnd_var_LC_I, #function
	.proc	05
And__uAnd_var_LC_I:
.LLFB58:
	save	%sp, -96, %sp
.LLCFI58:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+64], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE58:
	.size	And__uAnd_var_LC_I, .-And__uAnd_var_LC_I
	.align 4
	.type	And__uAnd_param_LC_I, #function
	.proc	05
And__uAnd_param_LC_I:
.LLFB59:
	save	%sp, -104, %sp
.LLCFI59:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE59:
	.size	And__uAnd_param_LC_I, .-And__uAnd_param_LC_I
	.align 4
	.type	And__uAnd_var_LC_i64, #function
	.proc	05
And__uAnd_var_LC_i64:
.LLFB60:
	save	%sp, -96, %sp
.LLCFI60:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+64], %o2
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+88], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE60:
	.size	And__uAnd_var_LC_i64, .-And__uAnd_var_LC_i64
	.align 4
	.type	And__uAnd_param_LC_i64, #function
	.proc	05
And__uAnd_param_LC_i64:
.LLFB61:
	save	%sp, -112, %sp
.LLCFI61:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o2
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE61:
	.size	And__uAnd_param_LC_i64, .-And__uAnd_param_LC_i64
	.align 4
	.type	And__uAnd_var_LC_C, #function
	.proc	05
And__uAnd_var_LC_C:
.LLFB62:
	save	%sp, -96, %sp
.LLCFI62:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+64], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE62:
	.size	And__uAnd_var_LC_C, .-And__uAnd_var_LC_C
	.align 4
	.type	And__uAnd_param_LC_C, #function
	.proc	05
And__uAnd_param_LC_C:
.LLFB63:
	save	%sp, -104, %sp
.LLCFI63:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE63:
	.size	And__uAnd_param_LC_C, .-And__uAnd_param_LC_C
	.align 4
	.type	And__uAnd_var_LC_u8, #function
	.proc	05
And__uAnd_var_LC_u8:
.LLFB64:
	save	%sp, -96, %sp
.LLCFI64:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+64], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE64:
	.size	And__uAnd_var_LC_u8, .-And__uAnd_var_LC_u8
	.align 4
	.type	And__uAnd_param_LC_u8, #function
	.proc	05
And__uAnd_param_LC_u8:
.LLFB65:
	save	%sp, -104, %sp
.LLCFI65:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE65:
	.size	And__uAnd_param_LC_u8, .-And__uAnd_param_LC_u8
	.align 4
	.type	And__uAnd_var_LC_L, #function
	.proc	05
And__uAnd_var_LC_L:
.LLFB66:
	save	%sp, -96, %sp
.LLCFI66:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+64], %o2
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+120], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE66:
	.size	And__uAnd_var_LC_L, .-And__uAnd_var_LC_L
	.align 4
	.type	And__uAnd_param_LC_L, #function
	.proc	05
And__uAnd_param_LC_L:
.LLFB67:
	save	%sp, -112, %sp
.LLCFI67:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o2
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE67:
	.size	And__uAnd_param_LC_L, .-And__uAnd_param_LC_L
	.align 4
	.type	And__uAnd_var_LC_i8, #function
	.proc	05
And__uAnd_var_LC_i8:
.LLFB68:
	save	%sp, -96, %sp
.LLCFI68:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+64], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE68:
	.size	And__uAnd_var_LC_i8, .-And__uAnd_var_LC_i8
	.align 4
	.type	And__uAnd_param_LC_i8, #function
	.proc	05
And__uAnd_param_LC_i8:
.LLFB69:
	save	%sp, -104, %sp
.LLCFI69:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE69:
	.size	And__uAnd_param_LC_i8, .-And__uAnd_param_LC_i8
	.align 4
	.type	And__uAnd_var_LC_u32, #function
	.proc	05
And__uAnd_var_LC_u32:
.LLFB70:
	save	%sp, -96, %sp
.LLCFI70:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+64], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE70:
	.size	And__uAnd_var_LC_u32, .-And__uAnd_var_LC_u32
	.align 4
	.type	And__uAnd_param_LC_u32, #function
	.proc	05
And__uAnd_param_LC_u32:
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
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE71:
	.size	And__uAnd_param_LC_u32, .-And__uAnd_param_LC_u32
	.align 4
	.type	And__uAnd_var_i32_u16, #function
	.proc	04
And__uAnd_var_i32_u16:
.LLFB72:
	save	%sp, -96, %sp
.LLCFI72:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE72:
	.size	And__uAnd_var_i32_u16, .-And__uAnd_var_i32_u16
	.align 4
	.type	And__uAnd_param_i32_u16, #function
	.proc	04
And__uAnd_param_i32_u16:
.LLFB73:
	save	%sp, -96, %sp
.LLCFI73:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ld	[%fp+68], %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE73:
	.size	And__uAnd_param_i32_u16, .-And__uAnd_param_i32_u16
	.align 4
	.type	And__uAnd_var_i32_u64, #function
	.proc	05
And__uAnd_var_i32_u64:
.LLFB74:
	save	%sp, -96, %sp
.LLCFI74:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+56], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE74:
	.size	And__uAnd_var_i32_u64, .-And__uAnd_var_i32_u64
	.align 4
	.type	And__uAnd_param_i32_u64, #function
	.proc	05
And__uAnd_param_i32_u64:
.LLFB75:
	save	%sp, -104, %sp
.LLCFI75:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE75:
	.size	And__uAnd_param_i32_u64, .-And__uAnd_param_i32_u64
	.align 4
	.type	And__uAnd_var_i32_LC, #function
	.proc	05
And__uAnd_var_i32_LC:
.LLFB76:
	save	%sp, -96, %sp
.LLCFI76:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+64], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE76:
	.size	And__uAnd_var_i32_LC, .-And__uAnd_var_i32_LC
	.align 4
	.type	And__uAnd_param_i32_LC, #function
	.proc	05
And__uAnd_param_i32_LC:
.LLFB77:
	save	%sp, -104, %sp
.LLCFI77:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE77:
	.size	And__uAnd_param_i32_LC, .-And__uAnd_param_i32_LC
	.align 4
	.type	And__uAnd_var_i32_i32, #function
	.proc	04
And__uAnd_var_i32_i32:
.LLFB78:
	save	%sp, -96, %sp
.LLCFI78:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+72], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE78:
	.size	And__uAnd_var_i32_i32, .-And__uAnd_var_i32_i32
	.align 4
	.type	And__uAnd_param_i32_i32, #function
	.proc	04
And__uAnd_param_i32_i32:
.LLFB79:
	save	%sp, -96, %sp
.LLCFI79:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE79:
	.size	And__uAnd_param_i32_i32, .-And__uAnd_param_i32_i32
	.align 4
	.type	And__uAnd_var_i32_i16, #function
	.proc	04
And__uAnd_var_i32_i16:
.LLFB80:
	save	%sp, -96, %sp
.LLCFI80:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE80:
	.size	And__uAnd_var_i32_i16, .-And__uAnd_var_i32_i16
	.align 4
	.type	And__uAnd_param_i32_i16, #function
	.proc	04
And__uAnd_param_i32_i16:
.LLFB81:
	save	%sp, -96, %sp
.LLCFI81:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ld	[%fp+68], %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE81:
	.size	And__uAnd_param_i32_i16, .-And__uAnd_param_i32_i16
	.align 4
	.type	And__uAnd_var_i32_I, #function
	.proc	04
And__uAnd_var_i32_I:
.LLFB82:
	save	%sp, -96, %sp
.LLCFI82:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+80], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE82:
	.size	And__uAnd_var_i32_I, .-And__uAnd_var_i32_I
	.align 4
	.type	And__uAnd_param_i32_I, #function
	.proc	04
And__uAnd_param_i32_I:
.LLFB83:
	save	%sp, -96, %sp
.LLCFI83:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE83:
	.size	And__uAnd_param_i32_I, .-And__uAnd_param_i32_I
	.align 4
	.type	And__uAnd_var_i32_i64, #function
	.proc	05
And__uAnd_var_i32_i64:
.LLFB84:
	save	%sp, -96, %sp
.LLCFI84:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+88], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE84:
	.size	And__uAnd_var_i32_i64, .-And__uAnd_var_i32_i64
	.align 4
	.type	And__uAnd_param_i32_i64, #function
	.proc	05
And__uAnd_param_i32_i64:
.LLFB85:
	save	%sp, -104, %sp
.LLCFI85:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE85:
	.size	And__uAnd_param_i32_i64, .-And__uAnd_param_i32_i64
	.align 4
	.type	And__uAnd_var_i32_C, #function
	.proc	04
And__uAnd_var_i32_C:
.LLFB86:
	save	%sp, -96, %sp
.LLCFI86:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+96], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE86:
	.size	And__uAnd_var_i32_C, .-And__uAnd_var_i32_C
	.align 4
	.type	And__uAnd_param_i32_C, #function
	.proc	04
And__uAnd_param_i32_C:
.LLFB87:
	save	%sp, -96, %sp
.LLCFI87:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE87:
	.size	And__uAnd_param_i32_C, .-And__uAnd_param_i32_C
	.align 4
	.type	And__uAnd_var_i32_u8, #function
	.proc	04
And__uAnd_var_i32_u8:
.LLFB88:
	save	%sp, -96, %sp
.LLCFI88:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE88:
	.size	And__uAnd_var_i32_u8, .-And__uAnd_var_i32_u8
	.align 4
	.type	And__uAnd_param_i32_u8, #function
	.proc	04
And__uAnd_param_i32_u8:
.LLFB89:
	save	%sp, -96, %sp
.LLCFI89:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE89:
	.size	And__uAnd_param_i32_u8, .-And__uAnd_param_i32_u8
	.align 4
	.type	And__uAnd_var_i32_L, #function
	.proc	05
And__uAnd_var_i32_L:
.LLFB90:
	save	%sp, -96, %sp
.LLCFI90:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+120], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE90:
	.size	And__uAnd_var_i32_L, .-And__uAnd_var_i32_L
	.align 4
	.type	And__uAnd_param_i32_L, #function
	.proc	05
And__uAnd_param_i32_L:
.LLFB91:
	save	%sp, -104, %sp
.LLCFI91:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE91:
	.size	And__uAnd_param_i32_L, .-And__uAnd_param_i32_L
	.align 4
	.type	And__uAnd_var_i32_i8, #function
	.proc	04
And__uAnd_var_i32_i8:
.LLFB92:
	save	%sp, -96, %sp
.LLCFI92:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE92:
	.size	And__uAnd_var_i32_i8, .-And__uAnd_var_i32_i8
	.align 4
	.type	And__uAnd_param_i32_i8, #function
	.proc	04
And__uAnd_param_i32_i8:
.LLFB93:
	save	%sp, -96, %sp
.LLCFI93:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE93:
	.size	And__uAnd_param_i32_i8, .-And__uAnd_param_i32_i8
	.align 4
	.type	And__uAnd_var_i32_u32, #function
	.proc	04
And__uAnd_var_i32_u32:
.LLFB94:
	save	%sp, -96, %sp
.LLCFI94:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+132], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE94:
	.size	And__uAnd_var_i32_u32, .-And__uAnd_var_i32_u32
	.align 4
	.type	And__uAnd_param_i32_u32, #function
	.proc	04
And__uAnd_param_i32_u32:
.LLFB95:
	save	%sp, -96, %sp
.LLCFI95:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE95:
	.size	And__uAnd_param_i32_u32, .-And__uAnd_param_i32_u32
	.align 4
	.type	And__uAnd_var_i16_u16, #function
	.proc	04
And__uAnd_var_i16_u16:
.LLFB96:
	save	%sp, -96, %sp
.LLCFI96:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE96:
	.size	And__uAnd_var_i16_u16, .-And__uAnd_var_i16_u16
	.align 4
	.type	And__uAnd_param_i16_u16, #function
	.proc	04
And__uAnd_param_i16_u16:
.LLFB97:
	save	%sp, -96, %sp
.LLCFI97:
	sth	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE97:
	.size	And__uAnd_param_i16_u16, .-And__uAnd_param_i16_u16
	.align 4
	.type	And__uAnd_var_i16_u64, #function
	.proc	05
And__uAnd_var_i16_u64:
.LLFB98:
	save	%sp, -96, %sp
.LLCFI98:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+56], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE98:
	.size	And__uAnd_var_i16_u64, .-And__uAnd_var_i16_u64
	.align 4
	.type	And__uAnd_param_i16_u64, #function
	.proc	05
And__uAnd_param_i16_u64:
.LLFB99:
	save	%sp, -104, %sp
.LLCFI99:
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
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE99:
	.size	And__uAnd_param_i16_u64, .-And__uAnd_param_i16_u64
	.align 4
	.type	And__uAnd_var_i16_LC, #function
	.proc	05
And__uAnd_var_i16_LC:
.LLFB100:
	save	%sp, -96, %sp
.LLCFI100:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+64], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE100:
	.size	And__uAnd_var_i16_LC, .-And__uAnd_var_i16_LC
	.align 4
	.type	And__uAnd_param_i16_LC, #function
	.proc	05
And__uAnd_param_i16_LC:
.LLFB101:
	save	%sp, -104, %sp
.LLCFI101:
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
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE101:
	.size	And__uAnd_param_i16_LC, .-And__uAnd_param_i16_LC
	.align 4
	.type	And__uAnd_var_i16_i32, #function
	.proc	04
And__uAnd_var_i16_i32:
.LLFB102:
	save	%sp, -96, %sp
.LLCFI102:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+72], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE102:
	.size	And__uAnd_var_i16_i32, .-And__uAnd_var_i16_i32
	.align 4
	.type	And__uAnd_param_i16_i32, #function
	.proc	04
And__uAnd_param_i16_i32:
.LLFB103:
	save	%sp, -96, %sp
.LLCFI103:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE103:
	.size	And__uAnd_param_i16_i32, .-And__uAnd_param_i16_i32
	.align 4
	.type	And__uAnd_var_i16_i16, #function
	.proc	04
And__uAnd_var_i16_i16:
.LLFB104:
	save	%sp, -96, %sp
.LLCFI104:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE104:
	.size	And__uAnd_var_i16_i16, .-And__uAnd_var_i16_i16
	.align 4
	.type	And__uAnd_param_i16_i16, #function
	.proc	04
And__uAnd_param_i16_i16:
.LLFB105:
	save	%sp, -96, %sp
.LLCFI105:
	sth	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE105:
	.size	And__uAnd_param_i16_i16, .-And__uAnd_param_i16_i16
	.align 4
	.type	And__uAnd_var_i16_I, #function
	.proc	04
And__uAnd_var_i16_I:
.LLFB106:
	save	%sp, -96, %sp
.LLCFI106:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+80], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE106:
	.size	And__uAnd_var_i16_I, .-And__uAnd_var_i16_I
	.align 4
	.type	And__uAnd_param_i16_I, #function
	.proc	04
And__uAnd_param_i16_I:
.LLFB107:
	save	%sp, -96, %sp
.LLCFI107:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE107:
	.size	And__uAnd_param_i16_I, .-And__uAnd_param_i16_I
	.align 4
	.type	And__uAnd_var_i16_i64, #function
	.proc	05
And__uAnd_var_i16_i64:
.LLFB108:
	save	%sp, -96, %sp
.LLCFI108:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+88], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE108:
	.size	And__uAnd_var_i16_i64, .-And__uAnd_var_i16_i64
	.align 4
	.type	And__uAnd_param_i16_i64, #function
	.proc	05
And__uAnd_param_i16_i64:
.LLFB109:
	save	%sp, -104, %sp
.LLCFI109:
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
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE109:
	.size	And__uAnd_param_i16_i64, .-And__uAnd_param_i16_i64
	.align 4
	.type	And__uAnd_var_i16_C, #function
	.proc	04
And__uAnd_var_i16_C:
.LLFB110:
	save	%sp, -96, %sp
.LLCFI110:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+96], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE110:
	.size	And__uAnd_var_i16_C, .-And__uAnd_var_i16_C
	.align 4
	.type	And__uAnd_param_i16_C, #function
	.proc	04
And__uAnd_param_i16_C:
.LLFB111:
	save	%sp, -96, %sp
.LLCFI111:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE111:
	.size	And__uAnd_param_i16_C, .-And__uAnd_param_i16_C
	.align 4
	.type	And__uAnd_var_i16_u8, #function
	.proc	04
And__uAnd_var_i16_u8:
.LLFB112:
	save	%sp, -96, %sp
.LLCFI112:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE112:
	.size	And__uAnd_var_i16_u8, .-And__uAnd_var_i16_u8
	.align 4
	.type	And__uAnd_param_i16_u8, #function
	.proc	04
And__uAnd_param_i16_u8:
.LLFB113:
	save	%sp, -96, %sp
.LLCFI113:
	sth	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE113:
	.size	And__uAnd_param_i16_u8, .-And__uAnd_param_i16_u8
	.align 4
	.type	And__uAnd_var_i16_L, #function
	.proc	05
And__uAnd_var_i16_L:
.LLFB114:
	save	%sp, -96, %sp
.LLCFI114:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+120], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE114:
	.size	And__uAnd_var_i16_L, .-And__uAnd_var_i16_L
	.align 4
	.type	And__uAnd_param_i16_L, #function
	.proc	05
And__uAnd_param_i16_L:
.LLFB115:
	save	%sp, -104, %sp
.LLCFI115:
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
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE115:
	.size	And__uAnd_param_i16_L, .-And__uAnd_param_i16_L
	.align 4
	.type	And__uAnd_var_i16_i8, #function
	.proc	04
And__uAnd_var_i16_i8:
.LLFB116:
	save	%sp, -96, %sp
.LLCFI116:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE116:
	.size	And__uAnd_var_i16_i8, .-And__uAnd_var_i16_i8
	.align 4
	.type	And__uAnd_param_i16_i8, #function
	.proc	04
And__uAnd_param_i16_i8:
.LLFB117:
	save	%sp, -96, %sp
.LLCFI117:
	sth	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE117:
	.size	And__uAnd_param_i16_i8, .-And__uAnd_param_i16_i8
	.align 4
	.type	And__uAnd_var_i16_u32, #function
	.proc	04
And__uAnd_var_i16_u32:
.LLFB118:
	save	%sp, -96, %sp
.LLCFI118:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+132], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE118:
	.size	And__uAnd_var_i16_u32, .-And__uAnd_var_i16_u32
	.align 4
	.type	And__uAnd_param_i16_u32, #function
	.proc	04
And__uAnd_param_i16_u32:
.LLFB119:
	save	%sp, -96, %sp
.LLCFI119:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE119:
	.size	And__uAnd_param_i16_u32, .-And__uAnd_param_i16_u32
	.align 4
	.type	And__uAnd_var_I_u16, #function
	.proc	04
And__uAnd_var_I_u16:
.LLFB120:
	save	%sp, -96, %sp
.LLCFI120:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE120:
	.size	And__uAnd_var_I_u16, .-And__uAnd_var_I_u16
	.align 4
	.type	And__uAnd_param_I_u16, #function
	.proc	04
And__uAnd_param_I_u16:
.LLFB121:
	save	%sp, -96, %sp
.LLCFI121:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ld	[%fp+68], %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE121:
	.size	And__uAnd_param_I_u16, .-And__uAnd_param_I_u16
	.align 4
	.type	And__uAnd_var_I_u64, #function
	.proc	05
And__uAnd_var_I_u64:
.LLFB122:
	save	%sp, -96, %sp
.LLCFI122:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+56], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE122:
	.size	And__uAnd_var_I_u64, .-And__uAnd_var_I_u64
	.align 4
	.type	And__uAnd_param_I_u64, #function
	.proc	05
And__uAnd_param_I_u64:
.LLFB123:
	save	%sp, -104, %sp
.LLCFI123:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE123:
	.size	And__uAnd_param_I_u64, .-And__uAnd_param_I_u64
	.align 4
	.type	And__uAnd_var_I_LC, #function
	.proc	05
And__uAnd_var_I_LC:
.LLFB124:
	save	%sp, -96, %sp
.LLCFI124:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+64], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE124:
	.size	And__uAnd_var_I_LC, .-And__uAnd_var_I_LC
	.align 4
	.type	And__uAnd_param_I_LC, #function
	.proc	05
And__uAnd_param_I_LC:
.LLFB125:
	save	%sp, -104, %sp
.LLCFI125:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE125:
	.size	And__uAnd_param_I_LC, .-And__uAnd_param_I_LC
	.align 4
	.type	And__uAnd_var_I_i32, #function
	.proc	04
And__uAnd_var_I_i32:
.LLFB126:
	save	%sp, -96, %sp
.LLCFI126:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+72], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE126:
	.size	And__uAnd_var_I_i32, .-And__uAnd_var_I_i32
	.align 4
	.type	And__uAnd_param_I_i32, #function
	.proc	04
And__uAnd_param_I_i32:
.LLFB127:
	save	%sp, -96, %sp
.LLCFI127:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE127:
	.size	And__uAnd_param_I_i32, .-And__uAnd_param_I_i32
	.align 4
	.type	And__uAnd_var_I_i16, #function
	.proc	04
And__uAnd_var_I_i16:
.LLFB128:
	save	%sp, -96, %sp
.LLCFI128:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE128:
	.size	And__uAnd_var_I_i16, .-And__uAnd_var_I_i16
	.align 4
	.type	And__uAnd_param_I_i16, #function
	.proc	04
And__uAnd_param_I_i16:
.LLFB129:
	save	%sp, -96, %sp
.LLCFI129:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ld	[%fp+68], %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE129:
	.size	And__uAnd_param_I_i16, .-And__uAnd_param_I_i16
	.align 4
	.type	And__uAnd_var_I_I, #function
	.proc	04
And__uAnd_var_I_I:
.LLFB130:
	save	%sp, -96, %sp
.LLCFI130:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+80], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE130:
	.size	And__uAnd_var_I_I, .-And__uAnd_var_I_I
	.align 4
	.type	And__uAnd_param_I_I, #function
	.proc	04
And__uAnd_param_I_I:
.LLFB131:
	save	%sp, -96, %sp
.LLCFI131:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE131:
	.size	And__uAnd_param_I_I, .-And__uAnd_param_I_I
	.align 4
	.type	And__uAnd_var_I_i64, #function
	.proc	05
And__uAnd_var_I_i64:
.LLFB132:
	save	%sp, -96, %sp
.LLCFI132:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+88], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE132:
	.size	And__uAnd_var_I_i64, .-And__uAnd_var_I_i64
	.align 4
	.type	And__uAnd_param_I_i64, #function
	.proc	05
And__uAnd_param_I_i64:
.LLFB133:
	save	%sp, -104, %sp
.LLCFI133:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE133:
	.size	And__uAnd_param_I_i64, .-And__uAnd_param_I_i64
	.align 4
	.type	And__uAnd_var_I_C, #function
	.proc	04
And__uAnd_var_I_C:
.LLFB134:
	save	%sp, -96, %sp
.LLCFI134:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+96], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE134:
	.size	And__uAnd_var_I_C, .-And__uAnd_var_I_C
	.align 4
	.type	And__uAnd_param_I_C, #function
	.proc	04
And__uAnd_param_I_C:
.LLFB135:
	save	%sp, -96, %sp
.LLCFI135:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE135:
	.size	And__uAnd_param_I_C, .-And__uAnd_param_I_C
	.align 4
	.type	And__uAnd_var_I_u8, #function
	.proc	04
And__uAnd_var_I_u8:
.LLFB136:
	save	%sp, -96, %sp
.LLCFI136:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE136:
	.size	And__uAnd_var_I_u8, .-And__uAnd_var_I_u8
	.align 4
	.type	And__uAnd_param_I_u8, #function
	.proc	04
And__uAnd_param_I_u8:
.LLFB137:
	save	%sp, -96, %sp
.LLCFI137:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE137:
	.size	And__uAnd_param_I_u8, .-And__uAnd_param_I_u8
	.align 4
	.type	And__uAnd_var_I_L, #function
	.proc	05
And__uAnd_var_I_L:
.LLFB138:
	save	%sp, -96, %sp
.LLCFI138:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+120], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE138:
	.size	And__uAnd_var_I_L, .-And__uAnd_var_I_L
	.align 4
	.type	And__uAnd_param_I_L, #function
	.proc	05
And__uAnd_param_I_L:
.LLFB139:
	save	%sp, -104, %sp
.LLCFI139:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE139:
	.size	And__uAnd_param_I_L, .-And__uAnd_param_I_L
	.align 4
	.type	And__uAnd_var_I_i8, #function
	.proc	04
And__uAnd_var_I_i8:
.LLFB140:
	save	%sp, -96, %sp
.LLCFI140:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE140:
	.size	And__uAnd_var_I_i8, .-And__uAnd_var_I_i8
	.align 4
	.type	And__uAnd_param_I_i8, #function
	.proc	04
And__uAnd_param_I_i8:
.LLFB141:
	save	%sp, -96, %sp
.LLCFI141:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE141:
	.size	And__uAnd_param_I_i8, .-And__uAnd_param_I_i8
	.align 4
	.type	And__uAnd_var_I_u32, #function
	.proc	04
And__uAnd_var_I_u32:
.LLFB142:
	save	%sp, -96, %sp
.LLCFI142:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+132], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE142:
	.size	And__uAnd_var_I_u32, .-And__uAnd_var_I_u32
	.align 4
	.type	And__uAnd_param_I_u32, #function
	.proc	04
And__uAnd_param_I_u32:
.LLFB143:
	save	%sp, -96, %sp
.LLCFI143:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE143:
	.size	And__uAnd_param_I_u32, .-And__uAnd_param_I_u32
	.align 4
	.type	And__uAnd_var_i64_u16, #function
	.proc	05
And__uAnd_var_i64_u16:
.LLFB144:
	save	%sp, -96, %sp
.LLCFI144:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+88], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE144:
	.size	And__uAnd_var_i64_u16, .-And__uAnd_var_i64_u16
	.align 4
	.type	And__uAnd_param_i64_u16, #function
	.proc	05
And__uAnd_param_i64_u16:
.LLFB145:
	save	%sp, -104, %sp
.LLCFI145:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE145:
	.size	And__uAnd_param_i64_u16, .-And__uAnd_param_i64_u16
	.align 4
	.type	And__uAnd_var_i64_u64, #function
	.proc	05
And__uAnd_var_i64_u64:
.LLFB146:
	save	%sp, -96, %sp
.LLCFI146:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+88], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+56], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE146:
	.size	And__uAnd_var_i64_u64, .-And__uAnd_var_i64_u64
	.align 4
	.type	And__uAnd_param_i64_u64, #function
	.proc	05
And__uAnd_param_i64_u64:
.LLFB147:
	save	%sp, -112, %sp
.LLCFI147:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE147:
	.size	And__uAnd_param_i64_u64, .-And__uAnd_param_i64_u64
	.align 4
	.type	And__uAnd_var_i64_LC, #function
	.proc	05
And__uAnd_var_i64_LC:
.LLFB148:
	save	%sp, -96, %sp
.LLCFI148:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+88], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+64], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE148:
	.size	And__uAnd_var_i64_LC, .-And__uAnd_var_i64_LC
	.align 4
	.type	And__uAnd_param_i64_LC, #function
	.proc	05
And__uAnd_param_i64_LC:
.LLFB149:
	save	%sp, -112, %sp
.LLCFI149:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE149:
	.size	And__uAnd_param_i64_LC, .-And__uAnd_param_i64_LC
	.align 4
	.type	And__uAnd_var_i64_i32, #function
	.proc	05
And__uAnd_var_i64_i32:
.LLFB150:
	save	%sp, -96, %sp
.LLCFI150:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+88], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE150:
	.size	And__uAnd_var_i64_i32, .-And__uAnd_var_i64_i32
	.align 4
	.type	And__uAnd_param_i64_i32, #function
	.proc	05
And__uAnd_param_i64_i32:
.LLFB151:
	save	%sp, -104, %sp
.LLCFI151:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE151:
	.size	And__uAnd_param_i64_i32, .-And__uAnd_param_i64_i32
	.align 4
	.type	And__uAnd_var_i64_i16, #function
	.proc	05
And__uAnd_var_i64_i16:
.LLFB152:
	save	%sp, -96, %sp
.LLCFI152:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+88], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE152:
	.size	And__uAnd_var_i64_i16, .-And__uAnd_var_i64_i16
	.align 4
	.type	And__uAnd_param_i64_i16, #function
	.proc	05
And__uAnd_param_i64_i16:
.LLFB153:
	save	%sp, -104, %sp
.LLCFI153:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE153:
	.size	And__uAnd_param_i64_i16, .-And__uAnd_param_i64_i16
	.align 4
	.type	And__uAnd_var_i64_I, #function
	.proc	05
And__uAnd_var_i64_I:
.LLFB154:
	save	%sp, -96, %sp
.LLCFI154:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+88], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE154:
	.size	And__uAnd_var_i64_I, .-And__uAnd_var_i64_I
	.align 4
	.type	And__uAnd_param_i64_I, #function
	.proc	05
And__uAnd_param_i64_I:
.LLFB155:
	save	%sp, -104, %sp
.LLCFI155:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE155:
	.size	And__uAnd_param_i64_I, .-And__uAnd_param_i64_I
	.align 4
	.type	And__uAnd_var_i64_i64, #function
	.proc	05
And__uAnd_var_i64_i64:
.LLFB156:
	save	%sp, -96, %sp
.LLCFI156:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+88], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+88], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE156:
	.size	And__uAnd_var_i64_i64, .-And__uAnd_var_i64_i64
	.align 4
	.type	And__uAnd_param_i64_i64, #function
	.proc	05
And__uAnd_param_i64_i64:
.LLFB157:
	save	%sp, -112, %sp
.LLCFI157:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE157:
	.size	And__uAnd_param_i64_i64, .-And__uAnd_param_i64_i64
	.align 4
	.type	And__uAnd_var_i64_C, #function
	.proc	05
And__uAnd_var_i64_C:
.LLFB158:
	save	%sp, -96, %sp
.LLCFI158:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+88], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE158:
	.size	And__uAnd_var_i64_C, .-And__uAnd_var_i64_C
	.align 4
	.type	And__uAnd_param_i64_C, #function
	.proc	05
And__uAnd_param_i64_C:
.LLFB159:
	save	%sp, -104, %sp
.LLCFI159:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE159:
	.size	And__uAnd_param_i64_C, .-And__uAnd_param_i64_C
	.align 4
	.type	And__uAnd_var_i64_u8, #function
	.proc	05
And__uAnd_var_i64_u8:
.LLFB160:
	save	%sp, -96, %sp
.LLCFI160:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+88], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE160:
	.size	And__uAnd_var_i64_u8, .-And__uAnd_var_i64_u8
	.align 4
	.type	And__uAnd_param_i64_u8, #function
	.proc	05
And__uAnd_param_i64_u8:
.LLFB161:
	save	%sp, -104, %sp
.LLCFI161:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE161:
	.size	And__uAnd_param_i64_u8, .-And__uAnd_param_i64_u8
	.align 4
	.type	And__uAnd_var_i64_L, #function
	.proc	05
And__uAnd_var_i64_L:
.LLFB162:
	save	%sp, -96, %sp
.LLCFI162:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+88], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+120], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE162:
	.size	And__uAnd_var_i64_L, .-And__uAnd_var_i64_L
	.align 4
	.type	And__uAnd_param_i64_L, #function
	.proc	05
And__uAnd_param_i64_L:
.LLFB163:
	save	%sp, -112, %sp
.LLCFI163:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE163:
	.size	And__uAnd_param_i64_L, .-And__uAnd_param_i64_L
	.align 4
	.type	And__uAnd_var_i64_i8, #function
	.proc	05
And__uAnd_var_i64_i8:
.LLFB164:
	save	%sp, -96, %sp
.LLCFI164:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+88], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE164:
	.size	And__uAnd_var_i64_i8, .-And__uAnd_var_i64_i8
	.align 4
	.type	And__uAnd_param_i64_i8, #function
	.proc	05
And__uAnd_param_i64_i8:
.LLFB165:
	save	%sp, -104, %sp
.LLCFI165:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE165:
	.size	And__uAnd_param_i64_i8, .-And__uAnd_param_i64_i8
	.align 4
	.type	And__uAnd_var_i64_u32, #function
	.proc	05
And__uAnd_var_i64_u32:
.LLFB166:
	save	%sp, -96, %sp
.LLCFI166:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+88], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE166:
	.size	And__uAnd_var_i64_u32, .-And__uAnd_var_i64_u32
	.align 4
	.type	And__uAnd_param_i64_u32, #function
	.proc	05
And__uAnd_param_i64_u32:
.LLFB167:
	save	%sp, -104, %sp
.LLCFI167:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE167:
	.size	And__uAnd_param_i64_u32, .-And__uAnd_param_i64_u32
	.align 4
	.type	And__uAnd_var_C_u16, #function
	.proc	04
And__uAnd_var_C_u16:
.LLFB168:
	save	%sp, -96, %sp
.LLCFI168:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE168:
	.size	And__uAnd_var_C_u16, .-And__uAnd_var_C_u16
	.align 4
	.type	And__uAnd_param_C_u16, #function
	.proc	04
And__uAnd_param_C_u16:
.LLFB169:
	save	%sp, -96, %sp
.LLCFI169:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE169:
	.size	And__uAnd_param_C_u16, .-And__uAnd_param_C_u16
	.align 4
	.type	And__uAnd_var_C_u64, #function
	.proc	05
And__uAnd_var_C_u64:
.LLFB170:
	save	%sp, -96, %sp
.LLCFI170:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+56], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE170:
	.size	And__uAnd_var_C_u64, .-And__uAnd_var_C_u64
	.align 4
	.type	And__uAnd_param_C_u64, #function
	.proc	05
And__uAnd_param_C_u64:
.LLFB171:
	save	%sp, -104, %sp
.LLCFI171:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE171:
	.size	And__uAnd_param_C_u64, .-And__uAnd_param_C_u64
	.align 4
	.type	And__uAnd_var_C_LC, #function
	.proc	05
And__uAnd_var_C_LC:
.LLFB172:
	save	%sp, -96, %sp
.LLCFI172:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+64], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE172:
	.size	And__uAnd_var_C_LC, .-And__uAnd_var_C_LC
	.align 4
	.type	And__uAnd_param_C_LC, #function
	.proc	05
And__uAnd_param_C_LC:
.LLFB173:
	save	%sp, -104, %sp
.LLCFI173:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE173:
	.size	And__uAnd_param_C_LC, .-And__uAnd_param_C_LC
	.align 4
	.type	And__uAnd_var_C_i32, #function
	.proc	04
And__uAnd_var_C_i32:
.LLFB174:
	save	%sp, -96, %sp
.LLCFI174:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+72], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE174:
	.size	And__uAnd_var_C_i32, .-And__uAnd_var_C_i32
	.align 4
	.type	And__uAnd_param_C_i32, #function
	.proc	04
And__uAnd_param_C_i32:
.LLFB175:
	save	%sp, -96, %sp
.LLCFI175:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE175:
	.size	And__uAnd_param_C_i32, .-And__uAnd_param_C_i32
	.align 4
	.type	And__uAnd_var_C_i16, #function
	.proc	04
And__uAnd_var_C_i16:
.LLFB176:
	save	%sp, -96, %sp
.LLCFI176:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE176:
	.size	And__uAnd_var_C_i16, .-And__uAnd_var_C_i16
	.align 4
	.type	And__uAnd_param_C_i16, #function
	.proc	04
And__uAnd_param_C_i16:
.LLFB177:
	save	%sp, -96, %sp
.LLCFI177:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE177:
	.size	And__uAnd_param_C_i16, .-And__uAnd_param_C_i16
	.align 4
	.type	And__uAnd_var_C_I, #function
	.proc	04
And__uAnd_var_C_I:
.LLFB178:
	save	%sp, -96, %sp
.LLCFI178:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+80], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE178:
	.size	And__uAnd_var_C_I, .-And__uAnd_var_C_I
	.align 4
	.type	And__uAnd_param_C_I, #function
	.proc	04
And__uAnd_param_C_I:
.LLFB179:
	save	%sp, -96, %sp
.LLCFI179:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE179:
	.size	And__uAnd_param_C_I, .-And__uAnd_param_C_I
	.align 4
	.type	And__uAnd_var_C_i64, #function
	.proc	05
And__uAnd_var_C_i64:
.LLFB180:
	save	%sp, -96, %sp
.LLCFI180:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+88], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE180:
	.size	And__uAnd_var_C_i64, .-And__uAnd_var_C_i64
	.align 4
	.type	And__uAnd_param_C_i64, #function
	.proc	05
And__uAnd_param_C_i64:
.LLFB181:
	save	%sp, -104, %sp
.LLCFI181:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE181:
	.size	And__uAnd_param_C_i64, .-And__uAnd_param_C_i64
	.align 4
	.type	And__uAnd_var_C_C, #function
	.proc	04
And__uAnd_var_C_C:
.LLFB182:
	save	%sp, -96, %sp
.LLCFI182:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+96], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE182:
	.size	And__uAnd_var_C_C, .-And__uAnd_var_C_C
	.align 4
	.type	And__uAnd_param_C_C, #function
	.proc	04
And__uAnd_param_C_C:
.LLFB183:
	save	%sp, -96, %sp
.LLCFI183:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE183:
	.size	And__uAnd_param_C_C, .-And__uAnd_param_C_C
	.align 4
	.type	And__uAnd_var_C_u8, #function
	.proc	04
And__uAnd_var_C_u8:
.LLFB184:
	save	%sp, -96, %sp
.LLCFI184:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE184:
	.size	And__uAnd_var_C_u8, .-And__uAnd_var_C_u8
	.align 4
	.type	And__uAnd_param_C_u8, #function
	.proc	04
And__uAnd_param_C_u8:
.LLFB185:
	save	%sp, -96, %sp
.LLCFI185:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE185:
	.size	And__uAnd_param_C_u8, .-And__uAnd_param_C_u8
	.align 4
	.type	And__uAnd_var_C_L, #function
	.proc	05
And__uAnd_var_C_L:
.LLFB186:
	save	%sp, -96, %sp
.LLCFI186:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+120], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE186:
	.size	And__uAnd_var_C_L, .-And__uAnd_var_C_L
	.align 4
	.type	And__uAnd_param_C_L, #function
	.proc	05
And__uAnd_param_C_L:
.LLFB187:
	save	%sp, -104, %sp
.LLCFI187:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE187:
	.size	And__uAnd_param_C_L, .-And__uAnd_param_C_L
	.align 4
	.type	And__uAnd_var_C_i8, #function
	.proc	04
And__uAnd_var_C_i8:
.LLFB188:
	save	%sp, -96, %sp
.LLCFI188:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE188:
	.size	And__uAnd_var_C_i8, .-And__uAnd_var_C_i8
	.align 4
	.type	And__uAnd_param_C_i8, #function
	.proc	04
And__uAnd_param_C_i8:
.LLFB189:
	save	%sp, -96, %sp
.LLCFI189:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE189:
	.size	And__uAnd_param_C_i8, .-And__uAnd_param_C_i8
	.align 4
	.type	And__uAnd_var_C_u32, #function
	.proc	04
And__uAnd_var_C_u32:
.LLFB190:
	save	%sp, -96, %sp
.LLCFI190:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+132], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE190:
	.size	And__uAnd_var_C_u32, .-And__uAnd_var_C_u32
	.align 4
	.type	And__uAnd_param_C_u32, #function
	.proc	04
And__uAnd_param_C_u32:
.LLFB191:
	save	%sp, -96, %sp
.LLCFI191:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE191:
	.size	And__uAnd_param_C_u32, .-And__uAnd_param_C_u32
	.align 4
	.type	And__uAnd_var_u8_u16, #function
	.proc	04
And__uAnd_var_u8_u16:
.LLFB192:
	save	%sp, -96, %sp
.LLCFI192:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE192:
	.size	And__uAnd_var_u8_u16, .-And__uAnd_var_u8_u16
	.align 4
	.type	And__uAnd_param_u8_u16, #function
	.proc	04
And__uAnd_param_u8_u16:
.LLFB193:
	save	%sp, -96, %sp
.LLCFI193:
	stb	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE193:
	.size	And__uAnd_param_u8_u16, .-And__uAnd_param_u8_u16
	.align 4
	.type	And__uAnd_var_u8_u64, #function
	.proc	05
And__uAnd_var_u8_u64:
.LLFB194:
	save	%sp, -96, %sp
.LLCFI194:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+56], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE194:
	.size	And__uAnd_var_u8_u64, .-And__uAnd_var_u8_u64
	.align 4
	.type	And__uAnd_param_u8_u64, #function
	.proc	05
And__uAnd_param_u8_u64:
.LLFB195:
	save	%sp, -104, %sp
.LLCFI195:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE195:
	.size	And__uAnd_param_u8_u64, .-And__uAnd_param_u8_u64
	.align 4
	.type	And__uAnd_var_u8_LC, #function
	.proc	05
And__uAnd_var_u8_LC:
.LLFB196:
	save	%sp, -96, %sp
.LLCFI196:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+64], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE196:
	.size	And__uAnd_var_u8_LC, .-And__uAnd_var_u8_LC
	.align 4
	.type	And__uAnd_param_u8_LC, #function
	.proc	05
And__uAnd_param_u8_LC:
.LLFB197:
	save	%sp, -104, %sp
.LLCFI197:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE197:
	.size	And__uAnd_param_u8_LC, .-And__uAnd_param_u8_LC
	.align 4
	.type	And__uAnd_var_u8_i32, #function
	.proc	04
And__uAnd_var_u8_i32:
.LLFB198:
	save	%sp, -96, %sp
.LLCFI198:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+72], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE198:
	.size	And__uAnd_var_u8_i32, .-And__uAnd_var_u8_i32
	.align 4
	.type	And__uAnd_param_u8_i32, #function
	.proc	04
And__uAnd_param_u8_i32:
.LLFB199:
	save	%sp, -96, %sp
.LLCFI199:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE199:
	.size	And__uAnd_param_u8_i32, .-And__uAnd_param_u8_i32
	.align 4
	.type	And__uAnd_var_u8_i16, #function
	.proc	04
And__uAnd_var_u8_i16:
.LLFB200:
	save	%sp, -96, %sp
.LLCFI200:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE200:
	.size	And__uAnd_var_u8_i16, .-And__uAnd_var_u8_i16
	.align 4
	.type	And__uAnd_param_u8_i16, #function
	.proc	04
And__uAnd_param_u8_i16:
.LLFB201:
	save	%sp, -96, %sp
.LLCFI201:
	stb	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE201:
	.size	And__uAnd_param_u8_i16, .-And__uAnd_param_u8_i16
	.align 4
	.type	And__uAnd_var_u8_I, #function
	.proc	04
And__uAnd_var_u8_I:
.LLFB202:
	save	%sp, -96, %sp
.LLCFI202:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+80], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE202:
	.size	And__uAnd_var_u8_I, .-And__uAnd_var_u8_I
	.align 4
	.type	And__uAnd_param_u8_I, #function
	.proc	04
And__uAnd_param_u8_I:
.LLFB203:
	save	%sp, -96, %sp
.LLCFI203:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE203:
	.size	And__uAnd_param_u8_I, .-And__uAnd_param_u8_I
	.align 4
	.type	And__uAnd_var_u8_i64, #function
	.proc	05
And__uAnd_var_u8_i64:
.LLFB204:
	save	%sp, -96, %sp
.LLCFI204:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+88], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE204:
	.size	And__uAnd_var_u8_i64, .-And__uAnd_var_u8_i64
	.align 4
	.type	And__uAnd_param_u8_i64, #function
	.proc	05
And__uAnd_param_u8_i64:
.LLFB205:
	save	%sp, -104, %sp
.LLCFI205:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE205:
	.size	And__uAnd_param_u8_i64, .-And__uAnd_param_u8_i64
	.align 4
	.type	And__uAnd_var_u8_C, #function
	.proc	04
And__uAnd_var_u8_C:
.LLFB206:
	save	%sp, -96, %sp
.LLCFI206:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+96], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE206:
	.size	And__uAnd_var_u8_C, .-And__uAnd_var_u8_C
	.align 4
	.type	And__uAnd_param_u8_C, #function
	.proc	04
And__uAnd_param_u8_C:
.LLFB207:
	save	%sp, -96, %sp
.LLCFI207:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE207:
	.size	And__uAnd_param_u8_C, .-And__uAnd_param_u8_C
	.align 4
	.type	And__uAnd_var_u8_u8, #function
	.proc	04
And__uAnd_var_u8_u8:
.LLFB208:
	save	%sp, -96, %sp
.LLCFI208:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE208:
	.size	And__uAnd_var_u8_u8, .-And__uAnd_var_u8_u8
	.align 4
	.type	And__uAnd_param_u8_u8, #function
	.proc	04
And__uAnd_param_u8_u8:
.LLFB209:
	save	%sp, -96, %sp
.LLCFI209:
	stb	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE209:
	.size	And__uAnd_param_u8_u8, .-And__uAnd_param_u8_u8
	.align 4
	.type	And__uAnd_var_u8_L, #function
	.proc	05
And__uAnd_var_u8_L:
.LLFB210:
	save	%sp, -96, %sp
.LLCFI210:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+120], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE210:
	.size	And__uAnd_var_u8_L, .-And__uAnd_var_u8_L
	.align 4
	.type	And__uAnd_param_u8_L, #function
	.proc	05
And__uAnd_param_u8_L:
.LLFB211:
	save	%sp, -104, %sp
.LLCFI211:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE211:
	.size	And__uAnd_param_u8_L, .-And__uAnd_param_u8_L
	.align 4
	.type	And__uAnd_var_u8_i8, #function
	.proc	04
And__uAnd_var_u8_i8:
.LLFB212:
	save	%sp, -96, %sp
.LLCFI212:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE212:
	.size	And__uAnd_var_u8_i8, .-And__uAnd_var_u8_i8
	.align 4
	.type	And__uAnd_param_u8_i8, #function
	.proc	04
And__uAnd_param_u8_i8:
.LLFB213:
	save	%sp, -96, %sp
.LLCFI213:
	stb	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE213:
	.size	And__uAnd_param_u8_i8, .-And__uAnd_param_u8_i8
	.align 4
	.type	And__uAnd_var_u8_u32, #function
	.proc	04
And__uAnd_var_u8_u32:
.LLFB214:
	save	%sp, -96, %sp
.LLCFI214:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+132], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE214:
	.size	And__uAnd_var_u8_u32, .-And__uAnd_var_u8_u32
	.align 4
	.type	And__uAnd_param_u8_u32, #function
	.proc	04
And__uAnd_param_u8_u32:
.LLFB215:
	save	%sp, -96, %sp
.LLCFI215:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE215:
	.size	And__uAnd_param_u8_u32, .-And__uAnd_param_u8_u32
	.align 4
	.type	And__uAnd_var_L_u16, #function
	.proc	05
And__uAnd_var_L_u16:
.LLFB216:
	save	%sp, -96, %sp
.LLCFI216:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+120], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE216:
	.size	And__uAnd_var_L_u16, .-And__uAnd_var_L_u16
	.align 4
	.type	And__uAnd_param_L_u16, #function
	.proc	05
And__uAnd_param_L_u16:
.LLFB217:
	save	%sp, -104, %sp
.LLCFI217:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE217:
	.size	And__uAnd_param_L_u16, .-And__uAnd_param_L_u16
	.align 4
	.type	And__uAnd_var_L_u64, #function
	.proc	05
And__uAnd_var_L_u64:
.LLFB218:
	save	%sp, -96, %sp
.LLCFI218:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+120], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+56], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE218:
	.size	And__uAnd_var_L_u64, .-And__uAnd_var_L_u64
	.align 4
	.type	And__uAnd_param_L_u64, #function
	.proc	05
And__uAnd_param_L_u64:
.LLFB219:
	save	%sp, -112, %sp
.LLCFI219:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE219:
	.size	And__uAnd_param_L_u64, .-And__uAnd_param_L_u64
	.align 4
	.type	And__uAnd_var_L_LC, #function
	.proc	05
And__uAnd_var_L_LC:
.LLFB220:
	save	%sp, -96, %sp
.LLCFI220:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+120], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+64], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE220:
	.size	And__uAnd_var_L_LC, .-And__uAnd_var_L_LC
	.align 4
	.type	And__uAnd_param_L_LC, #function
	.proc	05
And__uAnd_param_L_LC:
.LLFB221:
	save	%sp, -112, %sp
.LLCFI221:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE221:
	.size	And__uAnd_param_L_LC, .-And__uAnd_param_L_LC
	.align 4
	.type	And__uAnd_var_L_i32, #function
	.proc	05
And__uAnd_var_L_i32:
.LLFB222:
	save	%sp, -96, %sp
.LLCFI222:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+120], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE222:
	.size	And__uAnd_var_L_i32, .-And__uAnd_var_L_i32
	.align 4
	.type	And__uAnd_param_L_i32, #function
	.proc	05
And__uAnd_param_L_i32:
.LLFB223:
	save	%sp, -104, %sp
.LLCFI223:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE223:
	.size	And__uAnd_param_L_i32, .-And__uAnd_param_L_i32
	.align 4
	.type	And__uAnd_var_L_i16, #function
	.proc	05
And__uAnd_var_L_i16:
.LLFB224:
	save	%sp, -96, %sp
.LLCFI224:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+120], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE224:
	.size	And__uAnd_var_L_i16, .-And__uAnd_var_L_i16
	.align 4
	.type	And__uAnd_param_L_i16, #function
	.proc	05
And__uAnd_param_L_i16:
.LLFB225:
	save	%sp, -104, %sp
.LLCFI225:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE225:
	.size	And__uAnd_param_L_i16, .-And__uAnd_param_L_i16
	.align 4
	.type	And__uAnd_var_L_I, #function
	.proc	05
And__uAnd_var_L_I:
.LLFB226:
	save	%sp, -96, %sp
.LLCFI226:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+120], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE226:
	.size	And__uAnd_var_L_I, .-And__uAnd_var_L_I
	.align 4
	.type	And__uAnd_param_L_I, #function
	.proc	05
And__uAnd_param_L_I:
.LLFB227:
	save	%sp, -104, %sp
.LLCFI227:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE227:
	.size	And__uAnd_param_L_I, .-And__uAnd_param_L_I
	.align 4
	.type	And__uAnd_var_L_i64, #function
	.proc	05
And__uAnd_var_L_i64:
.LLFB228:
	save	%sp, -96, %sp
.LLCFI228:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+120], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+88], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE228:
	.size	And__uAnd_var_L_i64, .-And__uAnd_var_L_i64
	.align 4
	.type	And__uAnd_param_L_i64, #function
	.proc	05
And__uAnd_param_L_i64:
.LLFB229:
	save	%sp, -112, %sp
.LLCFI229:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE229:
	.size	And__uAnd_param_L_i64, .-And__uAnd_param_L_i64
	.align 4
	.type	And__uAnd_var_L_C, #function
	.proc	05
And__uAnd_var_L_C:
.LLFB230:
	save	%sp, -96, %sp
.LLCFI230:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+120], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE230:
	.size	And__uAnd_var_L_C, .-And__uAnd_var_L_C
	.align 4
	.type	And__uAnd_param_L_C, #function
	.proc	05
And__uAnd_param_L_C:
.LLFB231:
	save	%sp, -104, %sp
.LLCFI231:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE231:
	.size	And__uAnd_param_L_C, .-And__uAnd_param_L_C
	.align 4
	.type	And__uAnd_var_L_u8, #function
	.proc	05
And__uAnd_var_L_u8:
.LLFB232:
	save	%sp, -96, %sp
.LLCFI232:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+120], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE232:
	.size	And__uAnd_var_L_u8, .-And__uAnd_var_L_u8
	.align 4
	.type	And__uAnd_param_L_u8, #function
	.proc	05
And__uAnd_param_L_u8:
.LLFB233:
	save	%sp, -104, %sp
.LLCFI233:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE233:
	.size	And__uAnd_param_L_u8, .-And__uAnd_param_L_u8
	.align 4
	.type	And__uAnd_var_L_L, #function
	.proc	05
And__uAnd_var_L_L:
.LLFB234:
	save	%sp, -96, %sp
.LLCFI234:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+120], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+120], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE234:
	.size	And__uAnd_var_L_L, .-And__uAnd_var_L_L
	.align 4
	.type	And__uAnd_param_L_L, #function
	.proc	05
And__uAnd_param_L_L:
.LLFB235:
	save	%sp, -112, %sp
.LLCFI235:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE235:
	.size	And__uAnd_param_L_L, .-And__uAnd_param_L_L
	.align 4
	.type	And__uAnd_var_L_i8, #function
	.proc	05
And__uAnd_var_L_i8:
.LLFB236:
	save	%sp, -96, %sp
.LLCFI236:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+120], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE236:
	.size	And__uAnd_var_L_i8, .-And__uAnd_var_L_i8
	.align 4
	.type	And__uAnd_param_L_i8, #function
	.proc	05
And__uAnd_param_L_i8:
.LLFB237:
	save	%sp, -104, %sp
.LLCFI237:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE237:
	.size	And__uAnd_param_L_i8, .-And__uAnd_param_L_i8
	.align 4
	.type	And__uAnd_var_L_u32, #function
	.proc	05
And__uAnd_var_L_u32:
.LLFB238:
	save	%sp, -96, %sp
.LLCFI238:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+120], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE238:
	.size	And__uAnd_var_L_u32, .-And__uAnd_var_L_u32
	.align 4
	.type	And__uAnd_param_L_u32, #function
	.proc	05
And__uAnd_param_L_u32:
.LLFB239:
	save	%sp, -104, %sp
.LLCFI239:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE239:
	.size	And__uAnd_param_L_u32, .-And__uAnd_param_L_u32
	.align 4
	.type	And__uAnd_var_i8_u16, #function
	.proc	04
And__uAnd_var_i8_u16:
.LLFB240:
	save	%sp, -96, %sp
.LLCFI240:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE240:
	.size	And__uAnd_var_i8_u16, .-And__uAnd_var_i8_u16
	.align 4
	.type	And__uAnd_param_i8_u16, #function
	.proc	04
And__uAnd_param_i8_u16:
.LLFB241:
	save	%sp, -96, %sp
.LLCFI241:
	stb	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE241:
	.size	And__uAnd_param_i8_u16, .-And__uAnd_param_i8_u16
	.align 4
	.type	And__uAnd_var_i8_u64, #function
	.proc	05
And__uAnd_var_i8_u64:
.LLFB242:
	save	%sp, -96, %sp
.LLCFI242:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+56], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE242:
	.size	And__uAnd_var_i8_u64, .-And__uAnd_var_i8_u64
	.align 4
	.type	And__uAnd_param_i8_u64, #function
	.proc	05
And__uAnd_param_i8_u64:
.LLFB243:
	save	%sp, -104, %sp
.LLCFI243:
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
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE243:
	.size	And__uAnd_param_i8_u64, .-And__uAnd_param_i8_u64
	.align 4
	.type	And__uAnd_var_i8_LC, #function
	.proc	05
And__uAnd_var_i8_LC:
.LLFB244:
	save	%sp, -96, %sp
.LLCFI244:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+64], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE244:
	.size	And__uAnd_var_i8_LC, .-And__uAnd_var_i8_LC
	.align 4
	.type	And__uAnd_param_i8_LC, #function
	.proc	05
And__uAnd_param_i8_LC:
.LLFB245:
	save	%sp, -104, %sp
.LLCFI245:
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
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE245:
	.size	And__uAnd_param_i8_LC, .-And__uAnd_param_i8_LC
	.align 4
	.type	And__uAnd_var_i8_i32, #function
	.proc	04
And__uAnd_var_i8_i32:
.LLFB246:
	save	%sp, -96, %sp
.LLCFI246:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+72], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE246:
	.size	And__uAnd_var_i8_i32, .-And__uAnd_var_i8_i32
	.align 4
	.type	And__uAnd_param_i8_i32, #function
	.proc	04
And__uAnd_param_i8_i32:
.LLFB247:
	save	%sp, -96, %sp
.LLCFI247:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE247:
	.size	And__uAnd_param_i8_i32, .-And__uAnd_param_i8_i32
	.align 4
	.type	And__uAnd_var_i8_i16, #function
	.proc	04
And__uAnd_var_i8_i16:
.LLFB248:
	save	%sp, -96, %sp
.LLCFI248:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE248:
	.size	And__uAnd_var_i8_i16, .-And__uAnd_var_i8_i16
	.align 4
	.type	And__uAnd_param_i8_i16, #function
	.proc	04
And__uAnd_param_i8_i16:
.LLFB249:
	save	%sp, -96, %sp
.LLCFI249:
	stb	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE249:
	.size	And__uAnd_param_i8_i16, .-And__uAnd_param_i8_i16
	.align 4
	.type	And__uAnd_var_i8_I, #function
	.proc	04
And__uAnd_var_i8_I:
.LLFB250:
	save	%sp, -96, %sp
.LLCFI250:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+80], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE250:
	.size	And__uAnd_var_i8_I, .-And__uAnd_var_i8_I
	.align 4
	.type	And__uAnd_param_i8_I, #function
	.proc	04
And__uAnd_param_i8_I:
.LLFB251:
	save	%sp, -96, %sp
.LLCFI251:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE251:
	.size	And__uAnd_param_i8_I, .-And__uAnd_param_i8_I
	.align 4
	.type	And__uAnd_var_i8_i64, #function
	.proc	05
And__uAnd_var_i8_i64:
.LLFB252:
	save	%sp, -96, %sp
.LLCFI252:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+88], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE252:
	.size	And__uAnd_var_i8_i64, .-And__uAnd_var_i8_i64
	.align 4
	.type	And__uAnd_param_i8_i64, #function
	.proc	05
And__uAnd_param_i8_i64:
.LLFB253:
	save	%sp, -104, %sp
.LLCFI253:
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
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE253:
	.size	And__uAnd_param_i8_i64, .-And__uAnd_param_i8_i64
	.align 4
	.type	And__uAnd_var_i8_C, #function
	.proc	04
And__uAnd_var_i8_C:
.LLFB254:
	save	%sp, -96, %sp
.LLCFI254:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+96], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE254:
	.size	And__uAnd_var_i8_C, .-And__uAnd_var_i8_C
	.align 4
	.type	And__uAnd_param_i8_C, #function
	.proc	04
And__uAnd_param_i8_C:
.LLFB255:
	save	%sp, -96, %sp
.LLCFI255:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE255:
	.size	And__uAnd_param_i8_C, .-And__uAnd_param_i8_C
	.align 4
	.type	And__uAnd_var_i8_u8, #function
	.proc	04
And__uAnd_var_i8_u8:
.LLFB256:
	save	%sp, -96, %sp
.LLCFI256:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE256:
	.size	And__uAnd_var_i8_u8, .-And__uAnd_var_i8_u8
	.align 4
	.type	And__uAnd_param_i8_u8, #function
	.proc	04
And__uAnd_param_i8_u8:
.LLFB257:
	save	%sp, -96, %sp
.LLCFI257:
	stb	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE257:
	.size	And__uAnd_param_i8_u8, .-And__uAnd_param_i8_u8
	.align 4
	.type	And__uAnd_var_i8_L, #function
	.proc	05
And__uAnd_var_i8_L:
.LLFB258:
	save	%sp, -96, %sp
.LLCFI258:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+120], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE258:
	.size	And__uAnd_var_i8_L, .-And__uAnd_var_i8_L
	.align 4
	.type	And__uAnd_param_i8_L, #function
	.proc	05
And__uAnd_param_i8_L:
.LLFB259:
	save	%sp, -104, %sp
.LLCFI259:
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
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE259:
	.size	And__uAnd_param_i8_L, .-And__uAnd_param_i8_L
	.align 4
	.type	And__uAnd_var_i8_i8, #function
	.proc	04
And__uAnd_var_i8_i8:
.LLFB260:
	save	%sp, -96, %sp
.LLCFI260:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE260:
	.size	And__uAnd_var_i8_i8, .-And__uAnd_var_i8_i8
	.align 4
	.type	And__uAnd_param_i8_i8, #function
	.proc	04
And__uAnd_param_i8_i8:
.LLFB261:
	save	%sp, -96, %sp
.LLCFI261:
	stb	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE261:
	.size	And__uAnd_param_i8_i8, .-And__uAnd_param_i8_i8
	.align 4
	.type	And__uAnd_var_i8_u32, #function
	.proc	04
And__uAnd_var_i8_u32:
.LLFB262:
	save	%sp, -96, %sp
.LLCFI262:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+132], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE262:
	.size	And__uAnd_var_i8_u32, .-And__uAnd_var_i8_u32
	.align 4
	.type	And__uAnd_param_i8_u32, #function
	.proc	04
And__uAnd_param_i8_u32:
.LLFB263:
	save	%sp, -96, %sp
.LLCFI263:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE263:
	.size	And__uAnd_param_i8_u32, .-And__uAnd_param_i8_u32
	.align 4
	.type	And__uAnd_var_u32_u16, #function
	.proc	04
And__uAnd_var_u32_u16:
.LLFB264:
	save	%sp, -96, %sp
.LLCFI264:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE264:
	.size	And__uAnd_var_u32_u16, .-And__uAnd_var_u32_u16
	.align 4
	.type	And__uAnd_param_u32_u16, #function
	.proc	04
And__uAnd_param_u32_u16:
.LLFB265:
	save	%sp, -96, %sp
.LLCFI265:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ld	[%fp+68], %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE265:
	.size	And__uAnd_param_u32_u16, .-And__uAnd_param_u32_u16
	.align 4
	.type	And__uAnd_var_u32_u64, #function
	.proc	05
And__uAnd_var_u32_u64:
.LLFB266:
	save	%sp, -96, %sp
.LLCFI266:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+56], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE266:
	.size	And__uAnd_var_u32_u64, .-And__uAnd_var_u32_u64
	.align 4
	.type	And__uAnd_param_u32_u64, #function
	.proc	05
And__uAnd_param_u32_u64:
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
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE267:
	.size	And__uAnd_param_u32_u64, .-And__uAnd_param_u32_u64
	.align 4
	.type	And__uAnd_var_u32_LC, #function
	.proc	05
And__uAnd_var_u32_LC:
.LLFB268:
	save	%sp, -96, %sp
.LLCFI268:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+64], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE268:
	.size	And__uAnd_var_u32_LC, .-And__uAnd_var_u32_LC
	.align 4
	.type	And__uAnd_param_u32_LC, #function
	.proc	05
And__uAnd_param_u32_LC:
.LLFB269:
	save	%sp, -104, %sp
.LLCFI269:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE269:
	.size	And__uAnd_param_u32_LC, .-And__uAnd_param_u32_LC
	.align 4
	.type	And__uAnd_var_u32_i32, #function
	.proc	04
And__uAnd_var_u32_i32:
.LLFB270:
	save	%sp, -96, %sp
.LLCFI270:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+72], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE270:
	.size	And__uAnd_var_u32_i32, .-And__uAnd_var_u32_i32
	.align 4
	.type	And__uAnd_param_u32_i32, #function
	.proc	04
And__uAnd_param_u32_i32:
.LLFB271:
	save	%sp, -96, %sp
.LLCFI271:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE271:
	.size	And__uAnd_param_u32_i32, .-And__uAnd_param_u32_i32
	.align 4
	.type	And__uAnd_var_u32_i16, #function
	.proc	04
And__uAnd_var_u32_i16:
.LLFB272:
	save	%sp, -96, %sp
.LLCFI272:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE272:
	.size	And__uAnd_var_u32_i16, .-And__uAnd_var_u32_i16
	.align 4
	.type	And__uAnd_param_u32_i16, #function
	.proc	04
And__uAnd_param_u32_i16:
.LLFB273:
	save	%sp, -96, %sp
.LLCFI273:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ld	[%fp+68], %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE273:
	.size	And__uAnd_param_u32_i16, .-And__uAnd_param_u32_i16
	.align 4
	.type	And__uAnd_var_u32_I, #function
	.proc	04
And__uAnd_var_u32_I:
.LLFB274:
	save	%sp, -96, %sp
.LLCFI274:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+80], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE274:
	.size	And__uAnd_var_u32_I, .-And__uAnd_var_u32_I
	.align 4
	.type	And__uAnd_param_u32_I, #function
	.proc	04
And__uAnd_param_u32_I:
.LLFB275:
	save	%sp, -96, %sp
.LLCFI275:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE275:
	.size	And__uAnd_param_u32_I, .-And__uAnd_param_u32_I
	.align 4
	.type	And__uAnd_var_u32_i64, #function
	.proc	05
And__uAnd_var_u32_i64:
.LLFB276:
	save	%sp, -96, %sp
.LLCFI276:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+88], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE276:
	.size	And__uAnd_var_u32_i64, .-And__uAnd_var_u32_i64
	.align 4
	.type	And__uAnd_param_u32_i64, #function
	.proc	05
And__uAnd_param_u32_i64:
.LLFB277:
	save	%sp, -104, %sp
.LLCFI277:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE277:
	.size	And__uAnd_param_u32_i64, .-And__uAnd_param_u32_i64
	.align 4
	.type	And__uAnd_var_u32_C, #function
	.proc	04
And__uAnd_var_u32_C:
.LLFB278:
	save	%sp, -96, %sp
.LLCFI278:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+96], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE278:
	.size	And__uAnd_var_u32_C, .-And__uAnd_var_u32_C
	.align 4
	.type	And__uAnd_param_u32_C, #function
	.proc	04
And__uAnd_param_u32_C:
.LLFB279:
	save	%sp, -96, %sp
.LLCFI279:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE279:
	.size	And__uAnd_param_u32_C, .-And__uAnd_param_u32_C
	.align 4
	.type	And__uAnd_var_u32_u8, #function
	.proc	04
And__uAnd_var_u32_u8:
.LLFB280:
	save	%sp, -96, %sp
.LLCFI280:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE280:
	.size	And__uAnd_var_u32_u8, .-And__uAnd_var_u32_u8
	.align 4
	.type	And__uAnd_param_u32_u8, #function
	.proc	04
And__uAnd_param_u32_u8:
.LLFB281:
	save	%sp, -96, %sp
.LLCFI281:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE281:
	.size	And__uAnd_param_u32_u8, .-And__uAnd_param_u32_u8
	.align 4
	.type	And__uAnd_var_u32_L, #function
	.proc	05
And__uAnd_var_u32_L:
.LLFB282:
	save	%sp, -96, %sp
.LLCFI282:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ldd	[%g1+120], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE282:
	.size	And__uAnd_var_u32_L, .-And__uAnd_var_u32_L
	.align 4
	.type	And__uAnd_param_u32_L, #function
	.proc	05
And__uAnd_param_u32_L:
.LLFB283:
	save	%sp, -104, %sp
.LLCFI283:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	and	%o4, %o2, %o4
	and	%o5, %o3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE283:
	.size	And__uAnd_param_u32_L, .-And__uAnd_param_u32_L
	.align 4
	.type	And__uAnd_var_u32_i8, #function
	.proc	04
And__uAnd_var_u32_i8:
.LLFB284:
	save	%sp, -96, %sp
.LLCFI284:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE284:
	.size	And__uAnd_var_u32_i8, .-And__uAnd_var_u32_i8
	.align 4
	.type	And__uAnd_param_u32_i8, #function
	.proc	04
And__uAnd_param_u32_i8:
.LLFB285:
	save	%sp, -96, %sp
.LLCFI285:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE285:
	.size	And__uAnd_param_u32_i8, .-And__uAnd_param_u32_i8
	.align 4
	.type	And__uAnd_var_u32_u32, #function
	.proc	04
And__uAnd_var_u32_u32:
.LLFB286:
	save	%sp, -96, %sp
.LLCFI286:
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	ld	[%g1+132], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE286:
	.size	And__uAnd_var_u32_u32, .-And__uAnd_var_u32_u32
	.align 4
	.type	And__uAnd_param_u32_u32, #function
	.proc	04
And__uAnd_param_u32_u32:
.LLFB287:
	save	%sp, -96, %sp
.LLCFI287:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE287:
	.size	And__uAnd_param_u32_u32, .-And__uAnd_param_u32_u32
	.align 4
	.global And_M3
	.type	And_M3, #function
	.proc	0120
And_M3:
.LLFB288:
	save	%sp, -96, %sp
.LLCFI288:
	st	%i0, [%fp+68]
	sethi	%hi(MM_And), %g1
	or	%g1, %lo(MM_And), %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE288:
	.size	And_M3, .-And_M3
	.section	".data"
	.align 8
	.type	L_1, #object
	.size	L_1, 7096
L_1:
	.ascii	"And_M3"
	.skip 1
	.ascii	"uAnd_param_u32_u32"
	.skip 1
	.ascii	"uAnd_var_u32_u32"
	.skip 1
	.ascii	"uAnd_param_u32_i8"
	.skip 1
	.ascii	"uAnd_var_u32_i8"
	.skip 1
	.ascii	"uAnd_param_u32_L"
	.skip 1
	.ascii	"uAnd_var_u32_L"
	.skip 1
	.ascii	"uAnd_param_u32_u8"
	.skip 1
	.ascii	"uAnd_var_u32_u8"
	.skip 1
	.ascii	"uAnd_param_u32_C"
	.skip 1
	.ascii	"uAnd_var_u32_C"
	.skip 1
	.ascii	"uAnd_param_u32_i64"
	.skip 1
	.ascii	"uAnd_var_u32_i64"
	.skip 1
	.ascii	"uAnd_param_u32_I"
	.skip 1
	.ascii	"uAnd_var_u32_I"
	.skip 1
	.ascii	"uAnd_param_u32_i16"
	.skip 1
	.ascii	"uAnd_var_u32_i16"
	.skip 1
	.ascii	"uAnd_param_u32_i32"
	.skip 1
	.ascii	"uAnd_var_u32_i32"
	.skip 1
	.ascii	"uAnd_param_u32_LC"
	.skip 1
	.ascii	"uAnd_var_u32_LC"
	.skip 1
	.ascii	"uAnd_param_u32_u64"
	.skip 1
	.ascii	"uAnd_var_u32_u64"
	.skip 1
	.ascii	"uAnd_param_u32_u16"
	.skip 1
	.ascii	"uAnd_var_u32_u16"
	.skip 1
	.ascii	"uAnd_param_i8_u32"
	.skip 1
	.ascii	"uAnd_var_i8_u32"
	.skip 1
	.ascii	"uAnd_param_i8_i8"
	.skip 1
	.ascii	"uAnd_var_i8_i8"
	.skip 1
	.ascii	"uAnd_param_i8_L"
	.skip 1
	.ascii	"uAnd_var_i8_L"
	.skip 1
	.ascii	"uAnd_param_i8_u8"
	.skip 1
	.ascii	"uAnd_var_i8_u8"
	.skip 1
	.ascii	"uAnd_param_i8_C"
	.skip 1
	.ascii	"uAnd_var_i8_C"
	.skip 1
	.ascii	"uAnd_param_i8_i64"
	.skip 1
	.ascii	"uAnd_var_i8_i64"
	.skip 1
	.ascii	"uAnd_param_i8_I"
	.skip 1
	.ascii	"uAnd_var_i8_I"
	.skip 1
	.ascii	"uAnd_param_i8_i16"
	.skip 1
	.ascii	"uAnd_var_i8_i16"
	.skip 1
	.ascii	"uAnd_param_i8_i32"
	.skip 1
	.ascii	"uAnd_var_i8_i32"
	.skip 1
	.ascii	"uAnd_param_i8_LC"
	.skip 1
	.ascii	"uAnd_var_i8_LC"
	.skip 1
	.ascii	"uAnd_param_i8_u64"
	.skip 1
	.ascii	"uAnd_var_i8_u64"
	.skip 1
	.ascii	"uAnd_param_i8_u16"
	.skip 1
	.ascii	"uAnd_var_i8_u16"
	.skip 1
	.ascii	"uAnd_param_L_u32"
	.skip 1
	.ascii	"uAnd_var_L_u32"
	.skip 1
	.ascii	"uAnd_param_L_i8"
	.skip 1
	.ascii	"uAnd_var_L_i8"
	.skip 1
	.ascii	"uAnd_param_L_L"
	.skip 1
	.ascii	"uAnd_var_L_L"
	.skip 1
	.ascii	"uAnd_param_L_u8"
	.skip 1
	.ascii	"uAnd_var_L_u8"
	.skip 1
	.ascii	"uAnd_param_L_C"
	.skip 1
	.ascii	"uAnd_var_L_C"
	.skip 1
	.ascii	"uAnd_param_L_i64"
	.skip 1
	.ascii	"uAnd_var_L_i64"
	.skip 1
	.ascii	"uAnd_param_L_I"
	.skip 1
	.ascii	"uAnd_var_L_I"
	.skip 1
	.ascii	"uAnd_param_L_i16"
	.skip 1
	.ascii	"uAnd_var_L_i16"
	.skip 1
	.ascii	"uAnd_param_L_i32"
	.skip 1
	.ascii	"uAnd_var_L_i32"
	.skip 1
	.ascii	"uAnd_param_L_LC"
	.skip 1
	.ascii	"uAnd_var_L_LC"
	.skip 1
	.ascii	"uAnd_param_L_u64"
	.skip 1
	.ascii	"uAnd_var_L_u64"
	.skip 1
	.ascii	"uAnd_param_L_u16"
	.skip 1
	.ascii	"uAnd_var_L_u16"
	.skip 1
	.ascii	"uAnd_param_u8_u32"
	.skip 1
	.ascii	"uAnd_var_u8_u32"
	.skip 1
	.ascii	"uAnd_param_u8_i8"
	.skip 1
	.ascii	"uAnd_var_u8_i8"
	.skip 1
	.ascii	"uAnd_param_u8_L"
	.skip 1
	.ascii	"uAnd_var_u8_L"
	.skip 1
	.ascii	"uAnd_param_u8_u8"
	.skip 1
	.ascii	"uAnd_var_u8_u8"
	.skip 1
	.ascii	"uAnd_param_u8_C"
	.skip 1
	.ascii	"uAnd_var_u8_C"
	.skip 1
	.ascii	"uAnd_param_u8_i64"
	.skip 1
	.ascii	"uAnd_var_u8_i64"
	.skip 1
	.ascii	"uAnd_param_u8_I"
	.skip 1
	.ascii	"uAnd_var_u8_I"
	.skip 1
	.ascii	"uAnd_param_u8_i16"
	.skip 1
	.ascii	"uAnd_var_u8_i16"
	.skip 1
	.ascii	"uAnd_param_u8_i32"
	.skip 1
	.ascii	"uAnd_var_u8_i32"
	.skip 1
	.ascii	"uAnd_param_u8_LC"
	.skip 1
	.ascii	"uAnd_var_u8_LC"
	.skip 1
	.ascii	"uAnd_param_u8_u64"
	.skip 1
	.ascii	"uAnd_var_u8_u64"
	.skip 1
	.ascii	"uAnd_param_u8_u16"
	.skip 1
	.ascii	"uAnd_var_u8_u16"
	.skip 1
	.ascii	"uAnd_param_C_u32"
	.skip 1
	.ascii	"uAnd_var_C_u32"
	.skip 1
	.ascii	"uAnd_param_C_i8"
	.skip 1
	.ascii	"uAnd_var_C_i8"
	.skip 1
	.ascii	"uAnd_param_C_L"
	.skip 1
	.ascii	"uAnd_var_C_L"
	.skip 1
	.ascii	"uAnd_param_C_u8"
	.skip 1
	.ascii	"uAnd_var_C_u8"
	.skip 1
	.ascii	"uAnd_param_C_C"
	.skip 1
	.ascii	"uAnd_var_C_C"
	.skip 1
	.ascii	"uAnd_param_C_i64"
	.skip 1
	.ascii	"uAnd_var_C_i64"
	.skip 1
	.ascii	"uAnd_param_C_I"
	.skip 1
	.ascii	"uAnd_var_C_I"
	.skip 1
	.ascii	"uAnd_param_C_i16"
	.skip 1
	.ascii	"uAnd_var_C_i16"
	.skip 1
	.ascii	"uAnd_param_C_i32"
	.skip 1
	.ascii	"uAnd_var_C_i32"
	.skip 1
	.ascii	"uAnd_param_C_LC"
	.skip 1
	.ascii	"uAnd_var_C_LC"
	.skip 1
	.ascii	"uAnd_param_C_u64"
	.skip 1
	.ascii	"uAnd_var_C_u64"
	.skip 1
	.ascii	"uAnd_param_C_u16"
	.skip 1
	.ascii	"uAnd_var_C_u16"
	.skip 1
	.ascii	"uAnd_param_i64_u32"
	.skip 1
	.ascii	"uAnd_var_i64_u32"
	.skip 1
	.ascii	"uAnd_param_i64_i8"
	.skip 1
	.ascii	"uAnd_var_i64_i8"
	.skip 1
	.ascii	"uAnd_param_i64_L"
	.skip 1
	.ascii	"uAnd_var_i64_L"
	.skip 1
	.ascii	"uAnd_param_i64_u8"
	.skip 1
	.ascii	"uAnd_var_i64_u8"
	.skip 1
	.ascii	"uAnd_param_i64_C"
	.skip 1
	.ascii	"uAnd_var_i64_C"
	.skip 1
	.ascii	"uAnd_param_i64_i64"
	.skip 1
	.ascii	"uAnd_var_i64_i64"
	.skip 1
	.ascii	"uAnd_param_i64_I"
	.skip 1
	.ascii	"uAnd_var_i64_I"
	.skip 1
	.ascii	"uAnd_param_i64_i16"
	.skip 1
	.ascii	"uAnd_var_i64_i16"
	.skip 1
	.ascii	"uAnd_param_i64_i32"
	.skip 1
	.ascii	"uAnd_var_i64_i32"
	.skip 1
	.ascii	"uAnd_param_i64_LC"
	.skip 1
	.ascii	"uAnd_var_i64_LC"
	.skip 1
	.ascii	"uAnd_param_i64_u64"
	.skip 1
	.ascii	"uAnd_var_i64_u64"
	.skip 1
	.ascii	"uAnd_param_i64_u16"
	.skip 1
	.ascii	"uAnd_var_i64_u16"
	.skip 1
	.ascii	"uAnd_param_I_u32"
	.skip 1
	.ascii	"uAnd_var_I_u32"
	.skip 1
	.ascii	"uAnd_param_I_i8"
	.skip 1
	.ascii	"uAnd_var_I_i8"
	.skip 1
	.ascii	"uAnd_param_I_L"
	.skip 1
	.ascii	"uAnd_var_I_L"
	.skip 1
	.ascii	"uAnd_param_I_u8"
	.skip 1
	.ascii	"uAnd_var_I_u8"
	.skip 1
	.ascii	"uAnd_param_I_C"
	.skip 1
	.ascii	"uAnd_var_I_C"
	.skip 1
	.ascii	"uAnd_param_I_i64"
	.skip 1
	.ascii	"uAnd_var_I_i64"
	.skip 1
	.ascii	"uAnd_param_I_I"
	.skip 1
	.ascii	"uAnd_var_I_I"
	.skip 1
	.ascii	"uAnd_param_I_i16"
	.skip 1
	.ascii	"uAnd_var_I_i16"
	.skip 1
	.ascii	"uAnd_param_I_i32"
	.skip 1
	.ascii	"uAnd_var_I_i32"
	.skip 1
	.ascii	"uAnd_param_I_LC"
	.skip 1
	.ascii	"uAnd_var_I_LC"
	.skip 1
	.ascii	"uAnd_param_I_u64"
	.skip 1
	.ascii	"uAnd_var_I_u64"
	.skip 1
	.ascii	"uAnd_param_I_u16"
	.skip 1
	.ascii	"uAnd_var_I_u16"
	.skip 1
	.ascii	"uAnd_param_i16_u32"
	.skip 1
	.ascii	"uAnd_var_i16_u32"
	.skip 1
	.ascii	"uAnd_param_i16_i8"
	.skip 1
	.ascii	"uAnd_var_i16_i8"
	.skip 1
	.ascii	"uAnd_param_i16_L"
	.skip 1
	.ascii	"uAnd_var_i16_L"
	.skip 1
	.ascii	"uAnd_param_i16_u8"
	.skip 1
	.ascii	"uAnd_var_i16_u8"
	.skip 1
	.ascii	"uAnd_param_i16_C"
	.skip 1
	.ascii	"uAnd_var_i16_C"
	.skip 1
	.ascii	"uAnd_param_i16_i64"
	.skip 1
	.ascii	"uAnd_var_i16_i64"
	.skip 1
	.ascii	"uAnd_param_i16_I"
	.skip 1
	.ascii	"uAnd_var_i16_I"
	.skip 1
	.ascii	"uAnd_param_i16_i16"
	.skip 1
	.ascii	"uAnd_var_i16_i16"
	.skip 1
	.ascii	"uAnd_param_i16_i32"
	.skip 1
	.ascii	"uAnd_var_i16_i32"
	.skip 1
	.ascii	"uAnd_param_i16_LC"
	.skip 1
	.ascii	"uAnd_var_i16_LC"
	.skip 1
	.ascii	"uAnd_param_i16_u64"
	.skip 1
	.ascii	"uAnd_var_i16_u64"
	.skip 1
	.ascii	"uAnd_param_i16_u16"
	.skip 1
	.ascii	"uAnd_var_i16_u16"
	.skip 1
	.ascii	"uAnd_param_i32_u32"
	.skip 1
	.ascii	"uAnd_var_i32_u32"
	.skip 1
	.ascii	"uAnd_param_i32_i8"
	.skip 1
	.ascii	"uAnd_var_i32_i8"
	.skip 1
	.ascii	"uAnd_param_i32_L"
	.skip 1
	.ascii	"uAnd_var_i32_L"
	.skip 1
	.ascii	"uAnd_param_i32_u8"
	.skip 1
	.ascii	"uAnd_var_i32_u8"
	.skip 1
	.ascii	"uAnd_param_i32_C"
	.skip 1
	.ascii	"uAnd_var_i32_C"
	.skip 1
	.ascii	"uAnd_param_i32_i64"
	.skip 1
	.ascii	"uAnd_var_i32_i64"
	.skip 1
	.ascii	"uAnd_param_i32_I"
	.skip 1
	.ascii	"uAnd_var_i32_I"
	.skip 1
	.ascii	"uAnd_param_i32_i16"
	.skip 1
	.ascii	"uAnd_var_i32_i16"
	.skip 1
	.ascii	"uAnd_param_i32_i32"
	.skip 1
	.ascii	"uAnd_var_i32_i32"
	.skip 1
	.ascii	"uAnd_param_i32_LC"
	.skip 1
	.ascii	"uAnd_var_i32_LC"
	.skip 1
	.ascii	"uAnd_param_i32_u64"
	.skip 1
	.ascii	"uAnd_var_i32_u64"
	.skip 1
	.ascii	"uAnd_param_i32_u16"
	.skip 1
	.ascii	"uAnd_var_i32_u16"
	.skip 1
	.ascii	"uAnd_param_LC_u32"
	.skip 1
	.ascii	"uAnd_var_LC_u32"
	.skip 1
	.ascii	"uAnd_param_LC_i8"
	.skip 1
	.ascii	"uAnd_var_LC_i8"
	.skip 1
	.ascii	"uAnd_param_LC_L"
	.skip 1
	.ascii	"uAnd_var_LC_L"
	.skip 1
	.ascii	"uAnd_param_LC_u8"
	.skip 1
	.ascii	"uAnd_var_LC_u8"
	.skip 1
	.ascii	"uAnd_param_LC_C"
	.skip 1
	.ascii	"uAnd_var_LC_C"
	.skip 1
	.ascii	"uAnd_param_LC_i64"
	.skip 1
	.ascii	"uAnd_var_LC_i64"
	.skip 1
	.ascii	"uAnd_param_LC_I"
	.skip 1
	.ascii	"uAnd_var_LC_I"
	.skip 1
	.ascii	"uAnd_param_LC_i16"
	.skip 1
	.ascii	"uAnd_var_LC_i16"
	.skip 1
	.ascii	"uAnd_param_LC_i32"
	.skip 1
	.ascii	"uAnd_var_LC_i32"
	.skip 1
	.ascii	"uAnd_param_LC_LC"
	.skip 1
	.ascii	"uAnd_var_LC_LC"
	.skip 1
	.ascii	"uAnd_param_LC_u64"
	.skip 1
	.ascii	"uAnd_var_LC_u64"
	.skip 1
	.ascii	"uAnd_param_LC_u16"
	.skip 1
	.ascii	"uAnd_var_LC_u16"
	.skip 1
	.ascii	"uAnd_param_u64_u32"
	.skip 1
	.ascii	"uAnd_var_u64_u32"
	.skip 1
	.ascii	"uAnd_param_u64_i8"
	.skip 1
	.ascii	"uAnd_var_u64_i8"
	.skip 1
	.ascii	"uAnd_param_u64_L"
	.skip 1
	.ascii	"uAnd_var_u64_L"
	.skip 1
	.ascii	"uAnd_param_u64_u8"
	.skip 1
	.ascii	"uAnd_var_u64_u8"
	.skip 1
	.ascii	"uAnd_param_u64_C"
	.skip 1
	.ascii	"uAnd_var_u64_C"
	.skip 1
	.ascii	"uAnd_param_u64_i64"
	.skip 1
	.ascii	"uAnd_var_u64_i64"
	.skip 1
	.ascii	"uAnd_param_u64_I"
	.skip 1
	.ascii	"uAnd_var_u64_I"
	.skip 1
	.ascii	"uAnd_param_u64_i16"
	.skip 1
	.ascii	"uAnd_var_u64_i16"
	.skip 1
	.ascii	"uAnd_param_u64_i32"
	.skip 1
	.ascii	"uAnd_var_u64_i32"
	.skip 1
	.ascii	"uAnd_param_u64_LC"
	.skip 1
	.ascii	"uAnd_var_u64_LC"
	.skip 1
	.ascii	"uAnd_param_u64_u64"
	.skip 1
	.ascii	"uAnd_var_u64_u64"
	.skip 1
	.ascii	"uAnd_param_u64_u16"
	.skip 1
	.ascii	"uAnd_var_u64_u16"
	.skip 1
	.ascii	"uAnd_param_u16_u32"
	.skip 1
	.ascii	"uAnd_var_u16_u32"
	.skip 1
	.ascii	"uAnd_param_u16_i8"
	.skip 1
	.ascii	"uAnd_var_u16_i8"
	.skip 1
	.ascii	"uAnd_param_u16_L"
	.skip 1
	.ascii	"uAnd_var_u16_L"
	.skip 1
	.ascii	"uAnd_param_u16_u8"
	.skip 1
	.ascii	"uAnd_var_u16_u8"
	.skip 1
	.ascii	"uAnd_param_u16_C"
	.skip 1
	.ascii	"uAnd_var_u16_C"
	.skip 1
	.ascii	"uAnd_param_u16_i64"
	.skip 1
	.ascii	"uAnd_var_u16_i64"
	.skip 1
	.ascii	"uAnd_param_u16_I"
	.skip 1
	.ascii	"uAnd_var_u16_I"
	.skip 1
	.ascii	"uAnd_param_u16_i16"
	.skip 1
	.ascii	"uAnd_var_u16_i16"
	.skip 1
	.ascii	"uAnd_param_u16_i32"
	.skip 1
	.ascii	"uAnd_var_u16_i32"
	.skip 1
	.ascii	"uAnd_param_u16_LC"
	.skip 1
	.ascii	"uAnd_var_u16_LC"
	.skip 1
	.ascii	"uAnd_param_u16_u64"
	.skip 1
	.ascii	"uAnd_var_u16_u64"
	.skip 1
	.ascii	"uAnd_param_u16_u16"
	.skip 1
	.ascii	"uAnd_var_u16_u16"
	.skip 2
	.long	And_M3
	.long	L_1
	.long	And__uAnd_param_u32_u32
	.long	L_1+7
	.long	And__uAnd_var_u32_u32
	.long	L_1+26
	.long	And__uAnd_param_u32_i8
	.long	L_1+43
	.long	And__uAnd_var_u32_i8
	.long	L_1+61
	.long	And__uAnd_param_u32_L
	.long	L_1+77
	.long	And__uAnd_var_u32_L
	.long	L_1+94
	.long	And__uAnd_param_u32_u8
	.long	L_1+109
	.long	And__uAnd_var_u32_u8
	.long	L_1+127
	.long	And__uAnd_param_u32_C
	.long	L_1+143
	.long	And__uAnd_var_u32_C
	.long	L_1+160
	.long	And__uAnd_param_u32_i64
	.long	L_1+175
	.long	And__uAnd_var_u32_i64
	.long	L_1+194
	.long	And__uAnd_param_u32_I
	.long	L_1+211
	.long	And__uAnd_var_u32_I
	.long	L_1+228
	.long	And__uAnd_param_u32_i16
	.long	L_1+243
	.long	And__uAnd_var_u32_i16
	.long	L_1+262
	.long	And__uAnd_param_u32_i32
	.long	L_1+279
	.long	And__uAnd_var_u32_i32
	.long	L_1+298
	.long	And__uAnd_param_u32_LC
	.long	L_1+315
	.long	And__uAnd_var_u32_LC
	.long	L_1+333
	.long	And__uAnd_param_u32_u64
	.long	L_1+349
	.long	And__uAnd_var_u32_u64
	.long	L_1+368
	.long	And__uAnd_param_u32_u16
	.long	L_1+385
	.long	And__uAnd_var_u32_u16
	.long	L_1+404
	.long	And__uAnd_param_i8_u32
	.long	L_1+421
	.long	And__uAnd_var_i8_u32
	.long	L_1+439
	.long	And__uAnd_param_i8_i8
	.long	L_1+455
	.long	And__uAnd_var_i8_i8
	.long	L_1+472
	.long	And__uAnd_param_i8_L
	.long	L_1+487
	.long	And__uAnd_var_i8_L
	.long	L_1+503
	.long	And__uAnd_param_i8_u8
	.long	L_1+517
	.long	And__uAnd_var_i8_u8
	.long	L_1+534
	.long	And__uAnd_param_i8_C
	.long	L_1+549
	.long	And__uAnd_var_i8_C
	.long	L_1+565
	.long	And__uAnd_param_i8_i64
	.long	L_1+579
	.long	And__uAnd_var_i8_i64
	.long	L_1+597
	.long	And__uAnd_param_i8_I
	.long	L_1+613
	.long	And__uAnd_var_i8_I
	.long	L_1+629
	.long	And__uAnd_param_i8_i16
	.long	L_1+643
	.long	And__uAnd_var_i8_i16
	.long	L_1+661
	.long	And__uAnd_param_i8_i32
	.long	L_1+677
	.long	And__uAnd_var_i8_i32
	.long	L_1+695
	.long	And__uAnd_param_i8_LC
	.long	L_1+711
	.long	And__uAnd_var_i8_LC
	.long	L_1+728
	.long	And__uAnd_param_i8_u64
	.long	L_1+743
	.long	And__uAnd_var_i8_u64
	.long	L_1+761
	.long	And__uAnd_param_i8_u16
	.long	L_1+777
	.long	And__uAnd_var_i8_u16
	.long	L_1+795
	.long	And__uAnd_param_L_u32
	.long	L_1+811
	.long	And__uAnd_var_L_u32
	.long	L_1+828
	.long	And__uAnd_param_L_i8
	.long	L_1+843
	.long	And__uAnd_var_L_i8
	.long	L_1+859
	.long	And__uAnd_param_L_L
	.long	L_1+873
	.long	And__uAnd_var_L_L
	.long	L_1+888
	.long	And__uAnd_param_L_u8
	.long	L_1+901
	.long	And__uAnd_var_L_u8
	.long	L_1+917
	.long	And__uAnd_param_L_C
	.long	L_1+931
	.long	And__uAnd_var_L_C
	.long	L_1+946
	.long	And__uAnd_param_L_i64
	.long	L_1+959
	.long	And__uAnd_var_L_i64
	.long	L_1+976
	.long	And__uAnd_param_L_I
	.long	L_1+991
	.long	And__uAnd_var_L_I
	.long	L_1+1006
	.long	And__uAnd_param_L_i16
	.long	L_1+1019
	.long	And__uAnd_var_L_i16
	.long	L_1+1036
	.long	And__uAnd_param_L_i32
	.long	L_1+1051
	.long	And__uAnd_var_L_i32
	.long	L_1+1068
	.long	And__uAnd_param_L_LC
	.long	L_1+1083
	.long	And__uAnd_var_L_LC
	.long	L_1+1099
	.long	And__uAnd_param_L_u64
	.long	L_1+1113
	.long	And__uAnd_var_L_u64
	.long	L_1+1130
	.long	And__uAnd_param_L_u16
	.long	L_1+1145
	.long	And__uAnd_var_L_u16
	.long	L_1+1162
	.long	And__uAnd_param_u8_u32
	.long	L_1+1177
	.long	And__uAnd_var_u8_u32
	.long	L_1+1195
	.long	And__uAnd_param_u8_i8
	.long	L_1+1211
	.long	And__uAnd_var_u8_i8
	.long	L_1+1228
	.long	And__uAnd_param_u8_L
	.long	L_1+1243
	.long	And__uAnd_var_u8_L
	.long	L_1+1259
	.long	And__uAnd_param_u8_u8
	.long	L_1+1273
	.long	And__uAnd_var_u8_u8
	.long	L_1+1290
	.long	And__uAnd_param_u8_C
	.long	L_1+1305
	.long	And__uAnd_var_u8_C
	.long	L_1+1321
	.long	And__uAnd_param_u8_i64
	.long	L_1+1335
	.long	And__uAnd_var_u8_i64
	.long	L_1+1353
	.long	And__uAnd_param_u8_I
	.long	L_1+1369
	.long	And__uAnd_var_u8_I
	.long	L_1+1385
	.long	And__uAnd_param_u8_i16
	.long	L_1+1399
	.long	And__uAnd_var_u8_i16
	.long	L_1+1417
	.long	And__uAnd_param_u8_i32
	.long	L_1+1433
	.long	And__uAnd_var_u8_i32
	.long	L_1+1451
	.long	And__uAnd_param_u8_LC
	.long	L_1+1467
	.long	And__uAnd_var_u8_LC
	.long	L_1+1484
	.long	And__uAnd_param_u8_u64
	.long	L_1+1499
	.long	And__uAnd_var_u8_u64
	.long	L_1+1517
	.long	And__uAnd_param_u8_u16
	.long	L_1+1533
	.long	And__uAnd_var_u8_u16
	.long	L_1+1551
	.long	And__uAnd_param_C_u32
	.long	L_1+1567
	.long	And__uAnd_var_C_u32
	.long	L_1+1584
	.long	And__uAnd_param_C_i8
	.long	L_1+1599
	.long	And__uAnd_var_C_i8
	.long	L_1+1615
	.long	And__uAnd_param_C_L
	.long	L_1+1629
	.long	And__uAnd_var_C_L
	.long	L_1+1644
	.long	And__uAnd_param_C_u8
	.long	L_1+1657
	.long	And__uAnd_var_C_u8
	.long	L_1+1673
	.long	And__uAnd_param_C_C
	.long	L_1+1687
	.long	And__uAnd_var_C_C
	.long	L_1+1702
	.long	And__uAnd_param_C_i64
	.long	L_1+1715
	.long	And__uAnd_var_C_i64
	.long	L_1+1732
	.long	And__uAnd_param_C_I
	.long	L_1+1747
	.long	And__uAnd_var_C_I
	.long	L_1+1762
	.long	And__uAnd_param_C_i16
	.long	L_1+1775
	.long	And__uAnd_var_C_i16
	.long	L_1+1792
	.long	And__uAnd_param_C_i32
	.long	L_1+1807
	.long	And__uAnd_var_C_i32
	.long	L_1+1824
	.long	And__uAnd_param_C_LC
	.long	L_1+1839
	.long	And__uAnd_var_C_LC
	.long	L_1+1855
	.long	And__uAnd_param_C_u64
	.long	L_1+1869
	.long	And__uAnd_var_C_u64
	.long	L_1+1886
	.long	And__uAnd_param_C_u16
	.long	L_1+1901
	.long	And__uAnd_var_C_u16
	.long	L_1+1918
	.long	And__uAnd_param_i64_u32
	.long	L_1+1933
	.long	And__uAnd_var_i64_u32
	.long	L_1+1952
	.long	And__uAnd_param_i64_i8
	.long	L_1+1969
	.long	And__uAnd_var_i64_i8
	.long	L_1+1987
	.long	And__uAnd_param_i64_L
	.long	L_1+2003
	.long	And__uAnd_var_i64_L
	.long	L_1+2020
	.long	And__uAnd_param_i64_u8
	.long	L_1+2035
	.long	And__uAnd_var_i64_u8
	.long	L_1+2053
	.long	And__uAnd_param_i64_C
	.long	L_1+2069
	.long	And__uAnd_var_i64_C
	.long	L_1+2086
	.long	And__uAnd_param_i64_i64
	.long	L_1+2101
	.long	And__uAnd_var_i64_i64
	.long	L_1+2120
	.long	And__uAnd_param_i64_I
	.long	L_1+2137
	.long	And__uAnd_var_i64_I
	.long	L_1+2154
	.long	And__uAnd_param_i64_i16
	.long	L_1+2169
	.long	And__uAnd_var_i64_i16
	.long	L_1+2188
	.long	And__uAnd_param_i64_i32
	.long	L_1+2205
	.long	And__uAnd_var_i64_i32
	.long	L_1+2224
	.long	And__uAnd_param_i64_LC
	.long	L_1+2241
	.long	And__uAnd_var_i64_LC
	.long	L_1+2259
	.long	And__uAnd_param_i64_u64
	.long	L_1+2275
	.long	And__uAnd_var_i64_u64
	.long	L_1+2294
	.long	And__uAnd_param_i64_u16
	.long	L_1+2311
	.long	And__uAnd_var_i64_u16
	.long	L_1+2330
	.long	And__uAnd_param_I_u32
	.long	L_1+2347
	.long	And__uAnd_var_I_u32
	.long	L_1+2364
	.long	And__uAnd_param_I_i8
	.long	L_1+2379
	.long	And__uAnd_var_I_i8
	.long	L_1+2395
	.long	And__uAnd_param_I_L
	.long	L_1+2409
	.long	And__uAnd_var_I_L
	.long	L_1+2424
	.long	And__uAnd_param_I_u8
	.long	L_1+2437
	.long	And__uAnd_var_I_u8
	.long	L_1+2453
	.long	And__uAnd_param_I_C
	.long	L_1+2467
	.long	And__uAnd_var_I_C
	.long	L_1+2482
	.long	And__uAnd_param_I_i64
	.long	L_1+2495
	.long	And__uAnd_var_I_i64
	.long	L_1+2512
	.long	And__uAnd_param_I_I
	.long	L_1+2527
	.long	And__uAnd_var_I_I
	.long	L_1+2542
	.long	And__uAnd_param_I_i16
	.long	L_1+2555
	.long	And__uAnd_var_I_i16
	.long	L_1+2572
	.long	And__uAnd_param_I_i32
	.long	L_1+2587
	.long	And__uAnd_var_I_i32
	.long	L_1+2604
	.long	And__uAnd_param_I_LC
	.long	L_1+2619
	.long	And__uAnd_var_I_LC
	.long	L_1+2635
	.long	And__uAnd_param_I_u64
	.long	L_1+2649
	.long	And__uAnd_var_I_u64
	.long	L_1+2666
	.long	And__uAnd_param_I_u16
	.long	L_1+2681
	.long	And__uAnd_var_I_u16
	.long	L_1+2698
	.long	And__uAnd_param_i16_u32
	.long	L_1+2713
	.long	And__uAnd_var_i16_u32
	.long	L_1+2732
	.long	And__uAnd_param_i16_i8
	.long	L_1+2749
	.long	And__uAnd_var_i16_i8
	.long	L_1+2767
	.long	And__uAnd_param_i16_L
	.long	L_1+2783
	.long	And__uAnd_var_i16_L
	.long	L_1+2800
	.long	And__uAnd_param_i16_u8
	.long	L_1+2815
	.long	And__uAnd_var_i16_u8
	.long	L_1+2833
	.long	And__uAnd_param_i16_C
	.long	L_1+2849
	.long	And__uAnd_var_i16_C
	.long	L_1+2866
	.long	And__uAnd_param_i16_i64
	.long	L_1+2881
	.long	And__uAnd_var_i16_i64
	.long	L_1+2900
	.long	And__uAnd_param_i16_I
	.long	L_1+2917
	.long	And__uAnd_var_i16_I
	.long	L_1+2934
	.long	And__uAnd_param_i16_i16
	.long	L_1+2949
	.long	And__uAnd_var_i16_i16
	.long	L_1+2968
	.long	And__uAnd_param_i16_i32
	.long	L_1+2985
	.long	And__uAnd_var_i16_i32
	.long	L_1+3004
	.long	And__uAnd_param_i16_LC
	.long	L_1+3021
	.long	And__uAnd_var_i16_LC
	.long	L_1+3039
	.long	And__uAnd_param_i16_u64
	.long	L_1+3055
	.long	And__uAnd_var_i16_u64
	.long	L_1+3074
	.long	And__uAnd_param_i16_u16
	.long	L_1+3091
	.long	And__uAnd_var_i16_u16
	.long	L_1+3110
	.long	And__uAnd_param_i32_u32
	.long	L_1+3127
	.long	And__uAnd_var_i32_u32
	.long	L_1+3146
	.long	And__uAnd_param_i32_i8
	.long	L_1+3163
	.long	And__uAnd_var_i32_i8
	.long	L_1+3181
	.long	And__uAnd_param_i32_L
	.long	L_1+3197
	.long	And__uAnd_var_i32_L
	.long	L_1+3214
	.long	And__uAnd_param_i32_u8
	.long	L_1+3229
	.long	And__uAnd_var_i32_u8
	.long	L_1+3247
	.long	And__uAnd_param_i32_C
	.long	L_1+3263
	.long	And__uAnd_var_i32_C
	.long	L_1+3280
	.long	And__uAnd_param_i32_i64
	.long	L_1+3295
	.long	And__uAnd_var_i32_i64
	.long	L_1+3314
	.long	And__uAnd_param_i32_I
	.long	L_1+3331
	.long	And__uAnd_var_i32_I
	.long	L_1+3348
	.long	And__uAnd_param_i32_i16
	.long	L_1+3363
	.long	And__uAnd_var_i32_i16
	.long	L_1+3382
	.long	And__uAnd_param_i32_i32
	.long	L_1+3399
	.long	And__uAnd_var_i32_i32
	.long	L_1+3418
	.long	And__uAnd_param_i32_LC
	.long	L_1+3435
	.long	And__uAnd_var_i32_LC
	.long	L_1+3453
	.long	And__uAnd_param_i32_u64
	.long	L_1+3469
	.long	And__uAnd_var_i32_u64
	.long	L_1+3488
	.long	And__uAnd_param_i32_u16
	.long	L_1+3505
	.long	And__uAnd_var_i32_u16
	.long	L_1+3524
	.long	And__uAnd_param_LC_u32
	.long	L_1+3541
	.long	And__uAnd_var_LC_u32
	.long	L_1+3559
	.long	And__uAnd_param_LC_i8
	.long	L_1+3575
	.long	And__uAnd_var_LC_i8
	.long	L_1+3592
	.long	And__uAnd_param_LC_L
	.long	L_1+3607
	.long	And__uAnd_var_LC_L
	.long	L_1+3623
	.long	And__uAnd_param_LC_u8
	.long	L_1+3637
	.long	And__uAnd_var_LC_u8
	.long	L_1+3654
	.long	And__uAnd_param_LC_C
	.long	L_1+3669
	.long	And__uAnd_var_LC_C
	.long	L_1+3685
	.long	And__uAnd_param_LC_i64
	.long	L_1+3699
	.long	And__uAnd_var_LC_i64
	.long	L_1+3717
	.long	And__uAnd_param_LC_I
	.long	L_1+3733
	.long	And__uAnd_var_LC_I
	.long	L_1+3749
	.long	And__uAnd_param_LC_i16
	.long	L_1+3763
	.long	And__uAnd_var_LC_i16
	.long	L_1+3781
	.long	And__uAnd_param_LC_i32
	.long	L_1+3797
	.long	And__uAnd_var_LC_i32
	.long	L_1+3815
	.long	And__uAnd_param_LC_LC
	.long	L_1+3831
	.long	And__uAnd_var_LC_LC
	.long	L_1+3848
	.long	And__uAnd_param_LC_u64
	.long	L_1+3863
	.long	And__uAnd_var_LC_u64
	.long	L_1+3881
	.long	And__uAnd_param_LC_u16
	.long	L_1+3897
	.long	And__uAnd_var_LC_u16
	.long	L_1+3915
	.long	And__uAnd_param_u64_u32
	.long	L_1+3931
	.long	And__uAnd_var_u64_u32
	.long	L_1+3950
	.long	And__uAnd_param_u64_i8
	.long	L_1+3967
	.long	And__uAnd_var_u64_i8
	.long	L_1+3985
	.long	And__uAnd_param_u64_L
	.long	L_1+4001
	.long	And__uAnd_var_u64_L
	.long	L_1+4018
	.long	And__uAnd_param_u64_u8
	.long	L_1+4033
	.long	And__uAnd_var_u64_u8
	.long	L_1+4051
	.long	And__uAnd_param_u64_C
	.long	L_1+4067
	.long	And__uAnd_var_u64_C
	.long	L_1+4084
	.long	And__uAnd_param_u64_i64
	.long	L_1+4099
	.long	And__uAnd_var_u64_i64
	.long	L_1+4118
	.long	And__uAnd_param_u64_I
	.long	L_1+4135
	.long	And__uAnd_var_u64_I
	.long	L_1+4152
	.long	And__uAnd_param_u64_i16
	.long	L_1+4167
	.long	And__uAnd_var_u64_i16
	.long	L_1+4186
	.long	And__uAnd_param_u64_i32
	.long	L_1+4203
	.long	And__uAnd_var_u64_i32
	.long	L_1+4222
	.long	And__uAnd_param_u64_LC
	.long	L_1+4239
	.long	And__uAnd_var_u64_LC
	.long	L_1+4257
	.long	And__uAnd_param_u64_u64
	.long	L_1+4273
	.long	And__uAnd_var_u64_u64
	.long	L_1+4292
	.long	And__uAnd_param_u64_u16
	.long	L_1+4309
	.long	And__uAnd_var_u64_u16
	.long	L_1+4328
	.long	And__uAnd_param_u16_u32
	.long	L_1+4345
	.long	And__uAnd_var_u16_u32
	.long	L_1+4364
	.long	And__uAnd_param_u16_i8
	.long	L_1+4381
	.long	And__uAnd_var_u16_i8
	.long	L_1+4399
	.long	And__uAnd_param_u16_L
	.long	L_1+4415
	.long	And__uAnd_var_u16_L
	.long	L_1+4432
	.long	And__uAnd_param_u16_u8
	.long	L_1+4447
	.long	And__uAnd_var_u16_u8
	.long	L_1+4465
	.long	And__uAnd_param_u16_C
	.long	L_1+4481
	.long	And__uAnd_var_u16_C
	.long	L_1+4498
	.long	And__uAnd_param_u16_i64
	.long	L_1+4513
	.long	And__uAnd_var_u16_i64
	.long	L_1+4532
	.long	And__uAnd_param_u16_I
	.long	L_1+4549
	.long	And__uAnd_var_u16_I
	.long	L_1+4566
	.long	And__uAnd_param_u16_i16
	.long	L_1+4581
	.long	And__uAnd_var_u16_i16
	.long	L_1+4600
	.long	And__uAnd_param_u16_i32
	.long	L_1+4617
	.long	And__uAnd_var_u16_i32
	.long	L_1+4636
	.long	And__uAnd_param_u16_LC
	.long	L_1+4653
	.long	And__uAnd_var_u16_LC
	.long	L_1+4671
	.long	And__uAnd_param_u16_u64
	.long	L_1+4687
	.long	And__uAnd_var_u16_u64
	.long	L_1+4706
	.long	And__uAnd_param_u16_u16
	.long	L_1+4723
	.long	And__uAnd_var_u16_u16
	.long	L_1+4742
	.skip 4
	.ascii	"../SOLsun/And.m3"
	.skip 4
	.align 8
	.type	MM_And, #object
	.size	MM_And, 208
MM_And:
	.long	L_1+7076
	.skip 16
	.long	L_1+4760
	.skip 12
	.long	MM_And+148
	.skip 4
	.long	And_M3
	.long	3
	.half	433
	.skip 2
	.long	0
	.long	434
	.long	0
	.long	435
	.long	436
	.half	437
	.skip 2
	.long	438
	.skip 4
	.long	0
	.long	439
	.long	440
	.skip 4
	.long	1081841426
	.long	1855425872
	.byte	-69
	.skip 7
	.long	0
	.long	444
	.byte	-67
	.skip 3
	.long	446
	.long	1138735448
	.skip 12
	.long	And_I3
	.long	MM_And+160
	.skip 4
	.long	Long_I3
	.long	MM_And+172
	.skip 4
	.long	Word_I3
	.long	MM_And+184
	.skip 4
	.long	Cstdint_I3
	.long	MM_And+196
	.skip 4
	.long	RTHooks_I3
	.skip 4
	.ident	"GCC: (GNU) 4.5.1"
