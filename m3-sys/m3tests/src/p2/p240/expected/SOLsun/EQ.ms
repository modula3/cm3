	.file	"EQ.mc"
	.section	".text"
	.align 4
	.type	EQ__EQ_var_u16_u16, #function
	.proc	014
EQ__EQ_var_u16_u16:
.LLFB0:
	save	%sp, -96, %sp
.LLCFI0:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE0:
	.size	EQ__EQ_var_u16_u16, .-EQ__EQ_var_u16_u16
	.align 4
	.type	EQ__EQ_param_u16_u16, #function
	.proc	014
EQ__EQ_param_u16_u16:
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
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1:
	.size	EQ__EQ_param_u16_u16, .-EQ__EQ_param_u16_u16
	.align 4
	.type	EQ__EQ_var_u16_u64, #function
	.proc	014
EQ__EQ_var_u16_u64:
.LLFB2:
	save	%sp, -96, %sp
.LLCFI2:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+56], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE2:
	.size	EQ__EQ_var_u16_u64, .-EQ__EQ_var_u16_u64
	.align 4
	.type	EQ__EQ_param_u16_u64, #function
	.proc	014
EQ__EQ_param_u16_u64:
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
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE3:
	.size	EQ__EQ_param_u16_u64, .-EQ__EQ_param_u16_u64
	.align 4
	.type	EQ__EQ_var_u16_LC, #function
	.proc	014
EQ__EQ_var_u16_LC:
.LLFB4:
	save	%sp, -96, %sp
.LLCFI4:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+64], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE4:
	.size	EQ__EQ_var_u16_LC, .-EQ__EQ_var_u16_LC
	.align 4
	.type	EQ__EQ_param_u16_LC, #function
	.proc	014
EQ__EQ_param_u16_LC:
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
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE5:
	.size	EQ__EQ_param_u16_LC, .-EQ__EQ_param_u16_LC
	.align 4
	.type	EQ__EQ_var_u16_i32, #function
	.proc	014
EQ__EQ_var_u16_i32:
.LLFB6:
	save	%sp, -96, %sp
.LLCFI6:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+72], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE6:
	.size	EQ__EQ_var_u16_i32, .-EQ__EQ_var_u16_i32
	.align 4
	.type	EQ__EQ_param_u16_i32, #function
	.proc	014
EQ__EQ_param_u16_i32:
.LLFB7:
	save	%sp, -96, %sp
.LLCFI7:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE7:
	.size	EQ__EQ_param_u16_i32, .-EQ__EQ_param_u16_i32
	.align 4
	.type	EQ__EQ_var_u16_i16, #function
	.proc	014
EQ__EQ_var_u16_i16:
.LLFB8:
	save	%sp, -96, %sp
.LLCFI8:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE8:
	.size	EQ__EQ_var_u16_i16, .-EQ__EQ_var_u16_i16
	.align 4
	.type	EQ__EQ_param_u16_i16, #function
	.proc	014
EQ__EQ_param_u16_i16:
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
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE9:
	.size	EQ__EQ_param_u16_i16, .-EQ__EQ_param_u16_i16
	.align 4
	.type	EQ__EQ_var_u16_I, #function
	.proc	014
EQ__EQ_var_u16_I:
.LLFB10:
	save	%sp, -96, %sp
.LLCFI10:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+80], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE10:
	.size	EQ__EQ_var_u16_I, .-EQ__EQ_var_u16_I
	.align 4
	.type	EQ__EQ_param_u16_I, #function
	.proc	014
EQ__EQ_param_u16_I:
.LLFB11:
	save	%sp, -96, %sp
.LLCFI11:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE11:
	.size	EQ__EQ_param_u16_I, .-EQ__EQ_param_u16_I
	.align 4
	.type	EQ__EQ_var_u16_i64, #function
	.proc	014
EQ__EQ_var_u16_i64:
.LLFB12:
	save	%sp, -96, %sp
.LLCFI12:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+88], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE12:
	.size	EQ__EQ_var_u16_i64, .-EQ__EQ_var_u16_i64
	.align 4
	.type	EQ__EQ_param_u16_i64, #function
	.proc	014
EQ__EQ_param_u16_i64:
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
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE13:
	.size	EQ__EQ_param_u16_i64, .-EQ__EQ_param_u16_i64
	.align 4
	.type	EQ__EQ_var_u16_C, #function
	.proc	014
EQ__EQ_var_u16_C:
.LLFB14:
	save	%sp, -96, %sp
.LLCFI14:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+96], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE14:
	.size	EQ__EQ_var_u16_C, .-EQ__EQ_var_u16_C
	.align 4
	.type	EQ__EQ_param_u16_C, #function
	.proc	014
EQ__EQ_param_u16_C:
.LLFB15:
	save	%sp, -96, %sp
.LLCFI15:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE15:
	.size	EQ__EQ_param_u16_C, .-EQ__EQ_param_u16_C
	.align 4
	.type	EQ__EQ_var_u16_u8, #function
	.proc	014
EQ__EQ_var_u16_u8:
.LLFB16:
	save	%sp, -96, %sp
.LLCFI16:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	mov	%g1, %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE16:
	.size	EQ__EQ_var_u16_u8, .-EQ__EQ_var_u16_u8
	.align 4
	.type	EQ__EQ_param_u16_u8, #function
	.proc	014
EQ__EQ_param_u16_u8:
.LLFB17:
	save	%sp, -96, %sp
.LLCFI17:
	sth	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE17:
	.size	EQ__EQ_param_u16_u8, .-EQ__EQ_param_u16_u8
	.align 4
	.type	EQ__EQ_var_u16_L, #function
	.proc	014
EQ__EQ_var_u16_L:
.LLFB18:
	save	%sp, -96, %sp
.LLCFI18:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+120], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE18:
	.size	EQ__EQ_var_u16_L, .-EQ__EQ_var_u16_L
	.align 4
	.type	EQ__EQ_param_u16_L, #function
	.proc	014
EQ__EQ_param_u16_L:
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
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE19:
	.size	EQ__EQ_param_u16_L, .-EQ__EQ_param_u16_L
	.align 4
	.type	EQ__EQ_var_u16_i8, #function
	.proc	014
EQ__EQ_var_u16_i8:
.LLFB20:
	save	%sp, -96, %sp
.LLCFI20:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE20:
	.size	EQ__EQ_var_u16_i8, .-EQ__EQ_var_u16_i8
	.align 4
	.type	EQ__EQ_param_u16_i8, #function
	.proc	014
EQ__EQ_param_u16_i8:
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
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE21:
	.size	EQ__EQ_param_u16_i8, .-EQ__EQ_param_u16_i8
	.align 4
	.type	EQ__EQ_var_u16_u32, #function
	.proc	014
EQ__EQ_var_u16_u32:
.LLFB22:
	save	%sp, -96, %sp
.LLCFI22:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+132], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE22:
	.size	EQ__EQ_var_u16_u32, .-EQ__EQ_var_u16_u32
	.align 4
	.type	EQ__EQ_param_u16_u32, #function
	.proc	014
EQ__EQ_param_u16_u32:
.LLFB23:
	save	%sp, -96, %sp
.LLCFI23:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+72], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE23:
	.size	EQ__EQ_param_u16_u32, .-EQ__EQ_param_u16_u32
	.align 4
	.type	EQ__EQ_var_u64_u16, #function
	.proc	014
EQ__EQ_var_u64_u16:
.LLFB24:
	save	%sp, -96, %sp
.LLCFI24:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+56], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE24:
	.size	EQ__EQ_var_u64_u16, .-EQ__EQ_var_u64_u16
	.align 4
	.type	EQ__EQ_param_u64_u16, #function
	.proc	014
EQ__EQ_param_u64_u16:
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
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE25:
	.size	EQ__EQ_param_u64_u16, .-EQ__EQ_param_u64_u16
	.align 4
	.type	EQ__EQ_var_u64_u64, #function
	.proc	014
EQ__EQ_var_u64_u64:
.LLFB26:
	save	%sp, -96, %sp
.LLCFI26:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE26:
	.size	EQ__EQ_var_u64_u64, .-EQ__EQ_var_u64_u64
	.align 4
	.type	EQ__EQ_param_u64_u64, #function
	.proc	014
EQ__EQ_param_u64_u64:
.LLFB27:
	save	%sp, -112, %sp
.LLCFI27:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE27:
	.size	EQ__EQ_param_u64_u64, .-EQ__EQ_param_u64_u64
	.align 4
	.type	EQ__EQ_var_u64_LC, #function
	.proc	014
EQ__EQ_var_u64_LC:
.LLFB28:
	save	%sp, -96, %sp
.LLCFI28:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+56], %o2
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+64], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE28:
	.size	EQ__EQ_var_u64_LC, .-EQ__EQ_var_u64_LC
	.align 4
	.type	EQ__EQ_param_u64_LC, #function
	.proc	014
EQ__EQ_param_u64_LC:
.LLFB29:
	save	%sp, -112, %sp
.LLCFI29:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o2
	ldd	[%fp-8], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE29:
	.size	EQ__EQ_param_u64_LC, .-EQ__EQ_param_u64_LC
	.align 4
	.type	EQ__EQ_var_u64_i32, #function
	.proc	014
EQ__EQ_var_u64_i32:
.LLFB30:
	save	%sp, -96, %sp
.LLCFI30:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+56], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE30:
	.size	EQ__EQ_var_u64_i32, .-EQ__EQ_var_u64_i32
	.align 4
	.type	EQ__EQ_param_u64_i32, #function
	.proc	014
EQ__EQ_param_u64_i32:
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
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE31:
	.size	EQ__EQ_param_u64_i32, .-EQ__EQ_param_u64_i32
	.align 4
	.type	EQ__EQ_var_u64_i16, #function
	.proc	014
EQ__EQ_var_u64_i16:
.LLFB32:
	save	%sp, -96, %sp
.LLCFI32:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+56], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE32:
	.size	EQ__EQ_var_u64_i16, .-EQ__EQ_var_u64_i16
	.align 4
	.type	EQ__EQ_param_u64_i16, #function
	.proc	014
EQ__EQ_param_u64_i16:
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
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE33:
	.size	EQ__EQ_param_u64_i16, .-EQ__EQ_param_u64_i16
	.align 4
	.type	EQ__EQ_var_u64_I, #function
	.proc	014
EQ__EQ_var_u64_I:
.LLFB34:
	save	%sp, -96, %sp
.LLCFI34:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+56], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE34:
	.size	EQ__EQ_var_u64_I, .-EQ__EQ_var_u64_I
	.align 4
	.type	EQ__EQ_param_u64_I, #function
	.proc	014
EQ__EQ_param_u64_I:
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
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE35:
	.size	EQ__EQ_param_u64_I, .-EQ__EQ_param_u64_I
	.align 4
	.type	EQ__EQ_var_u64_i64, #function
	.proc	014
EQ__EQ_var_u64_i64:
.LLFB36:
	save	%sp, -96, %sp
.LLCFI36:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+56], %o2
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+88], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE36:
	.size	EQ__EQ_var_u64_i64, .-EQ__EQ_var_u64_i64
	.align 4
	.type	EQ__EQ_param_u64_i64, #function
	.proc	014
EQ__EQ_param_u64_i64:
.LLFB37:
	save	%sp, -112, %sp
.LLCFI37:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE37:
	.size	EQ__EQ_param_u64_i64, .-EQ__EQ_param_u64_i64
	.align 4
	.type	EQ__EQ_var_u64_C, #function
	.proc	014
EQ__EQ_var_u64_C:
.LLFB38:
	save	%sp, -96, %sp
.LLCFI38:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+56], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE38:
	.size	EQ__EQ_var_u64_C, .-EQ__EQ_var_u64_C
	.align 4
	.type	EQ__EQ_param_u64_C, #function
	.proc	014
EQ__EQ_param_u64_C:
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
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE39:
	.size	EQ__EQ_param_u64_C, .-EQ__EQ_param_u64_C
	.align 4
	.type	EQ__EQ_var_u64_u8, #function
	.proc	014
EQ__EQ_var_u64_u8:
.LLFB40:
	save	%sp, -96, %sp
.LLCFI40:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+56], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE40:
	.size	EQ__EQ_var_u64_u8, .-EQ__EQ_var_u64_u8
	.align 4
	.type	EQ__EQ_param_u64_u8, #function
	.proc	014
EQ__EQ_param_u64_u8:
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
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE41:
	.size	EQ__EQ_param_u64_u8, .-EQ__EQ_param_u64_u8
	.align 4
	.type	EQ__EQ_var_u64_L, #function
	.proc	014
EQ__EQ_var_u64_L:
.LLFB42:
	save	%sp, -96, %sp
.LLCFI42:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+56], %o2
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+120], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE42:
	.size	EQ__EQ_var_u64_L, .-EQ__EQ_var_u64_L
	.align 4
	.type	EQ__EQ_param_u64_L, #function
	.proc	014
EQ__EQ_param_u64_L:
.LLFB43:
	save	%sp, -112, %sp
.LLCFI43:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE43:
	.size	EQ__EQ_param_u64_L, .-EQ__EQ_param_u64_L
	.align 4
	.type	EQ__EQ_var_u64_i8, #function
	.proc	014
EQ__EQ_var_u64_i8:
.LLFB44:
	save	%sp, -96, %sp
.LLCFI44:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+56], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE44:
	.size	EQ__EQ_var_u64_i8, .-EQ__EQ_var_u64_i8
	.align 4
	.type	EQ__EQ_param_u64_i8, #function
	.proc	014
EQ__EQ_param_u64_i8:
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
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE45:
	.size	EQ__EQ_param_u64_i8, .-EQ__EQ_param_u64_i8
	.align 4
	.type	EQ__EQ_var_u64_u32, #function
	.proc	014
EQ__EQ_var_u64_u32:
.LLFB46:
	save	%sp, -96, %sp
.LLCFI46:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+56], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE46:
	.size	EQ__EQ_var_u64_u32, .-EQ__EQ_var_u64_u32
	.align 4
	.type	EQ__EQ_param_u64_u32, #function
	.proc	014
EQ__EQ_param_u64_u32:
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
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE47:
	.size	EQ__EQ_param_u64_u32, .-EQ__EQ_param_u64_u32
	.align 4
	.type	EQ__EQ_var_LC_u16, #function
	.proc	014
EQ__EQ_var_LC_u16:
.LLFB48:
	save	%sp, -96, %sp
.LLCFI48:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+64], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE48:
	.size	EQ__EQ_var_LC_u16, .-EQ__EQ_var_LC_u16
	.align 4
	.type	EQ__EQ_param_LC_u16, #function
	.proc	014
EQ__EQ_param_LC_u16:
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
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE49:
	.size	EQ__EQ_param_LC_u16, .-EQ__EQ_param_LC_u16
	.align 4
	.type	EQ__EQ_var_LC_u64, #function
	.proc	014
EQ__EQ_var_LC_u64:
.LLFB50:
	save	%sp, -96, %sp
.LLCFI50:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+64], %o2
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+56], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE50:
	.size	EQ__EQ_var_LC_u64, .-EQ__EQ_var_LC_u64
	.align 4
	.type	EQ__EQ_param_LC_u64, #function
	.proc	014
EQ__EQ_param_LC_u64:
.LLFB51:
	save	%sp, -112, %sp
.LLCFI51:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o2
	ldd	[%fp-8], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE51:
	.size	EQ__EQ_param_LC_u64, .-EQ__EQ_param_LC_u64
	.align 4
	.type	EQ__EQ_var_LC_LC, #function
	.proc	014
EQ__EQ_var_LC_LC:
.LLFB52:
	save	%sp, -96, %sp
.LLCFI52:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE52:
	.size	EQ__EQ_var_LC_LC, .-EQ__EQ_var_LC_LC
	.align 4
	.type	EQ__EQ_param_LC_LC, #function
	.proc	014
EQ__EQ_param_LC_LC:
.LLFB53:
	save	%sp, -112, %sp
.LLCFI53:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE53:
	.size	EQ__EQ_param_LC_LC, .-EQ__EQ_param_LC_LC
	.align 4
	.type	EQ__EQ_var_LC_i32, #function
	.proc	014
EQ__EQ_var_LC_i32:
.LLFB54:
	save	%sp, -96, %sp
.LLCFI54:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+64], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE54:
	.size	EQ__EQ_var_LC_i32, .-EQ__EQ_var_LC_i32
	.align 4
	.type	EQ__EQ_param_LC_i32, #function
	.proc	014
EQ__EQ_param_LC_i32:
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
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE55:
	.size	EQ__EQ_param_LC_i32, .-EQ__EQ_param_LC_i32
	.align 4
	.type	EQ__EQ_var_LC_i16, #function
	.proc	014
EQ__EQ_var_LC_i16:
.LLFB56:
	save	%sp, -96, %sp
.LLCFI56:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+64], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE56:
	.size	EQ__EQ_var_LC_i16, .-EQ__EQ_var_LC_i16
	.align 4
	.type	EQ__EQ_param_LC_i16, #function
	.proc	014
EQ__EQ_param_LC_i16:
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
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE57:
	.size	EQ__EQ_param_LC_i16, .-EQ__EQ_param_LC_i16
	.align 4
	.type	EQ__EQ_var_LC_I, #function
	.proc	014
EQ__EQ_var_LC_I:
.LLFB58:
	save	%sp, -96, %sp
.LLCFI58:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+64], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE58:
	.size	EQ__EQ_var_LC_I, .-EQ__EQ_var_LC_I
	.align 4
	.type	EQ__EQ_param_LC_I, #function
	.proc	014
EQ__EQ_param_LC_I:
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
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE59:
	.size	EQ__EQ_param_LC_I, .-EQ__EQ_param_LC_I
	.align 4
	.type	EQ__EQ_var_LC_i64, #function
	.proc	014
EQ__EQ_var_LC_i64:
.LLFB60:
	save	%sp, -96, %sp
.LLCFI60:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+64], %o2
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+88], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE60:
	.size	EQ__EQ_var_LC_i64, .-EQ__EQ_var_LC_i64
	.align 4
	.type	EQ__EQ_param_LC_i64, #function
	.proc	014
EQ__EQ_param_LC_i64:
.LLFB61:
	save	%sp, -112, %sp
.LLCFI61:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o2
	ldd	[%fp-8], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE61:
	.size	EQ__EQ_param_LC_i64, .-EQ__EQ_param_LC_i64
	.align 4
	.type	EQ__EQ_var_LC_C, #function
	.proc	014
EQ__EQ_var_LC_C:
.LLFB62:
	save	%sp, -96, %sp
.LLCFI62:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+64], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE62:
	.size	EQ__EQ_var_LC_C, .-EQ__EQ_var_LC_C
	.align 4
	.type	EQ__EQ_param_LC_C, #function
	.proc	014
EQ__EQ_param_LC_C:
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
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE63:
	.size	EQ__EQ_param_LC_C, .-EQ__EQ_param_LC_C
	.align 4
	.type	EQ__EQ_var_LC_u8, #function
	.proc	014
EQ__EQ_var_LC_u8:
.LLFB64:
	save	%sp, -96, %sp
.LLCFI64:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+64], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE64:
	.size	EQ__EQ_var_LC_u8, .-EQ__EQ_var_LC_u8
	.align 4
	.type	EQ__EQ_param_LC_u8, #function
	.proc	014
EQ__EQ_param_LC_u8:
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
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE65:
	.size	EQ__EQ_param_LC_u8, .-EQ__EQ_param_LC_u8
	.align 4
	.type	EQ__EQ_var_LC_L, #function
	.proc	014
EQ__EQ_var_LC_L:
.LLFB66:
	save	%sp, -96, %sp
.LLCFI66:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+64], %o2
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+120], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE66:
	.size	EQ__EQ_var_LC_L, .-EQ__EQ_var_LC_L
	.align 4
	.type	EQ__EQ_param_LC_L, #function
	.proc	014
EQ__EQ_param_LC_L:
.LLFB67:
	save	%sp, -112, %sp
.LLCFI67:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o2
	ldd	[%fp-8], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE67:
	.size	EQ__EQ_param_LC_L, .-EQ__EQ_param_LC_L
	.align 4
	.type	EQ__EQ_var_LC_i8, #function
	.proc	014
EQ__EQ_var_LC_i8:
.LLFB68:
	save	%sp, -96, %sp
.LLCFI68:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+64], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE68:
	.size	EQ__EQ_var_LC_i8, .-EQ__EQ_var_LC_i8
	.align 4
	.type	EQ__EQ_param_LC_i8, #function
	.proc	014
EQ__EQ_param_LC_i8:
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
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE69:
	.size	EQ__EQ_param_LC_i8, .-EQ__EQ_param_LC_i8
	.align 4
	.type	EQ__EQ_var_LC_u32, #function
	.proc	014
EQ__EQ_var_LC_u32:
.LLFB70:
	save	%sp, -96, %sp
.LLCFI70:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+64], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE70:
	.size	EQ__EQ_var_LC_u32, .-EQ__EQ_var_LC_u32
	.align 4
	.type	EQ__EQ_param_LC_u32, #function
	.proc	014
EQ__EQ_param_LC_u32:
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
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE71:
	.size	EQ__EQ_param_LC_u32, .-EQ__EQ_param_LC_u32
	.align 4
	.type	EQ__EQ_var_i32_u16, #function
	.proc	014
EQ__EQ_var_i32_u16:
.LLFB72:
	save	%sp, -96, %sp
.LLCFI72:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE72:
	.size	EQ__EQ_var_i32_u16, .-EQ__EQ_var_i32_u16
	.align 4
	.type	EQ__EQ_param_i32_u16, #function
	.proc	014
EQ__EQ_param_i32_u16:
.LLFB73:
	save	%sp, -96, %sp
.LLCFI73:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+68], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE73:
	.size	EQ__EQ_param_i32_u16, .-EQ__EQ_param_i32_u16
	.align 4
	.type	EQ__EQ_var_i32_u64, #function
	.proc	014
EQ__EQ_var_i32_u64:
.LLFB74:
	save	%sp, -96, %sp
.LLCFI74:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+56], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE74:
	.size	EQ__EQ_var_i32_u64, .-EQ__EQ_var_i32_u64
	.align 4
	.type	EQ__EQ_param_i32_u64, #function
	.proc	014
EQ__EQ_param_i32_u64:
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
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE75:
	.size	EQ__EQ_param_i32_u64, .-EQ__EQ_param_i32_u64
	.align 4
	.type	EQ__EQ_var_i32_LC, #function
	.proc	014
EQ__EQ_var_i32_LC:
.LLFB76:
	save	%sp, -96, %sp
.LLCFI76:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+64], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE76:
	.size	EQ__EQ_var_i32_LC, .-EQ__EQ_var_i32_LC
	.align 4
	.type	EQ__EQ_param_i32_LC, #function
	.proc	014
EQ__EQ_param_i32_LC:
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
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE77:
	.size	EQ__EQ_param_i32_LC, .-EQ__EQ_param_i32_LC
	.align 4
	.type	EQ__EQ_var_i32_i32, #function
	.proc	014
EQ__EQ_var_i32_i32:
.LLFB78:
	save	%sp, -96, %sp
.LLCFI78:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE78:
	.size	EQ__EQ_var_i32_i32, .-EQ__EQ_var_i32_i32
	.align 4
	.type	EQ__EQ_param_i32_i32, #function
	.proc	014
EQ__EQ_param_i32_i32:
.LLFB79:
	save	%sp, -96, %sp
.LLCFI79:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE79:
	.size	EQ__EQ_param_i32_i32, .-EQ__EQ_param_i32_i32
	.align 4
	.type	EQ__EQ_var_i32_i16, #function
	.proc	014
EQ__EQ_var_i32_i16:
.LLFB80:
	save	%sp, -96, %sp
.LLCFI80:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE80:
	.size	EQ__EQ_var_i32_i16, .-EQ__EQ_var_i32_i16
	.align 4
	.type	EQ__EQ_param_i32_i16, #function
	.proc	014
EQ__EQ_param_i32_i16:
.LLFB81:
	save	%sp, -96, %sp
.LLCFI81:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+68], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE81:
	.size	EQ__EQ_param_i32_i16, .-EQ__EQ_param_i32_i16
	.align 4
	.type	EQ__EQ_var_i32_I, #function
	.proc	014
EQ__EQ_var_i32_I:
.LLFB82:
	save	%sp, -96, %sp
.LLCFI82:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+80], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE82:
	.size	EQ__EQ_var_i32_I, .-EQ__EQ_var_i32_I
	.align 4
	.type	EQ__EQ_param_i32_I, #function
	.proc	014
EQ__EQ_param_i32_I:
.LLFB83:
	save	%sp, -96, %sp
.LLCFI83:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE83:
	.size	EQ__EQ_param_i32_I, .-EQ__EQ_param_i32_I
	.align 4
	.type	EQ__EQ_var_i32_i64, #function
	.proc	014
EQ__EQ_var_i32_i64:
.LLFB84:
	save	%sp, -96, %sp
.LLCFI84:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+88], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE84:
	.size	EQ__EQ_var_i32_i64, .-EQ__EQ_var_i32_i64
	.align 4
	.type	EQ__EQ_param_i32_i64, #function
	.proc	014
EQ__EQ_param_i32_i64:
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
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE85:
	.size	EQ__EQ_param_i32_i64, .-EQ__EQ_param_i32_i64
	.align 4
	.type	EQ__EQ_var_i32_C, #function
	.proc	014
EQ__EQ_var_i32_C:
.LLFB86:
	save	%sp, -96, %sp
.LLCFI86:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+96], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE86:
	.size	EQ__EQ_var_i32_C, .-EQ__EQ_var_i32_C
	.align 4
	.type	EQ__EQ_param_i32_C, #function
	.proc	014
EQ__EQ_param_i32_C:
.LLFB87:
	save	%sp, -96, %sp
.LLCFI87:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE87:
	.size	EQ__EQ_param_i32_C, .-EQ__EQ_param_i32_C
	.align 4
	.type	EQ__EQ_var_i32_u8, #function
	.proc	014
EQ__EQ_var_i32_u8:
.LLFB88:
	save	%sp, -96, %sp
.LLCFI88:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE88:
	.size	EQ__EQ_var_i32_u8, .-EQ__EQ_var_i32_u8
	.align 4
	.type	EQ__EQ_param_i32_u8, #function
	.proc	014
EQ__EQ_param_i32_u8:
.LLFB89:
	save	%sp, -96, %sp
.LLCFI89:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+68], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE89:
	.size	EQ__EQ_param_i32_u8, .-EQ__EQ_param_i32_u8
	.align 4
	.type	EQ__EQ_var_i32_L, #function
	.proc	014
EQ__EQ_var_i32_L:
.LLFB90:
	save	%sp, -96, %sp
.LLCFI90:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+120], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE90:
	.size	EQ__EQ_var_i32_L, .-EQ__EQ_var_i32_L
	.align 4
	.type	EQ__EQ_param_i32_L, #function
	.proc	014
EQ__EQ_param_i32_L:
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
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE91:
	.size	EQ__EQ_param_i32_L, .-EQ__EQ_param_i32_L
	.align 4
	.type	EQ__EQ_var_i32_i8, #function
	.proc	014
EQ__EQ_var_i32_i8:
.LLFB92:
	save	%sp, -96, %sp
.LLCFI92:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE92:
	.size	EQ__EQ_var_i32_i8, .-EQ__EQ_var_i32_i8
	.align 4
	.type	EQ__EQ_param_i32_i8, #function
	.proc	014
EQ__EQ_param_i32_i8:
.LLFB93:
	save	%sp, -96, %sp
.LLCFI93:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+68], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE93:
	.size	EQ__EQ_param_i32_i8, .-EQ__EQ_param_i32_i8
	.align 4
	.type	EQ__EQ_var_i32_u32, #function
	.proc	014
EQ__EQ_var_i32_u32:
.LLFB94:
	save	%sp, -96, %sp
.LLCFI94:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+72], %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+132], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE94:
	.size	EQ__EQ_var_i32_u32, .-EQ__EQ_var_i32_u32
	.align 4
	.type	EQ__EQ_param_i32_u32, #function
	.proc	014
EQ__EQ_param_i32_u32:
.LLFB95:
	save	%sp, -96, %sp
.LLCFI95:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE95:
	.size	EQ__EQ_param_i32_u32, .-EQ__EQ_param_i32_u32
	.align 4
	.type	EQ__EQ_var_i16_u16, #function
	.proc	014
EQ__EQ_var_i16_u16:
.LLFB96:
	save	%sp, -96, %sp
.LLCFI96:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE96:
	.size	EQ__EQ_var_i16_u16, .-EQ__EQ_var_i16_u16
	.align 4
	.type	EQ__EQ_param_i16_u16, #function
	.proc	014
EQ__EQ_param_i16_u16:
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
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE97:
	.size	EQ__EQ_param_i16_u16, .-EQ__EQ_param_i16_u16
	.align 4
	.type	EQ__EQ_var_i16_u64, #function
	.proc	014
EQ__EQ_var_i16_u64:
.LLFB98:
	save	%sp, -96, %sp
.LLCFI98:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+56], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE98:
	.size	EQ__EQ_var_i16_u64, .-EQ__EQ_var_i16_u64
	.align 4
	.type	EQ__EQ_param_i16_u64, #function
	.proc	014
EQ__EQ_param_i16_u64:
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
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE99:
	.size	EQ__EQ_param_i16_u64, .-EQ__EQ_param_i16_u64
	.align 4
	.type	EQ__EQ_var_i16_LC, #function
	.proc	014
EQ__EQ_var_i16_LC:
.LLFB100:
	save	%sp, -96, %sp
.LLCFI100:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+64], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE100:
	.size	EQ__EQ_var_i16_LC, .-EQ__EQ_var_i16_LC
	.align 4
	.type	EQ__EQ_param_i16_LC, #function
	.proc	014
EQ__EQ_param_i16_LC:
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
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE101:
	.size	EQ__EQ_param_i16_LC, .-EQ__EQ_param_i16_LC
	.align 4
	.type	EQ__EQ_var_i16_i32, #function
	.proc	014
EQ__EQ_var_i16_i32:
.LLFB102:
	save	%sp, -96, %sp
.LLCFI102:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+72], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE102:
	.size	EQ__EQ_var_i16_i32, .-EQ__EQ_var_i16_i32
	.align 4
	.type	EQ__EQ_param_i16_i32, #function
	.proc	014
EQ__EQ_param_i16_i32:
.LLFB103:
	save	%sp, -96, %sp
.LLCFI103:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE103:
	.size	EQ__EQ_param_i16_i32, .-EQ__EQ_param_i16_i32
	.align 4
	.type	EQ__EQ_var_i16_i16, #function
	.proc	014
EQ__EQ_var_i16_i16:
.LLFB104:
	save	%sp, -96, %sp
.LLCFI104:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE104:
	.size	EQ__EQ_var_i16_i16, .-EQ__EQ_var_i16_i16
	.align 4
	.type	EQ__EQ_param_i16_i16, #function
	.proc	014
EQ__EQ_param_i16_i16:
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
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE105:
	.size	EQ__EQ_param_i16_i16, .-EQ__EQ_param_i16_i16
	.align 4
	.type	EQ__EQ_var_i16_I, #function
	.proc	014
EQ__EQ_var_i16_I:
.LLFB106:
	save	%sp, -96, %sp
.LLCFI106:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+80], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE106:
	.size	EQ__EQ_var_i16_I, .-EQ__EQ_var_i16_I
	.align 4
	.type	EQ__EQ_param_i16_I, #function
	.proc	014
EQ__EQ_param_i16_I:
.LLFB107:
	save	%sp, -96, %sp
.LLCFI107:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE107:
	.size	EQ__EQ_param_i16_I, .-EQ__EQ_param_i16_I
	.align 4
	.type	EQ__EQ_var_i16_i64, #function
	.proc	014
EQ__EQ_var_i16_i64:
.LLFB108:
	save	%sp, -96, %sp
.LLCFI108:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+88], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE108:
	.size	EQ__EQ_var_i16_i64, .-EQ__EQ_var_i16_i64
	.align 4
	.type	EQ__EQ_param_i16_i64, #function
	.proc	014
EQ__EQ_param_i16_i64:
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
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE109:
	.size	EQ__EQ_param_i16_i64, .-EQ__EQ_param_i16_i64
	.align 4
	.type	EQ__EQ_var_i16_C, #function
	.proc	014
EQ__EQ_var_i16_C:
.LLFB110:
	save	%sp, -96, %sp
.LLCFI110:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+96], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE110:
	.size	EQ__EQ_var_i16_C, .-EQ__EQ_var_i16_C
	.align 4
	.type	EQ__EQ_param_i16_C, #function
	.proc	014
EQ__EQ_param_i16_C:
.LLFB111:
	save	%sp, -96, %sp
.LLCFI111:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE111:
	.size	EQ__EQ_param_i16_C, .-EQ__EQ_param_i16_C
	.align 4
	.type	EQ__EQ_var_i16_u8, #function
	.proc	014
EQ__EQ_var_i16_u8:
.LLFB112:
	save	%sp, -96, %sp
.LLCFI112:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE112:
	.size	EQ__EQ_var_i16_u8, .-EQ__EQ_var_i16_u8
	.align 4
	.type	EQ__EQ_param_i16_u8, #function
	.proc	014
EQ__EQ_param_i16_u8:
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
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE113:
	.size	EQ__EQ_param_i16_u8, .-EQ__EQ_param_i16_u8
	.align 4
	.type	EQ__EQ_var_i16_L, #function
	.proc	014
EQ__EQ_var_i16_L:
.LLFB114:
	save	%sp, -96, %sp
.LLCFI114:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+120], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE114:
	.size	EQ__EQ_var_i16_L, .-EQ__EQ_var_i16_L
	.align 4
	.type	EQ__EQ_param_i16_L, #function
	.proc	014
EQ__EQ_param_i16_L:
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
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE115:
	.size	EQ__EQ_param_i16_L, .-EQ__EQ_param_i16_L
	.align 4
	.type	EQ__EQ_var_i16_i8, #function
	.proc	014
EQ__EQ_var_i16_i8:
.LLFB116:
	save	%sp, -96, %sp
.LLCFI116:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE116:
	.size	EQ__EQ_var_i16_i8, .-EQ__EQ_var_i16_i8
	.align 4
	.type	EQ__EQ_param_i16_i8, #function
	.proc	014
EQ__EQ_param_i16_i8:
.LLFB117:
	save	%sp, -96, %sp
.LLCFI117:
	sth	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE117:
	.size	EQ__EQ_param_i16_i8, .-EQ__EQ_param_i16_i8
	.align 4
	.type	EQ__EQ_var_i16_u32, #function
	.proc	014
EQ__EQ_var_i16_u32:
.LLFB118:
	save	%sp, -96, %sp
.LLCFI118:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+132], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE118:
	.size	EQ__EQ_var_i16_u32, .-EQ__EQ_var_i16_u32
	.align 4
	.type	EQ__EQ_param_i16_u32, #function
	.proc	014
EQ__EQ_param_i16_u32:
.LLFB119:
	save	%sp, -96, %sp
.LLCFI119:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+72], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE119:
	.size	EQ__EQ_param_i16_u32, .-EQ__EQ_param_i16_u32
	.align 4
	.type	EQ__EQ_var_I_u16, #function
	.proc	014
EQ__EQ_var_I_u16:
.LLFB120:
	save	%sp, -96, %sp
.LLCFI120:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE120:
	.size	EQ__EQ_var_I_u16, .-EQ__EQ_var_I_u16
	.align 4
	.type	EQ__EQ_param_I_u16, #function
	.proc	014
EQ__EQ_param_I_u16:
.LLFB121:
	save	%sp, -96, %sp
.LLCFI121:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+68], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE121:
	.size	EQ__EQ_param_I_u16, .-EQ__EQ_param_I_u16
	.align 4
	.type	EQ__EQ_var_I_u64, #function
	.proc	014
EQ__EQ_var_I_u64:
.LLFB122:
	save	%sp, -96, %sp
.LLCFI122:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+56], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE122:
	.size	EQ__EQ_var_I_u64, .-EQ__EQ_var_I_u64
	.align 4
	.type	EQ__EQ_param_I_u64, #function
	.proc	014
EQ__EQ_param_I_u64:
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
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE123:
	.size	EQ__EQ_param_I_u64, .-EQ__EQ_param_I_u64
	.align 4
	.type	EQ__EQ_var_I_LC, #function
	.proc	014
EQ__EQ_var_I_LC:
.LLFB124:
	save	%sp, -96, %sp
.LLCFI124:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+64], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE124:
	.size	EQ__EQ_var_I_LC, .-EQ__EQ_var_I_LC
	.align 4
	.type	EQ__EQ_param_I_LC, #function
	.proc	014
EQ__EQ_param_I_LC:
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
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE125:
	.size	EQ__EQ_param_I_LC, .-EQ__EQ_param_I_LC
	.align 4
	.type	EQ__EQ_var_I_i32, #function
	.proc	014
EQ__EQ_var_I_i32:
.LLFB126:
	save	%sp, -96, %sp
.LLCFI126:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+72], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE126:
	.size	EQ__EQ_var_I_i32, .-EQ__EQ_var_I_i32
	.align 4
	.type	EQ__EQ_param_I_i32, #function
	.proc	014
EQ__EQ_param_I_i32:
.LLFB127:
	save	%sp, -96, %sp
.LLCFI127:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE127:
	.size	EQ__EQ_param_I_i32, .-EQ__EQ_param_I_i32
	.align 4
	.type	EQ__EQ_var_I_i16, #function
	.proc	014
EQ__EQ_var_I_i16:
.LLFB128:
	save	%sp, -96, %sp
.LLCFI128:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE128:
	.size	EQ__EQ_var_I_i16, .-EQ__EQ_var_I_i16
	.align 4
	.type	EQ__EQ_param_I_i16, #function
	.proc	014
EQ__EQ_param_I_i16:
.LLFB129:
	save	%sp, -96, %sp
.LLCFI129:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+68], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE129:
	.size	EQ__EQ_param_I_i16, .-EQ__EQ_param_I_i16
	.align 4
	.type	EQ__EQ_var_I_I, #function
	.proc	014
EQ__EQ_var_I_I:
.LLFB130:
	save	%sp, -96, %sp
.LLCFI130:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE130:
	.size	EQ__EQ_var_I_I, .-EQ__EQ_var_I_I
	.align 4
	.type	EQ__EQ_param_I_I, #function
	.proc	014
EQ__EQ_param_I_I:
.LLFB131:
	save	%sp, -96, %sp
.LLCFI131:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE131:
	.size	EQ__EQ_param_I_I, .-EQ__EQ_param_I_I
	.align 4
	.type	EQ__EQ_var_I_i64, #function
	.proc	014
EQ__EQ_var_I_i64:
.LLFB132:
	save	%sp, -96, %sp
.LLCFI132:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+88], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE132:
	.size	EQ__EQ_var_I_i64, .-EQ__EQ_var_I_i64
	.align 4
	.type	EQ__EQ_param_I_i64, #function
	.proc	014
EQ__EQ_param_I_i64:
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
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE133:
	.size	EQ__EQ_param_I_i64, .-EQ__EQ_param_I_i64
	.align 4
	.type	EQ__EQ_var_I_C, #function
	.proc	014
EQ__EQ_var_I_C:
.LLFB134:
	save	%sp, -96, %sp
.LLCFI134:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+96], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE134:
	.size	EQ__EQ_var_I_C, .-EQ__EQ_var_I_C
	.align 4
	.type	EQ__EQ_param_I_C, #function
	.proc	014
EQ__EQ_param_I_C:
.LLFB135:
	save	%sp, -96, %sp
.LLCFI135:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE135:
	.size	EQ__EQ_param_I_C, .-EQ__EQ_param_I_C
	.align 4
	.type	EQ__EQ_var_I_u8, #function
	.proc	014
EQ__EQ_var_I_u8:
.LLFB136:
	save	%sp, -96, %sp
.LLCFI136:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE136:
	.size	EQ__EQ_var_I_u8, .-EQ__EQ_var_I_u8
	.align 4
	.type	EQ__EQ_param_I_u8, #function
	.proc	014
EQ__EQ_param_I_u8:
.LLFB137:
	save	%sp, -96, %sp
.LLCFI137:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+68], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE137:
	.size	EQ__EQ_param_I_u8, .-EQ__EQ_param_I_u8
	.align 4
	.type	EQ__EQ_var_I_L, #function
	.proc	014
EQ__EQ_var_I_L:
.LLFB138:
	save	%sp, -96, %sp
.LLCFI138:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+120], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE138:
	.size	EQ__EQ_var_I_L, .-EQ__EQ_var_I_L
	.align 4
	.type	EQ__EQ_param_I_L, #function
	.proc	014
EQ__EQ_param_I_L:
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
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE139:
	.size	EQ__EQ_param_I_L, .-EQ__EQ_param_I_L
	.align 4
	.type	EQ__EQ_var_I_i8, #function
	.proc	014
EQ__EQ_var_I_i8:
.LLFB140:
	save	%sp, -96, %sp
.LLCFI140:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE140:
	.size	EQ__EQ_var_I_i8, .-EQ__EQ_var_I_i8
	.align 4
	.type	EQ__EQ_param_I_i8, #function
	.proc	014
EQ__EQ_param_I_i8:
.LLFB141:
	save	%sp, -96, %sp
.LLCFI141:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+68], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE141:
	.size	EQ__EQ_param_I_i8, .-EQ__EQ_param_I_i8
	.align 4
	.type	EQ__EQ_var_I_u32, #function
	.proc	014
EQ__EQ_var_I_u32:
.LLFB142:
	save	%sp, -96, %sp
.LLCFI142:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+80], %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+132], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE142:
	.size	EQ__EQ_var_I_u32, .-EQ__EQ_var_I_u32
	.align 4
	.type	EQ__EQ_param_I_u32, #function
	.proc	014
EQ__EQ_param_I_u32:
.LLFB143:
	save	%sp, -96, %sp
.LLCFI143:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE143:
	.size	EQ__EQ_param_I_u32, .-EQ__EQ_param_I_u32
	.align 4
	.type	EQ__EQ_var_i64_u16, #function
	.proc	014
EQ__EQ_var_i64_u16:
.LLFB144:
	save	%sp, -96, %sp
.LLCFI144:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+88], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE144:
	.size	EQ__EQ_var_i64_u16, .-EQ__EQ_var_i64_u16
	.align 4
	.type	EQ__EQ_param_i64_u16, #function
	.proc	014
EQ__EQ_param_i64_u16:
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
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE145:
	.size	EQ__EQ_param_i64_u16, .-EQ__EQ_param_i64_u16
	.align 4
	.type	EQ__EQ_var_i64_u64, #function
	.proc	014
EQ__EQ_var_i64_u64:
.LLFB146:
	save	%sp, -96, %sp
.LLCFI146:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+88], %o2
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+56], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE146:
	.size	EQ__EQ_var_i64_u64, .-EQ__EQ_var_i64_u64
	.align 4
	.type	EQ__EQ_param_i64_u64, #function
	.proc	014
EQ__EQ_param_i64_u64:
.LLFB147:
	save	%sp, -112, %sp
.LLCFI147:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE147:
	.size	EQ__EQ_param_i64_u64, .-EQ__EQ_param_i64_u64
	.align 4
	.type	EQ__EQ_var_i64_LC, #function
	.proc	014
EQ__EQ_var_i64_LC:
.LLFB148:
	save	%sp, -96, %sp
.LLCFI148:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+88], %o2
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+64], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE148:
	.size	EQ__EQ_var_i64_LC, .-EQ__EQ_var_i64_LC
	.align 4
	.type	EQ__EQ_param_i64_LC, #function
	.proc	014
EQ__EQ_param_i64_LC:
.LLFB149:
	save	%sp, -112, %sp
.LLCFI149:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o2
	ldd	[%fp-8], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE149:
	.size	EQ__EQ_param_i64_LC, .-EQ__EQ_param_i64_LC
	.align 4
	.type	EQ__EQ_var_i64_i32, #function
	.proc	014
EQ__EQ_var_i64_i32:
.LLFB150:
	save	%sp, -96, %sp
.LLCFI150:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+88], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE150:
	.size	EQ__EQ_var_i64_i32, .-EQ__EQ_var_i64_i32
	.align 4
	.type	EQ__EQ_param_i64_i32, #function
	.proc	014
EQ__EQ_param_i64_i32:
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
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE151:
	.size	EQ__EQ_param_i64_i32, .-EQ__EQ_param_i64_i32
	.align 4
	.type	EQ__EQ_var_i64_i16, #function
	.proc	014
EQ__EQ_var_i64_i16:
.LLFB152:
	save	%sp, -96, %sp
.LLCFI152:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+88], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE152:
	.size	EQ__EQ_var_i64_i16, .-EQ__EQ_var_i64_i16
	.align 4
	.type	EQ__EQ_param_i64_i16, #function
	.proc	014
EQ__EQ_param_i64_i16:
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
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE153:
	.size	EQ__EQ_param_i64_i16, .-EQ__EQ_param_i64_i16
	.align 4
	.type	EQ__EQ_var_i64_I, #function
	.proc	014
EQ__EQ_var_i64_I:
.LLFB154:
	save	%sp, -96, %sp
.LLCFI154:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+88], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE154:
	.size	EQ__EQ_var_i64_I, .-EQ__EQ_var_i64_I
	.align 4
	.type	EQ__EQ_param_i64_I, #function
	.proc	014
EQ__EQ_param_i64_I:
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
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE155:
	.size	EQ__EQ_param_i64_I, .-EQ__EQ_param_i64_I
	.align 4
	.type	EQ__EQ_var_i64_i64, #function
	.proc	014
EQ__EQ_var_i64_i64:
.LLFB156:
	save	%sp, -96, %sp
.LLCFI156:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE156:
	.size	EQ__EQ_var_i64_i64, .-EQ__EQ_var_i64_i64
	.align 4
	.type	EQ__EQ_param_i64_i64, #function
	.proc	014
EQ__EQ_param_i64_i64:
.LLFB157:
	save	%sp, -112, %sp
.LLCFI157:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE157:
	.size	EQ__EQ_param_i64_i64, .-EQ__EQ_param_i64_i64
	.align 4
	.type	EQ__EQ_var_i64_C, #function
	.proc	014
EQ__EQ_var_i64_C:
.LLFB158:
	save	%sp, -96, %sp
.LLCFI158:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+88], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE158:
	.size	EQ__EQ_var_i64_C, .-EQ__EQ_var_i64_C
	.align 4
	.type	EQ__EQ_param_i64_C, #function
	.proc	014
EQ__EQ_param_i64_C:
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
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE159:
	.size	EQ__EQ_param_i64_C, .-EQ__EQ_param_i64_C
	.align 4
	.type	EQ__EQ_var_i64_u8, #function
	.proc	014
EQ__EQ_var_i64_u8:
.LLFB160:
	save	%sp, -96, %sp
.LLCFI160:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+88], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE160:
	.size	EQ__EQ_var_i64_u8, .-EQ__EQ_var_i64_u8
	.align 4
	.type	EQ__EQ_param_i64_u8, #function
	.proc	014
EQ__EQ_param_i64_u8:
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
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE161:
	.size	EQ__EQ_param_i64_u8, .-EQ__EQ_param_i64_u8
	.align 4
	.type	EQ__EQ_var_i64_L, #function
	.proc	014
EQ__EQ_var_i64_L:
.LLFB162:
	save	%sp, -96, %sp
.LLCFI162:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+88], %o2
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+120], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE162:
	.size	EQ__EQ_var_i64_L, .-EQ__EQ_var_i64_L
	.align 4
	.type	EQ__EQ_param_i64_L, #function
	.proc	014
EQ__EQ_param_i64_L:
.LLFB163:
	save	%sp, -112, %sp
.LLCFI163:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE163:
	.size	EQ__EQ_param_i64_L, .-EQ__EQ_param_i64_L
	.align 4
	.type	EQ__EQ_var_i64_i8, #function
	.proc	014
EQ__EQ_var_i64_i8:
.LLFB164:
	save	%sp, -96, %sp
.LLCFI164:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+88], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE164:
	.size	EQ__EQ_var_i64_i8, .-EQ__EQ_var_i64_i8
	.align 4
	.type	EQ__EQ_param_i64_i8, #function
	.proc	014
EQ__EQ_param_i64_i8:
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
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE165:
	.size	EQ__EQ_param_i64_i8, .-EQ__EQ_param_i64_i8
	.align 4
	.type	EQ__EQ_var_i64_u32, #function
	.proc	014
EQ__EQ_var_i64_u32:
.LLFB166:
	save	%sp, -96, %sp
.LLCFI166:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+88], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE166:
	.size	EQ__EQ_var_i64_u32, .-EQ__EQ_var_i64_u32
	.align 4
	.type	EQ__EQ_param_i64_u32, #function
	.proc	014
EQ__EQ_param_i64_u32:
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
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE167:
	.size	EQ__EQ_param_i64_u32, .-EQ__EQ_param_i64_u32
	.align 4
	.type	EQ__EQ_var_C_u16, #function
	.proc	014
EQ__EQ_var_C_u16:
.LLFB168:
	save	%sp, -96, %sp
.LLCFI168:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE168:
	.size	EQ__EQ_var_C_u16, .-EQ__EQ_var_C_u16
	.align 4
	.type	EQ__EQ_param_C_u16, #function
	.proc	014
EQ__EQ_param_C_u16:
.LLFB169:
	save	%sp, -96, %sp
.LLCFI169:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+68], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE169:
	.size	EQ__EQ_param_C_u16, .-EQ__EQ_param_C_u16
	.align 4
	.type	EQ__EQ_var_C_u64, #function
	.proc	014
EQ__EQ_var_C_u64:
.LLFB170:
	save	%sp, -96, %sp
.LLCFI170:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+56], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE170:
	.size	EQ__EQ_var_C_u64, .-EQ__EQ_var_C_u64
	.align 4
	.type	EQ__EQ_param_C_u64, #function
	.proc	014
EQ__EQ_param_C_u64:
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
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE171:
	.size	EQ__EQ_param_C_u64, .-EQ__EQ_param_C_u64
	.align 4
	.type	EQ__EQ_var_C_LC, #function
	.proc	014
EQ__EQ_var_C_LC:
.LLFB172:
	save	%sp, -96, %sp
.LLCFI172:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+64], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE172:
	.size	EQ__EQ_var_C_LC, .-EQ__EQ_var_C_LC
	.align 4
	.type	EQ__EQ_param_C_LC, #function
	.proc	014
EQ__EQ_param_C_LC:
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
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE173:
	.size	EQ__EQ_param_C_LC, .-EQ__EQ_param_C_LC
	.align 4
	.type	EQ__EQ_var_C_i32, #function
	.proc	014
EQ__EQ_var_C_i32:
.LLFB174:
	save	%sp, -96, %sp
.LLCFI174:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+72], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE174:
	.size	EQ__EQ_var_C_i32, .-EQ__EQ_var_C_i32
	.align 4
	.type	EQ__EQ_param_C_i32, #function
	.proc	014
EQ__EQ_param_C_i32:
.LLFB175:
	save	%sp, -96, %sp
.LLCFI175:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE175:
	.size	EQ__EQ_param_C_i32, .-EQ__EQ_param_C_i32
	.align 4
	.type	EQ__EQ_var_C_i16, #function
	.proc	014
EQ__EQ_var_C_i16:
.LLFB176:
	save	%sp, -96, %sp
.LLCFI176:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE176:
	.size	EQ__EQ_var_C_i16, .-EQ__EQ_var_C_i16
	.align 4
	.type	EQ__EQ_param_C_i16, #function
	.proc	014
EQ__EQ_param_C_i16:
.LLFB177:
	save	%sp, -96, %sp
.LLCFI177:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+68], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE177:
	.size	EQ__EQ_param_C_i16, .-EQ__EQ_param_C_i16
	.align 4
	.type	EQ__EQ_var_C_I, #function
	.proc	014
EQ__EQ_var_C_I:
.LLFB178:
	save	%sp, -96, %sp
.LLCFI178:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+80], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE178:
	.size	EQ__EQ_var_C_I, .-EQ__EQ_var_C_I
	.align 4
	.type	EQ__EQ_param_C_I, #function
	.proc	014
EQ__EQ_param_C_I:
.LLFB179:
	save	%sp, -96, %sp
.LLCFI179:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE179:
	.size	EQ__EQ_param_C_I, .-EQ__EQ_param_C_I
	.align 4
	.type	EQ__EQ_var_C_i64, #function
	.proc	014
EQ__EQ_var_C_i64:
.LLFB180:
	save	%sp, -96, %sp
.LLCFI180:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+88], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE180:
	.size	EQ__EQ_var_C_i64, .-EQ__EQ_var_C_i64
	.align 4
	.type	EQ__EQ_param_C_i64, #function
	.proc	014
EQ__EQ_param_C_i64:
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
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE181:
	.size	EQ__EQ_param_C_i64, .-EQ__EQ_param_C_i64
	.align 4
	.type	EQ__EQ_var_C_C, #function
	.proc	014
EQ__EQ_var_C_C:
.LLFB182:
	save	%sp, -96, %sp
.LLCFI182:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE182:
	.size	EQ__EQ_var_C_C, .-EQ__EQ_var_C_C
	.align 4
	.type	EQ__EQ_param_C_C, #function
	.proc	014
EQ__EQ_param_C_C:
.LLFB183:
	save	%sp, -96, %sp
.LLCFI183:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE183:
	.size	EQ__EQ_param_C_C, .-EQ__EQ_param_C_C
	.align 4
	.type	EQ__EQ_var_C_u8, #function
	.proc	014
EQ__EQ_var_C_u8:
.LLFB184:
	save	%sp, -96, %sp
.LLCFI184:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE184:
	.size	EQ__EQ_var_C_u8, .-EQ__EQ_var_C_u8
	.align 4
	.type	EQ__EQ_param_C_u8, #function
	.proc	014
EQ__EQ_param_C_u8:
.LLFB185:
	save	%sp, -96, %sp
.LLCFI185:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+68], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE185:
	.size	EQ__EQ_param_C_u8, .-EQ__EQ_param_C_u8
	.align 4
	.type	EQ__EQ_var_C_L, #function
	.proc	014
EQ__EQ_var_C_L:
.LLFB186:
	save	%sp, -96, %sp
.LLCFI186:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+120], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE186:
	.size	EQ__EQ_var_C_L, .-EQ__EQ_var_C_L
	.align 4
	.type	EQ__EQ_param_C_L, #function
	.proc	014
EQ__EQ_param_C_L:
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
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE187:
	.size	EQ__EQ_param_C_L, .-EQ__EQ_param_C_L
	.align 4
	.type	EQ__EQ_var_C_i8, #function
	.proc	014
EQ__EQ_var_C_i8:
.LLFB188:
	save	%sp, -96, %sp
.LLCFI188:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE188:
	.size	EQ__EQ_var_C_i8, .-EQ__EQ_var_C_i8
	.align 4
	.type	EQ__EQ_param_C_i8, #function
	.proc	014
EQ__EQ_param_C_i8:
.LLFB189:
	save	%sp, -96, %sp
.LLCFI189:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+68], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE189:
	.size	EQ__EQ_param_C_i8, .-EQ__EQ_param_C_i8
	.align 4
	.type	EQ__EQ_var_C_u32, #function
	.proc	014
EQ__EQ_var_C_u32:
.LLFB190:
	save	%sp, -96, %sp
.LLCFI190:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+96], %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+132], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE190:
	.size	EQ__EQ_var_C_u32, .-EQ__EQ_var_C_u32
	.align 4
	.type	EQ__EQ_param_C_u32, #function
	.proc	014
EQ__EQ_param_C_u32:
.LLFB191:
	save	%sp, -96, %sp
.LLCFI191:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE191:
	.size	EQ__EQ_param_C_u32, .-EQ__EQ_param_C_u32
	.align 4
	.type	EQ__EQ_var_f64_f64, #function
	.proc	014
EQ__EQ_var_f64_f64:
.LLFB192:
	save	%sp, -96, %sp
.LLCFI192:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	add	%g1, 104, %g1
	ldd	[%g1], %f10
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	add	%g1, 104, %g1
	ldd	[%g1], %f8
	fcmpd	%fcc0, %f10, %f8
	mov	0, %g1
	move	%fcc0, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE192:
	.size	EQ__EQ_var_f64_f64, .-EQ__EQ_var_f64_f64
	.align 4
	.type	EQ__EQ_param_f64_f64, #function
	.proc	014
EQ__EQ_param_f64_f64:
.LLFB193:
	save	%sp, -112, %sp
.LLCFI193:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %f10
	ldd	[%fp-16], %f8
	fcmpd	%fcc1, %f10, %f8
	mov	0, %g1
	move	%fcc1, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE193:
	.size	EQ__EQ_param_f64_f64, .-EQ__EQ_param_f64_f64
	.align 4
	.type	EQ__EQ_var_u8_u16, #function
	.proc	014
EQ__EQ_var_u8_u16:
.LLFB194:
	save	%sp, -96, %sp
.LLCFI194:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE194:
	.size	EQ__EQ_var_u8_u16, .-EQ__EQ_var_u8_u16
	.align 4
	.type	EQ__EQ_param_u8_u16, #function
	.proc	014
EQ__EQ_param_u8_u16:
.LLFB195:
	save	%sp, -96, %sp
.LLCFI195:
	stb	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE195:
	.size	EQ__EQ_param_u8_u16, .-EQ__EQ_param_u8_u16
	.align 4
	.type	EQ__EQ_var_u8_u64, #function
	.proc	014
EQ__EQ_var_u8_u64:
.LLFB196:
	save	%sp, -96, %sp
.LLCFI196:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+56], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE196:
	.size	EQ__EQ_var_u8_u64, .-EQ__EQ_var_u8_u64
	.align 4
	.type	EQ__EQ_param_u8_u64, #function
	.proc	014
EQ__EQ_param_u8_u64:
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
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE197:
	.size	EQ__EQ_param_u8_u64, .-EQ__EQ_param_u8_u64
	.align 4
	.type	EQ__EQ_var_u8_LC, #function
	.proc	014
EQ__EQ_var_u8_LC:
.LLFB198:
	save	%sp, -96, %sp
.LLCFI198:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+64], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE198:
	.size	EQ__EQ_var_u8_LC, .-EQ__EQ_var_u8_LC
	.align 4
	.type	EQ__EQ_param_u8_LC, #function
	.proc	014
EQ__EQ_param_u8_LC:
.LLFB199:
	save	%sp, -104, %sp
.LLCFI199:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE199:
	.size	EQ__EQ_param_u8_LC, .-EQ__EQ_param_u8_LC
	.align 4
	.type	EQ__EQ_var_u8_i32, #function
	.proc	014
EQ__EQ_var_u8_i32:
.LLFB200:
	save	%sp, -96, %sp
.LLCFI200:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+72], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE200:
	.size	EQ__EQ_var_u8_i32, .-EQ__EQ_var_u8_i32
	.align 4
	.type	EQ__EQ_param_u8_i32, #function
	.proc	014
EQ__EQ_param_u8_i32:
.LLFB201:
	save	%sp, -96, %sp
.LLCFI201:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE201:
	.size	EQ__EQ_param_u8_i32, .-EQ__EQ_param_u8_i32
	.align 4
	.type	EQ__EQ_var_u8_i16, #function
	.proc	014
EQ__EQ_var_u8_i16:
.LLFB202:
	save	%sp, -96, %sp
.LLCFI202:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE202:
	.size	EQ__EQ_var_u8_i16, .-EQ__EQ_var_u8_i16
	.align 4
	.type	EQ__EQ_param_u8_i16, #function
	.proc	014
EQ__EQ_param_u8_i16:
.LLFB203:
	save	%sp, -96, %sp
.LLCFI203:
	stb	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE203:
	.size	EQ__EQ_param_u8_i16, .-EQ__EQ_param_u8_i16
	.align 4
	.type	EQ__EQ_var_u8_I, #function
	.proc	014
EQ__EQ_var_u8_I:
.LLFB204:
	save	%sp, -96, %sp
.LLCFI204:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+80], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE204:
	.size	EQ__EQ_var_u8_I, .-EQ__EQ_var_u8_I
	.align 4
	.type	EQ__EQ_param_u8_I, #function
	.proc	014
EQ__EQ_param_u8_I:
.LLFB205:
	save	%sp, -96, %sp
.LLCFI205:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE205:
	.size	EQ__EQ_param_u8_I, .-EQ__EQ_param_u8_I
	.align 4
	.type	EQ__EQ_var_u8_i64, #function
	.proc	014
EQ__EQ_var_u8_i64:
.LLFB206:
	save	%sp, -96, %sp
.LLCFI206:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+88], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE206:
	.size	EQ__EQ_var_u8_i64, .-EQ__EQ_var_u8_i64
	.align 4
	.type	EQ__EQ_param_u8_i64, #function
	.proc	014
EQ__EQ_param_u8_i64:
.LLFB207:
	save	%sp, -104, %sp
.LLCFI207:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE207:
	.size	EQ__EQ_param_u8_i64, .-EQ__EQ_param_u8_i64
	.align 4
	.type	EQ__EQ_var_u8_C, #function
	.proc	014
EQ__EQ_var_u8_C:
.LLFB208:
	save	%sp, -96, %sp
.LLCFI208:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+96], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE208:
	.size	EQ__EQ_var_u8_C, .-EQ__EQ_var_u8_C
	.align 4
	.type	EQ__EQ_param_u8_C, #function
	.proc	014
EQ__EQ_param_u8_C:
.LLFB209:
	save	%sp, -96, %sp
.LLCFI209:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE209:
	.size	EQ__EQ_param_u8_C, .-EQ__EQ_param_u8_C
	.align 4
	.type	EQ__EQ_var_u8_u8, #function
	.proc	014
EQ__EQ_var_u8_u8:
.LLFB210:
	save	%sp, -96, %sp
.LLCFI210:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE210:
	.size	EQ__EQ_var_u8_u8, .-EQ__EQ_var_u8_u8
	.align 4
	.type	EQ__EQ_param_u8_u8, #function
	.proc	014
EQ__EQ_param_u8_u8:
.LLFB211:
	save	%sp, -96, %sp
.LLCFI211:
	stb	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE211:
	.size	EQ__EQ_param_u8_u8, .-EQ__EQ_param_u8_u8
	.align 4
	.type	EQ__EQ_var_u8_L, #function
	.proc	014
EQ__EQ_var_u8_L:
.LLFB212:
	save	%sp, -96, %sp
.LLCFI212:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+120], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE212:
	.size	EQ__EQ_var_u8_L, .-EQ__EQ_var_u8_L
	.align 4
	.type	EQ__EQ_param_u8_L, #function
	.proc	014
EQ__EQ_param_u8_L:
.LLFB213:
	save	%sp, -104, %sp
.LLCFI213:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE213:
	.size	EQ__EQ_param_u8_L, .-EQ__EQ_param_u8_L
	.align 4
	.type	EQ__EQ_var_u8_i8, #function
	.proc	014
EQ__EQ_var_u8_i8:
.LLFB214:
	save	%sp, -96, %sp
.LLCFI214:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE214:
	.size	EQ__EQ_var_u8_i8, .-EQ__EQ_var_u8_i8
	.align 4
	.type	EQ__EQ_param_u8_i8, #function
	.proc	014
EQ__EQ_param_u8_i8:
.LLFB215:
	save	%sp, -96, %sp
.LLCFI215:
	stb	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE215:
	.size	EQ__EQ_param_u8_i8, .-EQ__EQ_param_u8_i8
	.align 4
	.type	EQ__EQ_var_u8_u32, #function
	.proc	014
EQ__EQ_var_u8_u32:
.LLFB216:
	save	%sp, -96, %sp
.LLCFI216:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+132], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE216:
	.size	EQ__EQ_var_u8_u32, .-EQ__EQ_var_u8_u32
	.align 4
	.type	EQ__EQ_param_u8_u32, #function
	.proc	014
EQ__EQ_param_u8_u32:
.LLFB217:
	save	%sp, -96, %sp
.LLCFI217:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+72], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE217:
	.size	EQ__EQ_param_u8_u32, .-EQ__EQ_param_u8_u32
	.align 4
	.type	EQ__EQ_var_L_u16, #function
	.proc	014
EQ__EQ_var_L_u16:
.LLFB218:
	save	%sp, -96, %sp
.LLCFI218:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+120], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE218:
	.size	EQ__EQ_var_L_u16, .-EQ__EQ_var_L_u16
	.align 4
	.type	EQ__EQ_param_L_u16, #function
	.proc	014
EQ__EQ_param_L_u16:
.LLFB219:
	save	%sp, -104, %sp
.LLCFI219:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE219:
	.size	EQ__EQ_param_L_u16, .-EQ__EQ_param_L_u16
	.align 4
	.type	EQ__EQ_var_L_u64, #function
	.proc	014
EQ__EQ_var_L_u64:
.LLFB220:
	save	%sp, -96, %sp
.LLCFI220:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+120], %o2
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+56], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE220:
	.size	EQ__EQ_var_L_u64, .-EQ__EQ_var_L_u64
	.align 4
	.type	EQ__EQ_param_L_u64, #function
	.proc	014
EQ__EQ_param_L_u64:
.LLFB221:
	save	%sp, -112, %sp
.LLCFI221:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE221:
	.size	EQ__EQ_param_L_u64, .-EQ__EQ_param_L_u64
	.align 4
	.type	EQ__EQ_var_L_LC, #function
	.proc	014
EQ__EQ_var_L_LC:
.LLFB222:
	save	%sp, -96, %sp
.LLCFI222:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+120], %o2
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+64], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE222:
	.size	EQ__EQ_var_L_LC, .-EQ__EQ_var_L_LC
	.align 4
	.type	EQ__EQ_param_L_LC, #function
	.proc	014
EQ__EQ_param_L_LC:
.LLFB223:
	save	%sp, -112, %sp
.LLCFI223:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o2
	ldd	[%fp-8], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE223:
	.size	EQ__EQ_param_L_LC, .-EQ__EQ_param_L_LC
	.align 4
	.type	EQ__EQ_var_L_i32, #function
	.proc	014
EQ__EQ_var_L_i32:
.LLFB224:
	save	%sp, -96, %sp
.LLCFI224:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+120], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE224:
	.size	EQ__EQ_var_L_i32, .-EQ__EQ_var_L_i32
	.align 4
	.type	EQ__EQ_param_L_i32, #function
	.proc	014
EQ__EQ_param_L_i32:
.LLFB225:
	save	%sp, -104, %sp
.LLCFI225:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE225:
	.size	EQ__EQ_param_L_i32, .-EQ__EQ_param_L_i32
	.align 4
	.type	EQ__EQ_var_L_i16, #function
	.proc	014
EQ__EQ_var_L_i16:
.LLFB226:
	save	%sp, -96, %sp
.LLCFI226:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+120], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE226:
	.size	EQ__EQ_var_L_i16, .-EQ__EQ_var_L_i16
	.align 4
	.type	EQ__EQ_param_L_i16, #function
	.proc	014
EQ__EQ_param_L_i16:
.LLFB227:
	save	%sp, -104, %sp
.LLCFI227:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE227:
	.size	EQ__EQ_param_L_i16, .-EQ__EQ_param_L_i16
	.align 4
	.type	EQ__EQ_var_L_I, #function
	.proc	014
EQ__EQ_var_L_I:
.LLFB228:
	save	%sp, -96, %sp
.LLCFI228:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+120], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE228:
	.size	EQ__EQ_var_L_I, .-EQ__EQ_var_L_I
	.align 4
	.type	EQ__EQ_param_L_I, #function
	.proc	014
EQ__EQ_param_L_I:
.LLFB229:
	save	%sp, -104, %sp
.LLCFI229:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE229:
	.size	EQ__EQ_param_L_I, .-EQ__EQ_param_L_I
	.align 4
	.type	EQ__EQ_var_L_i64, #function
	.proc	014
EQ__EQ_var_L_i64:
.LLFB230:
	save	%sp, -96, %sp
.LLCFI230:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+120], %o2
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+88], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE230:
	.size	EQ__EQ_var_L_i64, .-EQ__EQ_var_L_i64
	.align 4
	.type	EQ__EQ_param_L_i64, #function
	.proc	014
EQ__EQ_param_L_i64:
.LLFB231:
	save	%sp, -112, %sp
.LLCFI231:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE231:
	.size	EQ__EQ_param_L_i64, .-EQ__EQ_param_L_i64
	.align 4
	.type	EQ__EQ_var_L_C, #function
	.proc	014
EQ__EQ_var_L_C:
.LLFB232:
	save	%sp, -96, %sp
.LLCFI232:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+120], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE232:
	.size	EQ__EQ_var_L_C, .-EQ__EQ_var_L_C
	.align 4
	.type	EQ__EQ_param_L_C, #function
	.proc	014
EQ__EQ_param_L_C:
.LLFB233:
	save	%sp, -104, %sp
.LLCFI233:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE233:
	.size	EQ__EQ_param_L_C, .-EQ__EQ_param_L_C
	.align 4
	.type	EQ__EQ_var_L_u8, #function
	.proc	014
EQ__EQ_var_L_u8:
.LLFB234:
	save	%sp, -96, %sp
.LLCFI234:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+120], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE234:
	.size	EQ__EQ_var_L_u8, .-EQ__EQ_var_L_u8
	.align 4
	.type	EQ__EQ_param_L_u8, #function
	.proc	014
EQ__EQ_param_L_u8:
.LLFB235:
	save	%sp, -104, %sp
.LLCFI235:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE235:
	.size	EQ__EQ_param_L_u8, .-EQ__EQ_param_L_u8
	.align 4
	.type	EQ__EQ_var_L_L, #function
	.proc	014
EQ__EQ_var_L_L:
.LLFB236:
	save	%sp, -96, %sp
.LLCFI236:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE236:
	.size	EQ__EQ_var_L_L, .-EQ__EQ_var_L_L
	.align 4
	.type	EQ__EQ_param_L_L, #function
	.proc	014
EQ__EQ_param_L_L:
.LLFB237:
	save	%sp, -112, %sp
.LLCFI237:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	xor	%o2, %o4, %o4
	xor	%o3, %o5, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE237:
	.size	EQ__EQ_param_L_L, .-EQ__EQ_param_L_L
	.align 4
	.type	EQ__EQ_var_L_i8, #function
	.proc	014
EQ__EQ_var_L_i8:
.LLFB238:
	save	%sp, -96, %sp
.LLCFI238:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+120], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE238:
	.size	EQ__EQ_var_L_i8, .-EQ__EQ_var_L_i8
	.align 4
	.type	EQ__EQ_param_L_i8, #function
	.proc	014
EQ__EQ_param_L_i8:
.LLFB239:
	save	%sp, -104, %sp
.LLCFI239:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE239:
	.size	EQ__EQ_param_L_i8, .-EQ__EQ_param_L_i8
	.align 4
	.type	EQ__EQ_var_L_u32, #function
	.proc	014
EQ__EQ_var_L_u32:
.LLFB240:
	save	%sp, -96, %sp
.LLCFI240:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+120], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE240:
	.size	EQ__EQ_var_L_u32, .-EQ__EQ_var_L_u32
	.align 4
	.type	EQ__EQ_param_L_u32, #function
	.proc	014
EQ__EQ_param_L_u32:
.LLFB241:
	save	%sp, -104, %sp
.LLCFI241:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE241:
	.size	EQ__EQ_param_L_u32, .-EQ__EQ_param_L_u32
	.align 4
	.type	EQ__EQ_var_i8_u16, #function
	.proc	014
EQ__EQ_var_i8_u16:
.LLFB242:
	save	%sp, -96, %sp
.LLCFI242:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE242:
	.size	EQ__EQ_var_i8_u16, .-EQ__EQ_var_i8_u16
	.align 4
	.type	EQ__EQ_param_i8_u16, #function
	.proc	014
EQ__EQ_param_i8_u16:
.LLFB243:
	save	%sp, -96, %sp
.LLCFI243:
	stb	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE243:
	.size	EQ__EQ_param_i8_u16, .-EQ__EQ_param_i8_u16
	.align 4
	.type	EQ__EQ_var_i8_u64, #function
	.proc	014
EQ__EQ_var_i8_u64:
.LLFB244:
	save	%sp, -96, %sp
.LLCFI244:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+56], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE244:
	.size	EQ__EQ_var_i8_u64, .-EQ__EQ_var_i8_u64
	.align 4
	.type	EQ__EQ_param_i8_u64, #function
	.proc	014
EQ__EQ_param_i8_u64:
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
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE245:
	.size	EQ__EQ_param_i8_u64, .-EQ__EQ_param_i8_u64
	.align 4
	.type	EQ__EQ_var_i8_LC, #function
	.proc	014
EQ__EQ_var_i8_LC:
.LLFB246:
	save	%sp, -96, %sp
.LLCFI246:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+64], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE246:
	.size	EQ__EQ_var_i8_LC, .-EQ__EQ_var_i8_LC
	.align 4
	.type	EQ__EQ_param_i8_LC, #function
	.proc	014
EQ__EQ_param_i8_LC:
.LLFB247:
	save	%sp, -104, %sp
.LLCFI247:
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
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE247:
	.size	EQ__EQ_param_i8_LC, .-EQ__EQ_param_i8_LC
	.align 4
	.type	EQ__EQ_var_i8_i32, #function
	.proc	014
EQ__EQ_var_i8_i32:
.LLFB248:
	save	%sp, -96, %sp
.LLCFI248:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+72], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE248:
	.size	EQ__EQ_var_i8_i32, .-EQ__EQ_var_i8_i32
	.align 4
	.type	EQ__EQ_param_i8_i32, #function
	.proc	014
EQ__EQ_param_i8_i32:
.LLFB249:
	save	%sp, -96, %sp
.LLCFI249:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE249:
	.size	EQ__EQ_param_i8_i32, .-EQ__EQ_param_i8_i32
	.align 4
	.type	EQ__EQ_var_i8_i16, #function
	.proc	014
EQ__EQ_var_i8_i16:
.LLFB250:
	save	%sp, -96, %sp
.LLCFI250:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE250:
	.size	EQ__EQ_var_i8_i16, .-EQ__EQ_var_i8_i16
	.align 4
	.type	EQ__EQ_param_i8_i16, #function
	.proc	014
EQ__EQ_param_i8_i16:
.LLFB251:
	save	%sp, -96, %sp
.LLCFI251:
	stb	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE251:
	.size	EQ__EQ_param_i8_i16, .-EQ__EQ_param_i8_i16
	.align 4
	.type	EQ__EQ_var_i8_I, #function
	.proc	014
EQ__EQ_var_i8_I:
.LLFB252:
	save	%sp, -96, %sp
.LLCFI252:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+80], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE252:
	.size	EQ__EQ_var_i8_I, .-EQ__EQ_var_i8_I
	.align 4
	.type	EQ__EQ_param_i8_I, #function
	.proc	014
EQ__EQ_param_i8_I:
.LLFB253:
	save	%sp, -96, %sp
.LLCFI253:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE253:
	.size	EQ__EQ_param_i8_I, .-EQ__EQ_param_i8_I
	.align 4
	.type	EQ__EQ_var_i8_i64, #function
	.proc	014
EQ__EQ_var_i8_i64:
.LLFB254:
	save	%sp, -96, %sp
.LLCFI254:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+88], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE254:
	.size	EQ__EQ_var_i8_i64, .-EQ__EQ_var_i8_i64
	.align 4
	.type	EQ__EQ_param_i8_i64, #function
	.proc	014
EQ__EQ_param_i8_i64:
.LLFB255:
	save	%sp, -104, %sp
.LLCFI255:
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
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE255:
	.size	EQ__EQ_param_i8_i64, .-EQ__EQ_param_i8_i64
	.align 4
	.type	EQ__EQ_var_i8_C, #function
	.proc	014
EQ__EQ_var_i8_C:
.LLFB256:
	save	%sp, -96, %sp
.LLCFI256:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+96], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE256:
	.size	EQ__EQ_var_i8_C, .-EQ__EQ_var_i8_C
	.align 4
	.type	EQ__EQ_param_i8_C, #function
	.proc	014
EQ__EQ_param_i8_C:
.LLFB257:
	save	%sp, -96, %sp
.LLCFI257:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE257:
	.size	EQ__EQ_param_i8_C, .-EQ__EQ_param_i8_C
	.align 4
	.type	EQ__EQ_var_i8_u8, #function
	.proc	014
EQ__EQ_var_i8_u8:
.LLFB258:
	save	%sp, -96, %sp
.LLCFI258:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE258:
	.size	EQ__EQ_var_i8_u8, .-EQ__EQ_var_i8_u8
	.align 4
	.type	EQ__EQ_param_i8_u8, #function
	.proc	014
EQ__EQ_param_i8_u8:
.LLFB259:
	save	%sp, -96, %sp
.LLCFI259:
	stb	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE259:
	.size	EQ__EQ_param_i8_u8, .-EQ__EQ_param_i8_u8
	.align 4
	.type	EQ__EQ_var_i8_L, #function
	.proc	014
EQ__EQ_var_i8_L:
.LLFB260:
	save	%sp, -96, %sp
.LLCFI260:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+120], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE260:
	.size	EQ__EQ_var_i8_L, .-EQ__EQ_var_i8_L
	.align 4
	.type	EQ__EQ_param_i8_L, #function
	.proc	014
EQ__EQ_param_i8_L:
.LLFB261:
	save	%sp, -104, %sp
.LLCFI261:
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
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE261:
	.size	EQ__EQ_param_i8_L, .-EQ__EQ_param_i8_L
	.align 4
	.type	EQ__EQ_var_i8_i8, #function
	.proc	014
EQ__EQ_var_i8_i8:
.LLFB262:
	save	%sp, -96, %sp
.LLCFI262:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE262:
	.size	EQ__EQ_var_i8_i8, .-EQ__EQ_var_i8_i8
	.align 4
	.type	EQ__EQ_param_i8_i8, #function
	.proc	014
EQ__EQ_param_i8_i8:
.LLFB263:
	save	%sp, -96, %sp
.LLCFI263:
	stb	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE263:
	.size	EQ__EQ_param_i8_i8, .-EQ__EQ_param_i8_i8
	.align 4
	.type	EQ__EQ_var_i8_u32, #function
	.proc	014
EQ__EQ_var_i8_u32:
.LLFB264:
	save	%sp, -96, %sp
.LLCFI264:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+132], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE264:
	.size	EQ__EQ_var_i8_u32, .-EQ__EQ_var_i8_u32
	.align 4
	.type	EQ__EQ_param_i8_u32, #function
	.proc	014
EQ__EQ_param_i8_u32:
.LLFB265:
	save	%sp, -96, %sp
.LLCFI265:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+72], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE265:
	.size	EQ__EQ_param_i8_u32, .-EQ__EQ_param_i8_u32
	.align 4
	.type	EQ__EQ_var_u32_u16, #function
	.proc	014
EQ__EQ_var_u32_u16:
.LLFB266:
	save	%sp, -96, %sp
.LLCFI266:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+132], %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE266:
	.size	EQ__EQ_var_u32_u16, .-EQ__EQ_var_u32_u16
	.align 4
	.type	EQ__EQ_param_u32_u16, #function
	.proc	014
EQ__EQ_param_u32_u16:
.LLFB267:
	save	%sp, -96, %sp
.LLCFI267:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+68], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE267:
	.size	EQ__EQ_param_u32_u16, .-EQ__EQ_param_u32_u16
	.align 4
	.type	EQ__EQ_var_u32_u64, #function
	.proc	014
EQ__EQ_var_u32_u64:
.LLFB268:
	save	%sp, -96, %sp
.LLCFI268:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+56], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE268:
	.size	EQ__EQ_var_u32_u64, .-EQ__EQ_var_u32_u64
	.align 4
	.type	EQ__EQ_param_u32_u64, #function
	.proc	014
EQ__EQ_param_u32_u64:
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
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE269:
	.size	EQ__EQ_param_u32_u64, .-EQ__EQ_param_u32_u64
	.align 4
	.type	EQ__EQ_var_u32_LC, #function
	.proc	014
EQ__EQ_var_u32_LC:
.LLFB270:
	save	%sp, -96, %sp
.LLCFI270:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+64], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE270:
	.size	EQ__EQ_var_u32_LC, .-EQ__EQ_var_u32_LC
	.align 4
	.type	EQ__EQ_param_u32_LC, #function
	.proc	014
EQ__EQ_param_u32_LC:
.LLFB271:
	save	%sp, -104, %sp
.LLCFI271:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE271:
	.size	EQ__EQ_param_u32_LC, .-EQ__EQ_param_u32_LC
	.align 4
	.type	EQ__EQ_var_u32_i32, #function
	.proc	014
EQ__EQ_var_u32_i32:
.LLFB272:
	save	%sp, -96, %sp
.LLCFI272:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+132], %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+72], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE272:
	.size	EQ__EQ_var_u32_i32, .-EQ__EQ_var_u32_i32
	.align 4
	.type	EQ__EQ_param_u32_i32, #function
	.proc	014
EQ__EQ_param_u32_i32:
.LLFB273:
	save	%sp, -96, %sp
.LLCFI273:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE273:
	.size	EQ__EQ_param_u32_i32, .-EQ__EQ_param_u32_i32
	.align 4
	.type	EQ__EQ_var_u32_i16, #function
	.proc	014
EQ__EQ_var_u32_i16:
.LLFB274:
	save	%sp, -96, %sp
.LLCFI274:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+132], %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE274:
	.size	EQ__EQ_var_u32_i16, .-EQ__EQ_var_u32_i16
	.align 4
	.type	EQ__EQ_param_u32_i16, #function
	.proc	014
EQ__EQ_param_u32_i16:
.LLFB275:
	save	%sp, -96, %sp
.LLCFI275:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+68], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE275:
	.size	EQ__EQ_param_u32_i16, .-EQ__EQ_param_u32_i16
	.align 4
	.type	EQ__EQ_var_u32_I, #function
	.proc	014
EQ__EQ_var_u32_I:
.LLFB276:
	save	%sp, -96, %sp
.LLCFI276:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+132], %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+80], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE276:
	.size	EQ__EQ_var_u32_I, .-EQ__EQ_var_u32_I
	.align 4
	.type	EQ__EQ_param_u32_I, #function
	.proc	014
EQ__EQ_param_u32_I:
.LLFB277:
	save	%sp, -96, %sp
.LLCFI277:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE277:
	.size	EQ__EQ_param_u32_I, .-EQ__EQ_param_u32_I
	.align 4
	.type	EQ__EQ_var_u32_i64, #function
	.proc	014
EQ__EQ_var_u32_i64:
.LLFB278:
	save	%sp, -96, %sp
.LLCFI278:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+88], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE278:
	.size	EQ__EQ_var_u32_i64, .-EQ__EQ_var_u32_i64
	.align 4
	.type	EQ__EQ_param_u32_i64, #function
	.proc	014
EQ__EQ_param_u32_i64:
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
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE279:
	.size	EQ__EQ_param_u32_i64, .-EQ__EQ_param_u32_i64
	.align 4
	.type	EQ__EQ_var_u32_C, #function
	.proc	014
EQ__EQ_var_u32_C:
.LLFB280:
	save	%sp, -96, %sp
.LLCFI280:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+132], %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+96], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE280:
	.size	EQ__EQ_var_u32_C, .-EQ__EQ_var_u32_C
	.align 4
	.type	EQ__EQ_param_u32_C, #function
	.proc	014
EQ__EQ_param_u32_C:
.LLFB281:
	save	%sp, -96, %sp
.LLCFI281:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE281:
	.size	EQ__EQ_param_u32_C, .-EQ__EQ_param_u32_C
	.align 4
	.type	EQ__EQ_var_u32_u8, #function
	.proc	014
EQ__EQ_var_u32_u8:
.LLFB282:
	save	%sp, -96, %sp
.LLCFI282:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+132], %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE282:
	.size	EQ__EQ_var_u32_u8, .-EQ__EQ_var_u32_u8
	.align 4
	.type	EQ__EQ_param_u32_u8, #function
	.proc	014
EQ__EQ_param_u32_u8:
.LLFB283:
	save	%sp, -96, %sp
.LLCFI283:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+68], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE283:
	.size	EQ__EQ_param_u32_u8, .-EQ__EQ_param_u32_u8
	.align 4
	.type	EQ__EQ_var_u32_L, #function
	.proc	014
EQ__EQ_var_u32_L:
.LLFB284:
	save	%sp, -96, %sp
.LLCFI284:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ldd	[%g1+120], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE284:
	.size	EQ__EQ_var_u32_L, .-EQ__EQ_var_u32_L
	.align 4
	.type	EQ__EQ_param_u32_L, #function
	.proc	014
EQ__EQ_param_u32_L:
.LLFB285:
	save	%sp, -104, %sp
.LLCFI285:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	xor	%o4, %o2, %o4
	xor	%o5, %o3, %o5
	or	%o4, %o5, %g1
	xor	%g1, 0, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE285:
	.size	EQ__EQ_param_u32_L, .-EQ__EQ_param_u32_L
	.align 4
	.type	EQ__EQ_var_u32_i8, #function
	.proc	014
EQ__EQ_var_u32_i8:
.LLFB286:
	save	%sp, -96, %sp
.LLCFI286:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+132], %o5
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE286:
	.size	EQ__EQ_var_u32_i8, .-EQ__EQ_var_u32_i8
	.align 4
	.type	EQ__EQ_param_u32_i8, #function
	.proc	014
EQ__EQ_param_u32_i8:
.LLFB287:
	save	%sp, -96, %sp
.LLCFI287:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+68], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE287:
	.size	EQ__EQ_param_u32_i8, .-EQ__EQ_param_u32_i8
	.align 4
	.type	EQ__EQ_var_u32_u32, #function
	.proc	014
EQ__EQ_var_u32_u32:
.LLFB288:
	save	%sp, -96, %sp
.LLCFI288:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE288:
	.size	EQ__EQ_var_u32_u32, .-EQ__EQ_var_u32_u32
	.align 4
	.type	EQ__EQ_param_u32_u32, #function
	.proc	014
EQ__EQ_param_u32_u32:
.LLFB289:
	save	%sp, -96, %sp
.LLCFI289:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	ld	[%fp+72], %g1
	xor	%o5, %g1, %g1
	subcc	%g0, %g1, %g0
	subx	%g0, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE289:
	.size	EQ__EQ_param_u32_u32, .-EQ__EQ_param_u32_u32
	.align 4
	.type	EQ__EQ_var_f32_f32, #function
	.proc	014
EQ__EQ_var_f32_f32:
.LLFB290:
	save	%sp, -96, %sp
.LLCFI290:
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	add	%g1, 136, %g1
	ld	[%g1], %f9
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	add	%g1, 136, %g1
	ld	[%g1], %f8
	fcmps	%fcc2, %f9, %f8
	mov	0, %g1
	move	%fcc2, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE290:
	.size	EQ__EQ_var_f32_f32, .-EQ__EQ_var_f32_f32
	.align 4
	.type	EQ__EQ_param_f32_f32, #function
	.proc	014
EQ__EQ_param_f32_f32:
.LLFB291:
	save	%sp, -96, %sp
.LLCFI291:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %f9
	ld	[%fp+72], %f8
	fcmps	%fcc3, %f9, %f8
	mov	0, %g1
	move	%fcc3, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE291:
	.size	EQ__EQ_param_f32_f32, .-EQ__EQ_param_f32_f32
	.align 4
	.global EQ_M3
	.type	EQ_M3, #function
	.proc	0120
EQ_M3:
.LLFB292:
	save	%sp, -96, %sp
.LLCFI292:
	st	%i0, [%fp+68]
	sethi	%hi(MM_EQ), %g1
	or	%g1, %lo(MM_EQ), %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE292:
	.size	EQ_M3, .-EQ_M3
	.section	".data"
	.align 8
	.type	L_1, #object
	.size	L_1, 6616
L_1:
	.ascii	"EQ_M3"
	.skip 1
	.ascii	"EQ_param_f32_f32"
	.skip 1
	.ascii	"EQ_var_f32_f32"
	.skip 1
	.ascii	"EQ_param_u32_u32"
	.skip 1
	.ascii	"EQ_var_u32_u32"
	.skip 1
	.ascii	"EQ_param_u32_i8"
	.skip 1
	.ascii	"EQ_var_u32_i8"
	.skip 1
	.ascii	"EQ_param_u32_L"
	.skip 1
	.ascii	"EQ_var_u32_L"
	.skip 1
	.ascii	"EQ_param_u32_u8"
	.skip 1
	.ascii	"EQ_var_u32_u8"
	.skip 1
	.ascii	"EQ_param_u32_C"
	.skip 1
	.ascii	"EQ_var_u32_C"
	.skip 1
	.ascii	"EQ_param_u32_i64"
	.skip 1
	.ascii	"EQ_var_u32_i64"
	.skip 1
	.ascii	"EQ_param_u32_I"
	.skip 1
	.ascii	"EQ_var_u32_I"
	.skip 1
	.ascii	"EQ_param_u32_i16"
	.skip 1
	.ascii	"EQ_var_u32_i16"
	.skip 1
	.ascii	"EQ_param_u32_i32"
	.skip 1
	.ascii	"EQ_var_u32_i32"
	.skip 1
	.ascii	"EQ_param_u32_LC"
	.skip 1
	.ascii	"EQ_var_u32_LC"
	.skip 1
	.ascii	"EQ_param_u32_u64"
	.skip 1
	.ascii	"EQ_var_u32_u64"
	.skip 1
	.ascii	"EQ_param_u32_u16"
	.skip 1
	.ascii	"EQ_var_u32_u16"
	.skip 1
	.ascii	"EQ_param_i8_u32"
	.skip 1
	.ascii	"EQ_var_i8_u32"
	.skip 1
	.ascii	"EQ_param_i8_i8"
	.skip 1
	.ascii	"EQ_var_i8_i8"
	.skip 1
	.ascii	"EQ_param_i8_L"
	.skip 1
	.ascii	"EQ_var_i8_L"
	.skip 1
	.ascii	"EQ_param_i8_u8"
	.skip 1
	.ascii	"EQ_var_i8_u8"
	.skip 1
	.ascii	"EQ_param_i8_C"
	.skip 1
	.ascii	"EQ_var_i8_C"
	.skip 1
	.ascii	"EQ_param_i8_i64"
	.skip 1
	.ascii	"EQ_var_i8_i64"
	.skip 1
	.ascii	"EQ_param_i8_I"
	.skip 1
	.ascii	"EQ_var_i8_I"
	.skip 1
	.ascii	"EQ_param_i8_i16"
	.skip 1
	.ascii	"EQ_var_i8_i16"
	.skip 1
	.ascii	"EQ_param_i8_i32"
	.skip 1
	.ascii	"EQ_var_i8_i32"
	.skip 1
	.ascii	"EQ_param_i8_LC"
	.skip 1
	.ascii	"EQ_var_i8_LC"
	.skip 1
	.ascii	"EQ_param_i8_u64"
	.skip 1
	.ascii	"EQ_var_i8_u64"
	.skip 1
	.ascii	"EQ_param_i8_u16"
	.skip 1
	.ascii	"EQ_var_i8_u16"
	.skip 1
	.ascii	"EQ_param_L_u32"
	.skip 1
	.ascii	"EQ_var_L_u32"
	.skip 1
	.ascii	"EQ_param_L_i8"
	.skip 1
	.ascii	"EQ_var_L_i8"
	.skip 1
	.ascii	"EQ_param_L_L"
	.skip 1
	.ascii	"EQ_var_L_L"
	.skip 1
	.ascii	"EQ_param_L_u8"
	.skip 1
	.ascii	"EQ_var_L_u8"
	.skip 1
	.ascii	"EQ_param_L_C"
	.skip 1
	.ascii	"EQ_var_L_C"
	.skip 1
	.ascii	"EQ_param_L_i64"
	.skip 1
	.ascii	"EQ_var_L_i64"
	.skip 1
	.ascii	"EQ_param_L_I"
	.skip 1
	.ascii	"EQ_var_L_I"
	.skip 1
	.ascii	"EQ_param_L_i16"
	.skip 1
	.ascii	"EQ_var_L_i16"
	.skip 1
	.ascii	"EQ_param_L_i32"
	.skip 1
	.ascii	"EQ_var_L_i32"
	.skip 1
	.ascii	"EQ_param_L_LC"
	.skip 1
	.ascii	"EQ_var_L_LC"
	.skip 1
	.ascii	"EQ_param_L_u64"
	.skip 1
	.ascii	"EQ_var_L_u64"
	.skip 1
	.ascii	"EQ_param_L_u16"
	.skip 1
	.ascii	"EQ_var_L_u16"
	.skip 1
	.ascii	"EQ_param_u8_u32"
	.skip 1
	.ascii	"EQ_var_u8_u32"
	.skip 1
	.ascii	"EQ_param_u8_i8"
	.skip 1
	.ascii	"EQ_var_u8_i8"
	.skip 1
	.ascii	"EQ_param_u8_L"
	.skip 1
	.ascii	"EQ_var_u8_L"
	.skip 1
	.ascii	"EQ_param_u8_u8"
	.skip 1
	.ascii	"EQ_var_u8_u8"
	.skip 1
	.ascii	"EQ_param_u8_C"
	.skip 1
	.ascii	"EQ_var_u8_C"
	.skip 1
	.ascii	"EQ_param_u8_i64"
	.skip 1
	.ascii	"EQ_var_u8_i64"
	.skip 1
	.ascii	"EQ_param_u8_I"
	.skip 1
	.ascii	"EQ_var_u8_I"
	.skip 1
	.ascii	"EQ_param_u8_i16"
	.skip 1
	.ascii	"EQ_var_u8_i16"
	.skip 1
	.ascii	"EQ_param_u8_i32"
	.skip 1
	.ascii	"EQ_var_u8_i32"
	.skip 1
	.ascii	"EQ_param_u8_LC"
	.skip 1
	.ascii	"EQ_var_u8_LC"
	.skip 1
	.ascii	"EQ_param_u8_u64"
	.skip 1
	.ascii	"EQ_var_u8_u64"
	.skip 1
	.ascii	"EQ_param_u8_u16"
	.skip 1
	.ascii	"EQ_var_u8_u16"
	.skip 1
	.ascii	"EQ_param_f64_f64"
	.skip 1
	.ascii	"EQ_var_f64_f64"
	.skip 1
	.ascii	"EQ_param_C_u32"
	.skip 1
	.ascii	"EQ_var_C_u32"
	.skip 1
	.ascii	"EQ_param_C_i8"
	.skip 1
	.ascii	"EQ_var_C_i8"
	.skip 1
	.ascii	"EQ_param_C_L"
	.skip 1
	.ascii	"EQ_var_C_L"
	.skip 1
	.ascii	"EQ_param_C_u8"
	.skip 1
	.ascii	"EQ_var_C_u8"
	.skip 1
	.ascii	"EQ_param_C_C"
	.skip 1
	.ascii	"EQ_var_C_C"
	.skip 1
	.ascii	"EQ_param_C_i64"
	.skip 1
	.ascii	"EQ_var_C_i64"
	.skip 1
	.ascii	"EQ_param_C_I"
	.skip 1
	.ascii	"EQ_var_C_I"
	.skip 1
	.ascii	"EQ_param_C_i16"
	.skip 1
	.ascii	"EQ_var_C_i16"
	.skip 1
	.ascii	"EQ_param_C_i32"
	.skip 1
	.ascii	"EQ_var_C_i32"
	.skip 1
	.ascii	"EQ_param_C_LC"
	.skip 1
	.ascii	"EQ_var_C_LC"
	.skip 1
	.ascii	"EQ_param_C_u64"
	.skip 1
	.ascii	"EQ_var_C_u64"
	.skip 1
	.ascii	"EQ_param_C_u16"
	.skip 1
	.ascii	"EQ_var_C_u16"
	.skip 1
	.ascii	"EQ_param_i64_u32"
	.skip 1
	.ascii	"EQ_var_i64_u32"
	.skip 1
	.ascii	"EQ_param_i64_i8"
	.skip 1
	.ascii	"EQ_var_i64_i8"
	.skip 1
	.ascii	"EQ_param_i64_L"
	.skip 1
	.ascii	"EQ_var_i64_L"
	.skip 1
	.ascii	"EQ_param_i64_u8"
	.skip 1
	.ascii	"EQ_var_i64_u8"
	.skip 1
	.ascii	"EQ_param_i64_C"
	.skip 1
	.ascii	"EQ_var_i64_C"
	.skip 1
	.ascii	"EQ_param_i64_i64"
	.skip 1
	.ascii	"EQ_var_i64_i64"
	.skip 1
	.ascii	"EQ_param_i64_I"
	.skip 1
	.ascii	"EQ_var_i64_I"
	.skip 1
	.ascii	"EQ_param_i64_i16"
	.skip 1
	.ascii	"EQ_var_i64_i16"
	.skip 1
	.ascii	"EQ_param_i64_i32"
	.skip 1
	.ascii	"EQ_var_i64_i32"
	.skip 1
	.ascii	"EQ_param_i64_LC"
	.skip 1
	.ascii	"EQ_var_i64_LC"
	.skip 1
	.ascii	"EQ_param_i64_u64"
	.skip 1
	.ascii	"EQ_var_i64_u64"
	.skip 1
	.ascii	"EQ_param_i64_u16"
	.skip 1
	.ascii	"EQ_var_i64_u16"
	.skip 1
	.ascii	"EQ_param_I_u32"
	.skip 1
	.ascii	"EQ_var_I_u32"
	.skip 1
	.ascii	"EQ_param_I_i8"
	.skip 1
	.ascii	"EQ_var_I_i8"
	.skip 1
	.ascii	"EQ_param_I_L"
	.skip 1
	.ascii	"EQ_var_I_L"
	.skip 1
	.ascii	"EQ_param_I_u8"
	.skip 1
	.ascii	"EQ_var_I_u8"
	.skip 1
	.ascii	"EQ_param_I_C"
	.skip 1
	.ascii	"EQ_var_I_C"
	.skip 1
	.ascii	"EQ_param_I_i64"
	.skip 1
	.ascii	"EQ_var_I_i64"
	.skip 1
	.ascii	"EQ_param_I_I"
	.skip 1
	.ascii	"EQ_var_I_I"
	.skip 1
	.ascii	"EQ_param_I_i16"
	.skip 1
	.ascii	"EQ_var_I_i16"
	.skip 1
	.ascii	"EQ_param_I_i32"
	.skip 1
	.ascii	"EQ_var_I_i32"
	.skip 1
	.ascii	"EQ_param_I_LC"
	.skip 1
	.ascii	"EQ_var_I_LC"
	.skip 1
	.ascii	"EQ_param_I_u64"
	.skip 1
	.ascii	"EQ_var_I_u64"
	.skip 1
	.ascii	"EQ_param_I_u16"
	.skip 1
	.ascii	"EQ_var_I_u16"
	.skip 1
	.ascii	"EQ_param_i16_u32"
	.skip 1
	.ascii	"EQ_var_i16_u32"
	.skip 1
	.ascii	"EQ_param_i16_i8"
	.skip 1
	.ascii	"EQ_var_i16_i8"
	.skip 1
	.ascii	"EQ_param_i16_L"
	.skip 1
	.ascii	"EQ_var_i16_L"
	.skip 1
	.ascii	"EQ_param_i16_u8"
	.skip 1
	.ascii	"EQ_var_i16_u8"
	.skip 1
	.ascii	"EQ_param_i16_C"
	.skip 1
	.ascii	"EQ_var_i16_C"
	.skip 1
	.ascii	"EQ_param_i16_i64"
	.skip 1
	.ascii	"EQ_var_i16_i64"
	.skip 1
	.ascii	"EQ_param_i16_I"
	.skip 1
	.ascii	"EQ_var_i16_I"
	.skip 1
	.ascii	"EQ_param_i16_i16"
	.skip 1
	.ascii	"EQ_var_i16_i16"
	.skip 1
	.ascii	"EQ_param_i16_i32"
	.skip 1
	.ascii	"EQ_var_i16_i32"
	.skip 1
	.ascii	"EQ_param_i16_LC"
	.skip 1
	.ascii	"EQ_var_i16_LC"
	.skip 1
	.ascii	"EQ_param_i16_u64"
	.skip 1
	.ascii	"EQ_var_i16_u64"
	.skip 1
	.ascii	"EQ_param_i16_u16"
	.skip 1
	.ascii	"EQ_var_i16_u16"
	.skip 1
	.ascii	"EQ_param_i32_u32"
	.skip 1
	.ascii	"EQ_var_i32_u32"
	.skip 1
	.ascii	"EQ_param_i32_i8"
	.skip 1
	.ascii	"EQ_var_i32_i8"
	.skip 1
	.ascii	"EQ_param_i32_L"
	.skip 1
	.ascii	"EQ_var_i32_L"
	.skip 1
	.ascii	"EQ_param_i32_u8"
	.skip 1
	.ascii	"EQ_var_i32_u8"
	.skip 1
	.ascii	"EQ_param_i32_C"
	.skip 1
	.ascii	"EQ_var_i32_C"
	.skip 1
	.ascii	"EQ_param_i32_i64"
	.skip 1
	.ascii	"EQ_var_i32_i64"
	.skip 1
	.ascii	"EQ_param_i32_I"
	.skip 1
	.ascii	"EQ_var_i32_I"
	.skip 1
	.ascii	"EQ_param_i32_i16"
	.skip 1
	.ascii	"EQ_var_i32_i16"
	.skip 1
	.ascii	"EQ_param_i32_i32"
	.skip 1
	.ascii	"EQ_var_i32_i32"
	.skip 1
	.ascii	"EQ_param_i32_LC"
	.skip 1
	.ascii	"EQ_var_i32_LC"
	.skip 1
	.ascii	"EQ_param_i32_u64"
	.skip 1
	.ascii	"EQ_var_i32_u64"
	.skip 1
	.ascii	"EQ_param_i32_u16"
	.skip 1
	.ascii	"EQ_var_i32_u16"
	.skip 1
	.ascii	"EQ_param_LC_u32"
	.skip 1
	.ascii	"EQ_var_LC_u32"
	.skip 1
	.ascii	"EQ_param_LC_i8"
	.skip 1
	.ascii	"EQ_var_LC_i8"
	.skip 1
	.ascii	"EQ_param_LC_L"
	.skip 1
	.ascii	"EQ_var_LC_L"
	.skip 1
	.ascii	"EQ_param_LC_u8"
	.skip 1
	.ascii	"EQ_var_LC_u8"
	.skip 1
	.ascii	"EQ_param_LC_C"
	.skip 1
	.ascii	"EQ_var_LC_C"
	.skip 1
	.ascii	"EQ_param_LC_i64"
	.skip 1
	.ascii	"EQ_var_LC_i64"
	.skip 1
	.ascii	"EQ_param_LC_I"
	.skip 1
	.ascii	"EQ_var_LC_I"
	.skip 1
	.ascii	"EQ_param_LC_i16"
	.skip 1
	.ascii	"EQ_var_LC_i16"
	.skip 1
	.ascii	"EQ_param_LC_i32"
	.skip 1
	.ascii	"EQ_var_LC_i32"
	.skip 1
	.ascii	"EQ_param_LC_LC"
	.skip 1
	.ascii	"EQ_var_LC_LC"
	.skip 1
	.ascii	"EQ_param_LC_u64"
	.skip 1
	.ascii	"EQ_var_LC_u64"
	.skip 1
	.ascii	"EQ_param_LC_u16"
	.skip 1
	.ascii	"EQ_var_LC_u16"
	.skip 1
	.ascii	"EQ_param_u64_u32"
	.skip 1
	.ascii	"EQ_var_u64_u32"
	.skip 1
	.ascii	"EQ_param_u64_i8"
	.skip 1
	.ascii	"EQ_var_u64_i8"
	.skip 1
	.ascii	"EQ_param_u64_L"
	.skip 1
	.ascii	"EQ_var_u64_L"
	.skip 1
	.ascii	"EQ_param_u64_u8"
	.skip 1
	.ascii	"EQ_var_u64_u8"
	.skip 1
	.ascii	"EQ_param_u64_C"
	.skip 1
	.ascii	"EQ_var_u64_C"
	.skip 1
	.ascii	"EQ_param_u64_i64"
	.skip 1
	.ascii	"EQ_var_u64_i64"
	.skip 1
	.ascii	"EQ_param_u64_I"
	.skip 1
	.ascii	"EQ_var_u64_I"
	.skip 1
	.ascii	"EQ_param_u64_i16"
	.skip 1
	.ascii	"EQ_var_u64_i16"
	.skip 1
	.ascii	"EQ_param_u64_i32"
	.skip 1
	.ascii	"EQ_var_u64_i32"
	.skip 1
	.ascii	"EQ_param_u64_LC"
	.skip 1
	.ascii	"EQ_var_u64_LC"
	.skip 1
	.ascii	"EQ_param_u64_u64"
	.skip 1
	.ascii	"EQ_var_u64_u64"
	.skip 1
	.ascii	"EQ_param_u64_u16"
	.skip 1
	.ascii	"EQ_var_u64_u16"
	.skip 1
	.ascii	"EQ_param_u16_u32"
	.skip 1
	.ascii	"EQ_var_u16_u32"
	.skip 1
	.ascii	"EQ_param_u16_i8"
	.skip 1
	.ascii	"EQ_var_u16_i8"
	.skip 1
	.ascii	"EQ_param_u16_L"
	.skip 1
	.ascii	"EQ_var_u16_L"
	.skip 1
	.ascii	"EQ_param_u16_u8"
	.skip 1
	.ascii	"EQ_var_u16_u8"
	.skip 1
	.ascii	"EQ_param_u16_C"
	.skip 1
	.ascii	"EQ_var_u16_C"
	.skip 1
	.ascii	"EQ_param_u16_i64"
	.skip 1
	.ascii	"EQ_var_u16_i64"
	.skip 1
	.ascii	"EQ_param_u16_I"
	.skip 1
	.ascii	"EQ_var_u16_I"
	.skip 1
	.ascii	"EQ_param_u16_i16"
	.skip 1
	.ascii	"EQ_var_u16_i16"
	.skip 1
	.ascii	"EQ_param_u16_i32"
	.skip 1
	.ascii	"EQ_var_u16_i32"
	.skip 1
	.ascii	"EQ_param_u16_LC"
	.skip 1
	.ascii	"EQ_var_u16_LC"
	.skip 1
	.ascii	"EQ_param_u16_u64"
	.skip 1
	.ascii	"EQ_var_u16_u64"
	.skip 1
	.ascii	"EQ_param_u16_u16"
	.skip 1
	.ascii	"EQ_var_u16_u16"
	.skip 3
	.long	EQ_M3
	.long	L_1
	.long	EQ__EQ_param_f32_f32
	.long	L_1+6
	.long	EQ__EQ_var_f32_f32
	.long	L_1+23
	.long	EQ__EQ_param_u32_u32
	.long	L_1+38
	.long	EQ__EQ_var_u32_u32
	.long	L_1+55
	.long	EQ__EQ_param_u32_i8
	.long	L_1+70
	.long	EQ__EQ_var_u32_i8
	.long	L_1+86
	.long	EQ__EQ_param_u32_L
	.long	L_1+100
	.long	EQ__EQ_var_u32_L
	.long	L_1+115
	.long	EQ__EQ_param_u32_u8
	.long	L_1+128
	.long	EQ__EQ_var_u32_u8
	.long	L_1+144
	.long	EQ__EQ_param_u32_C
	.long	L_1+158
	.long	EQ__EQ_var_u32_C
	.long	L_1+173
	.long	EQ__EQ_param_u32_i64
	.long	L_1+186
	.long	EQ__EQ_var_u32_i64
	.long	L_1+203
	.long	EQ__EQ_param_u32_I
	.long	L_1+218
	.long	EQ__EQ_var_u32_I
	.long	L_1+233
	.long	EQ__EQ_param_u32_i16
	.long	L_1+246
	.long	EQ__EQ_var_u32_i16
	.long	L_1+263
	.long	EQ__EQ_param_u32_i32
	.long	L_1+278
	.long	EQ__EQ_var_u32_i32
	.long	L_1+295
	.long	EQ__EQ_param_u32_LC
	.long	L_1+310
	.long	EQ__EQ_var_u32_LC
	.long	L_1+326
	.long	EQ__EQ_param_u32_u64
	.long	L_1+340
	.long	EQ__EQ_var_u32_u64
	.long	L_1+357
	.long	EQ__EQ_param_u32_u16
	.long	L_1+372
	.long	EQ__EQ_var_u32_u16
	.long	L_1+389
	.long	EQ__EQ_param_i8_u32
	.long	L_1+404
	.long	EQ__EQ_var_i8_u32
	.long	L_1+420
	.long	EQ__EQ_param_i8_i8
	.long	L_1+434
	.long	EQ__EQ_var_i8_i8
	.long	L_1+449
	.long	EQ__EQ_param_i8_L
	.long	L_1+462
	.long	EQ__EQ_var_i8_L
	.long	L_1+476
	.long	EQ__EQ_param_i8_u8
	.long	L_1+488
	.long	EQ__EQ_var_i8_u8
	.long	L_1+503
	.long	EQ__EQ_param_i8_C
	.long	L_1+516
	.long	EQ__EQ_var_i8_C
	.long	L_1+530
	.long	EQ__EQ_param_i8_i64
	.long	L_1+542
	.long	EQ__EQ_var_i8_i64
	.long	L_1+558
	.long	EQ__EQ_param_i8_I
	.long	L_1+572
	.long	EQ__EQ_var_i8_I
	.long	L_1+586
	.long	EQ__EQ_param_i8_i16
	.long	L_1+598
	.long	EQ__EQ_var_i8_i16
	.long	L_1+614
	.long	EQ__EQ_param_i8_i32
	.long	L_1+628
	.long	EQ__EQ_var_i8_i32
	.long	L_1+644
	.long	EQ__EQ_param_i8_LC
	.long	L_1+658
	.long	EQ__EQ_var_i8_LC
	.long	L_1+673
	.long	EQ__EQ_param_i8_u64
	.long	L_1+686
	.long	EQ__EQ_var_i8_u64
	.long	L_1+702
	.long	EQ__EQ_param_i8_u16
	.long	L_1+716
	.long	EQ__EQ_var_i8_u16
	.long	L_1+732
	.long	EQ__EQ_param_L_u32
	.long	L_1+746
	.long	EQ__EQ_var_L_u32
	.long	L_1+761
	.long	EQ__EQ_param_L_i8
	.long	L_1+774
	.long	EQ__EQ_var_L_i8
	.long	L_1+788
	.long	EQ__EQ_param_L_L
	.long	L_1+800
	.long	EQ__EQ_var_L_L
	.long	L_1+813
	.long	EQ__EQ_param_L_u8
	.long	L_1+824
	.long	EQ__EQ_var_L_u8
	.long	L_1+838
	.long	EQ__EQ_param_L_C
	.long	L_1+850
	.long	EQ__EQ_var_L_C
	.long	L_1+863
	.long	EQ__EQ_param_L_i64
	.long	L_1+874
	.long	EQ__EQ_var_L_i64
	.long	L_1+889
	.long	EQ__EQ_param_L_I
	.long	L_1+902
	.long	EQ__EQ_var_L_I
	.long	L_1+915
	.long	EQ__EQ_param_L_i16
	.long	L_1+926
	.long	EQ__EQ_var_L_i16
	.long	L_1+941
	.long	EQ__EQ_param_L_i32
	.long	L_1+954
	.long	EQ__EQ_var_L_i32
	.long	L_1+969
	.long	EQ__EQ_param_L_LC
	.long	L_1+982
	.long	EQ__EQ_var_L_LC
	.long	L_1+996
	.long	EQ__EQ_param_L_u64
	.long	L_1+1008
	.long	EQ__EQ_var_L_u64
	.long	L_1+1023
	.long	EQ__EQ_param_L_u16
	.long	L_1+1036
	.long	EQ__EQ_var_L_u16
	.long	L_1+1051
	.long	EQ__EQ_param_u8_u32
	.long	L_1+1064
	.long	EQ__EQ_var_u8_u32
	.long	L_1+1080
	.long	EQ__EQ_param_u8_i8
	.long	L_1+1094
	.long	EQ__EQ_var_u8_i8
	.long	L_1+1109
	.long	EQ__EQ_param_u8_L
	.long	L_1+1122
	.long	EQ__EQ_var_u8_L
	.long	L_1+1136
	.long	EQ__EQ_param_u8_u8
	.long	L_1+1148
	.long	EQ__EQ_var_u8_u8
	.long	L_1+1163
	.long	EQ__EQ_param_u8_C
	.long	L_1+1176
	.long	EQ__EQ_var_u8_C
	.long	L_1+1190
	.long	EQ__EQ_param_u8_i64
	.long	L_1+1202
	.long	EQ__EQ_var_u8_i64
	.long	L_1+1218
	.long	EQ__EQ_param_u8_I
	.long	L_1+1232
	.long	EQ__EQ_var_u8_I
	.long	L_1+1246
	.long	EQ__EQ_param_u8_i16
	.long	L_1+1258
	.long	EQ__EQ_var_u8_i16
	.long	L_1+1274
	.long	EQ__EQ_param_u8_i32
	.long	L_1+1288
	.long	EQ__EQ_var_u8_i32
	.long	L_1+1304
	.long	EQ__EQ_param_u8_LC
	.long	L_1+1318
	.long	EQ__EQ_var_u8_LC
	.long	L_1+1333
	.long	EQ__EQ_param_u8_u64
	.long	L_1+1346
	.long	EQ__EQ_var_u8_u64
	.long	L_1+1362
	.long	EQ__EQ_param_u8_u16
	.long	L_1+1376
	.long	EQ__EQ_var_u8_u16
	.long	L_1+1392
	.long	EQ__EQ_param_f64_f64
	.long	L_1+1406
	.long	EQ__EQ_var_f64_f64
	.long	L_1+1423
	.long	EQ__EQ_param_C_u32
	.long	L_1+1438
	.long	EQ__EQ_var_C_u32
	.long	L_1+1453
	.long	EQ__EQ_param_C_i8
	.long	L_1+1466
	.long	EQ__EQ_var_C_i8
	.long	L_1+1480
	.long	EQ__EQ_param_C_L
	.long	L_1+1492
	.long	EQ__EQ_var_C_L
	.long	L_1+1505
	.long	EQ__EQ_param_C_u8
	.long	L_1+1516
	.long	EQ__EQ_var_C_u8
	.long	L_1+1530
	.long	EQ__EQ_param_C_C
	.long	L_1+1542
	.long	EQ__EQ_var_C_C
	.long	L_1+1555
	.long	EQ__EQ_param_C_i64
	.long	L_1+1566
	.long	EQ__EQ_var_C_i64
	.long	L_1+1581
	.long	EQ__EQ_param_C_I
	.long	L_1+1594
	.long	EQ__EQ_var_C_I
	.long	L_1+1607
	.long	EQ__EQ_param_C_i16
	.long	L_1+1618
	.long	EQ__EQ_var_C_i16
	.long	L_1+1633
	.long	EQ__EQ_param_C_i32
	.long	L_1+1646
	.long	EQ__EQ_var_C_i32
	.long	L_1+1661
	.long	EQ__EQ_param_C_LC
	.long	L_1+1674
	.long	EQ__EQ_var_C_LC
	.long	L_1+1688
	.long	EQ__EQ_param_C_u64
	.long	L_1+1700
	.long	EQ__EQ_var_C_u64
	.long	L_1+1715
	.long	EQ__EQ_param_C_u16
	.long	L_1+1728
	.long	EQ__EQ_var_C_u16
	.long	L_1+1743
	.long	EQ__EQ_param_i64_u32
	.long	L_1+1756
	.long	EQ__EQ_var_i64_u32
	.long	L_1+1773
	.long	EQ__EQ_param_i64_i8
	.long	L_1+1788
	.long	EQ__EQ_var_i64_i8
	.long	L_1+1804
	.long	EQ__EQ_param_i64_L
	.long	L_1+1818
	.long	EQ__EQ_var_i64_L
	.long	L_1+1833
	.long	EQ__EQ_param_i64_u8
	.long	L_1+1846
	.long	EQ__EQ_var_i64_u8
	.long	L_1+1862
	.long	EQ__EQ_param_i64_C
	.long	L_1+1876
	.long	EQ__EQ_var_i64_C
	.long	L_1+1891
	.long	EQ__EQ_param_i64_i64
	.long	L_1+1904
	.long	EQ__EQ_var_i64_i64
	.long	L_1+1921
	.long	EQ__EQ_param_i64_I
	.long	L_1+1936
	.long	EQ__EQ_var_i64_I
	.long	L_1+1951
	.long	EQ__EQ_param_i64_i16
	.long	L_1+1964
	.long	EQ__EQ_var_i64_i16
	.long	L_1+1981
	.long	EQ__EQ_param_i64_i32
	.long	L_1+1996
	.long	EQ__EQ_var_i64_i32
	.long	L_1+2013
	.long	EQ__EQ_param_i64_LC
	.long	L_1+2028
	.long	EQ__EQ_var_i64_LC
	.long	L_1+2044
	.long	EQ__EQ_param_i64_u64
	.long	L_1+2058
	.long	EQ__EQ_var_i64_u64
	.long	L_1+2075
	.long	EQ__EQ_param_i64_u16
	.long	L_1+2090
	.long	EQ__EQ_var_i64_u16
	.long	L_1+2107
	.long	EQ__EQ_param_I_u32
	.long	L_1+2122
	.long	EQ__EQ_var_I_u32
	.long	L_1+2137
	.long	EQ__EQ_param_I_i8
	.long	L_1+2150
	.long	EQ__EQ_var_I_i8
	.long	L_1+2164
	.long	EQ__EQ_param_I_L
	.long	L_1+2176
	.long	EQ__EQ_var_I_L
	.long	L_1+2189
	.long	EQ__EQ_param_I_u8
	.long	L_1+2200
	.long	EQ__EQ_var_I_u8
	.long	L_1+2214
	.long	EQ__EQ_param_I_C
	.long	L_1+2226
	.long	EQ__EQ_var_I_C
	.long	L_1+2239
	.long	EQ__EQ_param_I_i64
	.long	L_1+2250
	.long	EQ__EQ_var_I_i64
	.long	L_1+2265
	.long	EQ__EQ_param_I_I
	.long	L_1+2278
	.long	EQ__EQ_var_I_I
	.long	L_1+2291
	.long	EQ__EQ_param_I_i16
	.long	L_1+2302
	.long	EQ__EQ_var_I_i16
	.long	L_1+2317
	.long	EQ__EQ_param_I_i32
	.long	L_1+2330
	.long	EQ__EQ_var_I_i32
	.long	L_1+2345
	.long	EQ__EQ_param_I_LC
	.long	L_1+2358
	.long	EQ__EQ_var_I_LC
	.long	L_1+2372
	.long	EQ__EQ_param_I_u64
	.long	L_1+2384
	.long	EQ__EQ_var_I_u64
	.long	L_1+2399
	.long	EQ__EQ_param_I_u16
	.long	L_1+2412
	.long	EQ__EQ_var_I_u16
	.long	L_1+2427
	.long	EQ__EQ_param_i16_u32
	.long	L_1+2440
	.long	EQ__EQ_var_i16_u32
	.long	L_1+2457
	.long	EQ__EQ_param_i16_i8
	.long	L_1+2472
	.long	EQ__EQ_var_i16_i8
	.long	L_1+2488
	.long	EQ__EQ_param_i16_L
	.long	L_1+2502
	.long	EQ__EQ_var_i16_L
	.long	L_1+2517
	.long	EQ__EQ_param_i16_u8
	.long	L_1+2530
	.long	EQ__EQ_var_i16_u8
	.long	L_1+2546
	.long	EQ__EQ_param_i16_C
	.long	L_1+2560
	.long	EQ__EQ_var_i16_C
	.long	L_1+2575
	.long	EQ__EQ_param_i16_i64
	.long	L_1+2588
	.long	EQ__EQ_var_i16_i64
	.long	L_1+2605
	.long	EQ__EQ_param_i16_I
	.long	L_1+2620
	.long	EQ__EQ_var_i16_I
	.long	L_1+2635
	.long	EQ__EQ_param_i16_i16
	.long	L_1+2648
	.long	EQ__EQ_var_i16_i16
	.long	L_1+2665
	.long	EQ__EQ_param_i16_i32
	.long	L_1+2680
	.long	EQ__EQ_var_i16_i32
	.long	L_1+2697
	.long	EQ__EQ_param_i16_LC
	.long	L_1+2712
	.long	EQ__EQ_var_i16_LC
	.long	L_1+2728
	.long	EQ__EQ_param_i16_u64
	.long	L_1+2742
	.long	EQ__EQ_var_i16_u64
	.long	L_1+2759
	.long	EQ__EQ_param_i16_u16
	.long	L_1+2774
	.long	EQ__EQ_var_i16_u16
	.long	L_1+2791
	.long	EQ__EQ_param_i32_u32
	.long	L_1+2806
	.long	EQ__EQ_var_i32_u32
	.long	L_1+2823
	.long	EQ__EQ_param_i32_i8
	.long	L_1+2838
	.long	EQ__EQ_var_i32_i8
	.long	L_1+2854
	.long	EQ__EQ_param_i32_L
	.long	L_1+2868
	.long	EQ__EQ_var_i32_L
	.long	L_1+2883
	.long	EQ__EQ_param_i32_u8
	.long	L_1+2896
	.long	EQ__EQ_var_i32_u8
	.long	L_1+2912
	.long	EQ__EQ_param_i32_C
	.long	L_1+2926
	.long	EQ__EQ_var_i32_C
	.long	L_1+2941
	.long	EQ__EQ_param_i32_i64
	.long	L_1+2954
	.long	EQ__EQ_var_i32_i64
	.long	L_1+2971
	.long	EQ__EQ_param_i32_I
	.long	L_1+2986
	.long	EQ__EQ_var_i32_I
	.long	L_1+3001
	.long	EQ__EQ_param_i32_i16
	.long	L_1+3014
	.long	EQ__EQ_var_i32_i16
	.long	L_1+3031
	.long	EQ__EQ_param_i32_i32
	.long	L_1+3046
	.long	EQ__EQ_var_i32_i32
	.long	L_1+3063
	.long	EQ__EQ_param_i32_LC
	.long	L_1+3078
	.long	EQ__EQ_var_i32_LC
	.long	L_1+3094
	.long	EQ__EQ_param_i32_u64
	.long	L_1+3108
	.long	EQ__EQ_var_i32_u64
	.long	L_1+3125
	.long	EQ__EQ_param_i32_u16
	.long	L_1+3140
	.long	EQ__EQ_var_i32_u16
	.long	L_1+3157
	.long	EQ__EQ_param_LC_u32
	.long	L_1+3172
	.long	EQ__EQ_var_LC_u32
	.long	L_1+3188
	.long	EQ__EQ_param_LC_i8
	.long	L_1+3202
	.long	EQ__EQ_var_LC_i8
	.long	L_1+3217
	.long	EQ__EQ_param_LC_L
	.long	L_1+3230
	.long	EQ__EQ_var_LC_L
	.long	L_1+3244
	.long	EQ__EQ_param_LC_u8
	.long	L_1+3256
	.long	EQ__EQ_var_LC_u8
	.long	L_1+3271
	.long	EQ__EQ_param_LC_C
	.long	L_1+3284
	.long	EQ__EQ_var_LC_C
	.long	L_1+3298
	.long	EQ__EQ_param_LC_i64
	.long	L_1+3310
	.long	EQ__EQ_var_LC_i64
	.long	L_1+3326
	.long	EQ__EQ_param_LC_I
	.long	L_1+3340
	.long	EQ__EQ_var_LC_I
	.long	L_1+3354
	.long	EQ__EQ_param_LC_i16
	.long	L_1+3366
	.long	EQ__EQ_var_LC_i16
	.long	L_1+3382
	.long	EQ__EQ_param_LC_i32
	.long	L_1+3396
	.long	EQ__EQ_var_LC_i32
	.long	L_1+3412
	.long	EQ__EQ_param_LC_LC
	.long	L_1+3426
	.long	EQ__EQ_var_LC_LC
	.long	L_1+3441
	.long	EQ__EQ_param_LC_u64
	.long	L_1+3454
	.long	EQ__EQ_var_LC_u64
	.long	L_1+3470
	.long	EQ__EQ_param_LC_u16
	.long	L_1+3484
	.long	EQ__EQ_var_LC_u16
	.long	L_1+3500
	.long	EQ__EQ_param_u64_u32
	.long	L_1+3514
	.long	EQ__EQ_var_u64_u32
	.long	L_1+3531
	.long	EQ__EQ_param_u64_i8
	.long	L_1+3546
	.long	EQ__EQ_var_u64_i8
	.long	L_1+3562
	.long	EQ__EQ_param_u64_L
	.long	L_1+3576
	.long	EQ__EQ_var_u64_L
	.long	L_1+3591
	.long	EQ__EQ_param_u64_u8
	.long	L_1+3604
	.long	EQ__EQ_var_u64_u8
	.long	L_1+3620
	.long	EQ__EQ_param_u64_C
	.long	L_1+3634
	.long	EQ__EQ_var_u64_C
	.long	L_1+3649
	.long	EQ__EQ_param_u64_i64
	.long	L_1+3662
	.long	EQ__EQ_var_u64_i64
	.long	L_1+3679
	.long	EQ__EQ_param_u64_I
	.long	L_1+3694
	.long	EQ__EQ_var_u64_I
	.long	L_1+3709
	.long	EQ__EQ_param_u64_i16
	.long	L_1+3722
	.long	EQ__EQ_var_u64_i16
	.long	L_1+3739
	.long	EQ__EQ_param_u64_i32
	.long	L_1+3754
	.long	EQ__EQ_var_u64_i32
	.long	L_1+3771
	.long	EQ__EQ_param_u64_LC
	.long	L_1+3786
	.long	EQ__EQ_var_u64_LC
	.long	L_1+3802
	.long	EQ__EQ_param_u64_u64
	.long	L_1+3816
	.long	EQ__EQ_var_u64_u64
	.long	L_1+3833
	.long	EQ__EQ_param_u64_u16
	.long	L_1+3848
	.long	EQ__EQ_var_u64_u16
	.long	L_1+3865
	.long	EQ__EQ_param_u16_u32
	.long	L_1+3880
	.long	EQ__EQ_var_u16_u32
	.long	L_1+3897
	.long	EQ__EQ_param_u16_i8
	.long	L_1+3912
	.long	EQ__EQ_var_u16_i8
	.long	L_1+3928
	.long	EQ__EQ_param_u16_L
	.long	L_1+3942
	.long	EQ__EQ_var_u16_L
	.long	L_1+3957
	.long	EQ__EQ_param_u16_u8
	.long	L_1+3970
	.long	EQ__EQ_var_u16_u8
	.long	L_1+3986
	.long	EQ__EQ_param_u16_C
	.long	L_1+4000
	.long	EQ__EQ_var_u16_C
	.long	L_1+4015
	.long	EQ__EQ_param_u16_i64
	.long	L_1+4028
	.long	EQ__EQ_var_u16_i64
	.long	L_1+4045
	.long	EQ__EQ_param_u16_I
	.long	L_1+4060
	.long	EQ__EQ_var_u16_I
	.long	L_1+4075
	.long	EQ__EQ_param_u16_i16
	.long	L_1+4088
	.long	EQ__EQ_var_u16_i16
	.long	L_1+4105
	.long	EQ__EQ_param_u16_i32
	.long	L_1+4120
	.long	EQ__EQ_var_u16_i32
	.long	L_1+4137
	.long	EQ__EQ_param_u16_LC
	.long	L_1+4152
	.long	EQ__EQ_var_u16_LC
	.long	L_1+4168
	.long	EQ__EQ_param_u16_u64
	.long	L_1+4182
	.long	EQ__EQ_var_u16_u64
	.long	L_1+4199
	.long	EQ__EQ_param_u16_u16
	.long	L_1+4214
	.long	EQ__EQ_var_u16_u16
	.long	L_1+4231
	.skip 4
	.ascii	"../SOLsun/EQ.m3"
	.skip 5
	.align 8
	.type	MM_EQ, #object
	.size	MM_EQ, 208
MM_EQ:
	.long	L_1+6596
	.skip 16
	.long	L_1+4248
	.skip 12
	.long	MM_EQ+148
	.skip 4
	.long	EQ_M3
	.long	3
	.half	529
	.skip 2
	.long	0
	.long	530
	.long	0
	.long	531
	.long	532
	.half	533
	.skip 2
	.long	534
	.skip 4
	.long	0
	.long	535
	.long	536
	.skip 4
	.long	1082182733
	.long	-755914244
	.byte	27
	.skip 7
	.long	0
	.long	540
	.byte	29
	.skip 3
	.long	542
	.long	1141367505
	.skip 12
	.long	EQ_I3
	.long	MM_EQ+160
	.skip 4
	.long	Long_I3
	.long	MM_EQ+172
	.skip 4
	.long	Word_I3
	.long	MM_EQ+184
	.skip 4
	.long	Cstdint_I3
	.long	MM_EQ+196
	.skip 4
	.long	RTHooks_I3
	.skip 4
	.ident	"GCC: (GNU) 4.5.1"
