	.file	"divmod_pow2.mc"
	.section	".text"
	.align 4
	.global divmod_pow2__div_pow2_i32_0
	.hidden	divmod_pow2__div_pow2_i32_0
	.type	divmod_pow2__div_pow2_i32_0, #function
	.proc	04
divmod_pow2__div_pow2_i32_0:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i32_0, .-divmod_pow2__div_pow2_i32_0
	.align 4
	.global divmod_pow2__mod_pow2_i32_0
	.hidden	divmod_pow2__mod_pow2_i32_0
	.type	divmod_pow2__mod_pow2_i32_0, #function
	.proc	04
divmod_pow2__mod_pow2_i32_0:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	mov	0, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i32_0, .-divmod_pow2__mod_pow2_i32_0
	.align 4
	.global divmod_pow2__div_pow2_i32_1
	.hidden	divmod_pow2__div_pow2_i32_1
	.type	divmod_pow2__div_pow2_i32_1, #function
	.proc	04
divmod_pow2__div_pow2_i32_1:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i32_1, .-divmod_pow2__div_pow2_i32_1
	.align 4
	.global divmod_pow2__mod_pow2_i32_1
	.hidden	divmod_pow2__mod_pow2_i32_1
	.type	divmod_pow2__mod_pow2_i32_1, #function
	.proc	04
divmod_pow2__mod_pow2_i32_1:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i32_1, .-divmod_pow2__mod_pow2_i32_1
	.align 4
	.global divmod_pow2__div_pow2_i32_2
	.hidden	divmod_pow2__div_pow2_i32_2
	.type	divmod_pow2__div_pow2_i32_2, #function
	.proc	04
divmod_pow2__div_pow2_i32_2:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 2, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i32_2, .-divmod_pow2__div_pow2_i32_2
	.align 4
	.global divmod_pow2__mod_pow2_i32_2
	.hidden	divmod_pow2__mod_pow2_i32_2
	.type	divmod_pow2__mod_pow2_i32_2, #function
	.proc	04
divmod_pow2__mod_pow2_i32_2:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 3, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i32_2, .-divmod_pow2__mod_pow2_i32_2
	.align 4
	.global divmod_pow2__div_pow2_i32_3
	.hidden	divmod_pow2__div_pow2_i32_3
	.type	divmod_pow2__div_pow2_i32_3, #function
	.proc	04
divmod_pow2__div_pow2_i32_3:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 3, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i32_3, .-divmod_pow2__div_pow2_i32_3
	.align 4
	.global divmod_pow2__mod_pow2_i32_3
	.hidden	divmod_pow2__mod_pow2_i32_3
	.type	divmod_pow2__mod_pow2_i32_3, #function
	.proc	04
divmod_pow2__mod_pow2_i32_3:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 7, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i32_3, .-divmod_pow2__mod_pow2_i32_3
	.align 4
	.global divmod_pow2__div_pow2_i32_4
	.hidden	divmod_pow2__div_pow2_i32_4
	.type	divmod_pow2__div_pow2_i32_4, #function
	.proc	04
divmod_pow2__div_pow2_i32_4:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 4, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i32_4, .-divmod_pow2__div_pow2_i32_4
	.align 4
	.global divmod_pow2__mod_pow2_i32_4
	.hidden	divmod_pow2__mod_pow2_i32_4
	.type	divmod_pow2__mod_pow2_i32_4, #function
	.proc	04
divmod_pow2__mod_pow2_i32_4:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 15, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i32_4, .-divmod_pow2__mod_pow2_i32_4
	.align 4
	.global divmod_pow2__div_pow2_i32_5
	.hidden	divmod_pow2__div_pow2_i32_5
	.type	divmod_pow2__div_pow2_i32_5, #function
	.proc	04
divmod_pow2__div_pow2_i32_5:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i32_5, .-divmod_pow2__div_pow2_i32_5
	.align 4
	.global divmod_pow2__mod_pow2_i32_5
	.hidden	divmod_pow2__mod_pow2_i32_5
	.type	divmod_pow2__mod_pow2_i32_5, #function
	.proc	04
divmod_pow2__mod_pow2_i32_5:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i32_5, .-divmod_pow2__mod_pow2_i32_5
	.align 4
	.global divmod_pow2__div_pow2_i32_6
	.hidden	divmod_pow2__div_pow2_i32_6
	.type	divmod_pow2__div_pow2_i32_6, #function
	.proc	04
divmod_pow2__div_pow2_i32_6:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 6, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i32_6, .-divmod_pow2__div_pow2_i32_6
	.align 4
	.global divmod_pow2__mod_pow2_i32_6
	.hidden	divmod_pow2__mod_pow2_i32_6
	.type	divmod_pow2__mod_pow2_i32_6, #function
	.proc	04
divmod_pow2__mod_pow2_i32_6:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 63, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i32_6, .-divmod_pow2__mod_pow2_i32_6
	.align 4
	.global divmod_pow2__div_pow2_i32_7
	.hidden	divmod_pow2__div_pow2_i32_7
	.type	divmod_pow2__div_pow2_i32_7, #function
	.proc	04
divmod_pow2__div_pow2_i32_7:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 7, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i32_7, .-divmod_pow2__div_pow2_i32_7
	.align 4
	.global divmod_pow2__mod_pow2_i32_7
	.hidden	divmod_pow2__mod_pow2_i32_7
	.type	divmod_pow2__mod_pow2_i32_7, #function
	.proc	04
divmod_pow2__mod_pow2_i32_7:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 127, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i32_7, .-divmod_pow2__mod_pow2_i32_7
	.align 4
	.global divmod_pow2__div_pow2_i32_8
	.hidden	divmod_pow2__div_pow2_i32_8
	.type	divmod_pow2__div_pow2_i32_8, #function
	.proc	04
divmod_pow2__div_pow2_i32_8:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 8, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i32_8, .-divmod_pow2__div_pow2_i32_8
	.align 4
	.global divmod_pow2__mod_pow2_i32_8
	.hidden	divmod_pow2__mod_pow2_i32_8
	.type	divmod_pow2__mod_pow2_i32_8, #function
	.proc	04
divmod_pow2__mod_pow2_i32_8:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 255, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i32_8, .-divmod_pow2__mod_pow2_i32_8
	.align 4
	.global divmod_pow2__div_pow2_i32_9
	.hidden	divmod_pow2__div_pow2_i32_9
	.type	divmod_pow2__div_pow2_i32_9, #function
	.proc	04
divmod_pow2__div_pow2_i32_9:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 9, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i32_9, .-divmod_pow2__div_pow2_i32_9
	.align 4
	.global divmod_pow2__mod_pow2_i32_9
	.hidden	divmod_pow2__mod_pow2_i32_9
	.type	divmod_pow2__mod_pow2_i32_9, #function
	.proc	04
divmod_pow2__mod_pow2_i32_9:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 511, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i32_9, .-divmod_pow2__mod_pow2_i32_9
	.align 4
	.global divmod_pow2__div_pow2_i32_10
	.hidden	divmod_pow2__div_pow2_i32_10
	.type	divmod_pow2__div_pow2_i32_10, #function
	.proc	04
divmod_pow2__div_pow2_i32_10:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 10, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i32_10, .-divmod_pow2__div_pow2_i32_10
	.align 4
	.global divmod_pow2__mod_pow2_i32_10
	.hidden	divmod_pow2__mod_pow2_i32_10
	.type	divmod_pow2__mod_pow2_i32_10, #function
	.proc	04
divmod_pow2__mod_pow2_i32_10:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 1023, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i32_10, .-divmod_pow2__mod_pow2_i32_10
	.align 4
	.global divmod_pow2__div_pow2_i32_11
	.hidden	divmod_pow2__div_pow2_i32_11
	.type	divmod_pow2__div_pow2_i32_11, #function
	.proc	04
divmod_pow2__div_pow2_i32_11:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 11, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i32_11, .-divmod_pow2__div_pow2_i32_11
	.align 4
	.global divmod_pow2__mod_pow2_i32_11
	.hidden	divmod_pow2__mod_pow2_i32_11
	.type	divmod_pow2__mod_pow2_i32_11, #function
	.proc	04
divmod_pow2__mod_pow2_i32_11:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 2047, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i32_11, .-divmod_pow2__mod_pow2_i32_11
	.align 4
	.global divmod_pow2__div_pow2_i32_12
	.hidden	divmod_pow2__div_pow2_i32_12
	.type	divmod_pow2__div_pow2_i32_12, #function
	.proc	04
divmod_pow2__div_pow2_i32_12:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 12, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i32_12, .-divmod_pow2__div_pow2_i32_12
	.align 4
	.global divmod_pow2__mod_pow2_i32_12
	.hidden	divmod_pow2__mod_pow2_i32_12
	.type	divmod_pow2__mod_pow2_i32_12, #function
	.proc	04
divmod_pow2__mod_pow2_i32_12:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 4095, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i32_12, .-divmod_pow2__mod_pow2_i32_12
	.align 4
	.global divmod_pow2__div_pow2_i32_13
	.hidden	divmod_pow2__div_pow2_i32_13
	.type	divmod_pow2__div_pow2_i32_13, #function
	.proc	04
divmod_pow2__div_pow2_i32_13:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 13, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i32_13, .-divmod_pow2__div_pow2_i32_13
	.align 4
	.global divmod_pow2__mod_pow2_i32_13
	.hidden	divmod_pow2__mod_pow2_i32_13
	.type	divmod_pow2__mod_pow2_i32_13, #function
	.proc	04
divmod_pow2__mod_pow2_i32_13:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i32_13, .-divmod_pow2__mod_pow2_i32_13
	.align 4
	.global divmod_pow2__div_pow2_i32_14
	.hidden	divmod_pow2__div_pow2_i32_14
	.type	divmod_pow2__div_pow2_i32_14, #function
	.proc	04
divmod_pow2__div_pow2_i32_14:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 14, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i32_14, .-divmod_pow2__div_pow2_i32_14
	.align 4
	.global divmod_pow2__mod_pow2_i32_14
	.hidden	divmod_pow2__mod_pow2_i32_14
	.type	divmod_pow2__mod_pow2_i32_14, #function
	.proc	04
divmod_pow2__mod_pow2_i32_14:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i32_14, .-divmod_pow2__mod_pow2_i32_14
	.align 4
	.global divmod_pow2__div_pow2_i32_15
	.hidden	divmod_pow2__div_pow2_i32_15
	.type	divmod_pow2__div_pow2_i32_15, #function
	.proc	04
divmod_pow2__div_pow2_i32_15:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 15, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i32_15, .-divmod_pow2__div_pow2_i32_15
	.align 4
	.global divmod_pow2__mod_pow2_i32_15
	.hidden	divmod_pow2__mod_pow2_i32_15
	.type	divmod_pow2__mod_pow2_i32_15, #function
	.proc	04
divmod_pow2__mod_pow2_i32_15:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i32_15, .-divmod_pow2__mod_pow2_i32_15
	.align 4
	.global divmod_pow2__div_pow2_i32_16
	.hidden	divmod_pow2__div_pow2_i32_16
	.type	divmod_pow2__div_pow2_i32_16, #function
	.proc	04
divmod_pow2__div_pow2_i32_16:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i32_16, .-divmod_pow2__div_pow2_i32_16
	.align 4
	.global divmod_pow2__mod_pow2_i32_16
	.hidden	divmod_pow2__mod_pow2_i32_16
	.type	divmod_pow2__mod_pow2_i32_16, #function
	.proc	04
divmod_pow2__mod_pow2_i32_16:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i32_16, .-divmod_pow2__mod_pow2_i32_16
	.align 4
	.global divmod_pow2__div_pow2_i32_17
	.hidden	divmod_pow2__div_pow2_i32_17
	.type	divmod_pow2__div_pow2_i32_17, #function
	.proc	04
divmod_pow2__div_pow2_i32_17:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 17, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i32_17, .-divmod_pow2__div_pow2_i32_17
	.align 4
	.global divmod_pow2__mod_pow2_i32_17
	.hidden	divmod_pow2__mod_pow2_i32_17
	.type	divmod_pow2__mod_pow2_i32_17, #function
	.proc	04
divmod_pow2__mod_pow2_i32_17:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(130048), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i32_17, .-divmod_pow2__mod_pow2_i32_17
	.align 4
	.global divmod_pow2__div_pow2_i32_18
	.hidden	divmod_pow2__div_pow2_i32_18
	.type	divmod_pow2__div_pow2_i32_18, #function
	.proc	04
divmod_pow2__div_pow2_i32_18:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 18, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i32_18, .-divmod_pow2__div_pow2_i32_18
	.align 4
	.global divmod_pow2__mod_pow2_i32_18
	.hidden	divmod_pow2__mod_pow2_i32_18
	.type	divmod_pow2__mod_pow2_i32_18, #function
	.proc	04
divmod_pow2__mod_pow2_i32_18:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(261120), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i32_18, .-divmod_pow2__mod_pow2_i32_18
	.align 4
	.global divmod_pow2__div_pow2_i32_19
	.hidden	divmod_pow2__div_pow2_i32_19
	.type	divmod_pow2__div_pow2_i32_19, #function
	.proc	04
divmod_pow2__div_pow2_i32_19:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 19, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i32_19, .-divmod_pow2__div_pow2_i32_19
	.align 4
	.global divmod_pow2__mod_pow2_i32_19
	.hidden	divmod_pow2__mod_pow2_i32_19
	.type	divmod_pow2__mod_pow2_i32_19, #function
	.proc	04
divmod_pow2__mod_pow2_i32_19:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(523264), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i32_19, .-divmod_pow2__mod_pow2_i32_19
	.align 4
	.global divmod_pow2__div_pow2_i32_20
	.hidden	divmod_pow2__div_pow2_i32_20
	.type	divmod_pow2__div_pow2_i32_20, #function
	.proc	04
divmod_pow2__div_pow2_i32_20:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 20, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i32_20, .-divmod_pow2__div_pow2_i32_20
	.align 4
	.global divmod_pow2__mod_pow2_i32_20
	.hidden	divmod_pow2__mod_pow2_i32_20
	.type	divmod_pow2__mod_pow2_i32_20, #function
	.proc	04
divmod_pow2__mod_pow2_i32_20:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(1047552), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i32_20, .-divmod_pow2__mod_pow2_i32_20
	.align 4
	.global divmod_pow2__div_pow2_i32_21
	.hidden	divmod_pow2__div_pow2_i32_21
	.type	divmod_pow2__div_pow2_i32_21, #function
	.proc	04
divmod_pow2__div_pow2_i32_21:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 21, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i32_21, .-divmod_pow2__div_pow2_i32_21
	.align 4
	.global divmod_pow2__mod_pow2_i32_21
	.hidden	divmod_pow2__mod_pow2_i32_21
	.type	divmod_pow2__mod_pow2_i32_21, #function
	.proc	04
divmod_pow2__mod_pow2_i32_21:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(2096128), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i32_21, .-divmod_pow2__mod_pow2_i32_21
	.align 4
	.global divmod_pow2__div_pow2_i32_22
	.hidden	divmod_pow2__div_pow2_i32_22
	.type	divmod_pow2__div_pow2_i32_22, #function
	.proc	04
divmod_pow2__div_pow2_i32_22:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 22, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i32_22, .-divmod_pow2__div_pow2_i32_22
	.align 4
	.global divmod_pow2__mod_pow2_i32_22
	.hidden	divmod_pow2__mod_pow2_i32_22
	.type	divmod_pow2__mod_pow2_i32_22, #function
	.proc	04
divmod_pow2__mod_pow2_i32_22:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(4193280), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i32_22, .-divmod_pow2__mod_pow2_i32_22
	.align 4
	.global divmod_pow2__div_pow2_i32_23
	.hidden	divmod_pow2__div_pow2_i32_23
	.type	divmod_pow2__div_pow2_i32_23, #function
	.proc	04
divmod_pow2__div_pow2_i32_23:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i32_23, .-divmod_pow2__div_pow2_i32_23
	.align 4
	.global divmod_pow2__mod_pow2_i32_23
	.hidden	divmod_pow2__mod_pow2_i32_23
	.type	divmod_pow2__mod_pow2_i32_23, #function
	.proc	04
divmod_pow2__mod_pow2_i32_23:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(8387584), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i32_23, .-divmod_pow2__mod_pow2_i32_23
	.align 4
	.global divmod_pow2__div_pow2_i32_24
	.hidden	divmod_pow2__div_pow2_i32_24
	.type	divmod_pow2__div_pow2_i32_24, #function
	.proc	04
divmod_pow2__div_pow2_i32_24:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i32_24, .-divmod_pow2__div_pow2_i32_24
	.align 4
	.global divmod_pow2__mod_pow2_i32_24
	.hidden	divmod_pow2__mod_pow2_i32_24
	.type	divmod_pow2__mod_pow2_i32_24, #function
	.proc	04
divmod_pow2__mod_pow2_i32_24:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(16776192), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i32_24, .-divmod_pow2__mod_pow2_i32_24
	.align 4
	.global divmod_pow2__div_pow2_i32_25
	.hidden	divmod_pow2__div_pow2_i32_25
	.type	divmod_pow2__div_pow2_i32_25, #function
	.proc	04
divmod_pow2__div_pow2_i32_25:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i32_25, .-divmod_pow2__div_pow2_i32_25
	.align 4
	.global divmod_pow2__mod_pow2_i32_25
	.hidden	divmod_pow2__mod_pow2_i32_25
	.type	divmod_pow2__mod_pow2_i32_25, #function
	.proc	04
divmod_pow2__mod_pow2_i32_25:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(33553408), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i32_25, .-divmod_pow2__mod_pow2_i32_25
	.align 4
	.global divmod_pow2__div_pow2_i32_26
	.hidden	divmod_pow2__div_pow2_i32_26
	.type	divmod_pow2__div_pow2_i32_26, #function
	.proc	04
divmod_pow2__div_pow2_i32_26:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i32_26, .-divmod_pow2__div_pow2_i32_26
	.align 4
	.global divmod_pow2__mod_pow2_i32_26
	.hidden	divmod_pow2__mod_pow2_i32_26
	.type	divmod_pow2__mod_pow2_i32_26, #function
	.proc	04
divmod_pow2__mod_pow2_i32_26:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(67107840), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i32_26, .-divmod_pow2__mod_pow2_i32_26
	.align 4
	.global divmod_pow2__div_pow2_i32_27
	.hidden	divmod_pow2__div_pow2_i32_27
	.type	divmod_pow2__div_pow2_i32_27, #function
	.proc	04
divmod_pow2__div_pow2_i32_27:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i32_27, .-divmod_pow2__div_pow2_i32_27
	.align 4
	.global divmod_pow2__mod_pow2_i32_27
	.hidden	divmod_pow2__mod_pow2_i32_27
	.type	divmod_pow2__mod_pow2_i32_27, #function
	.proc	04
divmod_pow2__mod_pow2_i32_27:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(134216704), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i32_27, .-divmod_pow2__mod_pow2_i32_27
	.align 4
	.global divmod_pow2__div_pow2_i32_28
	.hidden	divmod_pow2__div_pow2_i32_28
	.type	divmod_pow2__div_pow2_i32_28, #function
	.proc	04
divmod_pow2__div_pow2_i32_28:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i32_28, .-divmod_pow2__div_pow2_i32_28
	.align 4
	.global divmod_pow2__mod_pow2_i32_28
	.hidden	divmod_pow2__mod_pow2_i32_28
	.type	divmod_pow2__mod_pow2_i32_28, #function
	.proc	04
divmod_pow2__mod_pow2_i32_28:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(268434432), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i32_28, .-divmod_pow2__mod_pow2_i32_28
	.align 4
	.global divmod_pow2__div_pow2_i32_29
	.hidden	divmod_pow2__div_pow2_i32_29
	.type	divmod_pow2__div_pow2_i32_29, #function
	.proc	04
divmod_pow2__div_pow2_i32_29:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i32_29, .-divmod_pow2__div_pow2_i32_29
	.align 4
	.global divmod_pow2__mod_pow2_i32_29
	.hidden	divmod_pow2__mod_pow2_i32_29
	.type	divmod_pow2__mod_pow2_i32_29, #function
	.proc	04
divmod_pow2__mod_pow2_i32_29:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(536869888), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i32_29, .-divmod_pow2__mod_pow2_i32_29
	.align 4
	.global divmod_pow2__div_pow2_i32_30
	.hidden	divmod_pow2__div_pow2_i32_30
	.type	divmod_pow2__div_pow2_i32_30, #function
	.proc	04
divmod_pow2__div_pow2_i32_30:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i32_30, .-divmod_pow2__div_pow2_i32_30
	.align 4
	.global divmod_pow2__mod_pow2_i32_30
	.hidden	divmod_pow2__mod_pow2_i32_30
	.type	divmod_pow2__mod_pow2_i32_30, #function
	.proc	04
divmod_pow2__mod_pow2_i32_30:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(1073740800), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i32_30, .-divmod_pow2__mod_pow2_i32_30
	.align 4
	.global divmod_pow2__div_pow2_i32_31
	.hidden	divmod_pow2__div_pow2_i32_31
	.type	divmod_pow2__div_pow2_i32_31, #function
	.proc	04
divmod_pow2__div_pow2_i32_31:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sethi	%hi(-2147483648), %o0
	mov	%g1, %o1
	call	m3_div, 0
	 nop
	mov	%o0, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i32_31, .-divmod_pow2__div_pow2_i32_31
	.align 4
	.global divmod_pow2__mod_pow2_i32_31
	.hidden	divmod_pow2__mod_pow2_i32_31
	.type	divmod_pow2__mod_pow2_i32_31, #function
	.proc	04
divmod_pow2__mod_pow2_i32_31:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sethi	%hi(-2147483648), %o0
	mov	%g1, %o1
	call	m3_mod, 0
	 nop
	mov	%o0, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i32_31, .-divmod_pow2__mod_pow2_i32_31
	.align 4
	.global divmod_pow2__div_pow2_u32_0
	.hidden	divmod_pow2__div_pow2_u32_0
	.type	divmod_pow2__div_pow2_u32_0, #function
	.proc	04
divmod_pow2__div_pow2_u32_0:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u32_0, .-divmod_pow2__div_pow2_u32_0
	.align 4
	.global divmod_pow2__mod_pow2_u32_0
	.hidden	divmod_pow2__mod_pow2_u32_0
	.type	divmod_pow2__mod_pow2_u32_0, #function
	.proc	04
divmod_pow2__mod_pow2_u32_0:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	mov	0, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u32_0, .-divmod_pow2__mod_pow2_u32_0
	.align 4
	.global divmod_pow2__div_pow2_u32_1
	.hidden	divmod_pow2__div_pow2_u32_1
	.type	divmod_pow2__div_pow2_u32_1, #function
	.proc	04
divmod_pow2__div_pow2_u32_1:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u32_1, .-divmod_pow2__div_pow2_u32_1
	.align 4
	.global divmod_pow2__mod_pow2_u32_1
	.hidden	divmod_pow2__mod_pow2_u32_1
	.type	divmod_pow2__mod_pow2_u32_1, #function
	.proc	04
divmod_pow2__mod_pow2_u32_1:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u32_1, .-divmod_pow2__mod_pow2_u32_1
	.align 4
	.global divmod_pow2__div_pow2_u32_2
	.hidden	divmod_pow2__div_pow2_u32_2
	.type	divmod_pow2__div_pow2_u32_2, #function
	.proc	04
divmod_pow2__div_pow2_u32_2:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 2, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u32_2, .-divmod_pow2__div_pow2_u32_2
	.align 4
	.global divmod_pow2__mod_pow2_u32_2
	.hidden	divmod_pow2__mod_pow2_u32_2
	.type	divmod_pow2__mod_pow2_u32_2, #function
	.proc	04
divmod_pow2__mod_pow2_u32_2:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 3, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u32_2, .-divmod_pow2__mod_pow2_u32_2
	.align 4
	.global divmod_pow2__div_pow2_u32_3
	.hidden	divmod_pow2__div_pow2_u32_3
	.type	divmod_pow2__div_pow2_u32_3, #function
	.proc	04
divmod_pow2__div_pow2_u32_3:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 3, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u32_3, .-divmod_pow2__div_pow2_u32_3
	.align 4
	.global divmod_pow2__mod_pow2_u32_3
	.hidden	divmod_pow2__mod_pow2_u32_3
	.type	divmod_pow2__mod_pow2_u32_3, #function
	.proc	04
divmod_pow2__mod_pow2_u32_3:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 7, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u32_3, .-divmod_pow2__mod_pow2_u32_3
	.align 4
	.global divmod_pow2__div_pow2_u32_4
	.hidden	divmod_pow2__div_pow2_u32_4
	.type	divmod_pow2__div_pow2_u32_4, #function
	.proc	04
divmod_pow2__div_pow2_u32_4:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 4, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u32_4, .-divmod_pow2__div_pow2_u32_4
	.align 4
	.global divmod_pow2__mod_pow2_u32_4
	.hidden	divmod_pow2__mod_pow2_u32_4
	.type	divmod_pow2__mod_pow2_u32_4, #function
	.proc	04
divmod_pow2__mod_pow2_u32_4:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 15, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u32_4, .-divmod_pow2__mod_pow2_u32_4
	.align 4
	.global divmod_pow2__div_pow2_u32_5
	.hidden	divmod_pow2__div_pow2_u32_5
	.type	divmod_pow2__div_pow2_u32_5, #function
	.proc	04
divmod_pow2__div_pow2_u32_5:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u32_5, .-divmod_pow2__div_pow2_u32_5
	.align 4
	.global divmod_pow2__mod_pow2_u32_5
	.hidden	divmod_pow2__mod_pow2_u32_5
	.type	divmod_pow2__mod_pow2_u32_5, #function
	.proc	04
divmod_pow2__mod_pow2_u32_5:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u32_5, .-divmod_pow2__mod_pow2_u32_5
	.align 4
	.global divmod_pow2__div_pow2_u32_6
	.hidden	divmod_pow2__div_pow2_u32_6
	.type	divmod_pow2__div_pow2_u32_6, #function
	.proc	04
divmod_pow2__div_pow2_u32_6:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 6, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u32_6, .-divmod_pow2__div_pow2_u32_6
	.align 4
	.global divmod_pow2__mod_pow2_u32_6
	.hidden	divmod_pow2__mod_pow2_u32_6
	.type	divmod_pow2__mod_pow2_u32_6, #function
	.proc	04
divmod_pow2__mod_pow2_u32_6:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 63, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u32_6, .-divmod_pow2__mod_pow2_u32_6
	.align 4
	.global divmod_pow2__div_pow2_u32_7
	.hidden	divmod_pow2__div_pow2_u32_7
	.type	divmod_pow2__div_pow2_u32_7, #function
	.proc	04
divmod_pow2__div_pow2_u32_7:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 7, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u32_7, .-divmod_pow2__div_pow2_u32_7
	.align 4
	.global divmod_pow2__mod_pow2_u32_7
	.hidden	divmod_pow2__mod_pow2_u32_7
	.type	divmod_pow2__mod_pow2_u32_7, #function
	.proc	04
divmod_pow2__mod_pow2_u32_7:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 127, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u32_7, .-divmod_pow2__mod_pow2_u32_7
	.align 4
	.global divmod_pow2__div_pow2_u32_8
	.hidden	divmod_pow2__div_pow2_u32_8
	.type	divmod_pow2__div_pow2_u32_8, #function
	.proc	04
divmod_pow2__div_pow2_u32_8:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 8, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u32_8, .-divmod_pow2__div_pow2_u32_8
	.align 4
	.global divmod_pow2__mod_pow2_u32_8
	.hidden	divmod_pow2__mod_pow2_u32_8
	.type	divmod_pow2__mod_pow2_u32_8, #function
	.proc	04
divmod_pow2__mod_pow2_u32_8:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 255, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u32_8, .-divmod_pow2__mod_pow2_u32_8
	.align 4
	.global divmod_pow2__div_pow2_u32_9
	.hidden	divmod_pow2__div_pow2_u32_9
	.type	divmod_pow2__div_pow2_u32_9, #function
	.proc	04
divmod_pow2__div_pow2_u32_9:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 9, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u32_9, .-divmod_pow2__div_pow2_u32_9
	.align 4
	.global divmod_pow2__mod_pow2_u32_9
	.hidden	divmod_pow2__mod_pow2_u32_9
	.type	divmod_pow2__mod_pow2_u32_9, #function
	.proc	04
divmod_pow2__mod_pow2_u32_9:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 511, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u32_9, .-divmod_pow2__mod_pow2_u32_9
	.align 4
	.global divmod_pow2__div_pow2_u32_10
	.hidden	divmod_pow2__div_pow2_u32_10
	.type	divmod_pow2__div_pow2_u32_10, #function
	.proc	04
divmod_pow2__div_pow2_u32_10:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 10, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u32_10, .-divmod_pow2__div_pow2_u32_10
	.align 4
	.global divmod_pow2__mod_pow2_u32_10
	.hidden	divmod_pow2__mod_pow2_u32_10
	.type	divmod_pow2__mod_pow2_u32_10, #function
	.proc	04
divmod_pow2__mod_pow2_u32_10:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 1023, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u32_10, .-divmod_pow2__mod_pow2_u32_10
	.align 4
	.global divmod_pow2__div_pow2_u32_11
	.hidden	divmod_pow2__div_pow2_u32_11
	.type	divmod_pow2__div_pow2_u32_11, #function
	.proc	04
divmod_pow2__div_pow2_u32_11:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 11, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u32_11, .-divmod_pow2__div_pow2_u32_11
	.align 4
	.global divmod_pow2__mod_pow2_u32_11
	.hidden	divmod_pow2__mod_pow2_u32_11
	.type	divmod_pow2__mod_pow2_u32_11, #function
	.proc	04
divmod_pow2__mod_pow2_u32_11:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 2047, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u32_11, .-divmod_pow2__mod_pow2_u32_11
	.align 4
	.global divmod_pow2__div_pow2_u32_12
	.hidden	divmod_pow2__div_pow2_u32_12
	.type	divmod_pow2__div_pow2_u32_12, #function
	.proc	04
divmod_pow2__div_pow2_u32_12:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 12, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u32_12, .-divmod_pow2__div_pow2_u32_12
	.align 4
	.global divmod_pow2__mod_pow2_u32_12
	.hidden	divmod_pow2__mod_pow2_u32_12
	.type	divmod_pow2__mod_pow2_u32_12, #function
	.proc	04
divmod_pow2__mod_pow2_u32_12:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 4095, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u32_12, .-divmod_pow2__mod_pow2_u32_12
	.align 4
	.global divmod_pow2__div_pow2_u32_13
	.hidden	divmod_pow2__div_pow2_u32_13
	.type	divmod_pow2__div_pow2_u32_13, #function
	.proc	04
divmod_pow2__div_pow2_u32_13:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 13, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u32_13, .-divmod_pow2__div_pow2_u32_13
	.align 4
	.global divmod_pow2__mod_pow2_u32_13
	.hidden	divmod_pow2__mod_pow2_u32_13
	.type	divmod_pow2__mod_pow2_u32_13, #function
	.proc	04
divmod_pow2__mod_pow2_u32_13:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u32_13, .-divmod_pow2__mod_pow2_u32_13
	.align 4
	.global divmod_pow2__div_pow2_u32_14
	.hidden	divmod_pow2__div_pow2_u32_14
	.type	divmod_pow2__div_pow2_u32_14, #function
	.proc	04
divmod_pow2__div_pow2_u32_14:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 14, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u32_14, .-divmod_pow2__div_pow2_u32_14
	.align 4
	.global divmod_pow2__mod_pow2_u32_14
	.hidden	divmod_pow2__mod_pow2_u32_14
	.type	divmod_pow2__mod_pow2_u32_14, #function
	.proc	04
divmod_pow2__mod_pow2_u32_14:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u32_14, .-divmod_pow2__mod_pow2_u32_14
	.align 4
	.global divmod_pow2__div_pow2_u32_15
	.hidden	divmod_pow2__div_pow2_u32_15
	.type	divmod_pow2__div_pow2_u32_15, #function
	.proc	04
divmod_pow2__div_pow2_u32_15:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 15, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u32_15, .-divmod_pow2__div_pow2_u32_15
	.align 4
	.global divmod_pow2__mod_pow2_u32_15
	.hidden	divmod_pow2__mod_pow2_u32_15
	.type	divmod_pow2__mod_pow2_u32_15, #function
	.proc	04
divmod_pow2__mod_pow2_u32_15:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u32_15, .-divmod_pow2__mod_pow2_u32_15
	.align 4
	.global divmod_pow2__div_pow2_u32_16
	.hidden	divmod_pow2__div_pow2_u32_16
	.type	divmod_pow2__div_pow2_u32_16, #function
	.proc	04
divmod_pow2__div_pow2_u32_16:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u32_16, .-divmod_pow2__div_pow2_u32_16
	.align 4
	.global divmod_pow2__mod_pow2_u32_16
	.hidden	divmod_pow2__mod_pow2_u32_16
	.type	divmod_pow2__mod_pow2_u32_16, #function
	.proc	04
divmod_pow2__mod_pow2_u32_16:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u32_16, .-divmod_pow2__mod_pow2_u32_16
	.align 4
	.global divmod_pow2__div_pow2_u32_17
	.hidden	divmod_pow2__div_pow2_u32_17
	.type	divmod_pow2__div_pow2_u32_17, #function
	.proc	04
divmod_pow2__div_pow2_u32_17:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 17, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u32_17, .-divmod_pow2__div_pow2_u32_17
	.align 4
	.global divmod_pow2__mod_pow2_u32_17
	.hidden	divmod_pow2__mod_pow2_u32_17
	.type	divmod_pow2__mod_pow2_u32_17, #function
	.proc	04
divmod_pow2__mod_pow2_u32_17:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(130048), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u32_17, .-divmod_pow2__mod_pow2_u32_17
	.align 4
	.global divmod_pow2__div_pow2_u32_18
	.hidden	divmod_pow2__div_pow2_u32_18
	.type	divmod_pow2__div_pow2_u32_18, #function
	.proc	04
divmod_pow2__div_pow2_u32_18:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 18, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u32_18, .-divmod_pow2__div_pow2_u32_18
	.align 4
	.global divmod_pow2__mod_pow2_u32_18
	.hidden	divmod_pow2__mod_pow2_u32_18
	.type	divmod_pow2__mod_pow2_u32_18, #function
	.proc	04
divmod_pow2__mod_pow2_u32_18:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(261120), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u32_18, .-divmod_pow2__mod_pow2_u32_18
	.align 4
	.global divmod_pow2__div_pow2_u32_19
	.hidden	divmod_pow2__div_pow2_u32_19
	.type	divmod_pow2__div_pow2_u32_19, #function
	.proc	04
divmod_pow2__div_pow2_u32_19:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 19, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u32_19, .-divmod_pow2__div_pow2_u32_19
	.align 4
	.global divmod_pow2__mod_pow2_u32_19
	.hidden	divmod_pow2__mod_pow2_u32_19
	.type	divmod_pow2__mod_pow2_u32_19, #function
	.proc	04
divmod_pow2__mod_pow2_u32_19:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(523264), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u32_19, .-divmod_pow2__mod_pow2_u32_19
	.align 4
	.global divmod_pow2__div_pow2_u32_20
	.hidden	divmod_pow2__div_pow2_u32_20
	.type	divmod_pow2__div_pow2_u32_20, #function
	.proc	04
divmod_pow2__div_pow2_u32_20:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 20, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u32_20, .-divmod_pow2__div_pow2_u32_20
	.align 4
	.global divmod_pow2__mod_pow2_u32_20
	.hidden	divmod_pow2__mod_pow2_u32_20
	.type	divmod_pow2__mod_pow2_u32_20, #function
	.proc	04
divmod_pow2__mod_pow2_u32_20:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(1047552), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u32_20, .-divmod_pow2__mod_pow2_u32_20
	.align 4
	.global divmod_pow2__div_pow2_u32_21
	.hidden	divmod_pow2__div_pow2_u32_21
	.type	divmod_pow2__div_pow2_u32_21, #function
	.proc	04
divmod_pow2__div_pow2_u32_21:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 21, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u32_21, .-divmod_pow2__div_pow2_u32_21
	.align 4
	.global divmod_pow2__mod_pow2_u32_21
	.hidden	divmod_pow2__mod_pow2_u32_21
	.type	divmod_pow2__mod_pow2_u32_21, #function
	.proc	04
divmod_pow2__mod_pow2_u32_21:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(2096128), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u32_21, .-divmod_pow2__mod_pow2_u32_21
	.align 4
	.global divmod_pow2__div_pow2_u32_22
	.hidden	divmod_pow2__div_pow2_u32_22
	.type	divmod_pow2__div_pow2_u32_22, #function
	.proc	04
divmod_pow2__div_pow2_u32_22:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 22, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u32_22, .-divmod_pow2__div_pow2_u32_22
	.align 4
	.global divmod_pow2__mod_pow2_u32_22
	.hidden	divmod_pow2__mod_pow2_u32_22
	.type	divmod_pow2__mod_pow2_u32_22, #function
	.proc	04
divmod_pow2__mod_pow2_u32_22:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(4193280), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u32_22, .-divmod_pow2__mod_pow2_u32_22
	.align 4
	.global divmod_pow2__div_pow2_u32_23
	.hidden	divmod_pow2__div_pow2_u32_23
	.type	divmod_pow2__div_pow2_u32_23, #function
	.proc	04
divmod_pow2__div_pow2_u32_23:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u32_23, .-divmod_pow2__div_pow2_u32_23
	.align 4
	.global divmod_pow2__mod_pow2_u32_23
	.hidden	divmod_pow2__mod_pow2_u32_23
	.type	divmod_pow2__mod_pow2_u32_23, #function
	.proc	04
divmod_pow2__mod_pow2_u32_23:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(8387584), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u32_23, .-divmod_pow2__mod_pow2_u32_23
	.align 4
	.global divmod_pow2__div_pow2_u32_24
	.hidden	divmod_pow2__div_pow2_u32_24
	.type	divmod_pow2__div_pow2_u32_24, #function
	.proc	04
divmod_pow2__div_pow2_u32_24:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u32_24, .-divmod_pow2__div_pow2_u32_24
	.align 4
	.global divmod_pow2__mod_pow2_u32_24
	.hidden	divmod_pow2__mod_pow2_u32_24
	.type	divmod_pow2__mod_pow2_u32_24, #function
	.proc	04
divmod_pow2__mod_pow2_u32_24:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(16776192), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u32_24, .-divmod_pow2__mod_pow2_u32_24
	.align 4
	.global divmod_pow2__div_pow2_u32_25
	.hidden	divmod_pow2__div_pow2_u32_25
	.type	divmod_pow2__div_pow2_u32_25, #function
	.proc	04
divmod_pow2__div_pow2_u32_25:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u32_25, .-divmod_pow2__div_pow2_u32_25
	.align 4
	.global divmod_pow2__mod_pow2_u32_25
	.hidden	divmod_pow2__mod_pow2_u32_25
	.type	divmod_pow2__mod_pow2_u32_25, #function
	.proc	04
divmod_pow2__mod_pow2_u32_25:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(33553408), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u32_25, .-divmod_pow2__mod_pow2_u32_25
	.align 4
	.global divmod_pow2__div_pow2_u32_26
	.hidden	divmod_pow2__div_pow2_u32_26
	.type	divmod_pow2__div_pow2_u32_26, #function
	.proc	04
divmod_pow2__div_pow2_u32_26:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u32_26, .-divmod_pow2__div_pow2_u32_26
	.align 4
	.global divmod_pow2__mod_pow2_u32_26
	.hidden	divmod_pow2__mod_pow2_u32_26
	.type	divmod_pow2__mod_pow2_u32_26, #function
	.proc	04
divmod_pow2__mod_pow2_u32_26:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(67107840), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u32_26, .-divmod_pow2__mod_pow2_u32_26
	.align 4
	.global divmod_pow2__div_pow2_u32_27
	.hidden	divmod_pow2__div_pow2_u32_27
	.type	divmod_pow2__div_pow2_u32_27, #function
	.proc	04
divmod_pow2__div_pow2_u32_27:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u32_27, .-divmod_pow2__div_pow2_u32_27
	.align 4
	.global divmod_pow2__mod_pow2_u32_27
	.hidden	divmod_pow2__mod_pow2_u32_27
	.type	divmod_pow2__mod_pow2_u32_27, #function
	.proc	04
divmod_pow2__mod_pow2_u32_27:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(134216704), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u32_27, .-divmod_pow2__mod_pow2_u32_27
	.align 4
	.global divmod_pow2__div_pow2_u32_28
	.hidden	divmod_pow2__div_pow2_u32_28
	.type	divmod_pow2__div_pow2_u32_28, #function
	.proc	04
divmod_pow2__div_pow2_u32_28:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u32_28, .-divmod_pow2__div_pow2_u32_28
	.align 4
	.global divmod_pow2__mod_pow2_u32_28
	.hidden	divmod_pow2__mod_pow2_u32_28
	.type	divmod_pow2__mod_pow2_u32_28, #function
	.proc	04
divmod_pow2__mod_pow2_u32_28:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(268434432), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u32_28, .-divmod_pow2__mod_pow2_u32_28
	.align 4
	.global divmod_pow2__div_pow2_u32_29
	.hidden	divmod_pow2__div_pow2_u32_29
	.type	divmod_pow2__div_pow2_u32_29, #function
	.proc	04
divmod_pow2__div_pow2_u32_29:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u32_29, .-divmod_pow2__div_pow2_u32_29
	.align 4
	.global divmod_pow2__mod_pow2_u32_29
	.hidden	divmod_pow2__mod_pow2_u32_29
	.type	divmod_pow2__mod_pow2_u32_29, #function
	.proc	04
divmod_pow2__mod_pow2_u32_29:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(536869888), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u32_29, .-divmod_pow2__mod_pow2_u32_29
	.align 4
	.global divmod_pow2__div_pow2_u32_30
	.hidden	divmod_pow2__div_pow2_u32_30
	.type	divmod_pow2__div_pow2_u32_30, #function
	.proc	04
divmod_pow2__div_pow2_u32_30:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u32_30, .-divmod_pow2__div_pow2_u32_30
	.align 4
	.global divmod_pow2__mod_pow2_u32_30
	.hidden	divmod_pow2__mod_pow2_u32_30
	.type	divmod_pow2__mod_pow2_u32_30, #function
	.proc	04
divmod_pow2__mod_pow2_u32_30:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(1073740800), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u32_30, .-divmod_pow2__mod_pow2_u32_30
	.align 4
	.global divmod_pow2__div_pow2_u32_31
	.hidden	divmod_pow2__div_pow2_u32_31
	.type	divmod_pow2__div_pow2_u32_31, #function
	.proc	04
divmod_pow2__div_pow2_u32_31:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sethi	%hi(-2147483648), %o0
	mov	%g1, %o1
	call	m3_div, 0
	 nop
	mov	%o0, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u32_31, .-divmod_pow2__div_pow2_u32_31
	.align 4
	.global divmod_pow2__mod_pow2_u32_31
	.hidden	divmod_pow2__mod_pow2_u32_31
	.type	divmod_pow2__mod_pow2_u32_31, #function
	.proc	04
divmod_pow2__mod_pow2_u32_31:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sethi	%hi(-2147483648), %o0
	mov	%g1, %o1
	call	m3_mod, 0
	 nop
	mov	%o0, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u32_31, .-divmod_pow2__mod_pow2_u32_31
	.align 4
	.global divmod_pow2__div_pow2_i64_0
	.hidden	divmod_pow2__div_pow2_i64_0
	.type	divmod_pow2__div_pow2_i64_0, #function
	.proc	05
divmod_pow2__div_pow2_i64_0:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_0, .-divmod_pow2__div_pow2_i64_0
	.align 4
	.global divmod_pow2__mod_pow2_i64_0
	.hidden	divmod_pow2__mod_pow2_i64_0
	.type	divmod_pow2__mod_pow2_i64_0, #function
	.proc	05
divmod_pow2__mod_pow2_i64_0:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	mov	0, %g2
	mov	0, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_0, .-divmod_pow2__mod_pow2_i64_0
	.align 4
	.global divmod_pow2__div_pow2_i64_1
	.hidden	divmod_pow2__div_pow2_i64_1
	.type	divmod_pow2__div_pow2_i64_1, #function
	.proc	05
divmod_pow2__div_pow2_i64_1:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 1, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_1, .-divmod_pow2__div_pow2_i64_1
	.align 4
	.global divmod_pow2__mod_pow2_i64_1
	.hidden	divmod_pow2__mod_pow2_i64_1
	.type	divmod_pow2__mod_pow2_i64_1, #function
	.proc	05
divmod_pow2__mod_pow2_i64_1:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 1, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_1, .-divmod_pow2__mod_pow2_i64_1
	.align 4
	.global divmod_pow2__div_pow2_i64_2
	.hidden	divmod_pow2__div_pow2_i64_2
	.type	divmod_pow2__div_pow2_i64_2, #function
	.proc	05
divmod_pow2__div_pow2_i64_2:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 2, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_2, .-divmod_pow2__div_pow2_i64_2
	.align 4
	.global divmod_pow2__mod_pow2_i64_2
	.hidden	divmod_pow2__mod_pow2_i64_2
	.type	divmod_pow2__mod_pow2_i64_2, #function
	.proc	05
divmod_pow2__mod_pow2_i64_2:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_2, .-divmod_pow2__mod_pow2_i64_2
	.align 4
	.global divmod_pow2__div_pow2_i64_3
	.hidden	divmod_pow2__div_pow2_i64_3
	.type	divmod_pow2__div_pow2_i64_3, #function
	.proc	05
divmod_pow2__div_pow2_i64_3:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 3, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_3, .-divmod_pow2__div_pow2_i64_3
	.align 4
	.global divmod_pow2__mod_pow2_i64_3
	.hidden	divmod_pow2__mod_pow2_i64_3
	.type	divmod_pow2__mod_pow2_i64_3, #function
	.proc	05
divmod_pow2__mod_pow2_i64_3:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 7, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_3, .-divmod_pow2__mod_pow2_i64_3
	.align 4
	.global divmod_pow2__div_pow2_i64_4
	.hidden	divmod_pow2__div_pow2_i64_4
	.type	divmod_pow2__div_pow2_i64_4, #function
	.proc	05
divmod_pow2__div_pow2_i64_4:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 4, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_4, .-divmod_pow2__div_pow2_i64_4
	.align 4
	.global divmod_pow2__mod_pow2_i64_4
	.hidden	divmod_pow2__mod_pow2_i64_4
	.type	divmod_pow2__mod_pow2_i64_4, #function
	.proc	05
divmod_pow2__mod_pow2_i64_4:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 15, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_4, .-divmod_pow2__mod_pow2_i64_4
	.align 4
	.global divmod_pow2__div_pow2_i64_5
	.hidden	divmod_pow2__div_pow2_i64_5
	.type	divmod_pow2__div_pow2_i64_5, #function
	.proc	05
divmod_pow2__div_pow2_i64_5:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 5, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_5, .-divmod_pow2__div_pow2_i64_5
	.align 4
	.global divmod_pow2__mod_pow2_i64_5
	.hidden	divmod_pow2__mod_pow2_i64_5
	.type	divmod_pow2__mod_pow2_i64_5, #function
	.proc	05
divmod_pow2__mod_pow2_i64_5:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 31, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_5, .-divmod_pow2__mod_pow2_i64_5
	.align 4
	.global divmod_pow2__div_pow2_i64_6
	.hidden	divmod_pow2__div_pow2_i64_6
	.type	divmod_pow2__div_pow2_i64_6, #function
	.proc	05
divmod_pow2__div_pow2_i64_6:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 6, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_6, .-divmod_pow2__div_pow2_i64_6
	.align 4
	.global divmod_pow2__mod_pow2_i64_6
	.hidden	divmod_pow2__mod_pow2_i64_6
	.type	divmod_pow2__mod_pow2_i64_6, #function
	.proc	05
divmod_pow2__mod_pow2_i64_6:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 63, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_6, .-divmod_pow2__mod_pow2_i64_6
	.align 4
	.global divmod_pow2__div_pow2_i64_7
	.hidden	divmod_pow2__div_pow2_i64_7
	.type	divmod_pow2__div_pow2_i64_7, #function
	.proc	05
divmod_pow2__div_pow2_i64_7:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 7, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_7, .-divmod_pow2__div_pow2_i64_7
	.align 4
	.global divmod_pow2__mod_pow2_i64_7
	.hidden	divmod_pow2__mod_pow2_i64_7
	.type	divmod_pow2__mod_pow2_i64_7, #function
	.proc	05
divmod_pow2__mod_pow2_i64_7:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 127, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_7, .-divmod_pow2__mod_pow2_i64_7
	.align 4
	.global divmod_pow2__div_pow2_i64_8
	.hidden	divmod_pow2__div_pow2_i64_8
	.type	divmod_pow2__div_pow2_i64_8, #function
	.proc	05
divmod_pow2__div_pow2_i64_8:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 8, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_8, .-divmod_pow2__div_pow2_i64_8
	.align 4
	.global divmod_pow2__mod_pow2_i64_8
	.hidden	divmod_pow2__mod_pow2_i64_8
	.type	divmod_pow2__mod_pow2_i64_8, #function
	.proc	05
divmod_pow2__mod_pow2_i64_8:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 255, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_8, .-divmod_pow2__mod_pow2_i64_8
	.align 4
	.global divmod_pow2__div_pow2_i64_9
	.hidden	divmod_pow2__div_pow2_i64_9
	.type	divmod_pow2__div_pow2_i64_9, #function
	.proc	05
divmod_pow2__div_pow2_i64_9:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 9, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_9, .-divmod_pow2__div_pow2_i64_9
	.align 4
	.global divmod_pow2__mod_pow2_i64_9
	.hidden	divmod_pow2__mod_pow2_i64_9
	.type	divmod_pow2__mod_pow2_i64_9, #function
	.proc	05
divmod_pow2__mod_pow2_i64_9:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 511, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_9, .-divmod_pow2__mod_pow2_i64_9
	.align 4
	.global divmod_pow2__div_pow2_i64_10
	.hidden	divmod_pow2__div_pow2_i64_10
	.type	divmod_pow2__div_pow2_i64_10, #function
	.proc	05
divmod_pow2__div_pow2_i64_10:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 10, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_10, .-divmod_pow2__div_pow2_i64_10
	.align 4
	.global divmod_pow2__mod_pow2_i64_10
	.hidden	divmod_pow2__mod_pow2_i64_10
	.type	divmod_pow2__mod_pow2_i64_10, #function
	.proc	05
divmod_pow2__mod_pow2_i64_10:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 1023, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_10, .-divmod_pow2__mod_pow2_i64_10
	.align 4
	.global divmod_pow2__div_pow2_i64_11
	.hidden	divmod_pow2__div_pow2_i64_11
	.type	divmod_pow2__div_pow2_i64_11, #function
	.proc	05
divmod_pow2__div_pow2_i64_11:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 11, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_11, .-divmod_pow2__div_pow2_i64_11
	.align 4
	.global divmod_pow2__mod_pow2_i64_11
	.hidden	divmod_pow2__mod_pow2_i64_11
	.type	divmod_pow2__mod_pow2_i64_11, #function
	.proc	05
divmod_pow2__mod_pow2_i64_11:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 2047, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_11, .-divmod_pow2__mod_pow2_i64_11
	.align 4
	.global divmod_pow2__div_pow2_i64_12
	.hidden	divmod_pow2__div_pow2_i64_12
	.type	divmod_pow2__div_pow2_i64_12, #function
	.proc	05
divmod_pow2__div_pow2_i64_12:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 12, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_12, .-divmod_pow2__div_pow2_i64_12
	.align 4
	.global divmod_pow2__mod_pow2_i64_12
	.hidden	divmod_pow2__mod_pow2_i64_12
	.type	divmod_pow2__mod_pow2_i64_12, #function
	.proc	05
divmod_pow2__mod_pow2_i64_12:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 4095, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_12, .-divmod_pow2__mod_pow2_i64_12
	.align 4
	.global divmod_pow2__div_pow2_i64_13
	.hidden	divmod_pow2__div_pow2_i64_13
	.type	divmod_pow2__div_pow2_i64_13, #function
	.proc	05
divmod_pow2__div_pow2_i64_13:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 13, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_13, .-divmod_pow2__div_pow2_i64_13
	.align 4
	.global divmod_pow2__mod_pow2_i64_13
	.hidden	divmod_pow2__mod_pow2_i64_13
	.type	divmod_pow2__mod_pow2_i64_13, #function
	.proc	05
divmod_pow2__mod_pow2_i64_13:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(7168), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_13, .-divmod_pow2__mod_pow2_i64_13
	.align 4
	.global divmod_pow2__div_pow2_i64_14
	.hidden	divmod_pow2__div_pow2_i64_14
	.type	divmod_pow2__div_pow2_i64_14, #function
	.proc	05
divmod_pow2__div_pow2_i64_14:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 14, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_14, .-divmod_pow2__div_pow2_i64_14
	.align 4
	.global divmod_pow2__mod_pow2_i64_14
	.hidden	divmod_pow2__mod_pow2_i64_14
	.type	divmod_pow2__mod_pow2_i64_14, #function
	.proc	05
divmod_pow2__mod_pow2_i64_14:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(15360), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_14, .-divmod_pow2__mod_pow2_i64_14
	.align 4
	.global divmod_pow2__div_pow2_i64_15
	.hidden	divmod_pow2__div_pow2_i64_15
	.type	divmod_pow2__div_pow2_i64_15, #function
	.proc	05
divmod_pow2__div_pow2_i64_15:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 15, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_15, .-divmod_pow2__div_pow2_i64_15
	.align 4
	.global divmod_pow2__mod_pow2_i64_15
	.hidden	divmod_pow2__mod_pow2_i64_15
	.type	divmod_pow2__mod_pow2_i64_15, #function
	.proc	05
divmod_pow2__mod_pow2_i64_15:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(31744), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_15, .-divmod_pow2__mod_pow2_i64_15
	.align 4
	.global divmod_pow2__div_pow2_i64_16
	.hidden	divmod_pow2__div_pow2_i64_16
	.type	divmod_pow2__div_pow2_i64_16, #function
	.proc	05
divmod_pow2__div_pow2_i64_16:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 16, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_16, .-divmod_pow2__div_pow2_i64_16
	.align 4
	.global divmod_pow2__mod_pow2_i64_16
	.hidden	divmod_pow2__mod_pow2_i64_16
	.type	divmod_pow2__mod_pow2_i64_16, #function
	.proc	05
divmod_pow2__mod_pow2_i64_16:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(64512), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_16, .-divmod_pow2__mod_pow2_i64_16
	.align 4
	.global divmod_pow2__div_pow2_i64_17
	.hidden	divmod_pow2__div_pow2_i64_17
	.type	divmod_pow2__div_pow2_i64_17, #function
	.proc	05
divmod_pow2__div_pow2_i64_17:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 17, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_17, .-divmod_pow2__div_pow2_i64_17
	.align 4
	.global divmod_pow2__mod_pow2_i64_17
	.hidden	divmod_pow2__mod_pow2_i64_17
	.type	divmod_pow2__mod_pow2_i64_17, #function
	.proc	05
divmod_pow2__mod_pow2_i64_17:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(130048), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_17, .-divmod_pow2__mod_pow2_i64_17
	.align 4
	.global divmod_pow2__div_pow2_i64_18
	.hidden	divmod_pow2__div_pow2_i64_18
	.type	divmod_pow2__div_pow2_i64_18, #function
	.proc	05
divmod_pow2__div_pow2_i64_18:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 18, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_18, .-divmod_pow2__div_pow2_i64_18
	.align 4
	.global divmod_pow2__mod_pow2_i64_18
	.hidden	divmod_pow2__mod_pow2_i64_18
	.type	divmod_pow2__mod_pow2_i64_18, #function
	.proc	05
divmod_pow2__mod_pow2_i64_18:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(261120), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_18, .-divmod_pow2__mod_pow2_i64_18
	.align 4
	.global divmod_pow2__div_pow2_i64_19
	.hidden	divmod_pow2__div_pow2_i64_19
	.type	divmod_pow2__div_pow2_i64_19, #function
	.proc	05
divmod_pow2__div_pow2_i64_19:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 19, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_19, .-divmod_pow2__div_pow2_i64_19
	.align 4
	.global divmod_pow2__mod_pow2_i64_19
	.hidden	divmod_pow2__mod_pow2_i64_19
	.type	divmod_pow2__mod_pow2_i64_19, #function
	.proc	05
divmod_pow2__mod_pow2_i64_19:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(523264), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_19, .-divmod_pow2__mod_pow2_i64_19
	.align 4
	.global divmod_pow2__div_pow2_i64_20
	.hidden	divmod_pow2__div_pow2_i64_20
	.type	divmod_pow2__div_pow2_i64_20, #function
	.proc	05
divmod_pow2__div_pow2_i64_20:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 20, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_20, .-divmod_pow2__div_pow2_i64_20
	.align 4
	.global divmod_pow2__mod_pow2_i64_20
	.hidden	divmod_pow2__mod_pow2_i64_20
	.type	divmod_pow2__mod_pow2_i64_20, #function
	.proc	05
divmod_pow2__mod_pow2_i64_20:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(1047552), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_20, .-divmod_pow2__mod_pow2_i64_20
	.align 4
	.global divmod_pow2__div_pow2_i64_21
	.hidden	divmod_pow2__div_pow2_i64_21
	.type	divmod_pow2__div_pow2_i64_21, #function
	.proc	05
divmod_pow2__div_pow2_i64_21:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 21, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_21, .-divmod_pow2__div_pow2_i64_21
	.align 4
	.global divmod_pow2__mod_pow2_i64_21
	.hidden	divmod_pow2__mod_pow2_i64_21
	.type	divmod_pow2__mod_pow2_i64_21, #function
	.proc	05
divmod_pow2__mod_pow2_i64_21:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(2096128), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_21, .-divmod_pow2__mod_pow2_i64_21
	.align 4
	.global divmod_pow2__div_pow2_i64_22
	.hidden	divmod_pow2__div_pow2_i64_22
	.type	divmod_pow2__div_pow2_i64_22, #function
	.proc	05
divmod_pow2__div_pow2_i64_22:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 22, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_22, .-divmod_pow2__div_pow2_i64_22
	.align 4
	.global divmod_pow2__mod_pow2_i64_22
	.hidden	divmod_pow2__mod_pow2_i64_22
	.type	divmod_pow2__mod_pow2_i64_22, #function
	.proc	05
divmod_pow2__mod_pow2_i64_22:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(4193280), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_22, .-divmod_pow2__mod_pow2_i64_22
	.align 4
	.global divmod_pow2__div_pow2_i64_23
	.hidden	divmod_pow2__div_pow2_i64_23
	.type	divmod_pow2__div_pow2_i64_23, #function
	.proc	05
divmod_pow2__div_pow2_i64_23:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 23, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_23, .-divmod_pow2__div_pow2_i64_23
	.align 4
	.global divmod_pow2__mod_pow2_i64_23
	.hidden	divmod_pow2__mod_pow2_i64_23
	.type	divmod_pow2__mod_pow2_i64_23, #function
	.proc	05
divmod_pow2__mod_pow2_i64_23:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(8387584), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_23, .-divmod_pow2__mod_pow2_i64_23
	.align 4
	.global divmod_pow2__div_pow2_i64_24
	.hidden	divmod_pow2__div_pow2_i64_24
	.type	divmod_pow2__div_pow2_i64_24, #function
	.proc	05
divmod_pow2__div_pow2_i64_24:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 24, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_24, .-divmod_pow2__div_pow2_i64_24
	.align 4
	.global divmod_pow2__mod_pow2_i64_24
	.hidden	divmod_pow2__mod_pow2_i64_24
	.type	divmod_pow2__mod_pow2_i64_24, #function
	.proc	05
divmod_pow2__mod_pow2_i64_24:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(16776192), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_24, .-divmod_pow2__mod_pow2_i64_24
	.align 4
	.global divmod_pow2__div_pow2_i64_25
	.hidden	divmod_pow2__div_pow2_i64_25
	.type	divmod_pow2__div_pow2_i64_25, #function
	.proc	05
divmod_pow2__div_pow2_i64_25:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 25, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_25, .-divmod_pow2__div_pow2_i64_25
	.align 4
	.global divmod_pow2__mod_pow2_i64_25
	.hidden	divmod_pow2__mod_pow2_i64_25
	.type	divmod_pow2__mod_pow2_i64_25, #function
	.proc	05
divmod_pow2__mod_pow2_i64_25:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(33553408), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_25, .-divmod_pow2__mod_pow2_i64_25
	.align 4
	.global divmod_pow2__div_pow2_i64_26
	.hidden	divmod_pow2__div_pow2_i64_26
	.type	divmod_pow2__div_pow2_i64_26, #function
	.proc	05
divmod_pow2__div_pow2_i64_26:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 26, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_26, .-divmod_pow2__div_pow2_i64_26
	.align 4
	.global divmod_pow2__mod_pow2_i64_26
	.hidden	divmod_pow2__mod_pow2_i64_26
	.type	divmod_pow2__mod_pow2_i64_26, #function
	.proc	05
divmod_pow2__mod_pow2_i64_26:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(67107840), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_26, .-divmod_pow2__mod_pow2_i64_26
	.align 4
	.global divmod_pow2__div_pow2_i64_27
	.hidden	divmod_pow2__div_pow2_i64_27
	.type	divmod_pow2__div_pow2_i64_27, #function
	.proc	05
divmod_pow2__div_pow2_i64_27:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 27, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_27, .-divmod_pow2__div_pow2_i64_27
	.align 4
	.global divmod_pow2__mod_pow2_i64_27
	.hidden	divmod_pow2__mod_pow2_i64_27
	.type	divmod_pow2__mod_pow2_i64_27, #function
	.proc	05
divmod_pow2__mod_pow2_i64_27:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(134216704), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_27, .-divmod_pow2__mod_pow2_i64_27
	.align 4
	.global divmod_pow2__div_pow2_i64_28
	.hidden	divmod_pow2__div_pow2_i64_28
	.type	divmod_pow2__div_pow2_i64_28, #function
	.proc	05
divmod_pow2__div_pow2_i64_28:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 28, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_28, .-divmod_pow2__div_pow2_i64_28
	.align 4
	.global divmod_pow2__mod_pow2_i64_28
	.hidden	divmod_pow2__mod_pow2_i64_28
	.type	divmod_pow2__mod_pow2_i64_28, #function
	.proc	05
divmod_pow2__mod_pow2_i64_28:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(268434432), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_28, .-divmod_pow2__mod_pow2_i64_28
	.align 4
	.global divmod_pow2__div_pow2_i64_29
	.hidden	divmod_pow2__div_pow2_i64_29
	.type	divmod_pow2__div_pow2_i64_29, #function
	.proc	05
divmod_pow2__div_pow2_i64_29:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 29, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_29, .-divmod_pow2__div_pow2_i64_29
	.align 4
	.global divmod_pow2__mod_pow2_i64_29
	.hidden	divmod_pow2__mod_pow2_i64_29
	.type	divmod_pow2__mod_pow2_i64_29, #function
	.proc	05
divmod_pow2__mod_pow2_i64_29:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(536869888), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_29, .-divmod_pow2__mod_pow2_i64_29
	.align 4
	.global divmod_pow2__div_pow2_i64_30
	.hidden	divmod_pow2__div_pow2_i64_30
	.type	divmod_pow2__div_pow2_i64_30, #function
	.proc	05
divmod_pow2__div_pow2_i64_30:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 30, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_30, .-divmod_pow2__div_pow2_i64_30
	.align 4
	.global divmod_pow2__mod_pow2_i64_30
	.hidden	divmod_pow2__mod_pow2_i64_30
	.type	divmod_pow2__mod_pow2_i64_30, #function
	.proc	05
divmod_pow2__mod_pow2_i64_30:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(1073740800), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_30, .-divmod_pow2__mod_pow2_i64_30
	.align 4
	.global divmod_pow2__div_pow2_i64_31
	.hidden	divmod_pow2__div_pow2_i64_31
	.type	divmod_pow2__div_pow2_i64_31, #function
	.proc	05
divmod_pow2__div_pow2_i64_31:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 31, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_31, .-divmod_pow2__div_pow2_i64_31
	.align 4
	.global divmod_pow2__mod_pow2_i64_31
	.hidden	divmod_pow2__mod_pow2_i64_31
	.type	divmod_pow2__mod_pow2_i64_31, #function
	.proc	05
divmod_pow2__mod_pow2_i64_31:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(2147482624), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_31, .-divmod_pow2__mod_pow2_i64_31
	.align 4
	.global divmod_pow2__div_pow2_i64_32
	.hidden	divmod_pow2__div_pow2_i64_32
	.type	divmod_pow2__div_pow2_i64_32, #function
	.proc	05
divmod_pow2__div_pow2_i64_32:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 32, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_32, .-divmod_pow2__div_pow2_i64_32
	.align 4
	.global divmod_pow2__mod_pow2_i64_32
	.hidden	divmod_pow2__mod_pow2_i64_32
	.type	divmod_pow2__mod_pow2_i64_32, #function
	.proc	05
divmod_pow2__mod_pow2_i64_32:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	0, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_32, .-divmod_pow2__mod_pow2_i64_32
	.align 4
	.global divmod_pow2__div_pow2_i64_33
	.hidden	divmod_pow2__div_pow2_i64_33
	.type	divmod_pow2__div_pow2_i64_33, #function
	.proc	05
divmod_pow2__div_pow2_i64_33:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 33, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_33, .-divmod_pow2__div_pow2_i64_33
	.align 4
	.global divmod_pow2__mod_pow2_i64_33
	.hidden	divmod_pow2__mod_pow2_i64_33
	.type	divmod_pow2__mod_pow2_i64_33, #function
	.proc	05
divmod_pow2__mod_pow2_i64_33:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	1, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_33, .-divmod_pow2__mod_pow2_i64_33
	.align 4
	.global divmod_pow2__div_pow2_i64_34
	.hidden	divmod_pow2__div_pow2_i64_34
	.type	divmod_pow2__div_pow2_i64_34, #function
	.proc	05
divmod_pow2__div_pow2_i64_34:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 34, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_34, .-divmod_pow2__div_pow2_i64_34
	.align 4
	.global divmod_pow2__mod_pow2_i64_34
	.hidden	divmod_pow2__mod_pow2_i64_34
	.type	divmod_pow2__mod_pow2_i64_34, #function
	.proc	05
divmod_pow2__mod_pow2_i64_34:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	3, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_34, .-divmod_pow2__mod_pow2_i64_34
	.align 4
	.global divmod_pow2__div_pow2_i64_35
	.hidden	divmod_pow2__div_pow2_i64_35
	.type	divmod_pow2__div_pow2_i64_35, #function
	.proc	05
divmod_pow2__div_pow2_i64_35:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 35, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_35, .-divmod_pow2__div_pow2_i64_35
	.align 4
	.global divmod_pow2__mod_pow2_i64_35
	.hidden	divmod_pow2__mod_pow2_i64_35
	.type	divmod_pow2__mod_pow2_i64_35, #function
	.proc	05
divmod_pow2__mod_pow2_i64_35:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	7, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_35, .-divmod_pow2__mod_pow2_i64_35
	.align 4
	.global divmod_pow2__div_pow2_i64_36
	.hidden	divmod_pow2__div_pow2_i64_36
	.type	divmod_pow2__div_pow2_i64_36, #function
	.proc	05
divmod_pow2__div_pow2_i64_36:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 36, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_36, .-divmod_pow2__div_pow2_i64_36
	.align 4
	.global divmod_pow2__mod_pow2_i64_36
	.hidden	divmod_pow2__mod_pow2_i64_36
	.type	divmod_pow2__mod_pow2_i64_36, #function
	.proc	05
divmod_pow2__mod_pow2_i64_36:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	15, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_36, .-divmod_pow2__mod_pow2_i64_36
	.align 4
	.global divmod_pow2__div_pow2_i64_37
	.hidden	divmod_pow2__div_pow2_i64_37
	.type	divmod_pow2__div_pow2_i64_37, #function
	.proc	05
divmod_pow2__div_pow2_i64_37:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 37, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_37, .-divmod_pow2__div_pow2_i64_37
	.align 4
	.global divmod_pow2__mod_pow2_i64_37
	.hidden	divmod_pow2__mod_pow2_i64_37
	.type	divmod_pow2__mod_pow2_i64_37, #function
	.proc	05
divmod_pow2__mod_pow2_i64_37:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	31, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_37, .-divmod_pow2__mod_pow2_i64_37
	.align 4
	.global divmod_pow2__div_pow2_i64_38
	.hidden	divmod_pow2__div_pow2_i64_38
	.type	divmod_pow2__div_pow2_i64_38, #function
	.proc	05
divmod_pow2__div_pow2_i64_38:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 38, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_38, .-divmod_pow2__div_pow2_i64_38
	.align 4
	.global divmod_pow2__mod_pow2_i64_38
	.hidden	divmod_pow2__mod_pow2_i64_38
	.type	divmod_pow2__mod_pow2_i64_38, #function
	.proc	05
divmod_pow2__mod_pow2_i64_38:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	63, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_38, .-divmod_pow2__mod_pow2_i64_38
	.align 4
	.global divmod_pow2__div_pow2_i64_39
	.hidden	divmod_pow2__div_pow2_i64_39
	.type	divmod_pow2__div_pow2_i64_39, #function
	.proc	05
divmod_pow2__div_pow2_i64_39:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 39, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_39, .-divmod_pow2__div_pow2_i64_39
	.align 4
	.global divmod_pow2__mod_pow2_i64_39
	.hidden	divmod_pow2__mod_pow2_i64_39
	.type	divmod_pow2__mod_pow2_i64_39, #function
	.proc	05
divmod_pow2__mod_pow2_i64_39:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	127, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_39, .-divmod_pow2__mod_pow2_i64_39
	.align 4
	.global divmod_pow2__div_pow2_i64_40
	.hidden	divmod_pow2__div_pow2_i64_40
	.type	divmod_pow2__div_pow2_i64_40, #function
	.proc	05
divmod_pow2__div_pow2_i64_40:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 40, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_40, .-divmod_pow2__div_pow2_i64_40
	.align 4
	.global divmod_pow2__mod_pow2_i64_40
	.hidden	divmod_pow2__mod_pow2_i64_40
	.type	divmod_pow2__mod_pow2_i64_40, #function
	.proc	05
divmod_pow2__mod_pow2_i64_40:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	255, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_40, .-divmod_pow2__mod_pow2_i64_40
	.align 4
	.global divmod_pow2__div_pow2_i64_41
	.hidden	divmod_pow2__div_pow2_i64_41
	.type	divmod_pow2__div_pow2_i64_41, #function
	.proc	05
divmod_pow2__div_pow2_i64_41:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 41, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_41, .-divmod_pow2__div_pow2_i64_41
	.align 4
	.global divmod_pow2__mod_pow2_i64_41
	.hidden	divmod_pow2__mod_pow2_i64_41
	.type	divmod_pow2__mod_pow2_i64_41, #function
	.proc	05
divmod_pow2__mod_pow2_i64_41:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	511, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_41, .-divmod_pow2__mod_pow2_i64_41
	.align 4
	.global divmod_pow2__div_pow2_i64_42
	.hidden	divmod_pow2__div_pow2_i64_42
	.type	divmod_pow2__div_pow2_i64_42, #function
	.proc	05
divmod_pow2__div_pow2_i64_42:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 42, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_42, .-divmod_pow2__div_pow2_i64_42
	.align 4
	.global divmod_pow2__mod_pow2_i64_42
	.hidden	divmod_pow2__mod_pow2_i64_42
	.type	divmod_pow2__mod_pow2_i64_42, #function
	.proc	05
divmod_pow2__mod_pow2_i64_42:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	1023, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_42, .-divmod_pow2__mod_pow2_i64_42
	.align 4
	.global divmod_pow2__div_pow2_i64_43
	.hidden	divmod_pow2__div_pow2_i64_43
	.type	divmod_pow2__div_pow2_i64_43, #function
	.proc	05
divmod_pow2__div_pow2_i64_43:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 43, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_43, .-divmod_pow2__div_pow2_i64_43
	.align 4
	.global divmod_pow2__mod_pow2_i64_43
	.hidden	divmod_pow2__mod_pow2_i64_43
	.type	divmod_pow2__mod_pow2_i64_43, #function
	.proc	05
divmod_pow2__mod_pow2_i64_43:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	2047, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_43, .-divmod_pow2__mod_pow2_i64_43
	.align 4
	.global divmod_pow2__div_pow2_i64_44
	.hidden	divmod_pow2__div_pow2_i64_44
	.type	divmod_pow2__div_pow2_i64_44, #function
	.proc	05
divmod_pow2__div_pow2_i64_44:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 44, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_44, .-divmod_pow2__div_pow2_i64_44
	.align 4
	.global divmod_pow2__mod_pow2_i64_44
	.hidden	divmod_pow2__mod_pow2_i64_44
	.type	divmod_pow2__mod_pow2_i64_44, #function
	.proc	05
divmod_pow2__mod_pow2_i64_44:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	4095, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_44, .-divmod_pow2__mod_pow2_i64_44
	.align 4
	.global divmod_pow2__div_pow2_i64_45
	.hidden	divmod_pow2__div_pow2_i64_45
	.type	divmod_pow2__div_pow2_i64_45, #function
	.proc	05
divmod_pow2__div_pow2_i64_45:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 45, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_45, .-divmod_pow2__div_pow2_i64_45
	.align 4
	.global divmod_pow2__mod_pow2_i64_45
	.hidden	divmod_pow2__mod_pow2_i64_45
	.type	divmod_pow2__mod_pow2_i64_45, #function
	.proc	05
divmod_pow2__mod_pow2_i64_45:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(7168), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_45, .-divmod_pow2__mod_pow2_i64_45
	.align 4
	.global divmod_pow2__div_pow2_i64_46
	.hidden	divmod_pow2__div_pow2_i64_46
	.type	divmod_pow2__div_pow2_i64_46, #function
	.proc	05
divmod_pow2__div_pow2_i64_46:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 46, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_46, .-divmod_pow2__div_pow2_i64_46
	.align 4
	.global divmod_pow2__mod_pow2_i64_46
	.hidden	divmod_pow2__mod_pow2_i64_46
	.type	divmod_pow2__mod_pow2_i64_46, #function
	.proc	05
divmod_pow2__mod_pow2_i64_46:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(15360), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_46, .-divmod_pow2__mod_pow2_i64_46
	.align 4
	.global divmod_pow2__div_pow2_i64_47
	.hidden	divmod_pow2__div_pow2_i64_47
	.type	divmod_pow2__div_pow2_i64_47, #function
	.proc	05
divmod_pow2__div_pow2_i64_47:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 47, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_47, .-divmod_pow2__div_pow2_i64_47
	.align 4
	.global divmod_pow2__mod_pow2_i64_47
	.hidden	divmod_pow2__mod_pow2_i64_47
	.type	divmod_pow2__mod_pow2_i64_47, #function
	.proc	05
divmod_pow2__mod_pow2_i64_47:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(31744), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_47, .-divmod_pow2__mod_pow2_i64_47
	.align 4
	.global divmod_pow2__div_pow2_i64_48
	.hidden	divmod_pow2__div_pow2_i64_48
	.type	divmod_pow2__div_pow2_i64_48, #function
	.proc	05
divmod_pow2__div_pow2_i64_48:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 48, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_48, .-divmod_pow2__div_pow2_i64_48
	.align 4
	.global divmod_pow2__mod_pow2_i64_48
	.hidden	divmod_pow2__mod_pow2_i64_48
	.type	divmod_pow2__mod_pow2_i64_48, #function
	.proc	05
divmod_pow2__mod_pow2_i64_48:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(64512), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_48, .-divmod_pow2__mod_pow2_i64_48
	.align 4
	.global divmod_pow2__div_pow2_i64_49
	.hidden	divmod_pow2__div_pow2_i64_49
	.type	divmod_pow2__div_pow2_i64_49, #function
	.proc	05
divmod_pow2__div_pow2_i64_49:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 49, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_49, .-divmod_pow2__div_pow2_i64_49
	.align 4
	.global divmod_pow2__mod_pow2_i64_49
	.hidden	divmod_pow2__mod_pow2_i64_49
	.type	divmod_pow2__mod_pow2_i64_49, #function
	.proc	05
divmod_pow2__mod_pow2_i64_49:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(130048), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_49, .-divmod_pow2__mod_pow2_i64_49
	.align 4
	.global divmod_pow2__div_pow2_i64_50
	.hidden	divmod_pow2__div_pow2_i64_50
	.type	divmod_pow2__div_pow2_i64_50, #function
	.proc	05
divmod_pow2__div_pow2_i64_50:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 50, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_50, .-divmod_pow2__div_pow2_i64_50
	.align 4
	.global divmod_pow2__mod_pow2_i64_50
	.hidden	divmod_pow2__mod_pow2_i64_50
	.type	divmod_pow2__mod_pow2_i64_50, #function
	.proc	05
divmod_pow2__mod_pow2_i64_50:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(261120), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_50, .-divmod_pow2__mod_pow2_i64_50
	.align 4
	.global divmod_pow2__div_pow2_i64_51
	.hidden	divmod_pow2__div_pow2_i64_51
	.type	divmod_pow2__div_pow2_i64_51, #function
	.proc	05
divmod_pow2__div_pow2_i64_51:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 51, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_51, .-divmod_pow2__div_pow2_i64_51
	.align 4
	.global divmod_pow2__mod_pow2_i64_51
	.hidden	divmod_pow2__mod_pow2_i64_51
	.type	divmod_pow2__mod_pow2_i64_51, #function
	.proc	05
divmod_pow2__mod_pow2_i64_51:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(523264), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_51, .-divmod_pow2__mod_pow2_i64_51
	.align 4
	.global divmod_pow2__div_pow2_i64_52
	.hidden	divmod_pow2__div_pow2_i64_52
	.type	divmod_pow2__div_pow2_i64_52, #function
	.proc	05
divmod_pow2__div_pow2_i64_52:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 52, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_52, .-divmod_pow2__div_pow2_i64_52
	.align 4
	.global divmod_pow2__mod_pow2_i64_52
	.hidden	divmod_pow2__mod_pow2_i64_52
	.type	divmod_pow2__mod_pow2_i64_52, #function
	.proc	05
divmod_pow2__mod_pow2_i64_52:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(1047552), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_52, .-divmod_pow2__mod_pow2_i64_52
	.align 4
	.global divmod_pow2__div_pow2_i64_53
	.hidden	divmod_pow2__div_pow2_i64_53
	.type	divmod_pow2__div_pow2_i64_53, #function
	.proc	05
divmod_pow2__div_pow2_i64_53:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 53, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_53, .-divmod_pow2__div_pow2_i64_53
	.align 4
	.global divmod_pow2__mod_pow2_i64_53
	.hidden	divmod_pow2__mod_pow2_i64_53
	.type	divmod_pow2__mod_pow2_i64_53, #function
	.proc	05
divmod_pow2__mod_pow2_i64_53:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(2096128), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_53, .-divmod_pow2__mod_pow2_i64_53
	.align 4
	.global divmod_pow2__div_pow2_i64_54
	.hidden	divmod_pow2__div_pow2_i64_54
	.type	divmod_pow2__div_pow2_i64_54, #function
	.proc	05
divmod_pow2__div_pow2_i64_54:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 54, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_54, .-divmod_pow2__div_pow2_i64_54
	.align 4
	.global divmod_pow2__mod_pow2_i64_54
	.hidden	divmod_pow2__mod_pow2_i64_54
	.type	divmod_pow2__mod_pow2_i64_54, #function
	.proc	05
divmod_pow2__mod_pow2_i64_54:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(4193280), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_54, .-divmod_pow2__mod_pow2_i64_54
	.align 4
	.global divmod_pow2__div_pow2_i64_55
	.hidden	divmod_pow2__div_pow2_i64_55
	.type	divmod_pow2__div_pow2_i64_55, #function
	.proc	05
divmod_pow2__div_pow2_i64_55:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 55, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_55, .-divmod_pow2__div_pow2_i64_55
	.align 4
	.global divmod_pow2__mod_pow2_i64_55
	.hidden	divmod_pow2__mod_pow2_i64_55
	.type	divmod_pow2__mod_pow2_i64_55, #function
	.proc	05
divmod_pow2__mod_pow2_i64_55:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(8387584), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_55, .-divmod_pow2__mod_pow2_i64_55
	.align 4
	.global divmod_pow2__div_pow2_i64_56
	.hidden	divmod_pow2__div_pow2_i64_56
	.type	divmod_pow2__div_pow2_i64_56, #function
	.proc	05
divmod_pow2__div_pow2_i64_56:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 56, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_56, .-divmod_pow2__div_pow2_i64_56
	.align 4
	.global divmod_pow2__mod_pow2_i64_56
	.hidden	divmod_pow2__mod_pow2_i64_56
	.type	divmod_pow2__mod_pow2_i64_56, #function
	.proc	05
divmod_pow2__mod_pow2_i64_56:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(16776192), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_56, .-divmod_pow2__mod_pow2_i64_56
	.align 4
	.global divmod_pow2__div_pow2_i64_57
	.hidden	divmod_pow2__div_pow2_i64_57
	.type	divmod_pow2__div_pow2_i64_57, #function
	.proc	05
divmod_pow2__div_pow2_i64_57:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 57, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_57, .-divmod_pow2__div_pow2_i64_57
	.align 4
	.global divmod_pow2__mod_pow2_i64_57
	.hidden	divmod_pow2__mod_pow2_i64_57
	.type	divmod_pow2__mod_pow2_i64_57, #function
	.proc	05
divmod_pow2__mod_pow2_i64_57:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(33553408), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_57, .-divmod_pow2__mod_pow2_i64_57
	.align 4
	.global divmod_pow2__div_pow2_i64_58
	.hidden	divmod_pow2__div_pow2_i64_58
	.type	divmod_pow2__div_pow2_i64_58, #function
	.proc	05
divmod_pow2__div_pow2_i64_58:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 58, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_58, .-divmod_pow2__div_pow2_i64_58
	.align 4
	.global divmod_pow2__mod_pow2_i64_58
	.hidden	divmod_pow2__mod_pow2_i64_58
	.type	divmod_pow2__mod_pow2_i64_58, #function
	.proc	05
divmod_pow2__mod_pow2_i64_58:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(67107840), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_58, .-divmod_pow2__mod_pow2_i64_58
	.align 4
	.global divmod_pow2__div_pow2_i64_59
	.hidden	divmod_pow2__div_pow2_i64_59
	.type	divmod_pow2__div_pow2_i64_59, #function
	.proc	05
divmod_pow2__div_pow2_i64_59:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 59, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_59, .-divmod_pow2__div_pow2_i64_59
	.align 4
	.global divmod_pow2__mod_pow2_i64_59
	.hidden	divmod_pow2__mod_pow2_i64_59
	.type	divmod_pow2__mod_pow2_i64_59, #function
	.proc	05
divmod_pow2__mod_pow2_i64_59:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(134216704), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_59, .-divmod_pow2__mod_pow2_i64_59
	.align 4
	.global divmod_pow2__div_pow2_i64_60
	.hidden	divmod_pow2__div_pow2_i64_60
	.type	divmod_pow2__div_pow2_i64_60, #function
	.proc	05
divmod_pow2__div_pow2_i64_60:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 60, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_60, .-divmod_pow2__div_pow2_i64_60
	.align 4
	.global divmod_pow2__mod_pow2_i64_60
	.hidden	divmod_pow2__mod_pow2_i64_60
	.type	divmod_pow2__mod_pow2_i64_60, #function
	.proc	05
divmod_pow2__mod_pow2_i64_60:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(268434432), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_60, .-divmod_pow2__mod_pow2_i64_60
	.align 4
	.global divmod_pow2__div_pow2_i64_61
	.hidden	divmod_pow2__div_pow2_i64_61
	.type	divmod_pow2__div_pow2_i64_61, #function
	.proc	05
divmod_pow2__div_pow2_i64_61:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 61, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_61, .-divmod_pow2__div_pow2_i64_61
	.align 4
	.global divmod_pow2__mod_pow2_i64_61
	.hidden	divmod_pow2__mod_pow2_i64_61
	.type	divmod_pow2__mod_pow2_i64_61, #function
	.proc	05
divmod_pow2__mod_pow2_i64_61:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(536869888), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_61, .-divmod_pow2__mod_pow2_i64_61
	.align 4
	.global divmod_pow2__div_pow2_i64_62
	.hidden	divmod_pow2__div_pow2_i64_62
	.type	divmod_pow2__div_pow2_i64_62, #function
	.proc	05
divmod_pow2__div_pow2_i64_62:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 62, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_62, .-divmod_pow2__div_pow2_i64_62
	.align 4
	.global divmod_pow2__mod_pow2_i64_62
	.hidden	divmod_pow2__mod_pow2_i64_62
	.type	divmod_pow2__mod_pow2_i64_62, #function
	.proc	05
divmod_pow2__mod_pow2_i64_62:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(1073740800), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_62, .-divmod_pow2__mod_pow2_i64_62
	.align 4
	.global divmod_pow2__div_pow2_i64_63
	.hidden	divmod_pow2__div_pow2_i64_63
	.type	divmod_pow2__div_pow2_i64_63, #function
	.proc	05
divmod_pow2__div_pow2_i64_63:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sethi	%hi(2147483648), %o0
	mov	0, %o1
	mov	%g2, %o2
	mov	%g3, %o3
	call	m3_divL, 0
	 nop
	mov	%o0, %g2
	mov	%o1, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_i64_63, .-divmod_pow2__div_pow2_i64_63
	.align 4
	.global divmod_pow2__mod_pow2_i64_63
	.hidden	divmod_pow2__mod_pow2_i64_63
	.type	divmod_pow2__mod_pow2_i64_63, #function
	.proc	05
divmod_pow2__mod_pow2_i64_63:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sethi	%hi(2147483648), %o0
	mov	0, %o1
	mov	%g2, %o2
	mov	%g3, %o3
	call	m3_modL, 0
	 nop
	mov	%o0, %g2
	mov	%o1, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_i64_63, .-divmod_pow2__mod_pow2_i64_63
	.align 4
	.global divmod_pow2__div_pow2_u64_0
	.hidden	divmod_pow2__div_pow2_u64_0
	.type	divmod_pow2__div_pow2_u64_0, #function
	.proc	05
divmod_pow2__div_pow2_u64_0:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_0, .-divmod_pow2__div_pow2_u64_0
	.align 4
	.global divmod_pow2__mod_pow2_u64_0
	.hidden	divmod_pow2__mod_pow2_u64_0
	.type	divmod_pow2__mod_pow2_u64_0, #function
	.proc	05
divmod_pow2__mod_pow2_u64_0:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	mov	0, %g2
	mov	0, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_0, .-divmod_pow2__mod_pow2_u64_0
	.align 4
	.global divmod_pow2__div_pow2_u64_1
	.hidden	divmod_pow2__div_pow2_u64_1
	.type	divmod_pow2__div_pow2_u64_1, #function
	.proc	05
divmod_pow2__div_pow2_u64_1:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 1, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_1, .-divmod_pow2__div_pow2_u64_1
	.align 4
	.global divmod_pow2__mod_pow2_u64_1
	.hidden	divmod_pow2__mod_pow2_u64_1
	.type	divmod_pow2__mod_pow2_u64_1, #function
	.proc	05
divmod_pow2__mod_pow2_u64_1:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 1, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_1, .-divmod_pow2__mod_pow2_u64_1
	.align 4
	.global divmod_pow2__div_pow2_u64_2
	.hidden	divmod_pow2__div_pow2_u64_2
	.type	divmod_pow2__div_pow2_u64_2, #function
	.proc	05
divmod_pow2__div_pow2_u64_2:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 2, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_2, .-divmod_pow2__div_pow2_u64_2
	.align 4
	.global divmod_pow2__mod_pow2_u64_2
	.hidden	divmod_pow2__mod_pow2_u64_2
	.type	divmod_pow2__mod_pow2_u64_2, #function
	.proc	05
divmod_pow2__mod_pow2_u64_2:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_2, .-divmod_pow2__mod_pow2_u64_2
	.align 4
	.global divmod_pow2__div_pow2_u64_3
	.hidden	divmod_pow2__div_pow2_u64_3
	.type	divmod_pow2__div_pow2_u64_3, #function
	.proc	05
divmod_pow2__div_pow2_u64_3:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 3, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_3, .-divmod_pow2__div_pow2_u64_3
	.align 4
	.global divmod_pow2__mod_pow2_u64_3
	.hidden	divmod_pow2__mod_pow2_u64_3
	.type	divmod_pow2__mod_pow2_u64_3, #function
	.proc	05
divmod_pow2__mod_pow2_u64_3:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 7, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_3, .-divmod_pow2__mod_pow2_u64_3
	.align 4
	.global divmod_pow2__div_pow2_u64_4
	.hidden	divmod_pow2__div_pow2_u64_4
	.type	divmod_pow2__div_pow2_u64_4, #function
	.proc	05
divmod_pow2__div_pow2_u64_4:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 4, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_4, .-divmod_pow2__div_pow2_u64_4
	.align 4
	.global divmod_pow2__mod_pow2_u64_4
	.hidden	divmod_pow2__mod_pow2_u64_4
	.type	divmod_pow2__mod_pow2_u64_4, #function
	.proc	05
divmod_pow2__mod_pow2_u64_4:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 15, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_4, .-divmod_pow2__mod_pow2_u64_4
	.align 4
	.global divmod_pow2__div_pow2_u64_5
	.hidden	divmod_pow2__div_pow2_u64_5
	.type	divmod_pow2__div_pow2_u64_5, #function
	.proc	05
divmod_pow2__div_pow2_u64_5:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 5, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_5, .-divmod_pow2__div_pow2_u64_5
	.align 4
	.global divmod_pow2__mod_pow2_u64_5
	.hidden	divmod_pow2__mod_pow2_u64_5
	.type	divmod_pow2__mod_pow2_u64_5, #function
	.proc	05
divmod_pow2__mod_pow2_u64_5:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 31, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_5, .-divmod_pow2__mod_pow2_u64_5
	.align 4
	.global divmod_pow2__div_pow2_u64_6
	.hidden	divmod_pow2__div_pow2_u64_6
	.type	divmod_pow2__div_pow2_u64_6, #function
	.proc	05
divmod_pow2__div_pow2_u64_6:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 6, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_6, .-divmod_pow2__div_pow2_u64_6
	.align 4
	.global divmod_pow2__mod_pow2_u64_6
	.hidden	divmod_pow2__mod_pow2_u64_6
	.type	divmod_pow2__mod_pow2_u64_6, #function
	.proc	05
divmod_pow2__mod_pow2_u64_6:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 63, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_6, .-divmod_pow2__mod_pow2_u64_6
	.align 4
	.global divmod_pow2__div_pow2_u64_7
	.hidden	divmod_pow2__div_pow2_u64_7
	.type	divmod_pow2__div_pow2_u64_7, #function
	.proc	05
divmod_pow2__div_pow2_u64_7:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 7, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_7, .-divmod_pow2__div_pow2_u64_7
	.align 4
	.global divmod_pow2__mod_pow2_u64_7
	.hidden	divmod_pow2__mod_pow2_u64_7
	.type	divmod_pow2__mod_pow2_u64_7, #function
	.proc	05
divmod_pow2__mod_pow2_u64_7:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 127, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_7, .-divmod_pow2__mod_pow2_u64_7
	.align 4
	.global divmod_pow2__div_pow2_u64_8
	.hidden	divmod_pow2__div_pow2_u64_8
	.type	divmod_pow2__div_pow2_u64_8, #function
	.proc	05
divmod_pow2__div_pow2_u64_8:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 8, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_8, .-divmod_pow2__div_pow2_u64_8
	.align 4
	.global divmod_pow2__mod_pow2_u64_8
	.hidden	divmod_pow2__mod_pow2_u64_8
	.type	divmod_pow2__mod_pow2_u64_8, #function
	.proc	05
divmod_pow2__mod_pow2_u64_8:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 255, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_8, .-divmod_pow2__mod_pow2_u64_8
	.align 4
	.global divmod_pow2__div_pow2_u64_9
	.hidden	divmod_pow2__div_pow2_u64_9
	.type	divmod_pow2__div_pow2_u64_9, #function
	.proc	05
divmod_pow2__div_pow2_u64_9:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 9, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_9, .-divmod_pow2__div_pow2_u64_9
	.align 4
	.global divmod_pow2__mod_pow2_u64_9
	.hidden	divmod_pow2__mod_pow2_u64_9
	.type	divmod_pow2__mod_pow2_u64_9, #function
	.proc	05
divmod_pow2__mod_pow2_u64_9:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 511, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_9, .-divmod_pow2__mod_pow2_u64_9
	.align 4
	.global divmod_pow2__div_pow2_u64_10
	.hidden	divmod_pow2__div_pow2_u64_10
	.type	divmod_pow2__div_pow2_u64_10, #function
	.proc	05
divmod_pow2__div_pow2_u64_10:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 10, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_10, .-divmod_pow2__div_pow2_u64_10
	.align 4
	.global divmod_pow2__mod_pow2_u64_10
	.hidden	divmod_pow2__mod_pow2_u64_10
	.type	divmod_pow2__mod_pow2_u64_10, #function
	.proc	05
divmod_pow2__mod_pow2_u64_10:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 1023, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_10, .-divmod_pow2__mod_pow2_u64_10
	.align 4
	.global divmod_pow2__div_pow2_u64_11
	.hidden	divmod_pow2__div_pow2_u64_11
	.type	divmod_pow2__div_pow2_u64_11, #function
	.proc	05
divmod_pow2__div_pow2_u64_11:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 11, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_11, .-divmod_pow2__div_pow2_u64_11
	.align 4
	.global divmod_pow2__mod_pow2_u64_11
	.hidden	divmod_pow2__mod_pow2_u64_11
	.type	divmod_pow2__mod_pow2_u64_11, #function
	.proc	05
divmod_pow2__mod_pow2_u64_11:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 2047, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_11, .-divmod_pow2__mod_pow2_u64_11
	.align 4
	.global divmod_pow2__div_pow2_u64_12
	.hidden	divmod_pow2__div_pow2_u64_12
	.type	divmod_pow2__div_pow2_u64_12, #function
	.proc	05
divmod_pow2__div_pow2_u64_12:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 12, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_12, .-divmod_pow2__div_pow2_u64_12
	.align 4
	.global divmod_pow2__mod_pow2_u64_12
	.hidden	divmod_pow2__mod_pow2_u64_12
	.type	divmod_pow2__mod_pow2_u64_12, #function
	.proc	05
divmod_pow2__mod_pow2_u64_12:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 4095, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_12, .-divmod_pow2__mod_pow2_u64_12
	.align 4
	.global divmod_pow2__div_pow2_u64_13
	.hidden	divmod_pow2__div_pow2_u64_13
	.type	divmod_pow2__div_pow2_u64_13, #function
	.proc	05
divmod_pow2__div_pow2_u64_13:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 13, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_13, .-divmod_pow2__div_pow2_u64_13
	.align 4
	.global divmod_pow2__mod_pow2_u64_13
	.hidden	divmod_pow2__mod_pow2_u64_13
	.type	divmod_pow2__mod_pow2_u64_13, #function
	.proc	05
divmod_pow2__mod_pow2_u64_13:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(7168), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_13, .-divmod_pow2__mod_pow2_u64_13
	.align 4
	.global divmod_pow2__div_pow2_u64_14
	.hidden	divmod_pow2__div_pow2_u64_14
	.type	divmod_pow2__div_pow2_u64_14, #function
	.proc	05
divmod_pow2__div_pow2_u64_14:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 14, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_14, .-divmod_pow2__div_pow2_u64_14
	.align 4
	.global divmod_pow2__mod_pow2_u64_14
	.hidden	divmod_pow2__mod_pow2_u64_14
	.type	divmod_pow2__mod_pow2_u64_14, #function
	.proc	05
divmod_pow2__mod_pow2_u64_14:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(15360), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_14, .-divmod_pow2__mod_pow2_u64_14
	.align 4
	.global divmod_pow2__div_pow2_u64_15
	.hidden	divmod_pow2__div_pow2_u64_15
	.type	divmod_pow2__div_pow2_u64_15, #function
	.proc	05
divmod_pow2__div_pow2_u64_15:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 15, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_15, .-divmod_pow2__div_pow2_u64_15
	.align 4
	.global divmod_pow2__mod_pow2_u64_15
	.hidden	divmod_pow2__mod_pow2_u64_15
	.type	divmod_pow2__mod_pow2_u64_15, #function
	.proc	05
divmod_pow2__mod_pow2_u64_15:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(31744), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_15, .-divmod_pow2__mod_pow2_u64_15
	.align 4
	.global divmod_pow2__div_pow2_u64_16
	.hidden	divmod_pow2__div_pow2_u64_16
	.type	divmod_pow2__div_pow2_u64_16, #function
	.proc	05
divmod_pow2__div_pow2_u64_16:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 16, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_16, .-divmod_pow2__div_pow2_u64_16
	.align 4
	.global divmod_pow2__mod_pow2_u64_16
	.hidden	divmod_pow2__mod_pow2_u64_16
	.type	divmod_pow2__mod_pow2_u64_16, #function
	.proc	05
divmod_pow2__mod_pow2_u64_16:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(64512), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_16, .-divmod_pow2__mod_pow2_u64_16
	.align 4
	.global divmod_pow2__div_pow2_u64_17
	.hidden	divmod_pow2__div_pow2_u64_17
	.type	divmod_pow2__div_pow2_u64_17, #function
	.proc	05
divmod_pow2__div_pow2_u64_17:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 17, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_17, .-divmod_pow2__div_pow2_u64_17
	.align 4
	.global divmod_pow2__mod_pow2_u64_17
	.hidden	divmod_pow2__mod_pow2_u64_17
	.type	divmod_pow2__mod_pow2_u64_17, #function
	.proc	05
divmod_pow2__mod_pow2_u64_17:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(130048), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_17, .-divmod_pow2__mod_pow2_u64_17
	.align 4
	.global divmod_pow2__div_pow2_u64_18
	.hidden	divmod_pow2__div_pow2_u64_18
	.type	divmod_pow2__div_pow2_u64_18, #function
	.proc	05
divmod_pow2__div_pow2_u64_18:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 18, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_18, .-divmod_pow2__div_pow2_u64_18
	.align 4
	.global divmod_pow2__mod_pow2_u64_18
	.hidden	divmod_pow2__mod_pow2_u64_18
	.type	divmod_pow2__mod_pow2_u64_18, #function
	.proc	05
divmod_pow2__mod_pow2_u64_18:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(261120), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_18, .-divmod_pow2__mod_pow2_u64_18
	.align 4
	.global divmod_pow2__div_pow2_u64_19
	.hidden	divmod_pow2__div_pow2_u64_19
	.type	divmod_pow2__div_pow2_u64_19, #function
	.proc	05
divmod_pow2__div_pow2_u64_19:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 19, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_19, .-divmod_pow2__div_pow2_u64_19
	.align 4
	.global divmod_pow2__mod_pow2_u64_19
	.hidden	divmod_pow2__mod_pow2_u64_19
	.type	divmod_pow2__mod_pow2_u64_19, #function
	.proc	05
divmod_pow2__mod_pow2_u64_19:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(523264), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_19, .-divmod_pow2__mod_pow2_u64_19
	.align 4
	.global divmod_pow2__div_pow2_u64_20
	.hidden	divmod_pow2__div_pow2_u64_20
	.type	divmod_pow2__div_pow2_u64_20, #function
	.proc	05
divmod_pow2__div_pow2_u64_20:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 20, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_20, .-divmod_pow2__div_pow2_u64_20
	.align 4
	.global divmod_pow2__mod_pow2_u64_20
	.hidden	divmod_pow2__mod_pow2_u64_20
	.type	divmod_pow2__mod_pow2_u64_20, #function
	.proc	05
divmod_pow2__mod_pow2_u64_20:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(1047552), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_20, .-divmod_pow2__mod_pow2_u64_20
	.align 4
	.global divmod_pow2__div_pow2_u64_21
	.hidden	divmod_pow2__div_pow2_u64_21
	.type	divmod_pow2__div_pow2_u64_21, #function
	.proc	05
divmod_pow2__div_pow2_u64_21:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 21, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_21, .-divmod_pow2__div_pow2_u64_21
	.align 4
	.global divmod_pow2__mod_pow2_u64_21
	.hidden	divmod_pow2__mod_pow2_u64_21
	.type	divmod_pow2__mod_pow2_u64_21, #function
	.proc	05
divmod_pow2__mod_pow2_u64_21:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(2096128), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_21, .-divmod_pow2__mod_pow2_u64_21
	.align 4
	.global divmod_pow2__div_pow2_u64_22
	.hidden	divmod_pow2__div_pow2_u64_22
	.type	divmod_pow2__div_pow2_u64_22, #function
	.proc	05
divmod_pow2__div_pow2_u64_22:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 22, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_22, .-divmod_pow2__div_pow2_u64_22
	.align 4
	.global divmod_pow2__mod_pow2_u64_22
	.hidden	divmod_pow2__mod_pow2_u64_22
	.type	divmod_pow2__mod_pow2_u64_22, #function
	.proc	05
divmod_pow2__mod_pow2_u64_22:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(4193280), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_22, .-divmod_pow2__mod_pow2_u64_22
	.align 4
	.global divmod_pow2__div_pow2_u64_23
	.hidden	divmod_pow2__div_pow2_u64_23
	.type	divmod_pow2__div_pow2_u64_23, #function
	.proc	05
divmod_pow2__div_pow2_u64_23:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 23, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_23, .-divmod_pow2__div_pow2_u64_23
	.align 4
	.global divmod_pow2__mod_pow2_u64_23
	.hidden	divmod_pow2__mod_pow2_u64_23
	.type	divmod_pow2__mod_pow2_u64_23, #function
	.proc	05
divmod_pow2__mod_pow2_u64_23:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(8387584), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_23, .-divmod_pow2__mod_pow2_u64_23
	.align 4
	.global divmod_pow2__div_pow2_u64_24
	.hidden	divmod_pow2__div_pow2_u64_24
	.type	divmod_pow2__div_pow2_u64_24, #function
	.proc	05
divmod_pow2__div_pow2_u64_24:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 24, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_24, .-divmod_pow2__div_pow2_u64_24
	.align 4
	.global divmod_pow2__mod_pow2_u64_24
	.hidden	divmod_pow2__mod_pow2_u64_24
	.type	divmod_pow2__mod_pow2_u64_24, #function
	.proc	05
divmod_pow2__mod_pow2_u64_24:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(16776192), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_24, .-divmod_pow2__mod_pow2_u64_24
	.align 4
	.global divmod_pow2__div_pow2_u64_25
	.hidden	divmod_pow2__div_pow2_u64_25
	.type	divmod_pow2__div_pow2_u64_25, #function
	.proc	05
divmod_pow2__div_pow2_u64_25:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 25, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_25, .-divmod_pow2__div_pow2_u64_25
	.align 4
	.global divmod_pow2__mod_pow2_u64_25
	.hidden	divmod_pow2__mod_pow2_u64_25
	.type	divmod_pow2__mod_pow2_u64_25, #function
	.proc	05
divmod_pow2__mod_pow2_u64_25:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(33553408), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_25, .-divmod_pow2__mod_pow2_u64_25
	.align 4
	.global divmod_pow2__div_pow2_u64_26
	.hidden	divmod_pow2__div_pow2_u64_26
	.type	divmod_pow2__div_pow2_u64_26, #function
	.proc	05
divmod_pow2__div_pow2_u64_26:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 26, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_26, .-divmod_pow2__div_pow2_u64_26
	.align 4
	.global divmod_pow2__mod_pow2_u64_26
	.hidden	divmod_pow2__mod_pow2_u64_26
	.type	divmod_pow2__mod_pow2_u64_26, #function
	.proc	05
divmod_pow2__mod_pow2_u64_26:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(67107840), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_26, .-divmod_pow2__mod_pow2_u64_26
	.align 4
	.global divmod_pow2__div_pow2_u64_27
	.hidden	divmod_pow2__div_pow2_u64_27
	.type	divmod_pow2__div_pow2_u64_27, #function
	.proc	05
divmod_pow2__div_pow2_u64_27:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 27, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_27, .-divmod_pow2__div_pow2_u64_27
	.align 4
	.global divmod_pow2__mod_pow2_u64_27
	.hidden	divmod_pow2__mod_pow2_u64_27
	.type	divmod_pow2__mod_pow2_u64_27, #function
	.proc	05
divmod_pow2__mod_pow2_u64_27:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(134216704), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_27, .-divmod_pow2__mod_pow2_u64_27
	.align 4
	.global divmod_pow2__div_pow2_u64_28
	.hidden	divmod_pow2__div_pow2_u64_28
	.type	divmod_pow2__div_pow2_u64_28, #function
	.proc	05
divmod_pow2__div_pow2_u64_28:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 28, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_28, .-divmod_pow2__div_pow2_u64_28
	.align 4
	.global divmod_pow2__mod_pow2_u64_28
	.hidden	divmod_pow2__mod_pow2_u64_28
	.type	divmod_pow2__mod_pow2_u64_28, #function
	.proc	05
divmod_pow2__mod_pow2_u64_28:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(268434432), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_28, .-divmod_pow2__mod_pow2_u64_28
	.align 4
	.global divmod_pow2__div_pow2_u64_29
	.hidden	divmod_pow2__div_pow2_u64_29
	.type	divmod_pow2__div_pow2_u64_29, #function
	.proc	05
divmod_pow2__div_pow2_u64_29:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 29, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_29, .-divmod_pow2__div_pow2_u64_29
	.align 4
	.global divmod_pow2__mod_pow2_u64_29
	.hidden	divmod_pow2__mod_pow2_u64_29
	.type	divmod_pow2__mod_pow2_u64_29, #function
	.proc	05
divmod_pow2__mod_pow2_u64_29:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(536869888), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_29, .-divmod_pow2__mod_pow2_u64_29
	.align 4
	.global divmod_pow2__div_pow2_u64_30
	.hidden	divmod_pow2__div_pow2_u64_30
	.type	divmod_pow2__div_pow2_u64_30, #function
	.proc	05
divmod_pow2__div_pow2_u64_30:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 30, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_30, .-divmod_pow2__div_pow2_u64_30
	.align 4
	.global divmod_pow2__mod_pow2_u64_30
	.hidden	divmod_pow2__mod_pow2_u64_30
	.type	divmod_pow2__mod_pow2_u64_30, #function
	.proc	05
divmod_pow2__mod_pow2_u64_30:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(1073740800), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_30, .-divmod_pow2__mod_pow2_u64_30
	.align 4
	.global divmod_pow2__div_pow2_u64_31
	.hidden	divmod_pow2__div_pow2_u64_31
	.type	divmod_pow2__div_pow2_u64_31, #function
	.proc	05
divmod_pow2__div_pow2_u64_31:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 31, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_31, .-divmod_pow2__div_pow2_u64_31
	.align 4
	.global divmod_pow2__mod_pow2_u64_31
	.hidden	divmod_pow2__mod_pow2_u64_31
	.type	divmod_pow2__mod_pow2_u64_31, #function
	.proc	05
divmod_pow2__mod_pow2_u64_31:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(2147482624), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_31, .-divmod_pow2__mod_pow2_u64_31
	.align 4
	.global divmod_pow2__div_pow2_u64_32
	.hidden	divmod_pow2__div_pow2_u64_32
	.type	divmod_pow2__div_pow2_u64_32, #function
	.proc	05
divmod_pow2__div_pow2_u64_32:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 32, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_32, .-divmod_pow2__div_pow2_u64_32
	.align 4
	.global divmod_pow2__mod_pow2_u64_32
	.hidden	divmod_pow2__mod_pow2_u64_32
	.type	divmod_pow2__mod_pow2_u64_32, #function
	.proc	05
divmod_pow2__mod_pow2_u64_32:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	0, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_32, .-divmod_pow2__mod_pow2_u64_32
	.align 4
	.global divmod_pow2__div_pow2_u64_33
	.hidden	divmod_pow2__div_pow2_u64_33
	.type	divmod_pow2__div_pow2_u64_33, #function
	.proc	05
divmod_pow2__div_pow2_u64_33:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 33, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_33, .-divmod_pow2__div_pow2_u64_33
	.align 4
	.global divmod_pow2__mod_pow2_u64_33
	.hidden	divmod_pow2__mod_pow2_u64_33
	.type	divmod_pow2__mod_pow2_u64_33, #function
	.proc	05
divmod_pow2__mod_pow2_u64_33:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	1, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_33, .-divmod_pow2__mod_pow2_u64_33
	.align 4
	.global divmod_pow2__div_pow2_u64_34
	.hidden	divmod_pow2__div_pow2_u64_34
	.type	divmod_pow2__div_pow2_u64_34, #function
	.proc	05
divmod_pow2__div_pow2_u64_34:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 34, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_34, .-divmod_pow2__div_pow2_u64_34
	.align 4
	.global divmod_pow2__mod_pow2_u64_34
	.hidden	divmod_pow2__mod_pow2_u64_34
	.type	divmod_pow2__mod_pow2_u64_34, #function
	.proc	05
divmod_pow2__mod_pow2_u64_34:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	3, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_34, .-divmod_pow2__mod_pow2_u64_34
	.align 4
	.global divmod_pow2__div_pow2_u64_35
	.hidden	divmod_pow2__div_pow2_u64_35
	.type	divmod_pow2__div_pow2_u64_35, #function
	.proc	05
divmod_pow2__div_pow2_u64_35:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 35, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_35, .-divmod_pow2__div_pow2_u64_35
	.align 4
	.global divmod_pow2__mod_pow2_u64_35
	.hidden	divmod_pow2__mod_pow2_u64_35
	.type	divmod_pow2__mod_pow2_u64_35, #function
	.proc	05
divmod_pow2__mod_pow2_u64_35:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	7, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_35, .-divmod_pow2__mod_pow2_u64_35
	.align 4
	.global divmod_pow2__div_pow2_u64_36
	.hidden	divmod_pow2__div_pow2_u64_36
	.type	divmod_pow2__div_pow2_u64_36, #function
	.proc	05
divmod_pow2__div_pow2_u64_36:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 36, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_36, .-divmod_pow2__div_pow2_u64_36
	.align 4
	.global divmod_pow2__mod_pow2_u64_36
	.hidden	divmod_pow2__mod_pow2_u64_36
	.type	divmod_pow2__mod_pow2_u64_36, #function
	.proc	05
divmod_pow2__mod_pow2_u64_36:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	15, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_36, .-divmod_pow2__mod_pow2_u64_36
	.align 4
	.global divmod_pow2__div_pow2_u64_37
	.hidden	divmod_pow2__div_pow2_u64_37
	.type	divmod_pow2__div_pow2_u64_37, #function
	.proc	05
divmod_pow2__div_pow2_u64_37:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 37, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_37, .-divmod_pow2__div_pow2_u64_37
	.align 4
	.global divmod_pow2__mod_pow2_u64_37
	.hidden	divmod_pow2__mod_pow2_u64_37
	.type	divmod_pow2__mod_pow2_u64_37, #function
	.proc	05
divmod_pow2__mod_pow2_u64_37:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	31, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_37, .-divmod_pow2__mod_pow2_u64_37
	.align 4
	.global divmod_pow2__div_pow2_u64_38
	.hidden	divmod_pow2__div_pow2_u64_38
	.type	divmod_pow2__div_pow2_u64_38, #function
	.proc	05
divmod_pow2__div_pow2_u64_38:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 38, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_38, .-divmod_pow2__div_pow2_u64_38
	.align 4
	.global divmod_pow2__mod_pow2_u64_38
	.hidden	divmod_pow2__mod_pow2_u64_38
	.type	divmod_pow2__mod_pow2_u64_38, #function
	.proc	05
divmod_pow2__mod_pow2_u64_38:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	63, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_38, .-divmod_pow2__mod_pow2_u64_38
	.align 4
	.global divmod_pow2__div_pow2_u64_39
	.hidden	divmod_pow2__div_pow2_u64_39
	.type	divmod_pow2__div_pow2_u64_39, #function
	.proc	05
divmod_pow2__div_pow2_u64_39:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 39, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_39, .-divmod_pow2__div_pow2_u64_39
	.align 4
	.global divmod_pow2__mod_pow2_u64_39
	.hidden	divmod_pow2__mod_pow2_u64_39
	.type	divmod_pow2__mod_pow2_u64_39, #function
	.proc	05
divmod_pow2__mod_pow2_u64_39:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	127, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_39, .-divmod_pow2__mod_pow2_u64_39
	.align 4
	.global divmod_pow2__div_pow2_u64_40
	.hidden	divmod_pow2__div_pow2_u64_40
	.type	divmod_pow2__div_pow2_u64_40, #function
	.proc	05
divmod_pow2__div_pow2_u64_40:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 40, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_40, .-divmod_pow2__div_pow2_u64_40
	.align 4
	.global divmod_pow2__mod_pow2_u64_40
	.hidden	divmod_pow2__mod_pow2_u64_40
	.type	divmod_pow2__mod_pow2_u64_40, #function
	.proc	05
divmod_pow2__mod_pow2_u64_40:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	255, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_40, .-divmod_pow2__mod_pow2_u64_40
	.align 4
	.global divmod_pow2__div_pow2_u64_41
	.hidden	divmod_pow2__div_pow2_u64_41
	.type	divmod_pow2__div_pow2_u64_41, #function
	.proc	05
divmod_pow2__div_pow2_u64_41:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 41, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_41, .-divmod_pow2__div_pow2_u64_41
	.align 4
	.global divmod_pow2__mod_pow2_u64_41
	.hidden	divmod_pow2__mod_pow2_u64_41
	.type	divmod_pow2__mod_pow2_u64_41, #function
	.proc	05
divmod_pow2__mod_pow2_u64_41:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	511, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_41, .-divmod_pow2__mod_pow2_u64_41
	.align 4
	.global divmod_pow2__div_pow2_u64_42
	.hidden	divmod_pow2__div_pow2_u64_42
	.type	divmod_pow2__div_pow2_u64_42, #function
	.proc	05
divmod_pow2__div_pow2_u64_42:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 42, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_42, .-divmod_pow2__div_pow2_u64_42
	.align 4
	.global divmod_pow2__mod_pow2_u64_42
	.hidden	divmod_pow2__mod_pow2_u64_42
	.type	divmod_pow2__mod_pow2_u64_42, #function
	.proc	05
divmod_pow2__mod_pow2_u64_42:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	1023, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_42, .-divmod_pow2__mod_pow2_u64_42
	.align 4
	.global divmod_pow2__div_pow2_u64_43
	.hidden	divmod_pow2__div_pow2_u64_43
	.type	divmod_pow2__div_pow2_u64_43, #function
	.proc	05
divmod_pow2__div_pow2_u64_43:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 43, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_43, .-divmod_pow2__div_pow2_u64_43
	.align 4
	.global divmod_pow2__mod_pow2_u64_43
	.hidden	divmod_pow2__mod_pow2_u64_43
	.type	divmod_pow2__mod_pow2_u64_43, #function
	.proc	05
divmod_pow2__mod_pow2_u64_43:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	2047, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_43, .-divmod_pow2__mod_pow2_u64_43
	.align 4
	.global divmod_pow2__div_pow2_u64_44
	.hidden	divmod_pow2__div_pow2_u64_44
	.type	divmod_pow2__div_pow2_u64_44, #function
	.proc	05
divmod_pow2__div_pow2_u64_44:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 44, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_44, .-divmod_pow2__div_pow2_u64_44
	.align 4
	.global divmod_pow2__mod_pow2_u64_44
	.hidden	divmod_pow2__mod_pow2_u64_44
	.type	divmod_pow2__mod_pow2_u64_44, #function
	.proc	05
divmod_pow2__mod_pow2_u64_44:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	4095, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_44, .-divmod_pow2__mod_pow2_u64_44
	.align 4
	.global divmod_pow2__div_pow2_u64_45
	.hidden	divmod_pow2__div_pow2_u64_45
	.type	divmod_pow2__div_pow2_u64_45, #function
	.proc	05
divmod_pow2__div_pow2_u64_45:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 45, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_45, .-divmod_pow2__div_pow2_u64_45
	.align 4
	.global divmod_pow2__mod_pow2_u64_45
	.hidden	divmod_pow2__mod_pow2_u64_45
	.type	divmod_pow2__mod_pow2_u64_45, #function
	.proc	05
divmod_pow2__mod_pow2_u64_45:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(7168), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_45, .-divmod_pow2__mod_pow2_u64_45
	.align 4
	.global divmod_pow2__div_pow2_u64_46
	.hidden	divmod_pow2__div_pow2_u64_46
	.type	divmod_pow2__div_pow2_u64_46, #function
	.proc	05
divmod_pow2__div_pow2_u64_46:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 46, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_46, .-divmod_pow2__div_pow2_u64_46
	.align 4
	.global divmod_pow2__mod_pow2_u64_46
	.hidden	divmod_pow2__mod_pow2_u64_46
	.type	divmod_pow2__mod_pow2_u64_46, #function
	.proc	05
divmod_pow2__mod_pow2_u64_46:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(15360), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_46, .-divmod_pow2__mod_pow2_u64_46
	.align 4
	.global divmod_pow2__div_pow2_u64_47
	.hidden	divmod_pow2__div_pow2_u64_47
	.type	divmod_pow2__div_pow2_u64_47, #function
	.proc	05
divmod_pow2__div_pow2_u64_47:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 47, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_47, .-divmod_pow2__div_pow2_u64_47
	.align 4
	.global divmod_pow2__mod_pow2_u64_47
	.hidden	divmod_pow2__mod_pow2_u64_47
	.type	divmod_pow2__mod_pow2_u64_47, #function
	.proc	05
divmod_pow2__mod_pow2_u64_47:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(31744), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_47, .-divmod_pow2__mod_pow2_u64_47
	.align 4
	.global divmod_pow2__div_pow2_u64_48
	.hidden	divmod_pow2__div_pow2_u64_48
	.type	divmod_pow2__div_pow2_u64_48, #function
	.proc	05
divmod_pow2__div_pow2_u64_48:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 48, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_48, .-divmod_pow2__div_pow2_u64_48
	.align 4
	.global divmod_pow2__mod_pow2_u64_48
	.hidden	divmod_pow2__mod_pow2_u64_48
	.type	divmod_pow2__mod_pow2_u64_48, #function
	.proc	05
divmod_pow2__mod_pow2_u64_48:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(64512), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_48, .-divmod_pow2__mod_pow2_u64_48
	.align 4
	.global divmod_pow2__div_pow2_u64_49
	.hidden	divmod_pow2__div_pow2_u64_49
	.type	divmod_pow2__div_pow2_u64_49, #function
	.proc	05
divmod_pow2__div_pow2_u64_49:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 49, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_49, .-divmod_pow2__div_pow2_u64_49
	.align 4
	.global divmod_pow2__mod_pow2_u64_49
	.hidden	divmod_pow2__mod_pow2_u64_49
	.type	divmod_pow2__mod_pow2_u64_49, #function
	.proc	05
divmod_pow2__mod_pow2_u64_49:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(130048), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_49, .-divmod_pow2__mod_pow2_u64_49
	.align 4
	.global divmod_pow2__div_pow2_u64_50
	.hidden	divmod_pow2__div_pow2_u64_50
	.type	divmod_pow2__div_pow2_u64_50, #function
	.proc	05
divmod_pow2__div_pow2_u64_50:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 50, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_50, .-divmod_pow2__div_pow2_u64_50
	.align 4
	.global divmod_pow2__mod_pow2_u64_50
	.hidden	divmod_pow2__mod_pow2_u64_50
	.type	divmod_pow2__mod_pow2_u64_50, #function
	.proc	05
divmod_pow2__mod_pow2_u64_50:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(261120), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_50, .-divmod_pow2__mod_pow2_u64_50
	.align 4
	.global divmod_pow2__div_pow2_u64_51
	.hidden	divmod_pow2__div_pow2_u64_51
	.type	divmod_pow2__div_pow2_u64_51, #function
	.proc	05
divmod_pow2__div_pow2_u64_51:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 51, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_51, .-divmod_pow2__div_pow2_u64_51
	.align 4
	.global divmod_pow2__mod_pow2_u64_51
	.hidden	divmod_pow2__mod_pow2_u64_51
	.type	divmod_pow2__mod_pow2_u64_51, #function
	.proc	05
divmod_pow2__mod_pow2_u64_51:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(523264), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_51, .-divmod_pow2__mod_pow2_u64_51
	.align 4
	.global divmod_pow2__div_pow2_u64_52
	.hidden	divmod_pow2__div_pow2_u64_52
	.type	divmod_pow2__div_pow2_u64_52, #function
	.proc	05
divmod_pow2__div_pow2_u64_52:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 52, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_52, .-divmod_pow2__div_pow2_u64_52
	.align 4
	.global divmod_pow2__mod_pow2_u64_52
	.hidden	divmod_pow2__mod_pow2_u64_52
	.type	divmod_pow2__mod_pow2_u64_52, #function
	.proc	05
divmod_pow2__mod_pow2_u64_52:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(1047552), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_52, .-divmod_pow2__mod_pow2_u64_52
	.align 4
	.global divmod_pow2__div_pow2_u64_53
	.hidden	divmod_pow2__div_pow2_u64_53
	.type	divmod_pow2__div_pow2_u64_53, #function
	.proc	05
divmod_pow2__div_pow2_u64_53:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 53, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_53, .-divmod_pow2__div_pow2_u64_53
	.align 4
	.global divmod_pow2__mod_pow2_u64_53
	.hidden	divmod_pow2__mod_pow2_u64_53
	.type	divmod_pow2__mod_pow2_u64_53, #function
	.proc	05
divmod_pow2__mod_pow2_u64_53:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(2096128), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_53, .-divmod_pow2__mod_pow2_u64_53
	.align 4
	.global divmod_pow2__div_pow2_u64_54
	.hidden	divmod_pow2__div_pow2_u64_54
	.type	divmod_pow2__div_pow2_u64_54, #function
	.proc	05
divmod_pow2__div_pow2_u64_54:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 54, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_54, .-divmod_pow2__div_pow2_u64_54
	.align 4
	.global divmod_pow2__mod_pow2_u64_54
	.hidden	divmod_pow2__mod_pow2_u64_54
	.type	divmod_pow2__mod_pow2_u64_54, #function
	.proc	05
divmod_pow2__mod_pow2_u64_54:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(4193280), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_54, .-divmod_pow2__mod_pow2_u64_54
	.align 4
	.global divmod_pow2__div_pow2_u64_55
	.hidden	divmod_pow2__div_pow2_u64_55
	.type	divmod_pow2__div_pow2_u64_55, #function
	.proc	05
divmod_pow2__div_pow2_u64_55:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 55, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_55, .-divmod_pow2__div_pow2_u64_55
	.align 4
	.global divmod_pow2__mod_pow2_u64_55
	.hidden	divmod_pow2__mod_pow2_u64_55
	.type	divmod_pow2__mod_pow2_u64_55, #function
	.proc	05
divmod_pow2__mod_pow2_u64_55:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(8387584), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_55, .-divmod_pow2__mod_pow2_u64_55
	.align 4
	.global divmod_pow2__div_pow2_u64_56
	.hidden	divmod_pow2__div_pow2_u64_56
	.type	divmod_pow2__div_pow2_u64_56, #function
	.proc	05
divmod_pow2__div_pow2_u64_56:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 56, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_56, .-divmod_pow2__div_pow2_u64_56
	.align 4
	.global divmod_pow2__mod_pow2_u64_56
	.hidden	divmod_pow2__mod_pow2_u64_56
	.type	divmod_pow2__mod_pow2_u64_56, #function
	.proc	05
divmod_pow2__mod_pow2_u64_56:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(16776192), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_56, .-divmod_pow2__mod_pow2_u64_56
	.align 4
	.global divmod_pow2__div_pow2_u64_57
	.hidden	divmod_pow2__div_pow2_u64_57
	.type	divmod_pow2__div_pow2_u64_57, #function
	.proc	05
divmod_pow2__div_pow2_u64_57:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 57, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_57, .-divmod_pow2__div_pow2_u64_57
	.align 4
	.global divmod_pow2__mod_pow2_u64_57
	.hidden	divmod_pow2__mod_pow2_u64_57
	.type	divmod_pow2__mod_pow2_u64_57, #function
	.proc	05
divmod_pow2__mod_pow2_u64_57:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(33553408), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_57, .-divmod_pow2__mod_pow2_u64_57
	.align 4
	.global divmod_pow2__div_pow2_u64_58
	.hidden	divmod_pow2__div_pow2_u64_58
	.type	divmod_pow2__div_pow2_u64_58, #function
	.proc	05
divmod_pow2__div_pow2_u64_58:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 58, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_58, .-divmod_pow2__div_pow2_u64_58
	.align 4
	.global divmod_pow2__mod_pow2_u64_58
	.hidden	divmod_pow2__mod_pow2_u64_58
	.type	divmod_pow2__mod_pow2_u64_58, #function
	.proc	05
divmod_pow2__mod_pow2_u64_58:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(67107840), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_58, .-divmod_pow2__mod_pow2_u64_58
	.align 4
	.global divmod_pow2__div_pow2_u64_59
	.hidden	divmod_pow2__div_pow2_u64_59
	.type	divmod_pow2__div_pow2_u64_59, #function
	.proc	05
divmod_pow2__div_pow2_u64_59:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 59, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_59, .-divmod_pow2__div_pow2_u64_59
	.align 4
	.global divmod_pow2__mod_pow2_u64_59
	.hidden	divmod_pow2__mod_pow2_u64_59
	.type	divmod_pow2__mod_pow2_u64_59, #function
	.proc	05
divmod_pow2__mod_pow2_u64_59:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(134216704), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_59, .-divmod_pow2__mod_pow2_u64_59
	.align 4
	.global divmod_pow2__div_pow2_u64_60
	.hidden	divmod_pow2__div_pow2_u64_60
	.type	divmod_pow2__div_pow2_u64_60, #function
	.proc	05
divmod_pow2__div_pow2_u64_60:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 60, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_60, .-divmod_pow2__div_pow2_u64_60
	.align 4
	.global divmod_pow2__mod_pow2_u64_60
	.hidden	divmod_pow2__mod_pow2_u64_60
	.type	divmod_pow2__mod_pow2_u64_60, #function
	.proc	05
divmod_pow2__mod_pow2_u64_60:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(268434432), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_60, .-divmod_pow2__mod_pow2_u64_60
	.align 4
	.global divmod_pow2__div_pow2_u64_61
	.hidden	divmod_pow2__div_pow2_u64_61
	.type	divmod_pow2__div_pow2_u64_61, #function
	.proc	05
divmod_pow2__div_pow2_u64_61:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 61, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_61, .-divmod_pow2__div_pow2_u64_61
	.align 4
	.global divmod_pow2__mod_pow2_u64_61
	.hidden	divmod_pow2__mod_pow2_u64_61
	.type	divmod_pow2__mod_pow2_u64_61, #function
	.proc	05
divmod_pow2__mod_pow2_u64_61:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(536869888), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_61, .-divmod_pow2__mod_pow2_u64_61
	.align 4
	.global divmod_pow2__div_pow2_u64_62
	.hidden	divmod_pow2__div_pow2_u64_62
	.type	divmod_pow2__div_pow2_u64_62, #function
	.proc	05
divmod_pow2__div_pow2_u64_62:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 62, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_62, .-divmod_pow2__div_pow2_u64_62
	.align 4
	.global divmod_pow2__mod_pow2_u64_62
	.hidden	divmod_pow2__mod_pow2_u64_62
	.type	divmod_pow2__mod_pow2_u64_62, #function
	.proc	05
divmod_pow2__mod_pow2_u64_62:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(1073740800), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_62, .-divmod_pow2__mod_pow2_u64_62
	.align 4
	.global divmod_pow2__div_pow2_u64_63
	.hidden	divmod_pow2__div_pow2_u64_63
	.type	divmod_pow2__div_pow2_u64_63, #function
	.proc	05
divmod_pow2__div_pow2_u64_63:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sethi	%hi(2147483648), %o0
	mov	0, %o1
	mov	%g2, %o2
	mov	%g3, %o3
	call	m3_divL, 0
	 nop
	mov	%o0, %g2
	mov	%o1, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_u64_63, .-divmod_pow2__div_pow2_u64_63
	.align 4
	.global divmod_pow2__mod_pow2_u64_63
	.hidden	divmod_pow2__mod_pow2_u64_63
	.type	divmod_pow2__mod_pow2_u64_63, #function
	.proc	05
divmod_pow2__mod_pow2_u64_63:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sethi	%hi(2147483648), %o0
	mov	0, %o1
	mov	%g2, %o2
	mov	%g3, %o3
	call	m3_modL, 0
	 nop
	mov	%o0, %g2
	mov	%o1, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_u64_63, .-divmod_pow2__mod_pow2_u64_63
	.align 4
	.global divmod_pow2__div_pow2_I_0
	.hidden	divmod_pow2__div_pow2_I_0
	.type	divmod_pow2__div_pow2_I_0, #function
	.proc	04
divmod_pow2__div_pow2_I_0:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_I_0, .-divmod_pow2__div_pow2_I_0
	.align 4
	.global divmod_pow2__mod_pow2_I_0
	.hidden	divmod_pow2__mod_pow2_I_0
	.type	divmod_pow2__mod_pow2_I_0, #function
	.proc	04
divmod_pow2__mod_pow2_I_0:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	mov	0, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_I_0, .-divmod_pow2__mod_pow2_I_0
	.align 4
	.global divmod_pow2__div_pow2_I_1
	.hidden	divmod_pow2__div_pow2_I_1
	.type	divmod_pow2__div_pow2_I_1, #function
	.proc	04
divmod_pow2__div_pow2_I_1:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_I_1, .-divmod_pow2__div_pow2_I_1
	.align 4
	.global divmod_pow2__mod_pow2_I_1
	.hidden	divmod_pow2__mod_pow2_I_1
	.type	divmod_pow2__mod_pow2_I_1, #function
	.proc	04
divmod_pow2__mod_pow2_I_1:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_I_1, .-divmod_pow2__mod_pow2_I_1
	.align 4
	.global divmod_pow2__div_pow2_I_2
	.hidden	divmod_pow2__div_pow2_I_2
	.type	divmod_pow2__div_pow2_I_2, #function
	.proc	04
divmod_pow2__div_pow2_I_2:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 2, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_I_2, .-divmod_pow2__div_pow2_I_2
	.align 4
	.global divmod_pow2__mod_pow2_I_2
	.hidden	divmod_pow2__mod_pow2_I_2
	.type	divmod_pow2__mod_pow2_I_2, #function
	.proc	04
divmod_pow2__mod_pow2_I_2:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 3, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_I_2, .-divmod_pow2__mod_pow2_I_2
	.align 4
	.global divmod_pow2__div_pow2_I_3
	.hidden	divmod_pow2__div_pow2_I_3
	.type	divmod_pow2__div_pow2_I_3, #function
	.proc	04
divmod_pow2__div_pow2_I_3:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 3, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_I_3, .-divmod_pow2__div_pow2_I_3
	.align 4
	.global divmod_pow2__mod_pow2_I_3
	.hidden	divmod_pow2__mod_pow2_I_3
	.type	divmod_pow2__mod_pow2_I_3, #function
	.proc	04
divmod_pow2__mod_pow2_I_3:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 7, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_I_3, .-divmod_pow2__mod_pow2_I_3
	.align 4
	.global divmod_pow2__div_pow2_I_4
	.hidden	divmod_pow2__div_pow2_I_4
	.type	divmod_pow2__div_pow2_I_4, #function
	.proc	04
divmod_pow2__div_pow2_I_4:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 4, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_I_4, .-divmod_pow2__div_pow2_I_4
	.align 4
	.global divmod_pow2__mod_pow2_I_4
	.hidden	divmod_pow2__mod_pow2_I_4
	.type	divmod_pow2__mod_pow2_I_4, #function
	.proc	04
divmod_pow2__mod_pow2_I_4:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 15, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_I_4, .-divmod_pow2__mod_pow2_I_4
	.align 4
	.global divmod_pow2__div_pow2_I_5
	.hidden	divmod_pow2__div_pow2_I_5
	.type	divmod_pow2__div_pow2_I_5, #function
	.proc	04
divmod_pow2__div_pow2_I_5:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_I_5, .-divmod_pow2__div_pow2_I_5
	.align 4
	.global divmod_pow2__mod_pow2_I_5
	.hidden	divmod_pow2__mod_pow2_I_5
	.type	divmod_pow2__mod_pow2_I_5, #function
	.proc	04
divmod_pow2__mod_pow2_I_5:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_I_5, .-divmod_pow2__mod_pow2_I_5
	.align 4
	.global divmod_pow2__div_pow2_I_6
	.hidden	divmod_pow2__div_pow2_I_6
	.type	divmod_pow2__div_pow2_I_6, #function
	.proc	04
divmod_pow2__div_pow2_I_6:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 6, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_I_6, .-divmod_pow2__div_pow2_I_6
	.align 4
	.global divmod_pow2__mod_pow2_I_6
	.hidden	divmod_pow2__mod_pow2_I_6
	.type	divmod_pow2__mod_pow2_I_6, #function
	.proc	04
divmod_pow2__mod_pow2_I_6:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 63, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_I_6, .-divmod_pow2__mod_pow2_I_6
	.align 4
	.global divmod_pow2__div_pow2_I_7
	.hidden	divmod_pow2__div_pow2_I_7
	.type	divmod_pow2__div_pow2_I_7, #function
	.proc	04
divmod_pow2__div_pow2_I_7:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 7, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_I_7, .-divmod_pow2__div_pow2_I_7
	.align 4
	.global divmod_pow2__mod_pow2_I_7
	.hidden	divmod_pow2__mod_pow2_I_7
	.type	divmod_pow2__mod_pow2_I_7, #function
	.proc	04
divmod_pow2__mod_pow2_I_7:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 127, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_I_7, .-divmod_pow2__mod_pow2_I_7
	.align 4
	.global divmod_pow2__div_pow2_I_8
	.hidden	divmod_pow2__div_pow2_I_8
	.type	divmod_pow2__div_pow2_I_8, #function
	.proc	04
divmod_pow2__div_pow2_I_8:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 8, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_I_8, .-divmod_pow2__div_pow2_I_8
	.align 4
	.global divmod_pow2__mod_pow2_I_8
	.hidden	divmod_pow2__mod_pow2_I_8
	.type	divmod_pow2__mod_pow2_I_8, #function
	.proc	04
divmod_pow2__mod_pow2_I_8:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 255, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_I_8, .-divmod_pow2__mod_pow2_I_8
	.align 4
	.global divmod_pow2__div_pow2_I_9
	.hidden	divmod_pow2__div_pow2_I_9
	.type	divmod_pow2__div_pow2_I_9, #function
	.proc	04
divmod_pow2__div_pow2_I_9:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 9, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_I_9, .-divmod_pow2__div_pow2_I_9
	.align 4
	.global divmod_pow2__mod_pow2_I_9
	.hidden	divmod_pow2__mod_pow2_I_9
	.type	divmod_pow2__mod_pow2_I_9, #function
	.proc	04
divmod_pow2__mod_pow2_I_9:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 511, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_I_9, .-divmod_pow2__mod_pow2_I_9
	.align 4
	.global divmod_pow2__div_pow2_I_10
	.hidden	divmod_pow2__div_pow2_I_10
	.type	divmod_pow2__div_pow2_I_10, #function
	.proc	04
divmod_pow2__div_pow2_I_10:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 10, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_I_10, .-divmod_pow2__div_pow2_I_10
	.align 4
	.global divmod_pow2__mod_pow2_I_10
	.hidden	divmod_pow2__mod_pow2_I_10
	.type	divmod_pow2__mod_pow2_I_10, #function
	.proc	04
divmod_pow2__mod_pow2_I_10:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 1023, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_I_10, .-divmod_pow2__mod_pow2_I_10
	.align 4
	.global divmod_pow2__div_pow2_I_11
	.hidden	divmod_pow2__div_pow2_I_11
	.type	divmod_pow2__div_pow2_I_11, #function
	.proc	04
divmod_pow2__div_pow2_I_11:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 11, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_I_11, .-divmod_pow2__div_pow2_I_11
	.align 4
	.global divmod_pow2__mod_pow2_I_11
	.hidden	divmod_pow2__mod_pow2_I_11
	.type	divmod_pow2__mod_pow2_I_11, #function
	.proc	04
divmod_pow2__mod_pow2_I_11:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 2047, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_I_11, .-divmod_pow2__mod_pow2_I_11
	.align 4
	.global divmod_pow2__div_pow2_I_12
	.hidden	divmod_pow2__div_pow2_I_12
	.type	divmod_pow2__div_pow2_I_12, #function
	.proc	04
divmod_pow2__div_pow2_I_12:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 12, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_I_12, .-divmod_pow2__div_pow2_I_12
	.align 4
	.global divmod_pow2__mod_pow2_I_12
	.hidden	divmod_pow2__mod_pow2_I_12
	.type	divmod_pow2__mod_pow2_I_12, #function
	.proc	04
divmod_pow2__mod_pow2_I_12:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 4095, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_I_12, .-divmod_pow2__mod_pow2_I_12
	.align 4
	.global divmod_pow2__div_pow2_I_13
	.hidden	divmod_pow2__div_pow2_I_13
	.type	divmod_pow2__div_pow2_I_13, #function
	.proc	04
divmod_pow2__div_pow2_I_13:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 13, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_I_13, .-divmod_pow2__div_pow2_I_13
	.align 4
	.global divmod_pow2__mod_pow2_I_13
	.hidden	divmod_pow2__mod_pow2_I_13
	.type	divmod_pow2__mod_pow2_I_13, #function
	.proc	04
divmod_pow2__mod_pow2_I_13:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_I_13, .-divmod_pow2__mod_pow2_I_13
	.align 4
	.global divmod_pow2__div_pow2_I_14
	.hidden	divmod_pow2__div_pow2_I_14
	.type	divmod_pow2__div_pow2_I_14, #function
	.proc	04
divmod_pow2__div_pow2_I_14:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 14, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_I_14, .-divmod_pow2__div_pow2_I_14
	.align 4
	.global divmod_pow2__mod_pow2_I_14
	.hidden	divmod_pow2__mod_pow2_I_14
	.type	divmod_pow2__mod_pow2_I_14, #function
	.proc	04
divmod_pow2__mod_pow2_I_14:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_I_14, .-divmod_pow2__mod_pow2_I_14
	.align 4
	.global divmod_pow2__div_pow2_I_15
	.hidden	divmod_pow2__div_pow2_I_15
	.type	divmod_pow2__div_pow2_I_15, #function
	.proc	04
divmod_pow2__div_pow2_I_15:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 15, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_I_15, .-divmod_pow2__div_pow2_I_15
	.align 4
	.global divmod_pow2__mod_pow2_I_15
	.hidden	divmod_pow2__mod_pow2_I_15
	.type	divmod_pow2__mod_pow2_I_15, #function
	.proc	04
divmod_pow2__mod_pow2_I_15:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_I_15, .-divmod_pow2__mod_pow2_I_15
	.align 4
	.global divmod_pow2__div_pow2_I_16
	.hidden	divmod_pow2__div_pow2_I_16
	.type	divmod_pow2__div_pow2_I_16, #function
	.proc	04
divmod_pow2__div_pow2_I_16:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_I_16, .-divmod_pow2__div_pow2_I_16
	.align 4
	.global divmod_pow2__mod_pow2_I_16
	.hidden	divmod_pow2__mod_pow2_I_16
	.type	divmod_pow2__mod_pow2_I_16, #function
	.proc	04
divmod_pow2__mod_pow2_I_16:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_I_16, .-divmod_pow2__mod_pow2_I_16
	.align 4
	.global divmod_pow2__div_pow2_I_17
	.hidden	divmod_pow2__div_pow2_I_17
	.type	divmod_pow2__div_pow2_I_17, #function
	.proc	04
divmod_pow2__div_pow2_I_17:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 17, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_I_17, .-divmod_pow2__div_pow2_I_17
	.align 4
	.global divmod_pow2__mod_pow2_I_17
	.hidden	divmod_pow2__mod_pow2_I_17
	.type	divmod_pow2__mod_pow2_I_17, #function
	.proc	04
divmod_pow2__mod_pow2_I_17:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(130048), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_I_17, .-divmod_pow2__mod_pow2_I_17
	.align 4
	.global divmod_pow2__div_pow2_I_18
	.hidden	divmod_pow2__div_pow2_I_18
	.type	divmod_pow2__div_pow2_I_18, #function
	.proc	04
divmod_pow2__div_pow2_I_18:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 18, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_I_18, .-divmod_pow2__div_pow2_I_18
	.align 4
	.global divmod_pow2__mod_pow2_I_18
	.hidden	divmod_pow2__mod_pow2_I_18
	.type	divmod_pow2__mod_pow2_I_18, #function
	.proc	04
divmod_pow2__mod_pow2_I_18:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(261120), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_I_18, .-divmod_pow2__mod_pow2_I_18
	.align 4
	.global divmod_pow2__div_pow2_I_19
	.hidden	divmod_pow2__div_pow2_I_19
	.type	divmod_pow2__div_pow2_I_19, #function
	.proc	04
divmod_pow2__div_pow2_I_19:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 19, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_I_19, .-divmod_pow2__div_pow2_I_19
	.align 4
	.global divmod_pow2__mod_pow2_I_19
	.hidden	divmod_pow2__mod_pow2_I_19
	.type	divmod_pow2__mod_pow2_I_19, #function
	.proc	04
divmod_pow2__mod_pow2_I_19:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(523264), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_I_19, .-divmod_pow2__mod_pow2_I_19
	.align 4
	.global divmod_pow2__div_pow2_I_20
	.hidden	divmod_pow2__div_pow2_I_20
	.type	divmod_pow2__div_pow2_I_20, #function
	.proc	04
divmod_pow2__div_pow2_I_20:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 20, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_I_20, .-divmod_pow2__div_pow2_I_20
	.align 4
	.global divmod_pow2__mod_pow2_I_20
	.hidden	divmod_pow2__mod_pow2_I_20
	.type	divmod_pow2__mod_pow2_I_20, #function
	.proc	04
divmod_pow2__mod_pow2_I_20:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(1047552), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_I_20, .-divmod_pow2__mod_pow2_I_20
	.align 4
	.global divmod_pow2__div_pow2_I_21
	.hidden	divmod_pow2__div_pow2_I_21
	.type	divmod_pow2__div_pow2_I_21, #function
	.proc	04
divmod_pow2__div_pow2_I_21:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 21, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_I_21, .-divmod_pow2__div_pow2_I_21
	.align 4
	.global divmod_pow2__mod_pow2_I_21
	.hidden	divmod_pow2__mod_pow2_I_21
	.type	divmod_pow2__mod_pow2_I_21, #function
	.proc	04
divmod_pow2__mod_pow2_I_21:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(2096128), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_I_21, .-divmod_pow2__mod_pow2_I_21
	.align 4
	.global divmod_pow2__div_pow2_I_22
	.hidden	divmod_pow2__div_pow2_I_22
	.type	divmod_pow2__div_pow2_I_22, #function
	.proc	04
divmod_pow2__div_pow2_I_22:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 22, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_I_22, .-divmod_pow2__div_pow2_I_22
	.align 4
	.global divmod_pow2__mod_pow2_I_22
	.hidden	divmod_pow2__mod_pow2_I_22
	.type	divmod_pow2__mod_pow2_I_22, #function
	.proc	04
divmod_pow2__mod_pow2_I_22:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(4193280), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_I_22, .-divmod_pow2__mod_pow2_I_22
	.align 4
	.global divmod_pow2__div_pow2_I_23
	.hidden	divmod_pow2__div_pow2_I_23
	.type	divmod_pow2__div_pow2_I_23, #function
	.proc	04
divmod_pow2__div_pow2_I_23:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_I_23, .-divmod_pow2__div_pow2_I_23
	.align 4
	.global divmod_pow2__mod_pow2_I_23
	.hidden	divmod_pow2__mod_pow2_I_23
	.type	divmod_pow2__mod_pow2_I_23, #function
	.proc	04
divmod_pow2__mod_pow2_I_23:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(8387584), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_I_23, .-divmod_pow2__mod_pow2_I_23
	.align 4
	.global divmod_pow2__div_pow2_I_24
	.hidden	divmod_pow2__div_pow2_I_24
	.type	divmod_pow2__div_pow2_I_24, #function
	.proc	04
divmod_pow2__div_pow2_I_24:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_I_24, .-divmod_pow2__div_pow2_I_24
	.align 4
	.global divmod_pow2__mod_pow2_I_24
	.hidden	divmod_pow2__mod_pow2_I_24
	.type	divmod_pow2__mod_pow2_I_24, #function
	.proc	04
divmod_pow2__mod_pow2_I_24:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(16776192), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_I_24, .-divmod_pow2__mod_pow2_I_24
	.align 4
	.global divmod_pow2__div_pow2_I_25
	.hidden	divmod_pow2__div_pow2_I_25
	.type	divmod_pow2__div_pow2_I_25, #function
	.proc	04
divmod_pow2__div_pow2_I_25:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_I_25, .-divmod_pow2__div_pow2_I_25
	.align 4
	.global divmod_pow2__mod_pow2_I_25
	.hidden	divmod_pow2__mod_pow2_I_25
	.type	divmod_pow2__mod_pow2_I_25, #function
	.proc	04
divmod_pow2__mod_pow2_I_25:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(33553408), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_I_25, .-divmod_pow2__mod_pow2_I_25
	.align 4
	.global divmod_pow2__div_pow2_I_26
	.hidden	divmod_pow2__div_pow2_I_26
	.type	divmod_pow2__div_pow2_I_26, #function
	.proc	04
divmod_pow2__div_pow2_I_26:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_I_26, .-divmod_pow2__div_pow2_I_26
	.align 4
	.global divmod_pow2__mod_pow2_I_26
	.hidden	divmod_pow2__mod_pow2_I_26
	.type	divmod_pow2__mod_pow2_I_26, #function
	.proc	04
divmod_pow2__mod_pow2_I_26:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(67107840), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_I_26, .-divmod_pow2__mod_pow2_I_26
	.align 4
	.global divmod_pow2__div_pow2_I_27
	.hidden	divmod_pow2__div_pow2_I_27
	.type	divmod_pow2__div_pow2_I_27, #function
	.proc	04
divmod_pow2__div_pow2_I_27:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_I_27, .-divmod_pow2__div_pow2_I_27
	.align 4
	.global divmod_pow2__mod_pow2_I_27
	.hidden	divmod_pow2__mod_pow2_I_27
	.type	divmod_pow2__mod_pow2_I_27, #function
	.proc	04
divmod_pow2__mod_pow2_I_27:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(134216704), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_I_27, .-divmod_pow2__mod_pow2_I_27
	.align 4
	.global divmod_pow2__div_pow2_I_28
	.hidden	divmod_pow2__div_pow2_I_28
	.type	divmod_pow2__div_pow2_I_28, #function
	.proc	04
divmod_pow2__div_pow2_I_28:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_I_28, .-divmod_pow2__div_pow2_I_28
	.align 4
	.global divmod_pow2__mod_pow2_I_28
	.hidden	divmod_pow2__mod_pow2_I_28
	.type	divmod_pow2__mod_pow2_I_28, #function
	.proc	04
divmod_pow2__mod_pow2_I_28:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(268434432), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_I_28, .-divmod_pow2__mod_pow2_I_28
	.align 4
	.global divmod_pow2__div_pow2_I_29
	.hidden	divmod_pow2__div_pow2_I_29
	.type	divmod_pow2__div_pow2_I_29, #function
	.proc	04
divmod_pow2__div_pow2_I_29:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_I_29, .-divmod_pow2__div_pow2_I_29
	.align 4
	.global divmod_pow2__mod_pow2_I_29
	.hidden	divmod_pow2__mod_pow2_I_29
	.type	divmod_pow2__mod_pow2_I_29, #function
	.proc	04
divmod_pow2__mod_pow2_I_29:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(536869888), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_I_29, .-divmod_pow2__mod_pow2_I_29
	.align 4
	.global divmod_pow2__div_pow2_I_30
	.hidden	divmod_pow2__div_pow2_I_30
	.type	divmod_pow2__div_pow2_I_30, #function
	.proc	04
divmod_pow2__div_pow2_I_30:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_I_30, .-divmod_pow2__div_pow2_I_30
	.align 4
	.global divmod_pow2__mod_pow2_I_30
	.hidden	divmod_pow2__mod_pow2_I_30
	.type	divmod_pow2__mod_pow2_I_30, #function
	.proc	04
divmod_pow2__mod_pow2_I_30:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(1073740800), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_I_30, .-divmod_pow2__mod_pow2_I_30
	.align 4
	.global divmod_pow2__div_pow2_I_31
	.hidden	divmod_pow2__div_pow2_I_31
	.type	divmod_pow2__div_pow2_I_31, #function
	.proc	04
divmod_pow2__div_pow2_I_31:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sethi	%hi(-2147483648), %o0
	mov	%g1, %o1
	call	m3_div, 0
	 nop
	mov	%o0, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_I_31, .-divmod_pow2__div_pow2_I_31
	.align 4
	.global divmod_pow2__mod_pow2_I_31
	.hidden	divmod_pow2__mod_pow2_I_31
	.type	divmod_pow2__mod_pow2_I_31, #function
	.proc	04
divmod_pow2__mod_pow2_I_31:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sethi	%hi(-2147483648), %o0
	mov	%g1, %o1
	call	m3_mod, 0
	 nop
	mov	%o0, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_I_31, .-divmod_pow2__mod_pow2_I_31
	.align 4
	.global divmod_pow2__div_pow2_C_0
	.hidden	divmod_pow2__div_pow2_C_0
	.type	divmod_pow2__div_pow2_C_0, #function
	.proc	016
divmod_pow2__div_pow2_C_0:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bge	%icc, .LL898
	 nop
	sethi	%hi(29696), %g1
	or	%g1, 737, %o0
	call	_m3_fault, 0
	 nop
.LL898:
	ld	[%fp-24], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_C_0, .-divmod_pow2__div_pow2_C_0
	.align 4
	.global divmod_pow2__mod_pow2_C_0
	.hidden	divmod_pow2__mod_pow2_C_0
	.type	divmod_pow2__mod_pow2_C_0, #function
	.proc	016
divmod_pow2__mod_pow2_C_0:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	mov	0, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_C_0, .-divmod_pow2__mod_pow2_C_0
	.align 4
	.global divmod_pow2__div_pow2_C_1
	.hidden	divmod_pow2__div_pow2_C_1
	.type	divmod_pow2__div_pow2_C_1, #function
	.proc	016
divmod_pow2__div_pow2_C_1:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 1, %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bge	%icc, .LL903
	 nop
	sethi	%hi(29696), %g1
	or	%g1, 865, %o0
	call	_m3_fault, 0
	 nop
.LL903:
	ld	[%fp-24], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_C_1, .-divmod_pow2__div_pow2_C_1
	.align 4
	.global divmod_pow2__mod_pow2_C_1
	.hidden	divmod_pow2__mod_pow2_C_1
	.type	divmod_pow2__mod_pow2_C_1, #function
	.proc	016
divmod_pow2__mod_pow2_C_1:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_C_1, .-divmod_pow2__mod_pow2_C_1
	.align 4
	.global divmod_pow2__div_pow2_C_2
	.hidden	divmod_pow2__div_pow2_C_2
	.type	divmod_pow2__div_pow2_C_2, #function
	.proc	016
divmod_pow2__div_pow2_C_2:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 2, %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bge	%icc, .LL908
	 nop
	sethi	%hi(29696), %g1
	or	%g1, 993, %o0
	call	_m3_fault, 0
	 nop
.LL908:
	ld	[%fp-24], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_C_2, .-divmod_pow2__div_pow2_C_2
	.align 4
	.global divmod_pow2__mod_pow2_C_2
	.hidden	divmod_pow2__mod_pow2_C_2
	.type	divmod_pow2__mod_pow2_C_2, #function
	.proc	016
divmod_pow2__mod_pow2_C_2:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 3, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_C_2, .-divmod_pow2__mod_pow2_C_2
	.align 4
	.global divmod_pow2__div_pow2_C_3
	.hidden	divmod_pow2__div_pow2_C_3
	.type	divmod_pow2__div_pow2_C_3, #function
	.proc	016
divmod_pow2__div_pow2_C_3:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 3, %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bge	%icc, .LL913
	 nop
	sethi	%hi(30720), %g1
	or	%g1, 97, %o0
	call	_m3_fault, 0
	 nop
.LL913:
	ld	[%fp-24], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_C_3, .-divmod_pow2__div_pow2_C_3
	.align 4
	.global divmod_pow2__mod_pow2_C_3
	.hidden	divmod_pow2__mod_pow2_C_3
	.type	divmod_pow2__mod_pow2_C_3, #function
	.proc	016
divmod_pow2__mod_pow2_C_3:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 7, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_C_3, .-divmod_pow2__mod_pow2_C_3
	.align 4
	.global divmod_pow2__div_pow2_C_4
	.hidden	divmod_pow2__div_pow2_C_4
	.type	divmod_pow2__div_pow2_C_4, #function
	.proc	016
divmod_pow2__div_pow2_C_4:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 4, %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bge	%icc, .LL918
	 nop
	sethi	%hi(30720), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL918:
	ld	[%fp-24], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_C_4, .-divmod_pow2__div_pow2_C_4
	.align 4
	.global divmod_pow2__mod_pow2_C_4
	.hidden	divmod_pow2__mod_pow2_C_4
	.type	divmod_pow2__mod_pow2_C_4, #function
	.proc	016
divmod_pow2__mod_pow2_C_4:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 15, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_C_4, .-divmod_pow2__mod_pow2_C_4
	.align 4
	.global divmod_pow2__div_pow2_C_5
	.hidden	divmod_pow2__div_pow2_C_5
	.type	divmod_pow2__div_pow2_C_5, #function
	.proc	016
divmod_pow2__div_pow2_C_5:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 5, %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bge	%icc, .LL923
	 nop
	sethi	%hi(30720), %g1
	or	%g1, 353, %o0
	call	_m3_fault, 0
	 nop
.LL923:
	ld	[%fp-24], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_C_5, .-divmod_pow2__div_pow2_C_5
	.align 4
	.global divmod_pow2__mod_pow2_C_5
	.hidden	divmod_pow2__mod_pow2_C_5
	.type	divmod_pow2__mod_pow2_C_5, #function
	.proc	016
divmod_pow2__mod_pow2_C_5:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_C_5, .-divmod_pow2__mod_pow2_C_5
	.align 4
	.global divmod_pow2__div_pow2_C_6
	.hidden	divmod_pow2__div_pow2_C_6
	.type	divmod_pow2__div_pow2_C_6, #function
	.proc	016
divmod_pow2__div_pow2_C_6:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 6, %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bge	%icc, .LL928
	 nop
	sethi	%hi(30720), %g1
	or	%g1, 481, %o0
	call	_m3_fault, 0
	 nop
.LL928:
	ld	[%fp-24], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_C_6, .-divmod_pow2__div_pow2_C_6
	.align 4
	.global divmod_pow2__mod_pow2_C_6
	.hidden	divmod_pow2__mod_pow2_C_6
	.type	divmod_pow2__mod_pow2_C_6, #function
	.proc	016
divmod_pow2__mod_pow2_C_6:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 63, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_C_6, .-divmod_pow2__mod_pow2_C_6
	.align 4
	.global divmod_pow2__div_pow2_C_7
	.hidden	divmod_pow2__div_pow2_C_7
	.type	divmod_pow2__div_pow2_C_7, #function
	.proc	016
divmod_pow2__div_pow2_C_7:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 7, %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bge	%icc, .LL933
	 nop
	sethi	%hi(30720), %g1
	or	%g1, 609, %o0
	call	_m3_fault, 0
	 nop
.LL933:
	ld	[%fp-24], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_C_7, .-divmod_pow2__div_pow2_C_7
	.align 4
	.global divmod_pow2__mod_pow2_C_7
	.hidden	divmod_pow2__mod_pow2_C_7
	.type	divmod_pow2__mod_pow2_C_7, #function
	.proc	016
divmod_pow2__mod_pow2_C_7:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 127, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_C_7, .-divmod_pow2__mod_pow2_C_7
	.align 4
	.global divmod_pow2__div_pow2_C_8
	.hidden	divmod_pow2__div_pow2_C_8
	.type	divmod_pow2__div_pow2_C_8, #function
	.proc	016
divmod_pow2__div_pow2_C_8:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 8, %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bge	%icc, .LL938
	 nop
	sethi	%hi(30720), %g1
	or	%g1, 737, %o0
	call	_m3_fault, 0
	 nop
.LL938:
	ld	[%fp-24], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_C_8, .-divmod_pow2__div_pow2_C_8
	.align 4
	.global divmod_pow2__mod_pow2_C_8
	.hidden	divmod_pow2__mod_pow2_C_8
	.type	divmod_pow2__mod_pow2_C_8, #function
	.proc	016
divmod_pow2__mod_pow2_C_8:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 255, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_C_8, .-divmod_pow2__mod_pow2_C_8
	.align 4
	.global divmod_pow2__div_pow2_C_9
	.hidden	divmod_pow2__div_pow2_C_9
	.type	divmod_pow2__div_pow2_C_9, #function
	.proc	016
divmod_pow2__div_pow2_C_9:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 9, %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bge	%icc, .LL943
	 nop
	sethi	%hi(30720), %g1
	or	%g1, 865, %o0
	call	_m3_fault, 0
	 nop
.LL943:
	ld	[%fp-24], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_C_9, .-divmod_pow2__div_pow2_C_9
	.align 4
	.global divmod_pow2__mod_pow2_C_9
	.hidden	divmod_pow2__mod_pow2_C_9
	.type	divmod_pow2__mod_pow2_C_9, #function
	.proc	016
divmod_pow2__mod_pow2_C_9:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 511, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_C_9, .-divmod_pow2__mod_pow2_C_9
	.align 4
	.global divmod_pow2__div_pow2_C_10
	.hidden	divmod_pow2__div_pow2_C_10
	.type	divmod_pow2__div_pow2_C_10, #function
	.proc	016
divmod_pow2__div_pow2_C_10:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 10, %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bge	%icc, .LL948
	 nop
	sethi	%hi(30720), %g1
	or	%g1, 993, %o0
	call	_m3_fault, 0
	 nop
.LL948:
	ld	[%fp-24], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_C_10, .-divmod_pow2__div_pow2_C_10
	.align 4
	.global divmod_pow2__mod_pow2_C_10
	.hidden	divmod_pow2__mod_pow2_C_10
	.type	divmod_pow2__mod_pow2_C_10, #function
	.proc	016
divmod_pow2__mod_pow2_C_10:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 1023, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_C_10, .-divmod_pow2__mod_pow2_C_10
	.align 4
	.global divmod_pow2__div_pow2_C_11
	.hidden	divmod_pow2__div_pow2_C_11
	.type	divmod_pow2__div_pow2_C_11, #function
	.proc	016
divmod_pow2__div_pow2_C_11:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 11, %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bge	%icc, .LL953
	 nop
	sethi	%hi(31744), %g1
	or	%g1, 97, %o0
	call	_m3_fault, 0
	 nop
.LL953:
	ld	[%fp-24], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_C_11, .-divmod_pow2__div_pow2_C_11
	.align 4
	.global divmod_pow2__mod_pow2_C_11
	.hidden	divmod_pow2__mod_pow2_C_11
	.type	divmod_pow2__mod_pow2_C_11, #function
	.proc	016
divmod_pow2__mod_pow2_C_11:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 2047, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_C_11, .-divmod_pow2__mod_pow2_C_11
	.align 4
	.global divmod_pow2__div_pow2_C_12
	.hidden	divmod_pow2__div_pow2_C_12
	.type	divmod_pow2__div_pow2_C_12, #function
	.proc	016
divmod_pow2__div_pow2_C_12:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 12, %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bge	%icc, .LL958
	 nop
	sethi	%hi(31744), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL958:
	ld	[%fp-24], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_C_12, .-divmod_pow2__div_pow2_C_12
	.align 4
	.global divmod_pow2__mod_pow2_C_12
	.hidden	divmod_pow2__mod_pow2_C_12
	.type	divmod_pow2__mod_pow2_C_12, #function
	.proc	016
divmod_pow2__mod_pow2_C_12:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	and	%g1, 4095, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_C_12, .-divmod_pow2__mod_pow2_C_12
	.align 4
	.global divmod_pow2__div_pow2_C_13
	.hidden	divmod_pow2__div_pow2_C_13
	.type	divmod_pow2__div_pow2_C_13, #function
	.proc	016
divmod_pow2__div_pow2_C_13:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 13, %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bge	%icc, .LL963
	 nop
	sethi	%hi(31744), %g1
	or	%g1, 353, %o0
	call	_m3_fault, 0
	 nop
.LL963:
	ld	[%fp-24], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_C_13, .-divmod_pow2__div_pow2_C_13
	.align 4
	.global divmod_pow2__mod_pow2_C_13
	.hidden	divmod_pow2__mod_pow2_C_13
	.type	divmod_pow2__mod_pow2_C_13, #function
	.proc	016
divmod_pow2__mod_pow2_C_13:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_C_13, .-divmod_pow2__mod_pow2_C_13
	.align 4
	.global divmod_pow2__div_pow2_C_14
	.hidden	divmod_pow2__div_pow2_C_14
	.type	divmod_pow2__div_pow2_C_14, #function
	.proc	016
divmod_pow2__div_pow2_C_14:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 14, %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bge	%icc, .LL968
	 nop
	sethi	%hi(31744), %g1
	or	%g1, 481, %o0
	call	_m3_fault, 0
	 nop
.LL968:
	ld	[%fp-24], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_C_14, .-divmod_pow2__div_pow2_C_14
	.align 4
	.global divmod_pow2__mod_pow2_C_14
	.hidden	divmod_pow2__mod_pow2_C_14
	.type	divmod_pow2__mod_pow2_C_14, #function
	.proc	016
divmod_pow2__mod_pow2_C_14:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_C_14, .-divmod_pow2__mod_pow2_C_14
	.align 4
	.global divmod_pow2__div_pow2_C_15
	.hidden	divmod_pow2__div_pow2_C_15
	.type	divmod_pow2__div_pow2_C_15, #function
	.proc	016
divmod_pow2__div_pow2_C_15:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 15, %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bge	%icc, .LL973
	 nop
	sethi	%hi(31744), %g1
	or	%g1, 609, %o0
	call	_m3_fault, 0
	 nop
.LL973:
	ld	[%fp-24], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_C_15, .-divmod_pow2__div_pow2_C_15
	.align 4
	.global divmod_pow2__mod_pow2_C_15
	.hidden	divmod_pow2__mod_pow2_C_15
	.type	divmod_pow2__mod_pow2_C_15, #function
	.proc	016
divmod_pow2__mod_pow2_C_15:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_C_15, .-divmod_pow2__mod_pow2_C_15
	.align 4
	.global divmod_pow2__div_pow2_C_16
	.hidden	divmod_pow2__div_pow2_C_16
	.type	divmod_pow2__div_pow2_C_16, #function
	.proc	016
divmod_pow2__div_pow2_C_16:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 16, %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bge	%icc, .LL978
	 nop
	sethi	%hi(31744), %g1
	or	%g1, 737, %o0
	call	_m3_fault, 0
	 nop
.LL978:
	ld	[%fp-24], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_C_16, .-divmod_pow2__div_pow2_C_16
	.align 4
	.global divmod_pow2__mod_pow2_C_16
	.hidden	divmod_pow2__mod_pow2_C_16
	.type	divmod_pow2__mod_pow2_C_16, #function
	.proc	016
divmod_pow2__mod_pow2_C_16:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_C_16, .-divmod_pow2__mod_pow2_C_16
	.align 4
	.global divmod_pow2__div_pow2_C_17
	.hidden	divmod_pow2__div_pow2_C_17
	.type	divmod_pow2__div_pow2_C_17, #function
	.proc	016
divmod_pow2__div_pow2_C_17:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 17, %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bge	%icc, .LL983
	 nop
	sethi	%hi(31744), %g1
	or	%g1, 865, %o0
	call	_m3_fault, 0
	 nop
.LL983:
	ld	[%fp-24], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_C_17, .-divmod_pow2__div_pow2_C_17
	.align 4
	.global divmod_pow2__mod_pow2_C_17
	.hidden	divmod_pow2__mod_pow2_C_17
	.type	divmod_pow2__mod_pow2_C_17, #function
	.proc	016
divmod_pow2__mod_pow2_C_17:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(130048), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_C_17, .-divmod_pow2__mod_pow2_C_17
	.align 4
	.global divmod_pow2__div_pow2_C_18
	.hidden	divmod_pow2__div_pow2_C_18
	.type	divmod_pow2__div_pow2_C_18, #function
	.proc	016
divmod_pow2__div_pow2_C_18:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 18, %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bge	%icc, .LL988
	 nop
	sethi	%hi(31744), %g1
	or	%g1, 993, %o0
	call	_m3_fault, 0
	 nop
.LL988:
	ld	[%fp-24], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_C_18, .-divmod_pow2__div_pow2_C_18
	.align 4
	.global divmod_pow2__mod_pow2_C_18
	.hidden	divmod_pow2__mod_pow2_C_18
	.type	divmod_pow2__mod_pow2_C_18, #function
	.proc	016
divmod_pow2__mod_pow2_C_18:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(261120), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_C_18, .-divmod_pow2__mod_pow2_C_18
	.align 4
	.global divmod_pow2__div_pow2_C_19
	.hidden	divmod_pow2__div_pow2_C_19
	.type	divmod_pow2__div_pow2_C_19, #function
	.proc	016
divmod_pow2__div_pow2_C_19:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 19, %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bge	%icc, .LL993
	 nop
	sethi	%hi(32768), %g1
	or	%g1, 97, %o0
	call	_m3_fault, 0
	 nop
.LL993:
	ld	[%fp-24], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_C_19, .-divmod_pow2__div_pow2_C_19
	.align 4
	.global divmod_pow2__mod_pow2_C_19
	.hidden	divmod_pow2__mod_pow2_C_19
	.type	divmod_pow2__mod_pow2_C_19, #function
	.proc	016
divmod_pow2__mod_pow2_C_19:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(523264), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_C_19, .-divmod_pow2__mod_pow2_C_19
	.align 4
	.global divmod_pow2__div_pow2_C_20
	.hidden	divmod_pow2__div_pow2_C_20
	.type	divmod_pow2__div_pow2_C_20, #function
	.proc	016
divmod_pow2__div_pow2_C_20:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 20, %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bge	%icc, .LL998
	 nop
	sethi	%hi(32768), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL998:
	ld	[%fp-24], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_C_20, .-divmod_pow2__div_pow2_C_20
	.align 4
	.global divmod_pow2__mod_pow2_C_20
	.hidden	divmod_pow2__mod_pow2_C_20
	.type	divmod_pow2__mod_pow2_C_20, #function
	.proc	016
divmod_pow2__mod_pow2_C_20:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(1047552), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_C_20, .-divmod_pow2__mod_pow2_C_20
	.align 4
	.global divmod_pow2__div_pow2_C_21
	.hidden	divmod_pow2__div_pow2_C_21
	.type	divmod_pow2__div_pow2_C_21, #function
	.proc	016
divmod_pow2__div_pow2_C_21:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 21, %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bge	%icc, .LL1003
	 nop
	sethi	%hi(32768), %g1
	or	%g1, 353, %o0
	call	_m3_fault, 0
	 nop
.LL1003:
	ld	[%fp-24], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_C_21, .-divmod_pow2__div_pow2_C_21
	.align 4
	.global divmod_pow2__mod_pow2_C_21
	.hidden	divmod_pow2__mod_pow2_C_21
	.type	divmod_pow2__mod_pow2_C_21, #function
	.proc	016
divmod_pow2__mod_pow2_C_21:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(2096128), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_C_21, .-divmod_pow2__mod_pow2_C_21
	.align 4
	.global divmod_pow2__div_pow2_C_22
	.hidden	divmod_pow2__div_pow2_C_22
	.type	divmod_pow2__div_pow2_C_22, #function
	.proc	016
divmod_pow2__div_pow2_C_22:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 22, %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bge	%icc, .LL1008
	 nop
	sethi	%hi(32768), %g1
	or	%g1, 481, %o0
	call	_m3_fault, 0
	 nop
.LL1008:
	ld	[%fp-24], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_C_22, .-divmod_pow2__div_pow2_C_22
	.align 4
	.global divmod_pow2__mod_pow2_C_22
	.hidden	divmod_pow2__mod_pow2_C_22
	.type	divmod_pow2__mod_pow2_C_22, #function
	.proc	016
divmod_pow2__mod_pow2_C_22:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(4193280), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_C_22, .-divmod_pow2__mod_pow2_C_22
	.align 4
	.global divmod_pow2__div_pow2_C_23
	.hidden	divmod_pow2__div_pow2_C_23
	.type	divmod_pow2__div_pow2_C_23, #function
	.proc	016
divmod_pow2__div_pow2_C_23:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 23, %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bge	%icc, .LL1013
	 nop
	sethi	%hi(32768), %g1
	or	%g1, 609, %o0
	call	_m3_fault, 0
	 nop
.LL1013:
	ld	[%fp-24], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_C_23, .-divmod_pow2__div_pow2_C_23
	.align 4
	.global divmod_pow2__mod_pow2_C_23
	.hidden	divmod_pow2__mod_pow2_C_23
	.type	divmod_pow2__mod_pow2_C_23, #function
	.proc	016
divmod_pow2__mod_pow2_C_23:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(8387584), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_C_23, .-divmod_pow2__mod_pow2_C_23
	.align 4
	.global divmod_pow2__div_pow2_C_24
	.hidden	divmod_pow2__div_pow2_C_24
	.type	divmod_pow2__div_pow2_C_24, #function
	.proc	016
divmod_pow2__div_pow2_C_24:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 24, %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bge	%icc, .LL1018
	 nop
	sethi	%hi(32768), %g1
	or	%g1, 737, %o0
	call	_m3_fault, 0
	 nop
.LL1018:
	ld	[%fp-24], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_C_24, .-divmod_pow2__div_pow2_C_24
	.align 4
	.global divmod_pow2__mod_pow2_C_24
	.hidden	divmod_pow2__mod_pow2_C_24
	.type	divmod_pow2__mod_pow2_C_24, #function
	.proc	016
divmod_pow2__mod_pow2_C_24:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(16776192), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_C_24, .-divmod_pow2__mod_pow2_C_24
	.align 4
	.global divmod_pow2__div_pow2_C_25
	.hidden	divmod_pow2__div_pow2_C_25
	.type	divmod_pow2__div_pow2_C_25, #function
	.proc	016
divmod_pow2__div_pow2_C_25:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 25, %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bge	%icc, .LL1023
	 nop
	sethi	%hi(32768), %g1
	or	%g1, 865, %o0
	call	_m3_fault, 0
	 nop
.LL1023:
	ld	[%fp-24], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_C_25, .-divmod_pow2__div_pow2_C_25
	.align 4
	.global divmod_pow2__mod_pow2_C_25
	.hidden	divmod_pow2__mod_pow2_C_25
	.type	divmod_pow2__mod_pow2_C_25, #function
	.proc	016
divmod_pow2__mod_pow2_C_25:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(33553408), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_C_25, .-divmod_pow2__mod_pow2_C_25
	.align 4
	.global divmod_pow2__div_pow2_C_26
	.hidden	divmod_pow2__div_pow2_C_26
	.type	divmod_pow2__div_pow2_C_26, #function
	.proc	016
divmod_pow2__div_pow2_C_26:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 26, %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bge	%icc, .LL1028
	 nop
	sethi	%hi(32768), %g1
	or	%g1, 993, %o0
	call	_m3_fault, 0
	 nop
.LL1028:
	ld	[%fp-24], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_C_26, .-divmod_pow2__div_pow2_C_26
	.align 4
	.global divmod_pow2__mod_pow2_C_26
	.hidden	divmod_pow2__mod_pow2_C_26
	.type	divmod_pow2__mod_pow2_C_26, #function
	.proc	016
divmod_pow2__mod_pow2_C_26:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(67107840), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_C_26, .-divmod_pow2__mod_pow2_C_26
	.align 4
	.global divmod_pow2__div_pow2_C_27
	.hidden	divmod_pow2__div_pow2_C_27
	.type	divmod_pow2__div_pow2_C_27, #function
	.proc	016
divmod_pow2__div_pow2_C_27:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 27, %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bge	%icc, .LL1033
	 nop
	sethi	%hi(33792), %g1
	or	%g1, 97, %o0
	call	_m3_fault, 0
	 nop
.LL1033:
	ld	[%fp-24], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_C_27, .-divmod_pow2__div_pow2_C_27
	.align 4
	.global divmod_pow2__mod_pow2_C_27
	.hidden	divmod_pow2__mod_pow2_C_27
	.type	divmod_pow2__mod_pow2_C_27, #function
	.proc	016
divmod_pow2__mod_pow2_C_27:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(134216704), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_C_27, .-divmod_pow2__mod_pow2_C_27
	.align 4
	.global divmod_pow2__div_pow2_C_28
	.hidden	divmod_pow2__div_pow2_C_28
	.type	divmod_pow2__div_pow2_C_28, #function
	.proc	016
divmod_pow2__div_pow2_C_28:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 28, %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bge	%icc, .LL1038
	 nop
	sethi	%hi(33792), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL1038:
	ld	[%fp-24], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_C_28, .-divmod_pow2__div_pow2_C_28
	.align 4
	.global divmod_pow2__mod_pow2_C_28
	.hidden	divmod_pow2__mod_pow2_C_28
	.type	divmod_pow2__mod_pow2_C_28, #function
	.proc	016
divmod_pow2__mod_pow2_C_28:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(268434432), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_C_28, .-divmod_pow2__mod_pow2_C_28
	.align 4
	.global divmod_pow2__div_pow2_C_29
	.hidden	divmod_pow2__div_pow2_C_29
	.type	divmod_pow2__div_pow2_C_29, #function
	.proc	016
divmod_pow2__div_pow2_C_29:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 29, %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bge	%icc, .LL1043
	 nop
	sethi	%hi(33792), %g1
	or	%g1, 353, %o0
	call	_m3_fault, 0
	 nop
.LL1043:
	ld	[%fp-24], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_C_29, .-divmod_pow2__div_pow2_C_29
	.align 4
	.global divmod_pow2__mod_pow2_C_29
	.hidden	divmod_pow2__mod_pow2_C_29
	.type	divmod_pow2__mod_pow2_C_29, #function
	.proc	016
divmod_pow2__mod_pow2_C_29:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(536869888), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_C_29, .-divmod_pow2__mod_pow2_C_29
	.align 4
	.global divmod_pow2__div_pow2_C_30
	.hidden	divmod_pow2__div_pow2_C_30
	.type	divmod_pow2__div_pow2_C_30, #function
	.proc	016
divmod_pow2__div_pow2_C_30:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sra	%g1, 30, %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bge	%icc, .LL1048
	 nop
	sethi	%hi(33792), %g1
	or	%g1, 481, %o0
	call	_m3_fault, 0
	 nop
.LL1048:
	ld	[%fp-24], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_C_30, .-divmod_pow2__div_pow2_C_30
	.align 4
	.global divmod_pow2__mod_pow2_C_30
	.hidden	divmod_pow2__mod_pow2_C_30
	.type	divmod_pow2__mod_pow2_C_30, #function
	.proc	016
divmod_pow2__mod_pow2_C_30:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g2
	sethi	%hi(1073740800), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_C_30, .-divmod_pow2__mod_pow2_C_30
	.align 4
	.global divmod_pow2__div_pow2_C_31
	.hidden	divmod_pow2__div_pow2_C_31
	.type	divmod_pow2__div_pow2_C_31, #function
	.proc	016
divmod_pow2__div_pow2_C_31:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sethi	%hi(-2147483648), %o0
	mov	%g1, %o1
	call	m3_div, 0
	 nop
	mov	%o0, %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bge	%icc, .LL1053
	 nop
	sethi	%hi(33792), %g1
	or	%g1, 609, %o0
	call	_m3_fault, 0
	 nop
.LL1053:
	ld	[%fp-24], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_C_31, .-divmod_pow2__div_pow2_C_31
	.align 4
	.global divmod_pow2__mod_pow2_C_31
	.hidden	divmod_pow2__mod_pow2_C_31
	.type	divmod_pow2__mod_pow2_C_31, #function
	.proc	016
divmod_pow2__mod_pow2_C_31:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sethi	%hi(-2147483648), %o0
	mov	%g1, %o1
	call	m3_mod, 0
	 nop
	mov	%o0, %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bge	%icc, .LL1056
	 nop
	sethi	%hi(33792), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1056:
	ld	[%fp-24], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_C_31, .-divmod_pow2__mod_pow2_C_31
	.align 4
	.global divmod_pow2__div_pow2_L_0
	.hidden	divmod_pow2__div_pow2_L_0
	.type	divmod_pow2__div_pow2_L_0, #function
	.proc	05
divmod_pow2__div_pow2_L_0:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_0, .-divmod_pow2__div_pow2_L_0
	.align 4
	.global divmod_pow2__mod_pow2_L_0
	.hidden	divmod_pow2__mod_pow2_L_0
	.type	divmod_pow2__mod_pow2_L_0, #function
	.proc	05
divmod_pow2__mod_pow2_L_0:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	mov	0, %g2
	mov	0, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_0, .-divmod_pow2__mod_pow2_L_0
	.align 4
	.global divmod_pow2__div_pow2_L_1
	.hidden	divmod_pow2__div_pow2_L_1
	.type	divmod_pow2__div_pow2_L_1, #function
	.proc	05
divmod_pow2__div_pow2_L_1:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 1, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_1, .-divmod_pow2__div_pow2_L_1
	.align 4
	.global divmod_pow2__mod_pow2_L_1
	.hidden	divmod_pow2__mod_pow2_L_1
	.type	divmod_pow2__mod_pow2_L_1, #function
	.proc	05
divmod_pow2__mod_pow2_L_1:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 1, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_1, .-divmod_pow2__mod_pow2_L_1
	.align 4
	.global divmod_pow2__div_pow2_L_2
	.hidden	divmod_pow2__div_pow2_L_2
	.type	divmod_pow2__div_pow2_L_2, #function
	.proc	05
divmod_pow2__div_pow2_L_2:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 2, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_2, .-divmod_pow2__div_pow2_L_2
	.align 4
	.global divmod_pow2__mod_pow2_L_2
	.hidden	divmod_pow2__mod_pow2_L_2
	.type	divmod_pow2__mod_pow2_L_2, #function
	.proc	05
divmod_pow2__mod_pow2_L_2:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_2, .-divmod_pow2__mod_pow2_L_2
	.align 4
	.global divmod_pow2__div_pow2_L_3
	.hidden	divmod_pow2__div_pow2_L_3
	.type	divmod_pow2__div_pow2_L_3, #function
	.proc	05
divmod_pow2__div_pow2_L_3:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 3, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_3, .-divmod_pow2__div_pow2_L_3
	.align 4
	.global divmod_pow2__mod_pow2_L_3
	.hidden	divmod_pow2__mod_pow2_L_3
	.type	divmod_pow2__mod_pow2_L_3, #function
	.proc	05
divmod_pow2__mod_pow2_L_3:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 7, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_3, .-divmod_pow2__mod_pow2_L_3
	.align 4
	.global divmod_pow2__div_pow2_L_4
	.hidden	divmod_pow2__div_pow2_L_4
	.type	divmod_pow2__div_pow2_L_4, #function
	.proc	05
divmod_pow2__div_pow2_L_4:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 4, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_4, .-divmod_pow2__div_pow2_L_4
	.align 4
	.global divmod_pow2__mod_pow2_L_4
	.hidden	divmod_pow2__mod_pow2_L_4
	.type	divmod_pow2__mod_pow2_L_4, #function
	.proc	05
divmod_pow2__mod_pow2_L_4:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 15, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_4, .-divmod_pow2__mod_pow2_L_4
	.align 4
	.global divmod_pow2__div_pow2_L_5
	.hidden	divmod_pow2__div_pow2_L_5
	.type	divmod_pow2__div_pow2_L_5, #function
	.proc	05
divmod_pow2__div_pow2_L_5:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 5, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_5, .-divmod_pow2__div_pow2_L_5
	.align 4
	.global divmod_pow2__mod_pow2_L_5
	.hidden	divmod_pow2__mod_pow2_L_5
	.type	divmod_pow2__mod_pow2_L_5, #function
	.proc	05
divmod_pow2__mod_pow2_L_5:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 31, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_5, .-divmod_pow2__mod_pow2_L_5
	.align 4
	.global divmod_pow2__div_pow2_L_6
	.hidden	divmod_pow2__div_pow2_L_6
	.type	divmod_pow2__div_pow2_L_6, #function
	.proc	05
divmod_pow2__div_pow2_L_6:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 6, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_6, .-divmod_pow2__div_pow2_L_6
	.align 4
	.global divmod_pow2__mod_pow2_L_6
	.hidden	divmod_pow2__mod_pow2_L_6
	.type	divmod_pow2__mod_pow2_L_6, #function
	.proc	05
divmod_pow2__mod_pow2_L_6:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 63, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_6, .-divmod_pow2__mod_pow2_L_6
	.align 4
	.global divmod_pow2__div_pow2_L_7
	.hidden	divmod_pow2__div_pow2_L_7
	.type	divmod_pow2__div_pow2_L_7, #function
	.proc	05
divmod_pow2__div_pow2_L_7:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 7, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_7, .-divmod_pow2__div_pow2_L_7
	.align 4
	.global divmod_pow2__mod_pow2_L_7
	.hidden	divmod_pow2__mod_pow2_L_7
	.type	divmod_pow2__mod_pow2_L_7, #function
	.proc	05
divmod_pow2__mod_pow2_L_7:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 127, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_7, .-divmod_pow2__mod_pow2_L_7
	.align 4
	.global divmod_pow2__div_pow2_L_8
	.hidden	divmod_pow2__div_pow2_L_8
	.type	divmod_pow2__div_pow2_L_8, #function
	.proc	05
divmod_pow2__div_pow2_L_8:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 8, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_8, .-divmod_pow2__div_pow2_L_8
	.align 4
	.global divmod_pow2__mod_pow2_L_8
	.hidden	divmod_pow2__mod_pow2_L_8
	.type	divmod_pow2__mod_pow2_L_8, #function
	.proc	05
divmod_pow2__mod_pow2_L_8:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 255, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_8, .-divmod_pow2__mod_pow2_L_8
	.align 4
	.global divmod_pow2__div_pow2_L_9
	.hidden	divmod_pow2__div_pow2_L_9
	.type	divmod_pow2__div_pow2_L_9, #function
	.proc	05
divmod_pow2__div_pow2_L_9:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 9, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_9, .-divmod_pow2__div_pow2_L_9
	.align 4
	.global divmod_pow2__mod_pow2_L_9
	.hidden	divmod_pow2__mod_pow2_L_9
	.type	divmod_pow2__mod_pow2_L_9, #function
	.proc	05
divmod_pow2__mod_pow2_L_9:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 511, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_9, .-divmod_pow2__mod_pow2_L_9
	.align 4
	.global divmod_pow2__div_pow2_L_10
	.hidden	divmod_pow2__div_pow2_L_10
	.type	divmod_pow2__div_pow2_L_10, #function
	.proc	05
divmod_pow2__div_pow2_L_10:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 10, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_10, .-divmod_pow2__div_pow2_L_10
	.align 4
	.global divmod_pow2__mod_pow2_L_10
	.hidden	divmod_pow2__mod_pow2_L_10
	.type	divmod_pow2__mod_pow2_L_10, #function
	.proc	05
divmod_pow2__mod_pow2_L_10:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 1023, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_10, .-divmod_pow2__mod_pow2_L_10
	.align 4
	.global divmod_pow2__div_pow2_L_11
	.hidden	divmod_pow2__div_pow2_L_11
	.type	divmod_pow2__div_pow2_L_11, #function
	.proc	05
divmod_pow2__div_pow2_L_11:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 11, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_11, .-divmod_pow2__div_pow2_L_11
	.align 4
	.global divmod_pow2__mod_pow2_L_11
	.hidden	divmod_pow2__mod_pow2_L_11
	.type	divmod_pow2__mod_pow2_L_11, #function
	.proc	05
divmod_pow2__mod_pow2_L_11:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 2047, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_11, .-divmod_pow2__mod_pow2_L_11
	.align 4
	.global divmod_pow2__div_pow2_L_12
	.hidden	divmod_pow2__div_pow2_L_12
	.type	divmod_pow2__div_pow2_L_12, #function
	.proc	05
divmod_pow2__div_pow2_L_12:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 12, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_12, .-divmod_pow2__div_pow2_L_12
	.align 4
	.global divmod_pow2__mod_pow2_L_12
	.hidden	divmod_pow2__mod_pow2_L_12
	.type	divmod_pow2__mod_pow2_L_12, #function
	.proc	05
divmod_pow2__mod_pow2_L_12:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 4095, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_12, .-divmod_pow2__mod_pow2_L_12
	.align 4
	.global divmod_pow2__div_pow2_L_13
	.hidden	divmod_pow2__div_pow2_L_13
	.type	divmod_pow2__div_pow2_L_13, #function
	.proc	05
divmod_pow2__div_pow2_L_13:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 13, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_13, .-divmod_pow2__div_pow2_L_13
	.align 4
	.global divmod_pow2__mod_pow2_L_13
	.hidden	divmod_pow2__mod_pow2_L_13
	.type	divmod_pow2__mod_pow2_L_13, #function
	.proc	05
divmod_pow2__mod_pow2_L_13:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(7168), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_13, .-divmod_pow2__mod_pow2_L_13
	.align 4
	.global divmod_pow2__div_pow2_L_14
	.hidden	divmod_pow2__div_pow2_L_14
	.type	divmod_pow2__div_pow2_L_14, #function
	.proc	05
divmod_pow2__div_pow2_L_14:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 14, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_14, .-divmod_pow2__div_pow2_L_14
	.align 4
	.global divmod_pow2__mod_pow2_L_14
	.hidden	divmod_pow2__mod_pow2_L_14
	.type	divmod_pow2__mod_pow2_L_14, #function
	.proc	05
divmod_pow2__mod_pow2_L_14:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(15360), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_14, .-divmod_pow2__mod_pow2_L_14
	.align 4
	.global divmod_pow2__div_pow2_L_15
	.hidden	divmod_pow2__div_pow2_L_15
	.type	divmod_pow2__div_pow2_L_15, #function
	.proc	05
divmod_pow2__div_pow2_L_15:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 15, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_15, .-divmod_pow2__div_pow2_L_15
	.align 4
	.global divmod_pow2__mod_pow2_L_15
	.hidden	divmod_pow2__mod_pow2_L_15
	.type	divmod_pow2__mod_pow2_L_15, #function
	.proc	05
divmod_pow2__mod_pow2_L_15:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(31744), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_15, .-divmod_pow2__mod_pow2_L_15
	.align 4
	.global divmod_pow2__div_pow2_L_16
	.hidden	divmod_pow2__div_pow2_L_16
	.type	divmod_pow2__div_pow2_L_16, #function
	.proc	05
divmod_pow2__div_pow2_L_16:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 16, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_16, .-divmod_pow2__div_pow2_L_16
	.align 4
	.global divmod_pow2__mod_pow2_L_16
	.hidden	divmod_pow2__mod_pow2_L_16
	.type	divmod_pow2__mod_pow2_L_16, #function
	.proc	05
divmod_pow2__mod_pow2_L_16:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(64512), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_16, .-divmod_pow2__mod_pow2_L_16
	.align 4
	.global divmod_pow2__div_pow2_L_17
	.hidden	divmod_pow2__div_pow2_L_17
	.type	divmod_pow2__div_pow2_L_17, #function
	.proc	05
divmod_pow2__div_pow2_L_17:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 17, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_17, .-divmod_pow2__div_pow2_L_17
	.align 4
	.global divmod_pow2__mod_pow2_L_17
	.hidden	divmod_pow2__mod_pow2_L_17
	.type	divmod_pow2__mod_pow2_L_17, #function
	.proc	05
divmod_pow2__mod_pow2_L_17:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(130048), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_17, .-divmod_pow2__mod_pow2_L_17
	.align 4
	.global divmod_pow2__div_pow2_L_18
	.hidden	divmod_pow2__div_pow2_L_18
	.type	divmod_pow2__div_pow2_L_18, #function
	.proc	05
divmod_pow2__div_pow2_L_18:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 18, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_18, .-divmod_pow2__div_pow2_L_18
	.align 4
	.global divmod_pow2__mod_pow2_L_18
	.hidden	divmod_pow2__mod_pow2_L_18
	.type	divmod_pow2__mod_pow2_L_18, #function
	.proc	05
divmod_pow2__mod_pow2_L_18:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(261120), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_18, .-divmod_pow2__mod_pow2_L_18
	.align 4
	.global divmod_pow2__div_pow2_L_19
	.hidden	divmod_pow2__div_pow2_L_19
	.type	divmod_pow2__div_pow2_L_19, #function
	.proc	05
divmod_pow2__div_pow2_L_19:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 19, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_19, .-divmod_pow2__div_pow2_L_19
	.align 4
	.global divmod_pow2__mod_pow2_L_19
	.hidden	divmod_pow2__mod_pow2_L_19
	.type	divmod_pow2__mod_pow2_L_19, #function
	.proc	05
divmod_pow2__mod_pow2_L_19:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(523264), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_19, .-divmod_pow2__mod_pow2_L_19
	.align 4
	.global divmod_pow2__div_pow2_L_20
	.hidden	divmod_pow2__div_pow2_L_20
	.type	divmod_pow2__div_pow2_L_20, #function
	.proc	05
divmod_pow2__div_pow2_L_20:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 20, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_20, .-divmod_pow2__div_pow2_L_20
	.align 4
	.global divmod_pow2__mod_pow2_L_20
	.hidden	divmod_pow2__mod_pow2_L_20
	.type	divmod_pow2__mod_pow2_L_20, #function
	.proc	05
divmod_pow2__mod_pow2_L_20:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(1047552), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_20, .-divmod_pow2__mod_pow2_L_20
	.align 4
	.global divmod_pow2__div_pow2_L_21
	.hidden	divmod_pow2__div_pow2_L_21
	.type	divmod_pow2__div_pow2_L_21, #function
	.proc	05
divmod_pow2__div_pow2_L_21:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 21, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_21, .-divmod_pow2__div_pow2_L_21
	.align 4
	.global divmod_pow2__mod_pow2_L_21
	.hidden	divmod_pow2__mod_pow2_L_21
	.type	divmod_pow2__mod_pow2_L_21, #function
	.proc	05
divmod_pow2__mod_pow2_L_21:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(2096128), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_21, .-divmod_pow2__mod_pow2_L_21
	.align 4
	.global divmod_pow2__div_pow2_L_22
	.hidden	divmod_pow2__div_pow2_L_22
	.type	divmod_pow2__div_pow2_L_22, #function
	.proc	05
divmod_pow2__div_pow2_L_22:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 22, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_22, .-divmod_pow2__div_pow2_L_22
	.align 4
	.global divmod_pow2__mod_pow2_L_22
	.hidden	divmod_pow2__mod_pow2_L_22
	.type	divmod_pow2__mod_pow2_L_22, #function
	.proc	05
divmod_pow2__mod_pow2_L_22:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(4193280), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_22, .-divmod_pow2__mod_pow2_L_22
	.align 4
	.global divmod_pow2__div_pow2_L_23
	.hidden	divmod_pow2__div_pow2_L_23
	.type	divmod_pow2__div_pow2_L_23, #function
	.proc	05
divmod_pow2__div_pow2_L_23:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 23, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_23, .-divmod_pow2__div_pow2_L_23
	.align 4
	.global divmod_pow2__mod_pow2_L_23
	.hidden	divmod_pow2__mod_pow2_L_23
	.type	divmod_pow2__mod_pow2_L_23, #function
	.proc	05
divmod_pow2__mod_pow2_L_23:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(8387584), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_23, .-divmod_pow2__mod_pow2_L_23
	.align 4
	.global divmod_pow2__div_pow2_L_24
	.hidden	divmod_pow2__div_pow2_L_24
	.type	divmod_pow2__div_pow2_L_24, #function
	.proc	05
divmod_pow2__div_pow2_L_24:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 24, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_24, .-divmod_pow2__div_pow2_L_24
	.align 4
	.global divmod_pow2__mod_pow2_L_24
	.hidden	divmod_pow2__mod_pow2_L_24
	.type	divmod_pow2__mod_pow2_L_24, #function
	.proc	05
divmod_pow2__mod_pow2_L_24:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(16776192), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_24, .-divmod_pow2__mod_pow2_L_24
	.align 4
	.global divmod_pow2__div_pow2_L_25
	.hidden	divmod_pow2__div_pow2_L_25
	.type	divmod_pow2__div_pow2_L_25, #function
	.proc	05
divmod_pow2__div_pow2_L_25:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 25, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_25, .-divmod_pow2__div_pow2_L_25
	.align 4
	.global divmod_pow2__mod_pow2_L_25
	.hidden	divmod_pow2__mod_pow2_L_25
	.type	divmod_pow2__mod_pow2_L_25, #function
	.proc	05
divmod_pow2__mod_pow2_L_25:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(33553408), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_25, .-divmod_pow2__mod_pow2_L_25
	.align 4
	.global divmod_pow2__div_pow2_L_26
	.hidden	divmod_pow2__div_pow2_L_26
	.type	divmod_pow2__div_pow2_L_26, #function
	.proc	05
divmod_pow2__div_pow2_L_26:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 26, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_26, .-divmod_pow2__div_pow2_L_26
	.align 4
	.global divmod_pow2__mod_pow2_L_26
	.hidden	divmod_pow2__mod_pow2_L_26
	.type	divmod_pow2__mod_pow2_L_26, #function
	.proc	05
divmod_pow2__mod_pow2_L_26:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(67107840), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_26, .-divmod_pow2__mod_pow2_L_26
	.align 4
	.global divmod_pow2__div_pow2_L_27
	.hidden	divmod_pow2__div_pow2_L_27
	.type	divmod_pow2__div_pow2_L_27, #function
	.proc	05
divmod_pow2__div_pow2_L_27:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 27, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_27, .-divmod_pow2__div_pow2_L_27
	.align 4
	.global divmod_pow2__mod_pow2_L_27
	.hidden	divmod_pow2__mod_pow2_L_27
	.type	divmod_pow2__mod_pow2_L_27, #function
	.proc	05
divmod_pow2__mod_pow2_L_27:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(134216704), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_27, .-divmod_pow2__mod_pow2_L_27
	.align 4
	.global divmod_pow2__div_pow2_L_28
	.hidden	divmod_pow2__div_pow2_L_28
	.type	divmod_pow2__div_pow2_L_28, #function
	.proc	05
divmod_pow2__div_pow2_L_28:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 28, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_28, .-divmod_pow2__div_pow2_L_28
	.align 4
	.global divmod_pow2__mod_pow2_L_28
	.hidden	divmod_pow2__mod_pow2_L_28
	.type	divmod_pow2__mod_pow2_L_28, #function
	.proc	05
divmod_pow2__mod_pow2_L_28:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(268434432), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_28, .-divmod_pow2__mod_pow2_L_28
	.align 4
	.global divmod_pow2__div_pow2_L_29
	.hidden	divmod_pow2__div_pow2_L_29
	.type	divmod_pow2__div_pow2_L_29, #function
	.proc	05
divmod_pow2__div_pow2_L_29:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 29, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_29, .-divmod_pow2__div_pow2_L_29
	.align 4
	.global divmod_pow2__mod_pow2_L_29
	.hidden	divmod_pow2__mod_pow2_L_29
	.type	divmod_pow2__mod_pow2_L_29, #function
	.proc	05
divmod_pow2__mod_pow2_L_29:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(536869888), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_29, .-divmod_pow2__mod_pow2_L_29
	.align 4
	.global divmod_pow2__div_pow2_L_30
	.hidden	divmod_pow2__div_pow2_L_30
	.type	divmod_pow2__div_pow2_L_30, #function
	.proc	05
divmod_pow2__div_pow2_L_30:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 30, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_30, .-divmod_pow2__div_pow2_L_30
	.align 4
	.global divmod_pow2__mod_pow2_L_30
	.hidden	divmod_pow2__mod_pow2_L_30
	.type	divmod_pow2__mod_pow2_L_30, #function
	.proc	05
divmod_pow2__mod_pow2_L_30:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(1073740800), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_30, .-divmod_pow2__mod_pow2_L_30
	.align 4
	.global divmod_pow2__div_pow2_L_31
	.hidden	divmod_pow2__div_pow2_L_31
	.type	divmod_pow2__div_pow2_L_31, #function
	.proc	05
divmod_pow2__div_pow2_L_31:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 31, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_31, .-divmod_pow2__div_pow2_L_31
	.align 4
	.global divmod_pow2__mod_pow2_L_31
	.hidden	divmod_pow2__mod_pow2_L_31
	.type	divmod_pow2__mod_pow2_L_31, #function
	.proc	05
divmod_pow2__mod_pow2_L_31:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(2147482624), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_31, .-divmod_pow2__mod_pow2_L_31
	.align 4
	.global divmod_pow2__div_pow2_L_32
	.hidden	divmod_pow2__div_pow2_L_32
	.type	divmod_pow2__div_pow2_L_32, #function
	.proc	05
divmod_pow2__div_pow2_L_32:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 32, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_32, .-divmod_pow2__div_pow2_L_32
	.align 4
	.global divmod_pow2__mod_pow2_L_32
	.hidden	divmod_pow2__mod_pow2_L_32
	.type	divmod_pow2__mod_pow2_L_32, #function
	.proc	05
divmod_pow2__mod_pow2_L_32:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	0, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_32, .-divmod_pow2__mod_pow2_L_32
	.align 4
	.global divmod_pow2__div_pow2_L_33
	.hidden	divmod_pow2__div_pow2_L_33
	.type	divmod_pow2__div_pow2_L_33, #function
	.proc	05
divmod_pow2__div_pow2_L_33:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 33, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_33, .-divmod_pow2__div_pow2_L_33
	.align 4
	.global divmod_pow2__mod_pow2_L_33
	.hidden	divmod_pow2__mod_pow2_L_33
	.type	divmod_pow2__mod_pow2_L_33, #function
	.proc	05
divmod_pow2__mod_pow2_L_33:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	1, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_33, .-divmod_pow2__mod_pow2_L_33
	.align 4
	.global divmod_pow2__div_pow2_L_34
	.hidden	divmod_pow2__div_pow2_L_34
	.type	divmod_pow2__div_pow2_L_34, #function
	.proc	05
divmod_pow2__div_pow2_L_34:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 34, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_34, .-divmod_pow2__div_pow2_L_34
	.align 4
	.global divmod_pow2__mod_pow2_L_34
	.hidden	divmod_pow2__mod_pow2_L_34
	.type	divmod_pow2__mod_pow2_L_34, #function
	.proc	05
divmod_pow2__mod_pow2_L_34:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	3, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_34, .-divmod_pow2__mod_pow2_L_34
	.align 4
	.global divmod_pow2__div_pow2_L_35
	.hidden	divmod_pow2__div_pow2_L_35
	.type	divmod_pow2__div_pow2_L_35, #function
	.proc	05
divmod_pow2__div_pow2_L_35:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 35, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_35, .-divmod_pow2__div_pow2_L_35
	.align 4
	.global divmod_pow2__mod_pow2_L_35
	.hidden	divmod_pow2__mod_pow2_L_35
	.type	divmod_pow2__mod_pow2_L_35, #function
	.proc	05
divmod_pow2__mod_pow2_L_35:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	7, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_35, .-divmod_pow2__mod_pow2_L_35
	.align 4
	.global divmod_pow2__div_pow2_L_36
	.hidden	divmod_pow2__div_pow2_L_36
	.type	divmod_pow2__div_pow2_L_36, #function
	.proc	05
divmod_pow2__div_pow2_L_36:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 36, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_36, .-divmod_pow2__div_pow2_L_36
	.align 4
	.global divmod_pow2__mod_pow2_L_36
	.hidden	divmod_pow2__mod_pow2_L_36
	.type	divmod_pow2__mod_pow2_L_36, #function
	.proc	05
divmod_pow2__mod_pow2_L_36:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	15, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_36, .-divmod_pow2__mod_pow2_L_36
	.align 4
	.global divmod_pow2__div_pow2_L_37
	.hidden	divmod_pow2__div_pow2_L_37
	.type	divmod_pow2__div_pow2_L_37, #function
	.proc	05
divmod_pow2__div_pow2_L_37:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 37, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_37, .-divmod_pow2__div_pow2_L_37
	.align 4
	.global divmod_pow2__mod_pow2_L_37
	.hidden	divmod_pow2__mod_pow2_L_37
	.type	divmod_pow2__mod_pow2_L_37, #function
	.proc	05
divmod_pow2__mod_pow2_L_37:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	31, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_37, .-divmod_pow2__mod_pow2_L_37
	.align 4
	.global divmod_pow2__div_pow2_L_38
	.hidden	divmod_pow2__div_pow2_L_38
	.type	divmod_pow2__div_pow2_L_38, #function
	.proc	05
divmod_pow2__div_pow2_L_38:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 38, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_38, .-divmod_pow2__div_pow2_L_38
	.align 4
	.global divmod_pow2__mod_pow2_L_38
	.hidden	divmod_pow2__mod_pow2_L_38
	.type	divmod_pow2__mod_pow2_L_38, #function
	.proc	05
divmod_pow2__mod_pow2_L_38:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	63, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_38, .-divmod_pow2__mod_pow2_L_38
	.align 4
	.global divmod_pow2__div_pow2_L_39
	.hidden	divmod_pow2__div_pow2_L_39
	.type	divmod_pow2__div_pow2_L_39, #function
	.proc	05
divmod_pow2__div_pow2_L_39:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 39, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_39, .-divmod_pow2__div_pow2_L_39
	.align 4
	.global divmod_pow2__mod_pow2_L_39
	.hidden	divmod_pow2__mod_pow2_L_39
	.type	divmod_pow2__mod_pow2_L_39, #function
	.proc	05
divmod_pow2__mod_pow2_L_39:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	127, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_39, .-divmod_pow2__mod_pow2_L_39
	.align 4
	.global divmod_pow2__div_pow2_L_40
	.hidden	divmod_pow2__div_pow2_L_40
	.type	divmod_pow2__div_pow2_L_40, #function
	.proc	05
divmod_pow2__div_pow2_L_40:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 40, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_40, .-divmod_pow2__div_pow2_L_40
	.align 4
	.global divmod_pow2__mod_pow2_L_40
	.hidden	divmod_pow2__mod_pow2_L_40
	.type	divmod_pow2__mod_pow2_L_40, #function
	.proc	05
divmod_pow2__mod_pow2_L_40:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	255, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_40, .-divmod_pow2__mod_pow2_L_40
	.align 4
	.global divmod_pow2__div_pow2_L_41
	.hidden	divmod_pow2__div_pow2_L_41
	.type	divmod_pow2__div_pow2_L_41, #function
	.proc	05
divmod_pow2__div_pow2_L_41:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 41, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_41, .-divmod_pow2__div_pow2_L_41
	.align 4
	.global divmod_pow2__mod_pow2_L_41
	.hidden	divmod_pow2__mod_pow2_L_41
	.type	divmod_pow2__mod_pow2_L_41, #function
	.proc	05
divmod_pow2__mod_pow2_L_41:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	511, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_41, .-divmod_pow2__mod_pow2_L_41
	.align 4
	.global divmod_pow2__div_pow2_L_42
	.hidden	divmod_pow2__div_pow2_L_42
	.type	divmod_pow2__div_pow2_L_42, #function
	.proc	05
divmod_pow2__div_pow2_L_42:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 42, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_42, .-divmod_pow2__div_pow2_L_42
	.align 4
	.global divmod_pow2__mod_pow2_L_42
	.hidden	divmod_pow2__mod_pow2_L_42
	.type	divmod_pow2__mod_pow2_L_42, #function
	.proc	05
divmod_pow2__mod_pow2_L_42:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	1023, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_42, .-divmod_pow2__mod_pow2_L_42
	.align 4
	.global divmod_pow2__div_pow2_L_43
	.hidden	divmod_pow2__div_pow2_L_43
	.type	divmod_pow2__div_pow2_L_43, #function
	.proc	05
divmod_pow2__div_pow2_L_43:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 43, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_43, .-divmod_pow2__div_pow2_L_43
	.align 4
	.global divmod_pow2__mod_pow2_L_43
	.hidden	divmod_pow2__mod_pow2_L_43
	.type	divmod_pow2__mod_pow2_L_43, #function
	.proc	05
divmod_pow2__mod_pow2_L_43:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	2047, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_43, .-divmod_pow2__mod_pow2_L_43
	.align 4
	.global divmod_pow2__div_pow2_L_44
	.hidden	divmod_pow2__div_pow2_L_44
	.type	divmod_pow2__div_pow2_L_44, #function
	.proc	05
divmod_pow2__div_pow2_L_44:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 44, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_44, .-divmod_pow2__div_pow2_L_44
	.align 4
	.global divmod_pow2__mod_pow2_L_44
	.hidden	divmod_pow2__mod_pow2_L_44
	.type	divmod_pow2__mod_pow2_L_44, #function
	.proc	05
divmod_pow2__mod_pow2_L_44:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	4095, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_44, .-divmod_pow2__mod_pow2_L_44
	.align 4
	.global divmod_pow2__div_pow2_L_45
	.hidden	divmod_pow2__div_pow2_L_45
	.type	divmod_pow2__div_pow2_L_45, #function
	.proc	05
divmod_pow2__div_pow2_L_45:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 45, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_45, .-divmod_pow2__div_pow2_L_45
	.align 4
	.global divmod_pow2__mod_pow2_L_45
	.hidden	divmod_pow2__mod_pow2_L_45
	.type	divmod_pow2__mod_pow2_L_45, #function
	.proc	05
divmod_pow2__mod_pow2_L_45:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(7168), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_45, .-divmod_pow2__mod_pow2_L_45
	.align 4
	.global divmod_pow2__div_pow2_L_46
	.hidden	divmod_pow2__div_pow2_L_46
	.type	divmod_pow2__div_pow2_L_46, #function
	.proc	05
divmod_pow2__div_pow2_L_46:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 46, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_46, .-divmod_pow2__div_pow2_L_46
	.align 4
	.global divmod_pow2__mod_pow2_L_46
	.hidden	divmod_pow2__mod_pow2_L_46
	.type	divmod_pow2__mod_pow2_L_46, #function
	.proc	05
divmod_pow2__mod_pow2_L_46:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(15360), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_46, .-divmod_pow2__mod_pow2_L_46
	.align 4
	.global divmod_pow2__div_pow2_L_47
	.hidden	divmod_pow2__div_pow2_L_47
	.type	divmod_pow2__div_pow2_L_47, #function
	.proc	05
divmod_pow2__div_pow2_L_47:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 47, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_47, .-divmod_pow2__div_pow2_L_47
	.align 4
	.global divmod_pow2__mod_pow2_L_47
	.hidden	divmod_pow2__mod_pow2_L_47
	.type	divmod_pow2__mod_pow2_L_47, #function
	.proc	05
divmod_pow2__mod_pow2_L_47:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(31744), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_47, .-divmod_pow2__mod_pow2_L_47
	.align 4
	.global divmod_pow2__div_pow2_L_48
	.hidden	divmod_pow2__div_pow2_L_48
	.type	divmod_pow2__div_pow2_L_48, #function
	.proc	05
divmod_pow2__div_pow2_L_48:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 48, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_48, .-divmod_pow2__div_pow2_L_48
	.align 4
	.global divmod_pow2__mod_pow2_L_48
	.hidden	divmod_pow2__mod_pow2_L_48
	.type	divmod_pow2__mod_pow2_L_48, #function
	.proc	05
divmod_pow2__mod_pow2_L_48:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(64512), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_48, .-divmod_pow2__mod_pow2_L_48
	.align 4
	.global divmod_pow2__div_pow2_L_49
	.hidden	divmod_pow2__div_pow2_L_49
	.type	divmod_pow2__div_pow2_L_49, #function
	.proc	05
divmod_pow2__div_pow2_L_49:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 49, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_49, .-divmod_pow2__div_pow2_L_49
	.align 4
	.global divmod_pow2__mod_pow2_L_49
	.hidden	divmod_pow2__mod_pow2_L_49
	.type	divmod_pow2__mod_pow2_L_49, #function
	.proc	05
divmod_pow2__mod_pow2_L_49:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(130048), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_49, .-divmod_pow2__mod_pow2_L_49
	.align 4
	.global divmod_pow2__div_pow2_L_50
	.hidden	divmod_pow2__div_pow2_L_50
	.type	divmod_pow2__div_pow2_L_50, #function
	.proc	05
divmod_pow2__div_pow2_L_50:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 50, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_50, .-divmod_pow2__div_pow2_L_50
	.align 4
	.global divmod_pow2__mod_pow2_L_50
	.hidden	divmod_pow2__mod_pow2_L_50
	.type	divmod_pow2__mod_pow2_L_50, #function
	.proc	05
divmod_pow2__mod_pow2_L_50:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(261120), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_50, .-divmod_pow2__mod_pow2_L_50
	.align 4
	.global divmod_pow2__div_pow2_L_51
	.hidden	divmod_pow2__div_pow2_L_51
	.type	divmod_pow2__div_pow2_L_51, #function
	.proc	05
divmod_pow2__div_pow2_L_51:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 51, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_51, .-divmod_pow2__div_pow2_L_51
	.align 4
	.global divmod_pow2__mod_pow2_L_51
	.hidden	divmod_pow2__mod_pow2_L_51
	.type	divmod_pow2__mod_pow2_L_51, #function
	.proc	05
divmod_pow2__mod_pow2_L_51:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(523264), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_51, .-divmod_pow2__mod_pow2_L_51
	.align 4
	.global divmod_pow2__div_pow2_L_52
	.hidden	divmod_pow2__div_pow2_L_52
	.type	divmod_pow2__div_pow2_L_52, #function
	.proc	05
divmod_pow2__div_pow2_L_52:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 52, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_52, .-divmod_pow2__div_pow2_L_52
	.align 4
	.global divmod_pow2__mod_pow2_L_52
	.hidden	divmod_pow2__mod_pow2_L_52
	.type	divmod_pow2__mod_pow2_L_52, #function
	.proc	05
divmod_pow2__mod_pow2_L_52:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(1047552), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_52, .-divmod_pow2__mod_pow2_L_52
	.align 4
	.global divmod_pow2__div_pow2_L_53
	.hidden	divmod_pow2__div_pow2_L_53
	.type	divmod_pow2__div_pow2_L_53, #function
	.proc	05
divmod_pow2__div_pow2_L_53:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 53, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_53, .-divmod_pow2__div_pow2_L_53
	.align 4
	.global divmod_pow2__mod_pow2_L_53
	.hidden	divmod_pow2__mod_pow2_L_53
	.type	divmod_pow2__mod_pow2_L_53, #function
	.proc	05
divmod_pow2__mod_pow2_L_53:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(2096128), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_53, .-divmod_pow2__mod_pow2_L_53
	.align 4
	.global divmod_pow2__div_pow2_L_54
	.hidden	divmod_pow2__div_pow2_L_54
	.type	divmod_pow2__div_pow2_L_54, #function
	.proc	05
divmod_pow2__div_pow2_L_54:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 54, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_54, .-divmod_pow2__div_pow2_L_54
	.align 4
	.global divmod_pow2__mod_pow2_L_54
	.hidden	divmod_pow2__mod_pow2_L_54
	.type	divmod_pow2__mod_pow2_L_54, #function
	.proc	05
divmod_pow2__mod_pow2_L_54:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(4193280), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_54, .-divmod_pow2__mod_pow2_L_54
	.align 4
	.global divmod_pow2__div_pow2_L_55
	.hidden	divmod_pow2__div_pow2_L_55
	.type	divmod_pow2__div_pow2_L_55, #function
	.proc	05
divmod_pow2__div_pow2_L_55:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 55, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_55, .-divmod_pow2__div_pow2_L_55
	.align 4
	.global divmod_pow2__mod_pow2_L_55
	.hidden	divmod_pow2__mod_pow2_L_55
	.type	divmod_pow2__mod_pow2_L_55, #function
	.proc	05
divmod_pow2__mod_pow2_L_55:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(8387584), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_55, .-divmod_pow2__mod_pow2_L_55
	.align 4
	.global divmod_pow2__div_pow2_L_56
	.hidden	divmod_pow2__div_pow2_L_56
	.type	divmod_pow2__div_pow2_L_56, #function
	.proc	05
divmod_pow2__div_pow2_L_56:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 56, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_56, .-divmod_pow2__div_pow2_L_56
	.align 4
	.global divmod_pow2__mod_pow2_L_56
	.hidden	divmod_pow2__mod_pow2_L_56
	.type	divmod_pow2__mod_pow2_L_56, #function
	.proc	05
divmod_pow2__mod_pow2_L_56:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(16776192), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_56, .-divmod_pow2__mod_pow2_L_56
	.align 4
	.global divmod_pow2__div_pow2_L_57
	.hidden	divmod_pow2__div_pow2_L_57
	.type	divmod_pow2__div_pow2_L_57, #function
	.proc	05
divmod_pow2__div_pow2_L_57:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 57, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_57, .-divmod_pow2__div_pow2_L_57
	.align 4
	.global divmod_pow2__mod_pow2_L_57
	.hidden	divmod_pow2__mod_pow2_L_57
	.type	divmod_pow2__mod_pow2_L_57, #function
	.proc	05
divmod_pow2__mod_pow2_L_57:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(33553408), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_57, .-divmod_pow2__mod_pow2_L_57
	.align 4
	.global divmod_pow2__div_pow2_L_58
	.hidden	divmod_pow2__div_pow2_L_58
	.type	divmod_pow2__div_pow2_L_58, #function
	.proc	05
divmod_pow2__div_pow2_L_58:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 58, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_58, .-divmod_pow2__div_pow2_L_58
	.align 4
	.global divmod_pow2__mod_pow2_L_58
	.hidden	divmod_pow2__mod_pow2_L_58
	.type	divmod_pow2__mod_pow2_L_58, #function
	.proc	05
divmod_pow2__mod_pow2_L_58:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(67107840), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_58, .-divmod_pow2__mod_pow2_L_58
	.align 4
	.global divmod_pow2__div_pow2_L_59
	.hidden	divmod_pow2__div_pow2_L_59
	.type	divmod_pow2__div_pow2_L_59, #function
	.proc	05
divmod_pow2__div_pow2_L_59:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 59, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_59, .-divmod_pow2__div_pow2_L_59
	.align 4
	.global divmod_pow2__mod_pow2_L_59
	.hidden	divmod_pow2__mod_pow2_L_59
	.type	divmod_pow2__mod_pow2_L_59, #function
	.proc	05
divmod_pow2__mod_pow2_L_59:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(134216704), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_59, .-divmod_pow2__mod_pow2_L_59
	.align 4
	.global divmod_pow2__div_pow2_L_60
	.hidden	divmod_pow2__div_pow2_L_60
	.type	divmod_pow2__div_pow2_L_60, #function
	.proc	05
divmod_pow2__div_pow2_L_60:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 60, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_60, .-divmod_pow2__div_pow2_L_60
	.align 4
	.global divmod_pow2__mod_pow2_L_60
	.hidden	divmod_pow2__mod_pow2_L_60
	.type	divmod_pow2__mod_pow2_L_60, #function
	.proc	05
divmod_pow2__mod_pow2_L_60:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(268434432), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_60, .-divmod_pow2__mod_pow2_L_60
	.align 4
	.global divmod_pow2__div_pow2_L_61
	.hidden	divmod_pow2__div_pow2_L_61
	.type	divmod_pow2__div_pow2_L_61, #function
	.proc	05
divmod_pow2__div_pow2_L_61:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 61, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_61, .-divmod_pow2__div_pow2_L_61
	.align 4
	.global divmod_pow2__mod_pow2_L_61
	.hidden	divmod_pow2__mod_pow2_L_61
	.type	divmod_pow2__mod_pow2_L_61, #function
	.proc	05
divmod_pow2__mod_pow2_L_61:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(536869888), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_61, .-divmod_pow2__mod_pow2_L_61
	.align 4
	.global divmod_pow2__div_pow2_L_62
	.hidden	divmod_pow2__div_pow2_L_62
	.type	divmod_pow2__div_pow2_L_62, #function
	.proc	05
divmod_pow2__div_pow2_L_62:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 62, %g3
	srlx	%g3, 32, %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_62, .-divmod_pow2__div_pow2_L_62
	.align 4
	.global divmod_pow2__mod_pow2_L_62
	.hidden	divmod_pow2__mod_pow2_L_62
	.type	divmod_pow2__mod_pow2_L_62, #function
	.proc	05
divmod_pow2__mod_pow2_L_62:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(1073740800), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_62, .-divmod_pow2__mod_pow2_L_62
	.align 4
	.global divmod_pow2__div_pow2_L_63
	.hidden	divmod_pow2__div_pow2_L_63
	.type	divmod_pow2__div_pow2_L_63, #function
	.proc	05
divmod_pow2__div_pow2_L_63:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sethi	%hi(2147483648), %o0
	mov	0, %o1
	mov	%g2, %o2
	mov	%g3, %o3
	call	m3_divL, 0
	 nop
	mov	%o0, %g2
	mov	%o1, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_L_63, .-divmod_pow2__div_pow2_L_63
	.align 4
	.global divmod_pow2__mod_pow2_L_63
	.hidden	divmod_pow2__mod_pow2_L_63
	.type	divmod_pow2__mod_pow2_L_63, #function
	.proc	05
divmod_pow2__mod_pow2_L_63:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	sethi	%hi(2147483648), %o0
	mov	0, %o1
	mov	%g2, %o2
	mov	%g3, %o3
	call	m3_modL, 0
	 nop
	mov	%o0, %g2
	mov	%o1, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_L_63, .-divmod_pow2__mod_pow2_L_63
	.align 4
	.global divmod_pow2__div_pow2_LC_0
	.hidden	divmod_pow2__div_pow2_LC_0
	.type	divmod_pow2__div_pow2_LC_0, #function
	.proc	017
divmod_pow2__div_pow2_LC_0:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1318
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1315
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1315
	 nop
.LL1318:
	sethi	%hi(41984), %g1
	or	%g1, 737, %o0
	call	_m3_fault, 0
	 nop
.LL1315:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_0, .-divmod_pow2__div_pow2_LC_0
	.align 4
	.global divmod_pow2__mod_pow2_LC_0
	.hidden	divmod_pow2__mod_pow2_LC_0
	.type	divmod_pow2__mod_pow2_LC_0, #function
	.proc	017
divmod_pow2__mod_pow2_LC_0:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	mov	0, %g2
	mov	0, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_0, .-divmod_pow2__mod_pow2_LC_0
	.align 4
	.global divmod_pow2__div_pow2_LC_1
	.hidden	divmod_pow2__div_pow2_LC_1
	.type	divmod_pow2__div_pow2_LC_1, #function
	.proc	017
divmod_pow2__div_pow2_LC_1:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 1, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1325
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1322
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1322
	 nop
.LL1325:
	sethi	%hi(41984), %g1
	or	%g1, 865, %o0
	call	_m3_fault, 0
	 nop
.LL1322:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_1, .-divmod_pow2__div_pow2_LC_1
	.align 4
	.global divmod_pow2__mod_pow2_LC_1
	.hidden	divmod_pow2__mod_pow2_LC_1
	.type	divmod_pow2__mod_pow2_LC_1, #function
	.proc	017
divmod_pow2__mod_pow2_LC_1:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 1, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_1, .-divmod_pow2__mod_pow2_LC_1
	.align 4
	.global divmod_pow2__div_pow2_LC_2
	.hidden	divmod_pow2__div_pow2_LC_2
	.type	divmod_pow2__div_pow2_LC_2, #function
	.proc	017
divmod_pow2__div_pow2_LC_2:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 2, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1332
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1329
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1329
	 nop
.LL1332:
	sethi	%hi(41984), %g1
	or	%g1, 993, %o0
	call	_m3_fault, 0
	 nop
.LL1329:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_2, .-divmod_pow2__div_pow2_LC_2
	.align 4
	.global divmod_pow2__mod_pow2_LC_2
	.hidden	divmod_pow2__mod_pow2_LC_2
	.type	divmod_pow2__mod_pow2_LC_2, #function
	.proc	017
divmod_pow2__mod_pow2_LC_2:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_2, .-divmod_pow2__mod_pow2_LC_2
	.align 4
	.global divmod_pow2__div_pow2_LC_3
	.hidden	divmod_pow2__div_pow2_LC_3
	.type	divmod_pow2__div_pow2_LC_3, #function
	.proc	017
divmod_pow2__div_pow2_LC_3:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 3, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1339
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1336
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1336
	 nop
.LL1339:
	sethi	%hi(43008), %g1
	or	%g1, 97, %o0
	call	_m3_fault, 0
	 nop
.LL1336:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_3, .-divmod_pow2__div_pow2_LC_3
	.align 4
	.global divmod_pow2__mod_pow2_LC_3
	.hidden	divmod_pow2__mod_pow2_LC_3
	.type	divmod_pow2__mod_pow2_LC_3, #function
	.proc	017
divmod_pow2__mod_pow2_LC_3:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 7, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_3, .-divmod_pow2__mod_pow2_LC_3
	.align 4
	.global divmod_pow2__div_pow2_LC_4
	.hidden	divmod_pow2__div_pow2_LC_4
	.type	divmod_pow2__div_pow2_LC_4, #function
	.proc	017
divmod_pow2__div_pow2_LC_4:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 4, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1346
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1343
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1343
	 nop
.LL1346:
	sethi	%hi(43008), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL1343:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_4, .-divmod_pow2__div_pow2_LC_4
	.align 4
	.global divmod_pow2__mod_pow2_LC_4
	.hidden	divmod_pow2__mod_pow2_LC_4
	.type	divmod_pow2__mod_pow2_LC_4, #function
	.proc	017
divmod_pow2__mod_pow2_LC_4:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 15, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_4, .-divmod_pow2__mod_pow2_LC_4
	.align 4
	.global divmod_pow2__div_pow2_LC_5
	.hidden	divmod_pow2__div_pow2_LC_5
	.type	divmod_pow2__div_pow2_LC_5, #function
	.proc	017
divmod_pow2__div_pow2_LC_5:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 5, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1353
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1350
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1350
	 nop
.LL1353:
	sethi	%hi(43008), %g1
	or	%g1, 353, %o0
	call	_m3_fault, 0
	 nop
.LL1350:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_5, .-divmod_pow2__div_pow2_LC_5
	.align 4
	.global divmod_pow2__mod_pow2_LC_5
	.hidden	divmod_pow2__mod_pow2_LC_5
	.type	divmod_pow2__mod_pow2_LC_5, #function
	.proc	017
divmod_pow2__mod_pow2_LC_5:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 31, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_5, .-divmod_pow2__mod_pow2_LC_5
	.align 4
	.global divmod_pow2__div_pow2_LC_6
	.hidden	divmod_pow2__div_pow2_LC_6
	.type	divmod_pow2__div_pow2_LC_6, #function
	.proc	017
divmod_pow2__div_pow2_LC_6:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 6, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1360
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1357
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1357
	 nop
.LL1360:
	sethi	%hi(43008), %g1
	or	%g1, 481, %o0
	call	_m3_fault, 0
	 nop
.LL1357:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_6, .-divmod_pow2__div_pow2_LC_6
	.align 4
	.global divmod_pow2__mod_pow2_LC_6
	.hidden	divmod_pow2__mod_pow2_LC_6
	.type	divmod_pow2__mod_pow2_LC_6, #function
	.proc	017
divmod_pow2__mod_pow2_LC_6:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 63, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_6, .-divmod_pow2__mod_pow2_LC_6
	.align 4
	.global divmod_pow2__div_pow2_LC_7
	.hidden	divmod_pow2__div_pow2_LC_7
	.type	divmod_pow2__div_pow2_LC_7, #function
	.proc	017
divmod_pow2__div_pow2_LC_7:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 7, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1367
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1364
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1364
	 nop
.LL1367:
	sethi	%hi(43008), %g1
	or	%g1, 609, %o0
	call	_m3_fault, 0
	 nop
.LL1364:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_7, .-divmod_pow2__div_pow2_LC_7
	.align 4
	.global divmod_pow2__mod_pow2_LC_7
	.hidden	divmod_pow2__mod_pow2_LC_7
	.type	divmod_pow2__mod_pow2_LC_7, #function
	.proc	017
divmod_pow2__mod_pow2_LC_7:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 127, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_7, .-divmod_pow2__mod_pow2_LC_7
	.align 4
	.global divmod_pow2__div_pow2_LC_8
	.hidden	divmod_pow2__div_pow2_LC_8
	.type	divmod_pow2__div_pow2_LC_8, #function
	.proc	017
divmod_pow2__div_pow2_LC_8:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 8, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1374
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1371
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1371
	 nop
.LL1374:
	sethi	%hi(43008), %g1
	or	%g1, 737, %o0
	call	_m3_fault, 0
	 nop
.LL1371:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_8, .-divmod_pow2__div_pow2_LC_8
	.align 4
	.global divmod_pow2__mod_pow2_LC_8
	.hidden	divmod_pow2__mod_pow2_LC_8
	.type	divmod_pow2__mod_pow2_LC_8, #function
	.proc	017
divmod_pow2__mod_pow2_LC_8:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 255, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_8, .-divmod_pow2__mod_pow2_LC_8
	.align 4
	.global divmod_pow2__div_pow2_LC_9
	.hidden	divmod_pow2__div_pow2_LC_9
	.type	divmod_pow2__div_pow2_LC_9, #function
	.proc	017
divmod_pow2__div_pow2_LC_9:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 9, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1381
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1378
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1378
	 nop
.LL1381:
	sethi	%hi(43008), %g1
	or	%g1, 865, %o0
	call	_m3_fault, 0
	 nop
.LL1378:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_9, .-divmod_pow2__div_pow2_LC_9
	.align 4
	.global divmod_pow2__mod_pow2_LC_9
	.hidden	divmod_pow2__mod_pow2_LC_9
	.type	divmod_pow2__mod_pow2_LC_9, #function
	.proc	017
divmod_pow2__mod_pow2_LC_9:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 511, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_9, .-divmod_pow2__mod_pow2_LC_9
	.align 4
	.global divmod_pow2__div_pow2_LC_10
	.hidden	divmod_pow2__div_pow2_LC_10
	.type	divmod_pow2__div_pow2_LC_10, #function
	.proc	017
divmod_pow2__div_pow2_LC_10:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 10, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1388
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1385
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1385
	 nop
.LL1388:
	sethi	%hi(43008), %g1
	or	%g1, 993, %o0
	call	_m3_fault, 0
	 nop
.LL1385:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_10, .-divmod_pow2__div_pow2_LC_10
	.align 4
	.global divmod_pow2__mod_pow2_LC_10
	.hidden	divmod_pow2__mod_pow2_LC_10
	.type	divmod_pow2__mod_pow2_LC_10, #function
	.proc	017
divmod_pow2__mod_pow2_LC_10:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 1023, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_10, .-divmod_pow2__mod_pow2_LC_10
	.align 4
	.global divmod_pow2__div_pow2_LC_11
	.hidden	divmod_pow2__div_pow2_LC_11
	.type	divmod_pow2__div_pow2_LC_11, #function
	.proc	017
divmod_pow2__div_pow2_LC_11:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 11, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1395
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1392
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1392
	 nop
.LL1395:
	sethi	%hi(44032), %g1
	or	%g1, 97, %o0
	call	_m3_fault, 0
	 nop
.LL1392:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_11, .-divmod_pow2__div_pow2_LC_11
	.align 4
	.global divmod_pow2__mod_pow2_LC_11
	.hidden	divmod_pow2__mod_pow2_LC_11
	.type	divmod_pow2__mod_pow2_LC_11, #function
	.proc	017
divmod_pow2__mod_pow2_LC_11:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 2047, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_11, .-divmod_pow2__mod_pow2_LC_11
	.align 4
	.global divmod_pow2__div_pow2_LC_12
	.hidden	divmod_pow2__div_pow2_LC_12
	.type	divmod_pow2__div_pow2_LC_12, #function
	.proc	017
divmod_pow2__div_pow2_LC_12:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 12, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1402
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1399
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1399
	 nop
.LL1402:
	sethi	%hi(44032), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL1399:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_12, .-divmod_pow2__div_pow2_LC_12
	.align 4
	.global divmod_pow2__mod_pow2_LC_12
	.hidden	divmod_pow2__mod_pow2_LC_12
	.type	divmod_pow2__mod_pow2_LC_12, #function
	.proc	017
divmod_pow2__mod_pow2_LC_12:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	and	%g2, 0, %g2
	and	%g3, 4095, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_12, .-divmod_pow2__mod_pow2_LC_12
	.align 4
	.global divmod_pow2__div_pow2_LC_13
	.hidden	divmod_pow2__div_pow2_LC_13
	.type	divmod_pow2__div_pow2_LC_13, #function
	.proc	017
divmod_pow2__div_pow2_LC_13:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 13, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1409
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1406
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1406
	 nop
.LL1409:
	sethi	%hi(44032), %g1
	or	%g1, 353, %o0
	call	_m3_fault, 0
	 nop
.LL1406:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_13, .-divmod_pow2__div_pow2_LC_13
	.align 4
	.global divmod_pow2__mod_pow2_LC_13
	.hidden	divmod_pow2__mod_pow2_LC_13
	.type	divmod_pow2__mod_pow2_LC_13, #function
	.proc	017
divmod_pow2__mod_pow2_LC_13:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(7168), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_13, .-divmod_pow2__mod_pow2_LC_13
	.align 4
	.global divmod_pow2__div_pow2_LC_14
	.hidden	divmod_pow2__div_pow2_LC_14
	.type	divmod_pow2__div_pow2_LC_14, #function
	.proc	017
divmod_pow2__div_pow2_LC_14:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 14, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1416
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1413
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1413
	 nop
.LL1416:
	sethi	%hi(44032), %g1
	or	%g1, 481, %o0
	call	_m3_fault, 0
	 nop
.LL1413:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_14, .-divmod_pow2__div_pow2_LC_14
	.align 4
	.global divmod_pow2__mod_pow2_LC_14
	.hidden	divmod_pow2__mod_pow2_LC_14
	.type	divmod_pow2__mod_pow2_LC_14, #function
	.proc	017
divmod_pow2__mod_pow2_LC_14:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(15360), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_14, .-divmod_pow2__mod_pow2_LC_14
	.align 4
	.global divmod_pow2__div_pow2_LC_15
	.hidden	divmod_pow2__div_pow2_LC_15
	.type	divmod_pow2__div_pow2_LC_15, #function
	.proc	017
divmod_pow2__div_pow2_LC_15:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 15, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1423
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1420
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1420
	 nop
.LL1423:
	sethi	%hi(44032), %g1
	or	%g1, 609, %o0
	call	_m3_fault, 0
	 nop
.LL1420:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_15, .-divmod_pow2__div_pow2_LC_15
	.align 4
	.global divmod_pow2__mod_pow2_LC_15
	.hidden	divmod_pow2__mod_pow2_LC_15
	.type	divmod_pow2__mod_pow2_LC_15, #function
	.proc	017
divmod_pow2__mod_pow2_LC_15:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(31744), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_15, .-divmod_pow2__mod_pow2_LC_15
	.align 4
	.global divmod_pow2__div_pow2_LC_16
	.hidden	divmod_pow2__div_pow2_LC_16
	.type	divmod_pow2__div_pow2_LC_16, #function
	.proc	017
divmod_pow2__div_pow2_LC_16:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 16, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1430
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1427
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1427
	 nop
.LL1430:
	sethi	%hi(44032), %g1
	or	%g1, 737, %o0
	call	_m3_fault, 0
	 nop
.LL1427:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_16, .-divmod_pow2__div_pow2_LC_16
	.align 4
	.global divmod_pow2__mod_pow2_LC_16
	.hidden	divmod_pow2__mod_pow2_LC_16
	.type	divmod_pow2__mod_pow2_LC_16, #function
	.proc	017
divmod_pow2__mod_pow2_LC_16:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(64512), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_16, .-divmod_pow2__mod_pow2_LC_16
	.align 4
	.global divmod_pow2__div_pow2_LC_17
	.hidden	divmod_pow2__div_pow2_LC_17
	.type	divmod_pow2__div_pow2_LC_17, #function
	.proc	017
divmod_pow2__div_pow2_LC_17:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 17, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1437
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1434
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1434
	 nop
.LL1437:
	sethi	%hi(44032), %g1
	or	%g1, 865, %o0
	call	_m3_fault, 0
	 nop
.LL1434:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_17, .-divmod_pow2__div_pow2_LC_17
	.align 4
	.global divmod_pow2__mod_pow2_LC_17
	.hidden	divmod_pow2__mod_pow2_LC_17
	.type	divmod_pow2__mod_pow2_LC_17, #function
	.proc	017
divmod_pow2__mod_pow2_LC_17:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(130048), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_17, .-divmod_pow2__mod_pow2_LC_17
	.align 4
	.global divmod_pow2__div_pow2_LC_18
	.hidden	divmod_pow2__div_pow2_LC_18
	.type	divmod_pow2__div_pow2_LC_18, #function
	.proc	017
divmod_pow2__div_pow2_LC_18:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 18, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1444
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1441
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1441
	 nop
.LL1444:
	sethi	%hi(44032), %g1
	or	%g1, 993, %o0
	call	_m3_fault, 0
	 nop
.LL1441:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_18, .-divmod_pow2__div_pow2_LC_18
	.align 4
	.global divmod_pow2__mod_pow2_LC_18
	.hidden	divmod_pow2__mod_pow2_LC_18
	.type	divmod_pow2__mod_pow2_LC_18, #function
	.proc	017
divmod_pow2__mod_pow2_LC_18:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(261120), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_18, .-divmod_pow2__mod_pow2_LC_18
	.align 4
	.global divmod_pow2__div_pow2_LC_19
	.hidden	divmod_pow2__div_pow2_LC_19
	.type	divmod_pow2__div_pow2_LC_19, #function
	.proc	017
divmod_pow2__div_pow2_LC_19:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 19, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1451
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1448
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1448
	 nop
.LL1451:
	sethi	%hi(45056), %g1
	or	%g1, 97, %o0
	call	_m3_fault, 0
	 nop
.LL1448:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_19, .-divmod_pow2__div_pow2_LC_19
	.align 4
	.global divmod_pow2__mod_pow2_LC_19
	.hidden	divmod_pow2__mod_pow2_LC_19
	.type	divmod_pow2__mod_pow2_LC_19, #function
	.proc	017
divmod_pow2__mod_pow2_LC_19:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(523264), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_19, .-divmod_pow2__mod_pow2_LC_19
	.align 4
	.global divmod_pow2__div_pow2_LC_20
	.hidden	divmod_pow2__div_pow2_LC_20
	.type	divmod_pow2__div_pow2_LC_20, #function
	.proc	017
divmod_pow2__div_pow2_LC_20:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 20, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1458
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1455
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1455
	 nop
.LL1458:
	sethi	%hi(45056), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL1455:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_20, .-divmod_pow2__div_pow2_LC_20
	.align 4
	.global divmod_pow2__mod_pow2_LC_20
	.hidden	divmod_pow2__mod_pow2_LC_20
	.type	divmod_pow2__mod_pow2_LC_20, #function
	.proc	017
divmod_pow2__mod_pow2_LC_20:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(1047552), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_20, .-divmod_pow2__mod_pow2_LC_20
	.align 4
	.global divmod_pow2__div_pow2_LC_21
	.hidden	divmod_pow2__div_pow2_LC_21
	.type	divmod_pow2__div_pow2_LC_21, #function
	.proc	017
divmod_pow2__div_pow2_LC_21:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 21, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1465
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1462
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1462
	 nop
.LL1465:
	sethi	%hi(45056), %g1
	or	%g1, 353, %o0
	call	_m3_fault, 0
	 nop
.LL1462:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_21, .-divmod_pow2__div_pow2_LC_21
	.align 4
	.global divmod_pow2__mod_pow2_LC_21
	.hidden	divmod_pow2__mod_pow2_LC_21
	.type	divmod_pow2__mod_pow2_LC_21, #function
	.proc	017
divmod_pow2__mod_pow2_LC_21:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(2096128), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_21, .-divmod_pow2__mod_pow2_LC_21
	.align 4
	.global divmod_pow2__div_pow2_LC_22
	.hidden	divmod_pow2__div_pow2_LC_22
	.type	divmod_pow2__div_pow2_LC_22, #function
	.proc	017
divmod_pow2__div_pow2_LC_22:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 22, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1472
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1469
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1469
	 nop
.LL1472:
	sethi	%hi(45056), %g1
	or	%g1, 481, %o0
	call	_m3_fault, 0
	 nop
.LL1469:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_22, .-divmod_pow2__div_pow2_LC_22
	.align 4
	.global divmod_pow2__mod_pow2_LC_22
	.hidden	divmod_pow2__mod_pow2_LC_22
	.type	divmod_pow2__mod_pow2_LC_22, #function
	.proc	017
divmod_pow2__mod_pow2_LC_22:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(4193280), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_22, .-divmod_pow2__mod_pow2_LC_22
	.align 4
	.global divmod_pow2__div_pow2_LC_23
	.hidden	divmod_pow2__div_pow2_LC_23
	.type	divmod_pow2__div_pow2_LC_23, #function
	.proc	017
divmod_pow2__div_pow2_LC_23:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 23, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1479
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1476
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1476
	 nop
.LL1479:
	sethi	%hi(45056), %g1
	or	%g1, 609, %o0
	call	_m3_fault, 0
	 nop
.LL1476:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_23, .-divmod_pow2__div_pow2_LC_23
	.align 4
	.global divmod_pow2__mod_pow2_LC_23
	.hidden	divmod_pow2__mod_pow2_LC_23
	.type	divmod_pow2__mod_pow2_LC_23, #function
	.proc	017
divmod_pow2__mod_pow2_LC_23:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(8387584), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_23, .-divmod_pow2__mod_pow2_LC_23
	.align 4
	.global divmod_pow2__div_pow2_LC_24
	.hidden	divmod_pow2__div_pow2_LC_24
	.type	divmod_pow2__div_pow2_LC_24, #function
	.proc	017
divmod_pow2__div_pow2_LC_24:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 24, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1486
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1483
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1483
	 nop
.LL1486:
	sethi	%hi(45056), %g1
	or	%g1, 737, %o0
	call	_m3_fault, 0
	 nop
.LL1483:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_24, .-divmod_pow2__div_pow2_LC_24
	.align 4
	.global divmod_pow2__mod_pow2_LC_24
	.hidden	divmod_pow2__mod_pow2_LC_24
	.type	divmod_pow2__mod_pow2_LC_24, #function
	.proc	017
divmod_pow2__mod_pow2_LC_24:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(16776192), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_24, .-divmod_pow2__mod_pow2_LC_24
	.align 4
	.global divmod_pow2__div_pow2_LC_25
	.hidden	divmod_pow2__div_pow2_LC_25
	.type	divmod_pow2__div_pow2_LC_25, #function
	.proc	017
divmod_pow2__div_pow2_LC_25:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 25, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1493
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1490
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1490
	 nop
.LL1493:
	sethi	%hi(45056), %g1
	or	%g1, 865, %o0
	call	_m3_fault, 0
	 nop
.LL1490:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_25, .-divmod_pow2__div_pow2_LC_25
	.align 4
	.global divmod_pow2__mod_pow2_LC_25
	.hidden	divmod_pow2__mod_pow2_LC_25
	.type	divmod_pow2__mod_pow2_LC_25, #function
	.proc	017
divmod_pow2__mod_pow2_LC_25:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(33553408), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_25, .-divmod_pow2__mod_pow2_LC_25
	.align 4
	.global divmod_pow2__div_pow2_LC_26
	.hidden	divmod_pow2__div_pow2_LC_26
	.type	divmod_pow2__div_pow2_LC_26, #function
	.proc	017
divmod_pow2__div_pow2_LC_26:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 26, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1500
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1497
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1497
	 nop
.LL1500:
	sethi	%hi(45056), %g1
	or	%g1, 993, %o0
	call	_m3_fault, 0
	 nop
.LL1497:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_26, .-divmod_pow2__div_pow2_LC_26
	.align 4
	.global divmod_pow2__mod_pow2_LC_26
	.hidden	divmod_pow2__mod_pow2_LC_26
	.type	divmod_pow2__mod_pow2_LC_26, #function
	.proc	017
divmod_pow2__mod_pow2_LC_26:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(67107840), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_26, .-divmod_pow2__mod_pow2_LC_26
	.align 4
	.global divmod_pow2__div_pow2_LC_27
	.hidden	divmod_pow2__div_pow2_LC_27
	.type	divmod_pow2__div_pow2_LC_27, #function
	.proc	017
divmod_pow2__div_pow2_LC_27:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 27, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1507
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1504
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1504
	 nop
.LL1507:
	sethi	%hi(46080), %g1
	or	%g1, 97, %o0
	call	_m3_fault, 0
	 nop
.LL1504:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_27, .-divmod_pow2__div_pow2_LC_27
	.align 4
	.global divmod_pow2__mod_pow2_LC_27
	.hidden	divmod_pow2__mod_pow2_LC_27
	.type	divmod_pow2__mod_pow2_LC_27, #function
	.proc	017
divmod_pow2__mod_pow2_LC_27:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(134216704), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_27, .-divmod_pow2__mod_pow2_LC_27
	.align 4
	.global divmod_pow2__div_pow2_LC_28
	.hidden	divmod_pow2__div_pow2_LC_28
	.type	divmod_pow2__div_pow2_LC_28, #function
	.proc	017
divmod_pow2__div_pow2_LC_28:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 28, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1514
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1511
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1511
	 nop
.LL1514:
	sethi	%hi(46080), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL1511:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_28, .-divmod_pow2__div_pow2_LC_28
	.align 4
	.global divmod_pow2__mod_pow2_LC_28
	.hidden	divmod_pow2__mod_pow2_LC_28
	.type	divmod_pow2__mod_pow2_LC_28, #function
	.proc	017
divmod_pow2__mod_pow2_LC_28:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(268434432), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_28, .-divmod_pow2__mod_pow2_LC_28
	.align 4
	.global divmod_pow2__div_pow2_LC_29
	.hidden	divmod_pow2__div_pow2_LC_29
	.type	divmod_pow2__div_pow2_LC_29, #function
	.proc	017
divmod_pow2__div_pow2_LC_29:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 29, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1521
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1518
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1518
	 nop
.LL1521:
	sethi	%hi(46080), %g1
	or	%g1, 353, %o0
	call	_m3_fault, 0
	 nop
.LL1518:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_29, .-divmod_pow2__div_pow2_LC_29
	.align 4
	.global divmod_pow2__mod_pow2_LC_29
	.hidden	divmod_pow2__mod_pow2_LC_29
	.type	divmod_pow2__mod_pow2_LC_29, #function
	.proc	017
divmod_pow2__mod_pow2_LC_29:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(536869888), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_29, .-divmod_pow2__mod_pow2_LC_29
	.align 4
	.global divmod_pow2__div_pow2_LC_30
	.hidden	divmod_pow2__div_pow2_LC_30
	.type	divmod_pow2__div_pow2_LC_30, #function
	.proc	017
divmod_pow2__div_pow2_LC_30:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 30, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1528
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1525
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1525
	 nop
.LL1528:
	sethi	%hi(46080), %g1
	or	%g1, 481, %o0
	call	_m3_fault, 0
	 nop
.LL1525:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_30, .-divmod_pow2__div_pow2_LC_30
	.align 4
	.global divmod_pow2__mod_pow2_LC_30
	.hidden	divmod_pow2__mod_pow2_LC_30
	.type	divmod_pow2__mod_pow2_LC_30, #function
	.proc	017
divmod_pow2__mod_pow2_LC_30:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(1073740800), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_30, .-divmod_pow2__mod_pow2_LC_30
	.align 4
	.global divmod_pow2__div_pow2_LC_31
	.hidden	divmod_pow2__div_pow2_LC_31
	.type	divmod_pow2__div_pow2_LC_31, #function
	.proc	017
divmod_pow2__div_pow2_LC_31:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 31, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1535
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1532
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1532
	 nop
.LL1535:
	sethi	%hi(46080), %g1
	or	%g1, 609, %o0
	call	_m3_fault, 0
	 nop
.LL1532:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_31, .-divmod_pow2__div_pow2_LC_31
	.align 4
	.global divmod_pow2__mod_pow2_LC_31
	.hidden	divmod_pow2__mod_pow2_LC_31
	.type	divmod_pow2__mod_pow2_LC_31, #function
	.proc	017
divmod_pow2__mod_pow2_LC_31:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	mov	0, %g2
	sethi	%hi(2147482624), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_31, .-divmod_pow2__mod_pow2_LC_31
	.align 4
	.global divmod_pow2__div_pow2_LC_32
	.hidden	divmod_pow2__div_pow2_LC_32
	.type	divmod_pow2__div_pow2_LC_32, #function
	.proc	017
divmod_pow2__div_pow2_LC_32:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 32, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1542
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1539
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1539
	 nop
.LL1542:
	sethi	%hi(46080), %g1
	or	%g1, 737, %o0
	call	_m3_fault, 0
	 nop
.LL1539:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_32, .-divmod_pow2__div_pow2_LC_32
	.align 4
	.global divmod_pow2__mod_pow2_LC_32
	.hidden	divmod_pow2__mod_pow2_LC_32
	.type	divmod_pow2__mod_pow2_LC_32, #function
	.proc	017
divmod_pow2__mod_pow2_LC_32:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	0, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_32, .-divmod_pow2__mod_pow2_LC_32
	.align 4
	.global divmod_pow2__div_pow2_LC_33
	.hidden	divmod_pow2__div_pow2_LC_33
	.type	divmod_pow2__div_pow2_LC_33, #function
	.proc	017
divmod_pow2__div_pow2_LC_33:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 33, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1549
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1546
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1546
	 nop
.LL1549:
	sethi	%hi(46080), %g1
	or	%g1, 865, %o0
	call	_m3_fault, 0
	 nop
.LL1546:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_33, .-divmod_pow2__div_pow2_LC_33
	.align 4
	.global divmod_pow2__mod_pow2_LC_33
	.hidden	divmod_pow2__mod_pow2_LC_33
	.type	divmod_pow2__mod_pow2_LC_33, #function
	.proc	017
divmod_pow2__mod_pow2_LC_33:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	1, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_33, .-divmod_pow2__mod_pow2_LC_33
	.align 4
	.global divmod_pow2__div_pow2_LC_34
	.hidden	divmod_pow2__div_pow2_LC_34
	.type	divmod_pow2__div_pow2_LC_34, #function
	.proc	017
divmod_pow2__div_pow2_LC_34:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 34, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1556
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1553
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1553
	 nop
.LL1556:
	sethi	%hi(46080), %g1
	or	%g1, 993, %o0
	call	_m3_fault, 0
	 nop
.LL1553:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_34, .-divmod_pow2__div_pow2_LC_34
	.align 4
	.global divmod_pow2__mod_pow2_LC_34
	.hidden	divmod_pow2__mod_pow2_LC_34
	.type	divmod_pow2__mod_pow2_LC_34, #function
	.proc	017
divmod_pow2__mod_pow2_LC_34:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	3, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_34, .-divmod_pow2__mod_pow2_LC_34
	.align 4
	.global divmod_pow2__div_pow2_LC_35
	.hidden	divmod_pow2__div_pow2_LC_35
	.type	divmod_pow2__div_pow2_LC_35, #function
	.proc	017
divmod_pow2__div_pow2_LC_35:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 35, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1563
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1560
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1560
	 nop
.LL1563:
	sethi	%hi(47104), %g1
	or	%g1, 97, %o0
	call	_m3_fault, 0
	 nop
.LL1560:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_35, .-divmod_pow2__div_pow2_LC_35
	.align 4
	.global divmod_pow2__mod_pow2_LC_35
	.hidden	divmod_pow2__mod_pow2_LC_35
	.type	divmod_pow2__mod_pow2_LC_35, #function
	.proc	017
divmod_pow2__mod_pow2_LC_35:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	7, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_35, .-divmod_pow2__mod_pow2_LC_35
	.align 4
	.global divmod_pow2__div_pow2_LC_36
	.hidden	divmod_pow2__div_pow2_LC_36
	.type	divmod_pow2__div_pow2_LC_36, #function
	.proc	017
divmod_pow2__div_pow2_LC_36:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 36, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1570
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1567
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1567
	 nop
.LL1570:
	sethi	%hi(47104), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL1567:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_36, .-divmod_pow2__div_pow2_LC_36
	.align 4
	.global divmod_pow2__mod_pow2_LC_36
	.hidden	divmod_pow2__mod_pow2_LC_36
	.type	divmod_pow2__mod_pow2_LC_36, #function
	.proc	017
divmod_pow2__mod_pow2_LC_36:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	15, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_36, .-divmod_pow2__mod_pow2_LC_36
	.align 4
	.global divmod_pow2__div_pow2_LC_37
	.hidden	divmod_pow2__div_pow2_LC_37
	.type	divmod_pow2__div_pow2_LC_37, #function
	.proc	017
divmod_pow2__div_pow2_LC_37:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 37, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1577
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1574
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1574
	 nop
.LL1577:
	sethi	%hi(47104), %g1
	or	%g1, 353, %o0
	call	_m3_fault, 0
	 nop
.LL1574:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_37, .-divmod_pow2__div_pow2_LC_37
	.align 4
	.global divmod_pow2__mod_pow2_LC_37
	.hidden	divmod_pow2__mod_pow2_LC_37
	.type	divmod_pow2__mod_pow2_LC_37, #function
	.proc	017
divmod_pow2__mod_pow2_LC_37:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	31, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_37, .-divmod_pow2__mod_pow2_LC_37
	.align 4
	.global divmod_pow2__div_pow2_LC_38
	.hidden	divmod_pow2__div_pow2_LC_38
	.type	divmod_pow2__div_pow2_LC_38, #function
	.proc	017
divmod_pow2__div_pow2_LC_38:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 38, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1584
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1581
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1581
	 nop
.LL1584:
	sethi	%hi(47104), %g1
	or	%g1, 481, %o0
	call	_m3_fault, 0
	 nop
.LL1581:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_38, .-divmod_pow2__div_pow2_LC_38
	.align 4
	.global divmod_pow2__mod_pow2_LC_38
	.hidden	divmod_pow2__mod_pow2_LC_38
	.type	divmod_pow2__mod_pow2_LC_38, #function
	.proc	017
divmod_pow2__mod_pow2_LC_38:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	63, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_38, .-divmod_pow2__mod_pow2_LC_38
	.align 4
	.global divmod_pow2__div_pow2_LC_39
	.hidden	divmod_pow2__div_pow2_LC_39
	.type	divmod_pow2__div_pow2_LC_39, #function
	.proc	017
divmod_pow2__div_pow2_LC_39:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 39, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1591
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1588
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1588
	 nop
.LL1591:
	sethi	%hi(47104), %g1
	or	%g1, 609, %o0
	call	_m3_fault, 0
	 nop
.LL1588:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_39, .-divmod_pow2__div_pow2_LC_39
	.align 4
	.global divmod_pow2__mod_pow2_LC_39
	.hidden	divmod_pow2__mod_pow2_LC_39
	.type	divmod_pow2__mod_pow2_LC_39, #function
	.proc	017
divmod_pow2__mod_pow2_LC_39:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	127, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_39, .-divmod_pow2__mod_pow2_LC_39
	.align 4
	.global divmod_pow2__div_pow2_LC_40
	.hidden	divmod_pow2__div_pow2_LC_40
	.type	divmod_pow2__div_pow2_LC_40, #function
	.proc	017
divmod_pow2__div_pow2_LC_40:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 40, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1598
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1595
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1595
	 nop
.LL1598:
	sethi	%hi(47104), %g1
	or	%g1, 737, %o0
	call	_m3_fault, 0
	 nop
.LL1595:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_40, .-divmod_pow2__div_pow2_LC_40
	.align 4
	.global divmod_pow2__mod_pow2_LC_40
	.hidden	divmod_pow2__mod_pow2_LC_40
	.type	divmod_pow2__mod_pow2_LC_40, #function
	.proc	017
divmod_pow2__mod_pow2_LC_40:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	255, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_40, .-divmod_pow2__mod_pow2_LC_40
	.align 4
	.global divmod_pow2__div_pow2_LC_41
	.hidden	divmod_pow2__div_pow2_LC_41
	.type	divmod_pow2__div_pow2_LC_41, #function
	.proc	017
divmod_pow2__div_pow2_LC_41:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 41, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1605
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1602
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1602
	 nop
.LL1605:
	sethi	%hi(47104), %g1
	or	%g1, 865, %o0
	call	_m3_fault, 0
	 nop
.LL1602:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_41, .-divmod_pow2__div_pow2_LC_41
	.align 4
	.global divmod_pow2__mod_pow2_LC_41
	.hidden	divmod_pow2__mod_pow2_LC_41
	.type	divmod_pow2__mod_pow2_LC_41, #function
	.proc	017
divmod_pow2__mod_pow2_LC_41:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	511, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_41, .-divmod_pow2__mod_pow2_LC_41
	.align 4
	.global divmod_pow2__div_pow2_LC_42
	.hidden	divmod_pow2__div_pow2_LC_42
	.type	divmod_pow2__div_pow2_LC_42, #function
	.proc	017
divmod_pow2__div_pow2_LC_42:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 42, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1612
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1609
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1609
	 nop
.LL1612:
	sethi	%hi(47104), %g1
	or	%g1, 993, %o0
	call	_m3_fault, 0
	 nop
.LL1609:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_42, .-divmod_pow2__div_pow2_LC_42
	.align 4
	.global divmod_pow2__mod_pow2_LC_42
	.hidden	divmod_pow2__mod_pow2_LC_42
	.type	divmod_pow2__mod_pow2_LC_42, #function
	.proc	017
divmod_pow2__mod_pow2_LC_42:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	1023, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_42, .-divmod_pow2__mod_pow2_LC_42
	.align 4
	.global divmod_pow2__div_pow2_LC_43
	.hidden	divmod_pow2__div_pow2_LC_43
	.type	divmod_pow2__div_pow2_LC_43, #function
	.proc	017
divmod_pow2__div_pow2_LC_43:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 43, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1619
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1616
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1616
	 nop
.LL1619:
	sethi	%hi(48128), %g1
	or	%g1, 97, %o0
	call	_m3_fault, 0
	 nop
.LL1616:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_43, .-divmod_pow2__div_pow2_LC_43
	.align 4
	.global divmod_pow2__mod_pow2_LC_43
	.hidden	divmod_pow2__mod_pow2_LC_43
	.type	divmod_pow2__mod_pow2_LC_43, #function
	.proc	017
divmod_pow2__mod_pow2_LC_43:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	2047, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_43, .-divmod_pow2__mod_pow2_LC_43
	.align 4
	.global divmod_pow2__div_pow2_LC_44
	.hidden	divmod_pow2__div_pow2_LC_44
	.type	divmod_pow2__div_pow2_LC_44, #function
	.proc	017
divmod_pow2__div_pow2_LC_44:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 44, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1626
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1623
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1623
	 nop
.LL1626:
	sethi	%hi(48128), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL1623:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_44, .-divmod_pow2__div_pow2_LC_44
	.align 4
	.global divmod_pow2__mod_pow2_LC_44
	.hidden	divmod_pow2__mod_pow2_LC_44
	.type	divmod_pow2__mod_pow2_LC_44, #function
	.proc	017
divmod_pow2__mod_pow2_LC_44:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %g2
	mov	4095, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%g2, %o4, %g2
	and	%g3, %o5, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_44, .-divmod_pow2__mod_pow2_LC_44
	.align 4
	.global divmod_pow2__div_pow2_LC_45
	.hidden	divmod_pow2__div_pow2_LC_45
	.type	divmod_pow2__div_pow2_LC_45, #function
	.proc	017
divmod_pow2__div_pow2_LC_45:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 45, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1633
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1630
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1630
	 nop
.LL1633:
	sethi	%hi(48128), %g1
	or	%g1, 353, %o0
	call	_m3_fault, 0
	 nop
.LL1630:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_45, .-divmod_pow2__div_pow2_LC_45
	.align 4
	.global divmod_pow2__mod_pow2_LC_45
	.hidden	divmod_pow2__mod_pow2_LC_45
	.type	divmod_pow2__mod_pow2_LC_45, #function
	.proc	017
divmod_pow2__mod_pow2_LC_45:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(7168), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_45, .-divmod_pow2__mod_pow2_LC_45
	.align 4
	.global divmod_pow2__div_pow2_LC_46
	.hidden	divmod_pow2__div_pow2_LC_46
	.type	divmod_pow2__div_pow2_LC_46, #function
	.proc	017
divmod_pow2__div_pow2_LC_46:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 46, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1640
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1637
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1637
	 nop
.LL1640:
	sethi	%hi(48128), %g1
	or	%g1, 481, %o0
	call	_m3_fault, 0
	 nop
.LL1637:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_46, .-divmod_pow2__div_pow2_LC_46
	.align 4
	.global divmod_pow2__mod_pow2_LC_46
	.hidden	divmod_pow2__mod_pow2_LC_46
	.type	divmod_pow2__mod_pow2_LC_46, #function
	.proc	017
divmod_pow2__mod_pow2_LC_46:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(15360), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_46, .-divmod_pow2__mod_pow2_LC_46
	.align 4
	.global divmod_pow2__div_pow2_LC_47
	.hidden	divmod_pow2__div_pow2_LC_47
	.type	divmod_pow2__div_pow2_LC_47, #function
	.proc	017
divmod_pow2__div_pow2_LC_47:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 47, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1647
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1644
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1644
	 nop
.LL1647:
	sethi	%hi(48128), %g1
	or	%g1, 609, %o0
	call	_m3_fault, 0
	 nop
.LL1644:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_47, .-divmod_pow2__div_pow2_LC_47
	.align 4
	.global divmod_pow2__mod_pow2_LC_47
	.hidden	divmod_pow2__mod_pow2_LC_47
	.type	divmod_pow2__mod_pow2_LC_47, #function
	.proc	017
divmod_pow2__mod_pow2_LC_47:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(31744), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_47, .-divmod_pow2__mod_pow2_LC_47
	.align 4
	.global divmod_pow2__div_pow2_LC_48
	.hidden	divmod_pow2__div_pow2_LC_48
	.type	divmod_pow2__div_pow2_LC_48, #function
	.proc	017
divmod_pow2__div_pow2_LC_48:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 48, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1654
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1651
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1651
	 nop
.LL1654:
	sethi	%hi(48128), %g1
	or	%g1, 737, %o0
	call	_m3_fault, 0
	 nop
.LL1651:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_48, .-divmod_pow2__div_pow2_LC_48
	.align 4
	.global divmod_pow2__mod_pow2_LC_48
	.hidden	divmod_pow2__mod_pow2_LC_48
	.type	divmod_pow2__mod_pow2_LC_48, #function
	.proc	017
divmod_pow2__mod_pow2_LC_48:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(64512), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_48, .-divmod_pow2__mod_pow2_LC_48
	.align 4
	.global divmod_pow2__div_pow2_LC_49
	.hidden	divmod_pow2__div_pow2_LC_49
	.type	divmod_pow2__div_pow2_LC_49, #function
	.proc	017
divmod_pow2__div_pow2_LC_49:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 49, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1661
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1658
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1658
	 nop
.LL1661:
	sethi	%hi(48128), %g1
	or	%g1, 865, %o0
	call	_m3_fault, 0
	 nop
.LL1658:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_49, .-divmod_pow2__div_pow2_LC_49
	.align 4
	.global divmod_pow2__mod_pow2_LC_49
	.hidden	divmod_pow2__mod_pow2_LC_49
	.type	divmod_pow2__mod_pow2_LC_49, #function
	.proc	017
divmod_pow2__mod_pow2_LC_49:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(130048), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_49, .-divmod_pow2__mod_pow2_LC_49
	.align 4
	.global divmod_pow2__div_pow2_LC_50
	.hidden	divmod_pow2__div_pow2_LC_50
	.type	divmod_pow2__div_pow2_LC_50, #function
	.proc	017
divmod_pow2__div_pow2_LC_50:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 50, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1668
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1665
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1665
	 nop
.LL1668:
	sethi	%hi(48128), %g1
	or	%g1, 993, %o0
	call	_m3_fault, 0
	 nop
.LL1665:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_50, .-divmod_pow2__div_pow2_LC_50
	.align 4
	.global divmod_pow2__mod_pow2_LC_50
	.hidden	divmod_pow2__mod_pow2_LC_50
	.type	divmod_pow2__mod_pow2_LC_50, #function
	.proc	017
divmod_pow2__mod_pow2_LC_50:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(261120), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_50, .-divmod_pow2__mod_pow2_LC_50
	.align 4
	.global divmod_pow2__div_pow2_LC_51
	.hidden	divmod_pow2__div_pow2_LC_51
	.type	divmod_pow2__div_pow2_LC_51, #function
	.proc	017
divmod_pow2__div_pow2_LC_51:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 51, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1675
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1672
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1672
	 nop
.LL1675:
	sethi	%hi(49152), %g1
	or	%g1, 97, %o0
	call	_m3_fault, 0
	 nop
.LL1672:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_51, .-divmod_pow2__div_pow2_LC_51
	.align 4
	.global divmod_pow2__mod_pow2_LC_51
	.hidden	divmod_pow2__mod_pow2_LC_51
	.type	divmod_pow2__mod_pow2_LC_51, #function
	.proc	017
divmod_pow2__mod_pow2_LC_51:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(523264), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_51, .-divmod_pow2__mod_pow2_LC_51
	.align 4
	.global divmod_pow2__div_pow2_LC_52
	.hidden	divmod_pow2__div_pow2_LC_52
	.type	divmod_pow2__div_pow2_LC_52, #function
	.proc	017
divmod_pow2__div_pow2_LC_52:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 52, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1682
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1679
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1679
	 nop
.LL1682:
	sethi	%hi(49152), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL1679:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_52, .-divmod_pow2__div_pow2_LC_52
	.align 4
	.global divmod_pow2__mod_pow2_LC_52
	.hidden	divmod_pow2__mod_pow2_LC_52
	.type	divmod_pow2__mod_pow2_LC_52, #function
	.proc	017
divmod_pow2__mod_pow2_LC_52:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(1047552), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_52, .-divmod_pow2__mod_pow2_LC_52
	.align 4
	.global divmod_pow2__div_pow2_LC_53
	.hidden	divmod_pow2__div_pow2_LC_53
	.type	divmod_pow2__div_pow2_LC_53, #function
	.proc	017
divmod_pow2__div_pow2_LC_53:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 53, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1689
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1686
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1686
	 nop
.LL1689:
	sethi	%hi(49152), %g1
	or	%g1, 353, %o0
	call	_m3_fault, 0
	 nop
.LL1686:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_53, .-divmod_pow2__div_pow2_LC_53
	.align 4
	.global divmod_pow2__mod_pow2_LC_53
	.hidden	divmod_pow2__mod_pow2_LC_53
	.type	divmod_pow2__mod_pow2_LC_53, #function
	.proc	017
divmod_pow2__mod_pow2_LC_53:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(2096128), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_53, .-divmod_pow2__mod_pow2_LC_53
	.align 4
	.global divmod_pow2__div_pow2_LC_54
	.hidden	divmod_pow2__div_pow2_LC_54
	.type	divmod_pow2__div_pow2_LC_54, #function
	.proc	017
divmod_pow2__div_pow2_LC_54:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 54, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1696
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1693
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1693
	 nop
.LL1696:
	sethi	%hi(49152), %g1
	or	%g1, 481, %o0
	call	_m3_fault, 0
	 nop
.LL1693:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_54, .-divmod_pow2__div_pow2_LC_54
	.align 4
	.global divmod_pow2__mod_pow2_LC_54
	.hidden	divmod_pow2__mod_pow2_LC_54
	.type	divmod_pow2__mod_pow2_LC_54, #function
	.proc	017
divmod_pow2__mod_pow2_LC_54:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(4193280), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_54, .-divmod_pow2__mod_pow2_LC_54
	.align 4
	.global divmod_pow2__div_pow2_LC_55
	.hidden	divmod_pow2__div_pow2_LC_55
	.type	divmod_pow2__div_pow2_LC_55, #function
	.proc	017
divmod_pow2__div_pow2_LC_55:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 55, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1703
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1700
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1700
	 nop
.LL1703:
	sethi	%hi(49152), %g1
	or	%g1, 609, %o0
	call	_m3_fault, 0
	 nop
.LL1700:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_55, .-divmod_pow2__div_pow2_LC_55
	.align 4
	.global divmod_pow2__mod_pow2_LC_55
	.hidden	divmod_pow2__mod_pow2_LC_55
	.type	divmod_pow2__mod_pow2_LC_55, #function
	.proc	017
divmod_pow2__mod_pow2_LC_55:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(8387584), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_55, .-divmod_pow2__mod_pow2_LC_55
	.align 4
	.global divmod_pow2__div_pow2_LC_56
	.hidden	divmod_pow2__div_pow2_LC_56
	.type	divmod_pow2__div_pow2_LC_56, #function
	.proc	017
divmod_pow2__div_pow2_LC_56:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 56, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1710
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1707
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1707
	 nop
.LL1710:
	sethi	%hi(49152), %g1
	or	%g1, 737, %o0
	call	_m3_fault, 0
	 nop
.LL1707:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_56, .-divmod_pow2__div_pow2_LC_56
	.align 4
	.global divmod_pow2__mod_pow2_LC_56
	.hidden	divmod_pow2__mod_pow2_LC_56
	.type	divmod_pow2__mod_pow2_LC_56, #function
	.proc	017
divmod_pow2__mod_pow2_LC_56:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(16776192), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_56, .-divmod_pow2__mod_pow2_LC_56
	.align 4
	.global divmod_pow2__div_pow2_LC_57
	.hidden	divmod_pow2__div_pow2_LC_57
	.type	divmod_pow2__div_pow2_LC_57, #function
	.proc	017
divmod_pow2__div_pow2_LC_57:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 57, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1717
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1714
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1714
	 nop
.LL1717:
	sethi	%hi(49152), %g1
	or	%g1, 865, %o0
	call	_m3_fault, 0
	 nop
.LL1714:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_57, .-divmod_pow2__div_pow2_LC_57
	.align 4
	.global divmod_pow2__mod_pow2_LC_57
	.hidden	divmod_pow2__mod_pow2_LC_57
	.type	divmod_pow2__mod_pow2_LC_57, #function
	.proc	017
divmod_pow2__mod_pow2_LC_57:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(33553408), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_57, .-divmod_pow2__mod_pow2_LC_57
	.align 4
	.global divmod_pow2__div_pow2_LC_58
	.hidden	divmod_pow2__div_pow2_LC_58
	.type	divmod_pow2__div_pow2_LC_58, #function
	.proc	017
divmod_pow2__div_pow2_LC_58:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 58, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1724
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1721
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1721
	 nop
.LL1724:
	sethi	%hi(49152), %g1
	or	%g1, 993, %o0
	call	_m3_fault, 0
	 nop
.LL1721:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_58, .-divmod_pow2__div_pow2_LC_58
	.align 4
	.global divmod_pow2__mod_pow2_LC_58
	.hidden	divmod_pow2__mod_pow2_LC_58
	.type	divmod_pow2__mod_pow2_LC_58, #function
	.proc	017
divmod_pow2__mod_pow2_LC_58:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(67107840), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_58, .-divmod_pow2__mod_pow2_LC_58
	.align 4
	.global divmod_pow2__div_pow2_LC_59
	.hidden	divmod_pow2__div_pow2_LC_59
	.type	divmod_pow2__div_pow2_LC_59, #function
	.proc	017
divmod_pow2__div_pow2_LC_59:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 59, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1731
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1728
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1728
	 nop
.LL1731:
	sethi	%hi(50176), %g1
	or	%g1, 97, %o0
	call	_m3_fault, 0
	 nop
.LL1728:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_59, .-divmod_pow2__div_pow2_LC_59
	.align 4
	.global divmod_pow2__mod_pow2_LC_59
	.hidden	divmod_pow2__mod_pow2_LC_59
	.type	divmod_pow2__mod_pow2_LC_59, #function
	.proc	017
divmod_pow2__mod_pow2_LC_59:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(134216704), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_59, .-divmod_pow2__mod_pow2_LC_59
	.align 4
	.global divmod_pow2__div_pow2_LC_60
	.hidden	divmod_pow2__div_pow2_LC_60
	.type	divmod_pow2__div_pow2_LC_60, #function
	.proc	017
divmod_pow2__div_pow2_LC_60:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 60, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1738
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1735
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1735
	 nop
.LL1738:
	sethi	%hi(50176), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL1735:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_60, .-divmod_pow2__div_pow2_LC_60
	.align 4
	.global divmod_pow2__mod_pow2_LC_60
	.hidden	divmod_pow2__mod_pow2_LC_60
	.type	divmod_pow2__mod_pow2_LC_60, #function
	.proc	017
divmod_pow2__mod_pow2_LC_60:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(268434432), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_60, .-divmod_pow2__mod_pow2_LC_60
	.align 4
	.global divmod_pow2__div_pow2_LC_61
	.hidden	divmod_pow2__div_pow2_LC_61
	.type	divmod_pow2__div_pow2_LC_61, #function
	.proc	017
divmod_pow2__div_pow2_LC_61:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 61, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1745
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1742
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1742
	 nop
.LL1745:
	sethi	%hi(50176), %g1
	or	%g1, 353, %o0
	call	_m3_fault, 0
	 nop
.LL1742:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_61, .-divmod_pow2__div_pow2_LC_61
	.align 4
	.global divmod_pow2__mod_pow2_LC_61
	.hidden	divmod_pow2__mod_pow2_LC_61
	.type	divmod_pow2__mod_pow2_LC_61, #function
	.proc	017
divmod_pow2__mod_pow2_LC_61:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(536869888), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_61, .-divmod_pow2__mod_pow2_LC_61
	.align 4
	.global divmod_pow2__div_pow2_LC_62
	.hidden	divmod_pow2__div_pow2_LC_62
	.type	divmod_pow2__div_pow2_LC_62, #function
	.proc	017
divmod_pow2__div_pow2_LC_62:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sllx	%g2, 32, %g2
	or	%g3, %g2, %g2
	srax	%g2, 62, %g3
	srlx	%g3, 32, %g2
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1752
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1749
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1749
	 nop
.LL1752:
	sethi	%hi(50176), %g1
	or	%g1, 481, %o0
	call	_m3_fault, 0
	 nop
.LL1749:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_62, .-divmod_pow2__div_pow2_LC_62
	.align 4
	.global divmod_pow2__mod_pow2_LC_62
	.hidden	divmod_pow2__mod_pow2_LC_62
	.type	divmod_pow2__mod_pow2_LC_62, #function
	.proc	017
divmod_pow2__mod_pow2_LC_62:
	save	%sp, -128, %sp
	std	%i0, [%fp-32]
	ldd	[%fp-32], %i4
	sethi	%hi(1073740800), %g2
	or	%g2, 1023, %g2
	sethi	%hi(4294966272), %g3
	or	%g3, 1023, %g3
	and	%i4, %g2, %g2
	and	%i5, %g3, %g3
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_62, .-divmod_pow2__mod_pow2_LC_62
	.align 4
	.global divmod_pow2__div_pow2_LC_63
	.hidden	divmod_pow2__div_pow2_LC_63
	.type	divmod_pow2__div_pow2_LC_63, #function
	.proc	017
divmod_pow2__div_pow2_LC_63:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sethi	%hi(2147483648), %o0
	mov	0, %o1
	mov	%g2, %o2
	mov	%g3, %o3
	call	m3_divL, 0
	 nop
	mov	%o0, %g2
	mov	%o1, %g3
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1759
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1756
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1756
	 nop
.LL1759:
	sethi	%hi(50176), %g1
	or	%g1, 609, %o0
	call	_m3_fault, 0
	 nop
.LL1756:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__div_pow2_LC_63, .-divmod_pow2__div_pow2_LC_63
	.align 4
	.global divmod_pow2__mod_pow2_LC_63
	.hidden	divmod_pow2__mod_pow2_LC_63
	.type	divmod_pow2__mod_pow2_LC_63, #function
	.proc	017
divmod_pow2__mod_pow2_LC_63:
	save	%sp, -144, %sp
	std	%i0, [%fp-40]
	ldd	[%fp-40], %g2
	sethi	%hi(2147483648), %o0
	mov	0, %o1
	mov	%g2, %o2
	mov	%g3, %o3
	call	m3_modL, 0
	 nop
	mov	%o0, %g2
	mov	%o1, %g3
	st	%g3, [%fp-28]
	st	%g2, [%fp-32]
	ldd	[%fp-32], %g2
	std	%g2, [%fp-48]
	ld	[%fp-48], %g3
	cmp	%g3, 0
	bl	%icc, .LL1764
	 nop
	ld	[%fp-48], %g1
	cmp	%g1, 0
	bne	%icc, .LL1761
	 nop
	ld	[%fp-44], %g2
	cmp	%g2, 0
	ba,pt	%xcc, .LL1761
	 nop
.LL1764:
	sethi	%hi(50176), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1761:
	ldd	[%fp-32], %g2
	mov	%g2, %i0
	mov	%g3, %i1
	return	%i7+8
	 nop
	.size	divmod_pow2__mod_pow2_LC_63, .-divmod_pow2__mod_pow2_LC_63
	.align 4
	.global divmod_pow2_M3
	.type	divmod_pow2_M3, #function
	.proc	0120
divmod_pow2_M3:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sethi	%hi(MM_divmod_pow2), %g1
	or	%g1, %lo(MM_divmod_pow2), %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	divmod_pow2_M3, .-divmod_pow2_M3
	.align 4
	.type	_m3_fault, #function
	.proc	020
_m3_fault:
	save	%sp, -112, %sp
	st	%i0, [%fp+68]
	sethi	%hi(MM_divmod_pow2), %g1
	or	%g1, %lo(MM_divmod_pow2), %o0
	ld	[%fp+68], %o1
	call	RTHooks__ReportFault, 0
	 nop
	return	%i7+8
	 nop
	.size	_m3_fault, .-_m3_fault
	.section	".data"
	.align 8
	.type	L_1, #object
	.size	L_1, 17688
L_1:
	.ascii	"divmod_pow2_M3"
	.skip 1
	.ascii	"mod_pow2_LC_63"
	.skip 1
	.ascii	"div_pow2_LC_63"
	.skip 1
	.ascii	"mod_pow2_LC_62"
	.skip 1
	.ascii	"div_pow2_LC_62"
	.skip 1
	.ascii	"mod_pow2_LC_61"
	.skip 1
	.ascii	"div_pow2_LC_61"
	.skip 1
	.ascii	"mod_pow2_LC_60"
	.skip 1
	.ascii	"div_pow2_LC_60"
	.skip 1
	.ascii	"mod_pow2_LC_59"
	.skip 1
	.ascii	"div_pow2_LC_59"
	.skip 1
	.ascii	"mod_pow2_LC_58"
	.skip 1
	.ascii	"div_pow2_LC_58"
	.skip 1
	.ascii	"mod_pow2_LC_57"
	.skip 1
	.ascii	"div_pow2_LC_57"
	.skip 1
	.ascii	"mod_pow2_LC_56"
	.skip 1
	.ascii	"div_pow2_LC_56"
	.skip 1
	.ascii	"mod_pow2_LC_55"
	.skip 1
	.ascii	"div_pow2_LC_55"
	.skip 1
	.ascii	"mod_pow2_LC_54"
	.skip 1
	.ascii	"div_pow2_LC_54"
	.skip 1
	.ascii	"mod_pow2_LC_53"
	.skip 1
	.ascii	"div_pow2_LC_53"
	.skip 1
	.ascii	"mod_pow2_LC_52"
	.skip 1
	.ascii	"div_pow2_LC_52"
	.skip 1
	.ascii	"mod_pow2_LC_51"
	.skip 1
	.ascii	"div_pow2_LC_51"
	.skip 1
	.ascii	"mod_pow2_LC_50"
	.skip 1
	.ascii	"div_pow2_LC_50"
	.skip 1
	.ascii	"mod_pow2_LC_49"
	.skip 1
	.ascii	"div_pow2_LC_49"
	.skip 1
	.ascii	"mod_pow2_LC_48"
	.skip 1
	.ascii	"div_pow2_LC_48"
	.skip 1
	.ascii	"mod_pow2_LC_47"
	.skip 1
	.ascii	"div_pow2_LC_47"
	.skip 1
	.ascii	"mod_pow2_LC_46"
	.skip 1
	.ascii	"div_pow2_LC_46"
	.skip 1
	.ascii	"mod_pow2_LC_45"
	.skip 1
	.ascii	"div_pow2_LC_45"
	.skip 1
	.ascii	"mod_pow2_LC_44"
	.skip 1
	.ascii	"div_pow2_LC_44"
	.skip 1
	.ascii	"mod_pow2_LC_43"
	.skip 1
	.ascii	"div_pow2_LC_43"
	.skip 1
	.ascii	"mod_pow2_LC_42"
	.skip 1
	.ascii	"div_pow2_LC_42"
	.skip 1
	.ascii	"mod_pow2_LC_41"
	.skip 1
	.ascii	"div_pow2_LC_41"
	.skip 1
	.ascii	"mod_pow2_LC_40"
	.skip 1
	.ascii	"div_pow2_LC_40"
	.skip 1
	.ascii	"mod_pow2_LC_39"
	.skip 1
	.ascii	"div_pow2_LC_39"
	.skip 1
	.ascii	"mod_pow2_LC_38"
	.skip 1
	.ascii	"div_pow2_LC_38"
	.skip 1
	.ascii	"mod_pow2_LC_37"
	.skip 1
	.ascii	"div_pow2_LC_37"
	.skip 1
	.ascii	"mod_pow2_LC_36"
	.skip 1
	.ascii	"div_pow2_LC_36"
	.skip 1
	.ascii	"mod_pow2_LC_35"
	.skip 1
	.ascii	"div_pow2_LC_35"
	.skip 1
	.ascii	"mod_pow2_LC_34"
	.skip 1
	.ascii	"div_pow2_LC_34"
	.skip 1
	.ascii	"mod_pow2_LC_33"
	.skip 1
	.ascii	"div_pow2_LC_33"
	.skip 1
	.ascii	"mod_pow2_LC_32"
	.skip 1
	.ascii	"div_pow2_LC_32"
	.skip 1
	.ascii	"mod_pow2_LC_31"
	.skip 1
	.ascii	"div_pow2_LC_31"
	.skip 1
	.ascii	"mod_pow2_LC_30"
	.skip 1
	.ascii	"div_pow2_LC_30"
	.skip 1
	.ascii	"mod_pow2_LC_29"
	.skip 1
	.ascii	"div_pow2_LC_29"
	.skip 1
	.ascii	"mod_pow2_LC_28"
	.skip 1
	.ascii	"div_pow2_LC_28"
	.skip 1
	.ascii	"mod_pow2_LC_27"
	.skip 1
	.ascii	"div_pow2_LC_27"
	.skip 1
	.ascii	"mod_pow2_LC_26"
	.skip 1
	.ascii	"div_pow2_LC_26"
	.skip 1
	.ascii	"mod_pow2_LC_25"
	.skip 1
	.ascii	"div_pow2_LC_25"
	.skip 1
	.ascii	"mod_pow2_LC_24"
	.skip 1
	.ascii	"div_pow2_LC_24"
	.skip 1
	.ascii	"mod_pow2_LC_23"
	.skip 1
	.ascii	"div_pow2_LC_23"
	.skip 1
	.ascii	"mod_pow2_LC_22"
	.skip 1
	.ascii	"div_pow2_LC_22"
	.skip 1
	.ascii	"mod_pow2_LC_21"
	.skip 1
	.ascii	"div_pow2_LC_21"
	.skip 1
	.ascii	"mod_pow2_LC_20"
	.skip 1
	.ascii	"div_pow2_LC_20"
	.skip 1
	.ascii	"mod_pow2_LC_19"
	.skip 1
	.ascii	"div_pow2_LC_19"
	.skip 1
	.ascii	"mod_pow2_LC_18"
	.skip 1
	.ascii	"div_pow2_LC_18"
	.skip 1
	.ascii	"mod_pow2_LC_17"
	.skip 1
	.ascii	"div_pow2_LC_17"
	.skip 1
	.ascii	"mod_pow2_LC_16"
	.skip 1
	.ascii	"div_pow2_LC_16"
	.skip 1
	.ascii	"mod_pow2_LC_15"
	.skip 1
	.ascii	"div_pow2_LC_15"
	.skip 1
	.ascii	"mod_pow2_LC_14"
	.skip 1
	.ascii	"div_pow2_LC_14"
	.skip 1
	.ascii	"mod_pow2_LC_13"
	.skip 1
	.ascii	"div_pow2_LC_13"
	.skip 1
	.ascii	"mod_pow2_LC_12"
	.skip 1
	.ascii	"div_pow2_LC_12"
	.skip 1
	.ascii	"mod_pow2_LC_11"
	.skip 1
	.ascii	"div_pow2_LC_11"
	.skip 1
	.ascii	"mod_pow2_LC_10"
	.skip 1
	.ascii	"div_pow2_LC_10"
	.skip 1
	.ascii	"mod_pow2_LC_9"
	.skip 1
	.ascii	"div_pow2_LC_9"
	.skip 1
	.ascii	"mod_pow2_LC_8"
	.skip 1
	.ascii	"div_pow2_LC_8"
	.skip 1
	.ascii	"mod_pow2_LC_7"
	.skip 1
	.ascii	"div_pow2_LC_7"
	.skip 1
	.ascii	"mod_pow2_LC_6"
	.skip 1
	.ascii	"div_pow2_LC_6"
	.skip 1
	.ascii	"mod_pow2_LC_5"
	.skip 1
	.ascii	"div_pow2_LC_5"
	.skip 1
	.ascii	"mod_pow2_LC_4"
	.skip 1
	.ascii	"div_pow2_LC_4"
	.skip 1
	.ascii	"mod_pow2_LC_3"
	.skip 1
	.ascii	"div_pow2_LC_3"
	.skip 1
	.ascii	"mod_pow2_LC_2"
	.skip 1
	.ascii	"div_pow2_LC_2"
	.skip 1
	.ascii	"mod_pow2_LC_1"
	.skip 1
	.ascii	"div_pow2_LC_1"
	.skip 1
	.ascii	"mod_pow2_LC_0"
	.skip 1
	.ascii	"div_pow2_LC_0"
	.skip 1
	.ascii	"mod_pow2_L_63"
	.skip 1
	.ascii	"div_pow2_L_63"
	.skip 1
	.ascii	"mod_pow2_L_62"
	.skip 1
	.ascii	"div_pow2_L_62"
	.skip 1
	.ascii	"mod_pow2_L_61"
	.skip 1
	.ascii	"div_pow2_L_61"
	.skip 1
	.ascii	"mod_pow2_L_60"
	.skip 1
	.ascii	"div_pow2_L_60"
	.skip 1
	.ascii	"mod_pow2_L_59"
	.skip 1
	.ascii	"div_pow2_L_59"
	.skip 1
	.ascii	"mod_pow2_L_58"
	.skip 1
	.ascii	"div_pow2_L_58"
	.skip 1
	.ascii	"mod_pow2_L_57"
	.skip 1
	.ascii	"div_pow2_L_57"
	.skip 1
	.ascii	"mod_pow2_L_56"
	.skip 1
	.ascii	"div_pow2_L_56"
	.skip 1
	.ascii	"mod_pow2_L_55"
	.skip 1
	.ascii	"div_pow2_L_55"
	.skip 1
	.ascii	"mod_pow2_L_54"
	.skip 1
	.ascii	"div_pow2_L_54"
	.skip 1
	.ascii	"mod_pow2_L_53"
	.skip 1
	.ascii	"div_pow2_L_53"
	.skip 1
	.ascii	"mod_pow2_L_52"
	.skip 1
	.ascii	"div_pow2_L_52"
	.skip 1
	.ascii	"mod_pow2_L_51"
	.skip 1
	.ascii	"div_pow2_L_51"
	.skip 1
	.ascii	"mod_pow2_L_50"
	.skip 1
	.ascii	"div_pow2_L_50"
	.skip 1
	.ascii	"mod_pow2_L_49"
	.skip 1
	.ascii	"div_pow2_L_49"
	.skip 1
	.ascii	"mod_pow2_L_48"
	.skip 1
	.ascii	"div_pow2_L_48"
	.skip 1
	.ascii	"mod_pow2_L_47"
	.skip 1
	.ascii	"div_pow2_L_47"
	.skip 1
	.ascii	"mod_pow2_L_46"
	.skip 1
	.ascii	"div_pow2_L_46"
	.skip 1
	.ascii	"mod_pow2_L_45"
	.skip 1
	.ascii	"div_pow2_L_45"
	.skip 1
	.ascii	"mod_pow2_L_44"
	.skip 1
	.ascii	"div_pow2_L_44"
	.skip 1
	.ascii	"mod_pow2_L_43"
	.skip 1
	.ascii	"div_pow2_L_43"
	.skip 1
	.ascii	"mod_pow2_L_42"
	.skip 1
	.ascii	"div_pow2_L_42"
	.skip 1
	.ascii	"mod_pow2_L_41"
	.skip 1
	.ascii	"div_pow2_L_41"
	.skip 1
	.ascii	"mod_pow2_L_40"
	.skip 1
	.ascii	"div_pow2_L_40"
	.skip 1
	.ascii	"mod_pow2_L_39"
	.skip 1
	.ascii	"div_pow2_L_39"
	.skip 1
	.ascii	"mod_pow2_L_38"
	.skip 1
	.ascii	"div_pow2_L_38"
	.skip 1
	.ascii	"mod_pow2_L_37"
	.skip 1
	.ascii	"div_pow2_L_37"
	.skip 1
	.ascii	"mod_pow2_L_36"
	.skip 1
	.ascii	"div_pow2_L_36"
	.skip 1
	.ascii	"mod_pow2_L_35"
	.skip 1
	.ascii	"div_pow2_L_35"
	.skip 1
	.ascii	"mod_pow2_L_34"
	.skip 1
	.ascii	"div_pow2_L_34"
	.skip 1
	.ascii	"mod_pow2_L_33"
	.skip 1
	.ascii	"div_pow2_L_33"
	.skip 1
	.ascii	"mod_pow2_L_32"
	.skip 1
	.ascii	"div_pow2_L_32"
	.skip 1
	.ascii	"mod_pow2_L_31"
	.skip 1
	.ascii	"div_pow2_L_31"
	.skip 1
	.ascii	"mod_pow2_L_30"
	.skip 1
	.ascii	"div_pow2_L_30"
	.skip 1
	.ascii	"mod_pow2_L_29"
	.skip 1
	.ascii	"div_pow2_L_29"
	.skip 1
	.ascii	"mod_pow2_L_28"
	.skip 1
	.ascii	"div_pow2_L_28"
	.skip 1
	.ascii	"mod_pow2_L_27"
	.skip 1
	.ascii	"div_pow2_L_27"
	.skip 1
	.ascii	"mod_pow2_L_26"
	.skip 1
	.ascii	"div_pow2_L_26"
	.skip 1
	.ascii	"mod_pow2_L_25"
	.skip 1
	.ascii	"div_pow2_L_25"
	.skip 1
	.ascii	"mod_pow2_L_24"
	.skip 1
	.ascii	"div_pow2_L_24"
	.skip 1
	.ascii	"mod_pow2_L_23"
	.skip 1
	.ascii	"div_pow2_L_23"
	.skip 1
	.ascii	"mod_pow2_L_22"
	.skip 1
	.ascii	"div_pow2_L_22"
	.skip 1
	.ascii	"mod_pow2_L_21"
	.skip 1
	.ascii	"div_pow2_L_21"
	.skip 1
	.ascii	"mod_pow2_L_20"
	.skip 1
	.ascii	"div_pow2_L_20"
	.skip 1
	.ascii	"mod_pow2_L_19"
	.skip 1
	.ascii	"div_pow2_L_19"
	.skip 1
	.ascii	"mod_pow2_L_18"
	.skip 1
	.ascii	"div_pow2_L_18"
	.skip 1
	.ascii	"mod_pow2_L_17"
	.skip 1
	.ascii	"div_pow2_L_17"
	.skip 1
	.ascii	"mod_pow2_L_16"
	.skip 1
	.ascii	"div_pow2_L_16"
	.skip 1
	.ascii	"mod_pow2_L_15"
	.skip 1
	.ascii	"div_pow2_L_15"
	.skip 1
	.ascii	"mod_pow2_L_14"
	.skip 1
	.ascii	"div_pow2_L_14"
	.skip 1
	.ascii	"mod_pow2_L_13"
	.skip 1
	.ascii	"div_pow2_L_13"
	.skip 1
	.ascii	"mod_pow2_L_12"
	.skip 1
	.ascii	"div_pow2_L_12"
	.skip 1
	.ascii	"mod_pow2_L_11"
	.skip 1
	.ascii	"div_pow2_L_11"
	.skip 1
	.ascii	"mod_pow2_L_10"
	.skip 1
	.ascii	"div_pow2_L_10"
	.skip 1
	.ascii	"mod_pow2_L_9"
	.skip 1
	.ascii	"div_pow2_L_9"
	.skip 1
	.ascii	"mod_pow2_L_8"
	.skip 1
	.ascii	"div_pow2_L_8"
	.skip 1
	.ascii	"mod_pow2_L_7"
	.skip 1
	.ascii	"div_pow2_L_7"
	.skip 1
	.ascii	"mod_pow2_L_6"
	.skip 1
	.ascii	"div_pow2_L_6"
	.skip 1
	.ascii	"mod_pow2_L_5"
	.skip 1
	.ascii	"div_pow2_L_5"
	.skip 1
	.ascii	"mod_pow2_L_4"
	.skip 1
	.ascii	"div_pow2_L_4"
	.skip 1
	.ascii	"mod_pow2_L_3"
	.skip 1
	.ascii	"div_pow2_L_3"
	.skip 1
	.ascii	"mod_pow2_L_2"
	.skip 1
	.ascii	"div_pow2_L_2"
	.skip 1
	.ascii	"mod_pow2_L_1"
	.skip 1
	.ascii	"div_pow2_L_1"
	.skip 1
	.ascii	"mod_pow2_L_0"
	.skip 1
	.ascii	"div_pow2_L_0"
	.skip 1
	.ascii	"mod_pow2_C_31"
	.skip 1
	.ascii	"div_pow2_C_31"
	.skip 1
	.ascii	"mod_pow2_C_30"
	.skip 1
	.ascii	"div_pow2_C_30"
	.skip 1
	.ascii	"mod_pow2_C_29"
	.skip 1
	.ascii	"div_pow2_C_29"
	.skip 1
	.ascii	"mod_pow2_C_28"
	.skip 1
	.ascii	"div_pow2_C_28"
	.skip 1
	.ascii	"mod_pow2_C_27"
	.skip 1
	.ascii	"div_pow2_C_27"
	.skip 1
	.ascii	"mod_pow2_C_26"
	.skip 1
	.ascii	"div_pow2_C_26"
	.skip 1
	.ascii	"mod_pow2_C_25"
	.skip 1
	.ascii	"div_pow2_C_25"
	.skip 1
	.ascii	"mod_pow2_C_24"
	.skip 1
	.ascii	"div_pow2_C_24"
	.skip 1
	.ascii	"mod_pow2_C_23"
	.skip 1
	.ascii	"div_pow2_C_23"
	.skip 1
	.ascii	"mod_pow2_C_22"
	.skip 1
	.ascii	"div_pow2_C_22"
	.skip 1
	.ascii	"mod_pow2_C_21"
	.skip 1
	.ascii	"div_pow2_C_21"
	.skip 1
	.ascii	"mod_pow2_C_20"
	.skip 1
	.ascii	"div_pow2_C_20"
	.skip 1
	.ascii	"mod_pow2_C_19"
	.skip 1
	.ascii	"div_pow2_C_19"
	.skip 1
	.ascii	"mod_pow2_C_18"
	.skip 1
	.ascii	"div_pow2_C_18"
	.skip 1
	.ascii	"mod_pow2_C_17"
	.skip 1
	.ascii	"div_pow2_C_17"
	.skip 1
	.ascii	"mod_pow2_C_16"
	.skip 1
	.ascii	"div_pow2_C_16"
	.skip 1
	.ascii	"mod_pow2_C_15"
	.skip 1
	.ascii	"div_pow2_C_15"
	.skip 1
	.ascii	"mod_pow2_C_14"
	.skip 1
	.ascii	"div_pow2_C_14"
	.skip 1
	.ascii	"mod_pow2_C_13"
	.skip 1
	.ascii	"div_pow2_C_13"
	.skip 1
	.ascii	"mod_pow2_C_12"
	.skip 1
	.ascii	"div_pow2_C_12"
	.skip 1
	.ascii	"mod_pow2_C_11"
	.skip 1
	.ascii	"div_pow2_C_11"
	.skip 1
	.ascii	"mod_pow2_C_10"
	.skip 1
	.ascii	"div_pow2_C_10"
	.skip 1
	.ascii	"mod_pow2_C_9"
	.skip 1
	.ascii	"div_pow2_C_9"
	.skip 1
	.ascii	"mod_pow2_C_8"
	.skip 1
	.ascii	"div_pow2_C_8"
	.skip 1
	.ascii	"mod_pow2_C_7"
	.skip 1
	.ascii	"div_pow2_C_7"
	.skip 1
	.ascii	"mod_pow2_C_6"
	.skip 1
	.ascii	"div_pow2_C_6"
	.skip 1
	.ascii	"mod_pow2_C_5"
	.skip 1
	.ascii	"div_pow2_C_5"
	.skip 1
	.ascii	"mod_pow2_C_4"
	.skip 1
	.ascii	"div_pow2_C_4"
	.skip 1
	.ascii	"mod_pow2_C_3"
	.skip 1
	.ascii	"div_pow2_C_3"
	.skip 1
	.ascii	"mod_pow2_C_2"
	.skip 1
	.ascii	"div_pow2_C_2"
	.skip 1
	.ascii	"mod_pow2_C_1"
	.skip 1
	.ascii	"div_pow2_C_1"
	.skip 1
	.ascii	"mod_pow2_C_0"
	.skip 1
	.ascii	"div_pow2_C_0"
	.skip 1
	.ascii	"mod_pow2_I_31"
	.skip 1
	.ascii	"div_pow2_I_31"
	.skip 1
	.ascii	"mod_pow2_I_30"
	.skip 1
	.ascii	"div_pow2_I_30"
	.skip 1
	.ascii	"mod_pow2_I_29"
	.skip 1
	.ascii	"div_pow2_I_29"
	.skip 1
	.ascii	"mod_pow2_I_28"
	.skip 1
	.ascii	"div_pow2_I_28"
	.skip 1
	.ascii	"mod_pow2_I_27"
	.skip 1
	.ascii	"div_pow2_I_27"
	.skip 1
	.ascii	"mod_pow2_I_26"
	.skip 1
	.ascii	"div_pow2_I_26"
	.skip 1
	.ascii	"mod_pow2_I_25"
	.skip 1
	.ascii	"div_pow2_I_25"
	.skip 1
	.ascii	"mod_pow2_I_24"
	.skip 1
	.ascii	"div_pow2_I_24"
	.skip 1
	.ascii	"mod_pow2_I_23"
	.skip 1
	.ascii	"div_pow2_I_23"
	.skip 1
	.ascii	"mod_pow2_I_22"
	.skip 1
	.ascii	"div_pow2_I_22"
	.skip 1
	.ascii	"mod_pow2_I_21"
	.skip 1
	.ascii	"div_pow2_I_21"
	.skip 1
	.ascii	"mod_pow2_I_20"
	.skip 1
	.ascii	"div_pow2_I_20"
	.skip 1
	.ascii	"mod_pow2_I_19"
	.skip 1
	.ascii	"div_pow2_I_19"
	.skip 1
	.ascii	"mod_pow2_I_18"
	.skip 1
	.ascii	"div_pow2_I_18"
	.skip 1
	.ascii	"mod_pow2_I_17"
	.skip 1
	.ascii	"div_pow2_I_17"
	.skip 1
	.ascii	"mod_pow2_I_16"
	.skip 1
	.ascii	"div_pow2_I_16"
	.skip 1
	.ascii	"mod_pow2_I_15"
	.skip 1
	.ascii	"div_pow2_I_15"
	.skip 1
	.ascii	"mod_pow2_I_14"
	.skip 1
	.ascii	"div_pow2_I_14"
	.skip 1
	.ascii	"mod_pow2_I_13"
	.skip 1
	.ascii	"div_pow2_I_13"
	.skip 1
	.ascii	"mod_pow2_I_12"
	.skip 1
	.ascii	"div_pow2_I_12"
	.skip 1
	.ascii	"mod_pow2_I_11"
	.skip 1
	.ascii	"div_pow2_I_11"
	.skip 1
	.ascii	"mod_pow2_I_10"
	.skip 1
	.ascii	"div_pow2_I_10"
	.skip 1
	.ascii	"mod_pow2_I_9"
	.skip 1
	.ascii	"div_pow2_I_9"
	.skip 1
	.ascii	"mod_pow2_I_8"
	.skip 1
	.ascii	"div_pow2_I_8"
	.skip 1
	.ascii	"mod_pow2_I_7"
	.skip 1
	.ascii	"div_pow2_I_7"
	.skip 1
	.ascii	"mod_pow2_I_6"
	.skip 1
	.ascii	"div_pow2_I_6"
	.skip 1
	.ascii	"mod_pow2_I_5"
	.skip 1
	.ascii	"div_pow2_I_5"
	.skip 1
	.ascii	"mod_pow2_I_4"
	.skip 1
	.ascii	"div_pow2_I_4"
	.skip 1
	.ascii	"mod_pow2_I_3"
	.skip 1
	.ascii	"div_pow2_I_3"
	.skip 1
	.ascii	"mod_pow2_I_2"
	.skip 1
	.ascii	"div_pow2_I_2"
	.skip 1
	.ascii	"mod_pow2_I_1"
	.skip 1
	.ascii	"div_pow2_I_1"
	.skip 1
	.ascii	"mod_pow2_I_0"
	.skip 1
	.ascii	"div_pow2_I_0"
	.skip 1
	.ascii	"mod_pow2_u64_63"
	.skip 1
	.ascii	"div_pow2_u64_63"
	.skip 1
	.ascii	"mod_pow2_u64_62"
	.skip 1
	.ascii	"div_pow2_u64_62"
	.skip 1
	.ascii	"mod_pow2_u64_61"
	.skip 1
	.ascii	"div_pow2_u64_61"
	.skip 1
	.ascii	"mod_pow2_u64_60"
	.skip 1
	.ascii	"div_pow2_u64_60"
	.skip 1
	.ascii	"mod_pow2_u64_59"
	.skip 1
	.ascii	"div_pow2_u64_59"
	.skip 1
	.ascii	"mod_pow2_u64_58"
	.skip 1
	.ascii	"div_pow2_u64_58"
	.skip 1
	.ascii	"mod_pow2_u64_57"
	.skip 1
	.ascii	"div_pow2_u64_57"
	.skip 1
	.ascii	"mod_pow2_u64_56"
	.skip 1
	.ascii	"div_pow2_u64_56"
	.skip 1
	.ascii	"mod_pow2_u64_55"
	.skip 1
	.ascii	"div_pow2_u64_55"
	.skip 1
	.ascii	"mod_pow2_u64_54"
	.skip 1
	.ascii	"div_pow2_u64_54"
	.skip 1
	.ascii	"mod_pow2_u64_53"
	.skip 1
	.ascii	"div_pow2_u64_53"
	.skip 1
	.ascii	"mod_pow2_u64_52"
	.skip 1
	.ascii	"div_pow2_u64_52"
	.skip 1
	.ascii	"mod_pow2_u64_51"
	.skip 1
	.ascii	"div_pow2_u64_51"
	.skip 1
	.ascii	"mod_pow2_u64_50"
	.skip 1
	.ascii	"div_pow2_u64_50"
	.skip 1
	.ascii	"mod_pow2_u64_49"
	.skip 1
	.ascii	"div_pow2_u64_49"
	.skip 1
	.ascii	"mod_pow2_u64_48"
	.skip 1
	.ascii	"div_pow2_u64_48"
	.skip 1
	.ascii	"mod_pow2_u64_47"
	.skip 1
	.ascii	"div_pow2_u64_47"
	.skip 1
	.ascii	"mod_pow2_u64_46"
	.skip 1
	.ascii	"div_pow2_u64_46"
	.skip 1
	.ascii	"mod_pow2_u64_45"
	.skip 1
	.ascii	"div_pow2_u64_45"
	.skip 1
	.ascii	"mod_pow2_u64_44"
	.skip 1
	.ascii	"div_pow2_u64_44"
	.skip 1
	.ascii	"mod_pow2_u64_43"
	.skip 1
	.ascii	"div_pow2_u64_43"
	.skip 1
	.ascii	"mod_pow2_u64_42"
	.skip 1
	.ascii	"div_pow2_u64_42"
	.skip 1
	.ascii	"mod_pow2_u64_41"
	.skip 1
	.ascii	"div_pow2_u64_41"
	.skip 1
	.ascii	"mod_pow2_u64_40"
	.skip 1
	.ascii	"div_pow2_u64_40"
	.skip 1
	.ascii	"mod_pow2_u64_39"
	.skip 1
	.ascii	"div_pow2_u64_39"
	.skip 1
	.ascii	"mod_pow2_u64_38"
	.skip 1
	.ascii	"div_pow2_u64_38"
	.skip 1
	.ascii	"mod_pow2_u64_37"
	.skip 1
	.ascii	"div_pow2_u64_37"
	.skip 1
	.ascii	"mod_pow2_u64_36"
	.skip 1
	.ascii	"div_pow2_u64_36"
	.skip 1
	.ascii	"mod_pow2_u64_35"
	.skip 1
	.ascii	"div_pow2_u64_35"
	.skip 1
	.ascii	"mod_pow2_u64_34"
	.skip 1
	.ascii	"div_pow2_u64_34"
	.skip 1
	.ascii	"mod_pow2_u64_33"
	.skip 1
	.ascii	"div_pow2_u64_33"
	.skip 1
	.ascii	"mod_pow2_u64_32"
	.skip 1
	.ascii	"div_pow2_u64_32"
	.skip 1
	.ascii	"mod_pow2_u64_31"
	.skip 1
	.ascii	"div_pow2_u64_31"
	.skip 1
	.ascii	"mod_pow2_u64_30"
	.skip 1
	.ascii	"div_pow2_u64_30"
	.skip 1
	.ascii	"mod_pow2_u64_29"
	.skip 1
	.ascii	"div_pow2_u64_29"
	.skip 1
	.ascii	"mod_pow2_u64_28"
	.skip 1
	.ascii	"div_pow2_u64_28"
	.skip 1
	.ascii	"mod_pow2_u64_27"
	.skip 1
	.ascii	"div_pow2_u64_27"
	.skip 1
	.ascii	"mod_pow2_u64_26"
	.skip 1
	.ascii	"div_pow2_u64_26"
	.skip 1
	.ascii	"mod_pow2_u64_25"
	.skip 1
	.ascii	"div_pow2_u64_25"
	.skip 1
	.ascii	"mod_pow2_u64_24"
	.skip 1
	.ascii	"div_pow2_u64_24"
	.skip 1
	.ascii	"mod_pow2_u64_23"
	.skip 1
	.ascii	"div_pow2_u64_23"
	.skip 1
	.ascii	"mod_pow2_u64_22"
	.skip 1
	.ascii	"div_pow2_u64_22"
	.skip 1
	.ascii	"mod_pow2_u64_21"
	.skip 1
	.ascii	"div_pow2_u64_21"
	.skip 1
	.ascii	"mod_pow2_u64_20"
	.skip 1
	.ascii	"div_pow2_u64_20"
	.skip 1
	.ascii	"mod_pow2_u64_19"
	.skip 1
	.ascii	"div_pow2_u64_19"
	.skip 1
	.ascii	"mod_pow2_u64_18"
	.skip 1
	.ascii	"div_pow2_u64_18"
	.skip 1
	.ascii	"mod_pow2_u64_17"
	.skip 1
	.ascii	"div_pow2_u64_17"
	.skip 1
	.ascii	"mod_pow2_u64_16"
	.skip 1
	.ascii	"div_pow2_u64_16"
	.skip 1
	.ascii	"mod_pow2_u64_15"
	.skip 1
	.ascii	"div_pow2_u64_15"
	.skip 1
	.ascii	"mod_pow2_u64_14"
	.skip 1
	.ascii	"div_pow2_u64_14"
	.skip 1
	.ascii	"mod_pow2_u64_13"
	.skip 1
	.ascii	"div_pow2_u64_13"
	.skip 1
	.ascii	"mod_pow2_u64_12"
	.skip 1
	.ascii	"div_pow2_u64_12"
	.skip 1
	.ascii	"mod_pow2_u64_11"
	.skip 1
	.ascii	"div_pow2_u64_11"
	.skip 1
	.ascii	"mod_pow2_u64_10"
	.skip 1
	.ascii	"div_pow2_u64_10"
	.skip 1
	.ascii	"mod_pow2_u64_9"
	.skip 1
	.ascii	"div_pow2_u64_9"
	.skip 1
	.ascii	"mod_pow2_u64_8"
	.skip 1
	.ascii	"div_pow2_u64_8"
	.skip 1
	.ascii	"mod_pow2_u64_7"
	.skip 1
	.ascii	"div_pow2_u64_7"
	.skip 1
	.ascii	"mod_pow2_u64_6"
	.skip 1
	.ascii	"div_pow2_u64_6"
	.skip 1
	.ascii	"mod_pow2_u64_5"
	.skip 1
	.ascii	"div_pow2_u64_5"
	.skip 1
	.ascii	"mod_pow2_u64_4"
	.skip 1
	.ascii	"div_pow2_u64_4"
	.skip 1
	.ascii	"mod_pow2_u64_3"
	.skip 1
	.ascii	"div_pow2_u64_3"
	.skip 1
	.ascii	"mod_pow2_u64_2"
	.skip 1
	.ascii	"div_pow2_u64_2"
	.skip 1
	.ascii	"mod_pow2_u64_1"
	.skip 1
	.ascii	"div_pow2_u64_1"
	.skip 1
	.ascii	"mod_pow2_u64_0"
	.skip 1
	.ascii	"div_pow2_u64_0"
	.skip 1
	.ascii	"mod_pow2_i64_63"
	.skip 1
	.ascii	"div_pow2_i64_63"
	.skip 1
	.ascii	"mod_pow2_i64_62"
	.skip 1
	.ascii	"div_pow2_i64_62"
	.skip 1
	.ascii	"mod_pow2_i64_61"
	.skip 1
	.ascii	"div_pow2_i64_61"
	.skip 1
	.ascii	"mod_pow2_i64_60"
	.skip 1
	.ascii	"div_pow2_i64_60"
	.skip 1
	.ascii	"mod_pow2_i64_59"
	.skip 1
	.ascii	"div_pow2_i64_59"
	.skip 1
	.ascii	"mod_pow2_i64_58"
	.skip 1
	.ascii	"div_pow2_i64_58"
	.skip 1
	.ascii	"mod_pow2_i64_57"
	.skip 1
	.ascii	"div_pow2_i64_57"
	.skip 1
	.ascii	"mod_pow2_i64_56"
	.skip 1
	.ascii	"div_pow2_i64_56"
	.skip 1
	.ascii	"mod_pow2_i64_55"
	.skip 1
	.ascii	"div_pow2_i64_55"
	.skip 1
	.ascii	"mod_pow2_i64_54"
	.skip 1
	.ascii	"div_pow2_i64_54"
	.skip 1
	.ascii	"mod_pow2_i64_53"
	.skip 1
	.ascii	"div_pow2_i64_53"
	.skip 1
	.ascii	"mod_pow2_i64_52"
	.skip 1
	.ascii	"div_pow2_i64_52"
	.skip 1
	.ascii	"mod_pow2_i64_51"
	.skip 1
	.ascii	"div_pow2_i64_51"
	.skip 1
	.ascii	"mod_pow2_i64_50"
	.skip 1
	.ascii	"div_pow2_i64_50"
	.skip 1
	.ascii	"mod_pow2_i64_49"
	.skip 1
	.ascii	"div_pow2_i64_49"
	.skip 1
	.ascii	"mod_pow2_i64_48"
	.skip 1
	.ascii	"div_pow2_i64_48"
	.skip 1
	.ascii	"mod_pow2_i64_47"
	.skip 1
	.ascii	"div_pow2_i64_47"
	.skip 1
	.ascii	"mod_pow2_i64_46"
	.skip 1
	.ascii	"div_pow2_i64_46"
	.skip 1
	.ascii	"mod_pow2_i64_45"
	.skip 1
	.ascii	"div_pow2_i64_45"
	.skip 1
	.ascii	"mod_pow2_i64_44"
	.skip 1
	.ascii	"div_pow2_i64_44"
	.skip 1
	.ascii	"mod_pow2_i64_43"
	.skip 1
	.ascii	"div_pow2_i64_43"
	.skip 1
	.ascii	"mod_pow2_i64_42"
	.skip 1
	.ascii	"div_pow2_i64_42"
	.skip 1
	.ascii	"mod_pow2_i64_41"
	.skip 1
	.ascii	"div_pow2_i64_41"
	.skip 1
	.ascii	"mod_pow2_i64_40"
	.skip 1
	.ascii	"div_pow2_i64_40"
	.skip 1
	.ascii	"mod_pow2_i64_39"
	.skip 1
	.ascii	"div_pow2_i64_39"
	.skip 1
	.ascii	"mod_pow2_i64_38"
	.skip 1
	.ascii	"div_pow2_i64_38"
	.skip 1
	.ascii	"mod_pow2_i64_37"
	.skip 1
	.ascii	"div_pow2_i64_37"
	.skip 1
	.ascii	"mod_pow2_i64_36"
	.skip 1
	.ascii	"div_pow2_i64_36"
	.skip 1
	.ascii	"mod_pow2_i64_35"
	.skip 1
	.ascii	"div_pow2_i64_35"
	.skip 1
	.ascii	"mod_pow2_i64_34"
	.skip 1
	.ascii	"div_pow2_i64_34"
	.skip 1
	.ascii	"mod_pow2_i64_33"
	.skip 1
	.ascii	"div_pow2_i64_33"
	.skip 1
	.ascii	"mod_pow2_i64_32"
	.skip 1
	.ascii	"div_pow2_i64_32"
	.skip 1
	.ascii	"mod_pow2_i64_31"
	.skip 1
	.ascii	"div_pow2_i64_31"
	.skip 1
	.ascii	"mod_pow2_i64_30"
	.skip 1
	.ascii	"div_pow2_i64_30"
	.skip 1
	.ascii	"mod_pow2_i64_29"
	.skip 1
	.ascii	"div_pow2_i64_29"
	.skip 1
	.ascii	"mod_pow2_i64_28"
	.skip 1
	.ascii	"div_pow2_i64_28"
	.skip 1
	.ascii	"mod_pow2_i64_27"
	.skip 1
	.ascii	"div_pow2_i64_27"
	.skip 1
	.ascii	"mod_pow2_i64_26"
	.skip 1
	.ascii	"div_pow2_i64_26"
	.skip 1
	.ascii	"mod_pow2_i64_25"
	.skip 1
	.ascii	"div_pow2_i64_25"
	.skip 1
	.ascii	"mod_pow2_i64_24"
	.skip 1
	.ascii	"div_pow2_i64_24"
	.skip 1
	.ascii	"mod_pow2_i64_23"
	.skip 1
	.ascii	"div_pow2_i64_23"
	.skip 1
	.ascii	"mod_pow2_i64_22"
	.skip 1
	.ascii	"div_pow2_i64_22"
	.skip 1
	.ascii	"mod_pow2_i64_21"
	.skip 1
	.ascii	"div_pow2_i64_21"
	.skip 1
	.ascii	"mod_pow2_i64_20"
	.skip 1
	.ascii	"div_pow2_i64_20"
	.skip 1
	.ascii	"mod_pow2_i64_19"
	.skip 1
	.ascii	"div_pow2_i64_19"
	.skip 1
	.ascii	"mod_pow2_i64_18"
	.skip 1
	.ascii	"div_pow2_i64_18"
	.skip 1
	.ascii	"mod_pow2_i64_17"
	.skip 1
	.ascii	"div_pow2_i64_17"
	.skip 1
	.ascii	"mod_pow2_i64_16"
	.skip 1
	.ascii	"div_pow2_i64_16"
	.skip 1
	.ascii	"mod_pow2_i64_15"
	.skip 1
	.ascii	"div_pow2_i64_15"
	.skip 1
	.ascii	"mod_pow2_i64_14"
	.skip 1
	.ascii	"div_pow2_i64_14"
	.skip 1
	.ascii	"mod_pow2_i64_13"
	.skip 1
	.ascii	"div_pow2_i64_13"
	.skip 1
	.ascii	"mod_pow2_i64_12"
	.skip 1
	.ascii	"div_pow2_i64_12"
	.skip 1
	.ascii	"mod_pow2_i64_11"
	.skip 1
	.ascii	"div_pow2_i64_11"
	.skip 1
	.ascii	"mod_pow2_i64_10"
	.skip 1
	.ascii	"div_pow2_i64_10"
	.skip 1
	.ascii	"mod_pow2_i64_9"
	.skip 1
	.ascii	"div_pow2_i64_9"
	.skip 1
	.ascii	"mod_pow2_i64_8"
	.skip 1
	.ascii	"div_pow2_i64_8"
	.skip 1
	.ascii	"mod_pow2_i64_7"
	.skip 1
	.ascii	"div_pow2_i64_7"
	.skip 1
	.ascii	"mod_pow2_i64_6"
	.skip 1
	.ascii	"div_pow2_i64_6"
	.skip 1
	.ascii	"mod_pow2_i64_5"
	.skip 1
	.ascii	"div_pow2_i64_5"
	.skip 1
	.ascii	"mod_pow2_i64_4"
	.skip 1
	.ascii	"div_pow2_i64_4"
	.skip 1
	.ascii	"mod_pow2_i64_3"
	.skip 1
	.ascii	"div_pow2_i64_3"
	.skip 1
	.ascii	"mod_pow2_i64_2"
	.skip 1
	.ascii	"div_pow2_i64_2"
	.skip 1
	.ascii	"mod_pow2_i64_1"
	.skip 1
	.ascii	"div_pow2_i64_1"
	.skip 1
	.ascii	"mod_pow2_i64_0"
	.skip 1
	.ascii	"div_pow2_i64_0"
	.skip 1
	.ascii	"mod_pow2_u32_31"
	.skip 1
	.ascii	"div_pow2_u32_31"
	.skip 1
	.ascii	"mod_pow2_u32_30"
	.skip 1
	.ascii	"div_pow2_u32_30"
	.skip 1
	.ascii	"mod_pow2_u32_29"
	.skip 1
	.ascii	"div_pow2_u32_29"
	.skip 1
	.ascii	"mod_pow2_u32_28"
	.skip 1
	.ascii	"div_pow2_u32_28"
	.skip 1
	.ascii	"mod_pow2_u32_27"
	.skip 1
	.ascii	"div_pow2_u32_27"
	.skip 1
	.ascii	"mod_pow2_u32_26"
	.skip 1
	.ascii	"div_pow2_u32_26"
	.skip 1
	.ascii	"mod_pow2_u32_25"
	.skip 1
	.ascii	"div_pow2_u32_25"
	.skip 1
	.ascii	"mod_pow2_u32_24"
	.skip 1
	.ascii	"div_pow2_u32_24"
	.skip 1
	.ascii	"mod_pow2_u32_23"
	.skip 1
	.ascii	"div_pow2_u32_23"
	.skip 1
	.ascii	"mod_pow2_u32_22"
	.skip 1
	.ascii	"div_pow2_u32_22"
	.skip 1
	.ascii	"mod_pow2_u32_21"
	.skip 1
	.ascii	"div_pow2_u32_21"
	.skip 1
	.ascii	"mod_pow2_u32_20"
	.skip 1
	.ascii	"div_pow2_u32_20"
	.skip 1
	.ascii	"mod_pow2_u32_19"
	.skip 1
	.ascii	"div_pow2_u32_19"
	.skip 1
	.ascii	"mod_pow2_u32_18"
	.skip 1
	.ascii	"div_pow2_u32_18"
	.skip 1
	.ascii	"mod_pow2_u32_17"
	.skip 1
	.ascii	"div_pow2_u32_17"
	.skip 1
	.ascii	"mod_pow2_u32_16"
	.skip 1
	.ascii	"div_pow2_u32_16"
	.skip 1
	.ascii	"mod_pow2_u32_15"
	.skip 1
	.ascii	"div_pow2_u32_15"
	.skip 1
	.ascii	"mod_pow2_u32_14"
	.skip 1
	.ascii	"div_pow2_u32_14"
	.skip 1
	.ascii	"mod_pow2_u32_13"
	.skip 1
	.ascii	"div_pow2_u32_13"
	.skip 1
	.ascii	"mod_pow2_u32_12"
	.skip 1
	.ascii	"div_pow2_u32_12"
	.skip 1
	.ascii	"mod_pow2_u32_11"
	.skip 1
	.ascii	"div_pow2_u32_11"
	.skip 1
	.ascii	"mod_pow2_u32_10"
	.skip 1
	.ascii	"div_pow2_u32_10"
	.skip 1
	.ascii	"mod_pow2_u32_9"
	.skip 1
	.ascii	"div_pow2_u32_9"
	.skip 1
	.ascii	"mod_pow2_u32_8"
	.skip 1
	.ascii	"div_pow2_u32_8"
	.skip 1
	.ascii	"mod_pow2_u32_7"
	.skip 1
	.ascii	"div_pow2_u32_7"
	.skip 1
	.ascii	"mod_pow2_u32_6"
	.skip 1
	.ascii	"div_pow2_u32_6"
	.skip 1
	.ascii	"mod_pow2_u32_5"
	.skip 1
	.ascii	"div_pow2_u32_5"
	.skip 1
	.ascii	"mod_pow2_u32_4"
	.skip 1
	.ascii	"div_pow2_u32_4"
	.skip 1
	.ascii	"mod_pow2_u32_3"
	.skip 1
	.ascii	"div_pow2_u32_3"
	.skip 1
	.ascii	"mod_pow2_u32_2"
	.skip 1
	.ascii	"div_pow2_u32_2"
	.skip 1
	.ascii	"mod_pow2_u32_1"
	.skip 1
	.ascii	"div_pow2_u32_1"
	.skip 1
	.ascii	"mod_pow2_u32_0"
	.skip 1
	.ascii	"div_pow2_u32_0"
	.skip 1
	.ascii	"mod_pow2_i32_31"
	.skip 1
	.ascii	"div_pow2_i32_31"
	.skip 1
	.ascii	"mod_pow2_i32_30"
	.skip 1
	.ascii	"div_pow2_i32_30"
	.skip 1
	.ascii	"mod_pow2_i32_29"
	.skip 1
	.ascii	"div_pow2_i32_29"
	.skip 1
	.ascii	"mod_pow2_i32_28"
	.skip 1
	.ascii	"div_pow2_i32_28"
	.skip 1
	.ascii	"mod_pow2_i32_27"
	.skip 1
	.ascii	"div_pow2_i32_27"
	.skip 1
	.ascii	"mod_pow2_i32_26"
	.skip 1
	.ascii	"div_pow2_i32_26"
	.skip 1
	.ascii	"mod_pow2_i32_25"
	.skip 1
	.ascii	"div_pow2_i32_25"
	.skip 1
	.ascii	"mod_pow2_i32_24"
	.skip 1
	.ascii	"div_pow2_i32_24"
	.skip 1
	.ascii	"mod_pow2_i32_23"
	.skip 1
	.ascii	"div_pow2_i32_23"
	.skip 1
	.ascii	"mod_pow2_i32_22"
	.skip 1
	.ascii	"div_pow2_i32_22"
	.skip 1
	.ascii	"mod_pow2_i32_21"
	.skip 1
	.ascii	"div_pow2_i32_21"
	.skip 1
	.ascii	"mod_pow2_i32_20"
	.skip 1
	.ascii	"div_pow2_i32_20"
	.skip 1
	.ascii	"mod_pow2_i32_19"
	.skip 1
	.ascii	"div_pow2_i32_19"
	.skip 1
	.ascii	"mod_pow2_i32_18"
	.skip 1
	.ascii	"div_pow2_i32_18"
	.skip 1
	.ascii	"mod_pow2_i32_17"
	.skip 1
	.ascii	"div_pow2_i32_17"
	.skip 1
	.ascii	"mod_pow2_i32_16"
	.skip 1
	.ascii	"div_pow2_i32_16"
	.skip 1
	.ascii	"mod_pow2_i32_15"
	.skip 1
	.ascii	"div_pow2_i32_15"
	.skip 1
	.ascii	"mod_pow2_i32_14"
	.skip 1
	.ascii	"div_pow2_i32_14"
	.skip 1
	.ascii	"mod_pow2_i32_13"
	.skip 1
	.ascii	"div_pow2_i32_13"
	.skip 1
	.ascii	"mod_pow2_i32_12"
	.skip 1
	.ascii	"div_pow2_i32_12"
	.skip 1
	.ascii	"mod_pow2_i32_11"
	.skip 1
	.ascii	"div_pow2_i32_11"
	.skip 1
	.ascii	"mod_pow2_i32_10"
	.skip 1
	.ascii	"div_pow2_i32_10"
	.skip 1
	.ascii	"mod_pow2_i32_9"
	.skip 1
	.ascii	"div_pow2_i32_9"
	.skip 1
	.ascii	"mod_pow2_i32_8"
	.skip 1
	.ascii	"div_pow2_i32_8"
	.skip 1
	.ascii	"mod_pow2_i32_7"
	.skip 1
	.ascii	"div_pow2_i32_7"
	.skip 1
	.ascii	"mod_pow2_i32_6"
	.skip 1
	.ascii	"div_pow2_i32_6"
	.skip 1
	.ascii	"mod_pow2_i32_5"
	.skip 1
	.ascii	"div_pow2_i32_5"
	.skip 1
	.ascii	"mod_pow2_i32_4"
	.skip 1
	.ascii	"div_pow2_i32_4"
	.skip 1
	.ascii	"mod_pow2_i32_3"
	.skip 1
	.ascii	"div_pow2_i32_3"
	.skip 1
	.ascii	"mod_pow2_i32_2"
	.skip 1
	.ascii	"div_pow2_i32_2"
	.skip 1
	.ascii	"mod_pow2_i32_1"
	.skip 1
	.ascii	"div_pow2_i32_1"
	.skip 1
	.ascii	"mod_pow2_i32_0"
	.skip 1
	.ascii	"div_pow2_i32_0"
	.skip 2
	.long	divmod_pow2_M3
	.long	L_1
	.long	divmod_pow2__mod_pow2_LC_63
	.long	L_1+15
	.long	divmod_pow2__div_pow2_LC_63
	.long	L_1+30
	.long	divmod_pow2__mod_pow2_LC_62
	.long	L_1+45
	.long	divmod_pow2__div_pow2_LC_62
	.long	L_1+60
	.long	divmod_pow2__mod_pow2_LC_61
	.long	L_1+75
	.long	divmod_pow2__div_pow2_LC_61
	.long	L_1+90
	.long	divmod_pow2__mod_pow2_LC_60
	.long	L_1+105
	.long	divmod_pow2__div_pow2_LC_60
	.long	L_1+120
	.long	divmod_pow2__mod_pow2_LC_59
	.long	L_1+135
	.long	divmod_pow2__div_pow2_LC_59
	.long	L_1+150
	.long	divmod_pow2__mod_pow2_LC_58
	.long	L_1+165
	.long	divmod_pow2__div_pow2_LC_58
	.long	L_1+180
	.long	divmod_pow2__mod_pow2_LC_57
	.long	L_1+195
	.long	divmod_pow2__div_pow2_LC_57
	.long	L_1+210
	.long	divmod_pow2__mod_pow2_LC_56
	.long	L_1+225
	.long	divmod_pow2__div_pow2_LC_56
	.long	L_1+240
	.long	divmod_pow2__mod_pow2_LC_55
	.long	L_1+255
	.long	divmod_pow2__div_pow2_LC_55
	.long	L_1+270
	.long	divmod_pow2__mod_pow2_LC_54
	.long	L_1+285
	.long	divmod_pow2__div_pow2_LC_54
	.long	L_1+300
	.long	divmod_pow2__mod_pow2_LC_53
	.long	L_1+315
	.long	divmod_pow2__div_pow2_LC_53
	.long	L_1+330
	.long	divmod_pow2__mod_pow2_LC_52
	.long	L_1+345
	.long	divmod_pow2__div_pow2_LC_52
	.long	L_1+360
	.long	divmod_pow2__mod_pow2_LC_51
	.long	L_1+375
	.long	divmod_pow2__div_pow2_LC_51
	.long	L_1+390
	.long	divmod_pow2__mod_pow2_LC_50
	.long	L_1+405
	.long	divmod_pow2__div_pow2_LC_50
	.long	L_1+420
	.long	divmod_pow2__mod_pow2_LC_49
	.long	L_1+435
	.long	divmod_pow2__div_pow2_LC_49
	.long	L_1+450
	.long	divmod_pow2__mod_pow2_LC_48
	.long	L_1+465
	.long	divmod_pow2__div_pow2_LC_48
	.long	L_1+480
	.long	divmod_pow2__mod_pow2_LC_47
	.long	L_1+495
	.long	divmod_pow2__div_pow2_LC_47
	.long	L_1+510
	.long	divmod_pow2__mod_pow2_LC_46
	.long	L_1+525
	.long	divmod_pow2__div_pow2_LC_46
	.long	L_1+540
	.long	divmod_pow2__mod_pow2_LC_45
	.long	L_1+555
	.long	divmod_pow2__div_pow2_LC_45
	.long	L_1+570
	.long	divmod_pow2__mod_pow2_LC_44
	.long	L_1+585
	.long	divmod_pow2__div_pow2_LC_44
	.long	L_1+600
	.long	divmod_pow2__mod_pow2_LC_43
	.long	L_1+615
	.long	divmod_pow2__div_pow2_LC_43
	.long	L_1+630
	.long	divmod_pow2__mod_pow2_LC_42
	.long	L_1+645
	.long	divmod_pow2__div_pow2_LC_42
	.long	L_1+660
	.long	divmod_pow2__mod_pow2_LC_41
	.long	L_1+675
	.long	divmod_pow2__div_pow2_LC_41
	.long	L_1+690
	.long	divmod_pow2__mod_pow2_LC_40
	.long	L_1+705
	.long	divmod_pow2__div_pow2_LC_40
	.long	L_1+720
	.long	divmod_pow2__mod_pow2_LC_39
	.long	L_1+735
	.long	divmod_pow2__div_pow2_LC_39
	.long	L_1+750
	.long	divmod_pow2__mod_pow2_LC_38
	.long	L_1+765
	.long	divmod_pow2__div_pow2_LC_38
	.long	L_1+780
	.long	divmod_pow2__mod_pow2_LC_37
	.long	L_1+795
	.long	divmod_pow2__div_pow2_LC_37
	.long	L_1+810
	.long	divmod_pow2__mod_pow2_LC_36
	.long	L_1+825
	.long	divmod_pow2__div_pow2_LC_36
	.long	L_1+840
	.long	divmod_pow2__mod_pow2_LC_35
	.long	L_1+855
	.long	divmod_pow2__div_pow2_LC_35
	.long	L_1+870
	.long	divmod_pow2__mod_pow2_LC_34
	.long	L_1+885
	.long	divmod_pow2__div_pow2_LC_34
	.long	L_1+900
	.long	divmod_pow2__mod_pow2_LC_33
	.long	L_1+915
	.long	divmod_pow2__div_pow2_LC_33
	.long	L_1+930
	.long	divmod_pow2__mod_pow2_LC_32
	.long	L_1+945
	.long	divmod_pow2__div_pow2_LC_32
	.long	L_1+960
	.long	divmod_pow2__mod_pow2_LC_31
	.long	L_1+975
	.long	divmod_pow2__div_pow2_LC_31
	.long	L_1+990
	.long	divmod_pow2__mod_pow2_LC_30
	.long	L_1+1005
	.long	divmod_pow2__div_pow2_LC_30
	.long	L_1+1020
	.long	divmod_pow2__mod_pow2_LC_29
	.long	L_1+1035
	.long	divmod_pow2__div_pow2_LC_29
	.long	L_1+1050
	.long	divmod_pow2__mod_pow2_LC_28
	.long	L_1+1065
	.long	divmod_pow2__div_pow2_LC_28
	.long	L_1+1080
	.long	divmod_pow2__mod_pow2_LC_27
	.long	L_1+1095
	.long	divmod_pow2__div_pow2_LC_27
	.long	L_1+1110
	.long	divmod_pow2__mod_pow2_LC_26
	.long	L_1+1125
	.long	divmod_pow2__div_pow2_LC_26
	.long	L_1+1140
	.long	divmod_pow2__mod_pow2_LC_25
	.long	L_1+1155
	.long	divmod_pow2__div_pow2_LC_25
	.long	L_1+1170
	.long	divmod_pow2__mod_pow2_LC_24
	.long	L_1+1185
	.long	divmod_pow2__div_pow2_LC_24
	.long	L_1+1200
	.long	divmod_pow2__mod_pow2_LC_23
	.long	L_1+1215
	.long	divmod_pow2__div_pow2_LC_23
	.long	L_1+1230
	.long	divmod_pow2__mod_pow2_LC_22
	.long	L_1+1245
	.long	divmod_pow2__div_pow2_LC_22
	.long	L_1+1260
	.long	divmod_pow2__mod_pow2_LC_21
	.long	L_1+1275
	.long	divmod_pow2__div_pow2_LC_21
	.long	L_1+1290
	.long	divmod_pow2__mod_pow2_LC_20
	.long	L_1+1305
	.long	divmod_pow2__div_pow2_LC_20
	.long	L_1+1320
	.long	divmod_pow2__mod_pow2_LC_19
	.long	L_1+1335
	.long	divmod_pow2__div_pow2_LC_19
	.long	L_1+1350
	.long	divmod_pow2__mod_pow2_LC_18
	.long	L_1+1365
	.long	divmod_pow2__div_pow2_LC_18
	.long	L_1+1380
	.long	divmod_pow2__mod_pow2_LC_17
	.long	L_1+1395
	.long	divmod_pow2__div_pow2_LC_17
	.long	L_1+1410
	.long	divmod_pow2__mod_pow2_LC_16
	.long	L_1+1425
	.long	divmod_pow2__div_pow2_LC_16
	.long	L_1+1440
	.long	divmod_pow2__mod_pow2_LC_15
	.long	L_1+1455
	.long	divmod_pow2__div_pow2_LC_15
	.long	L_1+1470
	.long	divmod_pow2__mod_pow2_LC_14
	.long	L_1+1485
	.long	divmod_pow2__div_pow2_LC_14
	.long	L_1+1500
	.long	divmod_pow2__mod_pow2_LC_13
	.long	L_1+1515
	.long	divmod_pow2__div_pow2_LC_13
	.long	L_1+1530
	.long	divmod_pow2__mod_pow2_LC_12
	.long	L_1+1545
	.long	divmod_pow2__div_pow2_LC_12
	.long	L_1+1560
	.long	divmod_pow2__mod_pow2_LC_11
	.long	L_1+1575
	.long	divmod_pow2__div_pow2_LC_11
	.long	L_1+1590
	.long	divmod_pow2__mod_pow2_LC_10
	.long	L_1+1605
	.long	divmod_pow2__div_pow2_LC_10
	.long	L_1+1620
	.long	divmod_pow2__mod_pow2_LC_9
	.long	L_1+1635
	.long	divmod_pow2__div_pow2_LC_9
	.long	L_1+1649
	.long	divmod_pow2__mod_pow2_LC_8
	.long	L_1+1663
	.long	divmod_pow2__div_pow2_LC_8
	.long	L_1+1677
	.long	divmod_pow2__mod_pow2_LC_7
	.long	L_1+1691
	.long	divmod_pow2__div_pow2_LC_7
	.long	L_1+1705
	.long	divmod_pow2__mod_pow2_LC_6
	.long	L_1+1719
	.long	divmod_pow2__div_pow2_LC_6
	.long	L_1+1733
	.long	divmod_pow2__mod_pow2_LC_5
	.long	L_1+1747
	.long	divmod_pow2__div_pow2_LC_5
	.long	L_1+1761
	.long	divmod_pow2__mod_pow2_LC_4
	.long	L_1+1775
	.long	divmod_pow2__div_pow2_LC_4
	.long	L_1+1789
	.long	divmod_pow2__mod_pow2_LC_3
	.long	L_1+1803
	.long	divmod_pow2__div_pow2_LC_3
	.long	L_1+1817
	.long	divmod_pow2__mod_pow2_LC_2
	.long	L_1+1831
	.long	divmod_pow2__div_pow2_LC_2
	.long	L_1+1845
	.long	divmod_pow2__mod_pow2_LC_1
	.long	L_1+1859
	.long	divmod_pow2__div_pow2_LC_1
	.long	L_1+1873
	.long	divmod_pow2__mod_pow2_LC_0
	.long	L_1+1887
	.long	divmod_pow2__div_pow2_LC_0
	.long	L_1+1901
	.long	divmod_pow2__mod_pow2_L_63
	.long	L_1+1915
	.long	divmod_pow2__div_pow2_L_63
	.long	L_1+1929
	.long	divmod_pow2__mod_pow2_L_62
	.long	L_1+1943
	.long	divmod_pow2__div_pow2_L_62
	.long	L_1+1957
	.long	divmod_pow2__mod_pow2_L_61
	.long	L_1+1971
	.long	divmod_pow2__div_pow2_L_61
	.long	L_1+1985
	.long	divmod_pow2__mod_pow2_L_60
	.long	L_1+1999
	.long	divmod_pow2__div_pow2_L_60
	.long	L_1+2013
	.long	divmod_pow2__mod_pow2_L_59
	.long	L_1+2027
	.long	divmod_pow2__div_pow2_L_59
	.long	L_1+2041
	.long	divmod_pow2__mod_pow2_L_58
	.long	L_1+2055
	.long	divmod_pow2__div_pow2_L_58
	.long	L_1+2069
	.long	divmod_pow2__mod_pow2_L_57
	.long	L_1+2083
	.long	divmod_pow2__div_pow2_L_57
	.long	L_1+2097
	.long	divmod_pow2__mod_pow2_L_56
	.long	L_1+2111
	.long	divmod_pow2__div_pow2_L_56
	.long	L_1+2125
	.long	divmod_pow2__mod_pow2_L_55
	.long	L_1+2139
	.long	divmod_pow2__div_pow2_L_55
	.long	L_1+2153
	.long	divmod_pow2__mod_pow2_L_54
	.long	L_1+2167
	.long	divmod_pow2__div_pow2_L_54
	.long	L_1+2181
	.long	divmod_pow2__mod_pow2_L_53
	.long	L_1+2195
	.long	divmod_pow2__div_pow2_L_53
	.long	L_1+2209
	.long	divmod_pow2__mod_pow2_L_52
	.long	L_1+2223
	.long	divmod_pow2__div_pow2_L_52
	.long	L_1+2237
	.long	divmod_pow2__mod_pow2_L_51
	.long	L_1+2251
	.long	divmod_pow2__div_pow2_L_51
	.long	L_1+2265
	.long	divmod_pow2__mod_pow2_L_50
	.long	L_1+2279
	.long	divmod_pow2__div_pow2_L_50
	.long	L_1+2293
	.long	divmod_pow2__mod_pow2_L_49
	.long	L_1+2307
	.long	divmod_pow2__div_pow2_L_49
	.long	L_1+2321
	.long	divmod_pow2__mod_pow2_L_48
	.long	L_1+2335
	.long	divmod_pow2__div_pow2_L_48
	.long	L_1+2349
	.long	divmod_pow2__mod_pow2_L_47
	.long	L_1+2363
	.long	divmod_pow2__div_pow2_L_47
	.long	L_1+2377
	.long	divmod_pow2__mod_pow2_L_46
	.long	L_1+2391
	.long	divmod_pow2__div_pow2_L_46
	.long	L_1+2405
	.long	divmod_pow2__mod_pow2_L_45
	.long	L_1+2419
	.long	divmod_pow2__div_pow2_L_45
	.long	L_1+2433
	.long	divmod_pow2__mod_pow2_L_44
	.long	L_1+2447
	.long	divmod_pow2__div_pow2_L_44
	.long	L_1+2461
	.long	divmod_pow2__mod_pow2_L_43
	.long	L_1+2475
	.long	divmod_pow2__div_pow2_L_43
	.long	L_1+2489
	.long	divmod_pow2__mod_pow2_L_42
	.long	L_1+2503
	.long	divmod_pow2__div_pow2_L_42
	.long	L_1+2517
	.long	divmod_pow2__mod_pow2_L_41
	.long	L_1+2531
	.long	divmod_pow2__div_pow2_L_41
	.long	L_1+2545
	.long	divmod_pow2__mod_pow2_L_40
	.long	L_1+2559
	.long	divmod_pow2__div_pow2_L_40
	.long	L_1+2573
	.long	divmod_pow2__mod_pow2_L_39
	.long	L_1+2587
	.long	divmod_pow2__div_pow2_L_39
	.long	L_1+2601
	.long	divmod_pow2__mod_pow2_L_38
	.long	L_1+2615
	.long	divmod_pow2__div_pow2_L_38
	.long	L_1+2629
	.long	divmod_pow2__mod_pow2_L_37
	.long	L_1+2643
	.long	divmod_pow2__div_pow2_L_37
	.long	L_1+2657
	.long	divmod_pow2__mod_pow2_L_36
	.long	L_1+2671
	.long	divmod_pow2__div_pow2_L_36
	.long	L_1+2685
	.long	divmod_pow2__mod_pow2_L_35
	.long	L_1+2699
	.long	divmod_pow2__div_pow2_L_35
	.long	L_1+2713
	.long	divmod_pow2__mod_pow2_L_34
	.long	L_1+2727
	.long	divmod_pow2__div_pow2_L_34
	.long	L_1+2741
	.long	divmod_pow2__mod_pow2_L_33
	.long	L_1+2755
	.long	divmod_pow2__div_pow2_L_33
	.long	L_1+2769
	.long	divmod_pow2__mod_pow2_L_32
	.long	L_1+2783
	.long	divmod_pow2__div_pow2_L_32
	.long	L_1+2797
	.long	divmod_pow2__mod_pow2_L_31
	.long	L_1+2811
	.long	divmod_pow2__div_pow2_L_31
	.long	L_1+2825
	.long	divmod_pow2__mod_pow2_L_30
	.long	L_1+2839
	.long	divmod_pow2__div_pow2_L_30
	.long	L_1+2853
	.long	divmod_pow2__mod_pow2_L_29
	.long	L_1+2867
	.long	divmod_pow2__div_pow2_L_29
	.long	L_1+2881
	.long	divmod_pow2__mod_pow2_L_28
	.long	L_1+2895
	.long	divmod_pow2__div_pow2_L_28
	.long	L_1+2909
	.long	divmod_pow2__mod_pow2_L_27
	.long	L_1+2923
	.long	divmod_pow2__div_pow2_L_27
	.long	L_1+2937
	.long	divmod_pow2__mod_pow2_L_26
	.long	L_1+2951
	.long	divmod_pow2__div_pow2_L_26
	.long	L_1+2965
	.long	divmod_pow2__mod_pow2_L_25
	.long	L_1+2979
	.long	divmod_pow2__div_pow2_L_25
	.long	L_1+2993
	.long	divmod_pow2__mod_pow2_L_24
	.long	L_1+3007
	.long	divmod_pow2__div_pow2_L_24
	.long	L_1+3021
	.long	divmod_pow2__mod_pow2_L_23
	.long	L_1+3035
	.long	divmod_pow2__div_pow2_L_23
	.long	L_1+3049
	.long	divmod_pow2__mod_pow2_L_22
	.long	L_1+3063
	.long	divmod_pow2__div_pow2_L_22
	.long	L_1+3077
	.long	divmod_pow2__mod_pow2_L_21
	.long	L_1+3091
	.long	divmod_pow2__div_pow2_L_21
	.long	L_1+3105
	.long	divmod_pow2__mod_pow2_L_20
	.long	L_1+3119
	.long	divmod_pow2__div_pow2_L_20
	.long	L_1+3133
	.long	divmod_pow2__mod_pow2_L_19
	.long	L_1+3147
	.long	divmod_pow2__div_pow2_L_19
	.long	L_1+3161
	.long	divmod_pow2__mod_pow2_L_18
	.long	L_1+3175
	.long	divmod_pow2__div_pow2_L_18
	.long	L_1+3189
	.long	divmod_pow2__mod_pow2_L_17
	.long	L_1+3203
	.long	divmod_pow2__div_pow2_L_17
	.long	L_1+3217
	.long	divmod_pow2__mod_pow2_L_16
	.long	L_1+3231
	.long	divmod_pow2__div_pow2_L_16
	.long	L_1+3245
	.long	divmod_pow2__mod_pow2_L_15
	.long	L_1+3259
	.long	divmod_pow2__div_pow2_L_15
	.long	L_1+3273
	.long	divmod_pow2__mod_pow2_L_14
	.long	L_1+3287
	.long	divmod_pow2__div_pow2_L_14
	.long	L_1+3301
	.long	divmod_pow2__mod_pow2_L_13
	.long	L_1+3315
	.long	divmod_pow2__div_pow2_L_13
	.long	L_1+3329
	.long	divmod_pow2__mod_pow2_L_12
	.long	L_1+3343
	.long	divmod_pow2__div_pow2_L_12
	.long	L_1+3357
	.long	divmod_pow2__mod_pow2_L_11
	.long	L_1+3371
	.long	divmod_pow2__div_pow2_L_11
	.long	L_1+3385
	.long	divmod_pow2__mod_pow2_L_10
	.long	L_1+3399
	.long	divmod_pow2__div_pow2_L_10
	.long	L_1+3413
	.long	divmod_pow2__mod_pow2_L_9
	.long	L_1+3427
	.long	divmod_pow2__div_pow2_L_9
	.long	L_1+3440
	.long	divmod_pow2__mod_pow2_L_8
	.long	L_1+3453
	.long	divmod_pow2__div_pow2_L_8
	.long	L_1+3466
	.long	divmod_pow2__mod_pow2_L_7
	.long	L_1+3479
	.long	divmod_pow2__div_pow2_L_7
	.long	L_1+3492
	.long	divmod_pow2__mod_pow2_L_6
	.long	L_1+3505
	.long	divmod_pow2__div_pow2_L_6
	.long	L_1+3518
	.long	divmod_pow2__mod_pow2_L_5
	.long	L_1+3531
	.long	divmod_pow2__div_pow2_L_5
	.long	L_1+3544
	.long	divmod_pow2__mod_pow2_L_4
	.long	L_1+3557
	.long	divmod_pow2__div_pow2_L_4
	.long	L_1+3570
	.long	divmod_pow2__mod_pow2_L_3
	.long	L_1+3583
	.long	divmod_pow2__div_pow2_L_3
	.long	L_1+3596
	.long	divmod_pow2__mod_pow2_L_2
	.long	L_1+3609
	.long	divmod_pow2__div_pow2_L_2
	.long	L_1+3622
	.long	divmod_pow2__mod_pow2_L_1
	.long	L_1+3635
	.long	divmod_pow2__div_pow2_L_1
	.long	L_1+3648
	.long	divmod_pow2__mod_pow2_L_0
	.long	L_1+3661
	.long	divmod_pow2__div_pow2_L_0
	.long	L_1+3674
	.long	divmod_pow2__mod_pow2_C_31
	.long	L_1+3687
	.long	divmod_pow2__div_pow2_C_31
	.long	L_1+3701
	.long	divmod_pow2__mod_pow2_C_30
	.long	L_1+3715
	.long	divmod_pow2__div_pow2_C_30
	.long	L_1+3729
	.long	divmod_pow2__mod_pow2_C_29
	.long	L_1+3743
	.long	divmod_pow2__div_pow2_C_29
	.long	L_1+3757
	.long	divmod_pow2__mod_pow2_C_28
	.long	L_1+3771
	.long	divmod_pow2__div_pow2_C_28
	.long	L_1+3785
	.long	divmod_pow2__mod_pow2_C_27
	.long	L_1+3799
	.long	divmod_pow2__div_pow2_C_27
	.long	L_1+3813
	.long	divmod_pow2__mod_pow2_C_26
	.long	L_1+3827
	.long	divmod_pow2__div_pow2_C_26
	.long	L_1+3841
	.long	divmod_pow2__mod_pow2_C_25
	.long	L_1+3855
	.long	divmod_pow2__div_pow2_C_25
	.long	L_1+3869
	.long	divmod_pow2__mod_pow2_C_24
	.long	L_1+3883
	.long	divmod_pow2__div_pow2_C_24
	.long	L_1+3897
	.long	divmod_pow2__mod_pow2_C_23
	.long	L_1+3911
	.long	divmod_pow2__div_pow2_C_23
	.long	L_1+3925
	.long	divmod_pow2__mod_pow2_C_22
	.long	L_1+3939
	.long	divmod_pow2__div_pow2_C_22
	.long	L_1+3953
	.long	divmod_pow2__mod_pow2_C_21
	.long	L_1+3967
	.long	divmod_pow2__div_pow2_C_21
	.long	L_1+3981
	.long	divmod_pow2__mod_pow2_C_20
	.long	L_1+3995
	.long	divmod_pow2__div_pow2_C_20
	.long	L_1+4009
	.long	divmod_pow2__mod_pow2_C_19
	.long	L_1+4023
	.long	divmod_pow2__div_pow2_C_19
	.long	L_1+4037
	.long	divmod_pow2__mod_pow2_C_18
	.long	L_1+4051
	.long	divmod_pow2__div_pow2_C_18
	.long	L_1+4065
	.long	divmod_pow2__mod_pow2_C_17
	.long	L_1+4079
	.long	divmod_pow2__div_pow2_C_17
	.long	L_1+4093
	.long	divmod_pow2__mod_pow2_C_16
	.long	L_1+4107
	.long	divmod_pow2__div_pow2_C_16
	.long	L_1+4121
	.long	divmod_pow2__mod_pow2_C_15
	.long	L_1+4135
	.long	divmod_pow2__div_pow2_C_15
	.long	L_1+4149
	.long	divmod_pow2__mod_pow2_C_14
	.long	L_1+4163
	.long	divmod_pow2__div_pow2_C_14
	.long	L_1+4177
	.long	divmod_pow2__mod_pow2_C_13
	.long	L_1+4191
	.long	divmod_pow2__div_pow2_C_13
	.long	L_1+4205
	.long	divmod_pow2__mod_pow2_C_12
	.long	L_1+4219
	.long	divmod_pow2__div_pow2_C_12
	.long	L_1+4233
	.long	divmod_pow2__mod_pow2_C_11
	.long	L_1+4247
	.long	divmod_pow2__div_pow2_C_11
	.long	L_1+4261
	.long	divmod_pow2__mod_pow2_C_10
	.long	L_1+4275
	.long	divmod_pow2__div_pow2_C_10
	.long	L_1+4289
	.long	divmod_pow2__mod_pow2_C_9
	.long	L_1+4303
	.long	divmod_pow2__div_pow2_C_9
	.long	L_1+4316
	.long	divmod_pow2__mod_pow2_C_8
	.long	L_1+4329
	.long	divmod_pow2__div_pow2_C_8
	.long	L_1+4342
	.long	divmod_pow2__mod_pow2_C_7
	.long	L_1+4355
	.long	divmod_pow2__div_pow2_C_7
	.long	L_1+4368
	.long	divmod_pow2__mod_pow2_C_6
	.long	L_1+4381
	.long	divmod_pow2__div_pow2_C_6
	.long	L_1+4394
	.long	divmod_pow2__mod_pow2_C_5
	.long	L_1+4407
	.long	divmod_pow2__div_pow2_C_5
	.long	L_1+4420
	.long	divmod_pow2__mod_pow2_C_4
	.long	L_1+4433
	.long	divmod_pow2__div_pow2_C_4
	.long	L_1+4446
	.long	divmod_pow2__mod_pow2_C_3
	.long	L_1+4459
	.long	divmod_pow2__div_pow2_C_3
	.long	L_1+4472
	.long	divmod_pow2__mod_pow2_C_2
	.long	L_1+4485
	.long	divmod_pow2__div_pow2_C_2
	.long	L_1+4498
	.long	divmod_pow2__mod_pow2_C_1
	.long	L_1+4511
	.long	divmod_pow2__div_pow2_C_1
	.long	L_1+4524
	.long	divmod_pow2__mod_pow2_C_0
	.long	L_1+4537
	.long	divmod_pow2__div_pow2_C_0
	.long	L_1+4550
	.long	divmod_pow2__mod_pow2_I_31
	.long	L_1+4563
	.long	divmod_pow2__div_pow2_I_31
	.long	L_1+4577
	.long	divmod_pow2__mod_pow2_I_30
	.long	L_1+4591
	.long	divmod_pow2__div_pow2_I_30
	.long	L_1+4605
	.long	divmod_pow2__mod_pow2_I_29
	.long	L_1+4619
	.long	divmod_pow2__div_pow2_I_29
	.long	L_1+4633
	.long	divmod_pow2__mod_pow2_I_28
	.long	L_1+4647
	.long	divmod_pow2__div_pow2_I_28
	.long	L_1+4661
	.long	divmod_pow2__mod_pow2_I_27
	.long	L_1+4675
	.long	divmod_pow2__div_pow2_I_27
	.long	L_1+4689
	.long	divmod_pow2__mod_pow2_I_26
	.long	L_1+4703
	.long	divmod_pow2__div_pow2_I_26
	.long	L_1+4717
	.long	divmod_pow2__mod_pow2_I_25
	.long	L_1+4731
	.long	divmod_pow2__div_pow2_I_25
	.long	L_1+4745
	.long	divmod_pow2__mod_pow2_I_24
	.long	L_1+4759
	.long	divmod_pow2__div_pow2_I_24
	.long	L_1+4773
	.long	divmod_pow2__mod_pow2_I_23
	.long	L_1+4787
	.long	divmod_pow2__div_pow2_I_23
	.long	L_1+4801
	.long	divmod_pow2__mod_pow2_I_22
	.long	L_1+4815
	.long	divmod_pow2__div_pow2_I_22
	.long	L_1+4829
	.long	divmod_pow2__mod_pow2_I_21
	.long	L_1+4843
	.long	divmod_pow2__div_pow2_I_21
	.long	L_1+4857
	.long	divmod_pow2__mod_pow2_I_20
	.long	L_1+4871
	.long	divmod_pow2__div_pow2_I_20
	.long	L_1+4885
	.long	divmod_pow2__mod_pow2_I_19
	.long	L_1+4899
	.long	divmod_pow2__div_pow2_I_19
	.long	L_1+4913
	.long	divmod_pow2__mod_pow2_I_18
	.long	L_1+4927
	.long	divmod_pow2__div_pow2_I_18
	.long	L_1+4941
	.long	divmod_pow2__mod_pow2_I_17
	.long	L_1+4955
	.long	divmod_pow2__div_pow2_I_17
	.long	L_1+4969
	.long	divmod_pow2__mod_pow2_I_16
	.long	L_1+4983
	.long	divmod_pow2__div_pow2_I_16
	.long	L_1+4997
	.long	divmod_pow2__mod_pow2_I_15
	.long	L_1+5011
	.long	divmod_pow2__div_pow2_I_15
	.long	L_1+5025
	.long	divmod_pow2__mod_pow2_I_14
	.long	L_1+5039
	.long	divmod_pow2__div_pow2_I_14
	.long	L_1+5053
	.long	divmod_pow2__mod_pow2_I_13
	.long	L_1+5067
	.long	divmod_pow2__div_pow2_I_13
	.long	L_1+5081
	.long	divmod_pow2__mod_pow2_I_12
	.long	L_1+5095
	.long	divmod_pow2__div_pow2_I_12
	.long	L_1+5109
	.long	divmod_pow2__mod_pow2_I_11
	.long	L_1+5123
	.long	divmod_pow2__div_pow2_I_11
	.long	L_1+5137
	.long	divmod_pow2__mod_pow2_I_10
	.long	L_1+5151
	.long	divmod_pow2__div_pow2_I_10
	.long	L_1+5165
	.long	divmod_pow2__mod_pow2_I_9
	.long	L_1+5179
	.long	divmod_pow2__div_pow2_I_9
	.long	L_1+5192
	.long	divmod_pow2__mod_pow2_I_8
	.long	L_1+5205
	.long	divmod_pow2__div_pow2_I_8
	.long	L_1+5218
	.long	divmod_pow2__mod_pow2_I_7
	.long	L_1+5231
	.long	divmod_pow2__div_pow2_I_7
	.long	L_1+5244
	.long	divmod_pow2__mod_pow2_I_6
	.long	L_1+5257
	.long	divmod_pow2__div_pow2_I_6
	.long	L_1+5270
	.long	divmod_pow2__mod_pow2_I_5
	.long	L_1+5283
	.long	divmod_pow2__div_pow2_I_5
	.long	L_1+5296
	.long	divmod_pow2__mod_pow2_I_4
	.long	L_1+5309
	.long	divmod_pow2__div_pow2_I_4
	.long	L_1+5322
	.long	divmod_pow2__mod_pow2_I_3
	.long	L_1+5335
	.long	divmod_pow2__div_pow2_I_3
	.long	L_1+5348
	.long	divmod_pow2__mod_pow2_I_2
	.long	L_1+5361
	.long	divmod_pow2__div_pow2_I_2
	.long	L_1+5374
	.long	divmod_pow2__mod_pow2_I_1
	.long	L_1+5387
	.long	divmod_pow2__div_pow2_I_1
	.long	L_1+5400
	.long	divmod_pow2__mod_pow2_I_0
	.long	L_1+5413
	.long	divmod_pow2__div_pow2_I_0
	.long	L_1+5426
	.long	divmod_pow2__mod_pow2_u64_63
	.long	L_1+5439
	.long	divmod_pow2__div_pow2_u64_63
	.long	L_1+5455
	.long	divmod_pow2__mod_pow2_u64_62
	.long	L_1+5471
	.long	divmod_pow2__div_pow2_u64_62
	.long	L_1+5487
	.long	divmod_pow2__mod_pow2_u64_61
	.long	L_1+5503
	.long	divmod_pow2__div_pow2_u64_61
	.long	L_1+5519
	.long	divmod_pow2__mod_pow2_u64_60
	.long	L_1+5535
	.long	divmod_pow2__div_pow2_u64_60
	.long	L_1+5551
	.long	divmod_pow2__mod_pow2_u64_59
	.long	L_1+5567
	.long	divmod_pow2__div_pow2_u64_59
	.long	L_1+5583
	.long	divmod_pow2__mod_pow2_u64_58
	.long	L_1+5599
	.long	divmod_pow2__div_pow2_u64_58
	.long	L_1+5615
	.long	divmod_pow2__mod_pow2_u64_57
	.long	L_1+5631
	.long	divmod_pow2__div_pow2_u64_57
	.long	L_1+5647
	.long	divmod_pow2__mod_pow2_u64_56
	.long	L_1+5663
	.long	divmod_pow2__div_pow2_u64_56
	.long	L_1+5679
	.long	divmod_pow2__mod_pow2_u64_55
	.long	L_1+5695
	.long	divmod_pow2__div_pow2_u64_55
	.long	L_1+5711
	.long	divmod_pow2__mod_pow2_u64_54
	.long	L_1+5727
	.long	divmod_pow2__div_pow2_u64_54
	.long	L_1+5743
	.long	divmod_pow2__mod_pow2_u64_53
	.long	L_1+5759
	.long	divmod_pow2__div_pow2_u64_53
	.long	L_1+5775
	.long	divmod_pow2__mod_pow2_u64_52
	.long	L_1+5791
	.long	divmod_pow2__div_pow2_u64_52
	.long	L_1+5807
	.long	divmod_pow2__mod_pow2_u64_51
	.long	L_1+5823
	.long	divmod_pow2__div_pow2_u64_51
	.long	L_1+5839
	.long	divmod_pow2__mod_pow2_u64_50
	.long	L_1+5855
	.long	divmod_pow2__div_pow2_u64_50
	.long	L_1+5871
	.long	divmod_pow2__mod_pow2_u64_49
	.long	L_1+5887
	.long	divmod_pow2__div_pow2_u64_49
	.long	L_1+5903
	.long	divmod_pow2__mod_pow2_u64_48
	.long	L_1+5919
	.long	divmod_pow2__div_pow2_u64_48
	.long	L_1+5935
	.long	divmod_pow2__mod_pow2_u64_47
	.long	L_1+5951
	.long	divmod_pow2__div_pow2_u64_47
	.long	L_1+5967
	.long	divmod_pow2__mod_pow2_u64_46
	.long	L_1+5983
	.long	divmod_pow2__div_pow2_u64_46
	.long	L_1+5999
	.long	divmod_pow2__mod_pow2_u64_45
	.long	L_1+6015
	.long	divmod_pow2__div_pow2_u64_45
	.long	L_1+6031
	.long	divmod_pow2__mod_pow2_u64_44
	.long	L_1+6047
	.long	divmod_pow2__div_pow2_u64_44
	.long	L_1+6063
	.long	divmod_pow2__mod_pow2_u64_43
	.long	L_1+6079
	.long	divmod_pow2__div_pow2_u64_43
	.long	L_1+6095
	.long	divmod_pow2__mod_pow2_u64_42
	.long	L_1+6111
	.long	divmod_pow2__div_pow2_u64_42
	.long	L_1+6127
	.long	divmod_pow2__mod_pow2_u64_41
	.long	L_1+6143
	.long	divmod_pow2__div_pow2_u64_41
	.long	L_1+6159
	.long	divmod_pow2__mod_pow2_u64_40
	.long	L_1+6175
	.long	divmod_pow2__div_pow2_u64_40
	.long	L_1+6191
	.long	divmod_pow2__mod_pow2_u64_39
	.long	L_1+6207
	.long	divmod_pow2__div_pow2_u64_39
	.long	L_1+6223
	.long	divmod_pow2__mod_pow2_u64_38
	.long	L_1+6239
	.long	divmod_pow2__div_pow2_u64_38
	.long	L_1+6255
	.long	divmod_pow2__mod_pow2_u64_37
	.long	L_1+6271
	.long	divmod_pow2__div_pow2_u64_37
	.long	L_1+6287
	.long	divmod_pow2__mod_pow2_u64_36
	.long	L_1+6303
	.long	divmod_pow2__div_pow2_u64_36
	.long	L_1+6319
	.long	divmod_pow2__mod_pow2_u64_35
	.long	L_1+6335
	.long	divmod_pow2__div_pow2_u64_35
	.long	L_1+6351
	.long	divmod_pow2__mod_pow2_u64_34
	.long	L_1+6367
	.long	divmod_pow2__div_pow2_u64_34
	.long	L_1+6383
	.long	divmod_pow2__mod_pow2_u64_33
	.long	L_1+6399
	.long	divmod_pow2__div_pow2_u64_33
	.long	L_1+6415
	.long	divmod_pow2__mod_pow2_u64_32
	.long	L_1+6431
	.long	divmod_pow2__div_pow2_u64_32
	.long	L_1+6447
	.long	divmod_pow2__mod_pow2_u64_31
	.long	L_1+6463
	.long	divmod_pow2__div_pow2_u64_31
	.long	L_1+6479
	.long	divmod_pow2__mod_pow2_u64_30
	.long	L_1+6495
	.long	divmod_pow2__div_pow2_u64_30
	.long	L_1+6511
	.long	divmod_pow2__mod_pow2_u64_29
	.long	L_1+6527
	.long	divmod_pow2__div_pow2_u64_29
	.long	L_1+6543
	.long	divmod_pow2__mod_pow2_u64_28
	.long	L_1+6559
	.long	divmod_pow2__div_pow2_u64_28
	.long	L_1+6575
	.long	divmod_pow2__mod_pow2_u64_27
	.long	L_1+6591
	.long	divmod_pow2__div_pow2_u64_27
	.long	L_1+6607
	.long	divmod_pow2__mod_pow2_u64_26
	.long	L_1+6623
	.long	divmod_pow2__div_pow2_u64_26
	.long	L_1+6639
	.long	divmod_pow2__mod_pow2_u64_25
	.long	L_1+6655
	.long	divmod_pow2__div_pow2_u64_25
	.long	L_1+6671
	.long	divmod_pow2__mod_pow2_u64_24
	.long	L_1+6687
	.long	divmod_pow2__div_pow2_u64_24
	.long	L_1+6703
	.long	divmod_pow2__mod_pow2_u64_23
	.long	L_1+6719
	.long	divmod_pow2__div_pow2_u64_23
	.long	L_1+6735
	.long	divmod_pow2__mod_pow2_u64_22
	.long	L_1+6751
	.long	divmod_pow2__div_pow2_u64_22
	.long	L_1+6767
	.long	divmod_pow2__mod_pow2_u64_21
	.long	L_1+6783
	.long	divmod_pow2__div_pow2_u64_21
	.long	L_1+6799
	.long	divmod_pow2__mod_pow2_u64_20
	.long	L_1+6815
	.long	divmod_pow2__div_pow2_u64_20
	.long	L_1+6831
	.long	divmod_pow2__mod_pow2_u64_19
	.long	L_1+6847
	.long	divmod_pow2__div_pow2_u64_19
	.long	L_1+6863
	.long	divmod_pow2__mod_pow2_u64_18
	.long	L_1+6879
	.long	divmod_pow2__div_pow2_u64_18
	.long	L_1+6895
	.long	divmod_pow2__mod_pow2_u64_17
	.long	L_1+6911
	.long	divmod_pow2__div_pow2_u64_17
	.long	L_1+6927
	.long	divmod_pow2__mod_pow2_u64_16
	.long	L_1+6943
	.long	divmod_pow2__div_pow2_u64_16
	.long	L_1+6959
	.long	divmod_pow2__mod_pow2_u64_15
	.long	L_1+6975
	.long	divmod_pow2__div_pow2_u64_15
	.long	L_1+6991
	.long	divmod_pow2__mod_pow2_u64_14
	.long	L_1+7007
	.long	divmod_pow2__div_pow2_u64_14
	.long	L_1+7023
	.long	divmod_pow2__mod_pow2_u64_13
	.long	L_1+7039
	.long	divmod_pow2__div_pow2_u64_13
	.long	L_1+7055
	.long	divmod_pow2__mod_pow2_u64_12
	.long	L_1+7071
	.long	divmod_pow2__div_pow2_u64_12
	.long	L_1+7087
	.long	divmod_pow2__mod_pow2_u64_11
	.long	L_1+7103
	.long	divmod_pow2__div_pow2_u64_11
	.long	L_1+7119
	.long	divmod_pow2__mod_pow2_u64_10
	.long	L_1+7135
	.long	divmod_pow2__div_pow2_u64_10
	.long	L_1+7151
	.long	divmod_pow2__mod_pow2_u64_9
	.long	L_1+7167
	.long	divmod_pow2__div_pow2_u64_9
	.long	L_1+7182
	.long	divmod_pow2__mod_pow2_u64_8
	.long	L_1+7197
	.long	divmod_pow2__div_pow2_u64_8
	.long	L_1+7212
	.long	divmod_pow2__mod_pow2_u64_7
	.long	L_1+7227
	.long	divmod_pow2__div_pow2_u64_7
	.long	L_1+7242
	.long	divmod_pow2__mod_pow2_u64_6
	.long	L_1+7257
	.long	divmod_pow2__div_pow2_u64_6
	.long	L_1+7272
	.long	divmod_pow2__mod_pow2_u64_5
	.long	L_1+7287
	.long	divmod_pow2__div_pow2_u64_5
	.long	L_1+7302
	.long	divmod_pow2__mod_pow2_u64_4
	.long	L_1+7317
	.long	divmod_pow2__div_pow2_u64_4
	.long	L_1+7332
	.long	divmod_pow2__mod_pow2_u64_3
	.long	L_1+7347
	.long	divmod_pow2__div_pow2_u64_3
	.long	L_1+7362
	.long	divmod_pow2__mod_pow2_u64_2
	.long	L_1+7377
	.long	divmod_pow2__div_pow2_u64_2
	.long	L_1+7392
	.long	divmod_pow2__mod_pow2_u64_1
	.long	L_1+7407
	.long	divmod_pow2__div_pow2_u64_1
	.long	L_1+7422
	.long	divmod_pow2__mod_pow2_u64_0
	.long	L_1+7437
	.long	divmod_pow2__div_pow2_u64_0
	.long	L_1+7452
	.long	divmod_pow2__mod_pow2_i64_63
	.long	L_1+7467
	.long	divmod_pow2__div_pow2_i64_63
	.long	L_1+7483
	.long	divmod_pow2__mod_pow2_i64_62
	.long	L_1+7499
	.long	divmod_pow2__div_pow2_i64_62
	.long	L_1+7515
	.long	divmod_pow2__mod_pow2_i64_61
	.long	L_1+7531
	.long	divmod_pow2__div_pow2_i64_61
	.long	L_1+7547
	.long	divmod_pow2__mod_pow2_i64_60
	.long	L_1+7563
	.long	divmod_pow2__div_pow2_i64_60
	.long	L_1+7579
	.long	divmod_pow2__mod_pow2_i64_59
	.long	L_1+7595
	.long	divmod_pow2__div_pow2_i64_59
	.long	L_1+7611
	.long	divmod_pow2__mod_pow2_i64_58
	.long	L_1+7627
	.long	divmod_pow2__div_pow2_i64_58
	.long	L_1+7643
	.long	divmod_pow2__mod_pow2_i64_57
	.long	L_1+7659
	.long	divmod_pow2__div_pow2_i64_57
	.long	L_1+7675
	.long	divmod_pow2__mod_pow2_i64_56
	.long	L_1+7691
	.long	divmod_pow2__div_pow2_i64_56
	.long	L_1+7707
	.long	divmod_pow2__mod_pow2_i64_55
	.long	L_1+7723
	.long	divmod_pow2__div_pow2_i64_55
	.long	L_1+7739
	.long	divmod_pow2__mod_pow2_i64_54
	.long	L_1+7755
	.long	divmod_pow2__div_pow2_i64_54
	.long	L_1+7771
	.long	divmod_pow2__mod_pow2_i64_53
	.long	L_1+7787
	.long	divmod_pow2__div_pow2_i64_53
	.long	L_1+7803
	.long	divmod_pow2__mod_pow2_i64_52
	.long	L_1+7819
	.long	divmod_pow2__div_pow2_i64_52
	.long	L_1+7835
	.long	divmod_pow2__mod_pow2_i64_51
	.long	L_1+7851
	.long	divmod_pow2__div_pow2_i64_51
	.long	L_1+7867
	.long	divmod_pow2__mod_pow2_i64_50
	.long	L_1+7883
	.long	divmod_pow2__div_pow2_i64_50
	.long	L_1+7899
	.long	divmod_pow2__mod_pow2_i64_49
	.long	L_1+7915
	.long	divmod_pow2__div_pow2_i64_49
	.long	L_1+7931
	.long	divmod_pow2__mod_pow2_i64_48
	.long	L_1+7947
	.long	divmod_pow2__div_pow2_i64_48
	.long	L_1+7963
	.long	divmod_pow2__mod_pow2_i64_47
	.long	L_1+7979
	.long	divmod_pow2__div_pow2_i64_47
	.long	L_1+7995
	.long	divmod_pow2__mod_pow2_i64_46
	.long	L_1+8011
	.long	divmod_pow2__div_pow2_i64_46
	.long	L_1+8027
	.long	divmod_pow2__mod_pow2_i64_45
	.long	L_1+8043
	.long	divmod_pow2__div_pow2_i64_45
	.long	L_1+8059
	.long	divmod_pow2__mod_pow2_i64_44
	.long	L_1+8075
	.long	divmod_pow2__div_pow2_i64_44
	.long	L_1+8091
	.long	divmod_pow2__mod_pow2_i64_43
	.long	L_1+8107
	.long	divmod_pow2__div_pow2_i64_43
	.long	L_1+8123
	.long	divmod_pow2__mod_pow2_i64_42
	.long	L_1+8139
	.long	divmod_pow2__div_pow2_i64_42
	.long	L_1+8155
	.long	divmod_pow2__mod_pow2_i64_41
	.long	L_1+8171
	.long	divmod_pow2__div_pow2_i64_41
	.long	L_1+8187
	.long	divmod_pow2__mod_pow2_i64_40
	.long	L_1+8203
	.long	divmod_pow2__div_pow2_i64_40
	.long	L_1+8219
	.long	divmod_pow2__mod_pow2_i64_39
	.long	L_1+8235
	.long	divmod_pow2__div_pow2_i64_39
	.long	L_1+8251
	.long	divmod_pow2__mod_pow2_i64_38
	.long	L_1+8267
	.long	divmod_pow2__div_pow2_i64_38
	.long	L_1+8283
	.long	divmod_pow2__mod_pow2_i64_37
	.long	L_1+8299
	.long	divmod_pow2__div_pow2_i64_37
	.long	L_1+8315
	.long	divmod_pow2__mod_pow2_i64_36
	.long	L_1+8331
	.long	divmod_pow2__div_pow2_i64_36
	.long	L_1+8347
	.long	divmod_pow2__mod_pow2_i64_35
	.long	L_1+8363
	.long	divmod_pow2__div_pow2_i64_35
	.long	L_1+8379
	.long	divmod_pow2__mod_pow2_i64_34
	.long	L_1+8395
	.long	divmod_pow2__div_pow2_i64_34
	.long	L_1+8411
	.long	divmod_pow2__mod_pow2_i64_33
	.long	L_1+8427
	.long	divmod_pow2__div_pow2_i64_33
	.long	L_1+8443
	.long	divmod_pow2__mod_pow2_i64_32
	.long	L_1+8459
	.long	divmod_pow2__div_pow2_i64_32
	.long	L_1+8475
	.long	divmod_pow2__mod_pow2_i64_31
	.long	L_1+8491
	.long	divmod_pow2__div_pow2_i64_31
	.long	L_1+8507
	.long	divmod_pow2__mod_pow2_i64_30
	.long	L_1+8523
	.long	divmod_pow2__div_pow2_i64_30
	.long	L_1+8539
	.long	divmod_pow2__mod_pow2_i64_29
	.long	L_1+8555
	.long	divmod_pow2__div_pow2_i64_29
	.long	L_1+8571
	.long	divmod_pow2__mod_pow2_i64_28
	.long	L_1+8587
	.long	divmod_pow2__div_pow2_i64_28
	.long	L_1+8603
	.long	divmod_pow2__mod_pow2_i64_27
	.long	L_1+8619
	.long	divmod_pow2__div_pow2_i64_27
	.long	L_1+8635
	.long	divmod_pow2__mod_pow2_i64_26
	.long	L_1+8651
	.long	divmod_pow2__div_pow2_i64_26
	.long	L_1+8667
	.long	divmod_pow2__mod_pow2_i64_25
	.long	L_1+8683
	.long	divmod_pow2__div_pow2_i64_25
	.long	L_1+8699
	.long	divmod_pow2__mod_pow2_i64_24
	.long	L_1+8715
	.long	divmod_pow2__div_pow2_i64_24
	.long	L_1+8731
	.long	divmod_pow2__mod_pow2_i64_23
	.long	L_1+8747
	.long	divmod_pow2__div_pow2_i64_23
	.long	L_1+8763
	.long	divmod_pow2__mod_pow2_i64_22
	.long	L_1+8779
	.long	divmod_pow2__div_pow2_i64_22
	.long	L_1+8795
	.long	divmod_pow2__mod_pow2_i64_21
	.long	L_1+8811
	.long	divmod_pow2__div_pow2_i64_21
	.long	L_1+8827
	.long	divmod_pow2__mod_pow2_i64_20
	.long	L_1+8843
	.long	divmod_pow2__div_pow2_i64_20
	.long	L_1+8859
	.long	divmod_pow2__mod_pow2_i64_19
	.long	L_1+8875
	.long	divmod_pow2__div_pow2_i64_19
	.long	L_1+8891
	.long	divmod_pow2__mod_pow2_i64_18
	.long	L_1+8907
	.long	divmod_pow2__div_pow2_i64_18
	.long	L_1+8923
	.long	divmod_pow2__mod_pow2_i64_17
	.long	L_1+8939
	.long	divmod_pow2__div_pow2_i64_17
	.long	L_1+8955
	.long	divmod_pow2__mod_pow2_i64_16
	.long	L_1+8971
	.long	divmod_pow2__div_pow2_i64_16
	.long	L_1+8987
	.long	divmod_pow2__mod_pow2_i64_15
	.long	L_1+9003
	.long	divmod_pow2__div_pow2_i64_15
	.long	L_1+9019
	.long	divmod_pow2__mod_pow2_i64_14
	.long	L_1+9035
	.long	divmod_pow2__div_pow2_i64_14
	.long	L_1+9051
	.long	divmod_pow2__mod_pow2_i64_13
	.long	L_1+9067
	.long	divmod_pow2__div_pow2_i64_13
	.long	L_1+9083
	.long	divmod_pow2__mod_pow2_i64_12
	.long	L_1+9099
	.long	divmod_pow2__div_pow2_i64_12
	.long	L_1+9115
	.long	divmod_pow2__mod_pow2_i64_11
	.long	L_1+9131
	.long	divmod_pow2__div_pow2_i64_11
	.long	L_1+9147
	.long	divmod_pow2__mod_pow2_i64_10
	.long	L_1+9163
	.long	divmod_pow2__div_pow2_i64_10
	.long	L_1+9179
	.long	divmod_pow2__mod_pow2_i64_9
	.long	L_1+9195
	.long	divmod_pow2__div_pow2_i64_9
	.long	L_1+9210
	.long	divmod_pow2__mod_pow2_i64_8
	.long	L_1+9225
	.long	divmod_pow2__div_pow2_i64_8
	.long	L_1+9240
	.long	divmod_pow2__mod_pow2_i64_7
	.long	L_1+9255
	.long	divmod_pow2__div_pow2_i64_7
	.long	L_1+9270
	.long	divmod_pow2__mod_pow2_i64_6
	.long	L_1+9285
	.long	divmod_pow2__div_pow2_i64_6
	.long	L_1+9300
	.long	divmod_pow2__mod_pow2_i64_5
	.long	L_1+9315
	.long	divmod_pow2__div_pow2_i64_5
	.long	L_1+9330
	.long	divmod_pow2__mod_pow2_i64_4
	.long	L_1+9345
	.long	divmod_pow2__div_pow2_i64_4
	.long	L_1+9360
	.long	divmod_pow2__mod_pow2_i64_3
	.long	L_1+9375
	.long	divmod_pow2__div_pow2_i64_3
	.long	L_1+9390
	.long	divmod_pow2__mod_pow2_i64_2
	.long	L_1+9405
	.long	divmod_pow2__div_pow2_i64_2
	.long	L_1+9420
	.long	divmod_pow2__mod_pow2_i64_1
	.long	L_1+9435
	.long	divmod_pow2__div_pow2_i64_1
	.long	L_1+9450
	.long	divmod_pow2__mod_pow2_i64_0
	.long	L_1+9465
	.long	divmod_pow2__div_pow2_i64_0
	.long	L_1+9480
	.long	divmod_pow2__mod_pow2_u32_31
	.long	L_1+9495
	.long	divmod_pow2__div_pow2_u32_31
	.long	L_1+9511
	.long	divmod_pow2__mod_pow2_u32_30
	.long	L_1+9527
	.long	divmod_pow2__div_pow2_u32_30
	.long	L_1+9543
	.long	divmod_pow2__mod_pow2_u32_29
	.long	L_1+9559
	.long	divmod_pow2__div_pow2_u32_29
	.long	L_1+9575
	.long	divmod_pow2__mod_pow2_u32_28
	.long	L_1+9591
	.long	divmod_pow2__div_pow2_u32_28
	.long	L_1+9607
	.long	divmod_pow2__mod_pow2_u32_27
	.long	L_1+9623
	.long	divmod_pow2__div_pow2_u32_27
	.long	L_1+9639
	.long	divmod_pow2__mod_pow2_u32_26
	.long	L_1+9655
	.long	divmod_pow2__div_pow2_u32_26
	.long	L_1+9671
	.long	divmod_pow2__mod_pow2_u32_25
	.long	L_1+9687
	.long	divmod_pow2__div_pow2_u32_25
	.long	L_1+9703
	.long	divmod_pow2__mod_pow2_u32_24
	.long	L_1+9719
	.long	divmod_pow2__div_pow2_u32_24
	.long	L_1+9735
	.long	divmod_pow2__mod_pow2_u32_23
	.long	L_1+9751
	.long	divmod_pow2__div_pow2_u32_23
	.long	L_1+9767
	.long	divmod_pow2__mod_pow2_u32_22
	.long	L_1+9783
	.long	divmod_pow2__div_pow2_u32_22
	.long	L_1+9799
	.long	divmod_pow2__mod_pow2_u32_21
	.long	L_1+9815
	.long	divmod_pow2__div_pow2_u32_21
	.long	L_1+9831
	.long	divmod_pow2__mod_pow2_u32_20
	.long	L_1+9847
	.long	divmod_pow2__div_pow2_u32_20
	.long	L_1+9863
	.long	divmod_pow2__mod_pow2_u32_19
	.long	L_1+9879
	.long	divmod_pow2__div_pow2_u32_19
	.long	L_1+9895
	.long	divmod_pow2__mod_pow2_u32_18
	.long	L_1+9911
	.long	divmod_pow2__div_pow2_u32_18
	.long	L_1+9927
	.long	divmod_pow2__mod_pow2_u32_17
	.long	L_1+9943
	.long	divmod_pow2__div_pow2_u32_17
	.long	L_1+9959
	.long	divmod_pow2__mod_pow2_u32_16
	.long	L_1+9975
	.long	divmod_pow2__div_pow2_u32_16
	.long	L_1+9991
	.long	divmod_pow2__mod_pow2_u32_15
	.long	L_1+10007
	.long	divmod_pow2__div_pow2_u32_15
	.long	L_1+10023
	.long	divmod_pow2__mod_pow2_u32_14
	.long	L_1+10039
	.long	divmod_pow2__div_pow2_u32_14
	.long	L_1+10055
	.long	divmod_pow2__mod_pow2_u32_13
	.long	L_1+10071
	.long	divmod_pow2__div_pow2_u32_13
	.long	L_1+10087
	.long	divmod_pow2__mod_pow2_u32_12
	.long	L_1+10103
	.long	divmod_pow2__div_pow2_u32_12
	.long	L_1+10119
	.long	divmod_pow2__mod_pow2_u32_11
	.long	L_1+10135
	.long	divmod_pow2__div_pow2_u32_11
	.long	L_1+10151
	.long	divmod_pow2__mod_pow2_u32_10
	.long	L_1+10167
	.long	divmod_pow2__div_pow2_u32_10
	.long	L_1+10183
	.long	divmod_pow2__mod_pow2_u32_9
	.long	L_1+10199
	.long	divmod_pow2__div_pow2_u32_9
	.long	L_1+10214
	.long	divmod_pow2__mod_pow2_u32_8
	.long	L_1+10229
	.long	divmod_pow2__div_pow2_u32_8
	.long	L_1+10244
	.long	divmod_pow2__mod_pow2_u32_7
	.long	L_1+10259
	.long	divmod_pow2__div_pow2_u32_7
	.long	L_1+10274
	.long	divmod_pow2__mod_pow2_u32_6
	.long	L_1+10289
	.long	divmod_pow2__div_pow2_u32_6
	.long	L_1+10304
	.long	divmod_pow2__mod_pow2_u32_5
	.long	L_1+10319
	.long	divmod_pow2__div_pow2_u32_5
	.long	L_1+10334
	.long	divmod_pow2__mod_pow2_u32_4
	.long	L_1+10349
	.long	divmod_pow2__div_pow2_u32_4
	.long	L_1+10364
	.long	divmod_pow2__mod_pow2_u32_3
	.long	L_1+10379
	.long	divmod_pow2__div_pow2_u32_3
	.long	L_1+10394
	.long	divmod_pow2__mod_pow2_u32_2
	.long	L_1+10409
	.long	divmod_pow2__div_pow2_u32_2
	.long	L_1+10424
	.long	divmod_pow2__mod_pow2_u32_1
	.long	L_1+10439
	.long	divmod_pow2__div_pow2_u32_1
	.long	L_1+10454
	.long	divmod_pow2__mod_pow2_u32_0
	.long	L_1+10469
	.long	divmod_pow2__div_pow2_u32_0
	.long	L_1+10484
	.long	divmod_pow2__mod_pow2_i32_31
	.long	L_1+10499
	.long	divmod_pow2__div_pow2_i32_31
	.long	L_1+10515
	.long	divmod_pow2__mod_pow2_i32_30
	.long	L_1+10531
	.long	divmod_pow2__div_pow2_i32_30
	.long	L_1+10547
	.long	divmod_pow2__mod_pow2_i32_29
	.long	L_1+10563
	.long	divmod_pow2__div_pow2_i32_29
	.long	L_1+10579
	.long	divmod_pow2__mod_pow2_i32_28
	.long	L_1+10595
	.long	divmod_pow2__div_pow2_i32_28
	.long	L_1+10611
	.long	divmod_pow2__mod_pow2_i32_27
	.long	L_1+10627
	.long	divmod_pow2__div_pow2_i32_27
	.long	L_1+10643
	.long	divmod_pow2__mod_pow2_i32_26
	.long	L_1+10659
	.long	divmod_pow2__div_pow2_i32_26
	.long	L_1+10675
	.long	divmod_pow2__mod_pow2_i32_25
	.long	L_1+10691
	.long	divmod_pow2__div_pow2_i32_25
	.long	L_1+10707
	.long	divmod_pow2__mod_pow2_i32_24
	.long	L_1+10723
	.long	divmod_pow2__div_pow2_i32_24
	.long	L_1+10739
	.long	divmod_pow2__mod_pow2_i32_23
	.long	L_1+10755
	.long	divmod_pow2__div_pow2_i32_23
	.long	L_1+10771
	.long	divmod_pow2__mod_pow2_i32_22
	.long	L_1+10787
	.long	divmod_pow2__div_pow2_i32_22
	.long	L_1+10803
	.long	divmod_pow2__mod_pow2_i32_21
	.long	L_1+10819
	.long	divmod_pow2__div_pow2_i32_21
	.long	L_1+10835
	.long	divmod_pow2__mod_pow2_i32_20
	.long	L_1+10851
	.long	divmod_pow2__div_pow2_i32_20
	.long	L_1+10867
	.long	divmod_pow2__mod_pow2_i32_19
	.long	L_1+10883
	.long	divmod_pow2__div_pow2_i32_19
	.long	L_1+10899
	.long	divmod_pow2__mod_pow2_i32_18
	.long	L_1+10915
	.long	divmod_pow2__div_pow2_i32_18
	.long	L_1+10931
	.long	divmod_pow2__mod_pow2_i32_17
	.long	L_1+10947
	.long	divmod_pow2__div_pow2_i32_17
	.long	L_1+10963
	.long	divmod_pow2__mod_pow2_i32_16
	.long	L_1+10979
	.long	divmod_pow2__div_pow2_i32_16
	.long	L_1+10995
	.long	divmod_pow2__mod_pow2_i32_15
	.long	L_1+11011
	.long	divmod_pow2__div_pow2_i32_15
	.long	L_1+11027
	.long	divmod_pow2__mod_pow2_i32_14
	.long	L_1+11043
	.long	divmod_pow2__div_pow2_i32_14
	.long	L_1+11059
	.long	divmod_pow2__mod_pow2_i32_13
	.long	L_1+11075
	.long	divmod_pow2__div_pow2_i32_13
	.long	L_1+11091
	.long	divmod_pow2__mod_pow2_i32_12
	.long	L_1+11107
	.long	divmod_pow2__div_pow2_i32_12
	.long	L_1+11123
	.long	divmod_pow2__mod_pow2_i32_11
	.long	L_1+11139
	.long	divmod_pow2__div_pow2_i32_11
	.long	L_1+11155
	.long	divmod_pow2__mod_pow2_i32_10
	.long	L_1+11171
	.long	divmod_pow2__div_pow2_i32_10
	.long	L_1+11187
	.long	divmod_pow2__mod_pow2_i32_9
	.long	L_1+11203
	.long	divmod_pow2__div_pow2_i32_9
	.long	L_1+11218
	.long	divmod_pow2__mod_pow2_i32_8
	.long	L_1+11233
	.long	divmod_pow2__div_pow2_i32_8
	.long	L_1+11248
	.long	divmod_pow2__mod_pow2_i32_7
	.long	L_1+11263
	.long	divmod_pow2__div_pow2_i32_7
	.long	L_1+11278
	.long	divmod_pow2__mod_pow2_i32_6
	.long	L_1+11293
	.long	divmod_pow2__div_pow2_i32_6
	.long	L_1+11308
	.long	divmod_pow2__mod_pow2_i32_5
	.long	L_1+11323
	.long	divmod_pow2__div_pow2_i32_5
	.long	L_1+11338
	.long	divmod_pow2__mod_pow2_i32_4
	.long	L_1+11353
	.long	divmod_pow2__div_pow2_i32_4
	.long	L_1+11368
	.long	divmod_pow2__mod_pow2_i32_3
	.long	L_1+11383
	.long	divmod_pow2__div_pow2_i32_3
	.long	L_1+11398
	.long	divmod_pow2__mod_pow2_i32_2
	.long	L_1+11413
	.long	divmod_pow2__div_pow2_i32_2
	.long	L_1+11428
	.long	divmod_pow2__mod_pow2_i32_1
	.long	L_1+11443
	.long	divmod_pow2__div_pow2_i32_1
	.long	L_1+11458
	.long	divmod_pow2__mod_pow2_i32_0
	.long	L_1+11473
	.long	divmod_pow2__div_pow2_i32_0
	.long	L_1+11488
	.skip 4
	.ascii	"../SOLgnu/divmod_pow2.m3"
	.skip 4
	.align 8
	.type	MM_divmod_pow2, #object
	.size	MM_divmod_pow2, 208
MM_divmod_pow2:
	.long	L_1+17660
	.skip 16
	.long	L_1+11504
	.skip 12
	.long	MM_divmod_pow2+144
	.skip 4
	.long	divmod_pow2_M3
	.long	3
	.byte	57
	.skip 3
	.long	0
	.long	1082
	.long	0
	.long	1083
	.byte	60
	.skip 3
	.long	1085
	.long	1086
	.long	1087
	.half	1088
	.skip 6
	.long	0
	.long	1089
	.long	1083246703
	.long	-1209462791
	.half	1092
	.skip 6
	.long	0
	.long	1093
	.long	1149813633
	.long	1096
	.skip 12
	.long	divmod_pow2_I3
	.long	MM_divmod_pow2+156
	.skip 4
	.long	Long_I3
	.long	MM_divmod_pow2+168
	.skip 4
	.long	Word_I3
	.long	MM_divmod_pow2+180
	.skip 4
	.long	Cstdint_I3
	.long	MM_divmod_pow2+192
	.skip 4
	.long	RTHooks_I3
	.skip 8
	.ident	"GCC: (GNU) 4.3.0"
