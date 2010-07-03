	.file	"bitfield.mc"
	.section	".text"
	.align 4
	.global bitfield__read_var_BitField_offset1_count1
	.hidden	bitfield__read_var_BitField_offset1_count1
	.type	bitfield__read_var_BitField_offset1_count1, #function
	.proc	04
bitfield__read_var_BitField_offset1_count1:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset1_count1, .-bitfield__read_var_BitField_offset1_count1
	.align 4
	.global bitfield__write_var_BitField_offset1_count1
	.hidden	bitfield__write_var_BitField_offset1_count1
	.type	bitfield__write_var_BitField_offset1_count1, #function
	.proc	020
bitfield__write_var_BitField_offset1_count1:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL4
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1
	ble	%icc, .LL5
	 nop
.LL4:
	mov	1953, %o0
	call	_m3_fault, 0
	 nop
.LL5:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -65, %g2
	ld	[%fp-24], %g1
	and	%g1, 1, %g1
	sll	%g1, 6, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset1_count1, .-bitfield__write_var_BitField_offset1_count1
	.align 4
	.global bitfield__read_var_BitField_offset1_count2
	.hidden	bitfield__read_var_BitField_offset1_count2
	.type	bitfield__read_var_BitField_offset1_count2, #function
	.proc	04
bitfield__read_var_BitField_offset1_count2:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset1_count2, .-bitfield__read_var_BitField_offset1_count2
	.align 4
	.global bitfield__write_var_BitField_offset1_count2
	.hidden	bitfield__write_var_BitField_offset1_count2
	.type	bitfield__write_var_BitField_offset1_count2, #function
	.proc	020
bitfield__write_var_BitField_offset1_count2:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL10
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 3
	ble	%icc, .LL11
	 nop
.LL10:
	mov	2209, %o0
	call	_m3_fault, 0
	 nop
.LL11:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -97, %g2
	ld	[%fp-24], %g1
	and	%g1, 3, %g1
	sll	%g1, 5, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset1_count2, .-bitfield__write_var_BitField_offset1_count2
	.align 4
	.global bitfield__read_var_BitField_offset1_count3
	.hidden	bitfield__read_var_BitField_offset1_count3
	.type	bitfield__read_var_BitField_offset1_count3, #function
	.proc	04
bitfield__read_var_BitField_offset1_count3:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset1_count3, .-bitfield__read_var_BitField_offset1_count3
	.align 4
	.global bitfield__write_var_BitField_offset1_count3
	.hidden	bitfield__write_var_BitField_offset1_count3
	.type	bitfield__write_var_BitField_offset1_count3, #function
	.proc	020
bitfield__write_var_BitField_offset1_count3:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL16
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 7
	ble	%icc, .LL17
	 nop
.LL16:
	mov	2465, %o0
	call	_m3_fault, 0
	 nop
.LL17:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -113, %g2
	ld	[%fp-24], %g1
	and	%g1, 7, %g1
	sll	%g1, 4, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset1_count3, .-bitfield__write_var_BitField_offset1_count3
	.align 4
	.global bitfield__read_var_BitField_offset1_count4
	.hidden	bitfield__read_var_BitField_offset1_count4
	.type	bitfield__read_var_BitField_offset1_count4, #function
	.proc	04
bitfield__read_var_BitField_offset1_count4:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset1_count4, .-bitfield__read_var_BitField_offset1_count4
	.align 4
	.global bitfield__write_var_BitField_offset1_count4
	.hidden	bitfield__write_var_BitField_offset1_count4
	.type	bitfield__write_var_BitField_offset1_count4, #function
	.proc	020
bitfield__write_var_BitField_offset1_count4:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL22
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 15
	ble	%icc, .LL23
	 nop
.LL22:
	mov	2721, %o0
	call	_m3_fault, 0
	 nop
.LL23:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -121, %g2
	ld	[%fp-24], %g1
	and	%g1, 15, %g1
	sll	%g1, 3, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset1_count4, .-bitfield__write_var_BitField_offset1_count4
	.align 4
	.global bitfield__read_var_BitField_offset1_count5
	.hidden	bitfield__read_var_BitField_offset1_count5
	.type	bitfield__read_var_BitField_offset1_count5, #function
	.proc	04
bitfield__read_var_BitField_offset1_count5:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset1_count5, .-bitfield__read_var_BitField_offset1_count5
	.align 4
	.global bitfield__write_var_BitField_offset1_count5
	.hidden	bitfield__write_var_BitField_offset1_count5
	.type	bitfield__write_var_BitField_offset1_count5, #function
	.proc	020
bitfield__write_var_BitField_offset1_count5:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL28
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 31
	ble	%icc, .LL29
	 nop
.LL28:
	mov	2977, %o0
	call	_m3_fault, 0
	 nop
.LL29:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -125, %g2
	ld	[%fp-24], %g1
	and	%g1, 31, %g1
	sll	%g1, 2, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset1_count5, .-bitfield__write_var_BitField_offset1_count5
	.align 4
	.global bitfield__read_var_BitField_offset1_count6
	.hidden	bitfield__read_var_BitField_offset1_count6
	.type	bitfield__read_var_BitField_offset1_count6, #function
	.proc	04
bitfield__read_var_BitField_offset1_count6:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset1_count6, .-bitfield__read_var_BitField_offset1_count6
	.align 4
	.global bitfield__write_var_BitField_offset1_count6
	.hidden	bitfield__write_var_BitField_offset1_count6
	.type	bitfield__write_var_BitField_offset1_count6, #function
	.proc	020
bitfield__write_var_BitField_offset1_count6:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL34
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 63
	ble	%icc, .LL35
	 nop
.LL34:
	mov	3233, %o0
	call	_m3_fault, 0
	 nop
.LL35:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -127, %g2
	ld	[%fp-24], %g1
	and	%g1, 63, %g1
	add	%g1, %g1, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset1_count6, .-bitfield__write_var_BitField_offset1_count6
	.align 4
	.global bitfield__read_var_BitField_offset1_count7
	.hidden	bitfield__read_var_BitField_offset1_count7
	.type	bitfield__read_var_BitField_offset1_count7, #function
	.proc	04
bitfield__read_var_BitField_offset1_count7:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, 127, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset1_count7, .-bitfield__read_var_BitField_offset1_count7
	.align 4
	.global bitfield__write_var_BitField_offset1_count7
	.hidden	bitfield__write_var_BitField_offset1_count7
	.type	bitfield__write_var_BitField_offset1_count7, #function
	.proc	020
bitfield__write_var_BitField_offset1_count7:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL40
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 127
	ble	%icc, .LL41
	 nop
.LL40:
	mov	3489, %o0
	call	_m3_fault, 0
	 nop
.LL41:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -128, %g2
	ld	[%fp-24], %g1
	and	%g1, 127, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset1_count7, .-bitfield__write_var_BitField_offset1_count7
	.align 4
	.global bitfield__read_var_BitField_offset1_count8
	.hidden	bitfield__read_var_BitField_offset1_count8
	.type	bitfield__read_var_BitField_offset1_count8, #function
	.proc	04
bitfield__read_var_BitField_offset1_count8:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 17, %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset1_count8, .-bitfield__read_var_BitField_offset1_count8
	.align 4
	.global bitfield__write_var_BitField_offset1_count8
	.hidden	bitfield__write_var_BitField_offset1_count8
	.type	bitfield__write_var_BitField_offset1_count8, #function
	.proc	020
bitfield__write_var_BitField_offset1_count8:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL46
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 255
	ble	%icc, .LL47
	 nop
.LL46:
	mov	3745, %o0
	call	_m3_fault, 0
	 nop
.LL47:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-32768), %g1
	or	%g1, 127, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 255, %g1
	sll	%g1, 7, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset1_count8, .-bitfield__write_var_BitField_offset1_count8
	.align 4
	.global bitfield__read_var_BitField_offset1_count9
	.hidden	bitfield__read_var_BitField_offset1_count9
	.type	bitfield__read_var_BitField_offset1_count9, #function
	.proc	04
bitfield__read_var_BitField_offset1_count9:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 17, %g1
	srl	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset1_count9, .-bitfield__read_var_BitField_offset1_count9
	.align 4
	.global bitfield__write_var_BitField_offset1_count9
	.hidden	bitfield__write_var_BitField_offset1_count9
	.type	bitfield__write_var_BitField_offset1_count9, #function
	.proc	020
bitfield__write_var_BitField_offset1_count9:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL52
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 511
	ble	%icc, .LL53
	 nop
.LL52:
	mov	4001, %o0
	call	_m3_fault, 0
	 nop
.LL53:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-32768), %g1
	or	%g1, 63, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 511, %g1
	sll	%g1, 6, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset1_count9, .-bitfield__write_var_BitField_offset1_count9
	.align 4
	.global bitfield__read_var_BitField_offset1_count10
	.hidden	bitfield__read_var_BitField_offset1_count10
	.type	bitfield__read_var_BitField_offset1_count10, #function
	.proc	04
bitfield__read_var_BitField_offset1_count10:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 17, %g1
	srl	%g1, 22, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset1_count10, .-bitfield__read_var_BitField_offset1_count10
	.align 4
	.global bitfield__write_var_BitField_offset1_count10
	.hidden	bitfield__write_var_BitField_offset1_count10
	.type	bitfield__write_var_BitField_offset1_count10, #function
	.proc	020
bitfield__write_var_BitField_offset1_count10:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL58
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1023
	ble	%icc, .LL59
	 nop
.LL58:
	sethi	%hi(4096), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL59:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-32768), %g1
	or	%g1, 31, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 1023, %g1
	sll	%g1, 5, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset1_count10, .-bitfield__write_var_BitField_offset1_count10
	.align 4
	.global bitfield__read_var_BitField_offset1_count11
	.hidden	bitfield__read_var_BitField_offset1_count11
	.type	bitfield__read_var_BitField_offset1_count11, #function
	.proc	04
bitfield__read_var_BitField_offset1_count11:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 17, %g1
	srl	%g1, 21, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset1_count11, .-bitfield__read_var_BitField_offset1_count11
	.align 4
	.global bitfield__write_var_BitField_offset1_count11
	.hidden	bitfield__write_var_BitField_offset1_count11
	.type	bitfield__write_var_BitField_offset1_count11, #function
	.proc	020
bitfield__write_var_BitField_offset1_count11:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL64
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 2047
	ble	%icc, .LL65
	 nop
.LL64:
	sethi	%hi(4096), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL65:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-32768), %g1
	or	%g1, 15, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 2047, %g1
	sll	%g1, 4, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset1_count11, .-bitfield__write_var_BitField_offset1_count11
	.align 4
	.global bitfield__read_var_BitField_offset1_count12
	.hidden	bitfield__read_var_BitField_offset1_count12
	.type	bitfield__read_var_BitField_offset1_count12, #function
	.proc	04
bitfield__read_var_BitField_offset1_count12:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 17, %g1
	srl	%g1, 20, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset1_count12, .-bitfield__read_var_BitField_offset1_count12
	.align 4
	.global bitfield__write_var_BitField_offset1_count12
	.hidden	bitfield__write_var_BitField_offset1_count12
	.type	bitfield__write_var_BitField_offset1_count12, #function
	.proc	020
bitfield__write_var_BitField_offset1_count12:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL70
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 4095
	ble	%icc, .LL71
	 nop
.LL70:
	sethi	%hi(4096), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL71:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-32768), %g1
	or	%g1, 7, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 4095, %g1
	sll	%g1, 3, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset1_count12, .-bitfield__write_var_BitField_offset1_count12
	.align 4
	.global bitfield__read_var_BitField_offset1_count13
	.hidden	bitfield__read_var_BitField_offset1_count13
	.type	bitfield__read_var_BitField_offset1_count13, #function
	.proc	04
bitfield__read_var_BitField_offset1_count13:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 17, %g1
	srl	%g1, 19, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset1_count13, .-bitfield__read_var_BitField_offset1_count13
	.align 4
	.global bitfield__write_var_BitField_offset1_count13
	.hidden	bitfield__write_var_BitField_offset1_count13
	.type	bitfield__write_var_BitField_offset1_count13, #function
	.proc	020
bitfield__write_var_BitField_offset1_count13:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL76
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(7168), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL77
	 nop
.LL76:
	sethi	%hi(4096), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL77:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-32768), %g1
	or	%g1, 3, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 2, %g1
	or	%g3, %g1, %g1
	sth	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset1_count13, .-bitfield__write_var_BitField_offset1_count13
	.align 4
	.global bitfield__read_var_BitField_offset1_count14
	.hidden	bitfield__read_var_BitField_offset1_count14
	.type	bitfield__read_var_BitField_offset1_count14, #function
	.proc	04
bitfield__read_var_BitField_offset1_count14:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 17, %g1
	srl	%g1, 18, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset1_count14, .-bitfield__read_var_BitField_offset1_count14
	.align 4
	.global bitfield__write_var_BitField_offset1_count14
	.hidden	bitfield__write_var_BitField_offset1_count14
	.type	bitfield__write_var_BitField_offset1_count14, #function
	.proc	020
bitfield__write_var_BitField_offset1_count14:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL82
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(15360), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL83
	 nop
.LL82:
	sethi	%hi(5120), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL83:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-32768), %g1
	or	%g1, 1, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	add	%g1, %g1, %g1
	or	%g3, %g1, %g1
	sth	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset1_count14, .-bitfield__write_var_BitField_offset1_count14
	.align 4
	.global bitfield__read_var_BitField_offset1_count15
	.hidden	bitfield__read_var_BitField_offset1_count15
	.type	bitfield__read_var_BitField_offset1_count15, #function
	.proc	04
bitfield__read_var_BitField_offset1_count15:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset1_count15, .-bitfield__read_var_BitField_offset1_count15
	.align 4
	.global bitfield__write_var_BitField_offset1_count15
	.hidden	bitfield__write_var_BitField_offset1_count15
	.type	bitfield__write_var_BitField_offset1_count15, #function
	.proc	020
bitfield__write_var_BitField_offset1_count15:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL88
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(31744), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL89
	 nop
.LL88:
	sethi	%hi(5120), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL89:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-32768), %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	or	%g3, %g1, %g1
	sth	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset1_count15, .-bitfield__write_var_BitField_offset1_count15
	.align 4
	.global bitfield__read_var_BitField_offset1_count16
	.hidden	bitfield__read_var_BitField_offset1_count16
	.type	bitfield__read_var_BitField_offset1_count16, #function
	.proc	04
bitfield__read_var_BitField_offset1_count16:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	add	%g1, %g1, %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset1_count16, .-bitfield__read_var_BitField_offset1_count16
	.align 4
	.global bitfield__write_var_BitField_offset1_count16
	.hidden	bitfield__write_var_BitField_offset1_count16
	.type	bitfield__write_var_BitField_offset1_count16, #function
	.proc	020
bitfield__write_var_BitField_offset1_count16:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL94
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(64512), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL95
	 nop
.LL94:
	sethi	%hi(5120), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL95:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-2147451904), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 15, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset1_count16, .-bitfield__write_var_BitField_offset1_count16
	.align 4
	.global bitfield__read_var_BitField_offset1_count17
	.hidden	bitfield__read_var_BitField_offset1_count17
	.type	bitfield__read_var_BitField_offset1_count17, #function
	.proc	04
bitfield__read_var_BitField_offset1_count17:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	add	%g1, %g1, %g1
	srl	%g1, 15, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset1_count17, .-bitfield__read_var_BitField_offset1_count17
	.align 4
	.global bitfield__write_var_BitField_offset1_count17
	.hidden	bitfield__write_var_BitField_offset1_count17
	.type	bitfield__write_var_BitField_offset1_count17, #function
	.proc	020
bitfield__write_var_BitField_offset1_count17:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL100
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(130048), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL101
	 nop
.LL100:
	sethi	%hi(5120), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL101:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-2147468288), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(130048), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 14, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset1_count17, .-bitfield__write_var_BitField_offset1_count17
	.align 4
	.global bitfield__read_var_BitField_offset1_count18
	.hidden	bitfield__read_var_BitField_offset1_count18
	.type	bitfield__read_var_BitField_offset1_count18, #function
	.proc	04
bitfield__read_var_BitField_offset1_count18:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	add	%g1, %g1, %g1
	srl	%g1, 14, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset1_count18, .-bitfield__read_var_BitField_offset1_count18
	.align 4
	.global bitfield__write_var_BitField_offset1_count18
	.hidden	bitfield__write_var_BitField_offset1_count18
	.type	bitfield__write_var_BitField_offset1_count18, #function
	.proc	020
bitfield__write_var_BitField_offset1_count18:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL106
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(261120), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL107
	 nop
.LL106:
	sethi	%hi(6144), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL107:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-2147476480), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(261120), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 13, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset1_count18, .-bitfield__write_var_BitField_offset1_count18
	.align 4
	.global bitfield__read_var_BitField_offset1_count19
	.hidden	bitfield__read_var_BitField_offset1_count19
	.type	bitfield__read_var_BitField_offset1_count19, #function
	.proc	04
bitfield__read_var_BitField_offset1_count19:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	add	%g1, %g1, %g1
	srl	%g1, 13, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset1_count19, .-bitfield__read_var_BitField_offset1_count19
	.align 4
	.global bitfield__write_var_BitField_offset1_count19
	.hidden	bitfield__write_var_BitField_offset1_count19
	.type	bitfield__write_var_BitField_offset1_count19, #function
	.proc	020
bitfield__write_var_BitField_offset1_count19:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL112
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(523264), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL113
	 nop
.LL112:
	sethi	%hi(6144), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL113:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-2147480576), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(523264), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 12, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset1_count19, .-bitfield__write_var_BitField_offset1_count19
	.align 4
	.global bitfield__read_var_BitField_offset1_count20
	.hidden	bitfield__read_var_BitField_offset1_count20
	.type	bitfield__read_var_BitField_offset1_count20, #function
	.proc	04
bitfield__read_var_BitField_offset1_count20:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	add	%g1, %g1, %g1
	srl	%g1, 12, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset1_count20, .-bitfield__read_var_BitField_offset1_count20
	.align 4
	.global bitfield__write_var_BitField_offset1_count20
	.hidden	bitfield__write_var_BitField_offset1_count20
	.type	bitfield__write_var_BitField_offset1_count20, #function
	.proc	020
bitfield__write_var_BitField_offset1_count20:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL118
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(1047552), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL119
	 nop
.LL118:
	sethi	%hi(6144), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL119:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-2147482624), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(1047552), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 11, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset1_count20, .-bitfield__write_var_BitField_offset1_count20
	.align 4
	.global bitfield__read_var_BitField_offset1_count21
	.hidden	bitfield__read_var_BitField_offset1_count21
	.type	bitfield__read_var_BitField_offset1_count21, #function
	.proc	04
bitfield__read_var_BitField_offset1_count21:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	add	%g1, %g1, %g1
	srl	%g1, 11, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset1_count21, .-bitfield__read_var_BitField_offset1_count21
	.align 4
	.global bitfield__write_var_BitField_offset1_count21
	.hidden	bitfield__write_var_BitField_offset1_count21
	.type	bitfield__write_var_BitField_offset1_count21, #function
	.proc	020
bitfield__write_var_BitField_offset1_count21:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL124
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(2096128), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL125
	 nop
.LL124:
	sethi	%hi(6144), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL125:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-2147483648), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(2096128), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 10, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset1_count21, .-bitfield__write_var_BitField_offset1_count21
	.align 4
	.global bitfield__read_var_BitField_offset1_count22
	.hidden	bitfield__read_var_BitField_offset1_count22
	.type	bitfield__read_var_BitField_offset1_count22, #function
	.proc	04
bitfield__read_var_BitField_offset1_count22:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	add	%g1, %g1, %g1
	srl	%g1, 10, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset1_count22, .-bitfield__read_var_BitField_offset1_count22
	.align 4
	.global bitfield__write_var_BitField_offset1_count22
	.hidden	bitfield__write_var_BitField_offset1_count22
	.type	bitfield__write_var_BitField_offset1_count22, #function
	.proc	020
bitfield__write_var_BitField_offset1_count22:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL130
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(4193280), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL131
	 nop
.LL130:
	sethi	%hi(7168), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL131:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-2147483648), %g1
	or	%g1, 511, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(4193280), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 9, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset1_count22, .-bitfield__write_var_BitField_offset1_count22
	.align 4
	.global bitfield__read_var_BitField_offset1_count23
	.hidden	bitfield__read_var_BitField_offset1_count23
	.type	bitfield__read_var_BitField_offset1_count23, #function
	.proc	04
bitfield__read_var_BitField_offset1_count23:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	add	%g1, %g1, %g1
	srl	%g1, 9, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset1_count23, .-bitfield__read_var_BitField_offset1_count23
	.align 4
	.global bitfield__write_var_BitField_offset1_count23
	.hidden	bitfield__write_var_BitField_offset1_count23
	.type	bitfield__write_var_BitField_offset1_count23, #function
	.proc	020
bitfield__write_var_BitField_offset1_count23:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL136
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(8387584), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL137
	 nop
.LL136:
	sethi	%hi(7168), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL137:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-2147483648), %g1
	or	%g1, 255, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(8387584), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 8, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset1_count23, .-bitfield__write_var_BitField_offset1_count23
	.align 4
	.global bitfield__read_var_BitField_offset1_count24
	.hidden	bitfield__read_var_BitField_offset1_count24
	.type	bitfield__read_var_BitField_offset1_count24, #function
	.proc	04
bitfield__read_var_BitField_offset1_count24:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	add	%g1, %g1, %g1
	srl	%g1, 8, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset1_count24, .-bitfield__read_var_BitField_offset1_count24
	.align 4
	.global bitfield__write_var_BitField_offset1_count24
	.hidden	bitfield__write_var_BitField_offset1_count24
	.type	bitfield__write_var_BitField_offset1_count24, #function
	.proc	020
bitfield__write_var_BitField_offset1_count24:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL142
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(16776192), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL143
	 nop
.LL142:
	sethi	%hi(7168), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL143:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-2147483648), %g1
	or	%g1, 127, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(16776192), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 7, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset1_count24, .-bitfield__write_var_BitField_offset1_count24
	.align 4
	.global bitfield__read_var_BitField_offset1_count25
	.hidden	bitfield__read_var_BitField_offset1_count25
	.type	bitfield__read_var_BitField_offset1_count25, #function
	.proc	04
bitfield__read_var_BitField_offset1_count25:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	add	%g1, %g1, %g1
	srl	%g1, 7, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset1_count25, .-bitfield__read_var_BitField_offset1_count25
	.align 4
	.global bitfield__write_var_BitField_offset1_count25
	.hidden	bitfield__write_var_BitField_offset1_count25
	.type	bitfield__write_var_BitField_offset1_count25, #function
	.proc	020
bitfield__write_var_BitField_offset1_count25:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL148
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(33553408), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL149
	 nop
.LL148:
	sethi	%hi(7168), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL149:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-2147483648), %g1
	or	%g1, 63, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(33553408), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 6, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset1_count25, .-bitfield__write_var_BitField_offset1_count25
	.align 4
	.global bitfield__read_var_BitField_offset1_count26
	.hidden	bitfield__read_var_BitField_offset1_count26
	.type	bitfield__read_var_BitField_offset1_count26, #function
	.proc	04
bitfield__read_var_BitField_offset1_count26:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	add	%g1, %g1, %g1
	srl	%g1, 6, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset1_count26, .-bitfield__read_var_BitField_offset1_count26
	.align 4
	.global bitfield__write_var_BitField_offset1_count26
	.hidden	bitfield__write_var_BitField_offset1_count26
	.type	bitfield__write_var_BitField_offset1_count26, #function
	.proc	020
bitfield__write_var_BitField_offset1_count26:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL154
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(67107840), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL155
	 nop
.LL154:
	sethi	%hi(8192), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL155:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-2147483648), %g1
	or	%g1, 31, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(67107840), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 5, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset1_count26, .-bitfield__write_var_BitField_offset1_count26
	.align 4
	.global bitfield__read_var_BitField_offset1_count27
	.hidden	bitfield__read_var_BitField_offset1_count27
	.type	bitfield__read_var_BitField_offset1_count27, #function
	.proc	04
bitfield__read_var_BitField_offset1_count27:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	add	%g1, %g1, %g1
	srl	%g1, 5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset1_count27, .-bitfield__read_var_BitField_offset1_count27
	.align 4
	.global bitfield__write_var_BitField_offset1_count27
	.hidden	bitfield__write_var_BitField_offset1_count27
	.type	bitfield__write_var_BitField_offset1_count27, #function
	.proc	020
bitfield__write_var_BitField_offset1_count27:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL160
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(134216704), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL161
	 nop
.LL160:
	sethi	%hi(8192), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL161:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-2147483648), %g1
	or	%g1, 15, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(134216704), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 4, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset1_count27, .-bitfield__write_var_BitField_offset1_count27
	.align 4
	.global bitfield__read_var_BitField_offset1_count28
	.hidden	bitfield__read_var_BitField_offset1_count28
	.type	bitfield__read_var_BitField_offset1_count28, #function
	.proc	04
bitfield__read_var_BitField_offset1_count28:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	add	%g1, %g1, %g1
	srl	%g1, 4, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset1_count28, .-bitfield__read_var_BitField_offset1_count28
	.align 4
	.global bitfield__write_var_BitField_offset1_count28
	.hidden	bitfield__write_var_BitField_offset1_count28
	.type	bitfield__write_var_BitField_offset1_count28, #function
	.proc	020
bitfield__write_var_BitField_offset1_count28:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL166
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(268434432), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL167
	 nop
.LL166:
	sethi	%hi(8192), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL167:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-2147483648), %g1
	or	%g1, 7, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(268434432), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 3, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset1_count28, .-bitfield__write_var_BitField_offset1_count28
	.align 4
	.global bitfield__read_var_BitField_offset1_count29
	.hidden	bitfield__read_var_BitField_offset1_count29
	.type	bitfield__read_var_BitField_offset1_count29, #function
	.proc	04
bitfield__read_var_BitField_offset1_count29:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	add	%g1, %g1, %g1
	srl	%g1, 3, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset1_count29, .-bitfield__read_var_BitField_offset1_count29
	.align 4
	.global bitfield__write_var_BitField_offset1_count29
	.hidden	bitfield__write_var_BitField_offset1_count29
	.type	bitfield__write_var_BitField_offset1_count29, #function
	.proc	020
bitfield__write_var_BitField_offset1_count29:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL172
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(536869888), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL173
	 nop
.LL172:
	sethi	%hi(8192), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL173:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-2147483648), %g1
	or	%g1, 3, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(536869888), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 2, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset1_count29, .-bitfield__write_var_BitField_offset1_count29
	.align 4
	.global bitfield__read_var_BitField_offset1_count30
	.hidden	bitfield__read_var_BitField_offset1_count30
	.type	bitfield__read_var_BitField_offset1_count30, #function
	.proc	04
bitfield__read_var_BitField_offset1_count30:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	add	%g1, %g1, %g1
	srl	%g1, 2, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset1_count30, .-bitfield__read_var_BitField_offset1_count30
	.align 4
	.global bitfield__write_var_BitField_offset1_count30
	.hidden	bitfield__write_var_BitField_offset1_count30
	.type	bitfield__write_var_BitField_offset1_count30, #function
	.proc	020
bitfield__write_var_BitField_offset1_count30:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL178
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(1073740800), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL179
	 nop
.LL178:
	sethi	%hi(9216), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL179:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-2147483648), %g1
	or	%g1, 1, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(1073740800), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	add	%g1, %g1, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset1_count30, .-bitfield__write_var_BitField_offset1_count30
	.align 4
	.global bitfield__read_var_BitField_offset1_count31
	.hidden	bitfield__read_var_BitField_offset1_count31
	.type	bitfield__read_var_BitField_offset1_count31, #function
	.proc	04
bitfield__read_var_BitField_offset1_count31:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(2147482624), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset1_count31, .-bitfield__read_var_BitField_offset1_count31
	.align 4
	.global bitfield__write_var_BitField_offset1_count31
	.hidden	bitfield__write_var_BitField_offset1_count31
	.type	bitfield__write_var_BitField_offset1_count31, #function
	.proc	020
bitfield__write_var_BitField_offset1_count31:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bge	%icc, .LL184
	 nop
	sethi	%hi(9216), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL184:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-2147483648), %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(2147482624), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset1_count31, .-bitfield__write_var_BitField_offset1_count31
	.align 4
	.global bitfield__read_var_BitField_offset2_count1
	.hidden	bitfield__read_var_BitField_offset2_count1
	.type	bitfield__read_var_BitField_offset2_count1, #function
	.proc	04
bitfield__read_var_BitField_offset2_count1:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 26, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset2_count1, .-bitfield__read_var_BitField_offset2_count1
	.align 4
	.global bitfield__write_var_BitField_offset2_count1
	.hidden	bitfield__write_var_BitField_offset2_count1
	.type	bitfield__write_var_BitField_offset2_count1, #function
	.proc	020
bitfield__write_var_BitField_offset2_count1:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL189
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1
	ble	%icc, .LL190
	 nop
.LL189:
	sethi	%hi(9216), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL190:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -33, %g2
	ld	[%fp-24], %g1
	and	%g1, 1, %g1
	sll	%g1, 5, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset2_count1, .-bitfield__write_var_BitField_offset2_count1
	.align 4
	.global bitfield__read_var_BitField_offset2_count2
	.hidden	bitfield__read_var_BitField_offset2_count2
	.type	bitfield__read_var_BitField_offset2_count2, #function
	.proc	04
bitfield__read_var_BitField_offset2_count2:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 26, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset2_count2, .-bitfield__read_var_BitField_offset2_count2
	.align 4
	.global bitfield__write_var_BitField_offset2_count2
	.hidden	bitfield__write_var_BitField_offset2_count2
	.type	bitfield__write_var_BitField_offset2_count2, #function
	.proc	020
bitfield__write_var_BitField_offset2_count2:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL195
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 3
	ble	%icc, .LL196
	 nop
.LL195:
	sethi	%hi(9216), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL196:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -49, %g2
	ld	[%fp-24], %g1
	and	%g1, 3, %g1
	sll	%g1, 4, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset2_count2, .-bitfield__write_var_BitField_offset2_count2
	.align 4
	.global bitfield__read_var_BitField_offset2_count3
	.hidden	bitfield__read_var_BitField_offset2_count3
	.type	bitfield__read_var_BitField_offset2_count3, #function
	.proc	04
bitfield__read_var_BitField_offset2_count3:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 26, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset2_count3, .-bitfield__read_var_BitField_offset2_count3
	.align 4
	.global bitfield__write_var_BitField_offset2_count3
	.hidden	bitfield__write_var_BitField_offset2_count3
	.type	bitfield__write_var_BitField_offset2_count3, #function
	.proc	020
bitfield__write_var_BitField_offset2_count3:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL201
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 7
	ble	%icc, .LL202
	 nop
.LL201:
	sethi	%hi(10240), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL202:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -57, %g2
	ld	[%fp-24], %g1
	and	%g1, 7, %g1
	sll	%g1, 3, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset2_count3, .-bitfield__write_var_BitField_offset2_count3
	.align 4
	.global bitfield__read_var_BitField_offset2_count4
	.hidden	bitfield__read_var_BitField_offset2_count4
	.type	bitfield__read_var_BitField_offset2_count4, #function
	.proc	04
bitfield__read_var_BitField_offset2_count4:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 26, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset2_count4, .-bitfield__read_var_BitField_offset2_count4
	.align 4
	.global bitfield__write_var_BitField_offset2_count4
	.hidden	bitfield__write_var_BitField_offset2_count4
	.type	bitfield__write_var_BitField_offset2_count4, #function
	.proc	020
bitfield__write_var_BitField_offset2_count4:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL207
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 15
	ble	%icc, .LL208
	 nop
.LL207:
	sethi	%hi(10240), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL208:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -61, %g2
	ld	[%fp-24], %g1
	and	%g1, 15, %g1
	sll	%g1, 2, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset2_count4, .-bitfield__write_var_BitField_offset2_count4
	.align 4
	.global bitfield__read_var_BitField_offset2_count5
	.hidden	bitfield__read_var_BitField_offset2_count5
	.type	bitfield__read_var_BitField_offset2_count5, #function
	.proc	04
bitfield__read_var_BitField_offset2_count5:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 26, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset2_count5, .-bitfield__read_var_BitField_offset2_count5
	.align 4
	.global bitfield__write_var_BitField_offset2_count5
	.hidden	bitfield__write_var_BitField_offset2_count5
	.type	bitfield__write_var_BitField_offset2_count5, #function
	.proc	020
bitfield__write_var_BitField_offset2_count5:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL213
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 31
	ble	%icc, .LL214
	 nop
.LL213:
	sethi	%hi(10240), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL214:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -63, %g2
	ld	[%fp-24], %g1
	and	%g1, 31, %g1
	add	%g1, %g1, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset2_count5, .-bitfield__write_var_BitField_offset2_count5
	.align 4
	.global bitfield__read_var_BitField_offset2_count6
	.hidden	bitfield__read_var_BitField_offset2_count6
	.type	bitfield__read_var_BitField_offset2_count6, #function
	.proc	04
bitfield__read_var_BitField_offset2_count6:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, 63, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset2_count6, .-bitfield__read_var_BitField_offset2_count6
	.align 4
	.global bitfield__write_var_BitField_offset2_count6
	.hidden	bitfield__write_var_BitField_offset2_count6
	.type	bitfield__write_var_BitField_offset2_count6, #function
	.proc	020
bitfield__write_var_BitField_offset2_count6:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL219
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 63
	ble	%icc, .LL220
	 nop
.LL219:
	sethi	%hi(10240), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL220:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -64, %g2
	ld	[%fp-24], %g1
	and	%g1, 63, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset2_count6, .-bitfield__write_var_BitField_offset2_count6
	.align 4
	.global bitfield__read_var_BitField_offset2_count7
	.hidden	bitfield__read_var_BitField_offset2_count7
	.type	bitfield__read_var_BitField_offset2_count7, #function
	.proc	04
bitfield__read_var_BitField_offset2_count7:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 18, %g1
	srl	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset2_count7, .-bitfield__read_var_BitField_offset2_count7
	.align 4
	.global bitfield__write_var_BitField_offset2_count7
	.hidden	bitfield__write_var_BitField_offset2_count7
	.type	bitfield__write_var_BitField_offset2_count7, #function
	.proc	020
bitfield__write_var_BitField_offset2_count7:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL225
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 127
	ble	%icc, .LL226
	 nop
.LL225:
	sethi	%hi(11264), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL226:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-16384), %g1
	or	%g1, 127, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 127, %g1
	sll	%g1, 7, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset2_count7, .-bitfield__write_var_BitField_offset2_count7
	.align 4
	.global bitfield__read_var_BitField_offset2_count8
	.hidden	bitfield__read_var_BitField_offset2_count8
	.type	bitfield__read_var_BitField_offset2_count8, #function
	.proc	04
bitfield__read_var_BitField_offset2_count8:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 18, %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset2_count8, .-bitfield__read_var_BitField_offset2_count8
	.align 4
	.global bitfield__write_var_BitField_offset2_count8
	.hidden	bitfield__write_var_BitField_offset2_count8
	.type	bitfield__write_var_BitField_offset2_count8, #function
	.proc	020
bitfield__write_var_BitField_offset2_count8:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL231
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 255
	ble	%icc, .LL232
	 nop
.LL231:
	sethi	%hi(11264), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL232:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-16384), %g1
	or	%g1, 63, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 255, %g1
	sll	%g1, 6, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset2_count8, .-bitfield__write_var_BitField_offset2_count8
	.align 4
	.global bitfield__read_var_BitField_offset2_count9
	.hidden	bitfield__read_var_BitField_offset2_count9
	.type	bitfield__read_var_BitField_offset2_count9, #function
	.proc	04
bitfield__read_var_BitField_offset2_count9:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 18, %g1
	srl	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset2_count9, .-bitfield__read_var_BitField_offset2_count9
	.align 4
	.global bitfield__write_var_BitField_offset2_count9
	.hidden	bitfield__write_var_BitField_offset2_count9
	.type	bitfield__write_var_BitField_offset2_count9, #function
	.proc	020
bitfield__write_var_BitField_offset2_count9:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL237
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 511
	ble	%icc, .LL238
	 nop
.LL237:
	sethi	%hi(11264), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL238:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-16384), %g1
	or	%g1, 31, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 511, %g1
	sll	%g1, 5, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset2_count9, .-bitfield__write_var_BitField_offset2_count9
	.align 4
	.global bitfield__read_var_BitField_offset2_count10
	.hidden	bitfield__read_var_BitField_offset2_count10
	.type	bitfield__read_var_BitField_offset2_count10, #function
	.proc	04
bitfield__read_var_BitField_offset2_count10:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 18, %g1
	srl	%g1, 22, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset2_count10, .-bitfield__read_var_BitField_offset2_count10
	.align 4
	.global bitfield__write_var_BitField_offset2_count10
	.hidden	bitfield__write_var_BitField_offset2_count10
	.type	bitfield__write_var_BitField_offset2_count10, #function
	.proc	020
bitfield__write_var_BitField_offset2_count10:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL243
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1023
	ble	%icc, .LL244
	 nop
.LL243:
	sethi	%hi(11264), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL244:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-16384), %g1
	or	%g1, 15, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 1023, %g1
	sll	%g1, 4, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset2_count10, .-bitfield__write_var_BitField_offset2_count10
	.align 4
	.global bitfield__read_var_BitField_offset2_count11
	.hidden	bitfield__read_var_BitField_offset2_count11
	.type	bitfield__read_var_BitField_offset2_count11, #function
	.proc	04
bitfield__read_var_BitField_offset2_count11:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 18, %g1
	srl	%g1, 21, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset2_count11, .-bitfield__read_var_BitField_offset2_count11
	.align 4
	.global bitfield__write_var_BitField_offset2_count11
	.hidden	bitfield__write_var_BitField_offset2_count11
	.type	bitfield__write_var_BitField_offset2_count11, #function
	.proc	020
bitfield__write_var_BitField_offset2_count11:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL249
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 2047
	ble	%icc, .LL250
	 nop
.LL249:
	sethi	%hi(12288), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL250:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-16384), %g1
	or	%g1, 7, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 2047, %g1
	sll	%g1, 3, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset2_count11, .-bitfield__write_var_BitField_offset2_count11
	.align 4
	.global bitfield__read_var_BitField_offset2_count12
	.hidden	bitfield__read_var_BitField_offset2_count12
	.type	bitfield__read_var_BitField_offset2_count12, #function
	.proc	04
bitfield__read_var_BitField_offset2_count12:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 18, %g1
	srl	%g1, 20, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset2_count12, .-bitfield__read_var_BitField_offset2_count12
	.align 4
	.global bitfield__write_var_BitField_offset2_count12
	.hidden	bitfield__write_var_BitField_offset2_count12
	.type	bitfield__write_var_BitField_offset2_count12, #function
	.proc	020
bitfield__write_var_BitField_offset2_count12:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL255
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 4095
	ble	%icc, .LL256
	 nop
.LL255:
	sethi	%hi(12288), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL256:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-16384), %g1
	or	%g1, 3, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 4095, %g1
	sll	%g1, 2, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset2_count12, .-bitfield__write_var_BitField_offset2_count12
	.align 4
	.global bitfield__read_var_BitField_offset2_count13
	.hidden	bitfield__read_var_BitField_offset2_count13
	.type	bitfield__read_var_BitField_offset2_count13, #function
	.proc	04
bitfield__read_var_BitField_offset2_count13:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 18, %g1
	srl	%g1, 19, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset2_count13, .-bitfield__read_var_BitField_offset2_count13
	.align 4
	.global bitfield__write_var_BitField_offset2_count13
	.hidden	bitfield__write_var_BitField_offset2_count13
	.type	bitfield__write_var_BitField_offset2_count13, #function
	.proc	020
bitfield__write_var_BitField_offset2_count13:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL261
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(7168), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL262
	 nop
.LL261:
	sethi	%hi(12288), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL262:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-16384), %g1
	or	%g1, 1, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	add	%g1, %g1, %g1
	or	%g3, %g1, %g1
	sth	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset2_count13, .-bitfield__write_var_BitField_offset2_count13
	.align 4
	.global bitfield__read_var_BitField_offset2_count14
	.hidden	bitfield__read_var_BitField_offset2_count14
	.type	bitfield__read_var_BitField_offset2_count14, #function
	.proc	04
bitfield__read_var_BitField_offset2_count14:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset2_count14, .-bitfield__read_var_BitField_offset2_count14
	.align 4
	.global bitfield__write_var_BitField_offset2_count14
	.hidden	bitfield__write_var_BitField_offset2_count14
	.type	bitfield__write_var_BitField_offset2_count14, #function
	.proc	020
bitfield__write_var_BitField_offset2_count14:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL267
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(15360), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL268
	 nop
.LL267:
	sethi	%hi(12288), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL268:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-16384), %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	or	%g3, %g1, %g1
	sth	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset2_count14, .-bitfield__write_var_BitField_offset2_count14
	.align 4
	.global bitfield__read_var_BitField_offset2_count15
	.hidden	bitfield__read_var_BitField_offset2_count15
	.type	bitfield__read_var_BitField_offset2_count15, #function
	.proc	04
bitfield__read_var_BitField_offset2_count15:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 2, %g1
	srl	%g1, 17, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset2_count15, .-bitfield__read_var_BitField_offset2_count15
	.align 4
	.global bitfield__write_var_BitField_offset2_count15
	.hidden	bitfield__write_var_BitField_offset2_count15
	.type	bitfield__write_var_BitField_offset2_count15, #function
	.proc	020
bitfield__write_var_BitField_offset2_count15:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL273
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(31744), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL274
	 nop
.LL273:
	sethi	%hi(13312), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL274:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-1073710080), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 15, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset2_count15, .-bitfield__write_var_BitField_offset2_count15
	.align 4
	.global bitfield__read_var_BitField_offset2_count16
	.hidden	bitfield__read_var_BitField_offset2_count16
	.type	bitfield__read_var_BitField_offset2_count16, #function
	.proc	04
bitfield__read_var_BitField_offset2_count16:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 2, %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset2_count16, .-bitfield__read_var_BitField_offset2_count16
	.align 4
	.global bitfield__write_var_BitField_offset2_count16
	.hidden	bitfield__write_var_BitField_offset2_count16
	.type	bitfield__write_var_BitField_offset2_count16, #function
	.proc	020
bitfield__write_var_BitField_offset2_count16:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL279
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(64512), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL280
	 nop
.LL279:
	sethi	%hi(13312), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL280:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-1073726464), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 14, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset2_count16, .-bitfield__write_var_BitField_offset2_count16
	.align 4
	.global bitfield__read_var_BitField_offset2_count17
	.hidden	bitfield__read_var_BitField_offset2_count17
	.type	bitfield__read_var_BitField_offset2_count17, #function
	.proc	04
bitfield__read_var_BitField_offset2_count17:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 2, %g1
	srl	%g1, 15, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset2_count17, .-bitfield__read_var_BitField_offset2_count17
	.align 4
	.global bitfield__write_var_BitField_offset2_count17
	.hidden	bitfield__write_var_BitField_offset2_count17
	.type	bitfield__write_var_BitField_offset2_count17, #function
	.proc	020
bitfield__write_var_BitField_offset2_count17:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL285
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(130048), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL286
	 nop
.LL285:
	sethi	%hi(13312), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL286:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-1073734656), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(130048), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 13, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset2_count17, .-bitfield__write_var_BitField_offset2_count17
	.align 4
	.global bitfield__read_var_BitField_offset2_count18
	.hidden	bitfield__read_var_BitField_offset2_count18
	.type	bitfield__read_var_BitField_offset2_count18, #function
	.proc	04
bitfield__read_var_BitField_offset2_count18:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 2, %g1
	srl	%g1, 14, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset2_count18, .-bitfield__read_var_BitField_offset2_count18
	.align 4
	.global bitfield__write_var_BitField_offset2_count18
	.hidden	bitfield__write_var_BitField_offset2_count18
	.type	bitfield__write_var_BitField_offset2_count18, #function
	.proc	020
bitfield__write_var_BitField_offset2_count18:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL291
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(261120), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL292
	 nop
.LL291:
	sethi	%hi(13312), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL292:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-1073738752), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(261120), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 12, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset2_count18, .-bitfield__write_var_BitField_offset2_count18
	.align 4
	.global bitfield__read_var_BitField_offset2_count19
	.hidden	bitfield__read_var_BitField_offset2_count19
	.type	bitfield__read_var_BitField_offset2_count19, #function
	.proc	04
bitfield__read_var_BitField_offset2_count19:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 2, %g1
	srl	%g1, 13, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset2_count19, .-bitfield__read_var_BitField_offset2_count19
	.align 4
	.global bitfield__write_var_BitField_offset2_count19
	.hidden	bitfield__write_var_BitField_offset2_count19
	.type	bitfield__write_var_BitField_offset2_count19, #function
	.proc	020
bitfield__write_var_BitField_offset2_count19:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL297
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(523264), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL298
	 nop
.LL297:
	sethi	%hi(14336), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL298:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-1073740800), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(523264), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 11, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset2_count19, .-bitfield__write_var_BitField_offset2_count19
	.align 4
	.global bitfield__read_var_BitField_offset2_count20
	.hidden	bitfield__read_var_BitField_offset2_count20
	.type	bitfield__read_var_BitField_offset2_count20, #function
	.proc	04
bitfield__read_var_BitField_offset2_count20:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 2, %g1
	srl	%g1, 12, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset2_count20, .-bitfield__read_var_BitField_offset2_count20
	.align 4
	.global bitfield__write_var_BitField_offset2_count20
	.hidden	bitfield__write_var_BitField_offset2_count20
	.type	bitfield__write_var_BitField_offset2_count20, #function
	.proc	020
bitfield__write_var_BitField_offset2_count20:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL303
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(1047552), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL304
	 nop
.LL303:
	sethi	%hi(14336), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL304:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-1073741824), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(1047552), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 10, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset2_count20, .-bitfield__write_var_BitField_offset2_count20
	.align 4
	.global bitfield__read_var_BitField_offset2_count21
	.hidden	bitfield__read_var_BitField_offset2_count21
	.type	bitfield__read_var_BitField_offset2_count21, #function
	.proc	04
bitfield__read_var_BitField_offset2_count21:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 2, %g1
	srl	%g1, 11, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset2_count21, .-bitfield__read_var_BitField_offset2_count21
	.align 4
	.global bitfield__write_var_BitField_offset2_count21
	.hidden	bitfield__write_var_BitField_offset2_count21
	.type	bitfield__write_var_BitField_offset2_count21, #function
	.proc	020
bitfield__write_var_BitField_offset2_count21:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL309
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(2096128), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL310
	 nop
.LL309:
	sethi	%hi(14336), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL310:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-1073741824), %g1
	or	%g1, 511, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(2096128), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 9, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset2_count21, .-bitfield__write_var_BitField_offset2_count21
	.align 4
	.global bitfield__read_var_BitField_offset2_count22
	.hidden	bitfield__read_var_BitField_offset2_count22
	.type	bitfield__read_var_BitField_offset2_count22, #function
	.proc	04
bitfield__read_var_BitField_offset2_count22:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 2, %g1
	srl	%g1, 10, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset2_count22, .-bitfield__read_var_BitField_offset2_count22
	.align 4
	.global bitfield__write_var_BitField_offset2_count22
	.hidden	bitfield__write_var_BitField_offset2_count22
	.type	bitfield__write_var_BitField_offset2_count22, #function
	.proc	020
bitfield__write_var_BitField_offset2_count22:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL315
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(4193280), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL316
	 nop
.LL315:
	sethi	%hi(14336), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL316:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-1073741824), %g1
	or	%g1, 255, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(4193280), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 8, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset2_count22, .-bitfield__write_var_BitField_offset2_count22
	.align 4
	.global bitfield__read_var_BitField_offset2_count23
	.hidden	bitfield__read_var_BitField_offset2_count23
	.type	bitfield__read_var_BitField_offset2_count23, #function
	.proc	04
bitfield__read_var_BitField_offset2_count23:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 2, %g1
	srl	%g1, 9, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset2_count23, .-bitfield__read_var_BitField_offset2_count23
	.align 4
	.global bitfield__write_var_BitField_offset2_count23
	.hidden	bitfield__write_var_BitField_offset2_count23
	.type	bitfield__write_var_BitField_offset2_count23, #function
	.proc	020
bitfield__write_var_BitField_offset2_count23:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL321
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(8387584), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL322
	 nop
.LL321:
	sethi	%hi(15360), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL322:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-1073741824), %g1
	or	%g1, 127, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(8387584), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 7, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset2_count23, .-bitfield__write_var_BitField_offset2_count23
	.align 4
	.global bitfield__read_var_BitField_offset2_count24
	.hidden	bitfield__read_var_BitField_offset2_count24
	.type	bitfield__read_var_BitField_offset2_count24, #function
	.proc	04
bitfield__read_var_BitField_offset2_count24:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 2, %g1
	srl	%g1, 8, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset2_count24, .-bitfield__read_var_BitField_offset2_count24
	.align 4
	.global bitfield__write_var_BitField_offset2_count24
	.hidden	bitfield__write_var_BitField_offset2_count24
	.type	bitfield__write_var_BitField_offset2_count24, #function
	.proc	020
bitfield__write_var_BitField_offset2_count24:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL327
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(16776192), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL328
	 nop
.LL327:
	sethi	%hi(15360), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL328:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-1073741824), %g1
	or	%g1, 63, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(16776192), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 6, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset2_count24, .-bitfield__write_var_BitField_offset2_count24
	.align 4
	.global bitfield__read_var_BitField_offset2_count25
	.hidden	bitfield__read_var_BitField_offset2_count25
	.type	bitfield__read_var_BitField_offset2_count25, #function
	.proc	04
bitfield__read_var_BitField_offset2_count25:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 2, %g1
	srl	%g1, 7, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset2_count25, .-bitfield__read_var_BitField_offset2_count25
	.align 4
	.global bitfield__write_var_BitField_offset2_count25
	.hidden	bitfield__write_var_BitField_offset2_count25
	.type	bitfield__write_var_BitField_offset2_count25, #function
	.proc	020
bitfield__write_var_BitField_offset2_count25:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL333
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(33553408), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL334
	 nop
.LL333:
	sethi	%hi(15360), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL334:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-1073741824), %g1
	or	%g1, 31, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(33553408), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 5, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset2_count25, .-bitfield__write_var_BitField_offset2_count25
	.align 4
	.global bitfield__read_var_BitField_offset2_count26
	.hidden	bitfield__read_var_BitField_offset2_count26
	.type	bitfield__read_var_BitField_offset2_count26, #function
	.proc	04
bitfield__read_var_BitField_offset2_count26:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 2, %g1
	srl	%g1, 6, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset2_count26, .-bitfield__read_var_BitField_offset2_count26
	.align 4
	.global bitfield__write_var_BitField_offset2_count26
	.hidden	bitfield__write_var_BitField_offset2_count26
	.type	bitfield__write_var_BitField_offset2_count26, #function
	.proc	020
bitfield__write_var_BitField_offset2_count26:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL339
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(67107840), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL340
	 nop
.LL339:
	sethi	%hi(15360), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL340:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-1073741824), %g1
	or	%g1, 15, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(67107840), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 4, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset2_count26, .-bitfield__write_var_BitField_offset2_count26
	.align 4
	.global bitfield__read_var_BitField_offset2_count27
	.hidden	bitfield__read_var_BitField_offset2_count27
	.type	bitfield__read_var_BitField_offset2_count27, #function
	.proc	04
bitfield__read_var_BitField_offset2_count27:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 2, %g1
	srl	%g1, 5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset2_count27, .-bitfield__read_var_BitField_offset2_count27
	.align 4
	.global bitfield__write_var_BitField_offset2_count27
	.hidden	bitfield__write_var_BitField_offset2_count27
	.type	bitfield__write_var_BitField_offset2_count27, #function
	.proc	020
bitfield__write_var_BitField_offset2_count27:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL345
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(134216704), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL346
	 nop
.LL345:
	sethi	%hi(16384), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL346:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-1073741824), %g1
	or	%g1, 7, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(134216704), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 3, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset2_count27, .-bitfield__write_var_BitField_offset2_count27
	.align 4
	.global bitfield__read_var_BitField_offset2_count28
	.hidden	bitfield__read_var_BitField_offset2_count28
	.type	bitfield__read_var_BitField_offset2_count28, #function
	.proc	04
bitfield__read_var_BitField_offset2_count28:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 2, %g1
	srl	%g1, 4, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset2_count28, .-bitfield__read_var_BitField_offset2_count28
	.align 4
	.global bitfield__write_var_BitField_offset2_count28
	.hidden	bitfield__write_var_BitField_offset2_count28
	.type	bitfield__write_var_BitField_offset2_count28, #function
	.proc	020
bitfield__write_var_BitField_offset2_count28:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL351
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(268434432), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL352
	 nop
.LL351:
	sethi	%hi(16384), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL352:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-1073741824), %g1
	or	%g1, 3, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(268434432), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 2, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset2_count28, .-bitfield__write_var_BitField_offset2_count28
	.align 4
	.global bitfield__read_var_BitField_offset2_count29
	.hidden	bitfield__read_var_BitField_offset2_count29
	.type	bitfield__read_var_BitField_offset2_count29, #function
	.proc	04
bitfield__read_var_BitField_offset2_count29:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 2, %g1
	srl	%g1, 3, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset2_count29, .-bitfield__read_var_BitField_offset2_count29
	.align 4
	.global bitfield__write_var_BitField_offset2_count29
	.hidden	bitfield__write_var_BitField_offset2_count29
	.type	bitfield__write_var_BitField_offset2_count29, #function
	.proc	020
bitfield__write_var_BitField_offset2_count29:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL357
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(536869888), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL358
	 nop
.LL357:
	sethi	%hi(16384), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL358:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-1073741824), %g1
	or	%g1, 1, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(536869888), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	add	%g1, %g1, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset2_count29, .-bitfield__write_var_BitField_offset2_count29
	.align 4
	.global bitfield__read_var_BitField_offset2_count30
	.hidden	bitfield__read_var_BitField_offset2_count30
	.type	bitfield__read_var_BitField_offset2_count30, #function
	.proc	04
bitfield__read_var_BitField_offset2_count30:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(1073740800), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset2_count30, .-bitfield__read_var_BitField_offset2_count30
	.align 4
	.global bitfield__write_var_BitField_offset2_count30
	.hidden	bitfield__write_var_BitField_offset2_count30
	.type	bitfield__write_var_BitField_offset2_count30, #function
	.proc	020
bitfield__write_var_BitField_offset2_count30:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL363
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(1073740800), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL364
	 nop
.LL363:
	sethi	%hi(16384), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL364:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-1073741824), %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(1073740800), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset2_count30, .-bitfield__write_var_BitField_offset2_count30
	.align 4
	.global bitfield__read_var_BitField_offset3_count1
	.hidden	bitfield__read_var_BitField_offset3_count1
	.type	bitfield__read_var_BitField_offset3_count1, #function
	.proc	04
bitfield__read_var_BitField_offset3_count1:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 27, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset3_count1, .-bitfield__read_var_BitField_offset3_count1
	.align 4
	.global bitfield__write_var_BitField_offset3_count1
	.hidden	bitfield__write_var_BitField_offset3_count1
	.type	bitfield__write_var_BitField_offset3_count1, #function
	.proc	020
bitfield__write_var_BitField_offset3_count1:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL369
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1
	ble	%icc, .LL370
	 nop
.LL369:
	sethi	%hi(17408), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL370:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -17, %g2
	ld	[%fp-24], %g1
	and	%g1, 1, %g1
	sll	%g1, 4, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset3_count1, .-bitfield__write_var_BitField_offset3_count1
	.align 4
	.global bitfield__read_var_BitField_offset3_count2
	.hidden	bitfield__read_var_BitField_offset3_count2
	.type	bitfield__read_var_BitField_offset3_count2, #function
	.proc	04
bitfield__read_var_BitField_offset3_count2:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 27, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset3_count2, .-bitfield__read_var_BitField_offset3_count2
	.align 4
	.global bitfield__write_var_BitField_offset3_count2
	.hidden	bitfield__write_var_BitField_offset3_count2
	.type	bitfield__write_var_BitField_offset3_count2, #function
	.proc	020
bitfield__write_var_BitField_offset3_count2:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL375
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 3
	ble	%icc, .LL376
	 nop
.LL375:
	sethi	%hi(17408), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL376:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -25, %g2
	ld	[%fp-24], %g1
	and	%g1, 3, %g1
	sll	%g1, 3, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset3_count2, .-bitfield__write_var_BitField_offset3_count2
	.align 4
	.global bitfield__read_var_BitField_offset3_count3
	.hidden	bitfield__read_var_BitField_offset3_count3
	.type	bitfield__read_var_BitField_offset3_count3, #function
	.proc	04
bitfield__read_var_BitField_offset3_count3:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 27, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset3_count3, .-bitfield__read_var_BitField_offset3_count3
	.align 4
	.global bitfield__write_var_BitField_offset3_count3
	.hidden	bitfield__write_var_BitField_offset3_count3
	.type	bitfield__write_var_BitField_offset3_count3, #function
	.proc	020
bitfield__write_var_BitField_offset3_count3:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL381
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 7
	ble	%icc, .LL382
	 nop
.LL381:
	sethi	%hi(17408), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL382:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -29, %g2
	ld	[%fp-24], %g1
	and	%g1, 7, %g1
	sll	%g1, 2, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset3_count3, .-bitfield__write_var_BitField_offset3_count3
	.align 4
	.global bitfield__read_var_BitField_offset3_count4
	.hidden	bitfield__read_var_BitField_offset3_count4
	.type	bitfield__read_var_BitField_offset3_count4, #function
	.proc	04
bitfield__read_var_BitField_offset3_count4:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 27, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset3_count4, .-bitfield__read_var_BitField_offset3_count4
	.align 4
	.global bitfield__write_var_BitField_offset3_count4
	.hidden	bitfield__write_var_BitField_offset3_count4
	.type	bitfield__write_var_BitField_offset3_count4, #function
	.proc	020
bitfield__write_var_BitField_offset3_count4:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL387
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 15
	ble	%icc, .LL388
	 nop
.LL387:
	sethi	%hi(17408), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL388:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -31, %g2
	ld	[%fp-24], %g1
	and	%g1, 15, %g1
	add	%g1, %g1, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset3_count4, .-bitfield__write_var_BitField_offset3_count4
	.align 4
	.global bitfield__read_var_BitField_offset3_count5
	.hidden	bitfield__read_var_BitField_offset3_count5
	.type	bitfield__read_var_BitField_offset3_count5, #function
	.proc	04
bitfield__read_var_BitField_offset3_count5:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset3_count5, .-bitfield__read_var_BitField_offset3_count5
	.align 4
	.global bitfield__write_var_BitField_offset3_count5
	.hidden	bitfield__write_var_BitField_offset3_count5
	.type	bitfield__write_var_BitField_offset3_count5, #function
	.proc	020
bitfield__write_var_BitField_offset3_count5:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL393
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 31
	ble	%icc, .LL394
	 nop
.LL393:
	sethi	%hi(18432), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL394:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -32, %g2
	ld	[%fp-24], %g1
	and	%g1, 31, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset3_count5, .-bitfield__write_var_BitField_offset3_count5
	.align 4
	.global bitfield__read_var_BitField_offset3_count6
	.hidden	bitfield__read_var_BitField_offset3_count6
	.type	bitfield__read_var_BitField_offset3_count6, #function
	.proc	04
bitfield__read_var_BitField_offset3_count6:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 19, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset3_count6, .-bitfield__read_var_BitField_offset3_count6
	.align 4
	.global bitfield__write_var_BitField_offset3_count6
	.hidden	bitfield__write_var_BitField_offset3_count6
	.type	bitfield__write_var_BitField_offset3_count6, #function
	.proc	020
bitfield__write_var_BitField_offset3_count6:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL399
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 63
	ble	%icc, .LL400
	 nop
.LL399:
	sethi	%hi(18432), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL400:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-8192), %g1
	or	%g1, 127, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 63, %g1
	sll	%g1, 7, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset3_count6, .-bitfield__write_var_BitField_offset3_count6
	.align 4
	.global bitfield__read_var_BitField_offset3_count7
	.hidden	bitfield__read_var_BitField_offset3_count7
	.type	bitfield__read_var_BitField_offset3_count7, #function
	.proc	04
bitfield__read_var_BitField_offset3_count7:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 19, %g1
	srl	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset3_count7, .-bitfield__read_var_BitField_offset3_count7
	.align 4
	.global bitfield__write_var_BitField_offset3_count7
	.hidden	bitfield__write_var_BitField_offset3_count7
	.type	bitfield__write_var_BitField_offset3_count7, #function
	.proc	020
bitfield__write_var_BitField_offset3_count7:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL405
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 127
	ble	%icc, .LL406
	 nop
.LL405:
	sethi	%hi(18432), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL406:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-8192), %g1
	or	%g1, 63, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 127, %g1
	sll	%g1, 6, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset3_count7, .-bitfield__write_var_BitField_offset3_count7
	.align 4
	.global bitfield__read_var_BitField_offset3_count8
	.hidden	bitfield__read_var_BitField_offset3_count8
	.type	bitfield__read_var_BitField_offset3_count8, #function
	.proc	04
bitfield__read_var_BitField_offset3_count8:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 19, %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset3_count8, .-bitfield__read_var_BitField_offset3_count8
	.align 4
	.global bitfield__write_var_BitField_offset3_count8
	.hidden	bitfield__write_var_BitField_offset3_count8
	.type	bitfield__write_var_BitField_offset3_count8, #function
	.proc	020
bitfield__write_var_BitField_offset3_count8:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL411
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 255
	ble	%icc, .LL412
	 nop
.LL411:
	sethi	%hi(18432), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL412:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-8192), %g1
	or	%g1, 31, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 255, %g1
	sll	%g1, 5, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset3_count8, .-bitfield__write_var_BitField_offset3_count8
	.align 4
	.global bitfield__read_var_BitField_offset3_count9
	.hidden	bitfield__read_var_BitField_offset3_count9
	.type	bitfield__read_var_BitField_offset3_count9, #function
	.proc	04
bitfield__read_var_BitField_offset3_count9:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 19, %g1
	srl	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset3_count9, .-bitfield__read_var_BitField_offset3_count9
	.align 4
	.global bitfield__write_var_BitField_offset3_count9
	.hidden	bitfield__write_var_BitField_offset3_count9
	.type	bitfield__write_var_BitField_offset3_count9, #function
	.proc	020
bitfield__write_var_BitField_offset3_count9:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL417
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 511
	ble	%icc, .LL418
	 nop
.LL417:
	sethi	%hi(19456), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL418:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-8192), %g1
	or	%g1, 15, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 511, %g1
	sll	%g1, 4, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset3_count9, .-bitfield__write_var_BitField_offset3_count9
	.align 4
	.global bitfield__read_var_BitField_offset3_count10
	.hidden	bitfield__read_var_BitField_offset3_count10
	.type	bitfield__read_var_BitField_offset3_count10, #function
	.proc	04
bitfield__read_var_BitField_offset3_count10:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 19, %g1
	srl	%g1, 22, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset3_count10, .-bitfield__read_var_BitField_offset3_count10
	.align 4
	.global bitfield__write_var_BitField_offset3_count10
	.hidden	bitfield__write_var_BitField_offset3_count10
	.type	bitfield__write_var_BitField_offset3_count10, #function
	.proc	020
bitfield__write_var_BitField_offset3_count10:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL423
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1023
	ble	%icc, .LL424
	 nop
.LL423:
	sethi	%hi(19456), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL424:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-8192), %g1
	or	%g1, 7, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 1023, %g1
	sll	%g1, 3, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset3_count10, .-bitfield__write_var_BitField_offset3_count10
	.align 4
	.global bitfield__read_var_BitField_offset3_count11
	.hidden	bitfield__read_var_BitField_offset3_count11
	.type	bitfield__read_var_BitField_offset3_count11, #function
	.proc	04
bitfield__read_var_BitField_offset3_count11:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 19, %g1
	srl	%g1, 21, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset3_count11, .-bitfield__read_var_BitField_offset3_count11
	.align 4
	.global bitfield__write_var_BitField_offset3_count11
	.hidden	bitfield__write_var_BitField_offset3_count11
	.type	bitfield__write_var_BitField_offset3_count11, #function
	.proc	020
bitfield__write_var_BitField_offset3_count11:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL429
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 2047
	ble	%icc, .LL430
	 nop
.LL429:
	sethi	%hi(19456), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL430:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-8192), %g1
	or	%g1, 3, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 2047, %g1
	sll	%g1, 2, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset3_count11, .-bitfield__write_var_BitField_offset3_count11
	.align 4
	.global bitfield__read_var_BitField_offset3_count12
	.hidden	bitfield__read_var_BitField_offset3_count12
	.type	bitfield__read_var_BitField_offset3_count12, #function
	.proc	04
bitfield__read_var_BitField_offset3_count12:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 19, %g1
	srl	%g1, 20, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset3_count12, .-bitfield__read_var_BitField_offset3_count12
	.align 4
	.global bitfield__write_var_BitField_offset3_count12
	.hidden	bitfield__write_var_BitField_offset3_count12
	.type	bitfield__write_var_BitField_offset3_count12, #function
	.proc	020
bitfield__write_var_BitField_offset3_count12:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL435
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 4095
	ble	%icc, .LL436
	 nop
.LL435:
	sethi	%hi(19456), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL436:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-8192), %g1
	or	%g1, 1, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 4095, %g1
	add	%g1, %g1, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset3_count12, .-bitfield__write_var_BitField_offset3_count12
	.align 4
	.global bitfield__read_var_BitField_offset3_count13
	.hidden	bitfield__read_var_BitField_offset3_count13
	.type	bitfield__read_var_BitField_offset3_count13, #function
	.proc	04
bitfield__read_var_BitField_offset3_count13:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset3_count13, .-bitfield__read_var_BitField_offset3_count13
	.align 4
	.global bitfield__write_var_BitField_offset3_count13
	.hidden	bitfield__write_var_BitField_offset3_count13
	.type	bitfield__write_var_BitField_offset3_count13, #function
	.proc	020
bitfield__write_var_BitField_offset3_count13:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL441
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(7168), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL442
	 nop
.LL441:
	sethi	%hi(20480), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL442:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-8192), %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	or	%g3, %g1, %g1
	sth	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset3_count13, .-bitfield__write_var_BitField_offset3_count13
	.align 4
	.global bitfield__read_var_BitField_offset3_count14
	.hidden	bitfield__read_var_BitField_offset3_count14
	.type	bitfield__read_var_BitField_offset3_count14, #function
	.proc	04
bitfield__read_var_BitField_offset3_count14:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 3, %g1
	srl	%g1, 18, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset3_count14, .-bitfield__read_var_BitField_offset3_count14
	.align 4
	.global bitfield__write_var_BitField_offset3_count14
	.hidden	bitfield__write_var_BitField_offset3_count14
	.type	bitfield__write_var_BitField_offset3_count14, #function
	.proc	020
bitfield__write_var_BitField_offset3_count14:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL447
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(15360), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL448
	 nop
.LL447:
	sethi	%hi(20480), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL448:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-536839168), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 15, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset3_count14, .-bitfield__write_var_BitField_offset3_count14
	.align 4
	.global bitfield__read_var_BitField_offset3_count15
	.hidden	bitfield__read_var_BitField_offset3_count15
	.type	bitfield__read_var_BitField_offset3_count15, #function
	.proc	04
bitfield__read_var_BitField_offset3_count15:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 3, %g1
	srl	%g1, 17, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset3_count15, .-bitfield__read_var_BitField_offset3_count15
	.align 4
	.global bitfield__write_var_BitField_offset3_count15
	.hidden	bitfield__write_var_BitField_offset3_count15
	.type	bitfield__write_var_BitField_offset3_count15, #function
	.proc	020
bitfield__write_var_BitField_offset3_count15:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL453
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(31744), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL454
	 nop
.LL453:
	sethi	%hi(20480), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL454:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-536855552), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 14, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset3_count15, .-bitfield__write_var_BitField_offset3_count15
	.align 4
	.global bitfield__read_var_BitField_offset3_count16
	.hidden	bitfield__read_var_BitField_offset3_count16
	.type	bitfield__read_var_BitField_offset3_count16, #function
	.proc	04
bitfield__read_var_BitField_offset3_count16:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 3, %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset3_count16, .-bitfield__read_var_BitField_offset3_count16
	.align 4
	.global bitfield__write_var_BitField_offset3_count16
	.hidden	bitfield__write_var_BitField_offset3_count16
	.type	bitfield__write_var_BitField_offset3_count16, #function
	.proc	020
bitfield__write_var_BitField_offset3_count16:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL459
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(64512), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL460
	 nop
.LL459:
	sethi	%hi(20480), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL460:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-536863744), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 13, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset3_count16, .-bitfield__write_var_BitField_offset3_count16
	.align 4
	.global bitfield__read_var_BitField_offset3_count17
	.hidden	bitfield__read_var_BitField_offset3_count17
	.type	bitfield__read_var_BitField_offset3_count17, #function
	.proc	04
bitfield__read_var_BitField_offset3_count17:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 3, %g1
	srl	%g1, 15, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset3_count17, .-bitfield__read_var_BitField_offset3_count17
	.align 4
	.global bitfield__write_var_BitField_offset3_count17
	.hidden	bitfield__write_var_BitField_offset3_count17
	.type	bitfield__write_var_BitField_offset3_count17, #function
	.proc	020
bitfield__write_var_BitField_offset3_count17:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL465
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(130048), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL466
	 nop
.LL465:
	sethi	%hi(21504), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL466:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-536867840), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(130048), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 12, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset3_count17, .-bitfield__write_var_BitField_offset3_count17
	.align 4
	.global bitfield__read_var_BitField_offset3_count18
	.hidden	bitfield__read_var_BitField_offset3_count18
	.type	bitfield__read_var_BitField_offset3_count18, #function
	.proc	04
bitfield__read_var_BitField_offset3_count18:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 3, %g1
	srl	%g1, 14, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset3_count18, .-bitfield__read_var_BitField_offset3_count18
	.align 4
	.global bitfield__write_var_BitField_offset3_count18
	.hidden	bitfield__write_var_BitField_offset3_count18
	.type	bitfield__write_var_BitField_offset3_count18, #function
	.proc	020
bitfield__write_var_BitField_offset3_count18:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL471
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(261120), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL472
	 nop
.LL471:
	sethi	%hi(21504), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL472:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-536869888), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(261120), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 11, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset3_count18, .-bitfield__write_var_BitField_offset3_count18
	.align 4
	.global bitfield__read_var_BitField_offset3_count19
	.hidden	bitfield__read_var_BitField_offset3_count19
	.type	bitfield__read_var_BitField_offset3_count19, #function
	.proc	04
bitfield__read_var_BitField_offset3_count19:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 3, %g1
	srl	%g1, 13, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset3_count19, .-bitfield__read_var_BitField_offset3_count19
	.align 4
	.global bitfield__write_var_BitField_offset3_count19
	.hidden	bitfield__write_var_BitField_offset3_count19
	.type	bitfield__write_var_BitField_offset3_count19, #function
	.proc	020
bitfield__write_var_BitField_offset3_count19:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL477
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(523264), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL478
	 nop
.LL477:
	sethi	%hi(21504), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL478:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-536870912), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(523264), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 10, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset3_count19, .-bitfield__write_var_BitField_offset3_count19
	.align 4
	.global bitfield__read_var_BitField_offset3_count20
	.hidden	bitfield__read_var_BitField_offset3_count20
	.type	bitfield__read_var_BitField_offset3_count20, #function
	.proc	04
bitfield__read_var_BitField_offset3_count20:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 3, %g1
	srl	%g1, 12, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset3_count20, .-bitfield__read_var_BitField_offset3_count20
	.align 4
	.global bitfield__write_var_BitField_offset3_count20
	.hidden	bitfield__write_var_BitField_offset3_count20
	.type	bitfield__write_var_BitField_offset3_count20, #function
	.proc	020
bitfield__write_var_BitField_offset3_count20:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL483
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(1047552), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL484
	 nop
.LL483:
	sethi	%hi(21504), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL484:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-536870912), %g1
	or	%g1, 511, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(1047552), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 9, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset3_count20, .-bitfield__write_var_BitField_offset3_count20
	.align 4
	.global bitfield__read_var_BitField_offset3_count21
	.hidden	bitfield__read_var_BitField_offset3_count21
	.type	bitfield__read_var_BitField_offset3_count21, #function
	.proc	04
bitfield__read_var_BitField_offset3_count21:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 3, %g1
	srl	%g1, 11, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset3_count21, .-bitfield__read_var_BitField_offset3_count21
	.align 4
	.global bitfield__write_var_BitField_offset3_count21
	.hidden	bitfield__write_var_BitField_offset3_count21
	.type	bitfield__write_var_BitField_offset3_count21, #function
	.proc	020
bitfield__write_var_BitField_offset3_count21:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL489
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(2096128), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL490
	 nop
.LL489:
	sethi	%hi(22528), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL490:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-536870912), %g1
	or	%g1, 255, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(2096128), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 8, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset3_count21, .-bitfield__write_var_BitField_offset3_count21
	.align 4
	.global bitfield__read_var_BitField_offset3_count22
	.hidden	bitfield__read_var_BitField_offset3_count22
	.type	bitfield__read_var_BitField_offset3_count22, #function
	.proc	04
bitfield__read_var_BitField_offset3_count22:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 3, %g1
	srl	%g1, 10, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset3_count22, .-bitfield__read_var_BitField_offset3_count22
	.align 4
	.global bitfield__write_var_BitField_offset3_count22
	.hidden	bitfield__write_var_BitField_offset3_count22
	.type	bitfield__write_var_BitField_offset3_count22, #function
	.proc	020
bitfield__write_var_BitField_offset3_count22:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL495
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(4193280), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL496
	 nop
.LL495:
	sethi	%hi(22528), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL496:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-536870912), %g1
	or	%g1, 127, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(4193280), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 7, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset3_count22, .-bitfield__write_var_BitField_offset3_count22
	.align 4
	.global bitfield__read_var_BitField_offset3_count23
	.hidden	bitfield__read_var_BitField_offset3_count23
	.type	bitfield__read_var_BitField_offset3_count23, #function
	.proc	04
bitfield__read_var_BitField_offset3_count23:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 3, %g1
	srl	%g1, 9, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset3_count23, .-bitfield__read_var_BitField_offset3_count23
	.align 4
	.global bitfield__write_var_BitField_offset3_count23
	.hidden	bitfield__write_var_BitField_offset3_count23
	.type	bitfield__write_var_BitField_offset3_count23, #function
	.proc	020
bitfield__write_var_BitField_offset3_count23:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL501
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(8387584), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL502
	 nop
.LL501:
	sethi	%hi(22528), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL502:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-536870912), %g1
	or	%g1, 63, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(8387584), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 6, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset3_count23, .-bitfield__write_var_BitField_offset3_count23
	.align 4
	.global bitfield__read_var_BitField_offset3_count24
	.hidden	bitfield__read_var_BitField_offset3_count24
	.type	bitfield__read_var_BitField_offset3_count24, #function
	.proc	04
bitfield__read_var_BitField_offset3_count24:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 3, %g1
	srl	%g1, 8, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset3_count24, .-bitfield__read_var_BitField_offset3_count24
	.align 4
	.global bitfield__write_var_BitField_offset3_count24
	.hidden	bitfield__write_var_BitField_offset3_count24
	.type	bitfield__write_var_BitField_offset3_count24, #function
	.proc	020
bitfield__write_var_BitField_offset3_count24:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL507
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(16776192), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL508
	 nop
.LL507:
	sethi	%hi(22528), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL508:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-536870912), %g1
	or	%g1, 31, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(16776192), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 5, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset3_count24, .-bitfield__write_var_BitField_offset3_count24
	.align 4
	.global bitfield__read_var_BitField_offset3_count25
	.hidden	bitfield__read_var_BitField_offset3_count25
	.type	bitfield__read_var_BitField_offset3_count25, #function
	.proc	04
bitfield__read_var_BitField_offset3_count25:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 3, %g1
	srl	%g1, 7, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset3_count25, .-bitfield__read_var_BitField_offset3_count25
	.align 4
	.global bitfield__write_var_BitField_offset3_count25
	.hidden	bitfield__write_var_BitField_offset3_count25
	.type	bitfield__write_var_BitField_offset3_count25, #function
	.proc	020
bitfield__write_var_BitField_offset3_count25:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL513
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(33553408), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL514
	 nop
.LL513:
	sethi	%hi(23552), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL514:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-536870912), %g1
	or	%g1, 15, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(33553408), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 4, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset3_count25, .-bitfield__write_var_BitField_offset3_count25
	.align 4
	.global bitfield__read_var_BitField_offset3_count26
	.hidden	bitfield__read_var_BitField_offset3_count26
	.type	bitfield__read_var_BitField_offset3_count26, #function
	.proc	04
bitfield__read_var_BitField_offset3_count26:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 3, %g1
	srl	%g1, 6, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset3_count26, .-bitfield__read_var_BitField_offset3_count26
	.align 4
	.global bitfield__write_var_BitField_offset3_count26
	.hidden	bitfield__write_var_BitField_offset3_count26
	.type	bitfield__write_var_BitField_offset3_count26, #function
	.proc	020
bitfield__write_var_BitField_offset3_count26:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL519
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(67107840), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL520
	 nop
.LL519:
	sethi	%hi(23552), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL520:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-536870912), %g1
	or	%g1, 7, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(67107840), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 3, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset3_count26, .-bitfield__write_var_BitField_offset3_count26
	.align 4
	.global bitfield__read_var_BitField_offset3_count27
	.hidden	bitfield__read_var_BitField_offset3_count27
	.type	bitfield__read_var_BitField_offset3_count27, #function
	.proc	04
bitfield__read_var_BitField_offset3_count27:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 3, %g1
	srl	%g1, 5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset3_count27, .-bitfield__read_var_BitField_offset3_count27
	.align 4
	.global bitfield__write_var_BitField_offset3_count27
	.hidden	bitfield__write_var_BitField_offset3_count27
	.type	bitfield__write_var_BitField_offset3_count27, #function
	.proc	020
bitfield__write_var_BitField_offset3_count27:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL525
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(134216704), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL526
	 nop
.LL525:
	sethi	%hi(23552), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL526:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-536870912), %g1
	or	%g1, 3, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(134216704), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 2, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset3_count27, .-bitfield__write_var_BitField_offset3_count27
	.align 4
	.global bitfield__read_var_BitField_offset3_count28
	.hidden	bitfield__read_var_BitField_offset3_count28
	.type	bitfield__read_var_BitField_offset3_count28, #function
	.proc	04
bitfield__read_var_BitField_offset3_count28:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 3, %g1
	srl	%g1, 4, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset3_count28, .-bitfield__read_var_BitField_offset3_count28
	.align 4
	.global bitfield__write_var_BitField_offset3_count28
	.hidden	bitfield__write_var_BitField_offset3_count28
	.type	bitfield__write_var_BitField_offset3_count28, #function
	.proc	020
bitfield__write_var_BitField_offset3_count28:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL531
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(268434432), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL532
	 nop
.LL531:
	sethi	%hi(23552), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL532:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-536870912), %g1
	or	%g1, 1, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(268434432), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	add	%g1, %g1, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset3_count28, .-bitfield__write_var_BitField_offset3_count28
	.align 4
	.global bitfield__read_var_BitField_offset3_count29
	.hidden	bitfield__read_var_BitField_offset3_count29
	.type	bitfield__read_var_BitField_offset3_count29, #function
	.proc	04
bitfield__read_var_BitField_offset3_count29:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(536869888), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset3_count29, .-bitfield__read_var_BitField_offset3_count29
	.align 4
	.global bitfield__write_var_BitField_offset3_count29
	.hidden	bitfield__write_var_BitField_offset3_count29
	.type	bitfield__write_var_BitField_offset3_count29, #function
	.proc	020
bitfield__write_var_BitField_offset3_count29:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL537
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(536869888), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL538
	 nop
.LL537:
	sethi	%hi(24576), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL538:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-536870912), %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(536869888), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset3_count29, .-bitfield__write_var_BitField_offset3_count29
	.align 4
	.global bitfield__read_var_BitField_offset4_count1
	.hidden	bitfield__read_var_BitField_offset4_count1
	.type	bitfield__read_var_BitField_offset4_count1, #function
	.proc	04
bitfield__read_var_BitField_offset4_count1:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 28, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset4_count1, .-bitfield__read_var_BitField_offset4_count1
	.align 4
	.global bitfield__write_var_BitField_offset4_count1
	.hidden	bitfield__write_var_BitField_offset4_count1
	.type	bitfield__write_var_BitField_offset4_count1, #function
	.proc	020
bitfield__write_var_BitField_offset4_count1:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL543
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1
	ble	%icc, .LL544
	 nop
.LL543:
	sethi	%hi(24576), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL544:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -9, %g2
	ld	[%fp-24], %g1
	and	%g1, 1, %g1
	sll	%g1, 3, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset4_count1, .-bitfield__write_var_BitField_offset4_count1
	.align 4
	.global bitfield__read_var_BitField_offset4_count2
	.hidden	bitfield__read_var_BitField_offset4_count2
	.type	bitfield__read_var_BitField_offset4_count2, #function
	.proc	04
bitfield__read_var_BitField_offset4_count2:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 28, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset4_count2, .-bitfield__read_var_BitField_offset4_count2
	.align 4
	.global bitfield__write_var_BitField_offset4_count2
	.hidden	bitfield__write_var_BitField_offset4_count2
	.type	bitfield__write_var_BitField_offset4_count2, #function
	.proc	020
bitfield__write_var_BitField_offset4_count2:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL549
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 3
	ble	%icc, .LL550
	 nop
.LL549:
	sethi	%hi(24576), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL550:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -13, %g2
	ld	[%fp-24], %g1
	and	%g1, 3, %g1
	sll	%g1, 2, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset4_count2, .-bitfield__write_var_BitField_offset4_count2
	.align 4
	.global bitfield__read_var_BitField_offset4_count3
	.hidden	bitfield__read_var_BitField_offset4_count3
	.type	bitfield__read_var_BitField_offset4_count3, #function
	.proc	04
bitfield__read_var_BitField_offset4_count3:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 28, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset4_count3, .-bitfield__read_var_BitField_offset4_count3
	.align 4
	.global bitfield__write_var_BitField_offset4_count3
	.hidden	bitfield__write_var_BitField_offset4_count3
	.type	bitfield__write_var_BitField_offset4_count3, #function
	.proc	020
bitfield__write_var_BitField_offset4_count3:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL555
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 7
	ble	%icc, .LL556
	 nop
.LL555:
	sethi	%hi(24576), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL556:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -15, %g2
	ld	[%fp-24], %g1
	and	%g1, 7, %g1
	add	%g1, %g1, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset4_count3, .-bitfield__write_var_BitField_offset4_count3
	.align 4
	.global bitfield__read_var_BitField_offset4_count4
	.hidden	bitfield__read_var_BitField_offset4_count4
	.type	bitfield__read_var_BitField_offset4_count4, #function
	.proc	04
bitfield__read_var_BitField_offset4_count4:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, 15, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset4_count4, .-bitfield__read_var_BitField_offset4_count4
	.align 4
	.global bitfield__write_var_BitField_offset4_count4
	.hidden	bitfield__write_var_BitField_offset4_count4
	.type	bitfield__write_var_BitField_offset4_count4, #function
	.proc	020
bitfield__write_var_BitField_offset4_count4:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL561
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 15
	ble	%icc, .LL562
	 nop
.LL561:
	sethi	%hi(25600), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL562:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -16, %g2
	ld	[%fp-24], %g1
	and	%g1, 15, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset4_count4, .-bitfield__write_var_BitField_offset4_count4
	.align 4
	.global bitfield__read_var_BitField_offset4_count5
	.hidden	bitfield__read_var_BitField_offset4_count5
	.type	bitfield__read_var_BitField_offset4_count5, #function
	.proc	04
bitfield__read_var_BitField_offset4_count5:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 20, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset4_count5, .-bitfield__read_var_BitField_offset4_count5
	.align 4
	.global bitfield__write_var_BitField_offset4_count5
	.hidden	bitfield__write_var_BitField_offset4_count5
	.type	bitfield__write_var_BitField_offset4_count5, #function
	.proc	020
bitfield__write_var_BitField_offset4_count5:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL567
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 31
	ble	%icc, .LL568
	 nop
.LL567:
	sethi	%hi(25600), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL568:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -3969, %g2
	ld	[%fp-24], %g1
	and	%g1, 31, %g1
	sll	%g1, 7, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset4_count5, .-bitfield__write_var_BitField_offset4_count5
	.align 4
	.global bitfield__read_var_BitField_offset4_count6
	.hidden	bitfield__read_var_BitField_offset4_count6
	.type	bitfield__read_var_BitField_offset4_count6, #function
	.proc	04
bitfield__read_var_BitField_offset4_count6:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 20, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset4_count6, .-bitfield__read_var_BitField_offset4_count6
	.align 4
	.global bitfield__write_var_BitField_offset4_count6
	.hidden	bitfield__write_var_BitField_offset4_count6
	.type	bitfield__write_var_BitField_offset4_count6, #function
	.proc	020
bitfield__write_var_BitField_offset4_count6:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL573
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 63
	ble	%icc, .LL574
	 nop
.LL573:
	sethi	%hi(25600), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL574:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -4033, %g2
	ld	[%fp-24], %g1
	and	%g1, 63, %g1
	sll	%g1, 6, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset4_count6, .-bitfield__write_var_BitField_offset4_count6
	.align 4
	.global bitfield__read_var_BitField_offset4_count7
	.hidden	bitfield__read_var_BitField_offset4_count7
	.type	bitfield__read_var_BitField_offset4_count7, #function
	.proc	04
bitfield__read_var_BitField_offset4_count7:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 20, %g1
	srl	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset4_count7, .-bitfield__read_var_BitField_offset4_count7
	.align 4
	.global bitfield__write_var_BitField_offset4_count7
	.hidden	bitfield__write_var_BitField_offset4_count7
	.type	bitfield__write_var_BitField_offset4_count7, #function
	.proc	020
bitfield__write_var_BitField_offset4_count7:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL579
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 127
	ble	%icc, .LL580
	 nop
.LL579:
	sethi	%hi(25600), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL580:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -4065, %g2
	ld	[%fp-24], %g1
	and	%g1, 127, %g1
	sll	%g1, 5, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset4_count7, .-bitfield__write_var_BitField_offset4_count7
	.align 4
	.global bitfield__read_var_BitField_offset4_count8
	.hidden	bitfield__read_var_BitField_offset4_count8
	.type	bitfield__read_var_BitField_offset4_count8, #function
	.proc	04
bitfield__read_var_BitField_offset4_count8:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 20, %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset4_count8, .-bitfield__read_var_BitField_offset4_count8
	.align 4
	.global bitfield__write_var_BitField_offset4_count8
	.hidden	bitfield__write_var_BitField_offset4_count8
	.type	bitfield__write_var_BitField_offset4_count8, #function
	.proc	020
bitfield__write_var_BitField_offset4_count8:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL585
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 255
	ble	%icc, .LL586
	 nop
.LL585:
	sethi	%hi(26624), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL586:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -4081, %g2
	ld	[%fp-24], %g1
	and	%g1, 255, %g1
	sll	%g1, 4, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset4_count8, .-bitfield__write_var_BitField_offset4_count8
	.align 4
	.global bitfield__read_var_BitField_offset4_count9
	.hidden	bitfield__read_var_BitField_offset4_count9
	.type	bitfield__read_var_BitField_offset4_count9, #function
	.proc	04
bitfield__read_var_BitField_offset4_count9:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 20, %g1
	srl	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset4_count9, .-bitfield__read_var_BitField_offset4_count9
	.align 4
	.global bitfield__write_var_BitField_offset4_count9
	.hidden	bitfield__write_var_BitField_offset4_count9
	.type	bitfield__write_var_BitField_offset4_count9, #function
	.proc	020
bitfield__write_var_BitField_offset4_count9:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL591
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 511
	ble	%icc, .LL592
	 nop
.LL591:
	sethi	%hi(26624), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL592:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -4089, %g2
	ld	[%fp-24], %g1
	and	%g1, 511, %g1
	sll	%g1, 3, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset4_count9, .-bitfield__write_var_BitField_offset4_count9
	.align 4
	.global bitfield__read_var_BitField_offset4_count10
	.hidden	bitfield__read_var_BitField_offset4_count10
	.type	bitfield__read_var_BitField_offset4_count10, #function
	.proc	04
bitfield__read_var_BitField_offset4_count10:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 20, %g1
	srl	%g1, 22, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset4_count10, .-bitfield__read_var_BitField_offset4_count10
	.align 4
	.global bitfield__write_var_BitField_offset4_count10
	.hidden	bitfield__write_var_BitField_offset4_count10
	.type	bitfield__write_var_BitField_offset4_count10, #function
	.proc	020
bitfield__write_var_BitField_offset4_count10:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL597
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1023
	ble	%icc, .LL598
	 nop
.LL597:
	sethi	%hi(26624), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL598:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -4093, %g2
	ld	[%fp-24], %g1
	and	%g1, 1023, %g1
	sll	%g1, 2, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset4_count10, .-bitfield__write_var_BitField_offset4_count10
	.align 4
	.global bitfield__read_var_BitField_offset4_count11
	.hidden	bitfield__read_var_BitField_offset4_count11
	.type	bitfield__read_var_BitField_offset4_count11, #function
	.proc	04
bitfield__read_var_BitField_offset4_count11:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 20, %g1
	srl	%g1, 21, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset4_count11, .-bitfield__read_var_BitField_offset4_count11
	.align 4
	.global bitfield__write_var_BitField_offset4_count11
	.hidden	bitfield__write_var_BitField_offset4_count11
	.type	bitfield__write_var_BitField_offset4_count11, #function
	.proc	020
bitfield__write_var_BitField_offset4_count11:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL603
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 2047
	ble	%icc, .LL604
	 nop
.LL603:
	sethi	%hi(26624), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL604:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -4095, %g2
	ld	[%fp-24], %g1
	and	%g1, 2047, %g1
	add	%g1, %g1, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset4_count11, .-bitfield__write_var_BitField_offset4_count11
	.align 4
	.global bitfield__read_var_BitField_offset4_count12
	.hidden	bitfield__read_var_BitField_offset4_count12
	.type	bitfield__read_var_BitField_offset4_count12, #function
	.proc	04
bitfield__read_var_BitField_offset4_count12:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, 4095, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset4_count12, .-bitfield__read_var_BitField_offset4_count12
	.align 4
	.global bitfield__write_var_BitField_offset4_count12
	.hidden	bitfield__write_var_BitField_offset4_count12
	.type	bitfield__write_var_BitField_offset4_count12, #function
	.proc	020
bitfield__write_var_BitField_offset4_count12:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL609
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 4095
	ble	%icc, .LL610
	 nop
.LL609:
	sethi	%hi(27648), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL610:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -4096, %g2
	ld	[%fp-24], %g1
	and	%g1, 4095, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset4_count12, .-bitfield__write_var_BitField_offset4_count12
	.align 4
	.global bitfield__read_var_BitField_offset4_count13
	.hidden	bitfield__read_var_BitField_offset4_count13
	.type	bitfield__read_var_BitField_offset4_count13, #function
	.proc	04
bitfield__read_var_BitField_offset4_count13:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 4, %g1
	srl	%g1, 19, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset4_count13, .-bitfield__read_var_BitField_offset4_count13
	.align 4
	.global bitfield__write_var_BitField_offset4_count13
	.hidden	bitfield__write_var_BitField_offset4_count13
	.type	bitfield__write_var_BitField_offset4_count13, #function
	.proc	020
bitfield__write_var_BitField_offset4_count13:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL615
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(7168), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL616
	 nop
.LL615:
	sethi	%hi(27648), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL616:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-268403712), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 15, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset4_count13, .-bitfield__write_var_BitField_offset4_count13
	.align 4
	.global bitfield__read_var_BitField_offset4_count14
	.hidden	bitfield__read_var_BitField_offset4_count14
	.type	bitfield__read_var_BitField_offset4_count14, #function
	.proc	04
bitfield__read_var_BitField_offset4_count14:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 4, %g1
	srl	%g1, 18, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset4_count14, .-bitfield__read_var_BitField_offset4_count14
	.align 4
	.global bitfield__write_var_BitField_offset4_count14
	.hidden	bitfield__write_var_BitField_offset4_count14
	.type	bitfield__write_var_BitField_offset4_count14, #function
	.proc	020
bitfield__write_var_BitField_offset4_count14:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL621
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(15360), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL622
	 nop
.LL621:
	sethi	%hi(27648), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL622:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-268420096), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 14, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset4_count14, .-bitfield__write_var_BitField_offset4_count14
	.align 4
	.global bitfield__read_var_BitField_offset4_count15
	.hidden	bitfield__read_var_BitField_offset4_count15
	.type	bitfield__read_var_BitField_offset4_count15, #function
	.proc	04
bitfield__read_var_BitField_offset4_count15:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 4, %g1
	srl	%g1, 17, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset4_count15, .-bitfield__read_var_BitField_offset4_count15
	.align 4
	.global bitfield__write_var_BitField_offset4_count15
	.hidden	bitfield__write_var_BitField_offset4_count15
	.type	bitfield__write_var_BitField_offset4_count15, #function
	.proc	020
bitfield__write_var_BitField_offset4_count15:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL627
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(31744), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL628
	 nop
.LL627:
	sethi	%hi(27648), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL628:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-268428288), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 13, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset4_count15, .-bitfield__write_var_BitField_offset4_count15
	.align 4
	.global bitfield__read_var_BitField_offset4_count16
	.hidden	bitfield__read_var_BitField_offset4_count16
	.type	bitfield__read_var_BitField_offset4_count16, #function
	.proc	04
bitfield__read_var_BitField_offset4_count16:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 4, %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset4_count16, .-bitfield__read_var_BitField_offset4_count16
	.align 4
	.global bitfield__write_var_BitField_offset4_count16
	.hidden	bitfield__write_var_BitField_offset4_count16
	.type	bitfield__write_var_BitField_offset4_count16, #function
	.proc	020
bitfield__write_var_BitField_offset4_count16:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL633
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(64512), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL634
	 nop
.LL633:
	sethi	%hi(28672), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL634:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-268432384), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 12, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset4_count16, .-bitfield__write_var_BitField_offset4_count16
	.align 4
	.global bitfield__read_var_BitField_offset4_count17
	.hidden	bitfield__read_var_BitField_offset4_count17
	.type	bitfield__read_var_BitField_offset4_count17, #function
	.proc	04
bitfield__read_var_BitField_offset4_count17:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 4, %g1
	srl	%g1, 15, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset4_count17, .-bitfield__read_var_BitField_offset4_count17
	.align 4
	.global bitfield__write_var_BitField_offset4_count17
	.hidden	bitfield__write_var_BitField_offset4_count17
	.type	bitfield__write_var_BitField_offset4_count17, #function
	.proc	020
bitfield__write_var_BitField_offset4_count17:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL639
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(130048), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL640
	 nop
.LL639:
	sethi	%hi(28672), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL640:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-268434432), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(130048), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 11, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset4_count17, .-bitfield__write_var_BitField_offset4_count17
	.align 4
	.global bitfield__read_var_BitField_offset4_count18
	.hidden	bitfield__read_var_BitField_offset4_count18
	.type	bitfield__read_var_BitField_offset4_count18, #function
	.proc	04
bitfield__read_var_BitField_offset4_count18:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 4, %g1
	srl	%g1, 14, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset4_count18, .-bitfield__read_var_BitField_offset4_count18
	.align 4
	.global bitfield__write_var_BitField_offset4_count18
	.hidden	bitfield__write_var_BitField_offset4_count18
	.type	bitfield__write_var_BitField_offset4_count18, #function
	.proc	020
bitfield__write_var_BitField_offset4_count18:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL645
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(261120), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL646
	 nop
.LL645:
	sethi	%hi(28672), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL646:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-268435456), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(261120), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 10, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset4_count18, .-bitfield__write_var_BitField_offset4_count18
	.align 4
	.global bitfield__read_var_BitField_offset4_count19
	.hidden	bitfield__read_var_BitField_offset4_count19
	.type	bitfield__read_var_BitField_offset4_count19, #function
	.proc	04
bitfield__read_var_BitField_offset4_count19:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 4, %g1
	srl	%g1, 13, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset4_count19, .-bitfield__read_var_BitField_offset4_count19
	.align 4
	.global bitfield__write_var_BitField_offset4_count19
	.hidden	bitfield__write_var_BitField_offset4_count19
	.type	bitfield__write_var_BitField_offset4_count19, #function
	.proc	020
bitfield__write_var_BitField_offset4_count19:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL651
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(523264), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL652
	 nop
.LL651:
	sethi	%hi(28672), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL652:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-268435456), %g1
	or	%g1, 511, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(523264), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 9, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset4_count19, .-bitfield__write_var_BitField_offset4_count19
	.align 4
	.global bitfield__read_var_BitField_offset4_count20
	.hidden	bitfield__read_var_BitField_offset4_count20
	.type	bitfield__read_var_BitField_offset4_count20, #function
	.proc	04
bitfield__read_var_BitField_offset4_count20:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 4, %g1
	srl	%g1, 12, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset4_count20, .-bitfield__read_var_BitField_offset4_count20
	.align 4
	.global bitfield__write_var_BitField_offset4_count20
	.hidden	bitfield__write_var_BitField_offset4_count20
	.type	bitfield__write_var_BitField_offset4_count20, #function
	.proc	020
bitfield__write_var_BitField_offset4_count20:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL657
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(1047552), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL658
	 nop
.LL657:
	sethi	%hi(29696), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL658:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-268435456), %g1
	or	%g1, 255, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(1047552), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 8, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset4_count20, .-bitfield__write_var_BitField_offset4_count20
	.align 4
	.global bitfield__read_var_BitField_offset4_count21
	.hidden	bitfield__read_var_BitField_offset4_count21
	.type	bitfield__read_var_BitField_offset4_count21, #function
	.proc	04
bitfield__read_var_BitField_offset4_count21:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 4, %g1
	srl	%g1, 11, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset4_count21, .-bitfield__read_var_BitField_offset4_count21
	.align 4
	.global bitfield__write_var_BitField_offset4_count21
	.hidden	bitfield__write_var_BitField_offset4_count21
	.type	bitfield__write_var_BitField_offset4_count21, #function
	.proc	020
bitfield__write_var_BitField_offset4_count21:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL663
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(2096128), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL664
	 nop
.LL663:
	sethi	%hi(29696), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL664:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-268435456), %g1
	or	%g1, 127, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(2096128), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 7, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset4_count21, .-bitfield__write_var_BitField_offset4_count21
	.align 4
	.global bitfield__read_var_BitField_offset4_count22
	.hidden	bitfield__read_var_BitField_offset4_count22
	.type	bitfield__read_var_BitField_offset4_count22, #function
	.proc	04
bitfield__read_var_BitField_offset4_count22:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 4, %g1
	srl	%g1, 10, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset4_count22, .-bitfield__read_var_BitField_offset4_count22
	.align 4
	.global bitfield__write_var_BitField_offset4_count22
	.hidden	bitfield__write_var_BitField_offset4_count22
	.type	bitfield__write_var_BitField_offset4_count22, #function
	.proc	020
bitfield__write_var_BitField_offset4_count22:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL669
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(4193280), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL670
	 nop
.LL669:
	sethi	%hi(29696), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL670:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-268435456), %g1
	or	%g1, 63, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(4193280), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 6, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset4_count22, .-bitfield__write_var_BitField_offset4_count22
	.align 4
	.global bitfield__read_var_BitField_offset4_count23
	.hidden	bitfield__read_var_BitField_offset4_count23
	.type	bitfield__read_var_BitField_offset4_count23, #function
	.proc	04
bitfield__read_var_BitField_offset4_count23:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 4, %g1
	srl	%g1, 9, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset4_count23, .-bitfield__read_var_BitField_offset4_count23
	.align 4
	.global bitfield__write_var_BitField_offset4_count23
	.hidden	bitfield__write_var_BitField_offset4_count23
	.type	bitfield__write_var_BitField_offset4_count23, #function
	.proc	020
bitfield__write_var_BitField_offset4_count23:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL675
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(8387584), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL676
	 nop
.LL675:
	sethi	%hi(29696), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL676:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-268435456), %g1
	or	%g1, 31, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(8387584), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 5, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset4_count23, .-bitfield__write_var_BitField_offset4_count23
	.align 4
	.global bitfield__read_var_BitField_offset4_count24
	.hidden	bitfield__read_var_BitField_offset4_count24
	.type	bitfield__read_var_BitField_offset4_count24, #function
	.proc	04
bitfield__read_var_BitField_offset4_count24:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 4, %g1
	srl	%g1, 8, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset4_count24, .-bitfield__read_var_BitField_offset4_count24
	.align 4
	.global bitfield__write_var_BitField_offset4_count24
	.hidden	bitfield__write_var_BitField_offset4_count24
	.type	bitfield__write_var_BitField_offset4_count24, #function
	.proc	020
bitfield__write_var_BitField_offset4_count24:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL681
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(16776192), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL682
	 nop
.LL681:
	sethi	%hi(30720), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL682:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-268435456), %g1
	or	%g1, 15, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(16776192), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 4, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset4_count24, .-bitfield__write_var_BitField_offset4_count24
	.align 4
	.global bitfield__read_var_BitField_offset4_count25
	.hidden	bitfield__read_var_BitField_offset4_count25
	.type	bitfield__read_var_BitField_offset4_count25, #function
	.proc	04
bitfield__read_var_BitField_offset4_count25:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 4, %g1
	srl	%g1, 7, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset4_count25, .-bitfield__read_var_BitField_offset4_count25
	.align 4
	.global bitfield__write_var_BitField_offset4_count25
	.hidden	bitfield__write_var_BitField_offset4_count25
	.type	bitfield__write_var_BitField_offset4_count25, #function
	.proc	020
bitfield__write_var_BitField_offset4_count25:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL687
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(33553408), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL688
	 nop
.LL687:
	sethi	%hi(30720), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL688:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-268435456), %g1
	or	%g1, 7, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(33553408), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 3, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset4_count25, .-bitfield__write_var_BitField_offset4_count25
	.align 4
	.global bitfield__read_var_BitField_offset4_count26
	.hidden	bitfield__read_var_BitField_offset4_count26
	.type	bitfield__read_var_BitField_offset4_count26, #function
	.proc	04
bitfield__read_var_BitField_offset4_count26:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 4, %g1
	srl	%g1, 6, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset4_count26, .-bitfield__read_var_BitField_offset4_count26
	.align 4
	.global bitfield__write_var_BitField_offset4_count26
	.hidden	bitfield__write_var_BitField_offset4_count26
	.type	bitfield__write_var_BitField_offset4_count26, #function
	.proc	020
bitfield__write_var_BitField_offset4_count26:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL693
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(67107840), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL694
	 nop
.LL693:
	sethi	%hi(30720), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL694:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-268435456), %g1
	or	%g1, 3, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(67107840), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 2, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset4_count26, .-bitfield__write_var_BitField_offset4_count26
	.align 4
	.global bitfield__read_var_BitField_offset4_count27
	.hidden	bitfield__read_var_BitField_offset4_count27
	.type	bitfield__read_var_BitField_offset4_count27, #function
	.proc	04
bitfield__read_var_BitField_offset4_count27:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 4, %g1
	srl	%g1, 5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset4_count27, .-bitfield__read_var_BitField_offset4_count27
	.align 4
	.global bitfield__write_var_BitField_offset4_count27
	.hidden	bitfield__write_var_BitField_offset4_count27
	.type	bitfield__write_var_BitField_offset4_count27, #function
	.proc	020
bitfield__write_var_BitField_offset4_count27:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL699
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(134216704), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL700
	 nop
.LL699:
	sethi	%hi(30720), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL700:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-268435456), %g1
	or	%g1, 1, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(134216704), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	add	%g1, %g1, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset4_count27, .-bitfield__write_var_BitField_offset4_count27
	.align 4
	.global bitfield__read_var_BitField_offset4_count28
	.hidden	bitfield__read_var_BitField_offset4_count28
	.type	bitfield__read_var_BitField_offset4_count28, #function
	.proc	04
bitfield__read_var_BitField_offset4_count28:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(268434432), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset4_count28, .-bitfield__read_var_BitField_offset4_count28
	.align 4
	.global bitfield__write_var_BitField_offset4_count28
	.hidden	bitfield__write_var_BitField_offset4_count28
	.type	bitfield__write_var_BitField_offset4_count28, #function
	.proc	020
bitfield__write_var_BitField_offset4_count28:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL705
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(268434432), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL706
	 nop
.LL705:
	sethi	%hi(31744), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL706:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-268435456), %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(268434432), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset4_count28, .-bitfield__write_var_BitField_offset4_count28
	.align 4
	.global bitfield__read_var_BitField_offset5_count1
	.hidden	bitfield__read_var_BitField_offset5_count1
	.type	bitfield__read_var_BitField_offset5_count1, #function
	.proc	04
bitfield__read_var_BitField_offset5_count1:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 29, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset5_count1, .-bitfield__read_var_BitField_offset5_count1
	.align 4
	.global bitfield__write_var_BitField_offset5_count1
	.hidden	bitfield__write_var_BitField_offset5_count1
	.type	bitfield__write_var_BitField_offset5_count1, #function
	.proc	020
bitfield__write_var_BitField_offset5_count1:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL711
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1
	ble	%icc, .LL712
	 nop
.LL711:
	sethi	%hi(31744), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL712:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -5, %g2
	ld	[%fp-24], %g1
	and	%g1, 1, %g1
	sll	%g1, 2, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset5_count1, .-bitfield__write_var_BitField_offset5_count1
	.align 4
	.global bitfield__read_var_BitField_offset5_count2
	.hidden	bitfield__read_var_BitField_offset5_count2
	.type	bitfield__read_var_BitField_offset5_count2, #function
	.proc	04
bitfield__read_var_BitField_offset5_count2:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 29, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset5_count2, .-bitfield__read_var_BitField_offset5_count2
	.align 4
	.global bitfield__write_var_BitField_offset5_count2
	.hidden	bitfield__write_var_BitField_offset5_count2
	.type	bitfield__write_var_BitField_offset5_count2, #function
	.proc	020
bitfield__write_var_BitField_offset5_count2:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL717
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 3
	ble	%icc, .LL718
	 nop
.LL717:
	sethi	%hi(31744), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL718:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -7, %g2
	ld	[%fp-24], %g1
	and	%g1, 3, %g1
	add	%g1, %g1, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset5_count2, .-bitfield__write_var_BitField_offset5_count2
	.align 4
	.global bitfield__read_var_BitField_offset5_count3
	.hidden	bitfield__read_var_BitField_offset5_count3
	.type	bitfield__read_var_BitField_offset5_count3, #function
	.proc	04
bitfield__read_var_BitField_offset5_count3:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, 7, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset5_count3, .-bitfield__read_var_BitField_offset5_count3
	.align 4
	.global bitfield__write_var_BitField_offset5_count3
	.hidden	bitfield__write_var_BitField_offset5_count3
	.type	bitfield__write_var_BitField_offset5_count3, #function
	.proc	020
bitfield__write_var_BitField_offset5_count3:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL723
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 7
	ble	%icc, .LL724
	 nop
.LL723:
	sethi	%hi(31744), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL724:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -8, %g2
	ld	[%fp-24], %g1
	and	%g1, 7, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset5_count3, .-bitfield__write_var_BitField_offset5_count3
	.align 4
	.global bitfield__read_var_BitField_offset5_count4
	.hidden	bitfield__read_var_BitField_offset5_count4
	.type	bitfield__read_var_BitField_offset5_count4, #function
	.proc	04
bitfield__read_var_BitField_offset5_count4:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 21, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset5_count4, .-bitfield__read_var_BitField_offset5_count4
	.align 4
	.global bitfield__write_var_BitField_offset5_count4
	.hidden	bitfield__write_var_BitField_offset5_count4
	.type	bitfield__write_var_BitField_offset5_count4, #function
	.proc	020
bitfield__write_var_BitField_offset5_count4:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL729
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 15
	ble	%icc, .LL730
	 nop
.LL729:
	sethi	%hi(32768), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL730:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -1921, %g2
	ld	[%fp-24], %g1
	and	%g1, 15, %g1
	sll	%g1, 7, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset5_count4, .-bitfield__write_var_BitField_offset5_count4
	.align 4
	.global bitfield__read_var_BitField_offset5_count5
	.hidden	bitfield__read_var_BitField_offset5_count5
	.type	bitfield__read_var_BitField_offset5_count5, #function
	.proc	04
bitfield__read_var_BitField_offset5_count5:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 21, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset5_count5, .-bitfield__read_var_BitField_offset5_count5
	.align 4
	.global bitfield__write_var_BitField_offset5_count5
	.hidden	bitfield__write_var_BitField_offset5_count5
	.type	bitfield__write_var_BitField_offset5_count5, #function
	.proc	020
bitfield__write_var_BitField_offset5_count5:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL735
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 31
	ble	%icc, .LL736
	 nop
.LL735:
	sethi	%hi(32768), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL736:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -1985, %g2
	ld	[%fp-24], %g1
	and	%g1, 31, %g1
	sll	%g1, 6, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset5_count5, .-bitfield__write_var_BitField_offset5_count5
	.align 4
	.global bitfield__read_var_BitField_offset5_count6
	.hidden	bitfield__read_var_BitField_offset5_count6
	.type	bitfield__read_var_BitField_offset5_count6, #function
	.proc	04
bitfield__read_var_BitField_offset5_count6:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 21, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset5_count6, .-bitfield__read_var_BitField_offset5_count6
	.align 4
	.global bitfield__write_var_BitField_offset5_count6
	.hidden	bitfield__write_var_BitField_offset5_count6
	.type	bitfield__write_var_BitField_offset5_count6, #function
	.proc	020
bitfield__write_var_BitField_offset5_count6:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL741
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 63
	ble	%icc, .LL742
	 nop
.LL741:
	sethi	%hi(32768), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL742:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -2017, %g2
	ld	[%fp-24], %g1
	and	%g1, 63, %g1
	sll	%g1, 5, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset5_count6, .-bitfield__write_var_BitField_offset5_count6
	.align 4
	.global bitfield__read_var_BitField_offset5_count7
	.hidden	bitfield__read_var_BitField_offset5_count7
	.type	bitfield__read_var_BitField_offset5_count7, #function
	.proc	04
bitfield__read_var_BitField_offset5_count7:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 21, %g1
	srl	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset5_count7, .-bitfield__read_var_BitField_offset5_count7
	.align 4
	.global bitfield__write_var_BitField_offset5_count7
	.hidden	bitfield__write_var_BitField_offset5_count7
	.type	bitfield__write_var_BitField_offset5_count7, #function
	.proc	020
bitfield__write_var_BitField_offset5_count7:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL747
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 127
	ble	%icc, .LL748
	 nop
.LL747:
	sethi	%hi(32768), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL748:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -2033, %g2
	ld	[%fp-24], %g1
	and	%g1, 127, %g1
	sll	%g1, 4, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset5_count7, .-bitfield__write_var_BitField_offset5_count7
	.align 4
	.global bitfield__read_var_BitField_offset5_count8
	.hidden	bitfield__read_var_BitField_offset5_count8
	.type	bitfield__read_var_BitField_offset5_count8, #function
	.proc	04
bitfield__read_var_BitField_offset5_count8:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 21, %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset5_count8, .-bitfield__read_var_BitField_offset5_count8
	.align 4
	.global bitfield__write_var_BitField_offset5_count8
	.hidden	bitfield__write_var_BitField_offset5_count8
	.type	bitfield__write_var_BitField_offset5_count8, #function
	.proc	020
bitfield__write_var_BitField_offset5_count8:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL753
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 255
	ble	%icc, .LL754
	 nop
.LL753:
	sethi	%hi(33792), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL754:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -2041, %g2
	ld	[%fp-24], %g1
	and	%g1, 255, %g1
	sll	%g1, 3, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset5_count8, .-bitfield__write_var_BitField_offset5_count8
	.align 4
	.global bitfield__read_var_BitField_offset5_count9
	.hidden	bitfield__read_var_BitField_offset5_count9
	.type	bitfield__read_var_BitField_offset5_count9, #function
	.proc	04
bitfield__read_var_BitField_offset5_count9:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 21, %g1
	srl	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset5_count9, .-bitfield__read_var_BitField_offset5_count9
	.align 4
	.global bitfield__write_var_BitField_offset5_count9
	.hidden	bitfield__write_var_BitField_offset5_count9
	.type	bitfield__write_var_BitField_offset5_count9, #function
	.proc	020
bitfield__write_var_BitField_offset5_count9:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL759
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 511
	ble	%icc, .LL760
	 nop
.LL759:
	sethi	%hi(33792), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL760:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -2045, %g2
	ld	[%fp-24], %g1
	and	%g1, 511, %g1
	sll	%g1, 2, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset5_count9, .-bitfield__write_var_BitField_offset5_count9
	.align 4
	.global bitfield__read_var_BitField_offset5_count10
	.hidden	bitfield__read_var_BitField_offset5_count10
	.type	bitfield__read_var_BitField_offset5_count10, #function
	.proc	04
bitfield__read_var_BitField_offset5_count10:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 21, %g1
	srl	%g1, 22, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset5_count10, .-bitfield__read_var_BitField_offset5_count10
	.align 4
	.global bitfield__write_var_BitField_offset5_count10
	.hidden	bitfield__write_var_BitField_offset5_count10
	.type	bitfield__write_var_BitField_offset5_count10, #function
	.proc	020
bitfield__write_var_BitField_offset5_count10:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL765
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1023
	ble	%icc, .LL766
	 nop
.LL765:
	sethi	%hi(33792), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL766:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -2047, %g2
	ld	[%fp-24], %g1
	and	%g1, 1023, %g1
	add	%g1, %g1, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset5_count10, .-bitfield__write_var_BitField_offset5_count10
	.align 4
	.global bitfield__read_var_BitField_offset5_count11
	.hidden	bitfield__read_var_BitField_offset5_count11
	.type	bitfield__read_var_BitField_offset5_count11, #function
	.proc	04
bitfield__read_var_BitField_offset5_count11:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, 2047, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset5_count11, .-bitfield__read_var_BitField_offset5_count11
	.align 4
	.global bitfield__write_var_BitField_offset5_count11
	.hidden	bitfield__write_var_BitField_offset5_count11
	.type	bitfield__write_var_BitField_offset5_count11, #function
	.proc	020
bitfield__write_var_BitField_offset5_count11:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL771
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 2047
	ble	%icc, .LL772
	 nop
.LL771:
	sethi	%hi(33792), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL772:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -2048, %g2
	ld	[%fp-24], %g1
	and	%g1, 2047, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset5_count11, .-bitfield__write_var_BitField_offset5_count11
	.align 4
	.global bitfield__read_var_BitField_offset5_count12
	.hidden	bitfield__read_var_BitField_offset5_count12
	.type	bitfield__read_var_BitField_offset5_count12, #function
	.proc	04
bitfield__read_var_BitField_offset5_count12:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 5, %g1
	srl	%g1, 20, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset5_count12, .-bitfield__read_var_BitField_offset5_count12
	.align 4
	.global bitfield__write_var_BitField_offset5_count12
	.hidden	bitfield__write_var_BitField_offset5_count12
	.type	bitfield__write_var_BitField_offset5_count12, #function
	.proc	020
bitfield__write_var_BitField_offset5_count12:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL777
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 4095
	ble	%icc, .LL778
	 nop
.LL777:
	sethi	%hi(34816), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL778:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-134185984), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 4095, %g1
	sll	%g1, 15, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset5_count12, .-bitfield__write_var_BitField_offset5_count12
	.align 4
	.global bitfield__read_var_BitField_offset5_count13
	.hidden	bitfield__read_var_BitField_offset5_count13
	.type	bitfield__read_var_BitField_offset5_count13, #function
	.proc	04
bitfield__read_var_BitField_offset5_count13:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 5, %g1
	srl	%g1, 19, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset5_count13, .-bitfield__read_var_BitField_offset5_count13
	.align 4
	.global bitfield__write_var_BitField_offset5_count13
	.hidden	bitfield__write_var_BitField_offset5_count13
	.type	bitfield__write_var_BitField_offset5_count13, #function
	.proc	020
bitfield__write_var_BitField_offset5_count13:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL783
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(7168), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL784
	 nop
.LL783:
	sethi	%hi(34816), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL784:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-134202368), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 14, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset5_count13, .-bitfield__write_var_BitField_offset5_count13
	.align 4
	.global bitfield__read_var_BitField_offset5_count14
	.hidden	bitfield__read_var_BitField_offset5_count14
	.type	bitfield__read_var_BitField_offset5_count14, #function
	.proc	04
bitfield__read_var_BitField_offset5_count14:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 5, %g1
	srl	%g1, 18, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset5_count14, .-bitfield__read_var_BitField_offset5_count14
	.align 4
	.global bitfield__write_var_BitField_offset5_count14
	.hidden	bitfield__write_var_BitField_offset5_count14
	.type	bitfield__write_var_BitField_offset5_count14, #function
	.proc	020
bitfield__write_var_BitField_offset5_count14:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL789
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(15360), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL790
	 nop
.LL789:
	sethi	%hi(34816), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL790:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-134210560), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 13, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset5_count14, .-bitfield__write_var_BitField_offset5_count14
	.align 4
	.global bitfield__read_var_BitField_offset5_count15
	.hidden	bitfield__read_var_BitField_offset5_count15
	.type	bitfield__read_var_BitField_offset5_count15, #function
	.proc	04
bitfield__read_var_BitField_offset5_count15:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 5, %g1
	srl	%g1, 17, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset5_count15, .-bitfield__read_var_BitField_offset5_count15
	.align 4
	.global bitfield__write_var_BitField_offset5_count15
	.hidden	bitfield__write_var_BitField_offset5_count15
	.type	bitfield__write_var_BitField_offset5_count15, #function
	.proc	020
bitfield__write_var_BitField_offset5_count15:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL795
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(31744), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL796
	 nop
.LL795:
	sethi	%hi(34816), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL796:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-134214656), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 12, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset5_count15, .-bitfield__write_var_BitField_offset5_count15
	.align 4
	.global bitfield__read_var_BitField_offset5_count16
	.hidden	bitfield__read_var_BitField_offset5_count16
	.type	bitfield__read_var_BitField_offset5_count16, #function
	.proc	04
bitfield__read_var_BitField_offset5_count16:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 5, %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset5_count16, .-bitfield__read_var_BitField_offset5_count16
	.align 4
	.global bitfield__write_var_BitField_offset5_count16
	.hidden	bitfield__write_var_BitField_offset5_count16
	.type	bitfield__write_var_BitField_offset5_count16, #function
	.proc	020
bitfield__write_var_BitField_offset5_count16:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL801
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(64512), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL802
	 nop
.LL801:
	sethi	%hi(35840), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL802:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-134216704), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 11, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset5_count16, .-bitfield__write_var_BitField_offset5_count16
	.align 4
	.global bitfield__read_var_BitField_offset5_count17
	.hidden	bitfield__read_var_BitField_offset5_count17
	.type	bitfield__read_var_BitField_offset5_count17, #function
	.proc	04
bitfield__read_var_BitField_offset5_count17:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 5, %g1
	srl	%g1, 15, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset5_count17, .-bitfield__read_var_BitField_offset5_count17
	.align 4
	.global bitfield__write_var_BitField_offset5_count17
	.hidden	bitfield__write_var_BitField_offset5_count17
	.type	bitfield__write_var_BitField_offset5_count17, #function
	.proc	020
bitfield__write_var_BitField_offset5_count17:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL807
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(130048), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL808
	 nop
.LL807:
	sethi	%hi(35840), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL808:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-134217728), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(130048), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 10, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset5_count17, .-bitfield__write_var_BitField_offset5_count17
	.align 4
	.global bitfield__read_var_BitField_offset5_count18
	.hidden	bitfield__read_var_BitField_offset5_count18
	.type	bitfield__read_var_BitField_offset5_count18, #function
	.proc	04
bitfield__read_var_BitField_offset5_count18:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 5, %g1
	srl	%g1, 14, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset5_count18, .-bitfield__read_var_BitField_offset5_count18
	.align 4
	.global bitfield__write_var_BitField_offset5_count18
	.hidden	bitfield__write_var_BitField_offset5_count18
	.type	bitfield__write_var_BitField_offset5_count18, #function
	.proc	020
bitfield__write_var_BitField_offset5_count18:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL813
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(261120), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL814
	 nop
.LL813:
	sethi	%hi(35840), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL814:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-134217728), %g1
	or	%g1, 511, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(261120), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 9, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset5_count18, .-bitfield__write_var_BitField_offset5_count18
	.align 4
	.global bitfield__read_var_BitField_offset5_count19
	.hidden	bitfield__read_var_BitField_offset5_count19
	.type	bitfield__read_var_BitField_offset5_count19, #function
	.proc	04
bitfield__read_var_BitField_offset5_count19:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 5, %g1
	srl	%g1, 13, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset5_count19, .-bitfield__read_var_BitField_offset5_count19
	.align 4
	.global bitfield__write_var_BitField_offset5_count19
	.hidden	bitfield__write_var_BitField_offset5_count19
	.type	bitfield__write_var_BitField_offset5_count19, #function
	.proc	020
bitfield__write_var_BitField_offset5_count19:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL819
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(523264), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL820
	 nop
.LL819:
	sethi	%hi(35840), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL820:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-134217728), %g1
	or	%g1, 255, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(523264), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 8, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset5_count19, .-bitfield__write_var_BitField_offset5_count19
	.align 4
	.global bitfield__read_var_BitField_offset5_count20
	.hidden	bitfield__read_var_BitField_offset5_count20
	.type	bitfield__read_var_BitField_offset5_count20, #function
	.proc	04
bitfield__read_var_BitField_offset5_count20:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 5, %g1
	srl	%g1, 12, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset5_count20, .-bitfield__read_var_BitField_offset5_count20
	.align 4
	.global bitfield__write_var_BitField_offset5_count20
	.hidden	bitfield__write_var_BitField_offset5_count20
	.type	bitfield__write_var_BitField_offset5_count20, #function
	.proc	020
bitfield__write_var_BitField_offset5_count20:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL825
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(1047552), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL826
	 nop
.LL825:
	sethi	%hi(36864), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL826:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-134217728), %g1
	or	%g1, 127, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(1047552), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 7, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset5_count20, .-bitfield__write_var_BitField_offset5_count20
	.align 4
	.global bitfield__read_var_BitField_offset5_count21
	.hidden	bitfield__read_var_BitField_offset5_count21
	.type	bitfield__read_var_BitField_offset5_count21, #function
	.proc	04
bitfield__read_var_BitField_offset5_count21:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 5, %g1
	srl	%g1, 11, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset5_count21, .-bitfield__read_var_BitField_offset5_count21
	.align 4
	.global bitfield__write_var_BitField_offset5_count21
	.hidden	bitfield__write_var_BitField_offset5_count21
	.type	bitfield__write_var_BitField_offset5_count21, #function
	.proc	020
bitfield__write_var_BitField_offset5_count21:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL831
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(2096128), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL832
	 nop
.LL831:
	sethi	%hi(36864), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL832:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-134217728), %g1
	or	%g1, 63, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(2096128), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 6, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset5_count21, .-bitfield__write_var_BitField_offset5_count21
	.align 4
	.global bitfield__read_var_BitField_offset5_count22
	.hidden	bitfield__read_var_BitField_offset5_count22
	.type	bitfield__read_var_BitField_offset5_count22, #function
	.proc	04
bitfield__read_var_BitField_offset5_count22:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 5, %g1
	srl	%g1, 10, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset5_count22, .-bitfield__read_var_BitField_offset5_count22
	.align 4
	.global bitfield__write_var_BitField_offset5_count22
	.hidden	bitfield__write_var_BitField_offset5_count22
	.type	bitfield__write_var_BitField_offset5_count22, #function
	.proc	020
bitfield__write_var_BitField_offset5_count22:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL837
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(4193280), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL838
	 nop
.LL837:
	sethi	%hi(36864), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL838:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-134217728), %g1
	or	%g1, 31, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(4193280), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 5, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset5_count22, .-bitfield__write_var_BitField_offset5_count22
	.align 4
	.global bitfield__read_var_BitField_offset5_count23
	.hidden	bitfield__read_var_BitField_offset5_count23
	.type	bitfield__read_var_BitField_offset5_count23, #function
	.proc	04
bitfield__read_var_BitField_offset5_count23:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 5, %g1
	srl	%g1, 9, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset5_count23, .-bitfield__read_var_BitField_offset5_count23
	.align 4
	.global bitfield__write_var_BitField_offset5_count23
	.hidden	bitfield__write_var_BitField_offset5_count23
	.type	bitfield__write_var_BitField_offset5_count23, #function
	.proc	020
bitfield__write_var_BitField_offset5_count23:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL843
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(8387584), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL844
	 nop
.LL843:
	sethi	%hi(36864), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL844:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-134217728), %g1
	or	%g1, 15, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(8387584), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 4, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset5_count23, .-bitfield__write_var_BitField_offset5_count23
	.align 4
	.global bitfield__read_var_BitField_offset5_count24
	.hidden	bitfield__read_var_BitField_offset5_count24
	.type	bitfield__read_var_BitField_offset5_count24, #function
	.proc	04
bitfield__read_var_BitField_offset5_count24:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 5, %g1
	srl	%g1, 8, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset5_count24, .-bitfield__read_var_BitField_offset5_count24
	.align 4
	.global bitfield__write_var_BitField_offset5_count24
	.hidden	bitfield__write_var_BitField_offset5_count24
	.type	bitfield__write_var_BitField_offset5_count24, #function
	.proc	020
bitfield__write_var_BitField_offset5_count24:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL849
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(16776192), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL850
	 nop
.LL849:
	sethi	%hi(37888), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL850:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-134217728), %g1
	or	%g1, 7, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(16776192), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 3, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset5_count24, .-bitfield__write_var_BitField_offset5_count24
	.align 4
	.global bitfield__read_var_BitField_offset5_count25
	.hidden	bitfield__read_var_BitField_offset5_count25
	.type	bitfield__read_var_BitField_offset5_count25, #function
	.proc	04
bitfield__read_var_BitField_offset5_count25:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 5, %g1
	srl	%g1, 7, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset5_count25, .-bitfield__read_var_BitField_offset5_count25
	.align 4
	.global bitfield__write_var_BitField_offset5_count25
	.hidden	bitfield__write_var_BitField_offset5_count25
	.type	bitfield__write_var_BitField_offset5_count25, #function
	.proc	020
bitfield__write_var_BitField_offset5_count25:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL855
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(33553408), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL856
	 nop
.LL855:
	sethi	%hi(37888), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL856:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-134217728), %g1
	or	%g1, 3, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(33553408), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 2, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset5_count25, .-bitfield__write_var_BitField_offset5_count25
	.align 4
	.global bitfield__read_var_BitField_offset5_count26
	.hidden	bitfield__read_var_BitField_offset5_count26
	.type	bitfield__read_var_BitField_offset5_count26, #function
	.proc	04
bitfield__read_var_BitField_offset5_count26:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 5, %g1
	srl	%g1, 6, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset5_count26, .-bitfield__read_var_BitField_offset5_count26
	.align 4
	.global bitfield__write_var_BitField_offset5_count26
	.hidden	bitfield__write_var_BitField_offset5_count26
	.type	bitfield__write_var_BitField_offset5_count26, #function
	.proc	020
bitfield__write_var_BitField_offset5_count26:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL861
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(67107840), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL862
	 nop
.LL861:
	sethi	%hi(37888), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL862:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-134217728), %g1
	or	%g1, 1, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(67107840), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	add	%g1, %g1, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset5_count26, .-bitfield__write_var_BitField_offset5_count26
	.align 4
	.global bitfield__read_var_BitField_offset5_count27
	.hidden	bitfield__read_var_BitField_offset5_count27
	.type	bitfield__read_var_BitField_offset5_count27, #function
	.proc	04
bitfield__read_var_BitField_offset5_count27:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(134216704), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset5_count27, .-bitfield__read_var_BitField_offset5_count27
	.align 4
	.global bitfield__write_var_BitField_offset5_count27
	.hidden	bitfield__write_var_BitField_offset5_count27
	.type	bitfield__write_var_BitField_offset5_count27, #function
	.proc	020
bitfield__write_var_BitField_offset5_count27:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL867
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(134216704), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL868
	 nop
.LL867:
	sethi	%hi(37888), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL868:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-134217728), %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(134216704), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset5_count27, .-bitfield__write_var_BitField_offset5_count27
	.align 4
	.global bitfield__read_var_BitField_offset6_count1
	.hidden	bitfield__read_var_BitField_offset6_count1
	.type	bitfield__read_var_BitField_offset6_count1, #function
	.proc	04
bitfield__read_var_BitField_offset6_count1:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 30, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset6_count1, .-bitfield__read_var_BitField_offset6_count1
	.align 4
	.global bitfield__write_var_BitField_offset6_count1
	.hidden	bitfield__write_var_BitField_offset6_count1
	.type	bitfield__write_var_BitField_offset6_count1, #function
	.proc	020
bitfield__write_var_BitField_offset6_count1:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL873
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1
	ble	%icc, .LL874
	 nop
.LL873:
	sethi	%hi(38912), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL874:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -3, %g2
	ld	[%fp-24], %g1
	and	%g1, 1, %g1
	add	%g1, %g1, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset6_count1, .-bitfield__write_var_BitField_offset6_count1
	.align 4
	.global bitfield__read_var_BitField_offset6_count2
	.hidden	bitfield__read_var_BitField_offset6_count2
	.type	bitfield__read_var_BitField_offset6_count2, #function
	.proc	04
bitfield__read_var_BitField_offset6_count2:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, 3, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset6_count2, .-bitfield__read_var_BitField_offset6_count2
	.align 4
	.global bitfield__write_var_BitField_offset6_count2
	.hidden	bitfield__write_var_BitField_offset6_count2
	.type	bitfield__write_var_BitField_offset6_count2, #function
	.proc	020
bitfield__write_var_BitField_offset6_count2:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL879
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 3
	ble	%icc, .LL880
	 nop
.LL879:
	sethi	%hi(38912), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL880:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -4, %g2
	ld	[%fp-24], %g1
	and	%g1, 3, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset6_count2, .-bitfield__write_var_BitField_offset6_count2
	.align 4
	.global bitfield__read_var_BitField_offset6_count3
	.hidden	bitfield__read_var_BitField_offset6_count3
	.type	bitfield__read_var_BitField_offset6_count3, #function
	.proc	04
bitfield__read_var_BitField_offset6_count3:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 22, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset6_count3, .-bitfield__read_var_BitField_offset6_count3
	.align 4
	.global bitfield__write_var_BitField_offset6_count3
	.hidden	bitfield__write_var_BitField_offset6_count3
	.type	bitfield__write_var_BitField_offset6_count3, #function
	.proc	020
bitfield__write_var_BitField_offset6_count3:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL885
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 7
	ble	%icc, .LL886
	 nop
.LL885:
	sethi	%hi(38912), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL886:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -897, %g2
	ld	[%fp-24], %g1
	and	%g1, 7, %g1
	sll	%g1, 7, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset6_count3, .-bitfield__write_var_BitField_offset6_count3
	.align 4
	.global bitfield__read_var_BitField_offset6_count4
	.hidden	bitfield__read_var_BitField_offset6_count4
	.type	bitfield__read_var_BitField_offset6_count4, #function
	.proc	04
bitfield__read_var_BitField_offset6_count4:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 22, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset6_count4, .-bitfield__read_var_BitField_offset6_count4
	.align 4
	.global bitfield__write_var_BitField_offset6_count4
	.hidden	bitfield__write_var_BitField_offset6_count4
	.type	bitfield__write_var_BitField_offset6_count4, #function
	.proc	020
bitfield__write_var_BitField_offset6_count4:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL891
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 15
	ble	%icc, .LL892
	 nop
.LL891:
	sethi	%hi(38912), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL892:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -961, %g2
	ld	[%fp-24], %g1
	and	%g1, 15, %g1
	sll	%g1, 6, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset6_count4, .-bitfield__write_var_BitField_offset6_count4
	.align 4
	.global bitfield__read_var_BitField_offset6_count5
	.hidden	bitfield__read_var_BitField_offset6_count5
	.type	bitfield__read_var_BitField_offset6_count5, #function
	.proc	04
bitfield__read_var_BitField_offset6_count5:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 22, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset6_count5, .-bitfield__read_var_BitField_offset6_count5
	.align 4
	.global bitfield__write_var_BitField_offset6_count5
	.hidden	bitfield__write_var_BitField_offset6_count5
	.type	bitfield__write_var_BitField_offset6_count5, #function
	.proc	020
bitfield__write_var_BitField_offset6_count5:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL897
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 31
	ble	%icc, .LL898
	 nop
.LL897:
	sethi	%hi(39936), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL898:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -993, %g2
	ld	[%fp-24], %g1
	and	%g1, 31, %g1
	sll	%g1, 5, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset6_count5, .-bitfield__write_var_BitField_offset6_count5
	.align 4
	.global bitfield__read_var_BitField_offset6_count6
	.hidden	bitfield__read_var_BitField_offset6_count6
	.type	bitfield__read_var_BitField_offset6_count6, #function
	.proc	04
bitfield__read_var_BitField_offset6_count6:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 22, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset6_count6, .-bitfield__read_var_BitField_offset6_count6
	.align 4
	.global bitfield__write_var_BitField_offset6_count6
	.hidden	bitfield__write_var_BitField_offset6_count6
	.type	bitfield__write_var_BitField_offset6_count6, #function
	.proc	020
bitfield__write_var_BitField_offset6_count6:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL903
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 63
	ble	%icc, .LL904
	 nop
.LL903:
	sethi	%hi(39936), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL904:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -1009, %g2
	ld	[%fp-24], %g1
	and	%g1, 63, %g1
	sll	%g1, 4, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset6_count6, .-bitfield__write_var_BitField_offset6_count6
	.align 4
	.global bitfield__read_var_BitField_offset6_count7
	.hidden	bitfield__read_var_BitField_offset6_count7
	.type	bitfield__read_var_BitField_offset6_count7, #function
	.proc	04
bitfield__read_var_BitField_offset6_count7:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 22, %g1
	srl	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset6_count7, .-bitfield__read_var_BitField_offset6_count7
	.align 4
	.global bitfield__write_var_BitField_offset6_count7
	.hidden	bitfield__write_var_BitField_offset6_count7
	.type	bitfield__write_var_BitField_offset6_count7, #function
	.proc	020
bitfield__write_var_BitField_offset6_count7:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL909
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 127
	ble	%icc, .LL910
	 nop
.LL909:
	sethi	%hi(39936), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL910:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -1017, %g2
	ld	[%fp-24], %g1
	and	%g1, 127, %g1
	sll	%g1, 3, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset6_count7, .-bitfield__write_var_BitField_offset6_count7
	.align 4
	.global bitfield__read_var_BitField_offset6_count8
	.hidden	bitfield__read_var_BitField_offset6_count8
	.type	bitfield__read_var_BitField_offset6_count8, #function
	.proc	04
bitfield__read_var_BitField_offset6_count8:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 22, %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset6_count8, .-bitfield__read_var_BitField_offset6_count8
	.align 4
	.global bitfield__write_var_BitField_offset6_count8
	.hidden	bitfield__write_var_BitField_offset6_count8
	.type	bitfield__write_var_BitField_offset6_count8, #function
	.proc	020
bitfield__write_var_BitField_offset6_count8:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL915
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 255
	ble	%icc, .LL916
	 nop
.LL915:
	sethi	%hi(39936), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL916:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -1021, %g2
	ld	[%fp-24], %g1
	and	%g1, 255, %g1
	sll	%g1, 2, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset6_count8, .-bitfield__write_var_BitField_offset6_count8
	.align 4
	.global bitfield__read_var_BitField_offset6_count9
	.hidden	bitfield__read_var_BitField_offset6_count9
	.type	bitfield__read_var_BitField_offset6_count9, #function
	.proc	04
bitfield__read_var_BitField_offset6_count9:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 22, %g1
	srl	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset6_count9, .-bitfield__read_var_BitField_offset6_count9
	.align 4
	.global bitfield__write_var_BitField_offset6_count9
	.hidden	bitfield__write_var_BitField_offset6_count9
	.type	bitfield__write_var_BitField_offset6_count9, #function
	.proc	020
bitfield__write_var_BitField_offset6_count9:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL921
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 511
	ble	%icc, .LL922
	 nop
.LL921:
	sethi	%hi(40960), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL922:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -1023, %g2
	ld	[%fp-24], %g1
	and	%g1, 511, %g1
	add	%g1, %g1, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset6_count9, .-bitfield__write_var_BitField_offset6_count9
	.align 4
	.global bitfield__read_var_BitField_offset6_count10
	.hidden	bitfield__read_var_BitField_offset6_count10
	.type	bitfield__read_var_BitField_offset6_count10, #function
	.proc	04
bitfield__read_var_BitField_offset6_count10:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, 1023, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset6_count10, .-bitfield__read_var_BitField_offset6_count10
	.align 4
	.global bitfield__write_var_BitField_offset6_count10
	.hidden	bitfield__write_var_BitField_offset6_count10
	.type	bitfield__write_var_BitField_offset6_count10, #function
	.proc	020
bitfield__write_var_BitField_offset6_count10:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL927
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1023
	ble	%icc, .LL928
	 nop
.LL927:
	sethi	%hi(40960), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL928:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -1024, %g2
	ld	[%fp-24], %g1
	and	%g1, 1023, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset6_count10, .-bitfield__write_var_BitField_offset6_count10
	.align 4
	.global bitfield__read_var_BitField_offset6_count11
	.hidden	bitfield__read_var_BitField_offset6_count11
	.type	bitfield__read_var_BitField_offset6_count11, #function
	.proc	04
bitfield__read_var_BitField_offset6_count11:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 6, %g1
	srl	%g1, 21, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset6_count11, .-bitfield__read_var_BitField_offset6_count11
	.align 4
	.global bitfield__write_var_BitField_offset6_count11
	.hidden	bitfield__write_var_BitField_offset6_count11
	.type	bitfield__write_var_BitField_offset6_count11, #function
	.proc	020
bitfield__write_var_BitField_offset6_count11:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL933
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 2047
	ble	%icc, .LL934
	 nop
.LL933:
	sethi	%hi(40960), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL934:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-67077120), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 2047, %g1
	sll	%g1, 15, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset6_count11, .-bitfield__write_var_BitField_offset6_count11
	.align 4
	.global bitfield__read_var_BitField_offset6_count12
	.hidden	bitfield__read_var_BitField_offset6_count12
	.type	bitfield__read_var_BitField_offset6_count12, #function
	.proc	04
bitfield__read_var_BitField_offset6_count12:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 6, %g1
	srl	%g1, 20, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset6_count12, .-bitfield__read_var_BitField_offset6_count12
	.align 4
	.global bitfield__write_var_BitField_offset6_count12
	.hidden	bitfield__write_var_BitField_offset6_count12
	.type	bitfield__write_var_BitField_offset6_count12, #function
	.proc	020
bitfield__write_var_BitField_offset6_count12:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL939
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 4095
	ble	%icc, .LL940
	 nop
.LL939:
	sethi	%hi(40960), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL940:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-67093504), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 4095, %g1
	sll	%g1, 14, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset6_count12, .-bitfield__write_var_BitField_offset6_count12
	.align 4
	.global bitfield__read_var_BitField_offset6_count13
	.hidden	bitfield__read_var_BitField_offset6_count13
	.type	bitfield__read_var_BitField_offset6_count13, #function
	.proc	04
bitfield__read_var_BitField_offset6_count13:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 6, %g1
	srl	%g1, 19, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset6_count13, .-bitfield__read_var_BitField_offset6_count13
	.align 4
	.global bitfield__write_var_BitField_offset6_count13
	.hidden	bitfield__write_var_BitField_offset6_count13
	.type	bitfield__write_var_BitField_offset6_count13, #function
	.proc	020
bitfield__write_var_BitField_offset6_count13:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL945
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(7168), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL946
	 nop
.LL945:
	sethi	%hi(41984), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL946:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-67101696), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 13, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset6_count13, .-bitfield__write_var_BitField_offset6_count13
	.align 4
	.global bitfield__read_var_BitField_offset6_count14
	.hidden	bitfield__read_var_BitField_offset6_count14
	.type	bitfield__read_var_BitField_offset6_count14, #function
	.proc	04
bitfield__read_var_BitField_offset6_count14:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 6, %g1
	srl	%g1, 18, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset6_count14, .-bitfield__read_var_BitField_offset6_count14
	.align 4
	.global bitfield__write_var_BitField_offset6_count14
	.hidden	bitfield__write_var_BitField_offset6_count14
	.type	bitfield__write_var_BitField_offset6_count14, #function
	.proc	020
bitfield__write_var_BitField_offset6_count14:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL951
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(15360), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL952
	 nop
.LL951:
	sethi	%hi(41984), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL952:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-67105792), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 12, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset6_count14, .-bitfield__write_var_BitField_offset6_count14
	.align 4
	.global bitfield__read_var_BitField_offset6_count15
	.hidden	bitfield__read_var_BitField_offset6_count15
	.type	bitfield__read_var_BitField_offset6_count15, #function
	.proc	04
bitfield__read_var_BitField_offset6_count15:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 6, %g1
	srl	%g1, 17, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset6_count15, .-bitfield__read_var_BitField_offset6_count15
	.align 4
	.global bitfield__write_var_BitField_offset6_count15
	.hidden	bitfield__write_var_BitField_offset6_count15
	.type	bitfield__write_var_BitField_offset6_count15, #function
	.proc	020
bitfield__write_var_BitField_offset6_count15:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL957
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(31744), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL958
	 nop
.LL957:
	sethi	%hi(41984), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL958:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-67107840), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 11, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset6_count15, .-bitfield__write_var_BitField_offset6_count15
	.align 4
	.global bitfield__read_var_BitField_offset6_count16
	.hidden	bitfield__read_var_BitField_offset6_count16
	.type	bitfield__read_var_BitField_offset6_count16, #function
	.proc	04
bitfield__read_var_BitField_offset6_count16:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 6, %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset6_count16, .-bitfield__read_var_BitField_offset6_count16
	.align 4
	.global bitfield__write_var_BitField_offset6_count16
	.hidden	bitfield__write_var_BitField_offset6_count16
	.type	bitfield__write_var_BitField_offset6_count16, #function
	.proc	020
bitfield__write_var_BitField_offset6_count16:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL963
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(64512), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL964
	 nop
.LL963:
	sethi	%hi(41984), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL964:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-67108864), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 10, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset6_count16, .-bitfield__write_var_BitField_offset6_count16
	.align 4
	.global bitfield__read_var_BitField_offset6_count17
	.hidden	bitfield__read_var_BitField_offset6_count17
	.type	bitfield__read_var_BitField_offset6_count17, #function
	.proc	04
bitfield__read_var_BitField_offset6_count17:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 6, %g1
	srl	%g1, 15, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset6_count17, .-bitfield__read_var_BitField_offset6_count17
	.align 4
	.global bitfield__write_var_BitField_offset6_count17
	.hidden	bitfield__write_var_BitField_offset6_count17
	.type	bitfield__write_var_BitField_offset6_count17, #function
	.proc	020
bitfield__write_var_BitField_offset6_count17:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL969
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(130048), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL970
	 nop
.LL969:
	sethi	%hi(43008), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL970:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-67108864), %g1
	or	%g1, 511, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(130048), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 9, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset6_count17, .-bitfield__write_var_BitField_offset6_count17
	.align 4
	.global bitfield__read_var_BitField_offset6_count18
	.hidden	bitfield__read_var_BitField_offset6_count18
	.type	bitfield__read_var_BitField_offset6_count18, #function
	.proc	04
bitfield__read_var_BitField_offset6_count18:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 6, %g1
	srl	%g1, 14, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset6_count18, .-bitfield__read_var_BitField_offset6_count18
	.align 4
	.global bitfield__write_var_BitField_offset6_count18
	.hidden	bitfield__write_var_BitField_offset6_count18
	.type	bitfield__write_var_BitField_offset6_count18, #function
	.proc	020
bitfield__write_var_BitField_offset6_count18:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL975
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(261120), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL976
	 nop
.LL975:
	sethi	%hi(43008), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL976:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-67108864), %g1
	or	%g1, 255, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(261120), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 8, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset6_count18, .-bitfield__write_var_BitField_offset6_count18
	.align 4
	.global bitfield__read_var_BitField_offset6_count19
	.hidden	bitfield__read_var_BitField_offset6_count19
	.type	bitfield__read_var_BitField_offset6_count19, #function
	.proc	04
bitfield__read_var_BitField_offset6_count19:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 6, %g1
	srl	%g1, 13, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset6_count19, .-bitfield__read_var_BitField_offset6_count19
	.align 4
	.global bitfield__write_var_BitField_offset6_count19
	.hidden	bitfield__write_var_BitField_offset6_count19
	.type	bitfield__write_var_BitField_offset6_count19, #function
	.proc	020
bitfield__write_var_BitField_offset6_count19:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL981
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(523264), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL982
	 nop
.LL981:
	sethi	%hi(43008), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL982:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-67108864), %g1
	or	%g1, 127, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(523264), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 7, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset6_count19, .-bitfield__write_var_BitField_offset6_count19
	.align 4
	.global bitfield__read_var_BitField_offset6_count20
	.hidden	bitfield__read_var_BitField_offset6_count20
	.type	bitfield__read_var_BitField_offset6_count20, #function
	.proc	04
bitfield__read_var_BitField_offset6_count20:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 6, %g1
	srl	%g1, 12, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset6_count20, .-bitfield__read_var_BitField_offset6_count20
	.align 4
	.global bitfield__write_var_BitField_offset6_count20
	.hidden	bitfield__write_var_BitField_offset6_count20
	.type	bitfield__write_var_BitField_offset6_count20, #function
	.proc	020
bitfield__write_var_BitField_offset6_count20:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL987
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(1047552), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL988
	 nop
.LL987:
	sethi	%hi(43008), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL988:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-67108864), %g1
	or	%g1, 63, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(1047552), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 6, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset6_count20, .-bitfield__write_var_BitField_offset6_count20
	.align 4
	.global bitfield__read_var_BitField_offset6_count21
	.hidden	bitfield__read_var_BitField_offset6_count21
	.type	bitfield__read_var_BitField_offset6_count21, #function
	.proc	04
bitfield__read_var_BitField_offset6_count21:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 6, %g1
	srl	%g1, 11, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset6_count21, .-bitfield__read_var_BitField_offset6_count21
	.align 4
	.global bitfield__write_var_BitField_offset6_count21
	.hidden	bitfield__write_var_BitField_offset6_count21
	.type	bitfield__write_var_BitField_offset6_count21, #function
	.proc	020
bitfield__write_var_BitField_offset6_count21:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL993
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(2096128), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL994
	 nop
.LL993:
	sethi	%hi(44032), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL994:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-67108864), %g1
	or	%g1, 31, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(2096128), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 5, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset6_count21, .-bitfield__write_var_BitField_offset6_count21
	.align 4
	.global bitfield__read_var_BitField_offset6_count22
	.hidden	bitfield__read_var_BitField_offset6_count22
	.type	bitfield__read_var_BitField_offset6_count22, #function
	.proc	04
bitfield__read_var_BitField_offset6_count22:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 6, %g1
	srl	%g1, 10, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset6_count22, .-bitfield__read_var_BitField_offset6_count22
	.align 4
	.global bitfield__write_var_BitField_offset6_count22
	.hidden	bitfield__write_var_BitField_offset6_count22
	.type	bitfield__write_var_BitField_offset6_count22, #function
	.proc	020
bitfield__write_var_BitField_offset6_count22:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL999
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(4193280), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1000
	 nop
.LL999:
	sethi	%hi(44032), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1000:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-67108864), %g1
	or	%g1, 15, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(4193280), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 4, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset6_count22, .-bitfield__write_var_BitField_offset6_count22
	.align 4
	.global bitfield__read_var_BitField_offset6_count23
	.hidden	bitfield__read_var_BitField_offset6_count23
	.type	bitfield__read_var_BitField_offset6_count23, #function
	.proc	04
bitfield__read_var_BitField_offset6_count23:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 6, %g1
	srl	%g1, 9, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset6_count23, .-bitfield__read_var_BitField_offset6_count23
	.align 4
	.global bitfield__write_var_BitField_offset6_count23
	.hidden	bitfield__write_var_BitField_offset6_count23
	.type	bitfield__write_var_BitField_offset6_count23, #function
	.proc	020
bitfield__write_var_BitField_offset6_count23:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1005
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(8387584), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1006
	 nop
.LL1005:
	sethi	%hi(44032), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1006:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-67108864), %g1
	or	%g1, 7, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(8387584), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 3, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset6_count23, .-bitfield__write_var_BitField_offset6_count23
	.align 4
	.global bitfield__read_var_BitField_offset6_count24
	.hidden	bitfield__read_var_BitField_offset6_count24
	.type	bitfield__read_var_BitField_offset6_count24, #function
	.proc	04
bitfield__read_var_BitField_offset6_count24:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 6, %g1
	srl	%g1, 8, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset6_count24, .-bitfield__read_var_BitField_offset6_count24
	.align 4
	.global bitfield__write_var_BitField_offset6_count24
	.hidden	bitfield__write_var_BitField_offset6_count24
	.type	bitfield__write_var_BitField_offset6_count24, #function
	.proc	020
bitfield__write_var_BitField_offset6_count24:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1011
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(16776192), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1012
	 nop
.LL1011:
	sethi	%hi(44032), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1012:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-67108864), %g1
	or	%g1, 3, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(16776192), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 2, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset6_count24, .-bitfield__write_var_BitField_offset6_count24
	.align 4
	.global bitfield__read_var_BitField_offset6_count25
	.hidden	bitfield__read_var_BitField_offset6_count25
	.type	bitfield__read_var_BitField_offset6_count25, #function
	.proc	04
bitfield__read_var_BitField_offset6_count25:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 6, %g1
	srl	%g1, 7, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset6_count25, .-bitfield__read_var_BitField_offset6_count25
	.align 4
	.global bitfield__write_var_BitField_offset6_count25
	.hidden	bitfield__write_var_BitField_offset6_count25
	.type	bitfield__write_var_BitField_offset6_count25, #function
	.proc	020
bitfield__write_var_BitField_offset6_count25:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1017
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(33553408), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1018
	 nop
.LL1017:
	sethi	%hi(45056), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1018:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-67108864), %g1
	or	%g1, 1, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(33553408), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	add	%g1, %g1, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset6_count25, .-bitfield__write_var_BitField_offset6_count25
	.align 4
	.global bitfield__read_var_BitField_offset6_count26
	.hidden	bitfield__read_var_BitField_offset6_count26
	.type	bitfield__read_var_BitField_offset6_count26, #function
	.proc	04
bitfield__read_var_BitField_offset6_count26:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(67107840), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset6_count26, .-bitfield__read_var_BitField_offset6_count26
	.align 4
	.global bitfield__write_var_BitField_offset6_count26
	.hidden	bitfield__write_var_BitField_offset6_count26
	.type	bitfield__write_var_BitField_offset6_count26, #function
	.proc	020
bitfield__write_var_BitField_offset6_count26:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1023
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(67107840), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1024
	 nop
.LL1023:
	sethi	%hi(45056), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1024:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-67108864), %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(67107840), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset6_count26, .-bitfield__write_var_BitField_offset6_count26
	.align 4
	.global bitfield__read_var_BitField_offset7_count1
	.hidden	bitfield__read_var_BitField_offset7_count1
	.type	bitfield__read_var_BitField_offset7_count1, #function
	.proc	04
bitfield__read_var_BitField_offset7_count1:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset7_count1, .-bitfield__read_var_BitField_offset7_count1
	.align 4
	.global bitfield__write_var_BitField_offset7_count1
	.hidden	bitfield__write_var_BitField_offset7_count1
	.type	bitfield__write_var_BitField_offset7_count1, #function
	.proc	020
bitfield__write_var_BitField_offset7_count1:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1029
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1
	ble	%icc, .LL1030
	 nop
.LL1029:
	sethi	%hi(45056), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1030:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -2, %g2
	ld	[%fp-24], %g1
	and	%g1, 1, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset7_count1, .-bitfield__write_var_BitField_offset7_count1
	.align 4
	.global bitfield__read_var_BitField_offset7_count2
	.hidden	bitfield__read_var_BitField_offset7_count2
	.type	bitfield__read_var_BitField_offset7_count2, #function
	.proc	04
bitfield__read_var_BitField_offset7_count2:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 23, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset7_count2, .-bitfield__read_var_BitField_offset7_count2
	.align 4
	.global bitfield__write_var_BitField_offset7_count2
	.hidden	bitfield__write_var_BitField_offset7_count2
	.type	bitfield__write_var_BitField_offset7_count2, #function
	.proc	020
bitfield__write_var_BitField_offset7_count2:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1035
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 3
	ble	%icc, .LL1036
	 nop
.LL1035:
	sethi	%hi(45056), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1036:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -385, %g2
	ld	[%fp-24], %g1
	and	%g1, 3, %g1
	sll	%g1, 7, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset7_count2, .-bitfield__write_var_BitField_offset7_count2
	.align 4
	.global bitfield__read_var_BitField_offset7_count3
	.hidden	bitfield__read_var_BitField_offset7_count3
	.type	bitfield__read_var_BitField_offset7_count3, #function
	.proc	04
bitfield__read_var_BitField_offset7_count3:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 23, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset7_count3, .-bitfield__read_var_BitField_offset7_count3
	.align 4
	.global bitfield__write_var_BitField_offset7_count3
	.hidden	bitfield__write_var_BitField_offset7_count3
	.type	bitfield__write_var_BitField_offset7_count3, #function
	.proc	020
bitfield__write_var_BitField_offset7_count3:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1041
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 7
	ble	%icc, .LL1042
	 nop
.LL1041:
	sethi	%hi(46080), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1042:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -449, %g2
	ld	[%fp-24], %g1
	and	%g1, 7, %g1
	sll	%g1, 6, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset7_count3, .-bitfield__write_var_BitField_offset7_count3
	.align 4
	.global bitfield__read_var_BitField_offset7_count4
	.hidden	bitfield__read_var_BitField_offset7_count4
	.type	bitfield__read_var_BitField_offset7_count4, #function
	.proc	04
bitfield__read_var_BitField_offset7_count4:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 23, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset7_count4, .-bitfield__read_var_BitField_offset7_count4
	.align 4
	.global bitfield__write_var_BitField_offset7_count4
	.hidden	bitfield__write_var_BitField_offset7_count4
	.type	bitfield__write_var_BitField_offset7_count4, #function
	.proc	020
bitfield__write_var_BitField_offset7_count4:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1047
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 15
	ble	%icc, .LL1048
	 nop
.LL1047:
	sethi	%hi(46080), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1048:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -481, %g2
	ld	[%fp-24], %g1
	and	%g1, 15, %g1
	sll	%g1, 5, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset7_count4, .-bitfield__write_var_BitField_offset7_count4
	.align 4
	.global bitfield__read_var_BitField_offset7_count5
	.hidden	bitfield__read_var_BitField_offset7_count5
	.type	bitfield__read_var_BitField_offset7_count5, #function
	.proc	04
bitfield__read_var_BitField_offset7_count5:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 23, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset7_count5, .-bitfield__read_var_BitField_offset7_count5
	.align 4
	.global bitfield__write_var_BitField_offset7_count5
	.hidden	bitfield__write_var_BitField_offset7_count5
	.type	bitfield__write_var_BitField_offset7_count5, #function
	.proc	020
bitfield__write_var_BitField_offset7_count5:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1053
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 31
	ble	%icc, .LL1054
	 nop
.LL1053:
	sethi	%hi(46080), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1054:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -497, %g2
	ld	[%fp-24], %g1
	and	%g1, 31, %g1
	sll	%g1, 4, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset7_count5, .-bitfield__write_var_BitField_offset7_count5
	.align 4
	.global bitfield__read_var_BitField_offset7_count6
	.hidden	bitfield__read_var_BitField_offset7_count6
	.type	bitfield__read_var_BitField_offset7_count6, #function
	.proc	04
bitfield__read_var_BitField_offset7_count6:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 23, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset7_count6, .-bitfield__read_var_BitField_offset7_count6
	.align 4
	.global bitfield__write_var_BitField_offset7_count6
	.hidden	bitfield__write_var_BitField_offset7_count6
	.type	bitfield__write_var_BitField_offset7_count6, #function
	.proc	020
bitfield__write_var_BitField_offset7_count6:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1059
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 63
	ble	%icc, .LL1060
	 nop
.LL1059:
	sethi	%hi(46080), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1060:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -505, %g2
	ld	[%fp-24], %g1
	and	%g1, 63, %g1
	sll	%g1, 3, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset7_count6, .-bitfield__write_var_BitField_offset7_count6
	.align 4
	.global bitfield__read_var_BitField_offset7_count7
	.hidden	bitfield__read_var_BitField_offset7_count7
	.type	bitfield__read_var_BitField_offset7_count7, #function
	.proc	04
bitfield__read_var_BitField_offset7_count7:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 23, %g1
	srl	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset7_count7, .-bitfield__read_var_BitField_offset7_count7
	.align 4
	.global bitfield__write_var_BitField_offset7_count7
	.hidden	bitfield__write_var_BitField_offset7_count7
	.type	bitfield__write_var_BitField_offset7_count7, #function
	.proc	020
bitfield__write_var_BitField_offset7_count7:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1065
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 127
	ble	%icc, .LL1066
	 nop
.LL1065:
	sethi	%hi(47104), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1066:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -509, %g2
	ld	[%fp-24], %g1
	and	%g1, 127, %g1
	sll	%g1, 2, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset7_count7, .-bitfield__write_var_BitField_offset7_count7
	.align 4
	.global bitfield__read_var_BitField_offset7_count8
	.hidden	bitfield__read_var_BitField_offset7_count8
	.type	bitfield__read_var_BitField_offset7_count8, #function
	.proc	04
bitfield__read_var_BitField_offset7_count8:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 23, %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset7_count8, .-bitfield__read_var_BitField_offset7_count8
	.align 4
	.global bitfield__write_var_BitField_offset7_count8
	.hidden	bitfield__write_var_BitField_offset7_count8
	.type	bitfield__write_var_BitField_offset7_count8, #function
	.proc	020
bitfield__write_var_BitField_offset7_count8:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1071
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 255
	ble	%icc, .LL1072
	 nop
.LL1071:
	sethi	%hi(47104), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1072:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -511, %g2
	ld	[%fp-24], %g1
	and	%g1, 255, %g1
	add	%g1, %g1, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset7_count8, .-bitfield__write_var_BitField_offset7_count8
	.align 4
	.global bitfield__read_var_BitField_offset7_count9
	.hidden	bitfield__read_var_BitField_offset7_count9
	.type	bitfield__read_var_BitField_offset7_count9, #function
	.proc	04
bitfield__read_var_BitField_offset7_count9:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, 511, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset7_count9, .-bitfield__read_var_BitField_offset7_count9
	.align 4
	.global bitfield__write_var_BitField_offset7_count9
	.hidden	bitfield__write_var_BitField_offset7_count9
	.type	bitfield__write_var_BitField_offset7_count9, #function
	.proc	020
bitfield__write_var_BitField_offset7_count9:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1077
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 511
	ble	%icc, .LL1078
	 nop
.LL1077:
	sethi	%hi(47104), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1078:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -512, %g2
	ld	[%fp-24], %g1
	and	%g1, 511, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset7_count9, .-bitfield__write_var_BitField_offset7_count9
	.align 4
	.global bitfield__read_var_BitField_offset7_count10
	.hidden	bitfield__read_var_BitField_offset7_count10
	.type	bitfield__read_var_BitField_offset7_count10, #function
	.proc	04
bitfield__read_var_BitField_offset7_count10:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 7, %g1
	srl	%g1, 22, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset7_count10, .-bitfield__read_var_BitField_offset7_count10
	.align 4
	.global bitfield__write_var_BitField_offset7_count10
	.hidden	bitfield__write_var_BitField_offset7_count10
	.type	bitfield__write_var_BitField_offset7_count10, #function
	.proc	020
bitfield__write_var_BitField_offset7_count10:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1083
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1023
	ble	%icc, .LL1084
	 nop
.LL1083:
	sethi	%hi(47104), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1084:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-33522688), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 1023, %g1
	sll	%g1, 15, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset7_count10, .-bitfield__write_var_BitField_offset7_count10
	.align 4
	.global bitfield__read_var_BitField_offset7_count11
	.hidden	bitfield__read_var_BitField_offset7_count11
	.type	bitfield__read_var_BitField_offset7_count11, #function
	.proc	04
bitfield__read_var_BitField_offset7_count11:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 7, %g1
	srl	%g1, 21, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset7_count11, .-bitfield__read_var_BitField_offset7_count11
	.align 4
	.global bitfield__write_var_BitField_offset7_count11
	.hidden	bitfield__write_var_BitField_offset7_count11
	.type	bitfield__write_var_BitField_offset7_count11, #function
	.proc	020
bitfield__write_var_BitField_offset7_count11:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1089
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 2047
	ble	%icc, .LL1090
	 nop
.LL1089:
	sethi	%hi(48128), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1090:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-33539072), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 2047, %g1
	sll	%g1, 14, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset7_count11, .-bitfield__write_var_BitField_offset7_count11
	.align 4
	.global bitfield__read_var_BitField_offset7_count12
	.hidden	bitfield__read_var_BitField_offset7_count12
	.type	bitfield__read_var_BitField_offset7_count12, #function
	.proc	04
bitfield__read_var_BitField_offset7_count12:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 7, %g1
	srl	%g1, 20, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset7_count12, .-bitfield__read_var_BitField_offset7_count12
	.align 4
	.global bitfield__write_var_BitField_offset7_count12
	.hidden	bitfield__write_var_BitField_offset7_count12
	.type	bitfield__write_var_BitField_offset7_count12, #function
	.proc	020
bitfield__write_var_BitField_offset7_count12:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1095
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 4095
	ble	%icc, .LL1096
	 nop
.LL1095:
	sethi	%hi(48128), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1096:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-33547264), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 4095, %g1
	sll	%g1, 13, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset7_count12, .-bitfield__write_var_BitField_offset7_count12
	.align 4
	.global bitfield__read_var_BitField_offset7_count13
	.hidden	bitfield__read_var_BitField_offset7_count13
	.type	bitfield__read_var_BitField_offset7_count13, #function
	.proc	04
bitfield__read_var_BitField_offset7_count13:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 7, %g1
	srl	%g1, 19, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset7_count13, .-bitfield__read_var_BitField_offset7_count13
	.align 4
	.global bitfield__write_var_BitField_offset7_count13
	.hidden	bitfield__write_var_BitField_offset7_count13
	.type	bitfield__write_var_BitField_offset7_count13, #function
	.proc	020
bitfield__write_var_BitField_offset7_count13:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1101
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(7168), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1102
	 nop
.LL1101:
	sethi	%hi(48128), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1102:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-33551360), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 12, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset7_count13, .-bitfield__write_var_BitField_offset7_count13
	.align 4
	.global bitfield__read_var_BitField_offset7_count14
	.hidden	bitfield__read_var_BitField_offset7_count14
	.type	bitfield__read_var_BitField_offset7_count14, #function
	.proc	04
bitfield__read_var_BitField_offset7_count14:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 7, %g1
	srl	%g1, 18, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset7_count14, .-bitfield__read_var_BitField_offset7_count14
	.align 4
	.global bitfield__write_var_BitField_offset7_count14
	.hidden	bitfield__write_var_BitField_offset7_count14
	.type	bitfield__write_var_BitField_offset7_count14, #function
	.proc	020
bitfield__write_var_BitField_offset7_count14:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1107
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(15360), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1108
	 nop
.LL1107:
	sethi	%hi(48128), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1108:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-33553408), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 11, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset7_count14, .-bitfield__write_var_BitField_offset7_count14
	.align 4
	.global bitfield__read_var_BitField_offset7_count15
	.hidden	bitfield__read_var_BitField_offset7_count15
	.type	bitfield__read_var_BitField_offset7_count15, #function
	.proc	04
bitfield__read_var_BitField_offset7_count15:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 7, %g1
	srl	%g1, 17, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset7_count15, .-bitfield__read_var_BitField_offset7_count15
	.align 4
	.global bitfield__write_var_BitField_offset7_count15
	.hidden	bitfield__write_var_BitField_offset7_count15
	.type	bitfield__write_var_BitField_offset7_count15, #function
	.proc	020
bitfield__write_var_BitField_offset7_count15:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1113
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(31744), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1114
	 nop
.LL1113:
	sethi	%hi(49152), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1114:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-33554432), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 10, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset7_count15, .-bitfield__write_var_BitField_offset7_count15
	.align 4
	.global bitfield__read_var_BitField_offset7_count16
	.hidden	bitfield__read_var_BitField_offset7_count16
	.type	bitfield__read_var_BitField_offset7_count16, #function
	.proc	04
bitfield__read_var_BitField_offset7_count16:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 7, %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset7_count16, .-bitfield__read_var_BitField_offset7_count16
	.align 4
	.global bitfield__write_var_BitField_offset7_count16
	.hidden	bitfield__write_var_BitField_offset7_count16
	.type	bitfield__write_var_BitField_offset7_count16, #function
	.proc	020
bitfield__write_var_BitField_offset7_count16:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1119
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(64512), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1120
	 nop
.LL1119:
	sethi	%hi(49152), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1120:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-33554432), %g1
	or	%g1, 511, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 9, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset7_count16, .-bitfield__write_var_BitField_offset7_count16
	.align 4
	.global bitfield__read_var_BitField_offset7_count17
	.hidden	bitfield__read_var_BitField_offset7_count17
	.type	bitfield__read_var_BitField_offset7_count17, #function
	.proc	04
bitfield__read_var_BitField_offset7_count17:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 7, %g1
	srl	%g1, 15, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset7_count17, .-bitfield__read_var_BitField_offset7_count17
	.align 4
	.global bitfield__write_var_BitField_offset7_count17
	.hidden	bitfield__write_var_BitField_offset7_count17
	.type	bitfield__write_var_BitField_offset7_count17, #function
	.proc	020
bitfield__write_var_BitField_offset7_count17:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1125
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(130048), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1126
	 nop
.LL1125:
	sethi	%hi(49152), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1126:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-33554432), %g1
	or	%g1, 255, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(130048), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 8, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset7_count17, .-bitfield__write_var_BitField_offset7_count17
	.align 4
	.global bitfield__read_var_BitField_offset7_count18
	.hidden	bitfield__read_var_BitField_offset7_count18
	.type	bitfield__read_var_BitField_offset7_count18, #function
	.proc	04
bitfield__read_var_BitField_offset7_count18:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 7, %g1
	srl	%g1, 14, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset7_count18, .-bitfield__read_var_BitField_offset7_count18
	.align 4
	.global bitfield__write_var_BitField_offset7_count18
	.hidden	bitfield__write_var_BitField_offset7_count18
	.type	bitfield__write_var_BitField_offset7_count18, #function
	.proc	020
bitfield__write_var_BitField_offset7_count18:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1131
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(261120), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1132
	 nop
.LL1131:
	sethi	%hi(49152), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1132:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-33554432), %g1
	or	%g1, 127, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(261120), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 7, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset7_count18, .-bitfield__write_var_BitField_offset7_count18
	.align 4
	.global bitfield__read_var_BitField_offset7_count19
	.hidden	bitfield__read_var_BitField_offset7_count19
	.type	bitfield__read_var_BitField_offset7_count19, #function
	.proc	04
bitfield__read_var_BitField_offset7_count19:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 7, %g1
	srl	%g1, 13, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset7_count19, .-bitfield__read_var_BitField_offset7_count19
	.align 4
	.global bitfield__write_var_BitField_offset7_count19
	.hidden	bitfield__write_var_BitField_offset7_count19
	.type	bitfield__write_var_BitField_offset7_count19, #function
	.proc	020
bitfield__write_var_BitField_offset7_count19:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1137
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(523264), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1138
	 nop
.LL1137:
	sethi	%hi(50176), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1138:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-33554432), %g1
	or	%g1, 63, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(523264), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 6, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset7_count19, .-bitfield__write_var_BitField_offset7_count19
	.align 4
	.global bitfield__read_var_BitField_offset7_count20
	.hidden	bitfield__read_var_BitField_offset7_count20
	.type	bitfield__read_var_BitField_offset7_count20, #function
	.proc	04
bitfield__read_var_BitField_offset7_count20:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 7, %g1
	srl	%g1, 12, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset7_count20, .-bitfield__read_var_BitField_offset7_count20
	.align 4
	.global bitfield__write_var_BitField_offset7_count20
	.hidden	bitfield__write_var_BitField_offset7_count20
	.type	bitfield__write_var_BitField_offset7_count20, #function
	.proc	020
bitfield__write_var_BitField_offset7_count20:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1143
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(1047552), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1144
	 nop
.LL1143:
	sethi	%hi(50176), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1144:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-33554432), %g1
	or	%g1, 31, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(1047552), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 5, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset7_count20, .-bitfield__write_var_BitField_offset7_count20
	.align 4
	.global bitfield__read_var_BitField_offset7_count21
	.hidden	bitfield__read_var_BitField_offset7_count21
	.type	bitfield__read_var_BitField_offset7_count21, #function
	.proc	04
bitfield__read_var_BitField_offset7_count21:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 7, %g1
	srl	%g1, 11, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset7_count21, .-bitfield__read_var_BitField_offset7_count21
	.align 4
	.global bitfield__write_var_BitField_offset7_count21
	.hidden	bitfield__write_var_BitField_offset7_count21
	.type	bitfield__write_var_BitField_offset7_count21, #function
	.proc	020
bitfield__write_var_BitField_offset7_count21:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1149
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(2096128), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1150
	 nop
.LL1149:
	sethi	%hi(50176), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1150:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-33554432), %g1
	or	%g1, 15, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(2096128), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 4, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset7_count21, .-bitfield__write_var_BitField_offset7_count21
	.align 4
	.global bitfield__read_var_BitField_offset7_count22
	.hidden	bitfield__read_var_BitField_offset7_count22
	.type	bitfield__read_var_BitField_offset7_count22, #function
	.proc	04
bitfield__read_var_BitField_offset7_count22:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 7, %g1
	srl	%g1, 10, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset7_count22, .-bitfield__read_var_BitField_offset7_count22
	.align 4
	.global bitfield__write_var_BitField_offset7_count22
	.hidden	bitfield__write_var_BitField_offset7_count22
	.type	bitfield__write_var_BitField_offset7_count22, #function
	.proc	020
bitfield__write_var_BitField_offset7_count22:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1155
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(4193280), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1156
	 nop
.LL1155:
	sethi	%hi(50176), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1156:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-33554432), %g1
	or	%g1, 7, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(4193280), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 3, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset7_count22, .-bitfield__write_var_BitField_offset7_count22
	.align 4
	.global bitfield__read_var_BitField_offset7_count23
	.hidden	bitfield__read_var_BitField_offset7_count23
	.type	bitfield__read_var_BitField_offset7_count23, #function
	.proc	04
bitfield__read_var_BitField_offset7_count23:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 7, %g1
	srl	%g1, 9, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset7_count23, .-bitfield__read_var_BitField_offset7_count23
	.align 4
	.global bitfield__write_var_BitField_offset7_count23
	.hidden	bitfield__write_var_BitField_offset7_count23
	.type	bitfield__write_var_BitField_offset7_count23, #function
	.proc	020
bitfield__write_var_BitField_offset7_count23:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1161
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(8387584), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1162
	 nop
.LL1161:
	sethi	%hi(51200), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1162:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-33554432), %g1
	or	%g1, 3, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(8387584), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 2, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset7_count23, .-bitfield__write_var_BitField_offset7_count23
	.align 4
	.global bitfield__read_var_BitField_offset7_count24
	.hidden	bitfield__read_var_BitField_offset7_count24
	.type	bitfield__read_var_BitField_offset7_count24, #function
	.proc	04
bitfield__read_var_BitField_offset7_count24:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 7, %g1
	srl	%g1, 8, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset7_count24, .-bitfield__read_var_BitField_offset7_count24
	.align 4
	.global bitfield__write_var_BitField_offset7_count24
	.hidden	bitfield__write_var_BitField_offset7_count24
	.type	bitfield__write_var_BitField_offset7_count24, #function
	.proc	020
bitfield__write_var_BitField_offset7_count24:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1167
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(16776192), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1168
	 nop
.LL1167:
	sethi	%hi(51200), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1168:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-33554432), %g1
	or	%g1, 1, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(16776192), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	add	%g1, %g1, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset7_count24, .-bitfield__write_var_BitField_offset7_count24
	.align 4
	.global bitfield__read_var_BitField_offset7_count25
	.hidden	bitfield__read_var_BitField_offset7_count25
	.type	bitfield__read_var_BitField_offset7_count25, #function
	.proc	04
bitfield__read_var_BitField_offset7_count25:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(33553408), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset7_count25, .-bitfield__read_var_BitField_offset7_count25
	.align 4
	.global bitfield__write_var_BitField_offset7_count25
	.hidden	bitfield__write_var_BitField_offset7_count25
	.type	bitfield__write_var_BitField_offset7_count25, #function
	.proc	020
bitfield__write_var_BitField_offset7_count25:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1173
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(33553408), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1174
	 nop
.LL1173:
	sethi	%hi(51200), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1174:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-33554432), %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(33553408), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset7_count25, .-bitfield__write_var_BitField_offset7_count25
	.align 4
	.global bitfield__read_var_BitField_offset8_count1
	.hidden	bitfield__read_var_BitField_offset8_count1
	.type	bitfield__read_var_BitField_offset8_count1, #function
	.proc	04
bitfield__read_var_BitField_offset8_count1:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 24, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset8_count1, .-bitfield__read_var_BitField_offset8_count1
	.align 4
	.global bitfield__write_var_BitField_offset8_count1
	.hidden	bitfield__write_var_BitField_offset8_count1
	.type	bitfield__write_var_BitField_offset8_count1, #function
	.proc	020
bitfield__write_var_BitField_offset8_count1:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1179
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1
	ble	%icc, .LL1180
	 nop
.LL1179:
	sethi	%hi(51200), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1180:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 1, %g3
	ld	[%fp-20], %g1
	add	%g1, 1, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -129, %g2
	ld	[%fp-24], %g1
	sll	%g1, 7, %g1
	and	%g1, 255, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset8_count1, .-bitfield__write_var_BitField_offset8_count1
	.align 4
	.global bitfield__read_var_BitField_offset8_count2
	.hidden	bitfield__read_var_BitField_offset8_count2
	.type	bitfield__read_var_BitField_offset8_count2, #function
	.proc	04
bitfield__read_var_BitField_offset8_count2:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 24, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset8_count2, .-bitfield__read_var_BitField_offset8_count2
	.align 4
	.global bitfield__write_var_BitField_offset8_count2
	.hidden	bitfield__write_var_BitField_offset8_count2
	.type	bitfield__write_var_BitField_offset8_count2, #function
	.proc	020
bitfield__write_var_BitField_offset8_count2:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1185
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 3
	ble	%icc, .LL1186
	 nop
.LL1185:
	sethi	%hi(52224), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1186:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 1, %g3
	ld	[%fp-20], %g1
	add	%g1, 1, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -193, %g2
	ld	[%fp-24], %g1
	sll	%g1, 6, %g1
	and	%g1, 255, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset8_count2, .-bitfield__write_var_BitField_offset8_count2
	.align 4
	.global bitfield__read_var_BitField_offset8_count3
	.hidden	bitfield__read_var_BitField_offset8_count3
	.type	bitfield__read_var_BitField_offset8_count3, #function
	.proc	04
bitfield__read_var_BitField_offset8_count3:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 24, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset8_count3, .-bitfield__read_var_BitField_offset8_count3
	.align 4
	.global bitfield__write_var_BitField_offset8_count3
	.hidden	bitfield__write_var_BitField_offset8_count3
	.type	bitfield__write_var_BitField_offset8_count3, #function
	.proc	020
bitfield__write_var_BitField_offset8_count3:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1191
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 7
	ble	%icc, .LL1192
	 nop
.LL1191:
	sethi	%hi(52224), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1192:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 1, %g3
	ld	[%fp-20], %g1
	add	%g1, 1, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -225, %g2
	ld	[%fp-24], %g1
	sll	%g1, 5, %g1
	and	%g1, 255, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset8_count3, .-bitfield__write_var_BitField_offset8_count3
	.align 4
	.global bitfield__read_var_BitField_offset8_count4
	.hidden	bitfield__read_var_BitField_offset8_count4
	.type	bitfield__read_var_BitField_offset8_count4, #function
	.proc	04
bitfield__read_var_BitField_offset8_count4:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 24, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset8_count4, .-bitfield__read_var_BitField_offset8_count4
	.align 4
	.global bitfield__write_var_BitField_offset8_count4
	.hidden	bitfield__write_var_BitField_offset8_count4
	.type	bitfield__write_var_BitField_offset8_count4, #function
	.proc	020
bitfield__write_var_BitField_offset8_count4:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1197
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 15
	ble	%icc, .LL1198
	 nop
.LL1197:
	sethi	%hi(52224), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1198:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 1, %g3
	ld	[%fp-20], %g1
	add	%g1, 1, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -241, %g2
	ld	[%fp-24], %g1
	sll	%g1, 4, %g1
	and	%g1, 255, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset8_count4, .-bitfield__write_var_BitField_offset8_count4
	.align 4
	.global bitfield__read_var_BitField_offset8_count5
	.hidden	bitfield__read_var_BitField_offset8_count5
	.type	bitfield__read_var_BitField_offset8_count5, #function
	.proc	04
bitfield__read_var_BitField_offset8_count5:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 24, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset8_count5, .-bitfield__read_var_BitField_offset8_count5
	.align 4
	.global bitfield__write_var_BitField_offset8_count5
	.hidden	bitfield__write_var_BitField_offset8_count5
	.type	bitfield__write_var_BitField_offset8_count5, #function
	.proc	020
bitfield__write_var_BitField_offset8_count5:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1203
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 31
	ble	%icc, .LL1204
	 nop
.LL1203:
	sethi	%hi(52224), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1204:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 1, %g3
	ld	[%fp-20], %g1
	add	%g1, 1, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -249, %g2
	ld	[%fp-24], %g1
	sll	%g1, 3, %g1
	and	%g1, 255, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset8_count5, .-bitfield__write_var_BitField_offset8_count5
	.align 4
	.global bitfield__read_var_BitField_offset8_count6
	.hidden	bitfield__read_var_BitField_offset8_count6
	.type	bitfield__read_var_BitField_offset8_count6, #function
	.proc	04
bitfield__read_var_BitField_offset8_count6:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 24, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset8_count6, .-bitfield__read_var_BitField_offset8_count6
	.align 4
	.global bitfield__write_var_BitField_offset8_count6
	.hidden	bitfield__write_var_BitField_offset8_count6
	.type	bitfield__write_var_BitField_offset8_count6, #function
	.proc	020
bitfield__write_var_BitField_offset8_count6:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1209
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 63
	ble	%icc, .LL1210
	 nop
.LL1209:
	sethi	%hi(53248), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1210:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 1, %g3
	ld	[%fp-20], %g1
	add	%g1, 1, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -253, %g2
	ld	[%fp-24], %g1
	sll	%g1, 2, %g1
	and	%g1, 255, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset8_count6, .-bitfield__write_var_BitField_offset8_count6
	.align 4
	.global bitfield__read_var_BitField_offset8_count7
	.hidden	bitfield__read_var_BitField_offset8_count7
	.type	bitfield__read_var_BitField_offset8_count7, #function
	.proc	04
bitfield__read_var_BitField_offset8_count7:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 24, %g1
	srl	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset8_count7, .-bitfield__read_var_BitField_offset8_count7
	.align 4
	.global bitfield__write_var_BitField_offset8_count7
	.hidden	bitfield__write_var_BitField_offset8_count7
	.type	bitfield__write_var_BitField_offset8_count7, #function
	.proc	020
bitfield__write_var_BitField_offset8_count7:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1215
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 127
	ble	%icc, .LL1216
	 nop
.LL1215:
	sethi	%hi(53248), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1216:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 1, %g3
	ld	[%fp-20], %g1
	add	%g1, 1, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -255, %g2
	ld	[%fp-24], %g1
	add	%g1, %g1, %g1
	and	%g1, 255, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset8_count7, .-bitfield__write_var_BitField_offset8_count7
	.align 4
	.global bitfield__read_var_BitField_offset8_count8
	.hidden	bitfield__read_var_BitField_offset8_count8
	.type	bitfield__read_var_BitField_offset8_count8, #function
	.proc	04
bitfield__read_var_BitField_offset8_count8:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, 0xff, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset8_count8, .-bitfield__read_var_BitField_offset8_count8
	.align 4
	.global bitfield__write_var_BitField_offset8_count8
	.hidden	bitfield__write_var_BitField_offset8_count8
	.type	bitfield__write_var_BitField_offset8_count8, #function
	.proc	020
bitfield__write_var_BitField_offset8_count8:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	cmp	%g1, 0
	bl	%icc, .LL1221
	 nop
	ld	[%fp-20], %g1
	cmp	%g1, 255
	ble	%icc, .LL1222
	 nop
.LL1221:
	sethi	%hi(53248), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1222:
	ld	[%fp+68], %g1
	add	%g1, 1, %g2
	ld	[%fp-20], %g1
	stb	%g1, [%g2]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset8_count8, .-bitfield__write_var_BitField_offset8_count8
	.align 4
	.global bitfield__read_var_BitField_offset8_count9
	.hidden	bitfield__read_var_BitField_offset8_count9
	.type	bitfield__read_var_BitField_offset8_count9, #function
	.proc	04
bitfield__read_var_BitField_offset8_count9:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 8, %g1
	srl	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset8_count9, .-bitfield__read_var_BitField_offset8_count9
	.align 4
	.global bitfield__write_var_BitField_offset8_count9
	.hidden	bitfield__write_var_BitField_offset8_count9
	.type	bitfield__write_var_BitField_offset8_count9, #function
	.proc	020
bitfield__write_var_BitField_offset8_count9:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1227
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 511
	ble	%icc, .LL1228
	 nop
.LL1227:
	sethi	%hi(53248), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1228:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-16745472), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 511, %g1
	sll	%g1, 15, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset8_count9, .-bitfield__write_var_BitField_offset8_count9
	.align 4
	.global bitfield__read_var_BitField_offset8_count10
	.hidden	bitfield__read_var_BitField_offset8_count10
	.type	bitfield__read_var_BitField_offset8_count10, #function
	.proc	04
bitfield__read_var_BitField_offset8_count10:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 8, %g1
	srl	%g1, 22, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset8_count10, .-bitfield__read_var_BitField_offset8_count10
	.align 4
	.global bitfield__write_var_BitField_offset8_count10
	.hidden	bitfield__write_var_BitField_offset8_count10
	.type	bitfield__write_var_BitField_offset8_count10, #function
	.proc	020
bitfield__write_var_BitField_offset8_count10:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1233
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1023
	ble	%icc, .LL1234
	 nop
.LL1233:
	sethi	%hi(54272), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1234:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-16761856), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 1023, %g1
	sll	%g1, 14, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset8_count10, .-bitfield__write_var_BitField_offset8_count10
	.align 4
	.global bitfield__read_var_BitField_offset8_count11
	.hidden	bitfield__read_var_BitField_offset8_count11
	.type	bitfield__read_var_BitField_offset8_count11, #function
	.proc	04
bitfield__read_var_BitField_offset8_count11:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 8, %g1
	srl	%g1, 21, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset8_count11, .-bitfield__read_var_BitField_offset8_count11
	.align 4
	.global bitfield__write_var_BitField_offset8_count11
	.hidden	bitfield__write_var_BitField_offset8_count11
	.type	bitfield__write_var_BitField_offset8_count11, #function
	.proc	020
bitfield__write_var_BitField_offset8_count11:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1239
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 2047
	ble	%icc, .LL1240
	 nop
.LL1239:
	sethi	%hi(54272), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1240:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-16770048), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 2047, %g1
	sll	%g1, 13, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset8_count11, .-bitfield__write_var_BitField_offset8_count11
	.align 4
	.global bitfield__read_var_BitField_offset8_count12
	.hidden	bitfield__read_var_BitField_offset8_count12
	.type	bitfield__read_var_BitField_offset8_count12, #function
	.proc	04
bitfield__read_var_BitField_offset8_count12:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 8, %g1
	srl	%g1, 20, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset8_count12, .-bitfield__read_var_BitField_offset8_count12
	.align 4
	.global bitfield__write_var_BitField_offset8_count12
	.hidden	bitfield__write_var_BitField_offset8_count12
	.type	bitfield__write_var_BitField_offset8_count12, #function
	.proc	020
bitfield__write_var_BitField_offset8_count12:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1245
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 4095
	ble	%icc, .LL1246
	 nop
.LL1245:
	sethi	%hi(54272), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1246:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-16774144), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 4095, %g1
	sll	%g1, 12, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset8_count12, .-bitfield__write_var_BitField_offset8_count12
	.align 4
	.global bitfield__read_var_BitField_offset8_count13
	.hidden	bitfield__read_var_BitField_offset8_count13
	.type	bitfield__read_var_BitField_offset8_count13, #function
	.proc	04
bitfield__read_var_BitField_offset8_count13:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 8, %g1
	srl	%g1, 19, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset8_count13, .-bitfield__read_var_BitField_offset8_count13
	.align 4
	.global bitfield__write_var_BitField_offset8_count13
	.hidden	bitfield__write_var_BitField_offset8_count13
	.type	bitfield__write_var_BitField_offset8_count13, #function
	.proc	020
bitfield__write_var_BitField_offset8_count13:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1251
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(7168), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1252
	 nop
.LL1251:
	sethi	%hi(54272), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1252:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-16776192), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 11, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset8_count13, .-bitfield__write_var_BitField_offset8_count13
	.align 4
	.global bitfield__read_var_BitField_offset8_count14
	.hidden	bitfield__read_var_BitField_offset8_count14
	.type	bitfield__read_var_BitField_offset8_count14, #function
	.proc	04
bitfield__read_var_BitField_offset8_count14:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 8, %g1
	srl	%g1, 18, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset8_count14, .-bitfield__read_var_BitField_offset8_count14
	.align 4
	.global bitfield__write_var_BitField_offset8_count14
	.hidden	bitfield__write_var_BitField_offset8_count14
	.type	bitfield__write_var_BitField_offset8_count14, #function
	.proc	020
bitfield__write_var_BitField_offset8_count14:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1257
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(15360), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1258
	 nop
.LL1257:
	sethi	%hi(55296), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1258:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-16777216), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 10, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset8_count14, .-bitfield__write_var_BitField_offset8_count14
	.align 4
	.global bitfield__read_var_BitField_offset8_count15
	.hidden	bitfield__read_var_BitField_offset8_count15
	.type	bitfield__read_var_BitField_offset8_count15, #function
	.proc	04
bitfield__read_var_BitField_offset8_count15:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 8, %g1
	srl	%g1, 17, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset8_count15, .-bitfield__read_var_BitField_offset8_count15
	.align 4
	.global bitfield__write_var_BitField_offset8_count15
	.hidden	bitfield__write_var_BitField_offset8_count15
	.type	bitfield__write_var_BitField_offset8_count15, #function
	.proc	020
bitfield__write_var_BitField_offset8_count15:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1263
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(31744), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1264
	 nop
.LL1263:
	sethi	%hi(55296), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1264:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-16777216), %g1
	or	%g1, 511, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 9, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset8_count15, .-bitfield__write_var_BitField_offset8_count15
	.align 4
	.global bitfield__read_var_BitField_offset8_count16
	.hidden	bitfield__read_var_BitField_offset8_count16
	.type	bitfield__read_var_BitField_offset8_count16, #function
	.proc	04
bitfield__read_var_BitField_offset8_count16:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 8, %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset8_count16, .-bitfield__read_var_BitField_offset8_count16
	.align 4
	.global bitfield__write_var_BitField_offset8_count16
	.hidden	bitfield__write_var_BitField_offset8_count16
	.type	bitfield__write_var_BitField_offset8_count16, #function
	.proc	020
bitfield__write_var_BitField_offset8_count16:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1269
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(64512), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1270
	 nop
.LL1269:
	sethi	%hi(55296), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1270:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-16777216), %g1
	or	%g1, 255, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 8, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset8_count16, .-bitfield__write_var_BitField_offset8_count16
	.align 4
	.global bitfield__read_var_BitField_offset8_count17
	.hidden	bitfield__read_var_BitField_offset8_count17
	.type	bitfield__read_var_BitField_offset8_count17, #function
	.proc	04
bitfield__read_var_BitField_offset8_count17:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 8, %g1
	srl	%g1, 15, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset8_count17, .-bitfield__read_var_BitField_offset8_count17
	.align 4
	.global bitfield__write_var_BitField_offset8_count17
	.hidden	bitfield__write_var_BitField_offset8_count17
	.type	bitfield__write_var_BitField_offset8_count17, #function
	.proc	020
bitfield__write_var_BitField_offset8_count17:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1275
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(130048), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1276
	 nop
.LL1275:
	sethi	%hi(55296), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1276:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-16777216), %g1
	or	%g1, 127, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(130048), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 7, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset8_count17, .-bitfield__write_var_BitField_offset8_count17
	.align 4
	.global bitfield__read_var_BitField_offset8_count18
	.hidden	bitfield__read_var_BitField_offset8_count18
	.type	bitfield__read_var_BitField_offset8_count18, #function
	.proc	04
bitfield__read_var_BitField_offset8_count18:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 8, %g1
	srl	%g1, 14, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset8_count18, .-bitfield__read_var_BitField_offset8_count18
	.align 4
	.global bitfield__write_var_BitField_offset8_count18
	.hidden	bitfield__write_var_BitField_offset8_count18
	.type	bitfield__write_var_BitField_offset8_count18, #function
	.proc	020
bitfield__write_var_BitField_offset8_count18:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1281
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(261120), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1282
	 nop
.LL1281:
	sethi	%hi(56320), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1282:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-16777216), %g1
	or	%g1, 63, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(261120), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 6, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset8_count18, .-bitfield__write_var_BitField_offset8_count18
	.align 4
	.global bitfield__read_var_BitField_offset8_count19
	.hidden	bitfield__read_var_BitField_offset8_count19
	.type	bitfield__read_var_BitField_offset8_count19, #function
	.proc	04
bitfield__read_var_BitField_offset8_count19:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 8, %g1
	srl	%g1, 13, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset8_count19, .-bitfield__read_var_BitField_offset8_count19
	.align 4
	.global bitfield__write_var_BitField_offset8_count19
	.hidden	bitfield__write_var_BitField_offset8_count19
	.type	bitfield__write_var_BitField_offset8_count19, #function
	.proc	020
bitfield__write_var_BitField_offset8_count19:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1287
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(523264), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1288
	 nop
.LL1287:
	sethi	%hi(56320), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1288:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-16777216), %g1
	or	%g1, 31, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(523264), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 5, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset8_count19, .-bitfield__write_var_BitField_offset8_count19
	.align 4
	.global bitfield__read_var_BitField_offset8_count20
	.hidden	bitfield__read_var_BitField_offset8_count20
	.type	bitfield__read_var_BitField_offset8_count20, #function
	.proc	04
bitfield__read_var_BitField_offset8_count20:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 8, %g1
	srl	%g1, 12, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset8_count20, .-bitfield__read_var_BitField_offset8_count20
	.align 4
	.global bitfield__write_var_BitField_offset8_count20
	.hidden	bitfield__write_var_BitField_offset8_count20
	.type	bitfield__write_var_BitField_offset8_count20, #function
	.proc	020
bitfield__write_var_BitField_offset8_count20:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1293
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(1047552), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1294
	 nop
.LL1293:
	sethi	%hi(56320), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1294:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-16777216), %g1
	or	%g1, 15, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(1047552), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 4, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset8_count20, .-bitfield__write_var_BitField_offset8_count20
	.align 4
	.global bitfield__read_var_BitField_offset8_count21
	.hidden	bitfield__read_var_BitField_offset8_count21
	.type	bitfield__read_var_BitField_offset8_count21, #function
	.proc	04
bitfield__read_var_BitField_offset8_count21:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 8, %g1
	srl	%g1, 11, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset8_count21, .-bitfield__read_var_BitField_offset8_count21
	.align 4
	.global bitfield__write_var_BitField_offset8_count21
	.hidden	bitfield__write_var_BitField_offset8_count21
	.type	bitfield__write_var_BitField_offset8_count21, #function
	.proc	020
bitfield__write_var_BitField_offset8_count21:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1299
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(2096128), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1300
	 nop
.LL1299:
	sethi	%hi(56320), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1300:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-16777216), %g1
	or	%g1, 7, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(2096128), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 3, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset8_count21, .-bitfield__write_var_BitField_offset8_count21
	.align 4
	.global bitfield__read_var_BitField_offset8_count22
	.hidden	bitfield__read_var_BitField_offset8_count22
	.type	bitfield__read_var_BitField_offset8_count22, #function
	.proc	04
bitfield__read_var_BitField_offset8_count22:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 8, %g1
	srl	%g1, 10, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset8_count22, .-bitfield__read_var_BitField_offset8_count22
	.align 4
	.global bitfield__write_var_BitField_offset8_count22
	.hidden	bitfield__write_var_BitField_offset8_count22
	.type	bitfield__write_var_BitField_offset8_count22, #function
	.proc	020
bitfield__write_var_BitField_offset8_count22:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1305
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(4193280), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1306
	 nop
.LL1305:
	sethi	%hi(57344), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1306:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-16777216), %g1
	or	%g1, 3, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(4193280), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 2, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset8_count22, .-bitfield__write_var_BitField_offset8_count22
	.align 4
	.global bitfield__read_var_BitField_offset8_count23
	.hidden	bitfield__read_var_BitField_offset8_count23
	.type	bitfield__read_var_BitField_offset8_count23, #function
	.proc	04
bitfield__read_var_BitField_offset8_count23:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 8, %g1
	srl	%g1, 9, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset8_count23, .-bitfield__read_var_BitField_offset8_count23
	.align 4
	.global bitfield__write_var_BitField_offset8_count23
	.hidden	bitfield__write_var_BitField_offset8_count23
	.type	bitfield__write_var_BitField_offset8_count23, #function
	.proc	020
bitfield__write_var_BitField_offset8_count23:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1311
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(8387584), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1312
	 nop
.LL1311:
	sethi	%hi(57344), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1312:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-16777216), %g1
	or	%g1, 1, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(8387584), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	add	%g1, %g1, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset8_count23, .-bitfield__write_var_BitField_offset8_count23
	.align 4
	.global bitfield__read_var_BitField_offset8_count24
	.hidden	bitfield__read_var_BitField_offset8_count24
	.type	bitfield__read_var_BitField_offset8_count24, #function
	.proc	04
bitfield__read_var_BitField_offset8_count24:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(16776192), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset8_count24, .-bitfield__read_var_BitField_offset8_count24
	.align 4
	.global bitfield__write_var_BitField_offset8_count24
	.hidden	bitfield__write_var_BitField_offset8_count24
	.type	bitfield__write_var_BitField_offset8_count24, #function
	.proc	020
bitfield__write_var_BitField_offset8_count24:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1317
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(16776192), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1318
	 nop
.LL1317:
	sethi	%hi(57344), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1318:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-16777216), %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(16776192), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset8_count24, .-bitfield__write_var_BitField_offset8_count24
	.align 4
	.global bitfield__read_var_BitField_offset9_count1
	.hidden	bitfield__read_var_BitField_offset9_count1
	.type	bitfield__read_var_BitField_offset9_count1, #function
	.proc	04
bitfield__read_var_BitField_offset9_count1:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset9_count1, .-bitfield__read_var_BitField_offset9_count1
	.align 4
	.global bitfield__write_var_BitField_offset9_count1
	.hidden	bitfield__write_var_BitField_offset9_count1
	.type	bitfield__write_var_BitField_offset9_count1, #function
	.proc	020
bitfield__write_var_BitField_offset9_count1:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1323
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1
	ble	%icc, .LL1324
	 nop
.LL1323:
	sethi	%hi(57344), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1324:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 1, %g3
	ld	[%fp-20], %g1
	add	%g1, 1, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -65, %g2
	ld	[%fp-24], %g1
	and	%g1, 1, %g1
	sll	%g1, 6, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset9_count1, .-bitfield__write_var_BitField_offset9_count1
	.align 4
	.global bitfield__read_var_BitField_offset9_count2
	.hidden	bitfield__read_var_BitField_offset9_count2
	.type	bitfield__read_var_BitField_offset9_count2, #function
	.proc	04
bitfield__read_var_BitField_offset9_count2:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset9_count2, .-bitfield__read_var_BitField_offset9_count2
	.align 4
	.global bitfield__write_var_BitField_offset9_count2
	.hidden	bitfield__write_var_BitField_offset9_count2
	.type	bitfield__write_var_BitField_offset9_count2, #function
	.proc	020
bitfield__write_var_BitField_offset9_count2:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1329
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 3
	ble	%icc, .LL1330
	 nop
.LL1329:
	sethi	%hi(58368), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1330:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 1, %g3
	ld	[%fp-20], %g1
	add	%g1, 1, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -97, %g2
	ld	[%fp-24], %g1
	and	%g1, 3, %g1
	sll	%g1, 5, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset9_count2, .-bitfield__write_var_BitField_offset9_count2
	.align 4
	.global bitfield__read_var_BitField_offset9_count3
	.hidden	bitfield__read_var_BitField_offset9_count3
	.type	bitfield__read_var_BitField_offset9_count3, #function
	.proc	04
bitfield__read_var_BitField_offset9_count3:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset9_count3, .-bitfield__read_var_BitField_offset9_count3
	.align 4
	.global bitfield__write_var_BitField_offset9_count3
	.hidden	bitfield__write_var_BitField_offset9_count3
	.type	bitfield__write_var_BitField_offset9_count3, #function
	.proc	020
bitfield__write_var_BitField_offset9_count3:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1335
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 7
	ble	%icc, .LL1336
	 nop
.LL1335:
	sethi	%hi(58368), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1336:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 1, %g3
	ld	[%fp-20], %g1
	add	%g1, 1, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -113, %g2
	ld	[%fp-24], %g1
	and	%g1, 7, %g1
	sll	%g1, 4, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset9_count3, .-bitfield__write_var_BitField_offset9_count3
	.align 4
	.global bitfield__read_var_BitField_offset9_count4
	.hidden	bitfield__read_var_BitField_offset9_count4
	.type	bitfield__read_var_BitField_offset9_count4, #function
	.proc	04
bitfield__read_var_BitField_offset9_count4:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset9_count4, .-bitfield__read_var_BitField_offset9_count4
	.align 4
	.global bitfield__write_var_BitField_offset9_count4
	.hidden	bitfield__write_var_BitField_offset9_count4
	.type	bitfield__write_var_BitField_offset9_count4, #function
	.proc	020
bitfield__write_var_BitField_offset9_count4:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1341
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 15
	ble	%icc, .LL1342
	 nop
.LL1341:
	sethi	%hi(58368), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1342:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 1, %g3
	ld	[%fp-20], %g1
	add	%g1, 1, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -121, %g2
	ld	[%fp-24], %g1
	and	%g1, 15, %g1
	sll	%g1, 3, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset9_count4, .-bitfield__write_var_BitField_offset9_count4
	.align 4
	.global bitfield__read_var_BitField_offset9_count5
	.hidden	bitfield__read_var_BitField_offset9_count5
	.type	bitfield__read_var_BitField_offset9_count5, #function
	.proc	04
bitfield__read_var_BitField_offset9_count5:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset9_count5, .-bitfield__read_var_BitField_offset9_count5
	.align 4
	.global bitfield__write_var_BitField_offset9_count5
	.hidden	bitfield__write_var_BitField_offset9_count5
	.type	bitfield__write_var_BitField_offset9_count5, #function
	.proc	020
bitfield__write_var_BitField_offset9_count5:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1347
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 31
	ble	%icc, .LL1348
	 nop
.LL1347:
	sethi	%hi(58368), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1348:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 1, %g3
	ld	[%fp-20], %g1
	add	%g1, 1, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -125, %g2
	ld	[%fp-24], %g1
	and	%g1, 31, %g1
	sll	%g1, 2, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset9_count5, .-bitfield__write_var_BitField_offset9_count5
	.align 4
	.global bitfield__read_var_BitField_offset9_count6
	.hidden	bitfield__read_var_BitField_offset9_count6
	.type	bitfield__read_var_BitField_offset9_count6, #function
	.proc	04
bitfield__read_var_BitField_offset9_count6:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset9_count6, .-bitfield__read_var_BitField_offset9_count6
	.align 4
	.global bitfield__write_var_BitField_offset9_count6
	.hidden	bitfield__write_var_BitField_offset9_count6
	.type	bitfield__write_var_BitField_offset9_count6, #function
	.proc	020
bitfield__write_var_BitField_offset9_count6:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1353
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 63
	ble	%icc, .LL1354
	 nop
.LL1353:
	sethi	%hi(59392), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1354:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 1, %g3
	ld	[%fp-20], %g1
	add	%g1, 1, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -127, %g2
	ld	[%fp-24], %g1
	and	%g1, 63, %g1
	add	%g1, %g1, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset9_count6, .-bitfield__write_var_BitField_offset9_count6
	.align 4
	.global bitfield__read_var_BitField_offset9_count7
	.hidden	bitfield__read_var_BitField_offset9_count7
	.type	bitfield__read_var_BitField_offset9_count7, #function
	.proc	04
bitfield__read_var_BitField_offset9_count7:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, 127, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset9_count7, .-bitfield__read_var_BitField_offset9_count7
	.align 4
	.global bitfield__write_var_BitField_offset9_count7
	.hidden	bitfield__write_var_BitField_offset9_count7
	.type	bitfield__write_var_BitField_offset9_count7, #function
	.proc	020
bitfield__write_var_BitField_offset9_count7:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1359
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 127
	ble	%icc, .LL1360
	 nop
.LL1359:
	sethi	%hi(59392), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1360:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 1, %g3
	ld	[%fp-20], %g1
	add	%g1, 1, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -128, %g2
	ld	[%fp-24], %g1
	and	%g1, 127, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset9_count7, .-bitfield__write_var_BitField_offset9_count7
	.align 4
	.global bitfield__read_var_BitField_offset9_count8
	.hidden	bitfield__read_var_BitField_offset9_count8
	.type	bitfield__read_var_BitField_offset9_count8, #function
	.proc	04
bitfield__read_var_BitField_offset9_count8:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 9, %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset9_count8, .-bitfield__read_var_BitField_offset9_count8
	.align 4
	.global bitfield__write_var_BitField_offset9_count8
	.hidden	bitfield__write_var_BitField_offset9_count8
	.type	bitfield__write_var_BitField_offset9_count8, #function
	.proc	020
bitfield__write_var_BitField_offset9_count8:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1365
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 255
	ble	%icc, .LL1366
	 nop
.LL1365:
	sethi	%hi(59392), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1366:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-8356864), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 255, %g1
	sll	%g1, 15, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset9_count8, .-bitfield__write_var_BitField_offset9_count8
	.align 4
	.global bitfield__read_var_BitField_offset9_count9
	.hidden	bitfield__read_var_BitField_offset9_count9
	.type	bitfield__read_var_BitField_offset9_count9, #function
	.proc	04
bitfield__read_var_BitField_offset9_count9:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 9, %g1
	srl	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset9_count9, .-bitfield__read_var_BitField_offset9_count9
	.align 4
	.global bitfield__write_var_BitField_offset9_count9
	.hidden	bitfield__write_var_BitField_offset9_count9
	.type	bitfield__write_var_BitField_offset9_count9, #function
	.proc	020
bitfield__write_var_BitField_offset9_count9:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1371
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 511
	ble	%icc, .LL1372
	 nop
.LL1371:
	sethi	%hi(59392), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1372:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-8373248), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 511, %g1
	sll	%g1, 14, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset9_count9, .-bitfield__write_var_BitField_offset9_count9
	.align 4
	.global bitfield__read_var_BitField_offset9_count10
	.hidden	bitfield__read_var_BitField_offset9_count10
	.type	bitfield__read_var_BitField_offset9_count10, #function
	.proc	04
bitfield__read_var_BitField_offset9_count10:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 9, %g1
	srl	%g1, 22, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset9_count10, .-bitfield__read_var_BitField_offset9_count10
	.align 4
	.global bitfield__write_var_BitField_offset9_count10
	.hidden	bitfield__write_var_BitField_offset9_count10
	.type	bitfield__write_var_BitField_offset9_count10, #function
	.proc	020
bitfield__write_var_BitField_offset9_count10:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1377
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1023
	ble	%icc, .LL1378
	 nop
.LL1377:
	sethi	%hi(60416), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1378:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-8381440), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 1023, %g1
	sll	%g1, 13, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset9_count10, .-bitfield__write_var_BitField_offset9_count10
	.align 4
	.global bitfield__read_var_BitField_offset9_count11
	.hidden	bitfield__read_var_BitField_offset9_count11
	.type	bitfield__read_var_BitField_offset9_count11, #function
	.proc	04
bitfield__read_var_BitField_offset9_count11:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 9, %g1
	srl	%g1, 21, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset9_count11, .-bitfield__read_var_BitField_offset9_count11
	.align 4
	.global bitfield__write_var_BitField_offset9_count11
	.hidden	bitfield__write_var_BitField_offset9_count11
	.type	bitfield__write_var_BitField_offset9_count11, #function
	.proc	020
bitfield__write_var_BitField_offset9_count11:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1383
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 2047
	ble	%icc, .LL1384
	 nop
.LL1383:
	sethi	%hi(60416), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1384:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-8385536), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 2047, %g1
	sll	%g1, 12, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset9_count11, .-bitfield__write_var_BitField_offset9_count11
	.align 4
	.global bitfield__read_var_BitField_offset9_count12
	.hidden	bitfield__read_var_BitField_offset9_count12
	.type	bitfield__read_var_BitField_offset9_count12, #function
	.proc	04
bitfield__read_var_BitField_offset9_count12:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 9, %g1
	srl	%g1, 20, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset9_count12, .-bitfield__read_var_BitField_offset9_count12
	.align 4
	.global bitfield__write_var_BitField_offset9_count12
	.hidden	bitfield__write_var_BitField_offset9_count12
	.type	bitfield__write_var_BitField_offset9_count12, #function
	.proc	020
bitfield__write_var_BitField_offset9_count12:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1389
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 4095
	ble	%icc, .LL1390
	 nop
.LL1389:
	sethi	%hi(60416), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1390:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-8387584), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 4095, %g1
	sll	%g1, 11, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset9_count12, .-bitfield__write_var_BitField_offset9_count12
	.align 4
	.global bitfield__read_var_BitField_offset9_count13
	.hidden	bitfield__read_var_BitField_offset9_count13
	.type	bitfield__read_var_BitField_offset9_count13, #function
	.proc	04
bitfield__read_var_BitField_offset9_count13:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 9, %g1
	srl	%g1, 19, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset9_count13, .-bitfield__read_var_BitField_offset9_count13
	.align 4
	.global bitfield__write_var_BitField_offset9_count13
	.hidden	bitfield__write_var_BitField_offset9_count13
	.type	bitfield__write_var_BitField_offset9_count13, #function
	.proc	020
bitfield__write_var_BitField_offset9_count13:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1395
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(7168), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1396
	 nop
.LL1395:
	sethi	%hi(60416), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1396:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-8388608), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 10, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset9_count13, .-bitfield__write_var_BitField_offset9_count13
	.align 4
	.global bitfield__read_var_BitField_offset9_count14
	.hidden	bitfield__read_var_BitField_offset9_count14
	.type	bitfield__read_var_BitField_offset9_count14, #function
	.proc	04
bitfield__read_var_BitField_offset9_count14:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 9, %g1
	srl	%g1, 18, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset9_count14, .-bitfield__read_var_BitField_offset9_count14
	.align 4
	.global bitfield__write_var_BitField_offset9_count14
	.hidden	bitfield__write_var_BitField_offset9_count14
	.type	bitfield__write_var_BitField_offset9_count14, #function
	.proc	020
bitfield__write_var_BitField_offset9_count14:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1401
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(15360), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1402
	 nop
.LL1401:
	sethi	%hi(61440), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1402:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-8388608), %g1
	or	%g1, 511, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 9, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset9_count14, .-bitfield__write_var_BitField_offset9_count14
	.align 4
	.global bitfield__read_var_BitField_offset9_count15
	.hidden	bitfield__read_var_BitField_offset9_count15
	.type	bitfield__read_var_BitField_offset9_count15, #function
	.proc	04
bitfield__read_var_BitField_offset9_count15:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 9, %g1
	srl	%g1, 17, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset9_count15, .-bitfield__read_var_BitField_offset9_count15
	.align 4
	.global bitfield__write_var_BitField_offset9_count15
	.hidden	bitfield__write_var_BitField_offset9_count15
	.type	bitfield__write_var_BitField_offset9_count15, #function
	.proc	020
bitfield__write_var_BitField_offset9_count15:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1407
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(31744), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1408
	 nop
.LL1407:
	sethi	%hi(61440), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1408:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-8388608), %g1
	or	%g1, 255, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 8, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset9_count15, .-bitfield__write_var_BitField_offset9_count15
	.align 4
	.global bitfield__read_var_BitField_offset9_count16
	.hidden	bitfield__read_var_BitField_offset9_count16
	.type	bitfield__read_var_BitField_offset9_count16, #function
	.proc	04
bitfield__read_var_BitField_offset9_count16:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 9, %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset9_count16, .-bitfield__read_var_BitField_offset9_count16
	.align 4
	.global bitfield__write_var_BitField_offset9_count16
	.hidden	bitfield__write_var_BitField_offset9_count16
	.type	bitfield__write_var_BitField_offset9_count16, #function
	.proc	020
bitfield__write_var_BitField_offset9_count16:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1413
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(64512), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1414
	 nop
.LL1413:
	sethi	%hi(61440), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1414:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-8388608), %g1
	or	%g1, 127, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 7, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset9_count16, .-bitfield__write_var_BitField_offset9_count16
	.align 4
	.global bitfield__read_var_BitField_offset9_count17
	.hidden	bitfield__read_var_BitField_offset9_count17
	.type	bitfield__read_var_BitField_offset9_count17, #function
	.proc	04
bitfield__read_var_BitField_offset9_count17:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 9, %g1
	srl	%g1, 15, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset9_count17, .-bitfield__read_var_BitField_offset9_count17
	.align 4
	.global bitfield__write_var_BitField_offset9_count17
	.hidden	bitfield__write_var_BitField_offset9_count17
	.type	bitfield__write_var_BitField_offset9_count17, #function
	.proc	020
bitfield__write_var_BitField_offset9_count17:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1419
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(130048), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1420
	 nop
.LL1419:
	sethi	%hi(61440), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1420:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-8388608), %g1
	or	%g1, 63, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(130048), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 6, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset9_count17, .-bitfield__write_var_BitField_offset9_count17
	.align 4
	.global bitfield__read_var_BitField_offset9_count18
	.hidden	bitfield__read_var_BitField_offset9_count18
	.type	bitfield__read_var_BitField_offset9_count18, #function
	.proc	04
bitfield__read_var_BitField_offset9_count18:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 9, %g1
	srl	%g1, 14, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset9_count18, .-bitfield__read_var_BitField_offset9_count18
	.align 4
	.global bitfield__write_var_BitField_offset9_count18
	.hidden	bitfield__write_var_BitField_offset9_count18
	.type	bitfield__write_var_BitField_offset9_count18, #function
	.proc	020
bitfield__write_var_BitField_offset9_count18:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1425
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(261120), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1426
	 nop
.LL1425:
	sethi	%hi(62464), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1426:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-8388608), %g1
	or	%g1, 31, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(261120), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 5, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset9_count18, .-bitfield__write_var_BitField_offset9_count18
	.align 4
	.global bitfield__read_var_BitField_offset9_count19
	.hidden	bitfield__read_var_BitField_offset9_count19
	.type	bitfield__read_var_BitField_offset9_count19, #function
	.proc	04
bitfield__read_var_BitField_offset9_count19:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 9, %g1
	srl	%g1, 13, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset9_count19, .-bitfield__read_var_BitField_offset9_count19
	.align 4
	.global bitfield__write_var_BitField_offset9_count19
	.hidden	bitfield__write_var_BitField_offset9_count19
	.type	bitfield__write_var_BitField_offset9_count19, #function
	.proc	020
bitfield__write_var_BitField_offset9_count19:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1431
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(523264), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1432
	 nop
.LL1431:
	sethi	%hi(62464), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1432:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-8388608), %g1
	or	%g1, 15, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(523264), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 4, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset9_count19, .-bitfield__write_var_BitField_offset9_count19
	.align 4
	.global bitfield__read_var_BitField_offset9_count20
	.hidden	bitfield__read_var_BitField_offset9_count20
	.type	bitfield__read_var_BitField_offset9_count20, #function
	.proc	04
bitfield__read_var_BitField_offset9_count20:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 9, %g1
	srl	%g1, 12, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset9_count20, .-bitfield__read_var_BitField_offset9_count20
	.align 4
	.global bitfield__write_var_BitField_offset9_count20
	.hidden	bitfield__write_var_BitField_offset9_count20
	.type	bitfield__write_var_BitField_offset9_count20, #function
	.proc	020
bitfield__write_var_BitField_offset9_count20:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1437
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(1047552), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1438
	 nop
.LL1437:
	sethi	%hi(62464), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1438:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-8388608), %g1
	or	%g1, 7, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(1047552), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 3, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset9_count20, .-bitfield__write_var_BitField_offset9_count20
	.align 4
	.global bitfield__read_var_BitField_offset9_count21
	.hidden	bitfield__read_var_BitField_offset9_count21
	.type	bitfield__read_var_BitField_offset9_count21, #function
	.proc	04
bitfield__read_var_BitField_offset9_count21:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 9, %g1
	srl	%g1, 11, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset9_count21, .-bitfield__read_var_BitField_offset9_count21
	.align 4
	.global bitfield__write_var_BitField_offset9_count21
	.hidden	bitfield__write_var_BitField_offset9_count21
	.type	bitfield__write_var_BitField_offset9_count21, #function
	.proc	020
bitfield__write_var_BitField_offset9_count21:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1443
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(2096128), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1444
	 nop
.LL1443:
	sethi	%hi(62464), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1444:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-8388608), %g1
	or	%g1, 3, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(2096128), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 2, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset9_count21, .-bitfield__write_var_BitField_offset9_count21
	.align 4
	.global bitfield__read_var_BitField_offset9_count22
	.hidden	bitfield__read_var_BitField_offset9_count22
	.type	bitfield__read_var_BitField_offset9_count22, #function
	.proc	04
bitfield__read_var_BitField_offset9_count22:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 9, %g1
	srl	%g1, 10, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset9_count22, .-bitfield__read_var_BitField_offset9_count22
	.align 4
	.global bitfield__write_var_BitField_offset9_count22
	.hidden	bitfield__write_var_BitField_offset9_count22
	.type	bitfield__write_var_BitField_offset9_count22, #function
	.proc	020
bitfield__write_var_BitField_offset9_count22:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1449
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(4193280), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1450
	 nop
.LL1449:
	sethi	%hi(63488), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1450:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-8388608), %g1
	or	%g1, 1, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(4193280), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	add	%g1, %g1, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset9_count22, .-bitfield__write_var_BitField_offset9_count22
	.align 4
	.global bitfield__read_var_BitField_offset9_count23
	.hidden	bitfield__read_var_BitField_offset9_count23
	.type	bitfield__read_var_BitField_offset9_count23, #function
	.proc	04
bitfield__read_var_BitField_offset9_count23:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(8387584), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset9_count23, .-bitfield__read_var_BitField_offset9_count23
	.align 4
	.global bitfield__write_var_BitField_offset9_count23
	.hidden	bitfield__write_var_BitField_offset9_count23
	.type	bitfield__write_var_BitField_offset9_count23, #function
	.proc	020
bitfield__write_var_BitField_offset9_count23:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1455
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(8387584), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1456
	 nop
.LL1455:
	sethi	%hi(63488), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1456:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-8388608), %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(8387584), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset9_count23, .-bitfield__write_var_BitField_offset9_count23
	.align 4
	.global bitfield__read_var_BitField_offset10_count1
	.hidden	bitfield__read_var_BitField_offset10_count1
	.type	bitfield__read_var_BitField_offset10_count1, #function
	.proc	04
bitfield__read_var_BitField_offset10_count1:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 26, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset10_count1, .-bitfield__read_var_BitField_offset10_count1
	.align 4
	.global bitfield__write_var_BitField_offset10_count1
	.hidden	bitfield__write_var_BitField_offset10_count1
	.type	bitfield__write_var_BitField_offset10_count1, #function
	.proc	020
bitfield__write_var_BitField_offset10_count1:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1461
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1
	ble	%icc, .LL1462
	 nop
.LL1461:
	sethi	%hi(63488), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1462:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 1, %g3
	ld	[%fp-20], %g1
	add	%g1, 1, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -33, %g2
	ld	[%fp-24], %g1
	and	%g1, 1, %g1
	sll	%g1, 5, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset10_count1, .-bitfield__write_var_BitField_offset10_count1
	.align 4
	.global bitfield__read_var_BitField_offset10_count2
	.hidden	bitfield__read_var_BitField_offset10_count2
	.type	bitfield__read_var_BitField_offset10_count2, #function
	.proc	04
bitfield__read_var_BitField_offset10_count2:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 26, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset10_count2, .-bitfield__read_var_BitField_offset10_count2
	.align 4
	.global bitfield__write_var_BitField_offset10_count2
	.hidden	bitfield__write_var_BitField_offset10_count2
	.type	bitfield__write_var_BitField_offset10_count2, #function
	.proc	020
bitfield__write_var_BitField_offset10_count2:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1467
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 3
	ble	%icc, .LL1468
	 nop
.LL1467:
	sethi	%hi(63488), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1468:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 1, %g3
	ld	[%fp-20], %g1
	add	%g1, 1, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -49, %g2
	ld	[%fp-24], %g1
	and	%g1, 3, %g1
	sll	%g1, 4, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset10_count2, .-bitfield__write_var_BitField_offset10_count2
	.align 4
	.global bitfield__read_var_BitField_offset10_count3
	.hidden	bitfield__read_var_BitField_offset10_count3
	.type	bitfield__read_var_BitField_offset10_count3, #function
	.proc	04
bitfield__read_var_BitField_offset10_count3:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 26, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset10_count3, .-bitfield__read_var_BitField_offset10_count3
	.align 4
	.global bitfield__write_var_BitField_offset10_count3
	.hidden	bitfield__write_var_BitField_offset10_count3
	.type	bitfield__write_var_BitField_offset10_count3, #function
	.proc	020
bitfield__write_var_BitField_offset10_count3:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1473
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 7
	ble	%icc, .LL1474
	 nop
.LL1473:
	sethi	%hi(64512), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1474:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 1, %g3
	ld	[%fp-20], %g1
	add	%g1, 1, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -57, %g2
	ld	[%fp-24], %g1
	and	%g1, 7, %g1
	sll	%g1, 3, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset10_count3, .-bitfield__write_var_BitField_offset10_count3
	.align 4
	.global bitfield__read_var_BitField_offset10_count4
	.hidden	bitfield__read_var_BitField_offset10_count4
	.type	bitfield__read_var_BitField_offset10_count4, #function
	.proc	04
bitfield__read_var_BitField_offset10_count4:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 26, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset10_count4, .-bitfield__read_var_BitField_offset10_count4
	.align 4
	.global bitfield__write_var_BitField_offset10_count4
	.hidden	bitfield__write_var_BitField_offset10_count4
	.type	bitfield__write_var_BitField_offset10_count4, #function
	.proc	020
bitfield__write_var_BitField_offset10_count4:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1479
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 15
	ble	%icc, .LL1480
	 nop
.LL1479:
	sethi	%hi(64512), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1480:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 1, %g3
	ld	[%fp-20], %g1
	add	%g1, 1, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -61, %g2
	ld	[%fp-24], %g1
	and	%g1, 15, %g1
	sll	%g1, 2, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset10_count4, .-bitfield__write_var_BitField_offset10_count4
	.align 4
	.global bitfield__read_var_BitField_offset10_count5
	.hidden	bitfield__read_var_BitField_offset10_count5
	.type	bitfield__read_var_BitField_offset10_count5, #function
	.proc	04
bitfield__read_var_BitField_offset10_count5:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 26, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset10_count5, .-bitfield__read_var_BitField_offset10_count5
	.align 4
	.global bitfield__write_var_BitField_offset10_count5
	.hidden	bitfield__write_var_BitField_offset10_count5
	.type	bitfield__write_var_BitField_offset10_count5, #function
	.proc	020
bitfield__write_var_BitField_offset10_count5:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1485
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 31
	ble	%icc, .LL1486
	 nop
.LL1485:
	sethi	%hi(64512), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1486:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 1, %g3
	ld	[%fp-20], %g1
	add	%g1, 1, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -63, %g2
	ld	[%fp-24], %g1
	and	%g1, 31, %g1
	add	%g1, %g1, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset10_count5, .-bitfield__write_var_BitField_offset10_count5
	.align 4
	.global bitfield__read_var_BitField_offset10_count6
	.hidden	bitfield__read_var_BitField_offset10_count6
	.type	bitfield__read_var_BitField_offset10_count6, #function
	.proc	04
bitfield__read_var_BitField_offset10_count6:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, 63, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset10_count6, .-bitfield__read_var_BitField_offset10_count6
	.align 4
	.global bitfield__write_var_BitField_offset10_count6
	.hidden	bitfield__write_var_BitField_offset10_count6
	.type	bitfield__write_var_BitField_offset10_count6, #function
	.proc	020
bitfield__write_var_BitField_offset10_count6:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1491
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 63
	ble	%icc, .LL1492
	 nop
.LL1491:
	sethi	%hi(64512), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1492:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 1, %g3
	ld	[%fp-20], %g1
	add	%g1, 1, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -64, %g2
	ld	[%fp-24], %g1
	and	%g1, 63, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset10_count6, .-bitfield__write_var_BitField_offset10_count6
	.align 4
	.global bitfield__read_var_BitField_offset10_count7
	.hidden	bitfield__read_var_BitField_offset10_count7
	.type	bitfield__read_var_BitField_offset10_count7, #function
	.proc	04
bitfield__read_var_BitField_offset10_count7:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 10, %g1
	srl	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset10_count7, .-bitfield__read_var_BitField_offset10_count7
	.align 4
	.global bitfield__write_var_BitField_offset10_count7
	.hidden	bitfield__write_var_BitField_offset10_count7
	.type	bitfield__write_var_BitField_offset10_count7, #function
	.proc	020
bitfield__write_var_BitField_offset10_count7:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1497
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 127
	ble	%icc, .LL1498
	 nop
.LL1497:
	sethi	%hi(65536), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1498:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-4162560), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 127, %g1
	sll	%g1, 15, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset10_count7, .-bitfield__write_var_BitField_offset10_count7
	.align 4
	.global bitfield__read_var_BitField_offset10_count8
	.hidden	bitfield__read_var_BitField_offset10_count8
	.type	bitfield__read_var_BitField_offset10_count8, #function
	.proc	04
bitfield__read_var_BitField_offset10_count8:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 10, %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset10_count8, .-bitfield__read_var_BitField_offset10_count8
	.align 4
	.global bitfield__write_var_BitField_offset10_count8
	.hidden	bitfield__write_var_BitField_offset10_count8
	.type	bitfield__write_var_BitField_offset10_count8, #function
	.proc	020
bitfield__write_var_BitField_offset10_count8:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1503
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 255
	ble	%icc, .LL1504
	 nop
.LL1503:
	sethi	%hi(65536), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1504:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-4178944), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 255, %g1
	sll	%g1, 14, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset10_count8, .-bitfield__write_var_BitField_offset10_count8
	.align 4
	.global bitfield__read_var_BitField_offset10_count9
	.hidden	bitfield__read_var_BitField_offset10_count9
	.type	bitfield__read_var_BitField_offset10_count9, #function
	.proc	04
bitfield__read_var_BitField_offset10_count9:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 10, %g1
	srl	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset10_count9, .-bitfield__read_var_BitField_offset10_count9
	.align 4
	.global bitfield__write_var_BitField_offset10_count9
	.hidden	bitfield__write_var_BitField_offset10_count9
	.type	bitfield__write_var_BitField_offset10_count9, #function
	.proc	020
bitfield__write_var_BitField_offset10_count9:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1509
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 511
	ble	%icc, .LL1510
	 nop
.LL1509:
	sethi	%hi(65536), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1510:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-4187136), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 511, %g1
	sll	%g1, 13, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset10_count9, .-bitfield__write_var_BitField_offset10_count9
	.align 4
	.global bitfield__read_var_BitField_offset10_count10
	.hidden	bitfield__read_var_BitField_offset10_count10
	.type	bitfield__read_var_BitField_offset10_count10, #function
	.proc	04
bitfield__read_var_BitField_offset10_count10:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 10, %g1
	srl	%g1, 22, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset10_count10, .-bitfield__read_var_BitField_offset10_count10
	.align 4
	.global bitfield__write_var_BitField_offset10_count10
	.hidden	bitfield__write_var_BitField_offset10_count10
	.type	bitfield__write_var_BitField_offset10_count10, #function
	.proc	020
bitfield__write_var_BitField_offset10_count10:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1515
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1023
	ble	%icc, .LL1516
	 nop
.LL1515:
	sethi	%hi(65536), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1516:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-4191232), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 1023, %g1
	sll	%g1, 12, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset10_count10, .-bitfield__write_var_BitField_offset10_count10
	.align 4
	.global bitfield__read_var_BitField_offset10_count11
	.hidden	bitfield__read_var_BitField_offset10_count11
	.type	bitfield__read_var_BitField_offset10_count11, #function
	.proc	04
bitfield__read_var_BitField_offset10_count11:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 10, %g1
	srl	%g1, 21, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset10_count11, .-bitfield__read_var_BitField_offset10_count11
	.align 4
	.global bitfield__write_var_BitField_offset10_count11
	.hidden	bitfield__write_var_BitField_offset10_count11
	.type	bitfield__write_var_BitField_offset10_count11, #function
	.proc	020
bitfield__write_var_BitField_offset10_count11:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1521
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 2047
	ble	%icc, .LL1522
	 nop
.LL1521:
	sethi	%hi(66560), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1522:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-4193280), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 2047, %g1
	sll	%g1, 11, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset10_count11, .-bitfield__write_var_BitField_offset10_count11
	.align 4
	.global bitfield__read_var_BitField_offset10_count12
	.hidden	bitfield__read_var_BitField_offset10_count12
	.type	bitfield__read_var_BitField_offset10_count12, #function
	.proc	04
bitfield__read_var_BitField_offset10_count12:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 10, %g1
	srl	%g1, 20, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset10_count12, .-bitfield__read_var_BitField_offset10_count12
	.align 4
	.global bitfield__write_var_BitField_offset10_count12
	.hidden	bitfield__write_var_BitField_offset10_count12
	.type	bitfield__write_var_BitField_offset10_count12, #function
	.proc	020
bitfield__write_var_BitField_offset10_count12:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1527
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 4095
	ble	%icc, .LL1528
	 nop
.LL1527:
	sethi	%hi(66560), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1528:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-4194304), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 4095, %g1
	sll	%g1, 10, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset10_count12, .-bitfield__write_var_BitField_offset10_count12
	.align 4
	.global bitfield__read_var_BitField_offset10_count13
	.hidden	bitfield__read_var_BitField_offset10_count13
	.type	bitfield__read_var_BitField_offset10_count13, #function
	.proc	04
bitfield__read_var_BitField_offset10_count13:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 10, %g1
	srl	%g1, 19, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset10_count13, .-bitfield__read_var_BitField_offset10_count13
	.align 4
	.global bitfield__write_var_BitField_offset10_count13
	.hidden	bitfield__write_var_BitField_offset10_count13
	.type	bitfield__write_var_BitField_offset10_count13, #function
	.proc	020
bitfield__write_var_BitField_offset10_count13:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1533
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(7168), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1534
	 nop
.LL1533:
	sethi	%hi(66560), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1534:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-4194304), %g1
	or	%g1, 511, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 9, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset10_count13, .-bitfield__write_var_BitField_offset10_count13
	.align 4
	.global bitfield__read_var_BitField_offset10_count14
	.hidden	bitfield__read_var_BitField_offset10_count14
	.type	bitfield__read_var_BitField_offset10_count14, #function
	.proc	04
bitfield__read_var_BitField_offset10_count14:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 10, %g1
	srl	%g1, 18, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset10_count14, .-bitfield__read_var_BitField_offset10_count14
	.align 4
	.global bitfield__write_var_BitField_offset10_count14
	.hidden	bitfield__write_var_BitField_offset10_count14
	.type	bitfield__write_var_BitField_offset10_count14, #function
	.proc	020
bitfield__write_var_BitField_offset10_count14:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1539
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(15360), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1540
	 nop
.LL1539:
	sethi	%hi(66560), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1540:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-4194304), %g1
	or	%g1, 255, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 8, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset10_count14, .-bitfield__write_var_BitField_offset10_count14
	.align 4
	.global bitfield__read_var_BitField_offset10_count15
	.hidden	bitfield__read_var_BitField_offset10_count15
	.type	bitfield__read_var_BitField_offset10_count15, #function
	.proc	04
bitfield__read_var_BitField_offset10_count15:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 10, %g1
	srl	%g1, 17, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset10_count15, .-bitfield__read_var_BitField_offset10_count15
	.align 4
	.global bitfield__write_var_BitField_offset10_count15
	.hidden	bitfield__write_var_BitField_offset10_count15
	.type	bitfield__write_var_BitField_offset10_count15, #function
	.proc	020
bitfield__write_var_BitField_offset10_count15:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1545
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(31744), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1546
	 nop
.LL1545:
	sethi	%hi(67584), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1546:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-4194304), %g1
	or	%g1, 127, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 7, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset10_count15, .-bitfield__write_var_BitField_offset10_count15
	.align 4
	.global bitfield__read_var_BitField_offset10_count16
	.hidden	bitfield__read_var_BitField_offset10_count16
	.type	bitfield__read_var_BitField_offset10_count16, #function
	.proc	04
bitfield__read_var_BitField_offset10_count16:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 10, %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset10_count16, .-bitfield__read_var_BitField_offset10_count16
	.align 4
	.global bitfield__write_var_BitField_offset10_count16
	.hidden	bitfield__write_var_BitField_offset10_count16
	.type	bitfield__write_var_BitField_offset10_count16, #function
	.proc	020
bitfield__write_var_BitField_offset10_count16:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1551
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(64512), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1552
	 nop
.LL1551:
	sethi	%hi(67584), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1552:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-4194304), %g1
	or	%g1, 63, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 6, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset10_count16, .-bitfield__write_var_BitField_offset10_count16
	.align 4
	.global bitfield__read_var_BitField_offset10_count17
	.hidden	bitfield__read_var_BitField_offset10_count17
	.type	bitfield__read_var_BitField_offset10_count17, #function
	.proc	04
bitfield__read_var_BitField_offset10_count17:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 10, %g1
	srl	%g1, 15, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset10_count17, .-bitfield__read_var_BitField_offset10_count17
	.align 4
	.global bitfield__write_var_BitField_offset10_count17
	.hidden	bitfield__write_var_BitField_offset10_count17
	.type	bitfield__write_var_BitField_offset10_count17, #function
	.proc	020
bitfield__write_var_BitField_offset10_count17:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1557
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(130048), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1558
	 nop
.LL1557:
	sethi	%hi(67584), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1558:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-4194304), %g1
	or	%g1, 31, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(130048), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 5, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset10_count17, .-bitfield__write_var_BitField_offset10_count17
	.align 4
	.global bitfield__read_var_BitField_offset10_count18
	.hidden	bitfield__read_var_BitField_offset10_count18
	.type	bitfield__read_var_BitField_offset10_count18, #function
	.proc	04
bitfield__read_var_BitField_offset10_count18:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 10, %g1
	srl	%g1, 14, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset10_count18, .-bitfield__read_var_BitField_offset10_count18
	.align 4
	.global bitfield__write_var_BitField_offset10_count18
	.hidden	bitfield__write_var_BitField_offset10_count18
	.type	bitfield__write_var_BitField_offset10_count18, #function
	.proc	020
bitfield__write_var_BitField_offset10_count18:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1563
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(261120), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1564
	 nop
.LL1563:
	sethi	%hi(67584), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1564:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-4194304), %g1
	or	%g1, 15, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(261120), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 4, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset10_count18, .-bitfield__write_var_BitField_offset10_count18
	.align 4
	.global bitfield__read_var_BitField_offset10_count19
	.hidden	bitfield__read_var_BitField_offset10_count19
	.type	bitfield__read_var_BitField_offset10_count19, #function
	.proc	04
bitfield__read_var_BitField_offset10_count19:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 10, %g1
	srl	%g1, 13, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset10_count19, .-bitfield__read_var_BitField_offset10_count19
	.align 4
	.global bitfield__write_var_BitField_offset10_count19
	.hidden	bitfield__write_var_BitField_offset10_count19
	.type	bitfield__write_var_BitField_offset10_count19, #function
	.proc	020
bitfield__write_var_BitField_offset10_count19:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1569
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(523264), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1570
	 nop
.LL1569:
	sethi	%hi(68608), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1570:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-4194304), %g1
	or	%g1, 7, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(523264), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 3, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset10_count19, .-bitfield__write_var_BitField_offset10_count19
	.align 4
	.global bitfield__read_var_BitField_offset10_count20
	.hidden	bitfield__read_var_BitField_offset10_count20
	.type	bitfield__read_var_BitField_offset10_count20, #function
	.proc	04
bitfield__read_var_BitField_offset10_count20:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 10, %g1
	srl	%g1, 12, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset10_count20, .-bitfield__read_var_BitField_offset10_count20
	.align 4
	.global bitfield__write_var_BitField_offset10_count20
	.hidden	bitfield__write_var_BitField_offset10_count20
	.type	bitfield__write_var_BitField_offset10_count20, #function
	.proc	020
bitfield__write_var_BitField_offset10_count20:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1575
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(1047552), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1576
	 nop
.LL1575:
	sethi	%hi(68608), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1576:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-4194304), %g1
	or	%g1, 3, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(1047552), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 2, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset10_count20, .-bitfield__write_var_BitField_offset10_count20
	.align 4
	.global bitfield__read_var_BitField_offset10_count21
	.hidden	bitfield__read_var_BitField_offset10_count21
	.type	bitfield__read_var_BitField_offset10_count21, #function
	.proc	04
bitfield__read_var_BitField_offset10_count21:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 10, %g1
	srl	%g1, 11, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset10_count21, .-bitfield__read_var_BitField_offset10_count21
	.align 4
	.global bitfield__write_var_BitField_offset10_count21
	.hidden	bitfield__write_var_BitField_offset10_count21
	.type	bitfield__write_var_BitField_offset10_count21, #function
	.proc	020
bitfield__write_var_BitField_offset10_count21:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1581
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(2096128), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1582
	 nop
.LL1581:
	sethi	%hi(68608), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1582:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-4194304), %g1
	or	%g1, 1, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(2096128), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	add	%g1, %g1, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset10_count21, .-bitfield__write_var_BitField_offset10_count21
	.align 4
	.global bitfield__read_var_BitField_offset10_count22
	.hidden	bitfield__read_var_BitField_offset10_count22
	.type	bitfield__read_var_BitField_offset10_count22, #function
	.proc	04
bitfield__read_var_BitField_offset10_count22:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(4193280), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset10_count22, .-bitfield__read_var_BitField_offset10_count22
	.align 4
	.global bitfield__write_var_BitField_offset10_count22
	.hidden	bitfield__write_var_BitField_offset10_count22
	.type	bitfield__write_var_BitField_offset10_count22, #function
	.proc	020
bitfield__write_var_BitField_offset10_count22:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1587
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(4193280), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1588
	 nop
.LL1587:
	sethi	%hi(68608), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1588:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-4194304), %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(4193280), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset10_count22, .-bitfield__write_var_BitField_offset10_count22
	.align 4
	.global bitfield__read_var_BitField_offset11_count1
	.hidden	bitfield__read_var_BitField_offset11_count1
	.type	bitfield__read_var_BitField_offset11_count1, #function
	.proc	04
bitfield__read_var_BitField_offset11_count1:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 27, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset11_count1, .-bitfield__read_var_BitField_offset11_count1
	.align 4
	.global bitfield__write_var_BitField_offset11_count1
	.hidden	bitfield__write_var_BitField_offset11_count1
	.type	bitfield__write_var_BitField_offset11_count1, #function
	.proc	020
bitfield__write_var_BitField_offset11_count1:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1593
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1
	ble	%icc, .LL1594
	 nop
.LL1593:
	sethi	%hi(69632), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1594:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 1, %g3
	ld	[%fp-20], %g1
	add	%g1, 1, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -17, %g2
	ld	[%fp-24], %g1
	and	%g1, 1, %g1
	sll	%g1, 4, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset11_count1, .-bitfield__write_var_BitField_offset11_count1
	.align 4
	.global bitfield__read_var_BitField_offset11_count2
	.hidden	bitfield__read_var_BitField_offset11_count2
	.type	bitfield__read_var_BitField_offset11_count2, #function
	.proc	04
bitfield__read_var_BitField_offset11_count2:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 27, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset11_count2, .-bitfield__read_var_BitField_offset11_count2
	.align 4
	.global bitfield__write_var_BitField_offset11_count2
	.hidden	bitfield__write_var_BitField_offset11_count2
	.type	bitfield__write_var_BitField_offset11_count2, #function
	.proc	020
bitfield__write_var_BitField_offset11_count2:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1599
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 3
	ble	%icc, .LL1600
	 nop
.LL1599:
	sethi	%hi(69632), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1600:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 1, %g3
	ld	[%fp-20], %g1
	add	%g1, 1, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -25, %g2
	ld	[%fp-24], %g1
	and	%g1, 3, %g1
	sll	%g1, 3, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset11_count2, .-bitfield__write_var_BitField_offset11_count2
	.align 4
	.global bitfield__read_var_BitField_offset11_count3
	.hidden	bitfield__read_var_BitField_offset11_count3
	.type	bitfield__read_var_BitField_offset11_count3, #function
	.proc	04
bitfield__read_var_BitField_offset11_count3:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 27, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset11_count3, .-bitfield__read_var_BitField_offset11_count3
	.align 4
	.global bitfield__write_var_BitField_offset11_count3
	.hidden	bitfield__write_var_BitField_offset11_count3
	.type	bitfield__write_var_BitField_offset11_count3, #function
	.proc	020
bitfield__write_var_BitField_offset11_count3:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1605
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 7
	ble	%icc, .LL1606
	 nop
.LL1605:
	sethi	%hi(69632), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1606:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 1, %g3
	ld	[%fp-20], %g1
	add	%g1, 1, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -29, %g2
	ld	[%fp-24], %g1
	and	%g1, 7, %g1
	sll	%g1, 2, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset11_count3, .-bitfield__write_var_BitField_offset11_count3
	.align 4
	.global bitfield__read_var_BitField_offset11_count4
	.hidden	bitfield__read_var_BitField_offset11_count4
	.type	bitfield__read_var_BitField_offset11_count4, #function
	.proc	04
bitfield__read_var_BitField_offset11_count4:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 27, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset11_count4, .-bitfield__read_var_BitField_offset11_count4
	.align 4
	.global bitfield__write_var_BitField_offset11_count4
	.hidden	bitfield__write_var_BitField_offset11_count4
	.type	bitfield__write_var_BitField_offset11_count4, #function
	.proc	020
bitfield__write_var_BitField_offset11_count4:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1611
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 15
	ble	%icc, .LL1612
	 nop
.LL1611:
	sethi	%hi(69632), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1612:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 1, %g3
	ld	[%fp-20], %g1
	add	%g1, 1, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -31, %g2
	ld	[%fp-24], %g1
	and	%g1, 15, %g1
	add	%g1, %g1, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset11_count4, .-bitfield__write_var_BitField_offset11_count4
	.align 4
	.global bitfield__read_var_BitField_offset11_count5
	.hidden	bitfield__read_var_BitField_offset11_count5
	.type	bitfield__read_var_BitField_offset11_count5, #function
	.proc	04
bitfield__read_var_BitField_offset11_count5:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset11_count5, .-bitfield__read_var_BitField_offset11_count5
	.align 4
	.global bitfield__write_var_BitField_offset11_count5
	.hidden	bitfield__write_var_BitField_offset11_count5
	.type	bitfield__write_var_BitField_offset11_count5, #function
	.proc	020
bitfield__write_var_BitField_offset11_count5:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1617
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 31
	ble	%icc, .LL1618
	 nop
.LL1617:
	sethi	%hi(70656), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1618:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 1, %g3
	ld	[%fp-20], %g1
	add	%g1, 1, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -32, %g2
	ld	[%fp-24], %g1
	and	%g1, 31, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset11_count5, .-bitfield__write_var_BitField_offset11_count5
	.align 4
	.global bitfield__read_var_BitField_offset11_count6
	.hidden	bitfield__read_var_BitField_offset11_count6
	.type	bitfield__read_var_BitField_offset11_count6, #function
	.proc	04
bitfield__read_var_BitField_offset11_count6:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 11, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset11_count6, .-bitfield__read_var_BitField_offset11_count6
	.align 4
	.global bitfield__write_var_BitField_offset11_count6
	.hidden	bitfield__write_var_BitField_offset11_count6
	.type	bitfield__write_var_BitField_offset11_count6, #function
	.proc	020
bitfield__write_var_BitField_offset11_count6:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1623
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 63
	ble	%icc, .LL1624
	 nop
.LL1623:
	sethi	%hi(70656), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1624:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-2065408), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 63, %g1
	sll	%g1, 15, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset11_count6, .-bitfield__write_var_BitField_offset11_count6
	.align 4
	.global bitfield__read_var_BitField_offset11_count7
	.hidden	bitfield__read_var_BitField_offset11_count7
	.type	bitfield__read_var_BitField_offset11_count7, #function
	.proc	04
bitfield__read_var_BitField_offset11_count7:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 11, %g1
	srl	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset11_count7, .-bitfield__read_var_BitField_offset11_count7
	.align 4
	.global bitfield__write_var_BitField_offset11_count7
	.hidden	bitfield__write_var_BitField_offset11_count7
	.type	bitfield__write_var_BitField_offset11_count7, #function
	.proc	020
bitfield__write_var_BitField_offset11_count7:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1629
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 127
	ble	%icc, .LL1630
	 nop
.LL1629:
	sethi	%hi(70656), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1630:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-2081792), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 127, %g1
	sll	%g1, 14, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset11_count7, .-bitfield__write_var_BitField_offset11_count7
	.align 4
	.global bitfield__read_var_BitField_offset11_count8
	.hidden	bitfield__read_var_BitField_offset11_count8
	.type	bitfield__read_var_BitField_offset11_count8, #function
	.proc	04
bitfield__read_var_BitField_offset11_count8:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 11, %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset11_count8, .-bitfield__read_var_BitField_offset11_count8
	.align 4
	.global bitfield__write_var_BitField_offset11_count8
	.hidden	bitfield__write_var_BitField_offset11_count8
	.type	bitfield__write_var_BitField_offset11_count8, #function
	.proc	020
bitfield__write_var_BitField_offset11_count8:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1635
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 255
	ble	%icc, .LL1636
	 nop
.LL1635:
	sethi	%hi(70656), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1636:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-2089984), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 255, %g1
	sll	%g1, 13, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset11_count8, .-bitfield__write_var_BitField_offset11_count8
	.align 4
	.global bitfield__read_var_BitField_offset11_count9
	.hidden	bitfield__read_var_BitField_offset11_count9
	.type	bitfield__read_var_BitField_offset11_count9, #function
	.proc	04
bitfield__read_var_BitField_offset11_count9:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 11, %g1
	srl	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset11_count9, .-bitfield__read_var_BitField_offset11_count9
	.align 4
	.global bitfield__write_var_BitField_offset11_count9
	.hidden	bitfield__write_var_BitField_offset11_count9
	.type	bitfield__write_var_BitField_offset11_count9, #function
	.proc	020
bitfield__write_var_BitField_offset11_count9:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1641
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 511
	ble	%icc, .LL1642
	 nop
.LL1641:
	sethi	%hi(71680), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1642:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-2094080), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 511, %g1
	sll	%g1, 12, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset11_count9, .-bitfield__write_var_BitField_offset11_count9
	.align 4
	.global bitfield__read_var_BitField_offset11_count10
	.hidden	bitfield__read_var_BitField_offset11_count10
	.type	bitfield__read_var_BitField_offset11_count10, #function
	.proc	04
bitfield__read_var_BitField_offset11_count10:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 11, %g1
	srl	%g1, 22, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset11_count10, .-bitfield__read_var_BitField_offset11_count10
	.align 4
	.global bitfield__write_var_BitField_offset11_count10
	.hidden	bitfield__write_var_BitField_offset11_count10
	.type	bitfield__write_var_BitField_offset11_count10, #function
	.proc	020
bitfield__write_var_BitField_offset11_count10:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1647
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1023
	ble	%icc, .LL1648
	 nop
.LL1647:
	sethi	%hi(71680), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1648:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-2096128), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 1023, %g1
	sll	%g1, 11, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset11_count10, .-bitfield__write_var_BitField_offset11_count10
	.align 4
	.global bitfield__read_var_BitField_offset11_count11
	.hidden	bitfield__read_var_BitField_offset11_count11
	.type	bitfield__read_var_BitField_offset11_count11, #function
	.proc	04
bitfield__read_var_BitField_offset11_count11:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 11, %g1
	srl	%g1, 21, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset11_count11, .-bitfield__read_var_BitField_offset11_count11
	.align 4
	.global bitfield__write_var_BitField_offset11_count11
	.hidden	bitfield__write_var_BitField_offset11_count11
	.type	bitfield__write_var_BitField_offset11_count11, #function
	.proc	020
bitfield__write_var_BitField_offset11_count11:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1653
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 2047
	ble	%icc, .LL1654
	 nop
.LL1653:
	sethi	%hi(71680), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1654:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-2097152), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 2047, %g1
	sll	%g1, 10, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset11_count11, .-bitfield__write_var_BitField_offset11_count11
	.align 4
	.global bitfield__read_var_BitField_offset11_count12
	.hidden	bitfield__read_var_BitField_offset11_count12
	.type	bitfield__read_var_BitField_offset11_count12, #function
	.proc	04
bitfield__read_var_BitField_offset11_count12:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 11, %g1
	srl	%g1, 20, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset11_count12, .-bitfield__read_var_BitField_offset11_count12
	.align 4
	.global bitfield__write_var_BitField_offset11_count12
	.hidden	bitfield__write_var_BitField_offset11_count12
	.type	bitfield__write_var_BitField_offset11_count12, #function
	.proc	020
bitfield__write_var_BitField_offset11_count12:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1659
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 4095
	ble	%icc, .LL1660
	 nop
.LL1659:
	sethi	%hi(71680), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1660:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-2097152), %g1
	or	%g1, 511, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 4095, %g1
	sll	%g1, 9, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset11_count12, .-bitfield__write_var_BitField_offset11_count12
	.align 4
	.global bitfield__read_var_BitField_offset11_count13
	.hidden	bitfield__read_var_BitField_offset11_count13
	.type	bitfield__read_var_BitField_offset11_count13, #function
	.proc	04
bitfield__read_var_BitField_offset11_count13:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 11, %g1
	srl	%g1, 19, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset11_count13, .-bitfield__read_var_BitField_offset11_count13
	.align 4
	.global bitfield__write_var_BitField_offset11_count13
	.hidden	bitfield__write_var_BitField_offset11_count13
	.type	bitfield__write_var_BitField_offset11_count13, #function
	.proc	020
bitfield__write_var_BitField_offset11_count13:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1665
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(7168), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1666
	 nop
.LL1665:
	sethi	%hi(72704), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1666:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-2097152), %g1
	or	%g1, 255, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 8, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset11_count13, .-bitfield__write_var_BitField_offset11_count13
	.align 4
	.global bitfield__read_var_BitField_offset11_count14
	.hidden	bitfield__read_var_BitField_offset11_count14
	.type	bitfield__read_var_BitField_offset11_count14, #function
	.proc	04
bitfield__read_var_BitField_offset11_count14:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 11, %g1
	srl	%g1, 18, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset11_count14, .-bitfield__read_var_BitField_offset11_count14
	.align 4
	.global bitfield__write_var_BitField_offset11_count14
	.hidden	bitfield__write_var_BitField_offset11_count14
	.type	bitfield__write_var_BitField_offset11_count14, #function
	.proc	020
bitfield__write_var_BitField_offset11_count14:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1671
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(15360), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1672
	 nop
.LL1671:
	sethi	%hi(72704), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1672:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-2097152), %g1
	or	%g1, 127, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 7, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset11_count14, .-bitfield__write_var_BitField_offset11_count14
	.align 4
	.global bitfield__read_var_BitField_offset11_count15
	.hidden	bitfield__read_var_BitField_offset11_count15
	.type	bitfield__read_var_BitField_offset11_count15, #function
	.proc	04
bitfield__read_var_BitField_offset11_count15:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 11, %g1
	srl	%g1, 17, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset11_count15, .-bitfield__read_var_BitField_offset11_count15
	.align 4
	.global bitfield__write_var_BitField_offset11_count15
	.hidden	bitfield__write_var_BitField_offset11_count15
	.type	bitfield__write_var_BitField_offset11_count15, #function
	.proc	020
bitfield__write_var_BitField_offset11_count15:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1677
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(31744), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1678
	 nop
.LL1677:
	sethi	%hi(72704), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1678:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-2097152), %g1
	or	%g1, 63, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 6, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset11_count15, .-bitfield__write_var_BitField_offset11_count15
	.align 4
	.global bitfield__read_var_BitField_offset11_count16
	.hidden	bitfield__read_var_BitField_offset11_count16
	.type	bitfield__read_var_BitField_offset11_count16, #function
	.proc	04
bitfield__read_var_BitField_offset11_count16:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 11, %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset11_count16, .-bitfield__read_var_BitField_offset11_count16
	.align 4
	.global bitfield__write_var_BitField_offset11_count16
	.hidden	bitfield__write_var_BitField_offset11_count16
	.type	bitfield__write_var_BitField_offset11_count16, #function
	.proc	020
bitfield__write_var_BitField_offset11_count16:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1683
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(64512), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1684
	 nop
.LL1683:
	sethi	%hi(72704), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1684:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-2097152), %g1
	or	%g1, 31, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 5, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset11_count16, .-bitfield__write_var_BitField_offset11_count16
	.align 4
	.global bitfield__read_var_BitField_offset11_count17
	.hidden	bitfield__read_var_BitField_offset11_count17
	.type	bitfield__read_var_BitField_offset11_count17, #function
	.proc	04
bitfield__read_var_BitField_offset11_count17:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 11, %g1
	srl	%g1, 15, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset11_count17, .-bitfield__read_var_BitField_offset11_count17
	.align 4
	.global bitfield__write_var_BitField_offset11_count17
	.hidden	bitfield__write_var_BitField_offset11_count17
	.type	bitfield__write_var_BitField_offset11_count17, #function
	.proc	020
bitfield__write_var_BitField_offset11_count17:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1689
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(130048), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1690
	 nop
.LL1689:
	sethi	%hi(73728), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1690:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-2097152), %g1
	or	%g1, 15, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(130048), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 4, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset11_count17, .-bitfield__write_var_BitField_offset11_count17
	.align 4
	.global bitfield__read_var_BitField_offset11_count18
	.hidden	bitfield__read_var_BitField_offset11_count18
	.type	bitfield__read_var_BitField_offset11_count18, #function
	.proc	04
bitfield__read_var_BitField_offset11_count18:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 11, %g1
	srl	%g1, 14, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset11_count18, .-bitfield__read_var_BitField_offset11_count18
	.align 4
	.global bitfield__write_var_BitField_offset11_count18
	.hidden	bitfield__write_var_BitField_offset11_count18
	.type	bitfield__write_var_BitField_offset11_count18, #function
	.proc	020
bitfield__write_var_BitField_offset11_count18:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1695
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(261120), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1696
	 nop
.LL1695:
	sethi	%hi(73728), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1696:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-2097152), %g1
	or	%g1, 7, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(261120), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 3, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset11_count18, .-bitfield__write_var_BitField_offset11_count18
	.align 4
	.global bitfield__read_var_BitField_offset11_count19
	.hidden	bitfield__read_var_BitField_offset11_count19
	.type	bitfield__read_var_BitField_offset11_count19, #function
	.proc	04
bitfield__read_var_BitField_offset11_count19:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 11, %g1
	srl	%g1, 13, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset11_count19, .-bitfield__read_var_BitField_offset11_count19
	.align 4
	.global bitfield__write_var_BitField_offset11_count19
	.hidden	bitfield__write_var_BitField_offset11_count19
	.type	bitfield__write_var_BitField_offset11_count19, #function
	.proc	020
bitfield__write_var_BitField_offset11_count19:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1701
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(523264), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1702
	 nop
.LL1701:
	sethi	%hi(73728), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1702:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-2097152), %g1
	or	%g1, 3, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(523264), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 2, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset11_count19, .-bitfield__write_var_BitField_offset11_count19
	.align 4
	.global bitfield__read_var_BitField_offset11_count20
	.hidden	bitfield__read_var_BitField_offset11_count20
	.type	bitfield__read_var_BitField_offset11_count20, #function
	.proc	04
bitfield__read_var_BitField_offset11_count20:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 11, %g1
	srl	%g1, 12, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset11_count20, .-bitfield__read_var_BitField_offset11_count20
	.align 4
	.global bitfield__write_var_BitField_offset11_count20
	.hidden	bitfield__write_var_BitField_offset11_count20
	.type	bitfield__write_var_BitField_offset11_count20, #function
	.proc	020
bitfield__write_var_BitField_offset11_count20:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1707
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(1047552), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1708
	 nop
.LL1707:
	sethi	%hi(73728), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1708:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-2097152), %g1
	or	%g1, 1, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(1047552), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	add	%g1, %g1, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset11_count20, .-bitfield__write_var_BitField_offset11_count20
	.align 4
	.global bitfield__read_var_BitField_offset11_count21
	.hidden	bitfield__read_var_BitField_offset11_count21
	.type	bitfield__read_var_BitField_offset11_count21, #function
	.proc	04
bitfield__read_var_BitField_offset11_count21:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(2096128), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset11_count21, .-bitfield__read_var_BitField_offset11_count21
	.align 4
	.global bitfield__write_var_BitField_offset11_count21
	.hidden	bitfield__write_var_BitField_offset11_count21
	.type	bitfield__write_var_BitField_offset11_count21, #function
	.proc	020
bitfield__write_var_BitField_offset11_count21:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1713
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(2096128), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1714
	 nop
.LL1713:
	sethi	%hi(74752), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1714:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-2097152), %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(2096128), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset11_count21, .-bitfield__write_var_BitField_offset11_count21
	.align 4
	.global bitfield__read_var_BitField_offset12_count1
	.hidden	bitfield__read_var_BitField_offset12_count1
	.type	bitfield__read_var_BitField_offset12_count1, #function
	.proc	04
bitfield__read_var_BitField_offset12_count1:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 28, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset12_count1, .-bitfield__read_var_BitField_offset12_count1
	.align 4
	.global bitfield__write_var_BitField_offset12_count1
	.hidden	bitfield__write_var_BitField_offset12_count1
	.type	bitfield__write_var_BitField_offset12_count1, #function
	.proc	020
bitfield__write_var_BitField_offset12_count1:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1719
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1
	ble	%icc, .LL1720
	 nop
.LL1719:
	sethi	%hi(74752), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1720:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 1, %g3
	ld	[%fp-20], %g1
	add	%g1, 1, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -9, %g2
	ld	[%fp-24], %g1
	and	%g1, 1, %g1
	sll	%g1, 3, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset12_count1, .-bitfield__write_var_BitField_offset12_count1
	.align 4
	.global bitfield__read_var_BitField_offset12_count2
	.hidden	bitfield__read_var_BitField_offset12_count2
	.type	bitfield__read_var_BitField_offset12_count2, #function
	.proc	04
bitfield__read_var_BitField_offset12_count2:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 28, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset12_count2, .-bitfield__read_var_BitField_offset12_count2
	.align 4
	.global bitfield__write_var_BitField_offset12_count2
	.hidden	bitfield__write_var_BitField_offset12_count2
	.type	bitfield__write_var_BitField_offset12_count2, #function
	.proc	020
bitfield__write_var_BitField_offset12_count2:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1725
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 3
	ble	%icc, .LL1726
	 nop
.LL1725:
	sethi	%hi(74752), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1726:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 1, %g3
	ld	[%fp-20], %g1
	add	%g1, 1, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -13, %g2
	ld	[%fp-24], %g1
	and	%g1, 3, %g1
	sll	%g1, 2, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset12_count2, .-bitfield__write_var_BitField_offset12_count2
	.align 4
	.global bitfield__read_var_BitField_offset12_count3
	.hidden	bitfield__read_var_BitField_offset12_count3
	.type	bitfield__read_var_BitField_offset12_count3, #function
	.proc	04
bitfield__read_var_BitField_offset12_count3:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 28, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset12_count3, .-bitfield__read_var_BitField_offset12_count3
	.align 4
	.global bitfield__write_var_BitField_offset12_count3
	.hidden	bitfield__write_var_BitField_offset12_count3
	.type	bitfield__write_var_BitField_offset12_count3, #function
	.proc	020
bitfield__write_var_BitField_offset12_count3:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1731
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 7
	ble	%icc, .LL1732
	 nop
.LL1731:
	sethi	%hi(74752), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1732:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 1, %g3
	ld	[%fp-20], %g1
	add	%g1, 1, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -15, %g2
	ld	[%fp-24], %g1
	and	%g1, 7, %g1
	add	%g1, %g1, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset12_count3, .-bitfield__write_var_BitField_offset12_count3
	.align 4
	.global bitfield__read_var_BitField_offset12_count4
	.hidden	bitfield__read_var_BitField_offset12_count4
	.type	bitfield__read_var_BitField_offset12_count4, #function
	.proc	04
bitfield__read_var_BitField_offset12_count4:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, 15, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset12_count4, .-bitfield__read_var_BitField_offset12_count4
	.align 4
	.global bitfield__write_var_BitField_offset12_count4
	.hidden	bitfield__write_var_BitField_offset12_count4
	.type	bitfield__write_var_BitField_offset12_count4, #function
	.proc	020
bitfield__write_var_BitField_offset12_count4:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1737
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 15
	ble	%icc, .LL1738
	 nop
.LL1737:
	sethi	%hi(75776), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1738:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 1, %g3
	ld	[%fp-20], %g1
	add	%g1, 1, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -16, %g2
	ld	[%fp-24], %g1
	and	%g1, 15, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset12_count4, .-bitfield__write_var_BitField_offset12_count4
	.align 4
	.global bitfield__read_var_BitField_offset12_count5
	.hidden	bitfield__read_var_BitField_offset12_count5
	.type	bitfield__read_var_BitField_offset12_count5, #function
	.proc	04
bitfield__read_var_BitField_offset12_count5:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 12, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset12_count5, .-bitfield__read_var_BitField_offset12_count5
	.align 4
	.global bitfield__write_var_BitField_offset12_count5
	.hidden	bitfield__write_var_BitField_offset12_count5
	.type	bitfield__write_var_BitField_offset12_count5, #function
	.proc	020
bitfield__write_var_BitField_offset12_count5:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1743
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 31
	ble	%icc, .LL1744
	 nop
.LL1743:
	sethi	%hi(75776), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1744:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-1016832), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 31, %g1
	sll	%g1, 15, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset12_count5, .-bitfield__write_var_BitField_offset12_count5
	.align 4
	.global bitfield__read_var_BitField_offset12_count6
	.hidden	bitfield__read_var_BitField_offset12_count6
	.type	bitfield__read_var_BitField_offset12_count6, #function
	.proc	04
bitfield__read_var_BitField_offset12_count6:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 12, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset12_count6, .-bitfield__read_var_BitField_offset12_count6
	.align 4
	.global bitfield__write_var_BitField_offset12_count6
	.hidden	bitfield__write_var_BitField_offset12_count6
	.type	bitfield__write_var_BitField_offset12_count6, #function
	.proc	020
bitfield__write_var_BitField_offset12_count6:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1749
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 63
	ble	%icc, .LL1750
	 nop
.LL1749:
	sethi	%hi(75776), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1750:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-1033216), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 63, %g1
	sll	%g1, 14, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset12_count6, .-bitfield__write_var_BitField_offset12_count6
	.align 4
	.global bitfield__read_var_BitField_offset12_count7
	.hidden	bitfield__read_var_BitField_offset12_count7
	.type	bitfield__read_var_BitField_offset12_count7, #function
	.proc	04
bitfield__read_var_BitField_offset12_count7:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 12, %g1
	srl	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset12_count7, .-bitfield__read_var_BitField_offset12_count7
	.align 4
	.global bitfield__write_var_BitField_offset12_count7
	.hidden	bitfield__write_var_BitField_offset12_count7
	.type	bitfield__write_var_BitField_offset12_count7, #function
	.proc	020
bitfield__write_var_BitField_offset12_count7:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1755
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 127
	ble	%icc, .LL1756
	 nop
.LL1755:
	sethi	%hi(75776), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1756:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-1041408), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 127, %g1
	sll	%g1, 13, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset12_count7, .-bitfield__write_var_BitField_offset12_count7
	.align 4
	.global bitfield__read_var_BitField_offset12_count8
	.hidden	bitfield__read_var_BitField_offset12_count8
	.type	bitfield__read_var_BitField_offset12_count8, #function
	.proc	04
bitfield__read_var_BitField_offset12_count8:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 12, %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset12_count8, .-bitfield__read_var_BitField_offset12_count8
	.align 4
	.global bitfield__write_var_BitField_offset12_count8
	.hidden	bitfield__write_var_BitField_offset12_count8
	.type	bitfield__write_var_BitField_offset12_count8, #function
	.proc	020
bitfield__write_var_BitField_offset12_count8:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1761
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 255
	ble	%icc, .LL1762
	 nop
.LL1761:
	sethi	%hi(76800), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1762:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-1045504), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 255, %g1
	sll	%g1, 12, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset12_count8, .-bitfield__write_var_BitField_offset12_count8
	.align 4
	.global bitfield__read_var_BitField_offset12_count9
	.hidden	bitfield__read_var_BitField_offset12_count9
	.type	bitfield__read_var_BitField_offset12_count9, #function
	.proc	04
bitfield__read_var_BitField_offset12_count9:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 12, %g1
	srl	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset12_count9, .-bitfield__read_var_BitField_offset12_count9
	.align 4
	.global bitfield__write_var_BitField_offset12_count9
	.hidden	bitfield__write_var_BitField_offset12_count9
	.type	bitfield__write_var_BitField_offset12_count9, #function
	.proc	020
bitfield__write_var_BitField_offset12_count9:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1767
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 511
	ble	%icc, .LL1768
	 nop
.LL1767:
	sethi	%hi(76800), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1768:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-1047552), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 511, %g1
	sll	%g1, 11, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset12_count9, .-bitfield__write_var_BitField_offset12_count9
	.align 4
	.global bitfield__read_var_BitField_offset12_count10
	.hidden	bitfield__read_var_BitField_offset12_count10
	.type	bitfield__read_var_BitField_offset12_count10, #function
	.proc	04
bitfield__read_var_BitField_offset12_count10:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 12, %g1
	srl	%g1, 22, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset12_count10, .-bitfield__read_var_BitField_offset12_count10
	.align 4
	.global bitfield__write_var_BitField_offset12_count10
	.hidden	bitfield__write_var_BitField_offset12_count10
	.type	bitfield__write_var_BitField_offset12_count10, #function
	.proc	020
bitfield__write_var_BitField_offset12_count10:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1773
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1023
	ble	%icc, .LL1774
	 nop
.LL1773:
	sethi	%hi(76800), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1774:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-1048576), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 1023, %g1
	sll	%g1, 10, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset12_count10, .-bitfield__write_var_BitField_offset12_count10
	.align 4
	.global bitfield__read_var_BitField_offset12_count11
	.hidden	bitfield__read_var_BitField_offset12_count11
	.type	bitfield__read_var_BitField_offset12_count11, #function
	.proc	04
bitfield__read_var_BitField_offset12_count11:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 12, %g1
	srl	%g1, 21, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset12_count11, .-bitfield__read_var_BitField_offset12_count11
	.align 4
	.global bitfield__write_var_BitField_offset12_count11
	.hidden	bitfield__write_var_BitField_offset12_count11
	.type	bitfield__write_var_BitField_offset12_count11, #function
	.proc	020
bitfield__write_var_BitField_offset12_count11:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1779
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 2047
	ble	%icc, .LL1780
	 nop
.LL1779:
	sethi	%hi(76800), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1780:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-1048576), %g1
	or	%g1, 511, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 2047, %g1
	sll	%g1, 9, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset12_count11, .-bitfield__write_var_BitField_offset12_count11
	.align 4
	.global bitfield__read_var_BitField_offset12_count12
	.hidden	bitfield__read_var_BitField_offset12_count12
	.type	bitfield__read_var_BitField_offset12_count12, #function
	.proc	04
bitfield__read_var_BitField_offset12_count12:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 12, %g1
	srl	%g1, 20, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset12_count12, .-bitfield__read_var_BitField_offset12_count12
	.align 4
	.global bitfield__write_var_BitField_offset12_count12
	.hidden	bitfield__write_var_BitField_offset12_count12
	.type	bitfield__write_var_BitField_offset12_count12, #function
	.proc	020
bitfield__write_var_BitField_offset12_count12:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1785
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 4095
	ble	%icc, .LL1786
	 nop
.LL1785:
	sethi	%hi(77824), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1786:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-1048576), %g1
	or	%g1, 255, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 4095, %g1
	sll	%g1, 8, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset12_count12, .-bitfield__write_var_BitField_offset12_count12
	.align 4
	.global bitfield__read_var_BitField_offset12_count13
	.hidden	bitfield__read_var_BitField_offset12_count13
	.type	bitfield__read_var_BitField_offset12_count13, #function
	.proc	04
bitfield__read_var_BitField_offset12_count13:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 12, %g1
	srl	%g1, 19, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset12_count13, .-bitfield__read_var_BitField_offset12_count13
	.align 4
	.global bitfield__write_var_BitField_offset12_count13
	.hidden	bitfield__write_var_BitField_offset12_count13
	.type	bitfield__write_var_BitField_offset12_count13, #function
	.proc	020
bitfield__write_var_BitField_offset12_count13:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1791
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(7168), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1792
	 nop
.LL1791:
	sethi	%hi(77824), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1792:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-1048576), %g1
	or	%g1, 127, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 7, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset12_count13, .-bitfield__write_var_BitField_offset12_count13
	.align 4
	.global bitfield__read_var_BitField_offset12_count14
	.hidden	bitfield__read_var_BitField_offset12_count14
	.type	bitfield__read_var_BitField_offset12_count14, #function
	.proc	04
bitfield__read_var_BitField_offset12_count14:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 12, %g1
	srl	%g1, 18, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset12_count14, .-bitfield__read_var_BitField_offset12_count14
	.align 4
	.global bitfield__write_var_BitField_offset12_count14
	.hidden	bitfield__write_var_BitField_offset12_count14
	.type	bitfield__write_var_BitField_offset12_count14, #function
	.proc	020
bitfield__write_var_BitField_offset12_count14:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1797
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(15360), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1798
	 nop
.LL1797:
	sethi	%hi(77824), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1798:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-1048576), %g1
	or	%g1, 63, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 6, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset12_count14, .-bitfield__write_var_BitField_offset12_count14
	.align 4
	.global bitfield__read_var_BitField_offset12_count15
	.hidden	bitfield__read_var_BitField_offset12_count15
	.type	bitfield__read_var_BitField_offset12_count15, #function
	.proc	04
bitfield__read_var_BitField_offset12_count15:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 12, %g1
	srl	%g1, 17, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset12_count15, .-bitfield__read_var_BitField_offset12_count15
	.align 4
	.global bitfield__write_var_BitField_offset12_count15
	.hidden	bitfield__write_var_BitField_offset12_count15
	.type	bitfield__write_var_BitField_offset12_count15, #function
	.proc	020
bitfield__write_var_BitField_offset12_count15:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1803
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(31744), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1804
	 nop
.LL1803:
	sethi	%hi(77824), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1804:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-1048576), %g1
	or	%g1, 31, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 5, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset12_count15, .-bitfield__write_var_BitField_offset12_count15
	.align 4
	.global bitfield__read_var_BitField_offset12_count16
	.hidden	bitfield__read_var_BitField_offset12_count16
	.type	bitfield__read_var_BitField_offset12_count16, #function
	.proc	04
bitfield__read_var_BitField_offset12_count16:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 12, %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset12_count16, .-bitfield__read_var_BitField_offset12_count16
	.align 4
	.global bitfield__write_var_BitField_offset12_count16
	.hidden	bitfield__write_var_BitField_offset12_count16
	.type	bitfield__write_var_BitField_offset12_count16, #function
	.proc	020
bitfield__write_var_BitField_offset12_count16:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1809
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(64512), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1810
	 nop
.LL1809:
	sethi	%hi(78848), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1810:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-1048576), %g1
	or	%g1, 15, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 4, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset12_count16, .-bitfield__write_var_BitField_offset12_count16
	.align 4
	.global bitfield__read_var_BitField_offset12_count17
	.hidden	bitfield__read_var_BitField_offset12_count17
	.type	bitfield__read_var_BitField_offset12_count17, #function
	.proc	04
bitfield__read_var_BitField_offset12_count17:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 12, %g1
	srl	%g1, 15, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset12_count17, .-bitfield__read_var_BitField_offset12_count17
	.align 4
	.global bitfield__write_var_BitField_offset12_count17
	.hidden	bitfield__write_var_BitField_offset12_count17
	.type	bitfield__write_var_BitField_offset12_count17, #function
	.proc	020
bitfield__write_var_BitField_offset12_count17:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1815
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(130048), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1816
	 nop
.LL1815:
	sethi	%hi(78848), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1816:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-1048576), %g1
	or	%g1, 7, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(130048), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 3, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset12_count17, .-bitfield__write_var_BitField_offset12_count17
	.align 4
	.global bitfield__read_var_BitField_offset12_count18
	.hidden	bitfield__read_var_BitField_offset12_count18
	.type	bitfield__read_var_BitField_offset12_count18, #function
	.proc	04
bitfield__read_var_BitField_offset12_count18:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 12, %g1
	srl	%g1, 14, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset12_count18, .-bitfield__read_var_BitField_offset12_count18
	.align 4
	.global bitfield__write_var_BitField_offset12_count18
	.hidden	bitfield__write_var_BitField_offset12_count18
	.type	bitfield__write_var_BitField_offset12_count18, #function
	.proc	020
bitfield__write_var_BitField_offset12_count18:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1821
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(261120), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1822
	 nop
.LL1821:
	sethi	%hi(78848), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1822:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-1048576), %g1
	or	%g1, 3, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(261120), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 2, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset12_count18, .-bitfield__write_var_BitField_offset12_count18
	.align 4
	.global bitfield__read_var_BitField_offset12_count19
	.hidden	bitfield__read_var_BitField_offset12_count19
	.type	bitfield__read_var_BitField_offset12_count19, #function
	.proc	04
bitfield__read_var_BitField_offset12_count19:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 12, %g1
	srl	%g1, 13, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset12_count19, .-bitfield__read_var_BitField_offset12_count19
	.align 4
	.global bitfield__write_var_BitField_offset12_count19
	.hidden	bitfield__write_var_BitField_offset12_count19
	.type	bitfield__write_var_BitField_offset12_count19, #function
	.proc	020
bitfield__write_var_BitField_offset12_count19:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1827
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(523264), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1828
	 nop
.LL1827:
	sethi	%hi(78848), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1828:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-1048576), %g1
	or	%g1, 1, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(523264), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	add	%g1, %g1, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset12_count19, .-bitfield__write_var_BitField_offset12_count19
	.align 4
	.global bitfield__read_var_BitField_offset12_count20
	.hidden	bitfield__read_var_BitField_offset12_count20
	.type	bitfield__read_var_BitField_offset12_count20, #function
	.proc	04
bitfield__read_var_BitField_offset12_count20:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(1047552), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset12_count20, .-bitfield__read_var_BitField_offset12_count20
	.align 4
	.global bitfield__write_var_BitField_offset12_count20
	.hidden	bitfield__write_var_BitField_offset12_count20
	.type	bitfield__write_var_BitField_offset12_count20, #function
	.proc	020
bitfield__write_var_BitField_offset12_count20:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1833
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(1047552), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1834
	 nop
.LL1833:
	sethi	%hi(79872), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1834:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-1048576), %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(1047552), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset12_count20, .-bitfield__write_var_BitField_offset12_count20
	.align 4
	.global bitfield__read_var_BitField_offset13_count1
	.hidden	bitfield__read_var_BitField_offset13_count1
	.type	bitfield__read_var_BitField_offset13_count1, #function
	.proc	04
bitfield__read_var_BitField_offset13_count1:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 29, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset13_count1, .-bitfield__read_var_BitField_offset13_count1
	.align 4
	.global bitfield__write_var_BitField_offset13_count1
	.hidden	bitfield__write_var_BitField_offset13_count1
	.type	bitfield__write_var_BitField_offset13_count1, #function
	.proc	020
bitfield__write_var_BitField_offset13_count1:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1839
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1
	ble	%icc, .LL1840
	 nop
.LL1839:
	sethi	%hi(79872), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1840:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 1, %g3
	ld	[%fp-20], %g1
	add	%g1, 1, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -5, %g2
	ld	[%fp-24], %g1
	and	%g1, 1, %g1
	sll	%g1, 2, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset13_count1, .-bitfield__write_var_BitField_offset13_count1
	.align 4
	.global bitfield__read_var_BitField_offset13_count2
	.hidden	bitfield__read_var_BitField_offset13_count2
	.type	bitfield__read_var_BitField_offset13_count2, #function
	.proc	04
bitfield__read_var_BitField_offset13_count2:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 29, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset13_count2, .-bitfield__read_var_BitField_offset13_count2
	.align 4
	.global bitfield__write_var_BitField_offset13_count2
	.hidden	bitfield__write_var_BitField_offset13_count2
	.type	bitfield__write_var_BitField_offset13_count2, #function
	.proc	020
bitfield__write_var_BitField_offset13_count2:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1845
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 3
	ble	%icc, .LL1846
	 nop
.LL1845:
	sethi	%hi(79872), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1846:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 1, %g3
	ld	[%fp-20], %g1
	add	%g1, 1, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -7, %g2
	ld	[%fp-24], %g1
	and	%g1, 3, %g1
	add	%g1, %g1, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset13_count2, .-bitfield__write_var_BitField_offset13_count2
	.align 4
	.global bitfield__read_var_BitField_offset13_count3
	.hidden	bitfield__read_var_BitField_offset13_count3
	.type	bitfield__read_var_BitField_offset13_count3, #function
	.proc	04
bitfield__read_var_BitField_offset13_count3:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, 7, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset13_count3, .-bitfield__read_var_BitField_offset13_count3
	.align 4
	.global bitfield__write_var_BitField_offset13_count3
	.hidden	bitfield__write_var_BitField_offset13_count3
	.type	bitfield__write_var_BitField_offset13_count3, #function
	.proc	020
bitfield__write_var_BitField_offset13_count3:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1851
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 7
	ble	%icc, .LL1852
	 nop
.LL1851:
	sethi	%hi(79872), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1852:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 1, %g3
	ld	[%fp-20], %g1
	add	%g1, 1, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -8, %g2
	ld	[%fp-24], %g1
	and	%g1, 7, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset13_count3, .-bitfield__write_var_BitField_offset13_count3
	.align 4
	.global bitfield__read_var_BitField_offset13_count4
	.hidden	bitfield__read_var_BitField_offset13_count4
	.type	bitfield__read_var_BitField_offset13_count4, #function
	.proc	04
bitfield__read_var_BitField_offset13_count4:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 13, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset13_count4, .-bitfield__read_var_BitField_offset13_count4
	.align 4
	.global bitfield__write_var_BitField_offset13_count4
	.hidden	bitfield__write_var_BitField_offset13_count4
	.type	bitfield__write_var_BitField_offset13_count4, #function
	.proc	020
bitfield__write_var_BitField_offset13_count4:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1857
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 15
	ble	%icc, .LL1858
	 nop
.LL1857:
	sethi	%hi(80896), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1858:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-492544), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 15, %g1
	sll	%g1, 15, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset13_count4, .-bitfield__write_var_BitField_offset13_count4
	.align 4
	.global bitfield__read_var_BitField_offset13_count5
	.hidden	bitfield__read_var_BitField_offset13_count5
	.type	bitfield__read_var_BitField_offset13_count5, #function
	.proc	04
bitfield__read_var_BitField_offset13_count5:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 13, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset13_count5, .-bitfield__read_var_BitField_offset13_count5
	.align 4
	.global bitfield__write_var_BitField_offset13_count5
	.hidden	bitfield__write_var_BitField_offset13_count5
	.type	bitfield__write_var_BitField_offset13_count5, #function
	.proc	020
bitfield__write_var_BitField_offset13_count5:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1863
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 31
	ble	%icc, .LL1864
	 nop
.LL1863:
	sethi	%hi(80896), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1864:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-508928), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 31, %g1
	sll	%g1, 14, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset13_count5, .-bitfield__write_var_BitField_offset13_count5
	.align 4
	.global bitfield__read_var_BitField_offset13_count6
	.hidden	bitfield__read_var_BitField_offset13_count6
	.type	bitfield__read_var_BitField_offset13_count6, #function
	.proc	04
bitfield__read_var_BitField_offset13_count6:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 13, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset13_count6, .-bitfield__read_var_BitField_offset13_count6
	.align 4
	.global bitfield__write_var_BitField_offset13_count6
	.hidden	bitfield__write_var_BitField_offset13_count6
	.type	bitfield__write_var_BitField_offset13_count6, #function
	.proc	020
bitfield__write_var_BitField_offset13_count6:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1869
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 63
	ble	%icc, .LL1870
	 nop
.LL1869:
	sethi	%hi(80896), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1870:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-517120), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 63, %g1
	sll	%g1, 13, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset13_count6, .-bitfield__write_var_BitField_offset13_count6
	.align 4
	.global bitfield__read_var_BitField_offset13_count7
	.hidden	bitfield__read_var_BitField_offset13_count7
	.type	bitfield__read_var_BitField_offset13_count7, #function
	.proc	04
bitfield__read_var_BitField_offset13_count7:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 13, %g1
	srl	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset13_count7, .-bitfield__read_var_BitField_offset13_count7
	.align 4
	.global bitfield__write_var_BitField_offset13_count7
	.hidden	bitfield__write_var_BitField_offset13_count7
	.type	bitfield__write_var_BitField_offset13_count7, #function
	.proc	020
bitfield__write_var_BitField_offset13_count7:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1875
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 127
	ble	%icc, .LL1876
	 nop
.LL1875:
	sethi	%hi(80896), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1876:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-521216), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 127, %g1
	sll	%g1, 12, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset13_count7, .-bitfield__write_var_BitField_offset13_count7
	.align 4
	.global bitfield__read_var_BitField_offset13_count8
	.hidden	bitfield__read_var_BitField_offset13_count8
	.type	bitfield__read_var_BitField_offset13_count8, #function
	.proc	04
bitfield__read_var_BitField_offset13_count8:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 13, %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset13_count8, .-bitfield__read_var_BitField_offset13_count8
	.align 4
	.global bitfield__write_var_BitField_offset13_count8
	.hidden	bitfield__write_var_BitField_offset13_count8
	.type	bitfield__write_var_BitField_offset13_count8, #function
	.proc	020
bitfield__write_var_BitField_offset13_count8:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1881
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 255
	ble	%icc, .LL1882
	 nop
.LL1881:
	sethi	%hi(81920), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1882:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-523264), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 255, %g1
	sll	%g1, 11, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset13_count8, .-bitfield__write_var_BitField_offset13_count8
	.align 4
	.global bitfield__read_var_BitField_offset13_count9
	.hidden	bitfield__read_var_BitField_offset13_count9
	.type	bitfield__read_var_BitField_offset13_count9, #function
	.proc	04
bitfield__read_var_BitField_offset13_count9:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 13, %g1
	srl	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset13_count9, .-bitfield__read_var_BitField_offset13_count9
	.align 4
	.global bitfield__write_var_BitField_offset13_count9
	.hidden	bitfield__write_var_BitField_offset13_count9
	.type	bitfield__write_var_BitField_offset13_count9, #function
	.proc	020
bitfield__write_var_BitField_offset13_count9:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1887
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 511
	ble	%icc, .LL1888
	 nop
.LL1887:
	sethi	%hi(81920), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1888:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-524288), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 511, %g1
	sll	%g1, 10, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset13_count9, .-bitfield__write_var_BitField_offset13_count9
	.align 4
	.global bitfield__read_var_BitField_offset13_count10
	.hidden	bitfield__read_var_BitField_offset13_count10
	.type	bitfield__read_var_BitField_offset13_count10, #function
	.proc	04
bitfield__read_var_BitField_offset13_count10:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 13, %g1
	srl	%g1, 22, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset13_count10, .-bitfield__read_var_BitField_offset13_count10
	.align 4
	.global bitfield__write_var_BitField_offset13_count10
	.hidden	bitfield__write_var_BitField_offset13_count10
	.type	bitfield__write_var_BitField_offset13_count10, #function
	.proc	020
bitfield__write_var_BitField_offset13_count10:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1893
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1023
	ble	%icc, .LL1894
	 nop
.LL1893:
	sethi	%hi(81920), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1894:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-524288), %g1
	or	%g1, 511, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 1023, %g1
	sll	%g1, 9, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset13_count10, .-bitfield__write_var_BitField_offset13_count10
	.align 4
	.global bitfield__read_var_BitField_offset13_count11
	.hidden	bitfield__read_var_BitField_offset13_count11
	.type	bitfield__read_var_BitField_offset13_count11, #function
	.proc	04
bitfield__read_var_BitField_offset13_count11:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 13, %g1
	srl	%g1, 21, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset13_count11, .-bitfield__read_var_BitField_offset13_count11
	.align 4
	.global bitfield__write_var_BitField_offset13_count11
	.hidden	bitfield__write_var_BitField_offset13_count11
	.type	bitfield__write_var_BitField_offset13_count11, #function
	.proc	020
bitfield__write_var_BitField_offset13_count11:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1899
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 2047
	ble	%icc, .LL1900
	 nop
.LL1899:
	sethi	%hi(81920), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1900:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-524288), %g1
	or	%g1, 255, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 2047, %g1
	sll	%g1, 8, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset13_count11, .-bitfield__write_var_BitField_offset13_count11
	.align 4
	.global bitfield__read_var_BitField_offset13_count12
	.hidden	bitfield__read_var_BitField_offset13_count12
	.type	bitfield__read_var_BitField_offset13_count12, #function
	.proc	04
bitfield__read_var_BitField_offset13_count12:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 13, %g1
	srl	%g1, 20, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset13_count12, .-bitfield__read_var_BitField_offset13_count12
	.align 4
	.global bitfield__write_var_BitField_offset13_count12
	.hidden	bitfield__write_var_BitField_offset13_count12
	.type	bitfield__write_var_BitField_offset13_count12, #function
	.proc	020
bitfield__write_var_BitField_offset13_count12:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1905
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 4095
	ble	%icc, .LL1906
	 nop
.LL1905:
	sethi	%hi(82944), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1906:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-524288), %g1
	or	%g1, 127, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 4095, %g1
	sll	%g1, 7, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset13_count12, .-bitfield__write_var_BitField_offset13_count12
	.align 4
	.global bitfield__read_var_BitField_offset13_count13
	.hidden	bitfield__read_var_BitField_offset13_count13
	.type	bitfield__read_var_BitField_offset13_count13, #function
	.proc	04
bitfield__read_var_BitField_offset13_count13:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 13, %g1
	srl	%g1, 19, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset13_count13, .-bitfield__read_var_BitField_offset13_count13
	.align 4
	.global bitfield__write_var_BitField_offset13_count13
	.hidden	bitfield__write_var_BitField_offset13_count13
	.type	bitfield__write_var_BitField_offset13_count13, #function
	.proc	020
bitfield__write_var_BitField_offset13_count13:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1911
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(7168), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1912
	 nop
.LL1911:
	sethi	%hi(82944), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1912:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-524288), %g1
	or	%g1, 63, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 6, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset13_count13, .-bitfield__write_var_BitField_offset13_count13
	.align 4
	.global bitfield__read_var_BitField_offset13_count14
	.hidden	bitfield__read_var_BitField_offset13_count14
	.type	bitfield__read_var_BitField_offset13_count14, #function
	.proc	04
bitfield__read_var_BitField_offset13_count14:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 13, %g1
	srl	%g1, 18, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset13_count14, .-bitfield__read_var_BitField_offset13_count14
	.align 4
	.global bitfield__write_var_BitField_offset13_count14
	.hidden	bitfield__write_var_BitField_offset13_count14
	.type	bitfield__write_var_BitField_offset13_count14, #function
	.proc	020
bitfield__write_var_BitField_offset13_count14:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1917
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(15360), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1918
	 nop
.LL1917:
	sethi	%hi(82944), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1918:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-524288), %g1
	or	%g1, 31, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 5, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset13_count14, .-bitfield__write_var_BitField_offset13_count14
	.align 4
	.global bitfield__read_var_BitField_offset13_count15
	.hidden	bitfield__read_var_BitField_offset13_count15
	.type	bitfield__read_var_BitField_offset13_count15, #function
	.proc	04
bitfield__read_var_BitField_offset13_count15:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 13, %g1
	srl	%g1, 17, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset13_count15, .-bitfield__read_var_BitField_offset13_count15
	.align 4
	.global bitfield__write_var_BitField_offset13_count15
	.hidden	bitfield__write_var_BitField_offset13_count15
	.type	bitfield__write_var_BitField_offset13_count15, #function
	.proc	020
bitfield__write_var_BitField_offset13_count15:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1923
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(31744), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1924
	 nop
.LL1923:
	sethi	%hi(82944), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1924:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-524288), %g1
	or	%g1, 15, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 4, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset13_count15, .-bitfield__write_var_BitField_offset13_count15
	.align 4
	.global bitfield__read_var_BitField_offset13_count16
	.hidden	bitfield__read_var_BitField_offset13_count16
	.type	bitfield__read_var_BitField_offset13_count16, #function
	.proc	04
bitfield__read_var_BitField_offset13_count16:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 13, %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset13_count16, .-bitfield__read_var_BitField_offset13_count16
	.align 4
	.global bitfield__write_var_BitField_offset13_count16
	.hidden	bitfield__write_var_BitField_offset13_count16
	.type	bitfield__write_var_BitField_offset13_count16, #function
	.proc	020
bitfield__write_var_BitField_offset13_count16:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1929
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(64512), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1930
	 nop
.LL1929:
	sethi	%hi(83968), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1930:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-524288), %g1
	or	%g1, 7, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 3, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset13_count16, .-bitfield__write_var_BitField_offset13_count16
	.align 4
	.global bitfield__read_var_BitField_offset13_count17
	.hidden	bitfield__read_var_BitField_offset13_count17
	.type	bitfield__read_var_BitField_offset13_count17, #function
	.proc	04
bitfield__read_var_BitField_offset13_count17:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 13, %g1
	srl	%g1, 15, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset13_count17, .-bitfield__read_var_BitField_offset13_count17
	.align 4
	.global bitfield__write_var_BitField_offset13_count17
	.hidden	bitfield__write_var_BitField_offset13_count17
	.type	bitfield__write_var_BitField_offset13_count17, #function
	.proc	020
bitfield__write_var_BitField_offset13_count17:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1935
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(130048), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1936
	 nop
.LL1935:
	sethi	%hi(83968), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1936:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-524288), %g1
	or	%g1, 3, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(130048), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 2, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset13_count17, .-bitfield__write_var_BitField_offset13_count17
	.align 4
	.global bitfield__read_var_BitField_offset13_count18
	.hidden	bitfield__read_var_BitField_offset13_count18
	.type	bitfield__read_var_BitField_offset13_count18, #function
	.proc	04
bitfield__read_var_BitField_offset13_count18:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 13, %g1
	srl	%g1, 14, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset13_count18, .-bitfield__read_var_BitField_offset13_count18
	.align 4
	.global bitfield__write_var_BitField_offset13_count18
	.hidden	bitfield__write_var_BitField_offset13_count18
	.type	bitfield__write_var_BitField_offset13_count18, #function
	.proc	020
bitfield__write_var_BitField_offset13_count18:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1941
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(261120), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1942
	 nop
.LL1941:
	sethi	%hi(83968), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1942:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-524288), %g1
	or	%g1, 1, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(261120), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	add	%g1, %g1, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset13_count18, .-bitfield__write_var_BitField_offset13_count18
	.align 4
	.global bitfield__read_var_BitField_offset13_count19
	.hidden	bitfield__read_var_BitField_offset13_count19
	.type	bitfield__read_var_BitField_offset13_count19, #function
	.proc	04
bitfield__read_var_BitField_offset13_count19:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(523264), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset13_count19, .-bitfield__read_var_BitField_offset13_count19
	.align 4
	.global bitfield__write_var_BitField_offset13_count19
	.hidden	bitfield__write_var_BitField_offset13_count19
	.type	bitfield__write_var_BitField_offset13_count19, #function
	.proc	020
bitfield__write_var_BitField_offset13_count19:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1947
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(523264), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL1948
	 nop
.LL1947:
	sethi	%hi(83968), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1948:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-524288), %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(523264), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset13_count19, .-bitfield__write_var_BitField_offset13_count19
	.align 4
	.global bitfield__read_var_BitField_offset14_count1
	.hidden	bitfield__read_var_BitField_offset14_count1
	.type	bitfield__read_var_BitField_offset14_count1, #function
	.proc	04
bitfield__read_var_BitField_offset14_count1:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 30, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset14_count1, .-bitfield__read_var_BitField_offset14_count1
	.align 4
	.global bitfield__write_var_BitField_offset14_count1
	.hidden	bitfield__write_var_BitField_offset14_count1
	.type	bitfield__write_var_BitField_offset14_count1, #function
	.proc	020
bitfield__write_var_BitField_offset14_count1:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1953
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1
	ble	%icc, .LL1954
	 nop
.LL1953:
	sethi	%hi(84992), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1954:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 1, %g3
	ld	[%fp-20], %g1
	add	%g1, 1, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -3, %g2
	ld	[%fp-24], %g1
	and	%g1, 1, %g1
	add	%g1, %g1, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset14_count1, .-bitfield__write_var_BitField_offset14_count1
	.align 4
	.global bitfield__read_var_BitField_offset14_count2
	.hidden	bitfield__read_var_BitField_offset14_count2
	.type	bitfield__read_var_BitField_offset14_count2, #function
	.proc	04
bitfield__read_var_BitField_offset14_count2:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, 3, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset14_count2, .-bitfield__read_var_BitField_offset14_count2
	.align 4
	.global bitfield__write_var_BitField_offset14_count2
	.hidden	bitfield__write_var_BitField_offset14_count2
	.type	bitfield__write_var_BitField_offset14_count2, #function
	.proc	020
bitfield__write_var_BitField_offset14_count2:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1959
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 3
	ble	%icc, .LL1960
	 nop
.LL1959:
	sethi	%hi(84992), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1960:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 1, %g3
	ld	[%fp-20], %g1
	add	%g1, 1, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -4, %g2
	ld	[%fp-24], %g1
	and	%g1, 3, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset14_count2, .-bitfield__write_var_BitField_offset14_count2
	.align 4
	.global bitfield__read_var_BitField_offset14_count3
	.hidden	bitfield__read_var_BitField_offset14_count3
	.type	bitfield__read_var_BitField_offset14_count3, #function
	.proc	04
bitfield__read_var_BitField_offset14_count3:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 14, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset14_count3, .-bitfield__read_var_BitField_offset14_count3
	.align 4
	.global bitfield__write_var_BitField_offset14_count3
	.hidden	bitfield__write_var_BitField_offset14_count3
	.type	bitfield__write_var_BitField_offset14_count3, #function
	.proc	020
bitfield__write_var_BitField_offset14_count3:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1965
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 7
	ble	%icc, .LL1966
	 nop
.LL1965:
	sethi	%hi(84992), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1966:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-230400), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 7, %g1
	sll	%g1, 15, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset14_count3, .-bitfield__write_var_BitField_offset14_count3
	.align 4
	.global bitfield__read_var_BitField_offset14_count4
	.hidden	bitfield__read_var_BitField_offset14_count4
	.type	bitfield__read_var_BitField_offset14_count4, #function
	.proc	04
bitfield__read_var_BitField_offset14_count4:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 14, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset14_count4, .-bitfield__read_var_BitField_offset14_count4
	.align 4
	.global bitfield__write_var_BitField_offset14_count4
	.hidden	bitfield__write_var_BitField_offset14_count4
	.type	bitfield__write_var_BitField_offset14_count4, #function
	.proc	020
bitfield__write_var_BitField_offset14_count4:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1971
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 15
	ble	%icc, .LL1972
	 nop
.LL1971:
	sethi	%hi(84992), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1972:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-246784), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 15, %g1
	sll	%g1, 14, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset14_count4, .-bitfield__write_var_BitField_offset14_count4
	.align 4
	.global bitfield__read_var_BitField_offset14_count5
	.hidden	bitfield__read_var_BitField_offset14_count5
	.type	bitfield__read_var_BitField_offset14_count5, #function
	.proc	04
bitfield__read_var_BitField_offset14_count5:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 14, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset14_count5, .-bitfield__read_var_BitField_offset14_count5
	.align 4
	.global bitfield__write_var_BitField_offset14_count5
	.hidden	bitfield__write_var_BitField_offset14_count5
	.type	bitfield__write_var_BitField_offset14_count5, #function
	.proc	020
bitfield__write_var_BitField_offset14_count5:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1977
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 31
	ble	%icc, .LL1978
	 nop
.LL1977:
	sethi	%hi(86016), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1978:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-254976), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 31, %g1
	sll	%g1, 13, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset14_count5, .-bitfield__write_var_BitField_offset14_count5
	.align 4
	.global bitfield__read_var_BitField_offset14_count6
	.hidden	bitfield__read_var_BitField_offset14_count6
	.type	bitfield__read_var_BitField_offset14_count6, #function
	.proc	04
bitfield__read_var_BitField_offset14_count6:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 14, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset14_count6, .-bitfield__read_var_BitField_offset14_count6
	.align 4
	.global bitfield__write_var_BitField_offset14_count6
	.hidden	bitfield__write_var_BitField_offset14_count6
	.type	bitfield__write_var_BitField_offset14_count6, #function
	.proc	020
bitfield__write_var_BitField_offset14_count6:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1983
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 63
	ble	%icc, .LL1984
	 nop
.LL1983:
	sethi	%hi(86016), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1984:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-259072), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 63, %g1
	sll	%g1, 12, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset14_count6, .-bitfield__write_var_BitField_offset14_count6
	.align 4
	.global bitfield__read_var_BitField_offset14_count7
	.hidden	bitfield__read_var_BitField_offset14_count7
	.type	bitfield__read_var_BitField_offset14_count7, #function
	.proc	04
bitfield__read_var_BitField_offset14_count7:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 14, %g1
	srl	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset14_count7, .-bitfield__read_var_BitField_offset14_count7
	.align 4
	.global bitfield__write_var_BitField_offset14_count7
	.hidden	bitfield__write_var_BitField_offset14_count7
	.type	bitfield__write_var_BitField_offset14_count7, #function
	.proc	020
bitfield__write_var_BitField_offset14_count7:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1989
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 127
	ble	%icc, .LL1990
	 nop
.LL1989:
	sethi	%hi(86016), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1990:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-261120), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 127, %g1
	sll	%g1, 11, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset14_count7, .-bitfield__write_var_BitField_offset14_count7
	.align 4
	.global bitfield__read_var_BitField_offset14_count8
	.hidden	bitfield__read_var_BitField_offset14_count8
	.type	bitfield__read_var_BitField_offset14_count8, #function
	.proc	04
bitfield__read_var_BitField_offset14_count8:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 14, %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset14_count8, .-bitfield__read_var_BitField_offset14_count8
	.align 4
	.global bitfield__write_var_BitField_offset14_count8
	.hidden	bitfield__write_var_BitField_offset14_count8
	.type	bitfield__write_var_BitField_offset14_count8, #function
	.proc	020
bitfield__write_var_BitField_offset14_count8:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL1995
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 255
	ble	%icc, .LL1996
	 nop
.LL1995:
	sethi	%hi(86016), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1996:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-262144), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 255, %g1
	sll	%g1, 10, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset14_count8, .-bitfield__write_var_BitField_offset14_count8
	.align 4
	.global bitfield__read_var_BitField_offset14_count9
	.hidden	bitfield__read_var_BitField_offset14_count9
	.type	bitfield__read_var_BitField_offset14_count9, #function
	.proc	04
bitfield__read_var_BitField_offset14_count9:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 14, %g1
	srl	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset14_count9, .-bitfield__read_var_BitField_offset14_count9
	.align 4
	.global bitfield__write_var_BitField_offset14_count9
	.hidden	bitfield__write_var_BitField_offset14_count9
	.type	bitfield__write_var_BitField_offset14_count9, #function
	.proc	020
bitfield__write_var_BitField_offset14_count9:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2001
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 511
	ble	%icc, .LL2002
	 nop
.LL2001:
	sethi	%hi(87040), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2002:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-262144), %g1
	or	%g1, 511, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 511, %g1
	sll	%g1, 9, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset14_count9, .-bitfield__write_var_BitField_offset14_count9
	.align 4
	.global bitfield__read_var_BitField_offset14_count10
	.hidden	bitfield__read_var_BitField_offset14_count10
	.type	bitfield__read_var_BitField_offset14_count10, #function
	.proc	04
bitfield__read_var_BitField_offset14_count10:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 14, %g1
	srl	%g1, 22, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset14_count10, .-bitfield__read_var_BitField_offset14_count10
	.align 4
	.global bitfield__write_var_BitField_offset14_count10
	.hidden	bitfield__write_var_BitField_offset14_count10
	.type	bitfield__write_var_BitField_offset14_count10, #function
	.proc	020
bitfield__write_var_BitField_offset14_count10:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2007
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1023
	ble	%icc, .LL2008
	 nop
.LL2007:
	sethi	%hi(87040), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2008:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-262144), %g1
	or	%g1, 255, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 1023, %g1
	sll	%g1, 8, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset14_count10, .-bitfield__write_var_BitField_offset14_count10
	.align 4
	.global bitfield__read_var_BitField_offset14_count11
	.hidden	bitfield__read_var_BitField_offset14_count11
	.type	bitfield__read_var_BitField_offset14_count11, #function
	.proc	04
bitfield__read_var_BitField_offset14_count11:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 14, %g1
	srl	%g1, 21, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset14_count11, .-bitfield__read_var_BitField_offset14_count11
	.align 4
	.global bitfield__write_var_BitField_offset14_count11
	.hidden	bitfield__write_var_BitField_offset14_count11
	.type	bitfield__write_var_BitField_offset14_count11, #function
	.proc	020
bitfield__write_var_BitField_offset14_count11:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2013
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 2047
	ble	%icc, .LL2014
	 nop
.LL2013:
	sethi	%hi(87040), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2014:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-262144), %g1
	or	%g1, 127, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 2047, %g1
	sll	%g1, 7, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset14_count11, .-bitfield__write_var_BitField_offset14_count11
	.align 4
	.global bitfield__read_var_BitField_offset14_count12
	.hidden	bitfield__read_var_BitField_offset14_count12
	.type	bitfield__read_var_BitField_offset14_count12, #function
	.proc	04
bitfield__read_var_BitField_offset14_count12:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 14, %g1
	srl	%g1, 20, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset14_count12, .-bitfield__read_var_BitField_offset14_count12
	.align 4
	.global bitfield__write_var_BitField_offset14_count12
	.hidden	bitfield__write_var_BitField_offset14_count12
	.type	bitfield__write_var_BitField_offset14_count12, #function
	.proc	020
bitfield__write_var_BitField_offset14_count12:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2019
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 4095
	ble	%icc, .LL2020
	 nop
.LL2019:
	sethi	%hi(87040), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL2020:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-262144), %g1
	or	%g1, 63, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 4095, %g1
	sll	%g1, 6, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset14_count12, .-bitfield__write_var_BitField_offset14_count12
	.align 4
	.global bitfield__read_var_BitField_offset14_count13
	.hidden	bitfield__read_var_BitField_offset14_count13
	.type	bitfield__read_var_BitField_offset14_count13, #function
	.proc	04
bitfield__read_var_BitField_offset14_count13:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 14, %g1
	srl	%g1, 19, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset14_count13, .-bitfield__read_var_BitField_offset14_count13
	.align 4
	.global bitfield__write_var_BitField_offset14_count13
	.hidden	bitfield__write_var_BitField_offset14_count13
	.type	bitfield__write_var_BitField_offset14_count13, #function
	.proc	020
bitfield__write_var_BitField_offset14_count13:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2025
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(7168), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL2026
	 nop
.LL2025:
	sethi	%hi(88064), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2026:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-262144), %g1
	or	%g1, 31, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 5, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset14_count13, .-bitfield__write_var_BitField_offset14_count13
	.align 4
	.global bitfield__read_var_BitField_offset14_count14
	.hidden	bitfield__read_var_BitField_offset14_count14
	.type	bitfield__read_var_BitField_offset14_count14, #function
	.proc	04
bitfield__read_var_BitField_offset14_count14:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 14, %g1
	srl	%g1, 18, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset14_count14, .-bitfield__read_var_BitField_offset14_count14
	.align 4
	.global bitfield__write_var_BitField_offset14_count14
	.hidden	bitfield__write_var_BitField_offset14_count14
	.type	bitfield__write_var_BitField_offset14_count14, #function
	.proc	020
bitfield__write_var_BitField_offset14_count14:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2031
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(15360), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL2032
	 nop
.LL2031:
	sethi	%hi(88064), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2032:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-262144), %g1
	or	%g1, 15, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 4, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset14_count14, .-bitfield__write_var_BitField_offset14_count14
	.align 4
	.global bitfield__read_var_BitField_offset14_count15
	.hidden	bitfield__read_var_BitField_offset14_count15
	.type	bitfield__read_var_BitField_offset14_count15, #function
	.proc	04
bitfield__read_var_BitField_offset14_count15:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 14, %g1
	srl	%g1, 17, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset14_count15, .-bitfield__read_var_BitField_offset14_count15
	.align 4
	.global bitfield__write_var_BitField_offset14_count15
	.hidden	bitfield__write_var_BitField_offset14_count15
	.type	bitfield__write_var_BitField_offset14_count15, #function
	.proc	020
bitfield__write_var_BitField_offset14_count15:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2037
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(31744), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL2038
	 nop
.LL2037:
	sethi	%hi(88064), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2038:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-262144), %g1
	or	%g1, 7, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 3, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset14_count15, .-bitfield__write_var_BitField_offset14_count15
	.align 4
	.global bitfield__read_var_BitField_offset14_count16
	.hidden	bitfield__read_var_BitField_offset14_count16
	.type	bitfield__read_var_BitField_offset14_count16, #function
	.proc	04
bitfield__read_var_BitField_offset14_count16:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 14, %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset14_count16, .-bitfield__read_var_BitField_offset14_count16
	.align 4
	.global bitfield__write_var_BitField_offset14_count16
	.hidden	bitfield__write_var_BitField_offset14_count16
	.type	bitfield__write_var_BitField_offset14_count16, #function
	.proc	020
bitfield__write_var_BitField_offset14_count16:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2043
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(64512), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL2044
	 nop
.LL2043:
	sethi	%hi(88064), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL2044:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-262144), %g1
	or	%g1, 3, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 2, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset14_count16, .-bitfield__write_var_BitField_offset14_count16
	.align 4
	.global bitfield__read_var_BitField_offset14_count17
	.hidden	bitfield__read_var_BitField_offset14_count17
	.type	bitfield__read_var_BitField_offset14_count17, #function
	.proc	04
bitfield__read_var_BitField_offset14_count17:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 14, %g1
	srl	%g1, 15, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset14_count17, .-bitfield__read_var_BitField_offset14_count17
	.align 4
	.global bitfield__write_var_BitField_offset14_count17
	.hidden	bitfield__write_var_BitField_offset14_count17
	.type	bitfield__write_var_BitField_offset14_count17, #function
	.proc	020
bitfield__write_var_BitField_offset14_count17:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2049
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(130048), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL2050
	 nop
.LL2049:
	sethi	%hi(89088), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2050:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-262144), %g1
	or	%g1, 1, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(130048), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	add	%g1, %g1, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset14_count17, .-bitfield__write_var_BitField_offset14_count17
	.align 4
	.global bitfield__read_var_BitField_offset14_count18
	.hidden	bitfield__read_var_BitField_offset14_count18
	.type	bitfield__read_var_BitField_offset14_count18, #function
	.proc	04
bitfield__read_var_BitField_offset14_count18:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(261120), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset14_count18, .-bitfield__read_var_BitField_offset14_count18
	.align 4
	.global bitfield__write_var_BitField_offset14_count18
	.hidden	bitfield__write_var_BitField_offset14_count18
	.type	bitfield__write_var_BitField_offset14_count18, #function
	.proc	020
bitfield__write_var_BitField_offset14_count18:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2055
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(261120), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL2056
	 nop
.LL2055:
	sethi	%hi(89088), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2056:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-262144), %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(261120), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset14_count18, .-bitfield__write_var_BitField_offset14_count18
	.align 4
	.global bitfield__read_var_BitField_offset15_count1
	.hidden	bitfield__read_var_BitField_offset15_count1
	.type	bitfield__read_var_BitField_offset15_count1, #function
	.proc	04
bitfield__read_var_BitField_offset15_count1:
	save	%sp, -120, %sp
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset15_count1, .-bitfield__read_var_BitField_offset15_count1
	.align 4
	.global bitfield__write_var_BitField_offset15_count1
	.hidden	bitfield__write_var_BitField_offset15_count1
	.type	bitfield__write_var_BitField_offset15_count1, #function
	.proc	020
bitfield__write_var_BitField_offset15_count1:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2061
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1
	ble	%icc, .LL2062
	 nop
.LL2061:
	sethi	%hi(89088), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2062:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 1, %g3
	ld	[%fp-20], %g1
	add	%g1, 1, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -2, %g2
	ld	[%fp-24], %g1
	and	%g1, 1, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset15_count1, .-bitfield__write_var_BitField_offset15_count1
	.align 4
	.global bitfield__read_var_BitField_offset15_count2
	.hidden	bitfield__read_var_BitField_offset15_count2
	.type	bitfield__read_var_BitField_offset15_count2, #function
	.proc	04
bitfield__read_var_BitField_offset15_count2:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 15, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset15_count2, .-bitfield__read_var_BitField_offset15_count2
	.align 4
	.global bitfield__write_var_BitField_offset15_count2
	.hidden	bitfield__write_var_BitField_offset15_count2
	.type	bitfield__write_var_BitField_offset15_count2, #function
	.proc	020
bitfield__write_var_BitField_offset15_count2:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2067
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 3
	ble	%icc, .LL2068
	 nop
.LL2067:
	sethi	%hi(89088), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL2068:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-99328), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 3, %g1
	sll	%g1, 15, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset15_count2, .-bitfield__write_var_BitField_offset15_count2
	.align 4
	.global bitfield__read_var_BitField_offset15_count3
	.hidden	bitfield__read_var_BitField_offset15_count3
	.type	bitfield__read_var_BitField_offset15_count3, #function
	.proc	04
bitfield__read_var_BitField_offset15_count3:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 15, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset15_count3, .-bitfield__read_var_BitField_offset15_count3
	.align 4
	.global bitfield__write_var_BitField_offset15_count3
	.hidden	bitfield__write_var_BitField_offset15_count3
	.type	bitfield__write_var_BitField_offset15_count3, #function
	.proc	020
bitfield__write_var_BitField_offset15_count3:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2073
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 7
	ble	%icc, .LL2074
	 nop
.LL2073:
	sethi	%hi(90112), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2074:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-115712), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 7, %g1
	sll	%g1, 14, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset15_count3, .-bitfield__write_var_BitField_offset15_count3
	.align 4
	.global bitfield__read_var_BitField_offset15_count4
	.hidden	bitfield__read_var_BitField_offset15_count4
	.type	bitfield__read_var_BitField_offset15_count4, #function
	.proc	04
bitfield__read_var_BitField_offset15_count4:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 15, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset15_count4, .-bitfield__read_var_BitField_offset15_count4
	.align 4
	.global bitfield__write_var_BitField_offset15_count4
	.hidden	bitfield__write_var_BitField_offset15_count4
	.type	bitfield__write_var_BitField_offset15_count4, #function
	.proc	020
bitfield__write_var_BitField_offset15_count4:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2079
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 15
	ble	%icc, .LL2080
	 nop
.LL2079:
	sethi	%hi(90112), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2080:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-123904), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 15, %g1
	sll	%g1, 13, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset15_count4, .-bitfield__write_var_BitField_offset15_count4
	.align 4
	.global bitfield__read_var_BitField_offset15_count5
	.hidden	bitfield__read_var_BitField_offset15_count5
	.type	bitfield__read_var_BitField_offset15_count5, #function
	.proc	04
bitfield__read_var_BitField_offset15_count5:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 15, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset15_count5, .-bitfield__read_var_BitField_offset15_count5
	.align 4
	.global bitfield__write_var_BitField_offset15_count5
	.hidden	bitfield__write_var_BitField_offset15_count5
	.type	bitfield__write_var_BitField_offset15_count5, #function
	.proc	020
bitfield__write_var_BitField_offset15_count5:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2085
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 31
	ble	%icc, .LL2086
	 nop
.LL2085:
	sethi	%hi(90112), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2086:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-128000), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 31, %g1
	sll	%g1, 12, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset15_count5, .-bitfield__write_var_BitField_offset15_count5
	.align 4
	.global bitfield__read_var_BitField_offset15_count6
	.hidden	bitfield__read_var_BitField_offset15_count6
	.type	bitfield__read_var_BitField_offset15_count6, #function
	.proc	04
bitfield__read_var_BitField_offset15_count6:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 15, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset15_count6, .-bitfield__read_var_BitField_offset15_count6
	.align 4
	.global bitfield__write_var_BitField_offset15_count6
	.hidden	bitfield__write_var_BitField_offset15_count6
	.type	bitfield__write_var_BitField_offset15_count6, #function
	.proc	020
bitfield__write_var_BitField_offset15_count6:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2091
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 63
	ble	%icc, .LL2092
	 nop
.LL2091:
	sethi	%hi(90112), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL2092:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-130048), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 63, %g1
	sll	%g1, 11, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset15_count6, .-bitfield__write_var_BitField_offset15_count6
	.align 4
	.global bitfield__read_var_BitField_offset15_count7
	.hidden	bitfield__read_var_BitField_offset15_count7
	.type	bitfield__read_var_BitField_offset15_count7, #function
	.proc	04
bitfield__read_var_BitField_offset15_count7:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 15, %g1
	srl	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset15_count7, .-bitfield__read_var_BitField_offset15_count7
	.align 4
	.global bitfield__write_var_BitField_offset15_count7
	.hidden	bitfield__write_var_BitField_offset15_count7
	.type	bitfield__write_var_BitField_offset15_count7, #function
	.proc	020
bitfield__write_var_BitField_offset15_count7:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2097
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 127
	ble	%icc, .LL2098
	 nop
.LL2097:
	sethi	%hi(91136), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2098:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-131072), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 127, %g1
	sll	%g1, 10, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset15_count7, .-bitfield__write_var_BitField_offset15_count7
	.align 4
	.global bitfield__read_var_BitField_offset15_count8
	.hidden	bitfield__read_var_BitField_offset15_count8
	.type	bitfield__read_var_BitField_offset15_count8, #function
	.proc	04
bitfield__read_var_BitField_offset15_count8:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 15, %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset15_count8, .-bitfield__read_var_BitField_offset15_count8
	.align 4
	.global bitfield__write_var_BitField_offset15_count8
	.hidden	bitfield__write_var_BitField_offset15_count8
	.type	bitfield__write_var_BitField_offset15_count8, #function
	.proc	020
bitfield__write_var_BitField_offset15_count8:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2103
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 255
	ble	%icc, .LL2104
	 nop
.LL2103:
	sethi	%hi(91136), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2104:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-131072), %g1
	or	%g1, 511, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 255, %g1
	sll	%g1, 9, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset15_count8, .-bitfield__write_var_BitField_offset15_count8
	.align 4
	.global bitfield__read_var_BitField_offset15_count9
	.hidden	bitfield__read_var_BitField_offset15_count9
	.type	bitfield__read_var_BitField_offset15_count9, #function
	.proc	04
bitfield__read_var_BitField_offset15_count9:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 15, %g1
	srl	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset15_count9, .-bitfield__read_var_BitField_offset15_count9
	.align 4
	.global bitfield__write_var_BitField_offset15_count9
	.hidden	bitfield__write_var_BitField_offset15_count9
	.type	bitfield__write_var_BitField_offset15_count9, #function
	.proc	020
bitfield__write_var_BitField_offset15_count9:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2109
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 511
	ble	%icc, .LL2110
	 nop
.LL2109:
	sethi	%hi(91136), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2110:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-131072), %g1
	or	%g1, 255, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 511, %g1
	sll	%g1, 8, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset15_count9, .-bitfield__write_var_BitField_offset15_count9
	.align 4
	.global bitfield__read_var_BitField_offset15_count10
	.hidden	bitfield__read_var_BitField_offset15_count10
	.type	bitfield__read_var_BitField_offset15_count10, #function
	.proc	04
bitfield__read_var_BitField_offset15_count10:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 15, %g1
	srl	%g1, 22, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset15_count10, .-bitfield__read_var_BitField_offset15_count10
	.align 4
	.global bitfield__write_var_BitField_offset15_count10
	.hidden	bitfield__write_var_BitField_offset15_count10
	.type	bitfield__write_var_BitField_offset15_count10, #function
	.proc	020
bitfield__write_var_BitField_offset15_count10:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2115
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1023
	ble	%icc, .LL2116
	 nop
.LL2115:
	sethi	%hi(91136), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL2116:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-131072), %g1
	or	%g1, 127, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 1023, %g1
	sll	%g1, 7, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset15_count10, .-bitfield__write_var_BitField_offset15_count10
	.align 4
	.global bitfield__read_var_BitField_offset15_count11
	.hidden	bitfield__read_var_BitField_offset15_count11
	.type	bitfield__read_var_BitField_offset15_count11, #function
	.proc	04
bitfield__read_var_BitField_offset15_count11:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 15, %g1
	srl	%g1, 21, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset15_count11, .-bitfield__read_var_BitField_offset15_count11
	.align 4
	.global bitfield__write_var_BitField_offset15_count11
	.hidden	bitfield__write_var_BitField_offset15_count11
	.type	bitfield__write_var_BitField_offset15_count11, #function
	.proc	020
bitfield__write_var_BitField_offset15_count11:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2121
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 2047
	ble	%icc, .LL2122
	 nop
.LL2121:
	sethi	%hi(92160), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2122:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-131072), %g1
	or	%g1, 63, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 2047, %g1
	sll	%g1, 6, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset15_count11, .-bitfield__write_var_BitField_offset15_count11
	.align 4
	.global bitfield__read_var_BitField_offset15_count12
	.hidden	bitfield__read_var_BitField_offset15_count12
	.type	bitfield__read_var_BitField_offset15_count12, #function
	.proc	04
bitfield__read_var_BitField_offset15_count12:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 15, %g1
	srl	%g1, 20, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset15_count12, .-bitfield__read_var_BitField_offset15_count12
	.align 4
	.global bitfield__write_var_BitField_offset15_count12
	.hidden	bitfield__write_var_BitField_offset15_count12
	.type	bitfield__write_var_BitField_offset15_count12, #function
	.proc	020
bitfield__write_var_BitField_offset15_count12:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2127
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 4095
	ble	%icc, .LL2128
	 nop
.LL2127:
	sethi	%hi(92160), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2128:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g3
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-131072), %g1
	or	%g1, 31, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 4095, %g1
	sll	%g1, 5, %g1
	or	%g2, %g1, %g1
	st	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset15_count12, .-bitfield__write_var_BitField_offset15_count12
	.align 4
	.global bitfield__read_var_BitField_offset15_count13
	.hidden	bitfield__read_var_BitField_offset15_count13
	.type	bitfield__read_var_BitField_offset15_count13, #function
	.proc	04
bitfield__read_var_BitField_offset15_count13:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 15, %g1
	srl	%g1, 19, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset15_count13, .-bitfield__read_var_BitField_offset15_count13
	.align 4
	.global bitfield__write_var_BitField_offset15_count13
	.hidden	bitfield__write_var_BitField_offset15_count13
	.type	bitfield__write_var_BitField_offset15_count13, #function
	.proc	020
bitfield__write_var_BitField_offset15_count13:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2133
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(7168), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL2134
	 nop
.LL2133:
	sethi	%hi(92160), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2134:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-131072), %g1
	or	%g1, 15, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 4, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset15_count13, .-bitfield__write_var_BitField_offset15_count13
	.align 4
	.global bitfield__read_var_BitField_offset15_count14
	.hidden	bitfield__read_var_BitField_offset15_count14
	.type	bitfield__read_var_BitField_offset15_count14, #function
	.proc	04
bitfield__read_var_BitField_offset15_count14:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 15, %g1
	srl	%g1, 18, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset15_count14, .-bitfield__read_var_BitField_offset15_count14
	.align 4
	.global bitfield__write_var_BitField_offset15_count14
	.hidden	bitfield__write_var_BitField_offset15_count14
	.type	bitfield__write_var_BitField_offset15_count14, #function
	.proc	020
bitfield__write_var_BitField_offset15_count14:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2139
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(15360), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL2140
	 nop
.LL2139:
	sethi	%hi(92160), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL2140:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-131072), %g1
	or	%g1, 7, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 3, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset15_count14, .-bitfield__write_var_BitField_offset15_count14
	.align 4
	.global bitfield__read_var_BitField_offset15_count15
	.hidden	bitfield__read_var_BitField_offset15_count15
	.type	bitfield__read_var_BitField_offset15_count15, #function
	.proc	04
bitfield__read_var_BitField_offset15_count15:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 15, %g1
	srl	%g1, 17, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset15_count15, .-bitfield__read_var_BitField_offset15_count15
	.align 4
	.global bitfield__write_var_BitField_offset15_count15
	.hidden	bitfield__write_var_BitField_offset15_count15
	.type	bitfield__write_var_BitField_offset15_count15, #function
	.proc	020
bitfield__write_var_BitField_offset15_count15:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2145
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(31744), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL2146
	 nop
.LL2145:
	sethi	%hi(93184), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2146:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-131072), %g1
	or	%g1, 3, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 2, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset15_count15, .-bitfield__write_var_BitField_offset15_count15
	.align 4
	.global bitfield__read_var_BitField_offset15_count16
	.hidden	bitfield__read_var_BitField_offset15_count16
	.type	bitfield__read_var_BitField_offset15_count16, #function
	.proc	04
bitfield__read_var_BitField_offset15_count16:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 15, %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset15_count16, .-bitfield__read_var_BitField_offset15_count16
	.align 4
	.global bitfield__write_var_BitField_offset15_count16
	.hidden	bitfield__write_var_BitField_offset15_count16
	.type	bitfield__write_var_BitField_offset15_count16, #function
	.proc	020
bitfield__write_var_BitField_offset15_count16:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2151
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(64512), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL2152
	 nop
.LL2151:
	sethi	%hi(93184), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2152:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-131072), %g1
	or	%g1, 1, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	add	%g1, %g1, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset15_count16, .-bitfield__write_var_BitField_offset15_count16
	.align 4
	.global bitfield__read_var_BitField_offset15_count17
	.hidden	bitfield__read_var_BitField_offset15_count17
	.type	bitfield__read_var_BitField_offset15_count17, #function
	.proc	04
bitfield__read_var_BitField_offset15_count17:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(130048), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset15_count17, .-bitfield__read_var_BitField_offset15_count17
	.align 4
	.global bitfield__write_var_BitField_offset15_count17
	.hidden	bitfield__write_var_BitField_offset15_count17
	.type	bitfield__write_var_BitField_offset15_count17, #function
	.proc	020
bitfield__write_var_BitField_offset15_count17:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2157
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(130048), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL2158
	 nop
.LL2157:
	sethi	%hi(93184), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2158:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	mov	%g1, %g4
	ld	[%fp-20], %g1
	ld	[%g1], %g1
	mov	%g1, %g2
	sethi	%hi(-131072), %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(130048), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	or	%g3, %g1, %g1
	st	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset15_count17, .-bitfield__write_var_BitField_offset15_count17
	.align 4
	.global bitfield__read_var_BitField_offset16_count1
	.hidden	bitfield__read_var_BitField_offset16_count1
	.type	bitfield__read_var_BitField_offset16_count1, #function
	.proc	04
bitfield__read_var_BitField_offset16_count1:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sra	%g1, 8, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 24, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset16_count1, .-bitfield__read_var_BitField_offset16_count1
	.align 4
	.global bitfield__write_var_BitField_offset16_count1
	.hidden	bitfield__write_var_BitField_offset16_count1
	.type	bitfield__write_var_BitField_offset16_count1, #function
	.proc	020
bitfield__write_var_BitField_offset16_count1:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2163
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1
	ble	%icc, .LL2164
	 nop
.LL2163:
	sethi	%hi(93184), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL2164:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -129, %g2
	ld	[%fp-24], %g1
	sll	%g1, 7, %g1
	and	%g1, 255, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset16_count1, .-bitfield__write_var_BitField_offset16_count1
	.align 4
	.global bitfield__read_var_BitField_offset16_count2
	.hidden	bitfield__read_var_BitField_offset16_count2
	.type	bitfield__read_var_BitField_offset16_count2, #function
	.proc	04
bitfield__read_var_BitField_offset16_count2:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sra	%g1, 8, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 24, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset16_count2, .-bitfield__read_var_BitField_offset16_count2
	.align 4
	.global bitfield__write_var_BitField_offset16_count2
	.hidden	bitfield__write_var_BitField_offset16_count2
	.type	bitfield__write_var_BitField_offset16_count2, #function
	.proc	020
bitfield__write_var_BitField_offset16_count2:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2169
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 3
	ble	%icc, .LL2170
	 nop
.LL2169:
	sethi	%hi(94208), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2170:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -193, %g2
	ld	[%fp-24], %g1
	sll	%g1, 6, %g1
	and	%g1, 255, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset16_count2, .-bitfield__write_var_BitField_offset16_count2
	.align 4
	.global bitfield__read_var_BitField_offset16_count3
	.hidden	bitfield__read_var_BitField_offset16_count3
	.type	bitfield__read_var_BitField_offset16_count3, #function
	.proc	04
bitfield__read_var_BitField_offset16_count3:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sra	%g1, 8, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 24, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset16_count3, .-bitfield__read_var_BitField_offset16_count3
	.align 4
	.global bitfield__write_var_BitField_offset16_count3
	.hidden	bitfield__write_var_BitField_offset16_count3
	.type	bitfield__write_var_BitField_offset16_count3, #function
	.proc	020
bitfield__write_var_BitField_offset16_count3:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2175
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 7
	ble	%icc, .LL2176
	 nop
.LL2175:
	sethi	%hi(94208), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2176:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -225, %g2
	ld	[%fp-24], %g1
	sll	%g1, 5, %g1
	and	%g1, 255, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset16_count3, .-bitfield__write_var_BitField_offset16_count3
	.align 4
	.global bitfield__read_var_BitField_offset16_count4
	.hidden	bitfield__read_var_BitField_offset16_count4
	.type	bitfield__read_var_BitField_offset16_count4, #function
	.proc	04
bitfield__read_var_BitField_offset16_count4:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sra	%g1, 8, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 24, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset16_count4, .-bitfield__read_var_BitField_offset16_count4
	.align 4
	.global bitfield__write_var_BitField_offset16_count4
	.hidden	bitfield__write_var_BitField_offset16_count4
	.type	bitfield__write_var_BitField_offset16_count4, #function
	.proc	020
bitfield__write_var_BitField_offset16_count4:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2181
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 15
	ble	%icc, .LL2182
	 nop
.LL2181:
	sethi	%hi(94208), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2182:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -241, %g2
	ld	[%fp-24], %g1
	sll	%g1, 4, %g1
	and	%g1, 255, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset16_count4, .-bitfield__write_var_BitField_offset16_count4
	.align 4
	.global bitfield__read_var_BitField_offset16_count5
	.hidden	bitfield__read_var_BitField_offset16_count5
	.type	bitfield__read_var_BitField_offset16_count5, #function
	.proc	04
bitfield__read_var_BitField_offset16_count5:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sra	%g1, 8, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 24, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset16_count5, .-bitfield__read_var_BitField_offset16_count5
	.align 4
	.global bitfield__write_var_BitField_offset16_count5
	.hidden	bitfield__write_var_BitField_offset16_count5
	.type	bitfield__write_var_BitField_offset16_count5, #function
	.proc	020
bitfield__write_var_BitField_offset16_count5:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2187
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 31
	ble	%icc, .LL2188
	 nop
.LL2187:
	sethi	%hi(94208), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL2188:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -249, %g2
	ld	[%fp-24], %g1
	sll	%g1, 3, %g1
	and	%g1, 255, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset16_count5, .-bitfield__write_var_BitField_offset16_count5
	.align 4
	.global bitfield__read_var_BitField_offset16_count6
	.hidden	bitfield__read_var_BitField_offset16_count6
	.type	bitfield__read_var_BitField_offset16_count6, #function
	.proc	04
bitfield__read_var_BitField_offset16_count6:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sra	%g1, 8, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 24, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset16_count6, .-bitfield__read_var_BitField_offset16_count6
	.align 4
	.global bitfield__write_var_BitField_offset16_count6
	.hidden	bitfield__write_var_BitField_offset16_count6
	.type	bitfield__write_var_BitField_offset16_count6, #function
	.proc	020
bitfield__write_var_BitField_offset16_count6:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2193
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 63
	ble	%icc, .LL2194
	 nop
.LL2193:
	sethi	%hi(95232), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2194:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -253, %g2
	ld	[%fp-24], %g1
	sll	%g1, 2, %g1
	and	%g1, 255, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset16_count6, .-bitfield__write_var_BitField_offset16_count6
	.align 4
	.global bitfield__read_var_BitField_offset16_count7
	.hidden	bitfield__read_var_BitField_offset16_count7
	.type	bitfield__read_var_BitField_offset16_count7, #function
	.proc	04
bitfield__read_var_BitField_offset16_count7:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sra	%g1, 8, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 24, %g1
	srl	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset16_count7, .-bitfield__read_var_BitField_offset16_count7
	.align 4
	.global bitfield__write_var_BitField_offset16_count7
	.hidden	bitfield__write_var_BitField_offset16_count7
	.type	bitfield__write_var_BitField_offset16_count7, #function
	.proc	020
bitfield__write_var_BitField_offset16_count7:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2199
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 127
	ble	%icc, .LL2200
	 nop
.LL2199:
	sethi	%hi(95232), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2200:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -255, %g2
	ld	[%fp-24], %g1
	add	%g1, %g1, %g1
	and	%g1, 255, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset16_count7, .-bitfield__write_var_BitField_offset16_count7
	.align 4
	.global bitfield__read_var_BitField_offset16_count8
	.hidden	bitfield__read_var_BitField_offset16_count8
	.type	bitfield__read_var_BitField_offset16_count8, #function
	.proc	04
bitfield__read_var_BitField_offset16_count8:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sra	%g1, 8, %g1
	and	%g1, 0xff, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset16_count8, .-bitfield__read_var_BitField_offset16_count8
	.align 4
	.global bitfield__write_var_BitField_offset16_count8
	.hidden	bitfield__write_var_BitField_offset16_count8
	.type	bitfield__write_var_BitField_offset16_count8, #function
	.proc	020
bitfield__write_var_BitField_offset16_count8:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	cmp	%g1, 0
	bl	%icc, .LL2205
	 nop
	ld	[%fp-20], %g1
	cmp	%g1, 255
	ble	%icc, .LL2206
	 nop
.LL2205:
	sethi	%hi(95232), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2206:
	ld	[%fp+68], %g1
	add	%g1, 2, %g2
	ld	[%fp-20], %g1
	stb	%g1, [%g2]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset16_count8, .-bitfield__write_var_BitField_offset16_count8
	.align 4
	.global bitfield__read_var_BitField_offset16_count9
	.hidden	bitfield__read_var_BitField_offset16_count9
	.type	bitfield__read_var_BitField_offset16_count9, #function
	.proc	04
bitfield__read_var_BitField_offset16_count9:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset16_count9, .-bitfield__read_var_BitField_offset16_count9
	.align 4
	.global bitfield__write_var_BitField_offset16_count9
	.hidden	bitfield__write_var_BitField_offset16_count9
	.type	bitfield__write_var_BitField_offset16_count9, #function
	.proc	020
bitfield__write_var_BitField_offset16_count9:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2211
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 511
	ble	%icc, .LL2212
	 nop
.LL2211:
	sethi	%hi(95232), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL2212:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g4
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-65536), %g1
	or	%g1, 127, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g1
	sll	%g1, 7, %g2
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	or	%g3, %g1, %g1
	sth	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset16_count9, .-bitfield__write_var_BitField_offset16_count9
	.align 4
	.global bitfield__read_var_BitField_offset16_count10
	.hidden	bitfield__read_var_BitField_offset16_count10
	.type	bitfield__read_var_BitField_offset16_count10, #function
	.proc	04
bitfield__read_var_BitField_offset16_count10:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 22, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset16_count10, .-bitfield__read_var_BitField_offset16_count10
	.align 4
	.global bitfield__write_var_BitField_offset16_count10
	.hidden	bitfield__write_var_BitField_offset16_count10
	.type	bitfield__write_var_BitField_offset16_count10, #function
	.proc	020
bitfield__write_var_BitField_offset16_count10:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2217
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1023
	ble	%icc, .LL2218
	 nop
.LL2217:
	sethi	%hi(96256), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2218:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g4
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-65536), %g1
	or	%g1, 63, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g1
	sll	%g1, 6, %g2
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	or	%g3, %g1, %g1
	sth	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset16_count10, .-bitfield__write_var_BitField_offset16_count10
	.align 4
	.global bitfield__read_var_BitField_offset16_count11
	.hidden	bitfield__read_var_BitField_offset16_count11
	.type	bitfield__read_var_BitField_offset16_count11, #function
	.proc	04
bitfield__read_var_BitField_offset16_count11:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 21, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset16_count11, .-bitfield__read_var_BitField_offset16_count11
	.align 4
	.global bitfield__write_var_BitField_offset16_count11
	.hidden	bitfield__write_var_BitField_offset16_count11
	.type	bitfield__write_var_BitField_offset16_count11, #function
	.proc	020
bitfield__write_var_BitField_offset16_count11:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2223
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 2047
	ble	%icc, .LL2224
	 nop
.LL2223:
	sethi	%hi(96256), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2224:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g4
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-65536), %g1
	or	%g1, 31, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g1
	sll	%g1, 5, %g2
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	or	%g3, %g1, %g1
	sth	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset16_count11, .-bitfield__write_var_BitField_offset16_count11
	.align 4
	.global bitfield__read_var_BitField_offset16_count12
	.hidden	bitfield__read_var_BitField_offset16_count12
	.type	bitfield__read_var_BitField_offset16_count12, #function
	.proc	04
bitfield__read_var_BitField_offset16_count12:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 20, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset16_count12, .-bitfield__read_var_BitField_offset16_count12
	.align 4
	.global bitfield__write_var_BitField_offset16_count12
	.hidden	bitfield__write_var_BitField_offset16_count12
	.type	bitfield__write_var_BitField_offset16_count12, #function
	.proc	020
bitfield__write_var_BitField_offset16_count12:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2229
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 4095
	ble	%icc, .LL2230
	 nop
.LL2229:
	sethi	%hi(96256), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2230:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g4
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-65536), %g1
	or	%g1, 15, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g1
	sll	%g1, 4, %g2
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	or	%g3, %g1, %g1
	sth	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset16_count12, .-bitfield__write_var_BitField_offset16_count12
	.align 4
	.global bitfield__read_var_BitField_offset16_count13
	.hidden	bitfield__read_var_BitField_offset16_count13
	.type	bitfield__read_var_BitField_offset16_count13, #function
	.proc	04
bitfield__read_var_BitField_offset16_count13:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 19, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset16_count13, .-bitfield__read_var_BitField_offset16_count13
	.align 4
	.global bitfield__write_var_BitField_offset16_count13
	.hidden	bitfield__write_var_BitField_offset16_count13
	.type	bitfield__write_var_BitField_offset16_count13, #function
	.proc	020
bitfield__write_var_BitField_offset16_count13:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2235
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(7168), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL2236
	 nop
.LL2235:
	sethi	%hi(96256), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL2236:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g4
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-65536), %g1
	or	%g1, 7, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g1
	sll	%g1, 3, %g2
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	or	%g3, %g1, %g1
	sth	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset16_count13, .-bitfield__write_var_BitField_offset16_count13
	.align 4
	.global bitfield__read_var_BitField_offset16_count14
	.hidden	bitfield__read_var_BitField_offset16_count14
	.type	bitfield__read_var_BitField_offset16_count14, #function
	.proc	04
bitfield__read_var_BitField_offset16_count14:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 18, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset16_count14, .-bitfield__read_var_BitField_offset16_count14
	.align 4
	.global bitfield__write_var_BitField_offset16_count14
	.hidden	bitfield__write_var_BitField_offset16_count14
	.type	bitfield__write_var_BitField_offset16_count14, #function
	.proc	020
bitfield__write_var_BitField_offset16_count14:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2241
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(15360), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL2242
	 nop
.LL2241:
	sethi	%hi(97280), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2242:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g4
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-65536), %g1
	or	%g1, 3, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g1
	sll	%g1, 2, %g2
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	or	%g3, %g1, %g1
	sth	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset16_count14, .-bitfield__write_var_BitField_offset16_count14
	.align 4
	.global bitfield__read_var_BitField_offset16_count15
	.hidden	bitfield__read_var_BitField_offset16_count15
	.type	bitfield__read_var_BitField_offset16_count15, #function
	.proc	04
bitfield__read_var_BitField_offset16_count15:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 17, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset16_count15, .-bitfield__read_var_BitField_offset16_count15
	.align 4
	.global bitfield__write_var_BitField_offset16_count15
	.hidden	bitfield__write_var_BitField_offset16_count15
	.type	bitfield__write_var_BitField_offset16_count15, #function
	.proc	020
bitfield__write_var_BitField_offset16_count15:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2247
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(31744), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL2248
	 nop
.LL2247:
	sethi	%hi(97280), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2248:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g4
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-65536), %g1
	or	%g1, 1, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g1
	add	%g1, %g1, %g1
	mov	%g1, %g2
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	or	%g3, %g1, %g1
	sth	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset16_count15, .-bitfield__write_var_BitField_offset16_count15
	.align 4
	.global bitfield__read_var_BitField_offset16_count16
	.hidden	bitfield__read_var_BitField_offset16_count16
	.type	bitfield__read_var_BitField_offset16_count16, #function
	.proc	04
bitfield__read_var_BitField_offset16_count16:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset16_count16, .-bitfield__read_var_BitField_offset16_count16
	.align 4
	.global bitfield__write_var_BitField_offset16_count16
	.hidden	bitfield__write_var_BitField_offset16_count16
	.type	bitfield__write_var_BitField_offset16_count16, #function
	.proc	020
bitfield__write_var_BitField_offset16_count16:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	cmp	%g1, 0
	bl	%icc, .LL2253
	 nop
	ld	[%fp-20], %g1
	sethi	%hi(64512), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL2254
	 nop
.LL2253:
	sethi	%hi(97280), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2254:
	ld	[%fp+68], %g1
	add	%g1, 2, %g2
	ld	[%fp-20], %g1
	sth	%g1, [%g2]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset16_count16, .-bitfield__write_var_BitField_offset16_count16
	.align 4
	.global bitfield__read_var_BitField_offset17_count1
	.hidden	bitfield__read_var_BitField_offset17_count1
	.type	bitfield__read_var_BitField_offset17_count1, #function
	.proc	04
bitfield__read_var_BitField_offset17_count1:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sra	%g1, 8, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset17_count1, .-bitfield__read_var_BitField_offset17_count1
	.align 4
	.global bitfield__write_var_BitField_offset17_count1
	.hidden	bitfield__write_var_BitField_offset17_count1
	.type	bitfield__write_var_BitField_offset17_count1, #function
	.proc	020
bitfield__write_var_BitField_offset17_count1:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2259
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1
	ble	%icc, .LL2260
	 nop
.LL2259:
	sethi	%hi(97280), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL2260:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -65, %g2
	ld	[%fp-24], %g1
	and	%g1, 1, %g1
	sll	%g1, 6, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset17_count1, .-bitfield__write_var_BitField_offset17_count1
	.align 4
	.global bitfield__read_var_BitField_offset17_count2
	.hidden	bitfield__read_var_BitField_offset17_count2
	.type	bitfield__read_var_BitField_offset17_count2, #function
	.proc	04
bitfield__read_var_BitField_offset17_count2:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sra	%g1, 8, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset17_count2, .-bitfield__read_var_BitField_offset17_count2
	.align 4
	.global bitfield__write_var_BitField_offset17_count2
	.hidden	bitfield__write_var_BitField_offset17_count2
	.type	bitfield__write_var_BitField_offset17_count2, #function
	.proc	020
bitfield__write_var_BitField_offset17_count2:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2265
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 3
	ble	%icc, .LL2266
	 nop
.LL2265:
	sethi	%hi(98304), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2266:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -97, %g2
	ld	[%fp-24], %g1
	and	%g1, 3, %g1
	sll	%g1, 5, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset17_count2, .-bitfield__write_var_BitField_offset17_count2
	.align 4
	.global bitfield__read_var_BitField_offset17_count3
	.hidden	bitfield__read_var_BitField_offset17_count3
	.type	bitfield__read_var_BitField_offset17_count3, #function
	.proc	04
bitfield__read_var_BitField_offset17_count3:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sra	%g1, 8, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset17_count3, .-bitfield__read_var_BitField_offset17_count3
	.align 4
	.global bitfield__write_var_BitField_offset17_count3
	.hidden	bitfield__write_var_BitField_offset17_count3
	.type	bitfield__write_var_BitField_offset17_count3, #function
	.proc	020
bitfield__write_var_BitField_offset17_count3:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2271
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 7
	ble	%icc, .LL2272
	 nop
.LL2271:
	sethi	%hi(98304), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2272:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -113, %g2
	ld	[%fp-24], %g1
	and	%g1, 7, %g1
	sll	%g1, 4, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset17_count3, .-bitfield__write_var_BitField_offset17_count3
	.align 4
	.global bitfield__read_var_BitField_offset17_count4
	.hidden	bitfield__read_var_BitField_offset17_count4
	.type	bitfield__read_var_BitField_offset17_count4, #function
	.proc	04
bitfield__read_var_BitField_offset17_count4:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sra	%g1, 8, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset17_count4, .-bitfield__read_var_BitField_offset17_count4
	.align 4
	.global bitfield__write_var_BitField_offset17_count4
	.hidden	bitfield__write_var_BitField_offset17_count4
	.type	bitfield__write_var_BitField_offset17_count4, #function
	.proc	020
bitfield__write_var_BitField_offset17_count4:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2277
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 15
	ble	%icc, .LL2278
	 nop
.LL2277:
	sethi	%hi(98304), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2278:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -121, %g2
	ld	[%fp-24], %g1
	and	%g1, 15, %g1
	sll	%g1, 3, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset17_count4, .-bitfield__write_var_BitField_offset17_count4
	.align 4
	.global bitfield__read_var_BitField_offset17_count5
	.hidden	bitfield__read_var_BitField_offset17_count5
	.type	bitfield__read_var_BitField_offset17_count5, #function
	.proc	04
bitfield__read_var_BitField_offset17_count5:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sra	%g1, 8, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset17_count5, .-bitfield__read_var_BitField_offset17_count5
	.align 4
	.global bitfield__write_var_BitField_offset17_count5
	.hidden	bitfield__write_var_BitField_offset17_count5
	.type	bitfield__write_var_BitField_offset17_count5, #function
	.proc	020
bitfield__write_var_BitField_offset17_count5:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2283
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 31
	ble	%icc, .LL2284
	 nop
.LL2283:
	sethi	%hi(98304), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL2284:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -125, %g2
	ld	[%fp-24], %g1
	and	%g1, 31, %g1
	sll	%g1, 2, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset17_count5, .-bitfield__write_var_BitField_offset17_count5
	.align 4
	.global bitfield__read_var_BitField_offset17_count6
	.hidden	bitfield__read_var_BitField_offset17_count6
	.type	bitfield__read_var_BitField_offset17_count6, #function
	.proc	04
bitfield__read_var_BitField_offset17_count6:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sra	%g1, 8, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset17_count6, .-bitfield__read_var_BitField_offset17_count6
	.align 4
	.global bitfield__write_var_BitField_offset17_count6
	.hidden	bitfield__write_var_BitField_offset17_count6
	.type	bitfield__write_var_BitField_offset17_count6, #function
	.proc	020
bitfield__write_var_BitField_offset17_count6:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2289
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 63
	ble	%icc, .LL2290
	 nop
.LL2289:
	sethi	%hi(99328), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2290:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -127, %g2
	ld	[%fp-24], %g1
	and	%g1, 63, %g1
	add	%g1, %g1, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset17_count6, .-bitfield__write_var_BitField_offset17_count6
	.align 4
	.global bitfield__read_var_BitField_offset17_count7
	.hidden	bitfield__read_var_BitField_offset17_count7
	.type	bitfield__read_var_BitField_offset17_count7, #function
	.proc	04
bitfield__read_var_BitField_offset17_count7:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sra	%g1, 8, %g1
	and	%g1, 0xff, %g1
	and	%g1, 127, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset17_count7, .-bitfield__read_var_BitField_offset17_count7
	.align 4
	.global bitfield__write_var_BitField_offset17_count7
	.hidden	bitfield__write_var_BitField_offset17_count7
	.type	bitfield__write_var_BitField_offset17_count7, #function
	.proc	020
bitfield__write_var_BitField_offset17_count7:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2295
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 127
	ble	%icc, .LL2296
	 nop
.LL2295:
	sethi	%hi(99328), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2296:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -128, %g2
	ld	[%fp-24], %g1
	and	%g1, 127, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset17_count7, .-bitfield__write_var_BitField_offset17_count7
	.align 4
	.global bitfield__read_var_BitField_offset17_count8
	.hidden	bitfield__read_var_BitField_offset17_count8
	.type	bitfield__read_var_BitField_offset17_count8, #function
	.proc	04
bitfield__read_var_BitField_offset17_count8:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 17, %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset17_count8, .-bitfield__read_var_BitField_offset17_count8
	.align 4
	.global bitfield__write_var_BitField_offset17_count8
	.hidden	bitfield__write_var_BitField_offset17_count8
	.type	bitfield__write_var_BitField_offset17_count8, #function
	.proc	020
bitfield__write_var_BitField_offset17_count8:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2301
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 255
	ble	%icc, .LL2302
	 nop
.LL2301:
	sethi	%hi(99328), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2302:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-32768), %g1
	or	%g1, 127, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 255, %g1
	sll	%g1, 7, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset17_count8, .-bitfield__write_var_BitField_offset17_count8
	.align 4
	.global bitfield__read_var_BitField_offset17_count9
	.hidden	bitfield__read_var_BitField_offset17_count9
	.type	bitfield__read_var_BitField_offset17_count9, #function
	.proc	04
bitfield__read_var_BitField_offset17_count9:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 17, %g1
	srl	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset17_count9, .-bitfield__read_var_BitField_offset17_count9
	.align 4
	.global bitfield__write_var_BitField_offset17_count9
	.hidden	bitfield__write_var_BitField_offset17_count9
	.type	bitfield__write_var_BitField_offset17_count9, #function
	.proc	020
bitfield__write_var_BitField_offset17_count9:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2307
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 511
	ble	%icc, .LL2308
	 nop
.LL2307:
	sethi	%hi(99328), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL2308:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-32768), %g1
	or	%g1, 63, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 511, %g1
	sll	%g1, 6, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset17_count9, .-bitfield__write_var_BitField_offset17_count9
	.align 4
	.global bitfield__read_var_BitField_offset17_count10
	.hidden	bitfield__read_var_BitField_offset17_count10
	.type	bitfield__read_var_BitField_offset17_count10, #function
	.proc	04
bitfield__read_var_BitField_offset17_count10:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 17, %g1
	srl	%g1, 22, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset17_count10, .-bitfield__read_var_BitField_offset17_count10
	.align 4
	.global bitfield__write_var_BitField_offset17_count10
	.hidden	bitfield__write_var_BitField_offset17_count10
	.type	bitfield__write_var_BitField_offset17_count10, #function
	.proc	020
bitfield__write_var_BitField_offset17_count10:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2313
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1023
	ble	%icc, .LL2314
	 nop
.LL2313:
	sethi	%hi(100352), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2314:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-32768), %g1
	or	%g1, 31, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 1023, %g1
	sll	%g1, 5, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset17_count10, .-bitfield__write_var_BitField_offset17_count10
	.align 4
	.global bitfield__read_var_BitField_offset17_count11
	.hidden	bitfield__read_var_BitField_offset17_count11
	.type	bitfield__read_var_BitField_offset17_count11, #function
	.proc	04
bitfield__read_var_BitField_offset17_count11:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 17, %g1
	srl	%g1, 21, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset17_count11, .-bitfield__read_var_BitField_offset17_count11
	.align 4
	.global bitfield__write_var_BitField_offset17_count11
	.hidden	bitfield__write_var_BitField_offset17_count11
	.type	bitfield__write_var_BitField_offset17_count11, #function
	.proc	020
bitfield__write_var_BitField_offset17_count11:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2319
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 2047
	ble	%icc, .LL2320
	 nop
.LL2319:
	sethi	%hi(100352), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2320:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-32768), %g1
	or	%g1, 15, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 2047, %g1
	sll	%g1, 4, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset17_count11, .-bitfield__write_var_BitField_offset17_count11
	.align 4
	.global bitfield__read_var_BitField_offset17_count12
	.hidden	bitfield__read_var_BitField_offset17_count12
	.type	bitfield__read_var_BitField_offset17_count12, #function
	.proc	04
bitfield__read_var_BitField_offset17_count12:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 17, %g1
	srl	%g1, 20, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset17_count12, .-bitfield__read_var_BitField_offset17_count12
	.align 4
	.global bitfield__write_var_BitField_offset17_count12
	.hidden	bitfield__write_var_BitField_offset17_count12
	.type	bitfield__write_var_BitField_offset17_count12, #function
	.proc	020
bitfield__write_var_BitField_offset17_count12:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2325
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 4095
	ble	%icc, .LL2326
	 nop
.LL2325:
	sethi	%hi(100352), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2326:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-32768), %g1
	or	%g1, 7, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 4095, %g1
	sll	%g1, 3, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset17_count12, .-bitfield__write_var_BitField_offset17_count12
	.align 4
	.global bitfield__read_var_BitField_offset17_count13
	.hidden	bitfield__read_var_BitField_offset17_count13
	.type	bitfield__read_var_BitField_offset17_count13, #function
	.proc	04
bitfield__read_var_BitField_offset17_count13:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 17, %g1
	srl	%g1, 19, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset17_count13, .-bitfield__read_var_BitField_offset17_count13
	.align 4
	.global bitfield__write_var_BitField_offset17_count13
	.hidden	bitfield__write_var_BitField_offset17_count13
	.type	bitfield__write_var_BitField_offset17_count13, #function
	.proc	020
bitfield__write_var_BitField_offset17_count13:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2331
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(7168), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL2332
	 nop
.LL2331:
	sethi	%hi(100352), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL2332:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g4
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-32768), %g1
	or	%g1, 3, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	sll	%g1, 2, %g1
	or	%g3, %g1, %g1
	sth	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset17_count13, .-bitfield__write_var_BitField_offset17_count13
	.align 4
	.global bitfield__read_var_BitField_offset17_count14
	.hidden	bitfield__read_var_BitField_offset17_count14
	.type	bitfield__read_var_BitField_offset17_count14, #function
	.proc	04
bitfield__read_var_BitField_offset17_count14:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 17, %g1
	srl	%g1, 18, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset17_count14, .-bitfield__read_var_BitField_offset17_count14
	.align 4
	.global bitfield__write_var_BitField_offset17_count14
	.hidden	bitfield__write_var_BitField_offset17_count14
	.type	bitfield__write_var_BitField_offset17_count14, #function
	.proc	020
bitfield__write_var_BitField_offset17_count14:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2337
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(15360), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL2338
	 nop
.LL2337:
	sethi	%hi(101376), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2338:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g4
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-32768), %g1
	or	%g1, 1, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	add	%g1, %g1, %g1
	or	%g3, %g1, %g1
	sth	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset17_count14, .-bitfield__write_var_BitField_offset17_count14
	.align 4
	.global bitfield__read_var_BitField_offset17_count15
	.hidden	bitfield__read_var_BitField_offset17_count15
	.type	bitfield__read_var_BitField_offset17_count15, #function
	.proc	04
bitfield__read_var_BitField_offset17_count15:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset17_count15, .-bitfield__read_var_BitField_offset17_count15
	.align 4
	.global bitfield__write_var_BitField_offset17_count15
	.hidden	bitfield__write_var_BitField_offset17_count15
	.type	bitfield__write_var_BitField_offset17_count15, #function
	.proc	020
bitfield__write_var_BitField_offset17_count15:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2343
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(31744), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL2344
	 nop
.LL2343:
	sethi	%hi(101376), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2344:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g4
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-32768), %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	or	%g3, %g1, %g1
	sth	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset17_count15, .-bitfield__write_var_BitField_offset17_count15
	.align 4
	.global bitfield__read_var_BitField_offset18_count1
	.hidden	bitfield__read_var_BitField_offset18_count1
	.type	bitfield__read_var_BitField_offset18_count1, #function
	.proc	04
bitfield__read_var_BitField_offset18_count1:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sra	%g1, 8, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 26, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset18_count1, .-bitfield__read_var_BitField_offset18_count1
	.align 4
	.global bitfield__write_var_BitField_offset18_count1
	.hidden	bitfield__write_var_BitField_offset18_count1
	.type	bitfield__write_var_BitField_offset18_count1, #function
	.proc	020
bitfield__write_var_BitField_offset18_count1:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2349
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1
	ble	%icc, .LL2350
	 nop
.LL2349:
	sethi	%hi(101376), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2350:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -33, %g2
	ld	[%fp-24], %g1
	and	%g1, 1, %g1
	sll	%g1, 5, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset18_count1, .-bitfield__write_var_BitField_offset18_count1
	.align 4
	.global bitfield__read_var_BitField_offset18_count2
	.hidden	bitfield__read_var_BitField_offset18_count2
	.type	bitfield__read_var_BitField_offset18_count2, #function
	.proc	04
bitfield__read_var_BitField_offset18_count2:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sra	%g1, 8, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 26, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset18_count2, .-bitfield__read_var_BitField_offset18_count2
	.align 4
	.global bitfield__write_var_BitField_offset18_count2
	.hidden	bitfield__write_var_BitField_offset18_count2
	.type	bitfield__write_var_BitField_offset18_count2, #function
	.proc	020
bitfield__write_var_BitField_offset18_count2:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2355
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 3
	ble	%icc, .LL2356
	 nop
.LL2355:
	sethi	%hi(101376), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL2356:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -49, %g2
	ld	[%fp-24], %g1
	and	%g1, 3, %g1
	sll	%g1, 4, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset18_count2, .-bitfield__write_var_BitField_offset18_count2
	.align 4
	.global bitfield__read_var_BitField_offset18_count3
	.hidden	bitfield__read_var_BitField_offset18_count3
	.type	bitfield__read_var_BitField_offset18_count3, #function
	.proc	04
bitfield__read_var_BitField_offset18_count3:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sra	%g1, 8, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 26, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset18_count3, .-bitfield__read_var_BitField_offset18_count3
	.align 4
	.global bitfield__write_var_BitField_offset18_count3
	.hidden	bitfield__write_var_BitField_offset18_count3
	.type	bitfield__write_var_BitField_offset18_count3, #function
	.proc	020
bitfield__write_var_BitField_offset18_count3:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2361
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 7
	ble	%icc, .LL2362
	 nop
.LL2361:
	sethi	%hi(102400), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2362:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -57, %g2
	ld	[%fp-24], %g1
	and	%g1, 7, %g1
	sll	%g1, 3, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset18_count3, .-bitfield__write_var_BitField_offset18_count3
	.align 4
	.global bitfield__read_var_BitField_offset18_count4
	.hidden	bitfield__read_var_BitField_offset18_count4
	.type	bitfield__read_var_BitField_offset18_count4, #function
	.proc	04
bitfield__read_var_BitField_offset18_count4:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sra	%g1, 8, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 26, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset18_count4, .-bitfield__read_var_BitField_offset18_count4
	.align 4
	.global bitfield__write_var_BitField_offset18_count4
	.hidden	bitfield__write_var_BitField_offset18_count4
	.type	bitfield__write_var_BitField_offset18_count4, #function
	.proc	020
bitfield__write_var_BitField_offset18_count4:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2367
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 15
	ble	%icc, .LL2368
	 nop
.LL2367:
	sethi	%hi(102400), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2368:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -61, %g2
	ld	[%fp-24], %g1
	and	%g1, 15, %g1
	sll	%g1, 2, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset18_count4, .-bitfield__write_var_BitField_offset18_count4
	.align 4
	.global bitfield__read_var_BitField_offset18_count5
	.hidden	bitfield__read_var_BitField_offset18_count5
	.type	bitfield__read_var_BitField_offset18_count5, #function
	.proc	04
bitfield__read_var_BitField_offset18_count5:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sra	%g1, 8, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 26, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset18_count5, .-bitfield__read_var_BitField_offset18_count5
	.align 4
	.global bitfield__write_var_BitField_offset18_count5
	.hidden	bitfield__write_var_BitField_offset18_count5
	.type	bitfield__write_var_BitField_offset18_count5, #function
	.proc	020
bitfield__write_var_BitField_offset18_count5:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2373
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 31
	ble	%icc, .LL2374
	 nop
.LL2373:
	sethi	%hi(102400), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2374:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -63, %g2
	ld	[%fp-24], %g1
	and	%g1, 31, %g1
	add	%g1, %g1, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset18_count5, .-bitfield__write_var_BitField_offset18_count5
	.align 4
	.global bitfield__read_var_BitField_offset18_count6
	.hidden	bitfield__read_var_BitField_offset18_count6
	.type	bitfield__read_var_BitField_offset18_count6, #function
	.proc	04
bitfield__read_var_BitField_offset18_count6:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sra	%g1, 8, %g1
	and	%g1, 0xff, %g1
	and	%g1, 63, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset18_count6, .-bitfield__read_var_BitField_offset18_count6
	.align 4
	.global bitfield__write_var_BitField_offset18_count6
	.hidden	bitfield__write_var_BitField_offset18_count6
	.type	bitfield__write_var_BitField_offset18_count6, #function
	.proc	020
bitfield__write_var_BitField_offset18_count6:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2379
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 63
	ble	%icc, .LL2380
	 nop
.LL2379:
	sethi	%hi(102400), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL2380:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -64, %g2
	ld	[%fp-24], %g1
	and	%g1, 63, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset18_count6, .-bitfield__write_var_BitField_offset18_count6
	.align 4
	.global bitfield__read_var_BitField_offset18_count7
	.hidden	bitfield__read_var_BitField_offset18_count7
	.type	bitfield__read_var_BitField_offset18_count7, #function
	.proc	04
bitfield__read_var_BitField_offset18_count7:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 18, %g1
	srl	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset18_count7, .-bitfield__read_var_BitField_offset18_count7
	.align 4
	.global bitfield__write_var_BitField_offset18_count7
	.hidden	bitfield__write_var_BitField_offset18_count7
	.type	bitfield__write_var_BitField_offset18_count7, #function
	.proc	020
bitfield__write_var_BitField_offset18_count7:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2385
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 127
	ble	%icc, .LL2386
	 nop
.LL2385:
	sethi	%hi(103424), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2386:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-16384), %g1
	or	%g1, 127, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 127, %g1
	sll	%g1, 7, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset18_count7, .-bitfield__write_var_BitField_offset18_count7
	.align 4
	.global bitfield__read_var_BitField_offset18_count8
	.hidden	bitfield__read_var_BitField_offset18_count8
	.type	bitfield__read_var_BitField_offset18_count8, #function
	.proc	04
bitfield__read_var_BitField_offset18_count8:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 18, %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset18_count8, .-bitfield__read_var_BitField_offset18_count8
	.align 4
	.global bitfield__write_var_BitField_offset18_count8
	.hidden	bitfield__write_var_BitField_offset18_count8
	.type	bitfield__write_var_BitField_offset18_count8, #function
	.proc	020
bitfield__write_var_BitField_offset18_count8:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2391
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 255
	ble	%icc, .LL2392
	 nop
.LL2391:
	sethi	%hi(103424), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2392:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-16384), %g1
	or	%g1, 63, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 255, %g1
	sll	%g1, 6, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset18_count8, .-bitfield__write_var_BitField_offset18_count8
	.align 4
	.global bitfield__read_var_BitField_offset18_count9
	.hidden	bitfield__read_var_BitField_offset18_count9
	.type	bitfield__read_var_BitField_offset18_count9, #function
	.proc	04
bitfield__read_var_BitField_offset18_count9:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 18, %g1
	srl	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset18_count9, .-bitfield__read_var_BitField_offset18_count9
	.align 4
	.global bitfield__write_var_BitField_offset18_count9
	.hidden	bitfield__write_var_BitField_offset18_count9
	.type	bitfield__write_var_BitField_offset18_count9, #function
	.proc	020
bitfield__write_var_BitField_offset18_count9:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2397
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 511
	ble	%icc, .LL2398
	 nop
.LL2397:
	sethi	%hi(103424), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2398:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-16384), %g1
	or	%g1, 31, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 511, %g1
	sll	%g1, 5, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset18_count9, .-bitfield__write_var_BitField_offset18_count9
	.align 4
	.global bitfield__read_var_BitField_offset18_count10
	.hidden	bitfield__read_var_BitField_offset18_count10
	.type	bitfield__read_var_BitField_offset18_count10, #function
	.proc	04
bitfield__read_var_BitField_offset18_count10:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 18, %g1
	srl	%g1, 22, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset18_count10, .-bitfield__read_var_BitField_offset18_count10
	.align 4
	.global bitfield__write_var_BitField_offset18_count10
	.hidden	bitfield__write_var_BitField_offset18_count10
	.type	bitfield__write_var_BitField_offset18_count10, #function
	.proc	020
bitfield__write_var_BitField_offset18_count10:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2403
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1023
	ble	%icc, .LL2404
	 nop
.LL2403:
	sethi	%hi(103424), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL2404:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-16384), %g1
	or	%g1, 15, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 1023, %g1
	sll	%g1, 4, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset18_count10, .-bitfield__write_var_BitField_offset18_count10
	.align 4
	.global bitfield__read_var_BitField_offset18_count11
	.hidden	bitfield__read_var_BitField_offset18_count11
	.type	bitfield__read_var_BitField_offset18_count11, #function
	.proc	04
bitfield__read_var_BitField_offset18_count11:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 18, %g1
	srl	%g1, 21, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset18_count11, .-bitfield__read_var_BitField_offset18_count11
	.align 4
	.global bitfield__write_var_BitField_offset18_count11
	.hidden	bitfield__write_var_BitField_offset18_count11
	.type	bitfield__write_var_BitField_offset18_count11, #function
	.proc	020
bitfield__write_var_BitField_offset18_count11:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2409
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 2047
	ble	%icc, .LL2410
	 nop
.LL2409:
	sethi	%hi(104448), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2410:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-16384), %g1
	or	%g1, 7, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 2047, %g1
	sll	%g1, 3, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset18_count11, .-bitfield__write_var_BitField_offset18_count11
	.align 4
	.global bitfield__read_var_BitField_offset18_count12
	.hidden	bitfield__read_var_BitField_offset18_count12
	.type	bitfield__read_var_BitField_offset18_count12, #function
	.proc	04
bitfield__read_var_BitField_offset18_count12:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 18, %g1
	srl	%g1, 20, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset18_count12, .-bitfield__read_var_BitField_offset18_count12
	.align 4
	.global bitfield__write_var_BitField_offset18_count12
	.hidden	bitfield__write_var_BitField_offset18_count12
	.type	bitfield__write_var_BitField_offset18_count12, #function
	.proc	020
bitfield__write_var_BitField_offset18_count12:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2415
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 4095
	ble	%icc, .LL2416
	 nop
.LL2415:
	sethi	%hi(104448), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2416:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-16384), %g1
	or	%g1, 3, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 4095, %g1
	sll	%g1, 2, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset18_count12, .-bitfield__write_var_BitField_offset18_count12
	.align 4
	.global bitfield__read_var_BitField_offset18_count13
	.hidden	bitfield__read_var_BitField_offset18_count13
	.type	bitfield__read_var_BitField_offset18_count13, #function
	.proc	04
bitfield__read_var_BitField_offset18_count13:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 18, %g1
	srl	%g1, 19, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset18_count13, .-bitfield__read_var_BitField_offset18_count13
	.align 4
	.global bitfield__write_var_BitField_offset18_count13
	.hidden	bitfield__write_var_BitField_offset18_count13
	.type	bitfield__write_var_BitField_offset18_count13, #function
	.proc	020
bitfield__write_var_BitField_offset18_count13:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2421
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(7168), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL2422
	 nop
.LL2421:
	sethi	%hi(104448), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2422:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g4
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-16384), %g1
	or	%g1, 1, %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	add	%g1, %g1, %g1
	or	%g3, %g1, %g1
	sth	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset18_count13, .-bitfield__write_var_BitField_offset18_count13
	.align 4
	.global bitfield__read_var_BitField_offset18_count14
	.hidden	bitfield__read_var_BitField_offset18_count14
	.type	bitfield__read_var_BitField_offset18_count14, #function
	.proc	04
bitfield__read_var_BitField_offset18_count14:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset18_count14, .-bitfield__read_var_BitField_offset18_count14
	.align 4
	.global bitfield__write_var_BitField_offset18_count14
	.hidden	bitfield__write_var_BitField_offset18_count14
	.type	bitfield__write_var_BitField_offset18_count14, #function
	.proc	020
bitfield__write_var_BitField_offset18_count14:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2427
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(15360), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL2428
	 nop
.LL2427:
	sethi	%hi(104448), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL2428:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g4
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-16384), %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	or	%g3, %g1, %g1
	sth	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset18_count14, .-bitfield__write_var_BitField_offset18_count14
	.align 4
	.global bitfield__read_var_BitField_offset19_count1
	.hidden	bitfield__read_var_BitField_offset19_count1
	.type	bitfield__read_var_BitField_offset19_count1, #function
	.proc	04
bitfield__read_var_BitField_offset19_count1:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sra	%g1, 8, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 27, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset19_count1, .-bitfield__read_var_BitField_offset19_count1
	.align 4
	.global bitfield__write_var_BitField_offset19_count1
	.hidden	bitfield__write_var_BitField_offset19_count1
	.type	bitfield__write_var_BitField_offset19_count1, #function
	.proc	020
bitfield__write_var_BitField_offset19_count1:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2433
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1
	ble	%icc, .LL2434
	 nop
.LL2433:
	sethi	%hi(105472), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2434:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -17, %g2
	ld	[%fp-24], %g1
	and	%g1, 1, %g1
	sll	%g1, 4, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset19_count1, .-bitfield__write_var_BitField_offset19_count1
	.align 4
	.global bitfield__read_var_BitField_offset19_count2
	.hidden	bitfield__read_var_BitField_offset19_count2
	.type	bitfield__read_var_BitField_offset19_count2, #function
	.proc	04
bitfield__read_var_BitField_offset19_count2:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sra	%g1, 8, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 27, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset19_count2, .-bitfield__read_var_BitField_offset19_count2
	.align 4
	.global bitfield__write_var_BitField_offset19_count2
	.hidden	bitfield__write_var_BitField_offset19_count2
	.type	bitfield__write_var_BitField_offset19_count2, #function
	.proc	020
bitfield__write_var_BitField_offset19_count2:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2439
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 3
	ble	%icc, .LL2440
	 nop
.LL2439:
	sethi	%hi(105472), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2440:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -25, %g2
	ld	[%fp-24], %g1
	and	%g1, 3, %g1
	sll	%g1, 3, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset19_count2, .-bitfield__write_var_BitField_offset19_count2
	.align 4
	.global bitfield__read_var_BitField_offset19_count3
	.hidden	bitfield__read_var_BitField_offset19_count3
	.type	bitfield__read_var_BitField_offset19_count3, #function
	.proc	04
bitfield__read_var_BitField_offset19_count3:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sra	%g1, 8, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 27, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset19_count3, .-bitfield__read_var_BitField_offset19_count3
	.align 4
	.global bitfield__write_var_BitField_offset19_count3
	.hidden	bitfield__write_var_BitField_offset19_count3
	.type	bitfield__write_var_BitField_offset19_count3, #function
	.proc	020
bitfield__write_var_BitField_offset19_count3:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2445
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 7
	ble	%icc, .LL2446
	 nop
.LL2445:
	sethi	%hi(105472), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2446:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -29, %g2
	ld	[%fp-24], %g1
	and	%g1, 7, %g1
	sll	%g1, 2, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset19_count3, .-bitfield__write_var_BitField_offset19_count3
	.align 4
	.global bitfield__read_var_BitField_offset19_count4
	.hidden	bitfield__read_var_BitField_offset19_count4
	.type	bitfield__read_var_BitField_offset19_count4, #function
	.proc	04
bitfield__read_var_BitField_offset19_count4:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sra	%g1, 8, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 27, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset19_count4, .-bitfield__read_var_BitField_offset19_count4
	.align 4
	.global bitfield__write_var_BitField_offset19_count4
	.hidden	bitfield__write_var_BitField_offset19_count4
	.type	bitfield__write_var_BitField_offset19_count4, #function
	.proc	020
bitfield__write_var_BitField_offset19_count4:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2451
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 15
	ble	%icc, .LL2452
	 nop
.LL2451:
	sethi	%hi(105472), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL2452:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -31, %g2
	ld	[%fp-24], %g1
	and	%g1, 15, %g1
	add	%g1, %g1, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset19_count4, .-bitfield__write_var_BitField_offset19_count4
	.align 4
	.global bitfield__read_var_BitField_offset19_count5
	.hidden	bitfield__read_var_BitField_offset19_count5
	.type	bitfield__read_var_BitField_offset19_count5, #function
	.proc	04
bitfield__read_var_BitField_offset19_count5:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sra	%g1, 8, %g1
	and	%g1, 0xff, %g1
	and	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset19_count5, .-bitfield__read_var_BitField_offset19_count5
	.align 4
	.global bitfield__write_var_BitField_offset19_count5
	.hidden	bitfield__write_var_BitField_offset19_count5
	.type	bitfield__write_var_BitField_offset19_count5, #function
	.proc	020
bitfield__write_var_BitField_offset19_count5:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2457
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 31
	ble	%icc, .LL2458
	 nop
.LL2457:
	sethi	%hi(106496), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2458:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -32, %g2
	ld	[%fp-24], %g1
	and	%g1, 31, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset19_count5, .-bitfield__write_var_BitField_offset19_count5
	.align 4
	.global bitfield__read_var_BitField_offset19_count6
	.hidden	bitfield__read_var_BitField_offset19_count6
	.type	bitfield__read_var_BitField_offset19_count6, #function
	.proc	04
bitfield__read_var_BitField_offset19_count6:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 19, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset19_count6, .-bitfield__read_var_BitField_offset19_count6
	.align 4
	.global bitfield__write_var_BitField_offset19_count6
	.hidden	bitfield__write_var_BitField_offset19_count6
	.type	bitfield__write_var_BitField_offset19_count6, #function
	.proc	020
bitfield__write_var_BitField_offset19_count6:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2463
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 63
	ble	%icc, .LL2464
	 nop
.LL2463:
	sethi	%hi(106496), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2464:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-8192), %g1
	or	%g1, 127, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 63, %g1
	sll	%g1, 7, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset19_count6, .-bitfield__write_var_BitField_offset19_count6
	.align 4
	.global bitfield__read_var_BitField_offset19_count7
	.hidden	bitfield__read_var_BitField_offset19_count7
	.type	bitfield__read_var_BitField_offset19_count7, #function
	.proc	04
bitfield__read_var_BitField_offset19_count7:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 19, %g1
	srl	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset19_count7, .-bitfield__read_var_BitField_offset19_count7
	.align 4
	.global bitfield__write_var_BitField_offset19_count7
	.hidden	bitfield__write_var_BitField_offset19_count7
	.type	bitfield__write_var_BitField_offset19_count7, #function
	.proc	020
bitfield__write_var_BitField_offset19_count7:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2469
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 127
	ble	%icc, .LL2470
	 nop
.LL2469:
	sethi	%hi(106496), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2470:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-8192), %g1
	or	%g1, 63, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 127, %g1
	sll	%g1, 6, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset19_count7, .-bitfield__write_var_BitField_offset19_count7
	.align 4
	.global bitfield__read_var_BitField_offset19_count8
	.hidden	bitfield__read_var_BitField_offset19_count8
	.type	bitfield__read_var_BitField_offset19_count8, #function
	.proc	04
bitfield__read_var_BitField_offset19_count8:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 19, %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset19_count8, .-bitfield__read_var_BitField_offset19_count8
	.align 4
	.global bitfield__write_var_BitField_offset19_count8
	.hidden	bitfield__write_var_BitField_offset19_count8
	.type	bitfield__write_var_BitField_offset19_count8, #function
	.proc	020
bitfield__write_var_BitField_offset19_count8:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2475
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 255
	ble	%icc, .LL2476
	 nop
.LL2475:
	sethi	%hi(106496), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL2476:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-8192), %g1
	or	%g1, 31, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 255, %g1
	sll	%g1, 5, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset19_count8, .-bitfield__write_var_BitField_offset19_count8
	.align 4
	.global bitfield__read_var_BitField_offset19_count9
	.hidden	bitfield__read_var_BitField_offset19_count9
	.type	bitfield__read_var_BitField_offset19_count9, #function
	.proc	04
bitfield__read_var_BitField_offset19_count9:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 19, %g1
	srl	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset19_count9, .-bitfield__read_var_BitField_offset19_count9
	.align 4
	.global bitfield__write_var_BitField_offset19_count9
	.hidden	bitfield__write_var_BitField_offset19_count9
	.type	bitfield__write_var_BitField_offset19_count9, #function
	.proc	020
bitfield__write_var_BitField_offset19_count9:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2481
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 511
	ble	%icc, .LL2482
	 nop
.LL2481:
	sethi	%hi(107520), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2482:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-8192), %g1
	or	%g1, 15, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 511, %g1
	sll	%g1, 4, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset19_count9, .-bitfield__write_var_BitField_offset19_count9
	.align 4
	.global bitfield__read_var_BitField_offset19_count10
	.hidden	bitfield__read_var_BitField_offset19_count10
	.type	bitfield__read_var_BitField_offset19_count10, #function
	.proc	04
bitfield__read_var_BitField_offset19_count10:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 19, %g1
	srl	%g1, 22, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset19_count10, .-bitfield__read_var_BitField_offset19_count10
	.align 4
	.global bitfield__write_var_BitField_offset19_count10
	.hidden	bitfield__write_var_BitField_offset19_count10
	.type	bitfield__write_var_BitField_offset19_count10, #function
	.proc	020
bitfield__write_var_BitField_offset19_count10:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2487
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1023
	ble	%icc, .LL2488
	 nop
.LL2487:
	sethi	%hi(107520), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2488:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-8192), %g1
	or	%g1, 7, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 1023, %g1
	sll	%g1, 3, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset19_count10, .-bitfield__write_var_BitField_offset19_count10
	.align 4
	.global bitfield__read_var_BitField_offset19_count11
	.hidden	bitfield__read_var_BitField_offset19_count11
	.type	bitfield__read_var_BitField_offset19_count11, #function
	.proc	04
bitfield__read_var_BitField_offset19_count11:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 19, %g1
	srl	%g1, 21, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset19_count11, .-bitfield__read_var_BitField_offset19_count11
	.align 4
	.global bitfield__write_var_BitField_offset19_count11
	.hidden	bitfield__write_var_BitField_offset19_count11
	.type	bitfield__write_var_BitField_offset19_count11, #function
	.proc	020
bitfield__write_var_BitField_offset19_count11:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2493
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 2047
	ble	%icc, .LL2494
	 nop
.LL2493:
	sethi	%hi(107520), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2494:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-8192), %g1
	or	%g1, 3, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 2047, %g1
	sll	%g1, 2, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset19_count11, .-bitfield__write_var_BitField_offset19_count11
	.align 4
	.global bitfield__read_var_BitField_offset19_count12
	.hidden	bitfield__read_var_BitField_offset19_count12
	.type	bitfield__read_var_BitField_offset19_count12, #function
	.proc	04
bitfield__read_var_BitField_offset19_count12:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 19, %g1
	srl	%g1, 20, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset19_count12, .-bitfield__read_var_BitField_offset19_count12
	.align 4
	.global bitfield__write_var_BitField_offset19_count12
	.hidden	bitfield__write_var_BitField_offset19_count12
	.type	bitfield__write_var_BitField_offset19_count12, #function
	.proc	020
bitfield__write_var_BitField_offset19_count12:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2499
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 4095
	ble	%icc, .LL2500
	 nop
.LL2499:
	sethi	%hi(107520), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL2500:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-8192), %g1
	or	%g1, 1, %g1
	and	%g2, %g1, %g2
	ld	[%fp-24], %g1
	and	%g1, 4095, %g1
	add	%g1, %g1, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset19_count12, .-bitfield__write_var_BitField_offset19_count12
	.align 4
	.global bitfield__read_var_BitField_offset19_count13
	.hidden	bitfield__read_var_BitField_offset19_count13
	.type	bitfield__read_var_BitField_offset19_count13, #function
	.proc	04
bitfield__read_var_BitField_offset19_count13:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset19_count13, .-bitfield__read_var_BitField_offset19_count13
	.align 4
	.global bitfield__write_var_BitField_offset19_count13
	.hidden	bitfield__write_var_BitField_offset19_count13
	.type	bitfield__write_var_BitField_offset19_count13, #function
	.proc	020
bitfield__write_var_BitField_offset19_count13:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2505
	 nop
	ld	[%fp-24], %g1
	sethi	%hi(7168), %g2
	or	%g2, 1023, %g2
	cmp	%g1, %g2
	ble	%icc, .LL2506
	 nop
.LL2505:
	sethi	%hi(108544), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2506:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g4
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g2
	sethi	%hi(-8192), %g1
	and	%g2, %g1, %g3
	ld	[%fp-24], %g2
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	and	%g2, %g1, %g1
	or	%g3, %g1, %g1
	sth	%g1, [%g4]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset19_count13, .-bitfield__write_var_BitField_offset19_count13
	.align 4
	.global bitfield__read_var_BitField_offset20_count1
	.hidden	bitfield__read_var_BitField_offset20_count1
	.type	bitfield__read_var_BitField_offset20_count1, #function
	.proc	04
bitfield__read_var_BitField_offset20_count1:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sra	%g1, 8, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 28, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset20_count1, .-bitfield__read_var_BitField_offset20_count1
	.align 4
	.global bitfield__write_var_BitField_offset20_count1
	.hidden	bitfield__write_var_BitField_offset20_count1
	.type	bitfield__write_var_BitField_offset20_count1, #function
	.proc	020
bitfield__write_var_BitField_offset20_count1:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2511
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1
	ble	%icc, .LL2512
	 nop
.LL2511:
	sethi	%hi(108544), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2512:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -9, %g2
	ld	[%fp-24], %g1
	and	%g1, 1, %g1
	sll	%g1, 3, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset20_count1, .-bitfield__write_var_BitField_offset20_count1
	.align 4
	.global bitfield__read_var_BitField_offset20_count2
	.hidden	bitfield__read_var_BitField_offset20_count2
	.type	bitfield__read_var_BitField_offset20_count2, #function
	.proc	04
bitfield__read_var_BitField_offset20_count2:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sra	%g1, 8, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 28, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset20_count2, .-bitfield__read_var_BitField_offset20_count2
	.align 4
	.global bitfield__write_var_BitField_offset20_count2
	.hidden	bitfield__write_var_BitField_offset20_count2
	.type	bitfield__write_var_BitField_offset20_count2, #function
	.proc	020
bitfield__write_var_BitField_offset20_count2:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2517
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 3
	ble	%icc, .LL2518
	 nop
.LL2517:
	sethi	%hi(108544), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2518:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -13, %g2
	ld	[%fp-24], %g1
	and	%g1, 3, %g1
	sll	%g1, 2, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset20_count2, .-bitfield__write_var_BitField_offset20_count2
	.align 4
	.global bitfield__read_var_BitField_offset20_count3
	.hidden	bitfield__read_var_BitField_offset20_count3
	.type	bitfield__read_var_BitField_offset20_count3, #function
	.proc	04
bitfield__read_var_BitField_offset20_count3:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sra	%g1, 8, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 28, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset20_count3, .-bitfield__read_var_BitField_offset20_count3
	.align 4
	.global bitfield__write_var_BitField_offset20_count3
	.hidden	bitfield__write_var_BitField_offset20_count3
	.type	bitfield__write_var_BitField_offset20_count3, #function
	.proc	020
bitfield__write_var_BitField_offset20_count3:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2523
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 7
	ble	%icc, .LL2524
	 nop
.LL2523:
	sethi	%hi(108544), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL2524:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -15, %g2
	ld	[%fp-24], %g1
	and	%g1, 7, %g1
	add	%g1, %g1, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset20_count3, .-bitfield__write_var_BitField_offset20_count3
	.align 4
	.global bitfield__read_var_BitField_offset20_count4
	.hidden	bitfield__read_var_BitField_offset20_count4
	.type	bitfield__read_var_BitField_offset20_count4, #function
	.proc	04
bitfield__read_var_BitField_offset20_count4:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sra	%g1, 8, %g1
	and	%g1, 0xff, %g1
	and	%g1, 15, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset20_count4, .-bitfield__read_var_BitField_offset20_count4
	.align 4
	.global bitfield__write_var_BitField_offset20_count4
	.hidden	bitfield__write_var_BitField_offset20_count4
	.type	bitfield__write_var_BitField_offset20_count4, #function
	.proc	020
bitfield__write_var_BitField_offset20_count4:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2529
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 15
	ble	%icc, .LL2530
	 nop
.LL2529:
	sethi	%hi(109568), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2530:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -16, %g2
	ld	[%fp-24], %g1
	and	%g1, 15, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset20_count4, .-bitfield__write_var_BitField_offset20_count4
	.align 4
	.global bitfield__read_var_BitField_offset20_count5
	.hidden	bitfield__read_var_BitField_offset20_count5
	.type	bitfield__read_var_BitField_offset20_count5, #function
	.proc	04
bitfield__read_var_BitField_offset20_count5:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 20, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset20_count5, .-bitfield__read_var_BitField_offset20_count5
	.align 4
	.global bitfield__write_var_BitField_offset20_count5
	.hidden	bitfield__write_var_BitField_offset20_count5
	.type	bitfield__write_var_BitField_offset20_count5, #function
	.proc	020
bitfield__write_var_BitField_offset20_count5:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2535
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 31
	ble	%icc, .LL2536
	 nop
.LL2535:
	sethi	%hi(109568), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2536:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -3969, %g2
	ld	[%fp-24], %g1
	and	%g1, 31, %g1
	sll	%g1, 7, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset20_count5, .-bitfield__write_var_BitField_offset20_count5
	.align 4
	.global bitfield__read_var_BitField_offset20_count6
	.hidden	bitfield__read_var_BitField_offset20_count6
	.type	bitfield__read_var_BitField_offset20_count6, #function
	.proc	04
bitfield__read_var_BitField_offset20_count6:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 20, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset20_count6, .-bitfield__read_var_BitField_offset20_count6
	.align 4
	.global bitfield__write_var_BitField_offset20_count6
	.hidden	bitfield__write_var_BitField_offset20_count6
	.type	bitfield__write_var_BitField_offset20_count6, #function
	.proc	020
bitfield__write_var_BitField_offset20_count6:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2541
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 63
	ble	%icc, .LL2542
	 nop
.LL2541:
	sethi	%hi(109568), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2542:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -4033, %g2
	ld	[%fp-24], %g1
	and	%g1, 63, %g1
	sll	%g1, 6, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset20_count6, .-bitfield__write_var_BitField_offset20_count6
	.align 4
	.global bitfield__read_var_BitField_offset20_count7
	.hidden	bitfield__read_var_BitField_offset20_count7
	.type	bitfield__read_var_BitField_offset20_count7, #function
	.proc	04
bitfield__read_var_BitField_offset20_count7:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 20, %g1
	srl	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset20_count7, .-bitfield__read_var_BitField_offset20_count7
	.align 4
	.global bitfield__write_var_BitField_offset20_count7
	.hidden	bitfield__write_var_BitField_offset20_count7
	.type	bitfield__write_var_BitField_offset20_count7, #function
	.proc	020
bitfield__write_var_BitField_offset20_count7:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2547
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 127
	ble	%icc, .LL2548
	 nop
.LL2547:
	sethi	%hi(109568), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL2548:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -4065, %g2
	ld	[%fp-24], %g1
	and	%g1, 127, %g1
	sll	%g1, 5, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset20_count7, .-bitfield__write_var_BitField_offset20_count7
	.align 4
	.global bitfield__read_var_BitField_offset20_count8
	.hidden	bitfield__read_var_BitField_offset20_count8
	.type	bitfield__read_var_BitField_offset20_count8, #function
	.proc	04
bitfield__read_var_BitField_offset20_count8:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 20, %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset20_count8, .-bitfield__read_var_BitField_offset20_count8
	.align 4
	.global bitfield__write_var_BitField_offset20_count8
	.hidden	bitfield__write_var_BitField_offset20_count8
	.type	bitfield__write_var_BitField_offset20_count8, #function
	.proc	020
bitfield__write_var_BitField_offset20_count8:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2553
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 255
	ble	%icc, .LL2554
	 nop
.LL2553:
	sethi	%hi(110592), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2554:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -4081, %g2
	ld	[%fp-24], %g1
	and	%g1, 255, %g1
	sll	%g1, 4, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset20_count8, .-bitfield__write_var_BitField_offset20_count8
	.align 4
	.global bitfield__read_var_BitField_offset20_count9
	.hidden	bitfield__read_var_BitField_offset20_count9
	.type	bitfield__read_var_BitField_offset20_count9, #function
	.proc	04
bitfield__read_var_BitField_offset20_count9:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 20, %g1
	srl	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset20_count9, .-bitfield__read_var_BitField_offset20_count9
	.align 4
	.global bitfield__write_var_BitField_offset20_count9
	.hidden	bitfield__write_var_BitField_offset20_count9
	.type	bitfield__write_var_BitField_offset20_count9, #function
	.proc	020
bitfield__write_var_BitField_offset20_count9:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2559
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 511
	ble	%icc, .LL2560
	 nop
.LL2559:
	sethi	%hi(110592), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2560:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -4089, %g2
	ld	[%fp-24], %g1
	and	%g1, 511, %g1
	sll	%g1, 3, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset20_count9, .-bitfield__write_var_BitField_offset20_count9
	.align 4
	.global bitfield__read_var_BitField_offset20_count10
	.hidden	bitfield__read_var_BitField_offset20_count10
	.type	bitfield__read_var_BitField_offset20_count10, #function
	.proc	04
bitfield__read_var_BitField_offset20_count10:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 20, %g1
	srl	%g1, 22, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset20_count10, .-bitfield__read_var_BitField_offset20_count10
	.align 4
	.global bitfield__write_var_BitField_offset20_count10
	.hidden	bitfield__write_var_BitField_offset20_count10
	.type	bitfield__write_var_BitField_offset20_count10, #function
	.proc	020
bitfield__write_var_BitField_offset20_count10:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2565
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1023
	ble	%icc, .LL2566
	 nop
.LL2565:
	sethi	%hi(110592), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2566:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -4093, %g2
	ld	[%fp-24], %g1
	and	%g1, 1023, %g1
	sll	%g1, 2, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset20_count10, .-bitfield__write_var_BitField_offset20_count10
	.align 4
	.global bitfield__read_var_BitField_offset20_count11
	.hidden	bitfield__read_var_BitField_offset20_count11
	.type	bitfield__read_var_BitField_offset20_count11, #function
	.proc	04
bitfield__read_var_BitField_offset20_count11:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 20, %g1
	srl	%g1, 21, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset20_count11, .-bitfield__read_var_BitField_offset20_count11
	.align 4
	.global bitfield__write_var_BitField_offset20_count11
	.hidden	bitfield__write_var_BitField_offset20_count11
	.type	bitfield__write_var_BitField_offset20_count11, #function
	.proc	020
bitfield__write_var_BitField_offset20_count11:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2571
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 2047
	ble	%icc, .LL2572
	 nop
.LL2571:
	sethi	%hi(110592), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL2572:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -4095, %g2
	ld	[%fp-24], %g1
	and	%g1, 2047, %g1
	add	%g1, %g1, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset20_count11, .-bitfield__write_var_BitField_offset20_count11
	.align 4
	.global bitfield__read_var_BitField_offset20_count12
	.hidden	bitfield__read_var_BitField_offset20_count12
	.type	bitfield__read_var_BitField_offset20_count12, #function
	.proc	04
bitfield__read_var_BitField_offset20_count12:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, 4095, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset20_count12, .-bitfield__read_var_BitField_offset20_count12
	.align 4
	.global bitfield__write_var_BitField_offset20_count12
	.hidden	bitfield__write_var_BitField_offset20_count12
	.type	bitfield__write_var_BitField_offset20_count12, #function
	.proc	020
bitfield__write_var_BitField_offset20_count12:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2577
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 4095
	ble	%icc, .LL2578
	 nop
.LL2577:
	sethi	%hi(111616), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2578:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -4096, %g2
	ld	[%fp-24], %g1
	and	%g1, 4095, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset20_count12, .-bitfield__write_var_BitField_offset20_count12
	.align 4
	.global bitfield__read_var_BitField_offset21_count1
	.hidden	bitfield__read_var_BitField_offset21_count1
	.type	bitfield__read_var_BitField_offset21_count1, #function
	.proc	04
bitfield__read_var_BitField_offset21_count1:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sra	%g1, 8, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 29, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset21_count1, .-bitfield__read_var_BitField_offset21_count1
	.align 4
	.global bitfield__write_var_BitField_offset21_count1
	.hidden	bitfield__write_var_BitField_offset21_count1
	.type	bitfield__write_var_BitField_offset21_count1, #function
	.proc	020
bitfield__write_var_BitField_offset21_count1:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2583
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1
	ble	%icc, .LL2584
	 nop
.LL2583:
	sethi	%hi(111616), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2584:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -5, %g2
	ld	[%fp-24], %g1
	and	%g1, 1, %g1
	sll	%g1, 2, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset21_count1, .-bitfield__write_var_BitField_offset21_count1
	.align 4
	.global bitfield__read_var_BitField_offset21_count2
	.hidden	bitfield__read_var_BitField_offset21_count2
	.type	bitfield__read_var_BitField_offset21_count2, #function
	.proc	04
bitfield__read_var_BitField_offset21_count2:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sra	%g1, 8, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 29, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset21_count2, .-bitfield__read_var_BitField_offset21_count2
	.align 4
	.global bitfield__write_var_BitField_offset21_count2
	.hidden	bitfield__write_var_BitField_offset21_count2
	.type	bitfield__write_var_BitField_offset21_count2, #function
	.proc	020
bitfield__write_var_BitField_offset21_count2:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2589
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 3
	ble	%icc, .LL2590
	 nop
.LL2589:
	sethi	%hi(111616), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2590:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -7, %g2
	ld	[%fp-24], %g1
	and	%g1, 3, %g1
	add	%g1, %g1, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset21_count2, .-bitfield__write_var_BitField_offset21_count2
	.align 4
	.global bitfield__read_var_BitField_offset21_count3
	.hidden	bitfield__read_var_BitField_offset21_count3
	.type	bitfield__read_var_BitField_offset21_count3, #function
	.proc	04
bitfield__read_var_BitField_offset21_count3:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sra	%g1, 8, %g1
	and	%g1, 0xff, %g1
	and	%g1, 7, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset21_count3, .-bitfield__read_var_BitField_offset21_count3
	.align 4
	.global bitfield__write_var_BitField_offset21_count3
	.hidden	bitfield__write_var_BitField_offset21_count3
	.type	bitfield__write_var_BitField_offset21_count3, #function
	.proc	020
bitfield__write_var_BitField_offset21_count3:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2595
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 7
	ble	%icc, .LL2596
	 nop
.LL2595:
	sethi	%hi(111616), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL2596:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -8, %g2
	ld	[%fp-24], %g1
	and	%g1, 7, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset21_count3, .-bitfield__write_var_BitField_offset21_count3
	.align 4
	.global bitfield__read_var_BitField_offset21_count4
	.hidden	bitfield__read_var_BitField_offset21_count4
	.type	bitfield__read_var_BitField_offset21_count4, #function
	.proc	04
bitfield__read_var_BitField_offset21_count4:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 21, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset21_count4, .-bitfield__read_var_BitField_offset21_count4
	.align 4
	.global bitfield__write_var_BitField_offset21_count4
	.hidden	bitfield__write_var_BitField_offset21_count4
	.type	bitfield__write_var_BitField_offset21_count4, #function
	.proc	020
bitfield__write_var_BitField_offset21_count4:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2601
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 15
	ble	%icc, .LL2602
	 nop
.LL2601:
	sethi	%hi(112640), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2602:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -1921, %g2
	ld	[%fp-24], %g1
	and	%g1, 15, %g1
	sll	%g1, 7, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset21_count4, .-bitfield__write_var_BitField_offset21_count4
	.align 4
	.global bitfield__read_var_BitField_offset21_count5
	.hidden	bitfield__read_var_BitField_offset21_count5
	.type	bitfield__read_var_BitField_offset21_count5, #function
	.proc	04
bitfield__read_var_BitField_offset21_count5:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 21, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset21_count5, .-bitfield__read_var_BitField_offset21_count5
	.align 4
	.global bitfield__write_var_BitField_offset21_count5
	.hidden	bitfield__write_var_BitField_offset21_count5
	.type	bitfield__write_var_BitField_offset21_count5, #function
	.proc	020
bitfield__write_var_BitField_offset21_count5:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2607
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 31
	ble	%icc, .LL2608
	 nop
.LL2607:
	sethi	%hi(112640), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2608:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -1985, %g2
	ld	[%fp-24], %g1
	and	%g1, 31, %g1
	sll	%g1, 6, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset21_count5, .-bitfield__write_var_BitField_offset21_count5
	.align 4
	.global bitfield__read_var_BitField_offset21_count6
	.hidden	bitfield__read_var_BitField_offset21_count6
	.type	bitfield__read_var_BitField_offset21_count6, #function
	.proc	04
bitfield__read_var_BitField_offset21_count6:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 21, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset21_count6, .-bitfield__read_var_BitField_offset21_count6
	.align 4
	.global bitfield__write_var_BitField_offset21_count6
	.hidden	bitfield__write_var_BitField_offset21_count6
	.type	bitfield__write_var_BitField_offset21_count6, #function
	.proc	020
bitfield__write_var_BitField_offset21_count6:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2613
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 63
	ble	%icc, .LL2614
	 nop
.LL2613:
	sethi	%hi(112640), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2614:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -2017, %g2
	ld	[%fp-24], %g1
	and	%g1, 63, %g1
	sll	%g1, 5, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset21_count6, .-bitfield__write_var_BitField_offset21_count6
	.align 4
	.global bitfield__read_var_BitField_offset21_count7
	.hidden	bitfield__read_var_BitField_offset21_count7
	.type	bitfield__read_var_BitField_offset21_count7, #function
	.proc	04
bitfield__read_var_BitField_offset21_count7:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 21, %g1
	srl	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset21_count7, .-bitfield__read_var_BitField_offset21_count7
	.align 4
	.global bitfield__write_var_BitField_offset21_count7
	.hidden	bitfield__write_var_BitField_offset21_count7
	.type	bitfield__write_var_BitField_offset21_count7, #function
	.proc	020
bitfield__write_var_BitField_offset21_count7:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2619
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 127
	ble	%icc, .LL2620
	 nop
.LL2619:
	sethi	%hi(112640), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL2620:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -2033, %g2
	ld	[%fp-24], %g1
	and	%g1, 127, %g1
	sll	%g1, 4, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset21_count7, .-bitfield__write_var_BitField_offset21_count7
	.align 4
	.global bitfield__read_var_BitField_offset21_count8
	.hidden	bitfield__read_var_BitField_offset21_count8
	.type	bitfield__read_var_BitField_offset21_count8, #function
	.proc	04
bitfield__read_var_BitField_offset21_count8:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 21, %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset21_count8, .-bitfield__read_var_BitField_offset21_count8
	.align 4
	.global bitfield__write_var_BitField_offset21_count8
	.hidden	bitfield__write_var_BitField_offset21_count8
	.type	bitfield__write_var_BitField_offset21_count8, #function
	.proc	020
bitfield__write_var_BitField_offset21_count8:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2625
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 255
	ble	%icc, .LL2626
	 nop
.LL2625:
	sethi	%hi(113664), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2626:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -2041, %g2
	ld	[%fp-24], %g1
	and	%g1, 255, %g1
	sll	%g1, 3, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset21_count8, .-bitfield__write_var_BitField_offset21_count8
	.align 4
	.global bitfield__read_var_BitField_offset21_count9
	.hidden	bitfield__read_var_BitField_offset21_count9
	.type	bitfield__read_var_BitField_offset21_count9, #function
	.proc	04
bitfield__read_var_BitField_offset21_count9:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 21, %g1
	srl	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset21_count9, .-bitfield__read_var_BitField_offset21_count9
	.align 4
	.global bitfield__write_var_BitField_offset21_count9
	.hidden	bitfield__write_var_BitField_offset21_count9
	.type	bitfield__write_var_BitField_offset21_count9, #function
	.proc	020
bitfield__write_var_BitField_offset21_count9:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2631
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 511
	ble	%icc, .LL2632
	 nop
.LL2631:
	sethi	%hi(113664), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2632:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -2045, %g2
	ld	[%fp-24], %g1
	and	%g1, 511, %g1
	sll	%g1, 2, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset21_count9, .-bitfield__write_var_BitField_offset21_count9
	.align 4
	.global bitfield__read_var_BitField_offset21_count10
	.hidden	bitfield__read_var_BitField_offset21_count10
	.type	bitfield__read_var_BitField_offset21_count10, #function
	.proc	04
bitfield__read_var_BitField_offset21_count10:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 21, %g1
	srl	%g1, 22, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset21_count10, .-bitfield__read_var_BitField_offset21_count10
	.align 4
	.global bitfield__write_var_BitField_offset21_count10
	.hidden	bitfield__write_var_BitField_offset21_count10
	.type	bitfield__write_var_BitField_offset21_count10, #function
	.proc	020
bitfield__write_var_BitField_offset21_count10:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2637
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1023
	ble	%icc, .LL2638
	 nop
.LL2637:
	sethi	%hi(113664), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2638:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -2047, %g2
	ld	[%fp-24], %g1
	and	%g1, 1023, %g1
	add	%g1, %g1, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset21_count10, .-bitfield__write_var_BitField_offset21_count10
	.align 4
	.global bitfield__read_var_BitField_offset21_count11
	.hidden	bitfield__read_var_BitField_offset21_count11
	.type	bitfield__read_var_BitField_offset21_count11, #function
	.proc	04
bitfield__read_var_BitField_offset21_count11:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, 2047, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset21_count11, .-bitfield__read_var_BitField_offset21_count11
	.align 4
	.global bitfield__write_var_BitField_offset21_count11
	.hidden	bitfield__write_var_BitField_offset21_count11
	.type	bitfield__write_var_BitField_offset21_count11, #function
	.proc	020
bitfield__write_var_BitField_offset21_count11:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2643
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 2047
	ble	%icc, .LL2644
	 nop
.LL2643:
	sethi	%hi(113664), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL2644:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -2048, %g2
	ld	[%fp-24], %g1
	and	%g1, 2047, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset21_count11, .-bitfield__write_var_BitField_offset21_count11
	.align 4
	.global bitfield__read_var_BitField_offset22_count1
	.hidden	bitfield__read_var_BitField_offset22_count1
	.type	bitfield__read_var_BitField_offset22_count1, #function
	.proc	04
bitfield__read_var_BitField_offset22_count1:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sra	%g1, 8, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 30, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset22_count1, .-bitfield__read_var_BitField_offset22_count1
	.align 4
	.global bitfield__write_var_BitField_offset22_count1
	.hidden	bitfield__write_var_BitField_offset22_count1
	.type	bitfield__write_var_BitField_offset22_count1, #function
	.proc	020
bitfield__write_var_BitField_offset22_count1:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2649
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1
	ble	%icc, .LL2650
	 nop
.LL2649:
	sethi	%hi(114688), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2650:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -3, %g2
	ld	[%fp-24], %g1
	and	%g1, 1, %g1
	add	%g1, %g1, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset22_count1, .-bitfield__write_var_BitField_offset22_count1
	.align 4
	.global bitfield__read_var_BitField_offset22_count2
	.hidden	bitfield__read_var_BitField_offset22_count2
	.type	bitfield__read_var_BitField_offset22_count2, #function
	.proc	04
bitfield__read_var_BitField_offset22_count2:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sra	%g1, 8, %g1
	and	%g1, 0xff, %g1
	and	%g1, 3, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset22_count2, .-bitfield__read_var_BitField_offset22_count2
	.align 4
	.global bitfield__write_var_BitField_offset22_count2
	.hidden	bitfield__write_var_BitField_offset22_count2
	.type	bitfield__write_var_BitField_offset22_count2, #function
	.proc	020
bitfield__write_var_BitField_offset22_count2:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2655
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 3
	ble	%icc, .LL2656
	 nop
.LL2655:
	sethi	%hi(114688), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2656:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -4, %g2
	ld	[%fp-24], %g1
	and	%g1, 3, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset22_count2, .-bitfield__write_var_BitField_offset22_count2
	.align 4
	.global bitfield__read_var_BitField_offset22_count3
	.hidden	bitfield__read_var_BitField_offset22_count3
	.type	bitfield__read_var_BitField_offset22_count3, #function
	.proc	04
bitfield__read_var_BitField_offset22_count3:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 22, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset22_count3, .-bitfield__read_var_BitField_offset22_count3
	.align 4
	.global bitfield__write_var_BitField_offset22_count3
	.hidden	bitfield__write_var_BitField_offset22_count3
	.type	bitfield__write_var_BitField_offset22_count3, #function
	.proc	020
bitfield__write_var_BitField_offset22_count3:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2661
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 7
	ble	%icc, .LL2662
	 nop
.LL2661:
	sethi	%hi(114688), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2662:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -897, %g2
	ld	[%fp-24], %g1
	and	%g1, 7, %g1
	sll	%g1, 7, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset22_count3, .-bitfield__write_var_BitField_offset22_count3
	.align 4
	.global bitfield__read_var_BitField_offset22_count4
	.hidden	bitfield__read_var_BitField_offset22_count4
	.type	bitfield__read_var_BitField_offset22_count4, #function
	.proc	04
bitfield__read_var_BitField_offset22_count4:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 22, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset22_count4, .-bitfield__read_var_BitField_offset22_count4
	.align 4
	.global bitfield__write_var_BitField_offset22_count4
	.hidden	bitfield__write_var_BitField_offset22_count4
	.type	bitfield__write_var_BitField_offset22_count4, #function
	.proc	020
bitfield__write_var_BitField_offset22_count4:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2667
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 15
	ble	%icc, .LL2668
	 nop
.LL2667:
	sethi	%hi(114688), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL2668:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -961, %g2
	ld	[%fp-24], %g1
	and	%g1, 15, %g1
	sll	%g1, 6, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset22_count4, .-bitfield__write_var_BitField_offset22_count4
	.align 4
	.global bitfield__read_var_BitField_offset22_count5
	.hidden	bitfield__read_var_BitField_offset22_count5
	.type	bitfield__read_var_BitField_offset22_count5, #function
	.proc	04
bitfield__read_var_BitField_offset22_count5:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 22, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset22_count5, .-bitfield__read_var_BitField_offset22_count5
	.align 4
	.global bitfield__write_var_BitField_offset22_count5
	.hidden	bitfield__write_var_BitField_offset22_count5
	.type	bitfield__write_var_BitField_offset22_count5, #function
	.proc	020
bitfield__write_var_BitField_offset22_count5:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2673
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 31
	ble	%icc, .LL2674
	 nop
.LL2673:
	sethi	%hi(115712), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2674:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -993, %g2
	ld	[%fp-24], %g1
	and	%g1, 31, %g1
	sll	%g1, 5, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset22_count5, .-bitfield__write_var_BitField_offset22_count5
	.align 4
	.global bitfield__read_var_BitField_offset22_count6
	.hidden	bitfield__read_var_BitField_offset22_count6
	.type	bitfield__read_var_BitField_offset22_count6, #function
	.proc	04
bitfield__read_var_BitField_offset22_count6:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 22, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset22_count6, .-bitfield__read_var_BitField_offset22_count6
	.align 4
	.global bitfield__write_var_BitField_offset22_count6
	.hidden	bitfield__write_var_BitField_offset22_count6
	.type	bitfield__write_var_BitField_offset22_count6, #function
	.proc	020
bitfield__write_var_BitField_offset22_count6:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2679
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 63
	ble	%icc, .LL2680
	 nop
.LL2679:
	sethi	%hi(115712), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2680:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -1009, %g2
	ld	[%fp-24], %g1
	and	%g1, 63, %g1
	sll	%g1, 4, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset22_count6, .-bitfield__write_var_BitField_offset22_count6
	.align 4
	.global bitfield__read_var_BitField_offset22_count7
	.hidden	bitfield__read_var_BitField_offset22_count7
	.type	bitfield__read_var_BitField_offset22_count7, #function
	.proc	04
bitfield__read_var_BitField_offset22_count7:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 22, %g1
	srl	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset22_count7, .-bitfield__read_var_BitField_offset22_count7
	.align 4
	.global bitfield__write_var_BitField_offset22_count7
	.hidden	bitfield__write_var_BitField_offset22_count7
	.type	bitfield__write_var_BitField_offset22_count7, #function
	.proc	020
bitfield__write_var_BitField_offset22_count7:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2685
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 127
	ble	%icc, .LL2686
	 nop
.LL2685:
	sethi	%hi(115712), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2686:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -1017, %g2
	ld	[%fp-24], %g1
	and	%g1, 127, %g1
	sll	%g1, 3, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset22_count7, .-bitfield__write_var_BitField_offset22_count7
	.align 4
	.global bitfield__read_var_BitField_offset22_count8
	.hidden	bitfield__read_var_BitField_offset22_count8
	.type	bitfield__read_var_BitField_offset22_count8, #function
	.proc	04
bitfield__read_var_BitField_offset22_count8:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 22, %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset22_count8, .-bitfield__read_var_BitField_offset22_count8
	.align 4
	.global bitfield__write_var_BitField_offset22_count8
	.hidden	bitfield__write_var_BitField_offset22_count8
	.type	bitfield__write_var_BitField_offset22_count8, #function
	.proc	020
bitfield__write_var_BitField_offset22_count8:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2691
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 255
	ble	%icc, .LL2692
	 nop
.LL2691:
	sethi	%hi(115712), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL2692:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -1021, %g2
	ld	[%fp-24], %g1
	and	%g1, 255, %g1
	sll	%g1, 2, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset22_count8, .-bitfield__write_var_BitField_offset22_count8
	.align 4
	.global bitfield__read_var_BitField_offset22_count9
	.hidden	bitfield__read_var_BitField_offset22_count9
	.type	bitfield__read_var_BitField_offset22_count9, #function
	.proc	04
bitfield__read_var_BitField_offset22_count9:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 22, %g1
	srl	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset22_count9, .-bitfield__read_var_BitField_offset22_count9
	.align 4
	.global bitfield__write_var_BitField_offset22_count9
	.hidden	bitfield__write_var_BitField_offset22_count9
	.type	bitfield__write_var_BitField_offset22_count9, #function
	.proc	020
bitfield__write_var_BitField_offset22_count9:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2697
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 511
	ble	%icc, .LL2698
	 nop
.LL2697:
	sethi	%hi(116736), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2698:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -1023, %g2
	ld	[%fp-24], %g1
	and	%g1, 511, %g1
	add	%g1, %g1, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset22_count9, .-bitfield__write_var_BitField_offset22_count9
	.align 4
	.global bitfield__read_var_BitField_offset22_count10
	.hidden	bitfield__read_var_BitField_offset22_count10
	.type	bitfield__read_var_BitField_offset22_count10, #function
	.proc	04
bitfield__read_var_BitField_offset22_count10:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, 1023, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset22_count10, .-bitfield__read_var_BitField_offset22_count10
	.align 4
	.global bitfield__write_var_BitField_offset22_count10
	.hidden	bitfield__write_var_BitField_offset22_count10
	.type	bitfield__write_var_BitField_offset22_count10, #function
	.proc	020
bitfield__write_var_BitField_offset22_count10:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2703
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1023
	ble	%icc, .LL2704
	 nop
.LL2703:
	sethi	%hi(116736), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2704:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -1024, %g2
	ld	[%fp-24], %g1
	and	%g1, 1023, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset22_count10, .-bitfield__write_var_BitField_offset22_count10
	.align 4
	.global bitfield__read_var_BitField_offset23_count1
	.hidden	bitfield__read_var_BitField_offset23_count1
	.type	bitfield__read_var_BitField_offset23_count1, #function
	.proc	04
bitfield__read_var_BitField_offset23_count1:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sra	%g1, 8, %g1
	and	%g1, 0xff, %g1
	and	%g1, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset23_count1, .-bitfield__read_var_BitField_offset23_count1
	.align 4
	.global bitfield__write_var_BitField_offset23_count1
	.hidden	bitfield__write_var_BitField_offset23_count1
	.type	bitfield__write_var_BitField_offset23_count1, #function
	.proc	020
bitfield__write_var_BitField_offset23_count1:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2709
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1
	ble	%icc, .LL2710
	 nop
.LL2709:
	sethi	%hi(116736), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2710:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -2, %g2
	ld	[%fp-24], %g1
	and	%g1, 1, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset23_count1, .-bitfield__write_var_BitField_offset23_count1
	.align 4
	.global bitfield__read_var_BitField_offset23_count2
	.hidden	bitfield__read_var_BitField_offset23_count2
	.type	bitfield__read_var_BitField_offset23_count2, #function
	.proc	04
bitfield__read_var_BitField_offset23_count2:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 23, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset23_count2, .-bitfield__read_var_BitField_offset23_count2
	.align 4
	.global bitfield__write_var_BitField_offset23_count2
	.hidden	bitfield__write_var_BitField_offset23_count2
	.type	bitfield__write_var_BitField_offset23_count2, #function
	.proc	020
bitfield__write_var_BitField_offset23_count2:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2715
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 3
	ble	%icc, .LL2716
	 nop
.LL2715:
	sethi	%hi(116736), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL2716:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -385, %g2
	ld	[%fp-24], %g1
	and	%g1, 3, %g1
	sll	%g1, 7, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset23_count2, .-bitfield__write_var_BitField_offset23_count2
	.align 4
	.global bitfield__read_var_BitField_offset23_count3
	.hidden	bitfield__read_var_BitField_offset23_count3
	.type	bitfield__read_var_BitField_offset23_count3, #function
	.proc	04
bitfield__read_var_BitField_offset23_count3:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 23, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset23_count3, .-bitfield__read_var_BitField_offset23_count3
	.align 4
	.global bitfield__write_var_BitField_offset23_count3
	.hidden	bitfield__write_var_BitField_offset23_count3
	.type	bitfield__write_var_BitField_offset23_count3, #function
	.proc	020
bitfield__write_var_BitField_offset23_count3:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2721
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 7
	ble	%icc, .LL2722
	 nop
.LL2721:
	sethi	%hi(117760), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2722:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -449, %g2
	ld	[%fp-24], %g1
	and	%g1, 7, %g1
	sll	%g1, 6, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset23_count3, .-bitfield__write_var_BitField_offset23_count3
	.align 4
	.global bitfield__read_var_BitField_offset23_count4
	.hidden	bitfield__read_var_BitField_offset23_count4
	.type	bitfield__read_var_BitField_offset23_count4, #function
	.proc	04
bitfield__read_var_BitField_offset23_count4:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 23, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset23_count4, .-bitfield__read_var_BitField_offset23_count4
	.align 4
	.global bitfield__write_var_BitField_offset23_count4
	.hidden	bitfield__write_var_BitField_offset23_count4
	.type	bitfield__write_var_BitField_offset23_count4, #function
	.proc	020
bitfield__write_var_BitField_offset23_count4:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2727
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 15
	ble	%icc, .LL2728
	 nop
.LL2727:
	sethi	%hi(117760), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2728:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -481, %g2
	ld	[%fp-24], %g1
	and	%g1, 15, %g1
	sll	%g1, 5, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset23_count4, .-bitfield__write_var_BitField_offset23_count4
	.align 4
	.global bitfield__read_var_BitField_offset23_count5
	.hidden	bitfield__read_var_BitField_offset23_count5
	.type	bitfield__read_var_BitField_offset23_count5, #function
	.proc	04
bitfield__read_var_BitField_offset23_count5:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 23, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset23_count5, .-bitfield__read_var_BitField_offset23_count5
	.align 4
	.global bitfield__write_var_BitField_offset23_count5
	.hidden	bitfield__write_var_BitField_offset23_count5
	.type	bitfield__write_var_BitField_offset23_count5, #function
	.proc	020
bitfield__write_var_BitField_offset23_count5:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2733
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 31
	ble	%icc, .LL2734
	 nop
.LL2733:
	sethi	%hi(117760), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2734:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -497, %g2
	ld	[%fp-24], %g1
	and	%g1, 31, %g1
	sll	%g1, 4, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset23_count5, .-bitfield__write_var_BitField_offset23_count5
	.align 4
	.global bitfield__read_var_BitField_offset23_count6
	.hidden	bitfield__read_var_BitField_offset23_count6
	.type	bitfield__read_var_BitField_offset23_count6, #function
	.proc	04
bitfield__read_var_BitField_offset23_count6:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 23, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset23_count6, .-bitfield__read_var_BitField_offset23_count6
	.align 4
	.global bitfield__write_var_BitField_offset23_count6
	.hidden	bitfield__write_var_BitField_offset23_count6
	.type	bitfield__write_var_BitField_offset23_count6, #function
	.proc	020
bitfield__write_var_BitField_offset23_count6:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2739
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 63
	ble	%icc, .LL2740
	 nop
.LL2739:
	sethi	%hi(117760), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL2740:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -505, %g2
	ld	[%fp-24], %g1
	and	%g1, 63, %g1
	sll	%g1, 3, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset23_count6, .-bitfield__write_var_BitField_offset23_count6
	.align 4
	.global bitfield__read_var_BitField_offset23_count7
	.hidden	bitfield__read_var_BitField_offset23_count7
	.type	bitfield__read_var_BitField_offset23_count7, #function
	.proc	04
bitfield__read_var_BitField_offset23_count7:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 23, %g1
	srl	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset23_count7, .-bitfield__read_var_BitField_offset23_count7
	.align 4
	.global bitfield__write_var_BitField_offset23_count7
	.hidden	bitfield__write_var_BitField_offset23_count7
	.type	bitfield__write_var_BitField_offset23_count7, #function
	.proc	020
bitfield__write_var_BitField_offset23_count7:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2745
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 127
	ble	%icc, .LL2746
	 nop
.LL2745:
	sethi	%hi(118784), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2746:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -509, %g2
	ld	[%fp-24], %g1
	and	%g1, 127, %g1
	sll	%g1, 2, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset23_count7, .-bitfield__write_var_BitField_offset23_count7
	.align 4
	.global bitfield__read_var_BitField_offset23_count8
	.hidden	bitfield__read_var_BitField_offset23_count8
	.type	bitfield__read_var_BitField_offset23_count8, #function
	.proc	04
bitfield__read_var_BitField_offset23_count8:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 23, %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset23_count8, .-bitfield__read_var_BitField_offset23_count8
	.align 4
	.global bitfield__write_var_BitField_offset23_count8
	.hidden	bitfield__write_var_BitField_offset23_count8
	.type	bitfield__write_var_BitField_offset23_count8, #function
	.proc	020
bitfield__write_var_BitField_offset23_count8:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2751
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 255
	ble	%icc, .LL2752
	 nop
.LL2751:
	sethi	%hi(118784), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2752:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -511, %g2
	ld	[%fp-24], %g1
	and	%g1, 255, %g1
	add	%g1, %g1, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset23_count8, .-bitfield__write_var_BitField_offset23_count8
	.align 4
	.global bitfield__read_var_BitField_offset23_count9
	.hidden	bitfield__read_var_BitField_offset23_count9
	.type	bitfield__read_var_BitField_offset23_count9, #function
	.proc	04
bitfield__read_var_BitField_offset23_count9:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, 511, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset23_count9, .-bitfield__read_var_BitField_offset23_count9
	.align 4
	.global bitfield__write_var_BitField_offset23_count9
	.hidden	bitfield__write_var_BitField_offset23_count9
	.type	bitfield__write_var_BitField_offset23_count9, #function
	.proc	020
bitfield__write_var_BitField_offset23_count9:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2757
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 511
	ble	%icc, .LL2758
	 nop
.LL2757:
	sethi	%hi(118784), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2758:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 2, %g3
	ld	[%fp-20], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, -512, %g2
	ld	[%fp-24], %g1
	and	%g1, 511, %g1
	or	%g2, %g1, %g1
	sth	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset23_count9, .-bitfield__write_var_BitField_offset23_count9
	.align 4
	.global bitfield__read_var_BitField_offset24_count1
	.hidden	bitfield__read_var_BitField_offset24_count1
	.type	bitfield__read_var_BitField_offset24_count1, #function
	.proc	04
bitfield__read_var_BitField_offset24_count1:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 24, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset24_count1, .-bitfield__read_var_BitField_offset24_count1
	.align 4
	.global bitfield__write_var_BitField_offset24_count1
	.hidden	bitfield__write_var_BitField_offset24_count1
	.type	bitfield__write_var_BitField_offset24_count1, #function
	.proc	020
bitfield__write_var_BitField_offset24_count1:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2763
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1
	ble	%icc, .LL2764
	 nop
.LL2763:
	sethi	%hi(118784), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL2764:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 3, %g3
	ld	[%fp-20], %g1
	add	%g1, 3, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -129, %g2
	ld	[%fp-24], %g1
	sll	%g1, 7, %g1
	and	%g1, 255, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset24_count1, .-bitfield__write_var_BitField_offset24_count1
	.align 4
	.global bitfield__read_var_BitField_offset24_count2
	.hidden	bitfield__read_var_BitField_offset24_count2
	.type	bitfield__read_var_BitField_offset24_count2, #function
	.proc	04
bitfield__read_var_BitField_offset24_count2:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 24, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset24_count2, .-bitfield__read_var_BitField_offset24_count2
	.align 4
	.global bitfield__write_var_BitField_offset24_count2
	.hidden	bitfield__write_var_BitField_offset24_count2
	.type	bitfield__write_var_BitField_offset24_count2, #function
	.proc	020
bitfield__write_var_BitField_offset24_count2:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2769
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 3
	ble	%icc, .LL2770
	 nop
.LL2769:
	sethi	%hi(119808), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2770:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 3, %g3
	ld	[%fp-20], %g1
	add	%g1, 3, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -193, %g2
	ld	[%fp-24], %g1
	sll	%g1, 6, %g1
	and	%g1, 255, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset24_count2, .-bitfield__write_var_BitField_offset24_count2
	.align 4
	.global bitfield__read_var_BitField_offset24_count3
	.hidden	bitfield__read_var_BitField_offset24_count3
	.type	bitfield__read_var_BitField_offset24_count3, #function
	.proc	04
bitfield__read_var_BitField_offset24_count3:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 24, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset24_count3, .-bitfield__read_var_BitField_offset24_count3
	.align 4
	.global bitfield__write_var_BitField_offset24_count3
	.hidden	bitfield__write_var_BitField_offset24_count3
	.type	bitfield__write_var_BitField_offset24_count3, #function
	.proc	020
bitfield__write_var_BitField_offset24_count3:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2775
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 7
	ble	%icc, .LL2776
	 nop
.LL2775:
	sethi	%hi(119808), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2776:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 3, %g3
	ld	[%fp-20], %g1
	add	%g1, 3, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -225, %g2
	ld	[%fp-24], %g1
	sll	%g1, 5, %g1
	and	%g1, 255, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset24_count3, .-bitfield__write_var_BitField_offset24_count3
	.align 4
	.global bitfield__read_var_BitField_offset24_count4
	.hidden	bitfield__read_var_BitField_offset24_count4
	.type	bitfield__read_var_BitField_offset24_count4, #function
	.proc	04
bitfield__read_var_BitField_offset24_count4:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 24, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset24_count4, .-bitfield__read_var_BitField_offset24_count4
	.align 4
	.global bitfield__write_var_BitField_offset24_count4
	.hidden	bitfield__write_var_BitField_offset24_count4
	.type	bitfield__write_var_BitField_offset24_count4, #function
	.proc	020
bitfield__write_var_BitField_offset24_count4:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2781
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 15
	ble	%icc, .LL2782
	 nop
.LL2781:
	sethi	%hi(119808), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2782:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 3, %g3
	ld	[%fp-20], %g1
	add	%g1, 3, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -241, %g2
	ld	[%fp-24], %g1
	sll	%g1, 4, %g1
	and	%g1, 255, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset24_count4, .-bitfield__write_var_BitField_offset24_count4
	.align 4
	.global bitfield__read_var_BitField_offset24_count5
	.hidden	bitfield__read_var_BitField_offset24_count5
	.type	bitfield__read_var_BitField_offset24_count5, #function
	.proc	04
bitfield__read_var_BitField_offset24_count5:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 24, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset24_count5, .-bitfield__read_var_BitField_offset24_count5
	.align 4
	.global bitfield__write_var_BitField_offset24_count5
	.hidden	bitfield__write_var_BitField_offset24_count5
	.type	bitfield__write_var_BitField_offset24_count5, #function
	.proc	020
bitfield__write_var_BitField_offset24_count5:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2787
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 31
	ble	%icc, .LL2788
	 nop
.LL2787:
	sethi	%hi(119808), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL2788:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 3, %g3
	ld	[%fp-20], %g1
	add	%g1, 3, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -249, %g2
	ld	[%fp-24], %g1
	sll	%g1, 3, %g1
	and	%g1, 255, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset24_count5, .-bitfield__write_var_BitField_offset24_count5
	.align 4
	.global bitfield__read_var_BitField_offset24_count6
	.hidden	bitfield__read_var_BitField_offset24_count6
	.type	bitfield__read_var_BitField_offset24_count6, #function
	.proc	04
bitfield__read_var_BitField_offset24_count6:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 24, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset24_count6, .-bitfield__read_var_BitField_offset24_count6
	.align 4
	.global bitfield__write_var_BitField_offset24_count6
	.hidden	bitfield__write_var_BitField_offset24_count6
	.type	bitfield__write_var_BitField_offset24_count6, #function
	.proc	020
bitfield__write_var_BitField_offset24_count6:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2793
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 63
	ble	%icc, .LL2794
	 nop
.LL2793:
	sethi	%hi(120832), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2794:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 3, %g3
	ld	[%fp-20], %g1
	add	%g1, 3, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -253, %g2
	ld	[%fp-24], %g1
	sll	%g1, 2, %g1
	and	%g1, 255, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset24_count6, .-bitfield__write_var_BitField_offset24_count6
	.align 4
	.global bitfield__read_var_BitField_offset24_count7
	.hidden	bitfield__read_var_BitField_offset24_count7
	.type	bitfield__read_var_BitField_offset24_count7, #function
	.proc	04
bitfield__read_var_BitField_offset24_count7:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 24, %g1
	srl	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset24_count7, .-bitfield__read_var_BitField_offset24_count7
	.align 4
	.global bitfield__write_var_BitField_offset24_count7
	.hidden	bitfield__write_var_BitField_offset24_count7
	.type	bitfield__write_var_BitField_offset24_count7, #function
	.proc	020
bitfield__write_var_BitField_offset24_count7:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2799
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 127
	ble	%icc, .LL2800
	 nop
.LL2799:
	sethi	%hi(120832), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2800:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 3, %g3
	ld	[%fp-20], %g1
	add	%g1, 3, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -255, %g2
	ld	[%fp-24], %g1
	add	%g1, %g1, %g1
	and	%g1, 255, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset24_count7, .-bitfield__write_var_BitField_offset24_count7
	.align 4
	.global bitfield__read_var_BitField_offset24_count8
	.hidden	bitfield__read_var_BitField_offset24_count8
	.type	bitfield__read_var_BitField_offset24_count8, #function
	.proc	04
bitfield__read_var_BitField_offset24_count8:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, 0xff, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset24_count8, .-bitfield__read_var_BitField_offset24_count8
	.align 4
	.global bitfield__write_var_BitField_offset24_count8
	.hidden	bitfield__write_var_BitField_offset24_count8
	.type	bitfield__write_var_BitField_offset24_count8, #function
	.proc	020
bitfield__write_var_BitField_offset24_count8:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	cmp	%g1, 0
	bl	%icc, .LL2805
	 nop
	ld	[%fp-20], %g1
	cmp	%g1, 255
	ble	%icc, .LL2806
	 nop
.LL2805:
	sethi	%hi(120832), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2806:
	ld	[%fp+68], %g1
	add	%g1, 3, %g2
	ld	[%fp-20], %g1
	stb	%g1, [%g2]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset24_count8, .-bitfield__write_var_BitField_offset24_count8
	.align 4
	.global bitfield__read_var_BitField_offset25_count1
	.hidden	bitfield__read_var_BitField_offset25_count1
	.type	bitfield__read_var_BitField_offset25_count1, #function
	.proc	04
bitfield__read_var_BitField_offset25_count1:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset25_count1, .-bitfield__read_var_BitField_offset25_count1
	.align 4
	.global bitfield__write_var_BitField_offset25_count1
	.hidden	bitfield__write_var_BitField_offset25_count1
	.type	bitfield__write_var_BitField_offset25_count1, #function
	.proc	020
bitfield__write_var_BitField_offset25_count1:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2811
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1
	ble	%icc, .LL2812
	 nop
.LL2811:
	sethi	%hi(120832), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL2812:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 3, %g3
	ld	[%fp-20], %g1
	add	%g1, 3, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -65, %g2
	ld	[%fp-24], %g1
	and	%g1, 1, %g1
	sll	%g1, 6, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset25_count1, .-bitfield__write_var_BitField_offset25_count1
	.align 4
	.global bitfield__read_var_BitField_offset25_count2
	.hidden	bitfield__read_var_BitField_offset25_count2
	.type	bitfield__read_var_BitField_offset25_count2, #function
	.proc	04
bitfield__read_var_BitField_offset25_count2:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset25_count2, .-bitfield__read_var_BitField_offset25_count2
	.align 4
	.global bitfield__write_var_BitField_offset25_count2
	.hidden	bitfield__write_var_BitField_offset25_count2
	.type	bitfield__write_var_BitField_offset25_count2, #function
	.proc	020
bitfield__write_var_BitField_offset25_count2:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2817
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 3
	ble	%icc, .LL2818
	 nop
.LL2817:
	sethi	%hi(121856), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2818:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 3, %g3
	ld	[%fp-20], %g1
	add	%g1, 3, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -97, %g2
	ld	[%fp-24], %g1
	and	%g1, 3, %g1
	sll	%g1, 5, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset25_count2, .-bitfield__write_var_BitField_offset25_count2
	.align 4
	.global bitfield__read_var_BitField_offset25_count3
	.hidden	bitfield__read_var_BitField_offset25_count3
	.type	bitfield__read_var_BitField_offset25_count3, #function
	.proc	04
bitfield__read_var_BitField_offset25_count3:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset25_count3, .-bitfield__read_var_BitField_offset25_count3
	.align 4
	.global bitfield__write_var_BitField_offset25_count3
	.hidden	bitfield__write_var_BitField_offset25_count3
	.type	bitfield__write_var_BitField_offset25_count3, #function
	.proc	020
bitfield__write_var_BitField_offset25_count3:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2823
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 7
	ble	%icc, .LL2824
	 nop
.LL2823:
	sethi	%hi(121856), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2824:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 3, %g3
	ld	[%fp-20], %g1
	add	%g1, 3, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -113, %g2
	ld	[%fp-24], %g1
	and	%g1, 7, %g1
	sll	%g1, 4, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset25_count3, .-bitfield__write_var_BitField_offset25_count3
	.align 4
	.global bitfield__read_var_BitField_offset25_count4
	.hidden	bitfield__read_var_BitField_offset25_count4
	.type	bitfield__read_var_BitField_offset25_count4, #function
	.proc	04
bitfield__read_var_BitField_offset25_count4:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset25_count4, .-bitfield__read_var_BitField_offset25_count4
	.align 4
	.global bitfield__write_var_BitField_offset25_count4
	.hidden	bitfield__write_var_BitField_offset25_count4
	.type	bitfield__write_var_BitField_offset25_count4, #function
	.proc	020
bitfield__write_var_BitField_offset25_count4:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2829
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 15
	ble	%icc, .LL2830
	 nop
.LL2829:
	sethi	%hi(121856), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2830:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 3, %g3
	ld	[%fp-20], %g1
	add	%g1, 3, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -121, %g2
	ld	[%fp-24], %g1
	and	%g1, 15, %g1
	sll	%g1, 3, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset25_count4, .-bitfield__write_var_BitField_offset25_count4
	.align 4
	.global bitfield__read_var_BitField_offset25_count5
	.hidden	bitfield__read_var_BitField_offset25_count5
	.type	bitfield__read_var_BitField_offset25_count5, #function
	.proc	04
bitfield__read_var_BitField_offset25_count5:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset25_count5, .-bitfield__read_var_BitField_offset25_count5
	.align 4
	.global bitfield__write_var_BitField_offset25_count5
	.hidden	bitfield__write_var_BitField_offset25_count5
	.type	bitfield__write_var_BitField_offset25_count5, #function
	.proc	020
bitfield__write_var_BitField_offset25_count5:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2835
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 31
	ble	%icc, .LL2836
	 nop
.LL2835:
	sethi	%hi(121856), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL2836:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 3, %g3
	ld	[%fp-20], %g1
	add	%g1, 3, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -125, %g2
	ld	[%fp-24], %g1
	and	%g1, 31, %g1
	sll	%g1, 2, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset25_count5, .-bitfield__write_var_BitField_offset25_count5
	.align 4
	.global bitfield__read_var_BitField_offset25_count6
	.hidden	bitfield__read_var_BitField_offset25_count6
	.type	bitfield__read_var_BitField_offset25_count6, #function
	.proc	04
bitfield__read_var_BitField_offset25_count6:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset25_count6, .-bitfield__read_var_BitField_offset25_count6
	.align 4
	.global bitfield__write_var_BitField_offset25_count6
	.hidden	bitfield__write_var_BitField_offset25_count6
	.type	bitfield__write_var_BitField_offset25_count6, #function
	.proc	020
bitfield__write_var_BitField_offset25_count6:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2841
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 63
	ble	%icc, .LL2842
	 nop
.LL2841:
	sethi	%hi(122880), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2842:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 3, %g3
	ld	[%fp-20], %g1
	add	%g1, 3, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -127, %g2
	ld	[%fp-24], %g1
	and	%g1, 63, %g1
	add	%g1, %g1, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset25_count6, .-bitfield__write_var_BitField_offset25_count6
	.align 4
	.global bitfield__read_var_BitField_offset25_count7
	.hidden	bitfield__read_var_BitField_offset25_count7
	.type	bitfield__read_var_BitField_offset25_count7, #function
	.proc	04
bitfield__read_var_BitField_offset25_count7:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, 127, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset25_count7, .-bitfield__read_var_BitField_offset25_count7
	.align 4
	.global bitfield__write_var_BitField_offset25_count7
	.hidden	bitfield__write_var_BitField_offset25_count7
	.type	bitfield__write_var_BitField_offset25_count7, #function
	.proc	020
bitfield__write_var_BitField_offset25_count7:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2847
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 127
	ble	%icc, .LL2848
	 nop
.LL2847:
	sethi	%hi(122880), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2848:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 3, %g3
	ld	[%fp-20], %g1
	add	%g1, 3, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -128, %g2
	ld	[%fp-24], %g1
	and	%g1, 127, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset25_count7, .-bitfield__write_var_BitField_offset25_count7
	.align 4
	.global bitfield__read_var_BitField_offset26_count1
	.hidden	bitfield__read_var_BitField_offset26_count1
	.type	bitfield__read_var_BitField_offset26_count1, #function
	.proc	04
bitfield__read_var_BitField_offset26_count1:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 26, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset26_count1, .-bitfield__read_var_BitField_offset26_count1
	.align 4
	.global bitfield__write_var_BitField_offset26_count1
	.hidden	bitfield__write_var_BitField_offset26_count1
	.type	bitfield__write_var_BitField_offset26_count1, #function
	.proc	020
bitfield__write_var_BitField_offset26_count1:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2853
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1
	ble	%icc, .LL2854
	 nop
.LL2853:
	sethi	%hi(122880), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2854:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 3, %g3
	ld	[%fp-20], %g1
	add	%g1, 3, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -33, %g2
	ld	[%fp-24], %g1
	and	%g1, 1, %g1
	sll	%g1, 5, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset26_count1, .-bitfield__write_var_BitField_offset26_count1
	.align 4
	.global bitfield__read_var_BitField_offset26_count2
	.hidden	bitfield__read_var_BitField_offset26_count2
	.type	bitfield__read_var_BitField_offset26_count2, #function
	.proc	04
bitfield__read_var_BitField_offset26_count2:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 26, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset26_count2, .-bitfield__read_var_BitField_offset26_count2
	.align 4
	.global bitfield__write_var_BitField_offset26_count2
	.hidden	bitfield__write_var_BitField_offset26_count2
	.type	bitfield__write_var_BitField_offset26_count2, #function
	.proc	020
bitfield__write_var_BitField_offset26_count2:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2859
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 3
	ble	%icc, .LL2860
	 nop
.LL2859:
	sethi	%hi(122880), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL2860:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 3, %g3
	ld	[%fp-20], %g1
	add	%g1, 3, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -49, %g2
	ld	[%fp-24], %g1
	and	%g1, 3, %g1
	sll	%g1, 4, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset26_count2, .-bitfield__write_var_BitField_offset26_count2
	.align 4
	.global bitfield__read_var_BitField_offset26_count3
	.hidden	bitfield__read_var_BitField_offset26_count3
	.type	bitfield__read_var_BitField_offset26_count3, #function
	.proc	04
bitfield__read_var_BitField_offset26_count3:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 26, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset26_count3, .-bitfield__read_var_BitField_offset26_count3
	.align 4
	.global bitfield__write_var_BitField_offset26_count3
	.hidden	bitfield__write_var_BitField_offset26_count3
	.type	bitfield__write_var_BitField_offset26_count3, #function
	.proc	020
bitfield__write_var_BitField_offset26_count3:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2865
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 7
	ble	%icc, .LL2866
	 nop
.LL2865:
	sethi	%hi(123904), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2866:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 3, %g3
	ld	[%fp-20], %g1
	add	%g1, 3, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -57, %g2
	ld	[%fp-24], %g1
	and	%g1, 7, %g1
	sll	%g1, 3, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset26_count3, .-bitfield__write_var_BitField_offset26_count3
	.align 4
	.global bitfield__read_var_BitField_offset26_count4
	.hidden	bitfield__read_var_BitField_offset26_count4
	.type	bitfield__read_var_BitField_offset26_count4, #function
	.proc	04
bitfield__read_var_BitField_offset26_count4:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 26, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset26_count4, .-bitfield__read_var_BitField_offset26_count4
	.align 4
	.global bitfield__write_var_BitField_offset26_count4
	.hidden	bitfield__write_var_BitField_offset26_count4
	.type	bitfield__write_var_BitField_offset26_count4, #function
	.proc	020
bitfield__write_var_BitField_offset26_count4:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2871
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 15
	ble	%icc, .LL2872
	 nop
.LL2871:
	sethi	%hi(123904), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2872:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 3, %g3
	ld	[%fp-20], %g1
	add	%g1, 3, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -61, %g2
	ld	[%fp-24], %g1
	and	%g1, 15, %g1
	sll	%g1, 2, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset26_count4, .-bitfield__write_var_BitField_offset26_count4
	.align 4
	.global bitfield__read_var_BitField_offset26_count5
	.hidden	bitfield__read_var_BitField_offset26_count5
	.type	bitfield__read_var_BitField_offset26_count5, #function
	.proc	04
bitfield__read_var_BitField_offset26_count5:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 26, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset26_count5, .-bitfield__read_var_BitField_offset26_count5
	.align 4
	.global bitfield__write_var_BitField_offset26_count5
	.hidden	bitfield__write_var_BitField_offset26_count5
	.type	bitfield__write_var_BitField_offset26_count5, #function
	.proc	020
bitfield__write_var_BitField_offset26_count5:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2877
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 31
	ble	%icc, .LL2878
	 nop
.LL2877:
	sethi	%hi(123904), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2878:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 3, %g3
	ld	[%fp-20], %g1
	add	%g1, 3, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -63, %g2
	ld	[%fp-24], %g1
	and	%g1, 31, %g1
	add	%g1, %g1, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset26_count5, .-bitfield__write_var_BitField_offset26_count5
	.align 4
	.global bitfield__read_var_BitField_offset26_count6
	.hidden	bitfield__read_var_BitField_offset26_count6
	.type	bitfield__read_var_BitField_offset26_count6, #function
	.proc	04
bitfield__read_var_BitField_offset26_count6:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, 63, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset26_count6, .-bitfield__read_var_BitField_offset26_count6
	.align 4
	.global bitfield__write_var_BitField_offset26_count6
	.hidden	bitfield__write_var_BitField_offset26_count6
	.type	bitfield__write_var_BitField_offset26_count6, #function
	.proc	020
bitfield__write_var_BitField_offset26_count6:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2883
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 63
	ble	%icc, .LL2884
	 nop
.LL2883:
	sethi	%hi(123904), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL2884:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 3, %g3
	ld	[%fp-20], %g1
	add	%g1, 3, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -64, %g2
	ld	[%fp-24], %g1
	and	%g1, 63, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset26_count6, .-bitfield__write_var_BitField_offset26_count6
	.align 4
	.global bitfield__read_var_BitField_offset27_count1
	.hidden	bitfield__read_var_BitField_offset27_count1
	.type	bitfield__read_var_BitField_offset27_count1, #function
	.proc	04
bitfield__read_var_BitField_offset27_count1:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 27, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset27_count1, .-bitfield__read_var_BitField_offset27_count1
	.align 4
	.global bitfield__write_var_BitField_offset27_count1
	.hidden	bitfield__write_var_BitField_offset27_count1
	.type	bitfield__write_var_BitField_offset27_count1, #function
	.proc	020
bitfield__write_var_BitField_offset27_count1:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2889
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1
	ble	%icc, .LL2890
	 nop
.LL2889:
	sethi	%hi(124928), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2890:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 3, %g3
	ld	[%fp-20], %g1
	add	%g1, 3, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -17, %g2
	ld	[%fp-24], %g1
	and	%g1, 1, %g1
	sll	%g1, 4, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset27_count1, .-bitfield__write_var_BitField_offset27_count1
	.align 4
	.global bitfield__read_var_BitField_offset27_count2
	.hidden	bitfield__read_var_BitField_offset27_count2
	.type	bitfield__read_var_BitField_offset27_count2, #function
	.proc	04
bitfield__read_var_BitField_offset27_count2:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 27, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset27_count2, .-bitfield__read_var_BitField_offset27_count2
	.align 4
	.global bitfield__write_var_BitField_offset27_count2
	.hidden	bitfield__write_var_BitField_offset27_count2
	.type	bitfield__write_var_BitField_offset27_count2, #function
	.proc	020
bitfield__write_var_BitField_offset27_count2:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2895
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 3
	ble	%icc, .LL2896
	 nop
.LL2895:
	sethi	%hi(124928), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2896:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 3, %g3
	ld	[%fp-20], %g1
	add	%g1, 3, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -25, %g2
	ld	[%fp-24], %g1
	and	%g1, 3, %g1
	sll	%g1, 3, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset27_count2, .-bitfield__write_var_BitField_offset27_count2
	.align 4
	.global bitfield__read_var_BitField_offset27_count3
	.hidden	bitfield__read_var_BitField_offset27_count3
	.type	bitfield__read_var_BitField_offset27_count3, #function
	.proc	04
bitfield__read_var_BitField_offset27_count3:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 27, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset27_count3, .-bitfield__read_var_BitField_offset27_count3
	.align 4
	.global bitfield__write_var_BitField_offset27_count3
	.hidden	bitfield__write_var_BitField_offset27_count3
	.type	bitfield__write_var_BitField_offset27_count3, #function
	.proc	020
bitfield__write_var_BitField_offset27_count3:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2901
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 7
	ble	%icc, .LL2902
	 nop
.LL2901:
	sethi	%hi(124928), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2902:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 3, %g3
	ld	[%fp-20], %g1
	add	%g1, 3, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -29, %g2
	ld	[%fp-24], %g1
	and	%g1, 7, %g1
	sll	%g1, 2, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset27_count3, .-bitfield__write_var_BitField_offset27_count3
	.align 4
	.global bitfield__read_var_BitField_offset27_count4
	.hidden	bitfield__read_var_BitField_offset27_count4
	.type	bitfield__read_var_BitField_offset27_count4, #function
	.proc	04
bitfield__read_var_BitField_offset27_count4:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 27, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset27_count4, .-bitfield__read_var_BitField_offset27_count4
	.align 4
	.global bitfield__write_var_BitField_offset27_count4
	.hidden	bitfield__write_var_BitField_offset27_count4
	.type	bitfield__write_var_BitField_offset27_count4, #function
	.proc	020
bitfield__write_var_BitField_offset27_count4:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2907
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 15
	ble	%icc, .LL2908
	 nop
.LL2907:
	sethi	%hi(124928), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL2908:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 3, %g3
	ld	[%fp-20], %g1
	add	%g1, 3, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -31, %g2
	ld	[%fp-24], %g1
	and	%g1, 15, %g1
	add	%g1, %g1, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset27_count4, .-bitfield__write_var_BitField_offset27_count4
	.align 4
	.global bitfield__read_var_BitField_offset27_count5
	.hidden	bitfield__read_var_BitField_offset27_count5
	.type	bitfield__read_var_BitField_offset27_count5, #function
	.proc	04
bitfield__read_var_BitField_offset27_count5:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset27_count5, .-bitfield__read_var_BitField_offset27_count5
	.align 4
	.global bitfield__write_var_BitField_offset27_count5
	.hidden	bitfield__write_var_BitField_offset27_count5
	.type	bitfield__write_var_BitField_offset27_count5, #function
	.proc	020
bitfield__write_var_BitField_offset27_count5:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2913
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 31
	ble	%icc, .LL2914
	 nop
.LL2913:
	sethi	%hi(125952), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2914:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 3, %g3
	ld	[%fp-20], %g1
	add	%g1, 3, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -32, %g2
	ld	[%fp-24], %g1
	and	%g1, 31, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset27_count5, .-bitfield__write_var_BitField_offset27_count5
	.align 4
	.global bitfield__read_var_BitField_offset28_count1
	.hidden	bitfield__read_var_BitField_offset28_count1
	.type	bitfield__read_var_BitField_offset28_count1, #function
	.proc	04
bitfield__read_var_BitField_offset28_count1:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 28, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset28_count1, .-bitfield__read_var_BitField_offset28_count1
	.align 4
	.global bitfield__write_var_BitField_offset28_count1
	.hidden	bitfield__write_var_BitField_offset28_count1
	.type	bitfield__write_var_BitField_offset28_count1, #function
	.proc	020
bitfield__write_var_BitField_offset28_count1:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2919
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1
	ble	%icc, .LL2920
	 nop
.LL2919:
	sethi	%hi(125952), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2920:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 3, %g3
	ld	[%fp-20], %g1
	add	%g1, 3, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -9, %g2
	ld	[%fp-24], %g1
	and	%g1, 1, %g1
	sll	%g1, 3, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset28_count1, .-bitfield__write_var_BitField_offset28_count1
	.align 4
	.global bitfield__read_var_BitField_offset28_count2
	.hidden	bitfield__read_var_BitField_offset28_count2
	.type	bitfield__read_var_BitField_offset28_count2, #function
	.proc	04
bitfield__read_var_BitField_offset28_count2:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 28, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset28_count2, .-bitfield__read_var_BitField_offset28_count2
	.align 4
	.global bitfield__write_var_BitField_offset28_count2
	.hidden	bitfield__write_var_BitField_offset28_count2
	.type	bitfield__write_var_BitField_offset28_count2, #function
	.proc	020
bitfield__write_var_BitField_offset28_count2:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2925
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 3
	ble	%icc, .LL2926
	 nop
.LL2925:
	sethi	%hi(125952), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2926:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 3, %g3
	ld	[%fp-20], %g1
	add	%g1, 3, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -13, %g2
	ld	[%fp-24], %g1
	and	%g1, 3, %g1
	sll	%g1, 2, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset28_count2, .-bitfield__write_var_BitField_offset28_count2
	.align 4
	.global bitfield__read_var_BitField_offset28_count3
	.hidden	bitfield__read_var_BitField_offset28_count3
	.type	bitfield__read_var_BitField_offset28_count3, #function
	.proc	04
bitfield__read_var_BitField_offset28_count3:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 28, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset28_count3, .-bitfield__read_var_BitField_offset28_count3
	.align 4
	.global bitfield__write_var_BitField_offset28_count3
	.hidden	bitfield__write_var_BitField_offset28_count3
	.type	bitfield__write_var_BitField_offset28_count3, #function
	.proc	020
bitfield__write_var_BitField_offset28_count3:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2931
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 7
	ble	%icc, .LL2932
	 nop
.LL2931:
	sethi	%hi(125952), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL2932:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 3, %g3
	ld	[%fp-20], %g1
	add	%g1, 3, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -15, %g2
	ld	[%fp-24], %g1
	and	%g1, 7, %g1
	add	%g1, %g1, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset28_count3, .-bitfield__write_var_BitField_offset28_count3
	.align 4
	.global bitfield__read_var_BitField_offset28_count4
	.hidden	bitfield__read_var_BitField_offset28_count4
	.type	bitfield__read_var_BitField_offset28_count4, #function
	.proc	04
bitfield__read_var_BitField_offset28_count4:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, 15, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset28_count4, .-bitfield__read_var_BitField_offset28_count4
	.align 4
	.global bitfield__write_var_BitField_offset28_count4
	.hidden	bitfield__write_var_BitField_offset28_count4
	.type	bitfield__write_var_BitField_offset28_count4, #function
	.proc	020
bitfield__write_var_BitField_offset28_count4:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2937
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 15
	ble	%icc, .LL2938
	 nop
.LL2937:
	sethi	%hi(126976), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2938:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 3, %g3
	ld	[%fp-20], %g1
	add	%g1, 3, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -16, %g2
	ld	[%fp-24], %g1
	and	%g1, 15, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset28_count4, .-bitfield__write_var_BitField_offset28_count4
	.align 4
	.global bitfield__read_var_BitField_offset29_count1
	.hidden	bitfield__read_var_BitField_offset29_count1
	.type	bitfield__read_var_BitField_offset29_count1, #function
	.proc	04
bitfield__read_var_BitField_offset29_count1:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 29, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset29_count1, .-bitfield__read_var_BitField_offset29_count1
	.align 4
	.global bitfield__write_var_BitField_offset29_count1
	.hidden	bitfield__write_var_BitField_offset29_count1
	.type	bitfield__write_var_BitField_offset29_count1, #function
	.proc	020
bitfield__write_var_BitField_offset29_count1:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2943
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1
	ble	%icc, .LL2944
	 nop
.LL2943:
	sethi	%hi(126976), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2944:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 3, %g3
	ld	[%fp-20], %g1
	add	%g1, 3, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -5, %g2
	ld	[%fp-24], %g1
	and	%g1, 1, %g1
	sll	%g1, 2, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset29_count1, .-bitfield__write_var_BitField_offset29_count1
	.align 4
	.global bitfield__read_var_BitField_offset29_count2
	.hidden	bitfield__read_var_BitField_offset29_count2
	.type	bitfield__read_var_BitField_offset29_count2, #function
	.proc	04
bitfield__read_var_BitField_offset29_count2:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 29, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset29_count2, .-bitfield__read_var_BitField_offset29_count2
	.align 4
	.global bitfield__write_var_BitField_offset29_count2
	.hidden	bitfield__write_var_BitField_offset29_count2
	.type	bitfield__write_var_BitField_offset29_count2, #function
	.proc	020
bitfield__write_var_BitField_offset29_count2:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2949
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 3
	ble	%icc, .LL2950
	 nop
.LL2949:
	sethi	%hi(126976), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2950:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 3, %g3
	ld	[%fp-20], %g1
	add	%g1, 3, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -7, %g2
	ld	[%fp-24], %g1
	and	%g1, 3, %g1
	add	%g1, %g1, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset29_count2, .-bitfield__write_var_BitField_offset29_count2
	.align 4
	.global bitfield__read_var_BitField_offset29_count3
	.hidden	bitfield__read_var_BitField_offset29_count3
	.type	bitfield__read_var_BitField_offset29_count3, #function
	.proc	04
bitfield__read_var_BitField_offset29_count3:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, 7, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset29_count3, .-bitfield__read_var_BitField_offset29_count3
	.align 4
	.global bitfield__write_var_BitField_offset29_count3
	.hidden	bitfield__write_var_BitField_offset29_count3
	.type	bitfield__write_var_BitField_offset29_count3, #function
	.proc	020
bitfield__write_var_BitField_offset29_count3:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2955
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 7
	ble	%icc, .LL2956
	 nop
.LL2955:
	sethi	%hi(126976), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL2956:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 3, %g3
	ld	[%fp-20], %g1
	add	%g1, 3, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -8, %g2
	ld	[%fp-24], %g1
	and	%g1, 7, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset29_count3, .-bitfield__write_var_BitField_offset29_count3
	.align 4
	.global bitfield__read_var_BitField_offset30_count1
	.hidden	bitfield__read_var_BitField_offset30_count1
	.type	bitfield__read_var_BitField_offset30_count1, #function
	.proc	04
bitfield__read_var_BitField_offset30_count1:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 30, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset30_count1, .-bitfield__read_var_BitField_offset30_count1
	.align 4
	.global bitfield__write_var_BitField_offset30_count1
	.hidden	bitfield__write_var_BitField_offset30_count1
	.type	bitfield__write_var_BitField_offset30_count1, #function
	.proc	020
bitfield__write_var_BitField_offset30_count1:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2961
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1
	ble	%icc, .LL2962
	 nop
.LL2961:
	sethi	%hi(128000), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2962:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 3, %g3
	ld	[%fp-20], %g1
	add	%g1, 3, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -3, %g2
	ld	[%fp-24], %g1
	and	%g1, 1, %g1
	add	%g1, %g1, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset30_count1, .-bitfield__write_var_BitField_offset30_count1
	.align 4
	.global bitfield__read_var_BitField_offset30_count2
	.hidden	bitfield__read_var_BitField_offset30_count2
	.type	bitfield__read_var_BitField_offset30_count2, #function
	.proc	04
bitfield__read_var_BitField_offset30_count2:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, 3, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset30_count2, .-bitfield__read_var_BitField_offset30_count2
	.align 4
	.global bitfield__write_var_BitField_offset30_count2
	.hidden	bitfield__write_var_BitField_offset30_count2
	.type	bitfield__write_var_BitField_offset30_count2, #function
	.proc	020
bitfield__write_var_BitField_offset30_count2:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2967
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 3
	ble	%icc, .LL2968
	 nop
.LL2967:
	sethi	%hi(128000), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2968:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 3, %g3
	ld	[%fp-20], %g1
	add	%g1, 3, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -4, %g2
	ld	[%fp-24], %g1
	and	%g1, 3, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset30_count2, .-bitfield__write_var_BitField_offset30_count2
	.align 4
	.global bitfield__read_var_BitField_offset31_count1
	.hidden	bitfield__read_var_BitField_offset31_count1
	.type	bitfield__read_var_BitField_offset31_count1, #function
	.proc	04
bitfield__read_var_BitField_offset31_count1:
	save	%sp, -120, %sp
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield__read_var_BitField_offset31_count1, .-bitfield__read_var_BitField_offset31_count1
	.align 4
	.global bitfield__write_var_BitField_offset31_count1
	.hidden	bitfield__write_var_BitField_offset31_count1
	.type	bitfield__write_var_BitField_offset31_count1, #function
	.proc	020
bitfield__write_var_BitField_offset31_count1:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g2
	ld	[%fp-24], %g1
	and	%g1, 0, %g1
	or	%g1, %g2, %g1
	st	%g1, [%fp-24]
	ld	[%fp-24], %g1
	cmp	%g1, 0
	bl	%icc, .LL2973
	 nop
	ld	[%fp-24], %g1
	cmp	%g1, 1
	ble	%icc, .LL2974
	 nop
.LL2973:
	sethi	%hi(128000), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2974:
	ld	[%fp+68], %g2
	ld	[%fp-20], %g1
	and	%g1, 0, %g1
	or	%g2, %g1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %g1
	add	%g1, 3, %g3
	ld	[%fp-20], %g1
	add	%g1, 3, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, -2, %g2
	ld	[%fp-24], %g1
	and	%g1, 1, %g1
	or	%g2, %g1, %g1
	stb	%g1, [%g3]
	return	%i7+8
	 nop
	.size	bitfield__write_var_BitField_offset31_count1, .-bitfield__write_var_BitField_offset31_count1
	.align 4
	.global bitfield_M3
	.type	bitfield_M3, #function
	.proc	0120
bitfield_M3:
	save	%sp, -120, %sp
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	sethi	%hi(MM_bitfield), %g1
	or	%g1, %lo(MM_bitfield), %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
	.size	bitfield_M3, .-bitfield_M3
	.align 4
	.type	_m3_fault, #function
	.proc	020
_m3_fault:
	save	%sp, -112, %sp
	st	%i0, [%fp+68]
	sethi	%hi(MM_bitfield), %g1
	or	%g1, %lo(MM_bitfield), %o0
	ld	[%fp+68], %o1
	call	RTHooks__ReportFault, 0
	 nop
	return	%i7+8
	 nop
	.size	_m3_fault, .-_m3_fault
	.section	".data"
	.align 8
	.type	L_1, #object
	.size	L_1, 42232
L_1:
	.ascii	"bitfield_M3"
	.skip 1
	.ascii	"write_var_BitField_offset31_count1"
	.skip 1
	.ascii	"read_var_BitField_offset31_count1"
	.skip 1
	.ascii	"write_var_BitField_offset30_count2"
	.skip 1
	.ascii	"read_var_BitField_offset30_count2"
	.skip 1
	.ascii	"write_var_BitField_offset30_count1"
	.skip 1
	.ascii	"read_var_BitField_offset30_count1"
	.skip 1
	.ascii	"write_var_BitField_offset29_count3"
	.skip 1
	.ascii	"read_var_BitField_offset29_count3"
	.skip 1
	.ascii	"write_var_BitField_offset29_count2"
	.skip 1
	.ascii	"read_var_BitField_offset29_count2"
	.skip 1
	.ascii	"write_var_BitField_offset29_count1"
	.skip 1
	.ascii	"read_var_BitField_offset29_count1"
	.skip 1
	.ascii	"write_var_BitField_offset28_count4"
	.skip 1
	.ascii	"read_var_BitField_offset28_count4"
	.skip 1
	.ascii	"write_var_BitField_offset28_count3"
	.skip 1
	.ascii	"read_var_BitField_offset28_count3"
	.skip 1
	.ascii	"write_var_BitField_offset28_count2"
	.skip 1
	.ascii	"read_var_BitField_offset28_count2"
	.skip 1
	.ascii	"write_var_BitField_offset28_count1"
	.skip 1
	.ascii	"read_var_BitField_offset28_count1"
	.skip 1
	.ascii	"write_var_BitField_offset27_count5"
	.skip 1
	.ascii	"read_var_BitField_offset27_count5"
	.skip 1
	.ascii	"write_var_BitField_offset27_count4"
	.skip 1
	.ascii	"read_var_BitField_offset27_count4"
	.skip 1
	.ascii	"write_var_BitField_offset27_count3"
	.skip 1
	.ascii	"read_var_BitField_offset27_count3"
	.skip 1
	.ascii	"write_var_BitField_offset27_count2"
	.skip 1
	.ascii	"read_var_BitField_offset27_count2"
	.skip 1
	.ascii	"write_var_BitField_offset27_count1"
	.skip 1
	.ascii	"read_var_BitField_offset27_count1"
	.skip 1
	.ascii	"write_var_BitField_offset26_count6"
	.skip 1
	.ascii	"read_var_BitField_offset26_count6"
	.skip 1
	.ascii	"write_var_BitField_offset26_count5"
	.skip 1
	.ascii	"read_var_BitField_offset26_count5"
	.skip 1
	.ascii	"write_var_BitField_offset26_count4"
	.skip 1
	.ascii	"read_var_BitField_offset26_count4"
	.skip 1
	.ascii	"write_var_BitField_offset26_count3"
	.skip 1
	.ascii	"read_var_BitField_offset26_count3"
	.skip 1
	.ascii	"write_var_BitField_offset26_count2"
	.skip 1
	.ascii	"read_var_BitField_offset26_count2"
	.skip 1
	.ascii	"write_var_BitField_offset26_count1"
	.skip 1
	.ascii	"read_var_BitField_offset26_count1"
	.skip 1
	.ascii	"write_var_BitField_offset25_count7"
	.skip 1
	.ascii	"read_var_BitField_offset25_count7"
	.skip 1
	.ascii	"write_var_BitField_offset25_count6"
	.skip 1
	.ascii	"read_var_BitField_offset25_count6"
	.skip 1
	.ascii	"write_var_BitField_offset25_count5"
	.skip 1
	.ascii	"read_var_BitField_offset25_count5"
	.skip 1
	.ascii	"write_var_BitField_offset25_count4"
	.skip 1
	.ascii	"read_var_BitField_offset25_count4"
	.skip 1
	.ascii	"write_var_BitField_offset25_count3"
	.skip 1
	.ascii	"read_var_BitField_offset25_count3"
	.skip 1
	.ascii	"write_var_BitField_offset25_count2"
	.skip 1
	.ascii	"read_var_BitField_offset25_count2"
	.skip 1
	.ascii	"write_var_BitField_offset25_count1"
	.skip 1
	.ascii	"read_var_BitField_offset25_count1"
	.skip 1
	.ascii	"write_var_BitField_offset24_count8"
	.skip 1
	.ascii	"read_var_BitField_offset24_count8"
	.skip 1
	.ascii	"write_var_BitField_offset24_count7"
	.skip 1
	.ascii	"read_var_BitField_offset24_count7"
	.skip 1
	.ascii	"write_var_BitField_offset24_count6"
	.skip 1
	.ascii	"read_var_BitField_offset24_count6"
	.skip 1
	.ascii	"write_var_BitField_offset24_count5"
	.skip 1
	.ascii	"read_var_BitField_offset24_count5"
	.skip 1
	.ascii	"write_var_BitField_offset24_count4"
	.skip 1
	.ascii	"read_var_BitField_offset24_count4"
	.skip 1
	.ascii	"write_var_BitField_offset24_count3"
	.skip 1
	.ascii	"read_var_BitField_offset24_count3"
	.skip 1
	.ascii	"write_var_BitField_offset24_count2"
	.skip 1
	.ascii	"read_var_BitField_offset24_count2"
	.skip 1
	.ascii	"write_var_BitField_offset24_count1"
	.skip 1
	.ascii	"read_var_BitField_offset24_count1"
	.skip 1
	.ascii	"write_var_BitField_offset23_count9"
	.skip 1
	.ascii	"read_var_BitField_offset23_count9"
	.skip 1
	.ascii	"write_var_BitField_offset23_count8"
	.skip 1
	.ascii	"read_var_BitField_offset23_count8"
	.skip 1
	.ascii	"write_var_BitField_offset23_count7"
	.skip 1
	.ascii	"read_var_BitField_offset23_count7"
	.skip 1
	.ascii	"write_var_BitField_offset23_count6"
	.skip 1
	.ascii	"read_var_BitField_offset23_count6"
	.skip 1
	.ascii	"write_var_BitField_offset23_count5"
	.skip 1
	.ascii	"read_var_BitField_offset23_count5"
	.skip 1
	.ascii	"write_var_BitField_offset23_count4"
	.skip 1
	.ascii	"read_var_BitField_offset23_count4"
	.skip 1
	.ascii	"write_var_BitField_offset23_count3"
	.skip 1
	.ascii	"read_var_BitField_offset23_count3"
	.skip 1
	.ascii	"write_var_BitField_offset23_count2"
	.skip 1
	.ascii	"read_var_BitField_offset23_count2"
	.skip 1
	.ascii	"write_var_BitField_offset23_count1"
	.skip 1
	.ascii	"read_var_BitField_offset23_count1"
	.skip 1
	.ascii	"write_var_BitField_offset22_count10"
	.skip 1
	.ascii	"read_var_BitField_offset22_count10"
	.skip 1
	.ascii	"write_var_BitField_offset22_count9"
	.skip 1
	.ascii	"read_var_BitField_offset22_count9"
	.skip 1
	.ascii	"write_var_BitField_offset22_count8"
	.skip 1
	.ascii	"read_var_BitField_offset22_count8"
	.skip 1
	.ascii	"write_var_BitField_offset22_count7"
	.skip 1
	.ascii	"read_var_BitField_offset22_count7"
	.skip 1
	.ascii	"write_var_BitField_offset22_count6"
	.skip 1
	.ascii	"read_var_BitField_offset22_count6"
	.skip 1
	.ascii	"write_var_BitField_offset22_count5"
	.skip 1
	.ascii	"read_var_BitField_offset22_count5"
	.skip 1
	.ascii	"write_var_BitField_offset22_count4"
	.skip 1
	.ascii	"read_var_BitField_offset22_count4"
	.skip 1
	.ascii	"write_var_BitField_offset22_count3"
	.skip 1
	.ascii	"read_var_BitField_offset22_count3"
	.skip 1
	.ascii	"write_var_BitField_offset22_count2"
	.skip 1
	.ascii	"read_var_BitField_offset22_count2"
	.skip 1
	.ascii	"write_var_BitField_offset22_count1"
	.skip 1
	.ascii	"read_var_BitField_offset22_count1"
	.skip 1
	.ascii	"write_var_BitField_offset21_count11"
	.skip 1
	.ascii	"read_var_BitField_offset21_count11"
	.skip 1
	.ascii	"write_var_BitField_offset21_count10"
	.skip 1
	.ascii	"read_var_BitField_offset21_count10"
	.skip 1
	.ascii	"write_var_BitField_offset21_count9"
	.skip 1
	.ascii	"read_var_BitField_offset21_count9"
	.skip 1
	.ascii	"write_var_BitField_offset21_count8"
	.skip 1
	.ascii	"read_var_BitField_offset21_count8"
	.skip 1
	.ascii	"write_var_BitField_offset21_count7"
	.skip 1
	.ascii	"read_var_BitField_offset21_count7"
	.skip 1
	.ascii	"write_var_BitField_offset21_count6"
	.skip 1
	.ascii	"read_var_BitField_offset21_count6"
	.skip 1
	.ascii	"write_var_BitField_offset21_count5"
	.skip 1
	.ascii	"read_var_BitField_offset21_count5"
	.skip 1
	.ascii	"write_var_BitField_offset21_count4"
	.skip 1
	.ascii	"read_var_BitField_offset21_count4"
	.skip 1
	.ascii	"write_var_BitField_offset21_count3"
	.skip 1
	.ascii	"read_var_BitField_offset21_count3"
	.skip 1
	.ascii	"write_var_BitField_offset21_count2"
	.skip 1
	.ascii	"read_var_BitField_offset21_count2"
	.skip 1
	.ascii	"write_var_BitField_offset21_count1"
	.skip 1
	.ascii	"read_var_BitField_offset21_count1"
	.skip 1
	.ascii	"write_var_BitField_offset20_count12"
	.skip 1
	.ascii	"read_var_BitField_offset20_count12"
	.skip 1
	.ascii	"write_var_BitField_offset20_count11"
	.skip 1
	.ascii	"read_var_BitField_offset20_count11"
	.skip 1
	.ascii	"write_var_BitField_offset20_count10"
	.skip 1
	.ascii	"read_var_BitField_offset20_count10"
	.skip 1
	.ascii	"write_var_BitField_offset20_count9"
	.skip 1
	.ascii	"read_var_BitField_offset20_count9"
	.skip 1
	.ascii	"write_var_BitField_offset20_count8"
	.skip 1
	.ascii	"read_var_BitField_offset20_count8"
	.skip 1
	.ascii	"write_var_BitField_offset20_count7"
	.skip 1
	.ascii	"read_var_BitField_offset20_count7"
	.skip 1
	.ascii	"write_var_BitField_offset20_count6"
	.skip 1
	.ascii	"read_var_BitField_offset20_count6"
	.skip 1
	.ascii	"write_var_BitField_offset20_count5"
	.skip 1
	.ascii	"read_var_BitField_offset20_count5"
	.skip 1
	.ascii	"write_var_BitField_offset20_count4"
	.skip 1
	.ascii	"read_var_BitField_offset20_count4"
	.skip 1
	.ascii	"write_var_BitField_offset20_count3"
	.skip 1
	.ascii	"read_var_BitField_offset20_count3"
	.skip 1
	.ascii	"write_var_BitField_offset20_count2"
	.skip 1
	.ascii	"read_var_BitField_offset20_count2"
	.skip 1
	.ascii	"write_var_BitField_offset20_count1"
	.skip 1
	.ascii	"read_var_BitField_offset20_count1"
	.skip 1
	.ascii	"write_var_BitField_offset19_count13"
	.skip 1
	.ascii	"read_var_BitField_offset19_count13"
	.skip 1
	.ascii	"write_var_BitField_offset19_count12"
	.skip 1
	.ascii	"read_var_BitField_offset19_count12"
	.skip 1
	.ascii	"write_var_BitField_offset19_count11"
	.skip 1
	.ascii	"read_var_BitField_offset19_count11"
	.skip 1
	.ascii	"write_var_BitField_offset19_count10"
	.skip 1
	.ascii	"read_var_BitField_offset19_count10"
	.skip 1
	.ascii	"write_var_BitField_offset19_count9"
	.skip 1
	.ascii	"read_var_BitField_offset19_count9"
	.skip 1
	.ascii	"write_var_BitField_offset19_count8"
	.skip 1
	.ascii	"read_var_BitField_offset19_count8"
	.skip 1
	.ascii	"write_var_BitField_offset19_count7"
	.skip 1
	.ascii	"read_var_BitField_offset19_count7"
	.skip 1
	.ascii	"write_var_BitField_offset19_count6"
	.skip 1
	.ascii	"read_var_BitField_offset19_count6"
	.skip 1
	.ascii	"write_var_BitField_offset19_count5"
	.skip 1
	.ascii	"read_var_BitField_offset19_count5"
	.skip 1
	.ascii	"write_var_BitField_offset19_count4"
	.skip 1
	.ascii	"read_var_BitField_offset19_count4"
	.skip 1
	.ascii	"write_var_BitField_offset19_count3"
	.skip 1
	.ascii	"read_var_BitField_offset19_count3"
	.skip 1
	.ascii	"write_var_BitField_offset19_count2"
	.skip 1
	.ascii	"read_var_BitField_offset19_count2"
	.skip 1
	.ascii	"write_var_BitField_offset19_count1"
	.skip 1
	.ascii	"read_var_BitField_offset19_count1"
	.skip 1
	.ascii	"write_var_BitField_offset18_count14"
	.skip 1
	.ascii	"read_var_BitField_offset18_count14"
	.skip 1
	.ascii	"write_var_BitField_offset18_count13"
	.skip 1
	.ascii	"read_var_BitField_offset18_count13"
	.skip 1
	.ascii	"write_var_BitField_offset18_count12"
	.skip 1
	.ascii	"read_var_BitField_offset18_count12"
	.skip 1
	.ascii	"write_var_BitField_offset18_count11"
	.skip 1
	.ascii	"read_var_BitField_offset18_count11"
	.skip 1
	.ascii	"write_var_BitField_offset18_count10"
	.skip 1
	.ascii	"read_var_BitField_offset18_count10"
	.skip 1
	.ascii	"write_var_BitField_offset18_count9"
	.skip 1
	.ascii	"read_var_BitField_offset18_count9"
	.skip 1
	.ascii	"write_var_BitField_offset18_count8"
	.skip 1
	.ascii	"read_var_BitField_offset18_count8"
	.skip 1
	.ascii	"write_var_BitField_offset18_count7"
	.skip 1
	.ascii	"read_var_BitField_offset18_count7"
	.skip 1
	.ascii	"write_var_BitField_offset18_count6"
	.skip 1
	.ascii	"read_var_BitField_offset18_count6"
	.skip 1
	.ascii	"write_var_BitField_offset18_count5"
	.skip 1
	.ascii	"read_var_BitField_offset18_count5"
	.skip 1
	.ascii	"write_var_BitField_offset18_count4"
	.skip 1
	.ascii	"read_var_BitField_offset18_count4"
	.skip 1
	.ascii	"write_var_BitField_offset18_count3"
	.skip 1
	.ascii	"read_var_BitField_offset18_count3"
	.skip 1
	.ascii	"write_var_BitField_offset18_count2"
	.skip 1
	.ascii	"read_var_BitField_offset18_count2"
	.skip 1
	.ascii	"write_var_BitField_offset18_count1"
	.skip 1
	.ascii	"read_var_BitField_offset18_count1"
	.skip 1
	.ascii	"write_var_BitField_offset17_count15"
	.skip 1
	.ascii	"read_var_BitField_offset17_count15"
	.skip 1
	.ascii	"write_var_BitField_offset17_count14"
	.skip 1
	.ascii	"read_var_BitField_offset17_count14"
	.skip 1
	.ascii	"write_var_BitField_offset17_count13"
	.skip 1
	.ascii	"read_var_BitField_offset17_count13"
	.skip 1
	.ascii	"write_var_BitField_offset17_count12"
	.skip 1
	.ascii	"read_var_BitField_offset17_count12"
	.skip 1
	.ascii	"write_var_BitField_offset17_count11"
	.skip 1
	.ascii	"read_var_BitField_offset17_count11"
	.skip 1
	.ascii	"write_var_BitField_offset17_count10"
	.skip 1
	.ascii	"read_var_BitField_offset17_count10"
	.skip 1
	.ascii	"write_var_BitField_offset17_count9"
	.skip 1
	.ascii	"read_var_BitField_offset17_count9"
	.skip 1
	.ascii	"write_var_BitField_offset17_count8"
	.skip 1
	.ascii	"read_var_BitField_offset17_count8"
	.skip 1
	.ascii	"write_var_BitField_offset17_count7"
	.skip 1
	.ascii	"read_var_BitField_offset17_count7"
	.skip 1
	.ascii	"write_var_BitField_offset17_count6"
	.skip 1
	.ascii	"read_var_BitField_offset17_count6"
	.skip 1
	.ascii	"write_var_BitField_offset17_count5"
	.skip 1
	.ascii	"read_var_BitField_offset17_count5"
	.skip 1
	.ascii	"write_var_BitField_offset17_count4"
	.skip 1
	.ascii	"read_var_BitField_offset17_count4"
	.skip 1
	.ascii	"write_var_BitField_offset17_count3"
	.skip 1
	.ascii	"read_var_BitField_offset17_count3"
	.skip 1
	.ascii	"write_var_BitField_offset17_count2"
	.skip 1
	.ascii	"read_var_BitField_offset17_count2"
	.skip 1
	.ascii	"write_var_BitField_offset17_count1"
	.skip 1
	.ascii	"read_var_BitField_offset17_count1"
	.skip 1
	.ascii	"write_var_BitField_offset16_count16"
	.skip 1
	.ascii	"read_var_BitField_offset16_count16"
	.skip 1
	.ascii	"write_var_BitField_offset16_count15"
	.skip 1
	.ascii	"read_var_BitField_offset16_count15"
	.skip 1
	.ascii	"write_var_BitField_offset16_count14"
	.skip 1
	.ascii	"read_var_BitField_offset16_count14"
	.skip 1
	.ascii	"write_var_BitField_offset16_count13"
	.skip 1
	.ascii	"read_var_BitField_offset16_count13"
	.skip 1
	.ascii	"write_var_BitField_offset16_count12"
	.skip 1
	.ascii	"read_var_BitField_offset16_count12"
	.skip 1
	.ascii	"write_var_BitField_offset16_count11"
	.skip 1
	.ascii	"read_var_BitField_offset16_count11"
	.skip 1
	.ascii	"write_var_BitField_offset16_count10"
	.skip 1
	.ascii	"read_var_BitField_offset16_count10"
	.skip 1
	.ascii	"write_var_BitField_offset16_count9"
	.skip 1
	.ascii	"read_var_BitField_offset16_count9"
	.skip 1
	.ascii	"write_var_BitField_offset16_count8"
	.skip 1
	.ascii	"read_var_BitField_offset16_count8"
	.skip 1
	.ascii	"write_var_BitField_offset16_count7"
	.skip 1
	.ascii	"read_var_BitField_offset16_count7"
	.skip 1
	.ascii	"write_var_BitField_offset16_count6"
	.skip 1
	.ascii	"read_var_BitField_offset16_count6"
	.skip 1
	.ascii	"write_var_BitField_offset16_count5"
	.skip 1
	.ascii	"read_var_BitField_offset16_count5"
	.skip 1
	.ascii	"write_var_BitField_offset16_count4"
	.skip 1
	.ascii	"read_var_BitField_offset16_count4"
	.skip 1
	.ascii	"write_var_BitField_offset16_count3"
	.skip 1
	.ascii	"read_var_BitField_offset16_count3"
	.skip 1
	.ascii	"write_var_BitField_offset16_count2"
	.skip 1
	.ascii	"read_var_BitField_offset16_count2"
	.skip 1
	.ascii	"write_var_BitField_offset16_count1"
	.skip 1
	.ascii	"read_var_BitField_offset16_count1"
	.skip 1
	.ascii	"write_var_BitField_offset15_count17"
	.skip 1
	.ascii	"read_var_BitField_offset15_count17"
	.skip 1
	.ascii	"write_var_BitField_offset15_count16"
	.skip 1
	.ascii	"read_var_BitField_offset15_count16"
	.skip 1
	.ascii	"write_var_BitField_offset15_count15"
	.skip 1
	.ascii	"read_var_BitField_offset15_count15"
	.skip 1
	.ascii	"write_var_BitField_offset15_count14"
	.skip 1
	.ascii	"read_var_BitField_offset15_count14"
	.skip 1
	.ascii	"write_var_BitField_offset15_count13"
	.skip 1
	.ascii	"read_var_BitField_offset15_count13"
	.skip 1
	.ascii	"write_var_BitField_offset15_count12"
	.skip 1
	.ascii	"read_var_BitField_offset15_count12"
	.skip 1
	.ascii	"write_var_BitField_offset15_count11"
	.skip 1
	.ascii	"read_var_BitField_offset15_count11"
	.skip 1
	.ascii	"write_var_BitField_offset15_count10"
	.skip 1
	.ascii	"read_var_BitField_offset15_count10"
	.skip 1
	.ascii	"write_var_BitField_offset15_count9"
	.skip 1
	.ascii	"read_var_BitField_offset15_count9"
	.skip 1
	.ascii	"write_var_BitField_offset15_count8"
	.skip 1
	.ascii	"read_var_BitField_offset15_count8"
	.skip 1
	.ascii	"write_var_BitField_offset15_count7"
	.skip 1
	.ascii	"read_var_BitField_offset15_count7"
	.skip 1
	.ascii	"write_var_BitField_offset15_count6"
	.skip 1
	.ascii	"read_var_BitField_offset15_count6"
	.skip 1
	.ascii	"write_var_BitField_offset15_count5"
	.skip 1
	.ascii	"read_var_BitField_offset15_count5"
	.skip 1
	.ascii	"write_var_BitField_offset15_count4"
	.skip 1
	.ascii	"read_var_BitField_offset15_count4"
	.skip 1
	.ascii	"write_var_BitField_offset15_count3"
	.skip 1
	.ascii	"read_var_BitField_offset15_count3"
	.skip 1
	.ascii	"write_var_BitField_offset15_count2"
	.skip 1
	.ascii	"read_var_BitField_offset15_count2"
	.skip 1
	.ascii	"write_var_BitField_offset15_count1"
	.skip 1
	.ascii	"read_var_BitField_offset15_count1"
	.skip 1
	.ascii	"write_var_BitField_offset14_count18"
	.skip 1
	.ascii	"read_var_BitField_offset14_count18"
	.skip 1
	.ascii	"write_var_BitField_offset14_count17"
	.skip 1
	.ascii	"read_var_BitField_offset14_count17"
	.skip 1
	.ascii	"write_var_BitField_offset14_count16"
	.skip 1
	.ascii	"read_var_BitField_offset14_count16"
	.skip 1
	.ascii	"write_var_BitField_offset14_count15"
	.skip 1
	.ascii	"read_var_BitField_offset14_count15"
	.skip 1
	.ascii	"write_var_BitField_offset14_count14"
	.skip 1
	.ascii	"read_var_BitField_offset14_count14"
	.skip 1
	.ascii	"write_var_BitField_offset14_count13"
	.skip 1
	.ascii	"read_var_BitField_offset14_count13"
	.skip 1
	.ascii	"write_var_BitField_offset14_count12"
	.skip 1
	.ascii	"read_var_BitField_offset14_count12"
	.skip 1
	.ascii	"write_var_BitField_offset14_count11"
	.skip 1
	.ascii	"read_var_BitField_offset14_count11"
	.skip 1
	.ascii	"write_var_BitField_offset14_count10"
	.skip 1
	.ascii	"read_var_BitField_offset14_count10"
	.skip 1
	.ascii	"write_var_BitField_offset14_count9"
	.skip 1
	.ascii	"read_var_BitField_offset14_count9"
	.skip 1
	.ascii	"write_var_BitField_offset14_count8"
	.skip 1
	.ascii	"read_var_BitField_offset14_count8"
	.skip 1
	.ascii	"write_var_BitField_offset14_count7"
	.skip 1
	.ascii	"read_var_BitField_offset14_count7"
	.skip 1
	.ascii	"write_var_BitField_offset14_count6"
	.skip 1
	.ascii	"read_var_BitField_offset14_count6"
	.skip 1
	.ascii	"write_var_BitField_offset14_count5"
	.skip 1
	.ascii	"read_var_BitField_offset14_count5"
	.skip 1
	.ascii	"write_var_BitField_offset14_count4"
	.skip 1
	.ascii	"read_var_BitField_offset14_count4"
	.skip 1
	.ascii	"write_var_BitField_offset14_count3"
	.skip 1
	.ascii	"read_var_BitField_offset14_count3"
	.skip 1
	.ascii	"write_var_BitField_offset14_count2"
	.skip 1
	.ascii	"read_var_BitField_offset14_count2"
	.skip 1
	.ascii	"write_var_BitField_offset14_count1"
	.skip 1
	.ascii	"read_var_BitField_offset14_count1"
	.skip 1
	.ascii	"write_var_BitField_offset13_count19"
	.skip 1
	.ascii	"read_var_BitField_offset13_count19"
	.skip 1
	.ascii	"write_var_BitField_offset13_count18"
	.skip 1
	.ascii	"read_var_BitField_offset13_count18"
	.skip 1
	.ascii	"write_var_BitField_offset13_count17"
	.skip 1
	.ascii	"read_var_BitField_offset13_count17"
	.skip 1
	.ascii	"write_var_BitField_offset13_count16"
	.skip 1
	.ascii	"read_var_BitField_offset13_count16"
	.skip 1
	.ascii	"write_var_BitField_offset13_count15"
	.skip 1
	.ascii	"read_var_BitField_offset13_count15"
	.skip 1
	.ascii	"write_var_BitField_offset13_count14"
	.skip 1
	.ascii	"read_var_BitField_offset13_count14"
	.skip 1
	.ascii	"write_var_BitField_offset13_count13"
	.skip 1
	.ascii	"read_var_BitField_offset13_count13"
	.skip 1
	.ascii	"write_var_BitField_offset13_count12"
	.skip 1
	.ascii	"read_var_BitField_offset13_count12"
	.skip 1
	.ascii	"write_var_BitField_offset13_count11"
	.skip 1
	.ascii	"read_var_BitField_offset13_count11"
	.skip 1
	.ascii	"write_var_BitField_offset13_count10"
	.skip 1
	.ascii	"read_var_BitField_offset13_count10"
	.skip 1
	.ascii	"write_var_BitField_offset13_count9"
	.skip 1
	.ascii	"read_var_BitField_offset13_count9"
	.skip 1
	.ascii	"write_var_BitField_offset13_count8"
	.skip 1
	.ascii	"read_var_BitField_offset13_count8"
	.skip 1
	.ascii	"write_var_BitField_offset13_count7"
	.skip 1
	.ascii	"read_var_BitField_offset13_count7"
	.skip 1
	.ascii	"write_var_BitField_offset13_count6"
	.skip 1
	.ascii	"read_var_BitField_offset13_count6"
	.skip 1
	.ascii	"write_var_BitField_offset13_count5"
	.skip 1
	.ascii	"read_var_BitField_offset13_count5"
	.skip 1
	.ascii	"write_var_BitField_offset13_count4"
	.skip 1
	.ascii	"read_var_BitField_offset13_count4"
	.skip 1
	.ascii	"write_var_BitField_offset13_count3"
	.skip 1
	.ascii	"read_var_BitField_offset13_count3"
	.skip 1
	.ascii	"write_var_BitField_offset13_count2"
	.skip 1
	.ascii	"read_var_BitField_offset13_count2"
	.skip 1
	.ascii	"write_var_BitField_offset13_count1"
	.skip 1
	.ascii	"read_var_BitField_offset13_count1"
	.skip 1
	.ascii	"write_var_BitField_offset12_count20"
	.skip 1
	.ascii	"read_var_BitField_offset12_count20"
	.skip 1
	.ascii	"write_var_BitField_offset12_count19"
	.skip 1
	.ascii	"read_var_BitField_offset12_count19"
	.skip 1
	.ascii	"write_var_BitField_offset12_count18"
	.skip 1
	.ascii	"read_var_BitField_offset12_count18"
	.skip 1
	.ascii	"write_var_BitField_offset12_count17"
	.skip 1
	.ascii	"read_var_BitField_offset12_count17"
	.skip 1
	.ascii	"write_var_BitField_offset12_count16"
	.skip 1
	.ascii	"read_var_BitField_offset12_count16"
	.skip 1
	.ascii	"write_var_BitField_offset12_count15"
	.skip 1
	.ascii	"read_var_BitField_offset12_count15"
	.skip 1
	.ascii	"write_var_BitField_offset12_count14"
	.skip 1
	.ascii	"read_var_BitField_offset12_count14"
	.skip 1
	.ascii	"write_var_BitField_offset12_count13"
	.skip 1
	.ascii	"read_var_BitField_offset12_count13"
	.skip 1
	.ascii	"write_var_BitField_offset12_count12"
	.skip 1
	.ascii	"read_var_BitField_offset12_count12"
	.skip 1
	.ascii	"write_var_BitField_offset12_count11"
	.skip 1
	.ascii	"read_var_BitField_offset12_count11"
	.skip 1
	.ascii	"write_var_BitField_offset12_count10"
	.skip 1
	.ascii	"read_var_BitField_offset12_count10"
	.skip 1
	.ascii	"write_var_BitField_offset12_count9"
	.skip 1
	.ascii	"read_var_BitField_offset12_count9"
	.skip 1
	.ascii	"write_var_BitField_offset12_count8"
	.skip 1
	.ascii	"read_var_BitField_offset12_count8"
	.skip 1
	.ascii	"write_var_BitField_offset12_count7"
	.skip 1
	.ascii	"read_var_BitField_offset12_count7"
	.skip 1
	.ascii	"write_var_BitField_offset12_count6"
	.skip 1
	.ascii	"read_var_BitField_offset12_count6"
	.skip 1
	.ascii	"write_var_BitField_offset12_count5"
	.skip 1
	.ascii	"read_var_BitField_offset12_count5"
	.skip 1
	.ascii	"write_var_BitField_offset12_count4"
	.skip 1
	.ascii	"read_var_BitField_offset12_count4"
	.skip 1
	.ascii	"write_var_BitField_offset12_count3"
	.skip 1
	.ascii	"read_var_BitField_offset12_count3"
	.skip 1
	.ascii	"write_var_BitField_offset12_count2"
	.skip 1
	.ascii	"read_var_BitField_offset12_count2"
	.skip 1
	.ascii	"write_var_BitField_offset12_count1"
	.skip 1
	.ascii	"read_var_BitField_offset12_count1"
	.skip 1
	.ascii	"write_var_BitField_offset11_count21"
	.skip 1
	.ascii	"read_var_BitField_offset11_count21"
	.skip 1
	.ascii	"write_var_BitField_offset11_count20"
	.skip 1
	.ascii	"read_var_BitField_offset11_count20"
	.skip 1
	.ascii	"write_var_BitField_offset11_count19"
	.skip 1
	.ascii	"read_var_BitField_offset11_count19"
	.skip 1
	.ascii	"write_var_BitField_offset11_count18"
	.skip 1
	.ascii	"read_var_BitField_offset11_count18"
	.skip 1
	.ascii	"write_var_BitField_offset11_count17"
	.skip 1
	.ascii	"read_var_BitField_offset11_count17"
	.skip 1
	.ascii	"write_var_BitField_offset11_count16"
	.skip 1
	.ascii	"read_var_BitField_offset11_count16"
	.skip 1
	.ascii	"write_var_BitField_offset11_count15"
	.skip 1
	.ascii	"read_var_BitField_offset11_count15"
	.skip 1
	.ascii	"write_var_BitField_offset11_count14"
	.skip 1
	.ascii	"read_var_BitField_offset11_count14"
	.skip 1
	.ascii	"write_var_BitField_offset11_count13"
	.skip 1
	.ascii	"read_var_BitField_offset11_count13"
	.skip 1
	.ascii	"write_var_BitField_offset11_count12"
	.skip 1
	.ascii	"read_var_BitField_offset11_count12"
	.skip 1
	.ascii	"write_var_BitField_offset11_count11"
	.skip 1
	.ascii	"read_var_BitField_offset11_count11"
	.skip 1
	.ascii	"write_var_BitField_offset11_count10"
	.skip 1
	.ascii	"read_var_BitField_offset11_count10"
	.skip 1
	.ascii	"write_var_BitField_offset11_count9"
	.skip 1
	.ascii	"read_var_BitField_offset11_count9"
	.skip 1
	.ascii	"write_var_BitField_offset11_count8"
	.skip 1
	.ascii	"read_var_BitField_offset11_count8"
	.skip 1
	.ascii	"write_var_BitField_offset11_count7"
	.skip 1
	.ascii	"read_var_BitField_offset11_count7"
	.skip 1
	.ascii	"write_var_BitField_offset11_count6"
	.skip 1
	.ascii	"read_var_BitField_offset11_count6"
	.skip 1
	.ascii	"write_var_BitField_offset11_count5"
	.skip 1
	.ascii	"read_var_BitField_offset11_count5"
	.skip 1
	.ascii	"write_var_BitField_offset11_count4"
	.skip 1
	.ascii	"read_var_BitField_offset11_count4"
	.skip 1
	.ascii	"write_var_BitField_offset11_count3"
	.skip 1
	.ascii	"read_var_BitField_offset11_count3"
	.skip 1
	.ascii	"write_var_BitField_offset11_count2"
	.skip 1
	.ascii	"read_var_BitField_offset11_count2"
	.skip 1
	.ascii	"write_var_BitField_offset11_count1"
	.skip 1
	.ascii	"read_var_BitField_offset11_count1"
	.skip 1
	.ascii	"write_var_BitField_offset10_count22"
	.skip 1
	.ascii	"read_var_BitField_offset10_count22"
	.skip 1
	.ascii	"write_var_BitField_offset10_count21"
	.skip 1
	.ascii	"read_var_BitField_offset10_count21"
	.skip 1
	.ascii	"write_var_BitField_offset10_count20"
	.skip 1
	.ascii	"read_var_BitField_offset10_count20"
	.skip 1
	.ascii	"write_var_BitField_offset10_count19"
	.skip 1
	.ascii	"read_var_BitField_offset10_count19"
	.skip 1
	.ascii	"write_var_BitField_offset10_count18"
	.skip 1
	.ascii	"read_var_BitField_offset10_count18"
	.skip 1
	.ascii	"write_var_BitField_offset10_count17"
	.skip 1
	.ascii	"read_var_BitField_offset10_count17"
	.skip 1
	.ascii	"write_var_BitField_offset10_count16"
	.skip 1
	.ascii	"read_var_BitField_offset10_count16"
	.skip 1
	.ascii	"write_var_BitField_offset10_count15"
	.skip 1
	.ascii	"read_var_BitField_offset10_count15"
	.skip 1
	.ascii	"write_var_BitField_offset10_count14"
	.skip 1
	.ascii	"read_var_BitField_offset10_count14"
	.skip 1
	.ascii	"write_var_BitField_offset10_count13"
	.skip 1
	.ascii	"read_var_BitField_offset10_count13"
	.skip 1
	.ascii	"write_var_BitField_offset10_count12"
	.skip 1
	.ascii	"read_var_BitField_offset10_count12"
	.skip 1
	.ascii	"write_var_BitField_offset10_count11"
	.skip 1
	.ascii	"read_var_BitField_offset10_count11"
	.skip 1
	.ascii	"write_var_BitField_offset10_count10"
	.skip 1
	.ascii	"read_var_BitField_offset10_count10"
	.skip 1
	.ascii	"write_var_BitField_offset10_count9"
	.skip 1
	.ascii	"read_var_BitField_offset10_count9"
	.skip 1
	.ascii	"write_var_BitField_offset10_count8"
	.skip 1
	.ascii	"read_var_BitField_offset10_count8"
	.skip 1
	.ascii	"write_var_BitField_offset10_count7"
	.skip 1
	.ascii	"read_var_BitField_offset10_count7"
	.skip 1
	.ascii	"write_var_BitField_offset10_count6"
	.skip 1
	.ascii	"read_var_BitField_offset10_count6"
	.skip 1
	.ascii	"write_var_BitField_offset10_count5"
	.skip 1
	.ascii	"read_var_BitField_offset10_count5"
	.skip 1
	.ascii	"write_var_BitField_offset10_count4"
	.skip 1
	.ascii	"read_var_BitField_offset10_count4"
	.skip 1
	.ascii	"write_var_BitField_offset10_count3"
	.skip 1
	.ascii	"read_var_BitField_offset10_count3"
	.skip 1
	.ascii	"write_var_BitField_offset10_count2"
	.skip 1
	.ascii	"read_var_BitField_offset10_count2"
	.skip 1
	.ascii	"write_var_BitField_offset10_count1"
	.skip 1
	.ascii	"read_var_BitField_offset10_count1"
	.skip 1
	.ascii	"write_var_BitField_offset9_count23"
	.skip 1
	.ascii	"read_var_BitField_offset9_count23"
	.skip 1
	.ascii	"write_var_BitField_offset9_count22"
	.skip 1
	.ascii	"read_var_BitField_offset9_count22"
	.skip 1
	.ascii	"write_var_BitField_offset9_count21"
	.skip 1
	.ascii	"read_var_BitField_offset9_count21"
	.skip 1
	.ascii	"write_var_BitField_offset9_count20"
	.skip 1
	.ascii	"read_var_BitField_offset9_count20"
	.skip 1
	.ascii	"write_var_BitField_offset9_count19"
	.skip 1
	.ascii	"read_var_BitField_offset9_count19"
	.skip 1
	.ascii	"write_var_BitField_offset9_count18"
	.skip 1
	.ascii	"read_var_BitField_offset9_count18"
	.skip 1
	.ascii	"write_var_BitField_offset9_count17"
	.skip 1
	.ascii	"read_var_BitField_offset9_count17"
	.skip 1
	.ascii	"write_var_BitField_offset9_count16"
	.skip 1
	.ascii	"read_var_BitField_offset9_count16"
	.skip 1
	.ascii	"write_var_BitField_offset9_count15"
	.skip 1
	.ascii	"read_var_BitField_offset9_count15"
	.skip 1
	.ascii	"write_var_BitField_offset9_count14"
	.skip 1
	.ascii	"read_var_BitField_offset9_count14"
	.skip 1
	.ascii	"write_var_BitField_offset9_count13"
	.skip 1
	.ascii	"read_var_BitField_offset9_count13"
	.skip 1
	.ascii	"write_var_BitField_offset9_count12"
	.skip 1
	.ascii	"read_var_BitField_offset9_count12"
	.skip 1
	.ascii	"write_var_BitField_offset9_count11"
	.skip 1
	.ascii	"read_var_BitField_offset9_count11"
	.skip 1
	.ascii	"write_var_BitField_offset9_count10"
	.skip 1
	.ascii	"read_var_BitField_offset9_count10"
	.skip 1
	.ascii	"write_var_BitField_offset9_count9"
	.skip 1
	.ascii	"read_var_BitField_offset9_count9"
	.skip 1
	.ascii	"write_var_BitField_offset9_count8"
	.skip 1
	.ascii	"read_var_BitField_offset9_count8"
	.skip 1
	.ascii	"write_var_BitField_offset9_count7"
	.skip 1
	.ascii	"read_var_BitField_offset9_count7"
	.skip 1
	.ascii	"write_var_BitField_offset9_count6"
	.skip 1
	.ascii	"read_var_BitField_offset9_count6"
	.skip 1
	.ascii	"write_var_BitField_offset9_count5"
	.skip 1
	.ascii	"read_var_BitField_offset9_count5"
	.skip 1
	.ascii	"write_var_BitField_offset9_count4"
	.skip 1
	.ascii	"read_var_BitField_offset9_count4"
	.skip 1
	.ascii	"write_var_BitField_offset9_count3"
	.skip 1
	.ascii	"read_var_BitField_offset9_count3"
	.skip 1
	.ascii	"write_var_BitField_offset9_count2"
	.skip 1
	.ascii	"read_var_BitField_offset9_count2"
	.skip 1
	.ascii	"write_var_BitField_offset9_count1"
	.skip 1
	.ascii	"read_var_BitField_offset9_count1"
	.skip 1
	.ascii	"write_var_BitField_offset8_count24"
	.skip 1
	.ascii	"read_var_BitField_offset8_count24"
	.skip 1
	.ascii	"write_var_BitField_offset8_count23"
	.skip 1
	.ascii	"read_var_BitField_offset8_count23"
	.skip 1
	.ascii	"write_var_BitField_offset8_count22"
	.skip 1
	.ascii	"read_var_BitField_offset8_count22"
	.skip 1
	.ascii	"write_var_BitField_offset8_count21"
	.skip 1
	.ascii	"read_var_BitField_offset8_count21"
	.skip 1
	.ascii	"write_var_BitField_offset8_count20"
	.skip 1
	.ascii	"read_var_BitField_offset8_count20"
	.skip 1
	.ascii	"write_var_BitField_offset8_count19"
	.skip 1
	.ascii	"read_var_BitField_offset8_count19"
	.skip 1
	.ascii	"write_var_BitField_offset8_count18"
	.skip 1
	.ascii	"read_var_BitField_offset8_count18"
	.skip 1
	.ascii	"write_var_BitField_offset8_count17"
	.skip 1
	.ascii	"read_var_BitField_offset8_count17"
	.skip 1
	.ascii	"write_var_BitField_offset8_count16"
	.skip 1
	.ascii	"read_var_BitField_offset8_count16"
	.skip 1
	.ascii	"write_var_BitField_offset8_count15"
	.skip 1
	.ascii	"read_var_BitField_offset8_count15"
	.skip 1
	.ascii	"write_var_BitField_offset8_count14"
	.skip 1
	.ascii	"read_var_BitField_offset8_count14"
	.skip 1
	.ascii	"write_var_BitField_offset8_count13"
	.skip 1
	.ascii	"read_var_BitField_offset8_count13"
	.skip 1
	.ascii	"write_var_BitField_offset8_count12"
	.skip 1
	.ascii	"read_var_BitField_offset8_count12"
	.skip 1
	.ascii	"write_var_BitField_offset8_count11"
	.skip 1
	.ascii	"read_var_BitField_offset8_count11"
	.skip 1
	.ascii	"write_var_BitField_offset8_count10"
	.skip 1
	.ascii	"read_var_BitField_offset8_count10"
	.skip 1
	.ascii	"write_var_BitField_offset8_count9"
	.skip 1
	.ascii	"read_var_BitField_offset8_count9"
	.skip 1
	.ascii	"write_var_BitField_offset8_count8"
	.skip 1
	.ascii	"read_var_BitField_offset8_count8"
	.skip 1
	.ascii	"write_var_BitField_offset8_count7"
	.skip 1
	.ascii	"read_var_BitField_offset8_count7"
	.skip 1
	.ascii	"write_var_BitField_offset8_count6"
	.skip 1
	.ascii	"read_var_BitField_offset8_count6"
	.skip 1
	.ascii	"write_var_BitField_offset8_count5"
	.skip 1
	.ascii	"read_var_BitField_offset8_count5"
	.skip 1
	.ascii	"write_var_BitField_offset8_count4"
	.skip 1
	.ascii	"read_var_BitField_offset8_count4"
	.skip 1
	.ascii	"write_var_BitField_offset8_count3"
	.skip 1
	.ascii	"read_var_BitField_offset8_count3"
	.skip 1
	.ascii	"write_var_BitField_offset8_count2"
	.skip 1
	.ascii	"read_var_BitField_offset8_count2"
	.skip 1
	.ascii	"write_var_BitField_offset8_count1"
	.skip 1
	.ascii	"read_var_BitField_offset8_count1"
	.skip 1
	.ascii	"write_var_BitField_offset7_count25"
	.skip 1
	.ascii	"read_var_BitField_offset7_count25"
	.skip 1
	.ascii	"write_var_BitField_offset7_count24"
	.skip 1
	.ascii	"read_var_BitField_offset7_count24"
	.skip 1
	.ascii	"write_var_BitField_offset7_count23"
	.skip 1
	.ascii	"read_var_BitField_offset7_count23"
	.skip 1
	.ascii	"write_var_BitField_offset7_count22"
	.skip 1
	.ascii	"read_var_BitField_offset7_count22"
	.skip 1
	.ascii	"write_var_BitField_offset7_count21"
	.skip 1
	.ascii	"read_var_BitField_offset7_count21"
	.skip 1
	.ascii	"write_var_BitField_offset7_count20"
	.skip 1
	.ascii	"read_var_BitField_offset7_count20"
	.skip 1
	.ascii	"write_var_BitField_offset7_count19"
	.skip 1
	.ascii	"read_var_BitField_offset7_count19"
	.skip 1
	.ascii	"write_var_BitField_offset7_count18"
	.skip 1
	.ascii	"read_var_BitField_offset7_count18"
	.skip 1
	.ascii	"write_var_BitField_offset7_count17"
	.skip 1
	.ascii	"read_var_BitField_offset7_count17"
	.skip 1
	.ascii	"write_var_BitField_offset7_count16"
	.skip 1
	.ascii	"read_var_BitField_offset7_count16"
	.skip 1
	.ascii	"write_var_BitField_offset7_count15"
	.skip 1
	.ascii	"read_var_BitField_offset7_count15"
	.skip 1
	.ascii	"write_var_BitField_offset7_count14"
	.skip 1
	.ascii	"read_var_BitField_offset7_count14"
	.skip 1
	.ascii	"write_var_BitField_offset7_count13"
	.skip 1
	.ascii	"read_var_BitField_offset7_count13"
	.skip 1
	.ascii	"write_var_BitField_offset7_count12"
	.skip 1
	.ascii	"read_var_BitField_offset7_count12"
	.skip 1
	.ascii	"write_var_BitField_offset7_count11"
	.skip 1
	.ascii	"read_var_BitField_offset7_count11"
	.skip 1
	.ascii	"write_var_BitField_offset7_count10"
	.skip 1
	.ascii	"read_var_BitField_offset7_count10"
	.skip 1
	.ascii	"write_var_BitField_offset7_count9"
	.skip 1
	.ascii	"read_var_BitField_offset7_count9"
	.skip 1
	.ascii	"write_var_BitField_offset7_count8"
	.skip 1
	.ascii	"read_var_BitField_offset7_count8"
	.skip 1
	.ascii	"write_var_BitField_offset7_count7"
	.skip 1
	.ascii	"read_var_BitField_offset7_count7"
	.skip 1
	.ascii	"write_var_BitField_offset7_count6"
	.skip 1
	.ascii	"read_var_BitField_offset7_count6"
	.skip 1
	.ascii	"write_var_BitField_offset7_count5"
	.skip 1
	.ascii	"read_var_BitField_offset7_count5"
	.skip 1
	.ascii	"write_var_BitField_offset7_count4"
	.skip 1
	.ascii	"read_var_BitField_offset7_count4"
	.skip 1
	.ascii	"write_var_BitField_offset7_count3"
	.skip 1
	.ascii	"read_var_BitField_offset7_count3"
	.skip 1
	.ascii	"write_var_BitField_offset7_count2"
	.skip 1
	.ascii	"read_var_BitField_offset7_count2"
	.skip 1
	.ascii	"write_var_BitField_offset7_count1"
	.skip 1
	.ascii	"read_var_BitField_offset7_count1"
	.skip 1
	.ascii	"write_var_BitField_offset6_count26"
	.skip 1
	.ascii	"read_var_BitField_offset6_count26"
	.skip 1
	.ascii	"write_var_BitField_offset6_count25"
	.skip 1
	.ascii	"read_var_BitField_offset6_count25"
	.skip 1
	.ascii	"write_var_BitField_offset6_count24"
	.skip 1
	.ascii	"read_var_BitField_offset6_count24"
	.skip 1
	.ascii	"write_var_BitField_offset6_count23"
	.skip 1
	.ascii	"read_var_BitField_offset6_count23"
	.skip 1
	.ascii	"write_var_BitField_offset6_count22"
	.skip 1
	.ascii	"read_var_BitField_offset6_count22"
	.skip 1
	.ascii	"write_var_BitField_offset6_count21"
	.skip 1
	.ascii	"read_var_BitField_offset6_count21"
	.skip 1
	.ascii	"write_var_BitField_offset6_count20"
	.skip 1
	.ascii	"read_var_BitField_offset6_count20"
	.skip 1
	.ascii	"write_var_BitField_offset6_count19"
	.skip 1
	.ascii	"read_var_BitField_offset6_count19"
	.skip 1
	.ascii	"write_var_BitField_offset6_count18"
	.skip 1
	.ascii	"read_var_BitField_offset6_count18"
	.skip 1
	.ascii	"write_var_BitField_offset6_count17"
	.skip 1
	.ascii	"read_var_BitField_offset6_count17"
	.skip 1
	.ascii	"write_var_BitField_offset6_count16"
	.skip 1
	.ascii	"read_var_BitField_offset6_count16"
	.skip 1
	.ascii	"write_var_BitField_offset6_count15"
	.skip 1
	.ascii	"read_var_BitField_offset6_count15"
	.skip 1
	.ascii	"write_var_BitField_offset6_count14"
	.skip 1
	.ascii	"read_var_BitField_offset6_count14"
	.skip 1
	.ascii	"write_var_BitField_offset6_count13"
	.skip 1
	.ascii	"read_var_BitField_offset6_count13"
	.skip 1
	.ascii	"write_var_BitField_offset6_count12"
	.skip 1
	.ascii	"read_var_BitField_offset6_count12"
	.skip 1
	.ascii	"write_var_BitField_offset6_count11"
	.skip 1
	.ascii	"read_var_BitField_offset6_count11"
	.skip 1
	.ascii	"write_var_BitField_offset6_count10"
	.skip 1
	.ascii	"read_var_BitField_offset6_count10"
	.skip 1
	.ascii	"write_var_BitField_offset6_count9"
	.skip 1
	.ascii	"read_var_BitField_offset6_count9"
	.skip 1
	.ascii	"write_var_BitField_offset6_count8"
	.skip 1
	.ascii	"read_var_BitField_offset6_count8"
	.skip 1
	.ascii	"write_var_BitField_offset6_count7"
	.skip 1
	.ascii	"read_var_BitField_offset6_count7"
	.skip 1
	.ascii	"write_var_BitField_offset6_count6"
	.skip 1
	.ascii	"read_var_BitField_offset6_count6"
	.skip 1
	.ascii	"write_var_BitField_offset6_count5"
	.skip 1
	.ascii	"read_var_BitField_offset6_count5"
	.skip 1
	.ascii	"write_var_BitField_offset6_count4"
	.skip 1
	.ascii	"read_var_BitField_offset6_count4"
	.skip 1
	.ascii	"write_var_BitField_offset6_count3"
	.skip 1
	.ascii	"read_var_BitField_offset6_count3"
	.skip 1
	.ascii	"write_var_BitField_offset6_count2"
	.skip 1
	.ascii	"read_var_BitField_offset6_count2"
	.skip 1
	.ascii	"write_var_BitField_offset6_count1"
	.skip 1
	.ascii	"read_var_BitField_offset6_count1"
	.skip 1
	.ascii	"write_var_BitField_offset5_count27"
	.skip 1
	.ascii	"read_var_BitField_offset5_count27"
	.skip 1
	.ascii	"write_var_BitField_offset5_count26"
	.skip 1
	.ascii	"read_var_BitField_offset5_count26"
	.skip 1
	.ascii	"write_var_BitField_offset5_count25"
	.skip 1
	.ascii	"read_var_BitField_offset5_count25"
	.skip 1
	.ascii	"write_var_BitField_offset5_count24"
	.skip 1
	.ascii	"read_var_BitField_offset5_count24"
	.skip 1
	.ascii	"write_var_BitField_offset5_count23"
	.skip 1
	.ascii	"read_var_BitField_offset5_count23"
	.skip 1
	.ascii	"write_var_BitField_offset5_count22"
	.skip 1
	.ascii	"read_var_BitField_offset5_count22"
	.skip 1
	.ascii	"write_var_BitField_offset5_count21"
	.skip 1
	.ascii	"read_var_BitField_offset5_count21"
	.skip 1
	.ascii	"write_var_BitField_offset5_count20"
	.skip 1
	.ascii	"read_var_BitField_offset5_count20"
	.skip 1
	.ascii	"write_var_BitField_offset5_count19"
	.skip 1
	.ascii	"read_var_BitField_offset5_count19"
	.skip 1
	.ascii	"write_var_BitField_offset5_count18"
	.skip 1
	.ascii	"read_var_BitField_offset5_count18"
	.skip 1
	.ascii	"write_var_BitField_offset5_count17"
	.skip 1
	.ascii	"read_var_BitField_offset5_count17"
	.skip 1
	.ascii	"write_var_BitField_offset5_count16"
	.skip 1
	.ascii	"read_var_BitField_offset5_count16"
	.skip 1
	.ascii	"write_var_BitField_offset5_count15"
	.skip 1
	.ascii	"read_var_BitField_offset5_count15"
	.skip 1
	.ascii	"write_var_BitField_offset5_count14"
	.skip 1
	.ascii	"read_var_BitField_offset5_count14"
	.skip 1
	.ascii	"write_var_BitField_offset5_count13"
	.skip 1
	.ascii	"read_var_BitField_offset5_count13"
	.skip 1
	.ascii	"write_var_BitField_offset5_count12"
	.skip 1
	.ascii	"read_var_BitField_offset5_count12"
	.skip 1
	.ascii	"write_var_BitField_offset5_count11"
	.skip 1
	.ascii	"read_var_BitField_offset5_count11"
	.skip 1
	.ascii	"write_var_BitField_offset5_count10"
	.skip 1
	.ascii	"read_var_BitField_offset5_count10"
	.skip 1
	.ascii	"write_var_BitField_offset5_count9"
	.skip 1
	.ascii	"read_var_BitField_offset5_count9"
	.skip 1
	.ascii	"write_var_BitField_offset5_count8"
	.skip 1
	.ascii	"read_var_BitField_offset5_count8"
	.skip 1
	.ascii	"write_var_BitField_offset5_count7"
	.skip 1
	.ascii	"read_var_BitField_offset5_count7"
	.skip 1
	.ascii	"write_var_BitField_offset5_count6"
	.skip 1
	.ascii	"read_var_BitField_offset5_count6"
	.skip 1
	.ascii	"write_var_BitField_offset5_count5"
	.skip 1
	.ascii	"read_var_BitField_offset5_count5"
	.skip 1
	.ascii	"write_var_BitField_offset5_count4"
	.skip 1
	.ascii	"read_var_BitField_offset5_count4"
	.skip 1
	.ascii	"write_var_BitField_offset5_count3"
	.skip 1
	.ascii	"read_var_BitField_offset5_count3"
	.skip 1
	.ascii	"write_var_BitField_offset5_count2"
	.skip 1
	.ascii	"read_var_BitField_offset5_count2"
	.skip 1
	.ascii	"write_var_BitField_offset5_count1"
	.skip 1
	.ascii	"read_var_BitField_offset5_count1"
	.skip 1
	.ascii	"write_var_BitField_offset4_count28"
	.skip 1
	.ascii	"read_var_BitField_offset4_count28"
	.skip 1
	.ascii	"write_var_BitField_offset4_count27"
	.skip 1
	.ascii	"read_var_BitField_offset4_count27"
	.skip 1
	.ascii	"write_var_BitField_offset4_count26"
	.skip 1
	.ascii	"read_var_BitField_offset4_count26"
	.skip 1
	.ascii	"write_var_BitField_offset4_count25"
	.skip 1
	.ascii	"read_var_BitField_offset4_count25"
	.skip 1
	.ascii	"write_var_BitField_offset4_count24"
	.skip 1
	.ascii	"read_var_BitField_offset4_count24"
	.skip 1
	.ascii	"write_var_BitField_offset4_count23"
	.skip 1
	.ascii	"read_var_BitField_offset4_count23"
	.skip 1
	.ascii	"write_var_BitField_offset4_count22"
	.skip 1
	.ascii	"read_var_BitField_offset4_count22"
	.skip 1
	.ascii	"write_var_BitField_offset4_count21"
	.skip 1
	.ascii	"read_var_BitField_offset4_count21"
	.skip 1
	.ascii	"write_var_BitField_offset4_count20"
	.skip 1
	.ascii	"read_var_BitField_offset4_count20"
	.skip 1
	.ascii	"write_var_BitField_offset4_count19"
	.skip 1
	.ascii	"read_var_BitField_offset4_count19"
	.skip 1
	.ascii	"write_var_BitField_offset4_count18"
	.skip 1
	.ascii	"read_var_BitField_offset4_count18"
	.skip 1
	.ascii	"write_var_BitField_offset4_count17"
	.skip 1
	.ascii	"read_var_BitField_offset4_count17"
	.skip 1
	.ascii	"write_var_BitField_offset4_count16"
	.skip 1
	.ascii	"read_var_BitField_offset4_count16"
	.skip 1
	.ascii	"write_var_BitField_offset4_count15"
	.skip 1
	.ascii	"read_var_BitField_offset4_count15"
	.skip 1
	.ascii	"write_var_BitField_offset4_count14"
	.skip 1
	.ascii	"read_var_BitField_offset4_count14"
	.skip 1
	.ascii	"write_var_BitField_offset4_count13"
	.skip 1
	.ascii	"read_var_BitField_offset4_count13"
	.skip 1
	.ascii	"write_var_BitField_offset4_count12"
	.skip 1
	.ascii	"read_var_BitField_offset4_count12"
	.skip 1
	.ascii	"write_var_BitField_offset4_count11"
	.skip 1
	.ascii	"read_var_BitField_offset4_count11"
	.skip 1
	.ascii	"write_var_BitField_offset4_count10"
	.skip 1
	.ascii	"read_var_BitField_offset4_count10"
	.skip 1
	.ascii	"write_var_BitField_offset4_count9"
	.skip 1
	.ascii	"read_var_BitField_offset4_count9"
	.skip 1
	.ascii	"write_var_BitField_offset4_count8"
	.skip 1
	.ascii	"read_var_BitField_offset4_count8"
	.skip 1
	.ascii	"write_var_BitField_offset4_count7"
	.skip 1
	.ascii	"read_var_BitField_offset4_count7"
	.skip 1
	.ascii	"write_var_BitField_offset4_count6"
	.skip 1
	.ascii	"read_var_BitField_offset4_count6"
	.skip 1
	.ascii	"write_var_BitField_offset4_count5"
	.skip 1
	.ascii	"read_var_BitField_offset4_count5"
	.skip 1
	.ascii	"write_var_BitField_offset4_count4"
	.skip 1
	.ascii	"read_var_BitField_offset4_count4"
	.skip 1
	.ascii	"write_var_BitField_offset4_count3"
	.skip 1
	.ascii	"read_var_BitField_offset4_count3"
	.skip 1
	.ascii	"write_var_BitField_offset4_count2"
	.skip 1
	.ascii	"read_var_BitField_offset4_count2"
	.skip 1
	.ascii	"write_var_BitField_offset4_count1"
	.skip 1
	.ascii	"read_var_BitField_offset4_count1"
	.skip 1
	.ascii	"write_var_BitField_offset3_count29"
	.skip 1
	.ascii	"read_var_BitField_offset3_count29"
	.skip 1
	.ascii	"write_var_BitField_offset3_count28"
	.skip 1
	.ascii	"read_var_BitField_offset3_count28"
	.skip 1
	.ascii	"write_var_BitField_offset3_count27"
	.skip 1
	.ascii	"read_var_BitField_offset3_count27"
	.skip 1
	.ascii	"write_var_BitField_offset3_count26"
	.skip 1
	.ascii	"read_var_BitField_offset3_count26"
	.skip 1
	.ascii	"write_var_BitField_offset3_count25"
	.skip 1
	.ascii	"read_var_BitField_offset3_count25"
	.skip 1
	.ascii	"write_var_BitField_offset3_count24"
	.skip 1
	.ascii	"read_var_BitField_offset3_count24"
	.skip 1
	.ascii	"write_var_BitField_offset3_count23"
	.skip 1
	.ascii	"read_var_BitField_offset3_count23"
	.skip 1
	.ascii	"write_var_BitField_offset3_count22"
	.skip 1
	.ascii	"read_var_BitField_offset3_count22"
	.skip 1
	.ascii	"write_var_BitField_offset3_count21"
	.skip 1
	.ascii	"read_var_BitField_offset3_count21"
	.skip 1
	.ascii	"write_var_BitField_offset3_count20"
	.skip 1
	.ascii	"read_var_BitField_offset3_count20"
	.skip 1
	.ascii	"write_var_BitField_offset3_count19"
	.skip 1
	.ascii	"read_var_BitField_offset3_count19"
	.skip 1
	.ascii	"write_var_BitField_offset3_count18"
	.skip 1
	.ascii	"read_var_BitField_offset3_count18"
	.skip 1
	.ascii	"write_var_BitField_offset3_count17"
	.skip 1
	.ascii	"read_var_BitField_offset3_count17"
	.skip 1
	.ascii	"write_var_BitField_offset3_count16"
	.skip 1
	.ascii	"read_var_BitField_offset3_count16"
	.skip 1
	.ascii	"write_var_BitField_offset3_count15"
	.skip 1
	.ascii	"read_var_BitField_offset3_count15"
	.skip 1
	.ascii	"write_var_BitField_offset3_count14"
	.skip 1
	.ascii	"read_var_BitField_offset3_count14"
	.skip 1
	.ascii	"write_var_BitField_offset3_count13"
	.skip 1
	.ascii	"read_var_BitField_offset3_count13"
	.skip 1
	.ascii	"write_var_BitField_offset3_count12"
	.skip 1
	.ascii	"read_var_BitField_offset3_count12"
	.skip 1
	.ascii	"write_var_BitField_offset3_count11"
	.skip 1
	.ascii	"read_var_BitField_offset3_count11"
	.skip 1
	.ascii	"write_var_BitField_offset3_count10"
	.skip 1
	.ascii	"read_var_BitField_offset3_count10"
	.skip 1
	.ascii	"write_var_BitField_offset3_count9"
	.skip 1
	.ascii	"read_var_BitField_offset3_count9"
	.skip 1
	.ascii	"write_var_BitField_offset3_count8"
	.skip 1
	.ascii	"read_var_BitField_offset3_count8"
	.skip 1
	.ascii	"write_var_BitField_offset3_count7"
	.skip 1
	.ascii	"read_var_BitField_offset3_count7"
	.skip 1
	.ascii	"write_var_BitField_offset3_count6"
	.skip 1
	.ascii	"read_var_BitField_offset3_count6"
	.skip 1
	.ascii	"write_var_BitField_offset3_count5"
	.skip 1
	.ascii	"read_var_BitField_offset3_count5"
	.skip 1
	.ascii	"write_var_BitField_offset3_count4"
	.skip 1
	.ascii	"read_var_BitField_offset3_count4"
	.skip 1
	.ascii	"write_var_BitField_offset3_count3"
	.skip 1
	.ascii	"read_var_BitField_offset3_count3"
	.skip 1
	.ascii	"write_var_BitField_offset3_count2"
	.skip 1
	.ascii	"read_var_BitField_offset3_count2"
	.skip 1
	.ascii	"write_var_BitField_offset3_count1"
	.skip 1
	.ascii	"read_var_BitField_offset3_count1"
	.skip 1
	.ascii	"write_var_BitField_offset2_count30"
	.skip 1
	.ascii	"read_var_BitField_offset2_count30"
	.skip 1
	.ascii	"write_var_BitField_offset2_count29"
	.skip 1
	.ascii	"read_var_BitField_offset2_count29"
	.skip 1
	.ascii	"write_var_BitField_offset2_count28"
	.skip 1
	.ascii	"read_var_BitField_offset2_count28"
	.skip 1
	.ascii	"write_var_BitField_offset2_count27"
	.skip 1
	.ascii	"read_var_BitField_offset2_count27"
	.skip 1
	.ascii	"write_var_BitField_offset2_count26"
	.skip 1
	.ascii	"read_var_BitField_offset2_count26"
	.skip 1
	.ascii	"write_var_BitField_offset2_count25"
	.skip 1
	.ascii	"read_var_BitField_offset2_count25"
	.skip 1
	.ascii	"write_var_BitField_offset2_count24"
	.skip 1
	.ascii	"read_var_BitField_offset2_count24"
	.skip 1
	.ascii	"write_var_BitField_offset2_count23"
	.skip 1
	.ascii	"read_var_BitField_offset2_count23"
	.skip 1
	.ascii	"write_var_BitField_offset2_count22"
	.skip 1
	.ascii	"read_var_BitField_offset2_count22"
	.skip 1
	.ascii	"write_var_BitField_offset2_count21"
	.skip 1
	.ascii	"read_var_BitField_offset2_count21"
	.skip 1
	.ascii	"write_var_BitField_offset2_count20"
	.skip 1
	.ascii	"read_var_BitField_offset2_count20"
	.skip 1
	.ascii	"write_var_BitField_offset2_count19"
	.skip 1
	.ascii	"read_var_BitField_offset2_count19"
	.skip 1
	.ascii	"write_var_BitField_offset2_count18"
	.skip 1
	.ascii	"read_var_BitField_offset2_count18"
	.skip 1
	.ascii	"write_var_BitField_offset2_count17"
	.skip 1
	.ascii	"read_var_BitField_offset2_count17"
	.skip 1
	.ascii	"write_var_BitField_offset2_count16"
	.skip 1
	.ascii	"read_var_BitField_offset2_count16"
	.skip 1
	.ascii	"write_var_BitField_offset2_count15"
	.skip 1
	.ascii	"read_var_BitField_offset2_count15"
	.skip 1
	.ascii	"write_var_BitField_offset2_count14"
	.skip 1
	.ascii	"read_var_BitField_offset2_count14"
	.skip 1
	.ascii	"write_var_BitField_offset2_count13"
	.skip 1
	.ascii	"read_var_BitField_offset2_count13"
	.skip 1
	.ascii	"write_var_BitField_offset2_count12"
	.skip 1
	.ascii	"read_var_BitField_offset2_count12"
	.skip 1
	.ascii	"write_var_BitField_offset2_count11"
	.skip 1
	.ascii	"read_var_BitField_offset2_count11"
	.skip 1
	.ascii	"write_var_BitField_offset2_count10"
	.skip 1
	.ascii	"read_var_BitField_offset2_count10"
	.skip 1
	.ascii	"write_var_BitField_offset2_count9"
	.skip 1
	.ascii	"read_var_BitField_offset2_count9"
	.skip 1
	.ascii	"write_var_BitField_offset2_count8"
	.skip 1
	.ascii	"read_var_BitField_offset2_count8"
	.skip 1
	.ascii	"write_var_BitField_offset2_count7"
	.skip 1
	.ascii	"read_var_BitField_offset2_count7"
	.skip 1
	.ascii	"write_var_BitField_offset2_count6"
	.skip 1
	.ascii	"read_var_BitField_offset2_count6"
	.skip 1
	.ascii	"write_var_BitField_offset2_count5"
	.skip 1
	.ascii	"read_var_BitField_offset2_count5"
	.skip 1
	.ascii	"write_var_BitField_offset2_count4"
	.skip 1
	.ascii	"read_var_BitField_offset2_count4"
	.skip 1
	.ascii	"write_var_BitField_offset2_count3"
	.skip 1
	.ascii	"read_var_BitField_offset2_count3"
	.skip 1
	.ascii	"write_var_BitField_offset2_count2"
	.skip 1
	.ascii	"read_var_BitField_offset2_count2"
	.skip 1
	.ascii	"write_var_BitField_offset2_count1"
	.skip 1
	.ascii	"read_var_BitField_offset2_count1"
	.skip 1
	.ascii	"write_var_BitField_offset1_count31"
	.skip 1
	.ascii	"read_var_BitField_offset1_count31"
	.skip 1
	.ascii	"write_var_BitField_offset1_count30"
	.skip 1
	.ascii	"read_var_BitField_offset1_count30"
	.skip 1
	.ascii	"write_var_BitField_offset1_count29"
	.skip 1
	.ascii	"read_var_BitField_offset1_count29"
	.skip 1
	.ascii	"write_var_BitField_offset1_count28"
	.skip 1
	.ascii	"read_var_BitField_offset1_count28"
	.skip 1
	.ascii	"write_var_BitField_offset1_count27"
	.skip 1
	.ascii	"read_var_BitField_offset1_count27"
	.skip 1
	.ascii	"write_var_BitField_offset1_count26"
	.skip 1
	.ascii	"read_var_BitField_offset1_count26"
	.skip 1
	.ascii	"write_var_BitField_offset1_count25"
	.skip 1
	.ascii	"read_var_BitField_offset1_count25"
	.skip 1
	.ascii	"write_var_BitField_offset1_count24"
	.skip 1
	.ascii	"read_var_BitField_offset1_count24"
	.skip 1
	.ascii	"write_var_BitField_offset1_count23"
	.skip 1
	.ascii	"read_var_BitField_offset1_count23"
	.skip 1
	.ascii	"write_var_BitField_offset1_count22"
	.skip 1
	.ascii	"read_var_BitField_offset1_count22"
	.skip 1
	.ascii	"write_var_BitField_offset1_count21"
	.skip 1
	.ascii	"read_var_BitField_offset1_count21"
	.skip 1
	.ascii	"write_var_BitField_offset1_count20"
	.skip 1
	.ascii	"read_var_BitField_offset1_count20"
	.skip 1
	.ascii	"write_var_BitField_offset1_count19"
	.skip 1
	.ascii	"read_var_BitField_offset1_count19"
	.skip 1
	.ascii	"write_var_BitField_offset1_count18"
	.skip 1
	.ascii	"read_var_BitField_offset1_count18"
	.skip 1
	.ascii	"write_var_BitField_offset1_count17"
	.skip 1
	.ascii	"read_var_BitField_offset1_count17"
	.skip 1
	.ascii	"write_var_BitField_offset1_count16"
	.skip 1
	.ascii	"read_var_BitField_offset1_count16"
	.skip 1
	.ascii	"write_var_BitField_offset1_count15"
	.skip 1
	.ascii	"read_var_BitField_offset1_count15"
	.skip 1
	.ascii	"write_var_BitField_offset1_count14"
	.skip 1
	.ascii	"read_var_BitField_offset1_count14"
	.skip 1
	.ascii	"write_var_BitField_offset1_count13"
	.skip 1
	.ascii	"read_var_BitField_offset1_count13"
	.skip 1
	.ascii	"write_var_BitField_offset1_count12"
	.skip 1
	.ascii	"read_var_BitField_offset1_count12"
	.skip 1
	.ascii	"write_var_BitField_offset1_count11"
	.skip 1
	.ascii	"read_var_BitField_offset1_count11"
	.skip 1
	.ascii	"write_var_BitField_offset1_count10"
	.skip 1
	.ascii	"read_var_BitField_offset1_count10"
	.skip 1
	.ascii	"write_var_BitField_offset1_count9"
	.skip 1
	.ascii	"read_var_BitField_offset1_count9"
	.skip 1
	.ascii	"write_var_BitField_offset1_count8"
	.skip 1
	.ascii	"read_var_BitField_offset1_count8"
	.skip 1
	.ascii	"write_var_BitField_offset1_count7"
	.skip 1
	.ascii	"read_var_BitField_offset1_count7"
	.skip 1
	.ascii	"write_var_BitField_offset1_count6"
	.skip 1
	.ascii	"read_var_BitField_offset1_count6"
	.skip 1
	.ascii	"write_var_BitField_offset1_count5"
	.skip 1
	.ascii	"read_var_BitField_offset1_count5"
	.skip 1
	.ascii	"write_var_BitField_offset1_count4"
	.skip 1
	.ascii	"read_var_BitField_offset1_count4"
	.skip 1
	.ascii	"write_var_BitField_offset1_count3"
	.skip 1
	.ascii	"read_var_BitField_offset1_count3"
	.skip 1
	.ascii	"write_var_BitField_offset1_count2"
	.skip 1
	.ascii	"read_var_BitField_offset1_count2"
	.skip 1
	.ascii	"write_var_BitField_offset1_count1"
	.skip 1
	.ascii	"read_var_BitField_offset1_count1"
	.skip 1
	.long	bitfield_M3
	.long	L_1
	.long	bitfield__write_var_BitField_offset31_count1
	.long	L_1+12
	.long	bitfield__read_var_BitField_offset31_count1
	.long	L_1+47
	.long	bitfield__write_var_BitField_offset30_count2
	.long	L_1+81
	.long	bitfield__read_var_BitField_offset30_count2
	.long	L_1+116
	.long	bitfield__write_var_BitField_offset30_count1
	.long	L_1+150
	.long	bitfield__read_var_BitField_offset30_count1
	.long	L_1+185
	.long	bitfield__write_var_BitField_offset29_count3
	.long	L_1+219
	.long	bitfield__read_var_BitField_offset29_count3
	.long	L_1+254
	.long	bitfield__write_var_BitField_offset29_count2
	.long	L_1+288
	.long	bitfield__read_var_BitField_offset29_count2
	.long	L_1+323
	.long	bitfield__write_var_BitField_offset29_count1
	.long	L_1+357
	.long	bitfield__read_var_BitField_offset29_count1
	.long	L_1+392
	.long	bitfield__write_var_BitField_offset28_count4
	.long	L_1+426
	.long	bitfield__read_var_BitField_offset28_count4
	.long	L_1+461
	.long	bitfield__write_var_BitField_offset28_count3
	.long	L_1+495
	.long	bitfield__read_var_BitField_offset28_count3
	.long	L_1+530
	.long	bitfield__write_var_BitField_offset28_count2
	.long	L_1+564
	.long	bitfield__read_var_BitField_offset28_count2
	.long	L_1+599
	.long	bitfield__write_var_BitField_offset28_count1
	.long	L_1+633
	.long	bitfield__read_var_BitField_offset28_count1
	.long	L_1+668
	.long	bitfield__write_var_BitField_offset27_count5
	.long	L_1+702
	.long	bitfield__read_var_BitField_offset27_count5
	.long	L_1+737
	.long	bitfield__write_var_BitField_offset27_count4
	.long	L_1+771
	.long	bitfield__read_var_BitField_offset27_count4
	.long	L_1+806
	.long	bitfield__write_var_BitField_offset27_count3
	.long	L_1+840
	.long	bitfield__read_var_BitField_offset27_count3
	.long	L_1+875
	.long	bitfield__write_var_BitField_offset27_count2
	.long	L_1+909
	.long	bitfield__read_var_BitField_offset27_count2
	.long	L_1+944
	.long	bitfield__write_var_BitField_offset27_count1
	.long	L_1+978
	.long	bitfield__read_var_BitField_offset27_count1
	.long	L_1+1013
	.long	bitfield__write_var_BitField_offset26_count6
	.long	L_1+1047
	.long	bitfield__read_var_BitField_offset26_count6
	.long	L_1+1082
	.long	bitfield__write_var_BitField_offset26_count5
	.long	L_1+1116
	.long	bitfield__read_var_BitField_offset26_count5
	.long	L_1+1151
	.long	bitfield__write_var_BitField_offset26_count4
	.long	L_1+1185
	.long	bitfield__read_var_BitField_offset26_count4
	.long	L_1+1220
	.long	bitfield__write_var_BitField_offset26_count3
	.long	L_1+1254
	.long	bitfield__read_var_BitField_offset26_count3
	.long	L_1+1289
	.long	bitfield__write_var_BitField_offset26_count2
	.long	L_1+1323
	.long	bitfield__read_var_BitField_offset26_count2
	.long	L_1+1358
	.long	bitfield__write_var_BitField_offset26_count1
	.long	L_1+1392
	.long	bitfield__read_var_BitField_offset26_count1
	.long	L_1+1427
	.long	bitfield__write_var_BitField_offset25_count7
	.long	L_1+1461
	.long	bitfield__read_var_BitField_offset25_count7
	.long	L_1+1496
	.long	bitfield__write_var_BitField_offset25_count6
	.long	L_1+1530
	.long	bitfield__read_var_BitField_offset25_count6
	.long	L_1+1565
	.long	bitfield__write_var_BitField_offset25_count5
	.long	L_1+1599
	.long	bitfield__read_var_BitField_offset25_count5
	.long	L_1+1634
	.long	bitfield__write_var_BitField_offset25_count4
	.long	L_1+1668
	.long	bitfield__read_var_BitField_offset25_count4
	.long	L_1+1703
	.long	bitfield__write_var_BitField_offset25_count3
	.long	L_1+1737
	.long	bitfield__read_var_BitField_offset25_count3
	.long	L_1+1772
	.long	bitfield__write_var_BitField_offset25_count2
	.long	L_1+1806
	.long	bitfield__read_var_BitField_offset25_count2
	.long	L_1+1841
	.long	bitfield__write_var_BitField_offset25_count1
	.long	L_1+1875
	.long	bitfield__read_var_BitField_offset25_count1
	.long	L_1+1910
	.long	bitfield__write_var_BitField_offset24_count8
	.long	L_1+1944
	.long	bitfield__read_var_BitField_offset24_count8
	.long	L_1+1979
	.long	bitfield__write_var_BitField_offset24_count7
	.long	L_1+2013
	.long	bitfield__read_var_BitField_offset24_count7
	.long	L_1+2048
	.long	bitfield__write_var_BitField_offset24_count6
	.long	L_1+2082
	.long	bitfield__read_var_BitField_offset24_count6
	.long	L_1+2117
	.long	bitfield__write_var_BitField_offset24_count5
	.long	L_1+2151
	.long	bitfield__read_var_BitField_offset24_count5
	.long	L_1+2186
	.long	bitfield__write_var_BitField_offset24_count4
	.long	L_1+2220
	.long	bitfield__read_var_BitField_offset24_count4
	.long	L_1+2255
	.long	bitfield__write_var_BitField_offset24_count3
	.long	L_1+2289
	.long	bitfield__read_var_BitField_offset24_count3
	.long	L_1+2324
	.long	bitfield__write_var_BitField_offset24_count2
	.long	L_1+2358
	.long	bitfield__read_var_BitField_offset24_count2
	.long	L_1+2393
	.long	bitfield__write_var_BitField_offset24_count1
	.long	L_1+2427
	.long	bitfield__read_var_BitField_offset24_count1
	.long	L_1+2462
	.long	bitfield__write_var_BitField_offset23_count9
	.long	L_1+2496
	.long	bitfield__read_var_BitField_offset23_count9
	.long	L_1+2531
	.long	bitfield__write_var_BitField_offset23_count8
	.long	L_1+2565
	.long	bitfield__read_var_BitField_offset23_count8
	.long	L_1+2600
	.long	bitfield__write_var_BitField_offset23_count7
	.long	L_1+2634
	.long	bitfield__read_var_BitField_offset23_count7
	.long	L_1+2669
	.long	bitfield__write_var_BitField_offset23_count6
	.long	L_1+2703
	.long	bitfield__read_var_BitField_offset23_count6
	.long	L_1+2738
	.long	bitfield__write_var_BitField_offset23_count5
	.long	L_1+2772
	.long	bitfield__read_var_BitField_offset23_count5
	.long	L_1+2807
	.long	bitfield__write_var_BitField_offset23_count4
	.long	L_1+2841
	.long	bitfield__read_var_BitField_offset23_count4
	.long	L_1+2876
	.long	bitfield__write_var_BitField_offset23_count3
	.long	L_1+2910
	.long	bitfield__read_var_BitField_offset23_count3
	.long	L_1+2945
	.long	bitfield__write_var_BitField_offset23_count2
	.long	L_1+2979
	.long	bitfield__read_var_BitField_offset23_count2
	.long	L_1+3014
	.long	bitfield__write_var_BitField_offset23_count1
	.long	L_1+3048
	.long	bitfield__read_var_BitField_offset23_count1
	.long	L_1+3083
	.long	bitfield__write_var_BitField_offset22_count10
	.long	L_1+3117
	.long	bitfield__read_var_BitField_offset22_count10
	.long	L_1+3153
	.long	bitfield__write_var_BitField_offset22_count9
	.long	L_1+3188
	.long	bitfield__read_var_BitField_offset22_count9
	.long	L_1+3223
	.long	bitfield__write_var_BitField_offset22_count8
	.long	L_1+3257
	.long	bitfield__read_var_BitField_offset22_count8
	.long	L_1+3292
	.long	bitfield__write_var_BitField_offset22_count7
	.long	L_1+3326
	.long	bitfield__read_var_BitField_offset22_count7
	.long	L_1+3361
	.long	bitfield__write_var_BitField_offset22_count6
	.long	L_1+3395
	.long	bitfield__read_var_BitField_offset22_count6
	.long	L_1+3430
	.long	bitfield__write_var_BitField_offset22_count5
	.long	L_1+3464
	.long	bitfield__read_var_BitField_offset22_count5
	.long	L_1+3499
	.long	bitfield__write_var_BitField_offset22_count4
	.long	L_1+3533
	.long	bitfield__read_var_BitField_offset22_count4
	.long	L_1+3568
	.long	bitfield__write_var_BitField_offset22_count3
	.long	L_1+3602
	.long	bitfield__read_var_BitField_offset22_count3
	.long	L_1+3637
	.long	bitfield__write_var_BitField_offset22_count2
	.long	L_1+3671
	.long	bitfield__read_var_BitField_offset22_count2
	.long	L_1+3706
	.long	bitfield__write_var_BitField_offset22_count1
	.long	L_1+3740
	.long	bitfield__read_var_BitField_offset22_count1
	.long	L_1+3775
	.long	bitfield__write_var_BitField_offset21_count11
	.long	L_1+3809
	.long	bitfield__read_var_BitField_offset21_count11
	.long	L_1+3845
	.long	bitfield__write_var_BitField_offset21_count10
	.long	L_1+3880
	.long	bitfield__read_var_BitField_offset21_count10
	.long	L_1+3916
	.long	bitfield__write_var_BitField_offset21_count9
	.long	L_1+3951
	.long	bitfield__read_var_BitField_offset21_count9
	.long	L_1+3986
	.long	bitfield__write_var_BitField_offset21_count8
	.long	L_1+4020
	.long	bitfield__read_var_BitField_offset21_count8
	.long	L_1+4055
	.long	bitfield__write_var_BitField_offset21_count7
	.long	L_1+4089
	.long	bitfield__read_var_BitField_offset21_count7
	.long	L_1+4124
	.long	bitfield__write_var_BitField_offset21_count6
	.long	L_1+4158
	.long	bitfield__read_var_BitField_offset21_count6
	.long	L_1+4193
	.long	bitfield__write_var_BitField_offset21_count5
	.long	L_1+4227
	.long	bitfield__read_var_BitField_offset21_count5
	.long	L_1+4262
	.long	bitfield__write_var_BitField_offset21_count4
	.long	L_1+4296
	.long	bitfield__read_var_BitField_offset21_count4
	.long	L_1+4331
	.long	bitfield__write_var_BitField_offset21_count3
	.long	L_1+4365
	.long	bitfield__read_var_BitField_offset21_count3
	.long	L_1+4400
	.long	bitfield__write_var_BitField_offset21_count2
	.long	L_1+4434
	.long	bitfield__read_var_BitField_offset21_count2
	.long	L_1+4469
	.long	bitfield__write_var_BitField_offset21_count1
	.long	L_1+4503
	.long	bitfield__read_var_BitField_offset21_count1
	.long	L_1+4538
	.long	bitfield__write_var_BitField_offset20_count12
	.long	L_1+4572
	.long	bitfield__read_var_BitField_offset20_count12
	.long	L_1+4608
	.long	bitfield__write_var_BitField_offset20_count11
	.long	L_1+4643
	.long	bitfield__read_var_BitField_offset20_count11
	.long	L_1+4679
	.long	bitfield__write_var_BitField_offset20_count10
	.long	L_1+4714
	.long	bitfield__read_var_BitField_offset20_count10
	.long	L_1+4750
	.long	bitfield__write_var_BitField_offset20_count9
	.long	L_1+4785
	.long	bitfield__read_var_BitField_offset20_count9
	.long	L_1+4820
	.long	bitfield__write_var_BitField_offset20_count8
	.long	L_1+4854
	.long	bitfield__read_var_BitField_offset20_count8
	.long	L_1+4889
	.long	bitfield__write_var_BitField_offset20_count7
	.long	L_1+4923
	.long	bitfield__read_var_BitField_offset20_count7
	.long	L_1+4958
	.long	bitfield__write_var_BitField_offset20_count6
	.long	L_1+4992
	.long	bitfield__read_var_BitField_offset20_count6
	.long	L_1+5027
	.long	bitfield__write_var_BitField_offset20_count5
	.long	L_1+5061
	.long	bitfield__read_var_BitField_offset20_count5
	.long	L_1+5096
	.long	bitfield__write_var_BitField_offset20_count4
	.long	L_1+5130
	.long	bitfield__read_var_BitField_offset20_count4
	.long	L_1+5165
	.long	bitfield__write_var_BitField_offset20_count3
	.long	L_1+5199
	.long	bitfield__read_var_BitField_offset20_count3
	.long	L_1+5234
	.long	bitfield__write_var_BitField_offset20_count2
	.long	L_1+5268
	.long	bitfield__read_var_BitField_offset20_count2
	.long	L_1+5303
	.long	bitfield__write_var_BitField_offset20_count1
	.long	L_1+5337
	.long	bitfield__read_var_BitField_offset20_count1
	.long	L_1+5372
	.long	bitfield__write_var_BitField_offset19_count13
	.long	L_1+5406
	.long	bitfield__read_var_BitField_offset19_count13
	.long	L_1+5442
	.long	bitfield__write_var_BitField_offset19_count12
	.long	L_1+5477
	.long	bitfield__read_var_BitField_offset19_count12
	.long	L_1+5513
	.long	bitfield__write_var_BitField_offset19_count11
	.long	L_1+5548
	.long	bitfield__read_var_BitField_offset19_count11
	.long	L_1+5584
	.long	bitfield__write_var_BitField_offset19_count10
	.long	L_1+5619
	.long	bitfield__read_var_BitField_offset19_count10
	.long	L_1+5655
	.long	bitfield__write_var_BitField_offset19_count9
	.long	L_1+5690
	.long	bitfield__read_var_BitField_offset19_count9
	.long	L_1+5725
	.long	bitfield__write_var_BitField_offset19_count8
	.long	L_1+5759
	.long	bitfield__read_var_BitField_offset19_count8
	.long	L_1+5794
	.long	bitfield__write_var_BitField_offset19_count7
	.long	L_1+5828
	.long	bitfield__read_var_BitField_offset19_count7
	.long	L_1+5863
	.long	bitfield__write_var_BitField_offset19_count6
	.long	L_1+5897
	.long	bitfield__read_var_BitField_offset19_count6
	.long	L_1+5932
	.long	bitfield__write_var_BitField_offset19_count5
	.long	L_1+5966
	.long	bitfield__read_var_BitField_offset19_count5
	.long	L_1+6001
	.long	bitfield__write_var_BitField_offset19_count4
	.long	L_1+6035
	.long	bitfield__read_var_BitField_offset19_count4
	.long	L_1+6070
	.long	bitfield__write_var_BitField_offset19_count3
	.long	L_1+6104
	.long	bitfield__read_var_BitField_offset19_count3
	.long	L_1+6139
	.long	bitfield__write_var_BitField_offset19_count2
	.long	L_1+6173
	.long	bitfield__read_var_BitField_offset19_count2
	.long	L_1+6208
	.long	bitfield__write_var_BitField_offset19_count1
	.long	L_1+6242
	.long	bitfield__read_var_BitField_offset19_count1
	.long	L_1+6277
	.long	bitfield__write_var_BitField_offset18_count14
	.long	L_1+6311
	.long	bitfield__read_var_BitField_offset18_count14
	.long	L_1+6347
	.long	bitfield__write_var_BitField_offset18_count13
	.long	L_1+6382
	.long	bitfield__read_var_BitField_offset18_count13
	.long	L_1+6418
	.long	bitfield__write_var_BitField_offset18_count12
	.long	L_1+6453
	.long	bitfield__read_var_BitField_offset18_count12
	.long	L_1+6489
	.long	bitfield__write_var_BitField_offset18_count11
	.long	L_1+6524
	.long	bitfield__read_var_BitField_offset18_count11
	.long	L_1+6560
	.long	bitfield__write_var_BitField_offset18_count10
	.long	L_1+6595
	.long	bitfield__read_var_BitField_offset18_count10
	.long	L_1+6631
	.long	bitfield__write_var_BitField_offset18_count9
	.long	L_1+6666
	.long	bitfield__read_var_BitField_offset18_count9
	.long	L_1+6701
	.long	bitfield__write_var_BitField_offset18_count8
	.long	L_1+6735
	.long	bitfield__read_var_BitField_offset18_count8
	.long	L_1+6770
	.long	bitfield__write_var_BitField_offset18_count7
	.long	L_1+6804
	.long	bitfield__read_var_BitField_offset18_count7
	.long	L_1+6839
	.long	bitfield__write_var_BitField_offset18_count6
	.long	L_1+6873
	.long	bitfield__read_var_BitField_offset18_count6
	.long	L_1+6908
	.long	bitfield__write_var_BitField_offset18_count5
	.long	L_1+6942
	.long	bitfield__read_var_BitField_offset18_count5
	.long	L_1+6977
	.long	bitfield__write_var_BitField_offset18_count4
	.long	L_1+7011
	.long	bitfield__read_var_BitField_offset18_count4
	.long	L_1+7046
	.long	bitfield__write_var_BitField_offset18_count3
	.long	L_1+7080
	.long	bitfield__read_var_BitField_offset18_count3
	.long	L_1+7115
	.long	bitfield__write_var_BitField_offset18_count2
	.long	L_1+7149
	.long	bitfield__read_var_BitField_offset18_count2
	.long	L_1+7184
	.long	bitfield__write_var_BitField_offset18_count1
	.long	L_1+7218
	.long	bitfield__read_var_BitField_offset18_count1
	.long	L_1+7253
	.long	bitfield__write_var_BitField_offset17_count15
	.long	L_1+7287
	.long	bitfield__read_var_BitField_offset17_count15
	.long	L_1+7323
	.long	bitfield__write_var_BitField_offset17_count14
	.long	L_1+7358
	.long	bitfield__read_var_BitField_offset17_count14
	.long	L_1+7394
	.long	bitfield__write_var_BitField_offset17_count13
	.long	L_1+7429
	.long	bitfield__read_var_BitField_offset17_count13
	.long	L_1+7465
	.long	bitfield__write_var_BitField_offset17_count12
	.long	L_1+7500
	.long	bitfield__read_var_BitField_offset17_count12
	.long	L_1+7536
	.long	bitfield__write_var_BitField_offset17_count11
	.long	L_1+7571
	.long	bitfield__read_var_BitField_offset17_count11
	.long	L_1+7607
	.long	bitfield__write_var_BitField_offset17_count10
	.long	L_1+7642
	.long	bitfield__read_var_BitField_offset17_count10
	.long	L_1+7678
	.long	bitfield__write_var_BitField_offset17_count9
	.long	L_1+7713
	.long	bitfield__read_var_BitField_offset17_count9
	.long	L_1+7748
	.long	bitfield__write_var_BitField_offset17_count8
	.long	L_1+7782
	.long	bitfield__read_var_BitField_offset17_count8
	.long	L_1+7817
	.long	bitfield__write_var_BitField_offset17_count7
	.long	L_1+7851
	.long	bitfield__read_var_BitField_offset17_count7
	.long	L_1+7886
	.long	bitfield__write_var_BitField_offset17_count6
	.long	L_1+7920
	.long	bitfield__read_var_BitField_offset17_count6
	.long	L_1+7955
	.long	bitfield__write_var_BitField_offset17_count5
	.long	L_1+7989
	.long	bitfield__read_var_BitField_offset17_count5
	.long	L_1+8024
	.long	bitfield__write_var_BitField_offset17_count4
	.long	L_1+8058
	.long	bitfield__read_var_BitField_offset17_count4
	.long	L_1+8093
	.long	bitfield__write_var_BitField_offset17_count3
	.long	L_1+8127
	.long	bitfield__read_var_BitField_offset17_count3
	.long	L_1+8162
	.long	bitfield__write_var_BitField_offset17_count2
	.long	L_1+8196
	.long	bitfield__read_var_BitField_offset17_count2
	.long	L_1+8231
	.long	bitfield__write_var_BitField_offset17_count1
	.long	L_1+8265
	.long	bitfield__read_var_BitField_offset17_count1
	.long	L_1+8300
	.long	bitfield__write_var_BitField_offset16_count16
	.long	L_1+8334
	.long	bitfield__read_var_BitField_offset16_count16
	.long	L_1+8370
	.long	bitfield__write_var_BitField_offset16_count15
	.long	L_1+8405
	.long	bitfield__read_var_BitField_offset16_count15
	.long	L_1+8441
	.long	bitfield__write_var_BitField_offset16_count14
	.long	L_1+8476
	.long	bitfield__read_var_BitField_offset16_count14
	.long	L_1+8512
	.long	bitfield__write_var_BitField_offset16_count13
	.long	L_1+8547
	.long	bitfield__read_var_BitField_offset16_count13
	.long	L_1+8583
	.long	bitfield__write_var_BitField_offset16_count12
	.long	L_1+8618
	.long	bitfield__read_var_BitField_offset16_count12
	.long	L_1+8654
	.long	bitfield__write_var_BitField_offset16_count11
	.long	L_1+8689
	.long	bitfield__read_var_BitField_offset16_count11
	.long	L_1+8725
	.long	bitfield__write_var_BitField_offset16_count10
	.long	L_1+8760
	.long	bitfield__read_var_BitField_offset16_count10
	.long	L_1+8796
	.long	bitfield__write_var_BitField_offset16_count9
	.long	L_1+8831
	.long	bitfield__read_var_BitField_offset16_count9
	.long	L_1+8866
	.long	bitfield__write_var_BitField_offset16_count8
	.long	L_1+8900
	.long	bitfield__read_var_BitField_offset16_count8
	.long	L_1+8935
	.long	bitfield__write_var_BitField_offset16_count7
	.long	L_1+8969
	.long	bitfield__read_var_BitField_offset16_count7
	.long	L_1+9004
	.long	bitfield__write_var_BitField_offset16_count6
	.long	L_1+9038
	.long	bitfield__read_var_BitField_offset16_count6
	.long	L_1+9073
	.long	bitfield__write_var_BitField_offset16_count5
	.long	L_1+9107
	.long	bitfield__read_var_BitField_offset16_count5
	.long	L_1+9142
	.long	bitfield__write_var_BitField_offset16_count4
	.long	L_1+9176
	.long	bitfield__read_var_BitField_offset16_count4
	.long	L_1+9211
	.long	bitfield__write_var_BitField_offset16_count3
	.long	L_1+9245
	.long	bitfield__read_var_BitField_offset16_count3
	.long	L_1+9280
	.long	bitfield__write_var_BitField_offset16_count2
	.long	L_1+9314
	.long	bitfield__read_var_BitField_offset16_count2
	.long	L_1+9349
	.long	bitfield__write_var_BitField_offset16_count1
	.long	L_1+9383
	.long	bitfield__read_var_BitField_offset16_count1
	.long	L_1+9418
	.long	bitfield__write_var_BitField_offset15_count17
	.long	L_1+9452
	.long	bitfield__read_var_BitField_offset15_count17
	.long	L_1+9488
	.long	bitfield__write_var_BitField_offset15_count16
	.long	L_1+9523
	.long	bitfield__read_var_BitField_offset15_count16
	.long	L_1+9559
	.long	bitfield__write_var_BitField_offset15_count15
	.long	L_1+9594
	.long	bitfield__read_var_BitField_offset15_count15
	.long	L_1+9630
	.long	bitfield__write_var_BitField_offset15_count14
	.long	L_1+9665
	.long	bitfield__read_var_BitField_offset15_count14
	.long	L_1+9701
	.long	bitfield__write_var_BitField_offset15_count13
	.long	L_1+9736
	.long	bitfield__read_var_BitField_offset15_count13
	.long	L_1+9772
	.long	bitfield__write_var_BitField_offset15_count12
	.long	L_1+9807
	.long	bitfield__read_var_BitField_offset15_count12
	.long	L_1+9843
	.long	bitfield__write_var_BitField_offset15_count11
	.long	L_1+9878
	.long	bitfield__read_var_BitField_offset15_count11
	.long	L_1+9914
	.long	bitfield__write_var_BitField_offset15_count10
	.long	L_1+9949
	.long	bitfield__read_var_BitField_offset15_count10
	.long	L_1+9985
	.long	bitfield__write_var_BitField_offset15_count9
	.long	L_1+10020
	.long	bitfield__read_var_BitField_offset15_count9
	.long	L_1+10055
	.long	bitfield__write_var_BitField_offset15_count8
	.long	L_1+10089
	.long	bitfield__read_var_BitField_offset15_count8
	.long	L_1+10124
	.long	bitfield__write_var_BitField_offset15_count7
	.long	L_1+10158
	.long	bitfield__read_var_BitField_offset15_count7
	.long	L_1+10193
	.long	bitfield__write_var_BitField_offset15_count6
	.long	L_1+10227
	.long	bitfield__read_var_BitField_offset15_count6
	.long	L_1+10262
	.long	bitfield__write_var_BitField_offset15_count5
	.long	L_1+10296
	.long	bitfield__read_var_BitField_offset15_count5
	.long	L_1+10331
	.long	bitfield__write_var_BitField_offset15_count4
	.long	L_1+10365
	.long	bitfield__read_var_BitField_offset15_count4
	.long	L_1+10400
	.long	bitfield__write_var_BitField_offset15_count3
	.long	L_1+10434
	.long	bitfield__read_var_BitField_offset15_count3
	.long	L_1+10469
	.long	bitfield__write_var_BitField_offset15_count2
	.long	L_1+10503
	.long	bitfield__read_var_BitField_offset15_count2
	.long	L_1+10538
	.long	bitfield__write_var_BitField_offset15_count1
	.long	L_1+10572
	.long	bitfield__read_var_BitField_offset15_count1
	.long	L_1+10607
	.long	bitfield__write_var_BitField_offset14_count18
	.long	L_1+10641
	.long	bitfield__read_var_BitField_offset14_count18
	.long	L_1+10677
	.long	bitfield__write_var_BitField_offset14_count17
	.long	L_1+10712
	.long	bitfield__read_var_BitField_offset14_count17
	.long	L_1+10748
	.long	bitfield__write_var_BitField_offset14_count16
	.long	L_1+10783
	.long	bitfield__read_var_BitField_offset14_count16
	.long	L_1+10819
	.long	bitfield__write_var_BitField_offset14_count15
	.long	L_1+10854
	.long	bitfield__read_var_BitField_offset14_count15
	.long	L_1+10890
	.long	bitfield__write_var_BitField_offset14_count14
	.long	L_1+10925
	.long	bitfield__read_var_BitField_offset14_count14
	.long	L_1+10961
	.long	bitfield__write_var_BitField_offset14_count13
	.long	L_1+10996
	.long	bitfield__read_var_BitField_offset14_count13
	.long	L_1+11032
	.long	bitfield__write_var_BitField_offset14_count12
	.long	L_1+11067
	.long	bitfield__read_var_BitField_offset14_count12
	.long	L_1+11103
	.long	bitfield__write_var_BitField_offset14_count11
	.long	L_1+11138
	.long	bitfield__read_var_BitField_offset14_count11
	.long	L_1+11174
	.long	bitfield__write_var_BitField_offset14_count10
	.long	L_1+11209
	.long	bitfield__read_var_BitField_offset14_count10
	.long	L_1+11245
	.long	bitfield__write_var_BitField_offset14_count9
	.long	L_1+11280
	.long	bitfield__read_var_BitField_offset14_count9
	.long	L_1+11315
	.long	bitfield__write_var_BitField_offset14_count8
	.long	L_1+11349
	.long	bitfield__read_var_BitField_offset14_count8
	.long	L_1+11384
	.long	bitfield__write_var_BitField_offset14_count7
	.long	L_1+11418
	.long	bitfield__read_var_BitField_offset14_count7
	.long	L_1+11453
	.long	bitfield__write_var_BitField_offset14_count6
	.long	L_1+11487
	.long	bitfield__read_var_BitField_offset14_count6
	.long	L_1+11522
	.long	bitfield__write_var_BitField_offset14_count5
	.long	L_1+11556
	.long	bitfield__read_var_BitField_offset14_count5
	.long	L_1+11591
	.long	bitfield__write_var_BitField_offset14_count4
	.long	L_1+11625
	.long	bitfield__read_var_BitField_offset14_count4
	.long	L_1+11660
	.long	bitfield__write_var_BitField_offset14_count3
	.long	L_1+11694
	.long	bitfield__read_var_BitField_offset14_count3
	.long	L_1+11729
	.long	bitfield__write_var_BitField_offset14_count2
	.long	L_1+11763
	.long	bitfield__read_var_BitField_offset14_count2
	.long	L_1+11798
	.long	bitfield__write_var_BitField_offset14_count1
	.long	L_1+11832
	.long	bitfield__read_var_BitField_offset14_count1
	.long	L_1+11867
	.long	bitfield__write_var_BitField_offset13_count19
	.long	L_1+11901
	.long	bitfield__read_var_BitField_offset13_count19
	.long	L_1+11937
	.long	bitfield__write_var_BitField_offset13_count18
	.long	L_1+11972
	.long	bitfield__read_var_BitField_offset13_count18
	.long	L_1+12008
	.long	bitfield__write_var_BitField_offset13_count17
	.long	L_1+12043
	.long	bitfield__read_var_BitField_offset13_count17
	.long	L_1+12079
	.long	bitfield__write_var_BitField_offset13_count16
	.long	L_1+12114
	.long	bitfield__read_var_BitField_offset13_count16
	.long	L_1+12150
	.long	bitfield__write_var_BitField_offset13_count15
	.long	L_1+12185
	.long	bitfield__read_var_BitField_offset13_count15
	.long	L_1+12221
	.long	bitfield__write_var_BitField_offset13_count14
	.long	L_1+12256
	.long	bitfield__read_var_BitField_offset13_count14
	.long	L_1+12292
	.long	bitfield__write_var_BitField_offset13_count13
	.long	L_1+12327
	.long	bitfield__read_var_BitField_offset13_count13
	.long	L_1+12363
	.long	bitfield__write_var_BitField_offset13_count12
	.long	L_1+12398
	.long	bitfield__read_var_BitField_offset13_count12
	.long	L_1+12434
	.long	bitfield__write_var_BitField_offset13_count11
	.long	L_1+12469
	.long	bitfield__read_var_BitField_offset13_count11
	.long	L_1+12505
	.long	bitfield__write_var_BitField_offset13_count10
	.long	L_1+12540
	.long	bitfield__read_var_BitField_offset13_count10
	.long	L_1+12576
	.long	bitfield__write_var_BitField_offset13_count9
	.long	L_1+12611
	.long	bitfield__read_var_BitField_offset13_count9
	.long	L_1+12646
	.long	bitfield__write_var_BitField_offset13_count8
	.long	L_1+12680
	.long	bitfield__read_var_BitField_offset13_count8
	.long	L_1+12715
	.long	bitfield__write_var_BitField_offset13_count7
	.long	L_1+12749
	.long	bitfield__read_var_BitField_offset13_count7
	.long	L_1+12784
	.long	bitfield__write_var_BitField_offset13_count6
	.long	L_1+12818
	.long	bitfield__read_var_BitField_offset13_count6
	.long	L_1+12853
	.long	bitfield__write_var_BitField_offset13_count5
	.long	L_1+12887
	.long	bitfield__read_var_BitField_offset13_count5
	.long	L_1+12922
	.long	bitfield__write_var_BitField_offset13_count4
	.long	L_1+12956
	.long	bitfield__read_var_BitField_offset13_count4
	.long	L_1+12991
	.long	bitfield__write_var_BitField_offset13_count3
	.long	L_1+13025
	.long	bitfield__read_var_BitField_offset13_count3
	.long	L_1+13060
	.long	bitfield__write_var_BitField_offset13_count2
	.long	L_1+13094
	.long	bitfield__read_var_BitField_offset13_count2
	.long	L_1+13129
	.long	bitfield__write_var_BitField_offset13_count1
	.long	L_1+13163
	.long	bitfield__read_var_BitField_offset13_count1
	.long	L_1+13198
	.long	bitfield__write_var_BitField_offset12_count20
	.long	L_1+13232
	.long	bitfield__read_var_BitField_offset12_count20
	.long	L_1+13268
	.long	bitfield__write_var_BitField_offset12_count19
	.long	L_1+13303
	.long	bitfield__read_var_BitField_offset12_count19
	.long	L_1+13339
	.long	bitfield__write_var_BitField_offset12_count18
	.long	L_1+13374
	.long	bitfield__read_var_BitField_offset12_count18
	.long	L_1+13410
	.long	bitfield__write_var_BitField_offset12_count17
	.long	L_1+13445
	.long	bitfield__read_var_BitField_offset12_count17
	.long	L_1+13481
	.long	bitfield__write_var_BitField_offset12_count16
	.long	L_1+13516
	.long	bitfield__read_var_BitField_offset12_count16
	.long	L_1+13552
	.long	bitfield__write_var_BitField_offset12_count15
	.long	L_1+13587
	.long	bitfield__read_var_BitField_offset12_count15
	.long	L_1+13623
	.long	bitfield__write_var_BitField_offset12_count14
	.long	L_1+13658
	.long	bitfield__read_var_BitField_offset12_count14
	.long	L_1+13694
	.long	bitfield__write_var_BitField_offset12_count13
	.long	L_1+13729
	.long	bitfield__read_var_BitField_offset12_count13
	.long	L_1+13765
	.long	bitfield__write_var_BitField_offset12_count12
	.long	L_1+13800
	.long	bitfield__read_var_BitField_offset12_count12
	.long	L_1+13836
	.long	bitfield__write_var_BitField_offset12_count11
	.long	L_1+13871
	.long	bitfield__read_var_BitField_offset12_count11
	.long	L_1+13907
	.long	bitfield__write_var_BitField_offset12_count10
	.long	L_1+13942
	.long	bitfield__read_var_BitField_offset12_count10
	.long	L_1+13978
	.long	bitfield__write_var_BitField_offset12_count9
	.long	L_1+14013
	.long	bitfield__read_var_BitField_offset12_count9
	.long	L_1+14048
	.long	bitfield__write_var_BitField_offset12_count8
	.long	L_1+14082
	.long	bitfield__read_var_BitField_offset12_count8
	.long	L_1+14117
	.long	bitfield__write_var_BitField_offset12_count7
	.long	L_1+14151
	.long	bitfield__read_var_BitField_offset12_count7
	.long	L_1+14186
	.long	bitfield__write_var_BitField_offset12_count6
	.long	L_1+14220
	.long	bitfield__read_var_BitField_offset12_count6
	.long	L_1+14255
	.long	bitfield__write_var_BitField_offset12_count5
	.long	L_1+14289
	.long	bitfield__read_var_BitField_offset12_count5
	.long	L_1+14324
	.long	bitfield__write_var_BitField_offset12_count4
	.long	L_1+14358
	.long	bitfield__read_var_BitField_offset12_count4
	.long	L_1+14393
	.long	bitfield__write_var_BitField_offset12_count3
	.long	L_1+14427
	.long	bitfield__read_var_BitField_offset12_count3
	.long	L_1+14462
	.long	bitfield__write_var_BitField_offset12_count2
	.long	L_1+14496
	.long	bitfield__read_var_BitField_offset12_count2
	.long	L_1+14531
	.long	bitfield__write_var_BitField_offset12_count1
	.long	L_1+14565
	.long	bitfield__read_var_BitField_offset12_count1
	.long	L_1+14600
	.long	bitfield__write_var_BitField_offset11_count21
	.long	L_1+14634
	.long	bitfield__read_var_BitField_offset11_count21
	.long	L_1+14670
	.long	bitfield__write_var_BitField_offset11_count20
	.long	L_1+14705
	.long	bitfield__read_var_BitField_offset11_count20
	.long	L_1+14741
	.long	bitfield__write_var_BitField_offset11_count19
	.long	L_1+14776
	.long	bitfield__read_var_BitField_offset11_count19
	.long	L_1+14812
	.long	bitfield__write_var_BitField_offset11_count18
	.long	L_1+14847
	.long	bitfield__read_var_BitField_offset11_count18
	.long	L_1+14883
	.long	bitfield__write_var_BitField_offset11_count17
	.long	L_1+14918
	.long	bitfield__read_var_BitField_offset11_count17
	.long	L_1+14954
	.long	bitfield__write_var_BitField_offset11_count16
	.long	L_1+14989
	.long	bitfield__read_var_BitField_offset11_count16
	.long	L_1+15025
	.long	bitfield__write_var_BitField_offset11_count15
	.long	L_1+15060
	.long	bitfield__read_var_BitField_offset11_count15
	.long	L_1+15096
	.long	bitfield__write_var_BitField_offset11_count14
	.long	L_1+15131
	.long	bitfield__read_var_BitField_offset11_count14
	.long	L_1+15167
	.long	bitfield__write_var_BitField_offset11_count13
	.long	L_1+15202
	.long	bitfield__read_var_BitField_offset11_count13
	.long	L_1+15238
	.long	bitfield__write_var_BitField_offset11_count12
	.long	L_1+15273
	.long	bitfield__read_var_BitField_offset11_count12
	.long	L_1+15309
	.long	bitfield__write_var_BitField_offset11_count11
	.long	L_1+15344
	.long	bitfield__read_var_BitField_offset11_count11
	.long	L_1+15380
	.long	bitfield__write_var_BitField_offset11_count10
	.long	L_1+15415
	.long	bitfield__read_var_BitField_offset11_count10
	.long	L_1+15451
	.long	bitfield__write_var_BitField_offset11_count9
	.long	L_1+15486
	.long	bitfield__read_var_BitField_offset11_count9
	.long	L_1+15521
	.long	bitfield__write_var_BitField_offset11_count8
	.long	L_1+15555
	.long	bitfield__read_var_BitField_offset11_count8
	.long	L_1+15590
	.long	bitfield__write_var_BitField_offset11_count7
	.long	L_1+15624
	.long	bitfield__read_var_BitField_offset11_count7
	.long	L_1+15659
	.long	bitfield__write_var_BitField_offset11_count6
	.long	L_1+15693
	.long	bitfield__read_var_BitField_offset11_count6
	.long	L_1+15728
	.long	bitfield__write_var_BitField_offset11_count5
	.long	L_1+15762
	.long	bitfield__read_var_BitField_offset11_count5
	.long	L_1+15797
	.long	bitfield__write_var_BitField_offset11_count4
	.long	L_1+15831
	.long	bitfield__read_var_BitField_offset11_count4
	.long	L_1+15866
	.long	bitfield__write_var_BitField_offset11_count3
	.long	L_1+15900
	.long	bitfield__read_var_BitField_offset11_count3
	.long	L_1+15935
	.long	bitfield__write_var_BitField_offset11_count2
	.long	L_1+15969
	.long	bitfield__read_var_BitField_offset11_count2
	.long	L_1+16004
	.long	bitfield__write_var_BitField_offset11_count1
	.long	L_1+16038
	.long	bitfield__read_var_BitField_offset11_count1
	.long	L_1+16073
	.long	bitfield__write_var_BitField_offset10_count22
	.long	L_1+16107
	.long	bitfield__read_var_BitField_offset10_count22
	.long	L_1+16143
	.long	bitfield__write_var_BitField_offset10_count21
	.long	L_1+16178
	.long	bitfield__read_var_BitField_offset10_count21
	.long	L_1+16214
	.long	bitfield__write_var_BitField_offset10_count20
	.long	L_1+16249
	.long	bitfield__read_var_BitField_offset10_count20
	.long	L_1+16285
	.long	bitfield__write_var_BitField_offset10_count19
	.long	L_1+16320
	.long	bitfield__read_var_BitField_offset10_count19
	.long	L_1+16356
	.long	bitfield__write_var_BitField_offset10_count18
	.long	L_1+16391
	.long	bitfield__read_var_BitField_offset10_count18
	.long	L_1+16427
	.long	bitfield__write_var_BitField_offset10_count17
	.long	L_1+16462
	.long	bitfield__read_var_BitField_offset10_count17
	.long	L_1+16498
	.long	bitfield__write_var_BitField_offset10_count16
	.long	L_1+16533
	.long	bitfield__read_var_BitField_offset10_count16
	.long	L_1+16569
	.long	bitfield__write_var_BitField_offset10_count15
	.long	L_1+16604
	.long	bitfield__read_var_BitField_offset10_count15
	.long	L_1+16640
	.long	bitfield__write_var_BitField_offset10_count14
	.long	L_1+16675
	.long	bitfield__read_var_BitField_offset10_count14
	.long	L_1+16711
	.long	bitfield__write_var_BitField_offset10_count13
	.long	L_1+16746
	.long	bitfield__read_var_BitField_offset10_count13
	.long	L_1+16782
	.long	bitfield__write_var_BitField_offset10_count12
	.long	L_1+16817
	.long	bitfield__read_var_BitField_offset10_count12
	.long	L_1+16853
	.long	bitfield__write_var_BitField_offset10_count11
	.long	L_1+16888
	.long	bitfield__read_var_BitField_offset10_count11
	.long	L_1+16924
	.long	bitfield__write_var_BitField_offset10_count10
	.long	L_1+16959
	.long	bitfield__read_var_BitField_offset10_count10
	.long	L_1+16995
	.long	bitfield__write_var_BitField_offset10_count9
	.long	L_1+17030
	.long	bitfield__read_var_BitField_offset10_count9
	.long	L_1+17065
	.long	bitfield__write_var_BitField_offset10_count8
	.long	L_1+17099
	.long	bitfield__read_var_BitField_offset10_count8
	.long	L_1+17134
	.long	bitfield__write_var_BitField_offset10_count7
	.long	L_1+17168
	.long	bitfield__read_var_BitField_offset10_count7
	.long	L_1+17203
	.long	bitfield__write_var_BitField_offset10_count6
	.long	L_1+17237
	.long	bitfield__read_var_BitField_offset10_count6
	.long	L_1+17272
	.long	bitfield__write_var_BitField_offset10_count5
	.long	L_1+17306
	.long	bitfield__read_var_BitField_offset10_count5
	.long	L_1+17341
	.long	bitfield__write_var_BitField_offset10_count4
	.long	L_1+17375
	.long	bitfield__read_var_BitField_offset10_count4
	.long	L_1+17410
	.long	bitfield__write_var_BitField_offset10_count3
	.long	L_1+17444
	.long	bitfield__read_var_BitField_offset10_count3
	.long	L_1+17479
	.long	bitfield__write_var_BitField_offset10_count2
	.long	L_1+17513
	.long	bitfield__read_var_BitField_offset10_count2
	.long	L_1+17548
	.long	bitfield__write_var_BitField_offset10_count1
	.long	L_1+17582
	.long	bitfield__read_var_BitField_offset10_count1
	.long	L_1+17617
	.long	bitfield__write_var_BitField_offset9_count23
	.long	L_1+17651
	.long	bitfield__read_var_BitField_offset9_count23
	.long	L_1+17686
	.long	bitfield__write_var_BitField_offset9_count22
	.long	L_1+17720
	.long	bitfield__read_var_BitField_offset9_count22
	.long	L_1+17755
	.long	bitfield__write_var_BitField_offset9_count21
	.long	L_1+17789
	.long	bitfield__read_var_BitField_offset9_count21
	.long	L_1+17824
	.long	bitfield__write_var_BitField_offset9_count20
	.long	L_1+17858
	.long	bitfield__read_var_BitField_offset9_count20
	.long	L_1+17893
	.long	bitfield__write_var_BitField_offset9_count19
	.long	L_1+17927
	.long	bitfield__read_var_BitField_offset9_count19
	.long	L_1+17962
	.long	bitfield__write_var_BitField_offset9_count18
	.long	L_1+17996
	.long	bitfield__read_var_BitField_offset9_count18
	.long	L_1+18031
	.long	bitfield__write_var_BitField_offset9_count17
	.long	L_1+18065
	.long	bitfield__read_var_BitField_offset9_count17
	.long	L_1+18100
	.long	bitfield__write_var_BitField_offset9_count16
	.long	L_1+18134
	.long	bitfield__read_var_BitField_offset9_count16
	.long	L_1+18169
	.long	bitfield__write_var_BitField_offset9_count15
	.long	L_1+18203
	.long	bitfield__read_var_BitField_offset9_count15
	.long	L_1+18238
	.long	bitfield__write_var_BitField_offset9_count14
	.long	L_1+18272
	.long	bitfield__read_var_BitField_offset9_count14
	.long	L_1+18307
	.long	bitfield__write_var_BitField_offset9_count13
	.long	L_1+18341
	.long	bitfield__read_var_BitField_offset9_count13
	.long	L_1+18376
	.long	bitfield__write_var_BitField_offset9_count12
	.long	L_1+18410
	.long	bitfield__read_var_BitField_offset9_count12
	.long	L_1+18445
	.long	bitfield__write_var_BitField_offset9_count11
	.long	L_1+18479
	.long	bitfield__read_var_BitField_offset9_count11
	.long	L_1+18514
	.long	bitfield__write_var_BitField_offset9_count10
	.long	L_1+18548
	.long	bitfield__read_var_BitField_offset9_count10
	.long	L_1+18583
	.long	bitfield__write_var_BitField_offset9_count9
	.long	L_1+18617
	.long	bitfield__read_var_BitField_offset9_count9
	.long	L_1+18651
	.long	bitfield__write_var_BitField_offset9_count8
	.long	L_1+18684
	.long	bitfield__read_var_BitField_offset9_count8
	.long	L_1+18718
	.long	bitfield__write_var_BitField_offset9_count7
	.long	L_1+18751
	.long	bitfield__read_var_BitField_offset9_count7
	.long	L_1+18785
	.long	bitfield__write_var_BitField_offset9_count6
	.long	L_1+18818
	.long	bitfield__read_var_BitField_offset9_count6
	.long	L_1+18852
	.long	bitfield__write_var_BitField_offset9_count5
	.long	L_1+18885
	.long	bitfield__read_var_BitField_offset9_count5
	.long	L_1+18919
	.long	bitfield__write_var_BitField_offset9_count4
	.long	L_1+18952
	.long	bitfield__read_var_BitField_offset9_count4
	.long	L_1+18986
	.long	bitfield__write_var_BitField_offset9_count3
	.long	L_1+19019
	.long	bitfield__read_var_BitField_offset9_count3
	.long	L_1+19053
	.long	bitfield__write_var_BitField_offset9_count2
	.long	L_1+19086
	.long	bitfield__read_var_BitField_offset9_count2
	.long	L_1+19120
	.long	bitfield__write_var_BitField_offset9_count1
	.long	L_1+19153
	.long	bitfield__read_var_BitField_offset9_count1
	.long	L_1+19187
	.long	bitfield__write_var_BitField_offset8_count24
	.long	L_1+19220
	.long	bitfield__read_var_BitField_offset8_count24
	.long	L_1+19255
	.long	bitfield__write_var_BitField_offset8_count23
	.long	L_1+19289
	.long	bitfield__read_var_BitField_offset8_count23
	.long	L_1+19324
	.long	bitfield__write_var_BitField_offset8_count22
	.long	L_1+19358
	.long	bitfield__read_var_BitField_offset8_count22
	.long	L_1+19393
	.long	bitfield__write_var_BitField_offset8_count21
	.long	L_1+19427
	.long	bitfield__read_var_BitField_offset8_count21
	.long	L_1+19462
	.long	bitfield__write_var_BitField_offset8_count20
	.long	L_1+19496
	.long	bitfield__read_var_BitField_offset8_count20
	.long	L_1+19531
	.long	bitfield__write_var_BitField_offset8_count19
	.long	L_1+19565
	.long	bitfield__read_var_BitField_offset8_count19
	.long	L_1+19600
	.long	bitfield__write_var_BitField_offset8_count18
	.long	L_1+19634
	.long	bitfield__read_var_BitField_offset8_count18
	.long	L_1+19669
	.long	bitfield__write_var_BitField_offset8_count17
	.long	L_1+19703
	.long	bitfield__read_var_BitField_offset8_count17
	.long	L_1+19738
	.long	bitfield__write_var_BitField_offset8_count16
	.long	L_1+19772
	.long	bitfield__read_var_BitField_offset8_count16
	.long	L_1+19807
	.long	bitfield__write_var_BitField_offset8_count15
	.long	L_1+19841
	.long	bitfield__read_var_BitField_offset8_count15
	.long	L_1+19876
	.long	bitfield__write_var_BitField_offset8_count14
	.long	L_1+19910
	.long	bitfield__read_var_BitField_offset8_count14
	.long	L_1+19945
	.long	bitfield__write_var_BitField_offset8_count13
	.long	L_1+19979
	.long	bitfield__read_var_BitField_offset8_count13
	.long	L_1+20014
	.long	bitfield__write_var_BitField_offset8_count12
	.long	L_1+20048
	.long	bitfield__read_var_BitField_offset8_count12
	.long	L_1+20083
	.long	bitfield__write_var_BitField_offset8_count11
	.long	L_1+20117
	.long	bitfield__read_var_BitField_offset8_count11
	.long	L_1+20152
	.long	bitfield__write_var_BitField_offset8_count10
	.long	L_1+20186
	.long	bitfield__read_var_BitField_offset8_count10
	.long	L_1+20221
	.long	bitfield__write_var_BitField_offset8_count9
	.long	L_1+20255
	.long	bitfield__read_var_BitField_offset8_count9
	.long	L_1+20289
	.long	bitfield__write_var_BitField_offset8_count8
	.long	L_1+20322
	.long	bitfield__read_var_BitField_offset8_count8
	.long	L_1+20356
	.long	bitfield__write_var_BitField_offset8_count7
	.long	L_1+20389
	.long	bitfield__read_var_BitField_offset8_count7
	.long	L_1+20423
	.long	bitfield__write_var_BitField_offset8_count6
	.long	L_1+20456
	.long	bitfield__read_var_BitField_offset8_count6
	.long	L_1+20490
	.long	bitfield__write_var_BitField_offset8_count5
	.long	L_1+20523
	.long	bitfield__read_var_BitField_offset8_count5
	.long	L_1+20557
	.long	bitfield__write_var_BitField_offset8_count4
	.long	L_1+20590
	.long	bitfield__read_var_BitField_offset8_count4
	.long	L_1+20624
	.long	bitfield__write_var_BitField_offset8_count3
	.long	L_1+20657
	.long	bitfield__read_var_BitField_offset8_count3
	.long	L_1+20691
	.long	bitfield__write_var_BitField_offset8_count2
	.long	L_1+20724
	.long	bitfield__read_var_BitField_offset8_count2
	.long	L_1+20758
	.long	bitfield__write_var_BitField_offset8_count1
	.long	L_1+20791
	.long	bitfield__read_var_BitField_offset8_count1
	.long	L_1+20825
	.long	bitfield__write_var_BitField_offset7_count25
	.long	L_1+20858
	.long	bitfield__read_var_BitField_offset7_count25
	.long	L_1+20893
	.long	bitfield__write_var_BitField_offset7_count24
	.long	L_1+20927
	.long	bitfield__read_var_BitField_offset7_count24
	.long	L_1+20962
	.long	bitfield__write_var_BitField_offset7_count23
	.long	L_1+20996
	.long	bitfield__read_var_BitField_offset7_count23
	.long	L_1+21031
	.long	bitfield__write_var_BitField_offset7_count22
	.long	L_1+21065
	.long	bitfield__read_var_BitField_offset7_count22
	.long	L_1+21100
	.long	bitfield__write_var_BitField_offset7_count21
	.long	L_1+21134
	.long	bitfield__read_var_BitField_offset7_count21
	.long	L_1+21169
	.long	bitfield__write_var_BitField_offset7_count20
	.long	L_1+21203
	.long	bitfield__read_var_BitField_offset7_count20
	.long	L_1+21238
	.long	bitfield__write_var_BitField_offset7_count19
	.long	L_1+21272
	.long	bitfield__read_var_BitField_offset7_count19
	.long	L_1+21307
	.long	bitfield__write_var_BitField_offset7_count18
	.long	L_1+21341
	.long	bitfield__read_var_BitField_offset7_count18
	.long	L_1+21376
	.long	bitfield__write_var_BitField_offset7_count17
	.long	L_1+21410
	.long	bitfield__read_var_BitField_offset7_count17
	.long	L_1+21445
	.long	bitfield__write_var_BitField_offset7_count16
	.long	L_1+21479
	.long	bitfield__read_var_BitField_offset7_count16
	.long	L_1+21514
	.long	bitfield__write_var_BitField_offset7_count15
	.long	L_1+21548
	.long	bitfield__read_var_BitField_offset7_count15
	.long	L_1+21583
	.long	bitfield__write_var_BitField_offset7_count14
	.long	L_1+21617
	.long	bitfield__read_var_BitField_offset7_count14
	.long	L_1+21652
	.long	bitfield__write_var_BitField_offset7_count13
	.long	L_1+21686
	.long	bitfield__read_var_BitField_offset7_count13
	.long	L_1+21721
	.long	bitfield__write_var_BitField_offset7_count12
	.long	L_1+21755
	.long	bitfield__read_var_BitField_offset7_count12
	.long	L_1+21790
	.long	bitfield__write_var_BitField_offset7_count11
	.long	L_1+21824
	.long	bitfield__read_var_BitField_offset7_count11
	.long	L_1+21859
	.long	bitfield__write_var_BitField_offset7_count10
	.long	L_1+21893
	.long	bitfield__read_var_BitField_offset7_count10
	.long	L_1+21928
	.long	bitfield__write_var_BitField_offset7_count9
	.long	L_1+21962
	.long	bitfield__read_var_BitField_offset7_count9
	.long	L_1+21996
	.long	bitfield__write_var_BitField_offset7_count8
	.long	L_1+22029
	.long	bitfield__read_var_BitField_offset7_count8
	.long	L_1+22063
	.long	bitfield__write_var_BitField_offset7_count7
	.long	L_1+22096
	.long	bitfield__read_var_BitField_offset7_count7
	.long	L_1+22130
	.long	bitfield__write_var_BitField_offset7_count6
	.long	L_1+22163
	.long	bitfield__read_var_BitField_offset7_count6
	.long	L_1+22197
	.long	bitfield__write_var_BitField_offset7_count5
	.long	L_1+22230
	.long	bitfield__read_var_BitField_offset7_count5
	.long	L_1+22264
	.long	bitfield__write_var_BitField_offset7_count4
	.long	L_1+22297
	.long	bitfield__read_var_BitField_offset7_count4
	.long	L_1+22331
	.long	bitfield__write_var_BitField_offset7_count3
	.long	L_1+22364
	.long	bitfield__read_var_BitField_offset7_count3
	.long	L_1+22398
	.long	bitfield__write_var_BitField_offset7_count2
	.long	L_1+22431
	.long	bitfield__read_var_BitField_offset7_count2
	.long	L_1+22465
	.long	bitfield__write_var_BitField_offset7_count1
	.long	L_1+22498
	.long	bitfield__read_var_BitField_offset7_count1
	.long	L_1+22532
	.long	bitfield__write_var_BitField_offset6_count26
	.long	L_1+22565
	.long	bitfield__read_var_BitField_offset6_count26
	.long	L_1+22600
	.long	bitfield__write_var_BitField_offset6_count25
	.long	L_1+22634
	.long	bitfield__read_var_BitField_offset6_count25
	.long	L_1+22669
	.long	bitfield__write_var_BitField_offset6_count24
	.long	L_1+22703
	.long	bitfield__read_var_BitField_offset6_count24
	.long	L_1+22738
	.long	bitfield__write_var_BitField_offset6_count23
	.long	L_1+22772
	.long	bitfield__read_var_BitField_offset6_count23
	.long	L_1+22807
	.long	bitfield__write_var_BitField_offset6_count22
	.long	L_1+22841
	.long	bitfield__read_var_BitField_offset6_count22
	.long	L_1+22876
	.long	bitfield__write_var_BitField_offset6_count21
	.long	L_1+22910
	.long	bitfield__read_var_BitField_offset6_count21
	.long	L_1+22945
	.long	bitfield__write_var_BitField_offset6_count20
	.long	L_1+22979
	.long	bitfield__read_var_BitField_offset6_count20
	.long	L_1+23014
	.long	bitfield__write_var_BitField_offset6_count19
	.long	L_1+23048
	.long	bitfield__read_var_BitField_offset6_count19
	.long	L_1+23083
	.long	bitfield__write_var_BitField_offset6_count18
	.long	L_1+23117
	.long	bitfield__read_var_BitField_offset6_count18
	.long	L_1+23152
	.long	bitfield__write_var_BitField_offset6_count17
	.long	L_1+23186
	.long	bitfield__read_var_BitField_offset6_count17
	.long	L_1+23221
	.long	bitfield__write_var_BitField_offset6_count16
	.long	L_1+23255
	.long	bitfield__read_var_BitField_offset6_count16
	.long	L_1+23290
	.long	bitfield__write_var_BitField_offset6_count15
	.long	L_1+23324
	.long	bitfield__read_var_BitField_offset6_count15
	.long	L_1+23359
	.long	bitfield__write_var_BitField_offset6_count14
	.long	L_1+23393
	.long	bitfield__read_var_BitField_offset6_count14
	.long	L_1+23428
	.long	bitfield__write_var_BitField_offset6_count13
	.long	L_1+23462
	.long	bitfield__read_var_BitField_offset6_count13
	.long	L_1+23497
	.long	bitfield__write_var_BitField_offset6_count12
	.long	L_1+23531
	.long	bitfield__read_var_BitField_offset6_count12
	.long	L_1+23566
	.long	bitfield__write_var_BitField_offset6_count11
	.long	L_1+23600
	.long	bitfield__read_var_BitField_offset6_count11
	.long	L_1+23635
	.long	bitfield__write_var_BitField_offset6_count10
	.long	L_1+23669
	.long	bitfield__read_var_BitField_offset6_count10
	.long	L_1+23704
	.long	bitfield__write_var_BitField_offset6_count9
	.long	L_1+23738
	.long	bitfield__read_var_BitField_offset6_count9
	.long	L_1+23772
	.long	bitfield__write_var_BitField_offset6_count8
	.long	L_1+23805
	.long	bitfield__read_var_BitField_offset6_count8
	.long	L_1+23839
	.long	bitfield__write_var_BitField_offset6_count7
	.long	L_1+23872
	.long	bitfield__read_var_BitField_offset6_count7
	.long	L_1+23906
	.long	bitfield__write_var_BitField_offset6_count6
	.long	L_1+23939
	.long	bitfield__read_var_BitField_offset6_count6
	.long	L_1+23973
	.long	bitfield__write_var_BitField_offset6_count5
	.long	L_1+24006
	.long	bitfield__read_var_BitField_offset6_count5
	.long	L_1+24040
	.long	bitfield__write_var_BitField_offset6_count4
	.long	L_1+24073
	.long	bitfield__read_var_BitField_offset6_count4
	.long	L_1+24107
	.long	bitfield__write_var_BitField_offset6_count3
	.long	L_1+24140
	.long	bitfield__read_var_BitField_offset6_count3
	.long	L_1+24174
	.long	bitfield__write_var_BitField_offset6_count2
	.long	L_1+24207
	.long	bitfield__read_var_BitField_offset6_count2
	.long	L_1+24241
	.long	bitfield__write_var_BitField_offset6_count1
	.long	L_1+24274
	.long	bitfield__read_var_BitField_offset6_count1
	.long	L_1+24308
	.long	bitfield__write_var_BitField_offset5_count27
	.long	L_1+24341
	.long	bitfield__read_var_BitField_offset5_count27
	.long	L_1+24376
	.long	bitfield__write_var_BitField_offset5_count26
	.long	L_1+24410
	.long	bitfield__read_var_BitField_offset5_count26
	.long	L_1+24445
	.long	bitfield__write_var_BitField_offset5_count25
	.long	L_1+24479
	.long	bitfield__read_var_BitField_offset5_count25
	.long	L_1+24514
	.long	bitfield__write_var_BitField_offset5_count24
	.long	L_1+24548
	.long	bitfield__read_var_BitField_offset5_count24
	.long	L_1+24583
	.long	bitfield__write_var_BitField_offset5_count23
	.long	L_1+24617
	.long	bitfield__read_var_BitField_offset5_count23
	.long	L_1+24652
	.long	bitfield__write_var_BitField_offset5_count22
	.long	L_1+24686
	.long	bitfield__read_var_BitField_offset5_count22
	.long	L_1+24721
	.long	bitfield__write_var_BitField_offset5_count21
	.long	L_1+24755
	.long	bitfield__read_var_BitField_offset5_count21
	.long	L_1+24790
	.long	bitfield__write_var_BitField_offset5_count20
	.long	L_1+24824
	.long	bitfield__read_var_BitField_offset5_count20
	.long	L_1+24859
	.long	bitfield__write_var_BitField_offset5_count19
	.long	L_1+24893
	.long	bitfield__read_var_BitField_offset5_count19
	.long	L_1+24928
	.long	bitfield__write_var_BitField_offset5_count18
	.long	L_1+24962
	.long	bitfield__read_var_BitField_offset5_count18
	.long	L_1+24997
	.long	bitfield__write_var_BitField_offset5_count17
	.long	L_1+25031
	.long	bitfield__read_var_BitField_offset5_count17
	.long	L_1+25066
	.long	bitfield__write_var_BitField_offset5_count16
	.long	L_1+25100
	.long	bitfield__read_var_BitField_offset5_count16
	.long	L_1+25135
	.long	bitfield__write_var_BitField_offset5_count15
	.long	L_1+25169
	.long	bitfield__read_var_BitField_offset5_count15
	.long	L_1+25204
	.long	bitfield__write_var_BitField_offset5_count14
	.long	L_1+25238
	.long	bitfield__read_var_BitField_offset5_count14
	.long	L_1+25273
	.long	bitfield__write_var_BitField_offset5_count13
	.long	L_1+25307
	.long	bitfield__read_var_BitField_offset5_count13
	.long	L_1+25342
	.long	bitfield__write_var_BitField_offset5_count12
	.long	L_1+25376
	.long	bitfield__read_var_BitField_offset5_count12
	.long	L_1+25411
	.long	bitfield__write_var_BitField_offset5_count11
	.long	L_1+25445
	.long	bitfield__read_var_BitField_offset5_count11
	.long	L_1+25480
	.long	bitfield__write_var_BitField_offset5_count10
	.long	L_1+25514
	.long	bitfield__read_var_BitField_offset5_count10
	.long	L_1+25549
	.long	bitfield__write_var_BitField_offset5_count9
	.long	L_1+25583
	.long	bitfield__read_var_BitField_offset5_count9
	.long	L_1+25617
	.long	bitfield__write_var_BitField_offset5_count8
	.long	L_1+25650
	.long	bitfield__read_var_BitField_offset5_count8
	.long	L_1+25684
	.long	bitfield__write_var_BitField_offset5_count7
	.long	L_1+25717
	.long	bitfield__read_var_BitField_offset5_count7
	.long	L_1+25751
	.long	bitfield__write_var_BitField_offset5_count6
	.long	L_1+25784
	.long	bitfield__read_var_BitField_offset5_count6
	.long	L_1+25818
	.long	bitfield__write_var_BitField_offset5_count5
	.long	L_1+25851
	.long	bitfield__read_var_BitField_offset5_count5
	.long	L_1+25885
	.long	bitfield__write_var_BitField_offset5_count4
	.long	L_1+25918
	.long	bitfield__read_var_BitField_offset5_count4
	.long	L_1+25952
	.long	bitfield__write_var_BitField_offset5_count3
	.long	L_1+25985
	.long	bitfield__read_var_BitField_offset5_count3
	.long	L_1+26019
	.long	bitfield__write_var_BitField_offset5_count2
	.long	L_1+26052
	.long	bitfield__read_var_BitField_offset5_count2
	.long	L_1+26086
	.long	bitfield__write_var_BitField_offset5_count1
	.long	L_1+26119
	.long	bitfield__read_var_BitField_offset5_count1
	.long	L_1+26153
	.long	bitfield__write_var_BitField_offset4_count28
	.long	L_1+26186
	.long	bitfield__read_var_BitField_offset4_count28
	.long	L_1+26221
	.long	bitfield__write_var_BitField_offset4_count27
	.long	L_1+26255
	.long	bitfield__read_var_BitField_offset4_count27
	.long	L_1+26290
	.long	bitfield__write_var_BitField_offset4_count26
	.long	L_1+26324
	.long	bitfield__read_var_BitField_offset4_count26
	.long	L_1+26359
	.long	bitfield__write_var_BitField_offset4_count25
	.long	L_1+26393
	.long	bitfield__read_var_BitField_offset4_count25
	.long	L_1+26428
	.long	bitfield__write_var_BitField_offset4_count24
	.long	L_1+26462
	.long	bitfield__read_var_BitField_offset4_count24
	.long	L_1+26497
	.long	bitfield__write_var_BitField_offset4_count23
	.long	L_1+26531
	.long	bitfield__read_var_BitField_offset4_count23
	.long	L_1+26566
	.long	bitfield__write_var_BitField_offset4_count22
	.long	L_1+26600
	.long	bitfield__read_var_BitField_offset4_count22
	.long	L_1+26635
	.long	bitfield__write_var_BitField_offset4_count21
	.long	L_1+26669
	.long	bitfield__read_var_BitField_offset4_count21
	.long	L_1+26704
	.long	bitfield__write_var_BitField_offset4_count20
	.long	L_1+26738
	.long	bitfield__read_var_BitField_offset4_count20
	.long	L_1+26773
	.long	bitfield__write_var_BitField_offset4_count19
	.long	L_1+26807
	.long	bitfield__read_var_BitField_offset4_count19
	.long	L_1+26842
	.long	bitfield__write_var_BitField_offset4_count18
	.long	L_1+26876
	.long	bitfield__read_var_BitField_offset4_count18
	.long	L_1+26911
	.long	bitfield__write_var_BitField_offset4_count17
	.long	L_1+26945
	.long	bitfield__read_var_BitField_offset4_count17
	.long	L_1+26980
	.long	bitfield__write_var_BitField_offset4_count16
	.long	L_1+27014
	.long	bitfield__read_var_BitField_offset4_count16
	.long	L_1+27049
	.long	bitfield__write_var_BitField_offset4_count15
	.long	L_1+27083
	.long	bitfield__read_var_BitField_offset4_count15
	.long	L_1+27118
	.long	bitfield__write_var_BitField_offset4_count14
	.long	L_1+27152
	.long	bitfield__read_var_BitField_offset4_count14
	.long	L_1+27187
	.long	bitfield__write_var_BitField_offset4_count13
	.long	L_1+27221
	.long	bitfield__read_var_BitField_offset4_count13
	.long	L_1+27256
	.long	bitfield__write_var_BitField_offset4_count12
	.long	L_1+27290
	.long	bitfield__read_var_BitField_offset4_count12
	.long	L_1+27325
	.long	bitfield__write_var_BitField_offset4_count11
	.long	L_1+27359
	.long	bitfield__read_var_BitField_offset4_count11
	.long	L_1+27394
	.long	bitfield__write_var_BitField_offset4_count10
	.long	L_1+27428
	.long	bitfield__read_var_BitField_offset4_count10
	.long	L_1+27463
	.long	bitfield__write_var_BitField_offset4_count9
	.long	L_1+27497
	.long	bitfield__read_var_BitField_offset4_count9
	.long	L_1+27531
	.long	bitfield__write_var_BitField_offset4_count8
	.long	L_1+27564
	.long	bitfield__read_var_BitField_offset4_count8
	.long	L_1+27598
	.long	bitfield__write_var_BitField_offset4_count7
	.long	L_1+27631
	.long	bitfield__read_var_BitField_offset4_count7
	.long	L_1+27665
	.long	bitfield__write_var_BitField_offset4_count6
	.long	L_1+27698
	.long	bitfield__read_var_BitField_offset4_count6
	.long	L_1+27732
	.long	bitfield__write_var_BitField_offset4_count5
	.long	L_1+27765
	.long	bitfield__read_var_BitField_offset4_count5
	.long	L_1+27799
	.long	bitfield__write_var_BitField_offset4_count4
	.long	L_1+27832
	.long	bitfield__read_var_BitField_offset4_count4
	.long	L_1+27866
	.long	bitfield__write_var_BitField_offset4_count3
	.long	L_1+27899
	.long	bitfield__read_var_BitField_offset4_count3
	.long	L_1+27933
	.long	bitfield__write_var_BitField_offset4_count2
	.long	L_1+27966
	.long	bitfield__read_var_BitField_offset4_count2
	.long	L_1+28000
	.long	bitfield__write_var_BitField_offset4_count1
	.long	L_1+28033
	.long	bitfield__read_var_BitField_offset4_count1
	.long	L_1+28067
	.long	bitfield__write_var_BitField_offset3_count29
	.long	L_1+28100
	.long	bitfield__read_var_BitField_offset3_count29
	.long	L_1+28135
	.long	bitfield__write_var_BitField_offset3_count28
	.long	L_1+28169
	.long	bitfield__read_var_BitField_offset3_count28
	.long	L_1+28204
	.long	bitfield__write_var_BitField_offset3_count27
	.long	L_1+28238
	.long	bitfield__read_var_BitField_offset3_count27
	.long	L_1+28273
	.long	bitfield__write_var_BitField_offset3_count26
	.long	L_1+28307
	.long	bitfield__read_var_BitField_offset3_count26
	.long	L_1+28342
	.long	bitfield__write_var_BitField_offset3_count25
	.long	L_1+28376
	.long	bitfield__read_var_BitField_offset3_count25
	.long	L_1+28411
	.long	bitfield__write_var_BitField_offset3_count24
	.long	L_1+28445
	.long	bitfield__read_var_BitField_offset3_count24
	.long	L_1+28480
	.long	bitfield__write_var_BitField_offset3_count23
	.long	L_1+28514
	.long	bitfield__read_var_BitField_offset3_count23
	.long	L_1+28549
	.long	bitfield__write_var_BitField_offset3_count22
	.long	L_1+28583
	.long	bitfield__read_var_BitField_offset3_count22
	.long	L_1+28618
	.long	bitfield__write_var_BitField_offset3_count21
	.long	L_1+28652
	.long	bitfield__read_var_BitField_offset3_count21
	.long	L_1+28687
	.long	bitfield__write_var_BitField_offset3_count20
	.long	L_1+28721
	.long	bitfield__read_var_BitField_offset3_count20
	.long	L_1+28756
	.long	bitfield__write_var_BitField_offset3_count19
	.long	L_1+28790
	.long	bitfield__read_var_BitField_offset3_count19
	.long	L_1+28825
	.long	bitfield__write_var_BitField_offset3_count18
	.long	L_1+28859
	.long	bitfield__read_var_BitField_offset3_count18
	.long	L_1+28894
	.long	bitfield__write_var_BitField_offset3_count17
	.long	L_1+28928
	.long	bitfield__read_var_BitField_offset3_count17
	.long	L_1+28963
	.long	bitfield__write_var_BitField_offset3_count16
	.long	L_1+28997
	.long	bitfield__read_var_BitField_offset3_count16
	.long	L_1+29032
	.long	bitfield__write_var_BitField_offset3_count15
	.long	L_1+29066
	.long	bitfield__read_var_BitField_offset3_count15
	.long	L_1+29101
	.long	bitfield__write_var_BitField_offset3_count14
	.long	L_1+29135
	.long	bitfield__read_var_BitField_offset3_count14
	.long	L_1+29170
	.long	bitfield__write_var_BitField_offset3_count13
	.long	L_1+29204
	.long	bitfield__read_var_BitField_offset3_count13
	.long	L_1+29239
	.long	bitfield__write_var_BitField_offset3_count12
	.long	L_1+29273
	.long	bitfield__read_var_BitField_offset3_count12
	.long	L_1+29308
	.long	bitfield__write_var_BitField_offset3_count11
	.long	L_1+29342
	.long	bitfield__read_var_BitField_offset3_count11
	.long	L_1+29377
	.long	bitfield__write_var_BitField_offset3_count10
	.long	L_1+29411
	.long	bitfield__read_var_BitField_offset3_count10
	.long	L_1+29446
	.long	bitfield__write_var_BitField_offset3_count9
	.long	L_1+29480
	.long	bitfield__read_var_BitField_offset3_count9
	.long	L_1+29514
	.long	bitfield__write_var_BitField_offset3_count8
	.long	L_1+29547
	.long	bitfield__read_var_BitField_offset3_count8
	.long	L_1+29581
	.long	bitfield__write_var_BitField_offset3_count7
	.long	L_1+29614
	.long	bitfield__read_var_BitField_offset3_count7
	.long	L_1+29648
	.long	bitfield__write_var_BitField_offset3_count6
	.long	L_1+29681
	.long	bitfield__read_var_BitField_offset3_count6
	.long	L_1+29715
	.long	bitfield__write_var_BitField_offset3_count5
	.long	L_1+29748
	.long	bitfield__read_var_BitField_offset3_count5
	.long	L_1+29782
	.long	bitfield__write_var_BitField_offset3_count4
	.long	L_1+29815
	.long	bitfield__read_var_BitField_offset3_count4
	.long	L_1+29849
	.long	bitfield__write_var_BitField_offset3_count3
	.long	L_1+29882
	.long	bitfield__read_var_BitField_offset3_count3
	.long	L_1+29916
	.long	bitfield__write_var_BitField_offset3_count2
	.long	L_1+29949
	.long	bitfield__read_var_BitField_offset3_count2
	.long	L_1+29983
	.long	bitfield__write_var_BitField_offset3_count1
	.long	L_1+30016
	.long	bitfield__read_var_BitField_offset3_count1
	.long	L_1+30050
	.long	bitfield__write_var_BitField_offset2_count30
	.long	L_1+30083
	.long	bitfield__read_var_BitField_offset2_count30
	.long	L_1+30118
	.long	bitfield__write_var_BitField_offset2_count29
	.long	L_1+30152
	.long	bitfield__read_var_BitField_offset2_count29
	.long	L_1+30187
	.long	bitfield__write_var_BitField_offset2_count28
	.long	L_1+30221
	.long	bitfield__read_var_BitField_offset2_count28
	.long	L_1+30256
	.long	bitfield__write_var_BitField_offset2_count27
	.long	L_1+30290
	.long	bitfield__read_var_BitField_offset2_count27
	.long	L_1+30325
	.long	bitfield__write_var_BitField_offset2_count26
	.long	L_1+30359
	.long	bitfield__read_var_BitField_offset2_count26
	.long	L_1+30394
	.long	bitfield__write_var_BitField_offset2_count25
	.long	L_1+30428
	.long	bitfield__read_var_BitField_offset2_count25
	.long	L_1+30463
	.long	bitfield__write_var_BitField_offset2_count24
	.long	L_1+30497
	.long	bitfield__read_var_BitField_offset2_count24
	.long	L_1+30532
	.long	bitfield__write_var_BitField_offset2_count23
	.long	L_1+30566
	.long	bitfield__read_var_BitField_offset2_count23
	.long	L_1+30601
	.long	bitfield__write_var_BitField_offset2_count22
	.long	L_1+30635
	.long	bitfield__read_var_BitField_offset2_count22
	.long	L_1+30670
	.long	bitfield__write_var_BitField_offset2_count21
	.long	L_1+30704
	.long	bitfield__read_var_BitField_offset2_count21
	.long	L_1+30739
	.long	bitfield__write_var_BitField_offset2_count20
	.long	L_1+30773
	.long	bitfield__read_var_BitField_offset2_count20
	.long	L_1+30808
	.long	bitfield__write_var_BitField_offset2_count19
	.long	L_1+30842
	.long	bitfield__read_var_BitField_offset2_count19
	.long	L_1+30877
	.long	bitfield__write_var_BitField_offset2_count18
	.long	L_1+30911
	.long	bitfield__read_var_BitField_offset2_count18
	.long	L_1+30946
	.long	bitfield__write_var_BitField_offset2_count17
	.long	L_1+30980
	.long	bitfield__read_var_BitField_offset2_count17
	.long	L_1+31015
	.long	bitfield__write_var_BitField_offset2_count16
	.long	L_1+31049
	.long	bitfield__read_var_BitField_offset2_count16
	.long	L_1+31084
	.long	bitfield__write_var_BitField_offset2_count15
	.long	L_1+31118
	.long	bitfield__read_var_BitField_offset2_count15
	.long	L_1+31153
	.long	bitfield__write_var_BitField_offset2_count14
	.long	L_1+31187
	.long	bitfield__read_var_BitField_offset2_count14
	.long	L_1+31222
	.long	bitfield__write_var_BitField_offset2_count13
	.long	L_1+31256
	.long	bitfield__read_var_BitField_offset2_count13
	.long	L_1+31291
	.long	bitfield__write_var_BitField_offset2_count12
	.long	L_1+31325
	.long	bitfield__read_var_BitField_offset2_count12
	.long	L_1+31360
	.long	bitfield__write_var_BitField_offset2_count11
	.long	L_1+31394
	.long	bitfield__read_var_BitField_offset2_count11
	.long	L_1+31429
	.long	bitfield__write_var_BitField_offset2_count10
	.long	L_1+31463
	.long	bitfield__read_var_BitField_offset2_count10
	.long	L_1+31498
	.long	bitfield__write_var_BitField_offset2_count9
	.long	L_1+31532
	.long	bitfield__read_var_BitField_offset2_count9
	.long	L_1+31566
	.long	bitfield__write_var_BitField_offset2_count8
	.long	L_1+31599
	.long	bitfield__read_var_BitField_offset2_count8
	.long	L_1+31633
	.long	bitfield__write_var_BitField_offset2_count7
	.long	L_1+31666
	.long	bitfield__read_var_BitField_offset2_count7
	.long	L_1+31700
	.long	bitfield__write_var_BitField_offset2_count6
	.long	L_1+31733
	.long	bitfield__read_var_BitField_offset2_count6
	.long	L_1+31767
	.long	bitfield__write_var_BitField_offset2_count5
	.long	L_1+31800
	.long	bitfield__read_var_BitField_offset2_count5
	.long	L_1+31834
	.long	bitfield__write_var_BitField_offset2_count4
	.long	L_1+31867
	.long	bitfield__read_var_BitField_offset2_count4
	.long	L_1+31901
	.long	bitfield__write_var_BitField_offset2_count3
	.long	L_1+31934
	.long	bitfield__read_var_BitField_offset2_count3
	.long	L_1+31968
	.long	bitfield__write_var_BitField_offset2_count2
	.long	L_1+32001
	.long	bitfield__read_var_BitField_offset2_count2
	.long	L_1+32035
	.long	bitfield__write_var_BitField_offset2_count1
	.long	L_1+32068
	.long	bitfield__read_var_BitField_offset2_count1
	.long	L_1+32102
	.long	bitfield__write_var_BitField_offset1_count31
	.long	L_1+32135
	.long	bitfield__read_var_BitField_offset1_count31
	.long	L_1+32170
	.long	bitfield__write_var_BitField_offset1_count30
	.long	L_1+32204
	.long	bitfield__read_var_BitField_offset1_count30
	.long	L_1+32239
	.long	bitfield__write_var_BitField_offset1_count29
	.long	L_1+32273
	.long	bitfield__read_var_BitField_offset1_count29
	.long	L_1+32308
	.long	bitfield__write_var_BitField_offset1_count28
	.long	L_1+32342
	.long	bitfield__read_var_BitField_offset1_count28
	.long	L_1+32377
	.long	bitfield__write_var_BitField_offset1_count27
	.long	L_1+32411
	.long	bitfield__read_var_BitField_offset1_count27
	.long	L_1+32446
	.long	bitfield__write_var_BitField_offset1_count26
	.long	L_1+32480
	.long	bitfield__read_var_BitField_offset1_count26
	.long	L_1+32515
	.long	bitfield__write_var_BitField_offset1_count25
	.long	L_1+32549
	.long	bitfield__read_var_BitField_offset1_count25
	.long	L_1+32584
	.long	bitfield__write_var_BitField_offset1_count24
	.long	L_1+32618
	.long	bitfield__read_var_BitField_offset1_count24
	.long	L_1+32653
	.long	bitfield__write_var_BitField_offset1_count23
	.long	L_1+32687
	.long	bitfield__read_var_BitField_offset1_count23
	.long	L_1+32722
	.long	bitfield__write_var_BitField_offset1_count22
	.long	L_1+32756
	.long	bitfield__read_var_BitField_offset1_count22
	.long	L_1+32791
	.long	bitfield__write_var_BitField_offset1_count21
	.long	L_1+32825
	.long	bitfield__read_var_BitField_offset1_count21
	.long	L_1+32860
	.long	bitfield__write_var_BitField_offset1_count20
	.long	L_1+32894
	.long	bitfield__read_var_BitField_offset1_count20
	.long	L_1+32929
	.long	bitfield__write_var_BitField_offset1_count19
	.long	L_1+32963
	.long	bitfield__read_var_BitField_offset1_count19
	.long	L_1+32998
	.long	bitfield__write_var_BitField_offset1_count18
	.long	L_1+33032
	.long	bitfield__read_var_BitField_offset1_count18
	.long	L_1+33067
	.long	bitfield__write_var_BitField_offset1_count17
	.long	L_1+33101
	.long	bitfield__read_var_BitField_offset1_count17
	.long	L_1+33136
	.long	bitfield__write_var_BitField_offset1_count16
	.long	L_1+33170
	.long	bitfield__read_var_BitField_offset1_count16
	.long	L_1+33205
	.long	bitfield__write_var_BitField_offset1_count15
	.long	L_1+33239
	.long	bitfield__read_var_BitField_offset1_count15
	.long	L_1+33274
	.long	bitfield__write_var_BitField_offset1_count14
	.long	L_1+33308
	.long	bitfield__read_var_BitField_offset1_count14
	.long	L_1+33343
	.long	bitfield__write_var_BitField_offset1_count13
	.long	L_1+33377
	.long	bitfield__read_var_BitField_offset1_count13
	.long	L_1+33412
	.long	bitfield__write_var_BitField_offset1_count12
	.long	L_1+33446
	.long	bitfield__read_var_BitField_offset1_count12
	.long	L_1+33481
	.long	bitfield__write_var_BitField_offset1_count11
	.long	L_1+33515
	.long	bitfield__read_var_BitField_offset1_count11
	.long	L_1+33550
	.long	bitfield__write_var_BitField_offset1_count10
	.long	L_1+33584
	.long	bitfield__read_var_BitField_offset1_count10
	.long	L_1+33619
	.long	bitfield__write_var_BitField_offset1_count9
	.long	L_1+33653
	.long	bitfield__read_var_BitField_offset1_count9
	.long	L_1+33687
	.long	bitfield__write_var_BitField_offset1_count8
	.long	L_1+33720
	.long	bitfield__read_var_BitField_offset1_count8
	.long	L_1+33754
	.long	bitfield__write_var_BitField_offset1_count7
	.long	L_1+33787
	.long	bitfield__read_var_BitField_offset1_count7
	.long	L_1+33821
	.long	bitfield__write_var_BitField_offset1_count6
	.long	L_1+33854
	.long	bitfield__read_var_BitField_offset1_count6
	.long	L_1+33888
	.long	bitfield__write_var_BitField_offset1_count5
	.long	L_1+33921
	.long	bitfield__read_var_BitField_offset1_count5
	.long	L_1+33955
	.long	bitfield__write_var_BitField_offset1_count4
	.long	L_1+33988
	.long	bitfield__read_var_BitField_offset1_count4
	.long	L_1+34022
	.long	bitfield__write_var_BitField_offset1_count3
	.long	L_1+34055
	.long	bitfield__read_var_BitField_offset1_count3
	.long	L_1+34089
	.long	bitfield__write_var_BitField_offset1_count2
	.long	L_1+34122
	.long	bitfield__read_var_BitField_offset1_count2
	.long	L_1+34156
	.long	bitfield__write_var_BitField_offset1_count1
	.long	L_1+34189
	.long	bitfield__read_var_BitField_offset1_count1
	.long	L_1+34223
	.skip 4
	.ascii	"../SOLgnu/bitfield.m3"
	.skip 7
	.align 8
	.type	MM_bitfield, #object
	.size	MM_bitfield, 208
MM_bitfield:
	.long	L_1+42204
	.skip 16
	.long	L_1+34256
	.skip 12
	.long	MM_bitfield+144
	.skip 4
	.long	bitfield_M3
	.long	3
	.byte	25
	.skip 3
	.long	0
	.long	1050
	.long	0
	.long	1051
	.byte	28
	.skip 3
	.long	1053
	.long	1054
	.long	1055
	.half	1056
	.skip 6
	.long	0
	.long	1057
	.long	1083213932
	.long	1896657558
	.half	1060
	.skip 6
	.long	0
	.long	1061
	.long	1149551463
	.long	1064
	.skip 12
	.long	bitfield_I3
	.long	MM_bitfield+156
	.skip 4
	.long	Long_I3
	.long	MM_bitfield+168
	.skip 4
	.long	Word_I3
	.long	MM_bitfield+180
	.skip 4
	.long	Cstdint_I3
	.long	MM_bitfield+192
	.skip 4
	.long	RTHooks_I3
	.skip 8
	.ident	"GCC: (GNU) 4.3.0"
